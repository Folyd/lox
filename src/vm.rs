use core::panic;
use std::{array, borrow::Cow, collections::HashMap};

use gc_arena::{
    lock::{GcRefLock, RefLock},
    Arena, Collect, Collection, CollectionPhase, Gc, Mutation, Rootable,
};

use crate::{
    builtins,
    compiler::compile,
    fuel::Fuel,
    object::{BoundMethod, Class, Closure, Instance, NativeFn, UpvalueObj},
    OpCode, Value,
};

const FRAME_MAX_SIZE: usize = 64;
const STACK_MAX_SIZE: usize = FRAME_MAX_SIZE * (u8::MAX as usize + 1);

#[derive(Debug)]
pub enum InterpretResult {
    Ok,
    #[allow(unused)]
    CompileError,
    RuntimeError(Cow<'static, str>),
}

#[derive(Clone)]
pub struct State<'gc> {
    pub mc: &'gc Mutation<'gc>,
    pub frames: Vec<CallFrame<'gc>>,
    pub frame_count: usize,
    pub stack: [Value<'gc>; STACK_MAX_SIZE],
    pub stack_top: usize,
    pub globals: HashMap<Gc<'gc, String>, Value<'gc>>,
    pub open_upvalues: Option<GcRefLock<'gc, UpvalueObj<'gc>>>,
}

unsafe impl<'gc> Collect for State<'gc> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, cc: &Collection) {
        self.frames.trace(cc);
        self.frame_count.trace(cc);
        self.stack.trace(cc);
        self.stack_top.trace(cc);
        // self.globals.trace(cc);
        self.open_upvalues.trace(cc);
    }
}

pub struct Vm {
    arena: Arena<Rootable![State<'_>]>,
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct CallFrame<'gc> {
    // pub function: Function,
    pub closure: Gc<'gc, Closure<'gc>>,
    // When we return from a function, the VM will
    // jump to the ip of the caller’s CallFrame and resume from there.
    pub ip: usize,
    // slot_start field points into the VM’s value stack
    // at the first slot that this function can use
    pub slot_start: usize,
}

impl<'gc> CallFrame<'gc> {
    fn read_byte(&mut self) -> u8 {
        let byte = self.closure.function[self.ip];
        self.ip += 1;
        byte
    }

    fn read_short(&mut self) -> u16 {
        let short = u16::from_be_bytes([
            self.closure.function[self.ip],
            self.closure.function[self.ip + 1],
        ]);
        self.ip += 2;
        short
    }

    fn read_constant(&mut self, byte: u8) -> Value<'gc> {
        self.closure.function.read_constant(byte)
    }

    fn read_string(&mut self) -> Gc<'gc, String> {
        let byte = self.read_byte();
        self.read_constant(byte).as_string().unwrap()
    }

    #[allow(unused)]
    fn disassemble(&self) {
        self.closure
            .function
            .disassemble(&self.closure.function.name);
    }

    #[allow(unused)]
    fn disassemble_instruction(&self, offset: usize) {
        self.closure.function.disassemble_instruction(offset);
    }
}

impl<'gc> State<'gc> {
    fn new(mc: &'gc Mutation<'gc>) -> Self {
        State {
            mc,
            frames: Vec::new(),
            frame_count: 0,
            stack: array::from_fn(|_| Value::Nil),
            stack_top: 0,
            globals: HashMap::default(),
            open_upvalues: None,
        }
    }
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            arena: Arena::<Rootable![State<'_>]>::new(|mc| State::new(mc)),
        }
    }

    pub fn interpret(&mut self, source: &'static str) -> InterpretResult {
        self.arena.mutate_root(|mc, state| {
            return match compile(mc, source) {
                Ok(function) => {
                    // function.disassemble("script");
                    state.define_builtins();
                    let closure = Gc::new(mc, Closure::new(mc, Gc::new(mc, function)));
                    state.push_stack(Value::from(closure));
                    state.call(closure, 0);
                    InterpretResult::Ok
                }
                Err(err) => {
                    println!("Compile error, {:?}", err);
                    InterpretResult::CompileError
                }
            };
        });

        loop {
            const FUEL_PER_GC: i32 = 4096;
            let mut fuel = Fuel::new(FUEL_PER_GC);
            // periodically exit the arena in order to collect garbage concurrently with running the VM.
            let result = self.arena.mutate_root(|_, state| state.step(&mut fuel));

            const COLLECTOR_GRANULARITY: f64 = 1024.0;
            if self.arena.metrics().allocation_debt() > COLLECTOR_GRANULARITY {
                // Do garbage collection.
                #[cfg(feature = "debug")]
                println!("Collecting...");
                if self.arena.collection_phase() == CollectionPhase::Collecting {
                    self.arena.collect_debt();
                } else {
                    // Immediately transition to `CollectionPhase::Collecting`.
                    self.arena.mark_all().unwrap().start_collecting();
                }
            }

            match result {
                Ok(finished) => {
                    if finished {
                        break;
                    }
                }
                Err(err) => return err,
            }
        }

        InterpretResult::Ok
    }
}

impl<'gc> State<'gc> {
    fn current_frame(&mut self) -> &mut CallFrame<'gc> {
        &mut self.frames[self.frame_count - 1]
    }

    // Runs the VM for a period of time controlled by the `fuel` parameter.
    //
    // Returns `Ok(false)` if the method has exhausted its fuel, but there is more work to
    // do, and returns `Ok(true)` if no more progress can be made.
    fn step(&mut self, fuel: &mut Fuel) -> Result<bool, InterpretResult> {
        loop {
            // Debug stack info
            #[cfg(feature = "debug")]
            self.print_stack();
            let frame = self.current_frame();
            // Disassemble instruction for debug
            #[cfg(feature = "debug")]
            frame.disassemble_instruction(frame.ip);
            match OpCode::try_from(frame.read_byte()).unwrap() {
                OpCode::Constant => {
                    let byte = frame.read_byte();
                    let constant = frame.read_constant(byte);
                    self.push_stack(constant);
                }
                OpCode::Add => match (self.peek(0), self.peek(1)) {
                    (Value::Number(_), Value::Number(_)) => {
                        let a = self.pop_stack().as_number().unwrap();
                        let b = self.pop_stack().as_number().unwrap();
                        self.push_stack((a + b).into());
                    }
                    (Value::String(_), Value::String(_)) => {
                        let b = self.pop_stack().as_string().unwrap();
                        let a = self.pop_stack().as_string().unwrap();
                        self.push_stack(Gc::new(self.mc, format!("{a}{b}")).into());
                    }
                    _ => {
                        return Err(InterpretResult::RuntimeError(
                            "Operands must be two numbers or two strings.".into(),
                        ))
                    }
                },
                OpCode::Subtract => {
                    let b = self.pop_stack().as_number().unwrap();
                    let a = self.pop_stack().as_number().unwrap();
                    self.push_stack((a - b).into());
                }
                OpCode::Multiply => {
                    let b = self.pop_stack().as_number().unwrap();
                    let a = self.pop_stack().as_number().unwrap();
                    self.push_stack((a * b).into());
                }
                OpCode::Divide => {
                    let b = self.pop_stack().as_number().unwrap();
                    let a = self.pop_stack().as_number().unwrap();
                    self.push_stack((a / b).into());
                }
                OpCode::Negate => {
                    let v = self.pop_stack().as_number().unwrap();
                    self.push_stack((-v).into());
                }
                OpCode::Return => {
                    let frame_slot_start = frame.slot_start;
                    let return_value = self.pop_stack();
                    self.close_upvalues(frame_slot_start);
                    // Must pop the frame from vec when returning
                    self.frames.pop();
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop_stack();
                        return Ok(true);
                    }
                    self.stack_top = frame_slot_start;
                    self.push_stack(return_value);
                }
                OpCode::Nil => self.push_stack(Value::Nil),
                OpCode::True => self.push_stack(Value::Boolean(true)),
                OpCode::False => self.push_stack(Value::Boolean(false)),
                OpCode::Not => {
                    let v = self.pop_stack().as_boolean().unwrap();
                    self.push_stack((!v).into())
                }
                OpCode::Equal => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    self.push_stack(a.equals(&b).into());
                }
                OpCode::Greater => {
                    let b = self.pop_stack().as_number().unwrap();
                    let a = self.pop_stack().as_number().unwrap();
                    self.push_stack((a > b).into());
                }
                OpCode::Less => {
                    let b = self.pop_stack().as_number().unwrap();
                    let a = self.pop_stack().as_number().unwrap();
                    self.push_stack((a < b).into());
                }
                OpCode::Print => {
                    let value = self.pop_stack();
                    println!("{value}");
                }
                OpCode::Pop => {
                    self.pop_stack();
                }
                OpCode::DefineGlobal => {
                    let byte = frame.read_byte();
                    let varible_name = frame.read_constant(byte).as_string().unwrap();
                    self.globals.insert(varible_name, *self.peek(0));
                    self.pop_stack();
                }
                OpCode::GetGlobal => {
                    let byte = frame.read_byte();
                    let varible_name = frame.read_constant(byte).as_string().unwrap();
                    if let Some(value) = self.globals.get(&varible_name) {
                        self.push_stack(*value);
                    } else {
                        return Err(InterpretResult::RuntimeError(
                            format!("Undefined variable '{}'", varible_name).into(),
                        ));
                    }
                }
                OpCode::SetGlobal => {
                    let byte = frame.read_byte();
                    let varible_name = frame.read_constant(byte).as_string().unwrap();
                    #[allow(clippy::map_entry)]
                    if self.globals.contains_key(&varible_name) {
                        self.globals.insert(varible_name, *self.peek(0));
                    } else {
                        return Err(InterpretResult::RuntimeError(
                            format!("Undefined variable '{}'", varible_name).into(),
                        ));
                    }
                }
                OpCode::GetLocal => {
                    let slot = frame.read_byte();
                    let value = self.stack[frame.slot_start + slot as usize];
                    self.push_stack(value);
                }
                OpCode::SetLocal => {
                    let slot = frame.read_byte();
                    let slot_start = frame.slot_start;
                    self.stack[slot_start + slot as usize] = *self.peek(0);
                }
                OpCode::JumpIfFalse => {
                    let is_falsy = self.peek(0).is_falsy();
                    let frame = self.current_frame();
                    // Alwasy jump to the next instruction, do not move this line into if block
                    let offset = frame.read_short();
                    if is_falsy {
                        frame.ip += offset as usize;
                    }
                }
                OpCode::Jump => {
                    let offset = frame.read_short();
                    frame.ip += offset as usize;
                }
                OpCode::Loop => {
                    let offset = frame.read_short();
                    frame.ip -= offset as usize;
                }
                OpCode::Call => {
                    let arg_count = frame.read_byte();
                    self.call_value(*self.peek(arg_count as usize), arg_count);
                }
                OpCode::Closure => {
                    // frame.disassemble();
                    let byte = frame.read_byte();
                    let function = frame.read_constant(byte).as_function().unwrap();
                    // let fn_name = function.name;
                    let mut closure = Closure::new(self.mc, function);

                    (0..closure.function.upvalue_count as usize).for_each(|i| {
                        let frame = self.current_frame();
                        let is_local = frame.read_byte();
                        let index = frame.read_byte() as usize;
                        if is_local == 1 {
                            let slot = frame.slot_start + index;
                            let upvalue = self.capture_upvalue(slot);
                            // println!("function {} capture local: {slot}, {:?}", fn_name, upvalue);
                            closure.upvalues[i] = upvalue;
                        } else {
                            // println!(
                            //     "function {} capture upvalue: {index} {:?}",
                            //     fn_name, &frame.closure.upvalues[index]
                            // );
                            closure.upvalues[i] = frame.closure.upvalues[index];
                        }
                    });

                    self.push_stack(Value::from(Gc::new(self.mc, closure)));
                }
                OpCode::GetUpvalue => {
                    let slot = frame.read_byte() as usize;
                    let upvalue = frame.closure.upvalues[slot];
                    if let Some(closed) = upvalue.borrow().closed {
                        self.push_stack(closed);
                    } else {
                        let location = frame.closure.upvalues[slot].borrow().location;
                        let upvalue = self.stack[location];
                        self.push_stack(upvalue);
                    }
                }
                OpCode::SetUpvalue => {
                    let slot = frame.read_byte() as usize;
                    let mut upvalue = frame.closure.upvalues[slot].borrow_mut(self.mc);
                    let stack_position = upvalue.location;
                    upvalue.location = slot;

                    let value = *self.peek(slot);
                    upvalue.closed = Some(value);
                    // Also update the stack value
                    self.stack[stack_position] = value;
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalues(self.stack_top - 1);
                    self.pop_stack();
                }
                OpCode::Class => {
                    let name = frame.read_string();
                    self.push_stack(Value::from(Gc::new(
                        self.mc,
                        RefLock::new(Class::new(name)),
                    )));
                }
                OpCode::GetProperty => {
                    if let Ok(instance) = self.peek(0).as_instance() {
                        let frame = self.current_frame();
                        let name = frame.read_string();
                        if let Some(property) = instance.borrow().fields.get(&name) {
                            self.pop_stack(); // Instance
                            self.push_stack(*property);
                        } else if !self.bind_method(instance.borrow().class, name) {
                            return Err(InterpretResult::RuntimeError(
                                format!("Undefined property '{}'", name).into(),
                            ));
                        }
                    } else {
                        return Err(InterpretResult::RuntimeError(
                            "Only instances have properties.".into(),
                        ));
                    }
                }
                OpCode::SetProperty => {
                    if let Ok(instantce) = self.peek(1).as_instance() {
                        let value = *self.peek(0);
                        let frame = self.current_frame();
                        let name = frame.read_string();
                        instantce.borrow_mut(self.mc).fields.insert(name, value);

                        let value = self.pop_stack(); // Value
                        self.pop_stack(); // Instance
                        self.push_stack(value);
                    } else {
                        return Err(InterpretResult::RuntimeError(
                            "Only instances have fields.".into(),
                        ));
                    }
                }
                OpCode::Method => {
                    let name = frame.read_string();
                    self.define_method(name);
                }
                OpCode::Invoke => {
                    let method_name = frame.read_string();
                    let arg_count = frame.read_byte();
                    self.invoke(method_name, arg_count);
                }
                OpCode::Unknown => {
                    return Err(InterpretResult::RuntimeError("Unknown opcode".into()))
                }
            }

            const FUEL_PER_STEP: i32 = 4;
            fuel.consume(FUEL_PER_STEP);

            if !fuel.should_continue() {
                return Ok(false);
            }
        }
    }

    fn capture_upvalue(&mut self, slot: usize) -> GcRefLock<'gc, UpvalueObj<'gc>> {
        let mut prev_upvalue = None;
        let mut open_upvalue = self.open_upvalues;
        while open_upvalue.map(|u| u.borrow().location > slot) == Some(true) {
            if let Some(upvalue) = open_upvalue {
                prev_upvalue = Some(upvalue);
                open_upvalue = upvalue.borrow().next;
            }
        }
        if let Some(upvalue) = open_upvalue {
            if upvalue.borrow().location == slot {
                // We found an existing upvalue capturing the variable,
                // so we reuse that upvalue.
                return upvalue;
            }
        }

        // Do not use peek() to get value! the slot would be incorrect offset to peek.
        // let local = &self.stack[slot].clone();
        // create a new upvalue for our local slot and insert it into the list at the right location.
        let created_upvalue = Gc::new(
            self.mc,
            RefLock::new(UpvalueObj {
                location: slot,
                closed: None,
                next: open_upvalue,
            }),
        );
        if let Some(prev) = prev_upvalue {
            prev.borrow_mut(self.mc).next = Some(created_upvalue);
        } else {
            self.open_upvalues = Some(created_upvalue);
        }
        created_upvalue
    }

    fn close_upvalues(&mut self, last: usize) {
        while let Some(upvalue) = self.open_upvalues.take() {
            let mut upvalue = upvalue.borrow_mut(self.mc);
            if upvalue.location < last {
                break;
            }
            let local = self.stack[upvalue.location];
            upvalue.closed = Some(local);
            // Dummy location after closed assigned
            // In C's version, the location is a pointer to upvalue.closed
            // upvalue.location = 0;
            self.open_upvalues = upvalue.next;
        }
    }

    fn define_method(&mut self, name: Gc<'gc, String>) {
        let class = self.peek(1).as_class().unwrap();
        class
            .borrow_mut(self.mc)
            .methods
            .insert(name, *self.peek(0));
        // pop the closure since we’re done with it.
        self.pop_stack();
    }

    pub fn define_builtins(&mut self) {
        self.define_native_function("clock", builtins::clock);
    }

    fn define_native_function(&mut self, name: &str, function: NativeFn<'gc>) {
        self.globals.insert(
            Gc::new(self.mc, name.to_owned()),
            Value::NativeFunction(function),
        );
    }

    fn bind_method(&mut self, class: GcRefLock<'gc, Class<'gc>>, name: Gc<'gc, String>) -> bool {
        if let Some(method) = class.borrow().methods.get(&name) {
            let bound = BoundMethod::new(*self.peek(0), (*method).as_closure().unwrap());
            // pop the instance and replace the top of
            // the stack with the bound method.
            self.pop_stack();
            self.push_stack(Value::from(Gc::new(self.mc, bound)));
            true
        } else {
            panic!("Undefined property '{}'", name);
        }
    }

    fn call_value(&mut self, callee: Value<'gc>, arg_count: u8) {
        match callee {
            Value::BoundMethod(bound) => {
                // inserts the receiver into the new CallFrame's slot zero.
                // normally, the receiver is 'this' keyword.
                /*
                   Diagram for this: scone.topping("berries", "cream");

                                                   stackTop
                                                       |
                    <-- -1 --> <------ argCount ---->  |
                       0         1         2         3 v
                       |         |         |         |
                       v         v         v         v
                   +----------+-----------+-----------+---
                   | script   |fn topping()| "berries" | "cream"
                   +----------+-----------+-----------+---
                       ^                               ^
                       |                               |
                       +-------------------------------+
                           topping Callframe
                */
                self.stack[self.stack_top - arg_count as usize - 1] = bound.receiver;
                self.call(bound.method, arg_count)
            }
            Value::Class(class) => {
                let instance = Instance::new(class);
                self.stack[self.stack_top - arg_count as usize - 1] =
                    Value::from(Gc::new(self.mc, RefLock::new(instance)));
                // FIXME: interne init string
                let init = Gc::new(self.mc, "init".to_owned());
                if let Some(initializer) = class.borrow().methods.get(&init) {
                    self.call(initializer.as_closure().unwrap(), arg_count);
                } else if arg_count != 0 {
                    panic!("Expected 0 arguments but got {}", arg_count);
                }
            }
            Value::Function(_) => {} //self.call(*function, arg_count),
            Value::Closure(closure) => self.call(closure, arg_count),
            Value::NativeFunction(function) => {
                let result = function(self.pop_stack_n(arg_count as usize));
                // Stack should be restored after native function called
                self.stack_top -= 1;
                self.push_stack(result);
            }
            _ => {
                panic!("Can only call functions and classes");
            }
        }
    }

    fn invoke_from_class(
        &mut self,
        class: GcRefLock<'gc, Class<'gc>>,
        name: Gc<'gc, String>,
        arg_count: u8,
    ) {
        if let Some(method) = class.borrow().methods.get(&name) {
            self.call(method.as_closure().unwrap(), arg_count);
        } else {
            panic!("Undefined property '{}'", name);
        }
    }

    fn invoke(&mut self, name: Gc<'gc, String>, arg_count: u8) {
        let receiver = self.peek(arg_count as usize);
        if let Value::Instance(instance) = receiver {
            if let Some(value) = instance.borrow().fields.get(&name) {
                self.stack[self.stack_top - arg_count as usize - 1] = *value;
                return self.call_value(*value, arg_count);
            }
            self.invoke_from_class(instance.borrow().class, name, arg_count);
        } else {
            panic!("Only instances have methods");
        }
    }

    fn call(&mut self, closure: Gc<'gc, Closure<'gc>>, arg_count: u8) {
        // closure.function.disassemble("fn");
        if arg_count != closure.function.arity {
            panic!(
                "Expected {} arguments but got {}",
                closure.function.arity, arg_count
            );
        }
        if self.frame_count == FRAME_MAX_SIZE {
            panic!("Too many active frames");
        }

        let call_frame = CallFrame {
            closure,
            ip: 0,
            slot_start: self.stack_top - arg_count as usize - 1,
        };
        // self.frames[self.frame_count] = call_frame;
        self.frames.push(call_frame);
        self.frame_count += 1;
    }

    fn push_stack(&mut self, value: Value<'gc>) {
        if self.stack_top < STACK_MAX_SIZE {
            self.stack[self.stack_top] = value;
            self.stack_top += 1;
        } else {
            panic!("Stack overflow");
        }
    }

    fn pop_stack(&mut self) -> Value<'gc> {
        if self.stack_top > 0 {
            self.stack_top -= 1;
            self.stack[self.stack_top]
        } else {
            // panic!("Stack underflow")
            Value::Nil
        }
    }

    fn pop_stack_n(&mut self, n: usize) -> Vec<Value<'gc>> {
        if n == 0 {
            return Vec::new();
        }

        // Ensure we don't pop more items than are on the stack
        let n = n.min(self.stack_top);

        let new_top = self.stack_top - n;
        let mut result = Vec::with_capacity(n);

        // Copy values from the stack to the result vector
        result.extend_from_slice(&self.stack[new_top..self.stack_top]);

        // Update the stack top
        self.stack_top = new_top;

        // No need to reverse as we're copying from bottom to top
        result
    }

    fn peek(&self, distance: usize) -> &Value<'gc> {
        &self.stack[self.stack_top - 1 - distance]
    }

    #[allow(unused)]
    fn print_stack(&self) {
        print!("          ");
        for value in self.stack.iter().take(self.stack_top) {
            print!("[ ");
            print!("{value}");
            print!(" ]")
        }
        println!();
    }
}
