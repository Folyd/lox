use std::{array, borrow::Cow, cell::RefCell, rc::Rc};

use ustr::UstrMap;

use crate::{
    builtins,
    compiler::compile,
    object::{Closure, NativeFn, UpvalueObj},
    value::intern_str,
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

pub struct Vm {
    frames: [CallFrame; FRAME_MAX_SIZE],
    frame_count: usize,
    stack: [Value; STACK_MAX_SIZE],
    stack_top: usize,
    globals: UstrMap<Value>,
    open_upvalues: Option<Rc<RefCell<UpvalueObj>>>,
}

#[derive(Clone, Default)]
pub struct CallFrame {
    // pub function: Function,
    pub closure: Closure,
    // When we return from a function, the VM will
    // jump to the ip of the caller’s CallFrame and resume from there.
    pub ip: usize,
    // slot_start field points into the VM’s value stack
    // at the first slot that this function can use
    pub slot_start: usize,
}

impl std::fmt::Debug for CallFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CallFrame")
            .field("function", &self.closure.function.name)
            .field("ip", &self.ip)
            .field("slot_start", &self.slot_start)
            .finish()
    }
}

impl CallFrame {
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

    fn read_constant(&mut self, byte: u8) -> Value {
        self.closure.function.read_constant(byte)
    }

    #[allow(unused)]
    fn disassemble(&self) {
        self.closure
            .function
            .chunk
            .disassemble(&self.closure.function.name);
    }

    #[allow(unused)]
    fn disassemble_instruction(&self, offset: usize) {
        self.closure.function.chunk.disassemble_instruction(offset);
    }
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            frames: array::from_fn(|_| CallFrame::default()),
            frame_count: 0,
            stack: array::from_fn(|_| Value::Nil),
            stack_top: 0,
            globals: UstrMap::default(),
            open_upvalues: None,
        }
    }

    pub fn define_builtins(&mut self) {
        self.define_native_function("clock", builtins::clock);
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        match compile(source) {
            Ok(function) => {
                // function.disassemble("script");
                let closure = Closure::new(function);
                self.push_stack(Value::from(closure.clone()));
                self.call(closure, 0);
                self.run()
            }
            Err(err) => {
                println!("Compile error, {:?}", err);
                InterpretResult::CompileError
            }
        }
    }

    fn current_frame(&mut self) -> &mut CallFrame {
        &mut self.frames[self.frame_count - 1]
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            // Debug stack info
            // self.print_stack();
            let frame = self.current_frame();
            // Disassemble instruction for debug
            // frame.disassemble_instruction(frame.ip);
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
                        self.push_stack(format!("{a}{b}").into());
                    }
                    _ => {
                        return InterpretResult::RuntimeError(
                            "Operands must be two numbers or two strings.".into(),
                        )
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
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop_stack();
                        return InterpretResult::Ok;
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
                    self.globals.insert(varible_name, self.peek(0).clone());
                    self.pop_stack();
                }
                OpCode::GetGlobal => {
                    let byte = frame.read_byte();
                    let varible_name = frame.read_constant(byte).as_string().unwrap();
                    if let Some(value) = self.globals.get(&varible_name) {
                        self.push_stack(value.clone());
                    } else {
                        return InterpretResult::RuntimeError(
                            format!("Undefined variable '{}'", varible_name).into(),
                        );
                    }
                }
                OpCode::SetGlobal => {
                    let byte = frame.read_byte();
                    let varible_name = frame.read_constant(byte).as_string().unwrap();
                    #[allow(clippy::map_entry)]
                    if self.globals.contains_key(&varible_name) {
                        self.globals.insert(varible_name, self.peek(0).clone());
                    } else {
                        return InterpretResult::RuntimeError(
                            format!("Undefined variable '{}'", varible_name).into(),
                        );
                    }
                }
                OpCode::GetLocal => {
                    let slot = frame.read_byte();
                    let value = self.stack[frame.slot_start + slot as usize].clone();
                    self.push_stack(value);
                }
                OpCode::SetLocal => {
                    let slot = frame.read_byte();
                    let slot_start = frame.slot_start;
                    self.stack[slot_start + slot as usize] = self.peek(0).clone();
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
                    self.call_value(self.peek(arg_count as usize).clone(), arg_count);
                }
                OpCode::Closure => {
                    // frame.disassemble();
                    let byte = frame.read_byte();
                    let function = frame.read_constant(byte).as_function().unwrap();
                    // let fn_name = function.name;
                    let mut closure = Closure::new(function);

                    (0..closure.function.upvalue_count as usize).for_each(|i: usize| {
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
                            closure.upvalues[i] = Rc::clone(&frame.closure.upvalues[index]);
                        }
                    });

                    self.push_stack(Value::from(closure));
                }
                OpCode::GetUpvalue => {
                    let slot = frame.read_byte() as usize;
                    let upvalue = (*frame.closure.upvalues[slot]).clone().into_inner();
                    if let Some(closed) = upvalue.closed.as_ref() {
                        self.push_stack(closed.clone());
                    } else {
                        let location = frame.closure.upvalues[slot].borrow().location;
                        let upvalue = self.stack[location].clone();
                        self.push_stack(upvalue);
                    }
                }
                OpCode::SetUpvalue => {
                    // let frame = self.current_frame();
                    let value = self.peek(0).clone();
                    let location = {
                        let frame = self.current_frame();
                        let slot = frame.read_byte() as usize;
                        // let value = self.peek(slot).clone();
                        // let frame = self.current_frame();
                        // frame.closure.upvalues[slot].borrow_mut().value = value;
                        let mut upvalue = frame.closure.upvalues[slot].borrow_mut();
                        let location = upvalue.location;
                        upvalue.location = slot;
                        upvalue.closed = Some(value.clone());
                        location
                    };
                    // Also update the stack value
                    self.stack[location] = value;
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalues(self.stack_top - 1);
                    self.pop_stack();
                }
                OpCode::Unknown => return InterpretResult::RuntimeError("Unknown opcode".into()),
            }
        }
    }
}

impl Vm {
    fn capture_upvalue(&mut self, slot: usize) -> Rc<RefCell<UpvalueObj>> {
        let mut prev_upvalue = None;
        let mut open_upvalue = self.open_upvalues.as_ref().map(Rc::clone);
        while open_upvalue.as_ref().map(|u| u.borrow().location > slot) == Some(true) {
            if let Some(upvalue) = open_upvalue.as_ref().map(Rc::clone) {
                prev_upvalue = Some(Rc::clone(&upvalue));
                open_upvalue = upvalue.borrow().next.as_ref().map(Rc::clone);
                //     open_upvalue = Some(next);
                // } else {
                //     break;
                // }
            }
        }
        if let Some(upvalue) = open_upvalue.as_ref() {
            if upvalue.borrow().location == slot {
                return Rc::clone(upvalue);
            }
        }
        // while let Some(upvalue) = open_upvalue.as_ref().map(Rc::clone) {
        //     // println!("capture upvalue: {:?}", upvalue);

        //     // if upvalue.borrow().value.equals(local) {
        //     if upvalue.borrow().location <= slot {
        //         // We found an existing upvalue capturing the variable,
        //         // so we reuse that upvalue.
        //         return Rc::clone(&upvalue);
        //     }

        //     prev_upvalue = Some(Rc::clone(&upvalue));
        //     if let Some(next) = upvalue.borrow().next.as_ref().map(Rc::clone) {
        //         open_upvalue = Some(next);
        //     } else {
        //         break;
        //     }
        // }

        // Do not use peek() to get value! the slot would be incorrect offset to peek.
        // let local = &self.stack[slot].clone();
        // create a new upvalue for our local slot and insert it into the list at the right location.
        let created_upvalue = Rc::new(RefCell::new(UpvalueObj::new(slot)));
        created_upvalue.borrow_mut().next = open_upvalue;
        if let Some(prev) = prev_upvalue {
            prev.borrow_mut().next = Some(Rc::clone(&created_upvalue));
        } else {
            self.open_upvalues = Some(Rc::clone(&created_upvalue));
        }
        created_upvalue
    }

    fn close_upvalues(&mut self, last: usize) {
        // let mut open_upvalue = self.open_upvalues.as_ref().map(Rc::clone);
        while let Some(upvalue) = self.open_upvalues.take() {
            // .map(|u| u.borrow().location >= last) == Some(true) {
            // let i = inner.borrow();
            let mut upvalue = upvalue.borrow_mut();
            // let location = upvalue.borrow().location;
            // let next = i.next;
            if upvalue.location < last {
                break;
                // *inner = inner.next.as_ref().map(Rc::clone);
                // *inner = inner
                //     .borrow()
                //     .next
                //     .as_ref()
                //     .map(Rc::clone)
                //     .unwrap_or_default();
                // if let Some(n) = inner.borrow().next.as_ref() {
                // *inner = (*n).clone().into_inner();
                // }
            }
            // let upvalue = self.open_upvalues
            let local = self.stack[upvalue.location].clone();
            upvalue.closed = Some(local);
            // Dummy location after closed assigned
            // In C's version, the location is a pointer to upvalue.closed
            // upvalue.location = 0;
            self.open_upvalues = upvalue.next.clone();
            // let _ = mem::replace(upvalue, n);
            // if let Some(upvalue) = self.open_upvalue.as_ref().map(Rc::clone) {
            //     open_upvalue = upvalue.borrow().next.as_ref().map(Rc::clone);
            //     //     open_upvalue = Some(next);
            //     // } else {
            //     //     break;
            //     // }
            // }
        }
    }

    fn define_native_function(&mut self, name: &str, function: NativeFn) {
        self.globals
            .insert(intern_str(name), Value::NativeFunction(function));
    }

    fn call_value(&mut self, callee: Value, arg_count: u8) {
        match callee {
            Value::Function(_) => {} //self.call(*function, arg_count),
            Value::Closure(closure) => self.call(*closure, arg_count),
            Value::NativeFunction(function) => {
                let result = function(self.pop_stack_n(arg_count as usize));
                self.push_stack(result);
            }
            _ => {
                panic!("Can only call functions and classes");
            }
        }
    }

    fn call(&mut self, closure: Closure, arg_count: u8) {
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
        self.frames[self.frame_count] = call_frame;
        self.frame_count += 1;
    }

    fn push_stack(&mut self, value: Value) {
        if self.stack_top < STACK_MAX_SIZE {
            self.stack[self.stack_top] = value;
            self.stack_top += 1;
        } else {
            panic!("Stack overflow");
        }
    }

    fn pop_stack(&mut self) -> Value {
        if self.stack_top > 0 {
            self.stack_top -= 1;
            self.stack[self.stack_top].clone()
        } else {
            // panic!("Stack underflow")
            Value::Nil
        }
    }

    fn pop_stack_n(&mut self, n: usize) -> Vec<Value> {
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

    fn peek(&self, distance: usize) -> &Value {
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
