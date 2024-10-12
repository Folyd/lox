#!/bin/python3

import argparse
import pathlib
import subprocess
import time

lang_list = {
    "lox": {"interpreter": "target/release/lox-lang", "dir": "lox", "ext": "lox"},
    "clox": {"interpreter": "clox", "dir": "lox", "ext": "lox"},
    "python": {"interpreter": "python3", "dir": "python", "ext": "py"},
    "perl": {"interpreter": "perl", "dir": "perl", "ext": "pl"},
}
BENCHMARK_DIR = "tests/benchmarks/{dir}"


parser = argparse.ArgumentParser(description="Run benchmarks")
parser.add_argument(
    "lang",
    metavar="lang",
    type=str,
)
args = parser.parse_args()

lang = args.lang
interpreter = lang_list[lang]["interpreter"]
dir = lang_list[lang]["dir"]
ext = lang_list[lang]["ext"]

print(f"Running benchmarks for {lang}...", BENCHMARK_DIR.format(dir=dir))
for benchmark in sorted(pathlib.Path(BENCHMARK_DIR.format(dir=dir)).glob(f"*.{ext}")):
    times = []
    for i in range(5):
        output = subprocess.check_output([interpreter, str(benchmark.resolve())])
        times.append(float(output.splitlines()[-1]))
        time.sleep(2)
    print(f"{benchmark.name}: {round(min(times), 4)}")
