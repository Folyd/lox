#!/bin/python3

import argparse
import pathlib
import subprocess
import time

lang_list = {
    "lox": {"interpreter": "target/release/lox-lang", "ext": "lox"},
    "python": {"interpreter": "python3", "ext": "py"},
    "perl": {"interpreter": "perl", "ext": "pl"},
}
BENCHMARK_DIR = "tests/benchmarks/{lang}"


parser = argparse.ArgumentParser(description="Run benchmarks")
parser.add_argument(
    "lang",
    metavar="lang",
    type=str,
)
args = parser.parse_args()

lang = args.lang
interpreter = lang_list[lang]["interpreter"]
ext = lang_list[lang]["ext"]

print(f"Running benchmarks for {lang}...", BENCHMARK_DIR.format(lang=lang))
for benchmark in sorted(pathlib.Path(BENCHMARK_DIR.format(lang=lang)).glob(f"*.{ext}")):
    times = []
    for i in range(5):
        output = subprocess.check_output([interpreter, str(benchmark.resolve())])
        times.append(float(output.splitlines()[-1]))
        time.sleep(2)
    print(f"{benchmark.name}: {round(min(times), 4)}")
