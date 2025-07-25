#!/bin/bash

project_dir=$(git rev-parse --show-toplevel)
sysy_base="$project_dir/testing/test-functional"
exe_test_input="$sysy_base/in"

output_base="$project_dir/target/test-functional"
exe_output="$output_base/exe"
exe_test_output="$output_base/exe-testout"

export RUST_BACKTRACE=1
export RUST_LOG=debug

# 测试所有编译产生的可执行文件是否能正确运行
for exe in "$exe_output"/*.elf; do
    exe_basename=$(basename "$exe" .elf)
    test_input="$exe_test_input/$exe_basename.in"
    test_output="$exe_test_output/$exe_basename.out"

    if [ -f "$test_input" ]; then
        echo "Running $exe with input from $test_input"
        "$exe" < "$test_input" > "$test_output" 2>&1
        exit_code=$?
    else
        echo "No input file for $exe_basename, running without input"
        "$exe" > "$test_output" 2>&1
        exit_code=$?
    fi

    echo "$exit_code" >> "$test_output"
done
