#!/bin/bash

project_dir=$(git rev-parse --show-toplevel)
sysy_base="$project_dir/testing/test-functional"
sysy_srcs="$sysy_base/sysy"
exe_test_input="$sysy_base/in"

output_base="$project_dir/target/test-functional"
ir_output="$output_base/ir"
exe_output="$output_base/exe"
log_output="$output_base/logs"
exe_test_output="$output_base/exe-testout"

export RUST_BACKTRACE=1
export RUST_LOG=debug

rm -rf "$ir_output" "$log_output"

mkdir -p "$sysy_srcs"
mkdir -p "$ir_output"
mkdir -p "$log_output"
mkdir -p "$exe_output"
mkdir -p "$exe_test_output"

cargo build --release
remusys_bin="$project_dir/target/release/remusys-compiler"

# cargo build
# remusys_bin="$project_dir/target/debug/remusys-compiler"

function prepare_sylib() {
    sylib_src="$sysy_base/sylib"
    sylib_output="$output_base/sylib"
    rm -rf "$sylib_output"
    mkdir -p "$sylib_output"
    clang -c "$sylib_src/sylib.c" -o "$sylib_output/sylib.o" -O2 || {
        echo "Failed to compile sylib.c"
        exit 1
    }
    echo "Compiled sylib.c successfully to $sylib_output/sylib.o"

    sylib_file="$sylib_output/sylib.o"
}

function process_one_sysy_source() {
    local src="$1"
    local ir_file="$(basename "$src" .sy).ll"
    local log_file="$log_output/$(basename "$src" .sy).log"

    "$remusys_bin" "$src" --emit-ir -o "$ir_output/$ir_file" > "$log_file" 2>&1

    if [ $? -ne 0 ]; then
        echo "Compilation failed for $(basename "$src"). Check log at $(basename "$log_file")"
        echo "========== [ Log output ] =========="
        cat "$log_file"
        echo "===================================="
        exit 1
    elif grep -q "panicked" "$log_file"; then
        echo "Compilation panicked for $(basename "$src"). Check log at $log_file"
        echo "========== [ Log output ] =========="
        cat "$log_file"
        echo "===================================="
        exit 1
    else
        echo "Compiled $(basename "$src") successfully to $(basename "$ir_file")".
    fi
}

prepare_sylib

# 测试该编译器是否能正确处理 sysy 语言的源代码并生成正确的 IR 文件
for src in "$sysy_srcs"/*.sy; do
    process_one_sysy_source "$src"
    ir_file="$(basename "$src" .sy).ll"
    log_file="$log_output/$(basename "$src" .sy).log"
    clang -c "$ir_output/$ir_file" -o "$ir_output/${ir_file%.ll}.o" || {
        echo "Failed to generate object file for $(basename "$src"). Check log at $(basename "$log_file")"
        exit 1
    }
    echo "Generated object file for $(basename "$src") at $(basename "$ir_output/${ir_file%.ll}.o")"

    # 然后把两个 .o 文件链接成一个可执行文件
    output_exe="$exe_output/$(basename "$src" .sy).elf"
    clang --target=x86_64-pc-linux-gnu "$ir_output/${ir_file%.ll}.o" "$sylib_file" -o "$output_exe" || {
        echo "Failed to link object file for $(basename "$src"). Check log at $(basename "$log_file")"
        exit 1
    }
    echo "Linked object file for $(basename "$src") to executable $output_exe"
done

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
