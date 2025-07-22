#!/bin/bash

project_dir=$(git rev-parse --show-toplevel)
sysy_base="$project_dir/testing/test-functional"
sysy_srcs="$sysy_base/sysy"

output_base="$project_dir/target/test-functional"
ir_output="$output_base/ir"
asm_output="$output_base/asm"
log_output="$output_base/logs"

export RUST_BACKTRACE=1
export RUST_LOG=debug

rm -rf "$ir_output" "$asm_output" "$log_output"

mkdir -p "$sysy_srcs"
mkdir -p "$ir_output"
mkdir -p "$asm_output"
mkdir -p "$log_output"

# cargo build --release
# remusys_bin="$project_dir/target/release/remusys-compiler"

cargo build
remusys_bin="$project_dir/target/debug/remusys-compiler"

function process_one_source() {
    local src="$1"
    local asm_file="$asm_output/$(basename "$src" .sy).s"
    local ir_file="$(basename "$src" .sy).ll"
    local log_file="$log_output/$(basename "$src" .sy).log"

    "$remusys_bin" "$src" --emit-ir -S -o "$asm_file" > "$log_file" 2>&1

    if [ $? -ne 0 ]; then
        mv "$sysy_srcs/$ir_file" "$ir_output/$ir_file"
        echo "Compilation failed for $(basename "$src"). Check log at $(basename "$log_file")"
        echo "========== [ Log output ] =========="
        cat "$log_file"
        echo "===================================="
        exit 1
    elif grep -q "panicked" "$log_file"; then
        mv "$sysy_srcs/$ir_file" "$ir_output/$ir_file"
        echo "Compilation panicked for $(basename "$src"). Check log at $log_file"
        echo "========== [ Log output ] =========="
        cat "$log_file"
        echo "===================================="
        echo "ended log output for $(basename "$src")"
        exit 1
    else
        mv "$sysy_srcs/$ir_file" "$ir_output/$ir_file"
        echo "Compiled $(basename "$src") successfully to $(basename "$asm_file")".
    fi
}

# for src in "$sysy_srcs"/*.sy; do
#     process_one_source "$src"
# done

process_one_source "$sysy_srcs/96_matrix_add.sy"

# 当命令行有选项 --asm 时，编译生成的汇编文件
for params in "$@"; do
    if [[ "$params" == "--asm" ]]; then
        compile_asm=true
        break
    fi
done

if [ -z "$compile_asm" ]; then
    echo "Skipping assembly compilation. Use --asm to compile assembly files."
    exit 0
fi

for asms in "$asm_output"/*.s; do
    if ! [ -f "$asms" ]; then
        continue
    fi
    echo "Assembly file: $(basename "$asms")"
    if ! grep -q "main" "$asms"; then
        echo "Warning: No 'main' function found in $(basename "$asms")"
    fi
    clang --target=aarch64-unknown-linux-gnu -c "$asms" -o "${asms%.s}.o" || {
        echo "Error compiling assembly file $(basename "$asms")"
        exit 1
    }
    echo "Compiled $(basename "$asms") to object file $(basename "${asms%.s}.o")"
done