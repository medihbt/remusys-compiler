#!/bin/bash

project_dir=$(git rev-parse --show-toplevel)
sysy_base="$project_dir/testing/test-functional"
sysy_srcs="$sysy_base/sysy"

output_base="$project_dir/target/test-functional"
ir_output="$output_base/ir"
log_output="$output_base/logs"

export RUST_BACKTRACE=1
export RUST_LOG=debug

rm -rf "$ir_output" "$log_output"

mkdir -p "$sysy_srcs"
mkdir -p "$ir_output"
mkdir -p "$log_output"

cargo build --release
remusys_bin="$project_dir/target/release/remusys-compiler"

# cargo build
# remusys_bin="$project_dir/target/debug/remusys-compiler"

function process_one_source() {
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

for src in "$sysy_srcs"/*.sy; do
    process_one_source "$src"
    ir_file="$(basename "$src" .sy).ll"
    log_file="$log_output/$(basename "$src" .sy).log"
    llc-19 -filetype=obj "$ir_output/$ir_file" -o "$ir_output/${ir_file%.ll}.o" || {
        echo "Failed to generate object file for $(basename "$src"). Check log at $(basename "$log_file")"
        exit 1
    }
    echo "Generated object file for $(basename "$src") at $(basename "$ir_output/${ir_file%.ll}.o")"
done