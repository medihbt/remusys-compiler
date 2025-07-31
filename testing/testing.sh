#!/bin/bash

project_dir=$(git rev-parse --show-toplevel)
sysy_base="$project_dir/testing/test-functional"
sysy_srcs="$sysy_base/sysy"
exe_test_input="$sysy_base/in"

output_base="$project_dir/target/test-functional"
ir_output="$output_base/ir"
asm_output="$output_base/asm"
exe_output="$output_base/exe"
log_output="$output_base/logs"
exe_test_output="$output_base/exe-testout"

export RUST_BACKTRACE=1
export RUST_LOG=debug

debug_build=0

rm -rf "$ir_output" "$asm_output" "$log_output"

mkdir -p "$sysy_srcs"
mkdir -p "$ir_output"
mkdir -p "$asm_output"
mkdir -p "$log_output"
mkdir -p "$exe_output"
mkdir -p "$exe_test_output"

function build_project() {
    local debug_build="$1"

    if [ -z "$debug_build" ]; then
        cargo build
        remusys_bin="$project_dir/target/release/remusys-compiler" || {
            echo "Failed to build the project in release mode. Please check the errors above."
            exit 1
        }
    else
        cargo build --release || {
            echo "Failed to build the project. Please check the errors above."
            exit 1
        }
        remusys_bin="$project_dir/target/debug/remusys-compiler"
    fi
}

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
        tail -n 30 "$log_file"
        echo "===================================="
        exit 1
    elif grep -q "panicked" "$log_file"; then
        mv "$sysy_srcs/$ir_file" "$ir_output/$ir_file"
        echo "Compilation panicked for $(basename "$src"). Check log at $log_file"
        echo "========== [ Log output ] =========="
        tail -n 30 "$log_file"
        echo "===================================="
        echo "ended log output for $(basename "$src")"
        exit 1
    else
        mv "$sysy_srcs/$ir_file" "$ir_output/$ir_file"
        echo "Compiled $(basename "$src") successfully to $(basename "$asm_file")".
    fi
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --asm)
            compile_asm=1
            shift
            ;;
        --debug)
            debug_build=1
            shift
            ;;
        --release)
            debug_build=0
            shift
            ;;
        --test-single)
            if [[ -n "$2" ]]; then
                build_one="$2"
                shift 2
            else
                echo "Error: --test-single requires a filename argument"
                exit 1
            fi
            ;;
        --help|-h)
            echo "Remusys Compiler Testing Script"
            echo "Usage: $0 [options]"
            echo "Options:"
            echo "  --asm      Compile assembly files"
            echo "  --debug    Build in debug mode"
            echo "  --release  Build in release mode"
            echo "  --test-single <file>  Test a single source file"
            echo "  --help     Show this help message"
            exit 0
            ;;
        --*)
            echo "Unknown option: $1"
            exit 1
            ;;
        *)
            echo "Unknown argument: $1"
            exit 1
            ;;
    esac
done

build_project "$debug_build"

if [ -z "$build_one" ]; then
    for src in "$sysy_srcs"/*.sy; do
        process_one_source "$src"
    done
else
    process_one_source "$sysy_srcs/$build_one.sy"
fi


if [ -z "$compile_asm" ]; then
    echo "Skipping assembly compilation. Use --asm to compile assembly files."
    exit 0
fi

function prepare_sylib() {
    sylib_src="$sysy_base/sylib"
    sylib_output="$output_base/sylib"
    rm -rf "$sylib_output"
    mkdir -p "$sylib_output"
    clang --target=aarch64-unknown-linux-gnu -c "$sylib_src/sylib.c" -o "$sylib_output/sylib.o" -O2 || {
        echo "Failed to compile sylib.c"
        exit 1
    }
    echo "Compiled sylib.c successfully to $sylib_output/sylib.o"

    sylib_file="$sylib_output/sylib.o"
}

prepare_sylib

for asm_file in "$asm_output"/*.s; do
    if ! [ -f "$asm_file" ]; then
        continue
    fi
    asm_basename=$(basename "$asm_file" .s)

    echo "Assembly file: $asm_basename.s"
    if ! grep -q "main" "$asm_file"; then
        echo "Warning: No 'main' function found in $(basename "$asm_file")"
    fi

    object_file="$asm_output/$asm_basename.o"
    # 使用 clang 汇编，添加更多 AArch64 特定选项
    clang --target=aarch64-unknown-linux-gnu \
          -c "$asm_file" \
          -o "$object_file" -fsanitize=address || {
        echo "Error compiling assembly file $asm_basename.s"
        exit 1
    }
    echo "Compiled $asm_basename.s to object file $(basename "$object_file")"

    exe_name="$asm_basename.elf"
    output_exe="$exe_output/$asm_basename.elf"
    # 链接为 AArch64 可执行文件，添加必要的链接选项
    clang --target=aarch64-unknown-linux-gnu \
          "$object_file" "$sylib_file" \
          -o "$output_exe" \
          -fsanitize=address || {
        echo "Error linking object file $asm_basename.o to executable $exe_name"
        exit 1
    }
    echo "Linked $(basename "$object_file") to executable $(basename "$output_exe")"
done