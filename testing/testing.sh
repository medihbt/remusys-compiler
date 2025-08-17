#!/bin/bash

default_srcdir="test-functional"

function init_dirs() {
    project_dir=$(git rev-parse --show-toplevel)
    sysy_base="$project_dir/testing/$default_srcdir"
    sysy_srcs="$sysy_base/sysy"
    exe_test_input="$sysy_base/in"

    output_base="$project_dir/target/$default_srcdir"
    ir_output="$output_base/ir"
    asm_output="$output_base/asm"
    exe_output="$output_base/exe"
    log_output="$output_base/logs"
    exe_test_output="$output_base/exe-testout"

    rm -rf "$ir_output" "$asm_output" "$log_output" "$exe_output" "$exe_test_output" "$ir_output"

    mkdir -p "$sysy_srcs"
    mkdir -p "$ir_output"
    mkdir -p "$asm_output"
    mkdir -p "$log_output"
    mkdir -p "$exe_output"
    mkdir -p "$exe_test_output"
}

export RUST_BACKTRACE=1

debug_build=0

function build_project() {
    local debug_build="$1"

    if [ "$debug_build" == 0 ]; then
        echo "building at release mode"
        cargo build --release || {
            echo "Failed to build the project in release mode. Please check the errors above."
            exit 1
        }
        remusys_bin="$project_dir/target/release/compiler"
    else
        echo "building at debug mode"
        cargo build || {
            echo "Failed to build the project. Please check the errors above."
            exit 1
        }
        remusys_bin="$project_dir/target/debug/compiler"
    fi
}

function process_one_source() {
    local src="$1"
    local src_basename=$(basename "$src" .sy)
    local asm_file="$asm_output/$src_basename.s"
    local ir_file="$(basename "$src" .sy).ll"
    local log_file="$log_output/$src_basename.log"

    "$remusys_bin" "$src" --emit-mir --emit-ir -S -O1 -o "$asm_file" > "$log_file" 2>&1

    if [ $? -ne 0 ]; then
        mv "$sysy_srcs/$ir_file" "$ir_output/$ir_file"
        echo "Compilation failed for $src_basename. Check log at $(basename "$log_file")"
        echo "========== [ Log output ] =========="
        tail -n 30 "$log_file"
        echo "===================================="
        exit 1
    elif grep -q "panicked" "$log_file"; then
        mv "$sysy_srcs/$ir_file" "$ir_output/$ir_file"
        echo "Compilation panicked for $src_basename. Check log at $log_file"
        echo "========== [ Log output ] =========="
        tail -n 30 "$log_file"
        echo "===================================="
        echo "ended log output for $src_basename"
        exit 1
    else
        mv "$sysy_srcs/$ir_file" "$ir_output/$ir_file"
        echo "Compiled $(basename "$src") successfully to $(basename "$asm_file")".
    fi

    echo "Testing IR file correctness for $ir_file"
    if ! grep -q "define" "$ir_output/$ir_file"; then
        echo "Error: IR file $ir_file does not contain a 'define' statement."
        exit 1
    fi

    local sylib_dir="$sysy_base/sylib"
    local exe_test_input="$sysy_base/in"
    local exe_file="$exe_output/$src_basename.elf"

    clang "$ir_output/$ir_file" "$sylib_dir/sylib.c" -o "$exe_file" || {
        echo "Error compiling IR file $ir_file with sylib.c"
        exit 1
    }
    echo "Compiled IR file $ir_file with sylib.c to executable $src_basename.elf. Now running the executable."

    local test_input="$exe_test_input/$src_basename.in"
    local test_output="$exe_test_output/$src_basename.out"
    if [ -f "$test_input" ]; then
        echo "Running $src_basename with input from $test_input"
        "$exe_file" < "$test_input" 1>"$test_output" 2>"$test_output.err"
        exit_code=$?
    else
        echo "No input file $test_input for $src_basename, running without input"
        "$exe_file" 1>"$test_output" 2>"$test_output.err"
        exit_code=$?
    fi

    # Check if test_output is not empty and doesn't end with a newline
    if [ -s "$test_output" ] && [ "$(tail -c 1 "$test_output" | wc -l)" -eq 0 ]; then
        echo -e "\n$exit_code" >> "$test_output"
    else
        echo "$exit_code" >> "$test_output"
    fi
    echo "Execution of $exe_file completed with exit code $exit_code. Output saved to $test_output"
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
        --dir)
            if [[ -n "$2" ]]; then
                default_srcdir="$2"
                shift 2
            else
                echo "Error: --dir requires a directory argument"
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

init_dirs
prepare_sylib
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