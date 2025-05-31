use std::{path::Path, rc::Rc};

pub mod irgen;
use clap::Parser;
use irgen::IRTranslator;
use remusys_ir::ir::{module::Module, util::writer::write_ir_module};
use remusys_lang::{
    ast::{AstModule, print::AstPrinter},
    normalize::AstNormalizer,
};

#[derive(Parser, Debug)]
#[command(name = "remusys")]
#[command(about = "Simple SysY compiler.", long_about = None)]
struct Cli {
    /// 输入文件路径
    sysy_input: String,

    /// 打印刚解析出来、没有正规化的 AST, 文件名为 ${sysy_input}.ast
    #[arg(long, action = clap::ArgAction::SetTrue)]
    emit_ast: bool,

    /// 打印经过正规化的 SST, 文件名为 ${sysy_input}.sst
    #[arg(long, action = clap::ArgAction::SetTrue)]
    emit_sst: bool,

    /// 打印翻译出来的中间代码, 文件名为 ${sysy_input}.ll
    #[arg(long, action = clap::ArgAction::SetTrue)]
    emit_ir: bool,

    /// 输出 aarch64 汇编代码, 默认文件名为 ${sysy_input}.s
    #[arg(short('S'), long)]
    emit_asm: bool,

    #[arg(short = 'O', long = "optimize", action = clap::ArgAction::Count)]
    optimization_level: u8,

    #[arg(short, long)]
    output_file: Option<String>,
}

#[derive(Debug)]
enum ActionStep {
    Parse(String),
    OutputAst(String),
    Normalize,
    OutputSst(String),
    Translate,
    OutputIr(String),
    Optimize,
    OutputAsm(String),
}

fn main() {
    let cli = Cli::parse();

    let step_queue = make_action_queue(cli);
    println!("Action steps to be executed: {:#?}", &step_queue);

    let main_thread = std::thread::Builder::new()
        .name("remusys_main_thread".to_string())
        .stack_size(64 * 1024 * 1024) // 64 MB stack size
        .spawn(move || match remusys_main(step_queue) {
            Ok(_) => println!("Processing completed successfully."),
            Err(e) => eprintln!("Error processing: {}", e),
        })
        .unwrap();
    main_thread.join().unwrap_or_else(|_| {
        eprintln!("remusys_main thread panicked");
    });
}

fn make_action_queue(cli: Cli) -> Vec<ActionStep> {
    let emit_ast = cli.emit_ast;
    let emit_sst = cli.emit_sst;
    let emit_ir = cli.emit_ir;
    let emit_asm = cli.emit_asm;
    let should_optimize = cli.optimization_level > 0;
    let custom_output = cli.output_file.is_some();

    let do_until_parse = !emit_sst && !emit_ir && !emit_asm;
    let do_until_normalize = !emit_ir && !emit_asm;
    let do_until_translate = !emit_asm;

    let input = &cli.sysy_input;
    let input_path = Path::new(&cli.sysy_input);
    let input_basename = match (input_path.parent(), input_path.file_stem()) {
        (Some(parent), Some(stem)) => {
            if parent.as_os_str().is_empty() {
                stem.to_string_lossy().to_string()
            } else {
                format!("{}/{}", parent.to_string_lossy(), stem.to_string_lossy())
            }
        }
        (None, Some(stem)) => stem.to_string_lossy().to_string(),
        _ => return vec![],
    };

    let mut step_queue = Vec::new();
    step_queue.push(ActionStep::Parse(input.to_string()));
    if emit_ast {
        let ast_output = if do_until_parse && custom_output {
            cli.output_file.as_ref().unwrap().to_string()
        } else {
            format!("{}.ast", input_basename)
        };
        step_queue.push(ActionStep::OutputAst(ast_output));
    }
    if do_until_parse {
        return step_queue;
    }
    step_queue.push(ActionStep::Normalize);
    if emit_sst {
        let sst_output = if do_until_normalize && custom_output {
            cli.output_file.as_ref().unwrap().to_string()
        } else {
            format!("{}.sst", input_basename)
        };
        step_queue.push(ActionStep::OutputSst(sst_output));
    }
    if do_until_normalize {
        return step_queue;
    }

    step_queue.push(ActionStep::Translate);
    if emit_ir {
        let ir_output = if do_until_translate && custom_output {
            cli.output_file.as_ref().unwrap().to_string()
        } else {
            format!("{}.ll", input_basename)
        };
        step_queue.push(ActionStep::OutputIr(ir_output));
    }
    if do_until_translate {
        return step_queue;
    }
    if should_optimize {
        step_queue.push(ActionStep::Optimize);
    }
    if emit_asm {
        let asm_output = if custom_output {
            cli.output_file.unwrap()
        } else {
            format!("{}.s", input_basename)
        };
        step_queue.push(ActionStep::OutputAsm(asm_output));
    }
    step_queue
}

enum TempData {
    Ast(AstModule),
    Sst(AstModule),
    IR(Rc<Module>),
    Asm,
    None,
}

fn remusys_main(actions: Vec<ActionStep>) -> Result<(), String> {
    let mut temp_data = TempData::None;
    for action in actions {
        temp_data = match action {
            ActionStep::Parse(source) => {
                let ast = remusys_lang::parser::parse_sysy_file(source.as_str());
                TempData::Ast(ast)
            }
            ActionStep::OutputAst(ast_output) => {
                let ast = match temp_data {
                    TempData::Ast(ast) => ast,
                    _ => return Err("Expected AST data".to_string()),
                };
                println!("Outputting AST to {}", ast_output);
                let ast_file = std::fs::File::create(ast_output)
                    .map_err(|e| format!("Failed to create AST output file: {}", e))?;
                let mut ast_writer = std::io::BufWriter::new(ast_file);
                AstPrinter::new(&ast, &mut ast_writer).print_module();
                TempData::Ast(ast)
            }
            ActionStep::Normalize => {
                let ast = match temp_data {
                    TempData::Ast(ast) => ast,
                    _ => return Err("Expected AST data for normalization".to_string()),
                };
                let normalized_ast = AstNormalizer::new(&ast).normalize();
                TempData::Sst(normalized_ast)
            }
            ActionStep::OutputSst(sst_output) => {
                let sst = match temp_data {
                    TempData::Sst(sst) => sst,
                    _ => return Err("Expected SST data".to_string()),
                };
                println!("Outputting SST to {}", sst_output);
                let sst_file = std::fs::File::create(sst_output)
                    .map_err(|e| format!("Failed to create SST output file: {}", e))?;
                let mut sst_writer = std::io::BufWriter::new(sst_file);
                AstPrinter::new(&sst, &mut sst_writer).print_module();
                TempData::Sst(sst)
            }
            ActionStep::Translate => {
                let sst = match temp_data {
                    TempData::Sst(sst) => sst,
                    _ => return Err("Expected SST data for translation".to_string()),
                };
                let ir_module = IRTranslator::new_competition(&sst).translate(&sst);
                TempData::IR(ir_module)
            }
            ActionStep::OutputIr(ir_output) => {
                let ir_module = match temp_data {
                    TempData::IR(ir) => ir,
                    _ => return Err("Expected IR data".to_string()),
                };
                println!("Outputting IR to {}", ir_output);
                let ir_output = std::fs::File::create(ir_output)
                    .map_err(|e| format!("Failed to create IR output file: {}", e))?;
                let mut ir_output = std::io::BufWriter::new(ir_output);
                write_ir_module(&ir_module, &mut ir_output, false, false, false);
                TempData::IR(ir_module)
            }
            ActionStep::Optimize => todo!("Optimization step not implemented yet"),
            ActionStep::OutputAsm(_name) => todo!("Output assembly step not implemented yet"),
        };
    }
    Ok(())
}
