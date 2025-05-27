use std::path::Path;

pub mod irgen;

fn remusys_main(file_path: &Path) -> Result<(), String> {
    // Placeholder for the actual implementation
    // This function would contain the logic to process the file at `file_path`
    println!("Processing file: {:?}", file_path);
    Ok(())
}

fn main() {
    let file_path = Path::new("example.sy");

    let remusys_main_thread = std::thread::Builder::new()
        .name("remusys_main_thread".to_string())
        .stack_size(64 * 1024 * 1024) // 64 MB stack size
        .spawn(move || {
            match remusys_main(file_path) {
                Ok(_) => println!("File processed successfully."),
                Err(e) => eprintln!("Error processing file: {}", e),
            }
        });
    remusys_main_thread
        .expect("Failed to spawn remusys_main thread")
        .join()
        .expect("remusys_main thread panicked");
    println!("Main thread completed.");
}
