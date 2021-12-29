use std::env;
use std::ffi::OsStr;
use std::fs::File;
use std::io::BufWriter;
use std::path::{Path, PathBuf};

mod translator;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filenames = get_filenames(&args);

    for filename in filenames {
        println!("converts {:?} to .asm file", filename);

        let f = File::open(&filename).expect("file not found");
        let mut parser = translator::Parser::create(f);

        let mut buf_writer = BufWriter::new(
            File::create(&filename.with_extension("asm")).expect("failed to create asm file."),
        );
        let mut code_writer = translator::CodeWriter::create(&mut buf_writer);

        while parser.has_more_commands() {
            parser.advance();
            let command_type = parser.command_type();
            println!("{:?}", command_type);
            code_writer.write_command(&command_type);
        }

        code_writer.flush()
    }
}

fn get_filenames(args: &Vec<String>) -> Vec<PathBuf> {
    if args.len() < 2 {
        panic!("this command needs at least one argument")
    }

    let path = Path::new(&args[1]);
    if path.is_dir() {
        // treats as directory
        let dir = std::fs::read_dir(&args[1]).expect("failed to read dir");
        let filenames = dir
            .into_iter()
            .map(|f| f.expect("failed to read files in dir").path())
            .filter(|path_buf| path_buf.as_path().extension().unwrap_or(OsStr::new("")) == "vm")
            .collect::<Vec<PathBuf>>();
        if filenames.is_empty() {
            panic!("there is no .vm file.");
        }
        return filenames;
    }

    // treats as file
    match path.extension() {
        Some(extension) => match extension.to_str().unwrap_or("") {
            "vm" => vec![path.to_path_buf()],
            unexpected_extension => panic!("unexpected file extension: {}.", unexpected_extension),
        },
        None => panic!("the specified file doesn't have extension."),
    }
}
