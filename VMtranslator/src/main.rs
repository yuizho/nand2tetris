use std::env;
use std::ffi::OsStr;
use std::fs;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};

mod translator;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filenames = get_filenames(&args);

    let absolute_path = fs::canonicalize(filenames.first().unwrap()).unwrap();
    let new_filename = absolute_path.parent().unwrap().join(format!(
        "{}.asm",
        absolute_path.file_stem().unwrap().to_str().unwrap()
    ));

    let mut buf_writer =
        BufWriter::new(File::create(new_filename).expect("failed to create asm file."));
    let mut code_writer = translator::CodeWriter::create(&mut buf_writer);

    code_writer.write_init();

    for filename in filenames {
        println!("converts {:?} to .asm file", filename);

        let f = File::open(&filename).expect("file not found");
        let mut parser = translator::Parser::create(f);


        while parser.has_more_commands() {
            parser.advance();
            let vm_name = filename.as_path().file_stem().unwrap().to_str().unwrap();
            let command_type = parser.command_type(vm_name);
            println!("{:?}", command_type);
            code_writer.write_command(&command_type);
        }
    }

    buf_writer.flush().expect("failed to write asm file");
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
