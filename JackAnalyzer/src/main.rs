use std::env;
use std::ffi::OsStr;
use std::fs;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};

mod analyzer;
use analyzer::ast::Node;
use analyzer::parser::Parser;
use analyzer::tokenizer::JackTokenizer;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_names = get_source_file_paths(&args);

    let new_file_name = create_output_file_path(&args);
    let mut buf_writer =
        BufWriter::new(File::create(new_file_name).expect("failed to create xml file."));

    for file_name in file_names {
        let f = File::open(&file_name).expect("file not found");
        let mut tokenizer = JackTokenizer::new(f);
        let mut parser = Parser::new(&mut tokenizer);
        // parse and write xml file
        parser
            .parse_program()
            .to_xml()
            .write(&mut buf_writer)
            .expect("failed to write xml");
    }

    buf_writer.flush().expect("failed to flush");
}

fn get_source_file_paths(args: &[String]) -> Vec<PathBuf> {
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
            .filter(|path_buf| {
                path_buf
                    .as_path()
                    .extension()
                    .unwrap_or_else(|| OsStr::new(""))
                    == "jack"
            })
            .collect::<Vec<PathBuf>>();
        if filenames.is_empty() {
            panic!("there is no .jack file.");
        }
        return filenames;
    }

    // treats as file
    match path.extension() {
        Some(extension) => match extension.to_str().unwrap_or("") {
            "jack" => vec![path.to_path_buf()],
            unexpected_extension => panic!("unexpected file extension: {}.", unexpected_extension),
        },
        None => panic!("the specified file doesn't have extension."),
    }
}

fn create_output_file_path(args: &[String]) -> PathBuf {
    if args.len() < 2 {
        panic!("this command needs at least one argument")
    }

    let absolute_path = fs::canonicalize(&args[1]).expect("illegal source file path.");
    if absolute_path.is_dir() {
        absolute_path.join(format!(
            "{}.parsed.xml",
            absolute_path.file_stem().unwrap().to_str().unwrap()
        ))
    } else {
        absolute_path.with_extension(OsStr::new("parsed.xml"))
    }
}
