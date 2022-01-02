use std::env;
use std::ffi::OsStr;
use std::fs;
use std::fs::File;
use std::io::{BufReader, BufWriter, Read, Write};
use std::path::{Path, PathBuf};

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_names = get_source_file_paths(&args);

    for file_name in file_names {
        let f = File::open(&file_name).expect("file not found");
        let mut buf_reader = BufReader::new(f);
        let mut readed = String::new();
        buf_reader
            .read_to_string(&mut readed)
            .expect("failed to read file");
        let chars = readed.chars().collect::<Vec<char>>();

        let mut buf_writer = BufWriter::new(
            File::create(create_output_file_path(&args)).expect("failed to create xml file."),
        );
        for c in chars {
            buf_writer
                .write(c.to_string().as_bytes())
                .expect("failed to write data");
        }

        buf_writer.flush().expect("failed to flush result");
    }
}

fn get_source_file_paths(args: &Vec<String>) -> Vec<PathBuf> {
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
            .filter(|path_buf| path_buf.as_path().extension().unwrap_or(OsStr::new("")) == "jack")
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

fn create_output_file_path(args: &Vec<String>) -> PathBuf {
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
