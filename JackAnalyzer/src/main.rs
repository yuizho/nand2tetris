use anyhow::{anyhow, Result};
use std::env;
use std::ffi::OsStr;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};
mod analyzer;
use analyzer::parser::Parser;
use analyzer::tokenizer::JackTokenizer;
use analyzer::vm::VmWriter;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    let file_names = get_source_file_paths(&args)?;

    for file_name in file_names {
        let new_file_name = file_name.with_extension(OsStr::new("vm"));
        let mut buf_writer = BufWriter::new(File::create(new_file_name)?);
        let f = File::open(&file_name)?;

        let mut tokenizer = JackTokenizer::new(f);
        let mut parser = Parser::new(
            &mut tokenizer,
            // filename is same as class name
            file_name.file_stem().unwrap().to_str().unwrap().to_string(),
        );
        let vm = parser.parse_program()?.to_vm();

        buf_writer.write_vm(&vm)?;
        buf_writer.flush()?;
    }

    Ok(())
}

fn get_source_file_paths(args: &[String]) -> Result<Vec<PathBuf>> {
    if args.len() < 2 {
        return Err(anyhow!("this command needs at least one argument"));
    }

    let path = Path::new(&args[1]);
    if path.is_dir() {
        // treats as directory
        let dir = std::fs::read_dir(&args[1])?;
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
            return Err(anyhow!("there is no .jack file."));
        }
        return Ok(filenames);
    }

    // treats as file
    match path.extension() {
        Some(extension) => match extension.to_str().unwrap_or("") {
            "jack" => Ok(vec![path.to_path_buf()]),
            unexpected_extension => {
                return Err(anyhow!(
                    "unexpected file extension: {}.",
                    unexpected_extension
                ))
            }
        },
        None => return Err(anyhow!("the specified file doesn't have extension.")),
    }
}
