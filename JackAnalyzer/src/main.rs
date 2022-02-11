use anyhow::{anyhow, Result};
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};
mod analyzer;
use analyzer::parser::Parser;
use analyzer::tokenizer::JackTokenizer;
use analyzer::xml::XmlWriter;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    let file_names = get_source_file_paths(&args)?;

    let new_file_name = create_output_file_path(&args)?;
    let mut buf_writer = BufWriter::new(File::create(new_file_name)?);

    for file_name in file_names {
        let f = File::open(&file_name)?;
        let mut tokenizer = JackTokenizer::new(f);
        let mut parser = Parser::new(&mut tokenizer);
        let elements = parser.parse_program()?.to_xml();
        // parse and write xml file
        buf_writer.write_xml(&elements)?;
    }

    buf_writer.flush()?;
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

fn create_output_file_path(args: &[String]) -> Result<PathBuf> {
    if args.len() < 2 {
        return Err(anyhow!("this command needs at least one argument"));
    }

    let absolute_path = fs::canonicalize(&args[1])?;
    if absolute_path.is_dir() {
        Ok(absolute_path.join(format!(
            "{}.parsed.xml",
            absolute_path.file_stem().unwrap().to_str().unwrap()
        )))
    } else {
        Ok(absolute_path.with_extension(OsStr::new("parsed.xml")))
    }
}
