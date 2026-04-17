use anyhow::{Context, Result, bail};
use clap::{Parser, ValueEnum};
use dump_of_fs::{Options, build_tree};
use std::fs;
use std::path::PathBuf;

#[derive(Copy, Clone, Debug, ValueEnum)]
enum Format {
    Json,
    Yaml,
}

#[derive(Parser, Debug)]
#[command(about = "Dump a file tree into a single JSON/YAML document")]
struct Cli {
    /// Root directory to dump
    root: PathBuf,

    /// Output format
    #[arg(long, value_enum, default_value_t = Format::Json)]
    format: Format,

    /// Output file (stdout if omitted)
    #[arg(long, short)]
    output: Option<PathBuf>,

    /// Disable default ignores (.gitignore + dotfiles)
    #[arg(long)]
    no_default_ignore: bool,

    /// Additional ignore pattern (gitignore syntax); repeatable
    #[arg(long = "ignore", value_name = "PATTERN")]
    ignores: Vec<String>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let root = cli
        .root
        .canonicalize()
        .with_context(|| format!("resolving root {:?}", cli.root))?;
    if !root.is_dir() {
        bail!("root {:?} is not a directory", root);
    }

    let opts = Options {
        no_default_ignore: cli.no_default_ignore,
        ignores: cli.ignores,
    };
    let tree = build_tree(&root, &opts)?;

    let rendered = match cli.format {
        Format::Json => serde_json::to_string_pretty(&tree)?,
        Format::Yaml => serde_yaml::to_string(&tree)?,
    };

    match cli.output {
        Some(path) => {
            fs::write(&path, rendered).with_context(|| format!("writing output {:?}", path))?
        }
        None => println!("{}", rendered),
    }
    Ok(())
}
