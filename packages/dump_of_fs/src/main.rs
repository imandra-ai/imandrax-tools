use anyhow::{Context, Result, bail};
use clap::{Parser, ValueEnum};
use ignore::WalkBuilder;
use serde_json::{Map, Value};
use std::fs;
use std::path::{Path, PathBuf};

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

    let tree = build_tree(&root, &cli)?;

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

fn build_tree(root: &Path, cli: &Cli) -> Result<Value> {
    let use_defaults = !cli.no_default_ignore;
    let mut builder = WalkBuilder::new(root);
    builder
        .standard_filters(use_defaults)
        .hidden(use_defaults)
        .git_ignore(use_defaults)
        .git_exclude(use_defaults)
        .git_global(use_defaults)
        .parents(use_defaults);

    if !cli.ignores.is_empty() {
        let mut overrides = ignore::overrides::OverrideBuilder::new(root);
        for pat in &cli.ignores {
            // gitignore patterns: prefix with ! to include, bare = exclude.
            // OverrideBuilder inverts that (bare = include), so prepend '!'.
            overrides.add(&format!("!{}", pat))?;
        }
        builder.overrides(overrides.build()?);
    }

    let mut root_obj = Map::new();
    for entry in builder.build() {
        let entry = entry?;
        if !entry.file_type().map(|t| t.is_file()).unwrap_or(false) {
            continue;
        }
        let rel = entry.path().strip_prefix(root)?;
        insert_file(&mut root_obj, rel, entry.path())?;
    }
    Ok(Value::Object(root_obj))
}

fn insert_file(root: &mut Map<String, Value>, rel: &Path, abs: &Path) -> Result<()> {
    let components: Vec<String> = rel
        .components()
        .map(|c| c.as_os_str().to_string_lossy().into_owned())
        .collect();
    if components.is_empty() {
        bail!("empty relative path for {:?}", abs);
    }

    let (file_name, dirs) = components.split_last().unwrap();
    let key = strip_extension(file_name);

    let mut cursor = root;
    for dir in dirs {
        let entry = cursor
            .entry(dir.clone())
            .or_insert_with(|| Value::Object(Map::new()));
        match entry {
            Value::Object(m) => cursor = m,
            _ => bail!(
                "key collision at {:?}: directory conflicts with existing value",
                abs
            ),
        }
    }

    if cursor.contains_key(&key) {
        bail!(
            "key collision: {:?} maps to key {:?} which already exists",
            abs,
            key
        );
    }

    let content = fs::read_to_string(abs).with_context(|| format!("reading {:?}", abs))?;
    cursor.insert(key, Value::String(content));
    Ok(())
}

fn strip_extension(name: &str) -> String {
    match Path::new(name).file_stem() {
        Some(stem) => stem.to_string_lossy().into_owned(),
        None => name.to_string(),
    }
}
