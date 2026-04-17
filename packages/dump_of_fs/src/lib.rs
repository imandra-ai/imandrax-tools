use anyhow::{Context, Result, bail};
use ignore::WalkBuilder;
use serde_json::{Map, Value};
use std::fs;
use std::path::Path;

#[derive(Debug, Clone, Default)]
pub struct Options {
    pub no_default_ignore: bool,
    pub ignores: Vec<String>,
}

pub fn build_tree(root: &Path, opts: &Options) -> Result<Value> {
    let use_defaults = !opts.no_default_ignore;
    let mut builder = WalkBuilder::new(root);
    builder
        .standard_filters(use_defaults)
        .hidden(use_defaults)
        .git_ignore(use_defaults)
        .git_exclude(use_defaults)
        .git_global(use_defaults)
        .parents(use_defaults);

    if !opts.ignores.is_empty() {
        let mut overrides = ignore::overrides::OverrideBuilder::new(root);
        for pat in &opts.ignores {
            // OverrideBuilder inverts gitignore semantics (bare = include),
            // so prepend '!' to make bare patterns exclude.
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

    let value = load_value(abs, file_name)?;
    cursor.insert(key, value);
    Ok(())
}

fn strip_extension(name: &str) -> String {
    match Path::new(name).file_stem() {
        Some(stem) => stem.to_string_lossy().into_owned(),
        None => name.to_string(),
    }
}

fn load_value(abs: &Path, file_name: &str) -> Result<Value> {
    let content = fs::read_to_string(abs).with_context(|| format!("reading {:?}", abs))?;
    let ext = Path::new(file_name)
        .extension()
        .and_then(|e| e.to_str())
        .map(|e| e.to_ascii_lowercase());

    match ext.as_deref() {
        Some("json") => {
            serde_json::from_str(&content).with_context(|| format!("parsing JSON {:?}", abs))
        }
        Some("yaml") | Some("yml") => {
            serde_yaml::from_str(&content).with_context(|| format!("parsing YAML {:?}", abs))
        }
        _ => Ok(Value::String(content)),
    }
}
