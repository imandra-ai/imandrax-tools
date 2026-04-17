use dump_of_fs::{Options, build_tree};
use serde_json::{Value, json};
use std::fs;
use std::path::Path;
use tempfile::TempDir;

fn write(root: &Path, rel: &str, contents: &str) {
    let path = root.join(rel);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, contents).unwrap();
}

fn run(root: &Path) -> Value {
    build_tree(root, &Options::default()).unwrap()
}

// ================================
// Basic tree + extension stripping
// ================================

#[test]
fn nests_dirs_and_strips_extensions() {
    let tmp = TempDir::new().unwrap();
    write(tmp.path(), "a.md", "hello");
    write(tmp.path(), "sub/b.txt", "world");

    let out = run(tmp.path());
    assert_eq!(
        out,
        json!({
            "a": "hello",
            "sub": { "b": "world" }
        })
    );
}

#[test]
fn files_without_extension_use_raw_name() {
    let tmp = TempDir::new().unwrap();
    write(tmp.path(), "README", "x");
    let out = run(tmp.path());
    assert_eq!(out, json!({ "README": "x" }));
}

// ================
// JSON injection
// ================

#[test]
fn injects_json_as_parsed_object() {
    let tmp = TempDir::new().unwrap();
    write(tmp.path(), "data.json", r#"{"x": 1, "y": [2, 3]}"#);
    let out = run(tmp.path());
    assert_eq!(out, json!({ "data": { "x": 1, "y": [2, 3] } }));
}

#[test]
fn injects_json_scalar_root() {
    let tmp = TempDir::new().unwrap();
    write(tmp.path(), "n.json", "42");
    let out = run(tmp.path());
    assert_eq!(out, json!({ "n": 42 }));
}

#[test]
fn injects_json_array_root() {
    let tmp = TempDir::new().unwrap();
    write(tmp.path(), "list.json", "[1,2,3]");
    let out = run(tmp.path());
    assert_eq!(out, json!({ "list": [1, 2, 3] }));
}

// ================
// YAML injection
// ================

#[test]
fn injects_yaml_extension() {
    let tmp = TempDir::new().unwrap();
    write(tmp.path(), "c.yaml", "name: test\ncount: 2\n");
    let out = run(tmp.path());
    assert_eq!(out, json!({ "c": { "name": "test", "count": 2 } }));
}

#[test]
fn injects_yml_extension() {
    let tmp = TempDir::new().unwrap();
    write(tmp.path(), "c.yml", "k: v\n");
    let out = run(tmp.path());
    assert_eq!(out, json!({ "c": { "k": "v" } }));
}

// ==================
// Default ignores
// ==================

#[test]
fn dotfiles_ignored_by_default() {
    let tmp = TempDir::new().unwrap();
    write(tmp.path(), "visible.txt", "v");
    write(tmp.path(), ".hidden", "h");
    let out = run(tmp.path());
    assert_eq!(out, json!({ "visible": "v" }));
}

#[test]
fn no_default_ignore_includes_dotfiles() {
    let tmp = TempDir::new().unwrap();
    write(tmp.path(), "visible.txt", "v");
    write(tmp.path(), ".hidden", "h");

    let opts = Options {
        no_default_ignore: true,
        ..Default::default()
    };
    let out = build_tree(tmp.path(), &opts).unwrap();
    assert_eq!(out, json!({ "visible": "v", ".hidden": "h" }));
}

#[test]
fn gitignore_respected_by_default() {
    let tmp = TempDir::new().unwrap();
    // `ignore` crate only applies .gitignore inside a git repo; mark it so.
    fs::create_dir_all(tmp.path().join(".git")).unwrap();
    write(tmp.path(), ".gitignore", "skipme.txt\n");
    write(tmp.path(), "keep.txt", "k");
    write(tmp.path(), "skipme.txt", "s");

    let out = run(tmp.path());
    assert_eq!(out, json!({ "keep": "k" }));
}

// ===============
// Error paths
// ===============

#[test]
fn errors_on_file_key_collision() {
    let tmp = TempDir::new().unwrap();
    write(tmp.path(), "foo.md", "a");
    write(tmp.path(), "foo.txt", "b");

    let err = build_tree(tmp.path(), &Options::default()).unwrap_err();
    let msg = format!("{err:#}");
    assert!(msg.contains("collision"), "unexpected error: {msg}");
}

#[test]
fn errors_on_dir_vs_file_collision() {
    let tmp = TempDir::new().unwrap();
    write(tmp.path(), "foo.md", "a");
    write(tmp.path(), "foo/bar.md", "b");

    let err = build_tree(tmp.path(), &Options::default()).unwrap_err();
    let msg = format!("{err:#}");
    assert!(msg.contains("collision"), "unexpected error: {msg}");
}

#[test]
fn errors_on_bad_json() {
    let tmp = TempDir::new().unwrap();
    write(tmp.path(), "bad.json", "{not valid");
    let err = build_tree(tmp.path(), &Options::default()).unwrap_err();
    let msg = format!("{err:#}");
    assert!(msg.contains("parsing JSON"), "unexpected error: {msg}");
}

#[test]
fn errors_on_bad_yaml() {
    let tmp = TempDir::new().unwrap();
    write(tmp.path(), "bad.yaml", "[unterminated\n");
    let err = build_tree(tmp.path(), &Options::default()).unwrap_err();
    let msg = format!("{err:#}");
    assert!(msg.contains("parsing YAML"), "unexpected error: {msg}");
}
