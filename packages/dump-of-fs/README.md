# dump-of-fs

- An executable that builds a single JSON / YAML file out of a file tree of text files.
- Useful for managing template engine context, e.g. for `jinja2`
- Support configurable ignore (e.g. dotfiles, user-provided patterns), inject JSON / YAML file (when encoutering JSON / YAML files, insert their contents)

## Usage

```sh
dump-of-fs <ROOT> [--format json|yaml] [--output FILE] [--ignore PATTERN]... [--no-default-ignore]
```

### Example

Given this tree:

```
ctx/
├── intro.md          "hello"
├── sub/
│   └── notes.txt     "world"
└── config.json       {"version": 1}
```

Running `dump-of-fs ctx` produces:

```json
{
  "intro": "hello",
  "sub": {
    "notes": "world"
  },
  "config": {
    "version": 1
  }
}
```

Ready to feed into jinja2: `{{ intro }}`, `{{ sub.notes }}`, `{{ config.version }}`.

## Behaviors
- file name: extension gets stripped
  - eg: `foo/bar/baz.md` -> `foo.bar.baz` access in jinja2
  - when there's name collision, raise an error
  - NOTE: this makes the transformation lossy, roundtrip is impossible because we lost the ext info. Roundtrip is a non-goal at the moment.
  - Future TODO: this might be configurable
- default ignore: `.gitignore` + dotfiles
  - CLI: support `--no-default-ignore` flag
- JSON / YAML injection
  - NOTE: this is the only way to get arrays
  - inject under file name. E.g.: `foo/bar.json` -> `"foo": { "bar": <content of bar.json> }`
    - Future: make this configurable
  - Future: opt-out flag (always-on for now)
  - Raise if: error during parsing, name collision
