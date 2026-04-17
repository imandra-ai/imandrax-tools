# dump-of-fs

- An executable that builds a single JSON / YAML file out of a file tree of text files.
- Useful for managing template engine context, e.g. for `jinja2`
- Support configurable ignore (e.g. dotfiles, user-provided patterns), inject JSON / YAML file (when encoutering JSON / YAML files, insert their contents)


## Behaviors
- file name: extension gets stripped
  - eg: `foo/bar/baz.md` -> `foo.bar.baz` access in jinja2
  - when there's name collision, raise an error
  - NOTE: this makes the transformation lossy, roundtrip is impossible because we lost the ext info. Roundtrip is a non-goal at the moment.
  - Future TODO: this might be configurable
- default ignore: `.gitignore` + dotfiles
  - CLI: support `--no-default-ignore` flag
