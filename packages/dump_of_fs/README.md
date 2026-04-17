# dump-of-fs

- An executable that builds a single JSON / YAML file out of a file tree of text files.
- Useful for managing template engine context, e.g. for `jinja2`
- Support configurable ignore (e.g. dotfiles, user-provided patterns), inject JSON / YAML file (when encoutering JSON / YAML files, insert their contents)
