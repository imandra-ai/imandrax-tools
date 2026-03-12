# ImandraX Code Generator

Code generator for ImandraX artifact

## System Dependencies

`libz.so.1` (zlib), `libgmp.so.10` (GMP), and `libprotobuf.so` are required at runtime by the bundled `art_parse.exe` binary; they are present on all standard Linux distributions but Nix users must add `pkgs.zlib`, `pkgs.gmp`, and `pkgs.protobuf` to `buildInputs` and `LD_LIBRARY_PATH`.
