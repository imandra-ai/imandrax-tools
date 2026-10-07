---
name: python-libraries
description: Python libraries for programmatically interacting with ImandraX. `imandrax-api-models` for client, `iml-query` for treesitter-based IML code manipulation.
tags: [reference]
---

- `imandrax-api-models`: 
  - `imandrax_api_models.client`: Python client instance
  - `imandrax_api_models.proto_models`: pydantic model definitions for ImandraX protobuf request and response messages
  - `imandrax_api_models.pp`: pretty-printer for ImandraX binding (decoded from artifacts)
- `iml-query`: IML treesitter parser and query utilities. For manipulating IML code.


- Available from PyPI
- Source code available on GitHub `imandra-ai/imandrax-tools/packages/`
- Tip: They are dependency of `codelogician` CLI. So you can find their installation from `codelogician`'s installation place.


Note: `codelogician` CLI is still the recommended default way to interact with ImandraX. Its UI and UX are optimized for both human and agent interactions, especially in the terminal.
