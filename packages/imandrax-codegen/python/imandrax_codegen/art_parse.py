import base64
import json
import subprocess
import sys
from functools import singledispatch
from pathlib import Path
from typing import Literal

import imandrax_codegen.ast_types as ast_types
from imandrax_api import url_dev, url_prod  # noqa: F401
from imandrax_api_models import Art
from imandrax_codegen.ast_deserialize import stmts_of_json

SUPPORTED_PLATFORMS = ('darwin', 'linux')

Mode = Literal['fun-decomp', 'model', 'decl']
Lang = Literal['python', 'typescript']


def find_art_parse_exe() -> Path:
    """Find the art_parse executable.

    Raises:
        ValueError: If the executable is not found or platform is unsupported
    """
    exe_path = Path(__file__).parent / 'art_parse.exe'

    if sys.platform not in SUPPORTED_PLATFORMS:
        raise ValueError(
            f'Platform {sys.platform!r} is not supported. '
            f'Supported platforms: {SUPPORTED_PLATFORMS}'
        )

    if not exe_path.exists():
        raise ValueError(
            f'art_parse.exe not found in {exe_path.parent}. '
            f'The package might not be built correctly. '
            f'Make sure you installed the platform-specific wheel, not the sdist.'
        )
    return exe_path


CODEGEN_EXE_PATH = find_art_parse_exe()


def _convert_to_standard_base64(data: str | bytes) -> str:
    """Convert bytes or URL-safe base64 string to standard base64.

    Handles two cases:
    1. If data is bytes: directly encode to standard base64
    2. If data is a URL-safe base64 string: convert to standard base64

    Pydantic serializes bytes as URL-safe base64 (using - and _ instead of + and /),
    but OCaml's Base64.decode_exn expects standard base64 encoding.

    Args:
        data: Either raw bytes or URL-safe base64 string

    Returns:
        Standard base64 string
    """
    if isinstance(data, bytes):
        # Directly encode bytes to standard base64
        return base64.b64encode(data).decode('ascii')

    # It's a string - assume it's URL-safe base64
    # Add padding if needed
    padding = (4 - len(data) % 4) % 4
    urlsafe_b64_padded = data + ('=' * padding)

    # Decode URL-safe and re-encode as standard base64
    decoded_bytes = base64.urlsafe_b64decode(urlsafe_b64_padded)
    return base64.b64encode(decoded_bytes).decode('ascii')


def _serialize_artifact(art: Art) -> str:
    """Serialize an artifact BaseModel to a JSON string."""
    art_dict = art.model_dump()
    art_dict['data'] = _convert_to_standard_base64(art_dict['data'])
    return json.dumps(art_dict)


def _run_parser(art_str: str, mode: Mode, lang: Lang) -> str:
    """Run the parser executable and return raw output."""
    cmd = [CODEGEN_EXE_PATH, '-', '-', '--mode', mode, '--lang', lang]
    result = subprocess.run(
        cmd,
        check=False,
        input=art_str,
        text=True,
        capture_output=True,
    )
    if result.returncode != 0:
        raise RuntimeError(f'Failed to run parser: {result.stderr}')
    return result.stdout


@singledispatch
def ast_of_art(art: str | Art, mode: Mode) -> list[ast_types.stmt]:
    raise NotImplementedError(f'Only Art and str are supported, got {type(art)}')


@ast_of_art.register
def _(art: str, mode: Mode) -> list[ast_types.stmt]:
    """Use the codegen executable to generate ASTs for a given artifact."""
    output = _run_parser(art, mode, 'python')
    return stmts_of_json(output)


@ast_of_art.register
def _(art: Art, mode: Mode) -> list[ast_types.stmt]:
    return ast_of_art(_serialize_artifact(art), mode)


@singledispatch
def codegen(art: str | Art, mode: Mode, lang: Lang = 'python') -> str:
    """Generate source code from an artifact."""
    raise NotImplementedError(f'Only Art and str are supported, got {type(art)}')


@codegen.register
def _(art: str, mode: Mode, lang: Lang = 'python') -> str:
    """Generate source code from an artifact string."""
    from imandrax_codegen.unparse import unparse

    if lang == 'typescript':
        return _run_parser(art, mode, 'typescript')
    else:
        stmts = ast_of_art(art, mode)
        return unparse(stmts)


@codegen.register
def _(art: Art, mode: Mode, lang: Lang = 'python') -> str:
    return codegen(_serialize_artifact(art), mode, lang)
