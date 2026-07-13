# pyright: basic
"""
IPython/Jupyter cell magic for IML.

Routes the body of any `%%iml` cell to a user-supplied imandrax-api client object's
`eval_src` method and prints the result.

Usage
-----
```jupyter
%load_ext imandrax_tools.widget.ipython_ext   # once per kernel session
%imandrax_client my_client      # once: name the client variable
%%iml
<iml source>               # -> shows eval results
```

The client is resolved from the user namespace *by name* at execution time,
so rebinding or mutating the bound object is reflected on the next cell.
A single client is shared by every `%%iml` cell; the extension is loaded once.
"""

from __future__ import annotations

from imandrax_api_models import EvalRes
from imandrax_api_models.client import ImandraXClient
from imandrax_api_models.context_utils import register_model_repr
from IPython.core.error import UsageError
from IPython.core.magic import Magics, cell_magic, line_magic, magics_class
from IPython.core.magic_arguments import (
    argument,
    magic_arguments,
    parse_argstring,
)


@magics_class
class IMLMagics(Magics):
    def __init__(self, shell):
        super().__init__(shell)
        # Name of the variable (in the user namespace) holding the client.
        # Stored on the single magics instance, so it persists across cells.
        self._client_name: str | None = None

    @property
    def _ns(self):
        shell = self.shell
        if shell is None:  # only possible if constructed outside IPython
            raise UsageError('no active IPython shell')
        return shell.user_ns

    @line_magic
    def imandrax_client(self, line):
        """Designate the client variable by name."""
        name = line.strip()
        if not name:
            raise UsageError('usage: %imandrax_client <variable_name>')
        if name not in self._ns:
            raise UsageError(f'no variable named {name!r} in the namespace')
        self._client_name = name

    def _resolve_client(self, override: str | None = None):
        name = override or self._client_name
        if name is None:
            raise UsageError(
                'no client set; run `%imandrax_client <variable_name>` first'
            )
        try:
            c = self._ns[name]
            if not isinstance(c, ImandraXClient):
                raise UsageError(f'variable {name!r} is not an imandrax client')
            return c
        except KeyError:
            raise UsageError(f'no variable named {name!r} in the namespace')

    @magic_arguments()
    @argument(
        '--client',
        default=None,
        help='override the client variable for this cell only',
    )
    @cell_magic
    def iml(self, line, cell):
        args = parse_argstring(self.iml, line)
        client = self._resolve_client(args.client)
        eval_res: EvalRes = client.eval_src(cell)
        # Returning the value lets IPython render it via the rich repr installed
        # by register_model_repr(), matching the notebook display.
        return eval_res


def load_ipython_extension(ipython):
    # Install the pretty __repr__ on the model classes so %%iml output matches
    # the notebook experience.
    register_model_repr()
    ipython.register_magics(IMLMagics)


def unload_ipython_extension(ipython):
    # Nothing to tear down; %reload_ext just re-registers the magics.
    pass
