# %%
import subprocess
from typing import Final

from IPython.core.getipython import get_ipython
from rich import print

if ip := get_ipython():
    ip.run_line_magic('reload_ext', 'autoreload')
    ip.run_line_magic('autoreload', '2')

from pathlib import Path

curr_dir = Path.cwd() if ip else Path(__file__).parent

WORKSPACE_DIR: Final[Path] = curr_dir.parent.parent.parent.parent

# %%
PO_RES_PP_BIN_PATH: Final[Path] = (
    WORKSPACE_DIR.parent
    / 'imandrax'
    / '_build'
    / 'default'
    / 'src/pp-goal-state/bin/pp_goal_state.exe'
)

# %%
po_res_zip_path = curr_dir.parent.parent / 'scripts/local/tmp_po_res_art_zip'
po_res_zip_path


# %%
out = subprocess.run(
    [
        f'{str(PO_RES_PP_BIN_PATH)}',
        f'{str(po_res_zip_path)}',
    ],
    capture_output=True,
)

# %%
print(out.stdout.decode())
