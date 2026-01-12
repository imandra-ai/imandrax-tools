# %%
import subprocess

from IPython.core.getipython import get_ipython

if ip := get_ipython():
    ip.run_line_magic('reload_ext', 'autoreload')
    ip.run_line_magic('autoreload', '2')

from pathlib import Path

curr_dir = Path.cwd() if ip else Path(__file__).parent

# %%
pp_bin_path = (
    curr_dir.parent.parent.parent.parent.parent
    / 'imandrax'
    / '_build'
    / 'default'
    / 'src/pp-goal-state/bin/pp_goal_state.exe'
)
pp_bin_path.resolve()
pp_bin_path

# %%
po_res_zip_path = curr_dir.parent.parent / 'scripts/local/tmp_po_res_art_zip'
po_res_zip_path


# %%
subprocess.run(
    [
        f'{str(pp_bin_path)}',
        f'{str(po_res_zip_path)}',
    ]
)
