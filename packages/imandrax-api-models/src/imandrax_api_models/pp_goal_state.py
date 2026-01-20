import subprocess
import tempfile
from pathlib import Path
from typing import Final

from imandrax_api import bindings as xbinding

from imandrax_api_models import proto_models

curr_dir = Path(__file__).parent

WORKSPACE_DIR: Final[Path] = curr_dir.parent.parent.parent.parent
PO_RES_PP_BIN_PATH: Final[Path] = (
    WORKSPACE_DIR.parent
    / 'imandrax'
    / '_build'
    / 'default'
    / 'src/pp-goal-state/bin/pp_goal_state.exe'
)


def pp_goal_state(
    po_res: Path
    | xbinding.api_pb2.ArtifactZip
    | proto_models.Art
    | xbinding.artmsg_pb2.Art,
) -> str:
    """
    Run the pp-goal-state binary on the given po-res file.

    Args:
        po_res:
            - the zip path, or json path
                - for zip file, it will contains manifest.json inside it
                - the json file is assumed to be dumped Artifact, with
                    kind, data, and api_version fields
            - if it's an api_pb2.ArtifactZip, we create a temp zip file

    """
    if not PO_RES_PP_BIN_PATH.exists():
        raise FileNotFoundError('imandrax-pp-goal-state binary not found')

    match po_res:
        # Base case
        # --------------------
        case Path():
            if po_res.suffix not in ('.zip', '.json'):
                raise ValueError('po_res must be a zip or json file')
            out = subprocess.run(
                [
                    f'{str(PO_RES_PP_BIN_PATH)}',
                    f'{str(po_res)}',
                ],
                capture_output=True,
            )
            if out.returncode != 0:
                raise RuntimeError(f'pp_goal_state failed: {out.stderr.decode()}')
            return out.stdout.decode()
        # Turn into base case
        # --------------------
        case xbinding.api_pb2.ArtifactZip():
            with tempfile.NamedTemporaryFile(suffix='.zip') as tmp:
                tmp.write(po_res.art_zip)
                tmp.flush()
                return pp_goal_state(Path(tmp.name))

        case proto_models.Art():
            with tempfile.NamedTemporaryFile(suffix='.json') as tmp:
                tmp.write(bytes(po_res.model_dump_json(), encoding='utf-8'))
                tmp.flush()
                return pp_goal_state(Path(tmp.name))

        case xbinding.artmsg_pb2.Art():
            with tempfile.NamedTemporaryFile(suffix='.json') as tmp:
                tmp.write(
                    bytes(
                        proto_models.Art.model_validate(po_res).model_dump_json(),
                        encoding='utf-8',
                    )
                )
                tmp.flush()
                return pp_goal_state(Path(tmp.name))
