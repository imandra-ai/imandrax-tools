from typing import Literal

from imandrax_api_models import Art

type Mode = Literal['fun-decomp', 'model', 'decl']
type Lang = Literal['python', 'typescript']

def code_of_art(art: str | Art, mode: Mode, lang: Lang) -> str: ...
