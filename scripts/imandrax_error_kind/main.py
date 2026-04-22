# ruff: noqa: E741
import json
import re
from dataclasses import dataclass
from typing import Iterator

import requests


def _fetch_content():

    URL = "https://github.com/imandra-ai/imandrax-api/blob/main/src/internal/error_data/imandrax_errors.ml"
    RAW_URL = URL.replace("github.com", "raw.githubusercontent.com").replace(
        "/blob/", "/"
    )

    response = requests.get(RAW_URL)

    if response.status_code == 200:
        content = response.text
    else:
        raise Exception(f"Failed to fetch content: {response.status_code}")

    return content


def fetch_content() -> str:
    import os

    from inline_snapshot import get_snapshot_value, snapshot

    UNTRUSTING = os.environ.get("UNTRUSTING") in ["1", "true", "True"]

    ss = snapshot("""\
(** Error kinds for Imandrax *)

type t = {
  name: string;
  descr: string;
}

let make name descr : t = { name; descr }

(** All errors *)
let all : t list =
  [
  make "AppliedSymbolTypeErr" "AppliedSymbol could not be created because of a type mismatch";
  make "ConfigError" "Error in CLI arguments or configuration";
  make "CptrGetOtherErr" "Other error in `Cptr.get`";
  make "CptrNotFoundInStorage" "Data for a `cptr` was not found in persistent storage";
  make "GenericUserError" "user error";
  make "InvalidFunDefinition" "Function definition is not valid";
  make "InvalidQident" "invalid qualified identifier";
  make "InvalidTypeAlias" "Type alias is not well-formed";
  make "InvalidTypeDefinition" "Type definition is not valid";
  make "InvalidCname" "Not a valid Cname";
  make "LevalError" "Error in lightweight evaluation";
  make "LowerCirError" "Error in lowering typingtree to CIR";
  make "LowerMirError" "Error in lowering CIR to MIR";
  make "LowerRirError" "Error in lowering MIR to RIR";
  make "ParserNotRegistered" "Parser for this syntax was not registered";
  make "PatmatchError" "Error in pattern matching compilation";
  make "SynTermCreateError" "Error when creating a Syn_term";
  make "SyntaxAttributeErr" "error in parsing attributes";
  make "SyntaxErr" "syntax error";
  make "SyntaxUnsugarErr" "error in unsugaring OCaml syntax";
  make "TacticEvalErr" "error when evaluating a tactic term";
  make "TermInvalidShape" "Term has an invalid shape for this operation";
  make "TermModelFIErr" "Error during creation of a FI (function interpretation) in TermModel";
  make "ThreadFutFailure" "Failure in future";
  make "ThreadTimerFailure" "Failure in timer";
  make "TyInferInternalError" "internal error in type inference";
  make "TypeArityMismatch" "number of parameters does not match expected arity";
  make "TypeCycleDetected" "Type variable occurs inside the type is is about to be bound to";
  make "TypeErr" "type error";
  make "TypeVarAlreadyBound" "Type variable is already bound";
  make "TypedSymbolNonGround" "Type cannot be turned into a nullary type schema";
  make "UidContentAddressingNameMismatch" "Uid has a different name than the Cname.t it should become";
  make "UidContentAddressingNotTemporary" "Uid was not created as a temporary to be turned into a Cname.t";
  make "UidNotContentAddressed" "Uid is not a Cname";
  make "UnknownBuiltinSymbolForUid" "No builtin symbol are know for this Uid";
  make "UnknownTypeDefinition" "Type definition could not be found";
  make "InteractiveProofErr" "Error in interactive proof";
  make "ProofDeserError" "Error when deserializing a proof";
  make "ProofCheckError" "Error during proof checking";
  make "InvalidAnchor" "Error when parsing an anchor";
  make "InductSchemaError" "Error when generating an induction schema";
  make "Unsupported" "Unsupported feature";
  make "LowerFolError" "Error in lowering RIR to FOL";
  make "ValidationError" "Error while validating a definition";
  make "LspError" "Error in LSP server";
  make "Interrupted" "Computation was interrupted";
  make "RedisError" "Redis error";
  make "CIRDeclNotFoundInStorage" "CIR storage error";
  make "SerializationError" "Error during serialization";
  make "DeserializationError" "Error during deserialization";
  make "DebugMode" "Feature disabled/inactive for debug reasons";
  make "ImportError" "Import failed";
  make "GenericIOError" "Generic IO error";
  make "DuneError" "Error related to handling of dune projects";
  make "RpcError" "Error in RPC";
  make "RpcDeserError" "Deserialization error in RPC";
  make "RpcNetworkError" "Network error in RPC";
  make "RpcTimeout" "Timeout in RPC";
  make "AuthError" "Authentication error";
  make "FileExists" "File already exists";
  make "DirectoryCreationError" "Error while creating directory";
  make "DecompError" "Error during decomposition";
  make "VersionMismatchError" "Versions do not match";
  make "OhNoError" "Oh no 😰";
  make "Debounced" "Task has been cancelled due to debouncing";
  ]
  [@ocamlformat "disable"]
""")
    if UNTRUSTING:
        assert ss == _fetch_content()

    return get_snapshot_value(ss)


all_lines: list[str] = fetch_content().splitlines()
make_lines = (l for l in all_lines if l.lstrip().startswith("make"))


def match(line: str) -> tuple[str, str]:
    matches = re.findall(r'"([^"]*)"', line)
    return matches[0], matches[1]


matches: Iterator[tuple[str, str]] = (match(l) for l in make_lines)

# err_names, err_descrs = zip(*matches)


@dataclass
class ErrorKind:
    name: str
    description: str


err_kinds = (ErrorKind(name, descr) for name, descr in matches)

if __name__ == "__main__":
    print(json.dumps([vars(ek) for ek in err_kinds], indent=2))
