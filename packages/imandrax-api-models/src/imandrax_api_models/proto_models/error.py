from __future__ import annotations

import re
from enum import Enum
from typing import Self

from pydantic import Field

from ..proto_utils import BaseModel
from .locs import Location


# from https://github.com/imandra-ai/imandrax-api/blob/1a4abb4eb59d9545c6f0af9698956437b5b5dcb8/src/internal/error_data/imandrax_errors.ml
class ErrorKind(str, Enum):
    APPLIED_SYMBOL_TYPE_ERR = 'AppliedSymbolTypeErr'
    CONFIG_ERROR = 'ConfigError'
    CPTR_GET_OTHER_ERR = 'CptrGetOtherErr'
    CPTR_NOT_FOUND_IN_STORAGE = 'CptrNotFoundInStorage'
    GENERIC_USER_ERROR = 'GenericUserError'
    INVALID_FUN_DEFINITION = 'InvalidFunDefinition'
    INVALID_QIDENT = 'InvalidQident'
    INVALID_TYPE_ALIAS = 'InvalidTypeAlias'
    INVALID_TYPE_DEFINITION = 'InvalidTypeDefinition'
    INVALID_CNAME = 'InvalidCname'
    LEVAL_ERROR = 'LevalError'
    LOWER_CIR_ERROR = 'LowerCirError'
    LOWER_MIR_ERROR = 'LowerMirError'
    LOWER_RIR_ERROR = 'LowerRirError'
    PARSER_NOT_REGISTERED = 'ParserNotRegistered'
    PATMATCH_ERROR = 'PatmatchError'
    SYN_TERM_CREATE_ERROR = 'SynTermCreateError'
    SYNTAX_ATTRIBUTE_ERR = 'SyntaxAttributeErr'
    SYNTAX_ERR = 'SyntaxErr'
    SYNTAX_UNSUGAR_ERR = 'SyntaxUnsugarErr'
    TACTIC_EVAL_ERR = 'TacticEvalErr'
    TERM_INVALID_SHAPE = 'TermInvalidShape'
    TERM_MODEL_FIERR = 'TermModelFIErr'
    THREAD_FUT_FAILURE = 'ThreadFutFailure'
    THREAD_TIMER_FAILURE = 'ThreadTimerFailure'
    TY_INFER_INTERNAL_ERROR = 'TyInferInternalError'
    TYPE_ARITY_MISMATCH = 'TypeArityMismatch'
    TYPE_CYCLE_DETECTED = 'TypeCycleDetected'
    TYPE_ERR = 'TypeErr'
    TYPE_VAR_ALREADY_BOUND = 'TypeVarAlreadyBound'
    TYPED_SYMBOL_NON_GROUND = 'TypedSymbolNonGround'
    UID_CONTENT_ADDRESSING_NAME_MISMATCH = 'UidContentAddressingNameMismatch'
    UID_CONTENT_ADDRESSING_NOT_TEMPORARY = 'UidContentAddressingNotTemporary'
    UID_NOT_CONTENT_ADDRESSED = 'UidNotContentAddressed'
    UNKNOWN_BUILTIN_SYMBOL_FOR_UID = 'UnknownBuiltinSymbolForUid'
    UNKNOWN_TYPE_DEFINITION = 'UnknownTypeDefinition'
    INTERACTIVE_PROOF_ERR = 'InteractiveProofErr'
    PROOF_DESER_ERROR = 'ProofDeserError'
    PROOF_CHECK_ERROR = 'ProofCheckError'
    INVALID_ANCHOR = 'InvalidAnchor'
    INDUCT_SCHEMA_ERROR = 'InductSchemaError'
    UNSUPPORTED = 'Unsupported'
    LOWER_FOL_ERROR = 'LowerFolError'
    VALIDATION_ERROR = 'ValidationError'
    LSP_ERROR = 'LspError'
    INTERRUPTED = 'Interrupted'
    REDIS_ERROR = 'RedisError'
    CIRDECL_NOT_FOUND_IN_STORAGE = 'CIRDeclNotFoundInStorage'
    SERIALIZATION_ERROR = 'SerializationError'
    DESERIALIZATION_ERROR = 'DeserializationError'
    DEBUG_MODE = 'DebugMode'
    IMPORT_ERROR = 'ImportError'
    GENERIC_IOERROR = 'GenericIOError'
    DUNE_ERROR = 'DuneError'
    RPC_ERROR = 'RpcError'
    RPC_DESER_ERROR = 'RpcDeserError'
    RPC_NETWORK_ERROR = 'RpcNetworkError'
    RPC_TIMEOUT = 'RpcTimeout'
    AUTH_ERROR = 'AuthError'
    FILE_EXISTS = 'FileExists'
    DIRECTORY_CREATION_ERROR = 'DirectoryCreationError'
    DECOMP_ERROR = 'DecompError'
    VERSION_MISMATCH_ERROR = 'VersionMismatchError'
    OH_NO_ERROR = 'OhNoError'
    DEBOUNCED = 'Debounced'

    @classmethod
    def from_proto_kind(cls, proto_kind: str) -> Self:
        kinds = re.findall(r'\{ Kind.name = "(.+)" \}', proto_kind)
        if len(kinds) != 1:
            raise ValueError('Unable to parse kind')
        return cls(kinds[0])


class ErrorMessage(BaseModel):
    msg: str
    locs: list[Location] = Field(
        default_factory=lambda: [], description='Locations for this message'
    )
    backtrace: str | None = Field(default=None, description='Captured backtrace')


class Error(BaseModel):
    msg: ErrorMessage | None = Field(
        default=None, description='The toplevel error message'
    )
    kind: str = Field(description='A string description of the kind of error')
    stack: list[ErrorMessage] = Field(
        default_factory=lambda: [], description='Context for the error'
    )
    process: str | None = Field(default=None)
