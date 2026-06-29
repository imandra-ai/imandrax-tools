"""Extended imandrax-api client with Pydantic model validation."""

import os
import time
from collections.abc import Iterator
from contextlib import contextmanager, nullcontext
from pathlib import Path
from typing import Any, Literal

import imandrax_api
import structlog

from .trace_utils import (
    otel_trace as _otel_trace,
    set_span_attrs,
    summarize,
    tracer,
)

try:
    from iml_query.processing import (
        extract_decomp_reqs,
        extract_instance_reqs,
        extract_test_reqs,
        extract_verify_reqs,
    )
    from iml_query.tree_sitter_utils import get_parser
except ImportError:
    msg = """\
To use extended ImandraX API client, optional dependency `iml-query` is required.
Install it with `pip install "imandrax-api-models[client]"`

For regular API client without Pydantic model validation, use `imandrax-api` instead.\
"""
    raise ImportError(msg)


from imandrax_api_models import (
    DecomposeRes,
    EvalRes,
    GetDeclsRes,
    InstanceRes,
    TestRes,
    TypecheckRes,
    VerifyRes,
)
from imandrax_api_models.proto_models.api import (
    Artifact,
    ArtifactListResult,
    ArtifactZip,
    CodeSnippetEvalResult,
)
from imandrax_api_models.proto_models.task import Task

logger = structlog.get_logger(__name__)


@contextmanager
def _trace_call(
    op: str, *, session_id: str | None = None, **fields: Any
) -> Iterator[None]:
    """
    Log + (optional) OTel span around an API call.

    `session_id` is tagged onto the span as `imandrax.session.id` so traces can
    be grouped per server-side session in the UI.
    """
    summarized = {k: summarize(k, v) for k, v in fields.items()}
    log = logger.bind(op=op, **summarized)
    if session_id is not None:
        log = log.bind(session_id=session_id)
    log.debug('imandrax_api_call_start')
    span_cm = (
        tracer.start_as_current_span(f'imandrax.{op}')
        if tracer is not None
        else nullcontext()
    )
    t0 = time.perf_counter()
    with span_cm as span:
        if span is not None and session_id is not None:
            span.set_attribute('imandrax.session.id', session_id)
        set_span_attrs(span, fields)
        try:
            yield
        except Exception as e:
            dur_ms = (time.perf_counter() - t0) * 1000
            log.warning(
                'imandrax_api_call_error', dur_ms=round(dur_ms, 1), error=str(e)
            )
            if span is not None and _otel_trace is not None:
                span.record_exception(e)
                span.set_status(_otel_trace.StatusCode.ERROR)
            raise
        else:
            dur_ms = (time.perf_counter() - t0) * 1000
            log.debug('imandrax_api_call_done', dur_ms=round(dur_ms, 1))


# Extended client definition
# ====================


def _client_session_id(client: Any) -> str | None:
    """
    Best-effort fetch of the server-side session id from a client.

    `Client._sesh` is set in `__init__` for the sync client and in `__aenter__`
    for the async client (where session creation is an async RPC). `_session_id`
    is the resume-case fallback for async pre-`__aenter__`.
    """
    return getattr(getattr(client, '_sesh', None), 'id', None) or getattr(
        client, '_session_id', None
    )


class ImandraXClient(imandrax_api.Client):
    """Extended sync client with Pydantic model validation."""

    def _trace(self, op: str, **fields: Any) -> Any:
        return _trace_call(op, session_id=_client_session_id(self), **fields)

    def eval_src(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        src: str,
        timeout: float | None = None,
    ) -> EvalRes:
        with self._trace('eval_src', src=src, timeout=timeout):
            res = super().eval_src(src=src, timeout=timeout)
        return EvalRes.model_validate(res)

    def typecheck(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        src: str,
        timeout: float | None = None,
    ) -> TypecheckRes:
        """
        Typecheck IML code.

        No eval_src is needed before typecheck.

        Example:
            ```
            >>> iml_code = '''
            ... let f x = x + 1
            ...
            ... let g x = f x + 1
            ... '''
            >>> client.typecheck(iml_code)
            TypecheckRes(success=True, types=[InferredType(name='g', ty='int -> int', line=3, column=1), InferredType(name='f', ty='int -> int', line=1, column=1)], errors=None)
            ```

        """
        with self._trace('typecheck', src=src, timeout=timeout):
            res = super().typecheck(src=src, timeout=timeout)
        return TypecheckRes.model_validate(res)

    def decompose(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        name: str,
        assuming: str | None = None,
        basis: list[str] | None = None,
        rule_specs: list[str] | None = None,
        prune: bool | None = None,
        ctx_simp: bool | None = None,
        lift_bool: Any | None = None,
        timeout: float | None = None,
        string_results: bool | None = None,
    ) -> DecomposeRes:
        with self._trace(
            'decompose',
            name=name,
            assuming=assuming,
            basis=basis,
            rule_specs=rule_specs,
            timeout=timeout,
        ):
            res = super().decompose(
                name=name,
                basis=basis,
                rule_specs=rule_specs,
                prune=prune,
                ctx_simp=ctx_simp,
                lift_bool=lift_bool,
                timeout=timeout,
                string_results=string_results,
            )
        return DecomposeRes.model_validate(res)

    def verify_src(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        src: str,
        hints: str | None = None,
        timeout: float | None = None,
    ) -> VerifyRes:
        with self._trace('verify_src', src=src, hints=hints, timeout=timeout):
            res = super().verify_src(src=src, hints=hints, timeout=timeout)
        return VerifyRes.model_validate(res)

    def instance_src(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        src: str,
        hints: str | None = None,
        timeout: float | None = None,
    ) -> InstanceRes:
        with self._trace('instance_src', src=src, hints=hints, timeout=timeout):
            res = super().instance_src(src=src, hints=hints, timeout=timeout)
        return InstanceRes.model_validate(res)

    def test_src(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        src: str,
        seed: int | None = None,
        timeout: float | None = None,
    ) -> TestRes:
        with self._trace('test_src', src=src, seed=seed, timeout=timeout):
            res = super().test_src(src=src, seed=seed, timeout=timeout)
        return TestRes.model_validate(res)

    def test_name(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        name: str,
        seed: int | None = None,
        timeout: float | None = None,
    ) -> TestRes:
        with self._trace('test_name', name=name, seed=seed, timeout=timeout):
            res = super().test_name(name=name, seed=seed, timeout=timeout)
        return TestRes.model_validate(res)

    def get_decls(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        names: list[str],
        timeout: float | None = None,
    ) -> GetDeclsRes:
        with self._trace('get_decls', names=names, timeout=timeout):
            res = super().get_decls(names=names, timeout=timeout)
        return GetDeclsRes.model_validate(res)

    def eval_code_snippet(
        self,
        code: str,
        task_filter: list[str] | None = None,
        timeout: float | None = None,
    ) -> CodeSnippetEvalResult:
        # TODO: upstream
        from imandrax_api.bindings import api_pb2

        with self._trace(
            'eval_code_snippet', code=code, task_filter=task_filter, timeout=timeout
        ):
            timeout = timeout or super()._timeout
            req = api_pb2.CodeSnippet(code=code, task_filter=task_filter or [])
            res = super()._api_client.eval_code_snippet(
                ctx=super().mk_context(),
                request=req,
                timeout=timeout,
            )
        return CodeSnippetEvalResult.model_validate(res)

    def parse_term(
        self,
        code: str,
        task_filter: list[str] | None = None,
        timeout: float | None = None,
    ) -> Artifact:
        # TODO: upstream
        from imandrax_api.bindings import api_pb2

        with self._trace(
            'parse_term', code=code, task_filter=task_filter, timeout=timeout
        ):
            timeout = timeout or super()._timeout
            req = api_pb2.CodeSnippet(code=code, task_filter=task_filter or [])
            res = super()._api_client.parse_term(
                ctx=super().mk_context(),
                request=req,
                timeout=timeout,
            )
        return Artifact.model_validate(res)

    def parse_type(
        self,
        code: str,
        task_filter: list[str] | None = None,
        timeout: float | None = None,
    ) -> Artifact:
        # TODO: upstream
        from imandrax_api.bindings import api_pb2

        with self._trace(
            'parse_type', code=code, task_filter=task_filter, timeout=timeout
        ):
            timeout = timeout or super()._timeout
            req = api_pb2.CodeSnippet(code=code, task_filter=task_filter or [])
            res = super()._api_client.parse_type(
                ctx=super().mk_context(),
                request=req,
                timeout=timeout,
            )
        return Artifact.model_validate(res)

    def get_artifact(
        self,
        task: Task,
        kind: str,
    ) -> Artifact:
        # TODO: upstream
        from imandrax_api.bindings import api_pb2

        task_id = task.to_proto().id

        with self._trace('get_artifact', kind=kind):
            res = self._api_client.get_artifact(
                ctx=super().mk_context(),
                request=api_pb2.ArtifactGetQuery(task_id=task_id, kind=kind),
            )
        return Artifact.model_validate(res)

    def list_artifacts(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        task: Task,
        timeout: float | None = None,
    ) -> ArtifactListResult:
        with self._trace('list_artifacts', timeout=timeout):
            res = super().list_artifacts(task=task.to_proto(), timeout=timeout)
        return ArtifactListResult.model_validate(res)

    def get_artifact_zip(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        task: Task,
        kind: str,
        timeout: float | None = None,
    ) -> ArtifactZip:
        with self._trace('get_artifact_zip', kind=kind, timeout=timeout):
            res = super().get_artifact_zip(
                task=task.to_proto(), kind=kind, timeout=timeout
            )
        return ArtifactZip.model_validate(res)

    # Additional methods
    # ====================

    def eval_model(
        self,
        src: str,
        timeout: float | None = None,
        with_vgs: bool = False,
        with_decomps: bool = False,
        with_tests: bool = False,
    ) -> EvalRes:
        """Eval without VGs, decomps, and tests."""
        with self._trace(
            'eval_model',
            src=src,
            with_vgs=with_vgs,
            with_tests=with_tests,
            with_decomps=with_decomps,
        ):
            iml = src
            tree = get_parser().parse(iml.encode('utf-8'))
            if not with_vgs:
                iml, tree, _verify_reqs, _ = extract_verify_reqs(iml, tree)
                iml, tree, _instance_reqs, _ = extract_instance_reqs(iml, tree)
            if not with_decomps:
                iml, tree, _decomp_reqs, _ = extract_decomp_reqs(iml, tree)
            if not with_tests:
                iml, tree, _test_reqs, _ = extract_test_reqs(iml, tree)
            return self.eval_src(src=iml, timeout=timeout)

    @property
    def session_id(self) -> str | None:
        """Id of the server-side session this client is bound to, if any."""
        sesh = getattr(self, '_sesh', None)
        return sesh.id if sesh is not None else None

    def detach(self) -> str:
        """
        Close the local HTTP transport but leave the server session alive.

        Unlike `__exit__`, this does not issue `end_session`.
        After `detach` the client is closed and must not be used for further RPCs.

        Returns:
            str | None: current session id

        """
        sid = self.session_id
        if sid is None:
            raise RuntimeError('cannot detach a client with no session')
        self._closed = True
        self._session.close()
        return sid


class ImandraXAsyncClient(imandrax_api.AsyncClient):
    """Extended async client with Pydantic model validation."""

    def _trace(self, op: str, **fields: Any) -> Any:
        return _trace_call(op, session_id=_client_session_id(self), **fields)

    async def eval_src(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        src: str,
        timeout: float | None = None,
    ) -> EvalRes:
        with self._trace('eval_src', src=src, timeout=timeout):
            res = await super().eval_src(src=src, timeout=timeout)
        return EvalRes.model_validate(res)

    async def typecheck(self, src: str, timeout: float | None = None) -> TypecheckRes:  # type: ignore[override] # ty: ignore[invalid-method-override]
        """
        Typecheck IML code.

        No eval_src is needed before typecheck.

        Example:
            ```
            >>> iml_code = '''
            ... let f x = x + 1
            ...
            ... let g x = f x + 1
            ... '''
            >>> await client.typecheck(iml_code)
            TypecheckRes(success=True, types=[InferredType(name='g', ty='int -> int', line=3, column=1), InferredType(name='f', ty='int -> int', line=1, column=1)], errors=None)
            ```

        """
        with self._trace('typecheck', src=src, timeout=timeout):
            res = await super().typecheck(src=src, timeout=timeout)
        return TypecheckRes.model_validate(res)

    async def decompose(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        name: str,
        assuming: str | None = None,
        basis: list[str] | None = None,
        rule_specs: list[str] | None = None,
        prune: bool | None = None,
        ctx_simp: bool | None = None,
        lift_bool: Any | None = None,
        timeout: float | None = None,
        string_results: bool | None = None,
    ) -> DecomposeRes:
        with self._trace(
            'decompose',
            name=name,
            assuming=assuming,
            basis=basis,
            rule_specs=rule_specs,
            timeout=timeout,
        ):
            res = await super().decompose(
                name=name,
                assuming=assuming,
                basis=basis,
                rule_specs=rule_specs,
                prune=prune,
                ctx_simp=ctx_simp,
                lift_bool=lift_bool,
                timeout=timeout,
                string_results=string_results,
            )
        return DecomposeRes.model_validate(res)

    async def verify_src(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        src: str,
        hints: str | None = None,
        timeout: float | None = None,
    ) -> VerifyRes:
        with self._trace('verify_src', src=src, hints=hints, timeout=timeout):
            res = await super().verify_src(src=src, hints=hints, timeout=timeout)
        return VerifyRes.model_validate(res)

    async def instance_src(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        src: str,
        hints: str | None = None,
        timeout: float | None = None,
    ) -> InstanceRes:
        with self._trace('instance_src', src=src, hints=hints, timeout=timeout):
            res = await super().instance_src(src=src, hints=hints, timeout=timeout)
        return InstanceRes.model_validate(res)

    async def test_src(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        src: str,
        seed: int | None = None,
        timeout: float | None = None,
    ) -> TestRes:
        with self._trace('test_src', src=src, seed=seed, timeout=timeout):
            res = await super().test_src(src=src, seed=seed, timeout=timeout)
        return TestRes.model_validate(res)

    async def test_name(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        name: str,
        seed: int | None = None,
        timeout: float | None = None,
    ) -> TestRes:
        with self._trace('test_name', name=name, seed=seed, timeout=timeout):
            res = await super().test_name(name=name, seed=seed, timeout=timeout)
        return TestRes.model_validate(res)

    async def get_decls(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        names: list[str],
        timeout: float | None = None,
    ) -> GetDeclsRes:
        with self._trace('get_decls', names=names, timeout=timeout):
            res = await super().get_decls(names=names, timeout=timeout)
        return GetDeclsRes.model_validate(res)

    async def eval_code_snippet(
        self,
        code: str,
        task_filter: list[str] | None = None,
        timeout: float | None = None,
    ) -> CodeSnippetEvalResult:
        # TODO: upstream
        from imandrax_api.bindings import api_pb2

        with self._trace(
            'eval_code_snippet', code=code, task_filter=task_filter, timeout=timeout
        ):
            timeout = timeout or super()._timeout
            req = api_pb2.CodeSnippet(code=code, task_filter=task_filter or [])
            res = await super()._api_client.eval_code_snippet(
                ctx=super().mk_context(),
                request=req,
                timeout=timeout,
            )
        return CodeSnippetEvalResult.model_validate(res)

    async def parse_term(
        self,
        code: str,
        task_filter: list[str] | None = None,
        timeout: float | None = None,
    ) -> Artifact:
        # TODO: upstream
        from imandrax_api.bindings import api_pb2

        with self._trace(
            'parse_term', code=code, task_filter=task_filter, timeout=timeout
        ):
            timeout = timeout or super()._timeout
            req = api_pb2.CodeSnippet(code=code, task_filter=task_filter or [])
            res = await super()._api_client.parse_term(
                ctx=super().mk_context(),
                request=req,
                timeout=timeout,
            )
        return Artifact.model_validate(res)

    async def parse_type(
        self,
        code: str,
        task_filter: list[str] | None = None,
        timeout: float | None = None,
    ) -> Artifact:
        # TODO: upstream
        from imandrax_api.bindings import api_pb2

        with self._trace(
            'parse_type', code=code, task_filter=task_filter, timeout=timeout
        ):
            timeout = timeout or super()._timeout
            req = api_pb2.CodeSnippet(code=code, task_filter=task_filter or [])
            res = await super()._api_client.parse_type(
                ctx=super().mk_context(),
                request=req,
                timeout=timeout,
            )
        return Artifact.model_validate(res)

    async def get_artifact(
        self,
        task: Task,
        kind: str,
    ) -> Artifact:
        # TODO: upstream
        from imandrax_api.bindings import api_pb2

        task_id = task.to_proto().id

        with self._trace('get_artifact', kind=kind):
            res = await self._api_client.get_artifact(
                ctx=super().mk_context(),
                request=api_pb2.ArtifactGetQuery(task_id=task_id, kind=kind),
            )
        return Artifact.model_validate(res)

    async def list_artifacts(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        task: Task,
        timeout: float | None = None,
    ) -> ArtifactListResult:
        with self._trace('list_artifacts', timeout=timeout):
            res = await super().list_artifacts(task=task.to_proto(), timeout=timeout)
        return ArtifactListResult.model_validate(res)

    async def get_artifact_zip(  # type: ignore[override] # ty: ignore[invalid-method-override]
        self,
        task: Task,
        kind: str,
        timeout: float | None = None,
    ) -> ArtifactZip:
        with self._trace('get_artifact_zip', kind=kind, timeout=timeout):
            res = await super().get_artifact_zip(
                task=task.to_proto(), kind=kind, timeout=timeout
            )
        return ArtifactZip.model_validate(res)

    # Additional methods
    # ====================

    async def eval_model(
        self,
        src: str,
        timeout: float | None = None,
        with_vgs: bool = False,
        with_decomps: bool = False,
        with_tests: bool = False,
    ) -> EvalRes:
        """Eval without VGs, decomps, and tests."""
        with self._trace(
            'eval_model',
            src=src,
            with_vgs=with_vgs,
            with_decomps=with_decomps,
            with_tests=with_tests,
        ):
            iml = src
            tree = get_parser().parse(iml.encode('utf-8'))
            if not with_vgs:
                iml, tree, _verify_reqs, _ = extract_verify_reqs(iml, tree)
                iml, tree, _instance_reqs, _ = extract_instance_reqs(iml, tree)
            if not with_decomps:
                iml, tree, _decomp_reqs, _ = extract_decomp_reqs(iml, tree)
            if not with_tests:
                iml, tree, _test_reqs, _ = extract_test_reqs(iml, tree)

            return await self.eval_src(src=iml, timeout=timeout)

    @property
    def session_id(self) -> str | None:
        """Id of the server-side session this client is bound to, if any."""
        return self._session_id

    async def detach(self) -> str:
        """
        Close the local HTTP transport but leave the server session alive.

        Unlike `__exit__`, this does not issue `end_session`.
        After `detach` the client is closed and must not be used for further RPCs.

        Returns:
            str: current session id

        """
        sid = self._session_id
        if sid is None:
            raise RuntimeError('cannot detach a client with no session')
        self._closed = True
        await self._session.close()
        return sid


# Helpers for creating client
# ====================


def _get_deployment_from_default_config() -> str | None:
    config_path = Path.home() / '.config' / 'imandrax' / 'config.toml'
    if config_path.exists():
        import tomllib

        deployment = tomllib.loads(config_path.read_text())
        return deployment.get('net', {}).get('deployment')


def get_imandrax_url(env: Literal['dev', 'prod'] | None = None) -> str | None:
    """
    Get the ImandraX URL from the environment variable or default config location.

    Precedence: env(IMANDRAX_URL) > env arg > env(IMANDRAX_ENV) > default config
    """
    if url := os.getenv('IMANDRAX_URL'):
        return url

    env_ = (
        env
        or os.getenv('IMANDRAX_ENV', 'prod')
        or _get_deployment_from_default_config()
    )
    if env_ == 'dev':
        url = imandrax_api.url_dev
    elif env_ == 'prod':
        url = imandrax_api.url_prod
    return url


def get_imandrax_api_key() -> str | None:
    """Get the API key from the environment variable or default config location."""
    api_key: str | None = os.getenv('IMANDRAX_API_KEY')

    if not api_key:
        # try to read from default config location
        config_path = Path.home() / '.config' / 'imandrax' / 'api_key'
        if config_path.exists():
            api_key = config_path.read_text().strip()
    return api_key


def get_imandrax_client(
    auth_token: str | None = None,
    env: Literal['dev', 'prod'] | None = None,
    session_id: str | None = None,
    create_if_not_found: bool = False,
) -> ImandraXClient:
    url = get_imandrax_url(env)
    if not url:
        raise ValueError('IMANDRAX_URL is not set')

    if auth_token is None:
        logger.debug('imandra_api_key is None, setting from env and default path')
    imandrax_api_key = auth_token or get_imandrax_api_key()
    if not imandrax_api_key:
        logger.error('IMANDRAX_API_KEY is None')
        raise ValueError('IMANDRAX_API_KEY is None')

    client = ImandraXClient(
        url=url,
        auth_token=imandrax_api_key,
        timeout=300,
        session_id=session_id,
        create_if_not_found=create_if_not_found,
    )
    logger.info('imandrax_client_initialized', url=url, session_id=session_id)
    return client


def get_imandrax_async_client(
    auth_token: str | None = None,
    env: Literal['dev', 'prod'] | None = None,
    session_id: str | None = None,
    create_if_not_found: bool = False,
) -> ImandraXAsyncClient:
    url = get_imandrax_url(env)
    if not url:
        raise ValueError('IMANDRAX_URL is not set')

    if auth_token is None:
        logger.debug('imandra_api_key is None, setting from env and default path')
    imandrax_api_key = auth_token or get_imandrax_api_key()
    if not imandrax_api_key:
        logger.error('IMANDRAX_API_KEY is None')
        raise ValueError('IMANDRAX_API_KEY is None')

    client = ImandraXAsyncClient(
        url=url,
        auth_token=imandrax_api_key,
        timeout=300,
        session_id=session_id,
        create_if_not_found=create_if_not_found,
    )
    logger.info('imandrax_client_initialized', url=url, session_id=session_id)
    return client


def _end_session(
    session_id: str,
    *,
    url: str = imandrax_api.url_prod,
    server_path_prefix: str = '/api/v1',
    auth_token: str | None = None,
    api_key: str | None = None,
    timeout: int = 30,
) -> None:
    """
    End a server-side session by id, without opening it first.

    A regular `Client(session_id=...)` would issue an `open_session` RPC on
    construction, which can be wasteful (and fails on an already-dead session)
    when only discarding a cached/stale id is needed.

    Errors propagate as `TwirpServerException` (e.g. the session is already
    gone); callers wanting best-effort cleanup should catch them.
    """
    import requests
    from imandrax_api.bindings import session_pb2, simple_api_twirp
    from imandrax_api.client._common import mk_context

    sess = requests.Session()
    token = api_key or auth_token
    if token:
        sess.headers['Authorization'] = f'Bearer {token}'
    try:
        client = simple_api_twirp.SimpleClient(
            url,
            timeout=timeout,
            server_path_prefix=server_path_prefix,
            session=sess,
        )
        client.end_session(
            ctx=mk_context(),
            request=session_pb2.Session(id=session_id),
            timeout=None,
        )
    finally:
        sess.close()


def end_session(
    session_id: str,
    auth_token: str | None = None,
    env: Literal['dev', 'prod'] | None = None,
) -> None:
    """
    End a server-side session by id, resolving url/key like `get_imandrax_client`.

    Errors propagate as `TwirpServerException`; callers wanting best-effort cleanup should catch.
    """
    url = get_imandrax_url(env)
    if not url:
        raise ValueError('IMANDRAX_URL is not set')
    imandrax_api_key = auth_token or get_imandrax_api_key()
    if not imandrax_api_key:
        raise ValueError('IMANDRAX_API_KEY is None')
    imandrax_api.end_session(session_id, url=url, auth_token=imandrax_api_key)
