"""Extended client with Pydantic model validation."""

import os
from pathlib import Path
from typing import TYPE_CHECKING, Any, Literal

import imandrax_api
import structlog

from imandrax_api_models import (
    DecomposeRes,
    EvalRes,
    InstanceRes,
    TypecheckRes,
    VerifyRes,
)

if TYPE_CHECKING:

    class AsyncClient:
        def __init__(self, url: str, auth_token: str, timeout: float) -> None: ...
        async def eval_src(self, src: str, timeout: float | None = None) -> EvalRes: ...
        async def typecheck(
            self, src: str, timeout: float | None = None
        ) -> TypecheckRes: ...
        async def decompose(
            self,
            name: str,
            assuming: str | None = None,
            basis: list[str] | None = None,
            rule_specs: list[str] | None = None,
            prune: bool | None = True,
            ctx_simp: bool | None = None,
            lift_bool: Any | None = None,
            timeout: float | None = None,
            str: bool | None = True,
        ) -> DecomposeRes: ...
        async def verify_src(
            self,
            src: str,
            hints: str | None = None,
            timeout: float | None = None,
        ) -> VerifyRes: ...
        async def instance_src(
            self,
            src: str,
            hints: str | None = None,
            timeout: float | None = None,
        ) -> InstanceRes: ...

else:
    from imandrax_api import AsyncClient

logger = structlog.get_logger(__name__)


class ImandraXClient(imandrax_api.Client):
    """Extended sync client with Pydantic model validation."""

    def eval_src(  # type: ignore[override]
        self,
        src: str,
        timeout: float | None = None,
    ) -> EvalRes:
        res = super().eval_src(src=src, timeout=timeout)
        return EvalRes.model_validate(res)

    def typecheck(self, src: str, timeout: float | None = None) -> TypecheckRes:  # type: ignore[override]
        """
        Typecheck IML code.

        No eval_src is needed before typecheck.

        Example:
            >>> iml_code = '''\
            ... let f x = x + 1
            ...
            ... let g x = f x + 1
            ... '''
            >>> client.typecheck(iml_code)
            TypecheckRes(success=True, types=[InferredType(name='g', ty='int -> int', line=3, column=1), InferredType(name='f', ty='int -> int', line=1, column=1)], errors=None)

        """
        res = super().typecheck(src=src, timeout=timeout)
        return TypecheckRes.model_validate(res)

    def decompose(  # type: ignore[override]
        self,
        name: str,
        assuming: str | None = None,
        basis: list[str] | None = None,
        rule_specs: list[str] | None = None,
        prune: bool | None = True,
        ctx_simp: bool | None = None,
        lift_bool: Any | None = None,
        timeout: float | None = None,
        str: bool | None = True,
    ) -> DecomposeRes:
        if basis is None:
            basis = []
        if rule_specs is None:
            rule_specs = []

        res = super().decompose(
            name=name,
            basis=basis,
            rule_specs=rule_specs,
            prune=prune,
            ctx_simp=ctx_simp,
            lift_bool=lift_bool,
            timeout=timeout,
            str=str,
        )
        return DecomposeRes.model_validate(res)

    def verify_src(  # type: ignore[override]
        self,
        src: str,
        hints: str | None = None,
        timeout: float | None = None,
    ) -> VerifyRes:
        res = super().verify_src(src=src, hints=hints, timeout=timeout)
        return VerifyRes.model_validate(res)

    def instance_src(  # type: ignore[override]
        self,
        src: str,
        hints: str | None = None,
        timeout: float | None = None,
    ) -> InstanceRes:
        res = super().instance_src(src=src, hints=hints, timeout=timeout)
        return InstanceRes.model_validate(res)


class ImandraXAsyncClient(AsyncClient):
    """Extended async client with Pydantic model validation."""

    async def eval_src(
        self,
        src: str,
        timeout: float | None = None,
    ) -> EvalRes:
        res = await super().eval_src(src=src, timeout=timeout)
        return EvalRes.model_validate(res)

    async def typecheck(self, src: str, timeout: float | None = None) -> TypecheckRes:
        """
        Typecheck IML code.

        No eval_src is needed before typecheck.

        Example:
            >>> iml_code = '''\
            ... let f x = x + 1
            ...
            ... let g x = f x + 1
            ... '''
            >>> await client.typecheck(iml_code)
            TypecheckRes(success=True, types=[InferredType(name='g', ty='int -> int', line=3, column=1), InferredType(name='f', ty='int -> int', line=1, column=1)], errors=None)

        """
        res = await super().typecheck(src=src, timeout=timeout)
        return TypecheckRes.model_validate(res)

    async def decompose(
        self,
        name: str,
        assuming: str | None = None,
        basis: list[str] | None = None,
        rule_specs: list[str] | None = None,
        prune: bool | None = True,
        ctx_simp: bool | None = None,
        lift_bool: Any | None = None,
        timeout: float | None = None,
        str: bool | None = True,
    ) -> DecomposeRes:
        if basis is None:
            basis = []
        if rule_specs is None:
            rule_specs = []

        res = await super().decompose(
            name=name,
            assuming=assuming,
            basis=basis,
            rule_specs=rule_specs,
            prune=prune,
            ctx_simp=ctx_simp,
            lift_bool=lift_bool,
            timeout=timeout,
            str=str,
        )
        return DecomposeRes.model_validate(res)

    async def verify_src(
        self,
        src: str,
        hints: str | None = None,
        timeout: float | None = None,
    ) -> VerifyRes:
        res = await super().verify_src(src=src, hints=hints, timeout=timeout)
        return VerifyRes.model_validate(res)

    async def instance_src(
        self,
        src: str,
        hints: str | None = None,
        timeout: float | None = None,
    ) -> InstanceRes:
        res = await super().instance_src(src=src, hints=hints, timeout=timeout)
        return InstanceRes.model_validate(res)


def get_imandrax_client(
    imandra_api_key: str | None = None,
    env: Literal['dev', 'prod'] | None = None,
) -> ImandraXClient:
    imandrax_env = env or os.getenv('IMANDRAX_ENV', 'prod')
    if imandrax_env == 'dev':
        url = imandrax_api.url_dev
    elif imandrax_env == 'prod':
        url = imandrax_api.url_prod
    else:
        url = os.getenv('IMANDRAX_URL')
        if not url:
            raise ValueError('IMANDRAX_URL is not set')

    if imandra_api_key is None:
        logger.debug('imandra_api_key is None, setting from env and default path')
    imandrax_api_key = imandra_api_key or os.getenv('IMANDRAX_API_KEY')

    if not imandrax_api_key:
        # try to read from default config location
        config_path = Path.home() / '.config' / 'imandrax' / 'api_key'
        if config_path.exists():
            imandrax_api_key = config_path.read_text().strip()

    if not imandrax_api_key:
        logger.error('IMANDRAX_API_KEY is None')
        raise ValueError('IMANDRAX_API_KEY is None')
    client = ImandraXClient(url=url, auth_token=imandrax_api_key, timeout=300)
    logger.info('imandrax_client_initialized', env=imandrax_env)
    return client


def get_imandrax_async_client(
    imandra_api_key: str | None = None,
    env: Literal['dev', 'prod'] | None = None,
) -> ImandraXAsyncClient:
    imandrax_env = env or os.getenv('IMANDRAX_ENV', 'prod')
    if imandrax_env == 'dev':
        url = imandrax_api.url_dev
    else:
        url = imandrax_api.url_prod

    if imandra_api_key is None:
        logger.debug('imandra_api_key is None, setting from env and default path')
    imandrax_api_key = imandra_api_key or os.getenv('IMANDRAX_API_KEY')

    if not imandrax_api_key:
        # try to read from default config location
        config_path = Path.home() / '.config' / 'imandrax' / 'api_key'
        if config_path.exists():
            imandrax_api_key = config_path.read_text().strip()

    if not imandrax_api_key:
        logger.error('IMANDRAX_API_KEY is None')
        raise ValueError('IMANDRAX_API_KEY is None')
    client = ImandraXAsyncClient(url=url, auth_token=imandrax_api_key, timeout=300)
    logger.info('imandrax_client_initialized', env=imandrax_env)
    return client
