"""
Trace-related tests for client.py, hitting the real API and capturing spans via an in-memory OTel exporter.

Expected behaviors:
- Always: Each call has session id
- Multiple calls of the same session has the same session id
- It's possible to have multiple sessions
"""

import json
from collections.abc import Iterable, Iterator
from typing import Any

import dotenv
import pytest
from dirty_equals import IsStr
from opentelemetry import trace as otel_trace
from opentelemetry.sdk.trace import ReadableSpan, TracerProvider
from opentelemetry.sdk.trace.export import SimpleSpanProcessor
from opentelemetry.sdk.trace.export.in_memory_span_exporter import (
    InMemorySpanExporter,
)

from imandrax_api_models.client import (
    ImandraXClient,
    _client_session_id,  # pyright: ignore[reportPrivateUsage]
    get_imandrax_client,
)

dotenv.load_dotenv()


# Dirty-equals matchers for unstable fields
# =========================================

IsTraceId = IsStr(regex=r'0x[0-9a-f]{32}')
IsSpanId = IsStr(regex=r'0x[0-9a-f]{16}')
IsIsoTime = IsStr(regex=r'\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d+Z')
IsUuid = IsStr(regex=r'[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}')


def dict_of_spans(spans: Iterable[ReadableSpan]) -> list[dict[str, Any]]:
    return [json.loads(span.to_json()) for span in spans]


def span_session_id(span: ReadableSpan) -> str | None:
    v = (span.attributes or {}).get('imandrax.session.id')
    return v if isinstance(v, str) else None


# OTel in-memory capture
# ======================


@pytest.fixture(scope='module')
def span_exporter() -> Iterator[InMemorySpanExporter]:
    """
    Install an InMemorySpanExporter as the global TracerProvider.

    The module-level `tracer` in `trace_utils` is a ProxyTracer when no SDK
    provider was set at import time, so it picks up this provider lazily.
    """
    exporter = InMemorySpanExporter()
    provider = TracerProvider()
    provider.add_span_processor(SimpleSpanProcessor(exporter))
    otel_trace.set_tracer_provider(provider)
    try:
        yield exporter
    finally:
        provider.shutdown()


@pytest.fixture
def clear_spans(span_exporter: InMemorySpanExporter) -> InMemorySpanExporter:
    span_exporter.clear()
    return span_exporter


@pytest.fixture(scope='module')
def client_a() -> ImandraXClient:
    return get_imandrax_client()


@pytest.fixture(scope='module')
def client_b() -> ImandraXClient:
    return get_imandrax_client()


def test_each_call_has_session_id(
    client_a: ImandraXClient, clear_spans: InMemorySpanExporter
) -> None:
    client_a.eval_src('let x = 1')
    spans = clear_spans.get_finished_spans()
    assert len(spans) == 1
    sesh_id = span_session_id(spans[0])
    assert sesh_id is not None
    assert sesh_id == _client_session_id(client_a)


def test_multiple_calls_same_session_share_id(
    client_a: ImandraXClient, clear_spans: InMemorySpanExporter
) -> None:
    client_a.eval_src('let a = 1')
    client_a.eval_src('let b = 2')
    client_a.typecheck('let c = 3')
    spans = clear_spans.get_finished_spans()
    assert len(spans) == 3
    sesh_ids = [span_session_id(span) for span in spans]
    assert all(sesh_id is not None for sesh_id in sesh_ids)
    for i, sesh_id in enumerate(sesh_ids):
        assert sesh_id == _client_session_id(client_a), (
            f'span {i} has sesh_id {sesh_id}, expected {_client_session_id(client_a)}'
        )


def test_multiple_sessions_have_distinct_ids(
    client_a: ImandraXClient,
    client_b: ImandraXClient,
    clear_spans: InMemorySpanExporter,
) -> None:
    client_a.eval_src('let a = 1')
    client_b.eval_src('let b = 2')
    spans = clear_spans.get_finished_spans()
    assert len(spans) == 2
    sid_a = _client_session_id(client_a)
    sid_b = _client_session_id(client_b)
    assert sid_a != sid_b
    assert span_session_id(spans[0]) == sid_a
    assert span_session_id(spans[1]) == sid_b
