from __future__ import annotations

import os
from contextvars import ContextVar
from typing import TYPE_CHECKING, Any, cast

if TYPE_CHECKING:
    from opentelemetry.sdk.trace import SpanProcessor

# Soft-import OpenTelemetry: when installed, each API call becomes a span;
# when not, the structlog debug logging still works.
otel_trace: Any = None
tracer: Any = None
try:
    from opentelemetry import trace as _otel_trace_imp

    otel_trace = _otel_trace_imp
    tracer = otel_trace.get_tracer('imandrax_api_models.client')
except ImportError:
    pass


# Session id propagation
# ----------------------
session_id_var: ContextVar[str | None] = ContextVar('imandrax_session_id', default=None)
"""
Set by the client on enter, read by the SessionIdSpanProcessor (registered in
logging_utils.configure_otel_console) so every span emitted while a client is
active is tagged with `imandrax.session.id`.
"""


def make_session_id_span_processor() -> SpanProcessor | None:
    """
    Build a SpanProcessor that tags every started span with the active session id.

    Returns None if the OTel SDK is not installed.
    """
    try:
        from opentelemetry.sdk.trace import SpanProcessor
    except ImportError:
        return None

    class SessionIdSpanProcessor(SpanProcessor):
        def on_start(self, span: Any, parent_context: Any = None) -> None:
            sid = session_id_var.get()
            if sid is not None:
                span.set_attribute('imandrax.session.id', sid)

        def on_end(self, span: Any) -> None:
            pass

        def shutdown(self) -> None:
            pass

        def force_flush(self, timeout_millis: int = 30000) -> bool:
            return True

    return SessionIdSpanProcessor()


# ## EOS

_SRC_PREVIEW_LEN = 200


def summarize(name: str, value: Any) -> Any:
    """Compact a value for logging/spans. Long strings/lists are truncated."""
    if isinstance(value, str):
        if len(value) > _SRC_PREVIEW_LEN:
            return {
                'len': len(value),
                'preview': value[:_SRC_PREVIEW_LEN] + '…',
            }
        return value
    if isinstance(value, list):
        value = cast(list[Any], value)
        items = cast(list[Any], value[:8])  # ty: ignore[redundant-cast]
        return {'len': len(value), 'items': items}
    return value


def set_span_attrs(span: Any, fields: dict[str, Any]) -> None:
    if span is None:
        return
    for k, v in fields.items():
        s = summarize(k, v)
        if isinstance(s, str | int | float | bool) or s is None:
            span.set_attribute(f'imandrax.{k}', s if s is not None else '')
        else:
            # dicts/lists — stringify for span attribute
            span.set_attribute(f'imandrax.{k}', repr(s))


def configure_otel_export(service_name: str = 'imandrax-api-client') -> bool:
    """
    Set up an OTel TracerProvider that exports spans to stderr.

    If OTEL_EXPORTER_OTLP_ENDPOINT is set, exports to that endpoint via gRPC;
    otherwise, exports to stderr (console).

    Idempotent: subsequent calls are no-ops if a provider is already set.

    Returns:
       bool: True if OTel SDK is installed and configured, False otherwise.

    """
    try:
        from opentelemetry import trace
        from opentelemetry.exporter.otlp.proto.grpc.trace_exporter import (
            OTLPSpanExporter,
        )
        from opentelemetry.sdk.resources import (
            Resource,
        )
        from opentelemetry.sdk.trace import (
            TracerProvider,
        )
        from opentelemetry.sdk.trace.export import (
            BatchSpanProcessor,
            ConsoleSpanExporter,
        )
    except ImportError:
        return False

    def make_exporter() -> ConsoleSpanExporter | OTLPSpanExporter:
        if os.getenv('OTEL_EXPORTER_OTLP_ENDPOINT'):
            return OTLPSpanExporter()  # reads endpoint from env automatically
        else:
            return ConsoleSpanExporter()

    current = trace.get_tracer_provider()
    if isinstance(current, TracerProvider):
        return True

    provider = TracerProvider(resource=Resource.create({'service.name': service_name}))
    provider.add_span_processor(BatchSpanProcessor(make_exporter()))

    session_proc = make_session_id_span_processor()
    if session_proc is not None:
        provider.add_span_processor(session_proc)

    trace.set_tracer_provider(provider)
    return True
