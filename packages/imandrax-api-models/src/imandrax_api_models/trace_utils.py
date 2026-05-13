from __future__ import annotations

import os
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
        # For local Jaeger gRPC: OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4317
        if os.getenv('OTEL_EXPORTER_OTLP_ENDPOINT'):
            return OTLPSpanExporter()  # reads endpoint from env automatically
        else:
            return ConsoleSpanExporter()

    current = trace.get_tracer_provider()
    if isinstance(current, TracerProvider):
        return True

    provider = TracerProvider(resource=Resource.create({'service.name': service_name}))
    provider.add_span_processor(BatchSpanProcessor(make_exporter()))
    bag_proc = _make_baggage_span_processor()
    if bag_proc is not None:
        provider.add_span_processor(bag_proc)
    trace.set_tracer_provider(provider)
    return True


def configure_otel_logs(service_name: str = 'imandrax-api-client') -> bool:
    """
    Set up an OTel LoggerProvider that exports log records (Otel to logging bridge).

    If OTEL_EXPORTER_OTLP_ENDPOINT is set, exports to that endpoint via gRPC;
    otherwise, exports to stderr (console).

    Idempotent: subsequent calls are no-ops if a LoggerProvider has already
    been installed by a previous invocation.

    Returns:
       bool: True if OTel logs SDK is installed and configured, False otherwise.

    """
    try:
        from opentelemetry._logs import get_logger_provider, set_logger_provider
        from opentelemetry.exporter.otlp.proto.grpc._log_exporter import (
            OTLPLogExporter,
        )
        from opentelemetry.sdk._logs import LoggerProvider
        from opentelemetry.sdk._logs.export import (
            BatchLogRecordProcessor,
            ConsoleLogRecordExporter,
        )
        from opentelemetry.sdk.resources import Resource
    except ImportError:
        return False

    # The default no-op provider isn't a LoggerProvider instance, so identity
    # via type check tells us whether we've already installed our SDK provider.
    current = get_logger_provider()
    if isinstance(current, LoggerProvider):
        return True

    def make_exporter() -> ConsoleLogRecordExporter | OTLPLogExporter:
        if os.getenv('OTEL_EXPORTER_OTLP_ENDPOINT'):
            return OTLPLogExporter()
        return ConsoleLogRecordExporter()

    provider = LoggerProvider(resource=Resource.create({'service.name': service_name}))
    provider.add_log_record_processor(BatchLogRecordProcessor(make_exporter()))
    set_logger_provider(provider)
    return True


def _make_baggage_span_processor() -> SpanProcessor | None:
    """Copy every OTel baggage entry onto each starting span as an attribute."""
    try:
        from opentelemetry import baggage
        from opentelemetry.sdk.trace import SpanProcessor
    except ImportError:
        return None

    class BaggageSpanProcessor(SpanProcessor):
        def on_start(self, span: Any, parent_context: Any = None) -> None:
            for k, v in baggage.get_all(parent_context).items():
                span.set_attribute(k, str(v))

        def on_end(self, span: Any) -> None:
            pass

        def shutdown(self) -> None:
            pass

        def force_flush(self, timeout_millis: int = 30000) -> bool:
            return True

    return BaggageSpanProcessor()
