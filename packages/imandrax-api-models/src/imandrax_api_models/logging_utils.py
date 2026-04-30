import logging
from typing import Any, Literal

import structlog


class NamedPrintLoggerFactory(structlog.PrintLoggerFactory):
    def __call__(self, *args: Any) -> Any:
        logger = super().__call__(*args)
        # Store the name (first argument) on the logger
        setattr(logger, 'name', args[0] if args else 'root')
        return logger


def add_logger_name(
    logger: Any, method_name: str, event_dict: structlog.types.EventDict
):
    if hasattr(logger, 'name'):
        event_dict['logger'] = logger.name
    return event_dict


def add_open_telemetry_spans(
    _: Any, __: str, event_dict: structlog.types.EventDict
) -> structlog.types.EventDict:
    """
    Enrich log events with the active OpenTelemetry span/trace IDs.

    No-op when opentelemetry-api is not installed or no span is recording.
    """
    try:
        from opentelemetry import trace
    except ImportError:
        return event_dict

    span = trace.get_current_span()
    if not span.is_recording():
        return event_dict

    ctx = span.get_span_context()
    parent = getattr(span, 'parent', None)
    event_dict['span'] = {
        'span_id': format(ctx.span_id, '016x'),
        'trace_id': format(ctx.trace_id, '032x'),
        'parent_span_id': None if not parent else format(parent.span_id, '016x'),
    }
    return event_dict


def configure_logging(
    min_level: Literal[
        'debug',
        'info',
        'warning',
        'error',
        'critical',
    ]
    | int
    | None = None,
):
    if min_level is None:
        min_level = logging.NOTSET
    elif isinstance(min_level, str):
        min_level = logging.getLevelNamesMapping().get(min_level.upper(), min_level)

    structlog.configure(
        processors=[
            add_logger_name,
            structlog.contextvars.merge_contextvars,
            structlog.processors.add_log_level,
            add_open_telemetry_spans,
            structlog.processors.StackInfoRenderer(),
            structlog.dev.set_exc_info,
            structlog.processors.TimeStamper(fmt='%m-%d %H:%M:%S', utc=False),
            structlog.dev.ConsoleRenderer(),
        ],
        wrapper_class=structlog.make_filtering_bound_logger(min_level),
        context_class=dict,
        logger_factory=NamedPrintLoggerFactory(),
        cache_logger_on_first_use=False,
    )


def configure_otel_console(service_name: str = 'imandrax-api-client') -> bool:
    """
    Set up an OTel TracerProvider that exports spans to stderr.

    Returns True if OTel SDK is installed and configured, False otherwise.
    Safe to call multiple times — subsequent calls are no-ops if a provider
    is already set.
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

    import os

    def make_exporter() -> ConsoleSpanExporter | OTLPSpanExporter:
        if os.getenv('OTEL_EXPORTER_OTLP_ENDPOINT'):
            return OTLPSpanExporter()  # reads endpoint from env automatically
        return ConsoleSpanExporter()  # fallback for local dev

    current = trace.get_tracer_provider()
    if isinstance(current, TracerProvider):
        return True

    provider = TracerProvider(resource=Resource.create({'service.name': service_name}))
    provider.add_span_processor(BatchSpanProcessor(make_exporter()))
    trace.set_tracer_provider(provider)
    return True
