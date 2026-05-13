import logging
import time
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


_OTEL_SEVERITY: dict[str, Any] = {}


def _otel_severity_map() -> dict[str, Any]:
    """Lazily build the structlog method-name → OTel SeverityNumber map."""
    global _OTEL_SEVERITY
    if _OTEL_SEVERITY:
        return _OTEL_SEVERITY
    try:
        from opentelemetry._logs import SeverityNumber
    except ImportError:
        return {}
    _OTEL_SEVERITY = {  # pyright: ignore[reportConstantRedefinition]
        'debug': SeverityNumber.DEBUG,
        'info': SeverityNumber.INFO,
        'warning': SeverityNumber.WARN,
        'warn': SeverityNumber.WARN,
        'error': SeverityNumber.ERROR,
        'critical': SeverityNumber.FATAL,
        'exception': SeverityNumber.ERROR,
    }
    return _OTEL_SEVERITY


def _emit_to_open_telemetry(
    logger: Any, method_name: str, event_dict: structlog.types.EventDict
) -> structlog.types.EventDict:
    """
    Mirror each structlog event into the OTel logs pipeline (Logging to otel bridge).

    No-op when opentelemetry-sdk is not installed or no LoggerProvider has
    been configured. Trace correlation is filled in from the currently active
    span so log records show up under the right trace in the backend.

    Returns the event_dict unchanged so downstream renderers (e.g. the console
    renderer for stderr) keep working.
    """
    try:
        from opentelemetry import context as otel_context
        from opentelemetry._logs import get_logger_provider
        from opentelemetry._logs._internal import LogRecord
        from opentelemetry.sdk._logs import LoggerProvider
    except ImportError:
        return event_dict

    provider = get_logger_provider()
    if not isinstance(provider, LoggerProvider):
        return event_dict

    current_ctx = otel_context.get_current()
    severity_number = _otel_severity_map().get(method_name)
    body = event_dict.get('event', '')

    # Attributes: scalar values only; stringify everything else to keep the
    # OTLP payload valid.
    attributes: dict[str, Any] = {}
    for k, v in event_dict.items():
        if k == 'event':
            continue
        if isinstance(v, str | int | float | bool):
            attributes[k] = v
        else:
            attributes[k] = repr(v)

    now = time.time_ns()
    record = LogRecord(
        timestamp=now,
        observed_timestamp=now,
        context=current_ctx,
        severity_text=method_name.upper(),
        severity_number=severity_number,
        body=body,
        attributes=attributes,
    )
    otel_logger = provider.get_logger(getattr(logger, 'name', 'structlog'))
    otel_logger.emit(record)
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
    emit_otel: bool = False,
) -> None:
    """
    Configure structlog.

    Args:
        min_level: minimum level to render to stderr.
        emit_otel: also mirror each event into the OTel logs pipeline. Caller
            must have already installed a ``LoggerProvider`` (e.g. via
            ``trace_utils.configure_otel_logs``); otherwise the processor is a
            no-op at runtime.

    """
    if min_level is None:
        min_level = logging.NOTSET
    elif isinstance(min_level, str):
        min_level = logging.getLevelNamesMapping().get(min_level.upper(), min_level)

    processors: list[structlog.types.Processor] = [
        add_logger_name,
        structlog.contextvars.merge_contextvars,
        structlog.processors.add_log_level,
        add_open_telemetry_spans,
        structlog.processors.StackInfoRenderer(),
        structlog.dev.set_exc_info,
        structlog.processors.TimeStamper(fmt='%m-%d %H:%M:%S', utc=False),
    ]
    if emit_otel:
        processors.append(_emit_to_open_telemetry)
    processors.append(structlog.dev.ConsoleRenderer())

    structlog.configure(
        processors=processors,
        wrapper_class=structlog.make_filtering_bound_logger(min_level),
        context_class=dict,
        logger_factory=NamedPrintLoggerFactory(),
        cache_logger_on_first_use=False,
    )
