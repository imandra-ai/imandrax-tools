from typing import Any

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
        return {'len': len(value), 'items': value[:8]}
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
