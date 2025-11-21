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
