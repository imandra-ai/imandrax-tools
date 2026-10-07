"""
How a client's URL and API key are resolved - no network.

A URL given explicitly (argument or $IMANDRAX_URL) is a self-hosted ImandraX: unauthenticated, so no key is
required. Imandra's cloud (chosen by `env` / $IMANDRAX_ENV) keeps requiring one.
"""

import pytest
from imandrax_api import url_dev, url_prod
from imandrax_api_models.client import (
    get_imandrax_client,
    get_imandrax_url,
    is_self_hosted_url,
    resolve_connection,
)


@pytest.fixture(autouse=True)
def _no_ambient_config(monkeypatch: pytest.MonkeyPatch):
    for v in ('IMANDRAX_URL', 'IMANDRAX_API_KEY', 'IMANDRAX_ENV'):
        monkeypatch.delenv(v, raising=False)
    # a key in ~/.config/imandrax/api_key must not make the cloud tests pass by accident
    monkeypatch.setattr('imandrax_api_models.client.get_imandrax_api_key', lambda: None)


def test_url_argument_wins_and_is_keyless():
    assert get_imandrax_url(None, 'http://my-vm:8086') == 'http://my-vm:8086'
    assert is_self_hosted_url('http://my-vm:8086')
    assert resolve_connection(None, None, 'http://my-vm:8086') == (
        'http://my-vm:8086',
        None,
    )


def test_url_from_env_is_keyless(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.setenv('IMANDRAX_URL', 'http://my-vm:8086')
    assert get_imandrax_url() == 'http://my-vm:8086'
    assert is_self_hosted_url()
    assert resolve_connection() == ('http://my-vm:8086', None)


def test_url_argument_beats_env(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.setenv('IMANDRAX_URL', 'http://other:8086')
    assert resolve_connection(None, None, 'http://my-vm:8086')[0] == 'http://my-vm:8086'


def test_self_hosted_keeps_an_explicit_key():
    assert resolve_connection('sekrit', None, 'http://my-vm:8086') == (
        'http://my-vm:8086',
        'sekrit',
    )


def test_cloud_path_unchanged():
    assert not is_self_hosted_url()
    assert resolve_connection('sekrit') == (url_prod, 'sekrit')
    assert resolve_connection('sekrit', 'dev') == (url_dev, 'sekrit')


def test_cloud_path_still_requires_a_key():
    with pytest.raises(ValueError, match='IMANDRAX_API_KEY'):
        resolve_connection()
    with pytest.raises(ValueError, match='IMANDRAX_API_KEY'):
        get_imandrax_client(env='prod')


@pytest.fixture
def made(monkeypatch: pytest.MonkeyPatch) -> dict:
    """What `get_imandrax_client` constructs - the client opens a session on construction, so a stub stands in."""
    seen: dict = {}

    class Stub:
        def __init__(self, **kwargs):
            seen.update(kwargs)

    monkeypatch.setattr('imandrax_api_models.client.ImandraXClient', Stub)
    return seen


def test_client_for_a_self_hosted_url_without_a_key(made: dict):
    get_imandrax_client(url='http://my-vm:8086')
    assert made['url'] == 'http://my-vm:8086'
    assert made['auth_token'] is None


def test_client_for_a_self_hosted_url_with_a_key(made: dict):
    get_imandrax_client(auth_token='sekrit', url='http://my-vm:8086')
    assert made == {
        'url': 'http://my-vm:8086',
        'auth_token': 'sekrit',
        'session_id': None,
        'create_if_not_found': False,
    }
