# pyright: reportPrivateUsage=false
"""
_

- URL precedence: `url` arg > $IMANDRAX_URL > `env` arg > $IMANDRAX_ENV > disk config
- Key precedence: `auth_token` > $IMANDRAX_API_KEY > disk config
- Cloud URL without a key gives a `ValueError`
- Self-hosted URL gets only an explicit `auth_token`, never the key from env or disk config
"""

import pytest
from imandrax_api import url_dev, url_prod
from imandrax_api_models.client import (
    _is_self_hosted_url,
    get_imandrax_client,
    get_imandrax_url,
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
    assert _is_self_hosted_url('http://my-vm:8086')
    assert resolve_connection(None, None, 'http://my-vm:8086') == (
        'http://my-vm:8086',
        None,
    )


def test_url_from_env_is_keyless(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.setenv('IMANDRAX_URL', 'http://my-vm:8086')
    assert get_imandrax_url() == 'http://my-vm:8086'
    assert _is_self_hosted_url(get_imandrax_url())
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
    assert not _is_self_hosted_url(url_prod)
    assert not _is_self_hosted_url(url_dev)
    assert resolve_connection('sekrit') == (url_prod, 'sekrit')
    assert resolve_connection('sekrit', 'dev') == (url_dev, 'sekrit')


def test_cloud_path_still_requires_a_key():
    with pytest.raises(ValueError, match='IMANDRAX_API_KEY'):
        resolve_connection()
    with pytest.raises(ValueError, match='IMANDRAX_API_KEY'):
        get_imandrax_client(env='prod')


@pytest.mark.parametrize(
    'cloud_url', [url_prod, url_dev, url_prod.rstrip('/'), url_dev.rstrip('/')]
)
def test_cloud_url_given_explicitly_still_requires_a_key(
    monkeypatch: pytest.MonkeyPatch, cloud_url: str
):
    with pytest.raises(ValueError, match='IMANDRAX_API_KEY'):
        resolve_connection(None, None, cloud_url)
    monkeypatch.setenv('IMANDRAX_URL', cloud_url)
    with pytest.raises(ValueError, match='IMANDRAX_API_KEY'):
        resolve_connection()


def test_self_hosted_never_gets_the_ambient_key(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.setattr(
        'imandrax_api_models.client.get_imandrax_api_key', lambda: 'cloud-key'
    )
    assert resolve_connection(None, None, 'http://my-vm:8086') == (
        'http://my-vm:8086',
        None,
    )
    monkeypatch.setenv('IMANDRAX_URL', 'http://my-vm:8086')
    assert resolve_connection() == ('http://my-vm:8086', None)
    # the cloud still picks it up
    assert resolve_connection(None, 'prod', url_prod) == (url_prod, 'cloud-key')


def test_unknown_env_is_a_clear_error(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.setenv('IMANDRAX_ENV', 'staging')
    with pytest.raises(ValueError, match="'staging'"):
        get_imandrax_url()
    with pytest.raises(ValueError, match="'Prod'"):
        get_imandrax_url('Prod')  # pyright: ignore[reportArgumentType]


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
