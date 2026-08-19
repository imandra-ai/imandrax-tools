"""URL/auth resolution for the module-internal ImandraX client — no network."""

import pytest
from imandrax_api import url_dev, url_prod
from imandrax_codegen.gen_src import _connection_kwargs


def test_url_param_is_verbatim_and_keyless(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.delenv('IMANDRAX_URL', raising=False)
    monkeypatch.delenv('IMANDRAX_API_KEY', raising=False)
    kw = _connection_kwargs(None, None, 'http://my-vm:8086')
    assert kw == {'url': 'http://my-vm:8086', 'auth_token': None}


def test_url_from_env(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.setenv('IMANDRAX_URL', 'http://my-vm:8086')
    monkeypatch.delenv('IMANDRAX_API_KEY', raising=False)
    kw = _connection_kwargs(None, None, None)
    assert kw == {'url': 'http://my-vm:8086', 'auth_token': None}


def test_url_with_explicit_key(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.delenv('IMANDRAX_URL', raising=False)
    kw = _connection_kwargs('sekrit', None, 'http://my-vm:8086')
    assert kw['auth_token'] == 'sekrit'


def test_cloud_path_unchanged(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.delenv('IMANDRAX_URL', raising=False)
    monkeypatch.delenv('IMANDRAX_ENV', raising=False)
    assert _connection_kwargs('sekrit', None, None) == {
        'url': url_prod,
        'auth_token': 'sekrit',
    }
    assert _connection_kwargs('sekrit', 'dev', None)['url'] == url_dev


def test_cloud_path_still_requires_key(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.delenv('IMANDRAX_URL', raising=False)
    monkeypatch.delenv('IMANDRAX_API_KEY', raising=False)
    with pytest.raises(KeyError):
        _connection_kwargs(None, None, None)
