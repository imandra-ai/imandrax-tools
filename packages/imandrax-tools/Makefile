.PHONY: build
build:
	rm -rf ./dist/imandrax_tools-*
	uv build --no-sources

.PHONY: test-wheel
test-wheel:
	uv run --isolated --no-project --with ./dist/imandrax_tools-*.whl tests/smoke_test.py

.PHONY: test-source-dist
test-source-dist:
	uv run --isolated --no-project --with ./dist/imandrax_tools-*.tar.gz tests/smoke_test.py

.PHONY: pre-release-test
pre-release-test:
	make build
	make test-wheel
	make test-source-dist

.PHONY: publish-testpypi
publish-testpypi: pre-release-test
	uv publish \
	--publish-url https://test.pypi.org/legacy/ \
	-u __token__ \
	-p $$(gcloud secrets versions access --project imandra-dev --secret pypi-test-imandrax-api-api-token latest) \
	./dist/imandrax_tools-*

.PHONY: publish-pypi
publish-pypi: pre-release-test
	uv publish \
	--publish-url https://upload.pypi.org/legacy/ \
	-u __token__ \
	-p $$(gcloud secrets versions access --project imandra-dev --secret pypi-imandrax-api-api-token latest) \
	./dist/imandrax_tools-*
