.PHONY: setup check demo restore python-env

PYTHON ?= $(if $(wildcard python/.venv/bin/python),python/.venv/bin/python,python3)

setup:
	git config core.hooksPath .githooks
	Rscript paper/run.R check

restore:
	Rscript paper/run.R restore

check:
	python3 -m unittest discover -s tools -p 'test_*.py'
	Rscript paper/tests/test_curve.R
	$(PYTHON) -m unittest discover -s python/tests
	python3 tools/publication_policy.py

python-env:
	uv venv python/.venv --python 3.11 && uv pip install --python python/.venv/bin/python -e python

demo:
	Rscript paper/run.R demo
