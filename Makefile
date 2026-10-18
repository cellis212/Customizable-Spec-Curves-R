.PHONY: setup check demo manuscript restore

setup:
	git config core.hooksPath .githooks
	Rscript paper/run.R check

restore:
	Rscript paper/run.R restore

check:
	python3 -m unittest discover -s tools -p 'test_*.py'
	Rscript paper/tests/test_curve.R
	python3 tools/publication_policy.py

demo:
	Rscript paper/run.R demo

manuscript:
	cd paper/manuscript && quarto render paper.qmd
