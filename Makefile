EMACS ?= emacs
CASK ?= cask

all: test

test:
	$(CASK) exec $(EMACS) -batch -Q -L . \
		-l tests/run-tests.el \
		-f ert-run-tests-batch-and-exit
