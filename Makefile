LISP=sbcl

.PHONY: run-tests

run-tests:
	$(LISP) --script tests/test-music-gen.lisp
