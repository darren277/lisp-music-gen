LISP=sbcl

PYTHON=python3
UTILITIES=utilities
TESTS=tests
VENV=venv
PIP=$(VENV)/bin/pip
PYTHON_EXEC=$(VENV)/bin/python
SHELL := /bin/sh
RM = $(if $(filter Windows_NT,$(OS)),powershell -Command "if (Test-Path '*.fasl') { Remove-Item -Force *.fasl }; if (Test-Path 'melody.wav') { Remove-Item -Force melody.wav }",rm -f)

ifeq ($(OS),Windows_NT)
    PIP=$(VENV)/Scripts/pip
	PYTHON_EXEC=$(VENV)/Scripts/python
endif

.PHONY: run-tests generate-audio generate-audio-cli clean setup-env install-deps

run-tests:
	$(LISP) --script tests/test-music-gen.lisp

setup-env:
	$(PYTHON) -m venv $(VENV)

install-deps: setup-env
	$(PIP) install -r requirements.txt

generate-audio-cli:
	$(PYTHON_EXEC) $(UTILITIES)/generate_audio.py 261.63 293.66 329.63 349.23

generate-audio: install-deps
	$(LISP) --script $(TESTS)/test-music-gen.lisp | $(PYTHON_EXEC) $(UTILITIES)/generate_audio.py

clean:
	@echo "Cleaning up generated files..."
	-$(RM)
	@echo "Cleanup complete."
