include .env

PORT ?= 5648
PG_HOST ?= localhost
PG_PORT ?= 5432
PG_USER ?= myusername
PG_PASS ?= mypassword
PG_DB ?= musicgen

LISP=sbcl

PYTHON=python3
UTILITIES=utilities
TESTS=tests
VENV=venv
PIP=$(VENV)/bin/pip
PYTHON_EXEC=$(VENV)/bin/python
SHELL := /bin/sh
RM = $(if $(filter Windows_NT,$(OS)),powershell -Command "if (Test-Path '*.fasl') { Remove-Item -Force *.fasl }; if (Test-Path 'melody.wav') { Remove-Item -Force melody.wav }",rm -f)

API_BASE_URL=http://localhost:$(PORT)/melodies
API_SIMPLE_TUNE=http://localhost:$(PORT)/simple-tune

ifeq ($(OS),Windows_NT)
    PIP=$(VENV)/Scripts/pip
	PYTHON_EXEC=$(VENV)/Scripts/python
endif

.PHONY: run-tests generate-audio generate-audio-cli clean setup-env install-deps run-server install-lisp-deps create-db init-db install-quicklisp

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


create-db:
	PGPASSWORD=$(PG_PASS) psql -U $(PG_USER) -d postgres -c "CREATE DATABASE $(PG_DB);"

init-db:
	PGPASSWORD=$(PG_PASS) psql -U $(PG_USER) -d $(PG_DB) -c "CREATE TABLE IF NOT EXISTS melodies (id SERIAL PRIMARY KEY, root_frequency FLOAT NOT NULL CHECK (root_frequency > 0), melody TEXT NOT NULL);"


install-quicklisp:
	curl -O https://beta.quicklisp.org/quicklisp.lisp
	sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql:add-to-init-file)" --eval "(quit)"

install-lisp-deps:
	sbcl --eval "(ql:quickload '(:hunchentoot :postmodern :cl-json))" --eval "(quit)"

run-server: install-lisp-deps
	PORT=$(PORT) PG_HOST=$(PG_HOST) PG_PORT=$(PG_PORT) PG_DB=$(PG_DB) PG_USER=$(PG_USER) PG_PASS=$(PG_PASS) $(LISP) --script src/web-api.lisp


# API TESTS (CRUD)

API_ROOT = http://localhost:$(PORT)

# GET /simple-tune/<root>
test-api-root:
	curl -X GET $(API_ROOT)/yo \
		-w "\nHTTP Status: %{http_code}\n" \
		-o /dev/null

# POST /simple-tune/<root>
test-simple-tune-gen:
	curl -X GET $(API_SIMPLE_TUNE)/440 \
		-w "\nHTTP Status: %{http_code}\n" \
		-o /dev/null

# POST /melodies
test-post:
	curl -X POST $(API_BASE_URL) \
		-H "Content-Type: application/json" \
		-d '{"root_frequency": 440, "melody": "440.0,493.88,523.25"}' \
		-w "\nHTTP Status: %{http_code}\n" \
		-o /dev/null

# GET /melodies/<id>
test-get-one:
	curl -X GET $(API_BASE_URL)/1 \
		-w "\nHTTP Status: %{http_code}\n" \
		-o /dev/null

# GET /melodies
test-get-many:
	curl -X GET $(API_BASE_URL) \
		-w "\nHTTP Status: %{http_code}\n" \
		-o /dev/null

# PUT /melodies/<id>
test-update:
	curl -X PUT $(API_BASE_URL)/1 \
		-H "Content-Type: application/json" \
		-d '{"root_frequency": 440, "melody": "440.0,493.88,523.25"}' \
		-w "\nHTTP Status: %{http_code}\n" \
		-o /dev/null

# DELETE /melodies/<id>
test-delete:
	curl -X DELETE $(API_BASE_URL)/1 \
		-w "\nHTTP Status: %{http_code}\n" \
		-o /dev/null

