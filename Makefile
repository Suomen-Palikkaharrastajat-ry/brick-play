VITE ?= vite --configLoader native
WATCH_PORT ?= 5175
PREVIEW_PORT ?= 4175

.DEFAULT_GOAL := help

.PHONY: help
help: ## Show available targets
	@grep -hE '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; !seen[$$1]++ {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

# ── Vendor / submodules ──────────────────────────────────────────────────────

.PHONY: vendor
vendor: ## Init and update all git submodules and configure symlinks
	@[ -z "$$CI" ] || git config --global url."https://github.com/".insteadOf "git@github.com:"
	@if [ -d .git ]; then \
		if [ -z "$$CI" ] && [ -d ../brick-studio/.git ] && [ -d ../master-builder/.git ]; then \
			git -c submodule."vendor/brick-studio".url="$(abspath ../brick-studio)" \
			    -c protocol.file.allow=always submodule update --init && \
			git -C vendor/brick-studio \
			    -c submodule."vendor/master-builder".url="$(abspath ../master-builder)" \
			    -c protocol.file.allow=always submodule update --init; \
		else \
			git submodule update --init --recursive; \
		fi; \
	fi
	ln -sfn vendor/brick-studio/vendor/master-builder/packages packages
	mkdir -p review/src
	ln -sfn ../../vendor/brick-studio/vendor/master-builder/review/src/LlmAgent review/src/LlmAgent

# ── Development environment ──────────────────────────────────────────────────

.PHONY: shell
shell: ## Enter devenv shell
	devenv shell

.PHONY: develop
develop: devenv.local.nix devenv.local.yaml ## Bootstrap devenv shell + VS Code
	devenv shell --profile=devcontainer -- code .

# ── Sources ──────────────────────────────────────────────────────────────────

SHARED_ELM_SOURCES := $(shell find vendor/brick-studio/packages/ -name '*.elm' 2>/dev/null)
PLAY_SOURCES := $(shell find src -name '*.elm') $(SHARED_ELM_SOURCES)
ELM_PACKAGE_SOURCES := $(shell find packages/ -name '*.elm' -o -name '*.css' 2>/dev/null)
JS_TESTS := $(wildcard *.test.mjs tests/*.test.mjs)

# ── Development & Build ──────────────────────────────────────────────────────

.PHONY: elm-tailwind-gen
elm-tailwind-gen: .elm-tailwind/.stamp ## Generate typed Tailwind Elm modules

.elm-tailwind/.stamp: elm.json vite.config.mjs main.css $(PLAY_SOURCES) $(ELM_PACKAGE_SOURCES)
	elm-tailwind-classes gen
	mkdir -p .elm-tailwind
	touch $@

.PHONY: play-dev
play-dev: elm-tailwind-gen ## Start the play Vite dev server (http://localhost:WATCH_PORT)
	$(VITE) --host 0.0.0.0 --port $(WATCH_PORT)

.PHONY: dev
dev: play-dev ## Start Vite dev server (WATCH_PORT)

.PHONY: watch
watch: dev ## Alias for dev (start Vite dev server on WATCH_PORT)

build/.stamp: elm-tailwind-gen index.html main.js main.css vite.config.mjs $(PLAY_SOURCES) $(ELM_PACKAGE_SOURCES)
	mkdir -p build
	$(VITE) build --outDir build
	touch $@

.PHONY: play-build
play-build: build/.stamp ## Production build of brick-play → build/

.PHONY: build
build: play-build ## Build brick-play

.PHONY: all
all: clean build ## Clean and rebuild everything

.PHONY: preview
preview: build ## Preview production build locally (PREVIEW_PORT)
	$(VITE) preview --outDir build --host 0.0.0.0 --port $(PREVIEW_PORT)

.PHONY: serve
serve: preview ## Alias for preview

# ── Test & Quality ───────────────────────────────────────────────────────────

.PHONY: elm-test
elm-test: elm-tailwind-gen ## Run all Elm unit tests
	elm-test

.PHONY: js-test
js-test: ## Run JS unit tests (*.test.mjs)
	$(if $(JS_TESTS),node --test $(JS_TESTS),@echo "No JS unit tests in brick-play")

.PHONY: test
test: check elm-test js-test ## Run check, Elm tests and JS tests

.PHONY: elm-check
elm-check: ## Check Elm formatting (no changes)
	elm-format --validate src/ tests/

.PHONY: elm-format
elm-format: ## Auto-format Elm source files
	elm-format --yes src/ tests/

.PHONY: elm-review
elm-review: ## Run elm-review rules
	elm-review

.PHONY: review
review: elm-review ## Alias for elm-review

.PHONY: elm-review-fix
elm-review-fix: ## Run elm-review with auto-fix
	elm-review --fix

.PHONY: review-fix
review-fix: elm-review-fix ## Alias for elm-review-fix

.PHONY: check
check: elm-check elm-review ## Validate Elm formatting and review rules

.PHONY: format
format: elm-format ## Auto-format source files
	treefmt

.PHONY: fmt
fmt: format ## Alias for format

# ── Cleanup ───────────────────────────────────────────────────────────────────

.PHONY: clean
clean: ## Remove build artifacts
	rm -rf build .elm-tailwind elm-stuff .vite
