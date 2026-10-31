VITE ?= vite --configLoader native
WATCH_PORT ?= 5173

.PHONY: help
help: ## Show available targets
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

# ── Submodules & Symlinks ────────────────────────────────────────────────────

.PHONY: vendor
vendor: ## Init and update submodules and configure symlinks
	@[ -z "$$CI" ] || git config --global url."https://github.com/".insteadOf "git@github.com:"
	@if [ -d .git ]; then git submodule update --init --recursive; fi
	ln -sfn vendor/brick-studio/vendor/master-builder/packages packages
	mkdir -p review/src
	ln -sfn ../../vendor/brick-studio/vendor/master-builder/review/src/LlmAgent review/src/LlmAgent

# ── Development & Build ──────────────────────────────────────────────────────

SHARED_ELM_SOURCES := $(shell find vendor/brick-studio/packages/ -name '*.elm' 2>/dev/null)
PLAY_SOURCES := $(shell find src -name '*.elm') $(SHARED_ELM_SOURCES)
ELM_PACKAGE_SOURCES := $(shell find packages/ -name '*.elm' -o -name '*.css' 2>/dev/null)

.PHONY: elm-tailwind-gen
elm-tailwind-gen: .elm-tailwind/.stamp ## Generate typed Tailwind Elm modules

.elm-tailwind/.stamp: elm.json vite.config.mjs main.css $(PLAY_SOURCES) $(ELM_PACKAGE_SOURCES)
	elm-tailwind-classes gen
	mkdir -p .elm-tailwind
	touch $@

.PHONY: dev
dev: elm-tailwind-gen ## Start Vite dev server (http://localhost:5173)
	$(VITE) --host 0.0.0.0 --port $(WATCH_PORT)

build/.stamp: elm-tailwind-gen index.html main.js main.css vite.config.mjs $(PLAY_SOURCES) $(ELM_PACKAGE_SOURCES)
	mkdir -p build
	$(VITE) build --outDir build
	touch $@

.PHONY: build
build: build/.stamp ## Production build of brick-play → build/

.PHONY: preview
preview: build ## Preview production build locally
	$(VITE) preview --outDir build --host 0.0.0.0 --port 4173

# ── Test & Quality ───────────────────────────────────────────────────────────

.PHONY: test
test: elm-tailwind-gen ## Run all unit tests
	elm-test

.PHONY: check
check: ## Validate Elm formatting and review rules
	elm-format --validate src/ tests/
	elm-review

.PHONY: format
format: ## Auto-format Elm source files
	elm-format --yes src/ tests/
	treefmt

.PHONY: clean
clean: ## Remove build artifacts
	rm -rf build .elm-tailwind elm-stuff .vite
