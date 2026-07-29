.PHONY: check check-gopogh run-tests json-tests html-page serve-tests clean fixtures

ifeq ($(OS),Windows_NT)
    BROWSER_OPEN = start
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Darwin)
        BROWSER_OPEN = open
    else
        BROWSER_OPEN = xdg-open
    endif
endif

TEXMACS_NCLIENTS ?= 1
TEXMACS_INSTANCES ?= localhost:6561
TESTS ?= '(TestLoad|TestServices)'
GOTESTFLAGS := -timeout 30s -v -shuffle on \
               -run $(TESTS) \
               -nclients $(TEXMACS_NCLIENTS) \
               -instances "$(TEXMACS_INSTANCES)"
HELPERS := $(wildcard fixture/helpers/*.scm)
TESTDATA := fixture/testdata/generate-cert.scm \
            fixture/testdata/admin.scm
            #fixture/testdata/accounts-and-resources.scm
SCENARIOS := $(wildcard scenarios/*.tmpl.scm)
JSON_TESTS=out.json
HTML_PAGE=$(JSON_TESTS:.json=.html)
GOPOGH=gopogh
GO=go

all: check

check-gopogh:
	@command -v $(GOPOGH) >/dev/null 2>&1 || \
		{ \
			echo >&2 "$(GOPOGH) not found. Please install it:"; \
			echo >&2 "go install github.com/medyagh/gopogh/cmd/gopogh@latest"; \
			exit 1; \
        }

check: run-tests

$(JSON_TESTS): $(SCENARIOS)
	$(GO) test -json $(GOTESTFLAGS) > $@

%.html: %.json
	$(GOPOGH) -in $< -out_html $@ -details "TeXmacs server scenario run"

run-tests:
	$(GO) test $(GOTESTFLAGS)

json-tests: $(JSON_TESTS)

html-page: $(HTML_PAGE)

serve-tests: check-gopogh $(HTML_PAGE)
	$(BROWSER_OPEN) $(HTML_PAGE)

my-init-texmacs.scm: $(HELPERS) $(TESTDATA)
	cat $^ > $@

certs:
	mkdir -p certs

fixtures: my-init-texmacs.scm certs

clean:
	$(RM) $(JSON_TESTS) $(HTML_PAGE)
	$(RM) -r debug
	$(RM) -r certs
	$(RM) my-init-texmacs.scm
