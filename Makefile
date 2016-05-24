VERSION ?= $(shell grep '^version:' oden.cabal | sed -Ee 's/^version\:(.+)$$/\1/' | xargs)
OS=$(shell tools/get_os.sh)

STACK_DIST_DIR=$(shell stack path --dist-dir)
STACK_ODEN_EXE=$(STACK_DIST_DIR)/build/oden-exe/oden-exe

DIST_NAME=oden-$(VERSION)-$(OS)
DIST_ARCHIVE=build/$(DIST_NAME).tar.gz

TEST_FLAG=
ifneq ($(strip $(TEST_PATTERN)),)
	TEST_FLAG=-m "$(TEST_PATTERN)"
endif

IMPORTER_SRC=$(shell find go/src/oden -name '*.go')

NODEMON=node_modules/.bin/nodemon
GITBOOK=node_modules/.bin/gitbook

LIBRARY_PATH_VAR=LD_LIBRARY_PATH=build/lib:$(LD_LIBRARY_PATH)
ifeq ($(OS),osx)
	LIBRARY_PATH_VAR=DYLD_LIBRARY_PATH=build/lib:$(DYLD_LIBRARY_PATH)
endif

dist: $(DIST_ARCHIVE)

$(STACK_ODEN_EXE):
	stack build

build/oden: $(STACK_ODEN_EXE)
	@mkdir -p build/oden/bin
	cp README.md build/oden/README.txt
	cp LICENSE.md build/oden/LICENSE.txt
	cp CREDITS.md build/oden/CREDITS.txt
	cp CHANGELOG.md build/oden/CHANGELOG.txt
	cp $(STACK_ODEN_EXE) build/oden/bin/oden-exe
	cp distribution/oden.sh build/oden/bin/oden
	rm -f build/lib/libimporter.h
	cp -r build/lib build/oden/lib
ifeq ($(OS),osx)
		./tools/change_osx_install_names.sh
endif

.PHONY: clean
clean:
	stack clean
	rm -rf build

.PHONY: test
test: # Runs lint and specifications.
	$(LIBRARY_PATH_VAR) stack build --test --test-arguments '$(TEST_FLAG)'

.PHONY: regression-test
regression-test:
	regression-test/run-regression-tests.sh validate

.PHONY: ci-test
ci-test: test regression-test

.PHONY: watch-test
watch-test: $(NODEMON)
	$(NODEMON) --watch src --watch test -e hs --exec 'make test || exit 1'

$(NODEMON):
	npm install nodemon

$(GITBOOK):
	npm install gitbook-cli

# TODO: Change these file dependencies to be recursive.
build/doc/user-guide: doc/user-guide/*.md $(GITBOOK)
	$(GITBOOK) install doc/user-guide
	$(GITBOOK) build doc/user-guide build/doc/user-guide
	cp doc/user-guide/logo.png build/doc/user-guide/gitbook/images/favicon.ico

.PHONY: doc
doc: build/doc/user-guide


.PHONY: watch-doc
watch-doc: $(GITBOOK)
	$(GITBOOK) serve doc/user-guide build/doc-watch

deploy-docs:
	aws s3 sync build/doc/user-guide s3://docs.oden-lang.org/$(VERSION)/ --acl=public-read

deploy-latest-docs:
	aws s3 sync build/doc/user-guide s3://docs.oden-lang.org/latest/ --acl=public-read

$(DIST_ARCHIVE): build/oden
	(cd build && tar -czf $(DIST_NAME).tar.gz oden)
