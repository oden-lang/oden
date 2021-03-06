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

.PHONY: $(STACK_ODEN_EXE)
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
	mkdir -p build/oden/lib
	cp -r libimporter.* build/oden/lib/
	cp -r build/doc build/oden/doc || echo "No docs found, will be excluded from distribution!"
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
	$(LIBRARY_PATH_VAR) stack build --test --test-arguments '$(TEST_FLAG)' --file-watch

$(NODEMON):
	npm install nodemon

$(GITBOOK):
	npm install gitbook-cli@2.2.0

.PHONY: doc
doc:
	INCLUDE_PDF_LINKS=$(INCLUDE_PDF_LINKS) \
			$(MAKE) -C doc/user-guide html
	mkdir -p build/doc
	rm -rf build/doc/user-guide
	cp -r doc/user-guide/target/site build/doc/user-guide

.PHONY: doc-pdf
doc-pdf:
	$(MAKE) -C doc/user-guide pdf pdf-ebook
	mkdir -p build/doc/user-guide
	cp -r doc/user-guide/target/site/*.pdf build/doc/user-guide/

.PHONY: clean-doc
	$(MAKE) -C doc/user-guide clean

.PHONY: watch-doc
watch-doc: $(NODEMON)
	$(NODEMON) --watch doc/user-guide/src -e md,tex,css,js,html,png,svg --exec 'make doc || exit 1'

deploy-docs:
	aws s3 sync build/doc/user-guide s3://docs.oden-lang.org/$(VERSION)/ --acl=public-read

deploy-latest-docs:
	aws s3 sync build/doc/user-guide s3://docs.oden-lang.org/latest/ --acl=public-read

deploy-pdfs:
	aws s3 cp build/doc/user-guide/user-guide.pdf s3://docs.oden-lang.org/$(VERSION)/ --acl=public-read
	aws s3 cp build/doc/user-guide/user-guide.ebook.pdf s3://docs.oden-lang.org/$(VERSION)/ --acl=public-read

deploy-latest-pdfs:
	aws s3 cp build/doc/user-guide/*.pdf s3://docs.oden-lang.org/latest/ --acl=public-read

$(DIST_ARCHIVE): build/oden
	(cd build && tar -czf $(DIST_NAME).tar.gz oden)
