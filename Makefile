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
	cp -r build/lib build/oden/lib

.PHONY: clean
clean:
	rm -rf build

.PHONY: test
test:
	$(LIBRARY_PATH_VAR) stack build --test --test-arguments '$(TEST_FLAG)'

.PHONY: regression-test
regression-test:
	regression-test/run-regression-tests.sh validate

.PHONY: lint
lint:
	hlint src cli

.PHONY: ci-test
ci-test: test regression-test lint

.PHONY: watch-test
watch-test: $(NODEMON)
	$(NODEMON) --watch src --watch test -e hs --exec 'make test || exit 1'

$(NODEMON):
	npm install nodemon

$(DIST_ARCHIVE): build/oden
	(cd build && tar -czf $(DIST_NAME).tar.gz oden)

release: $(DIST_ARCHIVE)
	@echo "\n\nDon't forget to set env variable GITHUB_TOKEN first!\n\n"
	go get github.com/aktau/github-release
	-git tag -a -m "Release $(VERSION)" $(VERSION) && git push origin +$(VERSION)
	-github-release release \
		--user oden-lang \
		--repo oden \
		--tag $(VERSION) \
		--name $(VERSION) \
		--pre-release
	find build -name 'oden-$(VERSION)-*.tar.gz' -execdir \
		github-release upload \
			--user oden-lang \
			--repo oden \
			--tag $(VERSION) \
			--name {} \
			--file {} \;
