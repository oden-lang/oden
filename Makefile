TMP = .tmp
VERSION = $(shell grep '^version:' oden.cabal | sed -Ee 's/^version\:(.+)$$/\1/' | xargs)
OS=$(shell tools/get_os.sh)
DIST_NAME=oden-$(VERSION)-$(OS)
DIST_ARCHIVE=dist/$(DIST_NAME).tar.gz

NODEMON=node_modules/.bin/nodemon

.PHONY: build
build:
	@cabal build

.PHONY: clean
clean:
	rm -rf dist

$(TMP):
	mkdir -p $(TMP)

.PHONY: test
test:
	cabal exec runhaskell -- -isrc -itest test/Spec.hs

.PHONY: watch-test
watch-test: $(NODEMON)
	$(NODEMON) --watch src --watch test -e hs --exec make test

$(NODEMON):
	npm install nodemon@1.2

init-dev:
	cabal sandbox init
	cabal install --enable-tests --only-dependencies
	cabal configure --enable-tests
	cabal build

dist/build/odenc/odenc:
	cabal build odenc

dist/build/odeni/odeni:
	cabal build odeni

dist/oden: dist/build/odenc/odenc dist/build/odeni/odeni
	@mkdir -p dist/oden/bin
	cp README.md dist/oden/README.txt
	cp LICENSE.md dist/oden/LICENSE.txt
	cp CREDITS.md dist/oden/CREDITS.txt
	cp dist/build/odenc/odenc dist/oden/bin/odenc
	cp dist/build/odeni/odeni dist/oden/bin/odeni

$(DIST_ARCHIVE): dist/oden
	(cd dist/oden && tar -czf ../$(DIST_NAME).tar.gz .)

dist: $(DIST_ARCHIVE)

docker-dist:
	@(docker kill oden-builder &> /dev/null ; true)
	@(docker rm oden-builder &> /dev/null ; true)
	docker build \
		-t oden-build:$(VERSION) \
		.
	docker run \
		--env VERSION=$(VERSION) \
		--name oden-builder \
		oden-build:$(VERSION)
	docker cp oden-builder:/src/oden/dist/oden-$(VERSION)-linux.tar.gz dist

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
	find dist -name 'oden-$(VERSION)-*.tar.gz' -execdir \
		github-release upload \
			--user oden-lang \
			--repo oden \
			--tag $(VERSION) \
			--name {} \
			--file {} \;
