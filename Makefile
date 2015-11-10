GIT_REV_LONG=$(shell git rev-parse HEAD)
GIT_REV_SHORT=$(shell git rev-parse --short HEAD)
VERSION ?= $(GIT_REV_SHORT)

GITBOOK=node_modules/.bin/gitbook

OS=$(shell tools/get_os.sh)
DIST_NAME=kashmir-$(VERSION)-$(OS)
DIST_ZIP=target/$(DIST_NAME).zip

all: docs dist

clean:
	rm -rf target

target/kmc:
	mkdir -p target
	raco exe -o target/kmc src/cmd/kmc.rkt

$(GITBOOK):
	npm install gitbook-cli

target/kashmir/docs: docs/* $(GITBOOK)
	$(GITBOOK) build docs target/kashmir/docs
	rm -f target/kashmir/docs/*.md~

.PHONY: docs
docs: target/kashmir/docs

.PHONY: watch-docs
watch-docs:
	$(GITBOOK) serve docs docs/_book

.PHONY: release-docs
release-docs:
	git reset --hard HEAD
	git checkout gh-pages
	git rebase master
	make clean docs
	cp -r target/kashmir/docs/* ./
	git add -A
	git commit -m "Auto-generated docs" .
	git push origin +gh-pages
	git checkout master

target/kashmir: target/kmc README.md
	mkdir -p target/kashmir
	raco distribute target/kashmir target/kmc
	cp README.md target/kashmir/README.txt
	echo "$(VERSION) (git revision: $(GIT_REV_LONG))" >> target/kashmir/VERSION.txt

$(DIST_ZIP): target/kashmir
	(cd target/kashmir && zip ../$(DIST_NAME).zip -r *)

dist: $(DIST_ZIP)

release: $(DIST_ZIP)
	@echo "\n\nDon't forget to set env variable GITHUB_TOKEN first!\n\n"
	go get github.com/aktau/github-release
	-git tag -d $(VERSION)
	git tag -a -m "Release $(VERSION)" $(VERSION)
	git push origin +$(VERSION)
	-github-release release \
		--user owickstrom \
		--repo kashmir \
		--tag $(VERSION) \
		--name $(VERSION) \
		--pre-release
	github-release upload \
		--user owickstrom \
		--repo kashmir \
		--tag $(VERSION) \
		--name "$(DIST_NAME).zip" \
		--file $(DIST_ZIP)








