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

target/kmi:
	mkdir -p target
	raco exe -o target/kmi repl.rkt

$(GITBOOK):
	npm install gitbook-cli

target/kashmir/docs: docs/* $(GITBOOK)
	$(GITBOOK) build docs target/kashmir/docs
	rm target/kashmir/docs/*.md~

.PHONY: docs
docs: target/kashmir/docs

.PHONY: watch-docs
watch-docs:
	$(GITBOOK) serve docs docs/_book

.PHONY: release-docs
release-docs:
	git reset --hard HEAD
	git checkout gh-pages
	make clean docs
	cp -r target/kashmir/docs/* ./
	git rebase master
	git add -a .
	git commit -m "Auto-generated docs" .
	git push origin gh-pages
	git checkout master

target/kashmir: target/kmi README.md
	mkdir -p target/kashmir
	raco distribute target/kashmir target/kmi
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








