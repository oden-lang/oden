VERSION ?= $(shell git rev-parse --short HEAD)

OS=$(shell tools/get_os.sh)
DIST_NAME=kashmir-$(VERSION)-$(OS)
DIST_ZIP=target/$(DIST_NAME).zip

all: dist

clean:
	rm -rf target

target/kmi:
	mkdir -p target
	raco exe -o target/kmi repl.rkt

target/kashmir: target/kmi README.md
	mkdir -p target/kashmir
	raco distribute target/kashmir target/kmi
	cp README.md target/kashmir/
	echo "$(VERSION)" >> target/kashmir/VERSION

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
		--name $(VERSION)
	github-release upload \
		--user owickstrom \
		--repo kashmir \
		--tag $(VERSION) \
		--name "$(DIST_NAME).zip" \
		--file $(DIST_ZIP) \
		--pre-release








