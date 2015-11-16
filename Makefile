GIT_REV_LONG=$(shell git rev-parse HEAD)
GIT_REV_SHORT=$(shell git rev-parse --short HEAD)
VERSION ?= $(GIT_REV_SHORT)

SOURCES=$(shell find src -name *.rkt)

OS=$(shell tools/get_os.sh)

ODENC=target/odenc
ifeq ($(OS),windows)
	ODENC=target/odenc.exe
endif

DIST_NAME=oden-$(VERSION)-$(OS)
DIST_ZIP=target/$(DIST_NAME).zip

.PHONY: all
all: dist

.PHONY:clean
clean:
	rm -rf target

.PHONY: test
test:
	raco test src/*-test.rkt

$(ODENC): $(SOURCES)
	mkdir -p target
	raco exe -o $(ODENC) src/cmd/odenc.rkt

$(GITBOOK):
	npm install gitbook-cli

target/oden: test $(ODENC) compile-experiments README.md
	mkdir -p target/oden
	raco distribute target/oden $(ODENC)
	cp README.md target/oden/README.txt
	echo "$(VERSION) (git revision: $(GIT_REV_LONG))" >> target/oden/VERSION.txt

$(DIST_ZIP): target/oden
	(cd target/oden && tar -czf ../$(DIST_NAME).tar.gz .)

.PHONY: compile-experiments
compile-experiments: $(ODENC)
	ODEN_PATH=experiments/working $(ODENC) $(PWD)/target/experiments
	GOPATH=$(PWD)/target/experiments go build ...

dist: $(DIST_ZIP)

release: $(DIST_ZIP)
	@echo "\n\nDon't forget to set env variable GITHUB_TOKEN first!\n\n"
	go get github.com/aktau/github-release
	-git tag -d $(VERSION)
	git tag -a -m "Release $(VERSION)" $(VERSION)
	git push origin +$(VERSION)
	-github-release release \
		--user oden-lang \
		--repo oden \
		--tag $(VERSION) \
		--name $(VERSION) \
		--pre-release
	github-release upload \
		--user oden-lang \
		--repo oden \
		--tag $(VERSION) \
		--name "$(DIST_NAME).zip" \
		--file $(DIST_ZIP)
