GIT_REV_LONG=$(shell git rev-parse HEAD)
GIT_REV_SHORT=$(shell git rev-parse --short HEAD)
VERSION ?= $(GIT_REV_SHORT)
VERSION_LONG="$(VERSION) (git revision: $(GIT_REV_LONG))"

ODENC_SOURCES=$(shell find odenc -name '*.rkt')
WORKING_EXPERIMENTS=$(shell find experiments/working/src -name '*.oden')
WORKING_EXPERIMENT_PACKAGES=$(WORKING_EXPERIMENTS:experiments/working/src/%.oden=%)

OS=$(shell tools/get_os.sh)

ODENC=target/odenc
ifeq ($(OS),windows)
	ODENC=target/odenc.exe
endif

DIST_NAME=oden-$(VERSION)-$(OS)
DIST_ARCHIVE=target/$(DIST_NAME).tar.gz

.PHONY: all
all: dist

.PHONY:clean
clean:
	rm -rf target

.PHONY: test
test:
	raco test $(ODENC_SOURCES)

$(ODENC): $(ODENC_SOURCES)
	mkdir -p target
	VERSION=$(VERSION_LONG) raco exe -o $(ODENC) odenc/main.rkt

target/oden: $(ODENC) README.md
	mkdir -p target/oden
	raco distribute target/oden $(ODENC)
	cp README.md target/oden/README.txt
	cp BUILD.md target/oden/BUILD.txt
	echo "$(VERSION) (git revision: $(GIT_REV_LONG))" >> target/oden/VERSION.txt

$(DIST_ARCHIVE): target/oden
	(cd target/oden && tar -czf ../$(DIST_NAME).tar.gz .)

.PHONY: compile-experiments
compile-experiments: $(ODENC) $(WORKING_EXPERIMENTS)
	$(ODENC) -o $(PWD)/target/experiments -p experiments/working
	GOPATH=$(PWD)/target/experiments go build $(WORKING_EXPERIMENT_PACKAGES)

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
	docker cp oden-builder:/src/oden/target/oden-$(VERSION)-linux.tar.gz target

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
	find target -name 'oden-$(VERSION)-*.tar.gz' -execdir \
		github-release upload \
			--user oden-lang \
			--repo oden \
			--tag $(VERSION) \
			--name {} \
			--file {} \;
