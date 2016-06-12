BASE=$(shell pwd)

MD_SOURCES=\
				src/index.md \
				src/getting-started.md \
				src/the-language.md \
				src/references.md

SHARED_PANDOC_OPTIONS=--standalone \
											-S \
											--toc \
											--chapters \
											"--metadata=subtitle:$(VERSION)" \
											-V base=$(BASE) \
											--filter pandoc-citeproc \
											--bibliography src/bibliography.bib

PDF_PANDOC_OPTIONS=$(SHARED_PANDOC_OPTIONS) \
									 --latex-engine=xelatex \
									 --listings \
									 -V links-as-notes \
									 --filter target/bin/include-code \
									 --template=src/template.tex


BEFORE_BODY_FLAGS=-B src/site/logo.html

ifneq ($(strip $(INCLUDE_PDF_LINKS)),)
	BEFORE_BODY_FLAGS= -B src/site/pdf-links.html \
										 -B src/site/logo.html
endif

all: html pdf pdf-ebook

html: target/bin/include-code
	@mkdir -p target
	@rm -rf target/site
	cp -r src/site target/site
	pandoc $(SHARED_PANDOC_OPTIONS) \
		-t html5 \
		--filter target/bin/include-code \
		-c styles/user-guide.css \
		-H src/site/head.html \
		$(BEFORE_BODY_FLAGS) \
		-A src/site/footer.html \
		-o target/site/index.html \
		--base-header-level=2 \
		--template=src/template.html \
		src/preface.md $(MD_SOURCES)

pdf: target/bin/include-code
	@mkdir -p target/site
	pandoc $(PDF_PANDOC_OPTIONS) -o target/site/user-guide.pdf $(MD_SOURCES)

pdf-ebook: target/bin/include-code
	@mkdir -p target/site
	pandoc $(PDF_PANDOC_OPTIONS) \
		-V "geometry:papersize={4.5in,6in}" \
		-V "geometry:margin={.25in,.75in}" \
		-o target/site/user-guide.ebook.pdf \
		$(MD_SOURCES)

target/bin/include-code: src/filters/IncludeCode.hs
	mkdir -p target/bin
	stack ghc -- $< -o $@

clean:
	rm -r target
