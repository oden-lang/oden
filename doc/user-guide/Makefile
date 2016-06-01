MD_SOURCES=\
				src/index.md \
				src/installation.md \
				src/workflow.md \
				src/the-language.md \
				src/references.md

DOC_TARGETS=html
BEFORE_BODY_FLAGS=-B src/site/logo.html

ifneq ($(strip $(BUILD_DOC_PDF)),)
	DOC_TARGETS=html pdf pdf-ebook

	BEFORE_BODY_FLAGS= -B src/site/pdf-links.html \
										 -B src/site/logo.html
endif

all: $(DOC_TARGETS)

html:
	@mkdir -p target
	@rm -rf target
	cp -r src/site target
	pandoc \
		--standalone \
		-S \
		--toc \
		--no-highlight \
		--chapters \
		"--metadata=subtitle:Version $(VERSION)" \
		--filter pandoc-citeproc \
		--bibliography src/bibliography.bib \
		-t html5 \
		-c styles/user-guide.css \
		-H src/site/head.html \
		$(BEFORE_BODY_FLAGS) \
		-A src/site/footer.html \
		-o target/index.html \
		src/preface.md $(MD_SOURCES)

pdf:
	@mkdir -p target
	pandoc \
		-S \
		--standalone \
		--toc \
		--listings \
		--chapters \
		-V links-as-notes \
		--filter pandoc-citeproc \
		--bibliography src/bibliography.bib \
		"--metadata=subtitle:Version $(VERSION)" \
		-o target/user-guide.pdf \
		--template=src/template.tex \
		$(MD_SOURCES)

pdf-ebook:
	@mkdir -p target
	pandoc \
		-S \
		--standalone \
		--toc \
		--listings \
		--chapters \
		-V "geometry:papersize={4.5in,6in}" \
		-V "geometry:margin={.25in,.75in}" \
		-V links-as-notes \
		--filter pandoc-citeproc \
		--bibliography src/bibliography.bib \
		"--metadata=subtitle:Version $(VERSION)" \
		-o target/user-guide.ebook.pdf \
		--template=src/template.tex \
		$(MD_SOURCES)

clean:
	rm -r target
