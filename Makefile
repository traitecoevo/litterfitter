all: install

test: install
	make -C inst/tests test

document:
	@mkdir -p man
	Rscript -e "library(methods); ${DEVTOOLS_DOCUMENT}"

install:
	R CMD INSTALL --no-test-load .

# No real targets!
.PHONY: all test document install
