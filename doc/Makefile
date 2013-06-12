DOXYGEN ?= doxygen
DOXYGEN_CONFIG = doxygen.conf

EXTRA_DIST=README $(DOXYGEN_CONFIG) doxygen.css doxygen.png

.PHONY: default doc

default: doc 

doc:
	$(DOXYGEN) $(DOXYGEN_CONFIG)
	$(MAKE) -C latex
	@cp latex/refman.pdf mossco_documentation.pdf

clean:
	@rm -rf latex
	@rm -f mossco_documentation.pdf
	

