DOXYGEN = doxygen
DOXYGEN_CONFIG = doxygen.conf

EXTRA_DIST=README $(DOXYGEN_CONFIG) doxygen.css doxygen.png

.PHONY: default doc

default: doc 

doc:
	$(DOXYGEN) $(DOXYGEN_CONFIG)
	make -C latex
	cp latex/refman.pdf mossco_documentation.pdf

clean:
	rm -rf latex

