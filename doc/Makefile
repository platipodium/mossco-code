DOXYGEN = doxygen
DOXYGEN_CONFIG = doxygen.conf

EXTRA_DIST=README $(DOXYGEN_CONFIG) doxygen.css doxygen.png

.PHONY: default doc

default: doc 

doc:
	$(DOXYGEN) $(DOXYGEN_CONFIG)

clean:
	rm -rf latex

