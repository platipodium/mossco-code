# This Makefile is part of MOSSCO
# 
# Copyright (C) 2013 Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#
DOXYGEN ?= doxygen
DOXYGEN_CONFIG = doxygen.conf

EXTRA_DIST=README $(DOXYGEN_CONFIG) doxygen.css doxygen.png

.PHONY: default doc www

default: doc 

doc:
	$(DOXYGEN) $(DOXYGEN_CONFIG)
	$(MAKE) -C latex
	@cp latex/refman.pdf mossco_documentation.pdf

clean:
	@rm -rf latex
	@rm -f mossco_documentation.pdf
	
www: src/mainpage.doxygen latex/refman.pdf
ifdef MOSSCO_SF_USER
	rsync -e ssh -alpr html/* $(MOSSCO_SF_USER)@web.sourceforge.net:/home/project-web/mossco/htdocs/doc/
else
	@echo "Please set the environment variable MOSSCO_SF_USER to your sourceforge user name."
endif

