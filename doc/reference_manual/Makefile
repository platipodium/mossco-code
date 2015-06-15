# This Makefile is part of MOSSCO
# 
# @copyright (C) 2013, 2014, 2015 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#
DOXYGEN ?= doxygen
DOXYGEN := $(shell which $(DOXYGEN))
ifeq (x$(DOXYGEN),x)
$(error  The program "doxygen" required to build the documentation was not found. Either specify the correct location of the doxygen executable with DOXYGEN=/your/path/to/bin/doxygen or type "make help" to learn how to access the online documentation.) 
endif

DOXYGEN_CONFIG := doxygen.conf
EXTRA_DIST=README $(DOXYGEN_CONFIG) 
GUIDE=../mossco_reference_manual.pdf

.PHONY: all doc www

all: doc 

doc:
	$(DOXYGEN) $(DOXYGEN_CONFIG)
	$(MAKE) -C latex
	@-cp latex/refman.pdf $(GUIDE)

clean:
	@-rm -rf latex html
	@-rm -f $(GUIDE)
	
www: doc src/mainpage.doxygen latex/refman.pdf
ifdef MOSSCO_SF_USER
	@rsync -e ssh -rt html/* $(MOSSCO_SF_USER)@web.sourceforge.net:/home/project-web/mossco/htdocs/doc/
	@rsync -e ssh -rt $(GUIDE) $(MOSSCO_SF_USER)@frs.sf.net:/home/pfs/p/mossco/Reference_Manual/
else
	@echo "Please set the environment variable MOSSCO_SF_USER to your sourceforge user name."
endif
