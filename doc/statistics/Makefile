# This Makefile is part of MOSSCO
# 
# @copyright (C) 2014 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README.md

SUBDIRS = 

ifeq ($(GITSTATS),)
GITSTATS = $(which gitstats)
endif


.PHONY: all gitstats plot extraclean subdirs $(SUBDIRS)

all: subdirs gitstats

gitstats:
	gitstats $(MOSSCO_DIR) ./gitstats	

plot: gitstats
	$(MAKE) -C python all



subdirs:
	@for dir in $(SUBDIRS) ; do make -C $$dir ; done

clean: extraclean

extraclean:
	@for dir in $(SUBDIRS) ; do make -C $$dir clean; done
	rm -rf ./gitstats 

