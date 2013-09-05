# This Makefile is part of MOSSCO
# 
# Copyright (C) 2013 Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#
EXTRA_DIST = README ACKNOWLEDGEMENTS AUTHORS .gitignore
SUBDIRS = doc src examples

export MOSSCO_DIR=$(CURDIR)
export MOSSCO_DATE=$(shell date "+%Y%m%d")

.PHONY: default doc src info examples all clean subdirs $(SUBDIRS)

default: src
all:  examples doc
examples: src

clean:
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done 
	@rm -rf modules lib bin

distclean: clean

all: subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

check: 
	make -C src check

update:
	git pull 

info:
	make -C src info

run: examples
	(cd examples/omexdia_p && ./omexdia_p_test)
	(cd examples/esmf_sediment && ./esmf_sediment_test)
	(cd examples/esmf_gotm && ./esmf_gotm_example)

archive:
	@git archive --format=tar.gz --prefix=mossco-$(MOSSCO_DATE)/ HEAD > $(MOSSCO_DIR)/../mossco-$(MOSSCO_DATE).tar.gz
ifdef MOSSCO_SF_USER
	rsync -e ssh -t $(MOSSCO_DIR)/../mossco-$(MOSSCO_DATE).tar.gz $(MOSSCO_SF_USER)@frs.sf.net:/home/pfs/p/mossco/Snapshots/
else
	@echo "Please set the environment variable MOSSCO_SF_USER to your sourceforge user name."
endif
