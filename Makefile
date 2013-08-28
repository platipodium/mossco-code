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
