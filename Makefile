# This Makefile is part of MOSSCO
#
# @copyright (C) 2013, 2014, 2015, 2016, 2017 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
# @author Knut Klingbeil, Institut für Ostseeforschung Warnemünde
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

EXTRA_DIST = README ACKNOWLEDGEMENTS AUTHORS .gitignore
SUBDIRS = doc src examples external

export MOSSCO_DATE=$(shell date "+%Y%m%d")

include $(MOSSCO_DIR)/src/Rules.make

.PHONY: all extraclean subdirs $(SUBDIRS)

all: src

clean: extraclean

extraclean:
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done
	@rm -rf modules lib bin

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

check:
	make -C src check

update:
	git pull
	$(MAKE) -C external $@


run: examples
	(cd examples/omexdia_p && ./omexdia_p_test)
	(cd examples/esmf_sediment && ./esmf_sediment_test)
	(cd examples/esmf_gotm && ./esmf_gotm_example)

archive:
	@git archive --format=tar.gz --prefix=mossco-code-$(MOSSCO_DATE)/ HEAD > $(MOSSCO_DIR)/../mossco-code-$(MOSSCO_DATE).tar.gz
ifdef MOSSCO_SF_USER
	rsync -e ssh -t $(MOSSCO_DIR)/../mossco-code-$(MOSSCO_DATE).tar.gz $(MOSSCO_SF_USER)@frs.sf.net:/home/pfs/p/mossco/Snapshots/
else
	@echo "To upload, set the environment variable MOSSCO_SF_USER to your sourceforge user name."
endif
