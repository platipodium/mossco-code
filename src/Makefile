# This Makefile is part of MOSSCO
# 
# @copyright 2021-2022, 2018 Helmholtz-Zentrum Hereon
# @copyright 2013-2021, 2018 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hereon.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the 
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file 
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms. 
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README Rules.make

SUBDIRS=include utilities drivers patch

ifeq ($(MOSSCO_ESMF),true)
SUBDIRS += components mediators connectors #nuopc
endif

.PHONY: subdirs $(SUBDIRS)

all: subdirs

components: drivers patch
drivers:  patch

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean: extraclean

extraclean:
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done
	@$(RM) $(MOSSCO_DIR)/src/include/git-sha.h	

check:
	@echo The check target is not implemented, yet
