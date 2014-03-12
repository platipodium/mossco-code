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
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/include$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

SUBDIRS=

.PHONY: sha

git-sha.h: sha

clean: extraclean

extraclean:
	@- for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done 
	@- rm -f git-sha.h
