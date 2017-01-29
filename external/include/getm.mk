# This Makefile is part of MOSSCO
#
# @copyright (C) 2017 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
# @author Knut Klingbeil, Institut für Ostseeforschung Warnemünde
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

# This is a Makefile stub included by ../Makefile.  Do append (+=) or conditionally
# define (?=) variables, but do not overwrite.
# This Makefile uses the following variables
# - $(MAKE) [read]
# - $(external_GETMDIR) [read]
# - $(GIT) [read]
# - $(GETM_CLONE_TARGETS) [append]
# - $(GETM_UPDATE_TARGETS) [append]
# - $(DISTCLEAN_TARGETS) [append]
# - $(VERSION_TARGETS) [append]

.PHONY: getm_clone getm_update getm_distclean

GIT_TARGETS += getm
DISTCLEAN_TARGETS += getm_distclean
VERSION_TARGETS += getm_version

getm: getm_clone

getm_clone:
ifeq ($(wildcard $(external_GETMDIR)/src/Makefile),)
	$(GIT) clone http://git.code.sf.net/p/getm/code $(external_GETMDIR)
	( cd $(external_GETMDIR) ; $(GIT) checkout -b iow origin/iow )
else
	$(MAKE) getm_update
endif

getm_update:
ifneq ($(wildcard $(external_GETMDIR)/src/Makefile),)
	( cd $(external_GETMDIR) ; $(GIT) pull origin iow)
endif

getm_distclean:
ifneq ($(wildcard $(external_GETMDIR)/src/Makefile),)
	( unset FABM ; $(MAKE) -C $(external_GETMDIR) distclean )
endif

getm_version:
ifneq ($(wildcard $(external_GETMDIR)/src/Makefile),)
	GETM_VERSION=$(shell cat /Users/lemmen/devel/MOSSCO/code/external/getm/code/VERSION)
	GETM_GIT_SHA=$(shell cd $(external_GETMDIR) ; $(GIT) log -n 1 |head -1 | cut -d" " -f2)

	@#echo "CPPFLAGS+=-DGETM_VERSION="${GETM_VERSION} >> $(MOSSCO_DIR)/src/include/versions.mk
	@#echo "CPPFLAGS+=-DGETM_GIT_SHA="${GETM_GIT_SHA} >> $(MOSSCO_DIR)/src/include/versions.mk
endif
