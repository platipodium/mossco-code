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

.PHONY: gotm gotm_distclean gotm_version

gotm:
ifeq ($(wildcard $(external_GOTMDIR)/src/gotm/gotm.F90),)
	@$(GIT) clone -b master --depth 1 https://github.com/gotm-model/code.git $(external_GOTMDIR)
else
#	@$(GIT) -C $(external_GOTMDIR) pull --ff-only
#for old git
	@( cd $(external_GOTMDIR) && $(GIT) pull --ff-only )
endif

gotm_distclean:
ifeq ($(MOSSCO_GOTM),true)
	@echo Cleaning the GOTM library in $(GOTM_PREFIX)
ifndef MOSSCO_GOTM_BINARY_DIR
	$(RM) -rf $(GOTM_BINARY_DIR)
endif
ifndef MOSSCO_GOTM_PREFIX
	$(RM) -rf $(GOTM_PREFIX)
endif
endif

gotm_version:
ifneq ($(wildcard $(external_GOTMDIR)/src/gotm/gotm.F90),)
  # git describe --long --tags --dirty --always
#	GOTM_VERSION=$(shell $(GIT) -C $(external_GOTMDIR) log -1 --format="'%h (%ci)'")
#for old git
	GOTM_VERSION=$(shell cd $(external_GOTMDIR) && $(GIT) log -1 --format="'%h (%ci)'")


	@#echo "CPPFLAGS+=-DGETM_VERSION="${GETM_VERSION} >> $(MOSSCO_DIR)/src/include/versions.mk
	@#echo "CPPFLAGS+=-DGETM_GIT_SHA="${GETM_GIT_SHA} >> $(MOSSCO_DIR)/src/include/versions.mk
endif

