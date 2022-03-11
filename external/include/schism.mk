# This Makefile is part of MOSSCO
#
# @copyright (C) 2022 Helmholtz-Zentrum Hereon
# @author Carsten Lemmen <carsten.lemmen@hereon.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

.PHONY: schism schism-esmf

schism:
ifeq ($(wildcard $(SCHISM_DIR)/src/Hydro/schism_step.F90),)
	@mkdir -p schism
	export SCHISM_DIR=$(MOSSCO_DIR)/external/schism/schism
	@$(GIT_CLONE) https://github.com/schism-dev/schism.git $(SCHISM_DIR)
else
	@( cd $(SCHISM_DIR) && $(GIT) pull origin master)
endif

schism-esmf:
ifeq ($(wildcard $(SCHISM_ESMF_DIR)/src/schism/schism_esmf_cap.F90),)
	@mkdir -p schism
	export SCHISM_ESMF_DIR=$(MOSSCO_DIR)/external/schism/schism-esmf
	@$(GIT_CLONE) https://github.com/schism-dev/schism.git $(SCHISM_ESMF_DIR)
else
	@( cd $(SCHISM_ESMF_DIR) && $(GIT) pull origin master )
endif
