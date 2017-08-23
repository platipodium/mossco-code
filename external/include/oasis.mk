# This Makefile is part of MOSSCO
#
# @copyright (C) 2017 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

.PHONY: oasis oasis_distclean oasis_version

oasis:
ifeq ($(wildcard $(external_oasisDIR)/lib/psmile/src/mod_oasis.F90),)
	@$(SVN) checkout http://oasis3mct.cerfacs.fr/svn/branches/OASIS3-MCT_3.0_branch/oasis3-mct $(external_oasisDIR)
else
	@( cd $(external_oasisDIR) && $(SVN) up )
endif

oasis_distclean:
ifneq ($(wildcard $(external_oasisDIR)/lib/psmile/src/mod_oasis.F90),)
	@( unset FABM ; $(MAKE) -C $(external_oasisDIR) distclean )
endif

oasis_version:
ifneq ($(wildcard $(external_oasisDIR)/lib/psmile/src/mod_oasis.F90),)
	oasis_VERSION=$(shell cd $(external_oasisDIR) && $(SVN) log -1 --format="'%h (%ci)'")
endif
