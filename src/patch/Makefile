# This Makefile is part of MOSSCO
#
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
ifndef MOSSCO_DIR
  export MOSSCO_DIR=$(subst /src/patch$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

TARGETS =

ifeq ($(MOSSCO_EROSED),true)
## @todo: reenable this when patching works again ...
#  TARGETS += erosed_patches
endif

.PHONY: $(TARGETS)

# Make targets
# Default is to make all of the targets
all: $(TARGETS)

erosed_patches:
	patch $(EROSED_DIR)/source/compbsskin.f90 < erosed_compbsskin.patch
	patch $(EROSED_DIR)/source/soursin_3d.f90 < erosed_soursin_3d.patch
	patch $(EROSED_DIR)/source/vanRijn84.f90 < erosed_vanRijn84.patch

clean: extraclean

ifeq ($(MOSSCO_EROSED),true)
extraclean: erosed_extraclean
else
extraclean:
endif


erosed_extraclean:
	patch -R $(EROSED_DIR)/source/compbsskin.f90 < erosed_compbsskin.patch
	patch -R $(EROSED_DIR)/source/soursin_3d.f90 < erosed_soursin_3d.patch
	patch -R $(EROSED_DIR)/source/vanRijn84.f90 < erosed_vanRijn84.patch
