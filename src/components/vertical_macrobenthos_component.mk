# This Makefile is part of MOSSCO
#
# @copyright Copyright (C) 2018 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, <carsten.lemmen@hereon.de>

#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
TARGETS += libmossco_vertical_macrobenthos
LIBS_TO_CLEAN += libmossco_vertical_macrobenthos.a

libmossco_vertical_macrobenthos:  libmossco_vertical_macrobenthos_component
libmossco_vertical_macrobenthos_component: prefix \
  libmossco_vertical_macrobenthos_driver libmossco_util \
  $(MOSSCO_LIBRARY_PATH)/libmossco_vertical_macrobenthos.a(vertical_macrobenthos_component.o)

libmossco_vertical_macrobenthos_driver:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers $@
