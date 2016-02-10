# This Makefile is part of MOSSCO
#
# @copyright Copyright (C) 2016 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, <carsten.lemmen@hzg.de>

#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
TARGETS += libmossco_grid
LIBS_TO_CLEAN += libmossco_grid.a

libmossco_grid: libmossco_grid_component
libmossco_grid_component: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_grid.a(grid_component.o)
