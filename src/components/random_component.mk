# This Makefile is part of MOSSCO
#
# @copyright Copyright (C) 2018 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
TARGETS += libmossco_random
LIBS_TO_CLEAN += libmossco_random.a

libmossco_random: libmossco_random_component
libmossco_random_component: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_random.a(random_component.o)
