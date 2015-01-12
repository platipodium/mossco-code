# This Makefile is part of MOSSCO
#
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/connectors$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

TARGETS=

ifeq ($(MOSSCO_ESMF),true)
TARGETS += libmossco_connector
endif

SUBDIRS=

.PHONY: default all subdirs $(TARGETS)

all: subdirs $(TARGETS)

libmossco_connector: libtransport_connector liblink_connector

libtransport_connector: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_connector.a(transport_connector.o)
liblink_connector: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_connector.a(link_connector.o)

libmossco_util:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

$(MOSSCO_LIBRARY_PATH)/libmossco_connector.a(transport_connector.o): transport_connector.o
	$(AR) cruvs $@ transport_connector.o

$(MOSSCO_LIBRARY_PATH)/libmossco_connector.a(link_connector.o): link_connector.o
	$(AR) cruvs $@ link_connector.o

subdirs:
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir ; done

clean: extraclean

extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libmossco_connector.a
	@for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done
