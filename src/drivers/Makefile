ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /src/drivers$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

EXTRA_DIST=README

DELFT_OBJECTS=$(DELFT_DIR)/modules/precision_basics.o $(DELFT_DIR)/modules/precision.o $(DELFT_DIR)/source/eromud.o $(DELFT_DIR)/source/erosand.o $(DELFT_DIR)/source/sand_mud.o $(DELFT_DIR)/source/shld.o $(DELFT_DIR)/source/vanRijn84.o

# Make targets

ALL_TARGETS=libsediment

ifdef DELFT_DIR
ALL_TARGETS +=liberosed
endif

all: $(ALL_TARGETS)

libsediment: prefix $(MOSSCO_LIBRARY_PATH)/libsediment.a

$(MOSSCO_LIBRARY_PATH)/libsediment.a: libfabm fabm_sediment_driver.o
	$(AR) cruvs $@ fabm_sediment_driver.o

liberosed: prefix $(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a

$(MOSSCO_LIBRARY_PATH)/libmossco_erosed.a: libdelft delft_erosed_driver.o
	$(AR) cruvs $@ delft_erosed_driver.o

libfabm: $(FABM_LIBRARY_FILE)

#libdelft: $(DELFT_OBJECTS)
#	$(AR) cruvs $(MOSSCO_LIBRARY_PATH)/libmossco_delft.a $(DELFT_OBJECTS) 

libdelft:
	$(F90) $(CPPFLAGS) -I$(DELFT_DIR)/source -I$(DELFT_DIR)/modules $(F90FLAGS) -c $(DELFT_DIR)/modules/precision_basics.f90 -o $(DELFT_DIR)/modules/precision_basics.o
	$(F90) $(CPPFLAGS) -I$(DELFT_DIR)/source -I$(DELFT_DIR)/modules $(F90FLAGS) -c $(DELFT_DIR)/modules/precision.f90 -o $(DELFT_DIR)/modules/precision.o
	$(F90) $(CPPFLAGS) -I$(DELFT_DIR)/source -I$(DELFT_DIR)/modules $(F90FLAGS) -c $(DELFT_DIR)/source/eromud.f90 -o $(DELFT_DIR)/source/eromud.o
	$(F90) $(CPPFLAGS) -I$(DELFT_DIR)/source -I$(DELFT_DIR)/modules $(F90FLAGS) -c $(DELFT_DIR)/source/erosand.f90 -o $(DELFT_DIR)/source/erosand.o
	$(F90) $(CPPFLAGS) -I$(DELFT_DIR)/source -I$(DELFT_DIR)/modules $(F90FLAGS) -c $(DELFT_DIR)/source/sand_mud.f90 -o $(DELFT_DIR)/source/sand_mud.o
	$(F90) $(CPPFLAGS) -I$(DELFT_DIR)/source -I$(DELFT_DIR)/modules $(F90FLAGS) -c $(DELFT_DIR)/source/shld.f90 -o $(DELFT_DIR)/source/shld.o
	$(F90) $(CPPFLAGS) -I$(DELFT_DIR)/source -I$(DELFT_DIR)/modules $(F90FLAGS) -c $(DELFT_DIR)/source/vanRijn84.f90 -o $(DELFT_DIR)/source/vanRijn84.o
	$(AR) cruvs $(MOSSCO_LIBRARY_PATH)/libmossco_delft.a $(DELFT_OBJECTS) 



$(FABM_LIBRARY_FILE):
	$(MAKE) -C $(FABMDIR)
	$(MAKE) -C $(FABMDIR)/src

clean: extraclean
extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libsediment.a
	@rm -f $(MOSSCO_MODULE_PATH)/fabm_sediment_driver.mod
	@rm -f $(MOSSCO_MODULE_PATH)/delft_erosed_driver.mod
