include ../Rules.make

EXTRA_DIST=README

#--------------------------------------
# Make targets
# add checks for libfabm and libsediment

all: libsediment

libsediment: prefix $(MOSSCO_LIBRARY_PATH)/libsediment.a
libsolver: prefix $(MOSSCO_module_path)/solver_library.mod

$(MOSSCO_LIBRARY_PATH)/libsediment.a: esmf_fabm_sediment_component.o
	$(AR) cruvs $@ esmf_fabm_sediment_component.o

$(MOSSCO_module_path)/solver_library.mod:
	make -C $(MOSSCODIR)/src/utilities libsolver

clean: extraclean
extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libsediment.a
	@rm -f $(MOSSCO_MODULE_PATH)/esmf_fabm_sediment_component.mod

