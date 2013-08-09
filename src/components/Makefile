include ../Rules.make

EXTRA_DIST=README

#--------------------------------------
# Make targets
# add checks for libfabm and libsediment

all: libsediment libgotm libsolver

libsediment: prefix $(MOSSCO_LIBRARY_PATH)/libsediment.a
libsolver: prefix $(MOSSCO_module_path)/solver_library.mod
libgotm: prefix $(MOSSCO_LIBRARY_PATH)/libgotm.a

$(MOSSCO_LIBRARY_PATH)/libsediment.a: esmf_fabm_sediment_component.o
	$(AR) cruvs $@ esmf_fabm_sediment_component.o

$(MOSSCO_LIBRARY_PATH)/libgotm.a: esmf_gotm_component.o
	$(AR) cruvs $@ esmf_gotm_component.o

esmf_gotm_component.o: esmf_gotm_component.F90
	@echo "Compiling $<"
	$(F90) $(CPPFLAGS) -I$(GOTMDIR)/modules/$(FORTRAN_COMPILER) $(F90FLAGS) -c $< -o $@

$(MOSSCO_module_path)/solver_library.mod:
	make -C $(MOSSCODIR)/src/utilities libsolver

clean: extraclean
extraclean:
	@rm -f $(MOSSCO_LIBRARY_PATH)/libsediment.a
	@rm -f $(MOSSCO_LIBRARY_PATH)/libgotm.a
	@rm -f $(MOSSCO_MODULE_PATH)/esmf_fabm_sediment_component.mod

