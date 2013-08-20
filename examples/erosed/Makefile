ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/omexdia_p$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

LIBS := -lmossco_delft -lmossco_erosed $(LIBS)

F90FLAGS += $(INCLUDES)
LDFLAGS += $(LIBRARY_PATHS) $(LIBS)

.PHONY: liberosed exec

all: exec

liberosed:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers liberosed	

exec: liberosed delft_erosed_example.o
	$(F90) -o erosed_test delft_erosed_example.o $(LDFLAGS)
	@echo "Created example binary erosed_test in  $(PWD)" 
	@echo "Please change to $(MOSSCO_DIR)/examples/erosed to run the example." > $(MOSSCO_BIN_PATH)/erosed.txt

clean: extraclean

extraclean: 
	@rm -f erosed_test
