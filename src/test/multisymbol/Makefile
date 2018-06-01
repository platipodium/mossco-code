export AWK:=$(shell which gawk 2> /dev/null)
ifeq ($(strip $(AWK)),)
export AWK:=$(shell which awk 2> /dev/null)
endif

ifeq ($(strip $(AWK)),)
$(error Could not find awk utility in path)
endif

export OBC:=$(shell which gobjcopy 2> /dev/null)
ifeq ($(strip $(OBC)),)
export OBC:=$(shell which objcopy 2> /dev/null)
endif

ifeq ($(strip $(OBC)),)
$(error Could not find objcopy utility in path)
endif


all: main.F90 m2.o m1.o
	gfortran -o main main.F90 -L f2 -lf m2.o -L f1 -lf m1.o

main: m2.o m1.o

m2.o: m2.F90 f2/libf.a
	gfortran -c -I./f2 -o $@ $< -L f2 -lf

m1.o: m1.F90 f1/libf.a
	gfortran -c -I./f1 -o $@ $< -L f1 -lf

f1/libf.a: f1/f.o
	ar cruvs $@ $<

f2/libf.a: f2/f.o
	ar cruvs $@ $<

f1/f.o: f1/f.F90
	gfortran -c -o $@ $<

f2/f.o: f2/f.F90
	gfortran -c -o $@ $<

clean:
	rm -rf f?/*o f?/*a *.o *.mod f?/*.mod main

replace: all
	nm ./f2/libf.a | grep _f_MOD | $(AWK) '{printf $$3 " "; gsub("_f_MOD","_f1_MOD",$$3); print $$3}' > replace.tsv
	$(OBC) --redefine-syms=replace.tsv m1.o
	$(OBC) --redefine-syms=replace.tsv f1/f.o
	$(OBC) --redefine-syms=replace.tsv f1/libf.a
	mv f1/libf.a f1/libf1.a
	gfortran -o main main.F90 m1.o m2.o -Lf2 -Lf1 -lf1 -lf && ./main
