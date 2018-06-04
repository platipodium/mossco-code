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

OS:=$(shell uname -s)
ifeq ($(OS),Darwin)
OBC=$(shell which objconv 2> /dev/null)
endif

ifeq ($(strip $(OBC)),)
$(error Could not find objcopy/objconv utility in path)
endif

F90?=gfortran


all: main.F90 m2.o m1.o
	$(F90) -o main main.F90 -L f2 -lf m2.o -L f1 -lf m1.o

main: m2.o m1.o

m2.o: m2.F90 f2/libf.a
	$(F90) -c -I./f2 -o $@ $< -L f2 -lf

m1.o: m1.F90 f1/libf.a
	$(F90) -c -I./f1 -o $@ $< -L f1 -lf

f1/libf.a: f1/f.o
	ar cruvs $@ $<

f2/libf.a: f2/f.o
	ar cruvs $@ $<

f1/f.o: f1/f.F90
	$(F90) -c -o $@ $<

f2/f.o: f2/f.F90
	$(F90) -c -o $@ $<

clean:
	rm -rf f?/*o f?/*a *.o *.mod f?/*.mod main

replace: all
ifeq ($(OS),Darwin)
	for F in m1.o f1/f.o f1/libf.a; do $(OBC) -np:___f_MOD_:___f1_MOD_ $$F $$F.tmp &&  mv $$F.tmp $$F; done
else
	nm ./f2/libf.a | grep _f_MOD | $(AWK) '{printf $$3 " "; gsub("_f_MOD","_f1_MOD",$$3); print $$3}' > replace.tsv
	for F in m1.o f1/f.o f1/libf.a; do $(OBC) --redefine-syms=replace.tsv $$F ; done
endif
	mv f1/libf.a f1/libf1.a
	$(F90) -o main main.F90 m1.o m2.o -Lf2 -Lf1 -lf1 -lf && ./main
