all: main.F90 m2.o m1.o
	gfortran -o main $^ -L f2 -lf

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
