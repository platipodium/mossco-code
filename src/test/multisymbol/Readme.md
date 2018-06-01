1. Issue make first

`make`

2. Recompile with different order of libraries

gfortran -o main main.F90 -Lf1 -lf m2.o -Lf2 -lf m1.o && ./main
 I am first
 I am first

3. Another order

gfortran -o main main.F90 -Lf2 -lf m2.o -Lf1 -lf m1.o && ./main
 I am second
 I am second

How can I make sure (without manipulating anything in the source of either libf) that 
m1 links to ./f1/libf.a and m2 links to ./f2/libf.a ?

Desired outcome

  I am first
  I am second
