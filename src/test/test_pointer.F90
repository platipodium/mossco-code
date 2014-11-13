program test_pointer

logical, dimension(:,:,:), pointer :: values=>null(),ptr=>null()

allocate(values(20,10,5))
values(:,:,:) = .false.
write(0,*) 'original data modified:',values(6,2,3) 

ptr => values(5:15,2:4,3:5)
write(*,*) lbound(ptr),ubound(ptr)
ptr(2,1,1) = .true.
write(0,*) 'data modified with pointer:',values(6,2,3) 

end program
