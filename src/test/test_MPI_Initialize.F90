!> @file test_MPI_Initialize.F90
!! @brief test initialization  and Finalization of MPI
!! @author Carsten Lemmen
!!

program test_MPI_Initialize


use mpi
implicit none

integer ierr, size, rank, rc 
logical flag

call MPI_Finalized(flag, ierr)
if (flag) then
  write(0,*) 'MPI finalized before initialized'
endif

call MPI_Initialized(flag, ierr)
if (flag) then
  write(0,*) 'MPI initialized before initialized'
endif

      call mpi_init_thread( MPI_THREAD_MULTIPLE, -1, ierr )

call MPI_Init(ierr)
if (ierr /= MPI_SUCCESS) then
  write(0,*) 'MPI could not be initialized'
  call MPI_Abort(MPI_COMM_WORLD, rc, ierr)
endif


call MPI_Initialized(flag, ierr)
if (.not.flag) then
  write(0,*) 'MPI not initialized after initialize'
endif


call MPI_Comm_Size(MPI_COMM_WORLD, size, ierr)
if (ierr /= MPI_SUCCESS) then
  write(0,*) 'MPI could not be get COMM_SIZE'
  call MPI_Abort(MPI_COMM_WORLD, rc, ierr)
endif

call MPI_Comm_Rank(MPI_COMM_WORLD, rank, ierr)
if (ierr /= MPI_SUCCESS) then
  write(0,*) 'MPI could not be get COMM_RANK'
  call MPI_Abort(MPI_COMM_WORLD, rc, ierr)
endif

write(*,'(A,I2,A,I2,A)') 'MPI running on PET ',rank,' of total ',size,' PETs.'
call MPI_Finalize(ierr)

end program 
