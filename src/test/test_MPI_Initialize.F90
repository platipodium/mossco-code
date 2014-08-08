!> @file test_MPI_Initialize.F90
!! @brief test initialization  and Finalization of MPI
!! @author Carsten Lemmen
!!

program test_MPI_Initialize

#ifdef MOSSCO_MPI

use mpi
implicit none

integer ierror, petCount, localPet, rc

call MPI_Init(ierror)
if (ierror /= MPI_SUCCESS) then
  write(0,*) 'MPI could not be initialized'
  call MPI_Abort(MPI_COMM_WORLD, rc, ierror)
endif

call MPI_Comm_Size(MPI_COMM_WORLD, petCount, ierror)
call MPI_Comm_Rank(MPI_COMM_WORLD, localPet, ierror)

write(*,'(A,I2,A,I2,A)') 'MPI running on PET ',localPet,' of total ',petCount,' PETs.'
call MPI_Finalize(ierror)

#endif

end program 
