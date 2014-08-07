!> @file test_ESMF_Initialize.F90
!! @brief test initialization  and Finalization of ESMF 
!! @author Carsten Lemmen
!!

program test_ESMF_Initialize

use esmf

call ESMF_Initialize()
call ESMF_Finalize()

end program 
