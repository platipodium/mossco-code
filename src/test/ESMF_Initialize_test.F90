!> @file ESMF_Initialize_test.F90
!! @author Carsten Lemmen
!!
!! Initializes and Finalizes ESMF

program esmf_initialize_test

use esmf

call ESMF_Initialize()
call ESMF_Finalize()

end program
