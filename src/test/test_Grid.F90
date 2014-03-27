!> @file test_Grid.F90
!! @brief Tests reading and writing grids
!! @author Carsten Lemmen
!!

program test_Grid

use esmf

integer              :: rc
type(ESMF_Grid)      :: grid

call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN)

grid=ESMF_GridCreate( &
  filename="clm_grid.nc", fileFormat=ESMF_FILEFORMAT_SCRIP, &
  regDecomp=(/1,1/), &
  isSphere=.false., &
rc=rc)

call ESMF_GridDestroy(grid)

call ESMF_Finalize()

end program
