!> @brief Implementation of grid utilities
!!
!! This computer program is part of MOSSCO. 
!! @copyright Copyright 2014, Helmholtz-Zentrum Geesthacht
!! @author Carsten Lemmen, HZG
!! @author Hartmut Kapitza, HZG

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "mossco_grid.F90"

module mossco_grid

use esmf

implicit none

contains
  
#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GridCreateRegional3D"
function MOSSCO_GridCreateRegional3D(name, rc) result(grid)
  
	character(ESMF_MAXSTR), intent(in) :: name
	integer,  intent(out)              :: rc
	type(ESMF_Grid)                    :: grid

	integer(ESMF_KIND_I4)      :: minIndex(3), maxIndex(3), regDecomp(3)
	type(ESMF_Index_Flag)      :: indexFlag
	type(ESMF_CoordSys_Flag)   :: coordSys
	integer                    :: localrc, i, lbnd(3), ubnd(3)
  real(ESMF_KIND_R8),dimension(:),pointer :: coordX, coordY  	
	
	rc = ESMF_SUCCESS
	
	minIndex=(/1,1,1/)
	maxIndex=(/40,50,10/)
	regDecomp=(/4,3,3/)
	coordSys=ESMF_COORDSYS_SPH_DEG
	indexFlag=ESMF_INDEX_GLOBAL

  grid = ESMF_GridCreateNoPeriDim(minIndex=minIndex, maxIndex=maxIndex, &
    regDecomp=regDecomp, coordSys=coordSys, indexFlag=indexFlag,  &
    name=trim(name)//' grid', coordTypeKind=ESMF_TYPEKIND_R8, coordDep1=(/1/), &
    coorddep2=(/2/), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_GridAddCoord(grid,staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  !> This example grid is a 40 x 50 grid at 0.1 degree resolution from 0..4 deg East
  !> to 50 .. 55 deg North
  call ESMF_GridGetCoord(grid,coordDim=1,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
    computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordX, rc=rc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  do i=lbnd(1),ubnd(1) 
    coordX(i) = 0 + 0.1 * i + 0.05
  enddo
  call ESMF_GridGetCoord(grid,coordDim=2,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
    computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordY, rc=rc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  do i=lbnd(1),ubnd(1) 
    coordY(i) = 50 + 0.1 * i + 0.05
  enddo
  if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  return
  
end function MOSSCO_GridCreateRegional3D
  
end module mossco_grid