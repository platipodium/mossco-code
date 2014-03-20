!> @file test_ArrayGet.F90
!! @brief test ESMF_ArrayGet performance
!! @author Carsten Lemmen, Richard Hofmeister
!!
!! runs 1e6 times the ESMF routine ArrayGet to retrieve the
!! array pointer from an ESMF_Array. The performance is much
!! improved in ESMF v.6.3beta4+

program test_ArrayGet
! Tests the speed of the call ESMF_ArrayGEt(array,farrayPtr=farrayPtr)

use esmf

type(ESMF_Array)     :: array
type(ESMF_DistGrid)  :: distgrid
real(ESMF_KIND_R8),dimension(:,:,:),pointer :: farrayPtr
real(ESMF_KIND_R8),dimension(:,:,:),allocatable, target :: farray
integer              :: rc
integer(kind=8)      :: i,imax=1000000

call ESMF_Initialize()

! Create a test array
allocate(farray(1,1,25))
distgrid = ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/1,1,25/), &
regDecomp=(/1,1,1/), rc=rc)
array = ESMF_ArrayCreate(distgrid,farray,indexflag=ESMF_INDEX_DELOCAL,rc=rc)

! get the farrayPtr <imax> times
!   imax=1000000 refers to coupling of 10 fields for an annual simulation
!   with appr. 6 min coupling timestep
do i = 1,imax
  call ESMF_ArrayGet(array,farrayPtr=farrayPtr,rc=rc)
end do

call ESMF_Finalize()

end program
