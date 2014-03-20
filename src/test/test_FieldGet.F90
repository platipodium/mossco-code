!> @file test_FieldGet.F90
!! @brief test ESMF_FieldGet performance
!! @author Carsten Lemmen, Richard Hofmeister
!!
!! runs 1e6 times the ESMF routine FieldGet to retrieve the
!! array pointer from an ESMF_Field. The performance is much
!! improved in ESMF v.6.3beta4+

program testfieldget

use esmf

type(ESMF_FIELD)     :: field
type(ESMF_GRID)      :: grid
type(ESMF_ArraySpec) :: arrayspec
real(ESMF_KIND_R8),dimension(:,:,:),pointer :: pointer
integer              :: rc
integer(kind=8)      :: i,imax=1000000

call esmf_initialize()

! Create a test field
grid  = ESMF_GridCreateNoPeriDim( &
         minIndex=(/1,1,1/),maxIndex=(/1,1,25/), regDecomp=(/1,1,1/),rc=rc)
call ESMF_ArraySpecSet(arrayspec, rank=3, typekind=ESMF_TYPEKIND_R8, rc=rc)
field = ESMF_FieldCreate(grid=grid,arrayspec=arrayspec,name="test field",rc=rc)

! get the farrayPtr <imax> times
!   imax=1000000 refers to coupling of 10 fields for an annual simulation
!   with appr. 6 min coupling timestep
do i = 1,imax
  call ESMF_FieldGet(field,farrayPtr=pointer,rc=rc)
end do

call esmf_finalize()

end program
