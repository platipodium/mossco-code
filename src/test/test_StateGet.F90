!> @file test_StateGet.F90
!! @brief test ESMF_StateGet performance
!! @author Carsten Lemmen, Richard Hofmeister
!!
!! runs 1e6 times the ESMF routines around StateGet to check
!! for state items, item types, field status and retrieve the
!! array pointer from the ESMF_Field.

program teststateget

use esmf

type(ESMF_FIELD)            :: field, statefield
type(ESMF_STATEITEM_FLAG)   :: itemType
type(ESMF_STATE)            :: state
type(ESMF_GRID)             :: grid
type(ESMF_ArraySpec)        :: arrayspec
real(ESMF_KIND_R8),dimension(:,:,:),pointer :: pointer
integer                     :: rc
integer(kind=8)             :: i,imax=1000000
integer(ESMF_KIND_I4)       :: itemCount
type(ESMF_FieldStatus_Flag) ::fieldStatus

call esmf_initialize()

! Create a state
state = ESMF_StateCreate(rc=rc)

! Create a test field
grid  = ESMF_GridCreateNoPeriDim( &
         minIndex=(/1,1,1/),maxIndex=(/1,1,25/), regDecomp=(/1,1,1/),rc=rc)
call ESMF_ArraySpecSet(arrayspec, rank=3, typekind=ESMF_TYPEKIND_R8, rc=rc)
field = ESMF_FieldCreate(grid=grid,arrayspec=arrayspec,name="test field",rc=rc)

! Add field to state
call ESMF_StateAdd(state, (/ field /), rc=rc)

! get the farrayPtr <imax> times
!   imax=1000000 refers to coupling of 10 fields for an annual simulation
!   with appr. 6 min coupling timestep
do i = 1,imax
  ! check for itemCount - 420 ms
  call ESMF_StateGet(state, itemSearch='test-field',itemCount=itemCount, rc=rc)
  ! check for state item (expected NOTFOUND) - 230 ms
  call ESMF_StateGet(state, 'test field 2',itemType, rc=rc)
  ! check for existing state item - 430 ms
  call ESMF_StateGet(state, 'test field',itemType, rc=rc)
  ! get field from state (this takes most time) - 627 ms
  call ESMF_StateGet(state, 'test field',statefield, rc=rc)
  ! get field status (this is cheap) - 100 ms
  call ESMF_FieldGet(statefield, status=fieldStatus, rc=rc)
  ! get the pointer - 210 ms
  call ESMF_FieldGet(statefield,farrayPtr=pointer,rc=rc)
end do

call esmf_finalize()

end program
