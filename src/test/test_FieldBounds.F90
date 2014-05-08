!> @file test_GridField.F90
!! @brief test the communication of Field data from different PET
!! @author Carsten Lemmen

program test_gridfield

use esmf
implicit none

type(ESMF_FIELD)       :: field, field1, field2
type(ESMF_GRID)        :: grid, grid1, grid2
type(ESMF_ArraySpec)   :: arrayspec
real(ESMF_KIND_R8),dimension(:,:),pointer :: farrayPtr
integer                :: rc
integer(ESMF_KIND_I4), dimension(2) :: totalLBound, totalUBound
character(ESMF_MAXSTR) :: message
integer(ESMF_KIND_I4)  :: petCount, localPet
type(ESMF_VM)          :: vm

call ESMF_Initialize()
call ESMF_VMGetGlobal(vm=vm, rc=rc)
call ESMF_VMGet(vm,localPet=localPet, petCount=petCount, rc=rc)

! Create a test field
grid  = ESMF_GridCreateNoPeriDim( &
         minIndex=(/1,1/),maxIndex=(/5,7/), rc=rc)

call ESMF_GridGetFieldBounds(grid, totalLBound=totalLBound, totalUBound=totalUBound, rc=rc)
write(message,'(A,I1,A,I1,A,I1,A,I1,A)') 'FieldBounds: (',totalLBound(1),':',totalUBound(1),&
  ')(',totalLBound(2),':',totalUBound(2),')'
call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)


call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R8, rc=rc)
field = ESMF_FieldCreate(grid=grid,arrayspec=arrayspec,name="field",rc=rc)

call ESMF_FieldGet(field,farrayPtr=farrayPtr,rc=rc)
farrayPtr(totalLBound(1):totalUBound(1),totalLBound(2):totalUBound(2))=localPet*1.0D0


call ESMF_Finalize()

end program
