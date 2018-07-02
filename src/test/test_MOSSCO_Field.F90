!> @file test_MOSSCO_Field.F90
!! @brief test functionality of interface in mossco_field.F90
!! @author Carsten Lemmen, Richard Hofmeister
!!

program test_mossco_field

use esmf
use mossco_field

type(ESMF_field)     :: field, otherField
type(ESMF_Grid)      :: grid3
type(ESMF_ArraySpec) :: arraySpec
integer              :: localrc, n
character(len=ESMF_MAXSTR) :: message
real(kind=ESMF_KIND_R8) :: real8 = 1.0
real(kind=ESMF_KIND_R4) :: real4 = 1.0
real(kind=ESMF_KIND_I8) :: int8 = 1
real(kind=ESMF_KIND_I4) :: int4 = 1
real(kind=ESMF_KIND_R8), pointer    :: farrayPtr3(:,:,:) => null()
real(kind=ESMF_KIND_R8), pointer    :: farrayPtr2(:,:) => null()
type(ESMF_Array)        :: array
integer(ESMF_KIND_I4), pointer :: mask2(:,:), mask3(:,:,:)
type(ESMF_DistGrid)     :: distGrid

call ESMF_initialize()

! Create a test field
grid3  = ESMF_GridCreateNoPeriDim( &
         minIndex=(/1,1,1/),maxIndex=(/5,6,1/), regDecomp=(/1,1,1/), rc=localrc)
call ESMF_ArraySpecSet(arrayspec, rank=3, typekind=ESMF_TYPEKIND_R8, rc=localrc)
field = ESMF_FieldCreate(grid=grid3,arrayspec=arrayspec,name="field", rc=localrc)
write(message,'(A)') 'Created '
call MOSSCO_FieldString(field, message)
call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

otherField = ESMF_FieldEmptyCreate(name="otherField", rc=localrc )

write(message,'(A)') 'Created '
call MOSSCO_FieldString(otherField,message)
call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

write(message,'(A)') 'Test length on '
call MOSSCO_FieldString(otherField, message, length=n)
write(message,'(A,I3)') trim(message)//' length ',n
call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

write(message,'(A)') 'Test prefix on '
call MOSSCO_FieldString(otherField, message, prefix='PREFIX: ')
call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

write(message,'(A)') 'Test return code on '
call MOSSCO_FieldString(otherField, message, rc=localrc)
call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

call ESMF_FieldEmptySet(otherField, grid=grid3, staggerLoc=ESMF_STAGGERLOC_CORNER, &
  rc=localrc)
write(message,'(A)') 'Set grid on '
call MOSSCO_FieldString(otherField, message, rc=localrc)
call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

call ESMF_AttributeSet(field, 'string1',value='string_')
call ESMF_AttributeSet(field, 'string2',value='string')
call ESMF_AttributeSet(field, 'logical',value=.false.)
call ESMF_AttributeSet(field, 'int4',value=int4)

call ESMF_AttributeSet(otherField, 'string1',value='string_')
call ESMF_AttributeSet(otherField, 'string2',value='stringbla')
call ESMF_AttributeSet(otherField, 'logical',value=.true.)
call ESMF_AttributeSet(otherField, 'int4',value=int4*2)

n = MOSSCO_FieldAttributesIdentical(field, otherField, rc=localrc)
write(message,'(A,I2.2,A)') 'found  ',n,' differing attributes in fields'
call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

call ESMF_FieldDestroy(field, rc=localrc)
call ESMF_FieldDestroy(otherField, rc=localrc)

write(*,'(A)') 'Testing MOSSCO_FieldInitialize ...'

field = ESMF_FieldEmptyCreate(name="field", rc=localrc)
call MOSSCO_FieldInitialize(field, rc=localrc)
if (localrc /= ESMF_RC_ARG_BAD) then
  write(*,'(A)') '  failed with empty field'
endif

call ESMF_FieldEmptySet(field, grid=grid3, rc=localrc)

call MOSSCO_FieldInitialize(field, rc=localrc)
if (localrc /= ESMF_RC_ARG_BAD) then
  write(*,'(A)') '  failed with gridset field'
endif

call ESMF_FieldEmptyComplete(field, arrayspec=arrayspec, rc=localrc)
call MOSSCO_FieldInitialize(field, rc=localrc)
if (localrc /= ESMF_SUCCESS) then
  write(*,'(A)') '  failed with complete field'
endif

call MOSSCO_FieldInitialize(field, owner='test', rc=localrc)
if (localrc /= ESMF_SUCCESS) then
  write(*,'(A)') '  failed with owner optional argument'
endif
call ESMF_FieldGet(field, farrayPtr=farrayPtr3, rc=localrc)
if (sum(farrayPtr3) /= 0.0) then
  write(*,*) '  failed with no value argument', sum(farrayPtr3),' /= 0.0'
endif

call MOSSCO_FieldInitialize(field, value=1.0D0, rc=localrc)
if (localrc /= ESMF_SUCCESS) then
  write(*,'(A)') '  failed with value argument'
endif

call ESMF_FieldGet(field, farrayPtr=farrayPtr3, rc=localrc)
if (sum(farrayPtr3) /= 30.0D0 ) then
  write(*,*) '  failed with value argument', sum(farrayPtr3),' /= 30.0'
endif
nullify(farrayPtr3)

call ESMF_ArraySpecSet(arrayspec, rank=3, typekind=ESMF_TYPEKIND_I4, rc=localrc)
call ESMF_GridGet(grid3, distgrid=distgrid, rc=localrc)
array = ESMF_ArrayCreate(distgrid=distgrid, arrayspec=arrayspec, rc=localrc)
call ESMF_ArrayGet(array, farrayPtr=mask3, rc=localrc)
mask3=1            ! Mark all 30 elements as valid
mask3(2:5,3:4,1)=0 ! Mask out 8 elements

call ESMF_GridSetItem(grid3, itemFlag=ESMF_GRIDITEM_MASK, array=array, rc=localrc)

call MOSSCO_FieldInitialize(field, value=2.0D0, rc=localrc)
call ESMF_FieldGet(field, farrayPtr=farrayPtr3, rc=localrc)
if (sum(farrayPtr3,mask=mask3>0) /= 44.0D0 ) then
  write(*,*) '  failed with 3D masked argument', sum(farrayPtr3,mask=mask3>0),' /= 44.0'
endif

!field = ESMF_FieldCreate(grid3=grid3,arrayspec=arrayspec,name="field", rc=localrc)

write(*,'(A)') 'Testing MOSSCO_FieldAdd'

call MOSSCO_FieldAdd(field, 2.0D0, rc=localrc)
call ESMF_FieldGet(field, farrayPtr=farrayPtr3, rc=localrc)
if (sum(farrayPtr3,mask=mask3>0) /= 88.0D0 ) then
  write(*,*) '  failed 3D masked with scalar argument', sum(farrayPtr3,mask=mask3>0),' /= 88.0'
endif

call MOSSCO_FieldAdd(field, field, rc=localrc)
call ESMF_FieldGet(field, farrayPtr=farrayPtr3, rc=localrc)
if (sum(farrayPtr3,mask=mask3>0) /= 176.0D0 ) then
  write(*,*) '  failed 3D masked with itself', sum(farrayPtr3, mask=mask3>0),' /= 176.0'
endif

nullify(farrayPtr3)
nullify(mask2)
nullify(mask3)
call ESMF_FieldDestroy(field, rc=localrc)
call ESMF_ArrayDestroy(array, rc=localrc)
call ESMF_DistGridDestroy(distGrid, rc=localrc)
call ESMF_GridDestroy(grid3, rc=localrc)

write(*,'(A)') 'All tests completed.'

call ESMF_finalize()

end program
