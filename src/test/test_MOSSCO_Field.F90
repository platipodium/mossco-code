!> @file test_MOSSCO_Field.F90
!! @brief test functionality of interface in mossco_field.F90
!! @author Carsten Lemmen, Richard Hofmeister
!!

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

program test_mossco_field

use esmf
use mossco_field

type(ESMF_field)     :: field, otherField
type(ESMF_Grid)      :: grid3, grid2
type(ESMF_ArraySpec) :: arraySpec
integer              :: localrc, n, rc
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
character(len=ESMF_MAXSTR), allocatable :: options(:)

call ESMF_initialize()

! Create a test field
grid2  = ESMF_GridCreateNoPeriDim(name='grid2D',  &
         minIndex=(/1,1/),maxIndex=(/5,6/), regDecomp=(/1,1/), rc=localrc)

grid3  = ESMF_GridCreateNoPeriDim(name='grid3D', &
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
if (sum(farrayPtr3) < 0.0 .or. sum(farrayPtr3) > 0.0) then
  write(*,*) '  failed with no value argument', sum(farrayPtr3),' /= 0.0'
endif

call MOSSCO_FieldInitialize(field, value=1.0D0, rc=localrc)
if (localrc /= ESMF_SUCCESS) then
  write(*,'(A)') '  failed with value argument'
endif

call ESMF_FieldGet(field, farrayPtr=farrayPtr3, rc=localrc)
if (sum(farrayPtr3) < 30.0D0  .or. sum(farrayPtr3) > 30.0D0) then
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
if (sum(farrayPtr3,mask=mask3>0) < 44.0D0 .or. sum(farrayPtr3,mask=mask3>0) > 44.0D0) then
  write(*,*) '  failed with 3D masked argument', sum(farrayPtr3,mask=mask3>0),' /= 44.0'
endif

!field = ESMF_FieldCreate(grid3=grid3,arrayspec=arrayspec,name="field", rc=localrc)

write(*,'(A)') 'Testing MOSSCO_FieldAdd'

call MOSSCO_FieldAdd(field, 2.0D0, rc=localrc)
call ESMF_FieldGet(field, farrayPtr=farrayPtr3, rc=localrc)
if (sum(farrayPtr3,mask=mask3>0) < 88.0D0 .or. sum(farrayPtr3,mask=mask3>0) > 88.0D0 ) then
  write(*,*) '  failed 3D masked with scalar argument', sum(farrayPtr3,mask=mask3>0),' /= 88.0'
endif

call MOSSCO_FieldAdd(field, field, rc=localrc)
call ESMF_FieldGet(field, farrayPtr=farrayPtr3, rc=localrc)
if (sum(farrayPtr3,mask=mask3>0) < 176.0D0  .or. sum(farrayPtr3,mask=mask3>0) > 176.0D0 ) then
  write(*,*) '  failed 3D masked with itself', sum(farrayPtr3, mask=mask3>0),' /= 176.0'
endif

write(*,'(A)') 'Testing MOSSCO_FieldValue'

call MOSSCO_FieldInitialize(field, value=8.0D0, rc=localrc)
real8=MOSSCO_FieldValue(field, operator='min')
if (int(real8) /= 8) then
  write(*,*) '  failed 3D masked min value', minval(farrayPtr3, mask=mask3>0),' /= ', real8
endif
real8=MOSSCO_FieldValue(field, operator='max')
if (int(real8) /= 8) then
  write(*,*) '  failed 3D masked max value', maxval(farrayPtr3, mask=mask3>0),' /= ', real8
endif
real8=MOSSCO_FieldValue(field, operator='mean')
if (int(real8) /= 8) then
  write(*,*) '  failed 3D masked mean value', sum(farrayPtr3, mask=mask3>0)/count(mask3>0),' /= ', real8
endif
real8=MOSSCO_FieldValue(field, operator='sum')
if (int(real8) /= 176) then
  write(*,*) '  failed 3D masked sum value', sum(farrayPtr3, mask=mask3>0),' /= ', real8
endif

write(*,'(A)') 'Testing MOSSCO_FieldString'
!write(*,*) farrayPtr3

allocate(options(4))
options(1)='creator'
options(2)='mean'
options(3)='geom'
options(4)='loc'

write(message,'(A)') ''
call MOSSCO_FieldString(field, message, options=options, rc=localrc)
if (trim(message) /= 'grid3D(r=3 5x6x1 m=22) O  8.00E+00') then
  write(*,*) '  failed 3D with options, " grid3D(r=3 5x6x1 m=22) O  8.00E+00" /= "'//trim(message)//'"'
endif

call ESMF_AttributeSet(field, 'creator', 'test', rc=localrc)
options(2)='sum'

write(message,'(A)') ''
call MOSSCO_FieldString(field, message, options=options, rc=localrc)
if (trim(message) /= ' [test]fieldgrid3D(r=3 5x6x1 m=22) O  1.76E+02') then
  write(*,*) '  failed 3D with options, " [test]fieldgrid3D(r=3 5x6x1 m=22) O  1.76E+02" /= "'//trim(message)//'"'
endif

options(1:4)=(/'min ','mean','max ','sum '/)
write(message,'(A)') ''
call MOSSCO_FieldString(field, message, options=options, rc=localrc)
if (trim(message) /= ' field  8.00E+00  1.76E+02  8.00E+00  8.00E+00') then
  write(*,*) '  failed 3D with options, " field  8.00E+00  1.76E+02  8.00E+00  8.00E+00" /= "'//trim(message)//'"'
endif

options(1:4)=(/'creator','geom   ','bounds ','       '/)
write(message,'(A)') ''
call MOSSCO_FieldString(field, message, options=options, rc=localrc)
if (trim(message) /= ' [test]fieldgrid3D(r=3 5x6x1 m=22) 1:5,1:6,1:1') then
  write(*,*) '  failed 3D with bounds option, " [test]fieldgrid3D(r=3 5x6x1 m=22) 1:5,1:6,1:1" /= "'//trim(message)//'"'
endif

call ESMF_FieldDestroy(field, rc=localrc)
!_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_I4, rc=localrc)
!_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_GridGet(grid2, distgrid=distgrid, rc=localrc)
!_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

array = ESMF_ArrayCreate(distgrid=distgrid, arrayspec=arrayspec, rc=localrc)
!_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_ArrayGet(array, farrayPtr=mask2, rc=localrc)
!_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

mask2=1            ! Mark all 30 elements as valid
mask2(2:5,3:4)=0 ! Mask out 8 elements
call ESMF_GridSetItem(grid2, itemFlag=ESMF_GRIDITEM_MASK, &
  staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=localrc)
!_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

field=ESMF_FieldCreate(name='field2D', grid=grid2, &
  staggerLoc=ESMF_STAGGERLOC_CENTER, typeKind=ESMF_TYPEKIND_R8, rc=localrc)
!_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call MOSSCO_FieldInitialize(field, value=3.0D0, rc=localrc)

write(*,'(A)') 'Testing MOSSCO_FieldAdd'

call MOSSCO_FieldAdd(field, 2.0D0, rc=localrc)
call ESMF_FieldGet(field, farrayPtr=farrayPtr2, rc=localrc)
if (sum(farrayPtr2,mask=mask2>0) < 110.0D0 .or. sum(farrayPtr2,mask=mask2>0) > 110.0D0 ) then
  write(*,*) '  failed 2D masked with scalar argument', sum(farrayPtr2,mask=mask2>0),' /= 110.0'
endif

call MOSSCO_FieldAdd(field, field, rc=localrc)
call ESMF_FieldGet(field, farrayPtr=farrayPtr2, rc=localrc)
if (sum(farrayPtr2,mask=mask2>0) < 220.0D0  .or. sum(farrayPtr2,mask=mask2>0) > 220.0D0 ) then
  write(*,*) '  failed 2D masked with itself', sum(farrayPtr2, mask=mask2>0),' /= 220.0'
endif


deallocate(options)

nullify(farrayPtr3)
nullify(farrayPtr2)
nullify(mask2)
nullify(mask3)
call ESMF_GridDestroy(grid3, rc=localrc)
!_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_GridDestroy(grid2, rc=localrc)
!_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_FieldDestroy(field, rc=localrc)
!_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_ArrayDestroy(array, rc=localrc)
!_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_DistGridDestroy(distGrid, rc=localrc)
!_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_GridDestroy(grid3, rc=localrc)
!_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

write(*,'(A)') 'All tests completed.'

call ESMF_finalize()

end program
