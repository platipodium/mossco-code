!> @file test_MOSSCO_Field.F90
!! @brief test functionality of interface in mossco_field.F90
!! @author Carsten Lemmen, Richard Hofmeister
!!

program test_mossco_field

use esmf
use mossco_field

type(ESMF_field)     :: field, otherField
type(ESMF_Grid)      :: grid
type(ESMF_ArraySpec) :: arraySpec
integer              :: localrc, n
character(len=ESMF_MAXSTR) :: message
real(kind=ESMF_KIND_R8) :: real8 = 1.0
real(kind=ESMF_KIND_R4) :: real4 = 1.0
real(kind=ESMF_KIND_I8) :: int8 = 1
real(kind=ESMF_KIND_I4) :: int4 = 1

call ESMF_initialize()

! Create a test field
grid  = ESMF_GridCreateNoPeriDim( &
         minIndex=(/1,1,1/),maxIndex=(/1,5,6/), regDecomp=(/1,1,1/), rc=localrc)
call ESMF_ArraySpecSet(arrayspec, rank=3, typekind=ESMF_TYPEKIND_R8, rc=localrc)
field = ESMF_FieldCreate(grid=grid,arrayspec=arrayspec,name="field", rc=localrc)
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

call ESMF_FieldEmptySet(otherField, grid=grid, staggerLoc=ESMF_STAGGERLOC_CORNER, &
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

call ESMF_finalize()

end program
