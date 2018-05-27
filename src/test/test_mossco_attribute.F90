!> @brief unit tests of attribute utilities
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2018, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#define ESMF_FILENAME "test_mossco_attribute.F90"
#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#define ESMF_METHOD "test_mossco_attribute"
program test_mossco_attribute

  use esmf
  use mossco_attribute

  implicit none

  integer(ESMF_KIND_I4)               :: rc, localrc, i
  integer(ESMF_KIND_I4)               :: int4
  integer(ESMF_KIND_I4), allocatable  :: int4list(:)
  integer(ESMF_KIND_I8)               :: int8
  integer(ESMF_KIND_I4), allocatable  :: int8list(:)
  real(ESMF_KIND_R4)                  :: real4
  real(ESMF_KIND_R4), allocatable     :: real4list(:)
  real(ESMF_KIND_R8)                  :: real8
  real(ESMF_KIND_R8 ), allocatable    :: real8list(:)
  logical                             :: boolean, isMatch
  logical, allocatable                :: booleanlist(:)
  character(len=ESMF_MAXSTR)          :: attributeString
  character(len=ESMF_MAXSTR), allocatable  :: attributeStrings(:)
  type(ESMF_State)                    :: state


  character(len=ESMF_MAXSTR)          :: string,format,remainder
  character(len=ESMF_MAXSTR), allocatable :: stringlist(:)

  call ESMF_Initialize(rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  write(*,'(A)') 'Testing interface "MOSSCO_AttributeGet"'
  state=ESMF_StateCreate(name='test', rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  allocate(attributeStrings(9))
  attributeStrings(1:9)=(/'1  ','1 2','1,2','1 X','1.2','1.X','1,,',',1,',',,1'/)

  do i = 1, ubound(attributeStrings,1)

    write(*,'(A)') '  test string "'//trim(attributeStrings(i))//'"'
    call ESMF_AttributeSet(state, 'test', attributeStrings(i), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !call MOSSCO_AttributeGet(state, 'test', string)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_AttributeGet(state, 'test', stringList)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !call MOSSCO_AttributeGet(state, 'test', int4)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_AttributeGet(state, 'test', int4List)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !call MOSSCO_AttributeGet(state, 'test', int8)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_AttributeGet(state, 'test', int8List)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !call MOSSCO_AttributeGet(state, 'test', real4)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !call MOSSCO_AttributeGet(state, 'test', real4List)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !call MOSSCO_AttributeGet(state, 'test', real8)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !call MOSSCO_AttributeGet(state, 'test', real8List)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  enddo

  deallocate(attributeStrings)
  call ESMF_StateDestroy(state, rc=localrc)

  write(*,'(A)') 'All tests done.'

  call ESMF_Finalize(rc=localrc)

end program
