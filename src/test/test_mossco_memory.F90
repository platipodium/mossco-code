!> @brief unit tests of memory utilities
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2018, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hereon.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "test_mossco_config.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

program test_mossco_config

  use ESMF
  use mossco_memory

  integer(ESMF_KIND_I4)             :: rc, localrc, i, itemCount, fieldCount

  type(ESMF_Field), dimension(:), allocatable            :: fieldList
  type(ESMF_StateItem_Flag), dimension(:), allocatable   :: itemTypeList
  character(len=ESMF_MAXSTR), dimension(:), allocatable  :: stringList,itemNameList
  character(len=ESMF_MAXSTR), dimension(:,:), allocatable:: stringDuoList
  type(ESMF_State)                                       :: state

  call ESMF_Initialize(rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call MOSSCO_Reallocate(stringList, 2, keep=.false., rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
  stringList(:) = (/'string1','string2'/)

  call MOSSCO_Reallocate(stringList, 4, keep=.false., rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
  stringList(3:4) = (/'string3','string4'/)

  fieldcount = 4
  call MOSSCO_Reallocate(fieldList, fieldCount, keep=.false., rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  state=ESMF_StateCreate(name = 'state', rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  do i=1, fieldCount
    fieldList(i)=ESMF_FieldEmptyCreate(name=stringList(i), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
  enddo

  call ESMF_StateAddReplace(state, fieldList, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_StateGet(state, itemCount=itemCount, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call MOSSCO_Reallocate(itemTypeList, itemCount, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call MOSSCO_Reallocate(itemNameList, itemCount, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_StateGet(state, itemNameList=itemNameList, itemTypeList=itemTypeList, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  do i=1,itemCount
    write(*,*) '  '//trim(itemNameList(i))//' of type ',itemTypeList(i)
  enddo

  call MOSSCO_Reallocate(itemTypeList, 0, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call MOSSCO_Reallocate(stringList, 0, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call MOSSCO_Reallocate(stringDuoList, 0, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_Finalize(endflag=ESMF_END_NORMAL)

end program
