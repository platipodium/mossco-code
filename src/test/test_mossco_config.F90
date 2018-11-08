!> @brief unit tests of config utilities
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2017, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>

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
  use mossco_config

  character(len=ESMF_MAXSTR)        :: filename = 'test_mossco_config.cfg'
  type(ESMF_Config)                 :: config
  integer(ESMF_KIND_I4)             :: rc

  integer(ESMF_KIND_I4)               :: int4
  integer(ESMF_KIND_I4), allocatable  :: int4list(:)
  integer(ESMF_KIND_I8)               :: int8
  integer(ESMF_KIND_I4), allocatable  :: int8list(:)
  integer(ESMF_KIND_R4)               :: real4
  integer(ESMF_KIND_R4), allocatable  :: real4list(:)
  integer(ESMF_KIND_R8)               :: real8
  integer(ESMF_KIND_R8 ), allocatable :: real8list(:)
  logical                             :: boolean
  logical, allocatable                :: booleanlist(:)

  character(len=ESMF_MAXSTR)          :: string
  character(len=ESMF_MAXSTR), allocatable :: stringlist(:), stringtable(:,:)

  call ESMF_Initialize(rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  config = ESMF_ConfigCreate(rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call ESMF_ConfigLoadFile(config, filename, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  ! MOSSCO_ConfigGetInt4
  call MOSSCO_ConfigGet(config, label='int4', value=int4, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  ! MOSSCO_ConfigGetInt8
  call MOSSCO_ConfigGet(config, label='int8', value=int8, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  ! MOSSCO_ConfigGetReal4
  call MOSSCO_ConfigGet(config, label='real4', value=real4, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  ! MOSSCO_ConfigGetReal8
  call MOSSCO_ConfigGet(config, label='real8', value=real8, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  ! MOSSCO_ConfigGetLogical
  call MOSSCO_ConfigGet(config, label='boolean', value=boolean, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  ! MOSSCO_ConfigGetString
  call MOSSCO_ConfigGet(config, label='string', value=string, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  !> Now test all simple lists

  ! subroutine MOSSCO_ConfigGetListInt4(config, label, value, &
  ! subroutine MOSSCO_ConfigGetListReal4(config, label, value, kwe, &
  ! subroutine MOSSCO_ConfigGetListReal8(config, label, value, &
  ! subroutine MOSSCO_ConfigGetListInt8(config, label, value, &

  ! MOSSCO_ConfigGetStringList
  call MOSSCO_ConfigGet(config, label='stringlistaslist', value=stringlist, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  ! MOSSCO_ConfigGetStringTable
  call MOSSCO_ConfigGet(config, label='stringlistastable', value=stringlist, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  !> Test a key=value list
  ! MOSSCO_ConfigGetStringListList
  call MOSSCO_ConfigGet(config, label='stringtableaslist', value=stringtable, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  !> Test a string table
  ! MOSSCO_ConfigGetStringTable
  call MOSSCO_ConfigGet(config, label='stringtableastable', value=stringtable, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
  write(6,'(4(A1,X))') stringtable

  call MOSSCO_ConfigGet(config, label='stringtableastable1', value=stringtable, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
  write(6,'(4(A1,X))') stringtable

  call MOSSCO_ConfigGet(config, label='stringtableastable2', value=stringtable, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
  write(6,'(4(A1,X))') stringtable

  call MOSSCO_ConfigGet(config, label='stringtableastable3', value=stringtable, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
  write(6,'(4(A1,X))') stringtable

  ! subroutine MOSSCO_ConfigGetStringListTable(config, label, value, &
  ! subroutine MOSSCO_ConfigGetStringListList(config, label, value, kwe, &
  ! subroutine MOSSCO_ConfigGetStringTableKeyValue(config, label, value, kwe, &
  ! subroutine MOSSCO_ConfigGetStringTableTable(config, label, value, &
  ! subroutine MOSSCO_ConfigGetStringTable(config, label, value, kwe, &
  ! subroutine MOSSCO_ConfigGetStringListTable1(config, label, value, kwe, isPresent, rc)
  ! subroutine MOSSCO_ConfigGetFileStringTable(fileName, label, value, &
  !

end program
