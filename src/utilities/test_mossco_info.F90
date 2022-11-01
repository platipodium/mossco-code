
!> @brief Implementation of test for extensions to the ESMF Info utilities
!
!  This computer program is part of MOSSCO.
!> @copyright 2022 Helmholtz-Zentrum Hereon
!> @author Carsten Lemmen <carsten.lemmen@hereon.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "test_mossco_info.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#ifndef VARLEN
#define VARLEN ESMF_MAXSTR
#endif

#define ESMF_METHOD "test_mossco_info"
program test_mossco_info

  use esmf
  use mossco_info

  type(ESMF_Log)   :: customLog, defaultLog
  type(ESMF_field) :: field
  type(ESMF_State) :: state
  type(ESMF_GridComp) :: gridComp
  type(ESMF_CplComp)  :: cplComp
  type(ESMF_Info)     :: info, from, to

  integer(ESMF_KIND_I4) :: rc, localrc, int4
  character(len=ESMF_MAXSTR) :: key, string

  localrc = ESMF_SUCCESS

  call ESMF_Initialize(rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  info = ESMF_InfoCreate('{"int":1,"real":4.5,"bool":true,"map":{"A":1,"B":2.0},"slist":["aL","bL"],"ilist":[1,3]}', rc=localrc)
  call ESMF_InfoSet(info, key="/sub/C", value=.true., rc=localrc)
  call ESMF_InfoSet(info, key="sub/D", value=.false., rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  from = ESMF_InfoCreate(info, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  to = ESMF_InfoCreate(rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  !> Test private MOSSCO_InfoCopyKey throught MOSSCO_InfoCopy interface
  !> optional args are typeKind and rc
  write(0,*) 'Testing element copies bool'
  call MOSSCO_InfoCopy(to, from, "bool")
  write(0,*) 'Testing element copies real'
  call MOSSCO_InfoCopy(to, from, "real")
  write(0,*) 'Testing element copies int'
  call MOSSCO_InfoCopy(to, from, "int")
  write(0,*) 'Testing element copies map'
  call MOSSCO_InfoCopy(to, from, "map")
  write(0,*) 'Testing element copies sub/C'
  call MOSSCO_InfoCopy(to, from, "sub/C")
  write(0,*) 'Testing element copies ilist'
  call MOSSCO_InfoCopy(to, from, "ilist")
  write(0,*) 'Testing element copies slist'
  call MOSSCO_InfoCopy(to, from, "slist")

  call ESMF_InfoPrint(from)
  call ESMF_InfoPrint(to)

  call ESMF_InfoDestroy(to)

  !call MOSSCO_InfoCopy(to, from, key, rc=localrc)
  !call MOSSCO_InfoCopy(to, from, key, typeKind=ESMF_TYPEKIND_I4, rc=localrc)

  !> Test private MOSSCO_InfoCopyAll throught MOSSCO_InfoCopy interface
  !> optional args are overwrite, rc, root
  to = ESMF_InfoCreate(rc=localrc)

  !call MOSSCO_InfoCopy(to, from)
  write(0,*) 'Testing entire copy'
  call MOSSCO_InfoCopy(to, from, rc=localrc)
  call ESMF_InfoPrint(to)
  !call MOSSCO_InfoCopy(to, from, overwrite=.false., rc=localrc)
  !call MOSSCO_InfoCopy(to, from, overwrite=.true., rc=localrc)
  !call MOSSCO_InfoCopy(to, from, root="/", rc=localrc)
  !call MOSSCO_InfoCopy(to, from, root="level1", rc=localrc)

  !> Test logging
  write(0,*) 'Testing log'
  call MOSSCO_InfoLog(from, rc=localrc)
  !call MOSSCO_InfoLogObject(gridComp, rc=localrc)
  !call MOSSCO_InfoLogObject(cplComp, rc=localrc)
  !call MOSSCO_InfoLogObject(field, rc=localrc)

  call ESMF_Finalize()

end program test_mossco_info
