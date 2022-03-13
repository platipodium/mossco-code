!> @brief Implementation of tests for ESMF link coupling
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2018 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, <carsten.lemmen@hereon.de>

! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "test_link_connector.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#define ESMF_METHOD "test_link_connector"
program test_link_connector

use esmf
use mossco_state
use mossco_time
use mossco_memory
use mossco_component
use mossco_field
use link_connector, only : link_connector_SetServices => SetServices

implicit none

type(ESMF_Clock)       :: clock
type(ESMF_Time)        :: startTime, stopTime
type(ESMF_CplComp)     :: linkConnector
type(ESMF_Field)       :: field
type(ESMF_FieldBundle) :: fieldBundle
type(ESMF_Field), allocatable :: exportFieldList(:), importFieldList(:)
type(ESMF_State)       :: importState, exportState
integer                :: localrc, i, exportFieldCount, importFieldCount, rc
character(len=ESMF_MAXSTR) :: message

call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

! Create and initialize a clock from mossco_run.nml
call MOSSCO_TimeSet(startTime, '2018-12-01 00:00:00', localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call MOSSCO_TimeSet(stopTime, '2018-12-31 00:00:00', localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

clock = ESMF_ClockCreate(startTime=startTime, stopTime=stopTime, &
  reftime=startTime, timeStep=(stopTime-startTime)/5, name='clock', rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

linkConnector = ESMF_CplCompCreate(name='link_connector', rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_CplCompSet(linkConnector, clock=clock, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_CplCompSetServices(linkConnector, link_connector_SetServices, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

exportState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_EXPORT, &
  name='exportState')
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

importState = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_IMPORT, &
  name='importState')
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

!> Phase 0
call ESMF_CplCompInitialize(linkConnector, exportState=exportState, &
  importState=importState, clock=clock, phase=0, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

!> Phase 1
call ESMF_CplCompInitialize(linkConnector, exportState=exportState, &
  importState=importState, clock=clock, phase=1, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call MOSSCO_StateGet(exportState, fieldList=exportFieldList, &
  fieldCount=exportFieldCount, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

do i=1,exportFieldCount
  write(message,'(A)') 'After I1/1 exportState contains '
  call MOSSCO_FieldString(exportFieldList(i), message)
  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
enddo

field =  ESMF_FieldEmptyCreate(name='emptyField', rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

fieldBundle = ESMF_FieldBundleCreate(name='bundledField', multiflag=.true.,rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_StateAddReplace(importState, (/field/), rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_StateAddReplace(importState, (/fieldBundle/), rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_CplCompInitialize(linkConnector, exportState=exportState, &
  importState=importState, clock=clock, phase=1, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call MOSSCO_StateGet(exportState, exportFieldList, &
  fieldCount=exportFieldCount, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

do i=1,exportFieldCount
  write(message,'(A)') 'After I1/2 exportState contains '
  call MOSSCO_FieldString(exportFieldList(i), message)
  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
enddo

field = ESMF_FieldEmptyCreate(name='bundledField', rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_FieldBundleAdd(fieldBundle, (/field/), multiflag=.true., rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

field = ESMF_FieldEmptyCreate(name='bundledField', rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_CplCompRun(linkConnector, exportState=exportState, &
  importState=importState, clock=clock, phase=1, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call MOSSCO_StateGet(exportState, exportFieldList, &
  fieldCount=exportFieldCount, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

do i=1,exportFieldCount
  write(message,'(A)') 'After R1 exportState contains '
  call MOSSCO_FieldString(exportFieldList(i), message)
  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
enddo


!> Clean up

call ESMF_CplCompFinalize(linkConnector, exportState=exportState, &
  importState=importState, clock=clock, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call MOSSCO_Reallocate(exportFieldList, 0 , rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call MOSSCO_Reallocate(importFieldList, 0 , rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_FieldBundleDestroy(fieldBundle, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_StateDestroy(exportState, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_StateDestroy(importState, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_CplCompDestroy(linkConnector, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_ClockDestroy(clock, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_Finalize()

write(*,'(A)') 'All tests completed.'

end program
