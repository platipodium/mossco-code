!> @brief Implementation of a vertical macrobenthos position
!> ESMF gridded component
!> @file vertical_macrobenthos_component.F90
!!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2018 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "vertical_macrobenthos_component.F90"

#define RANGE1D lbnd(1):ubnd(1)
#define RANGE2D RANGE1D,lbnd(2):ubnd(2)

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module vertical_macrobenthos_component

  use esmf

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  use mossco_component, Finalize => MOSSCO_GridCompFinalize
#undef  ESMF_METHOD

  use mossco_field
  use mossco_state
  use mossco_config
  use mossco_strings
  use mossco_attribute
  use mossco_config

  implicit none

  public SetServices

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    integer              :: localrc

    rc=ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_READRESTART, phase=1, &
      userRoutine=ReadRestart, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
  subroutine InitializeP0(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, &
      currTime=currTime, importState=importState,  exportState=exportState, rc=localrc)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", &
      valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(ESMF_MAXSTR):: name, configFileName, message
    type(ESMF_Time)       :: currTime
    integer(ESMF_KIND_I4) :: localrc
    integer(ESMF_KIND_I4) :: waittime
    type(ESMF_Config)     :: config
    logical               :: configIsPresent, fileIsPresent

    type(ESMF_Grid)          :: grid
    type(ESMF_Mesh)          :: mesh
    integer(ESMF_KIND_I4)    :: lbnd(3)=1, ubnd(3)=1, rank=0
    type(ESMF_GeomType_Flag) :: geomType
    logical                  :: isPresent

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState,  exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !! Check whether there is a config in memory and load it
    call ESMF_GridCompGet(gridComp, configIsPresent=configIsPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (configIsPresent) then
      call ESMF_GridCompGet(gridComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    else
      config=ESMF_ConfigCreate(rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_GridCompSet(gridComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    !! Check whether there is a local config file and load it
    configfilename=trim(name)//'.cfg'
    inquire(file=trim(configfilename), exist=fileIsPresent)

    if (fileIsPresent) then

      write(message,'(A)')  trim(name)//' reads configuration from '//trim(configFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_ConfigGet(config, label='wait', value=waittime, &
        defaultValue=0, rc = localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
      ispresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (isPresent) then
      call MOSSCO_StateGetForeignGrid(importState, &
        attributeName='foreign_grid_field_name', geomType=geomType, grid=grid, &
        mesh=mesh, owner=trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    else
      rc = ESMF_RC_NOT_IMPL
      call MOSSCO_CompExit(gridComp, localrc)
      return
    endif

    if (geomType == ESMF_GEOMTYPE_GRID) then

      call ESMF_GridGet(grid, rank=rank, rc=localrc)
      if (rank /= 2) then
        rc = ESMF_RC_NOT_IMPL
        call MOSSCO_CompExit(gridComp, localrc)
        return
      endif

      call ESMF_GridCompSet(gridComp, grid=grid, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    else
      rc = ESMF_RC_NOT_IMPL
      call MOSSCO_CompExit(gridComp, localrc)
      return
    endif


    call ESMF_StateValidate(exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "ReadRestart"
  subroutine ReadRestart(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer(ESMF_KIND_I4) :: localrc

    rc=ESMF_SUCCESS

    !> It does not make sense to readrestart this component, thus, we
    !> 1. do not log calls to this function with CompEntry/CompExit
    !> 2. use vertical_macrobenthos variables to avoid -Wunused

    call ESMF_GridCompValidate(gridComp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockValidate(parentClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine ReadRestart

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)     :: gridComp
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: parentClock
    integer, intent(out)    :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime, stopTime
    type(ESMF_Clock)        :: clock
    integer(ESMF_KIND_I4)   :: localrc
    integer(ESMF_KIND_I4)   :: waittime

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(gridComp, name='wait_time', &
      value=waittime, defaultValue=0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

!> @todo do we need an #ifdef __GNUC__ here as this is a gnu extension?
    if (waittime > 0) call sleep(waittime)

    call ESMF_ClockGet(clock, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (stopTime>currTime) then
      call ESMF_ClockAdvance(clock, timeStep=stopTime-currTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Finally, log the successful completion of this function
    call MOSSCO_CompExit(gridComp, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

end module vertical_macrobenthos_component
