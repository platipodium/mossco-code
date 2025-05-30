!> @brief Implementation of a vertical macrobenthos position
!> ESMF gridded component
!> @file vertical_macrobenthos_component.F90
!!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2021-2022 Helmholtz-Zentrum Hereon
!> @copyright Copyright (C) 2018-2021 Helmholtz-Zentrum Geesthacht
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
#define ESMF_FILENAME "vertical_macrobenthos_component.F90"

#define RANGE1D lbnd(1):ubnd(1)
#define RANGE2D RANGE1D,lbnd(2):ubnd(2)

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module vertical_macrobenthos_component

  use esmf
  use vertical_macrobenthos_driver

  use mossco_field
  use mossco_state
  use mossco_config
  use mossco_strings
  use mossco_info
  use mossco_config
  use mossco_component, ReadRestart => MOSSCO_GridCompReadRestart
  use mossco_component, Finalize => MOSSCO_GridCompFinalize

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
    type(ESMF_Info)             :: info 

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, &
      currTime=currTime, importState=importState,  exportState=exportState, rc=localrc)

    call ESMF_InfoGetFromHost(gridComp, info=info, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_InfoSet(info, key="NUOPC/General/InititalizePhaseMap", &
      values=InitializePhaseMap, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(gridComp, rc=localrc)
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
    type(ESMF_Field)         :: field
    integer(ESMF_KIND_I4)    :: lbnd(3)=1, ubnd(3)=1, rank=0, i
    type(ESMF_GeomType_Flag) :: geomType
    logical                  :: isPresent
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_Info)          :: info 

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

    call ESMF_InfoGetFromHost(importState, info=info, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_InfoGet(info, key='foreign_grid_field_name', &
      ispresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (isPresent) then
      call MOSSCO_StateGetForeignGrid(importState, &
        key='foreign_grid_field_name', geomType=geomType, grid=grid, &
        mesh=mesh, owner=trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    else
      rc = ESMF_RC_NOT_IMPL
      call MOSSCO_CompExit(gridComp, rc=localrc)
      return
    endif

    if (geomType == ESMF_GEOMTYPE_GRID) then

      call ESMF_GridGet(grid, rank=rank, rc=localrc)
      if (rank /= 2) then
        rc = ESMF_RC_NOT_IMPL
        call MOSSCO_CompExit(gridComp, rc=localrc)
        return
      endif

      call ESMF_GridCompSet(gridComp, grid=grid, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    else
      rc = ESMF_RC_NOT_IMPL
      call MOSSCO_CompExit(gridComp, rc=localrc)
      return
    endif

    ! Advertise fields for import and export

    ! Import
    ! (1) speed_in_water ! Bulk absolute value of velocity in lowest layer
    ! (2) roughness_length_at_soil_surface ! z0

    allocate(itemNameList(2))
    itemNameList(1) = 'concentration_of_particulate_organic_carbon_at_soil_surface'
    itemNameList(2) = 'bulk_speed_at_soil_surface'

    do i=lbound(itemNameList,1), ubound(itemNameList,1)

      field = ESMF_FieldEmptyCreate(name=itemNameList(i), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)


      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_StateAddReplace(importState, (/field/), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(name)//' created for import '
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    enddo
    deallocate(itemNameList)

    ! Export
    allocate(itemNameList(2))
    itemNameList(1) = 'depth_of_endobenthic_macrozoobenthos_at_soil_surface'
    itemNameList(2) = 'depth_of_epiobenthic_macrozoobenthos_at_soil_surface'
    itemNameList(3) = 'depth_of_macrozoobenthos_at_soil_surface'
    itemNameList(4) = 'carbon_biomass_of_endobenthic_macrozoobenthos_at_soil_surface'
    itemNameList(5) = 'carbon_biomass_of_endobenthic_macrozoobenthos_at_soil_surface'
    itemNameList(6) = 'carbon_biomass_of_endobenthic_macrozoobenthos_at_soil_surface'

    do i=lbound(itemNameList,1), ubound(itemNameList,1)

      field = ESMF_FieldEmptyCreate(name=itemNameList(i), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_InfoGetFromHost(field, info=info, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_InfoSet(info, key='creator', value=trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_StateAddReplace(exportState, (/field/), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(name)//' created for export '
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    enddo
    deallocate(itemNameList)

    call MOSSCO_CompExit(gridComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP1

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
    type(ESMF_Info)         :: info 

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_InfoGetFromHost(gridComp, info=info, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_InfoGet(info, key='wait_time', &
      value=waittime, default=0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

!> @todo do we need an #ifdef __GNUC__ here as this is a gnu extension?
    if (waittime > 0) call sleep(waittime)

    call ESMF_ClockGet(clock, stopTime=stopTime, rc=rc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (stopTime>currTime) then
      call ESMF_ClockAdvance(clock, timeStep=stopTime-currTime, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    call ESMF_StateValidate(exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !! Finally, log the successful completion of this function
    call MOSSCO_CompExit(gridComp, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Run

end module vertical_macrobenthos_component
