!> @brief Implementation of an ESMF component that delivers constant data fields
!
!> @import none
!> @export all variables that are located in a file read by this component
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2018 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen,  Helmholtz-Zentrum Geesthacht
!> @author Johannes Bieser, Helmholtz-Zentrum Geesthacht
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "cmaq_component.F90"

module cmaq_component

  use esmf
  use mossco_component
  use cmaq_driver

  implicit none

  private

  public SetServices

!   interface cmaqCmp_StateAddPtr
!     module procedure cmaqCmp_StateAddPtr2D
!     module procedure cmaqCmp_StateAddPtr3D
!   end interface

  ! variables offered from HAMSOM for use by other ESMF components
!   real(ESMF_KIND_R8), pointer :: CGRID(:,:,:,:)=>NULL()
!==================================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    integer              :: localrc

!----------------------------------------------------------------------------------

    rc=ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=2, &
      userRoutine=InitializeP2, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_READRESTART, phase=1, &
      userRoutine=ReadRestart, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine SetServices

!==================================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
  subroutine InitializeP0(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=10)           :: InitializePhaseMap(2)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

!----------------------------------------------------------------------------------
    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
!    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"
    InitializePhaseMap(2) = "IPDv00p2=2"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc))  &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP0

!==================================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=ESMF_MAXSTR) :: timestring, message, name, fileName
    character(len=ESMF_MAXSTR) :: startTimeString, stopTimeString
    type(ESMF_Time)            :: currTime, stopTime, startTime
    type(ESMF_TimeInterval)    :: timeInterval, timeStep
    integer(ESMF_KIND_I4)      :: petCount, localPet, localRc
    integer(ESMF_KIND_I8)      :: advanceCount
    type(ESMF_Clock)           :: clock

    logical                    :: isPresent
    type(ESMF_Grid)            :: grid2, grid3
    type(ESMF_Field)           :: field
    character(len=ESMF_MAXSTR) :: timeUnit, itemName, gridName

    integer(ESMF_KIND_I4)      :: itemCount, i, j
    integer(ESMF_KIND_I4)      :: Rank
    type(ESMF_Time)            :: refTime, time
    real(ESMF_KIND_R8)         :: seconds
    type(ESMF_Field), allocatable :: fieldList(:)
    integer(ESMF_KIND_I4), allocatable    :: ungriddedUbnd(:), ungriddedLbnd(:)
    integer(ESMF_KIND_I4), allocatable    :: Ubnd(:), Lbnd(:)

    integer              :: dayOfYear
    integer              :: year
    integer              :: hour
    integer, save        :: SDATE, STIME, EDATE, ETIME         !YYYYDDD 2010001 HHMMSS 100000
    character(len=10)          :: cmaq_grdID = 'AQ24'

!----------------------------------------------------------------------------------

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    !call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


    call ESMF_ClockGet(clock, startTime=startTime, stopTime=stopTime, &
      timeStep=timeStep, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(startTime,dayOfYear=dayOfYear,yy=year,h=hour,rc=localRc)
    SDATE = 1000*year + dayOfYear
    STIME = 10000*hour

    call ESMF_TimeGet(stopTime,dayOfYear=dayOfYear,yy=year,h=hour,rc=localRc)
    EDATE = 1000*year + dayOfYear
    ETIME = 10000*hour
    
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(stopTime,timeStringISOFrac=stopTimeString)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_TimeIntervalGet(timeStep,s_r8=seconds)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call init_cmaq(cmaq_grdID,SDATE,STIME,EDATE,ETIME)

    ! Ask cmaq for grid information, on both stagger locations
!     call cmaqCmp_init_grid(gridComp)

    ! Create fields on this grid, with correct stagger, and pointer to cmaq data
!     call cmaqmCmp_init_variables()

!     ! fill attributes for pointers to arrays maintained by cmaq
!     if (associated(AvrCumZ_2D)) then
!       call cmaqCmp_StateAddPtr("surface_elevatioan_following_time_averaged_lateral_transports_of_water",AvrCumZ_2D,exportState,"-",name)
!     end if
!     if (associated(AvrU_3D)) then
!       call cmaqCmp_StateAddPtr("time_averaged_zonal_transport_velocity_of_water",AvrU_3D,exportState,"m2 s-1",name)
!     end if
!     if (associated(AvrV_3D)) then
!       call cmaqCmp_StateAddPtr("time_averaged_meridional_transport_velocity_of_water",AvrV_3D,exportState,"m2 s-1",name)
!     end if
!     if (associated(AvrT_3D)) then
!       call cmaqCmp_StateAddPtr("time_averaged_temperature_in_water",AvrT_3D,exportState,"degC",name)
!     end if
!     if (associated(AvrS_3D)) then
!       call cmaqCmp_StateAddPtr("time_averaged_salinity_in_water",AvrS_3D,exportState,"psu",name)
!     end if
!     if (associated(AvrAv_3D)) then
!       call cmaqCmp_StateAddPtr("time_averaged_vertical_diffusion_coefficient_in_water",AvrAv_3D,exportState,"-",name)
!     end if
!     if (associated(AvrAh_3D)) then
!       call cmaqCmp_StateAddPtr("time_averaged_horizontal_diffusion_coefficient_in_water",AvrAh_3D,exportState,"-",name)
!     end if
!     if (associated(AvrSm_3D)) then
!       call cmaqCmp_StateAddPtr("time_averaged_schmidtnumber_in_water",AvrSm_3D,exportState,"-",name)
!     end if

    !MK: dont know if needed
    !call getmCmp_update_exportState()

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1

!==================================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP2"
  subroutine InitializeP2(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    rc=ESMF_SUCCESS

    !> Here comes your restart code, which in the simplest case copies
    !> values from all fields in importState to those in exportState

  end subroutine InitializeP2

!==================================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ReadRestart"
  subroutine ReadRestart(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    rc=ESMF_SUCCESS

    !> Here comes your restart code, which in the simplest case copies
    !> values from all fields in importState to those in exportState

  end subroutine ReadRestart

!==================================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)     :: gridComp
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: parentClock
    integer, intent(out)    :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime, stopTime, nextTime
    type(ESMF_Clock)        :: clock
    type(ESMF_TimeInterval) :: timeStep
    integer(ESMF_KIND_I4)   :: hours

    integer              :: localrc

!----------------------------------------------------------------------------------

    rc=ESMF_SUCCESS

    !call getmCmp_update_importState()

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
!    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_ClockGet(clock, stopTime=stopTime, timeStep=timeStep, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_ClockGetNextTime(parentClock, nextTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    timeStep=nextTime-currTime
    if (nextTime>currTime) then
      call ESMF_TimeIntervalGet(timeStep,h=hours)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call do_mainstep_cmaq(hours)

      call ESMF_ClockAdvance(clock, timeStep=timeStep, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    endif


    !call getmCmp_update_grid(gridComp)
    !call getmCmp_update_exportState()

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


  end subroutine Run

!==================================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer                 :: localrc
    integer(ESMF_KIND_I4)   :: petCount, localPet
    character(ESMF_MAXSTR)  :: name, message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock

!----------------------------------------------------------------------------------

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
!    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call finalize_cmaq()

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

! !==================================================================================
! #undef  ESMF_METHOD
! #define ESMF_METHOD "cmaqCmp_init_grid"
!   subroutine cmaqCmp_init_grid(gridComp)
!
!     implicit none
!
! ! !INPUT/OUTPUT PARAMETERS:
!     type(ESMF_GridComp)      :: gridComp
!
! ! !LOCAL VARIABLES
!     type(ESMF_CoordSys_Flag) :: coordSys
!     type(ESMF_StaggerLoc)    :: StaggerLoc
!     type(ESMF_Array)         :: xcArray2D,ycArray2D,xxArray2D,yxArray2D
!     type(ESMF_Array)         :: xcArray3D,ycArray3D,xxArray3D,yxArray3D
!     type(ESMF_Array)         :: array
!
!     integer,dimension(3)     :: coordDimCount
!     integer,dimension(3,3)   :: coordDimMap
!
! !----------------------------------------------------------------------------------
!
!
!    ! getm_component "case(2)"
!    coordSys = ESMF_COORDSYS_SPH_DEG    ! (default)
!    coordDimCount = (/ 1 , 1 , 3 /)     ! rectilinear horizontal coordinates
!    coordDimMap = reshape( (/1,2,1,0,0,2,0,0,3/) , (/3,3/) )
!
!    xcArray2D = ESMF_ArrayCreate(cmaqDistGrid2D,lonc1D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
!    call ESMF_AttributeSet(xcArray2D,'creator', trim(name), rc=localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!    ycArray2D = ESMF_ArrayCreate(cmaqDistGrid2D,latc1D, indexflag=ESMF_INDEX_DELOCAL, distgridToArrayMap=(/0,1/), rc=localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!    call ESMF_AttributeSet(ycArray2D,'creator', trim(name), rc=localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!    xcArray3D = ESMF_ArrayCreate(cmaqDistGrid3D,lonc1D, indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!    call ESMF_AttributeSet(xcArray2D,'creator', trim(name), rc=localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!    ycArray3D = ESMF_ArrayCreate(cmaqDistGrid3D,latc1D, indexflag=ESMF_INDEX_DELOCAL, distgridToArrayMap=(/0,1,0/), rc=localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!    call ESMF_AttributeSet(ycArray3D,'creator', trim(name), rc=localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!    xxArray2D = ESMF_ArrayCreate(cmaqDistGrid2D,lonx1D, indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!    call ESMF_AttributeSet(xxArray2D,'creator', trim(name), rc=localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!    yxArray2D = ESMF_ArrayCreate(cmaqDistGrid2D,latx1D, indexflag=ESMF_INDEX_DELOCAL, distgridToArrayMap=(/0,1/), rc=localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!    call ESMF_AttributeSet(yxArray2D,'creator', trim(name), rc=localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!    xxArray3D = ESMF_ArrayCreate(cmaqDistGrid3D,lonx1D, indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!    call ESMF_AttributeSet(xxArray3D,'creator', trim(name), rc=localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!    yxArray3D = ESMF_ArrayCreate(cmaqDistGrid3D,latx1D, indexflag=ESMF_INDEX_DELOCAL, distgridToArrayMap=(/0,1,0/), rc=localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!    call ESMF_AttributeSet(yxArray3D,'creator', trim(name), rc=localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!
!     cmaqGrid2D = ESMF_GridCreate(cmaqDistGrid2D,                   &
!                                 name="cmaqGrid2D_"//trim(name),      &
!                                 gridAlign=(/1,1/),                     &
!                                 coordSys=coordSys,                     &
!                                 coordDimCount=int(coordDimCount(1:2)), &
!                                 coordDimMap=int(coordDimMap(1:2,1:2)), &
!                                 rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_AttributeSet(cmaqGrid2D,'creator', trim(name), rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     cmaqGrid3D = ESMF_GridCreate(cmaqDistGrid3D,                   &
!                                 name="cmaqGrid3D_"//trim(name),      &
!                                 gridAlign=(/1,1,1/),                   &
!                                 coordSys=coordSys,                     &
!                                 coordDimCount=coordDimCount,           &
!                                 coordDimMap=coordDimMap,               &
!                                 rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_AttributeSet(cmaqGrid2D,'creator', trim(name), rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     StaggerLoc = ESMF_STAGGERLOC_CENTER ! (default)
!     !  2D grid
!     call ESMF_GridSetCoord(cmaqGrid2D,1,array=xcArray2D,staggerloc=StaggerLoc, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_GridSetCoord(cmaqGrid2D,2,array=ycArray2D,staggerloc=StaggerLoc, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     array = ESMF_ArrayCreate(cmaqDistGrid2D,maskC,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     call ESMF_GridSetItem(cmaqGrid2D,ESMF_GRIDITEM_MASK,array=array,staggerloc=StaggerLoc, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     !  3D grid
!     call ESMF_GridSetCoord(cmaqGrid3D,1,array=xcArray3D,staggerloc=StaggerLoc, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_GridSetCoord(cmaqGrid3D,2,array=ycArray3D,staggerloc=StaggerLoc, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     array = ESMF_ArrayCreate(cmaqDistGrid3D,zc,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     call ESMF_GridSetCoord(cmaqGrid3D,3,array=array,staggerloc=StaggerLoc, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     array = ESMF_ArrayCreate(cmaqDistGrid3D,maskC3D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     call ESMF_GridSetItem(cmaqGrid3D,ESMF_GRIDITEM_MASK,array=array,staggerloc=StaggerLoc, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     StaggerLoc = ESMF_STAGGERLOC_CORNER
!     !  2D grid
!     call ESMF_GridSetCoord(cmaqGrid2D,1,array=xxArray2D,staggerloc=StaggerLoc, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_GridSetCoord(cmaqGrid2D,2,array=yxArray2D,staggerloc=StaggerLoc, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     array = ESMF_ArrayCreate(cmaqDistGrid2D,maskX,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     call ESMF_GridSetItem(cmaqGrid2D,ESMF_GRIDITEM_MASK,array=array,staggerloc=StaggerLoc, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     !  3D grid
!     call ESMF_GridSetCoord(cmaqGrid3D,1,array=xxArray3D,staggerloc=StaggerLoc, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_GridSetCoord(cmaqGrid3D,2,array=yxArray3D,staggerloc=StaggerLoc, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     array = ESMF_ArrayCreate(cmaqDistGrid3D,zx,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     call ESMF_GridSetCoord(cmaqGrid3D,3,array=array,staggerloc=StaggerLoc, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     array = ESMF_ArrayCreate(cmaqDistGrid3D,maskX3D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     call ESMF_GridSetItem(cmaqGrid3D,ESMF_GRIDITEM_MASK,array=array,staggerloc=StaggerLoc, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     array = ESMF_ArrayCreate(cmaqDistGrid3D,zw,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     call ESMF_GridSetCoord(cmaqGrid3D,3,array=array,staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     call ESMF_GridCompSet(gridComp,grid=cmaqGrid3D, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     !call getmCmp_update_grid(gridComp)
!
!
!   end subroutine cmaqCmp_init_grid
!
! !==================================================================================
! #undef  ESMF_METHOD
! #define ESMF_METHOD "cmaqCmp_StateAddPtr2D"
!   subroutine cmaqCmp_StateAddPtr2D(name,p2d,state,units,componentName)
!
!     implicit none
!
! ! !INPUT/OUTPUT PARAMETERS:
!     character(len=*),intent(in)                            :: name,units,componentName
!     real(ESMF_KIND_R8),dimension(:,:),pointer,intent(in)   :: p2d
!     type(ESMF_State),intent(inout)                         :: state
!
! ! !LOCAL VARIABLES
!     type(ESMF_Field)      :: field
!     integer               :: rc
!     integer(ESMF_KIND_I4) :: localrc
!
! !----------------------------------------------------------------------------------
!
! !  in contrast to ESMF_ArrayCreate() no automatic determination of total[L|U]Width
!     field = ESMF_FieldCreate(cmaqGrid2D,farrayPtr=p2d,name=name,rc=localrc)
!
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_AttributeSet(field,'units',trim(units))
!
!     call ESMF_AttributeSet(field,'creator', trim(componentName), rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     call ESMF_StateAdd(state,(/field/),rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     return
!
!   end subroutine cmaqCmp_StateAddPtr2D
!
! !==================================================================================
!
! #undef  ESMF_METHOD
! #define ESMF_METHOD "cmaqCmp_StateAddPtr3D"
!   subroutine cmaqCmp_StateAddPtr3D(name,p3d,state,units,componentName)
!
!     implicit none
!
! ! !INPUT/OUTPUT PARAMETERS:
!     character(len=*),intent(in)                            :: name,units,componentName
!     real(ESMF_KIND_R8),dimension(:,:,:),pointer,intent(in) :: p3d
!     type(ESMF_State),intent(inout)                         :: state
!
! ! !LOCAL VARIABLES
!     type(ESMF_Field)      :: field
!     integer               :: rc
!     integer(ESMF_KIND_I4) :: localrc
!
! !----------------------------------------------------------------------------------
!
! !  in contrast to ESMF_ArrayCreate() no automatic determination of total[L|U]Width
!     field = ESMF_FieldCreate(cmaqGrid3D,farrayPtr=p3d,name=name,rc=localrc)
!
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_AttributeSet(field,'units',trim(units))
!
!     call ESMF_AttributeSet(field,'creator', trim(componentName), rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     call ESMF_StateAdd(state,(/field/),rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!
!     return
!
!   end subroutine cmaqCmp_StateAddPtr3D

!==================================================================================

  end module cmaq_component

!==================================================================================
