!> @brief Implementation of a GETM ocean component
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014 Helmholtz-Zentrum Geesthacht
!> @author Knut Klingbeil, IOW
!> @author Carsten Lemmen, HZG

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

!> @todo, get rid of include file here
#define FOREIGN_GRID
#include "cppdefs.h"

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "getm_component.F90"

module getm_component

  use esmf
  use getm_driver
  use mossco_component

  implicit none
  private

  public SetServices

  private getmCmp_init_variables
  private getmCmp_init_grid,getmCmp_update_grid

  interface getmCmp_StateAddPtr
    module procedure getmCmp_StateAddPtr2D
    module procedure getmCmp_StateAddPtr3D
  end interface

! the following objects point to deep objects and do not need to be
! requested everytime again
! Note (KK): the save attribute can be deleted for F2008 standard
  type(ESMF_DistGrid),save :: getmDistGrid2D,getmDistGrid3D
  type(ESMF_Grid)    ,save :: getmGrid2D,getmGrid3D

! The following objects are treated differently, depending on whether
! the kinds of GETM's internal REALTYPE matches ESMF_KIND_R8
  logical                    :: noKindMatch
  integer(ESMF_KIND_I4),pointer :: maskC(:,:)=>NULL(),maskX(:,:)=>NULL()
  integer(ESMF_KIND_I4),dimension(:,:,:),allocatable :: maskC3D,maskX3D
  real(ESMF_KIND_R8),pointer :: xc1D(:)  =>NULL(),yc1D(:)  =>NULL()
  real(ESMF_KIND_R8),pointer :: xx1D(:)  =>NULL(),yx1D(:)  =>NULL()
  real(ESMF_KIND_R8),pointer :: xc2D(:,:)=>NULL(),yc2D(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: xx2D(:,:)=>NULL(),yx2D(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: lonc1D(:)  =>NULL(),latc1D(:)  =>NULL()
  real(ESMF_KIND_R8),pointer :: lonx1D(:)  =>NULL(),latx1D(:)  =>NULL()
  real(ESMF_KIND_R8),pointer :: lonc2D(:,:)=>NULL(),latc2D(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: lonx2D(:,:)=>NULL(),latx2D(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: zw(:,:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: zc(:,:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: zx(:,:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: depth(:,:)=>NULL(),hbot(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: U2D (:,:)=>NULL(),V2D (:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: Ubot(:,:)=>NULL(),Vbot(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: Tbot(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: T3D(:,:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: nybot(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: windU(:,:)=>NULL(),windV(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: waveH(:,:)=>NULL(),waveT(:,:)=>NULL(),waveK(:,:)=>NULL(),waveDir(:,:)=>NULL()

  type :: ptrarray3D
     real(ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr=>NULL()
  end type ptrarray3D
  type(ptrarray3D),dimension(:),allocatable :: transport_ws,transport_conc

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(gridcomp, rc)

    implicit none

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc
    
    integer(ESMF_KIND_I4) :: localrc
    
    rc=ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(gridcomp,ESMF_METHOD_INITIALIZE, &
                                    userRoutine=InitializeP0, &
                                    phase=0,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp,ESMF_METHOD_INITIALIZE, &
                                    userRoutine=InitializeP1, &
                                    phase=1,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp,ESMF_METHOD_INITIALIZE, &
                                    userRoutine=InitializeP2, &
                                    phase=2,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine SetServices

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: InitializeP0 -
!
! !INTERFACE:
#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
   subroutine InitializeP0(gridComp,importState,exportState,clock,rc)
!
! !DESCRIPTION:
!  Note: [i|e]state and clock are uninitialized if the toplevel
!        component did not provide corresponding arguments to
!        ESMF_GridCompInitialize(gridComp).
!  The toplevel component can inquire rc via optional keyword argument
!  userRc to ESMF_GridCompInitialize().
!
! !USES:
    use NUOPC
    implicit none
!
! !INPUT/OUTPUT PARAMETERS:
    type(ESMF_GridComp) :: gridComp
    type(ESMF_State)    :: importState,exportState ! may be uninitialized
    type(ESMF_Clock)    :: clock        ! may be uninitialized
!
! !OUTPUT PARAMETERS:
    integer,intent(out) :: rc
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES
    character(len=NUOPC_PhaseMapStringLength) :: InitializePhaseMap(2)
    integer(ESMF_KIND_I4) :: localrc
!
!EOP
!-----------------------------------------------------------------------
!BOC

	   rc=ESMF_SUCCESS
	   
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'InitializeP0() # ',Ncall
#endif

   call MOSSCO_GridCompEntryLog(gridComp)

!  Note (KK): NUOPC initialises all components in various phases. By
!             default NUOPC assumes IPDv00 and thus requires userRoutines
!             for init phases 1 and 2 (other phases are added/executed by
!             NUOPC). If the userCode provides different phases, this
!             needs to be communicated to NUOPC via InitializePhaseMap
!             in an init phase 0.
!
!                                             | IPDv00 | IPDv01 | IPDv02
!=============================================|========|========|========
! add InitializePhaseMap attribute            | 0 (*?) | 0 (*?) | 0 (*?)
!=============================================|========|========|========
! advertise import and export fields          | p1 (!) | p1     | p1
!=============================================|========|========|========
! realize import and export fields (allocate) | p2 (!) | p3     | p3
!=============================================|========|========|========
! check field' Connected status               | p3     | p4     | p4
! set internal clock to other than pClock (*) |        |        |
!=============================================|========|========|========
! initialize fields (*)                       | p4     | p5     | p5
!=============================================|========|========|========
!
! (!) has to be done by the user, (*) optional by the user

   InitializePhaseMap(1) = "IPDv00p1=1"
   InitializePhaseMap(2) = "IPDv00p2=2"

   !call NUOPC_CompAttributeAdd(gridComp)
    call ESMF_AttributeAdd(gridComp,convention="NUOPC",purpose="General", &
                          attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
                          
                                                  
    call ESMF_AttributeSet(gridComp,name="InitializePhaseMap",           &
                                   valueList=InitializePhaseMap,        &
                                   convention="NUOPC",purpose="General",rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   call MOSSCO_GridCompExitLog(gridComp)

#ifdef DEBUG
   write(debug,*) 'Leaving InitializeP0()'
   write(debug,*)
#endif
   return

   end subroutine InitializeP0
!EOC
!-----------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp,importState,exportState,clock,rc)

    use time, only : getm_time_start => start, getm_time_stop => stop
    use time, only : getm_time_timestep => timestep
    use initialise,  only: init_model,dryrun
    use integration, only: MinN,MaxN
    use meteo      ,only: met_method
    use waves      ,only: waveforcing_method,WAVES_FROMWIND,WAVES_FROMFILE,WAVES_FROMEXT
#ifdef GETM_PARALLEL
    use mpi
    use halo_mpi, only: comm_getm
#endif

    implicit none

    type(ESMF_GridComp) :: gridComp
    type(ESMF_State)    :: importState,exportState ! may be uninitialized
    type(ESMF_Clock)    :: clock        ! may be uninitialized
    integer,intent(out) :: rc

    type(ESMF_Clock)      :: myClock
    type(ESMF_Time)       :: startTime,stopTime
    logical               :: vmIsPresent,clockIsPresent
    type(ESMF_TimeInterval) :: timeInterval
    type(ESMF_VM)         :: vm
    integer               :: comm,length

    type(ESMF_Time)         :: getmRefTime,getmStartTime,getmStopTime
    integer                 :: getmRunTimeStepCount
    character(len=8)        :: datestr
    character(len=10)       :: timestr
    character(len=19)       :: TimeStrISOFrac,start_external,stop_external
    character(len=MPI_MAX_ERROR_STRING) :: mpierrmsg
    integer(ESMF_KIND_I4) :: localrc

	  rc=ESMF_SUCCESS

    call MOSSCO_GridCompEntryLog(gridComp)

    call ESMF_GridCompGet(gridComp,vmIsPresent=vmIsPresent,       &
                                   clockIsPresent=clockIsPresent, &
                                   rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.vmIsPresent) then
      call ESMF_LogWrite('no VM present',ESMF_LOGMSG_ERROR, &
                         line=__LINE__,file=__FILE__,method='getmCmp_init()')
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end if

#ifdef GETM_PARALLEL
    call ESMF_GridCompGet(gridComp, vm=vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_VMGet(vm,mpiCommunicator=comm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MPI_COMM_DUP(comm,comm_getm,rc)
    if (rc .ne. MPI_SUCCESS) then
!     need depends on specified mpi error handler (i.e. not MPI_ERRORS_ARE_FATAL)
      call MPI_ERROR_STRING(rc,mpierrmsg,length,rc)
      call ESMF_LogWrite(mpierrmsg(1:length),ESMF_LOGMSG_ERROR,ESMF_CONTEXT)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end if
#endif

    call date_and_time(datestr,timestr)
    if (clockIsPresent) then

      ! use startTime and stopTime from already initialised getmClock
      call ESMF_GridCompGet(gridComp, clock=myClock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      
      call ESMF_ClockGet(myClock,startTime=startTime,stopTime=stopTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_TimeGet(startTime,timeStringISOFrac=start_external, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_TimeGet(stopTime,timeStringISOFrac=stop_external, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call preinit_model(datestr,timestr)
      call init_time(MinN,MaxN,start_external=start_external, &
                     stop_external=stop_external)
      call postinit_model()

      ! use internal GETM time step
      call ESMF_TimeIntervalSet(timeInterval,s_r8=getm_time_timestep, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ClockSet(myClock,name='getmClock',timeStep=timeInterval, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    else

      ! set up clock based on internal GETM specifications

      call init_model(datestr,timestr)

      TimeStrISOFrac=getm_time_start(1:10)//"T"//getm_time_start(12:19)
      call TimeStringISOFrac2ESMFtime(TimeStrISOFrac,getmRefTime)
      call ESMF_TimeIntervalSet(timeInterval,s_r8=getm_time_timestep, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      getmStartTime = getmRefTime + (MinN-1)*timeInterval
      getmStopTime  = getmRefTime + MaxN*timeInterval
      getmRunTimeStepCount = MaxN - MinN + 1

      myClock = ESMF_ClockCreate(timeInterval,getmStartTime,            &
                                   runTimeStepCount=getmRunTimeStepCount, &
                                   refTime=getmRefTime,                   &
                                   name='getmClock', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_GridCompSet(gridComp,clock=myClock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call getmCmp_init_variables()
    call getmCmp_init_grid(gridComp)

    if (associated(depth)) then
      call getmCmp_StateAddPtr("water_depth_at_soil_surface",depth,exportState)
    end if
    if (associated(hbot)) then
      call getmCmp_StateAddPtr("layer_height_at_soil_surface",hbot,exportState)
    end if
    if (associated(U2D)) then
      call getmCmp_StateAddPtr("depth_averaged_x_velocity_in_water",U2D,exportState)
    end if
    if (associated(V2D)) then
      call getmCmp_StateAddPtr("depth_averaged_y_velocity_in_water",V2D,exportState)
    end if
    if (associated(Ubot)) then
      call getmCmp_StateAddPtr("x_velocity_at_soil_surface",Ubot,exportState)
    end if
    if (associated(Vbot)) then
      call getmCmp_StateAddPtr("y_velocity_at_soil_surface",Vbot,exportState)
    end if
    if (associated(Tbot)) then
      call getmCmp_StateAddPtr("temperature_at_soil_surface",Tbot,exportState)
    end if
    if (associated(T3D)) then
      call getmCmp_StateAddPtr("temperature_in_water",T3D,exportState)
    end if
    if (associated(nybot)) then
      call getmCmp_StateAddPtr("turbulent_kinematic_viscosity_at_soil_surface",nybot,exportState)
    end if

    select case (met_method)
      case(2)
        if (associated(windU)) then
          call getmCmp_StateAddPtr("wind_x_velocity_at_10m",windU,exportState)
        end if
        if (associated(windV)) then
          call getmCmp_StateAddPtr("wind_y_velocity_at_10m",windV,exportState)
        end if
      case(3)
        if (associated(windU)) then
          call getmCmp_StateAddPtr("wind_x_velocity_at_10m",windU,importState)
        end if
        if (associated(windV)) then
          call getmCmp_StateAddPtr("wind_y_velocity_at_10m",windV,importState)
        end if
    end select

    select case (waveforcing_method)
      case(WAVES_FROMWIND,WAVES_FROMFILE)
        if (associated(waveH)) then
          call getmCmp_StateAddPtr("wave_height",waveH,exportState)
        end if
        if (associated(waveT)) then
          call getmCmp_StateAddPtr("wave_period",waveT,exportState)
        end if
        if (associated(waveK)) then
          call getmCmp_StateAddPtr("wave_number",waveK,exportState)
        end if
        if (associated(waveDir)) then
          call getmCmp_StateAddPtr("wave_direction",waveDir,exportState)
        end if
      case(WAVES_FROMEXT)
        if (associated(waveH)) then
          call getmCmp_StateAddPtr("wave_height",waveH,importState)
        end if
        if (associated(waveT)) then
          call getmCmp_StateAddPtr("wave_period",waveT,importState)
        end if
        if (associated(waveK)) then
          call getmCmp_StateAddPtr("wave_number",waveK,importState)
        end if
        if (associated(waveDir)) then
          call getmCmp_StateAddPtr("wave_direction",waveDir,importState)
        end if
    end select

    call getmCmp_update_exportState()

    if (.not.dryrun) then
      STDERR LINE
      LEVEL1 'integrating....'
      STDERR LINE
    end if

    call MOSSCO_GridCompExitLog(gridComp)

  end subroutine InitializeP1

!-----------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP2"
  subroutine InitializeP2(gridComp,importState,exportState,clock,rc)

      use domain, only: imin,imax,jmin,jmax,kmax
      implicit none

      type(ESMF_GridComp) :: gridComp
      type(ESMF_State)    :: importState,exportState ! may be uninitialized
      type(ESMF_Clock)    :: clock        ! may be uninitialized
      integer,intent(out) :: rc

      type(ESMF_StateItem_Flag) ,dimension(:),allocatable :: itemTypeList
      type(ESMF_FieldBundle)    ,dimension(:),allocatable :: fieldBundleList
      type(ESMF_FieldBundle)                              :: fieldBundle
      type(ESMF_Field)          ,dimension(:),allocatable :: fieldList_ws,fieldList_conc
      type(ESMF_FieldStatus_Flag)                         :: status
      character(len=ESMF_MAXSTR),dimension(:),allocatable :: itemNameList
      character(len=ESMF_MAXSTR)                          :: itemName
      integer                   ,dimension(:),allocatable :: transportFieldCountList,namelenList
      integer                                             :: transportFieldCount,itemCount
      integer                                             :: i,ii,n
      character(len=*),parameter :: ws_suffix="_z_velocity_in_water"
      character(len=*),parameter :: conc_suffix="_in_water"
    integer(ESMF_KIND_I4) :: localrc

	  rc=ESMF_SUCCESS

      call MOSSCO_GridCompEntryLog(gridComp)

      call ESMF_StateGet(importState,itemCount=itemCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemCount .gt. 0) then

         allocate(itemTypeList           (itemCount))
         allocate(itemNameList           (itemCount))
         allocate(namelenList            (itemCount))
         allocate(fieldBundleList        (itemCount))
         allocate(transportFieldCountList(itemCount))
         transportFieldCountList = 0

         call ESMF_StateGet(importState,itemNameList=itemNameList, &
                                   itemTypeList=itemTypeList, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         do i=1,itemCount
!           identify items to be transported by suffix
            namelenList(i) = len_trim(itemNameList(i))
            if ( namelenList(i) .le. len_trim(ws_suffix) ) cycle
            if (itemNameList(i)(namelenList(i)-len_trim(ws_suffix)+1:namelenList(i)) .ne. trim(ws_suffix)) cycle
            if (itemTypeList(i) .eq. ESMF_STATEITEM_FIELD) then
               transportFieldCountList(i) = 1
            else if (itemTypeList(i) .eq. ESMF_STATEITEM_FIELDBUNDLE) then
               call ESMF_StateGet(importState,itemNameList(i),fieldBundleList(i), rc=localrc)
               if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 
               call ESMF_FieldBundleGet(fieldBundleList(i),fieldCount=transportFieldCountList(i), rc=localrc)
               if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 
            end if
         end do

         transportFieldCount = sum(transportFieldCountList)


         if (transportFieldCount .gt. 0) then

            allocate(fieldList_ws  (transportFieldCount))
            allocate(fieldList_conc(transportFieldCount))
            n = 1

            do i=1,itemCount
               if (transportFieldCountList(i) .eq. 0) cycle
               if (itemTypeList(i) .eq. ESMF_STATEITEM_FIELD) then
                  call ESMF_StateGet(importState,itemNameList(i),fieldList_ws(n), rc=localrc)
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 

                  itemName = itemNameList(i)(:namelenList(i)-len_trim(ws_suffix))//conc_suffix
                  call ESMF_StateGet(importState,itemName,fieldList_conc(n),rc=localrc)
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

                  call ESMF_LogWrite(' will transport field '//trim(itemName),ESMF_LOGMSG_INFO)
                  n = n + 1
               else if (itemTypeList(i) .eq. ESMF_STATEITEM_FIELDBUNDLE) then
                  itemName = itemNameList(i)(:namelenList(i)-len_trim(ws_suffix))//conc_suffix
                  call ESMF_StateGet(importState,itemName,fieldBundle, rc=localrc)
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 

                  do ii=1,transportFieldCountList(i)
                     call ESMF_FieldBundleGet(fieldBundleList(i),ii,fieldList_ws(n), rc=localrc)
                     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 

                     call ESMF_FieldBundleGet(fieldBundle,ii,fieldList_conc(n),rc=localrc)
                     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
                     n = n + 1
                  end do
                  call ESMF_LogWrite(' will transport fieldbundle '//trim(itemName),ESMF_LOGMSG_INFO)
               end if
            end do

            allocate(transport_ws  (transportFieldCount))
            allocate(transport_conc(transportFieldCount))


            do n=1,transportFieldCount

               call ESMF_FieldGet(fieldList_ws(n),status=status, rc=localrc)
               if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT) 

               if (status.eq.ESMF_FIELDSTATUS_EMPTY) then
!                 Coupler called ESMF_FieldEmptyCreate(name),
!                 because fabm_pelagic ships with its own grid (coupler
!                 checks whether temperature field in fabm_pelagic's
!                 importState is already completed).
                  allocate(transport_ws(n)%ptr(I3DFIELD))
                  call ESMF_FieldEmptyComplete(fieldList_ws(n),getmGrid3D,        &
                                               transport_ws(n)%ptr,               &
                                               ESMF_INDEX_DELOCAL,                &
                                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                                               totalLWidth=(/HALO,HALO,1/),       &
                                               totalUWidth=(/HALO,HALO,0/),rc=localrc)
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
               else if (status .eq. ESMF_FIELDSTATUS_COMPLETE) then
!                 Coupler linked completed field from fabm_pelagic,
!                 because GETM's grid was provided to fabm_pelagic.
!                 The field MUST include the HALO zones and k=0 !!!
                  call ESMF_FieldGet(fieldList_ws(n),farrayPtr=transport_ws(n)%ptr,rc=localrc)
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
               else
                  call ESMF_LogWrite('field neither empty nor complete',ESMF_LOGMSG_ERROR, &
                                     line=__LINE__,file=__FILE__,method='InitializeP2()')
                  call ESMF_Finalize(endflag=ESMF_END_ABORT)
               end if

               call ESMF_FieldGet(fieldList_conc(n),status=status)

               if (status.eq.ESMF_FIELDSTATUS_EMPTY .or. status.eq.ESMF_FIELDSTATUS_GRIDSET) then
                  allocate(transport_conc(n)%ptr(I3DFIELD))
                  call ESMF_FieldEmptyComplete(fieldList_conc(n),getmGrid3D,      &
                                               transport_conc(n)%ptr,             &
                                               ESMF_INDEX_DELOCAL,                &
                                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                                               totalLWidth=(/HALO,HALO,1/),       &
                                               totalUWidth=(/HALO,HALO,0/),rc=localrc)
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
               else if (status .eq. ESMF_FIELDSTATUS_COMPLETE) then
                  call ESMF_FieldGet(fieldList_conc(n),farrayPtr=transport_conc(n)%ptr,rc=localrc)
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
               end if

            end do

         end if

      end if

    call MOSSCO_GridCompExitLog(gridComp)
    rc = ESMF_SUCCESS

   end subroutine InitializeP2

!-----------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(gridComp,importState,exportState,clock,rc)

    use initialise ,only: runtype,dryrun
    use integration,only: MinN
    use m3d, only: M

    implicit none

    type(ESMF_GridComp) :: gridComp
    type(ESMF_State)    :: importState,exportState ! may be uninitialized
    type(ESMF_Clock)    :: clock        ! may be uninitialized
    integer,intent(out) :: rc

    type(ESMF_Clock)      :: myClock
    type(ESMF_Time)       :: currTime, stopTime
    type(ESMF_TimeInterval) :: timeInterval
    integer(ESMF_KIND_I8) :: advanceCount
    type(ESMF_Time)         :: nextTime
    integer                 :: n
    integer(ESMF_KIND_I4) :: localrc

	  rc=ESMF_SUCCESS

    call MOSSCO_GridCompEntryLog(gridComp)

    call getmCmp_update_importState()

    call ESMF_GridCompGet(gridComp, clock=myClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(myClock,currTime=currTime, advanceCount=advanceCount, &
      timeStep=timeInterval, stopTime=stopTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


    !  use clock to do determine time of calling routine
    call ESMF_ClockGetNextTime(clock,nextTime,rc=localrc)
    if (rc .ne. ESMF_SUCCESS) then
      call ESMF_LogWrite('will continue until own stopTime',ESMF_LOGMSG_WARNING, &
       line=__LINE__,file=__FILE__,method='Run()')
      call ESMF_ClockGet(myClock,stopTime=NextTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end if


    do while (currTime + 0.5d0*timeInterval <= nextTime)

      if (ESMF_ClockIsStopTime(myClock)) then
        call ESMF_LogWrite('already exceeded stopTime',ESMF_LOGMSG_ERROR, &
                            line=__LINE__,file=__FILE__,method='Run()')
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if

!     This is where the model specific computation goes.
      if (.not.dryrun) then
        n = int(advanceCount,kind=kind(MinN))+MinN
        call time_step(runtype,n)
      end if

!     Call transport routine every macro timestep
      if (mod(n,M).eq.0) call getmCmp_transport()

      call ESMF_ClockAdvance(myClock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_ClockGet(myClock,currtime=currTime,advanceCount=advanceCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do

    call getmCmp_update_grid(gridComp)
    call getmCmp_update_exportState()

    call MOSSCO_GridCompExitLog(gridComp)

  end subroutine Run

!-----------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(gridComp, importState, exportState, clock, rc)

    use initialise ,only: runtype,dryrun
    use integration,only: MaxN

    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Grid)       :: getmGrid
    type(ESMF_Clock)      :: myClock
    logical               :: ClockIsPresent,GridIsPresent
    integer(ESMF_KIND_I4) :: localrc

	  rc=ESMF_SUCCESS

    call MOSSCO_GridCompEntryLog(gridComp)

    call clean_up(dryrun,runtype,MaxN)

    call ESMF_GridCompGet(gridComp,clockIsPresent=ClockIsPresent, &
                                   gridIsPresent=GridIsPresent,   &
                                   rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (ClockIsPresent) then
      call ESMF_GridCompGet(gridComp,clock=myClock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_ClockDestroy(myClock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end if

    if (GridIsPresent) then
      call ESMF_GridCompGet(gridComp,grid=getmGrid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !call ESMF_GridGet(getmGrid,distgrid=getmDistGrid)
      !call ESMF_GridGetCoord(getmGrid,coordDim=...,staggerloc=...,array=array)
      !call ESMF_ArrayDestroy(array)
      call ESMF_DistGridDestroy(getmDistGrid2D, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_DistGridDestroy(getmDistGrid3D, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_GridDestroy(getmGrid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end if

    call MOSSCO_GridCompExitLog(gridComp)
    rc = ESMF_SUCCESS

  end subroutine Finalize

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: getmCmp_init_variables
!
! !INTERFACE:
#undef  ESMF_METHOD
#define ESMF_METHOD "getmCmp_init_variables"
   subroutine getmCmp_init_variables()
!
! !DESCRIPTION:
!
! !USES:
   use domain         ,only: imin,jmin,imax,jmax,kmax
   use domain         ,only: az,ax
   use domain         ,only: xcord,ycord,xx,yx,lonx,latx
   use domain         ,only: xxcord,yxcord,xc,yc,lonc,latc
   use domain         ,only: grid_type
   use initialise     ,only: runtype
   use variables_2d   ,only: D
#ifndef NO_3D
   use variables_3d   ,only: hn,num
#ifndef NO_BAROCLINIC
   use variables_3d   ,only: T
#endif
#endif
   use meteo          ,only: metforcing,met_method,calc_met,u10,v10
   use waves          ,only: waveforcing_method,NO_WAVES
   use variables_waves,only: waveH_=>waveH,waveT_=>waveT,waveK_=>waveK
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
   REALTYPE,dimension(:,:),pointer :: p2d
   REALTYPE :: getmreal
   integer  :: k,klen
    integer(ESMF_KIND_I4) :: localrc
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_init_variables() # ',Ncall
#endif

   noKindMatch = ( kind(getmreal) .ne. ESMF_KIND_R8 )

   if (runtype .eq. 1) then
      klen = 1
   else
      klen = kmax
   end if

   if (noKindMatch) then
      select case (grid_type)
         case(1)
            allocate(xc1D(   _IRANGE_HALO_)) ; xc1D = xcord
            allocate(yc1D(   _JRANGE_HALO_)) ; yc1D = ycord
            allocate(xx1D(-1+_IRANGE_HALO_))
            allocate(yx1D(-1+_JRANGE_HALO_))
         case(2)
            allocate(lonc1D(   _IRANGE_HALO_)) ; lonc1D = xcord
            allocate(latc1D(   _JRANGE_HALO_)) ; latc1D = ycord
            allocate(lonx1D(-1+_IRANGE_HALO_))
            allocate(latx1D(-1+_JRANGE_HALO_))
         case(3)
            allocate(xx2D(E2DXFIELD)) ; xx2D = xx
            allocate(yx2D(E2DXFIELD)) ; yx2D = yx
            allocate(xc2D(E2DFIELD )) ; xc2D = xc
            allocate(yc2D(E2DFIELD )) ; yc2D = yc
         case(4)
            allocate(lonx2D(E2DXFIELD)) ; lonx2D = lonx
            allocate(latx2D(E2DXFIELD)) ; latx2D = latx
            allocate(lonc2D(E2DFIELD )) ; lonc2D = lonc
            allocate(latc2D(E2DFIELD )) ; latc2D = latc
      end select
      allocate(depth(E2DFIELD))
      if (runtype .eq. 1) then
         hbot => depth
      else
#ifndef NO_3D
         allocate(hbot(E2DFIELD))
         allocate(nybot(I2DFIELD))
#ifndef NO_BAROCLINIC
         if (runtype .gt. 2) then
            allocate(Tbot(I2DFIELD))
#ifdef FOREIGN_GRID
            allocate(T3D(I3DFIELD))
#else
            allocate(T3D(imin:imax,jmin:jmax,1:kmax))
#endif
         end if
#endif
#endif
      end if
      if (metforcing .and. (met_method.eq.2 .or. met_method.eq.3)) then ! still required...
      if (calc_met) then
         allocate(windU(E2DFIELD))
         allocate(windV(E2DFIELD))
      end if
      end if
      if (waveforcing_method .ne. NO_WAVES) then
         allocate(waveH  (E2DFIELD))
         allocate(waveT  (E2DFIELD))
         allocate(waveK  (E2DFIELD))
      end if
   else
      select case (grid_type)
         case(1)
            xc1D => xcord
            yc1D => ycord
            xx1D => xxcord
            yx1D => yxcord
         case(2)
            lonc1D => xcord
            latc1D => ycord
            lonx1D => xxcord
            latx1D => yxcord
         case(3)
            xx2D => xx
            yx2D => yx
            xc2D => xc
            yc2D => yc
         case(4)
            lonx2D => lonx
            latx2D => latx
            lonc2D => lonc
            latc2D => latc
      end select
      depth => D
      if (runtype .eq. 1) then
         hbot => D
      else
#ifndef NO_3D
#if 0
         hbot(imin-HALO:,jmin-HALO:) => hn(:,:,1)
#else
         p2d => hn(:,:,1)
         hbot(imin-HALO:,jmin-HALO:) => p2d
#endif
#if 0
         nybot(imin-HALO:,jmin-HALO:) => num(:,:,1)
#else
         allocate(nybot(I2DFIELD))
#endif
#ifndef NO_BAROCLINIC
         if (runtype .gt. 2) then
#if 0
            Tbot(imin-HALO:,jmin-HALO:) => T(:,:,1)
#else
            p2d => T(:,:,1)
            Tbot(imin-HALO:,jmin-HALO:) => p2d
#endif
#ifdef FOREIGN_GRID
            T3D => T
#else
            T3D => T(imin:imax,jmin:jmax,1:kmax)
#endif
         end if
#endif
#endif
      end if
      if (metforcing .and. (met_method.eq.2 .or. met_method.eq.3)) then ! still required...
      if (calc_met) then
         windU => u10
         windV => v10
      end if
      end if
      if (waveforcing_method .ne. NO_WAVES) then
         waveH   => waveH_
#if 0
         waveT   => waveT_
         waveK   => waveK_
#else
         allocate(waveT(E2DFIELD))
         allocate(waveK(E2DFIELD))
#endif
      end if
   end if

   select case (grid_type)
      case(1)
         xx1D(imin-HALO:imax+HALO-1) = _HALF_ * ( xc1D(imin-HALO:imax+HALO-1) + xc1D(imin-HALO+1:imax+HALO) )
         yx1D(jmin-HALO:jmax+HALO-1) = _HALF_ * ( yc1D(jmin-HALO:jmax+HALO-1) + yc1D(jmin-HALO+1:jmax+HALO) )
      case(2)
         lonx1D(imin-HALO:imax+HALO-1) = _HALF_ * ( lonc1D(imin-HALO:imax+HALO-1) + lonc1D(imin-HALO+1:imax+HALO) )
         latx1D(:) = latx(imin,:)
   end select

   if (kind(klen) .eq. ESMF_KIND_I4) then
      maskC => az
      maskX => ax
   else
      allocate(maskC(E2DFIELD)) ; maskC = az
      allocate(maskX(E2DFIELD)) ; maskX = ax
   end if

   allocate(maskC3D(E2DFIELD,1:klen))
   allocate(maskX3D(E2DFIELD,0:klen))

   do k=1,klen
      maskC3D(:,:,k) = az
      maskX3D(:,:,k) = ax
   end do
   maskX3D(:,:,0) = 0

   allocate(zw(E2DFIELD ,0:klen))
   allocate(zc(E2DFIELD ,1:klen))
   allocate(zx(E2DXFIELD,0:klen))

   allocate(U2D(E2DFIELD))
   allocate(V2D(E2DFIELD))

   if (klen .eq. 1) then
      Ubot => U2D
      Vbot => V2D
   else
      allocate(Ubot(E2DFIELD))
      allocate(Vbot(E2DFIELD))
   end if

   if (waveforcing_method .ne. NO_WAVES) then
      allocate(waveDir(E2DFIELD))
   end if


#ifdef DEBUG
   write(debug,*) 'getmCmp_init_variables()'
   write(debug,*)
#endif
   return

   end subroutine getmCmp_init_variables
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: getmCmp_init_grid - Creates Grid
!
! !INTERFACE:
#undef  ESMF_METHOD
#define ESMF_METHOD "getmCmp_init_grid"
  subroutine getmCmp_init_grid(gridComp)
!
! !DESCRIPTION:
!
! !USES:
    use initialise, only: runtype
    use domain    , only: ioff,joff,imax,jmax,kmax
    use domain    , only: grid_type
    implicit none
!
! !INPUT/OUTPUT PARAMETERS:
    type(ESMF_GridComp) :: gridComp
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
    type(ESMF_VM)            :: getmVM
    type(ESMF_CoordSys_Flag) :: coordSys
    type(ESMF_StaggerLoc)    :: StaggerLoc
    type(ESMF_Array)         :: xcArray2D,ycArray2D,xxArray2D,yxArray2D
    type(ESMF_Array)         :: xcArray3D,ycArray3D,xxArray3D,yxArray3D
    type(ESMF_Array)         :: array
!  Note (KK): ESMF_ARRAY's are deep classes, that persist after return.
!             (even without save attribute).
    integer(ESMF_KIND_I4),dimension(:),allocatable,target :: alledges
    integer(ESMF_KIND_I4),dimension(4),target             :: myedges
    integer                  :: getmPetCount
    integer                  :: pet,i0,j0,ilen,jlen,klen
    integer,dimension(3)     :: coordDimCount
    integer,dimension(3,3)   :: coordDimMap
    integer,dimension(:,:,:),allocatable                  :: deBlockList
    integer(ESMF_KIND_I4)    :: localrc, rc
    character(ESMF_MAXSTR)   :: name, message
!
!EOP
!-----------------------------------------------------------------------
!BOC

#ifdef DEBUG
    integer, save :: Ncall = 0
    Ncall = Ncall+1
    write(debug,*) 'getmCmp_init_grid() # ',Ncall
#endif

    call ESMF_GridCompGet(gridComp,vm=getmVM, name=name, petCount=getmPetCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    myedges = (/ ioff , joff , imax , jmax /)
    allocate(alledges(4*getmPetCount))
!  syncflag=ESMF_SYNC_BLOCKING (default)
    call ESMF_VMAllGather(getmVM,myedges,alledges,4, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   if (runtype .eq. 1) then
      klen = 1
   else
      klen = kmax
   end if

   allocate(deBlockList(3,2,getmPetCount))
   do pet = 0,getmPetCount-1
      i0   = 1 + alledges(1+4*pet)
      j0   = 1 + alledges(2+4*pet)
      ilen =     alledges(3+4*pet)
      jlen =     alledges(4+4*pet)
      deBlockList(:,1,1+pet) = (/ i0        , j0        , 1        /)
      deBlockList(:,2,1+pet) = (/ i0+ilen-1 , j0+jlen-1 , 1+klen-1 /)
   end do

!  indexflag=ESMF_INDEX_DELOCAL (default) starting at 1
!  (for ESMF_INDEX_USER [grid|stagger]MemLBound can be set)
#if 1
!  Single-tile DistGrid (1 subdomain = 1 DE)
!  internal call to ESMF_DistGridCreateDB()
   getmDistGrid2D = ESMF_DistGridCreate(minval(deBlockList(1:2,1,:),2), &
                                        maxval(deBlockList(1:2,2,:),2), &
                                        int(deBlockList(1:2,:,:)), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(getmDistGrid2D,'creator', trim(name), rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   getmDistGrid3D = ESMF_DistGridCreate(minval(deBlockList(:,1,:),2), &
                                        maxval(deBlockList(:,2,:),2), &
                                        deBlockList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(getmDistGrid3D,'creator', trim(name), rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#else
!  Multi-tile DistGrid (1 subdomain = 1 tile = 1 DE) by specification of
!  [min|max]IndexPTile.
!  Note (KK): int() intrinsic routines are needed, because ESMF does not
!             accept subarrays as arguments
!  internal call to ESMF_DistGridCreateRDT()
   getmDistGrid2D = ESMF_DistGridCreate(int(deBlockList(1:2,1,:)), &
                                        int(deBlockList(1:2,2,:)), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(getmDistGrid2D,'creator', trim(name), rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   getmDistGrid3D = ESMF_DistGridCreate(int(deBlockList(:,1,:)), &
                                        int(deBlockList(:,2,:)))
   call ESMF_AttributeSet(getmDistGrid3D,'creator', trim(name), rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#endif

   select case (grid_type)
      case(1)
         coordSys = ESMF_COORDSYS_CART
!        coordDimMap: for each grid dimension i map array dimension j to grid dimension coordDimMap(i,j)
!                     i=1,dimCount ; j=1,coordDimCount(i)
         coordDimCount = (/ 1 , 1 , 3 /)     ! rectilinear horizontal coordinates
         coordDimMap = reshape( (/1,2,1,0,0,2,0,0,3/) , (/3,3/) )
!        1D xcord is replicated automatically along 2nd DistGrid dimension
!        1D ycord is replicated along 1st DistGrid dimension as specified
!        by distgridToArrayMap.
!        total[L|U]Width are automatically determined from shape of [x|y]cord
!        datacopyflag = ESMF_DATACOPY_REFERENCE (default)
!        internal call to ESMF_ArrayCreateAssmdShape<rank><type><kind>()
!        (because of required indexflag)
!        Note (KK): These ArrayCreate()'s only work for 1DE per PET!!!
!                   Automatically determined coordDimMap for rectilinear
!                   coordinates is incorrect!
         xcArray2D = ESMF_ArrayCreate(getmDistGrid2D,xc1D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray2D = ESMF_ArrayCreate(getmDistGrid2D,yc1D,indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xcArray3D = ESMF_ArrayCreate(getmDistGrid3D,xc1D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray3D = ESMF_ArrayCreate(getmDistGrid3D,yc1D,indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1,0/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xxArray2D = ESMF_ArrayCreate(getmDistGrid2D,xx1D,          &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray2D = ESMF_ArrayCreate(getmDistGrid2D,yx1D,          &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1/),   &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xxArray3D = ESMF_ArrayCreate(getmDistGrid3D,xx1D,          &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray3D = ESMF_ArrayCreate(getmDistGrid3D,yx1D,          &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1,0/), &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      case(2)
         coordSys = ESMF_COORDSYS_SPH_DEG    ! (default)
         coordDimCount = (/ 1 , 1 , 3 /)     ! rectilinear horizontal coordinates
         coordDimMap = reshape( (/1,2,1,0,0,2,0,0,3/) , (/3,3/) )
         xcArray2D = ESMF_ArrayCreate(getmDistGrid2D,lonc1D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         call ESMF_AttributeSet(xcArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray2D = ESMF_ArrayCreate(getmDistGrid2D,latc1D,indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xcArray3D = ESMF_ArrayCreate(getmDistGrid3D,lonc1D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray3D = ESMF_ArrayCreate(getmDistGrid3D,latc1D,indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1,0/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xxArray2D = ESMF_ArrayCreate(getmDistGrid2D,lonx1D,        &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray2D = ESMF_ArrayCreate(getmDistGrid2D,latx1D,        &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1/),   &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xxArray3D = ESMF_ArrayCreate(getmDistGrid3D,lonx1D,        &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray3D = ESMF_ArrayCreate(getmDistGrid3D,latx1D,        &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1,0/), &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      case(3)
         coordSys = ESMF_COORDSYS_CART
         coordDimCount = (/ 2 , 2 , 3 /)
         coordDimMap = reshape( (/1,1,1,2,2,2,0,0,3/) , (/3,3/) ) ! (default)
!        Note (KK): automatically determined total[L|U]Width are not consistent
!                   with gridAlign specified later
         xxArray2D = ESMF_ArrayCreate(getmDistGrid2D,xx2D,           &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray2D = ESMF_ArrayCreate(getmDistGrid2D,yx2D,           &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xxArray3D = ESMF_ArrayCreate(getmDistGrid3D,xx2D,           &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray3D = ESMF_ArrayCreate(getmDistGrid3D,yx2D,           &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xcArray2D = ESMF_ArrayCreate(getmDistGrid2D,xc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         
         ycArray2D = ESMF_ArrayCreate(getmDistGrid2D,yc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xcArray3D = ESMF_ArrayCreate(getmDistGrid3D,xc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray3D = ESMF_ArrayCreate(getmDistGrid3D,yc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      case(4)
         coordSys = ESMF_COORDSYS_SPH_DEG                         ! (default)
         coordDimCount = (/ 2 , 2 , 3 /)
         coordDimMap = reshape( (/1,1,1,2,2,2,0,0,3/) , (/3,3/) ) ! (default)
         xxArray2D = ESMF_ArrayCreate(getmDistGrid2D,lonx2D,         &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray2D = ESMF_ArrayCreate(getmDistGrid2D,latx2D,         &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xxArray3D = ESMF_ArrayCreate(getmDistGrid3D,lonx2D,         &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray3D = ESMF_ArrayCreate(getmDistGrid3D,latx2D,         &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xcArray2D = ESMF_ArrayCreate(getmDistGrid2D,lonc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray2D = ESMF_ArrayCreate(getmDistGrid2D,latc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xcArray3D = ESMF_ArrayCreate(getmDistGrid3D,lonc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray3D = ESMF_ArrayCreate(getmDistGrid3D,latc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   end select

!  Note (KK): gridAlign specifies which corner point in a grid cell
!             shares the center indices [ default=(/-1,...,-1/) ].
!             gridEdge[L|U]Width only affect DE's at the edge of tiles
!             (thus it matters whether a single- or multi-tile DistGrid
!              was created). If gridEdgeWidth's are not set, they are set
!             automatically based on gridAlign.
!  internal call to ESMF_GridCreateFrmDistGrid()
   getmGrid2D = ESMF_GridCreate(getmDistGrid2D,name="getmGrid2D",      &
                                gridAlign=(/1,1/),                     &
                                coordSys=coordSys,                     &
                                coordDimCount=int(coordDimCount(1:2)), &
                                coordDimMap=int(coordDimMap(1:2,1:2)), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(getmGrid2D,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   getmGrid3D = ESMF_GridCreate(getmDistGrid3D,name="getmGrid3D", &
                                gridAlign=(/1,1,1/),              &
                                coordSys=coordSys,                &
                                coordDimCount=coordDimCount,      &
                                coordDimMap=coordDimMap, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(getmGrid2D,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


   StaggerLoc = ESMF_STAGGERLOC_CENTER ! (default)
!  2D grid
   call ESMF_GridSetCoord(getmGrid2D,1,array=xcArray2D,staggerloc=StaggerLoc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call ESMF_GridSetCoord(getmGrid2D,2,array=ycArray2D,staggerloc=StaggerLoc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   array = ESMF_ArrayCreate(getmDistGrid2D,maskC,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   call ESMF_GridSetItem(getmGrid2D,ESMF_GRIDITEM_MASK,array=array,staggerloc=StaggerLoc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!  3D grid
   call ESMF_GridSetCoord(getmGrid3D,1,array=xcArray3D,staggerloc=StaggerLoc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call ESMF_GridSetCoord(getmGrid3D,2,array=ycArray3D,staggerloc=StaggerLoc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   array = ESMF_ArrayCreate(getmDistGrid3D,zc,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   call ESMF_GridSetCoord(getmGrid3D,3,array=array,staggerloc=StaggerLoc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   !array = ESMF_ArrayCreate(getmDistGrid3D,maskC,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
   array = ESMF_ArrayCreate(getmDistGrid3D,maskC3D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   call ESMF_GridSetItem(getmGrid3D,ESMF_GRIDITEM_MASK,array=array,staggerloc=StaggerLoc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!  KK-TODO: add attribute with un-/mask value?

   StaggerLoc = ESMF_STAGGERLOC_CORNER
!  2D grid
   call ESMF_GridSetCoord(getmGrid2D,1,array=xxArray2D,staggerloc=StaggerLoc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call ESMF_GridSetCoord(getmGrid2D,2,array=yxArray2D,staggerloc=StaggerLoc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   array = ESMF_ArrayCreate(getmDistGrid2D,maskX,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   call ESMF_GridSetItem(getmGrid2D,ESMF_GRIDITEM_MASK,array=array,staggerloc=StaggerLoc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!  3D grid
   call ESMF_GridSetCoord(getmGrid3D,1,array=xxArray3D,staggerloc=StaggerLoc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call ESMF_GridSetCoord(getmGrid3D,2,array=yxArray3D,staggerloc=StaggerLoc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   array = ESMF_ArrayCreate(getmDistGrid3D,zx,indexflag=ESMF_INDEX_DELOCAL,              &
                            totalLWidth=(/HALO+1,HALO+1,1/),totalUWidth=(/HALO,HALO,0/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   call ESMF_GridSetCoord(getmGrid3D,3,array=array,staggerloc=StaggerLoc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   !array = ESMF_ArrayCreate(getmDistGrid3D,maskX,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
   array = ESMF_ArrayCreate(getmDistGrid3D,maskX3D,indexflag=ESMF_INDEX_DELOCAL,     &
                            totalLWidth=(/HALO,HALO,1/),totalUWidth=(/HALO,HALO,0/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   call ESMF_GridSetItem(getmGrid3D,ESMF_GRIDITEM_MASK,array=array,staggerloc=StaggerLoc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!  KK-TODO: add attribute with un-/mask value?

   array = ESMF_ArrayCreate(getmDistGrid3D,zw,indexflag=ESMF_INDEX_DELOCAL,          &
                            totalLWidth=(/HALO,HALO,1/),totalUWidth=(/HALO,HALO,0/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   call ESMF_GridSetCoord(getmGrid3D,3,array=array,staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   call ESMF_GridCompSet(gridComp,grid=getmGrid3D, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call getmCmp_update_grid(gridComp)

   deallocate(alledges)
   deallocate(deBlockList)


#ifdef DEBUG
   write(debug,*) 'getmCmp_init_grid()'
   write(debug,*)
#endif
   return

   end subroutine getmCmp_init_grid
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: getmCmp_update_grid -
!
! !INTERFACE:
#undef  ESMF_METHOD
#define ESMF_METHOD "getmCmp_update_grid"
   subroutine getmCmp_update_grid(gridComp)
!
! !DESCRIPTION:
!
! !USES:
   use initialise  , only: runtype
   use domain      , only: imin,imax,jmin,jmax,kmax,az,au,av,H
   use variables_2d, only: z
   use variables_3d, only: ssen,hn
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
   type(ESMF_GridComp) :: gridComp
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
   REALTYPE,dimension(E2DFIELD) :: zwu
   integer :: i,j,k,klen
    integer(ESMF_KIND_I4) :: localrc
!
!EOP
!-----------------------------------------------------------------------
!BOC
	  
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_update_grid() # ',Ncall
#endif

   zw(:,:,0) = -H
   if (runtype .eq. 1) then
      klen = 1
      zw(:,:,1) = z
   else
      klen = kmax
      do k=1,kmax-1
         zw(:,:,k) = zw(:,:,k-1) + hn(:,:,k)
         zc(:,:,k) = _HALF_ * ( zw(:,:,k-1) + zw(:,:,k) )
      end do
      zw(:,:,kmax) = ssen
   end if
   zc(:,:,klen) = _HALF_ * ( zw(:,:,klen-1) + zw(:,:,klen) )

   do k=0,klen
      do j=jmin-HALO,jmax+HALO
         do i=imin-HALO,imax+HALO-1
            if (au(i,j) .ne. 0) then
               zwu(i,j) = _HALF_ * ( zw(i,j,k) + zw(i+1,j,k) )
            else
               if (az(i,j) .ne. 0) then
                  zwu(i,j) = zw(i,j,k)
               else if (az(i+1,j) .ne. 0) then
                  zwu(i,j) = zw(i+1,j,k)
               end if
            end if
         end do
      end do
      do j=jmin-HALO,jmax+HALO-1
         do i=imin-HALO,imax+HALO-1
            if (av(i,j).ne.0 .or. av(i+1,j).ne.0) then
               zx(i,j,k) = _HALF_ * ( zwu(i,j) + zwu(i,j+1) )
            else
               if (az(i,j).ne.0 .or. az(i+1,j).ne.0) then
                  zx(i,j,k) = zwu(i,j)
               else if (az(i,j+1).ne.0 .or. az(i+1,j+1).ne.0) then
                  zx(i,j,k) = zwu(i,j+1)
               end if
            end if
         end do
      end do
   end do

#ifdef DEBUG
   write(debug,*) 'getmCmp_update_grid()'
   write(debug,*)
#endif
   return

   end subroutine getmCmp_update_grid
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: getmCmp_update_exportState -
!
! !INTERFACE:
#undef  ESMF_METHOD
#define ESMF_METHOD "getmCmp_update_exportState"
   subroutine getmCmp_update_exportState()
!
! !DESCRIPTION:
!
! !USES:
   use domain         ,only: imin,imax,jmin,jmax,kmax
   use domain         ,only: az
   use domain         ,only: grid_type,xc,xu,xv,yc,yu,yv
#if defined(CURVILINEAR) || defined(SPHERICAL)
   use domain         ,only: dxv,dyu,arcd1
#else
   use domain         ,only: dx,dy,ard1
#endif
   use initialise     ,only: runtype
   use variables_2d   ,only: zo,z,D,Dvel,U,DU,V,DV
#ifndef NO_3D
   use variables_3d   ,only: dt,ho,hn,hvel,uu,hun,vv,hvn,ww,num
#ifndef NO_BAROCLINIC
   use variables_3d   ,only: T
#endif
#endif
   use m2d            ,only: dtm
   use meteo          ,only: metforcing,met_method,calc_met,u10,v10
   use waves          ,only: waveforcing_method,WAVES_FROMWIND,WAVES_FROMFILE
   use variables_waves,only: waveH_=>waveH,waveT_=>waveT,waveK_=>waveK
   use variables_waves,only: coswavedir,sinwavedir
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
   REALTYPE,dimension(E2DFIELD)         :: wrk
   REALTYPE,dimension(E2DFIELD),target  :: t_vel
   REALTYPE,dimension(:,:),pointer      :: p_vel
   integer                              :: klen
   REALTYPE,parameter                   :: vel_missing=-9999.0
    integer(ESMF_KIND_I4) :: localrc
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_update_exportState() # ',Ncall
#endif

   if (noKindMatch) then
      depth = D
#ifndef NO_3D
      if (runtype .gt. 1) then
         hbot = hn(:,:,1)
         nybot = num(:,:,1)
#ifndef NO_BAROCLINIC
         if (runtype .gt. 2) then
            Tbot = T(:,:,1)
#ifdef FOREIGN_GRID
            T3D = T
#else
            T3D = T(imin:imax,jmin:jmax,1:kmax)
#endif
         end if
#endif
      end if
#endif
      if (metforcing) then ! still required...
      if (calc_met .and. met_method.eq.2) then
         windU = u10
         windV = v10
      end if
      end if
      if (waveforcing_method.eq.WAVES_FROMWIND .or. waveforcing_method.eq.WAVES_FROMFILE) then
         waveH   = waveH_
         waveT   = waveT_
         waveK   = waveK_
      end if
#if 1
   else
#ifndef NO_3D
      if (runtype .gt. 1) then
         nybot = num(:,:,1)
      end if
#endif
      if (waveforcing_method.eq.WAVES_FROMWIND .or. waveforcing_method.eq.WAVES_FROMFILE) then
         waveT   = waveT_
         waveK   = waveK_
      end if
#endif
   end if


   wrk = _ZERO_

   if (noKindMatch) then
      p_vel => t_vel
   else
      p_vel => U2D
   end if
   call to_u(imin,jmin,imax,jmax,az,                                 &
             dtm,grid_type,                                          &
#if defined(CURVILINEAR) || defined(SPHERICAL)
             dxv,dyu,arcd1,                                          &
#else
             dx,dy,ard1,                                             &
#endif
             xc,xu,xv,z,zo,Dvel,U,DU,V,DV,wrk,wrk,vel_missing,p_vel)
   if (noKindMatch) then
      U2D = t_vel
   end if

   if (noKindMatch) then
      p_vel => t_vel
   else
      p_vel => V2D
   end if
   call to_v(imin,jmin,imax,jmax,az,                                 &
             dtm,grid_type,                                          &
#if defined(CURVILINEAR) || defined(SPHERICAL)
             dxv,dyu,arcd1,                                          &
#else
             dx,dy,ard1,                                             &
#endif
             yc,yu,yv,z,zo,Dvel,U,DU,V,DV,wrk,wrk,vel_missing,p_vel)
   if (noKindMatch) then
      V2D = t_vel
   end if

#ifndef NO_3D
   if (runtype .eq. 1) then
      klen = 1
   else
      klen = kmax
   end if

   if (klen .gt. 1) then
      if (noKindMatch) then
         p_vel => t_vel
      else
         p_vel => Ubot
      end if
      call to_u(imin,jmin,imax,jmax,az,                            &
                dt,grid_type,                                      &
#if defined(CURVILINEAR) || defined(SPHERICAL)
                dxv,dyu,arcd1,                                     &
#else
                dx,dy,ard1,                                        &
#endif
                xc,xu,xv,hn(:,:,1),ho(:,:,1),hvel(:,:,1),          &
                uu(:,:,1),hun(:,:,1),vv(:,:,1),hvn(:,:,1),         &
                ww(:,:,0),ww(:,:,1),vel_missing,p_vel)
      if (noKindMatch) then
         Ubot = t_vel
      end if

      if (noKindMatch) then
         p_vel => t_vel
      else
         p_vel => Vbot
      end if
      call to_v(imin,jmin,imax,jmax,az,                            &
                dt,grid_type,                                      &
#if defined(CURVILINEAR) || defined(SPHERICAL)
                dxv,dyu,arcd1,                                     &
#else
                dx,dy,ard1,                                        &
#endif
                yc,yu,yv,hn(:,:,1),ho(:,:,1),hvel(:,:,1),          &
                uu(:,:,1),hun(:,:,1),vv(:,:,1),hvn(:,:,1),         &
                ww(:,:,0),ww(:,:,1),vel_missing,p_vel)
      if (noKindMatch) then
         Vbot = t_vel
      end if
   end if
#endif

   if (waveforcing_method.eq.WAVES_FROMWIND .or. waveforcing_method.eq.WAVES_FROMFILE) then
      waveDir = atan2(sinwavedir,coswavedir)
   end if


#ifdef DEBUG
   write(debug,*) 'getmCmp_update_exportState()'
   write(debug,*)
#endif
   return

   end subroutine getmCmp_update_exportState
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: getmCmp_update_importState -
!
! !INTERFACE:
#undef  ESMF_METHOD
#define ESMF_METHOD "getmCmp_update_importState"
   subroutine getmCmp_update_importState()
!
! !DESCRIPTION:
!
! !USES:
   use meteo          ,only: metforcing,met_method,calc_met,u10,v10
   use waves          ,only: waveforcing_method,WAVES_FROMEXT
   use variables_waves,only: waveH_=>waveH,waveT_=>waveT,waveK_=>waveK
   use variables_waves,only: coswavedir,sinwavedir
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
    integer(ESMF_KIND_I4) :: localrc
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_update_importState() # ',Ncall
#endif

   if (noKindMatch) then
      if (metforcing) then ! still required...
      if (calc_met .and. met_method.eq.3) then
         u10 = windU
         v10 = windV
      end if
      end if
      if (waveforcing_method .eq. WAVES_FROMEXT) then
         waveH_   = waveH
         waveT_   = waveT
         waveK_   = waveK
      end if
#if 1
   else
      if (waveforcing_method .eq. WAVES_FROMEXT) then
         waveT_   = waveT
         waveK_   = waveK
      end if
#endif
   end if

   if (waveforcing_method .eq. WAVES_FROMEXT) then
      coswavedir = cos(waveDir)
      sinwavedir = sin(waveDir)
   end if

#ifdef DEBUG
   write(debug,*) 'getmCmp_update_importState()'
   write(debug,*)
#endif
   return

   end subroutine getmCmp_update_importState
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: getmCmp_transport() - transport of additional fields
!
! !INTERFACE:
#undef  ESMF_METHOD
#define ESMF_METHOD "getmCmp_transport"
   subroutine getmCmp_transport()
!
! !DESCRIPTION:
!
! !USES:
   use domain, only: imin,imax,jmin,jmax,kmax
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !INPUT/OUPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
   REALTYPE,dimension(I3DFIELD),target  :: t_conc,t_ws
   REALTYPE,dimension(:,:,:)   ,pointer :: p_conc,p_ws
   integer                              :: n
    integer(ESMF_KIND_I4) :: localrc
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_transport() # ',Ncall
#endif

   if ( .not. allocated(transport_conc) ) return

   do n=1,size(transport_conc)

      if (noKindMatch) then
         t_conc = transport_conc(n)%ptr
         t_ws   = transport_ws  (n)%ptr
         p_conc => t_conc
         p_ws   => t_ws
      else
         p_conc => transport_conc(n)%ptr
         p_ws   => transport_ws  (n)%ptr
      end if

      call do_transport_3d(p_conc,p_ws)

      if (noKindMatch) then
         transport_conc(n)%ptr = t_conc
         transport_ws  (n)%ptr = t_ws
      end if

   end do

#ifdef DEBUG
   write(debug,*) 'Leaving getmCmp_transport()'
   write(debug,*)
#endif
   return

   end subroutine getmCmp_transport
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: getmCmp_StateAddPtr2D -
!
! !INTERFACE:
#undef  ESMF_METHOD
#define ESMF_METHOD "getmCmp_StateAddPtr2D"
  subroutine getmCmp_StateAddPtr2D(name,p2d,state)
!
! !DESCRIPTION:
!
! !USES:
   use domain, only: imin,imax,jmin,jmax
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   character(len=*),intent(in)                          :: name
   real(ESMF_KIND_R8),dimension(:,:),pointer,intent(in) :: p2d
!
! !INPUT/OUTPUT PARAMETERS:
   type(ESMF_State),intent(inout)                       :: state
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
   type(ESMF_Field) :: field
   integer          :: rc
    integer(ESMF_KIND_I4) :: localrc
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_StateAddPtr2D() # ',Ncall
#endif

!  in contrast to ESMF_ArrayCreate() no automatic determination of total[L|U]Width
#if 1
!  Note (KK): in former times ESMF_FieldCreateGridDataPtr<rank><type><kind>() failed
   field = ESMF_FieldCreate(getmGrid2D,farrayPtr=p2d,                    &
#else
!  internal call to ESMF_FieldCreateGridData<rank><type><kind>()
!  forced by indexflag argument.
   field = ESMF_FieldCreate(getmGrid2D,p2d,indexflag=ESMF_INDEX_DELOCAL, &
#endif
                            totalLWidth=int((/imin,jmin/)-lbound(p2d)),  &
                            totalUWidth=int(ubound(p2d)-(/imax,jmax/)),  &
                            name=name,rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


   call ESMF_StateAdd(state,(/field/),rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#ifdef DEBUG
   write(debug,*) 'Leaving getmCmp_StateAddPtr2D()'
   write(debug,*)
#endif
   return

   end subroutine getmCmp_StateAddPtr2D
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: getmCmp_StateAddPtr3D -
!
! !INTERFACE:
#undef  ESMF_METHOD
#define ESMF_METHOD "getmCmp_StateAddPtr3D"
  subroutine getmCmp_StateAddPtr3D(name,p3d,state)
!
! !DESCRIPTION:
!
! !USES:
   use initialise,only: runtype
   use domain    ,only: imax,jmax,kmax
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   character(len=*),intent(in)                            :: name
   real(ESMF_KIND_R8),dimension(:,:,:),pointer,intent(in) :: p3d
!
! !INPUT/OUTPUT PARAMETERS:
   type(ESMF_State),intent(inout)                       :: state
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
   type(ESMF_Field) :: field
   integer          :: klen,rc
    integer(ESMF_KIND_I4) :: localrc
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_StateAddPtr3D() # ',Ncall
#endif

   if (runtype .eq. 1) then
      klen = 1
   else
      klen = kmax
   end if

!  in contrast to ESMF_ArrayCreate() no automatic determination of total[L|U]Width
#if 1
!  Note (KK): in former times ESMF_FieldCreateGridDataPtr<rank><type><kind>() failed
   field = ESMF_FieldCreate(getmGrid3D,farrayPtr=p3d,                        &
#else
!  internal call to ESMF_FieldCreateGridData<rank><type><kind>()
!  forced by indexflag argument.
   field = ESMF_FieldCreate(getmGrid3D,p3d,indexflag=ESMF_INDEX_DELOCAL,     &
#endif
                            totalLWidth=int((/1,1,1/)-lbound(p3d)),          &
                            totalUWidth=int(ubound(p3d)-(/imax,jmax,klen/)), &
                            name=name,rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(field,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   call ESMF_StateAdd(state,(/field/),rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#ifdef DEBUG
   write(debug,*) 'Leaving getmCmp_StateAddPtr3D()'
   write(debug,*)
#endif
   return

   end subroutine getmCmp_StateAddPtr3D
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: TimeStringISOFrac2ESMFtime - converts timestring to ESMF_Time
!
! !INTERFACE:
#undef  ESMF_METHOD
#define ESMF_METHOD "TimeStringISOFrac2ESMFtime"
  subroutine TimeStringISOFrac2ESMFtime(TimeStrISOFrac,ESMFtime)
!
! !DESCRIPTION:
!  So far missing extension to ESMF_TimeSet().
!
! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   character(len=*),intent(in)  :: TimeStrISOFrac
!
! !OUTPUT PARAMETERS:
   type(ESMF_Time) ,intent(out) :: ESMFtime
!
! !REVISION HISTORY:
!  Original Author(s): Carsten Lemmen and Richard Hofmeister
!
! !LOCAL VARIABLES
   integer                       :: yy,mm,dd,h,m,s
    integer(ESMF_KIND_I4) :: localrc, rc
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'TimeStringISOFrac2ESMFtime() # ',Ncall
#endif

    read(TimeStrISOFrac( 1: 4),'(i4)') yy
    read(TimeStrISOFrac( 6: 7),'(i2)') mm
    read(TimeStrISOFrac( 9:10),'(i2)') dd
    read(TimeStrISOFrac(12:13),'(i2)') h
    read(TimeStrISOFrac(15:16),'(i2)') m
    read(TimeStrISOFrac(18:19),'(i2)') s

    call ESMF_TimeSet(ESMFtime,yy=yy,mm=mm,dd=dd,h=h,m=m,s=s,          &
                      calkindflag=ESMF_CALKIND_GREGORIAN, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#ifdef DEBUG
   write(debug,*) 'Leaving TimeStringISOFrac2ESMFtime()'
   write(debug,*)
#endif
   return

   end subroutine TimeStringISOFrac2ESMFtime
!EOC
!-----------------------------------------------------------------------

   end module getm_component

!-----------------------------------------------------------------------
! Copyright (C) 2013 - Knut Klingbeil                                  !
!-----------------------------------------------------------------------
