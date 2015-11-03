!> @brief Implementation of a GETM ocean component
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014, 2015 Helmholtz-Zentrum Geesthacht
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
  use mossco_state
  use mossco_field

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
  REALTYPE,dimension(:,:),allocatable                :: cosconv,sinconv
  real(ESMF_KIND_R8),dimension(:,:)  ,allocatable :: areaC
  real(ESMF_KIND_R8),dimension(:,:,:),allocatable :: areaW3D
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
  real(ESMF_KIND_R8),pointer :: U2D (:,:)  =>NULL(),V2D (:,:)  =>NULL()
  real(ESMF_KIND_R8),pointer :: U3D (:,:,:)=>NULL(),V3D (:,:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: Ubot(:,:)  =>NULL(),Vbot(:,:)  =>NULL()
  real(ESMF_KIND_R8),pointer :: Tbot(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: T3D(:,:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: S3D(:,:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: swr(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: nybot(:,:)=>NULL(),tkebot(:,:)=>NULL(),epsbot(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: tke3D(:,:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: windU(:,:)=>NULL(),windV(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: waveH(:,:)=>NULL(),waveT(:,:)=>NULL(),waveK(:,:)=>NULL(),waveDir(:,:)=>NULL()

  type :: ptrarray3D
     real(ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr=>NULL()
     real(ESMF_KIND_R8)                          :: hackmax=-1.0
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

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_READRESTART, phase=1, &
      userRoutine=ReadRestart, rc=localrc)
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
    use meteo      ,only: met_method,METEO_CONST,METEO_FROMFILE,METEO_FROMEXT
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
#ifdef GETM_PARALLEL
    character(len=MPI_MAX_ERROR_STRING) :: mpierrmsg
#endif
    type(ESMF_FieldBundle)  :: fieldBundle
    integer(ESMF_KIND_I4) :: localrc
    character(ESMF_MAXSTR)  :: name

    rc=ESMF_SUCCESS

    call MOSSCO_GridCompEntryLog(gridComp)

    call ESMF_GridCompGet(gridComp,vmIsPresent=vmIsPresent,       &
                                   clockIsPresent=clockIsPresent, &
                                   name=name, rc=localrc)
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
      call getmCmp_StateAddPtr("water_depth_at_soil_surface",depth,exportState,"m",name)
    end if
    if (associated(hbot)) then
      call getmCmp_StateAddPtr("layer_height_at_soil_surface",hbot,exportState,"m",name)
    end if
    if (associated(U2D)) then
      call getmCmp_StateAddPtr("depth_averaged_x_velocity_in_water",U2D,exportState,"m s-1",name)
    end if
    if (associated(V2D)) then
      call getmCmp_StateAddPtr("depth_averaged_y_velocity_in_water",V2D,exportState,"m s-1",name)
    end if
    if (associated(U3D)) then
      call getmCmp_StateAddPtr("x_velocity_in_water",U3D,exportState,"m s-1",name)
    end if
    if (associated(V3D)) then
      call getmCmp_StateAddPtr("y_velocity_in_water",V3D,exportState,"m s-1",name)
    end if
    if (associated(Ubot)) then
      call getmCmp_StateAddPtr("x_velocity_at_soil_surface",Ubot,exportState,"m s-1",name)
    end if
    if (associated(Vbot)) then
      call getmCmp_StateAddPtr("y_velocity_at_soil_surface",Vbot,exportState,"m s-1",name)
    end if
    if (associated(Tbot)) then
      call getmCmp_StateAddPtr("temperature_at_soil_surface",Tbot,exportState,"degC",name)
    end if
    if (associated(T3D)) then
      call getmCmp_StateAddPtr("temperature_in_water",T3D,exportState,"degC",name)
    end if
    if (associated(S3D)) then
      call getmCmp_StateAddPtr("salinity_in_water",S3D,exportState,"",name)
    end if
    if (associated(swr)) then
      call getmCmp_StateAddPtr("surface_downwelling_photosynthetic_radiative_flux",swr,exportState,"W m-2",name)
    end if
    if (associated(nybot)) then
      call getmCmp_StateAddPtr("turbulent_diffusivity_of_momentum_at_soil_surface",nybot,exportState,"m2 s-1",name)
    end if
    if (associated(tkebot)) then
      call getmCmp_StateAddPtr("turbulent_kinetic_energy_at_soil_surface",tkebot,exportState,"m2 s-2",name)
    end if
    if (associated(epsbot)) then
      call getmCmp_StateAddPtr("dissipation_of_tke_at_soil_surface",epsbot,exportState,"m2 s-3",name)
    end if
    if (associated(tke3D)) then
      call getmCmp_StateAddPtr("turbulent_kinetic_energy_in_water",tke3D,exportState,"m2 s-2",name,StaggerLoc=ESMF_STAGGERLOC_CENTER_VFACE)
    end if

    select case (met_method)
      case(METEO_CONST,METEO_FROMFILE)
        if (associated(windU)) then
          call getmCmp_StateAddPtr("wind_x_velocity_at_10m",windU,exportState,"m s-1",name)
        end if
        if (associated(windV)) then
          call getmCmp_StateAddPtr("wind_y_velocity_at_10m",windV,exportState,"m s-1",name)
        end if
      case(METEO_FROMEXT)
        if (associated(windU)) then
          call getmCmp_StateAddPtr("wind_x_velocity_at_10m",windU,importState,"m s-1",name)
        end if
        if (associated(windV)) then
          call getmCmp_StateAddPtr("wind_y_velocity_at_10m",windV,importState,"m s-1",name)
        end if
    end select

    select case (waveforcing_method)
      case(WAVES_FROMWIND,WAVES_FROMFILE)
        if (associated(waveH)) then
          call getmCmp_StateAddPtr("wave_height",waveH,exportState,"m",name)
        end if
        if (associated(waveT)) then
          call getmCmp_StateAddPtr("wave_period",waveT,exportState,"s",name)
        end if
        if (associated(waveK)) then
          call getmCmp_StateAddPtr("wave_number",waveK,exportState,"m-1",name)
        end if
        if (associated(waveDir)) then
          call getmCmp_StateAddPtr("wave_direction",waveDir,exportState,"rad",name)
        end if
      case(WAVES_FROMEXT)
        if (associated(waveH)) then
          call getmCmp_StateAddPtr("wave_height",waveH,importState,"m",name)
        end if
        if (associated(waveT)) then
          call getmCmp_StateAddPtr("wave_period",waveT,importState,"s",name)
        end if
        if (associated(waveK)) then
          call getmCmp_StateAddPtr("wave_number",waveK,importState,"m-1",name)
        end if
        if (associated(waveDir)) then
          call getmCmp_StateAddPtr("wave_direction",waveDir,importState,"rad",name)
        end if
    end select

    fieldBundle = ESMF_FieldBundleCreate(name='concentrations_in_water',multiflag=.true.,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleSet(fieldBundle,getmGrid3D,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(fieldBundle,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_StateAdd(importState,(/fieldBundle/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    fieldBundle = ESMF_FieldBundleCreate(name='concentrations_z_velocity_in_water',multiflag=.true.,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleSet(fieldBundle,getmGrid3D,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(fieldBundle,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_StateAdd(importState,(/fieldBundle/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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

      type(ESMF_FieldBundle)                              :: concFieldBundle,wsFieldBundle
      type(ESMF_Field)          ,dimension(:),allocatable :: concFieldList,fieldList
      type(ESMF_Field)                                    :: wsField
      type(ESMF_FieldStatus_Flag)                         :: status
      character(len=ESMF_MAXSTR),dimension(:),allocatable :: itemNameList
      character(len=ESMF_MAXSTR)                          :: itemName
      integer                   ,dimension(:),allocatable :: namelenList,concFlags
      integer                                             :: concFieldCount,transportFieldCount,FieldCount
      integer                                             :: conc_id,ws_id
      integer                                             :: i,ii,n
      character(len=*),parameter :: ws_suffix="_z_velocity_in_water"
      character(len=*),parameter :: conc_suffix="_in_water"
    integer(ESMF_KIND_I4) :: localrc

      rc=ESMF_SUCCESS

      call MOSSCO_GridCompEntryLog(gridComp)

      call ESMF_StateGet(importState,"concentrations_in_water",concFieldBundle, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldBundleGet(concFieldBundle,fieldCount=concFieldCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (concFieldCount .gt. 0) then

         allocate(concFieldList(concFieldCount))
         allocate(itemNameList (concFieldCount))
         allocate(namelenList  (concFieldCount))
         allocate(concFlags    (concFieldCount))

         call ESMF_FieldBundleGet(concFieldBundle, fieldList=concFieldList, fieldNameList=itemNameList, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         concFlags = 0
         do i=1,concFieldCount
!           identify concentrations by suffix
            namelenList(i) = len_trim(itemNameList(i))
            if (namelenList(i) .le. len_trim(conc_suffix) ) cycle
            if (itemNameList(i)(namelenList(i)-len_trim(conc_suffix)+1:namelenList(i)) .ne. trim(conc_suffix)) cycle
            concFlags(i) = 1
         end do

         transportFieldCount = sum(concFlags)

         if (transportFieldCount .gt. 0) then

            allocate(transport_conc(transportFieldCount))
            allocate(transport_ws  (transportFieldCount))

            call ESMF_StateGet(importState, "concentrations_z_velocity_in_water", wsFieldBundle, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            n = 1

            do i=1,concFieldCount

               if (concFlags(i) .eq. 0) cycle

               call ESMF_FieldGet(concFieldList(i),status=status, rc=localrc)
               if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

               if (status.eq.ESMF_FIELDSTATUS_EMPTY) then
                  call ESMF_LogWrite('  will transport internal field '//trim(itemNameList(i)),ESMF_LOGMSG_INFO)
                  allocate(transport_conc(n)%ptr(I3DFIELD))
                  call ESMF_FieldEmptyComplete(concFieldList(i),getmGrid3D,       &
                                               transport_conc(n)%ptr,             &
                                               ESMF_INDEX_DELOCAL,                &
                                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                                               totalLWidth=(/HALO,HALO,1/),       &
                                               totalUWidth=(/HALO,HALO,0/),rc=localrc)
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
               else if (status .eq. ESMF_FIELDSTATUS_COMPLETE) then
                  call ESMF_LogWrite('  will transport external field '//trim(itemNameList(i)),ESMF_LOGMSG_INFO)
                  call ESMF_FieldGet(concFieldList(i), farrayPtr=transport_conc(n)%ptr, rc=localrc)
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
                  if (.not. (      all(lbound(transport_conc(n)%ptr) .eq. (/imin-HALO,jmin-HALO,0   /)) &
                             .and. all(ubound(transport_conc(n)%ptr) .eq. (/imax+HALO,jmax+HALO,kmax/)) ) ) then
                     call ESMF_LogWrite('invalid field bounds', &
                                        ESMF_LOGMSG_ERROR,ESMF_CONTEXT)
                     call ESMF_Finalize(endflag=ESMF_END_ABORT)
                  end if
               else
                  call ESMF_LogWrite('field '//trim(itemNameList(i))//' neither empty nor complete', &
                                     ESMF_LOGMSG_ERROR,ESMF_CONTEXT)
                  call ESMF_Finalize(endflag=ESMF_END_ABORT)
               end if

               !> set maximum value for boundary condition
               if (trim(itemNameList(i))=='Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water') then
                 transport_conc(n)%hackmax=0.8
                 call ESMF_LogWrite('  use maximum boundary value of 0.8 for '//trim(itemNameList(i)),ESMF_LOGMSG_WARNING)
               end if
               if (trim(itemNameList(i))=='Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water') then
                 transport_conc(n)%hackmax=8.0
                 call ESMF_LogWrite('  use maximum boundary value of 8.0 for '//trim(itemNameList(i)),ESMF_LOGMSG_WARNING)
               end if

!              search for corresponding z_velocity
               itemName = itemNameList(i)(:namelenList(i)-len_trim(conc_suffix))//ws_suffix

               call ESMF_FieldBundleGet(wsFieldBundle, itemName, fieldCount=fieldCount, rc=localrc)
               if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

               if (fieldCount .eq. 0) then
                  call ESMF_LogWrite('  no corresponding field '//trim(itemName),ESMF_LOGMSG_INFO)
                  transport_ws(n)%ptr => null()
                  n = n + 1
                  cycle
               else if (fieldCount .eq. 1) then
                  call ESMF_FieldBundleGet(wsFieldBundle, itemName, field=wsField, rc=localrc)
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
               else
                  call ESMF_AttributeGet(concFieldList(i), 'external_index', value=conc_id, defaultValue=-1, rc=localrc)
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
                  allocate(fieldList(fieldCount))
                  call ESMF_FieldBundleGet(wsFieldBundle, itemName, fieldList=fieldList, rc=localrc)
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
                  do ii=1,fieldCount
                     call ESMF_AttributeGet(fieldList(ii), 'external_index', value=ws_id, defaultValue=-2, rc=localrc)
                     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
                     if (ws_id .eq. conc_id) then
                        wsField = fieldList(ii)
                        exit
                     end if
                  end do
                  deallocate(fieldList)
                  if (ws_id .ne. conc_id) then
                     call ESMF_LogWrite('  no unique field '//trim(itemName), &
                                        ESMF_LOGMSG_ERROR,ESMF_CONTEXT)
                     call ESMF_Finalize(endflag=ESMF_END_ABORT)
                  end if
               end if

               call ESMF_FieldGet(wsField, status=status, rc=localrc)
               if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

               if (status.eq.ESMF_FIELDSTATUS_EMPTY) then
                  call ESMF_LogWrite('  will use internal field '//trim(itemName),ESMF_LOGMSG_INFO)
                  allocate(transport_ws(n)%ptr(I3DFIELD))
                  call ESMF_FieldEmptyComplete(wsField,getmGrid3D,                  &
                                               transport_ws(n)%ptr,               &
                                               ESMF_INDEX_DELOCAL,                &
                                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                                               totalLWidth=(/HALO,HALO,1/),       &
                                               totalUWidth=(/HALO,HALO,0/),rc=localrc)
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
               else if (status .eq. ESMF_FIELDSTATUS_COMPLETE) then
                  call ESMF_LogWrite('  will use external field '//trim(itemName),ESMF_LOGMSG_INFO)
                  call ESMF_FieldGet(wsField,farrayPtr=transport_ws(n)%ptr,rc=localrc)
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
                  if (.not. (      all(lbound(transport_ws(n)%ptr) .eq. (/imin-HALO,jmin-HALO,0   /)) &
                             .and. all(ubound(transport_ws(n)%ptr) .eq. (/imax+HALO,jmax+HALO,kmax/)) ) ) then
                     call ESMF_LogWrite('  invalid field bounds', &
                                        ESMF_LOGMSG_ERROR,ESMF_CONTEXT)
                     call ESMF_Finalize(endflag=ESMF_END_ABORT)
                  end if
               else
                  call ESMF_LogWrite('  field '//trim(itemName)//' neither empty nor complete', &
                                     ESMF_LOGMSG_ERROR,ESMF_CONTEXT)
                  call ESMF_Finalize(endflag=ESMF_END_ABORT)
               end if

               n = n + 1

            end do

         end if

      end if

    call MOSSCO_GridCompExitLog(gridComp)
    rc = ESMF_SUCCESS

   end subroutine InitializeP2

!-----------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ReadRestart"
  subroutine ReadRestart(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    rc=ESMF_SUCCESS

    !> Here omes your restart code, which in the simplest case copies
    !> values from all fields in importState to those in exportState

  end subroutine ReadRestart


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
    if (localrc .ne. ESMF_SUCCESS) then
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
   use domain         ,only: grid_type,convc,arcd1
   use domain         ,only: getm_has_lonlat => have_lonlat
   use initialise     ,only: runtype
   use variables_2d   ,only: D
#ifndef NO_3D
   use variables_3d   ,only: hn,num,tke,eps
#ifndef NO_BAROCLINIC
   use m3d            ,only: calc_temp,calc_salt
   use variables_3d   ,only: T,S
#endif
#endif
   use meteo          ,only: metforcing,met_method,calc_met
   use meteo          ,only: METEO_CONST,METEO_FROMFILE,METEO_FROMEXT
   use meteo          ,only: u10,v10,swr_=>swr
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
   REALTYPE, parameter :: pi=3.1415926535897932384626433832795029d0
   REALTYPE, parameter :: deg2rad=pi/180
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
            if (getm_has_lonlat) then
               allocate(lonx2D(E2DXFIELD)) ; lonx2D = lonx
               allocate(latx2D(E2DXFIELD)) ; latx2D = latx
               allocate(lonc2D(E2DFIELD )) ; lonc2D = lonc
               allocate(latc2D(E2DFIELD )) ; latc2D = latc
            else
               allocate(xx2D(E2DXFIELD)) ; xx2D = xx
               allocate(yx2D(E2DXFIELD)) ; yx2D = yx
               allocate(xc2D(E2DFIELD )) ; xc2D = xc
               allocate(yc2D(E2DFIELD )) ; yc2D = yc
            end if
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
         allocate(tkebot(I2DFIELD))
         allocate(epsbot(I2DFIELD))
#ifdef FOREIGN_GRID
         allocate(tke3D(I3DFIELD))
#else
         allocate(tke3D(imin:imax,jmin:jmax,1:kmax))
#endif
#ifndef NO_BAROCLINIC
         if (calc_temp) then
            allocate(Tbot(I2DFIELD))
#ifdef FOREIGN_GRID
            allocate(T3D(I3DFIELD))
#else
            allocate(T3D(imin:imax,jmin:jmax,1:kmax))
#endif
         end if
         if (calc_salt) then
#ifdef FOREIGN_GRID
            allocate(S3D(I3DFIELD))
#else
            allocate(S3D(imin:imax,jmin:jmax,1:kmax))
#endif
         end if
#endif
#endif
      end if
      if (metforcing) then
         allocate(swr(E2DFIELD))
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
            if (getm_has_lonlat) then
               lonx2D => lonx
               latx2D => latx
               lonc2D => lonc
               latc2D => latc
            else
               xx2D => xx
               yx2D => yx
               xc2D => xc
               yc2D => yc
            end if
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
#if 1
!        turbulent quantities still without target attribute in getm
         allocate(nybot (I2DFIELD))
         allocate(tkebot(I2DFIELD))
         allocate(epsbot(I2DFIELD))
#ifdef FOREIGN_GRID
         allocate(tke3D (I3DFIELD))
#else
         allocate(tke3D (imin:imax,jmin:jmax,1:kmax))
#endif
#else
#if 0
         nybot (imin-HALO:,jmin-HALO:) => num(:,:,1)
         tkebot(imin-HALO:,jmin-HALO:) => tke(:,:,1)
         epsbot(imin-HALO:,jmin-HALO:) => eps(:,:,1)
#else
         p2d => num(:,:,1)
         nybot (imin-HALO:,jmin-HALO:) => p2d
         p2d => tke(:,:,1)
         tkebot(imin-HALO:,jmin-HALO:) => p2d
         p2d => eps(:,:,1)
         epsbot(imin-HALO:,jmin-HALO:) => p2d
#endif
#ifdef FOREIGN_GRID
         tke3d => tke
#else
         tke3D => tke(imin:imax,jmin:jmax,1:kmax)
#endif
#endif
#ifndef NO_BAROCLINIC
         if (calc_temp) then
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
         if (calc_salt) then
#ifdef FOREIGN_GRID
            S3D => S
#else
            S3D => S(imin:imax,jmin:jmax,1:kmax)
#endif
         end if
#endif
#endif
      end if
      if (metforcing) then
         swr => swr_
      end if
      if (waveforcing_method .ne. NO_WAVES) then
         waveH   => waveH_
         waveT   => waveT_
         waveK   => waveK_
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

   allocate(areaC(E2DFIELD))
   allocate(maskC3D(E2DFIELD,0:klen))
   allocate(maskX3D(E2DFIELD,0:klen))
   allocate(areaW3D(E2DFIELD,0:klen))

   where(az .gt. 0)
      areaC = _ONE_/arcd1
   elsewhere
      areaC = _ZERO_
   end where
   do k=1,klen
      maskC3D(:,:,k) = az
      maskX3D(:,:,k) = ax
      areaW3D(:,:,k) = areaC
   end do
   maskC3D(:,:,0) = 0
   maskX3D(:,:,0) = 0
   areaW3D(:,:,0) = areaC

   allocate(zw(E2DFIELD ,0:klen))
   allocate(zc(E2DFIELD ,1:klen))
   allocate(zx(E2DXFIELD,0:klen))

   allocate(cosconv(E2DFIELD))
   allocate(sinconv(E2DFIELD))
   cosconv = cos( deg2rad*convc )
   sinconv = sin( deg2rad*convc )

#ifdef FOREIGN_GRID
   allocate(U3D(I2DFIELD,0:klen))
   allocate(V3D(I2DFIELD,0:klen))
#else
   allocate(U3D(I2DFIELD,1:klen))
   allocate(V3D(I2DFIELD,1:klen))
#endif

   if (klen .eq. 1) then
      if (associated(U3D)) then
#if 0
         U2D(imin-HALO:,jmin-HALO:) => U3D(:,:,1)
#else
         p2d => U3D(:,:,1)
         U2D(imin-HALO:,jmin-HALO:) => p2d
#endif
      else
         allocate(U2D(E2DFIELD))
      end if
      if (associated(V3D)) then
#if 0
         V2D(imin-HALO:,jmin-HALO:) => U3D(:,:,1)
#else
         p2d => U3D(:,:,1)
         V2D(imin-HALO:,jmin-HALO:) => p2d
#endif
      else
         allocate(V2D(E2DFIELD))
      end if
      Ubot => U2D
      Vbot => V2D
   else
      allocate(U2D(E2DFIELD))
      allocate(V2D(E2DFIELD))

      if (associated(U3D)) then
#if 0
         Ubot(imin-HALO:,jmin-HALO:) => U3D(:,:,1)
#else
         p2d => U3D(:,:,1)
         Ubot(imin-HALO:,jmin-HALO:) => p2d
#endif
      else
         allocate(Ubot(E2DFIELD))
      end if
      if (associated(V3D)) then
#if 0
         Vbot(imin-HALO:,jmin-HALO:) => V3D(:,:,1)
#else
         p2d => V3D(:,:,1)
         Vbot(imin-HALO:,jmin-HALO:) => p2d
#endif
      else
         allocate(Vbot(E2DFIELD))
      end if
   end if

   if (metforcing) then
      if (calc_met .or. met_method.eq.METEO_CONST .or. met_method.eq.METEO_FROMFILE) then
         if (noKindMatch .or. grid_type.ne.2) then
            allocate(windU(E2DFIELD))
            allocate(windV(E2DFIELD))
         else
            windU => u10
            windV => v10
         end if
      end if
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
    use domain    , only: getm_has_lonlat => have_lonlat
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
    character(ESMF_MAXSTR)   :: name
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
         call ESMF_AttributeSet(xcArray2D,'units', 'm', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray2D = ESMF_ArrayCreate(getmDistGrid2D,yc1D,indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray2D,'units', 'm', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xcArray3D = ESMF_ArrayCreate(getmDistGrid3D,xc1D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray3D,'units', 'm', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray3D = ESMF_ArrayCreate(getmDistGrid3D,yc1D,indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1,0/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray3D,'units', 'm', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xxArray2D = ESMF_ArrayCreate(getmDistGrid2D,xx1D,          &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray2D,'units', 'm', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray2D = ESMF_ArrayCreate(getmDistGrid2D,yx1D,          &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1/),   &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray2D,'units', 'm', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xxArray3D = ESMF_ArrayCreate(getmDistGrid3D,xx1D,          &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray3D,'units', 'm', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray3D = ESMF_ArrayCreate(getmDistGrid3D,yx1D,          &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1,0/), &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray3D,'units', 'm', rc=localrc)
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
         call ESMF_AttributeSet(xcArray2D,'units', 'degrees_east', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray2D = ESMF_ArrayCreate(getmDistGrid2D,latc1D,indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray2D,'units', 'degrees_north', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xcArray3D = ESMF_ArrayCreate(getmDistGrid3D,lonc1D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray3D,'units', 'degrees_east', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray3D = ESMF_ArrayCreate(getmDistGrid3D,latc1D,indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1,0/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray3D,'units', 'degrees_north', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xxArray2D = ESMF_ArrayCreate(getmDistGrid2D,lonx1D,        &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray2D,'units', 'degrees_east', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray2D = ESMF_ArrayCreate(getmDistGrid2D,latx1D,        &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1/),   &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray2D,'units', 'degrees_north', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xxArray3D = ESMF_ArrayCreate(getmDistGrid3D,lonx1D,        &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray3D,'units', 'degrees_east', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray3D = ESMF_ArrayCreate(getmDistGrid3D,latx1D,        &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1,0/), &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray3D,'units', 'degrees_north', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      case(3)
        !> if getm has longitude and latitude information on the grid, then
        !! use this for the ESMF exchange arrays rather than Cartesian coordinates.
        !! Please be aware, that the curvilinear grid itself is created and defined
        !! inside GETM in Cartesian coordinates only for grid_type=3.
        if (getm_has_lonlat) then
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
         call ESMF_AttributeSet(xxArray2D,'units', 'degrees_east', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray2D = ESMF_ArrayCreate(getmDistGrid2D,latx2D,         &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray2D,'units', 'degrees_north', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xxArray3D = ESMF_ArrayCreate(getmDistGrid3D,lonx2D,         &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray3D,'units', 'degrees_east', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray3D = ESMF_ArrayCreate(getmDistGrid3D,latx2D,         &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray3D,'units', 'degrees_north', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xcArray2D = ESMF_ArrayCreate(getmDistGrid2D,lonc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray2D,'units', 'degrees_east', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray2D = ESMF_ArrayCreate(getmDistGrid2D,latc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray2D,'units', 'degrees_north', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xcArray3D = ESMF_ArrayCreate(getmDistGrid3D,lonc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray3D,'units', 'degrees_east', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray3D = ESMF_ArrayCreate(getmDistGrid3D,latc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray3D,'units', 'degrees_north', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        else
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
         call ESMF_AttributeSet(xxArray2D,'units', 'm', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray2D = ESMF_ArrayCreate(getmDistGrid2D,yx2D,           &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray2D,'units', 'm', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xxArray3D = ESMF_ArrayCreate(getmDistGrid3D,xx2D,           &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray3D,'units', 'm', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray3D = ESMF_ArrayCreate(getmDistGrid3D,yx2D,           &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray3D,'units', 'm', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xcArray2D = ESMF_ArrayCreate(getmDistGrid2D,xc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray2D,'units', 'm', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray2D = ESMF_ArrayCreate(getmDistGrid2D,yc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray2D,'units', 'm', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xcArray3D = ESMF_ArrayCreate(getmDistGrid3D,xc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray3D,'units', 'm', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray3D = ESMF_ArrayCreate(getmDistGrid3D,yc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray3D,'units', 'm', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        end if ! getm_has_lonlat

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
         call ESMF_AttributeSet(xxArray2D,'units', 'degrees_east', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray2D = ESMF_ArrayCreate(getmDistGrid2D,latx2D,         &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray2D,'units', 'degrees_north', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xxArray3D = ESMF_ArrayCreate(getmDistGrid3D,lonx2D,         &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xxArray3D,'units', 'degrees_east', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         yxArray3D = ESMF_ArrayCreate(getmDistGrid3D,latx2D,         &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(yxArray3D,'units', 'degrees_north', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xcArray2D = ESMF_ArrayCreate(getmDistGrid2D,lonc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray2D,'units', 'degrees_east', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray2D = ESMF_ArrayCreate(getmDistGrid2D,latc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray2D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray2D,'units', 'degrees_north', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         xcArray3D = ESMF_ArrayCreate(getmDistGrid3D,lonc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(xcArray3D,'units', 'degrees_east', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         ycArray3D = ESMF_ArrayCreate(getmDistGrid3D,latc2D,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray3D,'creator', trim(name), rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         call ESMF_AttributeSet(ycArray3D,'units', 'degrees_north', rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   end select

!  Note (KK): gridAlign specifies which corner point in a grid cell
!             shares the center indices [ default=(/-1,...,-1/) ].
!             gridEdge[L|U]Width only affect DE's at the edge of tiles
!             (thus it matters whether a single- or multi-tile DistGrid
!              was created). If gridEdgeWidth's are not set, they are set
!             automatically based on gridAlign.
!  internal call to ESMF_GridCreateFrmDistGrid()
   getmGrid2D = ESMF_GridCreate(getmDistGrid2D,                        &
                                name="getmGrid2D_"//trim(name),        &
                                gridAlign=(/1,1/),                     &
                                coordSys=coordSys,                     &
                                coordDimCount=int(coordDimCount(1:2)), &
                                coordDimMap=int(coordDimMap(1:2,1:2)), &
                                rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(getmGrid2D,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   getmGrid3D = ESMF_GridCreate(getmDistGrid3D,                 &
                                name="getmGrid3D_"//trim(name), &
                                gridAlign=(/1,1,1/),            &
                                coordSys=coordSys,              &
                                coordDimCount=coordDimCount,    &
                                coordDimMap=coordDimMap,        &
                                rc=localrc)
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
   array = ESMF_ArrayCreate(getmDistGrid2D,areaC,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(array,'units','m**2', rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call ESMF_GridSetItem(getmGrid2D,ESMF_GRIDITEM_AREA,array=array,staggerloc=StaggerLoc, rc=localrc)
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
   array = ESMF_ArrayCreate(getmDistGrid3D,maskC3D,indexflag=ESMF_INDEX_DELOCAL, &
                            totalLWidth=(/HALO,HALO,1/),totalUWidth=(/HALO,HALO,0/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call ESMF_GridSetItem(getmGrid3D,ESMF_GRIDITEM_MASK,array=array,staggerloc=StaggerLoc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!  KK-TODO: add attribute with un-/mask value?
   !array = ESMF_ArrayCreate(getmDistGrid3D,areaC,indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
   array = ESMF_ArrayCreate(getmDistGrid3D,areaW3D,indexflag=ESMF_INDEX_DELOCAL, &
                            totalLWidth=(/HALO,HALO,1/),totalUWidth=(/HALO,HALO,0/), rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(array,'units','m**2', rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(array,'creator', trim(name), rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call ESMF_GridSetItem(getmGrid3D,ESMF_GRIDITEM_AREA,array=array,staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
   use domain         ,only: dxv,dyu,arcd1
   use initialise     ,only: runtype
   use variables_2d   ,only: zo,z,D,Dvel,U,DU,V,DV
#ifndef NO_3D
   use variables_3d   ,only: dt,ho,hn,hvel,uu,hun,vv,hvn,ww,num,tke,eps
#ifndef NO_BAROCLINIC
   use m3d            ,only: calc_temp,calc_salt
   use variables_3d   ,only: T,S
#endif
#endif
   use m2d            ,only: dtm
   use meteo          ,only: metforcing,met_method,METEO_CONST,METEO_FROMFILE
   use meteo          ,only: u10,v10,swr_=>swr
   use waves          ,only: waveforcing_method,NO_WAVES,WAVES_FROMWIND,WAVES_FROMFILE
   use waves          ,only: waves_method,WAVES_NOSTOKES
   use variables_waves,only: waveH_=>waveH,waveT_=>waveT,waveK_=>waveK
   use variables_waves,only: coswavedir,sinwavedir
   use variables_waves,only: UStokesC,VStokesC,uuStokesC,vvStokesC

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
   REALTYPE,dimension(I3DFIELD),target  :: t_vel3d
   REALTYPE,dimension(:,:)  ,pointer    :: p_vel
   REALTYPE,dimension(:,:,:),pointer    :: p_vel3d
   integer                              :: k,klen
   REALTYPE,parameter                   :: vel_missing=0.0d0
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
         hbot   = hn (:,:,1)
         nybot  = num(:,:,1)
         tkebot = tke(:,:,1)
         epsbot = eps(:,:,1)
#ifdef FOREIGN_GRID
         tke3D = tke
#else
         tke3D = tke(imin:imax,jmin:jmax,1:kmax)
#endif
#ifndef NO_BAROCLINIC
         if (calc_temp) then
            Tbot = T(:,:,1)
#ifdef FOREIGN_GRID
            T3D = T
#else
            T3D = T(imin:imax,jmin:jmax,1:kmax)
#endif
         end if
         if (calc_salt) then
#ifdef FOREIGN_GRID
            S3D = S
#else
            S3D = S(imin:imax,jmin:jmax,1:kmax)
#endif
         end if
#endif
      end if
#endif
      if (metforcing) then ! still required...
         swr = swr_
      end if
      if (waveforcing_method.eq.WAVES_FROMWIND .or. waveforcing_method.eq.WAVES_FROMFILE) then
         waveH   = waveH_
         waveT   = waveT_
         waveK   = waveK_
      end if
   else
#if 1
!     turbulent quantities still without target attribute in getm
      if (runtype .gt. 1) then
         nybot  = num(:,:,1)
         tkebot = tke(:,:,1)
         epsbot = eps(:,:,1)
#ifdef FOREIGN_GRID
         tke3D = tke
#else
         tke3D = tke(imin:imax,jmin:jmax,1:kmax)
#endif
      end if
#endif
!     Note (KK): update pointer because of pointer swap within GETM
      if (metforcing .and. met_method.eq.2) then
         swr => swr_
      end if
      if (waveforcing_method .eq. WAVES_FROMFILE) then
         waveH => waveH_
      end if
   end if

   wrk = _ZERO_

   if (noKindMatch) then
      p_vel => t_vel
   else
      p_vel => U2D
   end if
   call to_u(imin,jmin,imax,jmax,az,                                 &
             dtm,grid_type,                                          &
             dxv,dyu,arcd1,                                          &
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
             dxv,dyu,arcd1,                                          &
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
      if (associated(U3D)) then
         if (noKindMatch) then
            p_vel3d => t_vel3d
         else
            p_vel3d => U3D
         end if
         do k=1,kmax
            call to_u(imin,jmin,imax,jmax,az,                            &
                      dt,grid_type,                                      &
                      dxv,dyu,arcd1,                                     &
                      xc,xu,xv,hn(:,:,k),ho(:,:,k),hvel(:,:,k),          &
                      uu(:,:,k),hun(:,:,k),vv(:,:,k),hvn(:,:,k),         &
                      ww(:,:,k-1),ww(:,:,k),vel_missing,p_vel3d(:,:,k))
         end do
         if (noKindMatch) then
            U3D(:,:,1:kmax) = t_vel3d(:,:,1:kmax)
         end if
      else
         if (noKindMatch) then
            p_vel => t_vel
         else
            p_vel => Ubot
         end if
         call to_u(imin,jmin,imax,jmax,az,                            &
                   dt,grid_type,                                      &
                   dxv,dyu,arcd1,                                     &
                   xc,xu,xv,hn(:,:,1),ho(:,:,1),hvel(:,:,1),          &
                   uu(:,:,1),hun(:,:,1),vv(:,:,1),hvn(:,:,1),         &
                   ww(:,:,0),ww(:,:,1),vel_missing,p_vel)
         if (noKindMatch) then
            Ubot = t_vel
         end if
      end if
      if (associated(V3D)) then
         if (noKindMatch) then
            p_vel3d => t_vel3d
         else
            p_vel3d => V3D
         end if
         do k=1,kmax
            call to_v(imin,jmin,imax,jmax,az,                            &
                      dt,grid_type,                                      &
                      dxv,dyu,arcd1,                                     &
                      yc,yu,yv,hn(:,:,k),ho(:,:,k),hvel(:,:,k),          &
                      uu(:,:,k),hun(:,:,k),vv(:,:,k),hvn(:,:,k),         &
                      ww(:,:,k-1),ww(:,:,k),vel_missing,p_vel3d(:,:,k))
         end do
         if (noKindMatch) then
            V3D(:,:,1:kmax) = t_vel3d(:,:,1:kmax)
         end if
      else
         if (noKindMatch) then
            p_vel => t_vel
         else
            p_vel => Vbot
         end if
         call to_v(imin,jmin,imax,jmax,az,                            &
                   dt,grid_type,                                      &
                   dxv,dyu,arcd1,                                     &
                   yc,yu,yv,hn(:,:,1),ho(:,:,1),hvel(:,:,1),          &
                   uu(:,:,1),hun(:,:,1),vv(:,:,1),hvn(:,:,1),         &
                   ww(:,:,0),ww(:,:,1),vel_missing,p_vel)
         if (noKindMatch) then
            Vbot = t_vel
         end if
      end if
   end if
#endif

   if (met_method.eq.METEO_CONST .or. met_method.eq.METEO_FROMFILE) then
      if (noKindMatch .or. grid_type.ne.2) then
         windU =  cosconv*u10 + sinconv*v10
         windV = -sinconv*u10 + cosconv*v10
      else
!        Note (KK): update pointer because of pointer swap within GETM
         windU => u10
         windV => v10
      end if
   end if

   if (waveforcing_method.eq.WAVES_FROMWIND .or. waveforcing_method.eq.WAVES_FROMFILE) then
      waveDir = atan2(sinwavedir,coswavedir)
   end if
   if (waveforcing_method .ne. NO_WAVES .and. waves_method.ne.WAVES_NOSTOKES) then
!     provide Eulerian velocities
      U2D = U2D - (  cosconv*UStokesC + sinconv*VStokesC )/Dvel
      V2D = V2D - ( -sinconv*UStokesC + cosconv*VStokesC )/Dvel
#ifndef NO_3D
      if (klen .gt. 1) then
         if (associated(U3D)) then
            do k=1,kmax
               U3D(:,:,k) = U3D(:,:,k) - (  cosconv*uuStokesC(:,:,k) + sinconv*vvStokesC(:,:,k) )/hvel(:,:,k)
            end do
         else
            Ubot = Ubot - (  cosconv*uuStokesC(:,:,1) + sinconv*vvStokesC(:,:,1) )/hvel(:,:,1)
         end if
         if (associated(V3D)) then
            do k=1,kmax
               V3D(:,:,k) = V3D(:,:,k) - ( -sinconv*uuStokesC(:,:,k) + cosconv*vvStokesC(:,:,k) )/hvel(:,:,k)
            end do
         else
            Vbot = Vbot - ( -sinconv*uuStokesC(:,:,1) + cosconv*vvStokesC(:,:,1) )/hvel(:,:,1)
         end if
      end if
#endif
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
   use domain         ,only: grid_type
   use meteo          ,only: metforcing,met_method,METEO_FROMEXT,calc_met
   use meteo          ,only: u10,v10,new_meteo
   use waves          ,only: waveforcing_method,WAVES_FROMEXT,new_waves
   use variables_waves,only: waveH_=>waveH,waveT_=>waveT,waveK_=>waveK
   use variables_waves,only: coswavedir,sinwavedir

   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES:
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_update_importState() # ',Ncall
#endif

   if (met_method .eq. METEO_FROMEXT) then
      new_meteo = .true. ! KK-TODO: should be set by coupler
      if (calc_met) then
         if (noKindMatch .or. grid_type.ne.2) then
            u10 = cosconv*windU - sinconv*windV
            v10 = sinconv*windU + cosconv*windV
         end if
      end if
   end if

   if (waveforcing_method .eq. WAVES_FROMEXT) then
      new_waves = .true. ! KK-TODO: should be set by coupler
      if (noKindMatch) then
         waveH_ = waveH
         waveT_ = waveT
         waveK_ = waveK
      end if
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
         p_conc => t_conc
         if (associated(transport_ws(n)%ptr)) then
            t_ws = transport_ws(n)%ptr
         else
            t_ws = _ZERO_
         end if
         p_ws => t_ws
      else
         p_conc => transport_conc(n)%ptr
         if (associated(transport_ws(n)%ptr)) then
            p_ws => transport_ws(n)%ptr
         else
            t_ws = _ZERO_
            p_ws => t_ws
         end if
      end if

      call do_transport_3d(p_conc,p_ws)
      !call zero_gradient_3d_bdy(p_conc,transport_conc(n)%hackmax)

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
  subroutine getmCmp_StateAddPtr2D(name,p2d,state,units,componentName)
!
! !DESCRIPTION:
!
! !USES:
   use domain, only: imin,imax,jmin,jmax
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   character(len=*),intent(in)                          :: name,units,componentName
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
   call ESMF_AttributeSet(field,'units',trim(units))

    call ESMF_AttributeSet(field,'creator', trim(componentName), rc=localrc)
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
  subroutine getmCmp_StateAddPtr3D(name,p3d,state,units,componentName,kwe,StaggerLoc)
!
! !DESCRIPTION:
!
! !USES:
   use initialise,only: runtype
   use domain    ,only: imax,jmax,kmax
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   character(len=*),intent(in)                            :: name,units,componentName
   real(ESMF_KIND_R8),dimension(:,:,:),pointer,intent(in) :: p3d
   logical              , intent(in), optional            :: kwe !keyword-enforcer
   type(ESMF_StaggerLoc), intent(in), optional            :: StaggerLoc
!
! !INPUT/OUTPUT PARAMETERS:
   type(ESMF_State),intent(inout)                       :: state
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
   type(ESMF_Field) :: field
   type(ESMF_StaggerLoc) :: StaggerLoc_
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

   if (present(StaggerLoc)) then
      StaggerLoc_ = StaggerLoc
   else
!     KK-TODO: ESMF_STAGGERLOC_CENTER_VCENTER ?
      StaggerLoc_ = ESMF_STAGGERLOC_CENTER
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
                            name=name,StaggerLoc=StaggerLoc_,rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(field,'units',trim(units))

    call ESMF_AttributeSet(field,'creator', trim(componentName), rc=localrc)
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
