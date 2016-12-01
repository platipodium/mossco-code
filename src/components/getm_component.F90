!> @brief Implementation of a GETM ocean component
!
!  This computer program is part of MOSSCO.
!> @author Knut Klingbeil <klingbeil@io-warnemuende.de>
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!> @author Richard Hofmeister <richard.hofmeister@hzg.de>

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

  interface getmCmp_StateAddPtr
    module procedure getmCmp_StateAddPtr2D
    module procedure getmCmp_StateAddPtr3D
  end interface

! the following objects point to deep objects and do not need to be
! requested everytime again
! Note (KK): the save attribute can be deleted for F2008 standard
  type(ESMF_Clock)   ,save :: controlClock
  type(ESMF_Grid)    ,save :: getmGrid2D,getmGrid3D
  type(ESMF_GridComp),save :: getmComp

! The following objects are treated differently, depending on whether
! the kinds of GETM's internal REALTYPE matches ESMF_KIND_R8
  logical                    :: noKindMatch
  real(ESMF_KIND_R8),pointer :: depth(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: h3D (:,:,:)=>NULL(),hbot(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: U2D (:,:)  =>NULL(),V2D (:,:)  =>NULL()
  real(ESMF_KIND_R8),pointer :: U3D (:,:,:)=>NULL(),V3D (:,:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: Ubot(:,:)  =>NULL(),Vbot(:,:)  =>NULL()
  real(ESMF_KIND_R8),pointer :: T3D  (:,:,:)=>NULL(),Tbot  (:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: S3D  (:,:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: swr(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: SS3D (:,:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: NN3D (:,:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: num3D(:,:,:)=>NULL(),numbot(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: nuh3D(:,:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: tke3D(:,:,:)=>NULL(),tkebot(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: eps3D(:,:,:)=>NULL(),epsbot(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: windU(:,:)=>NULL(),windV(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: waveH(:,:)=>NULL(),waveT(:,:)=>NULL(),waveK(:,:)=>NULL(),waveDir(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: taubmax(:,:)=>NULL()

  type :: ptrarray3D
     real(ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr=>NULL()
     real(ESMF_KIND_R8)                          :: hackmax=-1.0
     real(ESMF_KIND_R8)                          :: hackmaxmin=0.0
     logical                                     :: has_boundary_data=.false.
     character(len=ESMF_MAXSTR)                  :: fieldname
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

   call MOSSCO_CompEntry(gridComp, clock)

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

    call MOSSCO_CompExit(gridComp)
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

    use getm_esmf, only: getmSetServices=>SetServices
    use time, only : init_time
    use time, only : getm_time_start => start, getm_time_stop => stop
    use time, only : getm_time_timestep => timestep
    use initialise , only: init_model,init_initialise,do_initialise
    use initialise , only: dryrun
    use meteo      ,only: met_method,METEO_CONST,METEO_FROMFILE,METEO_FROMEXT
    use waves      ,only: waveforcing_method,WAVES_FROMWIND,WAVES_FROMFILE,WAVES_FROMEXT
    implicit none

    type(ESMF_GridComp) :: gridComp
    type(ESMF_State)    :: importState,exportState ! may be uninitialized
    type(ESMF_Clock)    :: clock        ! may be uninitialized
    integer,intent(out) :: rc

    type(ESMF_Clock)      :: myClock,getmClock
    !type(ESMF_Distgrid)   :: distgrid
    type(ESMF_Field)      :: field
    !type(ESMF_Grid)       :: grid
    logical               :: clockIsPresent
    type(ESMF_TimeInterval) :: timeInterval
    integer               :: phase,phase0,phaseCount
    logical                 :: phasezeroflag
    type(ESMF_FieldBundle)  :: fieldBundle
    integer(ESMF_KIND_I4) :: localrc
    character(ESMF_MAXSTR)  :: name

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, clock)

    call ESMF_GridCompGet(gridComp,clockIsPresent=clockIsPresent, &
                                   name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (clockIsPresent) then
      call ESMF_GridCompGet(gridComp,clock=myClock)
      clockIsPresent = ESMF_ClockIsCreated(myClock)
    end if

    if (clockIsPresent) then
      getmClock = ESMF_ClockCreate(myClock)
      call ESMF_ClockSet(getmClock,name="getmClock")
      getmComp = ESMF_GridCompCreate(name="getm",clock=getmClock)
    else
      getmComp = ESMF_GridCompCreate(name="getm")
    endif

    call ESMF_GridCompSetServices(getmComp,getmSetServices)
    call ESMF_GridCompGetEPPhaseCount(getmComp,ESMF_METHOD_INITIALIZE, &
                                      phaseCount,phaseZeroFlag)
    phase0=1
    if (phaseZeroFlag) phase0=0

    do phase=phase0,1
      call ESMF_GridCompInitialize(getmComp,clock=clock,               &
                                   importState=importState,            &
                                   exportState=exportState,            &
                                   phase=phase)
    end do

    if (clockIsPresent) then
       call ESMF_ClockGet(getmClock,timeStep=timeInterval)
       call ESMF_ClockSet(myClock,timeStep=timeInterval)
    else
      call ESMF_GridCompGet(getmComp,clock=getmClock)
      myClock = ESMF_ClockCreate(getmClock)
      call ESMF_ClockSet(myClock,name=trim(name)//"Clock")
      call ESMF_GridCompSet(gridComp,clock=myClock)
    end if

    controlClock = ESMF_ClockCreate(myClock)

    !call ESMF_GridCompGet(getmComp,grid=grid)
    !call ESMF_GridGet(grid,distgrid=distgrid)
    !getmGrid3D = ESMF_GridCreate(grid,distgrid,name=trim(name)//"Grid3D")
    call ESMF_GridCompGet(getmComp,grid=getmGrid3D)

    call ESMF_StateGet(exportState,"gridSetField2D",field)
    !call ESMF_FieldGet(field,grid=grid)
    !call ESMF_GridGet(grid,distgrid=distgrid)
    !getmGrid2D = ESMF_GridCreate(grid,distgrid,name=trim(name)//"Grid2D")
    call ESMF_FieldGet(field,grid=getmGrid2D)

    call getmCmp_init_variables()

    if (associated(depth)) then
      call getmCmp_StateAddPtr("water_depth_at_soil_surface",depth,exportState,"m",name)
    end if
    if (associated(h3D)) then
      call getmCmp_StateAddPtr("layer_height_in_water",h3D,exportState,"m",name)
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
    if (associated(SS3D)) then
      call getmCmp_StateAddPtr("shear_frequency_squared_in_water",SS3D,exportState,"s-2",name,StaggerLoc=ESMF_STAGGERLOC_CENTER_VFACE)
    end if
    if (associated(NN3D)) then
      call getmCmp_StateAddPtr("buoyancy_frequency_squared_in_water",NN3D,exportState,"s-2",name,StaggerLoc=ESMF_STAGGERLOC_CENTER_VFACE)
    end if
    if (associated(num3D)) then
      call getmCmp_StateAddPtr("turbulent_diffusivity_of_momentum_in_water",num3D,exportState,"m2 s-1",name,StaggerLoc=ESMF_STAGGERLOC_CENTER_VFACE)
    end if
    if (associated(numbot)) then
      call getmCmp_StateAddPtr("turbulent_diffusivity_of_momentum_at_soil_surface",numbot,exportState,"m2 s-1",name)
    end if
    if (associated(nuh3D)) then
      call getmCmp_StateAddPtr("turbulent_diffusivity_of_heat_in_water",nuh3D,exportState,"m2 s-1",name,StaggerLoc=ESMF_STAGGERLOC_CENTER_VFACE)
    end if
    if (associated(tke3D)) then
      call getmCmp_StateAddPtr("turbulent_kinetic_energy_in_water",tke3D,exportState,"m2 s-2",name,StaggerLoc=ESMF_STAGGERLOC_CENTER_VFACE)
    end if
    if (associated(tkebot)) then
      call getmCmp_StateAddPtr("turbulent_kinetic_energy_at_soil_surface",tkebot,exportState,"m2 s-2",name)
    end if
    if (associated(eps3D)) then
      call getmCmp_StateAddPtr("dissipation_of_tke_in_water",eps3D,exportState,"m2 s-3",name,StaggerLoc=ESMF_STAGGERLOC_CENTER_VFACE)
    end if
    if (associated(epsbot)) then
      call getmCmp_StateAddPtr("dissipation_of_tke_at_soil_surface",epsbot,exportState,"m2 s-3",name)
    end if

    select case (met_method)
      case(METEO_CONST,METEO_FROMFILE)
        if (associated(windU)) then
          call getmCmp_StateAddPtr("wind_x_velocity_at_10m",windU,exportState,"m s-1",name)
        end if
        if (associated(windV)) then
          call getmCmp_StateAddPtr("wind_y_velocity_at_10m",windV,exportState,"m s-1",name)
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
    end select
    if (associated(taubmax)) then
      call getmCmp_StateAddPtr("maximum_bottom_stress",taubmax,exportState,"Pa",name)
    end if

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

    call MOSSCO_CompExit(gridComp)

  end subroutine InitializeP1

!-----------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP2"
  subroutine InitializeP2(gridComp,importState,exportState,clock,rc)

      use domain, only: imin,imax,jmin,jmax,kmax
#ifndef NO_3D
      use variables_3d, only: dt
#endif
      implicit none

      type(ESMF_GridComp) :: gridComp
      type(ESMF_State)    :: importState,exportState ! may be uninitialized
      type(ESMF_Clock)    :: clock        ! may be uninitialized
      integer,intent(out) :: rc

      type(ESMF_Clock)                                    :: myClock
      type(ESMF_FieldBundle)                              :: concFieldBundle,wsFieldBundle
      type(ESMF_Field)          ,dimension(:),allocatable :: concFieldList,fieldList
      type(ESMF_Field)                                    :: wsField
      type(ESMF_FieldStatus_Flag)                         :: status
      type(ESMF_TimeInterval)                             :: timeInterval
      character(len=ESMF_MAXSTR),dimension(:),allocatable :: itemNameList
      character(len=ESMF_MAXSTR)                          :: itemName
      integer                   ,dimension(:),allocatable :: namelenList,concFlags
      integer                                             :: concFieldCount,transportFieldCount,FieldCount
      integer(ESMF_KIND_I8)                               :: conc_id,ws_id
      integer                                             :: i,ii,n
      integer :: phase,phaseCount
      character(len=*),parameter :: ws_suffix="_z_velocity_in_water"
      character(len=*),parameter :: conc_suffix="_in_water"
    integer(ESMF_KIND_I4) :: localrc

      rc=ESMF_SUCCESS

      call MOSSCO_CompEntry(gridComp, clock)

      call ESMF_GridCompGetEPPhaseCount(getmComp,ESMF_METHOD_INITIALIZE,  &
                                        phaseCount)
      do phase=2,phaseCount
         call ESMF_GridCompInitialize(getmComp,clock=clock,            &
                                      importState=importState,         &
                                      exportState=exportState,phase=phase)
      end do

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

#ifndef NO_3D
            call ESMF_TimeIntervalSet(timeInterval,s_r8=real(dt,kind=ESMF_KIND_R8))
            call ESMF_GridCompGet(gridComp,clock=myClock)
            call ESMF_ClockSet(myClock,timeStep=timeInterval)
#endif

            allocate(transport_conc(transportFieldCount))
            allocate(transport_ws  (transportFieldCount))

            call ESMF_StateGet(importState, "concentrations_z_velocity_in_water", wsFieldBundle, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

            n = 1

            do i=1,concFieldCount

               if (concFlags(i) .eq. 0) cycle

!              assign fieldname (to easy re-access the field later)
               transport_conc(n)%fieldname = trim(itemNameList(i))

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

               !> get information about boundary condition
               call ESMF_AttributeGet(concFieldList(i), 'has_boundary_data', value=transport_conc(n)%has_boundary_data, defaultValue=.false., rc=localrc)
               if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

               call ESMF_AttributeGet(concFieldList(i), 'hackmax', value=transport_conc(n)%hackmax, defaultValue=-1.d0, rc=localrc)
               if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

               call ESMF_AttributeGet(concFieldList(i), 'hackmaxmin', value=transport_conc(n)%hackmaxmin, defaultValue=-1.d0, rc=localrc)
               if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
                  call ESMF_AttributeGet(concFieldList(i), 'external_index', &
                    value=conc_id, defaultValue=int(-1,ESMF_KIND_I8), rc=localrc)
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
                  allocate(fieldList(fieldCount))
                  call ESMF_FieldBundleGet(wsFieldBundle, itemName, fieldList=fieldList, rc=localrc)
                  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
                  do ii=1,fieldCount
                     call ESMF_AttributeGet(fieldList(ii), 'external_index', &
                       value=ws_id, defaultValue=int(-2,ESMF_KIND_I8), rc=localrc)
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

    call MOSSCO_CompExit(gridComp)
    rc = ESMF_SUCCESS

   end subroutine InitializeP2

#undef  ESMF_METHOD
#define ESMF_METHOD "update_use_boundary_data"
   subroutine update_use_boundary_data(importState, advanceCount, rc)

    type(ESMF_State), intent(in)                  :: importState
    integer(ESMF_KIND_I8), intent(in), optional   :: advanceCount
    integer(ESMF_KIND_I4), intent(out), optional  :: rc

    type(ESMF_FieldBundle)        :: fieldBundle
    type(ESMF_Field), allocatable :: fieldList(:)
    integer                       :: localrc, i, j, rc_, fieldCount

    rc_ = ESMF_SUCCESS

    if (.not.allocated(transport_conc)) then
      if (present(rc)) rc = rc_
      return
    endif

    call ESMF_StateGet(importState, "concentrations_in_water", fieldBundle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1,size(transport_conc)
      call ESMF_FieldBundleGet(fieldBundle, trim(transport_conc(i)%fieldname), &
        fieldCount=fieldCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldCount < 1 ) cycle

      if (.not.allocated(fieldList)) allocate(fieldList(fieldCount), stat=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldBundleGet(fieldBundle, trim(transport_conc(i)%fieldname), &
        fieldList=fieldList,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      do j = 1, fieldCount
        call ESMF_AttributeGet(fieldList(j), 'has_boundary_data', &
          value=transport_conc(i)%has_boundary_data, defaultValue=.false., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (transport_conc(i)%has_boundary_data) then
          if ((present(advanceCount) .and. advanceCount == 0) &
            .or. (.not.present(advanceCount))) then
            call ESMF_LogWrite('  use boundary conditions for '//trim(transport_conc(i)%fieldname), ESMF_LOGMSG_INFO)
          endif
        end if
      enddo

      if (allocated(fieldList)) deallocate(fieldList)
    end do

   if (present(rc)) rc = rc_

   end subroutine update_use_boundary_data

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
    use integration,only: time_step
    use m3d, only: M

    implicit none

    type(ESMF_GridComp) :: gridComp
    type(ESMF_State)    :: importState,exportState ! may be uninitialized
    type(ESMF_Clock)    :: clock        ! may be uninitialized
    integer,intent(out) :: rc

    type(ESMF_Clock)        :: myClock
    type(ESMF_Time)         :: currTime, stopTime
    type(ESMF_TimeInterval) :: timeInterval,timeStep
    integer(ESMF_KIND_I8)   :: advanceCount
    type(ESMF_Time)         :: nextTime
    integer(ESMF_KIND_I4)   :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, clock)

    call getmCmp_update_importState()

    call ESMF_GridCompGet(gridComp, clock=myClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(myClock,currTime=currTime, advanceCount=advanceCount, &
      timeStep=timeStep, stopTime=stopTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !  use clock to do determine time of calling routine
    call ESMF_ClockGetNextTime(clock,nextTime,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not. allocated(transport_conc)) then
      timeInterval = nextTime - currTime
      call ESMF_ClockSet(controlClock,timeStep=timeInterval)
      call ESMF_GridCompRun(getmComp,clock=controlClock,               &
                            importState=importState,                   &
                            exportState=exportState,                   &
                            userRc=localrc)
      call ESMF_ClockAdvance(controlClock)
      if (localrc .eq. ESMF_RC_NOT_SET) return
    end if

    do while (currTime < nextTime )
    !do while (currTime + 0.5d0*timeStep < nextTime )

      ! MOSSCO's toplevel time stepping mixes up with myClock%stopTime
      !if (ESMF_ClockIsStopTime(myClock)) then
      !  call ESMF_LogWrite('already exceeded stopTime',ESMF_LOGMSG_ERROR, &
      !                      line=__LINE__,file=__FILE__,method='Run()')
      !  call ESMF_Finalize(endflag=ESMF_END_ABORT)
      !end if

!     optional Run of child components
      if (allocated(transport_conc)) then
        call ESMF_GridCompRun(getmComp,clock=myClock,                  &
                              importState=importState,                 &
                              exportState=exportState,                 &
                              userRc=localrc)
        if (localrc .eq. ESMF_RC_NOT_SET) exit
!       Update information about boundary conditions
        call update_use_boundary_data(importState, advanceCount=advanceCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!       Call transport routine every macro timestep
        call getmCmp_transport(currTime)
      end if

      call ESMF_ClockAdvance(myClock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_ClockGet(myClock,currtime=currTime,advanceCount=advanceCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    end do

    call getmCmp_update_exportState()

    call MOSSCO_CompExit(gridComp)

  end subroutine Run

!-----------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(gridComp, importState, exportState, clock, rc)

    use initialise ,only: runtype,dryrun
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Clock)      :: myClock
    integer(ESMF_KIND_I4) :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, clock)

    call ESMF_GridCompFinalize(getmComp,clock=clock,importState=importState, &
                               exportState=exportState)

    !call ESMF_GridCompDestroy(getmComp)

    call ESMF_ClockDestroy(controlClock)

    call ESMF_GridCompGet(gridComp,clock=myClock)
    call ESMF_ClockDestroy(myClock)

    call MOSSCO_CompExit(gridComp)
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
   use domain         ,only: grid_type
   use initialise     ,only: runtype
   use variables_2d   ,only: D
#ifndef NO_3D
   use variables_3d   ,only: hn,SS,num,nuh,tke,eps
#ifndef NO_BAROCLINIC
   use m3d            ,only: calc_temp,calc_salt
   use variables_3d   ,only: T,S,NN
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
   integer  :: klen
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
      allocate(depth(E2DFIELD))
      if (runtype .eq. 1) then
      else
#ifndef NO_3D
         allocate(h3D   (I3DFIELD))
         allocate(SS3D  (I3DFIELD))
         allocate(num3D (I3DFIELD))
         allocate(nuh3D (I3DFIELD))
         allocate(tke3D (I3DFIELD))
         allocate(eps3D (I3DFIELD))
#ifndef NO_BAROCLINIC
         allocate(NN3D  (I3DFIELD))
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
         waveH = 0.0d0
         allocate(waveT  (E2DFIELD))
         waveT = 0.0d0
         allocate(waveK  (E2DFIELD))
         waveK = 0.0d0
      end if
   else
      depth => D
      if (runtype .eq. 1) then
      else
#ifndef NO_3D
         h3D   => hn
         nuh3D => nuh
#if 1
!        some turbulent quantities still without target attribute in getm
         allocate(SS3D  (I3DFIELD))
#ifndef NO_BAROCLINIC
         allocate(NN3D  (I3DFIELD))
#endif
         allocate(num3D (I3DFIELD))
         allocate(tke3D (I3DFIELD))
         allocate(eps3D (I3DFIELD))
#else
         SS3D  => SS
#ifndef NO_BAROCLINIC
         NN3D  => NN
#endif
         num3D => num
         tke3D => tke
         eps3D => eps
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

   if (runtype .eq. 1) then
      hbot => depth
   else
      p2d => h3D  (:,:,1) ; hbot  (imin-HALO:,jmin-HALO:) => p2d
      p2d => num3D(:,:,1) ; numbot(imin-HALO:,jmin-HALO:) => p2d
      p2d => tke3D(:,:,1) ; tkebot(imin-HALO:,jmin-HALO:) => p2d
      p2d => eps3D(:,:,1) ; epsbot(imin-HALO:,jmin-HALO:) => p2d
   end if

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

   allocate(taubmax(I2DFIELD))
   taubmax = _ZERO_

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
      waveDir = 0.0d0
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
   use parameters     ,only: rho_0
   use domain         ,only: imin,imax,jmin,jmax,kmax
   use domain         ,only: az
   use domain         ,only: grid_type,xc,xu,xv,yc,yu,yv,convc
   use domain         ,only: cosconv,sinconv
   use domain         ,only: dxv,dyu,arcd1
   use initialise     ,only: runtype
   use variables_2d   ,only: zo,z,D,Dvel,U,DU,V,DV
#ifndef NO_3D
   use variables_3d   ,only: dt,ho,hn,hvel,uu,hun,vv,hvn,ww
   use variables_3d   ,only: SS,num,nuh,tke,eps
   use variables_3d   ,only: taubmax_3d
#ifndef NO_BAROCLINIC
   use m3d            ,only: calc_temp,calc_salt
   use variables_3d   ,only: T,S,NN
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
   REALTYPE, parameter :: pi=3.1415926535897932384626433832795029d0
   REALTYPE, parameter :: deg2rad=pi/180
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
         h3D    = hn
         SS3D   = SS
         num3D  = num
         nuh3D  = nuh
         tke3D  = tke
         eps3D  = eps
#ifndef NO_BAROCLINIC
         NN3D   = NN
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
!     some turbulent quantities still without target attribute in getm
      if (runtype .gt. 1) then
         SS3D   = SS
#ifndef NO_BAROCLINIC
         NN3D   = NN
#endif
         num3D  = num
         tke3D  = tke
         eps3D  = eps
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

   if (runtype .ge. 2) then
      taubmax = rho_0 * taubmax_3d
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
      waveDir = atan2(sinwavedir,coswavedir) - convc*deg2rad
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
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES:
   REALTYPE, parameter :: pi=3.1415926535897932384626433832795029d0
   REALTYPE, parameter :: deg2rad=pi/180
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_update_importState() # ',Ncall
#endif


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
   subroutine getmCmp_transport(currTime)
!
! !DESCRIPTION:
!
! !USES:
   use domain, only: imin,imax,jmin,jmax,kmax

   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
  type(ESMF_Time), intent(in), optional :: currTime

! !INPUT/OUPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
   REALTYPE,dimension(I3DFIELD),target  :: t_conc,t_ws
   REALTYPE,dimension(:,:,:)   ,pointer :: p_conc,p_ws
   integer                              :: n

  integer(ESMF_KIND_I4)      :: doy, localrc, rc
  real(ESMF_KIND_R8)         :: hackmax, y0, amplitude
  real(ESMF_KIND_R8), parameter :: pi=3.1415926535897932384626433832795028841971693993751D0
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

      if (.not.(transport_conc(n)%has_boundary_data)) then

        ! Hack for Kai with seasonally varying maximum value for boundary concentrations
        ! if you don't give currtime, then only an upper maximum is used.
        if (present(currTime)) then
          call ESMF_TimeGet(currTime, dayOfYear=doy, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          amplitude=transport_conc(n)%hackmax - transport_conc(n)%hackmaxmin
          y0=transport_conc(n)%hackmaxmin
          hackmax=y0 + amplitude * 0.5
!          hackmax=y0 + amplitude * cos(pi*doy/365.25)**2 ! reformulate according to need
          call zero_gradient_3d_bdy(p_conc,hackmax)
        else
          call zero_gradient_3d_bdy(p_conc,transport_conc(n)%hackmax)
        endif
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
