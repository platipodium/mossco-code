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

! the following objects point to deep objects and do not need to be
! requested everytime again
! Note (KK): the save attribute can be deleted for F2008 standard
  type(ESMF_Clock)   ,save :: getmClock
  type(ESMF_DistGrid),save :: getmDistGrid2D,getmDistGrid3D
  type(ESMF_Grid)    ,save :: getmGrid2D,getmGrid3D
  type(ESMF_Field)   ,save :: TbotField,T3DField

! The following objects are treated differently, depending on whether
! the kinds of GETM's internal REALTYPE matches ESMF_KIND_R8
  logical                    :: noKindMatch
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
  real(ESMF_KIND_R8),pointer :: Tbot(:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: T3D(:,:,:)=>NULL()

  type :: ptrarray3D
     real(ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr=>NULL()
  end type ptrarray3D
  type(ptrarray3D),dimension(:),allocatable :: transport_ws,transport_conc

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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=2, &
      userRoutine=InitializeP2, rc=localrc)
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
   subroutine InitializeP0(gridComp,iState,eState,iClock,rc)
!
! !DESCRIPTION:
!  Note: [i|e]state and iClock are uninitialized if the toplevel
!        component did not provide corresponding arguments to
!        ESMF_GridCompInitialize(gridComp).
!  The toplevel component can inquire rc via optional keyword argument
!  userRc to ESMF_GridCompInitialize().
!
! !USES:
   use NUOPC
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
   type(ESMF_GridComp) :: gridComp
   type(ESMF_State)    :: iState,eState ! may be uninitialized
   type(ESMF_Clock)    :: iClock        ! may be uninitialized
!
! !OUTPUT PARAMETERS:
   integer,intent(out) :: rc
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES
   character(len=NUOPC_PhaseMapStringLength) :: InitializePhaseMap(2)
   integer                :: localrc
   type(ESMF_Time)        :: currTime
   character(ESMF_MAXSTR) :: name

!
!EOP
!-----------------------------------------------------------------------
!BOC

    call MOSSCO_CompEntry(gridComp, iClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'InitializeP0() # ',Ncall
#endif

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

   call NUOPC_GridCompAttributeAdd(gridComp)
   call ESMF_AttributeSet(gridComp,name="InitializePhaseMap",           &
                                  valueList=InitializePhaseMap,        &
                                  convention="NUOPC",purpose="General",rc=rc)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
  subroutine InitializeP1(gridComp,iState,eState,iClock,rc)

    use time, only : getm_time_start => start, getm_time_stop => stop
    use time, only : getm_time_timestep => timestep
    use initialise,  only: init_model,dryrun
    use integration, only: MinN,MaxN
#ifdef GETM_PARALLEL
    use halo_mpi, only: comm_getm
#endif

    implicit none

    type(ESMF_GridComp) :: gridComp
    type(ESMF_State)    :: iState,eState ! may be uninitialized
    type(ESMF_Clock)    :: iClock        ! may be uninitialized
    integer,intent(out) :: rc

    character(ESMF_MAXSTR):: name, message, timeString, string
    type(ESMF_Clock)      :: clock
    type(ESMF_Time)       :: currTime, startTime, stopTime
    logical               :: vmIsPresent,clockIsPresent
    type(ESMF_TimeInterval) :: timeInterval
    integer(ESMF_KIND_I4) :: localPet, petCount
    type(ESMF_VM)         :: vm
    real(ESMF_KIND_R8)    :: h_r8
    integer               :: comm

    type(ESMF_Time)         :: getmRefTime,getmStartTime,getmStopTime
    integer                 :: getmRunTimeStepCount
    character(len=8)        :: datestr
    character(len=10)       :: timestr
    character(len=19)       :: TimeStrISOFrac,start_external,stop_external
    integer               :: localrc

    call MOSSCO_CompEntry(gridComp, iClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, vmIsPresent=vmIsPresent, clockIsPresent=clockIsPresent, &
                                    rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (.not.vmIsPresent) then
      call ESMF_LogWrite('no VM present',ESMF_LOGMSG_ERROR, &
                         line=__LINE__,file=__FILE__,method='getmCmp_init()')
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end if

#ifdef GETM_PARALLEL
    call ESMF_GridCompGet(gridComp, vm=vm, rc=rc)
    call ESMF_VMGet(vm,mpiCommunicator=comm)
    call MPI_COMM_DUP(comm,comm_getm,rc)
#endif

    call date_and_time(datestr,timestr)
    if (clockIsPresent) then
      ! Use startTime and stopTime from already initialised clock.
      call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
      call ESMF_ClockGet(clock, startTime=startTime, stopTime=stopTime, &
        timeStep=timeInterval, rc=rc)
      call ESMF_TimeGet(startTime,timeStringISOFrac=start_external)
      call ESMF_TimeGet(stopTime,timeStringISOFrac=stop_external)

      call preinit_model(datestr,timestr)
      call init_time(MinN,MaxN,start_external=start_external, &
                     stop_external=stop_external)
      call postinit_model()
    else
      ! set up clock based on internal GETM specifications
      ! I don' think we ever arrive here ..

      call init_model(datestr,timestr)
      TimeStrISOFrac=getm_time_start(1:10)//"T"//getm_time_start(12:19)
      call TimeStringISOFrac2ESMFtime(TimeStrISOFrac,getmRefTime)
      call ESMF_TimeIntervalSet(timeInterval,s_r8=getm_time_timestep)

      getmStartTime = getmRefTime + (MinN-1)*timeInterval
      getmStopTime  = getmRefTime + MaxN*timeInterval
      getmRunTimeStepCount = MaxN - MinN + 1

      clock = ESMF_ClockCreate(timeInterval,getmStartTime,            &
                                   runTimeStepCount=getmRunTimeStepCount, &
                                   refTime=getmRefTime,                   &
                                   name=trim(name)//' clock', rc=rc)
      call ESMF_GridCompSet(gridComp,clock=clock)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif

    call getmCmp_init_variables()
    call getmCmp_init_grid(gridComp)

    if (associated(Tbot)) then
!     internal call to ESMF_FieldCreateGridData<rank><type><kind>()
!     forced by indexflag argument.
!     KK-TODO: ESMF_FieldCreateGridDataPtr<rank><type><kind>() fails
!              (maybe only in case of non-1-based indices?)
!     in contrast to ESMF_ArrayCreate() no automatic determination of total[L|U]Width
      TbotField = ESMF_FieldCreate(getmGrid2D,Tbot,indexflag=ESMF_INDEX_DELOCAL,totalLWidth=(/HALO,HALO/),totalUWidth=(/HALO,HALO/),name="temperature_at_soil_surface",rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateAdd(eState,(/TbotField/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    end if
    if (associated(T3D)) then
#ifdef FOREIGN_GRID
      T3DField = ESMF_FieldCreate(getmGrid3D,T3D,indexflag=ESMF_INDEX_DELOCAL,totalLWidth=(/HALO,HALO,1/),totalUWidth=(/HALO,HALO,0/),name="temperature_in_water",rc=rc)
#else
      T3DField = ESMF_FieldCreate(getmGrid3D,T3D,name="temperature_in_water",rc=rc)
#endif
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateAdd(eState,(/T3DField/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    end if

    call getmCmp_update_eState()

    if (.not.dryrun) then
      STDERR LINE
      LEVEL1 'integrating....'
      STDERR LINE
    end if

    ! Set the internal time step
    call ESMF_TimeIntervalSet(timeInterval,s_r8=getm_time_timestep)
    call ESMF_ClockSet(clock,timeStep=timeInterval)

    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockSet(clock, name=trim(name)//'_clock', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Log the call to this function
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initializing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    !! Log processor information
    call ESMF_VmGet(vm, petCount=petCount, rc=rc)
    write(message,'(A,I6,A)') trim(timestring)//' '//trim(name)//' uses ',petCount
    call ESMF_VmGetGlobal(vm=vm, rc=rc)
    call ESMF_VmGet(vm, petCount=petCount, rc=rc)
    write(message,'(A,I6,A)') trim(message)//' of ', petCount,' PETs'

    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    !! Log clock information
    call ESMF_ClockGet(clock, startTime=startTime, rc=rc)
    call ESMF_TimeGet(startTime,timeStringISOFrac=string)
    write(message,'(A)') trim(timeString)//' '//trim(string)
    call ESMF_ClockGet(clock, timeStep=timeInterval, rc=rc)
    call ESMF_TimeIntervalGet(timeInterval,h_r8=h_r8)
    write(message,'(A,F8.2,A)') trim(message)//'--',h_r8,' h'
    call ESMF_ClockGet(clock, stopTime=stopTime, rc=rc)
    call ESMF_TimeGet(stopTime,timeStringISOFrac=string)
    write(message,'(A)') trim(message)//'--'//trim(string)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1

!-----------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP2"
   subroutine InitializeP2(gridComp,iState,eState,iClock,rc)

      use domain, only: imin,imax,jmin,jmax,kmax
      implicit none

      type(ESMF_GridComp) :: gridComp
      type(ESMF_State)    :: iState,eState ! may be uninitialized
      type(ESMF_Clock)    :: iClock        ! may be uninitialized
      integer,intent(out) :: rc

      type(ESMF_StateItem_Flag) ,dimension(:),allocatable :: itemTypeList
      type(ESMF_FieldBundle)    ,dimension(:),allocatable :: fieldBundleList
      type(ESMF_FieldBundle)                              :: fieldBundle
      type(ESMF_Field)          ,dimension(:),allocatable :: fieldList_ws,fieldList_conc
      type(ESMF_FieldStatus_Flag)                         :: status
      character(len=ESMF_MAXSTR),dimension(:),allocatable :: itemNameList
      integer                   ,dimension(:),allocatable :: transportFieldCountList,namelenList
      integer                                             :: transportFieldCount,itemCount
      integer                                             :: i,ii,n
      integer                    :: localrc
      type(ESMF_Time)            :: currTime
      character(len=ESMF_MAXSTR) :: name

      call MOSSCO_CompEntry(gridComp, iClock, name, currTime, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateGet(iState,itemCount=itemCount)


      if (itemCount .gt. 0) then

         allocate(itemTypeList           (itemCount))
         allocate(itemNameList           (itemCount))
         allocate(namelenList            (itemCount))
         allocate(fieldBundleList        (itemCount))
         allocate(transportFieldCountList(itemCount))
         transportFieldCountList = 0

         call ESMF_StateGet(iState,itemNameList=itemNameList, &
                                   itemTypeList=itemTypeList)

         do i=1,itemCount
!           identify items to be transported by suffix "_z_velocity"
            namelenList(i) = len_trim(itemNameList(i))
            if (itemNameList(i)(namelenList(i)-10:namelenList(i)) .ne. '_z_velocity') cycle
            if (itemTypeList(i) .eq. ESMF_STATEITEM_FIELD) then
               transportFieldCountList(i) = 1
            else if (itemTypeList(i) .eq. ESMF_STATEITEM_FIELDBUNDLE) then
               call ESMF_StateGet(iState,itemNameList(i),fieldBundleList(i))
               call ESMF_FieldBundleGet(fieldBundleList(i),fieldCount=transportFieldCountList(i))
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
                  call ESMF_StateGet(iState,itemNameList(i),fieldList_ws(n))
                  call ESMF_StateGet(iState,itemNameList(i)(:namelenList(i)-11),fieldList_conc(n))
                  n = n + 1
               else if (itemTypeList(i) .eq. ESMF_STATEITEM_FIELDBUNDLE) then
                  call ESMF_StateGet(iState,itemNameList(i)(:namelenList(i)-11),fieldBundle)
                  do ii=1,transportFieldCountList(i)
                     call ESMF_FieldBundleGet(fieldBundleList(i),ii,fieldList_ws(n))
                     call ESMF_FieldBundleGet(fieldBundle,ii,fieldList_conc(n))
                     n = n + 1
                  end do
               end if
            end do

            allocate(transport_ws  (transportFieldCount))
            allocate(transport_conc(transportFieldCount))


            do n=1,transportFieldCount

               call ESMF_FieldGet(fieldList_ws(n),status=status)

               if (status.eq.ESMF_FIELDSTATUS_EMPTY .or. status.eq.ESMF_FIELDSTATUS_GRIDSET) then
!                 Either coupler called ESMF_FieldEmptyCreate(name),
!                 because fabm_pelagic ships with its own grid (coupler
!                 checks whether temperature field in fabm_pelagic's
!                 iState is already completed). Or coupler copied empty
!                 field, because fabm_pelagic was created with getmGrid.
!                 In the latter case the state variables are allocated
!                 only here (and the exclusiveDomain still needs to be
!                 passed to FABM!) in order to include the total domain.
!                 PROBLEM: exclusiveDomain is not contiguous and cannot
!                          be provided to FABM!!!
                  allocate(transport_ws(n)%ptr(I3DFIELD))
                  call ESMF_FieldEmptyComplete(fieldList_ws(n),getmGrid3D,              &
                                               transport_ws(n)%ptr,                     &
                                               ESMF_INDEX_DELOCAL,                      &
                                               staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, &
                                               totalLWidth=(/HALO,HALO,1/),             &
                                               totalUWidth=(/HALO,HALO,0/))
               else if (status .eq. ESMF_FIELDSTATUS_COMPLETE) then
!                 Coupler copied completed fields from fabm_pelagic,
!                 because "foreignGridField" was provided to fabm_pelagic
!                 (coupler checks whether temperature field in fabm_pelagic's
!                  iState is empty).
!                 The field MUST include the HALO zones and k=0 !!!
                  call ESMF_FieldGet(fieldList_ws(n),farrayPtr=transport_ws(n)%ptr)
               end if

               call ESMF_FieldGet(fieldList_conc(n),status=status)

               if (status.eq.ESMF_FIELDSTATUS_EMPTY .or. status.eq.ESMF_FIELDSTATUS_GRIDSET) then
                  allocate(transport_conc(n)%ptr(I3DFIELD))
                  call ESMF_FieldEmptyComplete(fieldList_conc(n),getmGrid3D,      &
                                               transport_conc(n)%ptr,             &
                                               ESMF_INDEX_DELOCAL,                &
                                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                                               totalLWidth=(/HALO,HALO,1/),       &
                                               totalUWidth=(/HALO,HALO,0/))
               else if (status .eq. ESMF_FIELDSTATUS_COMPLETE) then
                  call ESMF_FieldGet(fieldList_conc(n),farrayPtr=transport_conc(n)%ptr)
               end if

            end do

         end if

      end if

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   end subroutine InitializeP2

!-----------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(gridComp,iState,eState,iClock,rc)

    use initialise ,only: runtype,dryrun
    use integration,only: MinN

    implicit none

    type(ESMF_GridComp) :: gridComp
    type(ESMF_State)    :: iState,eState ! may be uninitialized
    type(ESMF_Clock)    :: iClock        ! may be uninitialized
    integer,intent(out) :: rc

    character(ESMF_MAXSTR):: name, message, timeString
    type(ESMF_Clock)      :: clock
    type(ESMF_Time)       :: currTime, stopTime
    logical               :: clockIsPresent
    type(ESMF_TimeInterval) :: timeInterval

    integer(ESMF_KIND_I4) :: petCount, localPet, rank
    integer(ESMF_KIND_I8) :: advanceCount
    real(ESMF_KIND_R8)    :: h_r8


    type(ESMF_Time)         :: nextTime
    integer                 :: localrc
    integer                 :: n

    call MOSSCO_CompEntry(gridComp, iClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_ClockGet(clock,currTime=currTime, advanceCount=advanceCount, &
      timeStep=timeInterval, stopTime=stopTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


    !  use iClock to do determine time of calling routine
    call ESMF_ClockGetNextTime(iClock,nextTime,rc=localrc)
    if (localrc .ne. ESMF_SUCCESS) then
      call ESMF_LogWrite('will continue until own stopTime',ESMF_LOGMSG_WARNING, &
       line=__LINE__,file=__FILE__,method='Run()')
      call ESMF_ClockGet(clock,stopTime=NextTime)
    end if


    do while (currTime + 0.5d0*timeInterval <= nextTime)

      if (ESMF_ClockIsStopTime(clock)) then
        call ESMF_LogWrite('already exceeded stopTime',ESMF_LOGMSG_ERROR, &
                            line=__LINE__,file=__FILE__,method='Run()')
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if

!     This is where the model specific computation goes.
      if (.not.dryrun) then
        n = int(advanceCount,kind=kind(MinN))+MinN
        call time_step(runtype,n)
      end if

      call getmCmp_transport()

      call ESMF_ClockAdvance(clock)
      call ESMF_ClockGet(clock,currtime=currTime,advanceCount=advanceCount)
    end do

    call getmCmp_update_grid(gridComp)
    call getmCmp_update_eState()

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

!-----------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(gridComp, iState, eState, iClock, rc)

    use initialise ,only: runtype,dryrun
    use integration,only: MaxN
    use output     ,only: meanout

    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: iState, eState
    type(ESMF_Clock)     :: iClock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR):: name, message, timeString
    type(ESMF_Grid)       :: getmGrid
    type(ESMF_Clock)      :: clock
    type(ESMF_Time)       :: currTime
    logical               :: ClockIsPresent,GridIsPresent
    integer               :: localrc

    call MOSSCO_CompEntry(gridComp, iClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#ifndef NO_3D
    if (meanout .eq. 0) then
      call calc_mean_fields(MaxN,MaxN)
    end if
#endif
    call clean_up(dryrun,runtype,MaxN)

    
    if (GridIsPresent) then
      call ESMF_GridCompGet(gridComp,grid=getmGrid)
      !call ESMF_GridGet(getmGrid,distgrid=getmDistGrid)
      !call ESMF_GridGetCoord(getmGrid,coordDim=...,staggerloc=...,array=array)
      !call ESMF_ArrayDestroy(array)
      call ESMF_DistGridDestroy(getmDistGrid2D)
      call ESMF_DistGridDestroy(getmDistGrid3D)
      call ESMF_GridDestroy(getmGrid)
    end if

    call ESMF_GridCompGet(gridComp,clock=clock,rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
	  call ESMF_ClockDestroy(clock)    
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
   use domain      ,only: imin,jmin,imax,jmax,kmax
   use domain      ,only: xcord,ycord,xx,yx,lonx,latx
   use domain      ,only: xxcord,yxcord,xc,yc,lonc,latc
   use domain      ,only: grid_type
   use initialise  ,only: runtype
#ifndef NO_BAROCLINIC
   use variables_3d,only: T
#endif
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
   REALTYPE :: getmreal
   integer  :: klen
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
      if (runtype .gt. 2) then
#ifndef NO_BAROCLINIC
         allocate(Tbot(I2DFIELD))
#ifdef FOREIGN_GRID
         allocate(T3D(I3DFIELD))
#else
         allocate(T3D(imin:imax,jmin:jmax,1:kmax))
#endif
#endif
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
       if (runtype .gt. 2) then
#ifndef NO_BAROCLINIC
          Tbot(imin-HALO:,jmin-HALO:) => T(:,:,1)
#ifdef FOREIGN_GRID
          T3D=>T
#else
          T3D => T(imin:imax,jmin:jmax,1:kmax)
#endif
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

   if (runtype .eq. 1) then
      klen = 1
   else
      klen = kmax
   end if

   allocate(zw(E2DFIELD ,0:klen))
   allocate(zc(E2DFIELD ,1:klen))
   allocate(zx(E2DXFIELD,0:klen))

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
   IMPLICIT NONE
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
   type(ESMF_Array)         :: zwArray,zcArray,zxArray
!  Note (KK): ESMF_ARRAY's are deep classes, that persist after return.
!             (even without save attribute).
   integer(ESMF_KIND_I4),dimension(:),allocatable,target :: alledges
   integer(ESMF_KIND_I4),dimension(4),target             :: myedges
   integer                  :: getmPetCount
   integer                  :: pet,i0,j0,ilen,jlen,klen
   integer,dimension(3)     :: coordDimCount
   integer,dimension(3,3)   :: coordDimMap
   integer,dimension(:,:,:),allocatable                  :: deBlockList
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_init_grid() # ',Ncall
#endif

   call ESMF_GridCompGet(gridComp,vm=getmVM,petCount=getmPetCount)

   myedges = (/ ioff , joff , imax , jmax /)
   allocate(alledges(4*getmPetCount))
!  syncflag=ESMF_SYNC_BLOCKING (default)
   call ESMF_VMAllGather(getmVM,myedges,alledges,4)

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
                                        int(deBlockList(1:2,:,:)))
   getmDistGrid3D = ESMF_DistGridCreate(minval(deBlockList(:,1,:),2), &
                                        maxval(deBlockList(:,2,:),2), &
                                        deBlockList)
#else
!  Multi-tile DistGrid (1 subdomain = 1 tile = 1 DE) by specification of
!  [min|max]IndexPTile.
!  Note (KK): int() intrinsic routines are needed, because ESMF does not
!             accept subarrays as arguments
!  internal call to ESMF_DistGridCreateRDT()
   getmDistGrid2D = ESMF_DistGridCreate(int(deBlockList(1:2,1,:)), &
                                        int(deBlockList(1:2,2,:)))
   getmDistGrid3D = ESMF_DistGridCreate(int(deBlockList(:,1,:)), &
                                        int(deBlockList(:,2,:)))
#endif

   select case (grid_type)
      case(1)
         coordSys = ESMF_COORDSYS_CART
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
         xcArray2D = ESMF_ArrayCreate(getmDistGrid2D,xc1D,indexflag=ESMF_INDEX_DELOCAL)
         ycArray2D = ESMF_ArrayCreate(getmDistGrid2D,yc1D,indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1/))
         xcArray3D = ESMF_ArrayCreate(getmDistGrid3D,xc1D,indexflag=ESMF_INDEX_DELOCAL)
         ycArray3D = ESMF_ArrayCreate(getmDistGrid3D,yc1D,indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1,0/))
         xxArray2D = ESMF_ArrayCreate(getmDistGrid2D,xx1D,          &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/))
         yxArray2D = ESMF_ArrayCreate(getmDistGrid2D,yx1D,          &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1/),   &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/))
         xxArray3D = ESMF_ArrayCreate(getmDistGrid3D,xx1D,          &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/))
         yxArray3D = ESMF_ArrayCreate(getmDistGrid3D,yx1D,          &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1,0/), &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/))
      case(2)
         coordSys = ESMF_COORDSYS_SPH_DEG    ! (default)
         coordDimCount = (/ 1 , 1 , 3 /)     ! rectilinear horizontal coordinates
         coordDimMap = reshape( (/1,2,1,0,0,2,0,0,3/) , (/3,3/) )
         xcArray2D = ESMF_ArrayCreate(getmDistGrid2D,lonc1D,indexflag=ESMF_INDEX_DELOCAL)
         ycArray2D = ESMF_ArrayCreate(getmDistGrid2D,latc1D,indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1/))
         xcArray3D = ESMF_ArrayCreate(getmDistGrid3D,lonc1D,indexflag=ESMF_INDEX_DELOCAL)
         ycArray3D = ESMF_ArrayCreate(getmDistGrid3D,latc1D,indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1,0/))
         xxArray2D = ESMF_ArrayCreate(getmDistGrid2D,lonx1D,        &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/))
         yxArray2D = ESMF_ArrayCreate(getmDistGrid2D,latx1D,        &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1/),   &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/))
         xxArray3D = ESMF_ArrayCreate(getmDistGrid3D,lonx1D,        &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/))
         yxArray3D = ESMF_ArrayCreate(getmDistGrid3D,latx1D,        &
                                      indexflag=ESMF_INDEX_DELOCAL, &
                                      distgridToArrayMap=(/0,1,0/), &
                                      totalLWidth=(/HALO+1/),       &
                                      totalUWidth=(/HALO/))
      case(3)
         coordSys = ESMF_COORDSYS_CART
         coordDimCount = (/ 2 , 2 , 3 /)
         coordDimMap = reshape( (/1,1,1,2,2,2,0,0,3/) , (/3,3/) ) ! (default)
!        Note (KK): automatically determined total[L|U]Width are not consistent
!                   with gridAlign specified later
         xxArray2D = ESMF_ArrayCreate(getmDistGrid2D,xx2D,           &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/))
         yxArray2D = ESMF_ArrayCreate(getmDistGrid2D,yx2D,           &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/))
         xxArray3D = ESMF_ArrayCreate(getmDistGrid3D,xx2D,           &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/))
         yxArray3D = ESMF_ArrayCreate(getmDistGrid3D,yx2D,           &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/))
         xcArray2D = ESMF_ArrayCreate(getmDistGrid2D,xc2D,indexflag=ESMF_INDEX_DELOCAL)
         ycArray2D = ESMF_ArrayCreate(getmDistGrid2D,yc2D,indexflag=ESMF_INDEX_DELOCAL)
         xcArray3D = ESMF_ArrayCreate(getmDistGrid3D,xc2D,indexflag=ESMF_INDEX_DELOCAL)
         ycArray3D = ESMF_ArrayCreate(getmDistGrid3D,yc2D,indexflag=ESMF_INDEX_DELOCAL)
      case(4)
         coordSys = ESMF_COORDSYS_SPH_DEG                         ! (default)
         coordDimCount = (/ 2 , 2 , 3 /)
         coordDimMap = reshape( (/1,1,1,2,2,2,0,0,3/) , (/3,3/) ) ! (default)
         xxArray2D = ESMF_ArrayCreate(getmDistGrid2D,lonx2D,         &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/))
         yxArray2D = ESMF_ArrayCreate(getmDistGrid2D,latx2D,         &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/))
         xxArray3D = ESMF_ArrayCreate(getmDistGrid3D,lonx2D,         &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/))
         yxArray3D = ESMF_ArrayCreate(getmDistGrid3D,latx2D,         &
                                      indexflag=ESMF_INDEX_DELOCAL,  &
                                      totalLWidth=(/HALO+1,HALO+1/), &
                                      totalUWidth=(/HALO,HALO/))
         xcArray2D = ESMF_ArrayCreate(getmDistGrid2D,lonc2D,indexflag=ESMF_INDEX_DELOCAL)
         ycArray2D = ESMF_ArrayCreate(getmDistGrid2D,latc2D,indexflag=ESMF_INDEX_DELOCAL)
         xcArray3D = ESMF_ArrayCreate(getmDistGrid3D,lonc2D,indexflag=ESMF_INDEX_DELOCAL)
         ycArray3D = ESMF_ArrayCreate(getmDistGrid3D,latc2D,indexflag=ESMF_INDEX_DELOCAL)
   end select

   zwArray = ESMF_ArrayCreate(getmDistGrid3D,zw,            &
                              indexflag=ESMF_INDEX_DELOCAL, &
                              totalLWidth=(/HALO,HALO,1/),  &
                              totalUWidth=(/HALO,HALO,0/))
   zcArray = ESMF_ArrayCreate(getmDistGrid3D,zc,            &
                              indexflag=ESMF_INDEX_DELOCAL)
   zxArray = ESMF_ArrayCreate(getmDistGrid3D,zx,               &
                              indexflag=ESMF_INDEX_DELOCAL,    &
                              totalLWidth=(/HALO+1,HALO+1,1/), &
                              totalUWidth=(/HALO,HALO,0/))

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
                                coordDimMap=int(coordDimMap(1:2,1:2)))
   getmGrid3D = ESMF_GridCreate(getmDistGrid3D,name="getmGrid3D", &
                                gridAlign=(/1,1,1/),              &
                                coordSys=coordSys,                &
                                coordDimCount=coordDimCount,      &
                                coordDimMap=coordDimMap)

   StaggerLoc = ESMF_STAGGERLOC_CENTER ! (default)
   call ESMF_GridSetCoord(getmGrid2D,1,array=xcArray2D,staggerloc=StaggerLoc)
   call ESMF_GridSetCoord(getmGrid2D,2,array=ycArray2D,staggerloc=StaggerLoc)
   call ESMF_GridSetCoord(getmGrid3D,1,array=xcArray3D,staggerloc=StaggerLoc)
   call ESMF_GridSetCoord(getmGrid3D,2,array=ycArray3D,staggerloc=StaggerLoc)
   call ESMF_GridSetCoord(getmGrid3D,3,array=zcArray  ,staggerloc=StaggerLoc)

   StaggerLoc = ESMF_STAGGERLOC_CORNER
   call ESMF_GridSetCoord(getmGrid2D,1,array=xxArray2D,staggerloc=StaggerLoc)
   call ESMF_GridSetCoord(getmGrid2D,2,array=yxArray2D,staggerloc=StaggerLoc)
   call ESMF_GridSetCoord(getmGrid3D,1,array=xxArray3D,staggerloc=StaggerLoc)
   call ESMF_GridSetCoord(getmGrid3D,2,array=yxArray3D,staggerloc=StaggerLoc)
   call ESMF_GridSetCoord(getmGrid3D,3,array=zxArray  ,staggerloc=StaggerLoc)

   call ESMF_GridSetCoord(getmGrid3D,3,array=zwArray,staggerloc=ESMF_STAGGERLOC_CENTER_VFACE)

   call ESMF_GridCompSet(gridComp,grid=getmGrid3D)
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
! !ROUTINE: getmCmp_update_eState -
!
! !INTERFACE:
#undef  ESMF_METHOD
#define ESMF_METHOD "getmCmp_update_eState"
   subroutine getmCmp_update_eState()
!
! !DESCRIPTION:
!
! !USES:
   use domain    ,only: imin,imax,jmin,jmax,kmax
   use initialise  , only: runtype
#ifndef NO_BAROCLINIC
   use variables_3d, only: T
#endif
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_update_eState() # ',Ncall
#endif

   if (noKindMatch) then
      if (runtype .gt. 2) then
#ifndef NO_BAROCLINIC
         Tbot = T(:,:,1)
#ifdef FOREIGN_GRID
         T3D = T
#else
         T3D = T(imin:imax,jmin:jmax,1:kmax)
#endif
#endif
      end if
   end if

#ifdef DEBUG
   write(debug,*) 'getmCmp_update_eState()'
   write(debug,*)
#endif
   return

   end subroutine getmCmp_update_eState
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
                      calkindflag=ESMF_CALKIND_GREGORIAN)

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
