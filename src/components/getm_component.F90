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
#include "cppdefs.h"

module getm_component

  use esmf
  use getm_driver

  implicit none
  private

  public SetServices
  private getmCmp_init_variables,getmCmp_finalize_variables
  private getmCmp_init_grid

! The following objects are treated differently, depending on whether
! the kinds of GETM's internal REALTYPE matches ESMF_KIND_R8
  logical                    :: noKindMatch
  real(ESMF_KIND_R8),pointer :: xcord_(:)  =>NULL(),ycord_(:)  =>NULL()
  real(ESMF_KIND_R8),pointer :: xx_   (:,:)=>NULL(),yx_   (:,:)=>NULL()
  real(ESMF_KIND_R8),pointer :: lonx_ (:,:)=>NULL(),latx_ (:,:)=>NULL()

  contains

  subroutine SetServices(gridcomp, rc)

    implicit none

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine SetServices

  subroutine Initialize(gridComp,importState,exportState,parentClock,rc)

    use time, only : getm_time_start => start, getm_time_stop => stop
    use time, only : getm_time_timestep => timestep
    use initialise,  only: init_model,dryrun
    use integration, only: MinN,MaxN

    implicit none

    type(ESMF_GridComp) :: gridComp
    type(ESMF_State)    :: importState,exportState ! may be uninitialized
    type(ESMF_Clock)    :: parentClock        ! may be uninitialized
    integer,intent(out) :: rc

    character(ESMF_MAXSTR):: name, message, timeString, string
    type(ESMF_Clock)      :: clock
    type(ESMF_Time)       :: currTime, startTime, stopTime
    logical               :: vmIsPresent,clockIsPresent
    type(ESMF_TimeInterval) :: timeInterval
    integer(ESMF_KIND_I4) :: localPet, petCount
    type(ESMF_VM)         :: vm
    real(ESMF_KIND_R8)    :: h_r8

    type(ESMF_Time)         :: getmRefTime,getmStartTime,getmStopTime
    integer                 :: getmRunTimeStepCount
    character(len=8)        :: datestr
    character(len=10)       :: timestr
    character(len=19)       :: TimeStrISOFrac,start_external,stop_external

    !! Check whether there is already a clock (it might have been set
    !! with a prior ESMF_gridCompCreate() call.  If not, then create
    !! a local clock as a clone of the parent clock, and associate it
    !! with this component.  Finally, set the name of the local clock
    call ESMF_GridCompGet(gridComp, name=name,                     &
                                    vmIsPresent=vmIsPresent,       &
                                    clockIsPresent=clockIsPresent, &
                                    rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (.not.vmIsPresent) then
      call ESMF_LogWrite('no VM present',ESMF_LOGMSG_ERROR, &
                         line=__LINE__,file=__FILE__,method='getmCmp_init()')
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end if

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
    call ESMF_GridCompGet(gridComp, vm=vm, rc=rc)
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

    !> Here comes your own time initialization code
    !! In particular, this should contain
    !! 1. Setting your internal timestep and adding it to your clock, this could
    !!    be a timestep read from an external file
    !!    ESMF_TimeIntervalSet(timeStep)
    !!    ESMF_ClockSet(Clock, timeStep=timeStep)
    !!    The default behaviour is to take the parent's time step

    !! Finally, log the successful completion of this function
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initialized'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

  end subroutine Initialize

  subroutine Run(gridComp,importState,exportState,parentClock,rc)

    use initialise ,only: runtype,dryrun
    use integration,only: MinN

    implicit none

    type(ESMF_GridComp) :: gridComp
    type(ESMF_State)    :: importState,exportState ! may be uninitialized
    type(ESMF_Clock)    :: parentClock        ! may be uninitialized
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

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet, &
      name=name, clockIsPresent=clockIsPresent, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (.not.clockIsPresent) then
      call ESMF_LogWrite('Required clock not found in '//trim(name), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif

    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_ClockGet(clock,currTime=currTime, advanceCount=advanceCount, &
      timeStep=timeInterval, stopTime=stopTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A,I8)') trim(timestring)//' '//trim(name)//' run ',advanceCount
    call ESMF_TimeGet(stopTime,timeStringISOFrac=timestring)
    write(message,'(A,I8)') trim(message)//' to '//trim(timeString)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    !> Here comes your code for reading the import states

    !  use parentClock to do determine time of calling routine
    call ESMF_ClockGetNextTime(parentClock,nextTime,rc=localrc)
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

      call ESMF_ClockAdvance(clock)
      call ESMF_ClockGet(clock,currtime=currTime,advanceCount=advanceCount)
    end do

!  Fill export state here using ESMF_StateAdd(), etc
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    write(message,'(A,A)') trim(timeString)//' '//trim(name), &
          ' finished running.'
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=rc)


  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)

    use initialise ,only: runtype,dryrun
    use integration,only: MaxN
    use output     ,only: meanout

    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR):: name, message, timeString
    type(ESMF_Grid)       :: getmGrid
    type(ESMF_Clock)      :: clock
    type(ESMF_DistGrid)   :: getmDistGrid
    type(ESMF_Time)       :: currTime
    logical               :: ClockIsPresent,GridIsPresent

#ifndef NO_3D
    if (meanout .eq. 0) then
      call calc_mean_fields(MaxN,MaxN)
    end if
#endif
    call clean_up(dryrun,runtype,MaxN)

    call ESMF_GridCompGet(gridComp,name=name,                     &
                                   clockIsPresent=ClockIsPresent, &
                                   gridIsPresent=GridIsPresent,   &
                                   rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (ClockIsPresent) then
      call ESMF_GridCompGet(gridComp,clock=clock,rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_ClockDestroy(clock)
      call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      write(message,'(A,A)') trim(timeString)//' '//trim(name), &
            ' finalized.'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=rc)
    end if

    if (GridIsPresent) then
      call ESMF_GridCompGet(gridComp,grid=getmGrid)
      call ESMF_GridGet(getmGrid,distgrid=getmDistGrid)
      call ESMF_DistGridDestroy(getmDistGrid)
      call ESMF_GridDestroy(getmGrid)
    end if

    call getmCmp_finalize_variables()

   end subroutine Finalize

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: getmCmp_init_variables
!
! !INTERFACE:
   subroutine getmCmp_init_variables()
!
! !DESCRIPTION:
!
! !USES:
   use domain, only: imin,jmin,imax,jmax
   use domain, only: xcord,ycord,xx,yx,lonx,latx
   use domain, only: grid_type
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
   REALTYPE :: getmreal
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
!  KK-TODO: as long as coordinate arrays in GETM do not hold the target
!           attribute we have to copy the data.
   noKindMatch = .true.

   if (noKindMatch) then
      select case (grid_type)
         case(1,2)
            allocate(xcord_(_IRANGE_HALO_)) ; xcord_ = xcord
            allocate(ycord_(_JRANGE_HALO_)) ; ycord_ = ycord
         case(3,4)
            allocate(xx_(E2DXFIELD)) ; xx_ = xx
            allocate(yx_(E2DXFIELD)) ; yx_ = yx
      end select
   else
      !xcord_ => xcord ; ycord_ => ycord
      !xx_    => xx    ; yx_    => yx
      !lonx_  => lonx  ; latx_  => latx
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
! !ROUTINE: getmCmp_finalize_variables
!
! !INTERFACE:
   subroutine getmCmp_finalize_variables()
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
! !LOCAL VARIABLES
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_finalize_variables() # ',Ncall
#endif

   if (noKindMatch) then
      if(associated(xcord_)) deallocate(xcord_)
      if(associated(ycord_)) deallocate(ycord_)
      if(associated(xx_   )) deallocate(xx_   )
      if(associated(yx_   )) deallocate(yx_   )
      if(associated(lonx_ )) deallocate(lonx_ )
      if(associated(latx_ )) deallocate(latx_ )
   end if

#ifdef DEBUG
   write(debug,*) 'getmCmp_finalize_variables()'
   write(debug,*)
#endif
   return

   end subroutine getmCmp_finalize_variables
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: getmCmp_init_grid - Creates Grid
!
! !INTERFACE:
   subroutine getmCmp_init_grid(getmCmp)
!
! !DESCRIPTION:
!
! !USES:
   use domain, only: ioff,joff,imax,jmax
   use domain, only: grid_type
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
   type(ESMF_GridComp) :: getmCmp
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
   type(ESMF_VM)            :: getmVM
   type(ESMF_DistGrid)      :: getmDistGrid
   type(ESMF_Grid)          :: getmGrid
   type(ESMF_CoordSys_Flag) :: coordSys
   type(ESMF_StaggerLoc)    :: StaggerLoc
   type(ESMF_ARRAY)         :: xArray,yArray
   integer(ESMF_KIND_I4),dimension(:),allocatable,target :: alledges
   integer(ESMF_KIND_I4),dimension(4),target             :: myedges
   integer                  :: getmPetCount
   integer                  :: pet,i0,j0,ilen,jlen
   integer,dimension(2)     :: coordDimCount
   integer,dimension(2,2)   :: coordDimMap
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

   call ESMF_GridCompGet(getmCmp,vm=getmVM,petCount=getmPetCount)

   myedges = (/ ioff , joff , imax , jmax /)
   allocate(alledges(4*getmPetCount))
!  syncflag=ESMF_SYNC_BLOCKING (default)
   call ESMF_VMAllGather(getmVM,myedges,alledges,4)

   allocate(deBlockList(2,2,getmPetCount))
   do pet = 0,getmPetCount-1
      i0   = 1 + alledges(1+4*pet)
      j0   = 1 + alledges(2+4*pet)
      ilen =     alledges(3+4*pet)
      jlen =     alledges(4+4*pet)
      deBlockList(:,1,1+pet) = (/ i0        , j0        /)
      deBlockList(:,2,1+pet) = (/ i0+ilen-1 , j0+jlen-1 /)
   end do

!  indexflag=ESMF_INDEX_DELOCAL (default) starting at 1
!  (for ESMF_INDEX_USER [grid|stagger]MemLBound can be set)
#if 1
!  Single-tile DistGrid (1 subdomain = 1 DE)
   getmDistGrid = ESMF_DistGridCreate(minval(deBlockList(:,1,:),2), &
                                      maxval(deBlockList(:,2,:),2), &
                                      deBlockList)
#else
!  Multi-tile DistGrid (1 subdomain = 1 tile = 1 DE) by specification of
!  [min|max]IndexPTile.
!  Note (KK): int() intrinsic routines are needed, because ESMF does not
!             accept subarrays as arguments
   getmDistGrid = ESMF_DistGridCreate(int(deBlockList(:,1,:)),int(deBlockList(:,2,:)))
#endif

   select case (grid_type)
      case(1)
         coordSys = ESMF_COORDSYS_CART
         coordDimCount = (/ 1 , 1 /)         ! rectilinear coordinates
         coordDimMap = reshape( (/1,2,0,0/) , (/2,2/) )
         StaggerLoc = ESMF_STAGGERLOC_CENTER ! (default)
!        1D xcord is replicated automatically along 2nd DistGrid dimension
!        1D ycord is replicated along 1st DistGrid dimension as specified
!        by distgridToArrayMap.
!        total[L|U]Width are automatically determined from shape of [x|y]cord
!        datacopyflag = ESMF_DATACOPY_REFERENCE (default)
!        Note (KK): These ArrayCreate()'s only work for 1DE per PET!!!
!                   Automatically determined coordDimMap for rectilinear
!                   coordinates is incorrect!
         xArray = ESMF_ArrayCreate(getmDistGrid,xcord_,indexflag=ESMF_INDEX_DELOCAL)
         yArray = ESMF_ArrayCreate(getmDistGrid,ycord_,indexflag=ESMF_INDEX_DELOCAL, &
                                   distgridToArrayMap=(/0,1/))
      case(2)
         coordSys = ESMF_COORDSYS_SPH_DEG    ! (default)
         coordDimCount = (/ 1 , 1 /)         ! rectilinear coordinates
         coordDimMap = reshape( (/1,2,0,0/) , (/2,2/) )
         StaggerLoc = ESMF_STAGGERLOC_CENTER ! (default)
         xArray = ESMF_ArrayCreate(getmDistGrid,xcord_,indexflag=ESMF_INDEX_DELOCAL)
         yArray = ESMF_ArrayCreate(getmDistGrid,ycord_,indexflag=ESMF_INDEX_DELOCAL, &
                                   distgridToArrayMap=(/0,1/))
      case(3)
         coordSys = ESMF_COORDSYS_CART
         coordDimCount = (/ 2 , 2 /)                    ! (default for 2D Grid)
         coordDimMap = reshape( (/1,1,2,2/) , (/2,2/) ) ! (default for 2D Grid)
         StaggerLoc = ESMF_STAGGERLOC_CORNER
!        Note (KK): automatically determined total[L|U]Width are not consistent
!                   with gridAlign specified later
         xArray = ESMF_ArrayCreate(getmDistGrid,xx_,              &
                                   indexflag=ESMF_INDEX_DELOCAL,  &
                                   totalLWidth=(/HALO+1,HALO+1/), &
                                   totalUWidth=(/HALO,HALO/))
         yArray = ESMF_ArrayCreate(getmDistGrid,yx_,              &
                                   indexflag=ESMF_INDEX_DELOCAL,  &
                                   totalLWidth=(/HALO+1,HALO+1/), &
                                   totalUWidth=(/HALO,HALO/))
      case(4)
         coordSys = ESMF_COORDSYS_SPH_DEG               ! (default)
         coordDimCount = (/ 2 , 2 /)                    ! (default for 2D Grid)
         coordDimMap = reshape( (/1,1,2,2/) , (/2,2/) ) ! (default for 2D Grid)
         StaggerLoc = ESMF_STAGGERLOC_CORNER
         xArray = ESMF_ArrayCreate(getmDistGrid,lonx_,            &
                                   indexflag=ESMF_INDEX_DELOCAL,  &
                                   totalLWidth=(/HALO+1,HALO+1/), &
                                   totalUWidth=(/HALO,HALO/))
         yArray = ESMF_ArrayCreate(getmDistGrid,latx_,            &
                                   indexflag=ESMF_INDEX_DELOCAL,  &
                                   totalLWidth=(/HALO+1,HALO+1/), &
                                   totalUWidth=(/HALO,HALO/))
   end select

!  Note (KK): gridAlign specifies which corner point in a grid cell
!             shares the center indices [ default=(/-1,-1/) ].
!             gridEdge[L|U]Width only affect DE's at the edge of tiles
!             (thus it matters whether a single- or multi-tile DistGrid
!              was created). If gridEdgeWidth's are not set, they are set
!             automatically based on gridAlign.
   getmGrid = ESMF_GridCreate(getmDistGrid,name="getmGrid",             &
                                           gridAlign=(/1,1/),           &
                                           coordSys=coordSys,           &
                                           coordDimCount=coordDimCount, &
                                           coordDimMap=coordDimMap)
   call ESMF_GridSetCoord(getmGrid,1,array=xArray,staggerloc=StaggerLoc)
   call ESMF_GridSetCoord(getmGrid,2,array=yArray,staggerloc=StaggerLoc)

   call ESMF_GridCompSet(getmCmp,grid=getmGrid)

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
! !ROUTINE: TimeStringISOFrac2ESMFtime - converts timestring to ESMF_Time
!
! !INTERFACE:
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
