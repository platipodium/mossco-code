!> @brief Implementation of a GETM ocean component
!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2013, Helmholtz-Zentrum Geesthacht 
!> @author Knut Klingbeil, Institut für Ostseeforschung Warnemünde
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE:  getm_component - 
!
! !INTERFACE:
   module getm_component
!
! !DESCRIPTION:
!  Example for GriddedComponent can be found in
!  ESMFDIR/src/Superstructure/Component/examples/ESMF_GCompEx.F90.
!
! !USES:
   use esmf
   use getm_driver

   IMPLICIT NONE
   private
!
! !PUBLIC DATA MEMBERS:
   public getmCmp_SetServices ! must be public
   !public getm_esmf_SetVM     ! optional
!
! !PRIVATE DATA MEMBERS:
   private getmCmp_init,getmCmp_run,getmCmp_finalize
   type(ESMF_Clock) :: getmClock
!
! !REVISION HISTORY:
!  Original author(s): Knut Klingbeil
!
!EOP
!-----------------------------------------------------------------------

    contains

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: getmCmp_SetServices - register GriddedComponent GETM
!
! !INTERFACE:
   subroutine getmCmp_SetServices(getmCmp,rc)
!
! !DESCRIPTION:
!  The toplevel component requires this sub for its mandatory call to
!  ESMF_GridCompSetServices(getmCmp,userRoutine=getmCmp_SetServices).
!  For interface see ESMFDIR/src/Superstructure/Component/src/ESMF_GridComp.F90.
!  For the NUOPC Layer this routine must be named SetServices().
!  The toplevel component can inquire rc via optional keyword argument
!  userRc to ESMF_GridCompSetServices().
!
! !USES:
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
   type(ESMF_GridComp) :: getmCmp
!
! !OUTPUT PARAMETERS:
   integer,intent(out) :: rc
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_SetServices() # ',Ncall
#endif

!  Register user-code subroutines.
!  Set the entry points for standard ESMF Component methods
!  ESMF_GridComp[Initialize|Run|Finalize](), called by toplevel component.
!  Optional keyword argument "phase" (default: 1) promotes multi-phase user-code.

   call ESMF_GridCompSetEntryPoint(getmCmp,ESMF_METHOD_INITIALIZE,     &
                                   userRoutine=getmCmp_init,rc=rc)
   call ESMF_GridCompSetEntryPoint(getmCmp,ESMF_METHOD_RUN,            &
                                   userRoutine=getmCmp_run,rc=rc)
   call ESMF_GridCompSetEntryPoint(getmCmp,ESMF_METHOD_FINALIZE,       &
                                   userRoutine=getmCmp_finalize,rc=rc)

!  Optional registration of additional routines for checkpoint and
!  restart functions (ESMF_METHOD_[WRITE|READ]RESTART).
!  ...

#ifdef DEBUG
   write(debug,*) 'Leaving getmCmp_SetServices()'
   write(debug,*)
#endif
   return

   end subroutine getmCmp_SetServices
!EOC
!-----------------------------------------------------------------------
#if 0
!BOP
!
! !ROUTINE: getmCmp_SetVM - 
!
! !INTERFACE:
   subroutine getmCmp_SetVM(getmCmp,rc)
!
! !DESCRIPTION:
!  The toplevel component requires this sub for the optional call to
!  ESMF_GridCompSetVM(getmCmp,userRoutine=getmCmp_SetVM).
!  For interface see ESMFDIR/src/Superstructure/Component/src/ESMF_GridComp.F90.
!  The toplevel component can inquire rc via optional keyword argument
!  userRc to ESMF_GridCompSetVM().
!
! !USES:
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
   type(ESMF_GridComp) :: getmCmp
!
! !OUTPUT PARAMETERS:
   integer,intent(out) :: rc
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES
   type(ESMF_VM) :: gVM
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_SetVM() # ',Ncall
#endif

   call ESMF_VMGetGlobal(gVM,rc=rc)
!  Calls to ESMF_VMGet, ESMF_GridCompSetVMMaxPEs,
!  ESMF_GridCompSetVM[Min|Max]Threads to modify VM of component

#ifdef DEBUG
   write(debug,*) 'Leaving getmCmp_SetVM()'
   write(debug,*)
#endif
   return

   end subroutine getmCmp_SetVM
!EOC
#endif
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: getmCmp_init - 
!
! !INTERFACE:
   subroutine getmCmp_init(getmCmp,iState,eState,pClock,rc)
!
! !DESCRIPTION:
!  Note: [i|e]state and pClock are uninitialized if the toplevel
!        component did not provide corresponding arguments to
!        ESMF_GridCompInitialize(getmCmp).
!  The toplevel component can inquire rc via optional keyword argument
!  userRc to ESMF_GridCompInitialize().
!
! !USES:
   use time       ,only: start,stop,timestep
   use initialise ,only: init_model
   use integration,only: MinN,MaxN
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
   type(ESMF_GridComp) :: getmCmp
   type(ESMF_State)    :: iState,eState ! may be uninitialized
   type(ESMF_Clock)    :: pClock        ! may be uninitialized
!
! !OUTPUT PARAMETERS:
   integer,intent(out) :: rc
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES
   type(ESMF_Time)         :: getmRefTime,getmStartTime,getmStopTime
   type(ESMF_TimeInterval) :: getmTimeStep
   logical                 :: ClockIsPresent
   integer                 :: getmRunTimeStepCount
   character(len=8)        :: datestr
   character(len=10)       :: timestr
   character(len=19)       :: TimeStrISOFrac,start_external,stop_external
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_init() # ',Ncall
#endif

!  Optional creation of child components
!  (Create, SetServices, Initialize)
!  ...

!  This is where the model specific setup code goes
!  (allocation, open files, initial conditions).
   call date_and_time(datestr,timestr)

!  Check whether toplevel component called ESMF_GridCompCreate() with clock. 
   call ESMF_GridCompGet(getmCmp,clockIsPresent=ClockIsPresent)

   if (ClockIsPresent) then

!     Use startTime and stopTime from already initialised getmClock.
      call ESMF_GridCompGet(getmCmp,clock=getmClock)
      call ESMF_ClockGet(getmClock, &
                         startTime=getmStartTime,stopTime=getmStopTime)
      call ESMF_TimeGet(getmStartTime,timeStringISOFrac=start_external)
      call ESMF_TimeGet(getmStopTime,timeStringISOFrac=stop_external)

      call preinit_model(datestr,timestr)
      call init_time(MinN,MaxN,start_external=start_external, &
                     stop_external=stop_external)
      call postinit_model()

!     use internal GETM time step
      call ESMF_TimeIntervalSet(getmTimeStep,s_r8=timestep)
      call ESMF_ClockSet(getmClock,timeStep=getmTimeStep)
!     KK-TODO: does the getmCmp::clock point to getmClock???

   else

      call init_model(datestr,timestr)

!     reference time
      TimeStrISOFrac=start(1:10)//"T"//start(12:19)
      !call ESMF_TimeSet(refTime,timeStringISOFrac=TimeStrISOFrac)
      call TimeStringISOFrac2ESMFtime(TimeStrISOFrac,getmRefTime)

!     time step
      call ESMF_TimeIntervalSet(getmTimeStep,s_r8=timestep)

!     start time
      getmStartTime = getmRefTime + (MinN-1)*getmTimeStep

!     stop time
      getmStopTime = getmRefTime + MaxN*getmTimeStep
      getmRunTimeStepCount = MaxN - MinN + 1

!     set up clock based on internal GETM specifications
      getmClock = ESMF_ClockCreate(getmTimeStep,getmStartTime,            &
                                   runTimeStepCount=getmRunTimeStepCount, &
                                   refTime=getmRefTime,                   &
                                   name='getmClock')
      call ESMF_GridCompSet(getmCmp,clock=getmClock)

   end if

!  If the initial Export state needs to be filled, do it here.
   !call ESMF_StateAdd(eState,field,rc)
   !call ESMF_StateAdd(eState,bundle,rc)

   rc = ESMF_SUCCESS

#ifdef DEBUG
   write(debug,*) 'Leaving getmCmp_init()'
   write(debug,*)
#endif
   return

   end subroutine getmCmp_init
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: getmCmp_run - 
!
! !INTERFACE:
   subroutine getmCmp_run(getmCmp,iState,eState,pClock,rc)
!
! !DESCRIPTION:
!  Note: [i|e]state and pClock are uninitialized if the toplevel
!        component did not provide corresponding arguments to
!        ESMF_GridCompRun(getmCmp).
!  The toplevel component can inquire rc via optional keyword argument
!  userRc to ESMF_GridCompRun().
!
! !USES:
   use initialise ,only: runtype
   use integration,only: MinN
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
   type(ESMF_GridComp) :: getmCmp
   type(ESMF_State)    :: iState,eState ! may be uninitialized
   type(ESMF_Clock)    :: pClock        ! may be uninitialized
!
! !OUTPUT PARAMETERS:
   integer,intent(out) :: rc
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES
   type(ESMF_Time)         :: getmTime,NextTime
   type(ESMF_TimeInterval) :: getmTimeStep
   integer(ESMF_KIND_I8)   :: loop
   integer                 :: localrc
   integer                 :: n
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_run() # ',Ncall
#endif

!  call ESMF_StateGet(), etc to get fields, bundles, arrays from import state

   call ESMF_ClockGet(getmClock,timeStep=getmTimeStep,currtime=getmTime,advanceCount=loop)

!  use pClock to do determine time of calling routine
   call ESMF_ClockGetNextTime(pClock,NextTime,rc=localrc)
   if (localrc .ne. ESMF_SUCCESS) then
      call ESMF_LogWrite('will continue until own stopTime',ESMF_LOGMSG_WARNING, &
                         line=__LINE__,file=__FILE__,method='getmCmp_run()')
      call ESMF_ClockGet(getmClock,stopTime=NextTime)
   end if


   do while (getmTime + 0.5d0*getmTimeStep <= NextTime)

      if (ESMF_ClockIsStopTime(getmClock)) then
         call ESMF_LogWrite('already exceeded stopTime',ESMF_LOGMSG_ERROR, &
                            line=__LINE__,file=__FILE__,method='getmCmp_run()')
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if

!     optional Run of child components

!     This is where the model specific computation goes.
      n = int(loop,kind=kind(MinN))+MinN
      call time_step(runtype,n)

      call ESMF_ClockAdvance(getmClock)
      call ESMF_ClockGet(getmClock,currtime=getmTime,advanceCount=loop)

   end do

!  Fill export state here using ESMF_StateAdd(), etc

   rc = ESMF_SUCCESS

#ifdef DEBUG
   write(debug,*) 'Leaving getmCmp_run()'
   write(debug,*)
#endif
   return

   end subroutine getmCmp_run
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: getmCmp_finalize - 
!
! !INTERFACE:
   subroutine getmCmp_finalize(getmCmp,iState,eState,pClock,rc)
!
! !DESCRIPTION:
!  Note: [i|e]state and pClock are uninitialized if the toplevel
!        component did not provide corresponding arguments to
!        ESMF_GridCompFinalize(getmCmp).
!  The toplevel component can inquire rc via optional keyword argument
!  userRc to ESMF_GridCompFinalize().
!
! !USES:
   use initialise ,only: runtype,dryrun
   use integration,only: MaxN
   use output     ,only: meanout
   IMPLICIT NONE
!
! !INPUT/OUTPUT PARAMETERS:
   type(ESMF_GridComp) :: getmCmp
   type(ESMF_State)    :: iState,eState ! may be uninitialized
   type(ESMF_Clock)    :: pClock        ! may be uninitialized
!
! !OUTPUT PARAMETERS:
   integer,intent(out) :: rc
!
! !REVISION HISTORY:
!
! !LOCAL VARIABLES
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_finalize() # ',Ncall
#endif

!  optional Finalize of child components
!  Add whatever code here needed (deallocation,close files,flush results)
#ifndef NO_3D
   if (meanout .eq. 0) then
      call calc_mean_fields(MaxN,MaxN)
   end if
#endif
   call clean_up(dryrun,runtype,MaxN)

   call ESMF_ClockDestroy(getmClock)

   rc = ESMF_SUCCESS

#ifdef DEBUG
   write(debug,*) 'Leaving getmCmp_finalize()'
   write(debug,*)
#endif
   return

   end subroutine getmCmp_finalize
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
