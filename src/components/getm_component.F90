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

   IMPLICIT NONE
   private
!
! !PUBLIC DATA MEMBERS:
   public getmCmp_SetServices ! must be public
   !public getm_esmf_SetVM     ! optional
!
! !PRIVATE DATA MEMBERS:
   private getmCmp_init,getmCmp_run,getmCmp_finalize
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
   use time       ,only: start,timestep
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
   type(ESMF_Clock)        :: getmClock
   type(ESMF_Time)         :: getmRefTime,getmStartTime,getmStopTime
   type(ESMF_Time)         :: startTime,stopTime
   type(ESMF_TimeInterval) :: getmTimeStep
   type(ESMF_TimeInterval) :: TimeInterval
   logical                 :: ClockIsPresent
   integer                 :: getmRunTimeStepCount
   character(len=8)        :: datestr
   character(len=10)       :: timestr
   character(len=19)       :: TimeStrISOFrac
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
   call init_model(datestr,timestr)

!  reference time
   TimeStrISOFrac=start(1:10)//"T"//start(12:19)
   !call ESMF_TimeSet(refTime,timeStringISOFrac=TimeStrISOFrac)
   call TimeStringISOFrac2ESMFtime(TimeStrISOFrac,getmRefTime)

!  time step
   call ESMF_TimeIntervalSet(getmTimeStep,s_r8=timestep)

!  start time
   getmStartTime = getmRefTime + (MinN-1)*getmTimeStep

!  stop time
   getmStopTime = getmRefTime + MaxN*getmTimeStep
   getmRunTimeStepCount = MaxN - MinN + 1

!  Clock
   call ESMF_GridCompGet(getmCmp,clockIsPresent=ClockIsPresent)
   if (ClockIsPresent) then
      call ESMF_GridCompGet(getmCmp,clock=getmClock)
      call ESMF_ClockGet(getmClock,timeStep=TimeInterval,       &
                         startTime=startTime,stopTime=stopTime)
      if (getmTimeStep /= TimeInterval) then
         call ESMF_LogWrite('TimeStep does not match',ESMF_LOGMSG_ERROR, &
                            line=__LINE__,file=__FILE__,method='getmCmp_init()')
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
      if (getmStartTime /= startTime) then
         call ESMF_LogWrite('startTime does not match',ESMF_LOGMSG_ERROR, &
                            line=__LINE__,file=__FILE__,method='getmCmp_init()')
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
      if (getmStopTime /= stopTime) then
         call ESMF_LogWrite('stopTime does not match',ESMF_LOGMSG_ERROR, &
                            line=__LINE__,file=__FILE__,method='getmCmp_init()')
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
   else
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
   use integration
   use initialise,only: runtype
   use time,     only: update_time,timestep
   use domain,   only: kmax
   use meteo,    only: do_meteo,tausx,tausy,airp,fwf_method,evap,precip
   use m2d,      only: integrate_2d
   use variables_2d, only: fwf,fwf_int
#ifndef NO_3D
   use m3d,      only: integrate_3d,M
#ifndef NO_BAROCLINIC
   use variables_3d, only: T
#endif
   use rivers,   only: do_rivers
#ifdef _FABM_
   use getm_fabm, only: fabm_calc,do_getm_fabm
#endif
#ifdef GETM_BIO
   use bio, only: bio_calc
   use getm_bio, only: do_getm_bio
#endif
#endif
#ifdef SPM
   use suspended_matter, only: spm_calc,do_spm
#endif
   use input,    only: do_input
   use output,   only: do_output,meanout
#ifdef TEST_NESTING
   use nesting,   only: nesting_file
#endif
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
   type(ESMF_Clock)        :: getmClock
   type(ESMF_Time)         :: getmTime,NextTime
   type(ESMF_TimeInterval) :: getmTimeStep
   integer(ESMF_KIND_I8)   :: loop
   integer                 :: localrc
   integer                 :: n
   logical                 :: do_3d
   integer                 :: progress=100
   character(8)            :: d_
   character(10)           :: t_
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

   call ESMF_GridCompGet(getmCmp,clock=getmClock)
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

      if (progress .gt. 0 .and. mod(n,progress) .eq. 0) then
         call date_and_time(date=d_,time=t_)
         LEVEL1 t_(1:2),':',t_(3:4),':',t_(5:10),' n=',n
      end if

#ifndef NO_3D
      do_3d = (runtype .ge. 2 .and. mod(n,M) .eq. 0)
#endif

!     INPUT (for the time being)
      call do_input(n)

      if(runtype .le. 2) then
         call do_meteo(n)
#ifndef NO_3D
#ifndef NO_BAROCLINIC
      else
         call do_meteo(n,T(:,:,kmax))
#endif
#endif
      end if

      if (fwf_method .ge. 1) then
         fwf = evap+precip
#ifndef NO_3D
         fwf_int = fwf_int+timestep*fwf
#endif
      end if

#ifndef NO_BAROTROPIC
      call integrate_2d(runtype,n,tausx,tausy,airp)
#endif
#ifndef NO_3D
      call do_rivers(do_3d)
      if (do_3d) then
         call integrate_3d(runtype,n)
#ifdef SPM
         if (spm_calc) call do_spm()
#endif
#ifdef _FABM_
         if (fabm_calc) call do_getm_fabm(M*timestep)
#endif
#ifdef GETM_BIO
         if (bio_calc) call do_getm_bio(M*timestep)
#endif
#ifndef NO_3D
         if (fwf_method .ge. 1) then
            fwf_int = 0.0d0
         end if
#endif
      end if
#endif

#ifdef TEST_NESTING
      if (mod(n,80) .eq. 0) then
         call nesting_file(WRITING)
      end if
#endif
      call update_time(n)

!     OUTPUT (for the time being)
#ifndef NO_3D
      if(meanout .ge. 0) then
         call calc_mean_fields(n,meanout)
      end if
#endif
      call do_output(runtype,n,timestep)
#ifdef DIAGNOSE
      call diagnose(n,MaxN,runtype)
#endif

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
   type(ESMF_Clock) :: getmClock
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'getmCmp_finalize() # ',Ncall
#endif

   call ESMF_GridCompGet(getmCmp,clock=getmClock)

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
