#include <cppdefs.h>
#include "fabm_driver.h"
#define _GRID_ sed%grid
#define _INUM_ _GRID_%inum
#define _JNUM_ _GRID_%jnum
#define _KNUM_ _GRID_%knum

module esmf_fabm_sediment_component

  use esmf
  use fabm
  use fabm_types  ! for realkind
  use fabm_sediment_driver

  implicit none

  private
 
  real(rk)  :: dzmin,dt
  integer   :: t,tnum,funit,output,k,n,numyears,numlayers
  real(rk),dimension(:,:,:,:),allocatable,target :: conc
  real(rk),dimension(:,:,:),allocatable        :: bdys,fluxes
 
  type(type_sed),save :: sed
  type(ESMF_Alarm),save :: outputAlarm

  namelist /run_nml/ numyears,dt,output,numlayers,dzmin
 
  public :: empty_SetServices
  
  contains

  subroutine empty_SetServices(gridcomp, rc)
  
    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine empty_SetServices

  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    type(ESMF_TimeInterval) :: timeInterval,alarmInterval
    type(ESMF_Time)         :: startTime
    character(len=ESMF_MAXSTR) :: string

    !! read namelist input for control of time, this should not be done like this,
    !! but handled outside the component.  Maybe later introduce a local clock 
    open(33,file='run.nml',action='read',status='old')
    read(33,nml=run_nml)

    !! Set the time step end stop time
    call ESMF_TimeIntervalSet(timeInterval,s_r8=dt,rc=rc)
    call ESMF_ClockSet(parentClock,timeStep=timeInterval,rc=rc)
    call ESMF_TimeIntervalSet(timeInterval,yy=numyears,rc=rc)
    call ESMF_ClockGet(parentClock,startTime=startTime)
    call ESMF_ClockSet(parentClock,stopTime=startTime + timeInterval,rc=rc)
   
    !! also from namelist, the output timesteop is read and
    !! used to create an alarm
    call ESMF_TimeIntervalSet(alarmInterval,s=output,rc=rc)
    outputAlarm = ESMF_AlarmCreate(clock=parentClock,ringTime=startTime,ringInterval=alarmInterval,rc=rc)

    !! The grid specification should also go to outside this routine, and update the grid of
    !! this component, numlayers and dzmin are read from nml
    sed%grid%knum=numlayers
    sed%grid%dzmin=dzmin

    !! Write log entries
    write(string,'(A,I3,A)') 'Initialise grid with ',sed%grid%knum,' vertical layers'
    call ESMF_LogWrite(string,ESMF_LOGMSG_INFO)
    call init_sed_grid(sed%grid)

    !! Link conc to fabm_sediment_driver
    sed%conc => conc
    
    call ESMF_LogWrite('Initialise sediment module',ESMF_LOGMSG_INFO)
    call init_fabm_sed(sed)
    close(33)

    !! Allocate all arrays bdys, fluxes TODO: add these
    !! arrays to ESMF fields
    call ESMF_LogWrite('Allocate arrays',ESMF_LOGMSG_INFO)
    allocate(bdys(_INUM_,_JNUM_,sed%nvar+1))
    bdys(1,1,1:9) = 0.0_rk
    bdys(1,1,1) = 10._rk   ! degC temperature
    bdys(1,1,5) = 1.0_rk   ! mmolP/m**3 po4
    bdys(1,1,6) = 10.0_rk  ! mmolN/m**3 no3
    bdys(1,1,7) = 0.0_rk   ! mmolN/m**3 nh3
    bdys(1,1,8) = 250.0_rk ! mmolO2/m**3 oxy
    bdys(1,1,9) = 0.0_rk   ! odu

    allocate(fluxes(_INUM_,_JNUM_,sed%nvar))
    fluxes(1,1,1:8) = 0.0_rk
    fluxes(1,1,1) = 5.0_rk/86400.0_rk !fdet
    fluxes(1,1,2) = 5.0_rk/86400.0_rk !sdet
    fluxes(1,1,3) = 0.08/86400.0_rk !pdet

    !! define an output unit for tsv output, TODO: add netcdf output for this
    funit=2
    open(funit,file='output.dat')
    write(funit,*) 'time(s) ','depth(m) ','conc(n) '

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc
 
    character(len=19) :: timestring1,timestring2
    type(ESMF_Time)   :: wallTime, clockTime
    type(ESMF_TimeInterval) :: timeInterval
    type(ESMF_Grid)   :: grid
    type(ESMF_FieldBundle) :: fieldBundle
    type(ESMF_Field), allocatable, dimension(:) :: fieldList
    integer           :: fieldCount
    integer(8)     :: t
    character(len=ESMF_MAXSTR)  :: name,string
    
    call ESMF_TimeSet(clockTime)
    call ESMF_ClockGet(parentClock,currTime=clockTime)
    call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring1)
    call ESMF_TimeSet(wallTime)
    call ESMF_TimeSyncToRealTime(wallTime)
    call ESMF_TimeGet(wallTime,timeStringISOFrac=timestring2)
  
 
    call ESMF_LogWrite("Empty run at "//timestring1 &
      //" ("//timestring2//")", ESMF_LOGMSG_INFO)


    !call ESMF_StatePrint(importState,options="long",nestedFlag=.true.,rc=rc)
    call ESMF_StateGet(importState,"FABM field bundle",fieldBundle,rc=rc)
    !call ESMF_FieldBundlePrint(fieldBundle,rc=rc)
    call ESMF_FieldBundleGet(fieldBundle,fieldCount=fieldCount,rc=rc)
    allocate(fieldList(fieldCount))
    call ESMF_FieldBundleGet(fieldBundle,fieldList=fieldList,rc=rc)
    call ESMF_FieldGet(fieldList(1),name=name,rc=rc)
  
    !! Get integration time step from parent clock
    call ESMF_ClockGet(parentClock,timeStep=timeInterval,rc=rc)
    call ESMF_TimeIntervalGet(timeInterval,s_r8=dt)

!integrate TODO: enable this function !!! TODO: enable this function !!!
   call ode_solver(sed,bdys,fluxes,dt,0,fabm_sed_get_rhs)

 ! reset concentrations to mininum_value
     do n=1,sed%nvar
       do k=1,sed%grid%knum
         if (sed%conc(1,1,k,n) .lt. sed%model%info%state_variables(n)%minimum) then
            sed%conc(1,1,k,n) = sed%model%info%state_variables(n)%minimum
         end if
       end do
     end do

    !! Check if the output alarm is ringing, if so, quiet it and 
    !! get the current advance count (formerly t) from parent clock
    if (ESMF_AlarmIsRinging(outputAlarm)) then
      call ESMF_AlarmRingerOff(outputAlarm,rc=rc)
      call ESMF_ClockGet(parentClock,advanceCount=t)
      write(string,'(A,F7.1,A)') 'Elapsed ',t*dt/86400,' days'
      write(*,'(A,F7.1,A)') 'Elapsed ',t*dt/86400,' days'
      call ESMF_LogWrite(string,ESMF_LOGMSG_INFO)
      write(funit,*) t*dt,'fluxes',fluxes(1,1,:)
      do k=1,_KNUM_ 
        write(funit,*) t*dt,sed%grid%zc(1,1,k),sed%conc(1,1,k,:)
      end do
    endif
 
    deallocate(fieldList)

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    close(funit)
    call finalize_fabm_sed()
    deallocate(conc)
    deallocate(bdys)
    deallocate(fluxes)

    call ESMF_AlarmDestroy(outputAlarm,rc=rc)

  end subroutine Finalize


subroutine ode_solver(sedi,bdys,fluxes,dt,method,get_rhs)
use fabm_sediment_driver, only : type_sed
implicit none

integer            ,intent(in)   :: method
real(rk)           ,intent(in)   :: dt
type(type_sed)     ,intent(inout):: sedi
real(rk)           ,intent(in)   :: bdys(1:_INUM_,1:_JNUM_,1:sedi%nvar+1)
real(rk)           ,intent(inout):: fluxes(1:_INUM_,1:_JNUM_,1:sedi%nvar)


interface
   subroutine get_rhs(sed,bdys,fluxes,rhs)
   use fabm_sediment_driver, only: type_sed
      integer, parameter                   :: rk=selected_real_kind(12)
      type(type_sed), intent(inout)        :: sed
      real(rk), intent(in)                 :: bdys(1:_INUM_,1:_JNUM_,1:sed%nvar+1)
      real(rk), intent(inout)              :: fluxes(1:_INUM_,1:_JNUM_,1:sed%nvar)
      real(rk), intent(out)                :: rhs(1:_INUM_,1:_JNUM_,1:_KNUM_,1:sed%nvar)
   end
end interface

logical  :: first
real(rk),dimension(1:1,1:1,1:_KNUM_,1:sedi%nvar) :: rhs,rhs1,rhs2,rhs3
real(rk),target :: c1(1:1,1:1,1:_KNUM_,1:sedi%nvar)
real(rk),dimension(:,:,:,:),pointer :: c_pointer
integer  :: i,ci

!write(0,*) ' entered ode_solver'

select case (method)
case default
   ! Runge-Kutta-4th_order
   first=.true.
   c_pointer => sedi%conc
   call get_rhs(sedi,bdys,fluxes,rhs)
   first=.false.

   do i=1,sedi%nvar
      do ci=1,_KNUM_
         c1(1,1,ci,i)=sedi%conc(1,1,ci,i)+dt*rhs(1,1,ci,i)
      end do
   end do

   sedi%conc => c1
   call get_rhs(sedi,bdys,fluxes,rhs1)

   do i=1,sedi%nvar
      do ci=1,_KNUM_
         c1(1,1,ci,i)=c_pointer(1,1,ci,i)+dt*rhs1(1,1,ci,i)
      end do
   end do

   call get_rhs(sedi,bdys,fluxes,rhs2)

   do i=1,sedi%nvar
      do ci=1,_KNUM_
         c1(1,1,ci,i)=c_pointer(1,1,ci,i)+dt*rhs2(1,1,ci,i)
      end do
   end do

   call get_rhs(sedi,bdys,fluxes,rhs3)

   do i=1,sedi%nvar
      do ci=1,_KNUM_
         c_pointer(1,1,ci,i)=c_pointer(1,1,ci,i)+dt*1_rk/3_rk &
          *(0.5_rk*rhs(1,1,ci,i)+rhs1(1,1,ci,i)+rhs2(1,1,ci,i) &
          +0.5_rk*rhs3(1,1,ci,i))
      end do
   end do
   sedi%conc => c_pointer
   nullify(c_pointer)
end select

return
end subroutine ode_solver


end module esmf_fabm_sediment_component
