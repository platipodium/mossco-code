#include <cppdefs.h>
#include "fabm_driver.h"
#define _GRID_ sed%grid
#define _INUM_ _GRID_%inum
#define _JNUM_ _GRID_%jnum
#define _KNUM_ _GRID_%knum

#define _RK4_ 1
#define _ADAPTIVE_EULER_ 2

module esmf_fabm_sediment_component

  use esmf
  use fabm
  use fabm_types  ! for realkind
  use fabm_sediment_driver
  use solver_library!, only : ode_solver

  implicit none

  private
 
  real(rk)  :: dzmin,dt
  integer   :: t,tnum,funit,output,k,n,numyears,numlayers
  real(rk),dimension(:,:,:,:),allocatable,target :: conc
  real(rk),dimension(:,:,:),allocatable,target   :: bdys,fluxes
 
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
    character(len=ESMF_MAXSTR) :: string,filename
    type(ESMF_Config)     :: config
    type(ESMF_FieldBundle) :: fieldbundle(3)
    type(ESMF_Field), allocatable, dimension(:) :: fieldlist
    type(ESMF_Array)     :: array
    integer              :: fieldcount,i
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_Grid)      :: grid
    type(ESMF_ArraySpec) :: arrayspec

    real(ESMF_KIND_R8),dimension(:,:),pointer :: ptr_f2
    real(ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr_f3
    real(ESMF_KIND_R8),dimension(:,:,:,:),pointer :: ptr_f4
  
    !! read namelist input for control of time, this should not be done like this,
    !! but handled outside the component.  Maybe later introduce a local clock 
    open(33,file='run.nml',action='read',status='old')
    read(33,nml=run_nml)

    !config = ESMF_ConfigCreate(rc=rc) 
    !call ESMF_ConfigDestroy(config, rc=rc)

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

    call ESMF_LogWrite('Initialise sediment module',ESMF_LOGMSG_INFO)
    call init_fabm_sed(sed)
    close(33)

    !! Allocate all arrays conc, bdys, fluxes 
    call ESMF_LogWrite('Allocate arrays',ESMF_LOGMSG_INFO)
    
    allocate(conc(_INUM_,_JNUM_,_KNUM_,sed%nvar))
    ! link conc to fabm_sediment_driver
    sed%conc => conc
    ! initialise values
    conc = 0.0_rk
    call init_fabm_sed_concentrations(sed)

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
    !! netcdf output currently not working (see commented code below)
    funit=2
    open(funit,file='output.dat')
    write(funit,*) 'time(s) ','depth(m) ','conc(n) '

    call ESMF_GridCompGet(gridComp,grid=grid,rc=rc)
    call ESMF_GridGet(grid,distgrid=distgrid,rc=rc)

    !! Define the 2D boundary condition fields and collect them in a FieldBundle
    fieldcount=sed%nvar+1
    allocate(fieldlist(fieldcount))
    ptr_f2 =>   bdys(:,:,1)
    fieldlist(1) = ESMF_FieldCreate(grid,ptr_f2,staggerloc &
                 = ESMF_STAGGERLOC_CENTER, name="Temperature",rc=rc)
    ptr_f2 =>   bdys(:,:,5)
    fieldlist(5) = ESMF_FieldCreate(grid,ptr_f2,staggerloc &
                 = ESMF_STAGGERLOC_CENTER, name="Phosphate",rc=rc)
    ptr_f2 =>   bdys(:,:,6)
    fieldlist(6) = ESMF_FieldCreate(grid,ptr_f2,staggerloc &
                 = ESMF_STAGGERLOC_CENTER, name="Nitrate",rc=rc)
    ptr_f2 =>   bdys(:,:,7)
    fieldlist(7) = ESMF_FieldCreate(grid,ptr_f2,staggerloc &
                 = ESMF_STAGGERLOC_CENTER, name="Ammonia",rc=rc)
    ptr_f2 =>   bdys(:,:,8)
    fieldlist(8) = ESMF_FieldCreate(grid,ptr_f2,staggerloc &
                 = ESMF_STAGGERLOC_CENTER, name="Oxygen",rc=rc)
    ptr_f2 =>   bdys(:,:,9)
    fieldlist(9) = ESMF_FieldCreate(grid,ptr_f2,staggerloc &
                 = ESMF_STAGGERLOC_CENTER, name="odu",rc=rc)
    fieldbundle(1) = ESMF_FieldBundleCreate(name="Boundary conditions",rc=rc)
    call ESMF_FieldBundleAdd(fieldbundle(1),fieldlist((/1,5,6,7,8,9/)),rc=rc)
    deallocate(fieldlist)

    !! Define the 2D flux condition fields and collect them in a FieldBundle
    fieldcount=sed%nvar
    allocate(fieldlist(fieldcount))
    ptr_f2 =>   fluxes(:,:,1)
    fieldlist(1) = ESMF_FieldCreate(grid,ptr_f2,staggerloc &
                 = ESMF_STAGGERLOC_CENTER, name="fdet",rc=rc)
    ptr_f2 =>   fluxes(:,:,2)
    fieldlist(2) = ESMF_FieldCreate(grid,ptr_f2,staggerloc &
                 = ESMF_STAGGERLOC_CENTER, name="sdet",rc=rc)
    ptr_f2 =>   bdys(:,:,3)
    fieldlist(3) = ESMF_FieldCreate(grid,ptr_f2,staggerloc &
                 = ESMF_STAGGERLOC_CENTER, name="pdet",rc=rc)
    fieldbundle(2) = ESMF_FieldBundleCreate(name="Flux conditions",rc=rc)
    call ESMF_FieldBundleAdd(fieldbundle(2),fieldlist((/1,2,3/)),rc=rc)
    deallocate(fieldlist)

    !! Define the 3d concentrations and collect them in FieldBundle
    fieldcount=sed%nvar
    allocate(fieldlist(fieldcount))
    do i=1,fieldcount
      ptr_f3 =>  conc(:,:,:,i)
      write(string,'(A,I0.3)') 'Var',i
      fieldlist(i) = ESMF_FieldCreate(grid,ptr_f3,staggerloc &
                   = ESMF_STAGGERLOC_CENTER, name=trim(string),rc=rc)
    enddo
    fieldbundle(3) = ESMF_FieldBundleCreate(name="Concentrations",rc=rc)
    call ESMF_FieldBundleAdd(fieldbundle(3),fieldlist,rc=rc)
    !call ESMF_FieldWrite(fieldlist(1),"initialization_state.nc",iofmt=ESMF_IOFMT_NETCDF,rc=rc)
    !call ESMF_FieldBundleWrite(fieldbundle(1),"initialization_state.nc",singleFile=.true.,iofmt=ESMF_IOFMT_NETCDF,rc=rc)
    if (rc  /= ESMF_SUCCESS ) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    deallocate(fieldlist)

    call ESMF_StateAdd(exportState,fieldbundle,rc=rc)

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
    type(ESMF_Field), allocatable, dimension(:) :: fieldlist
    integer           :: fieldcount,ode_method
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
    call ESMF_FieldBundleGet(fieldBundle,fieldCount=fieldcount,rc=rc)
    allocate(fieldlist(fieldcount))
    call ESMF_FieldBundleGet(fieldBundle,fieldList=fieldlist,rc=rc)
    call ESMF_FieldGet(fieldlist(1),name=name,rc=rc)
  
    !! Get integration time step from parent clock
    call ESMF_ClockGet(parentClock,timeStep=timeInterval,rc=rc)
    call ESMF_TimeIntervalGet(timeInterval,s_r8=dt)
   ode_method=_ADAPTIVE_EULER_
   call ode_solver(sed,bdys,fluxes,dt,ode_method,fabm_sed_get_rhs)

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
 
    deallocate(fieldlist)

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


subroutine ode_solver_old(sedi,bdys,fluxes,dt,method,get_rhs)
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
end subroutine ode_solver_old


end module esmf_fabm_sediment_component
