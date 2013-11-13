!> @brief ESMF/FABM sediment driver component
!
!> The ESMF/FABM sediment driver component module provides infrastructure for the
!! MOSSCO sediment component.
!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2013, Helmholtz-Zentrum Geesthacht 
!> @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister, Helmholtz-Zentrum Geesthacht
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
#include "fabm_driver.h"
#define _GRID_ sed%grid
#define _INUM_ _GRID_%inum
#define _JNUM_ _GRID_%jnum
#define _KNUM_ _GRID_%knum
#define _IRANGE_ 1:_INUM_
#define _JRANGE_ 1:_JNUM_
#define _KRANGE_ 1:_KNUM_

#define _RK4_ 1
#define _ADAPTIVE_EULER_ 2

module fabm_sediment_component

  use esmf
  use fabm
  use fabm_sediment_driver
  use solver_library!, only : ode_solver

  implicit none

  private
 
  real(rk)  :: dzmin,dt
  integer   :: t,tnum,funit,output,k,n,numyears,numlayers
  integer   :: ode_method=_ADAPTIVE_EULER_
  real(rk),dimension(:,:,:,:),allocatable,target :: conc
  real(rk),dimension(:,:,:),pointer              :: diag
  real(rk),dimension(:,:,:),allocatable,target   :: bdys,fluxes
  real(rk),dimension(:,:),pointer   :: fptr2d
 
  type(type_sed),save :: sed
  type(ESMF_Alarm),save :: outputAlarm

  namelist /run_nml/ numyears,dt,output,numlayers,dzmin,ode_method
 
  public :: SetServices
  
  contains

  !> Provide an ESMF compliant SetServices routine, which defines
  !! the entry points for Init/Run/Finalize

  subroutine SetServices(gridcomp, rc)
  
    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine SetServices

  !> Initialize the component
  !!
  !! Allocate memory for boundaries and fluxes, create ESMF fields
  !! and export them
  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    type(ESMF_TimeInterval) :: timeInterval,alarmInterval
    type(ESMF_Time)         :: startTime
    character(len=ESMF_MAXSTR) :: string,fileName
    type(ESMF_Config)     :: config
    type(ESMF_FieldBundle) :: fieldBundle(3)
    type(ESMF_Field), allocatable, dimension(:) :: fieldList
    type(ESMF_Field)     :: field
    type(ESMF_Array)     :: array
    integer              :: i
    type(ESMF_DistGrid)  :: distGrid_3d,distGrid_2d
    type(ESMF_Grid)      :: grid
    type(ESMF_ArraySpec) :: arraySpec

    real(ESMF_KIND_R8),dimension(:,:),pointer :: ptr_f2
    real(ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr_f3
    real(ESMF_KIND_R8),dimension(:,:,:,:),pointer :: ptr_f4
    integer(ESMF_KIND_I4) :: itemcount,fieldcount
  
    call ESMF_LogWrite('Initializing FABM sediment module',ESMF_LOGMSG_INFO)
     !! read namelist input for control of time, this should not be done like this,
    !! but handled outside the component.  Maybe later introduce a local clock 
    open(33,file='run_sed.nml',action='read',status='old')
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
    call ESMF_TimeIntervalSet(alarmInterval,s_i8=int(dt*output,kind=ESMF_KIND_I8),rc=rc)
    outputAlarm = ESMF_AlarmCreate(clock=parentClock,ringTime=startTime+alarmInterval,ringInterval=alarmInterval,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! The grid specification should also go to outside this routine, and update the grid of
    !! this component, numlayers and dzmin are read from nml
    sed%grid%inum=1
    sed%grid%jnum=1
    sed%grid%knum=numlayers
    sed%grid%dzmin=dzmin
    !! Write log entries
    write(string,'(A,I3,A)') 'Initialise grid with ',sed%grid%knum,' vertical layers'
    call ESMF_LogWrite(string,ESMF_LOGMSG_INFO)
    call init_sed_grid(sed%grid)
    call init_fabm_sed(sed)
    close(33)
    !! Allocate all arrays conc, bdys, fluxes 
    allocate(conc(_INUM_,_JNUM_,_KNUM_,sed%nvar)) 
    ! link conc to fabm_sediment_driver
    sed%conc => conc
    ! initialise values
    conc = 0.0_rk
    call init_fabm_sed_concentrations(sed)
    !> Allocate boundary conditions and initialize with zero
    allocate(bdys(_INUM_,_JNUM_,sed%nvar+1))
    bdys(1:_INUM_,1:_JNUM_,1:9) = 0.0_rk
    allocate(fluxes(_INUM_,_JNUM_,sed%nvar))
    fluxes(_IRANGE_,_JRANGE_,1:8) = 0.0_rk

    call get_boundary_conditions(sed,importState,bdys,fluxes)

    !! define an output unit for tsv output, TODO: add netcdf output for this
    !! netcdf output currently not working (see commented code below)
    funit=2
    open(funit,file='output.dat')
    write(funit,fmt='(A,A,A)',advance='no') 'time(s) ','depth(m) ','layer-height(m) '
    do n=1,sed%nvar
      write(funit,fmt='(A,A)',advance='no') ' ',trim(sed%model%info%state_variables(n)%name)
    end do
    do n=1,size(sed%model%info%diagnostic_variables)
      write(funit,fmt='(A,A)',advance='no') ' ',trim(sed%model%info%diagnostic_variables(n)%name)
    end do
    write(funit,*)

    distGrid_3d =  ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/1,1,sed%grid%knum/), &
                                    indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    distGrid_2d =  ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/1,1,1/), &
                                    indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    ! put concentration array into export state
    ! it might be enough to do this once in initialize(?)
    do n=1,sed%nvar
      array = ESMF_ArrayCreate(distgrid=distgrid_3d,farray=conc(:,:,:,i), &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   name=trim(sed%model%info%state_variables(n)%long_name), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_StateAddReplace(exportState,(/array/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    end do
    do n=1,size(sed%model%info%diagnostic_variables)
      diag => fabm_sed_diagnostic_variables(sed,n)
      array = ESMF_ArrayCreate(distgrid=distgrid_3d,farrayPtr=diag, &
                   name=trim(sed%model%info%diagnostic_variables(n)%long_name), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_StateAddReplace(exportState,(/array/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    end do
    do n=1,sed%nvar
      write(string,*) trim(sed%model%info%state_variables(n)%long_name)//'_upward_flux'
      array = ESMF_ArrayCreate(distgrid=distgrid_2d,farray=fluxes(:,:,n), &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   name=trim(string), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_StateAddReplace(exportState,(/array/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    end do
    !call ESMF_StatePrint(exportState)

    call ESMF_LogWrite('Initialized FABM sediment module',ESMF_LOGMSG_INFO)

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
  
#ifdef DEBUG 
    call ESMF_LogWrite("FABM-Sediment run at "//timestring1 &
      //" ("//timestring2//")", ESMF_LOGMSG_INFO)
#endif

#if 0
    !call ESMF_StatePrint(importState,options="long",nestedFlag=.true.,rc=rc)
    call ESMF_StateGet(importState,"FABM field bundle",fieldBundle,rc=rc)
    !call ESMF_FieldBundlePrint(fieldBundle,rc=rc)
    call ESMF_FieldBundleGet(fieldBundle,fieldCount=fieldcount,rc=rc)
    allocate(fieldlist(fieldcount))
    call ESMF_FieldBundleGet(fieldBundle,fieldList=fieldlist,rc=rc)
    call ESMF_FieldGet(fieldlist(1),name=name,rc=rc)
#endif

    call get_boundary_conditions(sed,importState,bdys,fluxes)

    !! Get integration time step from parent clock
    call ESMF_ClockGet(parentClock,timeStep=timeInterval,rc=rc)
    call ESMF_TimeIntervalGet(timeInterval,s_r8=dt)
    sed%bdys   => bdys
    sed%fluxes => fluxes
    call ode_solver(sed,dt,ode_method)

   ! reset concentrations to mininum_value
     do n=1,sed%nvar
       do k=1,sed%grid%knum
         if (sed%conc(1,1,k,n) .lt. sed%model%info%state_variables(n)%minimum) then
            sed%conc(_IRANGE_,_JRANGE_,k,n) = sed%model%info%state_variables(n)%minimum
         end if
       end do
     end do

     !@TODO write fluxes into export State

    !@ TODO remove output and implement throughh ESMF in NetCDF:
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
          write(funit,FMT='(E15.3,A,E15.4E3,A,E15.4E3)',advance='no') t*dt,' ',sed%grid%zc(1,1,k),' ',sed%grid%dz(1,1,k)
          do n=1,sed%nvar
             write(funit,FMT='(A,E15.4E3)',advance='no') ' ',conc(1,1,k,n)
          end do
          do n=1,size(sed%model%info%diagnostic_variables)
             diag => fabm_sed_diagnostic_variables(sed,n)
             write(funit,FMT='(A,E15.4E3)',advance='no') ' ',diag(1,1,k)
          end do
          write(funit,*)
      end do
    endif
 
    if (allocated(fieldList)) deallocate(fieldlist)

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    close(funit)

    call finalize_fabm_sed()
    if (allocated(conc)) deallocate(conc)
    if (allocated(bdys)) deallocate(bdys)
    if (allocated(fluxes)) deallocate(fluxes)

    call ESMF_AlarmDestroy(outputAlarm,rc=rc)
    !@TODO destroy all items in import and export state

  end subroutine Finalize

  subroutine get_boundary_conditions(sed,importState,bdys,fluxes)
    
    real(rk),dimension(:,:,:),target :: bdys,fluxes
    type(type_sed)      :: sed
    type(ESMF_State)    :: importState
    real(ESMF_KIND_R8),pointer,dimension(:,:)  :: ptr_f2
    type(ESMF_Field)    :: Field
    type(ESMF_Array)    :: array
    integer             :: i,rc,itemcount
    character(len=ESMF_MAXSTR) :: string

    call ESMF_StateGet(importState,itemSearch="water_temperature",itemCount=itemcount,rc=rc)
    if (itemcount==0) then
      write(string,'(A)') "No temperature information found, using default value 10 deg_C"
      call ESMF_LogWrite(string,ESMF_LOGMSG_INFO)
      bdys(1:_INUM_,1:_JNUM_,1) = 10._rk   ! degC temperature
    else 
      call ESMF_StateGet(importState,"water_temperature",field,rc=rc)
      write(string,'(A)') "Water temperature information found"
      call ESMF_LogWrite(string,ESMF_LOGMSG_INFO)
      !call ESMF_FieldGet(field,farrayPtr=fptr2d,rc=rc) !> @todo SEGFAULT
      !bdys(:,:,1) = fptr2d   ! degC temperature
    endif

    do i=1,sed%nvar
      call ESMF_StateGet(importState, &
        trim(sed%model%info%state_variables(i)%long_name),array,rc=rc)
      if(rc /= ESMF_SUCCESS) then
        !call ESMF_LogWrite("variable not found",ESMF_LOGMSG_INFO)
      else
        if (sed%model%info%state_variables(n)%properties%get_logical( &
            'particulate',default=.false.)) then
          ptr_f2 => fluxes(_IRANGE_,_JRANGE_,i)
        else
          ptr_f2 => bdys(_IRANGE_,_JRANGE_,i+1)
        end if
          call ESMF_ArrayGet(array,farrayPtr=ptr_f2,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      endif
    end do

  end subroutine get_boundary_conditions


end module fabm_sediment_component
