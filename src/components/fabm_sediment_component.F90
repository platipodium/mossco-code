!> @brief FABM sediment ESMF component
!
!> The ESMF/FABM sediment driver component module provides infrastructure for the
!! MOSSCO sediment component.
!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2013, 2014, Helmholtz-Zentrum Geesthacht 
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
  use mossco_strings
  use mossco_state

  implicit none

  private
 
  real(rk)  :: dzmin,dt
  integer   :: t,tnum,funit,output,k,n,numyears,numlayers
  integer   :: ode_method=_ADAPTIVE_EULER_
  integer   :: presimulation_years=-1
  real(rk),dimension(:,:,:,:),allocatable,target :: conc
  real(rk),dimension(:,:,:),pointer              :: diag
  real(rk),dimension(:,:,:),allocatable,target   :: bdys,fluxes
  real(rk),dimension(:,:),pointer   :: fptr2d
 
  type(type_sed),save :: sed
  type(ESMF_Alarm),save :: outputAlarm

  namelist /run_nml/ numyears,dt,output,numlayers,dzmin,ode_method,presimulation_years
 
  public :: SetServices,bdys,fluxes,rk
  
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
    character(len=ESMF_MAXSTR) :: string,fileName
    type(ESMF_Config)     :: config
    type(ESMF_FieldBundle) :: fieldBundle(3)
    type(ESMF_Field), allocatable, dimension(:) :: fieldList
    type(ESMF_Field)     :: field
    type(ESMF_Array)     :: array
    integer              :: i
    type(ESMF_DistGrid)  :: distGrid_3d,distGrid_2d
    type(ESMF_Grid)      :: state_grid,flux_grid
    type(ESMF_ArraySpec) :: flux_array,state_array

    real(ESMF_KIND_R8),dimension(:,:),pointer :: ptr_f2
    real(ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr_f3
    real(ESMF_KIND_R8),dimension(:,:,:,:),pointer :: ptr_f4
    integer(ESMF_KIND_I4) :: fieldcount
    integer(ESMF_KIND_I4) :: lbnd2(2),ubnd2(2),lbnd3(3),ubnd3(3)
    integer(ESMF_KIND_I8) :: tidx
  
    character(len=ESMF_MAXSTR) :: timestring, name, message
    integer(ESMF_KIND_I4)      :: localPet, petCount, itemCount
    type(ESMF_Clock)           :: clock
    type(ESMF_Time)            :: currTime, startTime, stopTime
    integer(ESMF_KIND_I8)      :: seconds, advanceCount
    type(ESMF_TimeInterval)    :: timeStep

    call ESMF_GridCompGet(gridComp, name=name, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  
    ! Create a local clock, set its parameters to those of the parent clock
    clock = ESMF_ClockCreate(parentClock, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    call ESMF_ClockSet(clock, name=trim(name)//' clock', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    call ESMF_GridCompSet(gridComp, clock=clock, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_ClockGet(clock,currTime=currTime, startTime=startTime, &
      stopTime=stopTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(timestring)//' '//trim(name)//' initializing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    !! read namelist input for control of time, this should not be done like this,
    !! but handled outside the component.  Maybe later introduce a local clock 
    open(33,file='run_sed.nml',action='read',status='old')
    read(33,nml=run_nml)

    !! Set the time step end stop time
    call ESMF_TimeIntervalSet(timeInterval,s_r8=dt,rc=rc)
    call ESMF_ClockSet(clock,timeStep=timeInterval,rc=rc)
 
    !! also from namelist, the output timesteop is read and
    !! used to create an alarm
    call ESMF_TimeIntervalSet(alarmInterval,s_i8=int(dt*output,kind=ESMF_KIND_I8),rc=rc)
!    outputAlarm = ESMF_AlarmCreate(clock,ringTime=startTime+alarmInterval, &
!      name=trim(name)//' outputAlarm', ringInterval=alarmInterval,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! The grid specification should also go to outside this routine, and update the grid of
    !! this component, numlayers and dzmin are read from nml
    sed%grid%inum=1
    sed%grid%jnum=1
    sed%grid%knum=numlayers
    sed%grid%dzmin=dzmin
    !! Write log entries
    write(message,'(A,I3,A)') 'Initialise grid with ',sed%grid%knum,' vertical layers'
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
    call sed%grid%init_grid()
    call sed%initialize()
    close(33)
    !! Allocate all arrays conc, bdys, fluxes 
    allocate(conc(_INUM_,_JNUM_,_KNUM_,sed%nvar)) 
    ! link conc to fabm_sediment_driver
    sed%conc => conc
    ! initialise values
    conc = 0.0_rk
    call sed%init_concentrations()
    !> Allocate boundary conditions and initialize with zero
    allocate(bdys(_INUM_,_JNUM_,sed%nvar+1))
    bdys(1:_INUM_,1:_JNUM_,1:9) = 0.0_rk
    allocate(fluxes(_INUM_,_JNUM_,sed%nvar))
    fluxes(_IRANGE_,_JRANGE_,1:8) = 0.0_rk

    call set_boundary_flags(sed,importState)
    call get_boundary_conditions(sed,importState,bdys,fluxes)
    !> create list of state variables for export
    call sed%get_all_export_states()

    !> run for some years into quasi-steady-state
    if (presimulation_years.gt.0) &
      write(0,*) '  postinit run sediment model on initial profiles for ',presimulation_years,' years'
    dt=3600.0_rk
    sed%bdys   => bdys
    sed%fluxes => fluxes
    do tidx=1,int(presimulation_years*365*24/(dt/3600.0_rk),kind=ESMF_KIND_I8)
      call ode_solver(sed,dt,ode_method)
    end do
    !> use flux-boundary condition for dissolved variables as calculated in get_boundary_conditions
    !> after presimulation
    sed%bcup_dissolved_variables = 1

    !! define an output unit for tsv output, TODO: add netcdf output for this
    !! netcdf output currently not working (see commented code below)
    funit=2
    open(funit,file='output.dat')
    write(funit,fmt='(A,A,A,A)',advance='no') 'time(s) ','depth(m) ','layer-height(m) ','porosity() '
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

    call ESMF_ArraySpecSet(flux_array, rank=2, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ArraySpecSet(state_array, rank=3, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    flux_grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/_INUM_,_JNUM_/), &
      regDecomp=(/1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
      name="sediment fluxes grid",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
      coorddep2=(/2/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    state_grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/_INUM_,_JNUM_,sed%grid%knum/), &
      regDecomp=(/1,1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
      name="sediment states grid",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
      coorddep2=(/2/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    ! put concentration array into export state
    ! it might be enough to do this once in initialize(?)
    do n=1,size(sed%export_states)
      field = ESMF_FieldCreate(state_grid,state_array, &
                         name=trim(sed%export_states(n)%standard_name), &
                         staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field=field, localDe=0, farrayPtr=ptr_f3, &
                       totalLBound=lbnd3,totalUBound=ubnd3, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      ptr_f3 = sed%export_states(n)%data ! initialize with 0.0
      call ESMF_StateAddReplace(exportState,(/field/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      !> add boundary upward fluxes
      field = ESMF_FieldCreate(flux_grid,flux_array, &
                         name=trim(sed%export_states(n)%standard_name)//'_upward_flux', &
                         staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field=field, localDe=0, farrayPtr=ptr_f2, &
                       totalLBound=lbnd2,totalUBound=ubnd2, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      ptr_f2 = -fluxes(:,:,sed%export_states(n)%fabm_id)
      call ESMF_StateAddReplace(exportState,(/field/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    end do
    do n=1,size(sed%model%info%diagnostic_variables)
      diag => sed%diagnostic_variables(n)
      array = ESMF_ArrayCreate(distgrid=distgrid_3d,farrayPtr=diag, &
                   name=only_var_name(sed%model%info%diagnostic_variables(n)%long_name), rc=rc)
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
    type(ESMF_Field)  :: field
    real(ESMF_KIND_R8),pointer,dimension(:,:) :: ptr_f2
    real(ESMF_KIND_R8),pointer,dimension(:,:,:) :: ptr_f3
    integer           :: fieldcount
    integer(8)     :: t
    character(len=ESMF_MAXSTR)  :: string
 
    character(len=ESMF_MAXSTR) :: timestring, name, message
    integer(ESMF_KIND_I4)      :: localPet, petCount, itemCount
    type(ESMF_Clock)           :: clock
    type(ESMF_Time)            :: currTime, startTime, stopTime
    integer(ESMF_KIND_I8)      :: seconds, advanceCount
    type(ESMF_TimeInterval)    :: timeStep

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
 
    call ESMF_ClockGet(clock,startTime=startTime, currTime=currTime, &
      stopTime=stopTime, advanceCount=advanceCount, timeStep=timeStep, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' running'
     
    call ESMF_TimeGet(stopTime,timeStringISOFrac=timestring, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_TimeIntervalGet(timeStep, s_r8=dt, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    write(message,'(A,A,F6.0,A)') trim(message)//' with ', &
      ' steps of ',dt,' s to '//trim(timestring)
!#ifdef DEBUG
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
!#endif

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
    !! get the current advance count (formerly t) from clock
#if 0
    if (ESMF_AlarmIsRinging(outputAlarm)) then
      call ESMF_AlarmRingerOff(outputAlarm,rc=rc)
      call ESMF_ClockGet(clock,advanceCount=t)
      write(string,'(A,F7.1,A)') 'Elapsed ',t*dt/86400,' days'
      write(*,'(A,F7.1,A)') 'Elapsed ',t*dt/86400,' days'
      call ESMF_LogWrite(string,ESMF_LOGMSG_INFO)
      write(funit,*) t*dt,'fluxes',fluxes(1,1,:)
      do k=1,_KNUM_
          write(funit,FMT='(E15.3,A,E15.4E3,A,E15.4E3,A,E15.4E3)',advance='no') &
            t*dt,' ',sed%grid%zc(1,1,k),' ',sed%grid%dz(1,1,k),  &
            ' ',sed%porosity(1,1,k)
          do n=1,sed%nvar
             write(funit,FMT='(A,E15.4E3)',advance='no') ' ',conc(1,1,k,n)
          end do
          do n=1,size(sed%model%info%diagnostic_variables)
             diag => sed%diagnostic_variables(n)
             write(funit,FMT='(A,E15.4E3)',advance='no') ' ',diag(1,1,k)
          end do
          write(funit,*)
      end do
    endif
#endif

    ! write back fluxes into export State
    
    do n=1,size(sed%export_states)
      call ESMF_StateGet(exportState, &
             trim(sed%export_states(n)%standard_name), &
             field,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field=field, localDe=0, farrayPtr=ptr_f3, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      ptr_f3 = conc(:,:,:,sed%export_states(n)%fabm_id)
      call ESMF_StateGet(exportState, &
             trim(sed%export_states(n)%standard_name)//'_upward_flux', &
             field,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field=field, localDe=0, farrayPtr=ptr_f2, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      ptr_f2 = -fluxes(:,:,sed%export_states(n)%fabm_id)
    end do
 
    if (allocated(fieldList)) deallocate(fieldlist)
    
    do while (.not.ESMF_ClockIsStopTime(clock))
      call ESMF_ClockAdvance(clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo
    
    call ESMF_GridCompGet(gridComp, name=name, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(timestring)//' '//trim(name)//' finished running'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    
  
  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    close(funit)

    call sed%finalize()
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
    real(ESMF_KIND_R8),pointer,dimension(:,:,:)  :: ptr_f3,ptr_vs
    type(ESMF_Field)    :: field,vs_field
    type(ESMF_Array)    :: array,vs_array
    integer             :: i,rc,itemcount
    character(len=ESMF_MAXSTR) :: string
    character(len=ESMF_MAXSTR) :: varname
    real(rk),dimension(_IRANGE_,_JRANGE_),target :: vs,pom

    call ESMF_StateGet(importState,itemSearch="water_temperature",itemCount=itemcount,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (itemcount==0) then
#ifdef DEBUG
      write(string,'(A)') "No temperature information found, using default value 10 deg_C"
      call ESMF_LogWrite(string,ESMF_LOGMSG_INFO)
#endif
      bdys(1:_INUM_,1:_JNUM_,1) = 10._rk   ! degC temperature
    else 
      call ESMF_StateGet(importState,"water_temperature",field,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
#ifdef DEBUG
      write(string,'(A)') "Water temperature information found"
      call ESMF_LogWrite(string,ESMF_LOGMSG_INFO)
#endif
      ptr_f2 => bdys(:,:,1)
      call ESMF_FieldGet(field,farrayPtr=ptr_f3,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      ptr_f2 = ptr_f3(:,:,1)   ! get lowest vertical index for near-bed temperature
    endif

    do i=1,sed%nvar
      if (sed%model%info%state_variables(i)%standard_variable%name/='') then
        varname = &
          trim(sed%model%info%state_variables(i)%standard_variable%name)
      else
      !> otherwise use CF-ed version of long_name
        varname = trim(only_var_name( &
           sed%model%info%state_variables(i)%long_name))
      end if
      call ESMF_StateGet(importState,itemSearch=trim(varname),itemCount=itemcount,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      if (itemcount==0) then
#ifdef DEBUG
        write(string,'(A)') "Variable '"//trim(varname)//"' not found in State. Skipping."
        call ESMF_LogWrite(string,ESMF_LOGMSG_INFO)
#endif
      else
        call ESMF_StateGet(importState,trim(varname),field,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
#ifdef DEBUG
        if (rc == ESMF_SUCCESS) write(0,*) 'found field ',trim(varname)
#endif

        if (sed%model%info%state_variables(i)%properties%get_logical( &
            'particulate',default=.false.)) then
          !write(0,*) 'try to get ',trim(varname)//'_z_velocity'
          call ESMF_StateGet(importState,trim(varname)//'_z_velocity',vs_field,rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT,rc=rc)
          call ESMF_FieldGet(field,farrayPtr=ptr_f3,rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

          call ESMF_FieldGet(vs_field,farrayPtr=ptr_vs,rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

          fluxes(_IRANGE_,_JRANGE_,i) = -ptr_f3(:,:,1)*ptr_vs(:,:,1) ! downward flux is positive
        else
          ptr_f2 => bdys(:,:,i+1)
          call ESMF_FieldGet(field,farrayPtr=ptr_f3,rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          ptr_f2 = ptr_f3(:,:,1) ! get lowest vertical index for near-bed concentration
          fluxes(_IRANGE_,_JRANGE_,i) = -(sed%conc(:,:,1,i)-bdys(:,:,i+1))/ &
            sed%grid%dz(:,:,1)*(sed%diffusivity+bdys(:,:,1)*0.035d0)*sed%porosity(:,:,1)/86400._rk/10000._rk
        end if
      endif
    end do

  end subroutine get_boundary_conditions

  !> set ESMF attributes "required_flag", "required" and "optional" for
  !! all boundary conditions in the importState
  subroutine set_boundary_flags(sed,state)
    type(type_sed)             :: sed
    type(ESMF_State)           :: state
    character(len=ESMF_MAXSTR) :: name,varname
    integer                    :: n

    name='water_temperature'
    call set_item_flags(state,name,requiredFlag=.true.,requiredRank=3)

    do n=1,size(sed%model%info%state_variables)
      if (sed%model%info%state_variables(n)%standard_variable%name/='') then
        varname = &
          trim(sed%model%info%state_variables(n)%standard_variable%name)
      else
      !> otherwise use CF-ed version of long_name
        varname = trim(only_var_name( &
           sed%model%info%state_variables(n)%long_name))
      end if
      call set_item_flags(state,varname,requiredFlag=.true.,requiredRank=3)

      if (sed%model%info%state_variables(n)%properties%get_logical( &
            'particulate',default=.false.)) then
        name = trim(varname)//'_z_velocity'
        call set_item_flags(state,name,requiredFlag=.true.,requiredRank=3)
      end if
    end do
  end subroutine set_boundary_flags

end module fabm_sediment_component
