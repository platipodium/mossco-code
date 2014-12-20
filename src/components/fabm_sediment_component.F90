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
  use fabm_sediment_driver
  use solver_library!, only : ode_solver
  use mossco_strings
  use mossco_state
  use mossco_component

  implicit none

  private
 
  real(rk)  :: dzmin,dt,dt_spinup
  real(rk)  :: dt_min=1.0e-8_rk,relative_change_min=-0.9_rk
  integer   :: t,tnum,funit,output=-1,k,n,numyears,numlayers
  integer   :: ode_method=_ADAPTIVE_EULER_
  integer   :: presimulation_years=-1
  integer   :: bcup_dissolved_variables=2
  real(rk),dimension(:,:,:,:),allocatable,target :: conc
  real(rk),dimension(:,:,:),pointer              :: diag
  real(rk),dimension(:,:,:),allocatable,target   :: bdys,fluxes
  real(rk),dimension(:,:),pointer   :: fptr2d
  real(rk),dimension(:), pointer    :: fluxmesh_ptr
  real(rk),dimension(:), pointer    :: fluxmesh_ptr_vs
  real(rk),dimension(:,:), pointer  :: statemesh_ptr
  character(len=ESMF_MAXSTR) :: ugrid_name=''
 
  type(type_sed),save :: sed

  namelist /run_nml/ numyears,dt,output,numlayers,dzmin,ode_method,presimulation_years, &
                     dt_min,relative_change_min,ugrid_name, output, &
                     bcup_dissolved_variables
 
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
    character(len=ESMF_MAXSTR) :: string,fileName,varname
    type(ESMF_Config)     :: config
    type(ESMF_FieldBundle) :: fieldBundle(3)
    type(ESMF_Field), allocatable, dimension(:) :: fieldList
    type(ESMF_Field)     :: field
    type(ESMF_Array)     :: array
    integer              :: i
    type(ESMF_DistGrid)  :: distGrid_3d,distGrid_2d
    type(ESMF_Grid)      :: state_grid,flux_grid
    type(ESMF_Mesh)      :: surface_mesh, state_mesh
    type(ESMF_ArraySpec) :: flux_array,state_array
    type(ESMF_Index_Flag):: indexflag

    real(ESMF_KIND_R8),dimension(:,:),pointer :: ptr_f2
    real(ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr_f3
    real(ESMF_KIND_R8),dimension(:,:,:,:),pointer :: ptr_f4
    integer(ESMF_KIND_I4) :: fieldcount
    integer(ESMF_KIND_I4) :: lbnd2(2),ubnd2(2),lbnd3(3),ubnd3(3)
    integer(ESMF_KIND_I8) :: tidx
    type(ESMF_Alarm)      :: outputAlarm
  
    character(len=ESMF_MAXSTR) :: timestring, name, message, units
    integer(ESMF_KIND_I4)      :: localPet, petCount, itemCount
    type(ESMF_Clock)           :: clock
    type(ESMF_Time)            :: currTime, startTime, stopTime
    integer(ESMF_KIND_I8)      :: seconds, advanceCount
    type(ESMF_TimeInterval)    :: timeStep
    logical                    :: clockIsPresent
    integer                    :: numElements,numNodes, exclusiveCount(2), rank
    character(len=ESMF_MAXSTR) :: foreignGridFieldName

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! read namelist input for control of timestepping  
    open(33,file='run_sed.nml',action='read',status='old')
    read(33,nml=run_nml)

    !! Set the time step end stop time
    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeIntervalSet(timeInterval, s_r8=dt, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockSet(clock, timeStep=timeInterval, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
 
    !! also from namelist, the output timestep is read and
    !! used to create an alarm
    !! no output, if output <= 0
    sed%do_output = output .gt. 0

#if 0
    if (sed%do_output) then
      call ESMF_TimeIntervalSet(alarmInterval,s_i8=int(dt*output,kind=ESMF_KIND_I8),rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      outputAlarm = ESMF_AlarmCreate(clock,ringTime=startTime+alarmInterval, &
        name='outputAlarm', ringInterval=alarmInterval,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    end if
#endif

    !! read ugrid mesh to get number of sediment columns
    sed%grid%use_ugrid = ugrid_name /= ''
    if (sed%grid%use_ugrid) sed%grid%type=UGRID
    !> todo: check importState for foreign_grid_field_name

    if (sed%grid%type==UGRID) then
      surface_mesh = ESMF_MeshCreate(meshname='sediment_surface_mesh', &
                                  filename=ugrid_name, &
                                  filetypeflag=ESMF_FILEFORMAT_UGRID,rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_MeshGet(surface_mesh,numOwnedElements=numElements,numOwnedNodes=numNodes)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      sed%grid%inum=numElements
      sed%grid%jnum=1
      write(message,*) trim(name)//': use unstructured grid, number of local elements:',numElements
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
    else
      call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
           value=foreignGridFieldName, defaultValue='none',rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      if (trim(foreignGridFieldName)=='none') then 
        sed%grid%type=LOCAL_GRID
      else
        sed%grid%type=FOREIGN_GRID
      endif
    end if

    if (sed%grid%type==FOREIGN_GRID) then
      ! get 2d grid from foreign_grid_field
      write(message,*) '  use foreign horizontal grid '//trim(foreignGridFieldName)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

      call ESMF_StateGet(importState,foreignGridFieldName, field, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field, grid=flux_grid, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field, rank=rank, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      if (rank/=2) then
        write(message,*) '  foreign horizontal grid rank <> 2'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      end if
      call ESMF_FieldGetBounds(field, exclusiveLBound=lbnd2, exclusiveUBound=ubnd2, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      sed%grid%inum=ubnd2(1)-lbnd2(1)+1
      sed%grid%jnum=ubnd2(2)-lbnd2(2)+1

    elseif (sed%grid%type==LOCAL_GRID) then
      write(message,*) '  use local 1x1 horizontal grid'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      sed%grid%inum=1
      sed%grid%jnum=1
    end if
    !! The grid specification should also go to outside this routine, and update the grid of
    !! this component, numlayers and dzmin are read from nml
    sed%grid%knum=numlayers
    sed%grid%dzmin=dzmin
    !! Write log entries
    write(message,*) trim(name)//' initialise grid with [inum x jnum x knum]',_INUM_,' x ',_JNUM_,' x ',_KNUM_
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
    !> create list of state variables for export
    call sed%get_all_export_states()

    !> run for some years into quasi-steady-state
    if (presimulation_years.gt.0) then 
      write(0,*) '  postinit run sediment model on initial profiles for ',presimulation_years,' years'
      write(message,'(A,I3,A)') trim(name)//' runs ', presimulation_years, ' spinup years'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
    endif
    
    dt_spinup=3600.0_rk
    sed%bdys   => bdys
    sed%fluxes => fluxes

    ! set solver_settings:
    sed%dt_min=dt_min
    sed%relative_change_min=relative_change_min

    ! set boundary conditions for pre-simulation
    bdys(:,:,1) = 5.0 !degC
    do i=1,size(sed%model%info%state_variables)
      varname = trim(only_var_name(sed%model%info%state_variables(i)%long_name))
      if (trim(varname) == 'dissolved_nitrate') bdys(:,:,i+1)=2.5
      if (trim(varname) == 'dissolved_ammonium') bdys(:,:,i+1)=2.5
      if (trim(varname) == 'dissolved_phosphate') bdys(:,:,i+1)=0.15
      if (trim(varname) == 'dissolved_oxygen') bdys(:,:,i+1)=250.
      if (trim(varname) == 'dissolved_reduced_substances') bdys(:,:,i+1)=0.0
      if (trim(varname) == 'fast_detritus_C') fluxes(:,:,i)=5.0_rk/86400.0_rk
      if (trim(varname) == 'slow_detritus_C') fluxes(:,:,i)=5.0_rk/86400.0_rk
      if (trim(varname) == 'detritus-P') fluxes(:,:,i)=0.08_rk/86400.0_rk
      !write(0,*) i,trim(only_var_name(sed%model%info%state_variables(i)%long_name)),bdys(:,:,i+1),fluxes(:,:,i)
    end do

    ! use Dirichlet boundary condition for pre-simulation
    sed%bcup_dissolved_variables = 2
    do tidx=1,int(presimulation_years*365*24/(dt_spinup/3600.0_rk),kind=ESMF_KIND_I8)
      call ode_solver(sed,dt_spinup,ode_method)
    end do
    !> it is possible to use flux-boundary condition for dissolved variables
    !> as calculated in get_boundary_conditions after presimulation,
    !> Dirichlet boundary conditions are numerically more stable.
    sed%bcup_dissolved_variables = bcup_dissolved_variables

    !! define an output unit for tsv output
    if (sed%do_output) then
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
    end if

    if (sed%grid%type==UGRID) then
      !! create state mesh
#if 0
      state_mesh = ESMF_MeshCreate(surface_mesh,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
#endif

      !! create state and flux fields
      do n=1,size(sed%export_states)
        field = ESMF_FieldCreate(surface_mesh, &
                  name=trim(sed%export_states(n)%standard_name)//'_in_soil', &
                  typekind=ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, &
                  ungriddedLBound=(/1/), ungriddedUBound=(/sed%grid%knum/), &
                  gridToFieldMap=(/1,2/), rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_AttributeSet(field, 'creator', trim(name), rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        call ESMF_FieldGet(field=field, farrayPtr=statemesh_ptr, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        do k=1,sed%grid%knum
          statemesh_ptr(k,:) = sed%export_states(n)%data(:,1,k)
        end do
        call ESMF_StateAddReplace(exportState,(/field/),rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        if (sed%export_states(n)%fabm_id/=-1) then
          !> add boundary upward fluxes
          field = ESMF_FieldCreate(surface_mesh, &
                    name=trim(sed%export_states(n)%standard_name)//'_upward_flux_at_soil_surface', &
                    typekind=ESMF_TYPEKIND_R8, &
                    meshloc=ESMF_MESHLOC_ELEMENT,rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          call ESMF_AttributeSet(field, 'creator', trim(name), rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          call ESMF_FieldGet(field=field, farrayPtr=fluxmesh_ptr, rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          fluxmesh_ptr = -fluxes(:,1,sed%export_states(n)%fabm_id)
          call ESMF_StateAddReplace(exportState,(/field/),rc=rc)
          
          
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        end if
      end do
#if 0
      do n=1,size(sed%model%info%diagnostic_variables)
        diag => sed%diagnostic_variables(n)
        statemesh_ptr => diag(:,1,:)
        field = ESMF_FieldCreate(state_mesh,farrayPtr=statemesh_ptr, &
                   name=only_var_name(sed%model%info%diagnostic_variables(n)%long_name)//'_in_soil', rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_AttributeSet(field, 'creator', trim(name), rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        call ESMF_StateAddReplace(exportState,(/field/),rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      end do
#endif

      !! create boundary fields in import State
      field = ESMF_FieldCreate(surface_mesh, &
               name='temperature_at_soil_surface', &
               typekind=ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldGet(field,farrayPtr=fluxmesh_ptr,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_AttributeSet(field, 'creator', trim(name), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      fluxmesh_ptr(1:numElements)=bdys(1:numElements,1,1)
      call ESMF_StateAddReplace(importState,(/field/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      do n=1,size(sed%export_states)
        if (sed%export_states(n)%fabm_id/=-1) then
          field = ESMF_FieldCreate(surface_mesh, &
                   name=trim(sed%export_states(n)%standard_name)//'_at_soil_surface', &
                   typekind=ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          call ESMF_AttributeSet(field, 'creator', trim(name), rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          call ESMF_FieldGet(field,farrayPtr=fluxmesh_ptr,rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          fluxmesh_ptr(1:numElements)=bdys(:,1,sed%export_states(n)%fabm_id+1)
          call ESMF_StateAddReplace(importState,(/field/),rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          if (sed%model%info%state_variables(sed%export_states(n)%fabm_id)%properties%get_logical( &
              'particulate',default=.false.)) then
            ! overwrite states with fluxes and set z_velocity to -1.0
            call ESMF_FieldGet(field,farrayPtr=fluxmesh_ptr,rc=rc)
            if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
            fluxmesh_ptr(1:numElements)=fluxes(:,1,sed%export_states(n)%fabm_id)

            field = ESMF_FieldCreate(surface_mesh, &
                   name=trim(sed%export_states(n)%standard_name)//'_z_velocity_at_soil_surface', &
                   typekind=ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
            if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
            call ESMF_AttributeSet(field, 'creator', trim(name), rc=rc)
            if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
            call ESMF_FieldGet(field,farrayPtr=fluxmesh_ptr,rc=rc)
            if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
            fluxmesh_ptr(1:numElements)=-1.0_rk
            call ESMF_StateAddReplace(importState,(/field/),rc=rc)
            if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          end if
        end if
      end do  
    else ! sed%grid%use_ugrid
      if (sed%grid%type==LOCAL_GRID) then
        call ESMF_ArraySpecSet(flux_array, rank=2, typekind=ESMF_TYPEKIND_R8, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_ArraySpecSet(state_array, rank=3, typekind=ESMF_TYPEKIND_R8, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        flux_grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/_INUM_,_JNUM_/), &
          regDecomp=(/1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
          name="sediment fluxes grid",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
          coorddep2=(/2/),rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_AttributeSet(flux_grid, 'creator', trim(name), rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_GridAddCoord(flux_grid, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        ! skip state grid -> use ungridded dimension
        state_grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/_INUM_,_JNUM_,sed%grid%knum/), &
          regDecomp=(/1,1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
          name="sediment states grid",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
          coorddep2=(/2/),rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_AttributeSet(state_grid, 'creator', trim(name), rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_GridAddCoord(state_grid, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      end if
      ! by here, have flux_grid available
      call ESMF_GridGet(flux_grid, indexflag=indexflag,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      ! put concentration array into export state
      ! it might be enough to do this once in initialize(?)
      do n=1,size(sed%export_states)
        field = ESMF_FieldCreate(flux_grid, &
                         typekind=ESMF_TYPEKIND_R8, &
                         name=trim(sed%export_states(n)%standard_name)//'_in_soil', &
                         staggerloc=ESMF_STAGGERLOC_CENTER, &
                         ungriddedLBound=(/1/), ungriddedUBound=(/sed%grid%knum/), &
                         gridToFieldMap=(/1,2/), rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_AttributeSet(field, 'creator', trim(name), rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_AttributeSet(field,'units',trim(sed%export_states(n)%units))
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldGet(field=field, farrayPtr=ptr_f3, &
                       totalLBound=lbnd3,totalUBound=ubnd3, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        ptr_f3 = sed%export_states(n)%data ! initialize with 0.0
        call ESMF_StateAddReplace(exportState,(/field/),rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        if (sed%export_states(n)%fabm_id/=-1) then
          !> add boundary upward fluxes
          field = ESMF_FieldCreate(flux_grid, &
                         typekind=ESMF_TYPEKIND_R8, &
                         name=trim(sed%export_states(n)%standard_name)//'_upward_flux_at_soil_surface', &
                         staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
          !> fluxes are defined in concentration*m/s
          call ESMF_AttributeSet(field,'units',trim(sed%export_states(n)%units)//'/s')
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          call ESMF_AttributeSet(field, 'creator', trim(name), rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          call ESMF_FieldGet(field=field, localDe=0, farrayPtr=ptr_f2, &
                       totalLBound=lbnd2,totalUBound=ubnd2, rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          ptr_f2 = -fluxes(:,:,sed%export_states(n)%fabm_id)
          call ESMF_StateAddReplace(exportState,(/field/),rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        end if
      end do
      do n=1,size(sed%model%info%diagnostic_variables)
        diag => sed%diagnostic_variables(n)
        field = ESMF_FieldCreate(flux_grid,farray=diag, &
                   indexflag=indexflag, &
                   ungriddedLBound=(/1/), &
                   ungriddedUBound=(/sed%grid%knum/), &
                   gridToFieldMap=(/1,2/), &
                   name=only_var_name(sed%model%info%diagnostic_variables(n)%long_name)//'_in_soil', rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
       call ESMF_AttributeSet(field, 'creator', trim(name), rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
       call ESMF_AttributeSet(field,'units',trim(sed%model%info%diagnostic_variables(n)%units))
        
        call ESMF_StateAddReplace(exportState,(/field/),rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      end do

      !! create boundary fields in import State
      field = ESMF_FieldCreate(flux_grid, &
               name='temperature_at_soil_surface', &
               typekind=ESMF_TYPEKIND_R8, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_AttributeSet(field, 'creator', trim(name), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_AttributeSet(field,'units','degC')
      call ESMF_StateAddReplace(importState,(/field/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      do n=1,size(sed%export_states)
        if (sed%export_states(n)%fabm_id/=-1) then
          field = ESMF_FieldCreate(flux_grid, &
                   name=trim(sed%export_states(n)%standard_name)//'_at_soil_surface', &
                   typekind=ESMF_TYPEKIND_R8, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          call ESMF_AttributeSet(field, 'creator', trim(name), rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          call ESMF_AttributeSet(field,'units',trim(sed%export_states(n)%units))
          call ESMF_StateAddReplace(importState,(/field/),rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          if (sed%model%info%state_variables(sed%export_states(n)%fabm_id)%properties%get_logical( &
              'particulate',default=.false.)) then
            field = ESMF_FieldCreate(flux_grid, &
                   name=trim(sed%export_states(n)%standard_name)//'_z_velocity_at_soil_surface', &
                   typekind=ESMF_TYPEKIND_R8, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
            if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
            call ESMF_AttributeSet(field, 'creator', trim(name), rc=rc)
            if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
            call ESMF_AttributeSet(field,'units','m/s')
            call ESMF_StateAddReplace(importState,(/field/),rc=rc)
            if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          end if
        end if
      end do  
    endif ! self%use_ugrid
    call get_boundary_conditions(sed,importState,bdys,fluxes)
    !call ESMF_StatePrint(importState)
    !call ESMF_StatePrint(exportState)

    call MOSSCO_CompExit(gridComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

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
    integer           :: fieldcount, i
    integer(8)     :: t
    character(len=ESMF_MAXSTR)  :: string
    type(ESMF_Alarm)           :: outputAlarm
 
    character(len=ESMF_MAXSTR) :: timestring, name, message
    integer(ESMF_KIND_I4)      :: localPet, petCount, itemCount
    type(ESMF_Clock)           :: clock
    type(ESMF_Time)            :: currTime, startTime, stopTime
    integer(ESMF_KIND_I8)      :: seconds, advanceCount
    type(ESMF_TimeInterval)    :: timeStep
    logical                    :: clockIsPresent
    
    type(ESMF_Alarm), allocatable :: alarmList(:)
    integer(ESMF_KIND_I4)      :: alarmCount
    character(len=ESMF_MAXSTR) :: alarmName

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call get_boundary_conditions(sed,importState,bdys,fluxes)
    sed%bdys   => bdys
    sed%fluxes => fluxes

	  call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ClockGet(clock, alarmCount=alarmCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (alarmCount>0) then
      if (.not.allocated(alarmList)) allocate(alarmList(alarmCount))
      call ESMF_ClockGetAlarmList(clock, ESMF_ALARMLIST_ALL, alarmList=alarmList, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      do i=1,alarmCount
        call ESMF_AlarmGet(alarmList(i), name=alarmName, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        if (trim(alarmName)=='outputAlarm') then
           outputAlarm=alarmList(i)
           exit
        endif 
      enddo     
    endif

    do while (.not.ESMF_ClockIsStopTime(clock))
      call ode_solver(sed,dt,ode_method)

      ! reset concentrations to mininum_value
      do n=1,sed%nvar
        do k=1,sed%grid%knum
!!@todo This has to be adjusted for inum, jnum longer than 1
          if (sed%conc(1,1,k,n) .lt. sed%model%info%state_variables(n)%minimum) then
            sed%conc(_IRANGE_,_JRANGE_,k,n) = sed%model%info%state_variables(n)%minimum
          end if
        end do
      end do

      call ESMF_ClockGet(clock, advanceCount=t, rc=rc)

      if (sed%do_output) then
        !! Check if the output alarm is ringing, if so, quiet it and 
        !! get the current advance count (formerly t) from clock
        !if (ESMF_AlarmIsRinging(outputAlarm)) then
        !  call ESMF_AlarmRingerOff(outputAlarm,rc=rc)
        if (mod(advanceCount,output)==0) then
          !write(string,'(A,F7.1,A)') 'Elapsed ',t*dt/86400,' days'
          !write(*,'(A,F7.1,A)') 'Elapsed ',t*dt/86400,' days'
          !call ESMF_LogWrite(string,ESMF_LOGMSG_INFO)
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
        end if
      end if

#ifdef WRITE_PROGRESS
      if (mod(t*dt,(365.*86400.)).eq.0) write(0,*) '  elapsed [d]',dt*t/86400.
#endif

      call ESMF_ClockAdvance(clock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo
   
    ! write back fluxes into export State

    do n=1,size(sed%export_states)
      if (sed%grid%use_ugrid) then
        call ESMF_StateGet(exportState, &
             trim(sed%export_states(n)%standard_name)//'_in_soil', &
             field,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldGet(field=field, localDe=0, farrayPtr=statemesh_ptr, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        do k=1,sed%grid%knum
          statemesh_ptr(k,:) = sed%export_states(n)%data(:,1,k)
        end do
        if (sed%export_states(n)%fabm_id /= -1) then
          call ESMF_StateGet(exportState, &
             trim(sed%export_states(n)%standard_name)//'_upward_flux_at_soil_surface', &
             field,rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          call ESMF_FieldGet(field=field, localDe=0, farrayPtr=fluxmesh_ptr, rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          fluxmesh_ptr = -fluxes(:,1,sed%export_states(n)%fabm_id)
        end if
      else
        call ESMF_StateGet(exportState, &
             trim(sed%export_states(n)%standard_name)//'_in_soil', &
             field,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldGet(field=field, localDe=0, farrayPtr=ptr_f3, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        ptr_f3 = sed%export_states(n)%data
        if (sed%export_states(n)%fabm_id /= -1) then
          call ESMF_StateGet(exportState, &
             trim(sed%export_states(n)%standard_name)//'_upward_flux_at_soil_surface', &
             field,rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          call ESMF_FieldGet(field=field, localDe=0, farrayPtr=ptr_f2, rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
          ptr_f2 = -fluxes(:,:,sed%export_states(n)%fabm_id)
        end if
      end if ! sed%grid%use_ugrid
    end do
 
    if (allocated(fieldList)) deallocate(fieldlist)
    
    call MOSSCO_CompExit(gridComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  
  end subroutine Run

   subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer(ESMF_KIND_I4)   :: petCount, localPet
    character(ESMF_MAXSTR)  :: name, message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
    close(funit)

    call sed%finalize()
    if (allocated(conc)) deallocate(conc)
    if (allocated(bdys)) deallocate(bdys)
    if (allocated(fluxes)) deallocate(fluxes)


    !! @todo The clockIsPresent statement does not detect if a clock has been destroyed 
    !! previously, thus, we comment the clock destruction code while this has not
    !! been fixed by ESMF
    !if (clockIsPresent) call ESMF_ClockDestroy(clock, rc=rc)
    !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call MOSSCO_CompExit(gridComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine Finalize

  subroutine get_boundary_conditions(sed,importState,bdys,fluxes)
    
    real(rk),dimension(:,:,:),target :: bdys,fluxes
    type(type_sed)      :: sed
    type(ESMF_State)    :: importState
    real(ESMF_KIND_R8),pointer,dimension(:,:)  :: ptr_f2,ptr_vs_2d
    real(ESMF_KIND_R8),pointer,dimension(:,:,:)  :: ptr_f3,ptr_vs
    type(ESMF_Field)    :: field,vs_field
    type(ESMF_Array)    :: array,vs_array
    integer             :: i,rc,itemcount
    character(len=ESMF_MAXSTR) :: string
    character(len=ESMF_MAXSTR) :: varname
    real(rk),dimension(_IRANGE_,_JRANGE_),target :: vs,pom



    call ESMF_StateGet(importState,itemSearch="temperature_at_soil_surface",itemCount=itemcount,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (itemcount==0) then
#ifdef DEBUG
      write(string,'(A)') "No temperature information found, using default value 10 deg_C"
      call ESMF_LogWrite(string,ESMF_LOGMSG_WARNING)
#endif
      bdys(1:_INUM_,1:_JNUM_,1) = 10._rk   ! degC temperature
    else 
      call ESMF_StateGet(importState,"temperature_at_soil_surface",field,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
#ifdef DEBUG
      write(string,'(A)') "Water temperature information found"
      call ESMF_LogWrite(string,ESMF_LOGMSG_INFO)
#endif
      ptr_f2 => bdys(:,:,1)
      if (sed%grid%use_ugrid) then
        call ESMF_FieldGet(field,farrayPtr=fluxmesh_ptr,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        ptr_f2(:,1) = fluxmesh_ptr(:)
      else
        call ESMF_FieldGet(field,farrayPtr=ptr_f2,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        bdys(:,:,1) = ptr_f2(:,:)   ! get lowest vertical index for near-bed temperature
      end if
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
      call ESMF_StateGet(importState,itemSearch=trim(varname)//'_at_soil_surface', &
                         itemCount=itemcount,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      if (itemcount==0) then
#ifdef DEBUG
        write(string,'(A)') "Variable '"//trim(varname)//"' not found in State. Skipping."
        call ESMF_LogWrite(string,ESMF_LOGMSG_INFO)
#endif
      else
        call ESMF_StateGet(importState,trim(varname)//'_at_soil_surface',field,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
#ifdef DEBUG
        if (rc == ESMF_SUCCESS) write(0,*) 'found field ',trim(varname)
#endif

        if (sed%model%info%state_variables(i)%properties%get_logical( &
            'particulate',default=.false.)) then
          !write(0,*) 'try to get ',trim(varname)//'_z_velocity'
          call ESMF_StateGet(importState,trim(varname)//'_z_velocity_at_soil_surface', &
                             vs_field,rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT,rc=rc)
          if (sed%grid%use_ugrid) then
            call ESMF_FieldGet(field,farrayPtr=fluxmesh_ptr,rc=rc)
            if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
            call ESMF_FieldGet(vs_field,farrayPtr=fluxmesh_ptr_vs,rc=rc)
            if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
            fluxes(_IRANGE_,1,i) = -fluxmesh_ptr(:)*fluxmesh_ptr_vs(:) ! downward flux is positive
          else
            call ESMF_FieldGet(field,farrayPtr=ptr_f2,rc=rc)
            if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
            call ESMF_FieldGet(vs_field,farrayPtr=ptr_vs_2d,rc=rc)
            if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
            fluxes(_IRANGE_,_JRANGE_,i) = -ptr_f2(:,:)*ptr_vs_2d(:,:) ! downward flux is positive
          end if
        else
          ptr_f2 => bdys(:,:,i+1)
          if (sed%grid%use_ugrid) then
            call ESMF_FieldGet(field,farrayPtr=fluxmesh_ptr,rc=rc)
            if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
            ptr_f2(:,1) = fluxmesh_ptr(:)
          else
            call ESMF_FieldGet(field,farrayPtr=ptr_f2,rc=rc)
            if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
            bdys(:,:,i+1) = ptr_f2(:,:)
          end if
          if (sed%bcup_dissolved_variables .eq. 1) then
            fluxes(_IRANGE_,_JRANGE_,i) = -(sed%conc(:,:,1,i)-bdys(:,:,i+1))/ &
              sed%grid%dz(:,:,1)*(sed%diffusivity+bdys(:,:,1) * &
              0.035d0)*sed%porosity(:,:,1)/86400._rk/10000._rk
          else
            !> reset fluxes to zero
            fluxes(_IRANGE_,_JRANGE_,i) = 0.0d0
          end if
        end if
      endif
 
  
    end do

  end subroutine get_boundary_conditions

  !> set ESMF attributes "required_flag", "required" and "optional" for
  !! all boundary conditions in the importState
  subroutine set_boundary_flags(sed,state)
    type(type_sed)             :: sed
    type(ESMF_State)           :: state
    character(len=ESMF_MAXSTR) :: name,varname,attbasename
    integer                    :: n

    name='temperature_at_soil_surface'
    call set_item_flags(state,name,requiredFlag=.true.,requiredRank=2)

    do n=1,size(sed%model%info%state_variables)
      if (sed%model%info%state_variables(n)%standard_variable%name/='') then
        varname = &
          trim(sed%model%info%state_variables(n)%standard_variable%name)
      else
      !> otherwise use CF-ed version of long_name
        varname = trim(only_var_name( &
           sed%model%info%state_variables(n)%long_name))
      end if
      attbasename=trim(varname)//'_at_soil_surface'
      call set_item_flags(state,attbasename,requiredFlag=.true.,requiredRank=2)

      if (sed%model%info%state_variables(n)%properties%get_logical( &
            'particulate',default=.false.)) then
        name = trim(varname)//'_z_velocity_at_soil_surface'
        call set_item_flags(state,name,requiredFlag=.true.,requiredRank=2)
      end if
    end do
  end subroutine set_boundary_flags

end module fabm_sediment_component
