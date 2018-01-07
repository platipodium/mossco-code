!> @brief FABM sediment ESMF component
!
!> The ESMF/FABM sediment driver component module provides infrastructure for the
!! MOSSCO sediment component.
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018
!>  Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!> @author Richard Hofmeister <richard.hofmeister@hzg.de>
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

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "fabm_sediment_component.F90"
#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#define _MOSSCO_LOG_ERROR if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, ESMF_ERR_PASSTHRU, ESMF_CONTEXT)) continue

module fabm_sediment_component

  use esmf
  use fabm_sediment_driver
  use solver_library!, only : ode_solver
  use mossco_strings
  use mossco_state
  use mossco_field
  use mossco_component
  use mossco_grid
  use mossco_config

  implicit none

  private

  real(rk)  :: dzmin,dt,dt_spinup
  real(rk)  :: dt_min=1.0e-8_rk,relative_change_min=-0.9_rk
  integer   :: tnum,funit,output=-1,k,n,numyears,numlayers
  integer   :: ode_method=_ADAPTIVE_EULER_
  integer   :: presimulation_years=-1
  integer   :: bcup_dissolved_variables=2
  real(rk)  :: pel_NO3=5.0_rk, pel_NH4=5.0_rk, pel_PO4=0.5_rk, pel_O2=250_rk
  real(rk)  :: pflux_lDetC=10.0_rk, pflux_sDetC=10.0_rk, pflux_lDetN=1.5_rk
  real(rk)  :: pflux_sDetN=1.5_rk, pflux_lDetP=0.2_rk, pel_Temp=5.0_rk
  !> deprecated variables (to enable outdated namelists)
  real(rk)  :: pflux_sDet=10.0_rk, pflux_fDet=10.0_rk, pflux_detP=0.2_rk

  real(rk),dimension(:,:,:,:),allocatable,target :: conc
  real(rk),dimension(:,:,:),pointer              :: diag
  real(rk),dimension(:,:,:),allocatable,target   :: bdys,fluxes
  real(rk),dimension(:,:),pointer   :: fptr2d
  real(rk),dimension(:), pointer    :: fluxmesh_ptr
  real(rk),dimension(:), pointer    :: fluxmesh_ptr_vs
  real(rk),dimension(:,:), pointer  :: statemesh_ptr
  character(len=ESMF_MAXSTR) :: ugrid_name=''

  type(type_sed),save :: sed
  type(type_sed),save :: sed1d

  namelist /run_nml/ numyears, dt, output, numlayers, dzmin,                          &
                     ode_method, dt_min, relative_change_min,                         &
                     ugrid_name, bcup_dissolved_variables, presimulation_years,       &
                     pel_Temp, pel_NO3, pel_NH4, pel_PO4, pel_O2,                     &
                     pflux_lDetC, pflux_sDetC, pflux_lDetN, pflux_sDetN, pflux_lDetP, &
                     pflux_sDet, pflux_fDet, pflux_detP

  public SetServices

  contains

  !> Provide an ESMF compliant SetServices routine, which defines
  !! the entry points for Init/Run/Finalize

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    integer              :: localrc

    rc=ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=2, &
      userRoutine=InitializeP2, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_READRESTART, phase=1, &
      userRoutine=ReadRestart, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
  subroutine InitializeP0(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=10)           :: InitializePhaseMap(2)
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    InitializePhaseMap(1) = "IPDv00p1=1"
    InitializePhaseMap(2) = "IPDv00p2=2"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP0

  !> Initialize the component
  !!
  !! Allocate memory for boundaries and fluxes, create ESMF fields
  !! and export them
#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)
    use fabm_types, only: output_none
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
    integer              :: n,i,j,k
    type(ESMF_DistGrid)  :: distGrid_3d,distGrid_2d
    type(ESMF_Grid)      :: state_grid,flux_grid, grid
    type(ESMF_Mesh)      :: surface_mesh, state_mesh
    type(ESMF_ArraySpec) :: flux_array,state_array
    type(ESMF_Index_Flag):: indexflag

    real(ESMF_KIND_R8),dimension(:,:),pointer :: ptr_f2
    real(ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr_f3
    real(ESMF_KIND_R8),dimension(:,:,:,:),pointer :: ptr_f4
    real(ESMF_KIND_R8),dimension(:,:,:,:),pointer :: rhs
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
    logical                    :: clockIsPresent, isPresent
    integer                    :: numElements,numNodes, exclusiveCount(2), rank
    character(len=ESMF_MAXSTR) :: foreignGridFieldName
    integer(ESMF_KIND_I4)      :: localrc
    integer, dimension(:,:), pointer :: gridmask=>null()
    type(ESMF_StateItem_Flag)  :: itemType


    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !! read namelist input for control of timestepping
    open(33,file='run_sed.nml',action='read',status='old')
    read(33,nml=run_nml)

    !> Convert from deprecated namelist items detP, fDet, sDet
    if (pflux_sDet /= pflux_sDetC) pflux_sDetC = pflux_sDet
    if (pflux_fDet /= pflux_lDetC) pflux_lDetC = pflux_fDet
    if (pflux_detP /= pflux_lDetP) pflux_lDetP = pflux_detP

    !! Set the time step end stop time
    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_TimeIntervalSet(timeInterval, s_r8=dt, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ClockSet(clock, timeStep=timeInterval, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !! also from namelist, the output timestep is read and
    !! used to create an alarm
    !! no output, if output <= 0
    sed%do_output = output .gt. 0

#if 0
    if (sed%do_output) then
      call ESMF_TimeIntervalSet(alarmInterval,s_i8=int(dt*output,kind=ESMF_KIND_I8),rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      outputAlarm = ESMF_AlarmCreate(clock,ringTime=startTime+alarmInterval, &
        name='output', ringInterval=alarmInterval,rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(outputAlarm,'creator', trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif
#endif

    !! read ugrid mesh to get number of sediment columns
    sed%grid%use_ugrid = ugrid_name /= ''
    if (sed%grid%use_ugrid) sed%grid%type=UGRID
    !> todo: check importState for foreign_grid_field_name

    if (sed%grid%type==UGRID) then
      surface_mesh = ESMF_MeshCreate(                                  filename=ugrid_name, &
#if ESMF_VERSION_MAJOR > 6
          fileformat=ESMF_FILEFORMAT_UGRID, rc=localrc)
#else
          filetypeflag=ESMF_FILEFORMAT_UGRID, rc=localrc)
#endif
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !call ESMF_AttributeSet(surface_mesh,'creator', trim(name), rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_MeshGet(surface_mesh,numOwnedElements=numElements,numOwnedNodes=numNodes)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      sed%grid%inum=numElements
      sed%grid%jnum=1
      write(message,*) trim(name)//': use unstructured grid, number of local elements:',numElements
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
    else
      call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
           value=foreignGridFieldName, defaultValue='none',rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (trim(foreignGridFieldName)=='none') then
        sed%grid%type=LOCAL_GRID
      else
        sed%grid%type=FOREIGN_GRID
      endif
    endif

    if (sed%grid%type==FOREIGN_GRID) then

      call ESMF_StateGet(importState, trim(foreignGridFieldName), itemType, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (itemType /= ESMF_STATEITEM_FIELD) then
        call MOSSCO_StateLog(importState, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message,'(A)') trim(name)//' cannot find specified foreign grid field '//trim(foreignGridFieldName)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_StateGet(importState, foreignGridFieldName, field, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(name)//' uses foreign grid from field'
      call MOSSCO_FieldString(field, message, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

      call ESMF_FieldGet(field, grid=grid, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_GridGet(grid, rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank == 3) then
        flux_grid = MOSSCO_GridCreateFromOtherGrid(grid, rc=localrc)
        call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, &
          localDe=0, exclusiveUbound=ubnd3, exclusiveLbound=lbnd3, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (numlayers /= ubnd3(3)-lbnd3(3) + 1) then
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          numlayers = ubnd3(3)-lbnd3(3) + 1
          write(message,'(A,I3)') trim(name)//' overwrites namelist with 3D-grid numlayers = ',numlayers
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
        endif
      else
        flux_grid = grid
      endif

      call ESMF_GridGet(flux_grid, rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank/=2) then
        write(message,'(A)') trim(name)//' could not create rank 2 grid'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
      endif

      call ESMF_GridGet(flux_grid, staggerloc=ESMF_STAGGERLOC_CENTER, localDe=0, &
        exclusiveLBound=lbnd2, exclusiveUBound=ubnd2, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      sed%grid%inum=ubnd2(1)-lbnd2(1)+1
      sed%grid%jnum=ubnd2(2)-lbnd2(2)+1

    elseif (sed%grid%type==LOCAL_GRID) then
      write(message,*) '  use local 1x1 horizontal grid'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      sed%grid%inum=1
      sed%grid%jnum=1
    endif

    !! The grid specification should also go to outside this routine, and update the grid of
    !! this component, numlayers and dzmin are read from nml
    sed%grid%knum=numlayers
    sed%grid%dzmin=dzmin

    sed1d%grid%inum=1
    sed1d%grid%jnum=1
    sed1d%grid%knum=numlayers
    sed1d%grid%dzmin=dzmin

    !! Write log entries
    write(message,'(A,I5,A,I5,A,I5)') trim(name)//' initialize grid [inum x jnum x knum]', &
      _INUM_,' x ',_JNUM_,' x ',_KNUM_
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

    !! get grid mask
    allocate(sed%mask(1:sed%grid%inum,1:sed%grid%jnum,1:sed%grid%knum))
    sed%mask = .false.
    isPresent = .true.


#if ESMF_VERSION_MAJOR > 6
    if (sed%grid%type==FOREIGN_GRID) then
      call ESMF_GridGetItem(flux_grid, ESMF_GRIDITEM_MASK, isPresent=isPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif
#endif

    if (sed%grid%type==FOREIGN_GRID .and. isPresent) then

      call ESMF_GridGetItem(flux_grid, ESMF_GRIDITEM_MASK, farrayPtr=gridmask, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
#if ESMF_VERSION_MAJOR > 6
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#else
        call ESMF_LogWrite('  ignore error above', ESMF_LOGMSG_ERROR)
#endif

        do i=1,sed%grid%inum
          do j=1,sed%grid%jnum
            do k=1,sed%grid%knum
              sed%mask(i,j,k) = (gridmask(i,j).le.0)
            enddo
          enddo
        enddo
      endif

    if (.not.isPresent .or. localrc /= ESMF_SUCCESS) then
        write(message,'(A)') trim(name)//' found no mask in foreign grid, compute every sediment column'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
    endif

    call sed%grid%init_grid()
    call sed%initialize()
    close(33)
    !! Allocate all arrays conc, bdys, fluxes
    allocate(conc(_INUM_,_JNUM_,_KNUM_,sed%nvar))
    ! link conc to fabm_sediment_driver
    sed%conc => conc
    ! check for valid grid and porosity
    call sed%check_domain()
    ! initialise values
    conc = 0.0_rk
    call sed%init_concentrations()
    !> Allocate boundary conditions and initialize with zero
    allocate(bdys(_INUM_,_JNUM_,sed%nvar+1))
    bdys(_IRANGE_,_JRANGE_,:) = 0.0_rk
    allocate(fluxes(_INUM_,_JNUM_,sed%nvar))
    fluxes(_IRANGE_,_JRANGE_,:) = 0.0_rk

    call set_boundary_flags(sed,importState)
    !> create list of state variables for export
    call sed%get_all_export_states()

    !> run for some years into quasi-steady-state
    open(33,file='run_sed.nml',action='read',status='old')
    call sed1d%grid%init_grid()
    call sed1d%initialize()
    close(33)
    allocate(sed1d%conc(1,1,_KNUM_,1:sed%nvar))
    !> check for valid grid and porosity
    call sed1d%check_domain()
    call sed1d%init_concentrations()
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
    sed1d%dt_min=dt_min
    sed1d%relative_change_min=relative_change_min
    if (_INUM_ > 0 .and. _JNUM_ > 0) then
      sed1d%bdys   => bdys(1:1,1:1,:)
      sed1d%fluxes => fluxes(1:1,1:1,:)
    endif

    ! set boundary conditions for pre-simulation
    bdys(:,:,1) = pel_Temp !degC
    do n=1,size(sed%model%state_variables)
      varname = trim(only_var_name(sed%model%state_variables(n)%long_name))
      if (trim(varname) == 'dissolved_nitrate')            bdys(:,:,n+1)=pel_NO3
      if (trim(varname) == 'dissolved_ammonium')           bdys(:,:,n+1)=pel_NH4
      if (trim(varname) == 'dissolved_phosphate')          bdys(:,:,n+1)=pel_PO4
      if (trim(varname) == 'dissolved_oxygen')             bdys(:,:,n+1)=pel_O2
      if (trim(varname) == 'dissolved_reduced_substances') bdys(:,:,n+1)=0.0_rk
      if (trim(varname) == 'detritus_labile_carbon')       fluxes(:,:,n)=pflux_lDetC/86400.0_rk
      if (trim(varname) == 'detritus_semilabile_carbon')   fluxes(:,:,n)=pflux_sDetC/86400.0_rk
      if (trim(varname) == 'detritus_labile_nitrogen')     fluxes(:,:,n)=pflux_lDetN/86400.0_rk
      if (trim(varname) == 'detritus_semilabile_nitrogen') fluxes(:,:,n)=pflux_sDetN/86400.0_rk
      if (trim(varname) == 'detritus_labile_phosphorus')   fluxes(:,:,n)=pflux_lDetP/86400.0_rk
      !> For legacy reasons, these are the old names in omexdia
      if (trim(varname) == 'fast_detritus_C')              fluxes(:,:,i)=pflux_lDetC/86400.0_rk
      if (trim(varname) == 'slow_detritus_C')              fluxes(:,:,i)=pflux_sDetC/86400.0_rk
      if (trim(varname) == 'detritus-P')                   fluxes(:,:,i)=pflux_lDetP/86400.0_rk
      if (trim(varname) == 'detritus_phosphorus')          fluxes(:,:,n)=pflux_lDetP/86400.0_rk
    enddo

    ! use Dirichlet boundary condition for pre-simulation
    sed%bcup_dissolved_variables = 2
    sed1d%bcup_dissolved_variables = 2
    sed1d%adaptive_solver_diagnostics = .true.
    sed1d%bioturbation_profile=0
    if (presimulation_years.gt.0) then
      do tidx=1,int(presimulation_years*365*24/(dt_spinup/3600.0_rk),kind=ESMF_KIND_I8)
        call ode_solver(sed1d,dt_spinup,ode_method)
      enddo
    endif
    if (ode_method == 2) then
      write (message,*) 'minimum dt:',sed1d%last_min_dt,' at cell ',sed1d%last_min_dt_grid_cell
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
    endif

    do i=1,sed%inum
      do j=1,sed%jnum
        if (.not.sed%mask(i,j,1)) sed%conc(i,j,:,:) = sed1d%conc(1,1,:,:)
      enddo
    enddo

    !> call the model equations in order to fill the diagnostic variables
    allocate(rhs(sed%inum,sed%jnum,sed%knum,sed%nvar))
    call sed%get_rhs(rhs)
    deallocate(rhs)

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
        write(funit,fmt='(A,A)',advance='no') ' ',trim(sed%model%state_variables(n)%name)
      enddo
      do n=1,size(sed%model%diagnostic_variables)
        write(funit,fmt='(A,A)',advance='no') ' ',trim(sed%model%diagnostic_variables(n)%name)
      enddo
      write(funit,*)
    endif

    if (sed%grid%type==UGRID) then
      !! create state mesh
#if 0
      state_mesh = ESMF_MeshCreate(surface_mesh,rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      !call ESMF_AttributeSet(surface_mesh,'creator', trim(name), rc=localrc)
#endif

      !! create state and flux fields
      do n=1,size(sed%export_states)
        field = ESMF_FieldCreate(surface_mesh, &
                  name=trim(sed%export_states(n)%standard_name)//'_in_soil', &
                  typekind=ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, &
                  ungriddedLBound=(/1/), ungriddedUBound=(/sed%grid%knum/), &
                  gridToFieldMap=(/1,2/), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_FieldGet(field=field, farrayPtr=statemesh_ptr, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        do k=1,sed%grid%knum
          statemesh_ptr(k,:) = sed%export_states(n)%data(:,1,k)
        enddo

        write(message, '(A)') trim(name)//' created bulk field'
        call MOSSCO_FieldString(field, message, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        call ESMF_StateAddReplace(exportState,(/field/),rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (sed%export_states(n)%fabm_id/=-1) then
          !> add boundary upward fluxes
          field = ESMF_FieldCreate(surface_mesh, &
                    name=trim(sed%export_states(n)%standard_name)//'_upward_flux_at_soil_surface', &
                    typekind=ESMF_TYPEKIND_R8, &
                    meshloc=ESMF_MESHLOC_ELEMENT,rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_FieldGet(field=field, farrayPtr=fluxmesh_ptr, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          fluxmesh_ptr = -fluxes(:,1,sed%export_states(n)%fabm_id)

          write(message, '(A)') trim(name)//' created field'
          call MOSSCO_FieldString(field, message, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          call ESMF_StateAddReplace(exportState,(/field/),rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        endif
      enddo
#if 0
      do n=1,size(sed%model%diagnostic_variables)
        if (sed%model%diagnostic_variables(n)%output /= output_none) then
          diag => sed%diagnostic_variables(n)
          statemesh_ptr => diag(:,1,:)
          field = ESMF_FieldCreate(state_mesh,farrayPtr=statemesh_ptr, &
                   name=only_var_name(sed%model%diagnostic_variables(n)%long_name)//'_in_soil', rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message, '(A)') trim(name)//' created field'
          call MOSSCO_FieldString(field, message, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          call ESMF_StateAddReplace(exportState,(/field/),rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        endif
      enddo
#endif

      !! create boundary fields in import State
      field = ESMF_FieldEmptyCreate(name='porosity_at_soil_surface', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A)') trim(name)//' created empty field'
      call MOSSCO_FieldString(field, message, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_StateAddReplace(importState,(/field/),rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      field = ESMF_FieldCreate(surface_mesh, &
               name='temperature_at_soil_surface', &
               typekind=ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, rc=localrc)
                 _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(field,farrayPtr=fluxmesh_ptr,rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      fluxmesh_ptr(1:numElements)=bdys(1:numElements,1,1)

      write(message, '(A)') trim(name)//' created field'
      call MOSSCO_FieldString(field, message, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_StateAddReplace(importState,(/field/),rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      do n=1,size(sed%export_states)
        if (sed%export_states(n)%fabm_id/=-1) then
          field = ESMF_FieldCreate(surface_mesh, &
                   name=trim(sed%export_states(n)%standard_name)//'_at_soil_surface', &
                   typekind=ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_FieldGet(field,farrayPtr=fluxmesh_ptr,rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          fluxmesh_ptr(1:numElements)=bdys(:,1,sed%export_states(n)%fabm_id+1)

          write(message, '(A)') trim(name)//' created field'
          call MOSSCO_FieldString(field, message, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          call ESMF_StateAddReplace(importState,(/field/),rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (sed%model%state_variables(sed%export_states(n)%fabm_id)%properties%get_logical( &
              'particulate',default=.false.)) then
            ! overwrite states with fluxes and set z_velocity to -1.0
            call ESMF_FieldGet(field,farrayPtr=fluxmesh_ptr,rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            fluxmesh_ptr(1:numElements)=fluxes(:,1,sed%export_states(n)%fabm_id)

            field = ESMF_FieldCreate(surface_mesh, &
                   name=trim(sed%export_states(n)%standard_name)//'_z_velocity_at_soil_surface', &
                   typekind=ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            call ESMF_FieldGet(field,farrayPtr=fluxmesh_ptr,rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            fluxmesh_ptr(1:numElements)=-1.0_rk

            write(message, '(A)') trim(name)//' created field'
            call MOSSCO_FieldString(field, message, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

            call ESMF_StateAddReplace(importState,(/field/),rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          endif
        endif
      enddo
    else ! sed%grid%use_ugrid
      if (sed%grid%type==LOCAL_GRID) then
        call ESMF_ArraySpecSet(flux_array, rank=2, typekind=ESMF_TYPEKIND_R8, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_ArraySpecSet(state_array, rank=3, typekind=ESMF_TYPEKIND_R8, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        flux_grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/_INUM_,_JNUM_/), &
          regDecomp=(/1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
          name="sedimentFluxes",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
          coorddep2=(/2/),rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_AttributeSet(flux_grid, 'creator', trim(name), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_GridAddCoord(flux_grid, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        ! skip state grid -> use ungridded dimension
        state_grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/_INUM_,_JNUM_,sed%grid%knum/), &
          regDecomp=(/1,1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
          name="sedimentStates",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
          coorddep2=(/2/),rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_AttributeSet(state_grid, 'creator', trim(name), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_GridAddCoord(state_grid, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      endif
      ! by here, have flux_grid available
      call ESMF_GridGet(flux_grid, indexflag=indexflag,rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      ! put concentration array into export state
      ! it might be enough to do this once in initialize(?)
      do n=1,size(sed%export_states)
        field = ESMF_FieldCreate(flux_grid, &
                         typekind=ESMF_TYPEKIND_R8, &
                         name=trim(sed%export_states(n)%standard_name)//'_in_soil', &
                         staggerloc=ESMF_STAGGERLOC_CENTER, &
                         ungriddedLBound=(/1/), ungriddedUBound=(/sed%grid%knum/), &
                         gridToFieldMap=(/1,2/), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_AttributeSet(field,'units',trim(sed%export_states(n)%units), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        !> do not set missing value and leave this to netcdf component
        !call ESMF_AttributeSet(field,'missing_value',sed%missing_value, rc=localrc)
        !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_FieldGet(field=field, farrayPtr=ptr_f3, &
                       totalLBound=lbnd3,totalUBound=ubnd3, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        ptr_f3 = sed%export_states(n)%data ! initialize with 0.0

        write(message, '(A)') trim(name)//' created field'
        call MOSSCO_FieldString(field, message, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        call ESMF_StateAddReplace(exportState,(/field/),rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (sed%export_states(n)%fabm_id/=-1) then
          !> add boundary upward fluxes
          field = ESMF_FieldCreate(flux_grid, &
                         typekind=ESMF_TYPEKIND_R8, &
                         name=trim(sed%export_states(n)%standard_name)//'_upward_flux_at_soil_surface', &
                         staggerloc=ESMF_STAGGERLOC_CENTER,rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          !> fluxes are defined in concentration*m/s
          call ESMF_AttributeSet(field,'units',trim(sed%export_states(n)%units)//'/s')
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_FieldGet(field=field, localDe=0, farrayPtr=ptr_f2, &
                       totalLBound=lbnd2,totalUBound=ubnd2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          ptr_f2 = -fluxes(:,:,sed%export_states(n)%fabm_id)

          write(message, '(A)') trim(name)//' created field'
          call MOSSCO_FieldString(field, message, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          call ESMF_StateAddReplace(exportState,(/field/),rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
      enddo
      do n=1,size(sed%model%diagnostic_variables)
        if (sed%model%diagnostic_variables(n)%output /= output_none) then
          diag => sed%diagnostic_variables(n)
          field = ESMF_FieldCreate(flux_grid,farray=diag, &
                   indexflag=indexflag, &
                   ungriddedLBound=(/1/), &
                   ungriddedUBound=(/sed%grid%knum/), &
                   gridToFieldMap=(/1,2/), &
                   name=only_var_name(sed%model%diagnostic_variables(n)%long_name)//'_in_soil', rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_AttributeSet(field,'units',trim(sed%model%diagnostic_variables(n)%units))
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message, '(A)') trim(name)//' created diagnostic field'
          call MOSSCO_FieldString(field, message, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          call ESMF_StateAddReplace(exportState,(/field/),rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
      enddo

      !! create boundary fields in import State
      field = ESMF_FieldEmptyCreate(name='porosity_at_soil_surface', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A)') trim(name)//' created field'
      call MOSSCO_FieldString(field, message, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_StateAddReplace(importState,(/field/),rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      field = ESMF_FieldCreate(flux_grid, &
               name='photosynthetically_active_radiation_at_soil_surface', &
               typekind=ESMF_TYPEKIND_R8, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field,'units','W m-2', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A)') trim(name)//' created field'
      call MOSSCO_FieldString(field, message, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_StateAddReplace(importState,(/field/),rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

!       ! optionally look light from any model or rate
!       field = ESMF_FieldEmptyCreate(name='downwelling_photosynthetic_radiative_flux_at_soil_surface', rc=localrc)
!       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
!
!       call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
!       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
!
!       call ESMF_AttributeSet(field,'units','W m-2', rc=localrc)
!       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
!
!       write(message, '(A)') trim(name)//' created field'
!       call MOSSCO_FieldString(field, message, rc=localrc)
!       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
!       call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
!
!       call ESMF_StateAddReplace(importState,(/field/),rc=localrc)
!       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)


      field = ESMF_FieldCreate(flux_grid, &
               name='temperature_at_soil_surface', &
               typekind=ESMF_TYPEKIND_R8, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field,'units','degC', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A)') trim(name)//' created field'
      call MOSSCO_FieldString(field, message, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_StateAddReplace(importState,(/field/),rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      do n=1,size(sed%export_states)
        if (sed%export_states(n)%fabm_id/=-1) then
          field = ESMF_FieldCreate(flux_grid, &
                   name=trim(sed%export_states(n)%standard_name)//'_at_soil_surface', &
                   typekind=ESMF_TYPEKIND_R8, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_AttributeSet(field,'units',trim(sed%export_states(n)%units), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message, '(A)') trim(name)//' created horizontal field'
          call MOSSCO_FieldString(field, message, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          call ESMF_StateAddReplace(importState,(/field/),rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (sed%model%state_variables(sed%export_states(n)%fabm_id)%properties%get_logical( &
              'particulate',default=.false.)) then
            field = ESMF_FieldCreate(flux_grid, &
                   name=trim(sed%export_states(n)%standard_name)//'_z_velocity_at_soil_surface', &
                   typekind=ESMF_TYPEKIND_R8, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            call ESMF_AttributeSet(field,'units','m/s', rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            write(message, '(A)') trim(name)//' created horizontal field'
            call MOSSCO_FieldString(field, message, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

            call ESMF_StateAddReplace(importState,(/field/),rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          endif
        endif
      enddo
    endif ! self%use_ugrid
    call get_boundary_conditions(sed,importState,bdys,fluxes)
    !call ESMF_StatePrint(importState)
    !call ESMF_StatePrint(exportState)

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP1


#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP2"
  subroutine InitializeP2(gridComp, importState, exportState, parentClock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: currTime
    integer(ESMF_KIND_I4)       :: localrc
    character(len=ESMF_MAXSTR) :: name, message, itemname

    integer(ESMF_KIND_I4)          :: ubnd(2),lbnd(2)
    real(ESMF_KIND_R8), pointer    :: ptr_f2(:,:)
    type(ESMF_FieldStatus_Flag)    :: fieldstatus
    type(ESMF_StateItem_Flag)      :: itemtype
    type(ESMF_Field)               :: field

    !> here: * @todo: evtl. complete fields here
    !!       * check for porosity in importState and copy data
    !!       * check initialize_variables_method for:
    !!         a) set constant values
    !!         b) presimulate after setting constant values
    !!         c) copy 3d data for restarting previous simulation

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> check for porosity
    itemname='porosity_at_soil_surface'
    call ESMF_StateGet(importState, trim(itemname), itemType=itemType, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (itemType==ESMF_STATEITEM_FIELD) then
      call ESMF_StateGet(importState, trim(itemname), field=field, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(field, status=fieldstatus, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (fieldstatus==ESMF_FIELDSTATUS_COMPLETE) then
        call ESMF_FieldGet(field, farrayPtr=ptr_f2, &
               exclusiveUBound=ubnd, exclusiveLBound=lbnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if  (  (ubnd(1)-lbnd(1) /= _INUM_ - 1) .or. (ubnd(2)-lbnd(2) /= _JNUM_ - 1) ) then
          write(message,'(A)') trim(name)//' received incompatible bounds in '
          call MOSSCO_FieldString(field, message)
          localrc = ESMF_RC_ARG_BAD
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif

        sed%porosity(1:_INUM_,1:_JNUM_,1)=ptr_f2(lbnd(1):ubnd(1),lbnd(2):ubnd(2))
        call sed%update_porosity(from_surface=.true.)
        write(message,'(A)') trim(name)//' updated porosity from'
        call MOSSCO_FieldString(field, message, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      else
        write(message,'(A)') trim(name)//' received incomplete field, remove field'
        call mossco_fieldString(field, message)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
        call ESMF_StateRemove(importState,(/ trim(itemname) /), rc=localrc)
        call ESMF_FieldDestroy(field)
      endif
    else
      write(message,'(A)') trim(name)//' has no external porosity information'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
    endif

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP2


#undef  ESMF_METHOD
#define ESMF_METHOD "ReadRestart"
  subroutine ReadRestart(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=ESMF_MAXSTR)  :: name,message,varname
    type(ESMF_Time)             :: currTime
    integer                     :: localrc,n, rank

    integer(ESMF_KIND_I4)          :: ubnd(3),lbnd(3)!,ownshape(3)
    integer(ESMF_KIND_I4)          :: exportUbnd(3),exportLbnd(3),ownshape(3)
    real(ESMF_KIND_R8), pointer    :: ptr_f3(:,:,:)
    type(ESMF_FieldStatus_Flag)    :: fieldstatus
    type(ESMF_StateItem_Flag)      :: itemtype
    type(ESMF_Field)               :: field, exportField

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> browse through list of state variables and
    !! copy data from importState fields with same name
    do n = 1, size(sed%export_states)

      varname=trim(sed%export_states(n)%standard_name)//'_in_soil'
      call ESMF_StateGet(importState, trim(varname), itemType=itemType, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (itemType == ESMF_STATEITEM_NOTFOUND) cycle

      if (itemType /= ESMF_STATEITEM_FIELD) then
        write(message,'(A)') trim(name)//' skipped hotstart for non-field '//trim(varname)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
        call MOSSCO_StateLog(importState, rc=localrc)
        cycle
      endif

      call ESMF_StateGet(importState, trim(varname), field=field, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(field, status=fieldstatus, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (fieldstatus /= ESMF_FIELDSTATUS_COMPLETE) then
        write(message,'(A)') trim(name)//' skipped hotstart for incomplete '
        call MOSSCO_FieldString(field, message, rc=localrc)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
        cycle
      endif

      call ESMF_FieldGet(field, rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank /= 3) then
        write(message,'(A)') trim(name)//' skipped hotstart for not rank 3 '
        call MOSSCO_FieldString(field, message, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
        cycle
      endif

      call ESMF_FieldGet(field, farrayPtr=ptr_f3, &
        exclusiveUBound=ubnd, exclusiveLBound=lbnd, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !! Need to get shape from exportState field of same name to constrain the indices
      !! of the conc field
      call ESMF_StateGet(exportState, trim(varname), itemType=itemType, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_StateGet(exportState, trim(varname), field=exportfield, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGetBounds(exportField, exclusiveUbound=exportUbnd, &
        exclusiveLbound=exportLbnd, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (any (exportUbnd /= ubnd) .or. any(exportLbnd /= lbnd)) then
        write(message,'(A)') trim(name)//' skipped hotstart for no-match array bounds '
        call MOSSCO_FieldString(field, message, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
        cycle
      endif

      ! ownshape = shape(sed%export_states(n)%data)
      !
      ! if ((ubnd(1)-lbnd(1)+1.ne.ownshape(1)).or. &
      !         (ubnd(2)-lbnd(2)+1.ne.ownshape(2)).or. &
      !         (ubnd(3)-lbnd(3)+1.ne.ownshape(3))) then
      !       write(message,'(A)') trim(name)//' incompatible shape of field'
      !       call mossco_fieldString(field, message)
      !       call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      !       write(message,'(A,4I3,A,4I3)') trim(name)//' own shape', ownshape, ' other shape ', &
      !         ubnd(:)-lbnd(:)+ (/1,1,1/)
      !       call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      !       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      ! endif
#ifdef DEBUG
          if (trim(varname) == 'porosity_in_soil') then
            write(0,*) 'debugging output just before restart update of porosity_in_soil'
            write(0,*) 'ptr_f3',shape(ptr_f3),lbound(ptr_f3),ptr_f3(:,1,1)
            write(0,*) 'export',sed%export_states(n)%data(:,1,1)
            write(0,*) 'mask',sed%mask(:,1,1)
            write(0,*) 'porosity',sed%porosity(:,1,1)
          endif
#endif
        !   sed%export_states(n)%data = ptr_f3
        !   write(message,'(A)') trim(name)//' hotstarted field'
        !   call mossco_fieldString(field, message)
        !   call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
        ! else
        !   write(message,'(A)') trim(name)//' incomplete field'
        !   call mossco_fieldString(field, message)
        !   call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
        ! endif
      !endif

      sed%export_states(n)%data(exportLbnd(1):exportUBnd(1),exportLbnd(2):exportUbnd(2), &
        exportLBnd(3):exportUBnd(3)) = ptr_f3(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3))
      write(message,'(A)') trim(name)//' hotstarted '
      call MOSSCO_FieldString(field, message, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
    enddo

    !call sed%update_export_states()
    !> check for valid grid and porosity
#ifdef DEBUG
    write(0,*) 'fabm_sediment ReadRestart: check domain'
#endif
    call sed%check_domain()

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine ReadRestart

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    implicit none

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
    integer(ESMF_KIND_I4)      :: localrc

    type(ESMF_StateItem_Flag)   :: itemType
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    character(len=ESMF_MAXSTR)  :: itemName
    integer(ESMF_KIND_I4)       :: lbnd(2), ubnd(2)
    character(len=ESMF_MAXSTR), pointer  :: includelist(:) => null()

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

!    !> check for PAR
!    allocate( includelist(1) )
!    includelist(1) = 'photosynthetically_active_radiation_at_soil_surface'
!    !includelist(2) = 'bottom_downwelling_photosynthetic_radiative_flux'
!    call MOSSCO_StateGet(importState, fieldList=fieldList, &
!      fieldCount=fieldCount, fieldstatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
!    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
!    nullify(includelist)
!
!    if (fieldCount>0) then
!      call ESMF_FieldGet(fieldlist(1), farrayPtr=ptr_f2, &
!        exclusiveUBound=ubnd, exclusiveLBound=lbnd, rc=localrc)
!      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
!
!      sed%par_surface(1:_INUM_,1:_JNUM_)=ptr_f2(lbnd(1):ubnd(1),lbnd(2):ubnd(2))
!      write(message,'(A)') trim(name)//' updated par_surface from'
!      call MOSSCO_FieldString(field, message, rc=localrc)
!      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
!      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
!    else
!       write(message,'(A)') trim(name)//' has no external surface radiation information'
!       call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
!    endif

    !> check for PAR
    itemname='photosynthetically_active_radiation_at_soil_surface'
    call ESMF_StateGet(importState, trim(itemname), itemType=itemType, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (itemType==ESMF_STATEITEM_FIELD) then
      call ESMF_StateGet(importState, trim(itemname), field=field, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(field, status=fieldstatus, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (fieldstatus==ESMF_FIELDSTATUS_COMPLETE) then
        call ESMF_FieldGet(field, farrayPtr=ptr_f2, &
               exclusiveUBound=ubnd, exclusiveLBound=lbnd, rc=localrc)
         _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        sed%par_surface(1:_INUM_,1:_JNUM_)=ptr_f2(lbnd(1):ubnd(1),lbnd(2):ubnd(2))
        write(message,'(A)') trim(name)//' updated par_surface from'
        call MOSSCO_FieldString(field, message, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      else
        write(message,'(A)') trim(name)//' received incomplete field'
        call mossco_fieldString(field, message)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)

        call MOSSCO_StateLog(importState, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call MOSSCO_StateLog(exportState, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      end if
    else
      write(message,'(A)') trim(name)//' has no external surface radiation information'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
    end if

    !> check for porosity
    itemname='porosity_at_soil_surface'
    call ESMF_StateGet(importState, trim(itemname), itemType=itemType, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (itemType==ESMF_STATEITEM_FIELD) then
      call ESMF_StateGet(importState, trim(itemname), field=field, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(field, status=fieldstatus, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (fieldstatus==ESMF_FIELDSTATUS_COMPLETE) then
        call ESMF_FieldGet(field, farrayPtr=ptr_f2, &
               exclusiveUBound=ubnd, exclusiveLBound=lbnd, rc=localrc)
         _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        sed%porosity(1:_INUM_,1:_JNUM_,1)=ptr_f2(lbnd(1):ubnd(1),lbnd(2):ubnd(2))
        call sed%update_porosity(from_surface=.true.)
        write(message,'(A)') trim(name)//' updated porosity from'
        call MOSSCO_FieldString(field, message, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      else
        write(message,'(A)') trim(name)//' received incomplete field'
        call mossco_fieldString(field, message)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)

        call MOSSCO_StateLog(importState, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call MOSSCO_StateLog(exportState, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    else
      write(message,'(A)') trim(name)//' has no external porosity information'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
    endif

    call get_boundary_conditions(sed, importState, bdys, fluxes)
    sed%bdys   => bdys
    sed%fluxes => fluxes

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ClockGet(clock, alarmCount=alarmCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (alarmCount>0) then
      if (.not.allocated(alarmList)) allocate(alarmList(alarmCount))
      call ESMF_ClockGetAlarmList(clock, ESMF_ALARMLIST_ALL, alarmList=alarmList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      do i=1,alarmCount
        call ESMF_AlarmGet(alarmList(i), name=alarmName, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (trim(alarmName)=='outputAlarm') then
           outputAlarm=alarmList(i)
           exit
        endif
      enddo
    endif

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ClockGet(clock, stopTime=stopTime, currTime=currTime, timeStep=timeStep, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_TimeIntervalGet(timeStep, s_r8=dt, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    do while (.not.ESMF_ClockIsStopTime(clock))

      call ESMF_ClockGet(clock, currTime=currTime, advanceCount=advanceCount, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (currTime + timeStep > stopTime) then
        timeStep=stopTime-currTime
        call ESMF_TimeIntervalGet(timeStep, s_r8=dt, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      !write(0,*) 1171,trim(name)
      !> @todo the solver is not stable in example xf with sns topo

      call ode_solver(sed, dt, ode_method)

      !write(0,*) 1173,trim(name)

      ! reset concentrations to mininum_value
      if (_INUM_ > 0 .and. _JNUM_ > 0)  then
      do n=1,sed%nvar
        do k=1,sed%grid%knum
!!@todo This has to be adjusted for inum, jnum longer than 1
          if (sed%conc(1,1,k,n) .lt. sed%model%state_variables(n)%minimum) then
            sed%conc(_IRANGE_,_JRANGE_,k,n) = sed%model%state_variables(n)%minimum
          endif
        enddo
      enddo
    endif

      if (sed%do_output) then
        !! Check if the output alarm is ringing, if so, quiet it and
        !! get the current advance count from clock
        !if (ESMF_AlarmIsRinging(outputAlarm)) then
        !  call ESMF_AlarmRingerOff(outputAlarm,rc=localrc)
        if (mod(advanceCount,output)==0) then
          !write(string,'(A,F7.1,A)') 'Elapsed ',advanceCount*dt/86400,' days'
          !write(*,'(A,F7.1,A)') 'Elapsed ',advanceCount*dt/86400,' days'
          !call ESMF_LogWrite(string,ESMF_LOGMSG_INFO)
          write(funit,*) advanceCount*dt,'fluxes',fluxes(1,1,:)
          do k=1,_KNUM_
            write(funit,FMT='(E15.3,A,E15.4E3,A,E15.4E3,A,E15.4E3)',advance='no') &
              advanceCount*dt,' ',sed%grid%zc(1,1,k),' ',sed%grid%dz(1,1,k),  &
              ' ',sed%porosity(1,1,k)
            do n=1,sed%nvar
              write(funit,FMT='(A,E15.4E3)',advance='no') ' ',conc(1,1,k,n)
            enddo
            do n=1,size(sed%model%diagnostic_variables)
              diag => sed%diagnostic_variables(n)
              write(funit,FMT='(A,E15.4E3)',advance='no') ' ',diag(1,1,k)
            enddo
            write(funit,*)
          enddo
        endif
      endif



#ifdef WRITE_PROGRESS
      if (mod(advanceCount*dt,(365.*86400.)).eq.0) write(0,*) '  elapsed [d]',dt*advanceCount/86400.
#endif

      call ESMF_ClockAdvance(clock, timeStep=timeStep, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    enddo

    ! write back fluxes into export State

    do n=1,size(sed%export_states)
      if (sed%grid%use_ugrid) then
        call ESMF_StateGet(exportState, &
             trim(sed%export_states(n)%standard_name)//'_in_soil', &
             field,rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_FieldGet(field=field, localDe=0, farrayPtr=statemesh_ptr, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        do k=1,sed%grid%knum
          statemesh_ptr(k,:) = sed%export_states(n)%data(:,1,k)
        enddo
        if (sed%export_states(n)%fabm_id /= -1) then
          call ESMF_StateGet(exportState, &
             trim(sed%export_states(n)%standard_name)//'_upward_flux_at_soil_surface', &
             field,rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_FieldGet(field=field, localDe=0, farrayPtr=fluxmesh_ptr, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          fluxmesh_ptr = -fluxes(:,1,sed%export_states(n)%fabm_id)
        endif
      else
        call ESMF_StateGet(exportState, &
             trim(sed%export_states(n)%standard_name)//'_in_soil', &
             field,rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_FieldGet(field=field, localDe=0, farrayPtr=ptr_f3, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        ptr_f3 = sed%export_states(n)%data
        if (sed%export_states(n)%fabm_id /= -1) then
          call ESMF_StateGet(exportState, &
             trim(sed%export_states(n)%standard_name)//'_upward_flux_at_soil_surface', &
             field,rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_FieldGet(field=field, localDe=0, farrayPtr=ptr_f2, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          ptr_f2 = -fluxes(:,:,sed%export_states(n)%fabm_id)
        endif
      endif ! sed%grid%use_ugrid
    enddo

    if (allocated(fieldList)) deallocate(fieldlist)

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
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
    integer(ESMF_KIND_I4)      :: localrc

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    close(funit)

    call sed%finalize()
    if (allocated(conc)) deallocate(conc)
    if (allocated(bdys)) deallocate(bdys)
    if (allocated(fluxes)) deallocate(fluxes)

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Finalize

#undef  ESMF_METHOD
#define ESMF_METHOD "get_boundary_conditions"
  subroutine get_boundary_conditions(sed,importState,bdys,fluxes)

    real(rk),dimension(:,:,:),target :: bdys,fluxes
    type(type_sed)      :: sed
    type(ESMF_State)    :: importState
    real(ESMF_KIND_R8),pointer,dimension(:,:)  :: ptr_f2,ptr_vs_2d
    real(ESMF_KIND_R8),pointer,dimension(:,:,:)  :: ptr_f3,ptr_vs
    type(ESMF_Field)    :: field,vs_field
    type(ESMF_Array)    :: array,vs_array
    integer             :: n,rc,itemcount
    character(len=ESMF_MAXSTR) :: string
    character(len=ESMF_MAXSTR) :: varname
    real(rk),dimension(_IRANGE_,_JRANGE_),target :: vs,pom
    integer(ESMF_KIND_I4)      :: localrc

    call ESMF_StateGet(importState,itemSearch="temperature_at_soil_surface",itemCount=itemcount,rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (itemcount==0) then
#ifdef DEBUG
      write(string,'(A)') "No temperature information found, using default value 10 deg_C"
      call ESMF_LogWrite(string,ESMF_LOGMSG_WARNING)
#endif
      bdys(1:_INUM_,1:_JNUM_,1) = 10._rk   ! degC temperature
    else
      call ESMF_StateGet(importState,"temperature_at_soil_surface",field,rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
#ifdef DEBUG
      write(string,'(A)') "Water temperature information found"
      call ESMF_LogWrite(string,ESMF_LOGMSG_INFO)
#endif
      if (sed%grid%use_ugrid) then
        call ESMF_FieldGet(field,farrayPtr=fluxmesh_ptr,rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        bdys(:,1,1) = fluxmesh_ptr(:)
      else
        call ESMF_FieldGet(field,farrayPtr=ptr_f2,rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        bdys(1:_INUM_,1:_JNUM_,1) = ptr_f2(1:_INUM_,1:_JNUM_)   ! get lowest vertical index for near-bed temperature
      endif
    endif

    if (sed%bcup_dissolved_variables .gt. 0) then
    do n=1,sed%nvar
      if (sed%model%state_variables(n)%standard_variable%name/='') then
        varname = &
          trim(sed%model%state_variables(n)%standard_variable%name)
      else
      !> otherwise use CF-ed version of long_name
        varname = trim(only_var_name( &
           sed%model%state_variables(n)%long_name))
      endif
      call ESMF_StateGet(importState,itemSearch=trim(varname)//'_at_soil_surface', &
                         itemCount=itemcount,rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (itemcount==0) then
#ifdef DEBUG
        write(string,'(A)') "Variable '"//trim(varname)//"' not found in State. Skipping."
        call ESMF_LogWrite(string,ESMF_LOGMSG_INFO)
#endif
      else
        call ESMF_StateGet(importState,trim(varname)//'_at_soil_surface',field,rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
#ifdef DEBUG
        if (localrc == ESMF_SUCCESS) write(0,*) 'found field ',trim(varname)
#endif

        if (sed%model%state_variables(n)%properties%get_logical( &
            'particulate',default=.false.)) then
          !write(0,*) 'try to get ',trim(varname)//'_z_velocity'
          call ESMF_StateGet(importState,trim(varname)//'_z_velocity_at_soil_surface', vs_field,rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (sed%grid%use_ugrid) then
            call ESMF_FieldGet(field,farrayPtr=fluxmesh_ptr,rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            call ESMF_FieldGet(vs_field,farrayPtr=fluxmesh_ptr_vs,rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            fluxes(_IRANGE_,1,n) = -fluxmesh_ptr(:)*fluxmesh_ptr_vs(:) ! downward flux is positive
          else
            call ESMF_FieldGet(field,farrayPtr=ptr_f2,rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            call ESMF_FieldGet(vs_field,farrayPtr=ptr_vs_2d,rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            fluxes(_IRANGE_,_JRANGE_,n) = -ptr_f2(:,:)*ptr_vs_2d(:,:) ! downward flux is positive
          endif
#ifdef DEBUG
          write(0,*) '  flux',-fluxes(1,1,n)
#endif
        else
          ptr_f2 => bdys(:,:,n+1)
          if (sed%grid%use_ugrid) then
            call ESMF_FieldGet(field,farrayPtr=fluxmesh_ptr,rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            ptr_f2(:,1) = fluxmesh_ptr(:)
          else
            call ESMF_FieldGet(field,farrayPtr=ptr_f2,rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            bdys(:,:,n+1) = ptr_f2(:,:)
          endif
          if (sed%bcup_dissolved_variables .eq. 1) then

!            call ESMF_StateGet(exportState, trim(varname), field=exportfield, rc=localrc)

!            call mossco_state_get(exportState, &
!        (/'turbulent_kinetic_energy_at_soil_surface'/), tke, verbose=verbose, rc=localrc)
!tke(lbnd(1):ubnd(1),lbnd(2):ubnd(2))

            fluxes(_IRANGE_,_JRANGE_,n) = -(sed%conc(:,:,1,n)-bdys(:,:,n+1))/ &
              sed%grid%dz(:,:,1)*(sed%bioturbation + sed%diffusivity+bdys(:,:,1) * &
              0.035d0)*sed%porosity(:,:,1)/86400._rk/10000._rk
          else
            !> reset fluxes to zero
            fluxes(_IRANGE_,_JRANGE_,n) = 0.0d0
          endif
#ifdef DEBUG
          write(0,*) '  bdys',ptr_f2(1,1)
#endif
        endif !if "particulate"
      endif !if (itemcount==0)
    enddo
    endif !if (sed%bcup_dissolved_variables .gt. 0)

  end subroutine get_boundary_conditions

#undef  ESMF_METHOD
#define ESMF_METHOD "read_config"
!> Read the associated .cfg resource file and save its parameters in
!> the component's attributes
  subroutine read_config(gridComp, kwe, rc)

    implicit none

    type(ESMF_GridComp), intent(inout)               :: gridComp
    type(ESMF_KeyWordEnforcer), intent(in), optional :: kwe
    integer(ESMF_KIND_I4), intent(out), optional     :: rc

    integer(ESMF_KIND_I4)             :: rc_, localRc
    character(len=ESMF_MAXSTR)        :: configFileName, message
    character(len=ESMF_MAXSTR)        :: gridCompName
    logical                           :: labelIsPresent, fileIsPresent
    logical                           :: configIsPresent, configFileIsPresent
    type(ESMF_Config)                 :: config

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    call ESMF_GridCompGet(gridComp, configIsPresent=configIsPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (configIsPresent) then
      call ESMF_GridCompGet(gridComp, configIsPresent=configIsPresent, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    else
      config = ESMF_ConfigCreate(rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_GridCompSet(gridComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    call ESMF_GridCompGet(gridComp, configFileIsPresent=configFileIsPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_GridCompGet(gridComp, name=gridCompName, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (configFileIsPresent) then
      call ESMF_GridCompGet(gridComp, configFile=configFileName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    else
      configFileName=trim(gridCompName)//'.cfg'
    endif

    inquire(file=trim(configfilename), exist=fileIsPresent)
    if (.not. fileIsPresent) then
      configFileName = 'fabm_sediment.cfg'
      inquire(file=trim(configfilename), exist=fileIsPresent)
    endif
    if (.not. fileIsPresent) then
      configFileName = 'fabm_sediment.cfg'
      inquire(file=trim(configfilename), exist=fileIsPresent)
    endif
    if (.not. fileIsPresent) return

    write(message,'(A)')  trim(gridCompName)//' reads configuration from '//trim(configFileName)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_ConfigGet(config, label='dt', value=dt, &
      defaultValue=720.0d0, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A,ES9.2)') trim(gridCompName)// ' found config item dt = ',dt
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_AttributeSet(gridComp, 'timestep', dt, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    call MOSSCO_ConfigGet(config, label='dt_min', value=dt_min, &
      defaultValue=1.0d0, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A,ES9.2)') trim(gridCompName)// ' found config item dt_min = ',dt_min
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_AttributeSet(gridComp, 'minimum_timestep', dt_min, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    call MOSSCO_ConfigGet(config, label='numlayers', value=numlayers, &
      defaultValue=15, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_ConfigGet(config, label='dzmin', value=dzmin, &
      defaultValue=0.004d0, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A,ES9.2)') trim(gridCompName)// ' found config item dzmin = ',dzmin
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_AttributeSet(gridComp, 'minimum_layer_height', dzmin, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    call MOSSCO_ConfigGet(config, label='ode_method', value=ode_method, &
      defaultValue=_ADAPTIVE_EULER_, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_ConfigGet(config, label='relative_change_min', value=relative_change_min, &
      defaultValue=-0.9d0, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A,ES9.2)') trim(gridCompName)// ' found config item relative_change_min = ',relative_change_min
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_AttributeSet(gridComp, 'relative_change_min', relative_change_min, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    call MOSSCO_ConfigGet(config, label='ugrid_name', value=ugrid_name, &
      defaultValue='', isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !> Find out whether the label was specified.  If yes, then require
    !> the file to be present, and return if not found
    inquire(file=trim(ugrid_name), exist=fileIsPresent)

    if (labelIsPresent .and..not. fileIsPresent) then
      write(message, '(A)') trim(gridCompName)//' cannot find '//trim(ugrid_name)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      if (present(rc)) then
        rc = ESMF_RC_FILE_OPEN
        return
      else
        localrc = ESMF_RC_FILE_OPEN
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
      endif
    endif

    call MOSSCO_ConfigGet(config, label='presimulation_years', &
      value=presimulation_years, &
      defaultValue=2, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_ConfigGet(config, label='bcup_dissolved_variables', &
      value=bcup_dissolved_variables, &
      defaultValue=2, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_ConfigGet(config, label='pel_Temp', value=pel_Temp, &
      defaultValue=5.0d0, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A,ES9.2)') trim(gridCompName)// ' found config item pel_Temp = ',pel_Temp
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_AttributeSet(gridComp, 'boundary_temperature', pel_Temp, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    call MOSSCO_ConfigGet(config, label='pel_NO3', value=pel_NO3, &
      defaultValue=5.0d0, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A,ES9.2)') trim(gridCompName)// ' found config item pel_NO3 = ',pel_NO3
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_AttributeSet(gridComp, 'boundary_nitrate', pel_NO3, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    call MOSSCO_ConfigGet(config, label='pel_NH4', value=pel_NH4, &
      defaultValue=5.0d0, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A,ES9.2)') trim(gridCompName)// ' found config item pel_NH4 = ',pel_NH4
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_AttributeSet(gridComp, 'boundary_ammonium', pel_NH4, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    call MOSSCO_ConfigGet(config, label='pel_PO4', value=pel_PO4, &
      defaultValue=0.5d0, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A,ES9.2)') trim(gridCompName)// ' found config item pel_PO4 = ',pel_PO4
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      call ESMF_AttributeSet(gridComp, 'boundary_phosphate', pel_PO4, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif

    call MOSSCO_ConfigGet(config, label='pel_O2', value=pel_NO3, &
      defaultValue=250.0d0, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A,ES9.2)') trim(gridCompName)// ' found config item pel_PO4 = ',pel_PO4
    else
      write(message,'(A,ES9.2)') trim(gridCompName)// ' uses default pel_PO4 = ',pel_PO4
    endif
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    call ESMF_AttributeSet(gridComp, 'boundary_phosphate', pel_PO4, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_ConfigGet(config, label='pflux_lDetC', value=pflux_lDetC, &
      defaultValue=10.0d0, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !> For legacy reasons allow "l"abile carbon to be specified as "f"ast
    if (.not.labelIsPresent) then
      call MOSSCO_ConfigGet(config, label='pflux_fDet', value=pflux_lDetC, &
        defaultValue=10.0d0, isPresent=labelIsPresent, rc = localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (labelIsPresent) then
        write(message,'(A)') trim(gridCompName)// ' found deprecated config item pflux_fDet, please use pflux_lDetC.'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      endif
    endif

    if (labelIsPresent) then
      write(message,'(A,ES9.2)') trim(gridCompName)// ' found config item pflux_lDetC = ',pflux_lDetC
    else
      write(message,'(A,ES9.2)') trim(gridCompName)// ' uses default pflux_lDetC = ',pflux_lDetC
    endif
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    call ESMF_AttributeSet(gridComp, 'boundary_labile_carbon_flux', pflux_lDetC, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_ConfigGet(config, label='pflux_sDetC', value=pflux_sDetC, &
      defaultValue=10.0d0, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !> For legacy reasons allow sDet instead of sDetC
    if (.not.labelIsPresent) then
      call MOSSCO_ConfigGet(config, label='pflux_sDet', value=pflux_sDetC, &
        defaultValue=10.0d0, isPresent=labelIsPresent, rc = localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (labelIsPresent) then
        write(message,'(A)') trim(gridCompName)// ' found deprecated config item pflux_sDet, please use pflux_sDetC.'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      endif
    endif

    if (labelIsPresent) then
      write(message,'(A,ES9.2)') trim(gridCompName)// ' found config item pflux_sDetC = ',pflux_sDetC
    else
      write(message,'(A,ES9.2)') trim(gridCompName)// ' uses default pflux_sDetC = ',pflux_sDetC
    endif
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    call ESMF_AttributeSet(gridComp, 'boundary_semilabile_carbon_flux', pflux_sDetC, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_ConfigGet(config, label='pflux_lDetN', value=pflux_lDetN, &
      defaultValue=1.5d0, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A,ES9.2)') trim(gridCompName)// ' found config item pflux_lDetN = ',pflux_lDetN
    else
      write(message,'(A,ES9.2)') trim(gridCompName)// ' uses default pflux_lDetN = ',pflux_lDetN
    endif
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    call ESMF_AttributeSet(gridComp, 'boundary_labile_nitrogen_flux', pflux_lDetN, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_ConfigGet(config, label='pflux_sDetN', value=pflux_sDetN, &
      defaultValue=1.5d0, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (labelIsPresent) then
      write(message,'(A,ES9.2)') trim(gridCompName)// ' found config item pflux_sDetN = ',pflux_sDetN
    else
      write(message,'(A,ES9.2)') trim(gridCompName)// ' uses default pflux_sDetN = ',pflux_sDetN
    endif
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    call ESMF_AttributeSet(gridComp, 'boundary_semilabile_nitrogen_flux', pflux_sDetN, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_ConfigGet(config, label='pflux_lDetP', value=pflux_lDetP, &
      defaultValue=0.2d0, isPresent=labelIsPresent, rc = localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !> For legacy reasons read detP into lDetP
    if (.not.labelIsPresent) then
      call MOSSCO_ConfigGet(config, label='pflux_detP', value=pflux_lDetP, &
        defaultValue=0.2d0, isPresent=labelIsPresent, rc = localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (labelIsPresent) then
        write(message,'(A)') trim(gridCompName)// ' found deprecated config item pflux_detP, please use pflux_lDetP.'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      endif
    endif

    if (labelIsPresent) then
      write(message,'(A,ES9.2)') trim(gridCompName)// ' found config item pflux_lDetP = ',pflux_lDetP
    else
      write(message,'(A,ES9.2)') trim(gridCompName)// ' uses default pflux_lDetP = ',pflux_lDetP
    endif
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    call ESMF_AttributeSet(gridComp, 'boundary_phosphate_flux', pflux_lDetP, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  end subroutine read_config

#undef ESMF_METHOD
#define ESMF_METHOD "set_boundary_flags"
!> set ESMF attributes "required_flag", "required" and "optional" for
!! all boundary conditions in the importState
  subroutine set_boundary_flags(sed,state)
    type(type_sed)             :: sed
    type(ESMF_State)           :: state
    character(len=ESMF_MAXSTR) :: name,varname,attbasename
    integer                    :: n
    integer(ESMF_KIND_I4)      :: localrc

    name='temperature_at_soil_surface'
    call set_item_flags(state,name,requiredFlag=.true.,requiredRank=2)

    do n=1,size(sed%model%state_variables)
      if (sed%model%state_variables(n)%standard_variable%name/='') then
        varname = &
          trim(sed%model%state_variables(n)%standard_variable%name)
      else
      !> otherwise use CF-ed version of long_name
        varname = trim(only_var_name( &
           sed%model%state_variables(n)%long_name))
      endif
      attbasename=trim(varname)//'_at_soil_surface'
      call set_item_flags(state,attbasename,requiredFlag=.true.,requiredRank=2)

      if (sed%model%state_variables(n)%properties%get_logical( &
            'particulate',default=.false.)) then
        name = trim(varname)//'_z_velocity_at_soil_surface'
        call set_item_flags(state,name,requiredFlag=.true.,requiredRank=2)
      endif
    enddo
  end subroutine set_boundary_flags

end module fabm_sediment_component
