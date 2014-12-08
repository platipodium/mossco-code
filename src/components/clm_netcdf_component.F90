!> @brief Implementation of an ESMF CLM netcdf reader
!! This module contains routines of the CLM atmospheric component.
!!
!! This computer program is part of MOSSCO. 
!! @copyright Copyright (C) 2014, Helmholtz-Zentrum Geesthacht
!! @author Hartmut Kapitza, Carsten Lemmen

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "clm_netcdf_component.F90"

!> This module encapsulates the CLM atmospheric interface.
module clm_netcdf_component
    
    use esmf
    use mossco_component

    use clm_driver, only : CLM_init, CLM_final
    use clm_driver, only : CLM_getrecord, CLM_getdata
    
    implicit none

    private

    integer                     :: print_count  !< counter for printout
    integer                     :: nact         !< number of active variables
    integer, allocatable        :: varids(:)    !< indices of active variables
    real(ESMF_KIND_R8)          :: posx, posy, radius
    real(ESMF_KIND_R8), pointer :: atmos_P(:,:) !< atm. sea-level pressure
    real(ESMF_KIND_R8), pointer :: atmos_U(:,:) !< atm. u-component
    real(ESMF_KIND_R8), pointer :: atmos_V(:,:) !< atm. v-component
    real(ESMF_KIND_R8), pointer :: atmos_T(:,:) !< atm. temperature
    real(ESMF_KIND_R8), pointer :: atmos_Q(:,:) !< atm. relative humidity
    real(ESMF_KIND_R8), pointer :: atmos_C(:,:) !< atm. cloud cover
    real(ESMF_KIND_R8), pointer :: atmos_R(:,:) !< atm. rain rate
    type(ESMF_Field)            :: P_field
    type(ESMF_Field)            :: U_field
    type(ESMF_Field)            :: V_field
    type(ESMF_Field)            :: T_field
    type(ESMF_Field)            :: Q_field
    type(ESMF_Field)            :: C_field
    type(ESMF_Field)            :: R_field
    type(ESMF_Time)             :: clm_time
    type(ESMF_Time)             :: ref_time

    type atm_var
      character (len=4)           :: nam        !< field name
      integer                     :: id         !< netcdf variable ID
      real(4)                     :: scl        !< scale factor
      integer                     :: ndims      !< number of dimensions
      real(ESMF_KIND_R8), pointer :: array(:,:) !< array for atm. variable
      type(ESMF_Field)            :: field      !< ESMF field for atm. variable
    end type atm_var

    type(atm_var), allocatable  :: var(:)
    
    public SetServices

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

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine SetServices

!----------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
  subroutine InitializeP0(gridComp, importState, exportState, parentClock, rc)
 
    implicit none
  
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      
    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)

      type(ESMF_GridComp)  :: gridComp
      type(ESMF_State)     :: importState
      type(ESMF_State)     :: exportState
      type(ESMF_Clock)     :: parentClock
      integer, intent(out) :: rc

      type(ESMF_Grid)             :: grid
      type(ESMF_Field)            :: de_field
      type(ESMF_Config)           :: config
      real(ESMF_KIND_R8), pointer :: de(:,:)
      real(ESMF_KIND_R8), pointer :: coordX(:,:), coordY(:,:)
      real(ESMF_KIND_R8)          :: dx, dy, x0, y0
      real(ESMF_KIND_R8)          :: xc, yc, r
      integer                     :: lbnd(2), ubnd(2)
      integer                     :: ibuf(3)
      integer                     :: ib, ie, jb, je
      integer                     :: ierr, dimid, lrc
      integer                     :: i, j, iprocs, jprocs
      type(ESMF_TimeInterval)     :: d_time
      real(ESMF_KIND_R8)          :: app_time_secs
      integer                     :: ind, inda, nvar, id
      character (len=4), allocatable :: vname(:)
      integer, allocatable        :: active(:), ndims(:), lvarid(:)
      real(4), allocatable        :: vscale(:)
      character (len=2)           :: label2
      character (len=3)           :: label3
     
    character(len=ESMF_MAXSTR)    :: timeString, message, name, configFileName
    logical                       :: clockIsPresent
    type(ESMF_Clock)              :: clock
    type(ESMF_Time)               :: currTime
    integer(ESMF_KIND_I4)         :: localPet, petCount, localrc

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    iprocs = petCount
    jprocs = 1

    ! Load config file for variable selection
    configFileName=trim(name)//'.rc'
    config = ESMF_ConfigCreate(rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ConfigLoadFile(config, configFilename, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ConfigGetAttribute(config, nvar, label='nvar:', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate (lvarid(nvar))
    allocate ( vname(nvar))
    allocate (active(nvar))
    allocate (vscale(nvar))
    allocate ( ndims(nvar))

    do ind=1,nvar
      lvarid(ind) = ind
 
      if ( ind < 10 ) then
          write(label2,"(i1,a)") ind,':'
          call ESMF_ConfigFindLabel(config, label2, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
          write(label3,"(i2,a)") ind,':'
          call ESMF_ConfigFindLabel(config, label3, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_ConfigGetAttribute(config, vname(ind), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_ConfigGetAttribute(config, active(ind), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_ConfigGetAttribute(config, vscale(ind), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_ConfigGetAttribute(config, ndims(ind), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if ( localPet == 0 .and. active(ind) == 1 ) then
        ! print *,"ind,name,ndims=",ind,vname(ind),ndims(ind)
        write(message,'(A,I2,A,I1)') trim(name)//' uses variable ',ind,' '//trim(vname(ind))//' of rank ',ndims(ind)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      endif
    enddo
   ! write(0,*) 'Hurray'

! Create grid and retrieve local loop boundaries
    grid = ESMF_GridCreate(filename="clm_grid.nc",fileFormat=ESMF_FILEFORMAT_SCRIP, &
                             regDecomp=(/iprocs,jprocs/),            &
                             isSphere=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_GridAddCoord(grid, rc=localrc)
    !write(0,*) 'Hurray'
    call ESMF_GridGetCoordBounds(grid, coordDim=1, localDE=0, &
                           computationalLBound=lbnd, &
                           computationalUBound=ubnd, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !write(0,*) 'Hurray'

    ib = lbnd(1)
    ie = ubnd(1)
    jb = lbnd(2)
    je = ubnd(2)

    allocate ( de(ib:ie,jb:je) )

! Create variable type for active variables and fill with information

      nact = sum(active)
      allocate ( var(nact) )

      var%id = pack(lvarid,active>0)

      do ind=1,nact
        var(ind)%nam   = vname (var(ind)%id)
        var(ind)%scl   = vscale(var(ind)%id)
        var(ind)%ndims = ndims (var(ind)%id)
        allocate ( var(ind)%array(ib:ie,jb:je) )
        var(ind)%field = ESMF_FieldCreate(grid, var(ind)%array &
                       , name=var(ind)%nam, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo

!xxxxxxxxxxxxxxxx hier gehts weiter

      allocate ( atmos_P(ib:ie,jb:je) )
      allocate ( atmos_U(ib:ie,jb:je) )
      allocate ( atmos_V(ib:ie,jb:je) )
      allocate ( atmos_T(ib:ie,jb:je) )
      allocate ( atmos_Q(ib:ie,jb:je) )
      allocate ( atmos_C(ib:ie,jb:je) )
      allocate ( atmos_R(ib:ie,jb:je) )

! Create atmospheric fields and have it create the corresponding array internally
      P_field = ESMF_FieldCreate(grid, atmos_P, name="air_pressure_at_sea_level", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      U_field = ESMF_FieldCreate(grid, atmos_U, name="wind_x_velocity_at_10m", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      V_field = ESMF_FieldCreate(grid, atmos_V, name="wind_y_velocity_at_10m", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      T_field = ESMF_FieldCreate(grid, atmos_T, name="air_temperature_at_10m", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      Q_field = ESMF_FieldCreate(grid, atmos_Q, name="HUM", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      C_field = ESMF_FieldCreate(grid, atmos_C, name="CC", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      R_field = ESMF_FieldCreate(grid, atmos_R, name="RR", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

! Create decomposition field
      de_field = ESMF_FieldCreate(grid, de, name="DE", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

! Setup timing
      call ESMF_TimeSet(ref_time, yy=1948, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_ClockGet(clock, currTime=currTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      d_time = currTime - ref_time
      call ESMF_TimeIntervalGet(d_time,s_r8=app_time_secs, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

! Open netcdf file for atmospheric data
      call CLM_init(localPet, app_time_secs, lrc)
      if(lrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

! Find first record of data window
      call CLM_getrecord(localPet, app_time_secs, lrc)
      if(lrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

! Get initial values
      call CLM_getdata(atmos_P, ib, ie, jb, je, 'P')
      call CLM_getdata(atmos_U, ib, ie, jb, je, 'U')
      call CLM_getdata(atmos_V, ib, ie, jb, je, 'V')
      call CLM_getdata(atmos_T, ib, ie, jb, je, 'T')
      call CLM_getdata(atmos_Q, ib, ie, jb, je, 'Q')
      call CLM_getdata(atmos_C, ib, ie, jb, je, 'C')
      call CLM_getdata(atmos_R, ib, ie, jb, je, 'R')

      de = localPet

! Fill export state
      call ESMF_StateAdd(exportState, (/P_field/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateAdd(exportState, (/U_field/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateAdd(exportState, (/V_field/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateAdd(exportState, (/T_field/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateAdd(exportState, (/Q_field/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateAdd(exportState, (/C_field/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateAdd(exportState, (/R_field/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

! Output to netCDF files, seems to be broken on some systems.
#ifdef DEBUG
      print_count = 1
      call ESMF_FieldWrite(P_field, file="atmos_P.nc", timeslice=print_count, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_FieldWrite(U_field, file="atmos_U.nc", timeslice=print_count, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_FieldWrite(V_field, file="atmos_V.nc", timeslice=print_count, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldWrite(T_field, file="atmos_T.nc", timeslice=print_count, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_FieldWrite(Q_field, file="atmos_Q.nc", timeslice=print_count, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_FieldWrite(C_field, file="atmos_C.nc", timeslice=print_count, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_FieldWrite(R_field, file="atmos_R.nc", timeslice=print_count, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldWrite(de_field, file="atmos_de.nc", rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#endif

! Destroy field
      call ESMF_FieldDestroy(de_field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      deallocate ( de )

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    end subroutine InitializeP1

!----------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "Run"

    subroutine Run(gridComp, importState, exportState, parentClock, rc)

      type(ESMF_GridComp)  :: gridComp
      type(ESMF_State)     :: importState
      type(ESMF_State)     :: exportState
      type(ESMF_Clock)     :: parentClock
      integer, intent(out) :: rc

      integer                     :: ib, ie, jb, je, i, j, ierr, lrc
      real(ESMF_KIND_R8)          :: r
      type(ESMF_TimeInterval)     :: d_time
      real(ESMF_KIND_R8)          :: app_time_secs
      
      integer(ESMF_KIND_I4)       :: localPet, petCount, localrc
      character(len=ESMF_MAXSTR)  :: message, name, timeString
      type(ESMF_Time)             :: currTime, time, stopTime
      type(ESMF_Clock)            :: clock


    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet, &
      name=name, clock=clock, rc=localrc)  
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_ClockGet(parentClock,currTime=currTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)    

    call ESMF_ClockSet(clock,currTime=currTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ib = lbound(atmos_T,1)
    ie = ubound(atmos_T,1)
    jb = lbound(atmos_T,2)
    je = ubound(atmos_T,2)

    write(0,*) "Proc ",localPet," time=",trim(timestring)
    d_time = currTime - ref_time
    call ESMF_TimeIntervalGet(d_time,s_r8=app_time_secs, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

! Check data interval
    call CLM_getrecord(localPet, app_time_secs, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

! Get data interpolated in time
      call CLM_getdata(atmos_P, ib, ie, jb, je, 'P')
      call CLM_getdata(atmos_U, ib, ie, jb, je, 'U')
      call CLM_getdata(atmos_V, ib, ie, jb, je, 'V')
      call CLM_getdata(atmos_T, ib, ie, jb, je, 'T')
      call CLM_getdata(atmos_Q, ib, ie, jb, je, 'Q')
      call CLM_getdata(atmos_C, ib, ie, jb, je, 'C')
      call CLM_getdata(atmos_R, ib, ie, jb, je, 'R')

#ifdef debug
! Output to netCDF files, fails on some systems
      print_count = print_count + 1
      call ESMF_FieldWrite(P_field, file="atmos_P.nc", timeslice=print_count, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_FieldWrite(U_field, file="atmos_U.nc", timeslice=print_count, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_FieldWrite(V_field, file="atmos_V.nc", timeslice=print_count, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldWrite(T_field, file="atmos_T.nc", timeslice=print_count, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_FieldWrite(Q_field, file="atmos_Q.nc", timeslice=print_count, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_FieldWrite(C_field, file="atmos_C.nc", timeslice=print_count, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
!     call ESMF_FieldWrite(R_field, file="atmos_R.nc", timeslice=print_count, rc=localrc)
!     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#endif


    call ESMF_ClockGet(clock, stopTime=stopTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    
    call ESMF_ClockAdvance(clock, timeStep=stopTime-currTime, rc=localrc) 
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  
    call MOSSCO_CompExit(gridComp, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

!----------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer              :: ierr
     
    integer(ESMF_KIND_I4)   :: petCount, localPet, localrc
    character(ESMF_MAXSTR)  :: name, message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldDestroy(P_field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldDestroy(U_field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldDestroy(V_field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldDestroy(T_field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldDestroy(Q_field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldDestroy(C_field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldDestroy(R_field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      deallocate ( atmos_P )
      deallocate ( atmos_U )
      deallocate ( atmos_V )
      deallocate ( atmos_T )
      deallocate ( atmos_Q )
      deallocate ( atmos_C )
      deallocate ( atmos_R )

    call ESMF_GridCompGet(gridComp, localPet=localPet, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call CLM_final(localPet)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_ClockDestroy(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

end module clm_netcdf_component
