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

!> This module encapsulates the CLM atmospheric interface.
module clm_netcdf_component
    
    use esmf

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
    endtype atm_var

    type(atm_var), allocatable  :: var(:)
    
    public SetServices

    contains

    subroutine SetServices(gridComp, rc)

      type(ESMF_GridComp)  :: gridComp
      integer, intent(out) :: rc
      
      rc = ESMF_SUCCESS

      call ESMF_GridCompSetEntryPoint(gridComp, ESMF_METHOD_INITIALIZE, Initialize  &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_GridCompSetEntryPoint(gridComp, ESMF_METHOD_RUN,        Run   &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_GridCompSetEntryPoint(gridComp, ESMF_METHOD_FINALIZE,   atmos_final &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    end subroutine SetServices

!----------------------------------------------------------------------------------

    subroutine Initialize(gridComp, importState, exportState, parentClock, rc)

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
     
    character(len=ESMF_MAXSTR)    :: timeString, message, name
    logical                       :: clockIsPresent
    type(ESMF_Clock)              :: clock
    type(ESMF_Time)               :: currTime
    integer(ESMF_KIND_I4)         :: localPet, petCount

    rc = ESMF_SUCCESS
     
    !! Check whether there is already a clock (it might have been set 
    !! with a prior ESMF_gridCompCreate() call.  If not, then create 
    !! a local clock as a clone of the parent clock, and associate it
    !! with this component.  Finally, set the name of the local clock
    call ESMF_GridCompGet(gridComp, name=name, clockIsPresent=clockIsPresent, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (clockIsPresent) then
      call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)     
    else
      clock = ESMF_ClockCreate(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_GridCompSet(gridComp, clock=clock, rc=rc)    
    endif
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockSet(clock, name=trim(name)//' clock', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    !! Log the call to this function
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initializing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    call ESMF_GridCompGet(gridComp, localPet=localPet, petCount=petCount, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !> Load config file for atmospheric component
    config = ESMF_ConfigCreate(rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ConfigLoadFile(config, "atmos.rc", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ConfigGetAttribute(config, iprocs, label='iprocs:', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ConfigGetAttribute(config, jprocs, label='jprocs:', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ConfigDestroy(config, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

! Load config file for variable selection
    config = ESMF_ConfigCreate(rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ConfigLoadFile(config, "config.rc", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ConfigGetAttribute(config, nvar, label='nvar:', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    allocate (lvarid(nvar))
    allocate ( vname(nvar))
    allocate (active(nvar))
    allocate (vscale(nvar))
    allocate ( ndims(nvar))

    do ind=1,nvar

      lvarid(ind) = ind

      if ( ind < 10 ) then
          write(label2,"(i1,a)") ind,':'
          call ESMF_ConfigFindLabel(config, label2, rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      else
          write(label3,"(i2,a)") ind,':'
          call ESMF_ConfigFindLabel(config, label3, rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      endif

      call ESMF_ConfigGetAttribute(config, vname(ind), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_ConfigGetAttribute(config, active(ind), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_ConfigGetAttribute(config, vscale(ind), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_ConfigGetAttribute(config, ndims(ind), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      if ( localPet == 0 .and. active(ind) == 1 ) then
        ! print *,"ind,name,ndims=",ind,vname(ind),ndims(ind)
        write(message,'(A,I2,A,I1)') trim(name)//' uses variable ',ind,' '//trim(vname(ind))//' of rank ',ndims(ind)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      endif
    enddo

! Create grid and retrieve local loop boundaries
    grid = ESMF_GridCreate(filename="clm_grid.nc",fileFormat=ESMF_FILEFORMAT_SCRIP, &
                             regDecomp=(/iprocs,jprocs/),            &
                             isSphere=.false., rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridGetCoordBounds(grid, coordDim=1, localDE=0, &
                           computationalLBound=lbnd, &
                           computationalUBound=ubnd, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

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
                       , name=var(ind)%nam, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
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
      P_field = ESMF_FieldCreate(grid, atmos_P, name="air_pressure_at_sea_level", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      U_field = ESMF_FieldCreate(grid, atmos_U, name="wind_x_velocity_at_10m", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      V_field = ESMF_FieldCreate(grid, atmos_V, name="wind_y_velocity_at_10m", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      T_field = ESMF_FieldCreate(grid, atmos_T, name="air_temperature_at_10m", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      Q_field = ESMF_FieldCreate(grid, atmos_Q, name="HUM", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      C_field = ESMF_FieldCreate(grid, atmos_C, name="CC", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      R_field = ESMF_FieldCreate(grid, atmos_R, name="RR", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

! Create decomposition field
      de_field = ESMF_FieldCreate(grid, de, name="DE", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

! Setup timing
      call ESMF_TimeSet(ref_time, yy=1948, rc=rc)
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      d_time = currTime - ref_time
      call ESMF_TimeIntervalGet(d_time,s_r8=app_time_secs, rc=rc)

! Open netcdf file for atmospheric data
      call CLM_init(localPet, app_time_secs, lrc)
      if(lrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

! Find first record of data window
      call CLM_getrecord(localPet, app_time_secs, lrc)
      if(lrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

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
      call ESMF_StateAdd(exportState, (/P_field/), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateAdd(exportState, (/U_field/), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateAdd(exportState, (/V_field/), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateAdd(exportState, (/T_field/), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateAdd(exportState, (/Q_field/), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateAdd(exportState, (/C_field/), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateAdd(exportState, (/R_field/), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

! Output to netCDF files, seems to be broken on some systems.
#ifdef DEBUG
      print_count = 1
      call ESMF_FieldWrite(P_field, file="atmos_P.nc", timeslice=print_count, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!     call ESMF_FieldWrite(U_field, file="atmos_U.nc", timeslice=print_count, rc=rc)
!     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!     call ESMF_FieldWrite(V_field, file="atmos_V.nc", timeslice=print_count, rc=rc)
!     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldWrite(T_field, file="atmos_T.nc", timeslice=print_count, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!     call ESMF_FieldWrite(Q_field, file="atmos_Q.nc", timeslice=print_count, rc=rc)
!     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!     call ESMF_FieldWrite(C_field, file="atmos_C.nc", timeslice=print_count, rc=rc)
!     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!     call ESMF_FieldWrite(R_field, file="atmos_R.nc", timeslice=print_count, rc=rc)
!     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldWrite(de_field, file="atmos_de.nc", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
#endif

! Destroy field
      call ESMF_FieldDestroy(de_field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      deallocate ( de )

    !! Log the successful completion of this function
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initialized'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)


    end subroutine Initialize

!----------------------------------------------------------------------------------

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
      
      integer(ESMF_KIND_I4)       :: localPet, petCount
      character(len=ESMF_MAXSTR)  :: message, name, timeString
      type(ESMF_Time)             :: currTime, time
      type(ESMF_Clock)            :: clock


    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet, &
      name=name, clock=clock, rc=rc)  
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockGet(parentClock,currTime=currTime, rc=rc)
    call ESMF_ClockSet(clock,currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' running ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    ib = lbound(atmos_T,1)
    ie = ubound(atmos_T,1)
    jb = lbound(atmos_T,2)
    je = ubound(atmos_T,2)

    write(0,*) "Proc ",localPet," time=",trim(timestring)
    d_time = currTime - ref_time
    call ESMF_TimeIntervalGet(d_time,s_r8=app_time_secs, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

! Check data interval
    call CLM_getrecord(localPet, app_time_secs, lrc)
    if(lrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

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
      call ESMF_FieldWrite(P_field, file="atmos_P.nc", timeslice=print_count, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!     call ESMF_FieldWrite(U_field, file="atmos_U.nc", timeslice=print_count, rc=rc)
!     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!     call ESMF_FieldWrite(V_field, file="atmos_V.nc", timeslice=print_count, rc=rc)
!     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldWrite(T_field, file="atmos_T.nc", timeslice=print_count, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!     call ESMF_FieldWrite(Q_field, file="atmos_Q.nc", timeslice=print_count, rc=rc)
!     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!     call ESMF_FieldWrite(C_field, file="atmos_C.nc", timeslice=print_count, rc=rc)
!     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!     call ESMF_FieldWrite(R_field, file="atmos_R.nc", timeslice=print_count, rc=rc)
!     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
#endif

    do 
      call ESMF_ClockAdvance(clock, rc=rc) 
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
      if (ESMF_ClockIsStopTime(clock, rc=rc)) exit
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)           
    enddo

    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    write(message,'(A,A)') trim(timeString)//' '//trim(name), &
          ' finished running.'
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=rc);

    end subroutine Run

!----------------------------------------------------------------------------------

    subroutine atmos_final(gridComp, importState, exportState, parentClock, rc)

      type(ESMF_GridComp)  :: gridComp
      type(ESMF_State)     :: importState
      type(ESMF_State)     :: exportState
      type(ESMF_Clock)     :: parentClock
      integer, intent(out) :: rc

      integer              :: localPet, ierr
     
      call ESMF_GridCompGet(gridComp, localPet=localPet, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldDestroy(P_field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldDestroy(U_field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldDestroy(V_field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldDestroy(T_field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldDestroy(Q_field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldDestroy(C_field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldDestroy(R_field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      deallocate ( atmos_P )
      deallocate ( atmos_U )
      deallocate ( atmos_V )
      deallocate ( atmos_T )
      deallocate ( atmos_Q )
      deallocate ( atmos_C )
      deallocate ( atmos_R )

      call CLM_final(localPet)

      call ESMF_LogWrite("CLM atmos_final called", ESMF_LOGMSG_INFO)

    end subroutine atmos_final

end module clm_netcdf_component
