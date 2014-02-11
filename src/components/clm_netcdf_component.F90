!>  @file
!! This module contains routines of the CLM atmospheric component.

!> This module encapsulates the CLM atmospheric interface.
module clm_netcdf_component
    
    use ESMF

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

    subroutine SetServices(gcomp, rc)

      type(ESMF_GridComp)  :: gcomp
      integer, intent(out) :: rc

      call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, atmos_init  &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN,        atmos_run   &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE,   atmos_final &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    end subroutine SetServices

!----------------------------------------------------------------------------------

    subroutine atmos_init(gcomp, importState, exportState, externalclock, rc)

      type(ESMF_GridComp)  :: gcomp
      type(ESMF_State)     :: importState
      type(ESMF_State)     :: exportState
      type(ESMF_Clock)     :: externalclock
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
      integer                     :: myrank, ierr, dimid, lrc
      integer                     :: i, j, iprocs, jprocs
      character (len=ESMF_MAXSTR) :: timestring
      type(ESMF_Time)             :: app_time
      type(ESMF_TimeInterval)     :: d_time
      real(ESMF_KIND_R8)          :: app_time_secs
      integer                     :: ind, inda, nvar, id
      character (len=4), allocatable :: vname(:)
      integer, allocatable        :: active(:), ndims(:), lvarid(:)
      real(4), allocatable        :: vscale(:)
      character (len=2)           :: label2
      character (len=3)           :: label3
     
      call ESMF_LogWrite("CLM atmos_init called", ESMF_LOGMSG_INFO)

      call ESMF_GridCompGet(gcomp, localPet=myrank, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

! Load config file for atmospheric component
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

        if ( myrank == 0 .and. active(ind) == 1 ) print *,"ind,name,ndims=",ind,vname(ind),ndims(ind)

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
      call ESMF_ClockGet(externalclock, currtime=app_time, rc=rc)
      d_time = app_time - ref_time
      call ESMF_TimeIntervalGet(d_time,s_r8=app_time_secs, rc=rc)

! Open netcdf file for atmospheric data
      call CLM_init(myrank, app_time_secs, lrc)
      if(lrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

! Find first record of data window
      call CLM_getrecord(myrank, app_time_secs, lrc)
      if(lrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

! Get initial values
      call CLM_getdata(atmos_P, ib, ie, jb, je, 'P')
      call CLM_getdata(atmos_U, ib, ie, jb, je, 'U')
      call CLM_getdata(atmos_V, ib, ie, jb, je, 'V')
      call CLM_getdata(atmos_T, ib, ie, jb, je, 'T')
      call CLM_getdata(atmos_Q, ib, ie, jb, je, 'Q')
      call CLM_getdata(atmos_C, ib, ie, jb, je, 'C')
      call CLM_getdata(atmos_R, ib, ie, jb, je, 'R')

      de = myrank

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

! Output to netCDF files
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

! Destroy field
      call ESMF_FieldDestroy(de_field, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      deallocate ( de )

    end subroutine atmos_init

!----------------------------------------------------------------------------------

    subroutine atmos_run(gcomp, importState, exportState, externalclock, rc)

      type(ESMF_GridComp)  :: gcomp
      type(ESMF_State)     :: importState
      type(ESMF_State)     :: exportState
      type(ESMF_Clock)     :: externalclock
      integer, intent(out) :: rc

      integer                     :: myrank, ib, ie, jb, je, i, j, ierr, lrc
      character (len=ESMF_MAXSTR) :: timestring
      character (len=ESMF_MAXSTR) :: message
      real(ESMF_KIND_R8)          :: r
      type(ESMF_Time)             :: app_time
      type(ESMF_TimeInterval)     :: d_time
      real(ESMF_KIND_R8)          :: app_time_secs

      ib = lbound(atmos_T,1)
      ie = ubound(atmos_T,1)
      jb = lbound(atmos_T,2)
      je = ubound(atmos_T,2)
     
      call ESMF_GridCompGet(gcomp, localPet=myrank, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_ClockGet(externalclock, currtime=app_time, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_TimeGet(app_time, timeString=timestring, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      message = "atmos_run called at "//trim(timestring)
      call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)
      print *, "Proc ",myrank," time=",trim(timestring)
      d_time = app_time - ref_time
      call ESMF_TimeIntervalGet(d_time,s_r8=app_time_secs, rc=rc)

! Check data interval
      call CLM_getrecord(myrank, app_time_secs, lrc)
      if(lrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

! Get data interpolated in time
      call CLM_getdata(atmos_P, ib, ie, jb, je, 'P')
      call CLM_getdata(atmos_U, ib, ie, jb, je, 'U')
      call CLM_getdata(atmos_V, ib, ie, jb, je, 'V')
      call CLM_getdata(atmos_T, ib, ie, jb, je, 'T')
      call CLM_getdata(atmos_Q, ib, ie, jb, je, 'Q')
      call CLM_getdata(atmos_C, ib, ie, jb, je, 'C')
      call CLM_getdata(atmos_R, ib, ie, jb, je, 'R')

! Output to netCDF files
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

    end subroutine atmos_run

!----------------------------------------------------------------------------------

    subroutine atmos_final(gcomp, importState, exportState, externalclock, rc)

      type(ESMF_GridComp)  :: gcomp
      type(ESMF_State)     :: importState
      type(ESMF_State)     :: exportState
      type(ESMF_Clock)     :: externalclock
      integer, intent(out) :: rc

      integer              :: myrank, ierr
     
      call ESMF_GridCompGet(gcomp, localPet=myrank, rc=rc)
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

      call CLM_final(myrank)

      call ESMF_LogWrite("CLM atmos_final called", ESMF_LOGMSG_INFO)

    end subroutine atmos_final

end module clm_netcdf_component
