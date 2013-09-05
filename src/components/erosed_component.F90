
module erosed_component

  use esmf
  use mossco_erosed, only : initerosed, erosed
  use precision, only : fp

  implicit none
! These Parameters are defined in sedparam.inc seperately for delft-routine
integer, parameter :: SEDTYP_NONCOHESIVE_TOTALLOAD = 0
integer, parameter :: SEDTYP_NONCOHESIVE_SUSPENDED = 1
integer, parameter :: SEDTYP_COHESIVE              = 2


  public :: SetServices

  private

  real(ESMF_KIND_R8), pointer :: water_temperature(:,:,:)
  type(ESMF_Field)            :: water_temperature_field
  integer                     :: ubnd(3),lbnd(3)


   integer                                    :: nmlb           ! first cell number
   integer                                    :: nmub           ! last cell number
   integer                                    :: flufflyr       ! switch for fluff layer concept
   integer                                    :: iunderlyr      ! Underlayer mechanism
   integer                                    :: nfrac          ! number of sediment fractions
   real(fp)    , dimension(:,:)    , pointer :: mfluff         ! composition of fluff layer: mass of mud fractions [kg/m2]
   real(fp)    , dimension(:,:)    , pointer :: frac
    !
    ! Local variables
    !
    integer                                     :: i            ! diffusion layer counter
    integer                                     :: l            ! sediment counter
    integer                                     :: nm           ! cell counter
    integer                                     :: nstep        ! cell counter
   ! integer                                     :: istat        ! error flag
    integer     , dimension(:)  , allocatable   :: sedtyp       ! sediment type [-]
    real(fp)                                    :: dt           ! time step [s]
    real(fp)                                    :: g            ! gravitational acceleration [m/s2]
    real(fp)                                    :: morfac       ! morphological scale factor [-]
    real(fp)                                    :: rhow         ! density of water [kg/m3]
    real(fp)                                    :: tend         ! end time of computation [s]
    real(fp)                                    :: tstart       ! start time of computation [s]
    real(fp)    , dimension(:)  , allocatable   :: cdryb        ! dry bed density [kg/m3]
    real(fp)    , dimension(:)  , allocatable   :: chezy        ! Chezy coefficient for hydraulic roughness [m(1/2)/s]
    real(fp)    , dimension(:)  , allocatable   :: h0           ! water depth old time level [m]
    real(fp)    , dimension(:)  , allocatable   :: h1           ! water depth new time level [m]
    real(fp)    , dimension(:)  , allocatable   :: rhosol       ! specific sediment density [kg/m3]
    real(fp)    , dimension(:)  , allocatable   :: sedd50       ! 50% diameter sediment fraction [m]
    real(fp)    , dimension(:)  , allocatable   :: sedd90       ! 90% diameter sediment fraction [m]
    real(fp)    , dimension(:)  , allocatable   :: taub         ! bottom shear stress [N/m2]
    real(fp)    , dimension(:)  , allocatable   :: umod         ! depth averaged flow magnitude [m/s]
    real(fp)    , dimension(:,:), allocatable   :: mass         ! change in sediment composition of top layer, [kg/m2]
    real(fp)    , dimension(:,:), allocatable   :: massfluff    ! change in sediment composition of fluff layer [kg/m2]
    real(fp)    , dimension(:,:), allocatable   :: r0           ! concentration old time level[kg/m3]
    real(fp)    , dimension(:,:), allocatable   :: r1           ! concentration new time level[kg/m3]
    real(fp)    , dimension(:,:), allocatable   :: rn           ! concentration [kg/m3]
    real(fp)    , dimension(:,:), allocatable   :: sink         ! sediment sink flux [m/s]
    real(fp)    , dimension(:,:), allocatable   :: sinkf        ! sediment sink flux fluff layer [m/s]
    real(fp)    , dimension(:,:), allocatable   :: sour         ! sediment source flux [kg/m2/s]
    real(fp)    , dimension(:,:), allocatable   :: sourf        ! sediment source flux fluff layer [kg/m2/s]
    real(fp)    , dimension(:,:), allocatable   :: ws           ! settling velocity [m/s]
    real(fp)    , dimension(:)  , allocatable   :: mudfrac
    logical                                     ::lexist, anymud


  
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

    type(ESMF_GridComp) :: gridComp
    type(ESMF_State)    :: importState, exportState
    type(ESMF_Clock)    :: parentClock
    integer, intent(out)     :: rc

    type(ESMF_Grid)      :: grid
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array)     :: array
    real(ESMF_KIND_R8),dimension(:),pointer :: LonCoord,LatCoord,DepthCoord 

    character(len=19) :: timestring
    type(ESMF_Time)   :: wallTime, clockTime
    type(ESMF_TimeInterval) :: timeInterval
    real(ESMF_KIND_R8) :: dt
    character(len=80)  :: title
    character(len=256) :: din_variable='',pon_variable=''

    ! read 0d namelist

    call ESMF_LogWrite('Initializing Delft erosed component',ESMF_LOGMSG_INFO)
 
    g       = 9.81_fp   ! gravitational acceleration [m/s2]
    rhow    = 1000.0_fp ! density of water [kg/m3]
    !
    !
    ! ================================================================================
    !   USER INPUT  H.N.=> ToDo: namelist
    ! ================================================================================
    !
    nmlb    = 1                 ! first cell number
    nmub    = 1                 ! last cell number
    tstart  = 0.0               ! start time of computation [s]
    tend    = 50000.0            ! end time of computation [s]
    dt      = 100.0             ! time step [s]
    morfac  = 1.0               ! morphological scale factor [-]
    nstep  = (tend-tstart)/dt;  ! number of time steps
    !
    ! -----------------------------------------------------------
    !
    nfrac       = 2             ! number of sediment fractions
    iunderlyr   = 2             ! Underlayer mechanism (default = 1)
    flufflyr    = 1             ! switch for fluff layer concept
                                !  0: no fluff layer (default)
                                !  1: all mud to fluff layer, burial to bed layers
                                !  2: part mud to fluff layer, other part to bed layers (no burial)



   call initerosed( nmlb, nmub, nfrac)

  allocate (cdryb     (nfrac))
    allocate (rhosol    (nfrac))
    allocate (sedd50    (nfrac))
    allocate (sedd90    (nfrac))
    allocate (sedtyp    (nfrac))
    !
    allocate (chezy     (nmlb:nmub))
    allocate (h0        (nmlb:nmub))
    allocate (h1        (nmlb:nmub))
    allocate (umod      (nmlb:nmub))
    allocate (taub      (nmlb:nmub))
    allocate (r0        (nfrac,nmlb:nmub))
    allocate (r1        (nfrac,nmlb:nmub))
    allocate (rn        (nfrac,nmlb:nmub))
    allocate (ws        (nfrac,nmlb:nmub))
    !
    allocate (mass      (nfrac,nmlb:nmub))
    allocate (massfluff (nfrac,nmlb:nmub))
    allocate (sink      (nfrac,nmlb:nmub))
    allocate (sinkf     (nfrac,nmlb:nmub))
    allocate (sour      (nfrac,nmlb:nmub))
    allocate (sourf     (nfrac,nmlb:nmub))

    allocate (frac(nfrac,nmlb:nmub))
    allocate (mfluff(nfrac,nmlb:nmub))
    allocate (mudfrac (nmlb:nmub))


    !
    ! ================================================================================
    !   USER INPUT
    ! ================================================================================
    !
    !   Sediment properties (see also 'sedparams.inc')
    !
    sedtyp(1)   = SEDTYP_NONCOHESIVE_SUSPENDED  ! non-cohesive suspended sediment (sand)
    sedtyp(2)   = SEDTYP_COHESIVE               ! cohesive sediment (mud)
    cdryb       = 1650.0_fp                     ! dry bed density [kg/m3]
    rhosol      = 2650.0_fp                     ! specific density [kg/m3]
    sedd50      = 0.0001_fp                     ! 50% diameter sediment fraction [m]
    sedd90      = 0.0002_fp                     ! 90% diameter sediment fraction [m]

    frac = 0.5_fp
    !
    !   Initial bed composition
    !
    if (iunderlyr==2) then
        if (flufflyr>0) then
            mfluff  = 0.0_fp        ! composition of fluff layer: mass of mud fractions [kg/m2]
        endif
    endif
    !
    !   Initial flow conditions
    !
    chezy   = 50.0_fp       ! Chezy coefficient for hydraulic roughness [m(1/2)/s]
    h1      = 3.0_fp        ! water depth [m]
    umod    = 0.0_fp        ! depth averaged flow magnitude [m/s]
    ws      = 0.001_fp      ! Settling velocity [m/s]
    r1(1,:) = 2.0e-1_fp     ! sediment concentration [kg/m3]
    r1(2,:) = 2.0e-1_fp     ! sediment concentration [kg/m3]
    anymud      = .true.

    do nm = nmlb, nmub
        taub(nm) = umod(nm)*umod(nm)*rhow*g/(chezy(nm)*chezy(nm)) ! bottom shear stress [N/m2]
    enddo

    ! Open file for producing output
    inquire (file ='delft_sediment_test.out', exist = lexist)

    if (lexist) then
        write (*,*) ' The output file "delft_sediment_test.out" already exits. It will be overwritten!!!'
        open (unit = 707, file = 'delft_sediment_test.out', status = 'REPLACE', action = 'WRITE')
    else
        open (unit = 707, file = 'delft_sediment_test.out', status = 'NEW', action = 'WRITE')
    end if

    write (707, '(A4,2x,A8,2x, A5,3x,A10,3x,A11,4x,A5,6x,A7)') &
        'Step','Fractions','layer','Sink(m/s)','Source(m/s)', 'nfrac', 'mudfrac'
    write (*, '(A4,2x,A8,2x, A5,3x,A10,3x,A11,4x,A5,6x,A7)') &
        'Step','Fractions','layer','Sink(m/s)','Source(m/s)', 'nfrac', 'mudfrac'
  

    !> create grid
    distgrid =  ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/1,1,1/), &
                                    indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/1,1,1/),name="Erosed grid")

    !> create export fields
    !array = ESMF_ArrayCreate(distgrid,farray=din%conc,indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    !din_field = ESMF_FieldCreate(grid, array, name="dissolved_inorganic_nitrogen_in_water", rc=rc)
    !array = ESMF_ArrayCreate(distgrid,farray=pon%conc,indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    !pon_field = ESMF_FieldCreate(grid, array, name="particulare_organic_nitrogen_in_water", rc=rc)
    !array = ESMF_ArrayCreate(distgrid,farray=pon%ws,indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    !pon_ws_field = ESMF_FieldCreate(grid, arrayspec, name="pon_sinking_velocity_in_water", rc=rc)
    !> set export state
    !call ESMF_StateAdd(exportState,(/din_field,pon_field,pon_ws_field/),rc=rc)

    !> set clock for 0d component
#if 0
    call ESMF_TimeSet(clockTime)
    if (input_from_namelist) then !> overwrite the parent clock's settings with the namelist parameters
      call ESMF_LogWrite('Get GOTM input from namelist',ESMF_LOGMSG_INFO)
      call ESMF_TimeIntervalSet(timeInterval,s_r8=gotm_time_timestep,rc=rc)
      call ESMF_ClockSet(parentClock,timeStep=timeInterval,rc=rc)

      timestring=gotm_time_start(1:10)//"T"//gotm_time_start(12:19)
      call timestring2ESMF_Time(timestring,clockTime)
      call ESMF_ClockSet(parentClock,startTime=clockTime)
      
      timestring=gotm_time_stop(1:10)//"T"//gotm_time_stop(12:19)
      call timestring2ESMF_Time(timestring,clockTime)
      call ESMF_ClockSet(parentClock,stopTime=clockTime)
    else !> get parent clock and overwrite namelist parameters
      call ESMF_LogWrite('Set GOTM input from ESMF parent',ESMF_LOGMSG_INFO)
      call ESMF_ClockGet(parentClock,startTime=clockTime)
      call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring)
      gotm_time_start=timestring(1:10)//" "//timestring(12:19)

      call ESMF_ClockGet(parentClock,timeStep=timeInterval,rc=rc)
      call ESMF_TimeIntervalGet(timeInterval,s_r8=gotm_time_timestep,rc=rc)
     
      call ESMF_ClockGet(parentClock,stopTime=clockTime)
      call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring)
      gotm_time_stop=timestring(1:10)//" "//timestring(12:19)
      
    endif
#endif
    call ESMF_LogWrite('Initialized Delft erosed component',ESMF_LOGMSG_INFO)
    
  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=19)        :: timestring
    character(len=255)       :: logstring
    type(ESMF_Time)          :: clockTime
    type(ESMF_TimeInterval)  :: timestep
    integer(ESMF_KIND_I8)    :: advancecount
    real(ESMF_KIND_R8)       :: runtimestepcount

    ! get global clock , set n to global/local, call 0d, advance local clock n steps., 
    call ESMF_TimeSet(clockTime)
    call ESMF_ClockGet(parentClock,currTime=clockTime,AdvanceCount=advancecount,&
      runTimeStepCount=runtimestepcount,timeStep=timestep,rc=rc)
    call ESMF_TimeGet(clockTime,timeStringISOFrac=timestring)
    write (logstring,'(A,I6,A,A)') "Erosed run(",advancecount,") at ",timestring
    call ESMF_LogWrite(trim(logstring), ESMF_LOGMSG_INFO)
#if 0
    ! get import state
    if (forcing_from_coupler) then
      call ESMF_StateGet(importState, "water_temperature", water_temperature_field, rc=rc)
      call ESMF_FieldGet(water_temperature_field, farrayPtr=water_temperature, rc=rc)
      zerod%temp = water_temperature(1,1,1)
    end if

#endif

    call getfrac_dummy (anymud,sedtyp,nfrac,nmlb,nmub,frac,mudfrac)

    r0 = r1
    h0 = h1

    !   Computing erosion fluxes
    call erosed( nmlb     , nmub    , flufflyr , mfluff ,frac , mudfrac, &
                & ws        , umod    , h0        , chezy  , taub          , &
                & nfrac     , rhosol  , sedd50   , sedd90 , sedtyp        , &
                & sink      , sinkf   , sour     , sourf                         )

    !   Compute flow
    ! HN. @ToDo: the followings loop can be placed in a Module containing a generic procedure UPDATE
    h1      = h0
    umod    = abs(1.0_fp*sin(2*3.14*advancecount/runtimestepcount))

    ! Loop over all depths
    do nm = nmlb, nmub
      taub(nm) = umod(nm)*umod(nm)*rhow*g/(chezy(nm)*chezy(nm))
    enddo

    !   Updating sediment concentration in water column
    do l = 1, nfrac
            do nm = nmlb, nmub
                rn(l,nm) = r0(l,nm) ! explicit
!                r1(l,nm) = r0(l,nm) + dt*(sour(l,nm) + sourf(l,nm))/h0(nm) - dt*(sink(l,nm) + sinkf(l,nm))*rn(l,nm)/h1(nm)

             write (707, '(I4,4x,I4,4x,I5,4(4x,F8.4))' ) advancecount, l, nm, sink(l,nm), sour (l,nm),frac (l,nm), mudfrac(nm)
             write (*,  '(I4,4x,I4,4x,I5,4(4x,F8.4))' )  advancecount, l, nm, sink(l,nm), sour (l,nm),frac (l,nm), mudfrac(nm)
            enddo
    enddo
        !
        !   Compute change in sediment composition of top layer and fluff layer
        !
    mass       = 0.0_fp    ! change in sediment composition of top layer, [kg/m2]
    massfluff  = 0.0_fp    ! change in sediment composition of fluff layer [kg/m2]
        !
    do l = 1, nfrac
            do nm = nmlb, nmub
                !
                ! Update dbodsd value at nm
                !
                mass(l, nm) = mass(l, nm) + dt*morfac*( sink(l,nm)*rn(l,nm) - sour(l,nm) )
                !
                ! Update dfluff value at nm
                !
                if (flufflyr>0) then
                    massfluff(l, nm) = massfluff(l, nm) + dt*( sinkf(l,nm)*rn(l,nm) - sourf(l,nm) )
                endif
            enddo
    enddo
        !

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    close (707)

    deallocate (cdryb)

    deallocate (rhosol)
    deallocate (sedd50)
    deallocate (sedd90)
    !
    deallocate (chezy)
    deallocate (h0)
    deallocate (h1)
    deallocate (umod)
    deallocate (taub)
    deallocate (r0)
    deallocate (r1)
    deallocate (rn)
    deallocate (ws)
    !
    deallocate (mass)
    deallocate (massfluff)
    deallocate (sink)
    deallocate (sinkf)
    deallocate (sour)
    deallocate (sourf)
    !
    deallocate (mfluff, frac)
    deallocate (sedtyp)
    deallocate (mudfrac)
    !! @todo uncomment next line
    !deallocate (pmcrit , depeff,  depfac, eropar, parfluff0,  parfluff1, &
    !             & tcrdep,  tcrero, tcrfluff)
  end subroutine Finalize
  
end module erosed_component
