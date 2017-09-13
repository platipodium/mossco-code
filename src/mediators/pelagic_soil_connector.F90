!> @brief Implementation of an ESMF soil to pelagic mediation
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2015, 2016, 2017 Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister
!> @author Carsten Lemmen
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "pelagic_soil_connector.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#define RANGE2D lbnd(1):ubnd(1),lbnd(2):ubnd(2)
#define RANGE3D lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)

module pelagic_soil_connector

  use esmf
  use mossco_state
  use mossco_field
  use mossco_component

  implicit none

  private
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: DETN,DIN,vDETN
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: DIP=>null(),DETP,vDETP
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: vDETC=>null(),DETC=>null()
  real(ESMF_KIND_R8),dimension(:,:,:), pointer :: nit,amm
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: oxy=>null(),odu=>null()
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: depth=>null()
  real(ESMF_KIND_R8),dimension(:,:),   pointer :: tke=>null()

  !> parameters
  real(ESMF_KIND_R8) :: sinking_factor=0.3d0 !> 30% of Det sinks into sediment
  real(ESMF_KIND_R8) :: NC_ldet=0.20d0
  real(ESMF_KIND_R8) :: NC_sdet=0.04d0
  real(ESMF_KIND_R8) :: sinking_factor_min=0.02 !> minimum of 2% of Det sinks always into sediment
  real(ESMF_KIND_R8) :: half_sedimentation_depth=0.1 !> [m] use 50% of prescribed sinking factor at this depth
  real(ESMF_KIND_R8) :: half_sedimentation_tke=1.0d3 !> [m2/s2] use 50% of prescribed sinking factor for this tke
  real(ESMF_KIND_R8) :: critical_detritus=60.0 !> [mmolC/m3] use minimum sinking for det above critical_detritus


  public SetServices

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(cplComp, rc)

    implicit none

    type(ESMF_CplComp)   :: cplComp
    integer, intent(out) :: rc

    integer              :: localrc

    rc = ESMF_SUCCESS

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_RUN, Run, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
  subroutine InitializeP0(cplComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_cplComp)    :: cplComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer              :: localrc
    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)       :: currTime

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(cplComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_AttributeSet(cplComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call MOSSCO_CompExit(cplComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(cplcomp, importState, exportState, externalclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer, intent(out) :: rc

    type(ESMF_Field)            :: newfield
    character(len=ESMF_MAXSTR)  :: name, message, stateName, fieldName, geomName
    type(ESMF_Time)             :: currTime, stopTime
    integer                     :: localrc, i
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_STATEITEM_Flag), allocatable  :: itemTypeList(:)
    type(ESMF_STATEITEM_Flag)   :: stateItem, itemType
    type(ESMF_FIELDSTATUS_Flag) :: fieldStatus
    type(ESMF_GEOMTYPE_Flag)    :: geomType
    logical                     :: found = .false.

    type(ESMF_Grid)             :: grid
    type(ESMF_Field)            :: field
    integer(ESMF_KIND_I4)       :: rank, ubnd(2), lbnd(2), itemCount
    real(ESMF_KIND_R8),dimension(:,:,:), pointer :: ptr_f3 => null()
    real(ESMF_KIND_R8),dimension(:,:),   pointer :: ptr_f2 => null()
    logical                     :: isPresent
    integer                     :: nmlunit=127

    namelist /pelagic_soil_connector/ sinking_factor,sinking_factor_min,NC_ldet,NC_sdet, &
                                      half_sedimentation_depth,critical_detritus, &
                                      half_sedimentation_tke

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, externalClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    !read namelist
    inquire(file=trim(name)//'.nml', exist=isPresent)
    if (isPresent) then
      open(nmlunit,file='pelagic_soil_connector.nml',action='read',status='old')
      read(nmlunit,pelagic_soil_connector)
      close(nmlunit)
    endif

    !> @todo: check for necessary fields in export state?

    call MOSSCO_CompExit(cplComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

  end subroutine InitializeP1


#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
 subroutine Run(cplcomp, importState, exportState, externalclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer, intent(out) :: rc

    logical  :: hasAmmonium = .false.
    logical  :: hasNitrate = .false.
    logical  :: hasDIN = .false.
    logical  :: hasDIP = .false.

    integer                     :: myrank
    integer                     :: i,j,inum,jnum
    integer                     :: lbnd(3)=1,ubnd(3)=1
    integer                     :: lbnd_2nd(3)=1,ubnd_2nd(3)=1
    integer                     :: Clbnd(3),AMMlbnd(3),Plbnd(3)
    integer                     :: Cubnd(3),AMMubnd(3),Pubnd(3)
    type(ESMF_Time)             :: localtime
    character (len=ESMF_MAXSTR) :: timestring
    type(ESMF_Field)            :: field
    real(ESMF_KIND_R8),dimension(:,:),pointer :: CN_det=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: fac_fdet=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: fac_sdet=>null()
    real(ESMF_KIND_R8),dimension(:,:),pointer :: fac_env=>null()
    real(ESMF_KIND_R8),dimension(:,:,:), pointer :: ptr_f3 => null()
    real(ESMF_KIND_R8),dimension(:,:,:), pointer :: ptr_f3_2nd => null()
    real(ESMF_KIND_R8),dimension(:,:),   pointer :: ptr_f2 => null()

    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime, stopTime
    integer                     :: localrc, oxyrc, odurc, fieldCount
    integer(ESMF_KIND_I8)       :: advanceCount
    logical                     :: verbose=.true.
    type(ESMF_Field), allocatable :: fieldList(:)

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, externalClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_ClockGet(externalClock, advanceCount=advanceCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    if (advanceCount > 0) verbose=.false.
    !> fdet + sdet = CN_det*det
    !> NC_ldet*fdet + NC_sdet*sdet = det
    !> fdet = fac_fdet*det
    !> sdet = fac_sdet*det

    ! Transfer water temperature from pelagic 3D import to a soil surface 2D
    ! export
    !>@TODO Carsten: Was passiert, wenn wir kein PAR haben?
    call mossco_state_get(importState, (/
      'photosynthetically_active_radiation_in_water',
      'radiation_in_water                          ',
      'downwelling_photosynthetic_radiative_flux   '/),  &
      ptr_f3, lbnd=lbnd, ubnd=ubnd, verbose=verbose, rc=localrc)
    if (localrc == ESMF_SUCCESS) then
      call mossco_state_get(exportState,(/'photosynthetically_active_radiation_at_soil_surface'/), &
        ptr_f2,verbose=verbose, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)
      ptr_f2 = ptr_f3(RANGE2D,lbnd(3)) !>@TODO: eine halbe schicht tiefer gehen, damit man das Licht "at soil surface" bekommt
      nullify(ptr_f2)
    end if
    nullify(ptr_f3)

    ! Transfer water temperature from pelagic 3D import to a soil surface 2D
    ! export
    call mossco_state_get(importState, (/'temperature_in_water'/), ptr_f3, &
      lbnd=lbnd, ubnd=ubnd, verbose=verbose, rc=localrc)
    if (localrc == ESMF_SUCCESS) then
      call mossco_state_get(exportState,(/'temperature_at_soil_surface'/), &
        ptr_f2,verbose=verbose, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      ptr_f2 = ptr_f3(RANGE2D,lbnd(3))
      nullify(ptr_f2)
    end if
    nullify(ptr_f3)

    ! dissolved_oxygen:
    call mossco_state_get(importState,(/ &
        'concentration_of_dissolved_oxygen_in_water', &
        'oxygen_in_water                           ', &
        'dissolved_oxygen_oxy_in_water             ', &
        'dissolved_oxygen_in_water                 '/), &
        ptr_f3,lbnd=lbnd,ubnd=ubnd, verbose=verbose, rc=oxyrc)

    ! dissolved_reduced_substances:
    call mossco_state_get(importState,(/ &
        'dissolved_reduced_substances_odu_in_water ', &
        'dissolved_reduced_substances_in_water     '/), &
        ptr_f3_2nd,lbnd=lbnd_2nd,ubnd=ubnd_2nd,verbose=verbose, rc=odurc)

    if (oxyrc == ESMF_SUCCESS) then
      call mossco_state_get(exportState,(/'dissolved_oxygen_at_soil_surface'/), &
        ptr_f2,verbose=verbose, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      if (.not.associated(oxy)) allocate(oxy(RANGE2D))
      if (.not.associated(odu)) allocate(odu(RANGE2D))

      if (odurc == 0) then
        oxy = ptr_f3(RANGE2D,lbnd(3))
        odu = ptr_f3_2nd(RANGE2D,lbnd(3))
      else
        ! assume that negative oxygen is amount of reduced substances
        do j=lbnd(2),ubnd(2)
          do i=lbnd(1),ubnd(1)
            oxy(i,j) = max(0.0d0,ptr_f3(i,j,lbnd(3)))
            odu(i,j) = max(0.0d0,-ptr_f3(i,j,lbnd(3)))
          end do
        end do
      end if
      ptr_f2 = oxy(:,:)

      call mossco_state_get(exportState, &
        (/'dissolved_reduced_substances_at_soil_surface'/), &
        ptr_f2, verbose=verbose, rc=odurc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)
      ptr_f2 = odu(:,:)
    end if
    nullify(ptr_f3)

      !   Det flux:
    call mossco_state_get(importState,(/ &
      'detritus_in_water              ', &
      'detN_in_water                  ', &
      'Detritus_Nitrogen_detN_in_water'/), &
      DETN,lbnd=lbnd,ubnd=ubnd, verbose=verbose, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

#if DEBUG
    if ( ubnd(1)-lbnd(1)<0 .or. ubnd(2)-lbnd(2)<0 .or. ubnd(3)-lbnd(3)<0 ) then
      write(message,'(A)')  trim(name)//' received zero-length data for detritus nitrogen'
      write(0,*) 'lbnd = ', lbnd, 'ubnd = ', ubnd

      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif
#endif

    call mossco_state_get(importState,(/ &
            'detritus_z_velocity_in_water              ', &
            'detN_z_velocity_in_water                  ', &
            'Detritus_Nitrogen_detN_z_velocity_in_water'/),vDETN, verbose=verbose, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    inum=ubnd(1)-lbnd(1)+1
    jnum=ubnd(2)-lbnd(2)+1
    if (.not.associated(CN_det)) allocate(CN_det(1:inum,1:jnum))
    if (.not.associated(fac_fdet)) allocate(fac_fdet(1:inum,1:jnum))
    if (.not.associated(fac_sdet)) allocate(fac_sdet(1:inum,1:jnum))
    if (.not.associated(fac_env)) allocate(fac_env(1:inum,1:jnum))

    !> search for Detritus-C, if present, use Detritus C-to-N ratio and apply flux
    call mossco_state_get(importState,(/'Detritus_Carbon_detC_in_water'/), &
      DETC,lbnd=Clbnd,ubnd=Cubnd, verbose=verbose, rc=localrc)

    if (localrc /= 0) then
      CN_det=106.0d0/16.0d0
    else
        if ( Cubnd(1)-Clbnd(1)<0 .or. Cubnd(2)-Clbnd(2)<0 .or. Cubnd(3)-Clbnd(3)<0 ) then
          write(message,'(A)')  trim(name)//' received zero-length data for detritus carbon'
          write(0,*) 'Clbnd = ', lbnd, 'Cubnd = ', ubnd

          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          call ESMF_LogFlush()
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif

        CN_det = DETC(Clbnd(1):Cubnd(1),Clbnd(2):Cubnd(2),Clbnd(3))/ &
                  (1E-5 + DETN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3)))

    end if

    fac_fdet = (1.0d0-NC_sdet*CN_det)/(NC_ldet-NC_sdet)

! dirty=non-mass-conserving fix added by kw against unphysical partitioning

    where (fac_fdet .gt. CN_det)
      fac_fdet = CN_det
    endwhere

    where (fac_fdet .lt. 0.0d0)
      fac_fdet = 0.0d0
    endwhere

    fac_sdet = CN_det - fac_fdet
!    fac_sdet = (1.0d0-NC_ldet*CN_det)/(NC_sdet-NC_ldet)

    ! get depth from exportState, where the physical model has put its data
    call mossco_state_get(exportState, &
      (/'water_depth_at_soil_surface'/), depth, verbose=verbose, rc=localrc)

    fac_env = 1.0d0
    if (localrc == 0 .and. half_sedimentation_depth .gt. 1E-3) then
      ! reduce sedimentation due to depth (assuming higher wave erosion in shallow areas)
      fac_env = fac_env * depth(lbnd(1):ubnd(1),lbnd(2):ubnd(2))**2/(depth(lbnd(1):ubnd(1),lbnd(2):ubnd(2))**2 + half_sedimentation_depth**2)
    end if
    ! ensure minimum sedimentation

    ! get TKE from exportState, where the physical model has put its data
    call mossco_state_get(exportState, &
      (/'turbulent_kinetic_energy_at_soil_surface'/), tke, verbose=verbose, rc=localrc)

    if (localrc == 0 .and. half_sedimentation_tke .lt. 9E9 ) then
      ! reduce sedimentation due to depth (assuming higher wave erosion in shallow areas)
      fac_env = fac_env * half_sedimentation_tke/(tke(lbnd(1):ubnd(1),lbnd(2):ubnd(2)) + half_sedimentation_tke)
    end if
    fac_env = fac_env + sinking_factor_min/sinking_factor

    ! reduce sedimentation due to detritus-C (assuming higher DETN in shallow areas)
    if (associated(DETC)) then
      if (critical_detritus .gt. 1E-3 .and. critical_detritus .lt. 9E9) then
        fac_env = fac_env * 1.0d0/(1.0d0 + &
          (DETC(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))/critical_detritus)**4)
      end if
    end if

    call mossco_state_get(exportState, (/'detritus_labile_carbon_at_soil_surface'/), &
      ptr_f2, verbose=verbose, rc=localrc)
    if (localrc==0) ptr_f2 = fac_fdet * DETN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))

    call mossco_state_get(exportState, (/'detritus_semilabile_carbon_at_soil_surface'/),&
      ptr_f2, verbose=verbose, rc=localrc)
    if(localrc==0) ptr_f2 = fac_sdet * DETN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))

    call mossco_state_get(exportState, (/'detritus_labile_carbon_z_velocity_at_soil_surface'/), &
      ptr_f2, verbose=verbose, rc=localrc)
    if (localrc==0) ptr_f2 = sinking_factor * fac_env * vDETN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))
    call mossco_state_get(exportState, (/'detritus_semilabile_carbon_z_velocity_at_soil_surface'/), &
        ptr_f2, verbose=verbose, rc=localrc)
    if (localrc==0) ptr_f2 = sinking_factor * fac_env * vDETN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))

    !> check for Detritus-P and calculate flux either N-based
    !> or as present through the Detritus-P pool
    call mossco_state_get(exportState, (/'detritus_phosphorus_at_soil_surface'/), &
       ptr_f2, verbose=verbose, rc=localrc)
    call mossco_state_get(importState,(/ &
          'detP_in_water                    ', &
          'Detritus_Phosphorus_detP_in_water'/), &
          DETP,lbnd=Plbnd,ubnd=Pubnd, verbose=verbose, rc=localrc)
    if (localrc == 0) then
        ptr_f2 = DETP(Plbnd(1):Pubnd(1),Plbnd(2):Pubnd(2),plbnd(3))
    else
        ptr_f2 = 1.0d0/16.0d0 * DETN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))
    end if

    call mossco_state_get(exportState, &
        (/'detritus_phosphorus_z_velocity_at_soil_surface'/), &
        ptr_f2, verbose=verbose, rc=localrc)
    call mossco_state_get(importState,(/ &
              'detP_z_velocity_in_water                    ', &
              'Detritus_Phosphorus_detP_z_velocity_in_water'/), &
              vDETP, verbose=verbose, rc=localrc)
    if (localrc==0) then
        ptr_f2 = sinking_factor * fac_env * vDETP(Plbnd(1):Pubnd(1),Plbnd(2):Pubnd(2),Plbnd(3))
    else
        ptr_f2 = sinking_factor * fac_env * vDETN(lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3))
    end if

    ! Dissolved inorganic matter, i.e. nitrate, ammonium or DIN
    call MOSSCO_StateGet(importState, fieldList, itemSearchList= (/ &
      'nitrate_in_water'/), &
      fieldCount=fieldCount, fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    if (fieldCount > 0) then
      hasNitrate = .true.
      call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=lbnd, &
        exclusiveUBound=ubnd, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      call ESMF_FieldGet(fieldList(1), farrayPtr=nit, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      write(message,'(A)') trim(name)//' obtains DIN from '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call MOSSCO_StateGet(importState, fieldList, itemSearchList= (/ &
    'nutrients_in_water                            ', &
    'DIN_in_water                                  ', &
    'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water'/), &
      fieldCount=fieldCount, fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    if (fieldCount > 0) then
      hasDIN = .true.
      call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=lbnd, &
        exclusiveUBound=ubnd, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      call ESMF_FieldGet(fieldList(1), farrayPtr=din, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      write(message,'(A)') trim(name)//' obtains DIN from '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call MOSSCO_StateGet(importState, fieldList, itemSearchList= (/ &
      'ammonium_in_water              ', &
      'dissolved_ammonium_nh3_in_water'/), &
      fieldCount=fieldCount, fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    if (fieldCount > 0) then
      hasAmmonium = .true.
      call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=AMMlbnd, &
        exclusiveUBound=AMMubnd, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      call ESMF_FieldGet(fieldList(1), farrayPtr=amm, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      write(message,'(A)') trim(name)//' obtains ammonium from '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    ! Get mandatory ammonium field from export State and save either
    ! the pelagic ammonium (if present) or ammonium derived from combinations
    ! of NO3 and DIN

    call MOSSCO_StateGet(exportState, fieldList, &
      itemSearch='mole_concentration_of_ammonium_at_soil_surface', &
      fieldCount=fieldCount, fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    if (fieldCount /= 1) then
      write(message,'(A,I1)') 'Expected exactly one complete field for mole_concentration_of_ammonium_at_soil_surface, received ',fieldCount
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_BAD
      return
    endif

    call ESMF_FieldGet(fieldList(1), localde=0, farrayPtr=ptr_f2, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    if (hasAmmonium) then
      ptr_f2 = amm(AMMlbnd(1):AMMubnd(1),AMMlbnd(2):AMMubnd(2),AMMlbnd(3))
    elseif (hasDIN .and. hasNitrate) then
      ptr_f2 = din(RANGE2D,lbnd(3)) - nit(RANGE2D,lbnd(3))
      write(message,'(A)') trim(name)//' calculates NH4 as DIN - NO3'
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    elseif (hasDIN) then
      ptr_f2 = 0.5d0 * DIN(RANGE2D,lbnd(3))
      write(message,'(A)') trim(name)//' calculates NH4 as 0.5 * DIN'
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    elseif (hasNitrate) then
      ptr_f2 = nit(RANGE2D,lbnd(3))
      write(message,'(A)') trim(name)//' calculates NH4 as equal to NO3'
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    else
      write(message,'(A)') trim(name)//' did not receive any information on nitrogen'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_NOT_FOUND
      return
    end if

    ! Get mandatory nitrate field from export State and save either
    ! pelagic nitrate (if present) or nitrate derived from DIN and/or NH4

    call ESMF_StateGet(exportState, &
      'mole_concentration_of_nitrate_at_soil_surface', field, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_FieldGet(field, localde=0, farrayPtr=ptr_f2, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    if (hasNitrate) then
      ptr_f2 = nit(RANGE2D,lbnd(3))
    elseif (hasAmmonium .and. hasDIN) then
      ptr_f2 = din(RANGE2D,lbnd(3)) &
        - amm(AMMlbnd(1):AMMubnd(1),AMMlbnd(2):AMMubnd(2),AMMlbnd(3))
      write(message,'(A)') trim(name)//' calculates NO3 = DIN - NH4'
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    elseif (hasDIN) then
      ptr_f2 = 0.5d0 * DIN(RANGE2D,lbnd(3))
      write(message,'(A)') trim(name)//' calculates NO3 = 0.5 * DIN'
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    elseif (hasAmmonium) then
      ptr_f2 = amm(AMMlbnd(1):AMMubnd(1),AMMlbnd(2):AMMubnd(2),AMMlbnd(3))
      write(message,'(A)') trim(name)//' calculates NO3 equal to NH4'
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    else
      write(message,'(A)') trim(name)//' did not receive any information on nitrogen'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_NOT_FOUND
      return
    end if

    !> check for DIP, if present, take as is, if not calculate it N-based
    !> with Redfield Stoichiometry
    call MOSSCO_StateGet(importState, fieldList, itemSearchList= (/ &
      'DIP_in_water                                    ', &
      'phosphate_in_water                              ', &
      'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water'/), &
      fieldCount=fieldCount, fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    if (fieldCount > 0) then
      call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=Plbnd, &
        exclusiveUBound=Pubnd, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      call ESMF_FieldGet(fieldList(1), farrayPtr=dip, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

      write(message,'(A)') trim(name)//' obtains phosphorous from '
      call MOSSCO_FieldString(fieldList(1), message)
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    else
      if (.not.(associated(din))) then
        allocate(din(RANGE2D,lbnd(3)))
        if (hasAmmonium .and. hasNitrate) then
          din(RANGE2D,lbnd(3)) = nit(RANGE2D,lbnd(3)) &
            + amm(AMMlbnd(1):AMMubnd(1),AMMlbnd(2):AMMubnd(2),AMMlbnd(3))
          write(message,'(A)') trim(name)//' calculates DIN = NH4 + NO3'
          if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        elseif (hasAmmonium) then
          din(RANGE2D,lbnd(3)) = 2 * amm(AMMlbnd(1):AMMubnd(1),AMMlbnd(2):AMMubnd(2),AMMlbnd(3))
          write(message,'(A)') trim(name)//' calculates DIN = 2 * NH4'
          if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        else
          din(RANGE2D,lbnd(3)) = 2 * nit(RANGE2D,lbnd(3))
          write(message,'(A)') trim(name)//' calculates DIN = 2 * NO3'
          if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif
      endif

      if (.not.(associated(dip))) allocate(dip(RANGE2D,1))

      dip(RANGE2D,1) = 1.0d0/16.0d0 * DIN(RANGE2D,lbnd(3))
      write(message,'(A)') trim(name)//' calculates DIP from Redfield DIN'
      if (verbose) call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      Plbnd(3)=1
    endif

    call ESMF_StateGet(exportState,'mole_concentration_of_phosphate_at_soil_surface', field, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call ESMF_FieldGet(field,localde=0,farrayPtr=ptr_f2,rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    ptr_f2 = DIP(RANGE2D,Plbnd(3))

    call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call MOSSCO_CompExit(cplComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(cplcomp, importState, exportState, externalclock, rc)
    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer,intent(out)  :: rc

    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime, stopTime
    integer                     :: localrc

    call MOSSCO_CompEntry(cplComp, externalClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

    call MOSSCO_CompExit(cplComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(localrc)

  end subroutine Finalize

end module pelagic_soil_connector
