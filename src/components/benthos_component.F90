!> @brief Implementation of an ESMF component that calculates benthos effects
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014, 2015 Helmholtz-Zentrum Geesthacht
!> @author Hassan Nasermoaddeli
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
#define ESMF_FILENAME "benthos_component.F90"

module benthos_component

  use esmf
  use mossco_component
  use mossco_strings
  use mossco_state
  use mossco_field
  use mossco_variable_types
  use Macrofauna_interface
  use Microphytobenthos_class

  implicit none

  public :: SetServices

  private

  !! @todo hn: read CF documnetation for correct name
  type(MOSSCO_VariableFArray2d),dimension(:),allocatable :: importList
  ! Dimensions (x,y,z)
  real(ESMF_KIND_R8), dimension(:,:), pointer :: Effect_of_MPB_on_sediment_erodibility_at_bottom
  real(ESMF_KIND_R8), dimension(:,:), pointer :: Effect_of_MPB_on_critical_bed_shearstress
  real(ESMF_KIND_R8), dimension(:,:), pointer :: Effect_of_Mbalthica_on_sediment_erodibility_at_bottom
  real(ESMF_KIND_R8), dimension(:,:), pointer :: Effect_of_Mbalthica_on_critical_bed_shearstress
  integer(ESMF_KIND_I4),dimension(:,:),pointer           :: mask=>NULL()
  type(ESMF_Field), save         :: Microphytobenthos_erodibility,Microphytobenthos_critical_bed_shearstress, &
    &                               Macrofauna_erodibility,Macrofauna_critical_bed_shearstress
  integer                        :: ubnd(3),lbnd(3)

  type (microphytobenthos) ,save :: Micro
  type (BioturbationEffect),save :: Total_Bioturb
  real (fp)                      :: tau
  real (fp)                      :: Erod
  integer                        :: inum, jnum
  logical                        :: forcing_from_coupler=.false.
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=2, &
      userRoutine=InitializeP2, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
     call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_READRESTART, phase=1, &
      userRoutine=ReadRestart, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
  subroutine InitializeP0(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)         :: gridComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: parentClock
    integer, intent(out)        :: rc

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Time)             :: currTime
    integer                     :: localrc
    logical                     :: isPresent

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"

  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)    :: gridComp
    type(ESMF_State)       :: importState, exportState
    type(ESMF_Clock)       :: parentClock
    integer, intent(out)   :: rc

    type(ESMF_Grid)        :: grid, foreign_grid
    type(ESMF_DistGrid)    :: distgrid
    type(ESMF_Array)       :: array
    type(ESMF_Field)       :: field

    character(len=ESMF_MAXSTR) :: foreignGridFieldName

    integer                :: rank, localrc
    character(ESMF_MAXSTR) :: name, message
    type(ESMF_Clock)       :: clock
    type(ESMF_Time)        :: currTime
    logical                :: isPresent, foreignGridIsPresent=.false.
    type(ESMF_INDEX_Flag)  :: indexFlag

    integer(ESMF_KIND_I4)  :: ubnd2(2), lbnd2(2), ubnd3(3), lbnd3(3)
    integer                :: i

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! get/set grid:
    !! rely on field with name foreignGridFieldName given as attribute and field
    !! in importState
    !! and just take the same grid&distgrid.
!!! Create Grid
    call ESMF_GridCompGet(gridComp,gridIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_GridCompGet(gridComp,grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc,  endflag=ESMF_END_ABORT)
    else

    call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
      ispresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.isPresent) then
     inum=1
     jnum = 1
     ! call ESMF_ArraySpecSet(array, rank=3, typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
      grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), &
                   maxIndex=(/inum,jnum/), &
                   regDecomp=(/1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_GLOBAL,  &
                   name="benthos grid", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_GridAddCoord(grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
        value=foreignGridFieldName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      foreignGridIsPresent=.true.
      write(message,*) trim(name)//' uses foreign grid '//trim(foreignGridFieldName)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

      call ESMF_StateGet(importState, trim(foreignGridFieldName), field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldGet(field, rank=rank, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (rank<2 .or. rank>3) then
        write(message,*) 'foreign grid must be of rank 2 or 3'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
      end if

      if (rank==2) then
        call ESMF_FieldGet(field, grid=grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldGetBounds(field, exclusiveLBound=lbnd2, exclusiveUBound=ubnd2, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        inum=ubnd2(1)-lbnd2(1)+1
        jnum=ubnd2(2)-lbnd2(2)+1
      endif

      if (rank==3) then
        write(message,*) 'foreign grid of rank 3 not yet implemented'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

        call ESMF_FieldGet(field, grid=foreign_grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldGetBounds(field, exclusiveLBound=lbnd3, exclusiveUBound=ubnd3, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        inum=ubnd3(1)-lbnd3(1)+1
        jnum=ubnd3(2)-lbnd3(2)+1

        grid = ESMF_GridCreateNoPeriDim(minIndex=lbnd3(1:2), &
                   maxIndex=ubnd3(1:2), &
                   regDecomp=(/1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_GLOBAL,  &
                   name="benthos grid", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_GridAddCoord(grid, rc=localrc)   !> ToDO we need to copy the coordiane from foreign Grid.
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
    endif
    call ESMF_GridCompSet(gridComp, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  endif
    !> Create distgrid for arrays
    !   distgrid =  ESMF_DistGridCreate(minIndex=(/inum,jnum/), maxIndex=(/inum,jnum/), &
    !   indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
    call ESMF_GridGet(grid, distGrid=distGrid, indexflag=indexflag, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


    call Micro%initialize(inum, jnum)
    call Micro%set()

    call Macrofauna_init(Total_Bioturb,inum, jnum)
    call Macrofauna_set()


    !> create import lists for reading abunadnace and biomass from fields
    !> (for example read by netcdf-input component)
    allocate (importList(2))
    importList(1)%name  = 'tellina_fabula_mean_abundance'
    importList(1)%units = ''
    importList(2)%name  = 'microphytobenthos_at_soil_surface'
    importList(2)%units = ''

     do i=1,size(importList)

      if (foreignGridIsPresent) then
        if (trim(importList(i)%name) == foreignGridFieldName) cycle
      end if

      field = ESMF_FieldEmptyCreate(name=trim(importList(i)%name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_FieldEmptySet(field, grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'units',trim(importList(i)%units), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(field,'creator',trim(name), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message, '(A)') trim(name)//' created field'
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call ESMF_StateAdd(importState,(/field/),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    end do


   !> create export fields
    allocate(Effect_of_MPB_on_sediment_erodibility_at_bottom(inum,jnum))
    Effect_of_MPB_on_sediment_erodibility_at_bottom => Micro%ErodibilityEffect
!#define DEBUG

#ifdef DEBUG
    write(0,*) ' Effect_of_MPB_on_sediment_erodibility_at_soil_surface', &
    Effect_of_MPB_on_sediment_erodibility_at_bottom !, ubound(Effect_of_MPB_on_sediment_erodibility_at_bottom)
#endif

    array = ESMF_ArrayCreate(distgrid=distgrid,indexflag=indexFlag, &
      farray=Effect_of_MPB_on_sediment_erodibility_at_bottom,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    Microphytobenthos_erodibility = ESMF_FieldCreate(grid, array, &
      name="Effect_of_MPB_on_sediment_erodibility_at_soil_surface", &
      staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(Microphytobenthos_erodibility, 'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate(Effect_of_MPB_on_critical_bed_shearstress(inum,jnum))

    Effect_of_MPB_on_critical_bed_shearstress => Micro%TauEffect

#ifdef DEBUG
    write(0,*) 'Effect_of_MPB_on_critical_bed_shearstress_at_soil_surface',&
     Effect_of_MPB_on_critical_bed_shearstress
#endif

    array = ESMF_ArrayCreate(distgrid=distgrid,indexflag=indexFlag, &
      farray=Effect_of_MPB_on_critical_bed_shearstress, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    Microphytobenthos_critical_bed_shearstress= ESMF_FieldCreate(grid, array, &
      name="Effect_of_MPB_on_critical_bed_shearstress_at_soil_surface", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(Microphytobenthos_critical_bed_shearstress, 'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate( Effect_of_Mbalthica_on_sediment_erodibility_at_bottom(inum,jnum))

    Effect_of_Mbalthica_on_sediment_erodibility_at_bottom => Total_Bioturb%ErodibilityEffect

#ifdef DEBUG
    write(0,*) 'Effect_of_Mbalthica_on_sediment_erodibility_at_soil_surface', &
    Effect_of_Mbalthica_on_sediment_erodibility_at_bottom
#endif

    array = ESMF_ArrayCreate(distgrid=distgrid,indexflag=indexFlag,  &
      farray=Effect_of_Mbalthica_on_sediment_erodibility_at_bottom, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    Macrofauna_erodibility= ESMF_FieldCreate(grid, array, &
      name="Effect_of_Mbalthica_on_sediment_erodibility_at_soil_surface", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(Macrofauna_erodibility, 'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate(Effect_of_Mbalthica_on_critical_bed_shearstress(inum,jnum))

    Effect_of_Mbalthica_on_critical_bed_shearstress => Total_Bioturb%TauEffect

#ifdef DEBUG
    write(0,*) 'Effect_of_Mbalthica_on_critical_bed_shearstress_at_soil_surface',&
    Effect_of_Mbalthica_on_critical_bed_shearstress
#endif

    array = ESMF_ArrayCreate(distgrid=distgrid,indexflag=indexFlag, &
      farray=Effect_of_Mbalthica_on_critical_bed_shearstress, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    Macrofauna_critical_bed_shearstress= ESMF_FieldCreate(grid, array, &
      name="Effect_of_Mbalthica_on_critical_bed_shearstress_at_soil_surface", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(Macrofauna_critical_bed_shearstress, 'creator', trim(name), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> set export state
    call ESMF_StateAdd(exportState,(/Microphytobenthos_erodibility/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateAdd(exportState,(/Microphytobenthos_critical_bed_shearstress/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateAdd(exportState,(/Macrofauna_erodibility/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateAdd(exportState,(/Macrofauna_critical_bed_shearstress/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_StatePrint(exportstate, nestedFlag=.true.,rc=rc)
#ifdef DEBUG
    call ESMF_FieldPrint (Microphytobenthos_erodibility)
      write(0,*) 'Mircrophy. erodibility effect', Micro%ErodibilityEffect

    call ESMF_FieldPrint (Microphytobenthos_critical_bed_shearstress)
      write(0,*) 'Mircrophy. Tau effect', Micro%TauEffect

    call ESMF_FieldPrint (Macrofauna_erodibility)
      write(0,*) ' Macro. erodibility effect', Total_Bioturb%ErodibilityEffect

    call ESMF_FieldPrint (Macrofauna_critical_bed_shearstress)
      write(0,*) ' Macro. Critical bed Shear stress', Total_Bioturb%TauEffect
#endif

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1
#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP2"
  subroutine InitializeP2(gridComp, importState, exportState, clock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR)  :: name,message
    type(ESMF_Time)         :: currTime

    type(ESMF_Field), target     :: field
    type(ESMF_Grid)      :: grid
    type(ESMF_FieldStatus_Flag)     :: status
    integer              :: localrc, i,j

    integer,target :: coordDimCount(2),coordDimMap(2,2)
    integer,dimension(2)            :: totalLBound,totalUBound
    integer,dimension(2)            :: exclusiveLBound,exclusiveUBound
    type :: allocatable_integer_array
      integer,dimension(:),allocatable :: data
    end type
    type(allocatable_integer_array) :: coordTotalLBound(2),coordTotalUBound(2)


    logical :: isPresent

    call MOSSCO_CompEntry(gridComp, clock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

!   Get the total domain size from the coordinates associated with the Grid
    call ESMF_GridCompGet(gridComp,grid=grid)
    call ESMF_GridGet(grid,ESMF_STAGGERLOC_CENTER,0,                                   &
                      exclusiveLBound=exclusiveLBound,exclusiveUBound=exclusiveUBound)
    call ESMF_GridGet(grid,coordDimCount=coordDimCount,coordDimMap=coordDimMap)
    do i=1,2
      allocate(coordTotalLBound(i)%data(coordDimCount(i)))
      allocate(coordTotalUBound(i)%data(coordDimCount(i)))
      call ESMF_GridGetCoordBounds(grid,coordDim=i,                      &
                                   totalLBound=coordTotalLBound(i)%data, &
                                   totalUBound=coordTotalUBound(i)%data)
      do j=1,coordDimCount(i)
        if (coordDimMap(i,j) .eq. i) then
          totalLBound(i) = coordTotalLBound(i)%data(j)
          totalUBound(i) = coordTotalUBound(i)%data(j)
          exit
        end if
      end do
    end do
   !> The preferred interface would be to use isPresent, but htis only works in ESMF from Nov 2014
   !> @todo replace if 0 by ESMF_VERSION macros

#if 0
   call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, isPresent=isPresent, rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

   if (isPresent) then
#else
   call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, farrayPtr=mask, rc=localrc)
   !! Do not check for success here as NOT_FOUND is expected behaviour, @todo: check for NOT_FOUND flag
   if (localrc .ne. ESMF_SUCCESS) then
      call ESMF_LogWrite('ignore ERROR messages above related to GridGetItem - waiting for new ESMF release', &
                         ESMF_LOGMSG_INFO,ESMF_CONTEXT)
   end if
   if (localrc == ESMF_SUCCESS) then
#endif
      call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, farrayPtr=mask)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   else
      allocate(mask(totalLBound(1):totalUBound(1),totalLBound(2):totalUBound(2)))
      mask = 0
      mask(exclusiveLBound(1):exclusiveUBound(1),exclusiveLBound(2):exclusiveUBound(2)) = 1
   end if

!   Complete Import Fields
    do i=1,size(importList)
      call ESMF_StateGet(importState,trim(importList(i)%name),field)
      call ESMF_FieldGet(field,status=status)
      if (status.eq.ESMF_FIELDSTATUS_GRIDSET) then
        call ESMF_LogWrite(' import from internal field '//trim(importList(i)%name),ESMF_LOGMSG_INFO)
        allocate(importList(i)%data(totalLBound(1):totalUBound(1),totalLBound(2):totalUBound(2)))
        call ESMF_FieldEmptyComplete(field,importList(i)%data,                &
                                     ESMF_INDEX_DELOCAL,                      &
                                     totalLWidth=exclusiveLBound-totalLBound, &
                                     totalUWidth=totalUBound-exclusiveUBound)
        importList(i)%data = 0.0d0
      else if (status .eq. ESMF_FIELDSTATUS_COMPLETE) then
        call ESMF_LogWrite(' import from external field '//trim(importList(i)%name),ESMF_LOGMSG_INFO)
        call ESMF_FieldGet(field,farrayPtr=importList(i)%data,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT,rc=rc)
        if (.not. (      all(lbound(importList(i)%data) .eq. totalLBound) &
                   .and. all(ubound(importList(i)%data) .eq. totalUBound) ) ) then
          call ESMF_LogWrite('invalid field bounds',ESMF_LOGMSG_ERROR,ESMF_CONTEXT)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        end if
        forcing_from_coupler = .true.
        call ESMF_AttributeGet(field,'units', isPresent=isPresent, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         if (isPresent) then
           call ESMF_AttributeGet(field,'units',importList(i)%units, rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
         else
           write(message,'(A)')  trim(name)//' did not find "units" attribute in field '
           call MOSSCO_FieldString(field, message)
           call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
           importList(i)%units=''
           write (*,*) 'unit of macrofauna or microphytoobenthos is not present, therefore set to '')'
         endif

         if (trim (importList(i)%name )== 'tellina_fabula_mean_abundance') then
           call Macrofauna_set(importList(i)%data,importList(i)%units )
         elseif  (trim (importList(i)%name )== 'microphytobenthos_at_soil_surface') then
           call micro%set(importList(i)%data,importList(i)%units)
         endif

      else
        call ESMF_LogWrite('empty field: '//trim(importList(i)%name),ESMF_LOGMSG_ERROR, &
                           line=__LINE__,file=__FILE__,method='InitializeP2()')
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
    end do

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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

    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime
    integer                     :: localrc, i, itemCount

    type(ESMF_FieldStatus_Flag)    :: fieldstatus
    type(ESMF_StateItem_Flag)      :: itemtype
    type(ESMF_Field)               :: exportField, importField
    character(len=ESMF_MAXSTR), allocatable     :: itemNameList(:)

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> @todo
    !> 1. Go through  / create list of variables in export state
    !> 2. Get these variables from import state
    !> 2. Get the pointer to these fields
    !> 3. Copy the values

    itemCount=4

    allocate(itemNameList(itemCount))
    itemNameList(1)='Effect_of_Mbalthica_on_critical_bed_shearstress_at_soil_surface'
    itemNameList(2)='Effect_of_Mbalthica_on_sediment_erodibility_at_soil_surface'
    itemNameList(3)='Effect_of_MPB_on_critical_bed_shearstress_at_soil_surface'
    itemNameList(4)='Effect_of_MPB_on_sediment_erodibility_at_soil_surface'

    do i=1,itemCount

      call ESMF_StateGet(exportState, trim(itemNameList(i)), itemType=itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc,   endflag=ESMF_END_ABORT)
      if (itemType /= ESMF_STATEITEM_FIELD) cycle

      call ESMF_StateGet(exportState, trim(itemNameList(i)), field=exportField, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc,   endflag=ESMF_END_ABORT)

      call ESMF_StateGet(importState, trim(itemNameList(i)), itemType=itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc,   endflag=ESMF_END_ABORT)
      if (itemType /= ESMF_STATEITEM_FIELD) then
        write(message,'(A)') trim(name)//' skipped hotstart for item '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        cycle
      endif

      call ESMF_StateGet(importState, trim(itemNameList(i)), field=importField, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc,   endflag=ESMF_END_ABORT)
      call ESMF_FieldGet(importField, status=fieldstatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc,   endflag=ESMF_END_ABORT)

      if (fieldstatus /= ESMF_FIELDSTATUS_COMPLETE) then
        write(message,'(A)') trim(name)//' skipped hotstart for incomplete field'
        call MOSSCO_FieldString(importField, message)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
        cycle
      endif

      !@todo implement MOSSCO_FieldComplete(importField, exportField, rc=localrc)
      !! implement call MOSSCO_FieldCopyValues(importField, exportField, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end do

    if (allocated(itemNameList)) deallocate(itemNameList)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine ReadRestart

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)    :: gridComp
    type(ESMF_State)       :: importState, exportState
    type(ESMF_Clock)       :: parentClock
    integer, intent(out)   :: rc

    type(ESMF_TimeInterval)     :: timestep
    integer(ESMF_KIND_I8)       :: advancecount
    real(ESMF_KIND_R8)          :: runtimestepcount,dt

    character(ESMF_MAXSTR) :: name
    type(ESMF_Time)        :: currTime, stopTime
    type(ESMF_Clock)       :: clock
    integer                :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(clock,currTime=currTime, AdvanceCount=advancecount,&
      runTimeStepCount=runtimestepcount,timeStep=timestep, rc=localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalGet(timestep,s_r8=dt,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


    call Micro%run()
    call Macrofauna_run(Total_Bioturb)


#ifdef DEBUG
    call ESMF_FieldPrint (Microphytobenthos_erodibility)

    write(0,*) 'Mircrophy. erodibility effect', Micro%ErodibilityEffect

    call ESMF_FieldPrint (Microphytobenthos_critical_bed_shearstress)

    write(0,*) 'Mircrophy. Tau effect', Micro%TauEffect

    call ESMF_FieldPrint (Macrofauna_erodibility)

    write(0,*) ' Macro. erodibility effect', Total_Bioturb%ErodibilityEffect

    call ESMF_FieldPrint (Macrofauna_critical_bed_shearstress)

    write(0,*) ' Macro. Critical bed Shear stress', Total_Bioturb%TauEffect

    write(0,*) 'tau (macrofaunau and microphytobenthos) =' ,tau,' Both Biotic Critical bed shear stress effect= ',&
      &   Total_Bioturb%TauEffect, 'Both Biotic erodibility',Total_Bioturb%ErodibilityEffect

    write(0,*)
#endif

    call ESMF_ClockGet(clock, stopTime=stopTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (stopTime>currTime) then
      call ESMF_ClockAdvance(clock, timeStep=stopTime-currTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock

    logical                 :: isPresent
    integer                 :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Here comes your own finalization code
    !! 1. Destroy all fields that you created, be aware that other components
    !!    might have interfered with your fields, e.g., moved them into a fieldBundle
    !! 2. Deallocate all your model's internal allocated memory
    deallocate (Effect_of_MPB_on_sediment_erodibility_at_bottom)
    deallocate (Effect_of_MPB_on_critical_bed_shearstress)
    deallocate (Effect_of_Mbalthica_on_sediment_erodibility_at_bottom)
    deallocate (Effect_of_Mbalthica_on_critical_bed_shearstress)

    call ESMF_ClockDestroy(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


    call MOSSCO_CompExit(gridComp, rc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


  end subroutine Finalize

end module benthos_component
