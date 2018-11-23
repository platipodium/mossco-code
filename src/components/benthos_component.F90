!> @brief Implementation of an ESMF component that calculates benthos effects
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018
!>  Helmholtz-Zentrum Geesthacht, Bundesanstalt für Wasserbau
!> @author M. Hassan Nasermoaddeli
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

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#define RANGE1D lbnd(1):ubnd(1)
#define RANGE2D RANGE1D,lbnd(2):ubnd(2)
#define RANGE3D RANGE2D,lbnd(3):ubnd(3)
#define RANGE4D RANGE3D,lbnd(4):ubnd(4)
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
  type(MOSSCO_VariableFArray2d),dimension(:),allocatable :: importList,exportList
  ! Dimensions (x,y,z)
  integer(ESMF_KIND_I4),pointer   :: mask(:,:)=>NULL()
  integer                         :: lbnd(3)

  type (microphytobenthos) , save :: Micro
  type (BioturbationEffect), save :: Total_Bioturb
  integer                         :: inum, jnum
  logical                         :: forcing_from_coupler=.false.
contains


#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)   :: gridcomp
    integer(ESMF_KIND_I4), intent(out) :: rc
    integer(ESMF_KIND_I4) :: localrc

    rc = ESMF_SUCCESS

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

    type(ESMF_GridComp)         :: gridComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: parentClock
    integer, intent(out)        :: rc

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Time)             :: currTime
    integer(ESMF_KIND_I4)       :: localrc
    logical                     :: isPresent

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", &
      valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (isPresent) call ESMF_StateValidate(importState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (isPresent) call ESMF_StateValidate(exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

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
    type(ESMF_Field)       :: field

    character(len=ESMF_MAXSTR) :: foreignGridFieldName

    integer(ESMF_KIND_I4)  :: rank, localrc
    character(ESMF_MAXSTR) :: name, message
    type(ESMF_Clock)       :: clock
    type(ESMF_Time)        :: currTime
    logical                :: isPresent, foreignGridIsPresent=.false.
    type(ESMF_INDEX_Flag)  :: indexFlag
    type(ESMF_GeomType_Flag) :: geomType
    type(ESMF_Mesh)          :: mesh

    integer(ESMF_KIND_I4)  :: ubnd2(2), lbnd2(2), ubnd3(3), lbnd3(3), i

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !! get/set geometry:
    !! rely on field with name foreignGridFieldName given as attribute and field
    !! in importState
    !! and just take the same grid&distgrid or mesh.
!!! Create Grid
    call ESMF_GridCompGet(gridComp, gridIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (isPresent) then
      call ESMF_GridCompGet(gridComp, grid=grid, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    else

    call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
      ispresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (.not.isPresent) then
      inum=1
      jnum = 1
      ! call ESMF_ArraySpecSet(array, rank=3, typekind=ESMF_TYPEKIND_R8, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), &
                   maxIndex=(/inum,jnum/), &
                   regDecomp=(/1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_DELOCAL,  &
                   name="benthos grid", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_GridAddCoord(grid, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    else
      call ESMF_AttributeGet(importState, name='foreign_grid_field_name', &
        value=foreignGridFieldName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      foreignGridIsPresent=.true.
      write(message,'(A)') trim(name)//' uses foreign grid '//trim(foreignGridFieldName)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

      call ESMF_StateGet(importState, trim(foreignGridFieldName), field, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(field, rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank<2 .or. rank>3) then
        write(message, '(A)') 'foreign grid must be of rank 2 or 3'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
      end if

      if (rank==2) then

        call ESMF_FieldGet(field, grid=grid, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_FieldGetBounds(field, exclusiveLBound=lbnd2, &
          exclusiveUBound=ubnd2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        inum=ubnd2(1)-lbnd2(1)+1
        jnum=ubnd2(2)-lbnd2(2)+1
      endif

      if (rank==3) then
        write(message,*) 'foreign grid of rank 3 not yet implemented'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

        call ESMF_FieldGet(field, grid=foreign_grid, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_FieldGetBounds(field, exclusiveLBound=lbnd3, exclusiveUBound=ubnd3, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        inum=ubnd3(1)-lbnd3(1)+1
        jnum=ubnd3(2)-lbnd3(2)+1

        grid = ESMF_GridCreateNoPeriDim(minIndex=lbnd3(1:2), &
                   maxIndex=ubnd3(1:2), &
                   regDecomp=(/1,1/), &
                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                   indexflag=ESMF_INDEX_DELOCAL,  &
                   name="benthos grid", &
                   coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/), &
                   coorddep2=(/2/),rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_GridAddCoord(grid, rc=localrc)   !> ToDO we need to copy the coordiane from foreign Grid.
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif
    endif

    call ESMF_GridCompSet(gridComp, grid=grid, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  endif
    !> Create distgrid for arrays
    !   distgrid =  ESMF_DistGridCreate(minIndex=(/inum,jnum/), maxIndex=(/inum,jnum/), &
    !   indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
    call ESMF_GridGet(grid, distGrid=distGrid, indexflag=indexflag, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !> create import lists for reading abunadnace and biomass from fields
    !> (for example read by netcdf-input component)
    allocate (importList(2))
    importList(1)%name  = 'tellina_fabula_mean_abundance'
    importList(1)%units = 'm-2'
    importList(2)%name  = 'microphytobenthos_at_soil_surface'
    importList(2)%units = 'mg g-1' ! from mgg**-1, correct?

     do i=1, size(importList)

      if (foreignGridIsPresent) then
        if (trim(importList(i)%name) == foreignGridFieldName) cycle
      end if

      field = ESMF_FieldEmptyCreate(name=trim(importList(i)%name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldEmptySet(field, grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
        rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field, 'units', trim(importList(i)%units), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_StateAddReplace(importState,(/field/),rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A)') trim(name)//' created for import'
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    end do

!!! Advertise Export Fields
    allocate (exportList(4))
    exportList(1)%name  = 'Effect_of_MPB_on_sediment_erodibility_at_soil_surface'
    exportList(1)%units = '1'
    exportList(2)%name  = 'Effect_of_MPB_on_critical_bed_shearstress_at_soil_surface'
    exportList(2)%units = '1'
    exportList(3)%name  = 'Effect_of_Mbalthica_on_sediment_erodibility_at_soil_surface'
    exportList(3)%units = '1'
    exportList(4)%name  = 'Effect_of_Mbalthica_on_critical_bed_shearstress_at_soil_surface'
    exportList(4)%units = '1'

    do i=1,size(exportList)
      field = ESMF_FieldEmptyCreate(name=trim(exportList(i)%name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldEmptySet(field, grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
        rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field,'creator',trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field,'units',trim(exportList(i)%units), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_StateAddReplace(exportState,(/field/),rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A)') trim(name)//' created for export'
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    end do

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

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

    type(ESMF_Field), target        :: field
    type(ESMF_Grid)                 :: grid
    type(ESMF_FieldStatus_Flag)     :: status
    integer(ESMF_KIND_I4)           :: localrc, i,j

    integer,target :: coordDimCount(2),coordDimMap(2,2)
    integer,dimension(2)            :: totalLBound,totalUBound
    integer,dimension(2)            :: exclusiveLBound,exclusiveUBound
    type :: allocatable_integer_array
      integer,dimension(:),allocatable :: data
    end type
    type(allocatable_integer_array) :: coordTotalLBound(2),coordTotalUBound(2)

    logical :: isPresent

    call MOSSCO_CompEntry(gridComp, clock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

!   Get the total domain size from the coordinates associated with the Grid
    call ESMF_GridCompGet(gridComp, grid=grid, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
     localDe=0, exclusiveLBound=exclusiveLBound,  &
     exclusiveUBound=exclusiveUBound, rc=localrc)
   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridGet(grid,coordDimCount=coordDimCount, coordDimMap=coordDimMap, &
      rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

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

#if ESMF_VERSION_MAJOR > 6
   call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, isPresent=isPresent, rc=localrc)
   _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

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
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
   else
      allocate(mask(exclusiveLBound(1):exclusiveUBound(1),exclusiveLBound(2):exclusiveUBound(2)))
      mask = 1
   end if
  ! Initialize microphytobenthos and macrofauna effects on the erodibility and the critical bed shear stress
    call Micro%initialize(inum, jnum)
    call Macrofauna_init(Total_Bioturb, inum, jnum)

!   Complete Import Fields
    do i=1,ubound(importList,1)

      call ESMF_StateGet(importState, trim(importList(i)%name), field, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(field, status=status, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (status.eq.ESMF_FIELDSTATUS_GRIDSET) then

        write(message,'(A)') trim(name)//' import from internal field '// &
        trim(importList(i)%name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        allocate(importList(i)%data(exclusiveLBound(1):exclusiveUBound(1),exclusiveLBound(2):exclusiveUBound(2)))

        call ESMF_FieldEmptyComplete(field,importList(i)%data)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        importList(i)%data = 0.0d0
         if (trim (importList(i)%name )== 'tellina_fabula_mean_abundance') then
           call Macrofauna_set( )
       !   write (0,*)'tellina_fabula_mean_abundance internal',importList(i)%data
         elseif  (trim (importList(i)%name )== 'microphytobenthos_at_soil_surface') then
           call micro%set()
       !  write (0,*)'microphytobenthos internal',importList(i)%data
         endif
      else if (status .eq. ESMF_FIELDSTATUS_COMPLETE) then

        write(message,'(A)') trim(name)//' import from external field '// &
          trim(importList(i)%name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        call ESMF_FieldGet(field, farrayPtr=importList(i)%data,rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (.not. (      all(lbound(importList(i)%data) .eq. exclusiveLBound) &
                   .and. all(ubound(importList(i)%data) .eq. exclusiveUBound) ) ) then
          call ESMF_LogWrite('invalid field bounds',ESMF_LOGMSG_ERROR,ESMF_CONTEXT)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        end if
        forcing_from_coupler = .true.

        call ESMF_AttributeGet(field,'units', isPresent=isPresent, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

         if (isPresent) then
           call ESMF_AttributeGet(field,'units',importList(i)%units, rc=localrc)
           _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
         else
           write(message,'(A)')  trim(name)//' did not find "units" attribute in  '
           call MOSSCO_FieldString(field, message, rc=localrc)
           _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

           call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
           importList(i)%units=''
           write (0,*) 'unit of macrofauna or microphytoobenthos is not present, therefore set to '')'
         endif

         if (trim (importList(i)%name )== 'tellina_fabula_mean_abundance') then
           call Macrofauna_set(importList(i)%data,importList(i)%units )
      !   write (0,*)'tellina_fabula_mean_abundance data,external', importList(i)%data
      !   write (0,*)'tellina_fabula_mean_abundance data,external',importList(i)%units
         elseif  (trim (importList(i)%name )== 'microphytobenthos_at_soil_surface') then
           call micro%set(importList(i)%data,trim(importList(i)%units))
      !   write (0,*)'microphytobenthos_at_soil_surface, external', importList(i)%data
      !   write (0,*)'microphytobenthos_at_soil_surface, external', importList(i)%units
         endif

      else
        write(message,'(A)') trim(name)//' erroneous empty field '//&
          trim(importList(i)%name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        call MOSSCO_CompExit(gridComp, localrc)
        rc = ESMF_RC_NOT_FOUND
        return
      end if
    end do

!   Complete Export Fields
    do i=1,size(exportList)
      call ESMF_StateGet(exportState,trim(exportList(i)%name), field, rc=localrc)
      call ESMF_FieldGet(field,status=status)
      if (status.eq.ESMF_FIELDSTATUS_GRIDSET) then
        call ESMF_LogWrite(' export to internal field '//trim(exportList(i)%name),ESMF_LOGMSG_INFO)
        allocate(exportList(i)%data(exclusiveLBound(1):exclusiveUBound(1),exclusiveLBound(2):exclusiveUBound(2)))
        call ESMF_FieldEmptyComplete(field,exportList(i)%data)
        exportList(i)%data = 1.0d0
      else if (status .eq. ESMF_FIELDSTATUS_COMPLETE) then
        call ESMF_LogWrite(' export to external field '//trim(exportList(i)%name),ESMF_LOGMSG_INFO)
        call ESMF_FieldGet(field,farrayPtr=exportList(i)%data,rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT,rc=rc)
        if (.not. (      all(lbound(exportList(i)%data) .eq. exclusiveLBound) &
                   .and. all(ubound(exportList(i)%data) .eq. exclusiveUBound) ) ) then
          call ESMF_LogWrite('invalid field bounds',ESMF_LOGMSG_ERROR,ESMF_CONTEXT)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        end if
      else
        call ESMF_LogWrite('empty field: '//trim(exportList(i)%name),ESMF_LOGMSG_ERROR, &
                           line=__LINE__,file=__FILE__,method='InitializeP2()')
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
    end do



    deallocate(Micro%ErodibilityEffect)
    deallocate(Micro%TauEffect)
    deallocate(Total_Bioturb%ErodibilityEffect)
    deallocate(Total_Bioturb%TauEffect)

    Micro%ErodibilityEffect         => exportList(1)%data
    Micro%TauEffect                 => exportList(2)%data
    Total_Bioturb%ErodibilityEffect => exportList(3)%data
    Total_Bioturb%TauEffect         => exportList(4)%data

!#define DEBUG
#ifdef DEBUG
    call ESMF_StatePrint(exportstate, nestedFlag=.true.,rc=rc)
#endif

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

    character(len=ESMF_MAXSTR)  :: name, message, fieldName
    type(ESMF_Time)             :: currTime
    integer                     :: localrc, i

    character(len=ESMF_MAXSTR), pointer :: includeList(:)
    type(ESMF_Field), allocatable       :: exportFieldList(:), importFieldList(:)
    integer(ESMF_KIND_I4)               :: exportFieldCount, importFieldCount

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    allocate(includeList(4))
    includeList(1)='Effect_of_Mbalthica_on_critical_bed_shearstress'
    includeList(2)='Effect_of_Mbalthica_on_sediment_erodibility'
    includeList(3)='Effect_of_MPB_on_critical_bed_shearstress'
    includeList(4)='Effect_of_MPB_on_sediment_erodibility'

    do i=1, ubound(includeList,1)
      includeList(i) = trim(includeList(i))//'_at_soil_surface'
    enddo

    call MOSSCO_StateGet(exportState, exportFieldList, &
      fieldCount=exportFieldCount, include=includeList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    do i=1, exportFieldCount

      call ESMF_FieldGet(exportFieldList(i), name=fieldName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_StateGet(importState, importFieldList, &
        fieldCount=importFieldCount, itemSearch=trim(fieldName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (importFieldCount < 1) cycle

      call MOSSCO_FieldCopy(exportFieldList(i), importFieldList(1), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(name)//' hotstarted'
      call MOSSCO_FieldString(exportFieldList(i), message)
      call MOSSCO_MessageAdd(message,' from')
      call MOSSCO_FieldString(importFieldList(1), message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    enddo

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

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

    character(ESMF_MAXSTR) :: name, message, unit
    type(ESMF_Time)        :: currTime, stopTime
    type(ESMF_Clock)       :: clock
    integer(ESMF_KIND_I4)  :: localrc
    logical                :: verbose

    real(ESMF_KIND_R8), pointer   :: farrayPtr2(:,:) => null()
    type(ESMF_Field), allocatable :: fieldlist(:)
    integer(ESMF_KIND_I4)         :: fieldCount
    type(ESMF_GeomType_Flag)      :: geomType
    type(ESMF_Grid)               :: grid
    logical                       :: isPresent

    rc = ESMF_SUCCESS
    verbose = .false.

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ClockGet(clock, currTime=currTime, advanceCount=advanceCount,&
      runTimeStepCount=runtimestepcount, timeStep=timestep, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    !if (advanceCount < 1)
    verbose = .true.

    call MOSSCO_StateGet(importState, fieldList, fieldCount=fieldCount, &
      itemSearch='microphytobenthos_at_soil_surface', rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (fieldCount > 0) then

      call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !   if (geomType == ESMF_GEOMTYPE_GRID) then
    !
    !     call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, isPresent=isPresent, &
    !       rc=localrc)
    !     _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !     if (isPresent) then
    !       call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, farrayPtr=farrayPtr2, &
    !         rc=localrc)
    !       _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    !
    !       mask = farrayPtr2
    !     else
    !       mask(:,:) = 1.0
    !     endif
    !
      endif
    !

      call ESMF_AttributeGet(fieldList(1), 'units', value=unit, &
        defaultValue='1', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call Micro%set(farrayPtr2, trim(unit))
      call Micro%run()

    ! elseif (verbose) then
    !   write(message,'(A)') trim(name)// &
    !   ' obtained no information on microphytobentos'
    !   call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
    ! endif

    call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call Macrofauna_run(Total_Bioturb, inum, jnum)

    if (verbose) then
      write(message,'(A,es10.3,X,es10.3)') trim(name)// &
      ' microphyto erodibility/critical shear ', &
      Micro%ErodibilityEffect, Micro%TauEffect
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      write(message,'(A,es10.3,X,es10.3)') trim(name)// &
      ' macrozoo erodibility/critical shear ', &
      Total_Bioturb%ErodibilityEffect, Total_Bioturb%TauEffect
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call ESMF_TimeIntervalGet(timestep,s_r8=dt,rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ClockGet(clock, stopTime=stopTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (stopTime>currTime) then
      call ESMF_ClockAdvance(clock, timeStep=stopTime-currTime, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

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

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock
    type(ESMF_Config)       :: config

    logical                 :: isPresent
    integer                 :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridComp, configIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (isPresent) then

      call ESMF_GridCompGet(gridComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_ConfigDestroy(config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    end if

    call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (isPresent) then
      call ESMF_StateValidate(importState, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_DestroyOwn(importState, trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (isPresent) then
      call ESMF_StateValidate(exportState, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_DestroyOwn(exportState, trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Finalize

end module benthos_component
