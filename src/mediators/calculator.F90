!> @brief Implementation of an ESMF mediator component that performs
!> arbitrary field calculations (within the understood scope of operations)
!> @file calculator.F90
!!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2017, 2018 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "calculator.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#define RANGE1D lbnd(1):ubnd(1)
#define RANGE2D RANGE1D,lbnd(2):ubnd(2)
#define RANGE3D RANGE2D,lbnd(3):ubnd(3)
#define RANGE4D RANGE3D,lbnd(4):ubnd(4)

module calculator

  use esmf
  use mossco_field
  use mossco_state
  use mossco_component
  use mossco_config
  use mossco_attribute
  use mossco_logging
  use mossco_grid

  implicit none

  public SetServices

  private

  type MOSSCO_StackType
    integer(ESMF_KIND_I4)           :: rank = -1
    real(ESMF_KIND_R8), allocatable     :: scalar(:)
    real(ESMF_KIND_R8), allocatable     :: farray1(:)
    real(ESMF_KIND_R8), allocatable     :: farray2(:,:)
    real(ESMF_KIND_R8), allocatable     :: farray3(:,:,:)
    real(ESMF_KIND_R8), allocatable     :: farray4(:,:,:,:)
    logical, allocatable     :: mask1(:), mask2(:,:), mask3(:,:,:), mask4(:,:,:,:)
  end type MOSSCO_StackType

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(cplComp, rc)

    type(ESMF_cplComp)   :: cplComp
    integer, intent(out) :: rc
    integer              :: localrc

    rc=ESMF_SUCCESS

    call ESMF_cplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_cplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_cplCompSetEntryPoint(cplComp, ESMF_METHOD_RUN, Run, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_cplCompSetEntryPoint(cplComp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

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

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Time)             :: currTime
    integer(ESMF_KIND_I4)       :: localrc

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(cplComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeSet(cplComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_StateReconcile(importState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_StateReconcile(exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_CompExit(cplComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_cplComp)    :: cplComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=ESMF_MAXSTR)      :: name, message, configFileName
    type(ESMF_Time)                 :: currTime

    integer(ESMF_KIND_I4)           :: localrc, i, j, n

    type(ESMF_Config)               :: config
    logical                         :: labelIsPresent, isPresent, fileIsPresent
    character(len=ESMF_MAXSTR), allocatable :: exportList(:,:), rpnList(:,:)
    character(len=ESMF_MAXSTR), allocatable :: aliasList(:,:)
    type(ESMF_Field)                :: field
    type(ESMF_Field), allocatable   :: fieldList(:)
    integer(ESMF_KIND_I4)           :: fieldCount

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    configfilename=trim(name)//'.cfg'
    inquire(file=trim(configfilename), exist=fileIsPresent)

    if (fileIsPresent) then

      write(message,'(A)')  trim(name)//' reads configuration from '//trim(configFileName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      !> @todo deal with already existing config
      config = ESMF_ConfigCreate(rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_CplCompSet(cplComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_ConfigLoadFile(config, trim(configfilename), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_ConfigGet(config, 'export', exportList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_ConfigGet(config, 'alias', aliasList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    endif

    if (allocated(aliasList)) then
      call MOSSCO_AttributeSet(importState, 'alias_definition', aliasList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_Reallocate(aliasList, 0, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_AttributeGet(importState, 'alias_definition', aliasList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      do i = lbound(aliasList,1), ubound(aliasList,1)
        write(message,'(A)') trim(name)//' uses alias: '
        call MOSSCO_MessageAdd(message, trim(aliasList(i,1))//' = '//trim(aliasList(i,2)), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      enddo
    endif

    if (allocated(exportList)) then

      call MOSSCO_AttributeSet(exportState, 'export_items', exportList, &
        owner=trim(name), separator=',', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_Reallocate(exportList, 0, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_AttributeGet(exportState, 'export_items', exportList, &
        owner=name, separator=',', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      do i = lbound(exportList,1), ubound(exportList,1)
        write(message,'(A)') trim(name)//' uses export: '
        call MOSSCO_MessageAdd(message, trim(exportList(i,1)), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      enddo
    endif

    n=0
    if (allocated(exportList)) n=size(exportList)

    do i=1, n
      call MOSSCO_ConfigGet(config, exportList(i,1), rpnList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (.not.allocated(rpnList)) then
        write(message,'(A,A)') trim(name)//' does not define calculation for ', &
          trim(exportList(i,1))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        cycle
      endif

      call MOSSCO_AttributeSet(exportState, 'rpn_'//exportList(i,1), rpnList, &
        owner=name, separator=',', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_Reallocate(rpnList, 0, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call MOSSCO_AttributeGet(exportState, 'rpn_'//exportList(i,1), rpnList, &
        owner=name, separator=',', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(name)//' will calculate'
      call MOSSCO_MessageAdd(message, ' '//trim(exportList(i,1)), rc=localrc)
      call MOSSCO_MessageAdd(message,' as ')
      do j = lbound(rpnList,2), ubound(rpnList,2)
        !> @todo add multiline rpn
        call MOSSCO_MessageAdd(message, ' '//trim(rpnList(1,j)), rc=localrc)
      enddo
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      call MOSSCO_StateGet(exportState, fieldList, itemSearch=exportList(i,1), &
        fieldCount=fieldCount, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (fieldCount > 0) cycle

      field = ESMF_FieldEmptyCreate(name = exportList(i,1), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_AttributeSet(field, 'creator', trim(name), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !call MOSSCO_AttributeSet(field, 'rpn_'//exportList(i,1), rpnList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_StateAddReplace(exportState, (/field/), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(name)//' created for export '
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    enddo

    call MOSSCO_CompExit(cplComp, rc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_cplComp)      :: cplComp
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: parentClock
    integer, intent(out)    :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    integer(ESMF_KIND_I4)   :: localrc

    character(len=ESMF_MAXSTR), allocatable :: exportList(:,:), rpnList(:,:)
    character(len=ESMF_MAXSTR), allocatable :: aliasList(:,:), itemNameList(:)
    type(ESMF_Field)                :: field
    type(ESMF_Field), allocatable   :: fieldList(:), importFieldList(:)
    integer(ESMF_KIND_I4)           :: fieldCount, itemCount, n, i, j, k
    character(len=ESMF_MAXSTR), pointer :: includeList(:) => null()
    character(len=ESMF_MAXSTR)      :: message
    type(ESMF_STATEITEM_Flag), allocatable:: itemTypeList(:)
    logical                         :: isMatch, verbose
    character(len=ESMF_MAXSTR), allocatable, dimension(:) :: binaryOperatorList
    character(len=ESMF_MAXSTR), allocatable, dimension(:) :: unaryOperatorList
    character(len=ESMF_MAXSTR), allocatable, dimension(:) :: reductionOperatorList

    integer(ESMF_KIND_I4)              :: sp
    character(len=ESMF_MAXSTR)         :: rpnTypeString
    type(MOSSCO_StackType), allocatable   :: stack(:)

    real(ESMF_KIND_R8), pointer     :: farrayPtr1(:) => null()
    real(ESMF_KIND_R8), pointer     :: farrayPtr2(:,:) => null()
    real(ESMF_KIND_R8), pointer     :: farrayPtr3(:,:,:) => null()
    real(ESMF_KIND_R8), pointer     :: farrayPtr4(:,:,:,:) => null()

    integer(ESMF_KIND_I4)            :: rank, ubnd(4), lbnd(4)
    real(ESMF_KIND_R8), allocatable  :: scalarList(:)
    integer(ESMF_KIND_I4), allocatable  :: matchIndex(:)
    type(ESMF_FieldStatus_Flag)      :: fieldStatus
    type(ESMF_GeomType_Flag)         :: geomType = ESMF_GEOMTYPE_GRID
    type(ESMF_Mesh)                  :: mesh
    type(ESMF_Grid)                  :: grid, grid3

    rc = ESMF_SUCCESS

    allocate(binaryOperatorList(10))
    binaryOperatorList = (/'*  ','/  ','+  ','-  ','** ','^  ','%  ','mod', &
      'rem','pow'/)
    allocate(unaryOperatorList(13))
    unaryOperatorList = (/'e    ','log  ','ln   ','exp  ', 'lg   ','sin  ', 'cos  ', &
      'tan  ', 'sqrt ','asin ','atan ','acos ','abs  '/)
    allocate(reductionOperatorList(4))
    reductionOperatorList = (/'vmean','vsum ','upper','lower'/)

    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    verbose = .true.

    call MOSSCO_AttributeGet(exportState, 'export_items', exportList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    n = 0
    if (allocated(exportList)) n = size(exportList)
    if (.not.associated(includeList)) allocate(includeList(1))

    !call MOSSCO_AttributeGet(exportState, 'alias_definition', aliasList, rc=localrc)
    !_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_StateGet(importState, itemCount=itemCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (itemCount > 0) then
      allocate(itemTypeList(itemCount), itemNameList(itemCount))

      call ESMF_StateGet(importState, itemTypeList=itemTypeList, &
        itemNameList=itemNameList, rc=localrc )
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    do i=1, n

      call MOSSCO_AttributeGet(exportState, 'rpn_'//exportList(i,1), rpnList, &
        owner=name, separator=',', rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> Don't do anything if there is no calculation to be performed
      if (.not.allocated(rpnList)) then
        if (verbose) then
          write(message, '(A,A)') trim(name)//' did not define calculation for ', &
            trim(exportList(i,1))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        endif
        cycle
      endif

      includeList(1) = exportList(i,1)
      call MOSSCO_StateGet(exportState, fieldList, include=includeList, &
        fieldCount=fieldCount, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> Don't do anything if the field is not in the export state
      if (fieldCount < 1) then
        if (verbose) then
          write(message, '(A,A)') trim(name)//' did not find in export state ', &
            trim(exportList(i,1))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        endif
        cycle
      endif

      !> find symbols in rpnList and determine their grid
      !> @todo this only looks for the first grid, others are disregarded

      if (allocated(stack)) deallocate(stack)
      if (allocated(scalarList)) deallocate(scalarList)

      allocate(stack(size(rpnList)))
      allocate(scalarList(size(rpnList)))
      call MOSSCO_Reallocate(importFieldList, size(rpnList), rc=localrc)
      rpnTypeString(1:size(rpnList)) = 'x'

      do j=1, size(rpnList)

        call MOSSCO_StringFind(rpnList(j,1), reductionOperatorList, &
          isMatch=isMatch, owner=name, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (isMatch) then
          rpnTypeString(j:j) = 'r' ! unary reduction operator
          cycle
        endif

        call MOSSCO_StringFind(rpnList(j,1), unaryOperatorList, &
          isMatch=isMatch, owner=name, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (isMatch) then
          rpnTypeString(j:j) = 'u' ! unary operator
          cycle
        endif

        call MOSSCO_StringFind(rpnList(j,1), binaryOperatorList, &
          isMatch=isMatch, owner=name, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (isMatch) then
          rpnTypeString(j:j) = 'b' ! binary operator
          cycle
        endif

        if (isDecimal(rpnList(j,1), rc=localrc)) then
          read(unit=rpnList(j,1), fmt=*, iostat=localrc) scalarList(j)
          rpnTypeString(j:j) = 'd' ! this is a isDecimal
          cycle
        endif
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      enddo !j=1, size(rpnList), looking for operators 'rub'

      do j=1, size(rpnList)

        !> Skip operators which we have dealt with
        if (index('rub',rpnTypeString(j:j))>0) cycle

        if (allocated(matchIndex)) deallocate(matchIndex)
        call MOSSCO_StringFind(rpnList(j,1), itemNameList, isMatch=isMatch, &
          matchIndex=matchIndex, owner=name, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (.not.isMatch) then
          write(message,'(A)') trim(name)//' item "'//trim(rpnList(j,1))// &
            '" is not operator or input or number.'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
          cycle
        endif

        rpnTypeString(j:j) = 's' ! this is a symbol

        !> @todo consider alias
        includeList(1) = itemNameList(matchIndex(1))
        call MOSSCO_StateGet(importState, fieldList, fieldCount=fieldCount, &
          include=includeList, fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), &
          rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (fieldCount == 0) then
          write(message, '(A)') trim(name)//' could not find complete item "'// &
            trim(includeList(1))//'" required to calculate '//trim(exportList(i,1))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          rc = ESMF_RC_NOT_FOUND
          return
        endif

        importFieldList(j) = fieldList(1)

        call ESMF_FieldGet(fieldList(1), geomType=geomType, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        if (geomType == ESMF_GEOMTYPE_MESH) then
          call ESMF_FieldGet(fieldList(1), mesh=mesh, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        elseif (geomType == ESMF_GEOMTYPE_GRID) then

          call ESMF_FieldGet(fieldList(1), grid=grid, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          !> Reduce the grid if reduction operator present
          if (index(rpnTypeString,'r') > 0) then

            call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            if (rank /= 3) then
              localrc = ESMF_RC_NOT_IMPL
              _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
            endif

            grid3 = grid
            grid = MOSSCO_GridCreateFromOtherGrid(grid3, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          endif
        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif

        cycle
      enddo

      includeList(1) = exportList(i,1)
      call MOSSCO_StateGet(exportState, fieldList, include=includeList, &
        fieldCount=fieldCount, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(fieldList(1), status=fieldStatus, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) then
        if (geomType == ESMF_GEOMTYPE_GRID) then
          call ESMF_FieldEmptySet(fieldList(1), grid=grid, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        elseif (geomType == ESMF_GEOMTYPE_MESH) then
          call ESMF_FieldEmptySet(fieldList(1), mesh=mesh, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
        write(message, '(A)') trim(name)//' added geometry to '
        call MOSSCO_FieldString(fieldList(1), message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      call ESMF_FieldGet(fieldList(1), status=fieldStatus, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (fieldStatus == ESMF_FIELDSTATUS_GRIDSET) then
        call ESMF_FieldEmptyComplete(fieldList(1), typeKind=ESMF_TYPEKIND_R8, &
          rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        write(message, '(A)') trim(name)//' completed '
        call MOSSCO_FieldString(fieldList(1), message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      sp = 0
      do j=lbound(rpnList,1), ubound(rpnList,1)

        !> push item onto stack if it is not an operator
        if (index('rub', rpnTypeString(j:j)) < 1) then
          sp = sp + 1

          if (rpnTypeString(j:j) == 'd') then
            stack(sp)%rank=0
            allocate(stack(sp)%scalar(1))
            stack(sp)%scalar(1) = scalarList(j)

          elseif (rpnTypeString(j:j) == 's') then

            call MOSSCO_StringFind(rpnList(j,1), itemNameList, isMatch=isMatch, &
              matchIndex=matchIndex, owner=name, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            !> @todo consider alias

            includeList(1) = itemNameList(matchIndex(1))
            call MOSSCO_StateGet(importState, fieldList, include=includeList, &
                fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            call ESMF_FieldGet(fieldList(1), rank=stack(sp)%rank, rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=&
              lbnd(1:stack(sp)%rank), &
              exclusiveUBound=ubnd(1:stack(sp)%rank), rc=localrc)
            _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

            if (stack(sp)%rank == 4) then
              call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr4, rc=localrc)
              _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
              allocate(stack(sp)%farray4(RANGE4D))
              stack(sp)%farray4(RANGE4D) = farrayPtr4(RANGE4D)
              call MOSSCO_FieldGetMask(fieldList(1), mask4=stack(sp)%mask4, owner=name, rc=localrc)
              _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
            elseif (stack(sp)%rank == 3) then
              call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr3, rc=localrc)
              _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
              allocate(stack(sp)%farray3(RANGE3D))
              stack(sp)%farray3(RANGE3D) = farrayPtr3(RANGE3D)
              call MOSSCO_FieldGetMask(fieldList(1), mask3=stack(sp)%mask3, owner=name, rc=localrc)
              _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
            elseif (stack(sp)%rank == 2) then
              call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
              _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
              allocate(stack(sp)%farray2(RANGE2D))
              stack(sp)%farray2(RANGE2D) = farrayPtr2(RANGE2D)
              call MOSSCO_FieldGetMask(fieldList(1), mask2=stack(sp)%mask2, owner=name, rc=localrc)
              _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
            elseif (stack(sp)%rank == 1) then
              call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
              _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
              allocate(stack(sp)%farray1(RANGE1D))
              stack(sp)%farray1(RANGE1D) = farrayPtr1(RANGE1D)
              call MOSSCO_FieldGetMask(fieldList(1), mask1=stack(sp)%mask1, owner=name, rc=localrc)
              _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
            endif

          else
            write(message,'(A)') trim(name)//' encountered invalid rpn type'
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
            rc = ESMF_RC_ARG_BAD
            return
          endif

          cycle
        endif

        !> We found an operator, deal with unary and reduction first, i.e. pop item off the
        !> stack, operate on it, and push it back.  The stack pointer is not
        !> changed
        if (index('ru', rpnTypeString(j:j)) > 0) then
          write(0,*) 'rpn',j, sp, stack(sp)%rank, rpnTypeString(1:j), trim(rpnList(j,1))
          if (sp < 1) localrc=ESMF_RC_ARG_BAD
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          select case(stack(sp)%rank)
          case (0)
            write(message,'(A,ES10.3,A)') trim(name)//' calculates '//&
              trim(rpnList(j,1))//'(',stack(sp)%scalar(1),')'

            select case (trim(adjustl(rpnList(j,1))))
            case ('abs') ; stack(sp)%scalar(1) = abs(stack(sp)%scalar(1))
            case ('acos') ; stack(sp)%scalar(1) = acos(stack(sp)%scalar(1))
            case ('asin') ; stack(sp)%scalar(1) = asin(stack(sp)%scalar(1))
            case ('atan') ; stack(sp)%scalar(1) = atan(stack(sp)%scalar(1))
            case ('cos') ; stack(sp)%scalar(1) = cos(stack(sp)%scalar(1))
            case ('exp','e') ; stack(sp)%scalar(1) = exp(stack(sp)%scalar(1))
            case ('log','ln')
              if (stack(sp)%scalar(1) > 0) then
                stack(sp)%scalar(1) = log(stack(sp)%scalar(1))
              else ; stack(sp)%scalar(1) = -1D30; endif
            case ('lg')
              if (stack(sp)%scalar(1) > 0) then
                stack(sp)%scalar(1) = log10(stack(sp)%scalar(1))
              else ; stack(sp)%scalar(1) = -1D30; endif
            case ('sqrt') ; stack(sp)%scalar(1) = sqrt(stack(sp)%scalar(1))
              if (stack(sp)%scalar(1) < 0) then ; stack(sp)%scalar(1) = -1D30
              else ; stack(sp)%scalar(1) = sqrt(stack(sp)%scalar(1)); endif
            case ('sin') ; stack(sp)%scalar(1) = sin(stack(sp)%scalar(1))
            case default
              write(message,'(A)') trim(name)//' does not implement operation "'// &
                rpnList(j,1)//'" for scalar'
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
            end select
            write(message,'(A,ES10.3)') trim(message)//' = ', stack(sp)%scalar(1)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          case (1)
            write(message,'(A,ES10.3,A)') trim(name)//' calculates '//&
              trim(rpnList(j,1))//'(',sum(stack(sp)%farray1,stack(sp)%mask1)/count(stack(sp)%mask1),')'

            select case (trim(adjustl(rpnList(j,1))))
            case ('abs')
              where(stack(sp)%mask1) ; stack(sp)%farray1 = abs(stack(sp)%farray1); endwhere
            case ('acos')
              where(stack(sp)%mask1) ; stack(sp)%farray1 = acos(stack(sp)%farray1); endwhere
            case ('asin')
              where(stack(sp)%mask1) ; stack(sp)%farray1 = asin(stack(sp)%farray1); endwhere
            case ('atan')
              where(stack(sp)%mask1) ; stack(sp)%farray1 = atan(stack(sp)%farray1); endwhere
            case ('cos')
              where(stack(sp)%mask1) ; stack(sp)%farray1 = cos(stack(sp)%farray1); endwhere
            case ('exp','e')
              where(stack(sp)%mask1) ; stack(sp)%farray1 = exp(stack(sp)%farray1); endwhere
            case ('log','ln')
              where(stack(sp)%mask1 .and. stack(sp)%farray1 > 0)
                stack(sp)%farray1 = log(stack(sp)%farray1)
              endwhere
            case ('lg')
              where(stack(sp)%mask1 .and. stack(sp)%farray1 > 0)
                stack(sp)%farray1 = log10(stack(sp)%farray1)
              endwhere
            case ('sqrt')
              where(stack(sp)%mask1 .and. stack(sp)%farray1 > -tiny(0.0d0))
                stack(sp)%farray1 = log10(stack(sp)%farray1)
              endwhere
            case ('sin')
              where(stack(sp)%mask1) ; stack(sp)%farray1 = sin(stack(sp)%farray1); endwhere
            case default
              write(message,'(A,I1)') trim(name)//' does not implement operation "'// &
                rpnList(j,1)//'" for rank ',stack(sp)%rank
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
            end select
            write(message,'(A,ES10.3)') trim(message)//' = ', &
              sum(stack(sp)%farray1,stack(sp)%mask1)/count(stack(sp)%mask1)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          case (2)
            write(message,'(A,ES10.3,A)') trim(name)//' calculates '//&
              trim(rpnList(j,1))//'(',sum(stack(sp)%farray2,stack(sp)%mask2)/count(stack(sp)%mask2),')'

            select case (trim(adjustl(rpnList(j,1))))
            case ('abs')
              where(stack(sp)%mask2) ; stack(sp)%farray2 = abs(stack(sp)%farray2); endwhere
            case ('acos')
              where(stack(sp)%mask2) ; stack(sp)%farray2 = acos(stack(sp)%farray2); endwhere
            case ('asin')
              where(stack(sp)%mask2) ; stack(sp)%farray2 = asin(stack(sp)%farray2); endwhere
            case ('atan')
              where(stack(sp)%mask2) ; stack(sp)%farray2 = atan(stack(sp)%farray2); endwhere
            case ('cos')
              where(stack(sp)%mask2) ; stack(sp)%farray2 = cos(stack(sp)%farray2); endwhere
            case ('exp','e')
              where(stack(sp)%mask2) ; stack(sp)%farray2 = exp(stack(sp)%farray2); endwhere
            case ('log','ln')
              where(stack(sp)%mask2 .and. stack(sp)%farray2 > 0)
                stack(sp)%farray2 = log(stack(sp)%farray2)
              endwhere
            case ('lg')
              where(stack(sp)%mask2 .and. stack(sp)%farray2 > 0)
                stack(sp)%farray2 = log10(stack(sp)%farray2)
              endwhere
            case ('sqrt')
              where(stack(sp)%mask2 .and. stack(sp)%farray2 > -tiny(0.0d0))
                stack(sp)%farray2 = log10(stack(sp)%farray2)
              endwhere
            case ('sin')
              where(stack(sp)%mask2) ; stack(sp)%farray2 = sin(stack(sp)%farray2); endwhere
            case default
              write(message,'(A,I1)') trim(name)//' does not implement operation "'// &
                rpnList(j,1)//'" for rank ',stack(sp)%rank
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
            end select
            write(message,'(A,ES10.3)') trim(message)//' = ', &
              sum(stack(sp)%farray2,stack(sp)%mask2)/count(stack(sp)%mask2)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          case (3)
            write(message,'(A,ES10.3,A)') trim(name)//' calculates '//&
              trim(rpnList(j,1))//'(',sum(stack(sp)%farray3,stack(sp)%mask3)/count(stack(sp)%mask3),')'

            select case (trim(adjustl(rpnList(j,1))))
            case ('abs')
              where(stack(sp)%mask3) ; stack(sp)%farray3 = abs(stack(sp)%farray3); endwhere
            case ('acos')
              where(stack(sp)%mask3) ; stack(sp)%farray3 = acos(stack(sp)%farray3); endwhere
            case ('asin')
              where(stack(sp)%mask3) ; stack(sp)%farray3 = asin(stack(sp)%farray3); endwhere
            case ('atan')
              where(stack(sp)%mask3) ; stack(sp)%farray3 = atan(stack(sp)%farray3); endwhere
            case ('cos')
              where(stack(sp)%mask3) ; stack(sp)%farray3 = cos(stack(sp)%farray3); endwhere
            case ('exp','e')
              where(stack(sp)%mask3) ; stack(sp)%farray3 = exp(stack(sp)%farray3); endwhere
            case ('log','ln')
              where(stack(sp)%mask3 .and. stack(sp)%farray3 > 0)
                stack(sp)%farray3 = log(stack(sp)%farray3)
              endwhere
            case ('lg')
              where(stack(sp)%mask3 .and. stack(sp)%farray3 > 0)
                stack(sp)%farray3 = log10(stack(sp)%farray3)
              endwhere
            case ('sqrt')
              where(stack(sp)%mask3 .and. stack(sp)%farray3 > -tiny(0.0d0))
                stack(sp)%farray3 = log10(stack(sp)%farray3)
              endwhere
            case ('sin')
              where(stack(sp)%mask3) ; stack(sp)%farray3 = sin(stack(sp)%farray3); endwhere
            case ('lower','upper','vmean','vsum')
              allocate(stack(sp)%mask2(RANGE2D))
              allocate(stack(sp)%farray2(RANGE2D))
              if (trim(adjustl(rpnList(j,1)))=='lower') then
                stack(sp)%farray2 = stack(sp)%farray3(RANGE2D,lbnd(3))
              elseif (trim(adjustl(rpnList(j,1)))=='upper') then
                stack(sp)%farray2 = stack(sp)%farray3(RANGE2D,ubnd(3))
              elseif (trim(adjustl(rpnList(j,1)))=='vsum') then
                stack(sp)%farray2 = sum(stack(sp)%farray3, &
                  dim=stack(sp)%rank, mask=stack(sp)%mask3)
              elseif (trim(adjustl(rpnList(j,1)))=='vsum') then
                stack(sp)%farray2 = sum(stack(sp)%farray3, &
                  dim=stack(sp)%rank, mask=stack(sp)%mask3) / &
                  count(stack(sp)%mask3,dim=stack(sp)%rank)
              endif
              stack(sp)%mask2 = all(stack(sp)%mask3, dim=stack(sp)%rank)
              deallocate(stack(sp)%mask3,stack(sp)%farray3)
              stack(sp)%rank=stack(sp)%rank-1
            case default
              write(message,'(A,I1)') trim(name)//' does not implement operation "'// &
                rpnList(j,1)//'" for rank ',stack(sp)%rank
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
            end select
            if (stack(sp)%rank == 3) then
              write(message,'(A,ES10.3)') trim(message)//' = ', &
                sum(stack(sp)%farray3,stack(sp)%mask3)/count(stack(sp)%mask3)
            elseif (stack(sp)%rank == 2) then
              write(message,'(A,ES10.3)') trim(message)//' = ', &
                sum(stack(sp)%farray2,stack(sp)%mask2)/count(stack(sp)%mask2)
            endif
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          case (4)
            write(message,'(A,ES10.3,A)') trim(name)//' calculates '//&
              trim(rpnList(j,1))//'(',sum(stack(sp)%farray4,stack(sp)%mask4)/count(stack(sp)%mask4),')'

            select case (trim(adjustl(rpnList(j,1))))
            case ('abs')
              where(stack(sp)%mask4) ; stack(sp)%farray4 = abs(stack(sp)%farray4); endwhere
            case ('acos')
              where(stack(sp)%mask4) ; stack(sp)%farray4 = acos(stack(sp)%farray4); endwhere
            case ('asin')
              where(stack(sp)%mask4) ; stack(sp)%farray4 = asin(stack(sp)%farray4); endwhere
            case ('atan')
              where(stack(sp)%mask4) ; stack(sp)%farray4 = atan(stack(sp)%farray4); endwhere
            case ('cos')
              where(stack(sp)%mask4) ; stack(sp)%farray4 = cos(stack(sp)%farray4); endwhere
            case ('exp','e')
              where(stack(sp)%mask4) ; stack(sp)%farray4 = exp(stack(sp)%farray4); endwhere
            case ('log','ln')
              where(stack(sp)%mask4 .and. stack(sp)%farray4 > 0)
                stack(sp)%farray4 = log(stack(sp)%farray4)
              endwhere
            case ('lg')
              where(stack(sp)%mask4 .and. stack(sp)%farray4 > 0)
                stack(sp)%farray4 = log10(stack(sp)%farray4)
              endwhere
            case ('sqrt')
              where(stack(sp)%mask4 .and. stack(sp)%farray4 > -tiny(0.0d0))
                stack(sp)%farray4 = log10(stack(sp)%farray4)
              endwhere
            case ('sin')
              where(stack(sp)%mask4) ; stack(sp)%farray4 = sin(stack(sp)%farray4); endwhere
            case default
              write(message,'(A,I1)') trim(name)//' does not implement operation "'// &
                rpnList(j,1)//'" for rank ',stack(sp)%rank
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
            end select
            write(message,'(A,ES10.3)') trim(message)//' = ', &
              sum(stack(sp)%farray4,stack(sp)%mask4)/count(stack(sp)%mask4)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          end select

        elseif (rpnTypeString(j:j) == 'b') then
          write(0,*) 'rpn',j, sp, stack(sp)%rank, stack(sp-1)%rank, rpnTypeString(1:j), trim(rpnList(j,1))
          if (sp < 2) localrc=ESMF_RC_ARG_BAD
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          ! Promote a scalar first argument (which is overwritten) to rank of
          ! second argument
          if (stack(sp-1)%rank == 0 .and. stack(sp)%rank > 0) then
            select case(stack(sp)%rank)
            case(1)
              allocate(stack(sp-1)%mask1(RANGE1D)); stack(sp-1)%mask1=stack(sp)%mask1
              allocate(stack(sp-1)%farray1(RANGE1D))
              stack(sp-1)%farray1=stack(sp-1)%scalar(1)
            case(2)
              allocate(stack(sp-1)%mask2(RANGE2D)); stack(sp-1)%mask2=stack(sp)%mask2
              allocate(stack(sp-1)%farray2(RANGE2D))
              stack(sp-1)%farray2=stack(sp-1)%scalar(1)
            case(3)
              allocate(stack(sp-1)%mask3(RANGE3D)); stack(sp-1)%mask3=stack(sp)%mask3
              allocate(stack(sp-1)%farray3(RANGE3D))
              stack(sp-1)%farray3=stack(sp-1)%scalar(1)
            case(4)
              allocate(stack(sp-1)%mask4(RANGE4D)); stack(sp-1)%mask4=stack(sp)%mask4
              allocate(stack(sp-1)%farray4(RANGE4D))
              stack(sp-1)%farray4=stack(sp-1)%scalar(1)
            end select ! stack(sp)%rank
            stack(sp-1)%rank = stack(sp)%rank
            deallocate(stack(sp-1)%scalar)
          endif

          if (stack(sp)%rank /= 0 .and. stack(sp)%rank /= stack(sp-1)%rank) then
            write(message,'(A,I1,A,I1)') trim(name)//' does not implement operation "'// &
              rpnList(j,1)//'" for ranks ',stack(sp-1)%rank,' and ',stack(sp)%rank
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
          endif

          if (stack(sp)%rank == 0) then
            select case(stack(sp-1)%rank)
            case (0)
              write(message,'(A,ES10.3,X,A,X,ES10.3)') trim(name)//' calculates ',&
                stack(sp-1)%scalar(1),trim(rpnList(j,1)),stack(sp)%scalar(1)
              select case(trim(adjustl(rpnList(j,1))))
              case('+')
                stack(sp-1)%scalar(1) = stack(sp-1)%scalar(1) + stack(sp)%scalar(1)
              case('-')
                stack(sp-1)%scalar(1) = stack(sp-1)%scalar(1) - stack(sp)%scalar(1)
              case('*')
                stack(sp-1)%scalar(1) = stack(sp-1)%scalar(1) * stack(sp)%scalar(1)
              case('/')
                if (abs(stack(sp)%scalar(1)) > tiny(0.0d0)) then
                  stack(sp-1)%scalar(1)  = stack(sp-1)%scalar(1)  / stack(sp)%scalar(1)
                else
                  stack(sp-1)%scalar(1)  = -1.0d30
                endif
              case ('**','^','pow')
                stack(sp-1)%scalar(1) = stack(sp-1)%scalar(1) ** stack(sp)%scalar(1)
              case('%','mod')
                stack(sp-1)%scalar(1)  = modulo(stack(sp-1)%scalar(1) ,stack(sp)%scalar(1))
              case default
                write(message,'(A,I1,A)') trim(name)//' does not implement operation "'// &
                  rpnList(j,1)//'" for two scalars'
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
              end select
              write(message,'(A,ES10.3)') trim(message)//' = ', stack(sp-1)%scalar(1)
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
            case (1)
              write(message,'(A,ES10.3,X,A,X,ES10.3)') trim(name)//' calculates ',&
                sum(stack(sp-1)%farray1, &
                stack(sp-1)%mask1)/count(stack(sp-1)%mask1),trim(rpnList(j,1)), &
                stack(sp)%scalar(1)
              select case(trim(adjustl(rpnList(j,1))))
              case('+')
                where(stack(sp-1)%mask1)
                  stack(sp-1)%farray1 = stack(sp-1)%farray1 + stack(sp)%scalar(1)
                endwhere
              case('-')
                where(stack(sp-1)%mask1)
                  stack(sp-1)%farray1 = stack(sp-1)%farray1 - stack(sp)%scalar(1)
                endwhere
              case('*')
                where(stack(sp-1)%mask1)
                  stack(sp-1)%farray1 = stack(sp-1)%farray1 * stack(sp)%scalar(1)
                endwhere
              case('/')
                if (abs(stack(sp)%scalar(1)) > tiny(0.0d0)) then
                  where(stack(sp-1)%mask1)
                    stack(sp-1)%farray1 = stack(sp-1)%farray1 / stack(sp)%scalar(1)
                  endwhere
                else
                  stack(sp-1)%farray1 = -1.0d30
                endif
              case ('**','^','pow')
                where(stack(sp-1)%mask1)
                  stack(sp-1)%farray1 = stack(sp-1)%farray1 ** stack(sp)%scalar(1)
                endwhere
              case('%','mod')
                where(stack(sp-1)%mask1)
                  stack(sp-1)%farray1 = modulo(stack(sp-1)%farray1,stack(sp)%scalar(1))
                endwhere
              case default
                write(message,'(A,I1,A)') trim(name)//' does not implement operation "'// &
                  rpnList(j,1)//'" for ranks ',stack(sp-1)%rank,' and scalar'
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
              end select
              write(message,'(A,ES10.3)') trim(message)//' = ', &
                sum(stack(sp-1)%farray1,stack(sp-1)%mask1)/count(stack(sp-1)%mask1)
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
            case (2)
              write(message,'(A,ES10.3,X,A,X,ES10.3)') trim(name)//' calculates ',&
                sum(stack(sp-1)%farray2, &
                stack(sp-1)%mask2)/count(stack(sp-1)%mask2),trim(rpnList(j,1)), &
                stack(sp)%scalar(1)
              select case(trim(adjustl(rpnList(j,1))))
              case('+')
                where(stack(sp-1)%mask2)
                  stack(sp-1)%farray2 = stack(sp-1)%farray2 + stack(sp)%scalar(1)
                endwhere
              case('-')
                where(stack(sp-1)%mask2)
                  stack(sp-1)%farray2 = stack(sp-1)%farray2 - stack(sp)%scalar(1)
                endwhere
              case('*')
                where(stack(sp-1)%mask2)
                  stack(sp-1)%farray2 = stack(sp-1)%farray2 * stack(sp)%scalar(1)
                endwhere
              case('/')
                if (abs(stack(sp)%scalar(1)) > tiny(0.0d0)) then
                  where(stack(sp-1)%mask2)
                    stack(sp-1)%farray2 = stack(sp-1)%farray2 / stack(sp)%scalar(1)
                  endwhere
                else
                  stack(sp-1)%farray2 = -1.0d30
                endif
              case ('**','^','pow')
                where(stack(sp-1)%mask2)
                  stack(sp-1)%farray2 = stack(sp-1)%farray2 ** stack(sp)%scalar(1)
                endwhere
              case('%','mod')
                where(stack(sp-1)%mask2)
                  stack(sp-1)%farray2 = modulo(stack(sp-1)%farray2,stack(sp)%scalar(1))
                endwhere
              case default
                write(message,'(A,I1,A)') trim(name)//' does not implement operation "'// &
                  rpnList(j,1)//'" for ranks ',stack(sp-1)%rank,' and scalar'
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
              end select
              write(message,'(A,ES10.3)') trim(message)//' = ', &
                sum(stack(sp-1)%farray2,stack(sp-1)%mask2)/count(stack(sp-1)%mask2)
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
            case (3)
              write(message,'(A,ES10.3,X,A,X,ES10.3)') trim(name)//' calculates ',&
                sum(stack(sp-1)%farray3, &
                stack(sp-1)%mask3)/count(stack(sp-1)%mask3),trim(rpnList(j,1)), &
                stack(sp)%scalar(1)
              select case(trim(adjustl(rpnList(j,1))))
              case('+')
                where(stack(sp-1)%mask3)
                  stack(sp-1)%farray3 = stack(sp-1)%farray3 + stack(sp)%scalar(1)
                endwhere
              case('-')
                where(stack(sp-1)%mask3)
                  stack(sp-1)%farray3 = stack(sp-1)%farray3 - stack(sp)%scalar(1)
                endwhere
              case('*')
                where(stack(sp-1)%mask3)
                  stack(sp-1)%farray3 = stack(sp-1)%farray3 * stack(sp)%scalar(1)
                endwhere
              case('/')
                if (abs(stack(sp)%scalar(1)) > tiny(0.0d0)) then
                  where(stack(sp-1)%mask3)
                    stack(sp-1)%farray3 = stack(sp-1)%farray3 / stack(sp)%scalar(1)
                  endwhere
                else
                  stack(sp-1)%farray3 = -1.0d30
                endif
              case ('**','^','pow')
                where(stack(sp-1)%mask3)
                  stack(sp-1)%farray3 = stack(sp-1)%farray3 ** stack(sp)%scalar(1)
                endwhere
              case('%','mod')
                where(stack(sp-1)%mask3)
                  stack(sp-1)%farray3 = modulo(stack(sp-1)%farray3,stack(sp)%scalar(1))
                endwhere
              case default
                write(message,'(A,I1,A)') trim(name)//' does not implement operation "'// &
                  rpnList(j,1)//'" for ranks ',stack(sp-1)%rank,' and scalar'
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
              end select
              write(message,'(A,ES10.3)') trim(message)//' = ', &
                sum(stack(sp-1)%farray3,stack(sp-1)%mask3)/count(stack(sp-1)%mask3)
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
            case (4)
              write(message,'(A,ES10.3,X,A,X,ES10.3)') trim(name)//' calculates ',&
                sum(stack(sp-1)%farray4, &
                stack(sp-1)%mask4)/count(stack(sp-1)%mask4),trim(rpnList(j,1)), &
                stack(sp)%scalar(1)
              select case(trim(adjustl(rpnList(j,1))))
              case('+')
                where(stack(sp-1)%mask4)
                  stack(sp-1)%farray4 = stack(sp-1)%farray4 + stack(sp)%scalar(1)
                endwhere
              case('-')
                where(stack(sp-1)%mask4)
                  stack(sp-1)%farray4 = stack(sp-1)%farray4 - stack(sp)%scalar(1)
                endwhere
              case('*')
                where(stack(sp-1)%mask4)
                  stack(sp-1)%farray4 = stack(sp-1)%farray4 * stack(sp)%scalar(1)
                endwhere
              case('/')
                if (abs(stack(sp)%scalar(1)) > tiny(0.0d0)) then
                  where(stack(sp-1)%mask4)
                    stack(sp-1)%farray4 = stack(sp-1)%farray4 / stack(sp)%scalar(1)
                  endwhere
                else
                  stack(sp-1)%farray4 = -1.0d30
                endif
              case ('**','^','pow')
                where(stack(sp-1)%mask4)
                  stack(sp-1)%farray4 = stack(sp-1)%farray4 ** stack(sp)%scalar(1)
                endwhere
              case('%','mod')
                where(stack(sp-1)%mask4)
                  stack(sp-1)%farray4 = modulo(stack(sp-1)%farray4,stack(sp)%scalar(1))
                endwhere
              case default
                write(message,'(A,I1,A)') trim(name)//' does not implement operation "'// &
                  rpnList(j,1)//'" for ranks ',stack(sp-1)%rank,' and scalar'
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
              end select
              write(message,'(A,ES10.3)') trim(message)//' = ', &
                sum(stack(sp-1)%farray4,stack(sp-1)%mask4)/count(stack(sp-1)%mask4)
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
            end select
            deallocate(stack(sp)%scalar)
            stack(sp)%rank = -1
          elseif (stack(sp)%rank == stack(sp-1)%rank) then
            select case(stack(sp)%rank)
            case (1)
              stack(sp-1)%mask1 = (stack(sp-1)%mask1 .and. stack(sp)%mask1)
              write(message,'(A,ES10.3,X,A,X,ES10.3)') trim(name)//' calculates ',&
                sum(stack(sp-1)%farray1, &
                stack(sp-1)%mask1)/count(stack(sp-1)%mask1),trim(rpnList(j,1)), &
                sum(stack(sp)%farray1,stack(sp-1)%mask1)/count(stack(sp-1)%mask1)
              select case(trim(adjustl(rpnList(j,1))))
              case('+')
                where(stack(sp-1)%mask1)
                  stack(sp-1)%farray1 = stack(sp-1)%farray1 + stack(sp)%farray1
                endwhere
              case('-')
                where(stack(sp-1)%mask1)
                  stack(sp-1)%farray1 = stack(sp-1)%farray1 - stack(sp)%farray1
                endwhere
              case('*')
                where(stack(sp-1)%mask1)
                  stack(sp-1)%farray1 = stack(sp-1)%farray1 * stack(sp)%farray1
                endwhere
              case('/')
                where(stack(sp-1)%mask1 .and. stack(sp)%farray1>0)
                  stack(sp-1)%farray1 = stack(sp-1)%farray1 / stack(sp)%farray1
                endwhere
              case('%','mod')
                where(stack(sp-1)%mask1)
                  stack(sp-1)%farray1 = modulo(stack(sp-1)%farray1,stack(sp)%farray1)
                endwhere
              case default
                write(message,'(A,I1,A,I1)') trim(name)//' does not implement operation "'// &
                  rpnList(j,1)//'" for ranks ',stack(sp-1)%rank,' and ',stack(sp)%rank
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
              end select
              write(message,'(A,ES10.3)') trim(message)//' = ', &
                sum(stack(sp-1)%farray1,stack(sp-1)%mask1)/count(stack(sp-1)%mask1)
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
              deallocate(stack(sp)%farray1,stack(sp)%mask1)
              stack(sp)%rank = -1
            case (2)
              stack(sp-1)%mask2 = (stack(sp-1)%mask2 .and. stack(sp)%mask2)
              write(message,'(A,ES10.3,X,A,X,ES10.3)') trim(name)//' calculates ',&
                sum(stack(sp-1)%farray2, &
                stack(sp-1)%mask2)/count(stack(sp-1)%mask2),trim(rpnList(j,1)), &
                sum(stack(sp)%farray2,stack(sp-1)%mask2)/count(stack(sp-1)%mask2)
              select case(trim(adjustl(rpnList(j,1))))
              case('+')
                where(stack(sp-1)%mask2)
                  stack(sp-1)%farray2 = stack(sp-1)%farray2 + stack(sp)%farray2
                endwhere
              case('-')
                where(stack(sp-1)%mask2)
                  stack(sp-1)%farray2 = stack(sp-1)%farray2 - stack(sp)%farray2
                endwhere
              case('*')
                where(stack(sp-1)%mask2)
                  stack(sp-1)%farray2 = stack(sp-1)%farray2 * stack(sp)%farray2
                endwhere
              case('/')
                where(stack(sp-1)%mask2 .and. stack(sp)%farray2>0)
                  stack(sp-1)%farray2 = stack(sp-1)%farray2 / stack(sp)%farray2
                endwhere
              case('%','mod')
                where(stack(sp-1)%mask2)
                  stack(sp-1)%farray2 = modulo(stack(sp-1)%farray2,stack(sp)%farray2)
                endwhere
              case default
                write(message,'(A,I1,A,I1)') trim(name)//' does not implement operation "'// &
                  rpnList(j,1)//'" for ranks ',stack(sp-1)%rank,' and ',stack(sp)%rank
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
              end select
              write(message,'(A,ES10.3)') trim(message)//' = ', &
                sum(stack(sp-1)%farray2,stack(sp-1)%mask2)/count(stack(sp-1)%mask2)
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
              deallocate(stack(sp)%farray2,stack(sp)%mask2)
              stack(sp)%rank = -1
            case (3)
              stack(sp-1)%mask3 = (stack(sp-1)%mask3 .and. stack(sp)%mask3)
              write(message,'(A,ES10.3,X,A,X,ES10.3)') trim(name)//' calculates ',&
                sum(stack(sp-1)%farray3, &
                stack(sp-1)%mask3)/count(stack(sp-1)%mask3),trim(rpnList(j,1)), &
                sum(stack(sp)%farray3,stack(sp-1)%mask3)/count(stack(sp-1)%mask3)
              select case(trim(adjustl(rpnList(j,1))))
              case('+')
                where(stack(sp-1)%mask3)
                  stack(sp-1)%farray3 = stack(sp-1)%farray3 + stack(sp)%farray3
                endwhere
              case('-')
                where(stack(sp-1)%mask3)
                  stack(sp-1)%farray3 = stack(sp-1)%farray3 - stack(sp)%farray3
                endwhere
              case('*')
                where(stack(sp-1)%mask3)
                  stack(sp-1)%farray3 = stack(sp-1)%farray3 * stack(sp)%farray3
                endwhere
              case('/')
                where(stack(sp-1)%mask3 .and. stack(sp)%farray3>0)
                  stack(sp-1)%farray3 = stack(sp-1)%farray3 / stack(sp)%farray3
                endwhere
              case('%','mod')
                where(stack(sp-1)%mask3)
                  stack(sp-1)%farray3 = modulo(stack(sp-1)%farray3,stack(sp)%farray3)
                endwhere
              case default
                write(message,'(A,I1,A,I1)') trim(name)//' does not implement operation "'// &
                  rpnList(j,1)//'" for ranks ',stack(sp-1)%rank,' and ',stack(sp)%rank
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
              end select
              write(message,'(A,ES10.3)') trim(message)//' = ', &
                sum(stack(sp-1)%farray3,stack(sp-1)%mask3)/count(stack(sp-1)%mask3)
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
              deallocate(stack(sp)%farray3,stack(sp)%mask3)
              stack(sp)%rank = -1
            case (4)
              stack(sp-1)%mask4 = (stack(sp-1)%mask4 .and. stack(sp)%mask4)
              write(message,'(A,ES10.3,X,A,X,ES10.3)') trim(name)//' calculates ',&
                sum(stack(sp-1)%farray4, &
                stack(sp-1)%mask4)/count(stack(sp-1)%mask4),trim(rpnList(j,1)), &
                sum(stack(sp)%farray4,stack(sp-1)%mask4)/count(stack(sp-1)%mask4)
              select case(trim(adjustl(rpnList(j,1))))
              case('+')
                where(stack(sp-1)%mask4)
                  stack(sp-1)%farray4 = stack(sp-1)%farray4 + stack(sp)%farray4
                endwhere
              case('-')
                where(stack(sp-1)%mask4)
                  stack(sp-1)%farray4 = stack(sp-1)%farray4 - stack(sp)%farray4
                endwhere
              case('*')
                where(stack(sp-1)%mask4)
                  stack(sp-1)%farray4 = stack(sp-1)%farray4 * stack(sp)%farray4
                endwhere
              case('/')
                where(stack(sp-1)%mask4 .and. stack(sp)%farray4>0)
                  stack(sp-1)%farray4 = stack(sp-1)%farray4 / stack(sp)%farray4
                endwhere
              case('%','mod')
                where(stack(sp-1)%mask4)
                  stack(sp-1)%farray4 = modulo(stack(sp-1)%farray4,stack(sp)%farray4)
                endwhere
              case default
                write(message,'(A,I1,A,I1)') trim(name)//' does not implement operation "'// &
                  rpnList(j,1)//'" for ranks ',stack(sp-1)%rank,' and ',stack(sp)%rank
                call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, ESMF_CONTEXT)
              end select
              write(message,'(A,ES10.3)') trim(message)//' = ', &
                sum(stack(sp-1)%farray4,stack(sp-1)%mask4)/count(stack(sp-1)%mask4)
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
              deallocate(stack(sp)%farray4,stack(sp)%mask4)
              stack(sp)%rank = -1
            end select !case(stack(sp)%rank)
          endif ! stack(sp)%rank == stack(sp-1)%rank

          sp = sp - 1

        else
          localrc = ESMF_RC_NOT_IMPL
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif

      enddo ! j=lbound(rpnList,1), ubound(rpnList,1)

      if (sp /= 1) then
        write(0,*) 'j=', j, sp
        localrc = ESMF_RC_NOT_IMPL
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      includeList(1) = exportList(i,1)
      call MOSSCO_StateGet(exportState, fieldList, include=includeList, &
        fieldCount=fieldCount, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(fieldList(1), rank=rank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGetBounds(fieldList(1), exclusiveLBound=lbnd(1:rank), &
        exclusiveUbound=ubnd(1:rank), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (rank == 4) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr4, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        farrayPtr4(RANGE4D) = stack(sp)%farray4(RANGE4D)
        write(message,'(A,ES10.3)') trim(name)//' assigns to  '// &
          trim(includeList(1))//' value ', &
          sum(stack(sp)%farray4,stack(sp)%mask4)/count(stack(sp)%mask4)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        deallocate(stack(sp)%mask4,stack(sp)%farray4)
      elseif (rank == 3) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr3, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        farrayPtr3(RANGE3D) = stack(sp)%farray3(RANGE3D)
        write(message,'(A,ES10.3)') trim(name)//' assigns to  '// &
          trim(includeList(1))//' value ', &
          sum(stack(sp)%farray3,stack(sp)%mask3)/count(stack(sp)%mask3)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        deallocate(stack(sp)%mask3,stack(sp)%farray3)
      elseif (rank == 2) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr2, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        farrayPtr2(RANGE2D) = stack(sp)%farray2(RANGE2D)
        write(message,'(A,ES10.3)') trim(name)//' assigns to  '// &
          trim(includeList(1))//' value ', &
          sum(stack(sp)%farray2,stack(sp)%mask2)/count(stack(sp)%mask2)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        deallocate(stack(sp)%mask2,stack(sp)%farray2)
      elseif (rank == 1) then
        call ESMF_FieldGet(fieldList(1), farrayPtr=farrayPtr1, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        farrayPtr1(RANGE1D) = stack(sp)%farray1(RANGE1D)
        write(message,'(A,ES10.3)') trim(name)//' assigns to  '// &
          trim(includeList(1))//' value ', &
          sum(stack(sp)%farray1,stack(sp)%mask1)/count(stack(sp)%mask1)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        deallocate(stack(sp)%mask1,stack(sp)%farray1)
      endif
    enddo ! exportItems

    nullify(farrayPtr1)
    nullify(farrayPtr2)
    nullify(farrayPtr3)
    nullify(farrayPtr4)

    if (allocated(exportList)) deallocate(exportList)
    if (associated(includeList)) deallocate(includeList)
    if (allocated(reductionOperatorList)) deallocate(reductionOperatorList)
    if (allocated(unaryOperatorList)) deallocate(unaryOperatorList)
    if (allocated(binaryOperatorList)) deallocate(binaryOperatorList)
    if (allocated(scalarList)) deallocate(scalarList)
    if (allocated(matchIndex)) deallocate(matchIndex)
    if (allocated(stack)) deallocate(stack)
    if (allocated(rpnList)) deallocate(rpnList)
    if (allocated(aliasList)) deallocate(aliasList)
    if (allocated(itemTypeList)) deallocate(itemTypeList)
    if (allocated(itemNameList)) deallocate(itemNameList)

    !! Finally, log the successful completion of this function
    call MOSSCO_CompExit(cplComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Run


#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
subroutine Finalize(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)    :: cplComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    integer(ESMF_KIND_I4)   :: localrc
    logical                 :: isPresent
    type(ESMF_Config)       :: config

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, parentClock, name=name, currTime=currTime, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_CplCompGet(cplComp, configIsPresent=isPresent, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (isPresent) then

      call ESMF_CplCompGet(cplComp, config=config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_ConfigDestroy(config, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    end if

    call MOSSCO_CompExit(cplComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Finalize

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_CreateCalculatedExportFields"
  subroutine MOSSCO_CreateCalculatedExportFields(cplComp, importState, exportState, kwe, rc)

    type(ESMF_CplComp), intent(inout)                :: cplComp
    type(ESMF_State)                                 :: importState, exportState
    type(ESMF_KeywordEnforcer), optional, intent(in) :: kwe
    integer(ESMF_KIND_I4), optional, intent(out)     :: rc

    type(ESMF_Field), allocatable          :: importfieldList(:), exportFieldList(:), fieldList(:)
    character(ESMF_MAXSTR)                 :: message, itemName, name, operator
    integer(ESMF_KIND_I4)                  :: i, j, jj, rank
    integer(ESMF_KIND_I4)                  :: importFieldCount, exportFieldCount, fieldCount

    logical                                 :: isPresent, tagOnly_, isMatch
    character(len=ESMF_MAXSTR), allocatable :: filterExcludeList(:), filterIncludeList(:)
    character(len=ESMF_MAXSTR), allocatable :: checkExcludeList(:)

    real(ESMF_KIND_R8)                     :: offset, scale
    integer(ESMF_KIND_I4)                  :: localrc, rc_, matchIndex, matchScore
    integer(ESMF_KIND_I8)                  :: advanceCount
    type(ESMF_Clock)                       :: clock
    type(ESMF_Time)                        :: startTime, currTime
    type(ESMF_TimeInterval)                :: timeStep

    type(ESMF_FieldStatus_Flag)            :: fieldStatus
    type(ESMF_Grid)                        :: grid
    type(ESMF_Field)                       :: exportField

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    call ESMF_CplCompGet(cplComp, name=name, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_AttributeGet(cplComp, 'filter_pattern_include', filterIncludeList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    ! It is better to compare startTime with currTime in a connector, as it could be
    ! called multiple times for a single timeStep
    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, &
      timeStep=timeStep, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (currTime > startTime) then
      if (advanceCount < 1) advanceCount = nint((currTime - startTime) / timeStep)
      if (advanceCount < 1) advanceCount = 1
    else
      advanceCount = 0
    endif

    call MOSSCO_StateGetFieldList(importState, importFieldList, fieldCount=importFieldCount, &
      rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (importFieldCount < 1 .and. advanceCount < 2) then
      write(message,'(A)') trim(name)//' found no fields to reduce'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      if (present(rc)) rc=ESMF_SUCCESS
      return
    endif

    call MOSSCO_AttributeGet(cplComp, 'filter_pattern_include', filterIncludeList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_AttributeGet(cplComp, 'filter_pattern_exclude', filterExcludeList, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    do i=1, importFieldCount

      call ESMF_FieldGet(importFieldList(i), name=itemName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      ! Look for an exclusion pattern on this itemName
      if (allocated(filterExcludeList)) then
        do j=1,ubound(filterExcludeList,1)
          call MOSSCO_StringMatch(itemName, filterExcludeList(j), isMatch, localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (ismatch) exit
        enddo
        if (ismatch .and. advanceCount < 2) then
          write(message,'(A)')  trim(name)//' excluded item'
          call MOSSCO_MessageAdd(message, trim(itemName))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          cycle
        endif
      endif

      !! Look for an inclusion pattern on this field/bundle name
      if (allocated(filterIncludeList)) then
        do j=1,ubound(filterIncludeList,1)
          call MOSSCO_StringMatch(itemName, filterIncludeList(j), isMatch, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          if (ismatch) exit
        enddo
        if (.not.ismatch .and. advanceCount < 2) then
          write(message,'(A)')  trim(name)//' did not include'
          call MOSSCO_MessageAdd(message, ' '//trim(itemName))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          cycle
        endif
      endif

      !> @todo add later capability for field bundles, for now
      !> get a temporary fieldList with all items mathcing itemName
      call MOSSCO_StateGetFieldList(importState, fieldList, fieldCount=fieldCount, &
        itemSearch=trim(itemName), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_Reallocate(fieldList, 0,  rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldCount /= 1) cycle

      call ESMF_FieldGet(importFieldList(i), status=fieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) cycle

      !> Found out whether this field has a vertical dimension, if not, then cycle
      call ESMF_FieldGet(importFieldList(i), grid=grid, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_GridGet(grid, rank=rank, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (rank /= 3) cycle

      ! The field is created from gridset and complete fields, no error is thrown if
      ! the field exists
      call MOSSCO_StateGetFieldList(exportState, exportFieldList, fieldCount=exportFieldCount, &
        itemSearch='vred_'//trim(itemName), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (exportFieldCount < 1) then
        call MOSSCO_CreateCalculatedField(importFieldList(i), exportField, operator=operator, &
          scale=scale, offset=offset, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateAddReplace(exportState, (/exportField/), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      call MOSSCO_Reallocate(exportFieldList, 0,  rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    enddo

    call MOSSCO_Reallocate(importFieldList, 0,  rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_Reallocate(filterIncludeList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call MOSSCO_Reallocate(filterExcludeList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_CreateCalculatedExportFields

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ReduceFields"
  subroutine MOSSCO_ReduceFields(cplComp, importState, exportState, rc)

    type(ESMF_CplComp), intent(in)         :: cplComp
    type(ESMF_State)                       :: importState, exportState
    integer(ESMF_KIND_I4), optional        :: rc

    type(ESMF_Field), allocatable          :: importfieldList(:), exportFieldList(:)
    character(ESMF_MAXSTR)                 :: message, itemName, name, operator
    integer(ESMF_KIND_I4)                  :: i, j, importFieldCount, exportFieldCount

    logical                                 :: isPresent
    character(len=ESMF_MAXSTR), allocatable :: filterExcludeList(:), filterIncludeList(:)

    real(ESMF_KIND_R8)                     :: offset, scale
    integer(ESMF_KIND_I4)                  :: localrc, rc_
    integer(ESMF_KIND_I8)                  :: advanceCount
    type(ESMF_Clock)                       :: clock
    type(ESMF_Time)                        :: startTime, currTime
    type(ESMF_TimeInterval)                :: timeStep

    integer(ESMF_KIND_I4), allocatable     :: exportUbnd(:), exportLbnd(:)
    integer(ESMF_KIND_I4), allocatable     :: lbnd(:), ubnd(:)
    integer(ESMF_KIND_I4)                  :: exportRank, exportGridRank, importRank
    integer(ESMF_KIND_I4)                  :: importGridRank
    type(ESMF_Grid)                        :: importGrid, exportGrid
    type(ESMF_FieldStatus_Flag)            :: exportFieldStatus, importFieldStatus

    integer(ESMF_KIND_I4),dimension(:,:,:), pointer :: mask => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:) :: farrayPtr3 => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:)   :: farrayPtr2 => null()
    real(ESMF_KIND_R8), pointer, dimension(:,:,:)   :: depth_at_cell_face => null()
    real(ESMF_KIND_R8), allocatable, dimension(:,:,:)   :: weight, layer_height
    character(len=ESMF_MAXSTR)             :: importItemName
    type(ESMF_TypeKind_Flag)               :: typeKind

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    call ESMF_CplCompGet(cplComp, name=name, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_AttributeGet(cplComp, 'add_offset', value=offset, defaultValue=0.0D0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_CplCompGet(cplComp, clock=clock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    ! It is better to compare startTime with currTime in a connector, as it could be
    ! called multiple times for a single timeStep
    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, &
      timeStep=timeStep, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (currTime > startTime) then
      if (advanceCount < 1) advanceCount = nint((currTime - startTime) / timeStep)
      if (advanceCount < 1) advanceCount = 1
    else
      advanceCount = 0
    endif

    call MOSSCO_StateGetFieldList(exportState, exportFieldList, fieldCount=exportFieldCount, &
      rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (exportFieldCount < 1 .and. advanceCount < 2) then
      write(message,'(A)') trim(name)//' found no fields to reduce'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      if (present(rc)) rc=ESMF_SUCCESS
      return
    endif

    do i=1, exportFieldCount

      call ESMF_FieldGet(exportFieldList(i), name=itemName, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (itemName(1:5) /= 'vred_') cycle
      importItemName = itemName(6:len_trim(itemName))

      !> Find the name of the variable on which the reduction shall be applied
      !call ESMF_AttributeGet(exportFieldList(i), name='source', isPresent=isPresent, rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !if (.not.isPresent()) cycle

      !call ESMF_AttributeGet(exportFieldList(i), name='source', value=importItemName, rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_StateGet(importState, importFieldList, fieldCount=importFieldCount, &
        itemSearch=trim(importItemName), fieldStatusList=(/ESMF_FIELDSTATUS_COMPLETE/), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !call ESMF_LogWrite(trim(name)//' in loop'//trim(importItemName), ESMF_LOGMSG_INFO)

      !> if not found, or if multiple fields with the same name, then skip this
      !> @todo add later capability for field bundles
      if (importFieldCount /= 1) cycle

      !call ESMF_LogWrite(trim(name)//' importfielCount', ESMF_LOGMSG_INFO)

      !> Complete the field if it is gridset
      call ESMF_FieldGet(exportFieldList(i), status=exportFieldStatus,rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (exportfieldStatus /= ESMF_FIELDSTATUS_COMPLETE) cycle

      call ESMF_FieldGet(exportFieldList(i), grid=exportGrid, rank=exportRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_FieldGet(importFieldList(1), grid=importGrid, rank=importRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_GridGet(exportGrid, rank=exportGridRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_GridGet(importGrid, rank=importGridRank, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (.not.importRank == 3) cycle
      allocate(lbnd(3), stat=localrc)
      allocate(ubnd(3), stat=localrc)

      if (.not.importGridRank == 3) cycle

      call ESMF_FieldGet(importFieldList(1),  localDe=0, farrayPtr=farrayPtr3, exclusiveLbound=lbnd, &
        exclusiveUbound=ubnd, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> Get the 3D mask from the grid
      call ESMF_GridGetItem(importGrid, ESMF_GRIDITEM_MASK, farrayPtr=mask, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> Predefine the weight for vertically summing of layers as 1
      if (allocated(layer_height)) deallocate(layer_height)
      allocate(layer_height(RANGE3D), stat=localrc)
      layer_height(RANGE3D) = 0.0
      where(mask(RANGE3D) > 0)
        layer_height(RANGE3D) = 1.0
      endwhere

      !> Get the vertical (3rd coordinate) layer vface depths to calculate the weights
      !> for vertical averaging
      !> @todo what happens if this coordinate info is not present?
      call ESMF_GridGetCoord(importGrid, coordDim=3, staggerloc=ESMF_STAGGERLOC_CENTER_VFACE, &
        farrayPtr=depth_at_cell_face, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (associated(depth_at_cell_face)) then
        layer_height(RANGE3D) = depth_at_cell_face(RANGE3D)! -  depth_at_cell_face(RANGE2D, lbnd(3)-1:ubnd(3)-1)
      endif

      if (allocated(weight)) deallocate(weight)
      allocate(weight(RANGE3D), stat=localrc)
      weight(RANGE3D) = 0.0

      do j = lbnd(3), ubnd(3)
        where ( mask(RANGE2D,j) > 0 .and. layer_height(RANGE2D,j) > 0)
          weight(RANGE2D,j) = layer_height(RANGE2D,j)/sum(layer_height(RANGE3D), dim=3)
        endwhere
      enddo

      if (exportGridRank == 2 .and. exportRank == 2) then

        allocate(exportLbnd(2), stat=localrc)
        allocate(exportUbnd(2), stat=localrc)

        call ESMF_FieldGet(exportFieldList(i), localDe=0, farrayPtr=farrayPtr2, exclusiveLbound=exportLbnd, &
          exclusiveUbound=exportUbnd, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (any(ubnd(1:2) - lbnd(1:2) /= exportUbnd(1:2) - exportLbnd(1:2))) cycle

        select case(trim(operator))
        case ('total')
          farrayPtr2(exportLbnd(1):exportUbnd(1),exportLbnd(2):exportUbnd(2)) = &
            sum(farrayPtr3(RANGE3D) * layer_height(RANGE3D), dim=3, mask=(mask(RANGE3D)>0))
        case ('average')
          farrayPtr2(exportLbnd(1):exportUbnd(1),exportLbnd(2):exportUbnd(2)) = &
            sum(farrayPtr3(RANGE3D) * weight(RANGE3D), dim=3, mask=(mask(RANGE3D)>0))
        case ('minimum' )
          farrayPtr2(exportLbnd(1):exportUbnd(1),exportLbnd(2):exportUbnd(2)) = &
            minval(farrayPtr3(RANGE3D) * weight(RANGE3D), dim=3, mask=(mask(RANGE3D)>0))
        case ('maximum' )
          farrayPtr2(exportLbnd(1):exportUbnd(1),exportLbnd(2):exportUbnd(2)) = &
            maxval(farrayPtr3(RANGE3D) * weight(RANGE3D), dim=3, mask=(mask(RANGE3D)>0))
        !case ('norm', 'product')
        case default
          rc = ESMF_RC_NOT_IMPL
          call ESMF_LogWrite(trim(name)//' operator '//trim(operator)//' not implemented', ESMF_LOGMSG_ERROR)
          return
        end select
      else
        if (advanceCount < 2) then
          write(message, '(A)') trim(name)//' could not reduce non-rank 3 item '//trim(itemName)
        endif
      endif

      if (advanceCount < 2) then
        write(message,'(A)') trim(name)//' reduced '
        call MOSSCO_FieldString(importFieldList(1), message)
        call MOSSCO_MessageAdd(message,' to ')
        call MOSSCO_FieldString(exportFieldList(i), message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif

      if (allocated(layer_height)) deallocate(layer_height)
      if (allocated(weight)) deallocate(weight)
      nullify(depth_at_cell_face)
      nullify(mask)
      nullify(farrayPtr3)
      nullify(farrayPtr2)

      call MOSSCO_Reallocate(importFieldList, 0,  rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      deallocate(exportLbnd, stat=localrc)
      deallocate(exportUbnd, stat=localrc)
      deallocate(lbnd, stat=localrc)
      deallocate(lbnd, stat=localrc)

    enddo

    call MOSSCO_Reallocate(exportFieldList, 0,  rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_ReduceFields

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_CreateCalculatedField"
  subroutine MOSSCO_CreateCalculatedField(importField, exportfield, kwe, &
    scale, offset, operator, rc)

    type(ESMF_Field), intent(in)           :: importField
    type(ESMF_Field), intent(out)          :: exportField
    type(ESMF_KeywordEnforcer), optional   :: kwe
    real(ESMF_KIND_R8), optional, intent(in) :: scale, offset
    character(len=*), optional, intent(in) :: operator
    integer(ESMF_KIND_I4), optional        :: rc

    character(ESMF_MAXSTR)                 :: exportName, importName

    integer(ESMF_KIND_I4)                  :: localrc, rc_, rank
    real(ESMF_KIND_R8)                     :: scale_, offset_
    character(len=ESMF_MAXSTR)             :: operator_
    type(ESMF_FieldStatus_Flag)            :: fieldStatus
    type(ESMF_Grid)                        :: importGrid, exportGrid
    type(ESMF_TypeKind_Flag)               :: typeKind

    rc_ = ESMF_SUCCESS
    if (present(rc))  rc = rc_
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(scale)) scale_ = scale
    if (present(offset)) offset_ = offset
    if (present(operator)) operator_ = operator

    call ESMF_FieldGet(importField, name=importName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(importField, status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) return

    call ESMF_FieldGet(importField, grid=importGrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridGet(importGrid, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (rank /= 3) return

    exportGrid = MOSSCO_GridCreateFromOtherGrid(importGrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    exportName = 'vred_' // trim(importName)

    exportField = ESMF_FieldEmptyCreate(name=trim(exportName), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldEmptySet(exportField, exportGrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(importField, typeKind=typeKind, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldEmptyComplete(exportfield, typeKind=typeKind, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_FieldInitialize(exportField, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine MOSSCO_CreateCalculatedField

end module calculator
