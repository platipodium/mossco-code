!> @brief Implementation of ESMF State utilities
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2015, 2016, 2017 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!> @author Richard Hofmeister <richard.hofmeister@hzg.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#define ESMF_FILENAME "mossco_state.F90"

#define RANGE3D lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)
#define RANGE2D lbnd(1):ubnd(1),lbnd(2):ubnd(2)

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module mossco_state

use esmf
use mossco_field
use mossco_strings
use mossco_memory

implicit none

interface MOSSCO_StateGet
    module procedure MOSSCO_StateGetF1
    module procedure MOSSCO_StateGetF2
    module procedure MOSSCO_StateGetF3
    module procedure MOSSCO_StateGetFieldList
    module procedure MOSSCO_StateGetFieldsList
end interface

interface mossco_state_get
    module procedure MOSSCO_StateGetF1
    module procedure MOSSCO_StateGetF2
    module procedure MOSSCO_StateGetF3
end interface

interface MOSSCO_DestroyOwn
  module procedure MOSSCO_StateDestroyOwn
  module procedure MOSSCO_RouteHandleDestroyOwn
  module procedure MOSSCO_FieldDestroyOwn
  module procedure MOSSCO_FieldBundleDestroyOwn
  module procedure MOSSCO_ArrayDestroyOwn
  module procedure MOSSCO_ArrayBundleDestroyOwn
end interface

!#include "git-sha.h" !>@todo

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateGetF1"
  subroutine MOSSCO_StateGetF1(state, fieldNameList, farrayPtr, kwe, lbnd, ubnd, verbose, rc)

    type(ESMF_State), intent(in)                 :: state
    character(len=*), dimension(:), intent(in)   :: fieldNameList
    real(ESMF_KIND_R8), pointer,dimension(:), intent(inout) :: farrayPtr
    type(ESMF_KeywordEnforcer), optional         :: kwe !keyword-enforcerga
    integer(ESMF_KIND_I4), intent(out),optional  :: ubnd(1),lbnd(1)
    logical, intent(in), optional                :: verbose
    integer, intent(out), optional               :: rc

    integer(ESMF_KIND_I4)         :: ubnd_(1),lbnd_(1), rc_
    type(ESMF_Field)              :: field
    type(ESMF_FieldBundle)        :: fieldBundle
    integer(ESMF_KIND_I4)         :: localrc,i, fieldCount
    type(ESMF_StateItem_Flag)     :: itemType
    type(ESMF_Field), allocatable :: fieldList(:)
    logical                       :: isPresent, verbose_
    character(len=ESMF_MAXPATHLEN)    :: message, name
    type(ESMF_FieldStatus_Flag)   :: fieldStatus

    verbose_=.false.
    rc_=ESMF_SUCCESS
    ubnd_(:)=-1
    lbnd_(:)=0
    nullify(farrayPtr)
    if (present(verbose)) verbose_=verbose

    call ESMF_StateGet(state, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1,size(fieldNameList,1)
      call ESMF_StateGet(state, trim(fieldNameList(i)), itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemtype == ESMF_STATEITEM_FIELDBUNDLE) then

         call ESMF_StateGet(state,trim(fieldNameList(i)),fieldBundle,rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         call ESMF_FieldBundleGet(fieldBundle,fieldName=trim(fieldNameList(i)), &
           isPresent=isPresent, fieldCount=fieldCount, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         if (.not.isPresent) cycle
         if (fieldCount /= 1) cycle

         allocate(fieldList(fieldCount))

         call ESMF_FieldBundleGet(fieldBundle, trim(fieldNameList(i)), fieldList=fieldList, rc=localRc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        field=fieldList(1)

         deallocate(fieldList)

      elseif (itemtype == ESMF_STATEITEM_FIELD) then

         call ESMF_StateGet(state,trim(fieldNameList(i)),field,rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        cycle
      endif

      call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
         call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) cycle

      call ESMF_FieldGet(field,localde=0,farrayPtr=farrayPtr,exclusiveUBound=ubnd_, exclusiveLBound=lbnd_,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      exit
    end do

    if (verbose_) then
      if (associated(farrayPtr)) then
        write(message, '(A)') '  found field '//trim(fieldNameList(i))//' in '//trim(name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      else
        write(message, '(A)') '  did not find in '//trim(name)//' any of those field(s):'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        do i=1,size(fieldNameList,1)
          write(message, '(A)') '   - '//trim(fieldNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        end do
      endif
    endif

    if (.not.associated(farrayPtr)) rc_ = ESMF_RC_NOT_FOUND

    if (present(rc)) rc = rc_
    if (present(ubnd)) ubnd=ubnd_
    if (present(lbnd)) lbnd=lbnd_

  end subroutine MOSSCO_StateGetF1

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateGetF2"
  subroutine MOSSCO_StateGetF2(state, fieldNameList, farrayPtr, kwe, lbnd, ubnd, verbose, rc)

    type(ESMF_State), intent(in)                 :: state
    character(len=*), dimension(:), intent(in)    :: fieldNameList
    real(ESMF_KIND_R8), pointer,dimension(:,:), intent(inout) :: farrayPtr

    logical, intent(in ), optional               :: kwe !keyword-enforcer
    integer(ESMF_KIND_I4), intent(out),optional  :: ubnd(2),lbnd(2)
    logical, intent(in), optional                :: verbose
    integer, intent(out), optional               :: rc

    integer(ESMF_KIND_I4)         :: ubnd_(2),lbnd_(2), rc_
    type(ESMF_Field)              :: field
    type(ESMF_FieldBundle)        :: fieldBundle
    integer(ESMF_KIND_I4)         :: localrc, i, fieldCount
    type(ESMF_StateItem_Flag)     :: itemType
    type(ESMF_Field), allocatable :: fieldList(:)
    logical                       :: isPresent, verbose_
    character(len=ESMF_MAXPATHLEN):: message, name
    type(ESMF_FieldStatus_Flag)   :: fieldStatus

    verbose_ = .false.
    rc_ = ESMF_SUCCESS
    ubnd_(:) = -1
    lbnd_(:) = 0
    nullify(farrayPtr)
    if (present(verbose)) verbose_=verbose

    call ESMF_StateGet(state, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1,size(fieldNameList,1)
      call ESMF_StateGet(state, trim(fieldNameList(i)), itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemtype == ESMF_STATEITEM_FIELDBUNDLE) then

         call ESMF_StateGet(state,trim(fieldNameList(i)),fieldBundle,rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         call ESMF_FieldBundleGet(fieldBundle,fieldName=trim(fieldNameList(i)), isPresent=isPresent, fieldCount=fieldCount, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         if (.not.isPresent) cycle
         if (fieldCount /= 1) cycle

         allocate(fieldList(fieldCount))

         call ESMF_FieldBundleGet(fieldBundle, trim(fieldNameList(i)), fieldList=fieldList, rc=localRc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        field=fieldList(1)

         deallocate(fieldList)

      elseif (itemtype == ESMF_STATEITEM_FIELD) then

         call ESMF_StateGet(state,trim(fieldNameList(i)),field,rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        cycle
      endif

      call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
         call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) cycle

      call ESMF_FieldGet(field,localde=0,farrayPtr=farrayPtr,exclusiveUBound=ubnd_, exclusiveLBound=lbnd_,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      exit
    end do

    if (verbose_) then
      if (associated(farrayPtr)) then
        write(message, '(A)') '  found field '//trim(fieldNameList(i))//' in '//trim(name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      else
        write(message, '(A)') '  did not find in '//trim(name)//' any of those field(s):'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        do i=1,size(fieldNameList,1)
          write(message, '(A)') '   - '//trim(fieldNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        end do
      endif
    endif

    if (.not.associated(farrayPtr)) rc_ = ESMF_RC_NOT_FOUND

    if (present(rc)) rc = rc_
    if (present(ubnd)) ubnd=ubnd_
    if (present(lbnd)) lbnd=lbnd_

  end subroutine MOSSCO_StateGetF2

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateGetF3"
  subroutine MOSSCO_StateGetF3(state, fieldNameList, farrayPtr, kwe, lbnd, ubnd, verbose, rc)

    type(ESMF_State), intent(in)                 :: state
    character(len=*), dimension(:), intent(in)    :: fieldNameList
    real(ESMF_KIND_R8), pointer,dimension(:,:,:), intent(inout) :: farrayPtr

    logical, intent(in ), optional               :: kwe !keyword-enforcer
    integer(ESMF_KIND_I4), intent(out),optional  :: ubnd(3),lbnd(3)
    logical, intent(in), optional                :: verbose
    integer, intent(out), optional               :: rc

    integer(ESMF_KIND_I4)         :: ubnd_(3),lbnd_(3), rc_
    type(ESMF_Field)              :: field
    type(ESMF_FieldBundle)        :: fieldBundle
    integer(ESMF_KIND_I4)         :: localrc,i, fieldCount
    type(ESMF_StateItem_Flag)     :: itemType
    type(ESMF_Field), allocatable :: fieldList(:)
    logical                       :: isPresent, verbose_
    character(len=ESMF_MAXPATHLEN)    :: message, name
    type(ESMF_FieldStatus_Flag)   :: fieldStatus

    verbose_=.false.
    rc_=ESMF_SUCCESS
    ubnd_(:)=-1
    lbnd_(:)=0
    nullify(farrayPtr)
    if (present(verbose)) verbose_=verbose

    call ESMF_StateGet(state, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1,size(fieldNameList,1)
      call ESMF_StateGet(state, trim(fieldNameList(i)), itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemtype == ESMF_STATEITEM_FIELDBUNDLE) then

         call ESMF_StateGet(state,trim(fieldNameList(i)),fieldBundle,rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         call ESMF_FieldBundleGet(fieldBundle,fieldName=trim(fieldNameList(i)), isPresent=isPresent, fieldCount=fieldCount, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

         if (.not.isPresent) cycle
         if (fieldCount /= 1) cycle

         allocate(fieldList(fieldCount))

         call ESMF_FieldBundleGet(fieldBundle, trim(fieldNameList(i)), fieldList=fieldList, rc=localRc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        field=fieldList(1)

         deallocate(fieldList)

      elseif (itemtype == ESMF_STATEITEM_FIELD) then

         call ESMF_StateGet(state,trim(fieldNameList(i)),field,rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
           call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      else
        cycle
      endif

      call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
         call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) cycle

      call ESMF_FieldGet(field,localde=0,farrayPtr=farrayPtr,exclusiveUBound=ubnd_, exclusiveLBound=lbnd_,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      exit
    end do

    if (verbose_) then
      if (associated(farrayPtr)) then
        write(message, '(A)') '  found field '//trim(fieldNameList(i))//' in '//trim(name)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      else
        write(message, '(A)') '  did not find in '//trim(name)//' any of those field(s):'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        do i=1,size(fieldNameList,1)
          write(message, '(A)') '   - '//trim(fieldNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        end do
      endif
    endif

    if (.not.associated(farrayPtr)) rc_ = ESMF_RC_NOT_FOUND

    if (present(rc)) rc = rc_
    if (present(ubnd)) ubnd=ubnd_
    if (present(lbnd)) lbnd=lbnd_

  end subroutine MOSSCO_StateGetF3


  !> set ESMF attributes "required_flag", "required" and "optional" for
  !! an item's name in the importState
#undef  ESMF_METHOD
#define ESMF_METHOD "set_item_flags"
  subroutine set_item_flags(state, name, requiredFlag, optionalFlag, requiredRank)

    type(ESMF_State)           :: state
    character(len=*)           :: name
    integer, optional          :: requiredRank
    logical, optional          :: requiredFlag,optionalFlag

    integer                    :: localrc, rc_
    character(len=ESMF_MAXSTR) :: attname

    if (present(requiredFlag)) then
      attname=trim(name)//':required'
      call ESMF_AttributeSet(state,name=attname,value=requiredFlag,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end if
    if (present(requiredRank)) then
      attname=trim(name)//':required_rank'
      call ESMF_AttributeSet(state,name=attname,value=requiredRank,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end if
    if (present(optionalFlag)) then
      attname=trim(name)//':optional'
      call ESMF_AttributeSet(state,name=attname,value=optionalFlag, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end if
  end subroutine set_item_flags

  !> create fields from required attributes in a state
#undef  ESMF_METHOD
#define ESMF_METHOD "create_required_fields"
  subroutine create_required_fields(state, grid)
  type(ESMF_State), intent(inout)   :: state
  type(ESMF_Grid),  intent(in)      :: grid
  type(ESMF_Field)                  :: field
  type(ESMF_Typekind_Flag)          :: typeKind=ESMF_TYPEKIND_R8
  type(ESMF_StateItem_Flag)         :: itemFlag
  integer                           :: n, rc_, idx, attCount
  logical                           :: required
  character(len=ESMF_MAXPATHLEN)    :: attName,fieldName
  integer                           :: localrc

  !> get Attribute list
  call ESMF_AttributeGet(state, attCount, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  do n=1,attCount
    call ESMF_AttributeGet(state,attributeIndex=n,name=attName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> check for ":required"
    idx = index(attName,':required ')
    if (idx>0) then
      call ESMF_AttributeGet(state,name=attName,value=required,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (required) then
        fieldName=attName(1:idx-1)
      else
        !write(0,*) 'found attribute ',trim(attName),', but not required'
        cycle
      end if
    else
      !write(0,*) 'attribute not relevant: ',trim(attName)
      cycle
    end if
    !> check for fieldName in state
    call ESMF_StateGet(state,fieldName,itemFlag,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemFlag == ESMF_STATEITEM_NOTFOUND) then
      !> create field
      field = ESMF_FieldCreate(grid,typekind=typeKind,name=fieldName,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> append field to state
      call ESMF_StateAdd(state,(/ field /),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      !write(0,*) 'item ',trim(fieldName),' already present',itemFlag
    end if
  end do
  end subroutine create_required_fields

  !> create optional fields from available names and optional attributes in a state
#undef  ESMF_METHOD
#define ESMF_METHOD "create_optional_fields_from_names"
  subroutine create_optional_fields_from_names(state, names, grid)

  type(ESMF_State), intent(inout)   :: state
  type(ESMF_Grid),  intent(in)      :: grid
  character(len=*),dimension(:)     :: names
  type(ESMF_Field)                  :: field
  type(ESMF_Typekind_Flag)          :: typeKind=ESMF_TYPEKIND_R8
  type(ESMF_StateItem_Flag)         :: itemFlag
  integer                           :: n,rc,idx,attCount
  logical                           :: optional
  character(len=ESMF_MAXPATHLEN)            :: attName,potentialFieldName
  integer                           :: localrc, rc_

  !> get Attribute list
  call ESMF_AttributeGet(state,attCount,rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  do n=1,attCount
    call ESMF_AttributeGet(state,attributeIndex=n,name=attName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !> check for ":optional"
    idx = index(attName,':optional ')
    if (idx>0) then
      call ESMF_AttributeGet(state,name=attName,value=optional,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (optional) then
        potentialFieldName=attName(1:idx-1)
      else
        !write(0,*) 'found attribute ',trim(attName),', but not optional'
        cycle
      end if
    else
      !write(0,*) 'attribute not relevant: ',trim(attName)
      cycle
    end if

    !> check for available names == potentialFieldName
    do idx=1,ubound(names,1)
      if (trim(names(idx))==trim(potentialFieldName)) then
        !> check for fieldName in state
        call ESMF_StateGet(state,potentialFieldName,itemFlag,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (itemFlag == ESMF_STATEITEM_NOTFOUND) then
          !> create field
          field = ESMF_FieldCreate(grid,typekind=typeKind,name=potentialFieldName,rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          !> append field to state
         call ESMF_StateAdd(state,(/ field /),rc=rc)
        else
          !write(0,*) 'item ',trim(potentialFieldName),' already present',itemFlag
        end if
        exit
      else
        !write(0,*) 'name ',trim(names(idx)),' not matching optional field ',trim(potentialFieldName)
        cycle
      end if
    end do
  end do
  end subroutine create_optional_fields_from_names

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateLog"
  recursive subroutine MOSSCO_StateLog(state, kwe, deep, log, rc)
    type(ESMF_State)                :: state
    type(ESMF_KeywordEnforcer),optional :: kwe
    logical, intent(in), optional   :: deep
    type(ESMF_Log), optional        :: log
    integer(ESMF_KIND_I4), optional :: rc

    integer(ESMF_KIND_I4)           :: localRc, itemCount, i, rank, j, rc_
    integer(ESMF_KIND_I4)           :: attributeCount, maxDigits, count, fieldCount
    character(len=ESMF_MAXPATHLEN)  :: string, message
    character(len=ESMF_MAXSTR)      :: fieldName, name, gridName, attributeName
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:), fieldNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    type(ESMF_Field), allocatable   :: fieldList(:)
    type(ESMF_Field)                :: field
    type(ESMF_FieldBundle)          :: fieldBundle
    integer(ESMF_KIND_I4)           :: totalLWidth(7), totalUWidth(7)
    type(ESMF_Grid)                 :: grid
    logical                         :: isPresent, isNeeded, deep_
    type(ESMF_LocStream)            :: locStream
    type(ESMF_TypeKind_Flag)        :: typeKind
    logical, allocatable            :: logicalValueList(:)
    real(kind=ESMF_KIND_R4), allocatable    :: real4ValueList(:)
    real(kind=ESMF_KIND_R8), allocatable    :: real8ValueList(:)
    integer(kind=ESMF_KIND_I4), allocatable :: integer4ValueList(:)
    integer(kind=ESMF_KIND_I8), allocatable :: integer8ValueList(:)
    character(len=4096)       , allocatable :: characterValueList(:)
    type(ESMF_State)                        :: childState
    character(len=10)                       :: format

    if (present(rc)) rc = ESMF_SUCCESS
    if (present(kwe)) localrc = ESMF_SUCCESS
    deep_ = .false.
    if (present(deep)) deep_ = deep

    call ESMF_StateGet(state, name=name, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(state, itemCount=itemCount, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(state, count=attributeCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount == 0 .and. attributeCount == 0) then
      write(message,'(A)')  'state '//trim(name)//' contains neither attributes nor items'
      if (present(log)) then
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
      else
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif
    write(message, '(A)') 'state '//trim(name)
    if (present(log)) then
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
    else
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    do i=1, attributeCount
      call ESMF_AttributeGet(state, attributeIndex=i , name=attributeName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)')  trim(name)//':'
      call MOSSCO_MessageAdd(message,trim(attributeName)//' =')

      call ESMF_AttributeGet(state, name=attributeName, typekind=typekind,  itemCount=itemCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (typekind==ESMF_TYPEKIND_Logical) then
        call MOSSCO_MessageAdd(message, ' (L)')
        allocate(logicalValueList(itemCount))
        call ESMF_AttributeGet(state, name=attributeName, valueList=logicalValueList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,L)') trim(message)//' ',logicalValueList(1)
        do j=2, itemCount-1
          write(string,'(A,L)') ', ',logicalValueList(j)
          call MOSSCO_MessageAdd(message,', '//trim(string))
        enddo
        deallocate(logicalValueList)
      elseif (typekind==ESMF_TYPEKIND_CHARACTER) then
        if (allocated(characterValueList)) deallocate(characterValueList)
        allocate(characterValueList(itemCount))
        call ESMF_AttributeGet(state, name=attributeName, valueList=characterValueList, rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
          !(ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) then
          !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          !>@ todo: how to deal with very long attributes that don't fit into valueList?
          rc=ESMF_SUCCESS
        endif

        if (len_trim(message) + len_trim(characterValueList(1)) + 1 <= len(message)) then
          write(message,'(A)') trim(message)//' "'//trim(characterValueList(1))//'"'
          do j=2, itemCount-1
            write(string,'(A,A)') ', "',trim(characterValueList(j))//'"'
            call MOSSCO_MessageAdd(message,', '//trim(string))
          enddo
        endif
        if (allocated(characterValueList)) deallocate(characterValueList)
      elseif (typekind==ESMF_TYPEKIND_I4) then
        call MOSSCO_MessageAdd(message, ' (I4)')
        allocate(integer4ValueList(itemCount))
        call ESMF_AttributeGet(state, name=attributeName, valueList=integer4ValueList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(format,'(A)') '('//trim(intformat(integer4ValueList(1)))//')'
        write(string,format) integer4ValueList(1)
        call MOSSCO_MessageAdd(message,', '//trim(string))

        do j=2, itemCount-1
          write(format,'(A)') '(A,'//trim(intformat(integer4ValueList(j)))//')'
          write(string,format) ', ',integer4ValueList(j)
          call MOSSCO_MessageAdd(message,', '//trim(string))
        enddo
        deallocate(integer4ValueList)

      elseif (typekind==ESMF_TYPEKIND_I8) then
        call MOSSCO_MessageAdd(message, ' (I8)')
        allocate(integer8ValueList(itemCount))
        call ESMF_AttributeGet(state, name=attributeName, valueList=integer8ValueList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(format,'(A)') '('//trim(intformat(integer8ValueList(1)))//')'
        write(string,format) integer8ValueList(1)
        call MOSSCO_MessageAdd(message,', '//trim(string))

        do j=2, itemCount-1
          write(format,'(A)') '('//trim(intformat(integer8ValueList(j)))//')'
          write(string,format) integer8ValueList(j)
          call MOSSCO_MessageAdd(message,', '//trim(string))
        enddo
        deallocate(integer8ValueList)

      elseif (typekind==ESMF_TYPEKIND_R4) then
        call MOSSCO_MessageAdd(message, ' (R4)')
        allocate(real4ValueList(itemCount))
        call ESMF_AttributeGet(state, name=attributeName, valueList=real4ValueList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(string,'(ES9.2)') real4ValueList(1)
        call MOSSCO_MessageAdd(message,', '//trim(string))
        do j=2, itemCount-1
          write(string,'(A,ES9.2)') ', ',real4ValueList(j)
          call MOSSCO_MessageAdd(message,', '//trim(string))
        enddo
        deallocate(real4ValueList)

      elseif (typekind==ESMF_TYPEKIND_R8) then
        call MOSSCO_MessageAdd(message, ' (R8)')
        allocate(real8ValueList(itemCount))
        call ESMF_AttributeGet(state, name=attributeName, valueList=real8ValueList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(string,'(ES9.2)') real8ValueList(1)
        call MOSSCO_MessageAdd(message,', '//trim(string))
        do j=2, itemCount-1
          write(string,'(A,ES9.2)') ', ',real8ValueList(j)
          call MOSSCO_MessageAdd(message,', '//trim(string))
        enddo
        deallocate(real8ValueList)
      else
        write(message, '(A)') '  unknown typeKind encountered for attribute '//trim(attributeName)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
      if (present(log)) then
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
      else
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    enddo

    call ESMF_StateGet(state, itemCount=itemCount, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemTypeList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount > 0) then
      call ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=itemNameList, &
        rc=localRc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    do i=1,itemCount

      if (itemtypeList(i) == ESMF_STATEITEM_FIELD) then

        call ESMF_StateGet(state, itemNameList(i), field, rc=localrc)

        !> @todo the following ccycle statement should not be necessary (but we
        !have incidents on jureca
        if (localrc /= ESMF_SUCCESS) cycle

        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (deep_) then
          if (present(log)) then
            call MOSSCO_FieldLog(field, log=log, prefix=trim(name)//':', rc=localrc)
          else
            call MOSSCO_FieldLog(field, prefix=trim(name)//':', rc=localrc)
          endif
        else
          write(message,'(A)')  trim(name)//' field'
          call MOSSCO_FieldString(field, message, rc=localrc)

          if (present(log)) then
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
          else
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          endif
        endif

      elseif (itemtypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
        call ESMF_StateGet(state, itemNameList(i), fieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (fieldCount < 1) cycle

        call MOSSCO_Reallocate(fieldList, fieldCount, keep=.false., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_Reallocate(fieldNameList, fieldCount, keep=.false., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldBundleGet(fieldBundle, fieldNameList=fieldNameList, &
          fieldList=fieldList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (deep_) then
          do j=1, fieldCount
            call MOSSCO_FieldLog(fieldList(j), prefix=trim(name)//':'//trim(itemNameList(i))//':', rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            if (present(log)) then
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
            else
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
            endif
          enddo
        else
          do j=1, fieldCount
            write(message,'(A)')  trim(name)//' field '//trim(itemNameList(i))//':'
            call MOSSCO_FieldString(fieldList(j), message)
            if (present(log)) then
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
            else
              call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
            endif
          enddo
          deallocate(fieldList,fieldNameList)
        endif

        call MOSSCO_Reallocate(fieldList, 0, keep=.false., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_Reallocate(fieldNameList, 0, keep=.false., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      elseif (itemTypeList(i) == ESMF_STATEITEM_STATE) then

        write(message,'(A)')  trim(name)//' state  '//trim(itemNameList(i))
        call MOSSCO_MessageAdd(message, ' contains itself')
        if (present(log)) then
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING, log=log)
        else
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
        endif
        !call ESMF_StateGet(state, itemNameList(i), childState, rc=localrc)
        !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !if (present(log)) then
        !  call MOSSCO_StateLog(state, deep=deep_, log=log, rc=localrc)
        !else
        !  call MOSSCO_StateLog(state, deep=deep_, rc=localrc)
        !endif
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      elseif (itemTypeList(i) == ESMF_STATEITEM_ARRAY) then
        write(message,'(A)')  trim(name)//' array  '//trim(itemNameList(i))
        if (present(log)) then
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
        else
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif

      elseif (itemTypeList(i) == ESMF_STATEITEM_ARRAYBUNDLE) then
        write(message,'(A)')  trim(name)//' arrayBundle  '//trim(itemNameList(i))
        if (present(log)) then
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
        else
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif
      else
        write(message,'(A)')  trim(name)//' unknown item  '//trim(itemNameList(i))
        if (present(log)) then
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
        else
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif
      endif

      !call ESMF_LocStreamGet(locStream, name=gridName, rc=localRc)
      !if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      !maxDigits=1
      !do j=1,rank
      !  if (log10(totalUWidth(j)*1.0)>maxDigits) maxDigits=ceiling(log10(totalUWidth(j)*1.0))
      !enddo

      !write(string,'(A,I1,A,I1)') 'A,',rank,'I',maxDigits
      !write(message,string)  trim(name)//' field '//trim(fieldName)//' [',totalUWidth
    enddo

    call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemTypeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine MOSSCO_StateLog

!#undef  ESMF_METHOD
!#define ESMF_METHOD "MOSSCO_StateAttributeString"
!  subroutine MOSSCO_StateAttributeString(state, message , length, rc)
!
!    use mossco_strings
!    implicit none
!
!    type(ESMF_State), intent(in)                    :: state
!    character(len=ESMF_MAXPATHLEN), intent(inout)      :: message
!    integer(ESMF_KIND_I4), intent(inout), optional :: length
!    integer(ESMF_KIND_I4), intent(out), optional   :: rc
!
!    type(ESMF_TypeKind_Flag)        :: typeKind
!    logical, allocatable            :: logicalValueList(:)
!    real(kind=ESMF_KIND_R4), allocatable    :: real4ValueList(:)
!    real(kind=ESMF_KIND_R8), allocatable    :: real8ValueList(:)
!    integer(kind=ESMF_KIND_I4), allocatable :: integer4ValueList(:)
!    integer(kind=ESMF_KIND_I8), allocatable :: integer8ValueList(:)
!    character(len=ESMF_MAXPATHLEN), allocatable :: characterValueList(:)
!
!    integer(ESMF_KIND_I4)   :: rank, localrc, count, i, j, itemCount
!    character(len=ESMF_MAXPATHLEN)  :: attributeName
!
!    call ESMF_AttributeGet(state, count=count, rc=localrc)
!    if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!
!    do i=1, count
!      call ESMF_AttributeGet(state, attributeIndex=count-1 , name=attributeName, rc=localrc)
!      if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!      if (len_trim(message) + len_trim(attributeName) + 2 <= len(message)) then
!        write(message,'(A)')  trim(message)//' '//trim(attributeName)//':'
!
!      call ESMF_AttributeGet(state, name=attributeName, typekind=typekind,  itemCount=itemCount, rc=localrc)
!      if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!      if (typekind==ESMF_TYPEKIND_Logical) then
!        allocate(logicalValueList(itemCount))
!        call ESMF_AttributeGet(state, name=attributeName, valueList=logicalValueList, rc=localrc)
!        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!        write(message,'(A,L)') trim(message)//' ',logicalValueList(1)
!        do j=2, itemCount-1
!          write(message,'(A,L)') trim(message)//', ',logicalValueList(j)
!        enddo
!        deallocate(logicalValueList)
!      elseif (typekind==ESMF_TYPEKIND_CHARACTER) then
!        allocate(characterValueList(itemCount))
!        call ESMF_AttributeGet(state, name=attributeName, valueList=characterValueList, rc=localrc)
!        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!        if (len_trim(message) + len_trim(characterValueList(1)) + 1 <= len(message)) then
!          write(message,'(A,A)') trim(message)//' ',trim(characterValueList(1))
!          do j=2, itemCount-1
!            write(message,'(A,A)') trim(message)//', ',trim(characterValueList(j))
!          enddo
!          deallocate(characterValueList)
!        endif
!      elseif (typekind==ESMF_TYPEKIND_I4) then
!        allocate(integer4ValueList(itemCount))
!        call ESMF_AttributeGet(state, name=attributeName, valueList=integer4ValueList, rc=localrc)
!        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!        write(message,'(A,I3.3)') trim(message)//' ',integer4ValueList(1)
!        do j=2, itemCount-1
!          write(message,'(A,I3.3)') trim(message)//', ',integer4ValueList(j)
!        enddo
!        deallocate(integer4ValueList)
!      elseif (typekind==ESMF_TYPEKIND_I8) then
!        allocate(integer8ValueList(itemCount))
!        call ESMF_AttributeGet(state, name=attributeName, valueList=integer8ValueList, rc=localrc)
!        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!        write(message,'(A,I3.3)') trim(message)//' ',integer8ValueList(1)
!        do j=2, itemCount-1
!          write(message,'(A,I3.3)') trim(message)//', ',integer8ValueList(j)
!        enddo
!        deallocate(integer8ValueList)
!      elseif (typekind==ESMF_TYPEKIND_R4) then
!        allocate(real4ValueList(itemCount))
!        call ESMF_AttributeGet(state, name=attributeName, valueList=real4ValueList, rc=localrc)
!        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!        write(message,'(A,ES9.2)') trim(message)//' ',real4ValueList(1)
!        do j=2, itemCount-1
!          write(message,'(A,ES9.2)') trim(message)//', ',real4ValueList(j)
!        enddo
!        deallocate(real4ValueList)
!      elseif (typekind==ESMF_TYPEKIND_R8) then
!        allocate(real8ValueList(itemCount))
!        call ESMF_AttributeGet(state, name=attributeName, valueList=real8ValueList, rc=localrc)
!        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!        write(message,'(A,ES9.2)') trim(message)//' ',real8ValueList(1)
!        do j=2, itemCount-1
!          write(message,'(A,ES9.2)') trim(message)//', ',real8ValueList(j)
!        enddo
!        deallocate(real8ValueList)
!      endif
!    enddo
!
!    end subroutine MOSSCO_StateAttributeString
!

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateCheckFields"
  subroutine MOSSCO_StateCheckFields(state, status, rc)

    type(ESMF_State), intent(in)                       :: state
    type(ESMF_FieldStatus_Flag), intent(out), optional :: status
    integer(ESMF_KIND_I4), intent(out), optional       :: rc

    type(ESMF_FieldStatus_Flag)             :: status_
    integer(ESMF_KIND_I4)                   :: rc_, localrc

    integer(ESMF_KIND_I4)                   :: i,j, fieldCount, itemCount

    type(ESMF_Field)                        :: field
    type(ESMF_Field), allocatable           :: fieldList(:)
    type(ESMF_FieldBundle)                  :: fieldBundle
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    character(len=ESMF_MAXSTR)              :: name, fieldName

    rc_=ESMF_SUCCESS
    status_=ESMF_FIELDSTATUS_COMPLETE

    call ESMF_StateGet(state, name=name, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount > 0) then
      allocate(itemTypeList(itemCount))
      allocate(itemNameList(itemCount))

      call ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      do i=1, itemCount
        if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state, trim(itemNameList(i)), field, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_FieldGet(field, name=fieldName, status=status_, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          !write(0,*) trim(name), trim(fieldName), status_
        elseif (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
          call ESMF_StateGet(state, trim(itemNameList(i)), fieldBundle, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          allocate(fieldList(fieldCount))
          call ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          do j=1, fieldCount
            call ESMF_FieldGet(fieldList(j), status=status_, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
            if (status_ /= ESMF_FIELDSTATUS_COMPLETE) exit
          enddo
          deallocate(fieldList)
        endif
        if (status_ /= ESMF_FIELDSTATUS_COMPLETE) exit
      enddo

      deallocate(itemTypeList)
      deallocate(itemNameList)
    endif

    if (present(rc)) rc=rc_

    if (present(status)) then
      status=status_
    else
      if (present(rc) .and. status_ /= ESMF_FIELDSTATUS_COMPLETE) rc = 999
    endif

    return

  end subroutine MOSSCO_StateCheckFields

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateGetForeignGrid"
  subroutine MOSSCO_StateGetForeignGrid(state, grid, kwe, owner, other, &
     attributeName, rank, totalLWidth, totalUWidth, rc)

    type(ESMF_State), intent(in)                  :: state
    type(ESMF_Grid),  intent(out)                 :: grid
    type(ESMF_KeywordEnforcer), optional          :: kwe
    character(len=*), optional, intent(in)        :: owner
    type(ESMF_Grid), intent(out), optional        :: other
    character(len=*), optional, intent(in)        :: attributeName
    integer(ESMF_KIND_I4), intent(out), optional  :: rank
    integer(ESMF_KIND_I4), intent(out), optional, allocatable :: totalLWidth(:)
    integer(ESMF_KIND_I4), intent(out), optional, allocatable :: totalUWidth(:)
    integer(ESMF_KIND_I4), intent(out), optional  :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc, rank_, fieldCount
    character(len=ESMF_MAXSTR)          :: name, message, attributeName_
    character(len=ESMF_MAXSTR)          :: attributeValue, owner_
    logical                             :: isPresent
    type(ESMF_Field)                    :: field
    type(ESMF_Field), allocatable       :: fieldList(:)
    type(ESMF_FieldStatus_Flag)         :: fieldStatus
    type(ESMF_StateItem_Flag)           :: itemType
    type(ESMF_Grid)                     :: grid2, grid3
    integer(ESMF_KIND_I4)               :: totalLWidth3(3,1), totalUWidth3(3,1)
    integer(ESMF_KIND_I4)               :: totalLWidth2(2,1), totalUWidth2(2,1)

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc_ = ESMF_SUCCESS

    attributeName_ = 'foreign_grid_field_name'
    if (present(attributeName)) attributeName_ = trim(attributeName)

    call ESMF_AttributeGet(state, name=trim(attributeName_), value=attributeValue, &
      defaultValue='grid', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_StateGetFieldList(state, fieldList, fieldCount=fieldCount, &
      itemSearch=trim(attributeValue), fieldStatus=ESMF_FIELDSTATUS_COMPLETE, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldCount == 0) then
      call MOSSCO_StateGetFieldList(state, fieldList, fieldCount=fieldCount, &
        itemSearch=trim(attributeValue), fieldStatus=ESMF_FIELDSTATUS_GRIDSET, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (fieldCount == 0) then
      if (present(owner)) then
        write(message,'(A)') trim(owner)//' needs a foreign grid, which was not found.'
      else
        write(message,'(A)') '  needs a foreign grid, which was not found.'
      endif
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_NOT_FOUND
      return
    endif

    call ESMF_FieldGet(fieldList(1), grid=grid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridGet(grid, rank=rank_, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(rank)) rank = rank_

    if (rank_ < 2 .or. rank_ > 3) then
      if (present(owner)) then
        write(message,'(A)') trim(owner)//' needs a foreign grid of rank 2 or 3'
      else
        write(message,'(A)') '  needs a foreign grid of rank 2 or 3'
      endif
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_BAD
      return
    endif

    !> Look at the zones that are present in the field but
    !> not in the grid, by default make the zero
    totalLWidth3(:,1)=0
    totalUWidth3(:,1)=0
    totalLWidth2(:,1)=0
    totalUWidth2(:,1)=0

    call ESMF_FieldGet(fieldList(1), status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then

      call ESMF_FieldGet(fieldList(1), rank=rank_, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (rank_ == 3) then
        call ESMF_FieldGet(fieldList(1), totalLWidth=totalLWidth3, &
          totalUWidth=totalUWidth3, rc=localrc)
      elseif (rank_ == 2) then
          call ESMF_FieldGet(fieldList(1), totalLWidth=totalLWidth2, &
          totalUWidth=totalUWidth2, rc=localrc)
      else
          if (present(owner)) then
            write(message,'(A)') trim(owner)//' needs a foreign grid of rank 2 or 3'
          else
            write(message,'(A)') '  needs a foreign grid of rank 2 or 3'
          endif
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          rc = ESMF_RC_ARG_BAD
          return
      endif

      if (rank_ == 3) then
        if (present(totalUWidth)) then
          allocate(totalUWidth(3), stat=localrc)
          totalUWidth = totalUWidth3(:,1)
        endif
        if (present(totalLWidth)) then
          allocate(totalLWidth(3), stat=localrc)
          totalLWidth = totalLWidth3(:,1)
        endif
      else ! if rank_ == 2
        if (present(totalUWidth)) then
          allocate(totalUWidth(2), stat=localrc)
          totalUWidth = totalUWidth2(:,1)
        endif
        if (present(totalLWidth)) then
          allocate(totalLWidth(2), stat=localrc)
          totalLWidth = totalLWidth2(:,1)
        endif
      endif

    endif ! FIELDSTATUS_COMPLETE

    call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(other)) then
      write(message,'(A)') '  keyword "other" not implemented'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, rc=localrc)
      rc = ESMF_RC_NOT_IMPL
      return
    endif

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_StateGetForeignGrid

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateDestroyOwn"
  recursive subroutine MOSSCO_StateDestroyOwn(state, owner, rc)

    type(ESMF_State), intent(inout)              :: state
    character(len=*),  intent(in)                :: owner
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                        :: rc_, localrc, itemCount, i
    character(len=ESMF_MAXPATHLEN)               :: message, creator, name
    logical                                      :: isPresent
    type(ESMF_StateItem_Flag), allocatable       :: itemTypeList(:)
    character(len=ESMF_MAXSTR), allocatable      :: itemNameList(:)

    type(ESMF_State)       :: childState
    type(ESMF_Field)       :: field
    type(ESMF_FieldBundle) :: fieldBundle
    type(ESMF_RouteHandle) :: routeHandle
    type(ESMF_Array)       :: array
    type(ESMF_ArrayBundle) :: arrayBundle

    rc_ = ESMF_SUCCESS
    creator = 'none'

    call ESMF_AttributeGet(state, 'creator', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(state, 'creator', creator, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateGet(state, itemCount=itemCount, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemTypeList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount>0) then
      call ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    do i=1,itemCount
      if (itemTypeList(i) == ESMF_STATEITEM_STATE) then
        call ESMF_StateGet(state, itemNameList(i), childState, rc=localRc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call MOSSCO_DestroyOwn(childState, trim(owner), rc=localrc)
      elseif (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(state, itemNameList(i), field, rc=localRc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call MOSSCO_DestroyOwn(field, trim(owner), rc=localrc)
      elseif (itemTypeList(i) == ESMF_STATEITEM_ROUTEHANDLE) then
        call ESMF_StateGet(state, itemNameList(i), routeHandle, rc=localRc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call MOSSCO_DestroyOwn(routeHandle, trim(owner), rc=localrc)
      elseif (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
        call ESMF_StateGet(state, itemNameList(i), fieldBundle, rc=localRc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call MOSSCO_DestroyOwn(fieldBundle, trim(owner), rc=localrc)
      elseif (itemTypeList(i) == ESMF_STATEITEM_ARRAY) then
        call ESMF_StateGet(state, itemNameList(i), array, rc=localRc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call MOSSCO_DestroyOwn(array, trim(owner), rc=localrc)
      elseif (itemTypeList(i) == ESMF_STATEITEM_ARRAYBUNDLE) then
        call ESMF_StateGet(state, itemNameList(i), arrayBundle, rc=localRc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call MOSSCO_DestroyOwn(arrayBundle, trim(owner), rc=localrc)
      else
        write(message,'(A)') trim(owner)//' has unknown StateItem_Flag for item '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateRemove(state, (/itemNameList(i)/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    enddo

    call ESMF_StateGet(state, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemTypeList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount>0) then
      call ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    do i=1, itemCount
      write(message,'(A)') trim(owner)//' left item in state '//trim(name)
      call MOSSCO_MessageAdd(message,': '//trim(itemNameList(i)))
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
    enddo

    if (trim(owner) == trim(creator)) then
      call ESMF_StateDestroy(state, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      write(message,'(A)')  trim(owner)//'  destroyed its state '//trim(name)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call MOSSCO_Reallocate(itemTypeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(rc)) rc=localrc

  end subroutine MOSSCO_StateDestroyOwn

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldBundleDestroyOwn"
  subroutine MOSSCO_FieldBundleDestroyOwn(fieldBundle, owner, rc)

    type(ESMF_FieldBundle), intent(inout)        :: fieldBundle
    character(len=*),  intent(in)                :: owner
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                       :: rc_, localrc, itemCount, i
    character(len=ESMF_MAXSTR)                  :: message, creator, name
    logical                                     :: isPresent
    character(len=ESMF_MAXSTR), allocatable     :: itemNameList(:)
    type(ESMF_Field), allocatable               :: fieldList(:)

    rc_ = ESMF_SUCCESS
    creator = 'none'

    call ESMF_AttributeGet(fieldBundle, 'creator', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(fieldBundle, 'creator', creator, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    !> @todo: the call below throws an error, it is disregarded here
    call ESMF_FieldBundleGet(fieldBundle, fieldCount=itemCount, name=name, rc=localrc)
    !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (localrc /= ESMF_SUCCESS) then
      call ESMF_LogWrite('  disregard error above', ESMF_LOGMSG_ERROR)
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif

    call MOSSCO_Reallocate(itemNameList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(fieldList, itemCount, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount>0) then
      call ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList, &
        fieldNameList=itemNameList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    do i=1,itemCount

      call MOSSCO_DestroyOwn(fieldList(i), trim(owner), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (trim(owner) == trim(creator)) then
        call ESMF_FieldBundleRemove(fieldBundle, (/itemNameList(i)/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

    enddo

    if (trim(owner) == trim(creator)) then
      call ESMF_FieldBundleDestroy(fieldBundle, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)')  trim(owner)//'  destroyed its fieldBundle '//trim(name)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_FieldBundleDestroyOwn

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ArrayBundleDestroyOwn"
  subroutine MOSSCO_ArrayBundleDestroyOwn(arrayBundle, owner, rc)

    type(ESMF_ArrayBundle), intent(inout)        :: arrayBundle
    character(len=*),  intent(in)                :: owner
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc, itemCount, i
    character(len=ESMF_MAXPATHLEN)      :: message, creator, name
    logical                             :: isPresent
    character(len=ESMF_MAXPATHLEN), allocatable :: itemNameList(:)
    type(ESMF_Array), allocatable       :: arrayList(:)

    rc_ = ESMF_SUCCESS
    creator = 'none'
    name = 'none'

    call ESMF_AttributeGet(arrayBundle, 'creator', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(arrayBundle, 'creator', creator, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_ArrayBundleGet(arrayBundle, arrayCount=itemCount, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount>0) then
      allocate(itemNameList(itemCount))
      allocate(arrayList(itemCount))
    endif

    call ESMF_ArrayBundleGet(arrayBundle, arrayList=arrayList, arrayNameList=itemNameList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1,itemCount

      call MOSSCO_DestroyOwn(arrayList(i), trim(owner), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (trim(owner) == trim(creator)) then
        call ESMF_ArrayBundleRemove(arrayBundle, (/itemNameList(i)/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

    enddo

    if (trim(owner) == trim(creator)) then
      call ESMF_ArrayBundleDestroy(arrayBundle, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)')  trim(owner)//'  destroyed its arrayBundle '//trim(name)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    if (allocated(arrayList)) deallocate(arrayList)

    call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_ArrayBundleDestroyOwn

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ArrayDestroyOwn"
  subroutine MOSSCO_ArrayDestroyOwn(array, owner, rc)

    type(ESMF_Array), intent(inout)              :: array
    character(len=*),  intent(in)                :: owner
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc
    character(len=ESMF_MAXPATHLEN)      :: message, creator, name
    logical                             :: isPresent

    rc_ = ESMF_SUCCESS
    creator = 'none'
    name = 'none'

    call ESMF_ArrayGet(array, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(array, 'creator', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(array, 'creator', creator, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (trim(owner) == trim(creator)) then
      call ESMF_ArrayDestroy(array, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)')  trim(owner)//'  destroyed its array '//trim(name)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    endif

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_ArrayDestroyOwn

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldDestroyOwn"
  subroutine MOSSCO_FieldDestroyOwn(field, owner, rc)

    type(ESMF_Field), intent(inout)              :: field
    character(len=*),  intent(in)                :: owner
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc
    character(len=ESMF_MAXPATHLEN)      :: message, creator, name
    logical                             :: isPresent

    rc_ = ESMF_SUCCESS
    creator = 'none'
    name = 'none'

    call ESMF_FieldGet(field, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(field, 'creator', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(field, 'creator', creator, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (trim(owner) == trim(creator)) then
      call ESMF_FieldDestroy(field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)')  trim(owner)//'  destroyed its field '//trim(name)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    endif

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_FieldDestroyOwn

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_RouteHandleDestroyOwn"
  subroutine MOSSCO_RouteHandleDestroyOwn(routeHandle, owner, rc)

    type(ESMF_RouteHandle), intent(inout)        :: routeHandle
    character(len=*),  intent(in)                :: owner
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc
    character(len=ESMF_MAXPATHLEN)      :: message, creator, name
    logical                             :: isPresent

    rc_ = ESMF_SUCCESS
    creator = 'none'
    name = 'none'

    !> @todo
    isPresent = .false.
    !call ESMF_AttributeGet(routeHandle, 'creator', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      !call ESMF_AttributeGet(routeHandle, 'creator', creator, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (trim(owner) == trim(creator)) then
      call ESMF_RouteHandleDestroy(routeHandle, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)')  trim(owner)//'  destroyed its routeHandle '//trim(name)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    endif

    if (present(rc)) rc=rc_

  end subroutine MOSSCO_RouteHandleDestroyOwn

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateGetFieldGrid"
  subroutine MOSSCO_StateGetFieldGrid(state, itemName, grid, rc)

    type(ESMF_State), intent(in)        :: state
    character(len=*),  intent(in) :: itemName
    type(ESMF_Grid), intent(out)        :: grid
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc
    character(len=ESMF_MAXPATHLEN)              :: message, name
    logical                             :: isPresent
    type(ESMF_StateItem_Flag)           :: itemType
    type(ESMF_FieldStatus_Flag)         :: fieldStatus
    type(ESMF_GeomType_Flag)            :: geomType
    type(ESMF_Field)                    :: field

    rc_ = ESMF_SUCCESS

    call ESMF_StateGet(state, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(state, trim(itemName), itemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemType /= ESMF_STATEITEM_FIELD) then
      write(message,'(A)') trim(itemName)//' is not a field in state '//trim(name)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call MOSSCO_StateLog(state, rc=localrc)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateGet(state, trim(itemName), field, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) then
      write(message,'(A)') 'Cannot use empty field '
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_FieldGet(field, geomType=geomType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (geomType /= ESMF_GEOMTYPE_GRID) then
      write(message,'(A)') 'Cannot use non-gridded field '
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_FieldGet(field, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    return

  end subroutine MOSSCO_StateGetFieldGrid

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateItemNameCheck"
  subroutine MOSSCO_StateItemNameCheck(state, rc)

    type(ESMF_State), intent(in)        :: state
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc, i, itemCount
    character(len=ESMF_MAXSTR)              :: name
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)

    type(ESMF_StateItem_Flag)           :: itemType
    type(ESMF_Field)                    :: field

    rc_ = ESMF_SUCCESS

    call ESMF_StateGet(state, name=name, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount>0) then
      allocate(itemNameList(itemCount))
      allocate(itemTypeList(itemCount))
    endif

    do i=1,itemCount

      if (itemTypeList(i) == ESMF_STATEITEM_Field) then
        call ESMF_StateGet(state, trim(itemNameList(i)), field, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_FieldNameCheck(field, localrc)
      endif
    enddo

    if (present(rc)) rc = rc_

    return

  end subroutine MOSSCO_StateItemNameCheck

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StatePopulateAttributes"
    subroutine MOSSCO_StatePopulateAttributes(state, clock, kwe, rc)

#ifndef NO_ISO_FORTRAN_ENV
      use, intrinsic :: ISO_FORTRAN_ENV
#endif

      type(ESMF_State), intent(inout)              :: state
      type(ESMF_Clock), intent(in)                 :: clock
      logical, intent(in), optional                :: kwe !keyword-enforcer
      integer(ESMF_KIND_I4), intent(out), optional :: rc

      integer(ESMF_KIND_I4)               :: rc_, localrc
      logical                             :: isPresent
      character(len=ESMF_MAXPATHLEN)          :: string

      rc_ = ESMF_SUCCESS

      !call ESMF_AttributeSet(state, 'mossco_sha_key',MOSSCO_GIT_SHA_KEY, rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#ifndef NO_ISO_FORTRAN_ENV
      call ESMF_AttributeSet(state, 'compile_compiler_version',compiler_version(), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(state, 'compile_compiler_options',compiler_options(), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#endif

      call get_command(string)
      call ESMF_AttributeSet(state, 'run_command_line',trim(string), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call getcwd(string)
      call ESMF_AttributeSet(state, 'run_working_directory',trim(string), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#ifndef NO_ISO_FORTRAN_ENV
      call ESMF_AttributeSet(state, 'run_process_id',getpid(), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#endif

      !> @todo check cross-platform compatibility of these gnu extensions
      call getlog(string)
#ifndef NO_ISO_FORTRAN_ENV
      write(string,'(A,I5,A,I5,A)') trim(string)// '(id=',getuid(),', gid=',getgid(),')'
#endif
      call ESMF_AttributeSet(state, 'run_user',trim(string), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call hostnm(string)
      call ESMF_AttributeSet(state, 'run_user',trim(string), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(state, 'title','MOSSCO coupled simulation', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(state, 'institution','MOSSCO partners (HZG, IOW, and BAW)', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(state, 'institution_hzg','Helmholtz-Zentrum Geesthacht', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(state, 'institution_iow','Institut für Ostseeforschung Warnemünde', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_AttributeSet(state, 'institution_baw','Bundesanstalt für Wasserbau', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(state, 'history','Created by MOSSCO', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(state, 'source ','model_mossco', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(state, 'references ','http://www.mossco.de/doc', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(state, 'comment ','', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (present(rc)) rc = rc_

      return

  end subroutine MOSSCO_StatePopulateAttributes

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateLinkFieldsToBundle"
  subroutine MOSSCO_StateLinkFieldsToBundle(state, kwe, rc)

    type(ESMF_State), intent(inout)              :: state
    logical, intent(in), optional                :: kwe
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                   :: rc_, localrc, i, j, itemCount, k
    character(len=ESMF_MAXSTR)                  :: message, suffix
    character(len=ESMF_MAXPATHLEN)              :: name, itemName
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:), fieldNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    type(ESMF_StateItem_Flag)               :: itemType
    type(ESMF_Field)                        :: field
    type(ESMF_FieldBundle)                  :: fieldBundle

    rc_ = ESMF_SUCCESS

    call ESMF_StateGet(state, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount > 0) then
      allocate(itemTypeList(itemCount))
      allocate(itemNameList(itemCount))

      call ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=itemNameList, &
          rc=localRc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    do i=1,itemCount

      j=index(itemNameList(i),'_',back=.true.)
      if (j<1) cycle

      itemName=itemNameList(i)
      suffix=itemName(j+1:len_trim(itemName))

      ! Make sure the suffix is all numeric
      do k=1,len_trim(suffix)
        if (suffix(k:k) <'0' .or. suffix(k:k) > '9') then
          suffix(1:1)='!'  ! This is a stop marker
          exit
        endif
      enddo
      if (suffix(1:1)=='!') cycle

      if (itemtypeList(i) == ESMF_STATEITEM_FIELD) then

        call ESMF_StateGet(state, itemName(1:j-1), itemType=itemType, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (itemType == ESMF_STATEITEM_NOTFOUND) then

          fieldBundle = ESMF_FieldBundleCreate(name=itemName(1:j-1), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          write(message,'(A)')  '  created field bundle ',itemName(1:j-1)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          call ESMF_StateAddReplace(state, (/fieldBundle/), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        endif

        call ESMF_StateGet(state, itemName(1:j-1), itemType=itemType, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (itemType /= ESMF_STATEITEM_FIELDBUNDLE) then
          write(message,'(A)')  '  expected fieldBundle ',itemName(1:j-1)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif

        call ESMF_StateGet(state, itemName, field=field, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateGet(state, itemName(1:j-1), fieldBundle=fieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldBundleAdd(fieldBundle, (/field/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      elseif (itemtypeList(i) == ESMF_STATEITEM_ARRAY) then

        write(message,'(A)')  '  not implemented: link to arrayBundle ',itemName(1:j-1)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)

      endif
    enddo

    if (allocated(itemTypeList)) deallocate(itemTypeList)
    if (allocated(itemNameList)) deallocate(itemNameList)

    if (present(rc)) rc=rc_

    return

  end subroutine MOSSCO_StateLinkFieldsToBundle

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateMoveNumericFieldsToBundle"
  subroutine MOSSCO_StateMoveNumericFieldsToBundle(state, kwe, creator, rc)

    type(ESMF_State),      intent(inout)         :: state
    logical,               intent(in),  optional :: kwe
    character(len=*),      intent(in),  optional :: creator
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                   :: rc_, localrc, i, itemCount, j, k
    character(len=ESMF_MAXSTR)              :: name, itemName
    character(len=ESMF_MAXPATHLEN)          :: message, suffix
    type(ESMF_StateItem_Flag)               :: itemType
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    type(ESMF_Field)                        :: field
    type(ESMF_FieldBundle)                  :: fieldBundle

    rc_ = ESMF_SUCCESS
    if (present(rc))  rc = rc_
    if (present(kwe)) rc_ = rc_

    call ESMF_StateGet(state, itemCount=itemCount, rc=localrc)
    if (itemCount < 1) return

    call MOSSCO_Reallocate(itemTypeList, itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(state, itemNameList=itemNameList, itemTypeList=itemTypeList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i = 1, itemCount

      !> Only consider fields, later maybe add arrays?
      if (itemTypeList(i) /= ESMF_STATEITEM_FIELD) cycle

      itemName = itemNameList(i)
      j=index(itemName,'_',back=.true.)
      if (j<1) cycle

      suffix=itemName(j+1:len_trim(itemName))

      ! Make sure the suffix is all numeric
      do k=1,len_trim(suffix)
        if (suffix(k:k) <'0' .or. suffix(k:k) > '9') then
          suffix(1:1)='!'  ! This is a stop marker
        endif
      enddo
      if (suffix(1:1)=='!') cycle

      call ESMF_StateGet(state, itemName(1:j-1), itemType=itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> If the itemType is a field itself, then return as not implemented, we should
      !> think about moving this field to a new fieldBundle in the future
      if (itemType == ESMF_STATEITEM_FIELD) then
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        write(message,'(A)') '  field '//itemName(1:j-1)//' already exists, cannot bundle'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        return
      endif

      !> If not found, then create a fieldBundle
      if (itemType == ESMF_STATEITEM_NOTFOUND) then

        fieldBundle = ESMF_FieldBundleCreate(name=itemName(1:j-1), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (present(creator)) then
          call ESMF_AttributeSet(fieldBundle,'creator', trim(creator), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          write(message,'(A)')  trim(creator)//' created field bundle '//itemName(1:j-1)
        else
          write(message,'(A)')  '  created field bundle '//itemName(1:j-1)
        endif
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        call ESMF_StateAddReplace(state, (/fieldBundle/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_StateGet(state, itemName(1:j-1), itemType=itemType, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (itemType /= ESMF_STATEITEM_FIELDBUNDLE) then
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        write(message,'(A)') "  don't know how to handle this item"
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
        return
      endif

      call ESMF_StateGet(state, itemName, field=field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldBundleAdd(fieldBundle, (/field/), multiflag=.true., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateRemove(state, (/itemName/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldSet(field, name=trim(itemName(1:j-1)), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_AttributeSet(field, 'original_field_name', trim(itemName), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(i2,2x,A)')  i,'  moved '
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      write(message,'(4x,A)')  '  to '
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    enddo

    call MOSSCO_Reallocate(itemTypeList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(itemNameList, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(rc)) rc=rc_

    return

  end subroutine MOSSCO_StateMoveNumericFieldsToBundle

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateMoveFieldsToBundle"
  subroutine MOSSCO_StateMoveFieldsToBundle(state, kwe, include, rc)

    type(ESMF_State), intent(inout)              :: state
    logical, intent(in), optional                :: kwe
    character(len=*), intent(in), optional       :: include
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                   :: rc_, localrc, i, j, itemCount, k
    character(len=ESMF_MAXSTR)              :: name, includePattern
    character(len=ESMF_MAXPATHLEN)          :: message, suffix
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    type(ESMF_Field)                        :: field, newfield
    type(ESMF_FieldBundle)                  :: fieldBundle
    type(ESMF_TypeKind_Flag)                :: typeKind
    type(ESMF_Grid)                         :: grid

    rc_ = ESMF_SUCCESS
    includePattern = '*'
    if (present(include)) includePattern=trim(include)
    if (present(rc)) rc = rc_

    call ESMF_StateGet(state, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount < 1) return

    if (allocated(itemTypeList)) deallocate(itemTypeList)
    if (allocated(itemNameList)) deallocate(itemNameList)
    allocate(itemTypeList(itemCount), stat=localrc)
    if (localrc /= 0) then
        write(message,'(A)') '    could not allocate memory for itemTypeList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    allocate(itemNameList(itemCount), stat=localrc)
    if (localrc /= 0) then
        write(message,'(A)') '    could not allocate memory for itemTypeList'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1,itemCount

      if (len_trim(includePattern)>0) then
        j=index(itemNameList(i),trim(includePattern),back=.true.)
        if (j<1) cycle
        if (itemTypeList(i) /= ESMF_STATEITEM_FIELD) cycle
      endif

      call ESMF_StateGet(state, trim(itemNameList(i)), field, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateRemove(state, (/trim(itemNameList(i))/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      fieldBundle = ESMF_FieldBundleCreate(name=trim(itemNameList(i)), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !call ESMF_AttributeSet(fieldBundle, 'creator', trim(name), rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_FieldBundleAdd(fieldBundle, (/field/), multiflag=.true., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_StateAddReplace(state, (/fieldBundle/), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)')  '  moved '
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      write(message,'(A)')  '  to '
      call MOSSCO_FieldString(field, message, rc=localrc)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    enddo

    if (allocated(itemTypeList)) deallocate(itemTypeList)
    if (allocated(itemNameList)) deallocate(itemNameList)

    if (present(rc)) rc=rc_

    return

  end subroutine MOSSCO_StateMoveFieldsToBundle

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateGetFieldsList"
  !> @param rc: [optional] return code
  subroutine MOSSCO_StateGetFieldsList(state, fieldList, kwe, itemSearchList, &
    fieldCount, fieldStatus, verbose, owner, rc)

    type(ESMF_State), intent(in)                 :: state
    type(ESMF_Field), allocatable, intent(out)   :: fieldList(:)
    character(len=*), intent(in)                 :: itemSearchList(:)
    logical, intent(in), optional                :: kwe
    integer(ESMF_KIND_I4), intent(out), optional :: fieldCount
    type(ESMF_FieldStatus_Flag), intent(in), optional   :: fieldStatus
    logical, intent(in), optional                :: verbose
    character(len=*), optional, intent(in)       :: owner
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    type(ESMF_Field), allocatable   :: singleFieldList(:)
    integer(ESMF_KIND_I4)           :: i, fieldCount_
    integer(ESMF_KIND_I4)           :: singleFieldCount, rc_, localrc
    character(len=ESMF_MAXSTR)      :: message, owner_
    logical                         :: verbose_

    rc_ = ESMF_SUCCESS
    owner_ = '---'
    if (present(kwe)) rc_ = ESMF_SUCCESS
    if (present(rc)) rc = ESMF_SUCCESS
    verbose_ = .true.
    if (present(verbose)) verbose_ = verbose
    if (present(owner)) owner_ = trim(owner)

    fieldCount_ = 0
    call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    do i = lbound(itemSearchList,1),ubound(itemSearchList,1)
      if (present(fieldStatus)) then
        call MOSSCO_StateGetFieldList(state, singleFieldList, &
          itemSearch=trim(itemSearchlist(i)), fieldCount=singleFieldCount, &
          fieldStatus=fieldStatus, verbose=verbose_, rc=rc_)
      else
        call MOSSCO_StateGetFieldList(state, singleFieldList, &
          itemSearch=trim(itemSearchlist(i)), fieldCount=singleFieldCount, &
          verbose=verbose_, rc=rc_)
      endif
      ! write(message,'(A,X,I1,X,I1)') trim(itemSearchlist(i)),i,singleFieldCount
      ! call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)

      if (singleFieldCount > 0) then
        call MOSSCO_Reallocate(fieldList, fieldCount_ + singleFieldCount, &
          keep=.true., rc=localrc)
        fieldCount_ = size(fieldList,1)
        fieldList(fieldCount_ + 1 - singleFieldCount:size(fieldList,1))  &
          = singleFieldList(:)
      endif
    enddo

    ! do i=1,fieldCount_
    !   write(message,'(I1,X,I1)') i,fieldCount_
    !   call MOSSCO_FieldString(fieldList(i),message)
    !   call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    ! enddo

    if (present(fieldCount)) fieldCount = fieldCount_
    if (present(rc)) rc = rc_

  end subroutine MOSSCO_StateGetFieldsList

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateGetFieldList"
  !> @brief This function finds and returns fields in a state
  !> @param type(ESMF_State) state, [IN]
  !> @param type (ESMF_Field) fieldList(:), inout
  !> @param rc: [optional] return code
  subroutine MOSSCO_StateGetFieldList(state, fieldList, kwe, rc, itemSearch, &
    fieldCount, fieldStatus, include, exclude, verbose, owner)

    type(ESMF_State), intent(in)                 :: state
    type(ESMF_Field), allocatable, intent(out)   :: fieldList(:)
    logical, intent(in), optional                :: kwe
    character(len=*), intent(in), optional       :: itemSearch
    integer(ESMF_KIND_I4), intent(out), optional :: fieldCount
    type(ESMF_FieldStatus_Flag), intent(in), optional   :: fieldStatus
    character(len=*), intent(in), optional, pointer      :: include(:), exclude(:)
    character(len=*), intent(in), optional       :: owner
    logical, intent(in), optional                :: verbose
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                   :: rc_, localrc, i, j, itemCount
    integer(ESMF_KIND_I4)                   :: n, fieldCount_, fieldInBundleCount
    character(len=ESMF_MAXPATHLEN)          :: message
    character(len=ESMF_MAXSTR)              :: owner_
    type(ESMF_StateItem_Flag), allocatable, dimension(:) :: itemTypeList
    character(len=ESMF_MAXSTR), allocatable, dimension(:):: itemNameList, fieldNameList(:)
    type(ESMF_FieldBundle)                  :: fieldBundle
    type(ESMF_FieldStatus_Flag)             :: fieldStatus_
    type(ESMF_Field), allocatable           :: tempList(:), fieldInBundleList(:)
    logical                                 :: isMatch, verbose_

    owner_ = '---'
    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = ESMF_SUCCESS
    if (present(kwe)) rc_ = rc_
    if (present(owner)) owner_ = trim(adjustl(owner))

    fieldCount_ = 0
    itemCount = 0
    verbose_ = .false.
    if (present(verbose)) verbose_ = verbose

    call MOSSCO_Reallocate(fieldList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (present(itemSearch)) then
      !> With the optional itemSearch, it is only allowed to find one
      !> name-matching item.  Better use the include optional argument to filter
      !> for multiple matching fields.
      call ESMF_StateGet(state, itemSearch=trim(adjustl(itemSearch)), itemCount=itemCount, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (itemCount > 1) then
        write(message,'(A)') trim(owner_)//' found multiple items for name '//trim(itemSearch)
        rc_ = ESMF_RC_NOT_IMPL
        if (present(rc)) rc = rc_
        return
      endif
    else
      !> Otherwise get count of all items in the state (for now) and filter later
      call ESMF_StateGet(state, itemCount=itemCount, rc=localrc)
    endif
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !> If no fields are found, then an error is returned unless the fieldCount
    !> optional argument is provided.  This can correctly be zero.
    if (itemCount == 0) then
      if (present(fieldCount)) then
        fieldCount = 0
      else
        rc_ = ESMF_RC_NOT_FOUND
      endif
      if (present(rc)) rc = rc_
      return
    endif

    call MOSSCO_Reallocate(itemNameList, itemCount, keep=.false., rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    call MOSSCO_Reallocate(itemTypeList, itemCount, keep=.false., rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !> Get either one (with itemSearch option) or all field type and name information
    if (present(itemSearch)) then
      itemNameList(1)=trim(adjustl(itemSearch))
      call ESMF_StateGet(state, itemName=itemNameList(1), itemType=itemTypeList(1), &
        rc=localrc)
    else
      call ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=localrc)
    endif
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    !> Filter the list of all items such that it contains only fields, and complies with
    !> constraints from include/exclude/status optional arguments
    do i = 1, itemCount

      ! Look for an exclusion pattern on this item name
      if (present(exclude) .and. associated(exclude)) then
        do j = lbound(exclude,1),ubound(exclude,1)
          call MOSSCO_StringMatch(itemNameList(i), exclude(j), isMatch, localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

          if (ismatch) exit
        enddo

        if (isMatch) then
          if (verbose_) then
            write(message,'(A)') trim(owner_)//' excluded'
            call MOSSCO_MessageAdd(message, ' '//trim(itemNameList(i))//' with pattern ')
            call MOSSCO_MessageAdd(message, ' '//trim(exclude(j)))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          endif
          cycle
        endif
      endif

      !! Look for an inclusion pattern on this field or fieldBundle name
      if (present(include) .and. associated(include)) then
        do j = lbound(include,1),ubound(include,1)
          call MOSSCO_StringMatch(itemNameList(i), include(j), isMatch, localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

          if (ismatch .and. verbose_) then
            write(message,'(A)') trim(owner_)//' included'
            call MOSSCO_MessageAdd(message, ' '//trim(itemNameList(i))//' with pattern ')
            call MOSSCO_MessageAdd(message, ' '//trim(include(j)))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          endif
          if (ismatch) exit
        enddo
        if (.not.ismatch) then
          if (verbose_) then
            write(message,'(A)') trim(owner_)//' did not include'
            call MOSSCO_MessageAdd(message,' '//itemNameList(i))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
          endif
          cycle
        endif
      endif

      !> If it is a field, then add to a temporary fieldList
      if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then

        fieldCount_ = fieldCount_ + 1

        call MOSSCO_Reallocate(fieldList, fieldCount_, keep=.true., rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        !> @todo The following call resulted in NOT_FOUND sometimes, this should
        !> be investigated further
        call ESMF_StateGet(state, trim(itemNameList(i)), fieldList(fieldCount_), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        cycle
      endif

      !> if it is a fieldBundle, then add a matching field (only with itemSearch option)
      !> or all fields in the bundle to the temporary fieldList
      if (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then

        call ESMF_StateGet(state, trim(itemNameList(i)), fieldBundle, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        call ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldInBundleCount, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        if (fieldInBundleCount < 1) cycle

        call MOSSCO_Reallocate(fieldNameList, fieldInBundleCount, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        call MOSSCO_Reallocate(fieldInBundleList, fieldInBundleCount, keep=.false., rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        call ESMF_FieldBundleGet(fieldBundle, fieldList=fieldInBundleList, &
          fieldNameList=fieldNameList, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        do j = 1, fieldInBundleCount

          if (present(itemSearch)) then
            if (trim(fieldNameList(j)) /= trim(itemSearch)) cycle
          endif

          fieldCount_ = fieldCount_ + 1

          call MOSSCO_Reallocate(fieldList, fieldCount_, keep=.true., rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

          fieldList(fieldCount_) = fieldInBundleList(j)
        enddo

        call MOSSCO_Reallocate(fieldNameList, 0, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        call MOSSCO_Reallocate(fieldInBundleList, 0, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      endif
    enddo

    if (fieldCount_ == 0) then
      if (present(fieldCount)) then
        fieldCount = 0
      else
        rc_ = ESMF_RC_NOT_FOUND
      endif
    endif

    !> Filter all fields in fieldList for fieldStatus optional argument
    !> @todo add capability to have multiple status options
    if (present(fieldStatus) .and. fieldCount_ > 0) then
      call MOSSCO_Reallocate(tempList, fieldCount_, keep=.false., rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      n = 0
      do i = 1, fieldCount_

        call ESMF_FieldGet(fieldList(i), status=fieldStatus_, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

        if (fieldStatus /= fieldStatus_) cycle
        n = n + 1
        tempList(n) = fieldList(i)
      enddo

      fieldCount_ = n

      call MOSSCO_Reallocate(fieldList, fieldCount_, keep=.false., rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

      if (fieldCount_ > 0) then
        fieldList(1:fieldCount_) = tempList(1:fieldCount_)
      endif
    endif

    call MOSSCO_Reallocate(tempList, 0, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

    if (verbose_) then
      do i = 1, fieldCount_
        write(message,'(A)') trim(owner_)//' found '
        call MOSSCO_FieldString(fieldList(i), message)
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      enddo
    endif

    if (.not.present(fieldCount) .and. fieldCount_ == 0) rc_ = ESMF_RC_NOT_FOUND
    if (present(fieldCount)) fieldCount = fieldCount_
    if (present(rc)) rc = rc_

  end subroutine MOSSCO_StateGetFieldList

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateGetVelocity"
  !> @param rc: [optional] return code
  subroutine MOSSCO_StateGetVelocity(state, velocity,  kwe, direction, &
      sweep, transport, rc)

    type(ESMF_State), intent(in)                 :: state
    real(ESMF_KIND_R8),pointer,dimension(:,:,:), intent(out)  :: velocity
    logical, intent(in), optional                :: kwe
    real(ESMF_KIND_R8),pointer,dimension(:,:,:), intent(out), optional  :: direction
    real(ESMF_KIND_R8),pointer,dimension(:,:,:), intent(out), optional  :: sweep
    real(ESMF_KIND_R8),pointer,dimension(:,:,:), intent(out), optional  :: transport
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)              :: rc_, rank, localrc, i
    integer(ESMF_KIND_I4), allocatable :: ubnd(:), lbnd(:)
    real(ESMF_KIND_R8),pointer,dimension(:,:,:)  :: xVelocity, yVelocity, layer_height
    real(ESMF_KIND_R8),pointer,dimension(:,:)  :: area
    type(ESMF_Field)                   :: field
    type(ESMF_Grid)                    :: grid
    logical                            :: isPresent

    rc_ = ESMF_SUCCESS
    if (present(kwe)) rc = ESMF_SUCCESS
    if (present(rc)) rc = rc_
    nullify(velocity)

    call ESMF_StateGet(state, 'x_velocity_in_water', field, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate(ubnd(rank), stat=localrc)
    allocate(lbnd(rank), stat=localrc)

    call ESMF_FieldGetBounds(field, exclusiveUbound=ubnd, exclusiveLbound=lbnd, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, farrayPtr=xVelocity, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(state, 'y_velocity_in_water', field, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, farrayPtr=yVelocity, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    allocate(velocity(RANGE3D), stat=localrc)
    velocity(RANGE3D) = sqrt(yVelocity(RANGE3D) * yVelocity(RANGE3D) &
                      + xVelocity(RANGE3D) * xVelocity(RANGE3D))

    if (present(direction)) then
      nullify(direction)
      allocate(direction(RANGE3D), stat=localrc)
      velocity(RANGE3D) = datan2(yVelocity(RANGE3D)/velocity(RANGE3D), &
         xVelocity(RANGE3D)/velocity(RANGE3D)) * 180.0D0 / 3.14159265358979323846D0
    endif

    if (present(sweep) .or. present(transport)) then
      call ESMF_FieldGet(field, grid=grid, rc=localrc)

      !call ESMF_GridGetItem(grid, ESMF_GRIDITEM_AREA, isPresent=isPresent, rc=localrc)
      ! @todo this only works if the grid item AREA has been set in the grid.
      !if (isPresent) then
      !  call ESMF_GridGetItem(grid, ESMF_GRIDITEM_AREA, farrayPtr=area, rc=localrc)
      !else
      !  allocate(area(RANGE2D), stat=localrc)
      !  area = 1000 * 1000 ! assume 1 sqkm
      !endif
    endif

    if (present(sweep)) then
      nullify(sweep)
      allocate(sweep(RANGE3D), stat=localrc)
      do i = lbnd(3), ubnd(3)
        sweep(RANGE2D,i) = velocity(RANGE2D,i) * sqrt(area(RANGE2D))
      enddo
    endif

    if (present(transport)) then
      !> @todo
      ! call MOSSCO_GridGetLayerHeight(grid, layer_height, rc=localrc)
      nullify(transport)
      allocate(transport(RANGE3D), stat=localrc)
      do i = lbnd(3), ubnd(3)
        transport(RANGE2D,i) = velocity(RANGE2D,i) * sqrt(area(RANGE2D))
      enddo
      transport(RANGE3D) = transport(RANGE3D) * layer_height(RANGE3D)
    endif

    if (allocated(ubnd)) deallocate(ubnd)
    if (allocated(lbnd)) deallocate(lbnd)

  end subroutine MOSSCO_StateGetVelocity

end module mossco_state
