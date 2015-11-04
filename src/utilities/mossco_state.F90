!> @brief Implementation of ESMF State utilities
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2015 Helmholtz-Zentrum Geesthacht
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
#undef ESMF_FILENAME
#define ESMF_FILENAME "mossco_state.F90"

module mossco_state

use esmf
use mossco_field
use mossco_strings

implicit none

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

!#include "git-sha.h" !> @todo needs fix in Makefile for git_sha to avoid duplicate include

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateGetF1"
  subroutine MOSSCO_StateGetF1(state, fieldNameList, farrayPtr, kwe, lbnd, ubnd, verbose, rc)

    type(ESMF_State), intent(in)                 :: state
    character(len=*), dimension(:), intent(in)    :: fieldNameList
    real(ESMF_KIND_R8), pointer,dimension(:), intent(inout) :: farrayPtr

    logical, intent(in ), optional               :: kwe !keyword-enforcer
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
    character(len=ESMF_MAXSTR)    :: message, name
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

    do i=1,size(fieldNameList)
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
        do i=1,size(fieldNameList)
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
    integer(ESMF_KIND_I4)         :: localrc,i, fieldCount
    type(ESMF_StateItem_Flag)     :: itemType
    type(ESMF_Field), allocatable :: fieldList(:)
    logical                       :: isPresent, verbose_
    character(len=ESMF_MAXSTR)    :: message, name
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

    do i=1,size(fieldNameList)
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
        do i=1,size(fieldNameList)
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
    character(len=ESMF_MAXSTR)    :: message, name
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

    do i=1,size(fieldNameList)
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
        do i=1,size(fieldNameList)
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
  subroutine set_item_flags(state,name,requiredFlag,optionalFlag,requiredRank)
    type(ESMF_State)           :: state
    character(len=ESMF_MAXSTR) :: name,attname
    integer,optional           :: requiredRank
    logical,optional           :: requiredFlag,optionalFlag
    integer                    :: localrc, rc

    if (present(requiredFlag)) then
      attname=trim(name)//':required'
      call ESMF_AttributeSet(state,name=attname,value=requiredFlag,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end if
    if (present(requiredRank)) then
      attname=trim(name)//':required_rank'
      call ESMF_AttributeSet(state,name=attname,value=requiredRank,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end if
    if (present(optionalFlag)) then
      attname=trim(name)//':optional'
      call ESMF_AttributeSet(state,name=attname,value=optionalFlag,rc=rc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end if
  end subroutine set_item_flags


  !> create fields from required attributes in a state
#undef  ESMF_METHOD
#define ESMF_METHOD "create_required_fields"
  subroutine create_required_fields(state,grid)
  type(ESMF_State), intent(inout)   :: state
  type(ESMF_Grid),  intent(in)      :: grid
  type(ESMF_Field)                  :: field
  type(ESMF_Typekind_Flag)          :: typeKind=ESMF_TYPEKIND_R8
  type(ESMF_StateItem_Flag)         :: itemFlag
  integer                           :: n,rc,idx,attCount
  logical                           :: required
  character(ESMF_MAXSTR)            :: attName,fieldName
  integer                           :: localrc

  !> get Attribute list
  call ESMF_AttributeGet(state,attCount,rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  do n=1,attCount
    call ESMF_AttributeGet(state,attributeIndex=n,name=attName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> check for ":required"
    idx = index(attName,':required ')
    if (idx>0) then
      call ESMF_AttributeGet(state,name=attName,value=required,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemFlag == ESMF_STATEITEM_NOTFOUND) then
      !> create field
      field = ESMF_FieldCreate(grid,typekind=typeKind,name=fieldName,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      !> append field to state
      call ESMF_StateAdd(state,(/ field /),rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    else
      !write(0,*) 'item ',trim(fieldName),' already present',itemFlag
    end if
  end do
  end subroutine create_required_fields

  !> create optional fields from available names and optional attributes in a state
#undef  ESMF_METHOD
#define ESMF_METHOD "create_optional_fields_from_names"
  subroutine create_optional_fields_from_names(state,names,grid)
  type(ESMF_State), intent(inout)   :: state
  type(ESMF_Grid),  intent(in)      :: grid
  character(len=*),dimension(:)     :: names
  type(ESMF_Field)                  :: field
  type(ESMF_Typekind_Flag)          :: typeKind=ESMF_TYPEKIND_R8
  type(ESMF_StateItem_Flag)         :: itemFlag
  integer                           :: n,rc,idx,attCount
  logical                           :: optional
  character(ESMF_MAXSTR)            :: attName,potentialFieldName
  integer                           :: localrc

  !> get Attribute list
  call ESMF_AttributeGet(state,attCount,rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  do n=1,attCount
    call ESMF_AttributeGet(state,attributeIndex=n,name=attName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !> check for ":optional"
    idx = index(attName,':optional ')
    if (idx>0) then
      call ESMF_AttributeGet(state,name=attName,value=optional,rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (itemFlag == ESMF_STATEITEM_NOTFOUND) then
          !> create field
          field = ESMF_FieldCreate(grid,typekind=typeKind,name=potentialFieldName,rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
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
  subroutine MOSSCO_StateLog(state, kwe, log, rc)
    type(ESMF_State)                :: state
    logical,intent(in ),optional    :: kwe !keyword-enforcer
    type(ESMF_Log), optional        :: log
    integer(ESMF_KIND_I4), optional :: rc

    integer(ESMF_KIND_I4)           :: localRc, itemCount, i, rank, j, maxDigits, count, fieldCount
    character(len=ESMF_MAXSTR)      :: fieldName, name, string, gridName, attributeName
    character(len=4096)             :: message
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:), fieldNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    type(ESMF_Field), allocatable   :: fieldList(:)
    type(ESMF_Field)                :: field
    type(ESMF_FieldBundle)          :: fieldBundle
    integer(ESMF_KIND_I4)           :: totalLWidth(7), totalUWidth(7)
    type(ESMF_Grid)                 :: grid
    logical                         :: isPresent, isNeeded
    type(ESMF_LocStream)            :: locStream
    type(ESMF_TypeKind_Flag)        :: typeKind
    logical, allocatable            :: logicalValueList(:)
    real(kind=ESMF_KIND_R4), allocatable    :: real4ValueList(:)
    real(kind=ESMF_KIND_R8), allocatable    :: real8ValueList(:)
    integer(kind=ESMF_KIND_I4), allocatable :: integer4ValueList(:)
    integer(kind=ESMF_KIND_I8), allocatable :: integer8ValueList(:)
    character(len=4096)       , allocatable :: characterValueList(:)

    if (present(rc)) rc=ESMF_SUCCESS

    call ESMF_StateGet(state, name=name, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(state, count=count, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (count==0) then
      write(message,'(A)')  trim(name)//' contains no attributes'
      if (present(log)) then
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
      else
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    endif

    do i=1, count
      call ESMF_AttributeGet(state, attributeIndex=i , name=attributeName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      write(message,'(A)')  trim(name)//' attribute'
      call MOSSCO_MessageAdd(message,' '//trim(attributeName)//'=')

      call ESMF_AttributeGet(state, name=attributeName, typekind=typekind,  itemCount=itemCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (typekind==ESMF_TYPEKIND_Logical) then
        allocate(logicalValueList(itemCount))
        call ESMF_AttributeGet(state, name=attributeName, valueList=logicalValueList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,L)') trim(message)//' ',logicalValueList(1)
        do j=2, itemCount-1
          write(message,'(A,L)') trim(message)//', ',logicalValueList(j)
        enddo
        deallocate(logicalValueList)
      elseif (typekind==ESMF_TYPEKIND_CHARACTER) then
        if (allocated(characterValueList)) deallocate(characterValueList)
        allocate(characterValueList(itemCount))
        call ESMF_AttributeGet(state, name=attributeName, valueList=characterValueList, rc=localrc)
        if (localrc /= ESMF_SUCCESS) then
          !(ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) then
          !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          !>@ todo: how to deal with very long attributes that don't fit into valueList?
          rc=ESMF_SUCCESS
        endif

        if (len_trim(message) + len_trim(characterValueList(1)) + 1 <= len(message)) then
          write(message,'(A,A)') trim(message)//' ',trim(characterValueList(1))
          do j=2, itemCount-1
            write(message,'(A,A)') trim(message)//', ',trim(characterValueList(j))
          enddo
        endif
        if (allocated(characterValueList)) deallocate(characterValueList)
      elseif (typekind==ESMF_TYPEKIND_I4) then
        allocate(integer4ValueList(itemCount))
        call ESMF_AttributeGet(state, name=attributeName, valueList=integer4ValueList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,I3.3)') trim(message)//' ',integer4ValueList(1)
        do j=2, itemCount-1
          write(message,'(A,I3.3)') trim(message)//', ',integer4ValueList(j)
        enddo
        deallocate(integer4ValueList)
      elseif (typekind==ESMF_TYPEKIND_I8) then
        allocate(integer8ValueList(itemCount))
        call ESMF_AttributeGet(state, name=attributeName, valueList=integer8ValueList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,I3.3)') trim(message)//' ',integer8ValueList(1)
        do j=2, itemCount-1
          write(message,'(A,I3.3)') trim(message)//', ',integer8ValueList(j)
        enddo
        deallocate(integer8ValueList)
      elseif (typekind==ESMF_TYPEKIND_R4) then
        allocate(real4ValueList(itemCount))
        call ESMF_AttributeGet(state, name=attributeName, valueList=real4ValueList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,G8.2)') trim(message)//' ',real4ValueList(1)
        do j=2, itemCount-1
          write(message,'(A,G8.2)') trim(message)//', ',real4ValueList(j)
        enddo
        deallocate(real4ValueList)
      elseif (typekind==ESMF_TYPEKIND_R8) then
        allocate(real8ValueList(itemCount))
        call ESMF_AttributeGet(state, name=attributeName, valueList=real8ValueList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,G8.2)') trim(message)//' ',real8ValueList(1)
        do j=2, itemCount-1
          write(message,'(A,G8.2)') trim(message)//', ',real8ValueList(j)
        enddo
        deallocate(real8ValueList)
      endif
      if (present(log)) then
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
      else
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    enddo

    call ESMF_StateGet(state, itemCount=itemCount, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount==0) then
      write(message,'(A)')  trim(name)//' contains no data items'
      if (present(log)) then
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
      else
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      endif
    else
      allocate(itemTypeList(itemCount))
      allocate(itemNameList(itemCount))

      call ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=itemNameList, &
        rc=localRc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    do i=1,itemCount
      if (itemtypeList(i) == ESMF_STATEITEM_FIELD) then
        write(message,'(A)')  trim(name)//' field'

        call ESMF_StateGet(state, itemNameList(i), field, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_FieldString(field, message)
        !!> @todo write out attributes of field
        if (present(log)) then
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
        else
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif

      elseif (itemtypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
        call ESMF_StateGet(state, itemNameList(i), fieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        allocate(fieldList(fieldCount),fieldNameList(fieldCount))
        call ESMF_FieldBundleGet(fieldBundle, fieldNameList=fieldNameList, fieldList=fieldList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        do j=1, fieldCount
          write(message,'(A)')  trim(name)//' field '//trim(itemNameList(i))//'/'
          call MOSSCO_FieldString(fieldList(j), message)
          !!> @todo write out attributes of field
          if (present(log)) then
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
          else
            call ESMF_LogWrite(trim(message))
          endif
        enddo
        deallocate(fieldList,fieldNameList)
      else
        write(message,'(A)')  trim(name)//' non-field item '//trim(itemNameList(i))//' skipped'
        if (present(log)) then
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO, log=log)
        else
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        endif
        cycle
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

    if (allocated(itemTypeList)) deallocate(itemTypeList)
    if (allocated(itemNameList)) deallocate(itemNameList)

  end subroutine MOSSCO_StateLog

!#undef  ESMF_METHOD
!#define ESMF_METHOD "MOSSCO_StateAttributeString"
!  subroutine MOSSCO_StateAttributeString(state, message , length, rc)
!
!    use mossco_strings
!    implicit none
!
!    type(ESMF_State), intent(in)                    :: state
!    character(len=ESMF_MAXSTR), intent(inout)      :: message
!    integer(ESMF_KIND_I4), intent(inout), optional :: length
!    integer(ESMF_KIND_I4), intent(out), optional   :: rc
!
!    type(ESMF_TypeKind_Flag)        :: typeKind
!    logical, allocatable            :: logicalValueList(:)
!    real(kind=ESMF_KIND_R4), allocatable    :: real4ValueList(:)
!    real(kind=ESMF_KIND_R8), allocatable    :: real8ValueList(:)
!    integer(kind=ESMF_KIND_I4), allocatable :: integer4ValueList(:)
!    integer(kind=ESMF_KIND_I8), allocatable :: integer8ValueList(:)
!    character(len=ESMF_MAXSTR), allocatable :: characterValueList(:)
!
!    integer(ESMF_KIND_I4)   :: rank, localrc, count, i, j, itemCount
!    character(ESMF_MAXSTR)  :: attributeName
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
!        write(message,'(A,G8.2)') trim(message)//' ',real4ValueList(1)
!        do j=2, itemCount-1
!          write(message,'(A,G8.2)') trim(message)//', ',real4ValueList(j)
!        enddo
!        deallocate(real4ValueList)
!      elseif (typekind==ESMF_TYPEKIND_R8) then
!        allocate(real8ValueList(itemCount))
!        call ESMF_AttributeGet(state, name=attributeName, valueList=real8ValueList, rc=localrc)
!        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!        write(message,'(A,G8.2)') trim(message)//' ',real8ValueList(1)
!        do j=2, itemCount-1
!          write(message,'(A,G8.2)') trim(message)//', ',real8ValueList(j)
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount > 0) then
      allocate(itemTypeList(itemCount))
      allocate(itemNameList(itemCount))

      call ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      do i=1, itemCount
        if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state, trim(itemNameList(i)), field, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_FieldGet(field, name=fieldName, status=status_, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          !write(0,*) trim(name), trim(fieldName), status_
        elseif (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
          call ESMF_StateGet(state, trim(itemNameList(i)), fieldBundle, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          allocate(fieldList(fieldCount))
          call ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          do j=1, fieldCount
            call ESMF_FieldGet(fieldList(j), status=status_, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
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
  subroutine MOSSCO_StateGetForeignGrid(state, grid, rc)

    type(ESMF_State), intent(in)              :: state
    type(ESMF_Grid),  intent(out)             :: grid
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc
    character(ESMF_MAXSTR)              :: name, message, attributeName, attributeValue
    logical                             :: isPresent
    type(ESMF_Field)                    :: field
    type(ESMF_FieldStatus_Flag)         :: fieldStatus
    type(ESMF_StateItem_Flag)           :: itemType

    rc_ = ESMF_SUCCESS
    attributeName='foreign_grid_field_name'

    call ESMF_AttributeGet(state, name=trim(attributeName), &
      isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (.not.isPresent) then
      write(message, '(A)')  'Requested attribute '//trim(attributeName)//' not found.'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      call MOSSCO_StateLog(state, rc=localrc)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    endif

    call ESMF_AttributeGet(state, name=trim(attributeName), value=attributeValue, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(state, trim(attributeValue), itemType=itemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (itemType == ESMF_STATEITEM_NOTFOUND) then
      write(message, '(A)')  'Requested item '//trim(attributeValue)//' not found.'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      call MOSSCO_StateLog(state, rc=localrc)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    endif

    if (itemType /= ESMF_STATEITEM_FIELD) then
      write(message, '(A)')  'Requested item '//trim(attributeName)//' ist not a field.'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      call MOSSCO_StateLog(state, rc=localrc)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    endif

    call ESMF_StateGet(state, trim(attributeValue), field, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
    if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) then
      write(message, '(A)')  'Requested field '//trim(attributeName)//' is empty.'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_ERROR)
      call MOSSCO_StateLog(state, rc=localrc)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    endif

    call ESMF_FieldGet(field, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (present(rc)) rc=rc_
    return
  end subroutine MOSSCO_StateGetForeignGrid

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateDestroyOwn"
  recursive subroutine MOSSCO_StateDestroyOwn(importState, owner, rc)

    type(ESMF_State), intent(inout)              :: importState
    character(ESMF_MAXSTR),  intent(in)          :: owner
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc, itemCount, i
    character(ESMF_MAXSTR)              :: message, creator
    logical                             :: isPresent
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    character(ESMF_MAXSTR), allocatable :: itemNameList(:)

    type(ESMF_State)    :: state
    type(ESMF_Field)    :: field
    type(ESMF_FieldBundle) :: fieldBundle
    type(ESMF_RouteHandle) :: routeHandle
    type(ESMF_Array)    :: array
    type(ESMF_ArrayBundle) :: arrayBundle

    rc_ = ESMF_SUCCESS
    creator = 'none'

    call ESMF_AttributeGet(importState, 'creator', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(importState, 'creator', creator, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_StateGet(importState, itemCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount>0) then
      allocate(itemTypeList(itemCount))
      allocate(itemNameList(itemCount))
    endif
    call ESMF_StateGet(importState, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1,itemCount
      if (itemTypeList(i) == ESMF_STATEITEM_STATE) then
        call ESMF_StateGet(importState, itemNameList(i), state, rc=localRc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call MOSSCO_DestroyOwn(state, trim(owner), rc=localrc)
      elseif (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(importState, itemNameList(i), field, rc=localRc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call MOSSCO_DestroyOwn(field, trim(owner), rc=localrc)
      elseif (itemTypeList(i) == ESMF_STATEITEM_ROUTEHANDLE) then
        call ESMF_StateGet(importState, itemNameList(i), routeHandle, rc=localRc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call MOSSCO_DestroyOwn(routeHandle, trim(owner), rc=localrc)
      elseif (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
        call ESMF_StateGet(importState, itemNameList(i), fieldBundle, rc=localRc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call MOSSCO_DestroyOwn(fieldBundle, trim(owner), rc=localrc)
      elseif (itemTypeList(i) == ESMF_STATEITEM_ARRAY) then
        call ESMF_StateGet(importState, itemNameList(i), array, rc=localRc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call MOSSCO_DestroyOwn(array, trim(owner), rc=localrc)
      elseif (itemTypeList(i) == ESMF_STATEITEM_ARRAYBUNDLE) then
        call ESMF_StateGet(importState, itemNameList(i), arrayBundle, rc=localRc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call MOSSCO_DestroyOwn(arrayBundle, trim(owner), rc=localrc)
      else
        write(message,'(A)') 'Unknown StateItem_Flag for item '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (trim(owner) == trim(creator)) then
        call ESMF_StateRemove(importState, (/itemNameList(i)/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

    enddo

    if (trim(owner) == trim(creator)) then
      call ESMF_StateDestroy(importState, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    if (allocated(itemTypeList)) deallocate(itemTypeList)
    if (allocated(itemNameList))   deallocate(itemNameList)

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_StateDestroyOwn

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldBundleDestroyOwn"
  subroutine MOSSCO_FieldBundleDestroyOwn(fieldBundle, owner, rc)

    type(ESMF_FieldBundle), intent(inout)        :: fieldBundle
    character(ESMF_MAXSTR),  intent(in)          :: owner
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc, itemCount, i
    character(ESMF_MAXSTR)              :: message, creator
    logical                             :: isPresent
    character(ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_Field), allocatable       :: fieldList(:)

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

    call ESMF_FieldBundleGet(fieldBundle, fieldCount=itemCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount>0) then
      allocate(itemNameList(itemCount))
      allocate(fieldList(itemCount))
    endif

    call ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList, fieldNameList=itemNameList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    endif

    if (allocated(fieldList))  deallocate(fieldList)
    if (allocated(itemNameList))   deallocate(itemNameList)

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_FieldBundleDestroyOwn

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ArrayBundleDestroyOwn"
  subroutine MOSSCO_ArrayBundleDestroyOwn(arrayBundle, owner, rc)

    type(ESMF_ArrayBundle), intent(inout)        :: arrayBundle
    character(ESMF_MAXSTR),  intent(in)          :: owner
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc, itemCount, i
    character(ESMF_MAXSTR)              :: message, creator
    logical                             :: isPresent
    character(ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_Array), allocatable       :: arrayList(:)

    rc_ = ESMF_SUCCESS
    creator = 'none'

    call ESMF_AttributeGet(arrayBundle, 'creator', isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) then
      call ESMF_AttributeGet(arrayBundle, 'creator', creator, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_ArrayBundleGet(arrayBundle, arrayCount=itemCount, rc=localrc)
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
    endif


    if (allocated(arrayList))  deallocate(arrayList)
    if (allocated(itemNameList))   deallocate(itemNameList)

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_ArrayBundleDestroyOwn

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_ArrayDestroyOwn"
  subroutine MOSSCO_ArrayDestroyOwn(array, owner, rc)

    type(ESMF_Array), intent(inout)              :: array
    character(ESMF_MAXSTR),  intent(in)          :: owner
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc
    character(ESMF_MAXSTR)              :: message, creator
    logical                             :: isPresent

    rc_ = ESMF_SUCCESS
    creator = 'none'

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
    endif

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_ArrayDestroyOwn

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_FieldDestroyOwn"
  subroutine MOSSCO_FieldDestroyOwn(field, owner, rc)

    type(ESMF_Field), intent(inout)              :: field
    character(ESMF_MAXSTR),  intent(in)          :: owner
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc
    character(ESMF_MAXSTR)              :: message, creator
    logical                             :: isPresent

    rc_ = ESMF_SUCCESS
    creator = 'none'

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
    endif

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_FieldDestroyOwn

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_RouteHandleDestroyOwn"
  subroutine MOSSCO_RouteHandleDestroyOwn(routeHandle, owner, rc)

    type(ESMF_RouteHandle), intent(inout)        :: routeHandle
    character(ESMF_MAXSTR),  intent(in)          :: owner
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc
    character(ESMF_MAXSTR)              :: message, creator
    logical                             :: isPresent

    rc_ = ESMF_SUCCESS
    creator = 'none'

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
    endif

    if (present(rc)) rc=rc_
    return

  end subroutine MOSSCO_RouteHandleDestroyOwn

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StateGetFieldGrid"
  subroutine MOSSCO_StateGetFieldGrid(state, itemName, grid, rc)

    type(ESMF_State), intent(in)        :: state
    character(len=*),  intent(in) :: itemName
    type(ESMF_Grid), intent(out)        :: grid
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)               :: rc_, localrc
    character(ESMF_MAXSTR)              :: message, name
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
      call MOSSCO_FieldString(field, message)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_FieldGet(field, geomType=geomType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (geomType /= ESMF_GEOMTYPE_GRID) then
      write(message,'(A)') 'Cannot use non-gridded field '
      call MOSSCO_FieldString(field, message)
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
    character(ESMF_MAXSTR)              :: name
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
      character(len=ESMF_MAXSTR)          :: string

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
    character(ESMF_MAXSTR)                  :: message, name, suffix, itemName
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
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
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
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (itemType == ESMF_STATEITEM_NOTFOUND) then

          fieldBundle = ESMF_FieldBundleCreate(name=itemName(1:j-1), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          write(message,'(A)')  '  created field bundle ',itemName(1:j-1)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          call ESMF_StateAddReplace(state, (/fieldBundle/), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        endif

        call ESMF_StateGet(state, itemName(1:j-1), itemType=itemType, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (itemType /= ESMF_STATEITEM_FIELDBUNDLE) then
          write(message,'(A)')  '  expected fieldBundle ',itemName(1:j-1)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif

        call ESMF_StateGet(state, itemName, field=field, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateGet(state, itemName(1:j-1), fieldBundle=fieldBundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_FieldBundleAdd(fieldBundle, (/field/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
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

end module mossco_state
