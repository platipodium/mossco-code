module mossco_state

use esmf
implicit none

interface mossco_state_get
    module procedure mossco_state_get_f2
    module procedure mossco_state_get_f3
end interface

contains

  subroutine mossco_state_get_f2(state,name,fpointer,rc)
    type(ESMF_State) :: state
    character(len=*),dimension(:) :: name
    type(ESMF_Field) :: field
    real(ESMF_KIND_R8),pointer,dimension(:,:) :: fpointer
    integer(ESMF_KIND_I4) :: esmfrc,i
    integer,intent(out) :: rc
    type(ESMF_StateItem_Flag) :: itemType

    rc=1
    do i=1,size(name)
      call ESMF_StateGet(state,trim(name(i)),itemType, rc=esmfrc) ! this is really a call to StateGetInfo
      if(itemtype == ESMF_STATEITEM_NOTFOUND) then
!         write(0,*) 'not found field ',trim(name(i))
        continue
      else
         if(esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!        write(0,*) 'found field ',trim(name(i))
         call ESMF_StateGet(state,trim(name(i)),field,rc=esmfrc)
         if(esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
         call ESMF_FieldGet(field,localde=0,farrayPtr=fpointer,rc=esmfrc)
         if(esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
         rc=0
         exit
      end if
    end do
  end subroutine mossco_state_get_f2

  subroutine mossco_state_get_f3(state,name,fpointer,rc)
    type(ESMF_State) :: state
    type(ESMF_Field) :: field
    character(len=*),dimension(:) :: name
    real(ESMF_KIND_R8),pointer,dimension(:,:,:) :: fpointer
    integer(ESMF_KIND_I4) :: esmfrc,i
    integer,intent(out) :: rc
    type(ESMF_StateItem_Flag) :: itemType

    rc=1
    do i=1,size(name)
      call ESMF_StateGet(state,trim(name(i)),itemType, rc=esmfrc)
      if(itemType == ESMF_STATEITEM_NOTFOUND) then
!         write(0,*) 'not found field ',trim(name(i))
        continue
      else
         if(esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!        write(0,*) 'found field ',trim(name(i))
         call ESMF_StateGet(state,trim(name(i)),field,rc=esmfrc)
         if(esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
         call ESMF_FieldGet(field,localde=0,farrayPtr=fpointer,rc=esmfrc)
         if(esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
         rc=0
         exit
      end if
    end do
  end subroutine mossco_state_get_f3


  !> set ESMF attributes "required_flag", "required" and "optional" for
  !! an item's name in the importState
  subroutine set_item_flags(state,name,requiredFlag,optionalFlag,requiredRank)
    type(ESMF_State)           :: state
    character(len=ESMF_MAXSTR) :: name
    integer,optional           :: requiredRank
    logical,optional           :: requiredFlag,optionalFlag
    integer                    :: rc
     
    if (present(requiredFlag)) then
      call ESMF_AttributeSet(state,name=trim(name)//':required',value=requiredFlag,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    end if
    if (present(requiredRank)) then
      call ESMF_AttributeSet(state,name=trim(name)//':required_rank',value=requiredRank,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    end if
    if (present(optionalFlag)) then
      call ESMF_AttributeSet(state,name=trim(name)//':optional',value=optionalFlag,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    end if
  end subroutine set_item_flags


  !> create fields from required attributes in a state
  subroutine create_required_fields(state,grid)
  type(ESMF_State), intent(inout)   :: state
  type(ESMF_Grid),  intent(in)      :: grid
  type(ESMF_Field)                  :: field
  type(ESMF_Typekind_Flag)          :: typeKind=ESMF_TYPEKIND_R8
  type(ESMF_StateItem_Flag)         :: itemFlag
  integer                           :: n,rc,idx,attCount
  logical                           :: required
  character(ESMF_MAXSTR)            :: attName,fieldName

  !> get Attribute list
  call ESMF_AttributeGet(state,attCount,rc=rc)

  do n=1,attCount
    call ESMF_AttributeGet(state,attributeIndex=n,name=attName) 
    !> check for ":required"
    idx = index(attName,':required ')
    if (idx>0) then
      call ESMF_AttributeGet(state,name=attName,value=required,rc=rc)
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
    call ESMF_StateGet(state,fieldName,itemFlag,rc=rc)
    if (itemFlag == ESMF_STATEITEM_NOTFOUND) then
      !> create field
      field = ESMF_FieldCreate(grid,typekind=typeKind,name=fieldName,rc=rc)
      !> append field to state
      call ESMF_StateAdd(state,(/ field /),rc=rc)
    else
      !write(0,*) 'item ',trim(fieldName),' already present',itemFlag
    end if
  end do
  end subroutine create_required_fields

  !> create optional fields from available names and optional attributes in a state
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

  !> get Attribute list
  call ESMF_AttributeGet(state,attCount,rc=rc)
  write(0,*) 'check ',attCount,'attributes for names: ',names 

  do n=1,attCount
    call ESMF_AttributeGet(state,attributeIndex=n,name=attName) 
    !> check for ":optional"
    idx = index(attName,':optional ')
    if (idx>0) then
      call ESMF_AttributeGet(state,name=attName,value=optional,rc=rc)
      if (optional) then
        potentialFieldName=attName(1:idx-1)
      else
        write(0,*) 'found attribute ',trim(attName),', but not optional'
        cycle
      end if
    else
      write(0,*) 'attribute not relevant: ',trim(attName)
      cycle
    end if
    
    write(0,*) ' check for potential field name ',trim(potentialFieldName)
    !> check for available names == potentialFieldName
    do idx=1,ubound(names,1)
      if (trim(names(idx))==trim(potentialFieldName)) then
        !> check for fieldName in state
        call ESMF_StateGet(state,potentialFieldName,itemFlag,rc=rc)
        if (itemFlag == ESMF_STATEITEM_NOTFOUND) then
          !> create field
          field = ESMF_FieldCreate(grid,typekind=typeKind,name=potentialFieldName,rc=rc)
          !> append field to state
         call ESMF_StateAdd(state,(/ field /),rc=rc)
        else
          write(0,*) 'item ',trim(potentialFieldName),' already present',itemFlag
        end if
        exit
      else
        write(0,*) 'name ',trim(names(idx)),' not matching optional field ',trim(potentialFieldName)
        cycle
      end if
    end do
  end do
  end subroutine create_optional_fields_from_names

end module mossco_state
