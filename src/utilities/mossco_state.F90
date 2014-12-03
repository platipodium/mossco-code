module mossco_state

use esmf
implicit none

interface mossco_state_get
    module procedure mossco_state_get_f1
    module procedure mossco_state_get_f2
    module procedure mossco_state_get_f3
end interface

contains

  subroutine mossco_state_get_f2(state,name,fpointer,rc)
    type(ESMF_State), intent(in)              :: state
    character(len=*),dimension(:), intent(in) :: name
    real(ESMF_KIND_R8),pointer,dimension(:,:), intent(inout) :: fpointer
    integer,intent(out) :: rc
        
    type(ESMF_Field) :: field
    integer(ESMF_KIND_I4) :: esmfrc,i
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

  subroutine mossco_state_get_f1(state,name,fpointer,rc)
    type(ESMF_State), intent(in)              :: state
    character(len=*),dimension(:), intent(in) :: name
    real(ESMF_KIND_R8),pointer,dimension(:), intent(inout) :: fpointer
    integer,intent(out) :: rc
        
    type(ESMF_Field) :: field
    integer(ESMF_KIND_I4) :: esmfrc,i
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
  end subroutine mossco_state_get_f1



  subroutine mossco_state_get_f3(state,name,fpointer,lbnd,ubnd,rc)
    type(ESMF_State) :: state
    type(ESMF_Field) :: field
    character(len=*),dimension(:) :: name
    real(ESMF_KIND_R8),pointer,dimension(:,:,:) :: fpointer
    integer(ESMF_KIND_I4) :: esmfrc,i
    integer,intent(out), optional :: rc
    !> output exclusive bounds as lbnd,ubnd
    integer,intent(out),optional    :: ubnd(3),lbnd(3)
    integer                         :: ubnd_(3),lbnd_(3)
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
         call ESMF_FieldGet(field,localde=0,farrayPtr=fpointer, &
                exclusiveUBound=ubnd_, exclusiveLBound=lbnd_,rc=esmfrc)
         if(esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
         if (present(ubnd)) ubnd=ubnd_
         if (present(lbnd)) lbnd=lbnd_
         rc=0
         exit
      end if
    end do
  end subroutine mossco_state_get_f3


  !> set ESMF attributes "required_flag", "required" and "optional" for
  !! an item's name in the importState
  subroutine set_item_flags(state,name,requiredFlag,optionalFlag,requiredRank)
    type(ESMF_State)           :: state
    character(len=ESMF_MAXSTR) :: name,attname
    integer,optional           :: requiredRank
    logical,optional           :: requiredFlag,optionalFlag
    integer                    :: rc
    
    if (present(requiredFlag)) then
      attname=trim(name)//':required'
      call ESMF_AttributeSet(state,name=attname,value=requiredFlag,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    end if
    if (present(requiredRank)) then
      attname=trim(name)//':required_rank'
      call ESMF_AttributeSet(state,name=attname,value=requiredRank,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    end if
    if (present(optionalFlag)) then
      attname=trim(name)//':optional'
      call ESMF_AttributeSet(state,name=attname,value=optionalFlag,rc=rc)
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
  !write(0,*) 'check ',attCount,'attributes for names: ',names 

  do n=1,attCount
    call ESMF_AttributeGet(state,attributeIndex=n,name=attName) 
    !> check for ":optional"
    idx = index(attName,':optional ')
    if (idx>0) then
      call ESMF_AttributeGet(state,name=attName,value=optional,rc=rc)
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
        call ESMF_StateGet(state,potentialFieldName,itemFlag,rc=rc)
        if (itemFlag == ESMF_STATEITEM_NOTFOUND) then
          !> create field
          field = ESMF_FieldCreate(grid,typekind=typeKind,name=potentialFieldName,rc=rc)
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

  subroutine MOSSCO_StateLog(state, rc)
    type(ESMF_State)                :: state
    integer(ESMF_KIND_I4), optional :: rc

    integer(ESMF_KIND_I4)           :: localRc, itemCount, i, rank, j, maxDigits, count, fieldCount
    character(len=ESMF_MAXSTR)      :: fieldName, name, message, string, gridName, attributeName
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
    character(len=ESMF_MAXSTR), allocatable :: characterValueList(:)
      
    if (present(rc)) rc=ESMF_SUCCESS
    
    call ESMF_StateGet(state, name=name, rc=localRc)
    if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    write(message,'(A)')  trim(name)//' attribute '
    call MOSSCO_StateAttributeString(state, message)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)     
    
    call ESMF_StateGet(state, itemCount=itemCount, rc=localRc)
    if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (itemCount==0) then
      write(message,'(A)')  trim(name)//' contains no items'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
      return
    endif
      
    allocate(itemTypeList(itemCount))
    allocate(itemNameList(itemCount))
  
    call ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=itemNameList, &
      rc=localRc)
    if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    do i=1,itemCount
      if (itemtypeList(i) == ESMF_STATEITEM_FIELD) then
        write(message,'(A)')  trim(name)//' field'

        call ESMF_StateGet(state, itemNameList(i), field, rc=localrc)
        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  
        call MOSSCO_FieldString(field, message)
        !!> @todo write out attributes of field  
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        
      elseif (itemtypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
        call ESMF_StateGet(state, itemNameList(i), fieldBundle, rc=localrc)
        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        
        call ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount, rc=localrc)
        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

	      allocate(fieldList(fieldCount),fieldNameList(fieldCount)) 
        call ESMF_FieldBundleGet(fieldBundle, fieldNameList=fieldNameList, fieldList=fieldList, rc=localrc)
        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

	      do j=1, fieldCount	             
          write(message,'(A)')  trim(name)//' field '//trim(itemNameList(i))//'/'
          call MOSSCO_FieldString(fieldList(j), message)
          !!> @todo write out attributes of field  
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
        enddo
        deallocate(fieldList,fieldNameList)
      else
        write(message,'(A)')  trim(name)//' non-field item '//trim(itemNameList(i))//' skipped'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
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
    
    deallocate(itemTypeList)
    deallocate(itemNameList)  
  
  end subroutine MOSSCO_StateLog
     
  subroutine MOSSCO_FieldString(field, message, length_, rc_)
  
    use mossco_strings
    implicit none
    
    type(ESMF_Field), intent(in)                   :: field
    character(len=ESMF_MAXSTR), intent(inout)      :: message
    integer(ESMF_KIND_I4), intent(inout), optional :: length_
    integer(ESMF_KIND_I4), intent(out), optional   :: rc_
    
    integer(ESMF_KIND_I4)   :: rc, length, rank, localrc
    integer(ESMF_KIND_I4)   :: ubnd1(1), ubnd2(2), ubnd3(3), ubnd4(4)
    integer(ESMF_KIND_I4)   :: lbnd1(1), lbnd2(2), lbnd3(3), lbnd4(4)
    character(ESMF_MAXSTR)  :: geomName, name
    type(ESMF_Grid)         :: grid
    
    type(ESMF_GeomType_Flag) :: geomType
    type(ESMF_FieldStatus_Flag) :: fieldStatus
   
    rc=ESMF_SUCCESS
 
    call ESMF_FieldGet(field, name=name, status=fieldStatus, rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
  	write(message,'(A)') trim(message)//' '//trim(name)

    if (fieldStatus==ESMF_FIELDSTATUS_EMPTY) then
     	write(message,'(A)') trim(message)//' (empty) '
     	return
    elseif (fieldStatus==ESMF_FIELDSTATUS_GRIDSET) then
     	write(message,'(A)') trim(message)//' (gridset) '
    endif

    call ESMF_FieldGet(field, geomtype=geomtype, rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    if (geomtype==ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldGet(field, grid=grid, rc=localrc)
      if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_GridGet(grid, name=geomName, rc=localrc)  
      if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
     	write(message,'(A)') trim(message)//' '//trim(geomName)
    elseif (geomtype==ESMF_GEOMTYPE_MESH) then
     	write(message,'(A)') trim(message)//' mesh'
    elseif (geomtype==ESMF_GEOMTYPE_LOCSTREAM) then
     	write(message,'(A)') trim(message)//' locstream'
    elseif (geomtype==ESMF_GEOMTYPE_XGRID) then
     	write(message,'(A)') trim(message)//' xgrid'
    else
      write(0,*) 'ERROR: geomtype not defined'
    endif

    call ESMF_FieldGet(field, rank=rank, rc=localrc)
    if(localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
 
    if (len_trim(message) + 7<=len(message)) write(message,'(A,I1)') trim(message)//' rank ',rank 
  
    !if (rank==1) then
    !  call ESMF_FieldGetBounds(field, localDe=0, exclusiveUBound=ubnd1, exclusiveLBound=lbnd1, rc=rc)
    !elseif (rank==2) then
    !  call ESMF_FieldGetBounds(field, localDe=0, exclusiveUBound=ubnd2, exclusiveLBound=lbnd2, rc=rc)
    !elseif (rank==3) then
    !  call ESMF_FieldGetBounds(field, localDe=0, exclusiveUBound=ubnd3, exclusiveLBound=lbnd3, rc=rc)
    !elseif (rank==4) then
    !  call ESMF_FieldGetBounds(field, localDe=0, exclusiveUBound=ubnd4, exclusiveLBound=lbnd4, rc=rc)
    !else
    !  write(0,*) 'NOT implemented: rank > 4'
    !endif
    
    if (len_trim(message)>len(message)-3) message(len(message)-3:len(message))='.'
    
    length=len_trim(message)	
    if (present(length_)) length_=length
    if (present(rc_)) rc_=rc
  
  end subroutine MOSSCO_FieldString
  
    
  subroutine MOSSCO_StateAttributeString(state, message , length, rc) 

    use mossco_strings
    implicit none
    
    type(ESMF_State), intent(in)                    :: state
    character(len=ESMF_MAXSTR), intent(inout)      :: message
    integer(ESMF_KIND_I4), intent(inout), optional :: length
    integer(ESMF_KIND_I4), intent(out), optional   :: rc

    type(ESMF_TypeKind_Flag)        :: typeKind
    logical, allocatable            :: logicalValueList(:)
    real(kind=ESMF_KIND_R4), allocatable    :: real4ValueList(:)
    real(kind=ESMF_KIND_R8), allocatable    :: real8ValueList(:)
    integer(kind=ESMF_KIND_I4), allocatable :: integer4ValueList(:)
    integer(kind=ESMF_KIND_I8), allocatable :: integer8ValueList(:)
    character(len=ESMF_MAXSTR), allocatable :: characterValueList(:)
    
    integer(ESMF_KIND_I4)   :: rank, localrc, count, i, j, itemCount
    character(ESMF_MAXSTR)  :: attributeName
      
    call ESMF_AttributeGet(state, count=count, rc=localrc)
    if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    do i=1, count
      call ESMF_AttributeGet(state, attributeIndex=1 , name=attributeName, rc=localrc)
      if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      write(message,'(A)')  trim(message)//' '//trim(attributeName)//':'
      
      call ESMF_AttributeGet(state, name=attributeName, typekind=typekind,  itemCount=itemCount, rc=localrc)
      if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
			if (typekind==ESMF_TYPEKIND_Logical) then
			  allocate(logicalValueList(itemCount))
				call ESMF_AttributeGet(state, name=attributeName, valueList=logicalValueList, rc=localrc)
        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        write(message,'(A,L)') trim(message)//' ',logicalValueList(1)
        do j=2, itemCount-1
          write(message,'(A,L)') trim(message)//', ',logicalValueList(j)
        enddo
        deallocate(logicalValueList)
			elseif (typekind==ESMF_TYPEKIND_CHARACTER) then
			  allocate(characterValueList(itemCount))
				call ESMF_AttributeGet(state, name=attributeName, valueList=characterValueList, rc=localrc)
        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        if (len_trim(message) + len_trim(characterValueList(1)) + 1 <= len(message)) then
          write(message,'(A,A)') trim(message)//' ',trim(characterValueList(1))
          do j=2, itemCount-1
            write(message,'(A,A)') trim(message)//', ',trim(characterValueList(j))
          enddo
          deallocate(characterValueList)
        endif
			elseif (typekind==ESMF_TYPEKIND_I4) then
			  allocate(integer4ValueList(itemCount))
				call ESMF_AttributeGet(state, name=attributeName, valueList=integer4ValueList, rc=localrc)
        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        write(message,'(A,I3.3)') trim(message)//' ',integer4ValueList(1)
        do j=2, itemCount-1
          write(message,'(A,I3.3)') trim(message)//', ',integer4ValueList(j)
        enddo
        deallocate(integer4ValueList)
			elseif (typekind==ESMF_TYPEKIND_I8) then
			  allocate(integer8ValueList(itemCount))
				call ESMF_AttributeGet(state, name=attributeName, valueList=integer8ValueList, rc=localrc)
        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        write(message,'(A,I3.3)') trim(message)//' ',integer8ValueList(1)
        do j=2, itemCount-1
          write(message,'(A,I3.3)') trim(message)//', ',integer8ValueList(j)
        enddo
        deallocate(integer8ValueList)
			elseif (typekind==ESMF_TYPEKIND_R4) then
			  allocate(real4ValueList(itemCount))
				call ESMF_AttributeGet(state, name=attributeName, valueList=real4ValueList, rc=localrc)
        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        write(message,'(A,G8.2)') trim(message)//' ',real4ValueList(1)
        do j=2, itemCount-1
          write(message,'(A,G8.2)') trim(message)//', ',real4ValueList(j)
        enddo
        deallocate(real4ValueList)
			elseif (typekind==ESMF_TYPEKIND_R8) then
			  allocate(real8ValueList(itemCount))
				call ESMF_AttributeGet(state, name=attributeName, valueList=real8ValueList, rc=localrc)
        if(localRc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        write(message,'(A,G8.2)') trim(message)//' ',real8ValueList(1)
        do j=2, itemCount-1
          write(message,'(A,G8.2)') trim(message)//', ',real8ValueList(j)
        enddo
        deallocate(real8ValueList)
			endif
    enddo
    
    end subroutine MOSSCO_StateAttributeString
      
    
end module mossco_state
