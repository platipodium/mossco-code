!> @brief Implementation of an ESMF coupling between different grids
!>
!> This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2014, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "regrid_coupler.F90"

module regrid_coupler
    
  use esmf
!  use mossco_state
!  use mossco_field
!  use mossco_component

  implicit none

  private

  public SetServices

  type type_mossco_fields_handle
    type(ESMF_RouteHandle) :: routehandle
    type(ESMF_Field) :: srcField, dstField
    type(ESMF_State) :: srcState, dstState
    type(type_mossco_fields_handle), pointer :: next=>null()
  end type

  class(type_mossco_fields_handle), allocatable, target :: fieldsHandle

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(cplcomp, rc)

    type(ESMF_CplComp)   :: cplcomp
    integer, intent(out) :: rc
    
    integer :: localrc
    
    rc = ESMF_SUCCESS

    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_INITIALIZE, Initialize  &
                                      , rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_RUN,    Run   &
                                      , rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_FINALIZE, Finalize &
                                      , rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "Initialize"
  subroutine Initialize(cplcomp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentclock
    integer, intent(out) :: rc

    type(ESMF_Time)             :: currTime
    integer(ESMF_KIND_I4)       :: petCount, localPet, i, itemCount
    character(len=ESMF_MAXSTR)  :: message, name, timeString, exportName, importName
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    type(ESMf_StateItem_Flag)   :: itemType
    type(ESMF_RouteHandle)      :: routeHandle
    class(type_mossco_fields_handle), pointer :: currHandle=>null() 
    type(ESMF_Field)            :: importField, exportField
    integer                     :: localrc
   
    integer(ESMF_KIND_I4)       :: rank, localDeCount
    type(ESMF_FieldStatus_Flag) :: status
    type(ESMF_Mesh)             :: mesh
    type(ESMF_Grid)             :: grid
    type(ESMF_GeomType_Flag)    :: geomType
    character(ESMF_MAXSTR)      :: geomName
    integer                     :: numOwnedNodes, dimCount
    
    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(CplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Search for all fields that are present in both import and export state, 
    !! for each combination of fields
    !! - if they are defined on different grids, create a route handle and
    !!   name it with the name of the two grids for identification (todo)

    call ESMF_StateGet(exportState, name=exportName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_StateGet(importState, itemCount=itemCount, name=importName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount>0) then
      allocate(itemNameList(itemCount))
      allocate(itemTypeList(itemCount))
      
      call ESMF_StateGet(importState, itemNameList=itemNameList, &
        itemTypeList=itemTypeList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      
      do i=1,itemCount
        if (itemTypeList(i) /= ESMF_STATEITEM_FIELD) then 
          write(message,'(A)') trim(name)//' skipped non-field item '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
          cycle
        endif
     
        call ESMF_StateGet(exportState, itemName=itemNameList(i), itemType=itemType, rc=localrc)   
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   
        if (itemType==ESMF_STATEITEM_NOTFOUND)   then
          write(message,'(A)') trim(name)//' skipped field '//trim(itemNameList(i)) &
            //' (not in '//trim(exportName)//')'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
          cycle
        elseif (itemType/=ESMF_STATEITEM_FIELD) then
          write(message,'(A)') trim(name)//' skipped field '//trim(itemNameList(i)) &
            //' (not a field in '//trim(exportName)//')'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
          cycle
        endif
        
        call ESMF_StateGet(importState, itemNameList(i), importField, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_StateGet(exportState, itemNameList(i), exportField, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        
        if (importField==exportField) then
          write(message,'(A)') trim(name)//' skipped field '//trim(itemNameList(i)) &
            //' (already the same in '//trim(exportName)//')'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
          cycle
        endif
               
        write(message,'(A)') trim(name)//' considering '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      

		    call ESMF_FieldGet(importField, status=status, localDeCount=localDeCount, rank=rank, &
		      geomType=geomType, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (geomType == ESMF_GEOMTYPE_GRID) then
          call ESMF_FieldGet(importField, grid=grid, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_GridGet(grid, dimCount=dimCount, rank=rank, name=geomName, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          write(message,'(A,I1,A,I1,A,I1)') trim(name)//' grid '//trim(geomName)//' rank ',rank,' dimensions ',dimCount
        elseif (geomType == ESMF_GEOMTYPE_MESH) then
           call ESMF_FieldGet(importField, mesh=mesh, rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
           call ESMF_MeshGet(mesh, numOwnedNodes=numOwnedNodes, rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
           write(message,'(A,I5,A)') trim(name)//' mesh with ',numOwnedNodes,' nodes'
        else
           write(message,'(A)') trim(name)//' other geomtype'
           !! ESMF_GEOMTYPE_XGRID ESMF_TYPEKIND_LOCSTREAM
        endif
 
		    call ESMF_FieldGet(exportField, status=status, localDeCount=localDeCount, rank=rank, &
		      geomType=geomType, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (geomType == ESMF_GEOMTYPE_GRID) then
          call ESMF_FieldGet(exportField, grid=grid, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_GridGet(grid, dimCount=dimCount, rank=rank, name=geomName, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          write(message,'(A,I1,A,I1,A,I1)') trim(message)//' --> grid '//trim(geomName)//' rank ',rank,' dimensions ',dimCount
        elseif (geomType == ESMF_GEOMTYPE_MESH) then
           call ESMF_FieldGet(importField, mesh=mesh, rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
           call ESMF_MeshGet(mesh, numOwnedNodes=numOwnedNodes, rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
           write(message,'(A,I5,A)') trim(message)//' --> mesh with ',numOwnedNodes,' nodes.'
        else
           write(message,'(A)') trim(name)//' --> other geomtype.'
           !! ESMF_GEOMTYPE_XGRID ESMF_TYPEKIND_LOCSTREAM
        endif
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
        
        call ESMF_FieldRegridStore(srcField=importField, dstField=exportField,&
          routeHandle=routehandle,regridmethod=ESMF_REGRIDMETHOD_CONSERVE,rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) then
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
        !! ESMF_FieldRegrid.F90:2018 ESMF_FieldRegridGetIwts Invalid argument 
        !! - - can't currently regrid a grid       that contains a DE of width less than 2

        if (.not.associated(currHandle)) then
          allocate(fieldsHandle)
          currHandle=>fieldsHandle
        endif
        
        do while(associated(currHandle%next)) 
          currHandle=>currHandle%next
        enddo
        allocate (currHandle%next)
        currHandle=>currHandle%next

        currHandle%srcField=importField
        currHandle%dstField=exportField
        currHandle%srcState=importState
        currHandle%dstState=exportState
        currHandle%routeHandle=routeHandle

      enddo
      
    else
      write(message,'(A)') trim(name)//' no couplable fields in '//trim(importName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
    endif  
            
    if (allocated(itemNameList)) deallocate(itemNameList)
    if (allocated(itemTypeList)) deallocate(itemTypeList)
    
    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Initialize

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
 subroutine Run(cplcomp, importState, exportState, parentclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentclock
    integer, intent(out) :: rc

    type(ESMF_Time)             :: currTime
    integer(ESMF_KIND_I4)       :: petCount, localPet, i, itemCount
    character(len=ESMF_MAXSTR)  :: message, name, timeString, exportName, importName
    character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    type(ESMf_StateItem_Flag)   :: itemType
    class(type_mossco_fields_handle), pointer :: currHandle=>null() 
    type(ESMF_Field)            :: importField, exportField
    type(ESMF_RouteHandle)      :: routeHandle
    integer :: localrc
    
    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(CplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(exportState, name=exportName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_StateGet(importState, itemCount=itemCount, name=importName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (itemCount>0) then
      allocate(itemNameList(itemCount))
      allocate(itemTypeList(itemCount))
      
      call ESMF_StateGet(importState, itemNameList=itemNameList, &
        itemTypeList=itemTypeList, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          
      do i=1,itemCount
        if (itemTypeList(i) /= ESMF_STATEITEM_FIELD) then 
          write(message,'(A)') trim(name)//' skipped non-field item '//trim(itemNameList(i))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
          cycle
        endif
     
        call ESMF_StateGet(exportState, itemName=itemNameList(i), itemType=itemType, rc=localrc)   
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   
        if (itemType==ESMF_STATEITEM_NOTFOUND) then 
          write(message,'(A)') trim(name)//' skipped field '//trim(itemNameList(i)) &
            //' (not in '//trim(exportName)//')'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
          cycle
        elseif (itemType/=ESMF_STATEITEM_FIELD) then
          write(message,'(A)') trim(name)//' skipped field '//trim(itemNameList(i)) &
            //' (not a field in '//trim(exportName)//')'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
          cycle
        endif
        
        call ESMF_StateGet(importState, itemNameList(i), importField, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_StateGet(exportState, itemNameList(i), exportField, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        
        if (importField==exportField) then
          write(message,'(A)') trim(name)//' skipped field '//trim(itemNameList(i)) &
            //' (already the same in '//trim(exportName)//')'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
          cycle
        endif

        !! search for the correct routeHandle
        currHandle=>fieldsHandle
        do while (associated(currHandle%next))
          if (.not.((currHandle%srcField==importField).and.(currHandle%dstField==exportField))) &
            currHandle=>currHandle%next
        enddo
        routeHandle=currHandle%routeHandle
        
        call ESMF_FieldRegrid(srcField=importField, dstField=exportField,&
          routeHandle=routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        !! ESMF_FieldRegrid.F90:2018 ESMF_FieldRegridGetIwts Invalid argument 
        !! - - can't currently regrid a grid       that contains a DE of width less than 2

      enddo
      
    else
      write(message,'(A)') trim(name)//' no couplable fields in '//trim(importName)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)      
    endif  

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(cplComp, importState, exportState, parentClock, rc)
    
    type(ESMF_CplComp)   :: cplComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc
    integer :: localrc

    class(type_mossco_fields_handle), pointer :: currHandle=>null() 

    integer(ESMF_KIND_I4)   :: petCount, localPet
    character(ESMF_MAXSTR)  :: name, message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock
    
    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(CplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
   
    if (allocated(fieldsHandle)) then
      currHandle=>fieldsHandle
      do while (associated(currHandle%next))
        currHandle=>currHandle%next
        deallocate(fieldsHandle)
      enddo
    endif

    if (clockIsPresent) call ESMF_ClockDestroy(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize
 
end module regrid_coupler

#undef UNITTESTS
#ifdef UNITTESTS
#undef  ESMF_METHOD
#define ESMF_METHOD "test"
program test

  use esmf
  use regrid_coupler, only : SetServices

  implicit none
  
  type(ESMF_State) :: states(2)
  type(ESMF_Field) :: fields(9)
  type(ESMF_Grid)  :: grid34,grid55,grid565
  integer(ESMF_KIND_I4) :: i, rc
  type(ESMF_CplComp) :: coupler
  type(ESMF_Clock)   :: clock
  type(ESMF_TimeInterval) :: timeInterval
  type(ESMF_Time)         :: time
  integer :: localrc
  
  call ESMF_Initialize(defaultLogFileName="test_regrid_coupler", &
    logkindflag=ESMF_LOGKIND_MULTI,defaultCalKind=ESMF_CALKIND_GREGORIAN)
  
  !! Initialize
  do i=1,2
   states(i)=ESMF_StateCreate()
  enddo
  
  call ESMF_TimeSet(time, yy=2014)
  call ESMF_TimeSyncToRealTime(time,rc=localrc)
  call ESMF_TimeIntervalSet(timeInterval, d=1)
  clock=ESMF_ClockCreate(timeInterval, time)
  
  grid34=ESMF_GridCreate(maxIndex=(/3,4/))
  grid55=ESMF_GridCreate(maxIndex=(/5,5/))
  grid565=ESMF_GridCreate(maxIndex=(/5,6,5/))


  fields(1)=ESMF_FieldCreate(grid34, typekind=ESMF_TYPEKIND_R8, name="field1")
  fields(2)=ESMF_FieldCreate(grid34, typekind=ESMF_TYPEKIND_R8, name="field2")
  fields(3)=ESMF_FieldCreate(grid34, typekind=ESMF_TYPEKIND_R8, name="field3")
  fields(4)=ESMF_FieldCreate(grid55, typekind=ESMF_TYPEKIND_R8, name="field4")
  fields(5)=ESMF_FieldCreate(grid565, typekind=ESMF_TYPEKIND_R8, name="field5")

  !! Identical field
  fields(6)=ESMF_FieldCreate(grid34, typekind=ESMF_TYPEKIND_R8, name="field1")

  !! Same name, different grid, same rank
  fields(7)=ESMF_FieldCreate(grid55, typekind=ESMF_TYPEKIND_R8, name="field2")
  
  !! Same name, same grid, different type
  fields(8)=ESMF_FieldCreate(grid34, typekind=ESMF_TYPEKIND_I8, name="field3")
  
  !! Same name, different rank
  fields(9)=ESMF_FieldCreate(grid565, typekind=ESMF_TYPEKIND_I8, name="field4")
  
  
  coupler=ESMF_CplCompCreate(name="coupler")
  call ESMF_CplCompSetServices(coupler, SetServices, rc=localrc)

  !! Run tests

  call ESMF_StateAdd(states(1),fields(1:5)) 
  call ESMF_StateAdd(states(2),fields(6:9))

  call ESMF_StatePrint(states(1))
  call ESMF_StatePrint(states(2))

  call ESMF_CplCompInitialize(coupler, importState=states(1), exportState=states(2), clock=clock)
  !call ESMF_CplCompRun(coupler, importState=states(1), exportState=states(2))

  !! Cleanup
  do i=1,ubound(fields,1)
    call ESMF_FieldDestroy(fields(i))
  enddo

  do i=1,ubound(states,1)
    call ESMF_StateDestroy(states(i))
  enddo
  
  call ESMF_GridDestroy(grid34)
  call ESMF_GridDestroy(grid55)
  call ESMF_GridDestroy(grid565)

  call ESMF_Finalize()  

end program test
#endif
