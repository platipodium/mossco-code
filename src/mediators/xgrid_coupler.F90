!> @brief Implementation of an ESMF regrid coupling via an exchange grid
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
module xgrid_coupler
    
  use esmf
!  use mossco_state
!  use mossco_field
!  use mossco_component

  implicit none

  private

  public SetServices

  contains

  subroutine SetServices(cplcomp, rc)

    type(ESMF_CplComp)   :: cplcomp
    integer, intent(out) :: rc

    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_INITIALIZE, Initialize  &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_RUN,    Run   &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_FINALIZE, Finalize &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine SetServices


  subroutine Initialize(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)   :: cplComp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer(ESMF_KIND_I4)       :: petCount, localPet, dstDeCount, srcDeCount
    integer(ESMF_KIND_I4)       :: i, itemCount, srcRank, dstRank, dstItemCount, gridRank
    character (len=ESMF_MAXSTR) :: timeString, message, name
    type(ESMF_Time)             :: currTime
    character(len=ESMF_MAXSTR), dimension(:), allocatable, save :: itemNameList
    type(ESMF_StateItem_Flag),  dimension(:), allocatable, save :: itemTypeList
    type(ESMF_StateItem_Flag)   :: itemType
    type(ESMF_Field)            :: srcField, dstField, field
    type(ESMF_Grid)             :: srcGrid, dstGrid
    type(ESMF_XGrid)            :: xgrid
    type(ESMF_VM)               :: vm
    type(ESMF_RouteHandle)      :: rhList(3)
    real(ESMF_KIND_R8), pointer  :: farrayPtr1(:), farrayPtr2(:,:), farrayPtr3(:,:,:)
    type(ESMF_CoordSys_Flag)    :: coordSys
    
    call MOSSCO_CompEntry(CplComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
    ! Need to reconcile import and export states
    call ESMF_StateReconcile(importState, vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_StateReconcile(exportState, vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_StateGet(importState, itemCount=itemCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (.not.allocated(itemTypeList)) allocate(itemTypeList(itemCount))
    if (.not.allocated(itemNameList)) allocate(itemNameList(itemCount))

    call ESMF_StateGet(importState, itemTypeList=itemTypeList, &
      itemNameList=itemNameList, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    do i=1, itemCount
      if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then
        !! Search for this field in exportState
        !> @ todo what if more names are found?
        call ESMF_StateGet(exportState, itemSearch=trim(itemNameList(i)), &
          itemCount=dstItemCount, rc=rc)
        if (itemCount==0) then
          write(message,'(A,A)') trim(name)//' skipped field '//trim(itemNameList(i)), &
            ' in import state; it is not in export state.'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)   
          cycle
        endif
        call ESMF_StateGet(exportState, itemName=trim(itemNameList(i)), &
          itemType=itemType, rc=rc)        
        if (itemType/=ESMF_STATEITEM_FIELD) then
          write(message,'(A,A)') trim(name)//' skipped field '//trim(itemNameList(i)), &
            ' in import state; it is not a field in export state.'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)   
          cycle
        endif
          
        !! Found the field in export State, now deal with this by extracting
        !! the grids of src and dst fields     
        call ESMF_StateGet(importState, trim(itemNameList(i)), srcField, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_StateGet(importState, trim(itemNameList(i)), dstField, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldGet(srcField, grid=srcGrid, rank=srcRank, localDeCount=srcDeCount, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_FieldGet(dstField, grid=dstGrid, rank=dstRank, localDeCount=dstDeCount, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        
        !! Check whether ranks agree
        !> @todo check whether type agrees
        if (srcRank /= dstRank) then
           write(message,'(A,A)') trim(name)//' skipped field '//trim(itemNameList(i)), &
            ' in import state; rank disagrees with field in export state.'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)   
          cycle
        endif
        
        if (srcRank > 3) then
           write(message,'(A,A)') trim(name)//' skipped field '//trim(itemNameList(i)), &
            ' in import state; rank > 3 not implemented'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)   
          cycle
        endif
        
        if (dstDeCount < 2) then
           write(message,'(A,A)') 'Skipped field '//trim(itemNameList(i)), &
            ' in export state; xgrid not implemented for deCount=1'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)   
          cycle
        endif
        if (srcDeCount < 2) then
           write(message,'(A,A)') 'Skipped field '//trim(itemNameList(i)), &
            ' in import state; xgrid not implemented for deCount=1'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)   
          cycle
        endif


        call ESMF_GridGet(srcGrid, coordSys=coordSys, rank=gridRank, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        
        if ((coordSys == ESMF_COORDSYS_SPH_DEG) .and. (gridRank>2)) then
           write(message,'(A,A)') 'Skipped field '//trim(itemNameList(i)), &
            ' in import state; regridding not implemented for 3D spherical fields'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)   
          cycle
        endif

        call ESMF_GridGet(dstGrid, coordSys=coordSys, rank=gridRank, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        
        if ((coordSys == ESMF_COORDSYS_SPH_DEG) .and. (gridRank>2)) then
           write(message,'(A,A)') 'Skipped field '//trim(itemNameList(i)), &
            ' in export state; regridding not implemented for 3D spherical fields'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)   
          cycle
        endif
       
       
        xgrid = ESMF_XGridCreate(sideAGrid=(/srcGrid/), sideBGrid=(/dstGrid/), rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        field = ESMF_FieldCreate(xgrid, typekind=ESMF_TYPEKIND_R8, &
          name='x::'//trim(itemNameList(i)), rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        
        if (srcRank==1) then 
           call ESMF_FieldGet(field, farrayPtr=farrayPtr1, rc=rc)
           if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
           farrayPtr1(:)=0.0
        elseif (srcRank==2) then 
           call ESMF_FieldGet(field, farrayPtr=farrayPtr1, rc=rc)
           if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
           farrayPtr2(:,:)=0.0
        elseif (srcRank==3) then 
           call ESMF_FieldGet(field, farrayPtr=farrayPtr1, rc=rc)
           if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
           farrayPtr3(:,:,:)=0.0
        endif

        call ESMF_FieldRegridStore(xgrid, srcField, field, &
          routehandle=rhList(1), rc=rc)
        call ESMF_RoutehandleSet(rhList(1), name='f2x:://trim(itemNameList(i))', rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        call ESMF_FieldRegridStore(xgrid, field, dstField, &
          routehandle=rhList(3), rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_RoutehandleSet(rhList(3), name='x2f:://trim(itemNameList(i))', rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        call ESMF_StateAdd(exportState, (/rhList(1),rhList(3)/), rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        call ESMF_StateAdd(exportState, (/field/), rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        
      else
        write(message,'(A)') 'Did not setup regrid for non-field item '//trim(itemNameList(i))
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)            
      endif   
    enddo

    call MOSSCO_CompExit(CplComp, rc)

  end subroutine Initialize

  !> the Run() routine of this coupler copies all fields that are found
  !! in the importState into the exportState.  If the field exists in the
  !! exportState, then it will be replaced. 
  subroutine Run(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)   :: cplComp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer(ESMF_KIND_I4)       :: petCount, localPet
    integer(ESMF_KIND_I4)       :: i, itemCount
    character (len=ESMF_MAXSTR) :: timeString, message, name
    type(ESMF_Time)             :: currTime
    character(len=ESMF_MAXSTR), dimension(:), allocatable, save :: itemNameList
    character(len=ESMF_MAXSTR)  :: itemName
    type(ESMF_StateItem_Flag),  dimension(:), allocatable, save :: itemTypeList
    type(ESMF_Field)            :: field, srcField, dstField
    type(ESMF_RouteHandle)      :: rhList(2)


    call MOSSCO_CompEntry(CplComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

	  !! Search the export state for fields with 'x::' prefix
    call ESMF_StateGet(exportState, itemCount=itemCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (.not.allocated(itemTypeList)) allocate(itemTypeList(itemCount))
    if (.not.allocated(itemNameList)) allocate(itemNameList(itemCount))

    call ESMF_StateGet(exportState, itemTypeList=itemTypeList, &
      itemNameList=itemNameList, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    do i=1, itemCount
      itemName=trim(itemNameList(i))
      if (itemName(1:3) /= 'x::') cycle
      
      !> We skip all the check of itemType, rank, etc as this should have been
      !! ensured by the Initialize method
      call ESMF_StateGet(importState,  trim(itemName(4:ESMF_MAXSTR)), srcField, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateGet(exportState,  trim(itemName(4:ESMF_MAXSTR)), dstField, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateGet(exportState,  trim(itemName), field, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
      call ESMF_StateGet(exportState, 'f2x::'//trim(itemName(4:ESMF_MAXSTR)), routehandle=rhList(1), rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateGet(exportState, 'x2f::'//trim(itemName(4:ESMF_MAXSTR)), routehandle=rhList(2), rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_FieldRegrid(srcField, field, routehandle=rhList(1), zeroregion=ESMF_REGION_EMPTY, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_FieldRegrid(field, dstField, routehandle=rhList(2), rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
    enddo

    call MOSSCO_CompExit(CplComp, rc)

  end subroutine Run

  !> The Finalize() routine of this coupler is empty
  subroutine Finalize(cplComp, importState, exportState, parentClock, rc)

    type(ESMF_CplComp)   :: cplComp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc
    
    type(ESMF_Time)      :: currTime
    character(ESMF_MAXSTR) :: name
    
    call MOSSCO_CompEntry(CplComp, parentClock, name, currTime, rc)
    call MOSSCO_CompExit(CplComp, rc)
    
  end subroutine Finalize

end module xgrid_coupler

