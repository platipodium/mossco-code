!> @brief Implementation of coupler for expressing bottom/surface 2D fields from compartments
!
!> This module vertically integrates with weights grid_height 
!>
!> @import any number of Fields/FieldBundels
!> @export same number with add _at_surface and _at_bottom domain 2D exchange fields
!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2014, Helmholtz-Zentrum Geesthacht 
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

module surfaces_coupler
    
  use esmf
    
  implicit none

  private

  public SetServices

  contains

  subroutine SetServices(cplcomp, rc)

    type(ESMF_CplComp)   :: cplcomp
    integer, intent(out) :: rc

    rc=ESMF_SUCCESS
    
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_INITIALIZE, &
      Initialize , rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_RUN,    Run   &
                                  , rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_FINALIZE, Finalize &
                                      , rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine SetServices

  subroutine Initialize(cplcomp, importState, exportState, externalclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer, intent(out) :: rc
 
    integer(ESMF_KIND_I4)      :: itemCount, i, nlev, srcRank
    integer(ESMF_KIND_I4)      :: totalCount(3)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    type(ESMF_StateItem_Flag)  :: itemType
    character(len=ESMF_MAXSTR), allocatable :: itemNames(:)
    type(ESMF_Field)           :: srcField, dstField
    type(ESMF_Grid)            :: srcGrid, dstGrid
    type(ESMF_Index_Flag)      :: indexFlag
 
    real(ESMF_KIND_R8), pointer:: farrayPtr3d(:,:,:), farrayPtr2d(:,:)
 
    call ESMF_LogWrite("surfaces coupler initializing", ESMF_LOGMSG_INFO)

    call ESMF_StateGet(importState, itemCount=itemCount, nestedFlag=.false., rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    allocate(itemNames(itemCount))
    allocate(itemTypeList(itemCount))

    do i=1,itemCount
      
      call ESMF_StateGet(importState,itemNames(i), itemType, rc=rc)
      if (itemType == ESMF_STATEITEM_NOTFOUND) then
        call ESMF_LogWrite('Could not find item '//trim(itemNames(i))//'.', &
          ESMF_LOGMSG_ERROR)
        cycle
      endif
        
     ! Issue warning for items of type ESMF_FieldBundle (not yet implemented)
     if (itemType == ESMF_STATEITEM_FIELDBUNDLE) then
       call ESMF_LogWrite('Skipped item '//trim(itemNames(i))// &
         ', it is a FieldBundle.',ESMF_LOGMSG_INFO)
       cycle
     endif

     ! Issue warning for items of type ESMF_Array (not yet implemented)
     if (itemType == ESMF_STATEITEM_FIELDBUNDLE) then
       call ESMF_LogWrite('Skipped item '//trim(itemNames(i))// &
         ', it is an Array.',ESMF_LOGMSG_INFO)
       cycle
     endif

     ! Issue warning for items of type ESMF_ArrayBundle (not yet implemented)
     if (itemType == ESMF_STATEITEM_ARRAYBUNDLE) then
       call ESMF_LogWrite('Skipped item '//trim(itemNames(i))// &
         ', it is an ArrayBundle.',ESMF_LOGMSG_INFO)
       cycle
     endif

     ! Only consider items of type ESMF_Field, but this currently does 
     ! not evaluate correctly, so 
     !> @todo evaluate ESMF_STATEITEM_FIELD
     !if (itemType /= ESMF_STATEITEM_FIELD) then
     !  call ESMF_LogWrite('Skipped item '//trim(itemNames(i))//', it is not a field',ESMF_LOGMSG_INFO)
     !  write(*,*) itemNames(i), itemTypeList(i), itemType, ESMF_STATEITEM_FIELD
     !  cycle
     !endif
    
     ! Don't consider items that contain the string 'surface'
      if (index(itemNames(i),'surface')>0) then
        call ESMF_LogWrite('Skipped item '//trim(itemNames(i))//', it is already defined at surface',ESMF_LOGMSG_INFO)
        cycle
      endif

      ! Don't consider items that contain the string 'bottom'
      if (index(itemNames(i),'bottom')>0) then
        call ESMF_LogWrite('Skipped item '//trim(itemNames(i))//', it is already defined at bottom',ESMF_LOGMSG_INFO)
        cycle
      endif
  
      call ESMF_LogWrite("Seriously considering item "//trim(itemNames(i)), ESMF_LOGMSG_INFO)
     
      ! Get the field and its associated grid, if that is a 2D grid cycle
      call ESMF_StateGet(importState, trim(itemNames(i)), srcField, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  
      call ESMF_FieldGet(srcField, grid=srcGrid, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_GridGet(srcGrid, rank=srcRank, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
      if (srcRank == 2) then
        call ESMF_LogWrite('Skipped item '//trim(itemNames(i))//', it is defined on 2D grid already',ESMF_LOGMSG_INFO)
        cycle
      endif
     
      if (srcRank /= 3) then
        call ESMF_LogWrite('Skipped item '//trim(itemNames(i))//', it is not defined on 3D grid',ESMF_LOGMSG_WARNING)
        cycle
      endif
      
      ! Now we're ready to consider this 3D field and create for each 3D field in the 
      ! import state the respective 2D fields in the export state at bottom and surface
      ! First check whether we already have the bottom/surface fields
      
      call ESMF_StateGet(exportState, itemSearch=trim(itemNames(i))//'_at_surface', itemTypeList=itemTypeList, rc=rc)
      if (itemTypeList(1) /= ESMF_STATEITEM_NOTFOUND) then 
        call ESMF_LogWrite('Skipped item '//trim(itemNames(i))//', &
          there is already a corresponding surface field',ESMF_LOGMSG_WARNING)
        call ESMF_StateGet(exportState, itemSearch=trim(itemNames(i))//'_at_bottom', itemTypeList=itemTypeList, rc=rc)
        if (itemTypeList(1) /= ESMF_STATEITEM_NOTFOUND) then 
          call ESMF_LogWrite('Skipped item '//trim(itemNames(i))//', &
            there is already a corresponding bottom field',ESMF_LOGMSG_WARNING)
           cycle
        endif
      endif
      
      call ESMF_LogWrite('Create item '//trim(itemNames(i))//'_at_bottom', &
        ESMF_LOGMSG_INFO)
      call ESMF_LogWrite('Create item '//trim(itemNames(i))//'_at_surface', &
        ESMF_LOGMSG_INFO)

      call ESMF_FieldGet(srcField, farrayPtr=farrayPtr3d, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_FieldGetBounds(srcField, totalCount=totalCount)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      dstGrid = ESMF_GridCreate(minIndex=(/1,1/), maxIndex=(/totalCount(1),totalCount(2)/), rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
      call ESMF_StateGet(exportState, itemSearch=trim(itemNames(i))//'_at_surface', itemTypeList=itemTypeList, rc=rc)
      if (itemTypeList(1) == ESMF_STATEITEM_NOTFOUND) then
        farrayPtr2d = farrayPtr3d(:,:,nlev)
        dstField = ESMF_FieldCreate(dstGrid, name =  trim(itemNames(i))//'_at_surface', &
          farrayPtr=farrayPtr2d, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        call ESMF_StateAdd(exportState, (/dstField/), rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      endif    
     
      call ESMF_StateGet(exportState, itemSearch=trim(itemNames(i))//'_at_bottom', itemTypeList=itemTypeList, rc=rc)
      if (itemTypeList(1) == ESMF_STATEITEM_NOTFOUND) then
        farrayPtr2d => farrayPtr3d(:,:,nlev)
        dstField = ESMF_FieldCreate(dstGrid, name =  trim(itemNames(i))//'_at_bottom', &
          farrayPtr=farrayPtr2d, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

        call ESMF_StateAdd(exportState, (/dstField/), rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      endif    
    enddo 
      
    call ESMF_LogWrite("surfaces coupler initialized", ESMF_LOGMSG_INFO)

  end subroutine Initialize


  subroutine Run(cplcomp, importState, exportState, externalclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer, intent(out) :: rc

    integer                     :: myrank
    type(ESMF_Time)             :: localtime
    character (len=ESMF_MAXSTR) :: timestring
    character (len=ESMF_MAXSTR) :: message
    type(ESMF_Field)            :: srcfield, dstfield
     
! Print timed log message
    call ESMF_CplCompGet(cplcomp, localPet=myrank, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockGet(externalclock, currtime=localtime, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(localtime, timeString=timestring, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    message = "aocpl_run called at "//trim(timestring)
    call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)
    print *, "Proc ",myrank," time=",trim(timestring)

! Get fields from import and export states
    call ESMF_StateGet(importState, "air_temperature_at_surface", srcfield, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_StateGet(exportState, "air_temperature_at_surface", dstfield, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine Run

  subroutine Finalize(cplcomp, importState, exportState, externalclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer, intent(out) :: rc
     
    call ESMF_LogWrite("Vertical mean coupler finalizing", ESMF_LOGMSG_INFO)

    call ESMF_LogWrite("Vertical mean coupler finalized", ESMF_LOGMSG_INFO)
  end subroutine Finalize

end module surfaces_coupler
