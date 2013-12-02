!> @brief Implementation of an ESMF component that delivers constant data fields
!
!> @import 
!> @export water_temperature, salinity
!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2013, Helmholtz-Zentrum Geesthacht 
!> @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
module constant_component

  use esmf
  use mossco_variable_types

  implicit none

  private
  type(MOSSCO_VariableFArray3d), dimension(:), allocatable :: export_variables
  real(ESMF_KIND_R8), allocatable, target :: variables(:,:,:,:)

  public SetServices

  contains

  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine SetServices

  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(ESMF_MAXSTR)     :: name, message
    type(ESMF_Clock)      :: clock
    type(ESMF_Alarm)      :: alarm
    type(ESMF_Time)       :: time
    type(ESMF_TimeInterval) :: timeInterval, alarmInterval

    integer(ESMF_KIND_I4) :: nexport,lbnd(3),ubnd(3),farray_shape(3)
    integer(ESMF_KIND_I4) :: i,j,k
    type(ESMF_Field), dimension(:), allocatable :: exportField
    type(ESMF_Grid)                             :: grid
    type(ESMF_DistGrid)                         :: distgrid
    type(ESMF_ArraySpec)                        :: arrayspec
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farrayPtr  

    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/2,2,2/), &
      regDecomp=(/1,1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
      name="constants grid",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
      coorddep2=(/2/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    ! Get information to generate the fields that store the pointers to variables
    call ESMF_GridGet(grid,distgrid=distgrid,rc=rc)
    call ESMF_GridGetFieldBounds(grid=grid,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER,&
      totalCount=farray_shape,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !> Create export fields and add them to export state, allocate the space for these
    !> that will be filled later with data
    nexport=2
    allocate(export_variables(nexport))
    export_variables(1)%standard_name="water_temperature"
    export_variables(2)%standard_name="salinity"
    allocate(exportField(nexport))
    allocate(variables(farray_shape(1),farray_shape(2),farray_shape(3),nexport))

    call ESMF_ArraySpecSet(arrayspec, rank=3, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    do k=1,size(exportField)
      exportField(k) = ESMF_FieldCreate(grid, arrayspec, name=export_variables(k)%standard_name, &
        staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_StateAddReplace(exportState,(/exportField(k)/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    enddo

    !> Specify water temperature information
    call ESMF_FieldGet(field=exportField(1), localDe=0, farrayPtr=farrayPtr, &
                       totalLBound=lbnd,totalUBound=ubnd, rc=rc) 
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    variables(:,:,:,1) =  15.15
    farrayPtr=variables(:,:,:,1)        
    
    !> Specify salinity information
    call ESMF_FieldGet(field=exportField(2), localDe=0, farrayPtr=farrayPtr, &
                       totalLBound=lbnd,totalUBound=ubnd, rc=rc) 
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    variables(:,:,:,2) =  25.25
    farrayPtr=variables(:,:,:,2)        
 
    call ESMF_GridCompGet(gridComp,name=name, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        
    write(message,'(A,A,A)') 'Constant component ', trim(name), ' initialized'


    call ESMF_LogWrite(message,ESMF_LOGMSG_INFO) 

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer               :: petCount, localPet
    character(ESMF_MAXSTR)     :: name, message
    type(ESMF_Field) :: field
    integer               :: lbnd(3),ubnd(3)
    real(ESMF_KIND_R8), pointer, dimension(:,:,:) :: farrayPtr
    
    
    call ESMF_StateGet(exportState,"water_temperature", field, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    call ESMF_FieldGet(field=field, localDe=0, farrayPtr=farrayPtr, &
                       totalLBound=lbnd,totalUBound=ubnd, rc=rc) 
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    farrayPtr=variables(:,:,:,1)        
 
    call ESMF_StateGet(exportState,"salinity", field, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    call ESMF_FieldGet(field=field, localDe=0, farrayPtr=farrayPtr, &
                       totalLBound=lbnd,totalUBound=ubnd, rc=rc) 
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    farrayPtr=variables(:,:,:,2)        
 
    

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name)
    write(message,'(A,A,A)') 'Constant component ', name, ' finished running'
    call ESMF_LogWrite(message,ESMF_LOGMSG_INFO) 
 
  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer               :: petCount, localPet
    character(ESMF_MAXSTR)     :: name, message

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name)
    write(message,'(A,A,A)') 'Constant component ', name, ' finalized'
    call ESMF_LogWrite(message,ESMF_LOGMSG_INFO) 
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

end module constant_component
