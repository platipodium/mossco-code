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

  type,extends(MOSSCO_VariableFArray3d) :: variable_item_type
    type(variable_item_type), pointer     :: next => null()
    type(ESMF_Field)                      :: field
  end type

  type(variable_item_type), pointer :: cur_item,variable_items

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

    call ESMF_ArraySpecSet(arrayspec, rank=3, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !> create list of export_variables, that will come from a function
    !> which reads a text file

    allocate(variable_items)
    cur_item => variable_items
    cur_item%next => variable_items

    allocate(cur_item%next)
    cur_item => cur_item%next
    cur_item%standard_name='water_temperature'
    allocate(cur_item%data(farray_shape(1),farray_shape(2),farray_shape(3)))
    cur_item%data(:,:,:) = 15.15d0
    nullify(cur_item%next)
    
    allocate(cur_item%next)
    cur_item => cur_item%next
    cur_item%standard_name='salinity'
    allocate(cur_item%data(farray_shape(1),farray_shape(2),farray_shape(3)))
    cur_item%data = 25.25d0
    nullify(cur_item%next)

    allocate(cur_item%next)
    cur_item => cur_item%next
    cur_item%standard_name='hzg_omexdia_p dissolved oxygen'
    allocate(cur_item%data(farray_shape(1),farray_shape(2),farray_shape(3)))
    cur_item%data = 280.0d0 !mmol-O2/m3
    nullify(cur_item%next)

    allocate(cur_item%next)
    cur_item => cur_item%next
    cur_item%standard_name='hzg_omexdia_p dissolved phosphate'
    allocate(cur_item%data(farray_shape(1),farray_shape(2),farray_shape(3)))
    cur_item%data = 1.0d0 !mmol-P/m3
    nullify(cur_item%next)

    !> now go through list, create fields and add to exportState
    cur_item => variable_items%next
    do
      !write(0,*) 'set field ',trim(cur_item%standard_name),' to ',cur_item%data(1,1,1)
      cur_item%field = ESMF_FieldCreate(grid,arrayspec,name=cur_item%standard_name, &
                         staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_FieldGet(field=cur_item%field, localDe=0, farrayPtr=farrayPtr, &
                       totalLBound=lbnd,totalUBound=ubnd, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      farrayPtr = cur_item%data

      call ESMF_StateAddReplace(exportState,(/cur_item%field/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      if (associated(cur_item%next)) then
        cur_item => cur_item%next
      else
        exit
      end if

    end do

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
    write(message,'(A,A,A)') 'Constant component ', trim(name), ' finished running'
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
    write(message,'(A,A,A)') 'Constant component ', trim(name), ' finalized'
    call ESMF_LogWrite(message,ESMF_LOGMSG_INFO) 
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

end module constant_component
