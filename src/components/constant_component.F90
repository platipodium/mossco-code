!> @brief Implementation of an ESMF component that delivers constant data fields
!
!> @import 
!> @export water_temperature, salinity
!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2013, 2014, Helmholtz-Zentrum Geesthacht 
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
    type(ESMF_Alarm)      :: alarm
    type(ESMF_Clock)      :: clock
    type(ESMF_Time)       :: time
    type(ESMF_TimeInterval) :: timeInterval, alarmInterval

    integer(ESMF_KIND_I4) :: nexport,lbnd(3),ubnd(3),farray_shape(3)
    integer(ESMF_KIND_I4) :: i,j,k
    type(ESMF_Field), dimension(:), allocatable :: exportField
    type(ESMF_Grid)                             :: grid, grid111
    type(ESMF_DistGrid)                         :: distgrid
    type(ESMF_ArraySpec)                        :: arrayspec
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farrayPtr
    character(len=ESMF_MAXSTR)                  :: varname
    real(ESMF_KIND_R8)                          :: floatvalue
    integer, parameter                          :: fileunit=21
    logical                                     :: file_readable=.true.

    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/2,2,2/), &
      regDecomp=(/1,1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
      name="constants grid",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
      coorddep2=(/2/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)


    clock=ESMF_ClockCreate(parentClock, rc=rc)
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

    !> open constant_component.dat
    !! @todo: read filename from configuration namelist/yaml
    open(fileunit,file='constant_component.dat',err=99)

    if (file_readable) then
      do
        !> read constant_component.dat line by line, maybe add rank later
        !! format of each line is:
        !!   some_standard_name  12.345
        read(fileunit,*,end=5) varname,floatvalue

        !> add item to list of constants
        allocate(cur_item%next)
        cur_item => cur_item%next
        cur_item%standard_name=trim(varname)
        allocate(cur_item%data(farray_shape(1),farray_shape(2),farray_shape(3)))
        cur_item%data(:,:,:) = floatvalue
#if 1
        write(0,*) 'constant_component: create field ', &
            trim(varname),' =',floatvalue
#endif
        nullify(cur_item%next)
      end do
    close(fileunit)
    end if
5   continue
99  file_readable=.false.

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
    cur_item%standard_name='dissolved_oxygen'
    allocate(cur_item%data(farray_shape(1),farray_shape(2),farray_shape(3)))
    cur_item%data = 280.0d0 !mmol-O2/m3
    nullify(cur_item%next)

    allocate(cur_item%next)
    cur_item => cur_item%next
    cur_item%standard_name='dissolved_phosphate'
    allocate(cur_item%data(farray_shape(1),farray_shape(2),farray_shape(3)))
    cur_item%data = 1.0d0 !mmol-P/m3
    nullify(cur_item%next)
    
    
    grid111 = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/1,1,1/), &
      regDecomp=(/1,1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
      name="constants grid 1x1x1",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
      coorddep2=(/2/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)


    ! Get information to generate the fields that store the pointers to variables
    call ESMF_GridGetFieldBounds(grid=grid111,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER,&
      totalCount=farray_shape,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    allocate(cur_item%next)
    cur_item => cur_item%next
    cur_item%standard_name='water_depth'
    allocate(cur_item%data(farray_shape(1),farray_shape(2),farray_shape(3)))
    cur_item%data(:,:,:) = 32.40
    nullify(cur_item%next)
    
    allocate(cur_item%next)
    cur_item => cur_item%next
    cur_item%standard_name='wind_x_velocity_at_10m'
    allocate(cur_item%data(farray_shape(1),farray_shape(2),farray_shape(3)))
    cur_item%data(:,:,:) = 7.8
    nullify(cur_item%next)
    
    allocate(cur_item%next)
    cur_item => cur_item%next
    cur_item%standard_name='wind_y_velocity_at_10m'
    allocate(cur_item%data(farray_shape(1),farray_shape(2),farray_shape(3)))
    cur_item%data(:,:,:) = 5.2
    nullify(cur_item%next)


    !> now go through list, create fields and add to exportState
    cur_item => variable_items%next
    do
      !write(0,*) 'set field ',trim(cur_item%standard_name),' to ',cur_item%data(1,1,1)
      if (ubound(cur_item%data,3)>1) then
        cur_item%field = ESMF_FieldCreate(grid,farrayPtr=cur_item%data,name=cur_item%standard_name, &
          staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
      else
        cur_item%field = ESMF_FieldCreate(grid111,farrayPtr=cur_item%data,name=cur_item%standard_name, &
          staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
      endif    
                    
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

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

    integer                :: petCount, localPet
    character(ESMF_MAXSTR) :: name, message, timestring
    logical                :: clockIsPresent
    type(ESMF_Time)        :: currTime, stopTime
    type(ESMF_Clock)       :: clock
    
    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      clockIsPresent=clockIsPresent, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (.not.clockIsPresent) then
      call ESMF_LogWrite('Required clock not found in '//trim(name), ESMF_LOGMSG_ERROR)
      call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=rc)
    endif
    
    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_ClockGet(clock,currTime=currTime, stopTime=stopTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!#ifdef DEBUG
    write(message,'(A)') trim(timestring)//' '//trim(name)//' running ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
!#endif
 
    call ESMF_ClockAdvance(clock, timeStep=stopTime-currTime, rc=rc) 
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

!#ifdef DEBUG 
    write(message,'(A,A)') trim(timeString)//' '//trim(name), &
          ' finished running.'
    call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO, rc=rc);
!#endif

  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer                :: petCount, localPet
    character(ESMF_MAXSTR) :: name, message
    type(ESMF_Clock)       :: clock

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,clock=clock, name=name, rc=rc)
    call ESMF_ClockDestroy(clock, rc=rc)
    call ESMF_GridCompDestroy(gridComp, rc=rc)
    write(message,'(A,A,A)') 'Constant component ', trim(name), ' finalized'
    call ESMF_LogWrite(message,ESMF_LOGMSG_INFO) 
   
    rc=ESMF_SUCCESS

  end subroutine Finalize

end module constant_component
