!> @brief Implementation of a GOTM transport ESMF component
!
!> This module contains advection and diffusion in the 1D GOTM context
!! for all 1D fields in the importState.
!> @import 1D tracer fields
!> @export transported 1D tracer fields
!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2013, 2014, Helmholtz-Zentrum Geesthacht 
!> @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister, Helmholtz-Zentrum Geesthacht
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

module gotm_transport_component

  use esmf
  use mossco_strings
  
  implicit none

  private
 
  integer, parameter :: rk=selected_real_kind(13)
  type :: type_data_ptr
    real(rk), dimension(:,:,:), pointer :: conc
    real(rk), dimension(:,:,:), pointer :: ws
    integer :: n
  end type

  type(type_data_ptr),dimension(:),pointer :: tracer
  type(ESMF_Clock) :: clock

#define GOTM_REALTYPE real(kind=selected_real_kind(13))
#define _ZERO_ 0.0d0
#define _ONE_  1.0d0

  !> local variables for the setup control
  character(len=80)         :: title,name
  GOTM_REALTYPE             :: cnpar,latitude,longitude,depth
  GOTM_REALTYPE             :: T0,S0,p0,dtr0,dsr0
  integer                   :: buoy_method,eq_state_mode,eq_state_method
  GOTM_REALTYPE,allocatable :: ones(:),zeros(:),relaxTau(:)

    
  public :: SetServices
  
  contains

  !> Provide an ESMF compliant SetServices routine, which defines
  !! the entry points for Init/Run/Finalize
  subroutine SetServices(gridcomp, rc)
  
    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    rc=ESMF_SUCCESS
    
  end subroutine SetServices

  !> Initialize the component
  !!
  !! create list of pointers to the 1D fields in the importState;
  !! the exportState is not used
  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)
    use meanflow, only : gotm_heights => h 
    implicit none

    type(ESMF_GridComp)         :: gridComp
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Clock)            :: parentClock
    integer, intent(out)        :: rc

    character(len=19)           :: timestring
    character(len=ESMF_MAXSTR)  :: varname
    real(ESMF_KIND_R8)          :: dt
    integer                     :: lbnd(3), ubnd(3),farray_shape(3)
    integer                     :: i,ii,namelen
    integer                     :: nlev
    integer                     :: itemcount, fieldCount
    type(ESMF_StateItem_Flag), allocatable, dimension(:)  :: itemTypeList
    character(len=ESMF_MAXSTR), allocatable, dimension(:) :: itemNameList
    type(ESMF_Field), dimension(:), allocatable  :: fieldList, wsFieldlist
    type(ESMF_Field)            :: field,concfield,wsfield
    type(ESMF_FieldBundle)      :: fieldBundle, wsFieldBundle
    type(ESMF_StateItem_Flag)   :: itemType
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farrayPtr
    real(ESMF_KIND_R8)          :: attribute_r8
    character(len=ESMF_MAXSTR)  :: attribute_name
    logical                     :: clockIsPresent, fileIsPresent
    type(ESMF_Time)             :: currTime
    character(len=ESMF_MAXSTR)  :: message
    character(len=ESMF_MAXSTR)  :: configFileName
    type(type_data_ptr)         :: tracer_ptr

      
    !! Check whether there is already a clock (it might have been set 
    !! with a prior ESMF_gridCompCreate() call.  If not, then create 
    !! a local clock as a clone of the parent clock, and associate it
    !! with this component.  Finally, set the name of the local clock
    call ESMF_GridCompGet(gridComp, name=name, clockIsPresent=clockIsPresent, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if (clockIsPresent) then
      call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)     
    else
      clock = ESMF_ClockCreate(parentClock, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_GridCompSet(gridComp, clock=clock, rc=rc)    
    endif
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockSet(clock, name=trim(name)//' clock', rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    !! Log the call to this function
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initializing ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)
   
    !> fill the tracer and ws pointer lists from importState
    call ESMF_StateGet(importState, itemCount=itemCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (itemcount>0) then
      if (.not.allocated(itemTypeList)) allocate(itemTypeList(itemCount))
      if (.not.allocated(itemNameList)) allocate(itemNameList(itemCount))
      
      call ESMF_StateGet(importState, itemTypeList=itemTypeList, &
        itemNameList=itemNameList, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      do i=1,itemCount
        
        if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
          varname=trim(itemNameList(i))
          namelen=len_trim(varname)
          call ESMF_StateGet(importState, varname, concfield, rc=rc) 
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

          if (varname((namelen-8):namelen) == '_in_water') then
           if (namelen>20) then
            if (varname((namelen-19):(namelen-9))=='_z_velocity') cycle
           end if
            call ESMF_StateGet(importState, varname(1:namelen-9)//'_z_velocity_in_water', wsfield, rc=rc)
            ! go to next item, if no *_z_velocity is present
            if (rc /= ESMF_SUCCESS) cycle

            call ESMF_FieldGet(concfield, farrayPtr=tracer_ptr%conc, rc=rc)
            if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT) 
            call ESMF_FieldGet(wsfield, farrayPtr=tracer_ptr%ws, rc=rc)
            if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
            call tracer_append(tracer,tracer_ptr)
          end if


        elseif (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
          varname=trim(itemNameList(i))
          namelen=len_trim(varname)

          if (varname((namelen-8):namelen) == '_in_water') then
           if (namelen>20) then
            if (varname((namelen-19):(namelen-9))=='_z_velocity') cycle
           end if
          call ESMF_StateGet(importState, varname(1:namelen-9)//'_z_velocity_in_water', wsFieldBundle, rc=rc)
          ! go to next item, if no *_z_velocity is present
          if (rc /= ESMF_SUCCESS) cycle

          call ESMF_StateGet(importState, varname, fieldBundle, rc=rc) 
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
          call ESMF_FieldBundleGet(fieldBundle,fieldCount=fieldCount,rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
          allocate(fieldList(fieldCount))
          allocate(wsFieldList(fieldCount))
          call ESMF_FieldBundleGet(fieldBundle,fieldList=fieldList,rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
          call ESMF_FieldBundleGet(wsFieldBundle,fieldList=wsFieldList,rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

          !! go through lists of fields
          do ii=1,size(fieldList)
            call ESMF_FieldGet(fieldList(ii), farrayPtr=tracer_ptr%conc, rc=rc)
            if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
            call ESMF_FieldGet(wsFieldList(ii), farrayPtr=tracer_ptr%ws, rc=rc)
            if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
            call tracer_append(tracer, tracer_ptr)
          end do
          deallocate(fieldList)
          deallocate(wsFieldList)
          end if
        end if
      end do

    end if ! itemcount>0

    !> create helper arrays
    nlev = size(gotm_heights)-1
    allocate(ones(0:nlev))
    ones=_ONE_
    allocate(zeros(0:nlev))
    zeros=_ZERO_
    allocate(relaxTau(0:nlev))
    relaxTau=1.d15

    call ESMF_LogWrite("FABM/GOTM component initialized.",ESMF_LOGMSG_INFO)
    
  end subroutine Initialize


  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    use util, only: flux, Neumann
    use meanflow, only : w, gotm_heights => h 
    use turbulence, only: diffusivity => nuh

    type(ESMF_GridComp)     :: gridComp
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: parentClock, clock
    integer, intent(out)    :: rc

    character(len=19)       :: timestring
    type(ESMF_Time)         :: wallTime, clockTime, stopTime, currTime
    type(ESMF_TimeInterval) :: timeInterval, timeStep
    integer                 :: i, knum, namelen
    integer(ESMF_KIND_I8)   :: n
    real(ESMF_KIND_R8)      :: dt
    character(len=ESMF_MAXSTR) :: string,varname,message
    integer                 :: w_adv_method=1, w_adv_discr=6, w_adv_ctr=1
    GOTM_REALTYPE           :: cnpar=1.0
    
    integer(ESMF_KIND_I4)   :: localPet, petCount, hours, seconds, minutes
    logical                 :: clockIsPresent

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      clockIsPresent=clockIsPresent, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (.not.clockIsPresent) then
      call ESMF_LogWrite('Required clock not found in '//trim(name), ESMF_LOGMSG_ERROR)
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif
    
    call ESMF_GridCompGet(gridComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_ClockGet(clock,currTime=currTime,  timeStep=timeInterval, &
      stopTime=stopTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' running with dt='
    
    call ESMF_TimeIntervalGet(timeInterval, s_r8=dt, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A,F6.1,A)') trim(message),dt,' s to '
    call ESMF_TimeGet(stopTime,timeStringISOFrac=timestring, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(message)//' '//trim(timeString)//' ...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

    ! get number of layers as 3rd dimension of tracer pointer
    knum = ubound(zeros,1)

    ! @todo implement a solution for short outer timesteps or non-integer number of internal vs outer timesteps
     do while (.not.ESMF_ClockIsStopTime(clock))

       call ESMF_ClockGet(clock,currTime=clockTime, advanceCount=n, rc=rc)
       if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

       ! Vertical advection and residual movement (sinking/floating)
       do i=1,ubound(tracer,1)

         ! Do advection step due to settling or rising
         call adv_center(knum,dt,gotm_heights, gotm_heights, &
           tracer(i)%ws(1,1,:), flux, flux, _ZERO_, _ZERO_, &
           w_adv_discr,1,tracer(i)%conc(1,1,:))

         ! Do advection step due to vertical velocity
         if (w_adv_method/=0) &
           call adv_center(knum,dt,gotm_heights, gotm_heights, &
             w, flux, flux, _ZERO_, _ZERO_, &
             w_adv_ctr,0,tracer(i)%conc(1,1,:))

         ! Vertical diffusion
         call diff_center(knum,dt,cnpar,0,gotm_heights, &
            Neumann, Neumann, _ZERO_, _ZERO_, &
            diffusivity, zeros, zeros, relaxTau, &
            tracer(i)%conc(1,1,:),tracer(i)%conc(1,1,:))
       end do

      call ESMF_ClockAdvance(clock,rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end do ! end of time loop

  end subroutine Run


  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

  end subroutine Finalize


  subroutine tracer_append(tracer, tracerPtr)
  type(type_data_ptr),dimension(:),pointer,intent(inout) :: tracer
  type(type_data_ptr),dimension(:),pointer :: t_
  type(type_data_ptr)                      :: tracerPtr
  integer                                  :: n

  if (.not. associated(tracer)) then
    n = 1
    allocate(tracer(n))
  else
    n = size(tracer)
    allocate(t_(n))
    t_=tracer
    deallocate(tracer)
    allocate(tracer(n+1))
    tracer(1:n) = t_
    deallocate(t_)
    n=n+1
  end if
  tracer(n)=tracerPtr
  end subroutine


end module gotm_transport_component
