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
  integer                   :: nlev
  GOTM_REALTYPE             :: cnpar,latitude,longitude,depth
  GOTM_REALTYPE             :: T0,S0,p0,dtr0,dsr0
  integer                   :: buoy_method,eq_state_mode,eq_state_method

    
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
        
          if (varname((namelen-11):namelen)=='_z_velocity') cycle
         call ESMF_StateGet(importState, trim(varname)//'_z_velocity', wsfield, rc=rc)
          ! go to next item, if no *_z_velocity is present
          if (rc /= ESMF_SUCCESS) cycle

          call ESMF_FieldGet(concfield, farrayPtr=tracer_ptr%conc, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT) 
          call ESMF_FieldGet(wsfield, farrayPtr=tracer_ptr%ws, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
          call tracer_append(tracer,tracer_ptr)


        elseif (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
          varname=trim(itemNameList(i))
          namelen=len_trim(varname)

          if (varname((namelen-10):namelen)=='_z_velocity') cycle
          call ESMF_StateGet(importState, trim(varname)//'_z_velocity', wsFieldBundle, rc=rc)
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
      end do

    end if ! itemcount>0

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

    ! @todo implement a solution for short outer timesteps or non-integer number of internal vs outer timesteps
     do while (.not.ESMF_ClockIsStopTime(clock))

       call ESMF_ClockGet(clock,currTime=clockTime, advanceCount=n, rc=rc)
       if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

       ! Vertical advection and residual movement (sinking/floating)
       do i=1,size(tracer)

         ! get number of layers as 3rd dimension of tracer pointer
         knum = ubound(tracer(i)%conc,3)

         ! Do advection step due to settling or rising
         call mossco_adv_center(knum,dt,gotm_heights, &
           tracer(i)%ws, w_adv_discr,tracer(i)%conc(1,1,:))

         ! Do advection step due to vertical velocity
         if (w_adv_method/=0) call mossco_adv_center(knum,dt,gotm_heights, &
                                w(1:knum), w_adv_ctr,tracer(i)%conc(1,1,:))
       end do

       ! Vertical diffusion
       do i=1,size(tracer)
         call mossco_diff_center(knum,dt,cnpar,gotm_heights, &
            diffusivity,tracer(i)%conc(1,1,:))
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



   subroutine mossco_diff_center(N,dt,cnpar,h,nuY,Y)
   use util,          only  : Dirichlet, Neumann
   use mtridiagonal

!  number of vertical layers
   integer,  intent(in)                :: N

!  time step (s)
   GOTM_REALTYPE, intent(in)           :: dt

!  "implicitness" parameter
   GOTM_REALTYPE, intent(in)           :: cnpar

!  layer thickness (m)
   GOTM_REALTYPE, intent(in)           :: h(0:N)

!  diffusivity of Y
   GOTM_REALTYPE, intent(in)           :: nuY(0:N)

! !INPUT/OUTPUT PARAMETERS:
   GOTM_REALTYPE                       :: Y(1:N)

!  Original author(s): Lars Umlauf
   integer                        :: i
   GOTM_REALTYPE                  :: YY(0:N)
   GOTM_REALTYPE                  :: a,c,l

   YY(1:N) = Y

   !  set up matrix
   do i=2,N-1
      c     = 2.0*dt*nuY(i)  /(h(i)+h(i+1))/h(i)
      a     = 2.0*dt*nuY(i-1)/(h(i)+h(i-1))/h(i)

      cu(i) =-cnpar*c
      au(i) =-cnpar*a
      bu(i) = _ONE_ + cnpar*(a + c)
      du(i) = (_ONE_ - (_ONE_-cnpar)*(a + c))*Y(i)                  &
            + (_ONE_ - cnpar)*( a*Y(i-1) + c*Y(i+1) )
   end do

   ! upper boundary
      a     = 2.0*dt*nuY(N-1)/(h(N)+h(N-1))/h(N)
      au(N) =-cnpar*a
      bu(N) =  _ONE_ - au(N)
      du(N) = Y(N) + (_ONE_ - cnpar)*a*(Y(N-1)-Y(N))

   ! lower boundary
      c     = 2.0*dt*nuY(1)/(h(1)+h(2))/h(1)
      cu(1) =-cnpar*c
      bu(1) = _ONE_ - cu(1)
      du(1) = Y(1) + (_ONE_ - cnpar)*c*(Y(2)-Y(1))

!  solve linear system
   call tridiagonal(N,1,N,YY)
   Y=YY(1:N)
   
   return
   end subroutine mossco_diff_center


   subroutine mossco_adv_center(N,dt,h,ww,method,Y)
      use util
   IMPLICIT NONE
!
! !INPUT PARAMETERS:

!  number of vertical layers
   integer,  intent(in)                :: N

!  time step (s)
   GOTM_REALTYPE, intent(in)                :: dt

!  layer thickness (m)
   GOTM_REALTYPE, intent(in)                :: h(0:N)

!  vertical advection speed
   GOTM_REALTYPE, intent(in)                :: ww(1:N)

   !  type of advection scheme
   integer,  intent(in)                :: method
!
! !INPUT/OUTPUT PARAMETERS:
   GOTM_REALTYPE                            :: Y(1:N)
!
! !DEFINED PARAMETERS:
   GOTM_REALTYPE,     parameter             :: one6th=1.0d0/6.0d0
   integer,      parameter             :: itmax=100
!
! !REVISION HISTORY:
!  Original author(s): Lars Umlauf
!
!EOP
!
! !LOCAL VARIABLES:
   integer                              :: i,k,it
   GOTM_REALTYPE                             :: x,r,Phi,limit
   GOTM_REALTYPE                             :: Yu,Yc,Yd
   GOTM_REALTYPE                             :: c,cmax
   GOTM_REALTYPE                             :: cu(0:N)
!
!-----------------------------------------------------------------------
!BOC

!  initialize interface fluxes with zero
   cu   = _ZERO_

!  initialize maximum Courant number
   cmax = _ZERO_

!  compute maximum Courant number
   do k=1,N-1
      c=abs(ww(k))*dt/(0.5*(h(k)+h(k+1)))
      if (c.gt.cmax) cmax=c
   enddo

   it=min(itmax,int(cmax)+1)

#define STDERR write(0,*)
!#ifdef DEBUG
   if (it .gt. 1) then
      STDERR 'In adv_center():'
      STDERR 'Maximum Courant number is ',cmax
      STDERR it,' iterations used for vertical advection'
   endif
!#endif

!  splitting loop
   do i=1,it

!     vertical loop
      do k=1,N-1

!        compute the slope ration
         if (ww(k) .gt. _ZERO_) then

!           compute Courant number
            c=ww(k)/float(it)*dt/(0.5*(h(k)+h(k+1)))

            if (k .gt. 1) then
               Yu=Y(k-1)                              ! upstream value
            else
               Yu=Y(k)
            end if
            Yc=Y(k  )                                 ! central value
            Yd=Y(k+1)                                 ! downstream value

!           compute slope ration
            if (abs(Yd-Yc) .gt. 1e-10) then
               r=(Yc-Yu)/(Yd-Yc)
            else
               r=(Yc-Yu)*1.e10
            end if

!        negative speed
         else

!           compute Courant number
            c=-ww(k)/float(it)*dt/(0.5*(h(k)+h(k+1)))

            if (k .lt. N-1) then
               Yu=Y(k+2)                              ! upstream value
            else
               Yu=Y(k+1)
            end if
            Yc=Y(k+1)                                 ! central value
            Yd=Y(k  )                                 ! downstream value


!           compute slope ratio
            if (abs(Yc-Yd) .gt. 1e-10) then
               r=(Yu-Yc)/(Yc-Yd)
            else
               r=(Yu-Yc)*1.e10
            end if

         end if

!        compute the flux-factor phi
         x    =  one6th*(1.-2.0*c)
         Phi  =  (0.5+x)+(0.5-x)*r

!        limit the flux according to different suggestions
         select case (method)
            case (UPSTREAM)
               limit=_ZERO_
            case (P1)
               STDERR "P1 advection method not yet implemented, choose other method"
               stop  "adv_center.F90"
            case ((P2),(P2_PDM))
               if (method.eq.P2) then
                  limit=Phi
               else
                  limit=max(_ZERO_,min(Phi,2./(1.-c),2.*r/(c+1.e-10)))
               end if
            case (Superbee)
               limit=max(_ZERO_, min(_ONE_, 2.0*r), min(r,2.*_ONE_) )
            case (MUSCL)
               limit=max(_ZERO_,min(2.*_ONE_,2.0*r,0.5*(1.0+r)))
            case default
               STDERR method
               STDERR 'unkown advection method in adv_center()'
               stop
          end select

!        compute the limited flux
         cu(k)=ww(k)*(Yc+0.5*limit*(1-c)*(Yd-Yc))

      end do


!     do the vertical advection step which will be used for prescribed
!     vertical flow velocity and for settling of suspended matter.
      do k=1,N
         Y(k)=Y(k)-1./float(it)*dt*((cu(k)-cu(k-1))/h(k))
      enddo

   end do ! end of the iteration loop

   return
   end subroutine mossco_adv_center

end module gotm_transport_component
