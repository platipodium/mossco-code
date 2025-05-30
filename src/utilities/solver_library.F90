!> @brief Implementation of a generic 3D solver library
!! @file solver_library.F90, this file is part of MOSSCO.
!!
!! @author Richard Hofmeister
!! @author Carsten Lemmen <carsten.lemmen@hereon.de
!! @copyright 2021-2022 Helmholtz-Zentrum Hereon
!! @copyright 2013-2021 Helmholtz-Zentrum Geesthacht
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
!> @description The solver library contains the multi-method ode-solver
!! and the base class for model drivers
!!
!! The MOSSCO 3d solver library provides a solver for
!! cell-wise time integration of ODEs. The model drivers
!! are expected to provide a vector with the right-hand sides
!! (tendencies). Each driver has to inherit the base driver class
!! called rhs_driver in this module.

#define AVOID_DIVISION
#define _SHAPE3D_ 1:rhs_driver%inum,1:rhs_driver%jnum,1:rhs_driver%knum

module solver_library

implicit none
private

integer,parameter :: rk=selected_real_kind(13) !< real kind
integer,parameter :: EULER=0
integer,parameter :: RUNGE_KUTTA_4=1
integer,parameter :: ADAPTIVE_EULER=2
integer,parameter :: RUNGE_KUTTA_4_38=3

type, public :: type_rhs_driver !< base driver class
   integer :: inum,jnum,knum
   integer :: nvar
   real(selected_real_kind(13)) :: dt_min=1.d-9 ! minimum timestep
   real(selected_real_kind(13)) :: relative_change_min=-0.9d0 ! minimum relative change
   real(selected_real_kind(13)),dimension(:,:,:,:),pointer :: conc=>null()
   logical, dimension(:,:,:), pointer :: mask=>null()
   real(selected_real_kind(13)) :: last_min_dt=1.e20
   integer                      :: last_min_dt_grid_cell(4)=(/-99,-99,-99,-99/)
   logical                      :: adaptive_solver_diagnostics=.false.
contains
   procedure :: get_rhs => base_get_rhs
end type

public :: ode_solver

contains

!> base function for get_rhs()
!!
!! each driver instance inherits by default a get_rhs subroutine,
!! which returns zeros for the tendencies

subroutine base_get_rhs(rhs_driver,rhs)
   integer, parameter                                 :: rk=selected_real_kind(13)
   class(type_rhs_driver), intent(inout)              :: rhs_driver
   real(rk), intent(inout),dimension(:,:,:,:),pointer :: rhs
   nullify(rhs)
end subroutine base_get_rhs

!> solver for ODEs
!!
!! The solver provides different methods for temporal
!! integration of state variable tendencies. Each driver class
!! contains the array dimensions, a pointer to a concentration array (conc)
!! and the procedure get_rhs in order to get new tendencies (rhs) based on the
!! concentration array.
!!
!! Implemented methods are:
!! 1 - Runge-Kutta 4th-order
!! 2 - Adaptive Euler
!! 3 - Runge-Kutta 4th-order using 3/8 rule

subroutine ode_solver(rhs_driver,dt,method)
implicit none

integer            ,intent(in)        :: method
integer,parameter                     :: rk = selected_real_kind(13)
real(rk)           ,intent(in)        :: dt
class(type_rhs_driver)  ,intent(inout):: rhs_driver

real(rk),dimension(:,:,:,:),pointer :: rhs
real(rk),dimension(_SHAPE3D_,1:rhs_driver%nvar),target :: rhs0,rhs1,rhs2,rhs3
real(rk),target :: c1(_SHAPE3D_,1:rhs_driver%nvar)
real(rk),dimension(:,:,:,:),pointer :: c_pointer
real(rk) :: dt_red, dt_int
#ifndef AVOID_DIVISION
real(rk) ::   relative_change
#endif
real(rk),parameter :: third=1.0_rk/3.0_rk

select case (method)
case(EULER)
   rhs => rhs0
   call rhs_driver%get_rhs(rhs)
   rhs_driver%conc(_SHAPE3D_,:) = rhs_driver%conc(_SHAPE3D_,:) + dt*rhs

case(ADAPTIVE_EULER)
   rhs => rhs0
   dt_int = 0.0_rk
   dt_red = dt
   do while (dt_int .lt. dt)
      c_pointer => rhs_driver%conc
      call rhs_driver%get_rhs(rhs)
      c1 = rhs_driver%conc(_SHAPE3D_,:) + dt_red*rhs

! the avoid-division code is faster
! todo: still faster would be to solve grid cell by grid cell and refine timesteps
!       only locally. get_rhs has to take optional argument then for update of
!       a single grid-cell/water column.
! relative_change_min is a negative number, that gives the lower limit
!   for allowed relative changes (e.g. -0.9 for 90% loss of concentration
!   per timestep.
#ifdef AVOID_DIVISION
      if (any((c1-(1.0_rk+rhs_driver%relative_change_min)*c_pointer(_SHAPE3D_,:))<0.0_rk) &
#else
      relative_change = minval((c1-c_pointer(_SHAPE3D_,:))/c_pointer(_SHAPE3D_,:))
      if ((relative_change < rhs_driver%relative_change_min) &
#endif
              .and. (dt_red> rhs_driver%dt_min)) then
         dt_red = dt_red*0.25_rk
         write(0,*) 'Warning: solver subcycles with dt = ', dt_red
      else
         if (rhs_driver%adaptive_solver_diagnostics) then
           if (dt_red < rhs_driver%last_min_dt) then
             rhs_driver%last_min_dt = dt_red
             rhs_driver%last_min_dt_grid_cell = &
               minloc((c1-c_pointer(_SHAPE3D_,:))/c_pointer(_SHAPE3D_,:))
           end if
         end if
         rhs_driver%conc(_SHAPE3D_,:) = c1
         dt_int = dt_int + dt_red
      end if
   end do

case(RUNGE_KUTTA_4)
   ! Runge-Kutta-4th_order
   c_pointer => rhs_driver%conc
   rhs => rhs0
   call rhs_driver%get_rhs(rhs)
   c1 = c_pointer(_SHAPE3D_,:) + 0.5_rk*dt*rhs

   rhs_driver%conc => c1
   rhs => rhs1
   call rhs_driver%get_rhs(rhs)
   c1 = c_pointer(_SHAPE3D_,:) + 0.5_rk*dt*rhs

   rhs => rhs2
   call rhs_driver%get_rhs(rhs)
   c1 = c_pointer(_SHAPE3D_,:) + dt*rhs

   rhs => rhs3
   call rhs_driver%get_rhs(rhs)
   c_pointer(_SHAPE3D_,:) = c_pointer(_SHAPE3D_,:) + dt*third*(0.5_rk*rhs0 + rhs1 + rhs2 + 0.5_rk*rhs3)

   rhs_driver%conc => c_pointer
   nullify(c_pointer)
case(RUNGE_KUTTA_4_38)
   ! Runge-Kutta-4th_order using 3/8-rule from Kutta (1901)
   c_pointer => rhs_driver%conc
   rhs => rhs0
   call rhs_driver%get_rhs(rhs)
   c1 = c_pointer(_SHAPE3D_,:) + third*dt*rhs0

   rhs_driver%conc => c1
   rhs => rhs1
   call rhs_driver%get_rhs(rhs)
   c1 = c_pointer(_SHAPE3D_,:) + dt*(rhs1 - third*rhs0)

   rhs => rhs2
   call rhs_driver%get_rhs(rhs)
   c1 = c_pointer(_SHAPE3D_,:) + dt* (rhs0 - rhs1 + rhs2)

   rhs => rhs3
   call rhs_driver%get_rhs(rhs)
   c_pointer(_SHAPE3D_,:) = c_pointer(_SHAPE3D_,:) + dt*1.0_rk/8.0_rk*(rhs0 + 3.0_rk*rhs1 + 3.0_rk*rhs2 + rhs3)

   rhs_driver%conc => c_pointer
   nullify(c_pointer)
end select

return
end subroutine ode_solver

end module solver_library
