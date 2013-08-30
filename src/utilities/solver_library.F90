!> @brief Implementation of a generic 3D solver library 
!! @file solver_library.F90, this file is part of MOSSCO.
!! 
!! @author Richard Hofmeister
!! @author Carsten Lemmen
!! @copyright (C) 2013 Helmholtz-Zentrum Geesthacht
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

#define _RK4_ 1
#define _ADAPTIVE_EULER_ 2

module solver_library

implicit none
private

integer,parameter :: rk=selected_real_kind(13) !< real kind

type, public :: rhs_driver !< base driver class
   integer :: inum,jnum,knum
   integer :: nvar
   real(selected_real_kind(13)),dimension(:,:,:,:),pointer :: conc
contains
   procedure :: get_rhs => base_get_rhs
end type

public :: ode_solver

contains

!> base function for get_rhs()
!!
!! each driver instance inherits by default a get_rhs subroutine,
!! which returns zeros for the tendencies

subroutine base_get_rhs(rhsd,rhs)
   integer, parameter                   :: rk=selected_real_kind(13)
   class(rhs_driver), intent(inout)     :: rhsd
   real(rk), intent(out),dimension(:,:,:,:),pointer :: rhs
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

subroutine ode_solver(driver,dt,method)
implicit none

integer            ,intent(in)   :: method
integer,parameter :: rk = selected_real_kind(13)
real(rk)           ,intent(in)   :: dt
class(rhs_driver)  ,intent(inout):: driver

logical  :: first
real(rk),dimension(:,:,:,:),pointer :: rhs
real(rk),dimension(1:1,1:1,1:driver%knum,1:driver%nvar),target :: rhs0,rhs1,rhs2,rhs3
real(rk),target :: c1(1:1,1:1,1:driver%knum,1:driver%nvar)
real(rk),dimension(:,:,:,:),pointer :: c_pointer
integer  :: i,ci
real(rk) :: dt_red,dt_int,relative_change


select case (method)
case(_ADAPTIVE_EULER_)
   rhs => rhs0
   dt_int = 0.0_rk
   dt_red = dt
   do while (dt_int .lt. dt)
      c_pointer => driver%conc
      call driver%get_rhs(rhs)
      c1 = driver%conc + dt_red*rhs

      relative_change = minval((c1-c_pointer)/c_pointer)
      if (relative_change < -0.9_rk) then ! 90% tolerance in reduction 
         dt_red = dt_red/4
      else
         c_pointer = c1
         driver%conc => c_pointer
         dt_int = dt_int + dt_red
      end if
   end do

case(_RK4_)
   ! Runge-Kutta-4th_order
   first=.true.
   c_pointer => driver%conc
   rhs => rhs0
   call driver%get_rhs(rhs)
   first=.false.
   c1 = driver%conc + dt*rhs

   driver%conc => c1
   rhs => rhs1
   call driver%get_rhs(rhs)
   c1 = c_pointer + dt*rhs

   rhs => rhs2
   call driver%get_rhs(rhs)
   c1 = c_pointer + dt*rhs

   rhs => rhs3
   call driver%get_rhs(rhs)
   c_pointer = c_pointer + dt*1_rk/3_rk*(0.5_rk*rhs0 + rhs1 + rhs2 + 0.5_rk*rhs3)

   driver%conc => c_pointer
   nullify(c_pointer)
end select

return
end subroutine ode_solver

end module solver_library
