!> @file solver_library.F90
!! brief MOSSCO 3d solver library
!!
!! The solver library contains the multi-method ode-solver
!! and the base class for model drivers
!! @author Richard Hofmeister

!> The MOSSCO 3d solver library provides a solver for
!! cell-wise time integration of ODEs. The model drivers
!! are expected to provide a vector with the right-hand sides
!! (tendencies). Each driver has to inherit the base driver class
!! called rhs_driver in this module.
#define _RK4_ 1
#define _ADAPTIVE_EULER_ 2

module solver_library

implicit none
private

integer,parameter :: rk=selected_real_kind(12) !< real kind

type rhs_driver !< base driver class
   integer :: inum,jnum,knum
   integer :: nvar
   real(selected_real_kind(12)),dimension(:,:,:,:),pointer :: conc
contains
   procedure :: get_rhs => base_get_rhs
end type

public :: ode_solver,rhs_driver

contains

!> base function for get_rhs()
!!
!! each driver instance inherits by default a get_rhs subroutine,
!! which returns zeros for the tendencies

subroutine base_get_rhs(rhsd,rhs)
   integer, parameter                   :: rk=selected_real_kind(12)
   class(rhs_driver), intent(inout)     :: rhsd
   real(rk), intent(out)                :: rhs(1:rhsd%inum,1:rhsd%jnum,1:rhsd%knum,1:rhsd%nvar)
   rhs = 0.0d0
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
integer,parameter :: rk = selected_real_kind(12)
real(rk)           ,intent(in)   :: dt
class(rhs_driver)  ,intent(inout):: driver

logical  :: first
real(rk),dimension(1:1,1:1,1:driver%knum,1:driver%nvar) :: rhs,rhs1,rhs2,rhs3
real(rk),target :: c1(1:1,1:1,1:driver%knum,1:driver%nvar)
real(rk),dimension(:,:,:,:),pointer :: c_pointer
integer  :: i,ci
real(rk) :: dt_red,dt_int,relative_change


select case (method)
case(_ADAPTIVE_EULER_)
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
   call driver%get_rhs(rhs)
   first=.false.
   c1 = driver%conc + dt*rhs

   driver%conc => c1
   call driver%get_rhs(rhs1)
   c1 = c_pointer + dt*rhs1

   call driver%get_rhs(rhs2)
   c1 = c_pointer + dt*rhs2

   call driver%get_rhs(rhs3)
   c_pointer = c_pointer + dt*1_rk/3_rk*(0.5_rk*rhs + rhs1 + rhs2 + 0.5_rk*rhs3)

   driver%conc => c_pointer
   nullify(c_pointer)
end select

return
end subroutine ode_solver

end module solver_library
