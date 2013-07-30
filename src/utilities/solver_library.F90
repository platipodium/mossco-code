!#include "fabm_driver.h"
#define _GRID_ sed%grid
#define _INUM_ _GRID_%inum
#define _JNUM_ _GRID_%jnum
#define _KNUM_ _GRID_%knum

#define _RK4_ 1
#define _ADAPTIVE_EULER_ 2

module solver_library

implicit none
private

public :: ode_solver

contains

subroutine ode_solver(sed,bdys,fluxes,dt,method,get_rhs)
use fabm_sediment_driver
implicit none

integer            ,intent(in)   :: method
integer,parameter :: rk = selected_real_kind(12)
real(rk)           ,intent(in)   :: dt
type(type_sed)     ,intent(inout):: sed
real(rk)           ,intent(in)   :: bdys(1:_INUM_,1:_JNUM_,1:sed%nvar+1)
real(rk)           ,intent(inout):: fluxes(1:_INUM_,1:_JNUM_,1:sed%nvar)


interface
   subroutine get_rhs(sed,bdys,fluxes,rhs)
   use fabm_sediment_driver, only: type_sed
      integer, parameter                   :: rk=selected_real_kind(12)
      type(type_sed), intent(inout)        :: sed
      real(rk), intent(in)                 :: bdys(1:_INUM_,1:_JNUM_,1:sed%nvar+1)
      real(rk), intent(inout)              :: fluxes(1:_INUM_,1:_JNUM_,1:sed%nvar)
      real(rk), intent(out)                :: rhs(1:_INUM_,1:_JNUM_,1:_KNUM_,1:sed%nvar)
   end
end interface

logical  :: first
real(rk),dimension(1:1,1:1,1:_KNUM_,1:sed%nvar) :: rhs,rhs1,rhs2,rhs3
real(rk),target :: c1(1:1,1:1,1:_KNUM_,1:sed%nvar)
real(rk),dimension(:,:,:,:),pointer :: c_pointer
integer  :: i,ci
real(rk) :: dt_red,dt_int,relative_change


select case (method)
case(_ADAPTIVE_EULER_)
   dt_int = 0.0_rk
   dt_red = dt
   do while (dt_int .lt. dt)
      c_pointer => sed%conc
      call get_rhs(sed,bdys,fluxes,rhs)
      c1 = sed%conc + dt_red*rhs

      relative_change = minval((c1-c_pointer)/c_pointer)
      if (relative_change < -0.9_rk) then ! 90% tolerance in reduction 
         dt_red = dt_red/4
      else
         c_pointer = c1
         sed%conc => c_pointer
         dt_int = dt_int + dt_red
      end if
   end do

case(_RK4_)
   ! Runge-Kutta-4th_order
   first=.true.
   c_pointer => sed%conc
   call get_rhs(sed,bdys,fluxes,rhs)
   first=.false.
   c1 = sed%conc + dt*rhs

   sed%conc => c1
   call get_rhs(sed,bdys,fluxes,rhs1)
   c1 = c_pointer + dt*rhs1

   call get_rhs(sed,bdys,fluxes,rhs2)
   c1 = c_pointer + dt*rhs2

   call get_rhs(sed,bdys,fluxes,rhs3)
   c_pointer = c_pointer + dt*1_rk/3_rk*(0.5_rk*rhs + rhs1 + rhs2 + 0.5_rk*rhs3)

   sed%conc => c_pointer
   nullify(c_pointer)
end select

return
end subroutine ode_solver

end module solver_library
