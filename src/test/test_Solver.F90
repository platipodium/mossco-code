!> @file test_Solver.F90
!! @brief Tests the speed of the solver library
!! @author Richard Hofmeister
!!
!! Due to vectorized operations, the solver might be faster on big arrays
!! than a plain loop, both for an Euler integration. 
!! This seems to depend also on the array layout:
!!   (1  ,1 ,24,8) - plain loop faster
!!   (100,1 ,24,8) - solver faster
!!   (1, 100,24,8) - plain loop faster
!! You can try around while setting (_INUM_,_JNUM_,_KNUM_,_VNUM_):
#define _INUM_ 100
#define _JNUM_ 1
#define _KNUM_ 24
#define _VNUM_ 8


module testsolv
use solver_library
implicit none
type, extends(type_rhs_driver) :: type_solv
contains
  procedure :: get_rhs
end type
integer,parameter                     :: rk = selected_real_kind(13)

contains

subroutine get_rhs(rhs_driver, rhs)

class(type_solv)      ,intent(inout)          :: rhs_driver
real(rk),intent(inout),dimension(:,:,:,:),pointer :: rhs
real(rk),target,dimension(1:rhs_driver%inum,1:rhs_driver%jnum,1:rhs_driver%knum,1:rhs_driver%nvar)     :: rhs0
integer               :: i,j,k,v

rhs=0.0d0
do k=1,rhs_driver%knum
   do j=1,rhs_driver%jnum
      do i=1,rhs_driver%inum
        rhs(i,j,k,:)=(i+j+k)*1.0d-8
      end do
   end do
end do
end subroutine get_rhs

end module testsolv


program test_Solver
use solver_library
use testsolv, only: type_solv
implicit none

integer                   :: rc
integer(kind=8)           :: i,j,k,n,nn,v
integer(kind=8)           :: tic1,tic2,tic3,toc1,toc2,toc3,gettimetick
integer(kind=8)           :: alarmtick,totaltick,inittick,looptick,systemclock,ticks_per_sec
real(kind=8)              :: tick_rate

type(type_solv) :: solv

inittick=0
totaltick=0
alarmtick=0
looptick=0
gettimetick=0
nn=0
n=1e5

solv%inum=_INUM_
solv%jnum=_JNUM_
solv%knum=_KNUM_
solv%nvar=_VNUM_
allocate(solv%conc(1:solv%inum,1:solv%jnum,1:solv%knum,1:solv%nvar))
do k=1,solv%knum
  solv%conc(:,:,k,:)=1.0 + k*0.1
end do

call system_clock(tic1)
do nn=1,n
  call ode_solver(solv,1.0d0,0)
end do
call system_clock(toc1)
inittick=toc1-tic1

call system_clock(tic1)
do nn=1,n
 do v=1,solv%nvar
  do k=1,solv%knum
    do j=1,solv%jnum
      do i=1,solv%inum
          solv%conc(i,j,k,v) = solv%conc(i,j,k,v) + 1.0d0*(i+j+k)*1.0d-6
        end do
      end do
    end do
  end do
end do
call system_clock(toc1)
totaltick=toc1-tic1

call system_clock( count=systemclock, count_rate=ticks_per_sec)
tick_rate = 1.0d0/ticks_per_sec

write(0,*) ' finished',n,'timesteps on array shape:'
write(0,*) '(',_INUM_,_JNUM_,_KNUM_,_VNUM_,')'
write(0,*) '----------------------------------------------'
write(0,*) ' ode_solver [s]:',inittick*tick_rate
write(0,*) ' plain loop [s]:',totaltick*tick_rate

end program
