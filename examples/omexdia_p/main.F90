#include "fabm_driver.h"
#define _GRID_ sed%grid
#define _INUM_ _GRID_%inum
#define _JNUM_ _GRID_%jnum
#define _KNUM_ _GRID_%knum

#define _RK4_ 1
#define _ADAPTIVE_EULER_ 2

program fabmsed1d

use fabm_sediment_driver

implicit none

integer,parameter :: rk = selected_real_kind(12)
real(rk)       :: dt,dzmin
integer        :: tnum,t,funit,output,k,n,numyears,numlayers
integer        :: ode_method
type(type_sed) :: sed
real(rk),dimension(:,:,:,:),allocatable,target :: conc
real(rk),dimension(:,:,:), allocatable         :: bdys,fluxes

namelist/run_nml/ numyears,dt,output,numlayers,dzmin,ode_method

! initialise
dt=60._rk !s
numyears=1
output=86400/int(dt)
numlayers=10
dzmin=0.005
ode_method=_RK4_

open(33,file='run.nml',action='read',status='old')
read(33,nml=run_nml)
tnum=numyears*365*86400/int(dt)
sed%grid%knum=numlayers
sed%grid%dzmin=dzmin

funit=2

write(0,*) '  Initialise grid with',sed%grid%knum,'vertical layers'
call init_sed_grid(sed%grid)


write(0,*) '  Initialise sediment module'
call init_fabm_sed(sed)
close(33)

! allocate concentration array
allocate(conc(sed%grid%inum,sed%grid%jnum,sed%grid%knum,sed%nvar))
! link array conc to fabm_sediment_driver
sed%conc => conc
! initialise values
conc = 0.0_rk
call init_fabm_sed_concentrations(sed)

allocate(bdys(_INUM_,_JNUM_,sed%nvar+1))
bdys(1,1,1:9) = 0.0_rk
bdys(1,1,1) = 10._rk   ! degC temperature
bdys(1,1,5) = 1.0_rk   ! mmolP/m**3 po4
bdys(1,1,6) = 10.0_rk  ! mmolN/m**3 no3
bdys(1,1,7) = 0.0_rk   ! mmolN/m**3 nh3
bdys(1,1,8) = 250.0_rk ! mmolO2/m**3 oxy
bdys(1,1,9) = 0.0_rk   ! odu

allocate(fluxes(_INUM_,_JNUM_,sed%nvar))
fluxes(1,1,1:8) = 0.0_rk
fluxes(1,1,1) = 5.0_rk/86400.0_rk !fdet
fluxes(1,1,2) = 5.0_rk/86400.0_rk !sdet
fluxes(1,1,3) = 0.08/86400.0_rk !pdet

open(funit,file='output.dat')
write(funit,*) 'time(s) ','depth(m) ','conc(n) '

!integrate
do t=1,tnum
   call ode_solver(sed,bdys,fluxes,dt,ode_method,fabm_sed_get_rhs)
! reset concentrations to mininum_value
   do n=1,sed%nvar
      do k=1,sed%grid%knum
         if (sed%conc(1,1,k,n) .lt. sed%model%info%state_variables(n)%minimum) then
            sed%conc(1,1,k,n) = sed%model%info%state_variables(n)%minimum
         end if
      end do
   end do

   if (mod(t,output) .eq. 0) then
       write(0,*) ' elapsed ',t*dt/86400,' days'
       write(funit,*) t*dt,'fluxes',fluxes(1,1,:)
       do k=1,_KNUM_ 
          write(funit,*) t*dt,sed%grid%zc(1,1,k),conc(1,1,k,:)
       end do
   end if
end do

!finalize
call finalize_fabm_sed()
if (allocated(conc)) deallocate(conc)
if (allocated(bdys)) deallocate(bdys)
if (allocated(fluxes)) deallocate(fluxes)

end program fabmsed1d




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


