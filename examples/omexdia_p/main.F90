#include "fabm_driver.h"
#define _GRID_ sed%grid
#define _INUM_ _GRID_%inum
#define _JNUM_ _GRID_%jnum
#define _KNUM_ _GRID_%knum

program fabmsed1d

use fabm_sediment_driver

implicit none

integer,parameter :: rk = selected_real_kind(12)
real(rk)       :: dt,dzmin
integer        :: tnum,t,funit,output,k,n,numyears,numlayers
type(type_sed) :: sed
real(rk),dimension(:,:,:,:),allocatable,target :: conc
real(rk),dimension(:,:,:), allocatable         :: bdys,fluxes

namelist/run_nml/ numyears,dt,output,numlayers,dzmin

! initialise
dt=60._rk !s
numyears=1
output=86400/int(dt)
numlayers=10
dzmin=0.005

open(33,file='run.nml',action='read',status='old')
read(33,nml=run_nml)
tnum=numyears*365*86400/int(dt)
sed%grid%knum=numlayers
sed%grid%dzmin=dzmin

funit=2

write(0,*) '  Initialise grid with',sed%grid%knum,'vertical layers'
call init_sed_grid(sed%grid)

! link array conc to fabm_sediment_driver
sed%conc => conc

write(0,*) '  Initialise sediment module'
call init_fabm_sed(sed)
close(33)


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
   call ode_solver(sed,bdys,fluxes,dt,0,fabm_sed_get_rhs)
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
          write(funit,*) t*dt,sed%grid%zc(1,1,k),sed%conc(1,1,k,:)
       end do
   end if
end do

!finalize
call finalize_fabm_sed()
deallocate(conc)
deallocate(bdys)
deallocate(fluxes)

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

!write(0,*) ' entered ode_solver'

select case (method)
case default
   ! Runge-Kutta-4th_order
   first=.true.
   c_pointer => sed%conc
   call get_rhs(sed,bdys,fluxes,rhs)
   first=.false.

   do i=1,sed%nvar
      do ci=1,_KNUM_
         c1(1,1,ci,i)=sed%conc(1,1,ci,i)+dt*rhs(1,1,ci,i)
      end do
   end do

   sed%conc => c1
   call get_rhs(sed,bdys,fluxes,rhs1)

   do i=1,sed%nvar
      do ci=1,_KNUM_
         c1(1,1,ci,i)=c_pointer(1,1,ci,i)+dt*rhs1(1,1,ci,i)
      end do
   end do

   call get_rhs(sed,bdys,fluxes,rhs2)

   do i=1,sed%nvar
      do ci=1,_KNUM_
         c1(1,1,ci,i)=c_pointer(1,1,ci,i)+dt*rhs2(1,1,ci,i)
      end do
   end do

   call get_rhs(sed,bdys,fluxes,rhs3)

   do i=1,sed%nvar
      do ci=1,_KNUM_
         c_pointer(1,1,ci,i)=c_pointer(1,1,ci,i)+dt*1_rk/3_rk*(0.5_rk*rhs(1,1,ci,i) &
                            +rhs1(1,1,ci,i)+rhs2(1,1,ci,i)+0.5_rk*rhs3(1,1,ci,i))
      end do
   end do
   sed%conc => c_pointer
   nullify(c_pointer)
end select

return
end subroutine ode_solver


