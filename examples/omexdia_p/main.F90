#include "fabm_driver.h"
#define _GRID_ sed%grid
#define _INUM_ _GRID_%inum
#define _JNUM_ _GRID_%jnum
#define _KNUM_ _GRID_%knum

#define _RK4_ 1
#define _ADAPTIVE_EULER_ 2

program fabmsed1d

use fabm_sediment_driver
use solver_library

implicit none

real(rk)       :: dt,dzmin
integer        :: tnum,t,funit,output,k,n,numyears,numlayers
integer        :: ode_method
type(type_sed) :: sed
real(rk),dimension(:,:,:,:),allocatable,target :: conc
real(rk),dimension(:,:,:), allocatable,target  :: bdys,fluxes
real(rk),dimension(:,:,:),pointer              :: diag

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
write(funit,fmt='(A,A,A)',advance='no') 'time(s) ','depth(m) ','layer-height(m) '
do n=1,sed%nvar
   write(funit,fmt='(A,A)',advance='no') ' ',trim(sed%model%info%state_variables(n)%name)
end do
do n=1,size(sed%model%info%diagnostic_variables)
   write(funit,fmt='(A,A)',advance='no') ' ',trim(sed%model%info%diagnostic_variables(n)%name)
end do
write(funit,*)

!integrate
do t=1,tnum
   sed%bdys   => bdys
   sed%fluxes => fluxes
   call ode_solver(sed,dt,ode_method)
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
          write(funit,FMT='(E15.3,A,E15.4E3,A,E15.4E3)',advance='no') t*dt,' ',sed%grid%zc(1,1,k),' ',sed%grid%dz(1,1,k)
          do n=1,sed%nvar
             write(funit,FMT='(A,E15.4E3)',advance='no') ' ',conc(1,1,k,n)
          end do
          do n=1,size(sed%model%info%diagnostic_variables)
             diag => fabm_sed_diagnostic_variables(sed,n)
             write(funit,FMT='(A,E15.4E3)',advance='no') ' ',diag(1,1,k)
          end do
          write(funit,*)
       end do
   end if
end do

!finalize
call finalize_fabm_sed()
if (allocated(conc)) deallocate(conc)
if (allocated(bdys)) deallocate(bdys)
if (allocated(fluxes)) deallocate(fluxes)

end program fabmsed1d
