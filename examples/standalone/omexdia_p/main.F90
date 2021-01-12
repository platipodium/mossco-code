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
use mossco_strings, only: only_var_name

implicit none

real(rk)       :: dt,dzmin,dt_min, relative_change_min
real(rk)       :: bcup_dissolved_variables
integer        :: tnum,t,funit,output,k,n,numyears,numlayers
integer        :: ode_method
integer        :: presimulation_years=-1
type(type_sed) :: sed
real(rk),dimension(:,:,:,:),allocatable,target :: conc
real(rk),dimension(:,:,:), allocatable,target  :: bdys,fluxes
real(rk),dimension(:,:,:),pointer              :: diag
real(rk)  :: pel_NO3=5.0_rk, pel_NH4=5.0_rk, pel_PO4=0.5_rk, pel_O2=250_rk
real(rk)  :: pflux_lDetC=10.0_rk, pflux_sDetC=10.0_rk, pflux_lDetN=1.5_rk
real(rk)  :: pflux_sDetN=1.5_rk, pflux_lDetP=0.2_rk, pel_Temp=5.0_rk
!> deprecated variables (to enable outdated namelists)
real(rk)  :: pflux_sDet=10.0_rk, pflux_fDet=10.0_rk, pflux_detP=0.2_rk
character(len=256) :: ugrid_name='',varname=''

namelist /run_nml/ numyears, dt, output, numlayers, dzmin,                          &
                     ode_method, dt_min, relative_change_min,                         &
                     ugrid_name, bcup_dissolved_variables, presimulation_years,       &
                     pel_Temp, pel_NO3, pel_NH4, pel_PO4, pel_O2,                     &
                     pflux_lDetC, pflux_sDetC, pflux_lDetN, pflux_sDetN, pflux_lDetP, &
                     pflux_sDet, pflux_fDet, pflux_detP

! initialise
dt=60._rk !s
numyears=1
output=86400/int(dt)
numlayers=10
dzmin=0.005
ode_method=_RK4_
dt_min=1.0_rk
relative_change_min=-0.9_rk
bcup_dissolved_variables=2

open(33,file='run_sed.nml',action='read',status='old')
read(33,nml=run_nml)
tnum=numyears*365*86400/int(dt)
sed%grid%inum=1
sed%grid%jnum=1
sed%grid%knum=numlayers
sed%grid%dzmin=dzmin
sed%dt_min=dt_min
sed%relative_change_min=relative_change_min
sed%bcup_dissolved_variables=bcup_dissolved_variables
sed%adaptive_solver_diagnostics=.true.

funit=2

write(0,*) '  Initialise grid with',sed%grid%knum,'vertical layers'
call sed%grid%init_grid()


write(0,*) '  Initialise sediment module'
call sed%initialize(33)
close(33)

! allocate concentration array
allocate(conc(sed%grid%inum,sed%grid%jnum,sed%grid%knum,sed%nvar))
! link array conc to fabm_sediment_driver
sed%conc => conc
! initialise values
conc = 0.0_rk
call sed%init_concentrations()

allocate(bdys(_INUM_,_JNUM_,sed%nvar+1))
bdys(1,1,:) = 0.0_rk

allocate(fluxes(_INUM_,_JNUM_,sed%nvar))
fluxes(1,1,:) = 0.0_rk

! set boundary conditions from namelist
bdys(:,:,1) = pel_Temp !degC
do n=1,size(sed%model%state_variables)
  varname = trim(only_var_name(sed%model%state_variables(n)%long_name))
  if (trim(varname) == 'dissolved_nitrate')            bdys(:,:,n+1)=pel_NO3
  if (trim(varname) == 'dissolved_ammonium')           bdys(:,:,n+1)=pel_NH4
  if (trim(varname) == 'dissolved_phosphate')          bdys(:,:,n+1)=pel_PO4
  if (trim(varname) == 'dissolved_oxygen')             bdys(:,:,n+1)=pel_O2
  if (trim(varname) == 'dissolved_reduced_substances') bdys(:,:,n+1)=pel_O2 !0.0_rk
  if (trim(varname) == 'detritus_labile_carbon')       fluxes(:,:,n)=pflux_lDetC/86400.0_rk
  if (trim(varname) == 'detritus_semilabile_carbon')   fluxes(:,:,n)=pflux_sDetC/86400.0_rk
  if (trim(varname) == 'detritus_labile_nitrogen')     fluxes(:,:,n)=pflux_lDetN/86400.0_rk
  if (trim(varname) == 'detritus_semilabile_nitrogen') fluxes(:,:,n)=pflux_sDetN/86400.0_rk
  if (trim(varname) == 'detritus_labile_phosphorus')   fluxes(:,:,n)=pflux_lDetP/86400.0_rk
!  !> For legacy reasons, these are the old names in omexdia
!  if (trim(varname) == 'fast_detritus_C')              fluxes(:,:,n)=pflux_lDetC/86400.0_rk
!  if (trim(varname) == 'slow_detritus_C')              fluxes(:,:,n)=pflux_sDetC/86400.0_rk
!  if (trim(varname) == 'detritus-P')                   fluxes(:,:,n)=pflux_lDetP/86400.0_rk
!  if (trim(varname) == 'detritus_phosphorus')          fluxes(:,:,n)=pflux_lDetP/86400.0_rk
enddo

open(funit,file='output.dat')
write(funit,fmt='(A,A,A,A)',advance='no') 'time(s) ','depth(m) ','layer-height(m) ','porosity() '
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

       ! write adaptive timestep diagnostics
       if (ode_method .eq. 2) then
         write(0,FMT='(A,F6.2,A,I3,A,A)') '   last minimum timestep: ', &
           sed%last_min_dt,'s, layer ',sed%last_min_dt_grid_cell(3),', variable ', &
           trim(sed%model%state_variables(sed%last_min_dt_grid_cell(4))%name)
         ! reset last minimum timestep
         sed%last_min_dt = dt
       end if

       write(funit,*) t*dt,'fluxes',fluxes(1,1,:)
       do k=1,_KNUM_
          write(funit,FMT='(E15.3,A,E15.4E3,A,E15.4E3,A,E15.4E3)',advance='no') t*dt,' ',sed%grid%zc(1,1,k),' ',sed%grid%dz(1,1,k),' ',sed%porosity(1,1,k)
          do n=1,sed%nvar
             write(funit,FMT='(A,E15.4E3)',advance='no') ' ',conc(1,1,k,n)
          end do
          do n=1,size(sed%model%info%diagnostic_variables)
             diag => sed%diagnostic_variables(n)
             write(funit,FMT='(A,E15.4E3)',advance='no') ' ',diag(1,1,k)
          end do
          write(funit,*)
       end do
   end if
end do

!finalize
call sed%finalize()
if (allocated(conc)) deallocate(conc)
if (allocated(bdys)) deallocate(bdys)
if (allocated(fluxes)) deallocate(fluxes)

end program fabmsed1d
