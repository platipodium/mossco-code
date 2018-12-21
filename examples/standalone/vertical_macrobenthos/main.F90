!> @brief Implementation of a vertical macrobenthos position driver
!> @file vertical_macrobenthos_driver.F90
!!
!  self computer program is part of MOSSCO.
!> @copyright Copyright (C) 2018 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define RANGE1D lbnd(1):ubnd(1)
#define RANGE2D RANGE1D,lbnd(2):ubnd(2)

#ifndef KIND_R8
#define KIND_R8 selected_real_kind(13)
#endif

#ifndef KIND_I4
#define KIND_I4 selected_int_kind(5)
#endif

#define _RK4_ 1
#define _ADAPTIVE_EULER_ 2

program vertical_macrobenthos

use vertical_macrobenthos_driver
use solver_library

implicit none

real(KIND_R8)       :: dt, dzmin, dt_min, relative_change_min
real(KIND_R8)       :: bcup_dissolved_variables
integer        :: tnum,t,funit,output,k,n,numyears,numlayers
integer        :: ode_method
integer        :: presimulation_years=-1
type(type_vertical_macrobenthos) :: macrobenthos
real(KIND_R8),dimension(:,:,:,:),allocatable,target :: conc
real(KIND_R8),dimension(:,:,:), allocatable,target  :: bdys,fluxes
real(KIND_R8),dimension(:,:,:),pointer              :: diag
real(KIND_R8)  :: pel_NO3=5.0, pel_NH4=5.0, pel_PO4=0.5, pel_O2=250
real(KIND_R8)  :: pflux_lDetC=10.0, pflux_sDetC=10.0, pflux_lDetN=1.5
real(KIND_R8)  :: pflux_sDetN=1.5, pflux_lDetP=0.2, pel_Temp=5.0
!> deprecated variables (to enable outdated namelists)
real(KIND_R8)  :: pflux_sDet=10.0, pflux_fDet=10.0, pflux_detP=0.2
character(len=256) :: ugrid_name='',varname=''


macrobenthos = create_vertical_macrobenthos(5,4)

!
! ! initialise
! dt=60. !s
! numyears=1
! output=86400/int(dt)
! numlayers=10
! dzmin=0.005
! ode_method=4_
! dt_min=1.0
! relative_change_min=-0.9
! bcup_dissolved_variables=2
!
! tnum=numyears*365*86400/int(dt)
! macrobenthos%grid%inum=1
! macrobenthos%grid%jnum=1
! macrobenthos%grid%knum=numlayers
! macrobenthos%grid%dzmin=dzmin
! macrobenthos%dt_min=dt_min
! macrobenthos%relative_change_min=relative_change_min
! macrobenthos%bcup_dissolved_variables=bcup_dissolved_variables
! macrobenthos%adaptive_solver_diagnostics=.true.
!
! funit=2
!
! write(0,*) '  Initialise grid with',macrobenthos%grid%knum,'vertical layers'
! call macrobenthos%grid%init_grid()
!
!
! write(0,*) '  Initialise sediment module'
! call macrobenthos%initialize()
! close(33)
!
! ! allocate concentration array
! allocate(conc(macrobenthos%grid%inum,macrobenthos%grid%jnum,macrobenthos%grid%knum,macrobenthos%nvar))
! ! link array conc to fabm_sediment_driver
! macrobenthos%conc => conc
! ! initialise values
! conc = 0.0
! call macrobenthos%init_concentrations()
!
! allocate(bdys(_INUM_,_JNUM_,macrobenthos%nvar+1))
! bdys(1,1,:) = 0.0
!
! allocate(fluxes(_INUM_,_JNUM_,macrobenthos%nvar))
! fluxes(1,1,:) = 0.0
!
! ! set boundary conditions from namelist
! bdys(:,:,1) = pel_Temp !degC
! do n=1,size(macrobenthos%model%state_variables)
!   varname = trim(only_var_name(macrobenthos%model%state_variables(n)%long_name))
!   if (trim(varname) == 'dissolved_nitrate')            bdys(:,:,n+1)=pel_NO3
!   if (trim(varname) == 'dissolved_ammonium')           bdys(:,:,n+1)=pel_NH4
!   if (trim(varname) == 'dissolved_phosphate')          bdys(:,:,n+1)=pel_PO4
!   if (trim(varname) == 'dissolved_oxygen')             bdys(:,:,n+1)=pel_O2
!   if (trim(varname) == 'dissolved_reduced_substances') bdys(:,:,n+1)=pel_O2 !0.0
!   if (trim(varname) == 'detritus_labile_carbon')       fluxes(:,:,n)=pflux_lDetC/86400.0
!   if (trim(varname) == 'detritus_semilabile_carbon')   fluxes(:,:,n)=pflux_sDetC/86400.0
!   if (trim(varname) == 'detritus_labile_nitrogen')     fluxes(:,:,n)=pflux_lDetN/86400.0
!   if (trim(varname) == 'detritus_semilabile_nitrogen') fluxes(:,:,n)=pflux_sDetN/86400.0
!   if (trim(varname) == 'detritus_labile_phosphorus')   fluxes(:,:,n)=pflux_lDetP/86400.0
! !  !> For legacy reasons, these are the old names in omexdia
! !  if (trim(varname) == 'fast_detritus_C')              fluxes(:,:,n)=pflux_lDetC/86400.0
! !  if (trim(varname) == 'slow_detritus_C')              fluxes(:,:,n)=pflux_sDetC/86400.0
! !  if (trim(varname) == 'detritus-P')                   fluxes(:,:,n)=pflux_lDetP/86400.0
! !  if (trim(varname) == 'detritus_phosphorus')          fluxes(:,:,n)=pflux_lDetP/86400.0
! enddo
!
! open(funit,file='output.dat')
! write(funit,fmt='(A,A,A,A)',advance='no') 'time(s) ','depth(m) ','layer-height(m) ','porosity() '
! do n=1,macrobenthos%nvar
!    write(funit,fmt='(A,A)',advance='no') ' ',trim(macrobenthos%model%info%state_variables(n)%name)
! end do
! do n=1,size(macrobenthos%model%info%diagnostic_variables)
!    write(funit,fmt='(A,A)',advance='no') ' ',trim(macrobenthos%model%info%diagnostic_variables(n)%name)
! end do
! write(funit,*)
!
! !integrate
! do t=1,tnum
!    macrobenthos%bdys   => bdys
!    macrobenthos%fluxes => fluxes
!    call ode_solver(sed,dt,ode_method)
! ! reset concentrations to mininum_value
!    do n=1,macrobenthos%nvar
!       do k=1,macrobenthos%grid%knum
!          if (macrobenthos%conc(1,1,k,n) .lt. macrobenthos%model%info%state_variables(n)%minimum) then
!             macrobenthos%conc(1,1,k,n) = macrobenthos%model%info%state_variables(n)%minimum
!          end if
!       end do
!    end do
!
!    if (mod(t,output) .eq. 0) then
!        write(0,*) ' elapsed ',t*dt/86400,' days'
!
!        ! write adaptive timestep diagnostics
!        if (ode_method .eq. 2) then
!          write(0,FMT='(A,F6.2,A,I3,A,A)') '   last minimum timestep: ', &
!            macrobenthos%last_min_dt,'s, layer ',macrobenthos%last_min_dt_grid_cell(3),', variable ', &
!            trim(macrobenthos%model%state_variables(macrobenthos%last_min_dt_grid_cell(4))%name)
!          ! reset last minimum timestep
!          macrobenthos%last_min_dt = dt
!        end if
!
!        write(funit,*) t*dt,'fluxes',fluxes(1,1,:)
!        do k=1,_KNUM_
!           write(funit,FMT='(E15.3,A,E15.4E3,A,E15.4E3,A,E15.4E3)',advance='no') t*dt,' ',macrobenthos%grid%zc(1,1,k),' ',macrobenthos%grid%dz(1,1,k),' ',macrobenthos%porosity(1,1,k)
!           do n=1,macrobenthos%nvar
!              write(funit,FMT='(A,E15.4E3)',advance='no') ' ',conc(1,1,k,n)
!           end do
!           do n=1,size(macrobenthos%model%info%diagnostic_variables)
!              diag => macrobenthos%diagnostic_variables(n)
!              write(funit,FMT='(A,E15.4E3)',advance='no') ' ',diag(1,1,k)
!           end do
!           write(funit,*)
!        end do
!    end if
! end do
!
! !finalize
! call macrobenthos%finalize()
! if (allocated(conc)) deallocate(conc)
! if (allocated(bdys)) deallocate(bdys)
! if (allocated(fluxes)) deallocate(fluxes)
!
! end program fabmsed1d
!
! macrobenthos%bcup_dissolved_variables=bcup_dissolved_variables
! macrobenthos%adaptive_solver_diagnostics=.true.
!
! funit=2
!
! write(0,*) '  Initialise grid with',macrobenthos%grid%knum,'vertical layers'
! call macrobenthos%grid%init_grid()
!
!
! write(0,*) '  Initialise sediment module'
! call macrobenthos%initialize()
! close(33)
!
! ! allocate concentration array
! allocate(conc(macrobenthos%grid%inum,macrobenthos%grid%jnum,macrobenthos%grid%knum,macrobenthos%nvar))
! ! link array conc to fabm_sediment_driver
! macrobenthos%conc => conc
! ! initialise values
! conc = 0.0
! call macrobenthos%init_concentrations()
!
! allocate(bdys(_INUM_,_JNUM_,macrobenthos%nvar+1))
! bdys(1,1,:) = 0.0
!
! allocate(fluxes(_INUM_,_JNUM_,macrobenthos%nvar))
! fluxes(1,1,:) = 0.0
!
! ! set boundary conditions from namelist
! bdys(:,:,1) = pel_Temp !degC
! do n=1,size(macrobenthos%model%state_variables)
!   varname = trim(only_var_name(macrobenthos%model%state_variables(n)%long_name))
!   if (trim(varname) == 'dissolved_nitrate')            bdys(:,:,n+1)=pel_NO3
!   if (trim(varname) == 'dissolved_ammonium')           bdys(:,:,n+1)=pel_NH4
!   if (trim(varname) == 'dissolved_phosphate')          bdys(:,:,n+1)=pel_PO4
!   if (trim(varname) == 'dissolved_oxygen')             bdys(:,:,n+1)=pel_O2
!   if (trim(varname) == 'dissolved_reduced_substances') bdys(:,:,n+1)=pel_O2 !0.0
!   if (trim(varname) == 'detritus_labile_carbon')       fluxes(:,:,n)=pflux_lDetC/86400.0
!   if (trim(varname) == 'detritus_semilabile_carbon')   fluxes(:,:,n)=pflux_sDetC/86400.0
!   if (trim(varname) == 'detritus_labile_nitrogen')     fluxes(:,:,n)=pflux_lDetN/86400.0
!   if (trim(varname) == 'detritus_semilabile_nitrogen') fluxes(:,:,n)=pflux_sDetN/86400.0
!   if (trim(varname) == 'detritus_labile_phosphorus')   fluxes(:,:,n)=pflux_lDetP/86400.0
! !  !> For legacy reasons, these are the old names in omexdia
! !  if (trim(varname) == 'fast_detritus_C')              fluxes(:,:,n)=pflux_lDetC/86400.0
! !  if (trim(varname) == 'slow_detritus_C')              fluxes(:,:,n)=pflux_sDetC/86400.0
! !  if (trim(varname) == 'detritus-P')                   fluxes(:,:,n)=pflux_lDetP/86400.0
! !  if (trim(varname) == 'detritus_phosphorus')          fluxes(:,:,n)=pflux_lDetP/86400.0
! enddo
!
! open(funit,file='output.dat')
! write(funit,fmt='(A,A,A,A)',advance='no') 'time(s) ','depth(m) ','layer-height(m) ','porosity() '
! do n=1,macrobenthos%nvar
!    write(funit,fmt='(A,A)',advance='no') ' ',trim(macrobenthos%model%info%state_variables(n)%name)
! end do
! do n=1,size(macrobenthos%model%info%diagnostic_variables)
!    write(funit,fmt='(A,A)',advance='no') ' ',trim(macrobenthos%model%info%diagnostic_variables(n)%name)
! end do
! write(funit,*)
!
! !integrate
! do t=1,tnum
!    macrobenthos%bdys   => bdys
!    macrobenthos%fluxes => fluxes
!    call ode_solver(sed,dt,ode_method)
! ! reset concentrations to mininum_value
!    do n=1,macrobenthos%nvar
!       do k=1,macrobenthos%grid%knum
!          if (macrobenthos%conc(1,1,k,n) .lt. macrobenthos%model%info%state_variables(n)%minimum) then
!             macrobenthos%conc(1,1,k,n) = macrobenthos%model%info%state_variables(n)%minimum
!          end if
!       end do
!    end do
!
!    if (mod(t,output) .eq. 0) then
!        write(0,*) ' elapsed ',t*dt/86400,' days'
!
!        ! write adaptive timestep diagnostics
!        if (ode_method .eq. 2) then
!          write(0,FMT='(A,F6.2,A,I3,A,A)') '   last minimum timestep: ', &
!            macrobenthos%last_min_dt,'s, layer ',macrobenthos%last_min_dt_grid_cell(3),', variable ', &
!            trim(macrobenthos%model%state_variables(macrobenthos%last_min_dt_grid_cell(4))%name)
!          ! reset last minimum timestep
!          macrobenthos%last_min_dt = dt
!        end if
!
!        write(funit,*) t*dt,'fluxes',fluxes(1,1,:)
!        do k=1,_KNUM_
!           write(funit,FMT='(E15.3,A,E15.4E3,A,E15.4E3,A,E15.4E3)',advance='no') t*dt,' ',macrobenthos%grid%zc(1,1,k),' ',macrobenthos%grid%dz(1,1,k),' ',macrobenthos%porosity(1,1,k)
!           do n=1,macrobenthos%nvar
!              write(funit,FMT='(A,E15.4E3)',advance='no') ' ',conc(1,1,k,n)
!           end do
!           do n=1,size(macrobenthos%model%info%diagnostic_variables)
!              diag => macrobenthos%diagnostic_variables(n)
!              write(funit,FMT='(A,E15.4E3)',advance='no') ' ',diag(1,1,k)
!           end do
!           write(funit,*)
!        end do
!    end if
! end do

!finalize
call macrobenthos%finalize()
if (allocated(conc)) deallocate(conc)
if (allocated(bdys)) deallocate(bdys)
if (allocated(fluxes)) deallocate(fluxes)

end program vertical_macrobenthos
