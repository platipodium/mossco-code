!> @file test_Coupler.F90
!! @brief Tests soil_pelagic_connector
!! @author Richard Hofmeister
!!

program test_Coupler

use esmf
use soil_pelagic_mediator, only : soil_pelagic_connector_SetServices => SetServices

integer                     :: rc
type(ESMF_State)            :: importState, exportState
type(ESMF_CplComp)          :: couplerComp
type(ESMF_Time)             :: startTime, stopTime
type(ESMF_Clock)            :: clock
type(ESMF_Grid)             :: grid
type(ESMF_Field)            :: field
character(len=ESMF_MAXSTR)  :: varnames(8)
real(kind=ESMF_KIND_R8),pointer,dimension(:,:) :: ptr_f2 => null()
integer                     :: i

call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN)

call ESMF_TimeSet(startTime,yy=2002,mm=1,dd=1)
call ESMF_TimeSet(stopTime, yy=2003,mm=1,dd=1)
clock = ESMF_ClockCreate(timeStep=stopTime-startTime, startTime=startTime, stopTime=stopTime, name="clock", rc=rc)

!> Create States, register and initialize components
importState=ESMF_StateCreate(name='import', stateintent=ESMF_STATEINTENT_UNSPECIFIED, rc=rc)
exportState=ESMF_StateCreate(name='export', stateintent=ESMF_STATEINTENT_UNSPECIFIED, rc=rc)

!> create component
couplerComp=ESMF_CplCompCreate(name='coupler', clock=clock)
call ESMF_CplCompSetServices(couplerComp, soil_pelagic_connector_SetServices, rc=rc)

!> fill states with fields
grid  = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/2,2/), rc=rc)

!! add importState fields (from soil)
varnames(1:8) = (/ &
  'mole_concentration_of_nitrate_upward_flux_at_soil_surface  ', &
  'mole_concentration_of_ammonium_upward_flux_at_soil_surface ', &
  'mole_concentration_of_phosphate_upward_flux_at_soil_surface', &
  'dissolved_oxygen_upward_flux_at_soil_surface               ', &
  'dissolved_reduced_substances_upward_flux_at_soil_surface   ', &
  'slow_detritus_C_upward_flux_at_soil_surface                ', &
  'fast_detritus_C_upward_flux_at_soil_surface                ', &
  'detritus-P_upward_flux_at_soil_surface                     ' /)

do i=1,size(varnames)
  field = ESMF_FieldCreate(grid=grid, &
            name=trim(varnames(i)), &
            typekind=ESMF_TYPEKIND_R8, &
            rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_FieldGet(field,farrayPtr=ptr_f2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  ! initialize with varname index
  ptr_f2(:,:) = 1.0*i
 
  call ESMF_StateAdd(importState, (/field/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
end do

!! add exportState fields (to water)
varnames(1:5) = (/ &
  'nutrients_upward_flux_at_soil_surface                       ', &
  'detritus_upward_flux_at_soil_surface                        ', &
  'dissolved_reduced_substances_odu_upward_flux_at_soil_surface', &
  'dissolved_oxygen_oxy_upward_flux_at_soil_surface            ', &
  'oxygen_upward_flux_at_soil_surface                          ' /)

do i=1,5
  field = ESMF_FieldCreate(grid=grid, &
            name=trim(varnames(i)), &
            typekind=ESMF_TYPEKIND_R8, &
            rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  call ESMF_FieldGet(field,farrayPtr=ptr_f2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
  ! initialize with negative varname index
  ptr_f2(:,:) = -1.0*i
  
  call ESMF_StateAdd(exportState, (/field/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
end do

!> dump oxygen in exportState
call ESMF_StateGet(exportState,'oxygen_upward_flux_at_soil_surface',field,rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
call ESMF_FieldGet(field,farrayPtr=ptr_f2, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
write(0,*) 'oxygen flux before coupler'
write(0,*) 'ptr_f2 = ',ptr_f2
!> output = indices in coupler, substract oxygen - reduced substances



!> dump nutrients in exportState
call ESMF_StateGet(exportState,'nutrients_upward_flux_at_soil_surface',field,rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
call ESMF_FieldGet(field,farrayPtr=ptr_f2, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
write(0,*) 'nutrients flux before coupler'
write(0,*) 'ptr_f2 = ',ptr_f2

write(0,*) ''
write(0,*) ' .. coupler is running ..'
write(0,*) ''

!> now run the system
call ESMF_CplCompInitialize(couplerComp, importState=importState, exportState=exportState, clock=clock, rc=rc)

call ESMF_CplCompRun(couplerComp, importState=importState, exportState=exportState, clock=clock, rc=rc)

!> dump oxygen in exportState
call ESMF_StateGet(exportState,'oxygen_upward_flux_at_soil_surface',field,rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
call ESMF_FieldGet(field,farrayPtr=ptr_f2, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
write(0,*) 'oxygen flux after coupler'
write(0,*) 'ptr_f2 = ',ptr_f2

!> dump oxygen in exportState
call ESMF_StateGet(exportState,'dissolved_oxygen_oxy_upward_flux_at_soil_surface',field,rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
call ESMF_FieldGet(field,farrayPtr=ptr_f2, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
write(0,*) 'dissolved_oxygen_oxy flux after coupler'
write(0,*) 'ptr_f2 = ',ptr_f2

!> dump nutrients in exportState
call ESMF_StateGet(exportState,'nutrients_upward_flux_at_soil_surface',field,rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
call ESMF_FieldGet(field,farrayPtr=ptr_f2, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
write(0,*) 'nutrients flux after coupler'
write(0,*) 'ptr_f2 = ',ptr_f2

call ESMF_CplCompFinalize(couplerComp, importState=importState, exportState=exportState, clock=clock, rc=rc)

call ESMF_Finalize()

end program
