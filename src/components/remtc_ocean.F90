!> @brief Implementation of a simple ESMF ocean component
!
!> This module implements a flat 2D ocean, that heats with forcing at the surface
!> The ocean component imports a "air_temperature"
!> The ocean exports "water_temperature"
!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2013, Helmholtz-Zentrum Geesthacht 
!> @author Hartmut Kapitza, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

module remtc_ocean

  use esmf

  implicit none
  
  private

  real(ESMF_KIND_R8), allocatable :: photosynthetically_available_radiation(:,:,:)
  real(ESMF_KIND_R8), pointer :: photosynthetically_available_radiation_ptr(:,:,:)
  type(ESMF_Field)            :: photosynthetically_available_radiation_Field
  type(ESMF_Array)            :: photosynthetically_available_radiation_Array

  real(ESMF_KIND_R8), allocatable :: salinity(:,:,:)
  real(ESMF_KIND_R8), pointer :: salinity_ptr(:,:,:)
  type(ESMF_Field)            :: salinity_Field
  type(ESMF_Array)            :: salinity_Array

  real(ESMF_KIND_R8), allocatable :: water_temperature(:,:,:)
  real(ESMF_KIND_R8), pointer :: water_temperature_ptr(:,:,:)
  type(ESMF_Field)            :: water_temperature_Field
  type(ESMF_Array)            :: water_temperature_Array

  type(ESMF_Field)            :: air_temperature_Field
  type(ESMF_Array)            :: air_temperature_Array
  real(ESMF_KIND_R8), allocatable :: air_temperature(:,:,:)
  real(ESMF_KIND_R8), pointer :: air_temperature_ptr(:,:,:)

  public SetServices
 
  contains

  subroutine SetServices(gridcomp, rc)
  
    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine SetServices

  !> Initialize the component
  !!
  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)
    
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_Grid)      :: grid
    type(ESMF_ArraySpec) :: arrayspec
    integer                     :: lbnd(3), ubnd(3),farray_shape(3)
    integer                     :: myrank,i,j,k
    real(ESMF_KIND_R8),dimension(:),pointer :: coordX, coordY
 
    integer(ESMF_KIND_I4),allocatable :: deBlockList(:,:,:)
    call ESMF_LogWrite("Remtc Ocean component initializing ...",ESMF_LOGMSG_INFO)
 
    !> Create the grid and coordinates
    !> This example grid is a 40 x 40 grid at 0.1 degree resolution from 0..4 deg East
    !> to 50 .. 55 deg North
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/40, 50,1/), &
      regDecomp=(/1,1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
      name="ocean grid",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
      coorddep2=(/2/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_GridGet(grid,distgrid=distgrid,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_GridAddCoord(grid,staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_GridGetCoord(grid,coordDim=1,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordX, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    do i=lbnd(1),ubnd(1) 
      coordX(i) = 0 + 0.1 * i + 0.05
    enddo
    call ESMF_GridGetCoord(grid,coordDim=2,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=coordY, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    do i=lbnd(1),ubnd(1) 
      coordY(i) = 50 + 0.1 * i + 0.05
    enddo  

    !> Create a salinity field with a 2D array specification, fill the temperature
    !> field with some values, add the field to the ocean's export state
    call ESMF_GridGetFieldBounds(grid=grid,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER,&
      totalCount=farray_shape,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    allocate(salinity(40,50,1))
    salinity_Array = ESMF_ArrayCreate(distgrid=distgrid,farray=salinity, &
      indexflag=ESMF_INDEX_GLOBAL, name="salinity", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    salinity_Field = ESMF_FieldCreate(grid=grid, array=salinity_Array,&
       name="salinity", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_FieldGet(salinity_Field, farrayPtr=salinity_ptr,totalLBound=lbnd,&
      totalUBound=ubnd,localDE=0,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    do k=lbnd(3),ubnd(3)
      do j=lbnd(2),ubnd(2)
        do i=lbnd(1),ubnd(1) 
          salinity =  30.0 !+ 0.1*(i+j)
        enddo
      enddo
    enddo
    !> Create a photosynthetically_available_radiation field with a 2D array specification, fill the temperature
    !> field with some values, add the field to the ocean's export state
    call ESMF_GridGetFieldBounds(grid=grid,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER,&
      totalCount=farray_shape,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    allocate(photosynthetically_available_radiation(40,50,1))
    photosynthetically_available_radiation_Array = ESMF_ArrayCreate(distgrid=distgrid,&
      farray=photosynthetically_available_radiation, &
      indexflag=ESMF_INDEX_GLOBAL, name="photosynthetically_available_radiation", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    photosynthetically_available_radiation_Field = ESMF_FieldCreate(grid=grid, &
      array=photosynthetically_available_radiation_Array,&
      name="photosynthetically_available_radiation", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    photosynthetically_available_radiation =  100.0 !+ 0.1*(i+j)

    !> Create a water temperature field with a 2D array specification, fill the temperature
    !> field with some values, add the field to the ocean's export state
    allocate(water_temperature(40,50,1))
    water_temperature_Array = ESMF_ArrayCreate(distgrid=distgrid,farray=water_temperature, &
      indexflag=ESMF_INDEX_GLOBAL, name="water_temperature", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    water_temperature_Field = ESMF_FieldCreate(grid=grid, array=water_temperature_Array,&
       name="water_temperature", rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    do k=lbnd(3),ubnd(3)
      do j=lbnd(2),ubnd(2)
        do i=lbnd(1),ubnd(1) 
          water_temperature =  20.0 !+ 0.1*(i+j)
        enddo
      enddo
    enddo

    call ESMF_StateAddReplace(exportState,(/water_temperature_Field,salinity_Field, &
       photosynthetically_available_radiation_Field/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !! Do the same for air temperature input
    !allocate(air_temperature(40,50,1))
    !air_temperature_Array = ESMF_ArrayCreate(distgrid=distgrid,farray=air_temperature, &
    !  indexflag=ESMF_INDEX_GLOBAL, name="air_temperature", rc=rc)
    !if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    !air_temperature_Field = ESMF_FieldCreate(grid=grid, array=air_temperature_Array,&
    !   name="air_temperature", rc=rc)
    !if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
     

    !call ESMF_StateAddReplace(importState,(/air_temperature_Field/),rc=rc)
    !if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_LogWrite("Remtc Ocean component initialized.",ESMF_LOGMSG_INFO)
  end subroutine Initialize
    
 
  !> Run the component
  !!
  subroutine Run(gridComp, importState, exportState, parentClock, rc)
    
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer(ESMF_KIND_I8)       :: ku,kl,k 
    integer(ESMF_KIND_I4)       :: myrank
    type(ESMF_Time)             :: localtime
    character (len=ESMF_MAXSTR) :: timestring
    character (len=ESMF_MAXSTR) :: message
    integer(ESMF_KIND_I8)       :: advancecount
    integer(ESMF_KIND_I4)       :: printcount
     
    kl = lbound(water_temperature,3)
    ku = ubound(water_temperature,3)

    call ESMF_GridCompGet(gridComp, localPet=myrank, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockGet(parentClock, currtime=localtime, advanceCount=advancecount, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(localtime, timeString=timestring, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    message = "Remtc ocean called at "//trim(timestring)
    call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)

! Get import state and extract arrays
    call ESMF_StateGet(importState, "air_temperature", air_temperature_Field, rc=rc)
    if (rc == ESMF_SUCCESS) then
      call ESMF_FieldGet(air_temperature_Field, farrayPtr=air_temperature_ptr, localDE=0, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      water_temperature = water_temperature * 0.009d0 + air_temperature_ptr * 0.001d0
    elseif (rc == ESMF_RC_NOT_FOUND) then
      call ESMF_LogWrite("Import field not found, no local changes applied",ESMF_LOGMSG_INFO)
    else
      call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    endif
    rc = ESMF_SUCCESS
    
!> @todo do we need to communicate the update of water_temp back to export state?, No , we do not

! Output to netCDF files
    printcount=int(advancecount,ESMF_KIND_I4)
    !call ESMF_FieldWrite(water_temperature_Field, file="water_temperature.nc", timeslice=printcount, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine Run

  !> Finalize the component
  !!
  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    call ESMF_ArrayDestroy(water_temperature_Array, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldDestroy(water_temperature_Field, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (allocated(water_temperature)) deallocate(water_temperature)
    nullify(water_temperature_ptr)

    call ESMF_LogWrite("Remtc Ocean component finalized", ESMF_LOGMSG_INFO)

  end subroutine Finalize

end module remtc_ocean
