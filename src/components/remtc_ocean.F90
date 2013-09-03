!> @brief Implementation of a simple ESMF ocean component
!
!> This module implements a flat 2D ocean, that heats with forcing at the surface
!> The ocean component imports a "air_temperature_at_surface"
!> The ocean exports "water_temperature_at_surface"
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

  real(ESMF_KIND_R8), pointer :: water_temperature_at_surface(:,:)
  real(ESMF_KIND_R8), pointer :: air_temperature_at_surface(:,:)
  type(ESMF_Field)            :: water_temperature_at_surface_Field
  type(ESMF_Field)            :: air_temperature_at_surface_Field

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
    integer                     :: lbnd(2), ubnd(2)
    integer                     :: myrank,i,j
    real(ESMF_KIND_R8),dimension(:),pointer :: coordX, coordY
 
    integer(ESMF_KIND_I4),allocatable :: deBlockList(:,:,:)
    
    call ESMF_LogWrite("Remtc Ocean component initializing ...",ESMF_LOGMSG_INFO)
 
    !> Create a distribution for compute elements
    allocate( deBlockList(2,2,4) )
    deBlockList(:,1,1) = (/ 1, 1/) ! minindex 1st de-block
    deBlockList(:,2,1) = (/15,20/) ! maxindex
    deBlockList(:,1,2) = (/16, 1/) ! minindex 2nd de-block
    deBlockList(:,2,2) = (/40,12/) ! maxindex
    deBlockList(:,1,3) = (/ 1,21/) ! minindex 3rd de-block
    deBlockList(:,2,3) = (/15,40/) ! maxindex
    deBlockList(:,1,4) = (/16,13/) ! minindex 4th de-block
    deBlockList(:,2,4) = (/40,40/) ! maxindex

    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/),                     &
                                   maxIndex=(/40,40/),                   &
                                   deBlockList=deBlockList,              &
                                   indexflag=ESMF_INDEX_GLOBAL,          &
                                   rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)


    !> Create the grid and coordinates
    !> This example grid is a 40 x 40 grid at 0.1 degree resolution from 0..4 deg East
    !> to 50 .. 55 deg North
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/40, 50/), &
      regDecomp=(/2,2/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
      name="ocean grid",coordTypeKind=ESMF_TYPEKIND_R8,coordDep1=(/1/),&
      coorddep2=(/2/),rc=rc)
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

    !> Create a water temperature field with a 2D array specification, fill the temperature
    !> field with some values, add the field to the ocean's export state
    call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    water_temperature_at_surface_Field = ESMF_FieldCreate(grid, arrayspec, name="water_temperature_at_surface", &
      staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_FieldGet(water_temperature_at_surface_Field, farrayPtr=water_temperature_at_surface,totalLBound=lbnd,&
      totalUBound=ubnd,localDE=0,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    do i=lbnd(1),ubnd(1) 
      do j=lbnd(2),ubnd(2)
        water_temperature_at_surface =  20 + 0.1*(i+j)
      enddo
    enddo

    call ESMF_StateAdd(exportState,(/water_temperature_at_surface_Field/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !> Create an air surface temperature field with a 2D array specification, add the field
    !> to the ocean's import state
    air_temperature_at_surface_Field = ESMF_FieldCreate(grid, arrayspec, name="air_temperature_at_surface", &
      staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_StateAdd(importState,(/air_temperature_at_surface_Field/),rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

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

    integer                     :: myrank, ib, ie, jb, je
    type(ESMF_Time)             :: localtime
    character (len=ESMF_MAXSTR) :: timestring
    character (len=ESMF_MAXSTR) :: message
    integer(ESMF_KIND_I8)       :: advancecount
    integer(ESMF_KIND_I4)       :: printcount
     
    ib = lbound(water_temperature_at_surface,1)
    ie = ubound(water_temperature_at_surface,1)
    jb = lbound(water_temperature_at_surface,2)
    je = ubound(water_temperature_at_surface,2)

    call ESMF_GridCompGet(gridComp, localPet=myrank, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockGet(parentClock, currtime=localtime, advanceCount=advancecount, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(localtime, timeString=timestring, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    message = "Remtc ocean called at "//trim(timestring)
    call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)

! Get import state and extract arrays
    call ESMF_StateGet(importState, "air_temperature_at_surface", air_temperature_at_surface_Field, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    
    call ESMF_FieldGet(air_temperature_at_surface_Field, farrayPtr=air_temperature_at_surface, localDE=0, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

! Do something
    water_temperature_at_surface = water_temperature_at_surface *0.009d0 + air_temperature_at_surface * 0.001d0

!> @todo do we need to communicate the update of water_temp back to export state? Why do we need to get air temp from 
!> import state anyhow? 

! Output to netCDF files
    printcount=int(advancecount,ESMF_KIND_I4)
    !call ESMF_FieldWrite(water_temperature_at_surface_Field, file="water_temperature_at_surface.nc", timeslice=printcount, rc=rc)
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

    call ESMF_FieldDestroy(water_temperature_at_surface_Field, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_LogWrite("Remtc Ocean component finalized", ESMF_LOGMSG_INFO)

  end subroutine Finalize

end module remtc_ocean
