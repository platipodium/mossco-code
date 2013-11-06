!> @brief Implementation of a simple ESMF ocean component
!
!> This module implements a flat 2D ocean, that heats with forcing at the surface
!> The ocean component imports a "air_temperature"
!> The ocean exports "water_temperature, photosynthetically_available_radiation, and salinity"
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
  use mossco_variable_types

  implicit none
  
  private

  type(ESMF_Field)            :: air_temperature_Field
  type(ESMF_Array)            :: air_temperature_Array
  real(ESMF_KIND_R8), allocatable :: air_temperature(:,:,:)
  real(ESMF_KIND_R8), pointer :: air_temperature_ptr(:,:,:)

  real(ESMF_KIND_R8), allocatable, target :: variables(:,:,:,:)
  
  type(MOSSCO_VariableInfo), private, target :: par=MOSSCO_VariableInfo(&
    "photosynthetically_available_radiation", &
    "W m**-2","photosynthetically_available_radiation")
  type(MOSSCO_VariableInfo), private, target :: temp=MOSSCO_VariableInfo(&
    "water_temperature", &
    "degree_C","water_temperature")
  type(MOSSCO_VariableInfo), private, target :: salinity=MOSSCO_VariableInfo(&
    "salinity", &
    "PSU","salinity")
 type(MOSSCO_VariableInfo), private, target :: airtemp=MOSSCO_VariableInfo(&
    "air_temperature", &
    "degree_C","air_temperature")

  type(MOSSCO_VariableFArray3d), dimension(3) :: export_variables
  type(MOSSCO_VariableFArray3d), dimension(1) :: import_variables

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

    type(ESMF_Grid)      :: grid
    type(ESMF_ArraySpec) :: arrayspec
    integer                     :: lbnd(3), ubnd(3),farray_shape(3)
    integer                     :: myrank,i,j,k
    real(ESMF_KIND_R8),dimension(:),pointer :: coordX, coordY
 
    integer(ESMF_KIND_I4),allocatable :: deBlockList(:,:,:)

    real(ESMF_KIND_R8), allocatable, target :: farray(:,:,:)
    real(ESMF_KIND_R8), pointer ::  farrayPtr(:,:,:)
    type(ESMF_Field) :: exportField(3), importField(1)

    call ESMF_LogWrite("Remtc Ocean component initializing ...",ESMF_LOGMSG_INFO)
 
    !> Create the grid and coordinates
    !> This example grid is a 40 x 40 grid at 0.1 degree resolution from 0..4 deg East
    !> to 50 .. 55 deg North
    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/40, 50,1/), &
      regDecomp=(/1,1,1/),coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL,  &
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

    export_variables(1)%standard_name="salinity"
    export_variables(2)%standard_name="water_temperature"
    export_variables(3)%standard_name="photosynthetically_available_radiation"

    call ESMF_GridGetFieldBounds(grid=grid,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER,&
                                 totalCount=farray_shape,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !> Create export fields and add them to export state, allocate the space for these
    !> that will be filled later with data
    allocate(variables(farray_shape(1),farray_shape(2),farray_shape(3),4))
    do k=1,3
      farrayPtr=>variables(:,:,:,k)

      exportField(k) = ESMF_FieldCreate(grid, farrayPtr=farrayPtr, &
        staggerloc=ESMF_STAGGERLOC_CENTER,name=export_variables(k)%standard_name, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_StateAddReplace(exportState,(/exportField(k)/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    enddo
  
    !> Specify salinity information     
    call ESMF_FieldGet(field=exportField(1), localDe=0, farrayPtr=farrayPtr, &
                       totalLBound=lbnd,totalUBound=ubnd, rc=rc) 
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        
    do k=lbnd(3),ubnd(3)
      do j=lbnd(2),ubnd(2)
        do i=lbnd(1),ubnd(1) 
          variables(i,j,k,1) =  30.0 + 0.1*(i-j)
        enddo
      enddo
    enddo
        
    !> Specify temperature information     
    call ESMF_FieldGet(field=exportField(2), localDe=0, farrayPtr=farrayPtr, rc=rc) 
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    do k=lbnd(3),ubnd(3)
      do j=lbnd(2),ubnd(2)
        do i=lbnd(1),ubnd(1) 
          farrayPtr(i,j,k) =  20.0 + 0.1*(i+j)
        enddo
      enddo
    enddo

    !> Specify PAR      
    call ESMF_FieldGet(field=exportField(3), localDe=0, farrayPtr=farrayPtr, rc=rc) 
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    farrayPtr = 100.0 !+ 0.1*(i+j)

    !> For debugging and I/O testing purposes, write out all the export fields to netcdf
#ifndef ESMF_MPIUNI 
    !! ESMF_FieldWrite is not supported in ESMF_MPIUNI mode     
    do k=1,3
      call ESMF_FieldWrite(exportField(k), "remtc_ocean_export_"//export_variables(i)%standard_name, & 
        .true.,0,ESMF_IOFMT_NETCDF, rc = rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   enddo
#endif

    import_variables(1)%standard_name="air_temperature"

    !> Create import fields and add them to import state, allocate the space for these
    !> that will be filled later with data
    do k=1,size(importField)
      farrayPtr=>variables(:,:,:,k+size(exportField))

      importField(k) = ESMF_FieldCreate(grid, farrayPtr=farrayPtr, &
        staggerloc=ESMF_STAGGERLOC_CENTER,name=import_variables(k)%standard_name, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      call ESMF_StateAddReplace(importState,(/importField(k)/),rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    enddo
  
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

    integer(ESMF_KIND_I4)       :: myrank
    type(ESMF_Time)             :: localtime
    character (len=ESMF_MAXSTR) :: timestring
    character (len=ESMF_MAXSTR) :: message
    integer(ESMF_KIND_I8)       :: advancecount
    integer(ESMF_KIND_I4)       :: printcount
    type(ESMF_Field)            :: field
    real(ESMF_KIND_R8), pointer :: farrayPtr(:,:,:)
    
     
    call ESMF_GridCompGet(gridComp, localPet=myrank, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockGet(parentClock, currtime=localtime, advanceCount=advancecount, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(localtime, timeString=timestring, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    message = "Remtc ocean called at "//trim(timestring)
    call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)

    !> Get import state and extract arrays
    call ESMF_StateGet(importState, "air_temperature", field, rc=rc)
    if (rc == ESMF_SUCCESS) then
      call ESMF_FieldGet(field, farrayPtr=farrayPtr, localDE=0, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        !> water_temperature is in k=2, this needs to be programmatically determined  
        variables(:,:,:,2) = variables(:,:,:,2) * 0.009d0 + farrayPtr * 0.001d0
      elseif (rc == ESMF_RC_NOT_FOUND) then
        call ESMF_LogWrite("Import field not found, no local changes applied",ESMF_LOGMSG_INFO)
     else
       call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
     endif
    rc = ESMF_SUCCESS
    
    !> Output to netCDF files
    printcount=int(advancecount,ESMF_KIND_I4)
    !call ESMF_FieldWrite(field, file="water_temperature.nc", timeslice=printcount, rc=rc)
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

    !call ESMF_FieldDestroy(water_temperature_Field, rc=rc)
    !if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    if (allocated(variables)) deallocate(variables)
 
    call ESMF_LogWrite("Remtc Ocean component finalized", ESMF_LOGMSG_INFO)

  end subroutine Finalize

end module remtc_ocean
