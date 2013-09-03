!> @brief Implementation of a vertical weighted mean coupler
!
!> This module vertically integrates with weights grid_height 
!>
!> @import grid_height, any 3d variable
!> @export bulk variables 
!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2013, Helmholtz-Zentrum Geesthacht 
!> @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

module vertical_mean_coupler
    
  use esmf
    
  implicit none

  private

  type(ESMF_Field)  :: grid_height_Field
  real(ESMF_KIND_R8), pointer :: grid_height(:,:,:)
  type(ESMF_Field)            :: bulk_variable_Field 
  real(ESMF_KIND_R8), pointer :: bulk_variable(:,:)
  type(ESMF_Field)            :: variable_Field 
  real(ESMF_KIND_R8), pointer :: variable(:,:,:)

  public SetServices

  contains

  subroutine SetServices(cplcomp, rc)

    type(ESMF_CplComp)   :: cplcomp
    integer, intent(out) :: rc

    call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_INITIALIZE, Initialize  &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_RUN,    Run   &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_FINALIZE, Finalize &
                                      , rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine SetServices

  subroutine Initialize(cplcomp, importState, exportState, externalclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer, intent(out) :: rc
 
    integer(ESMF_KIND_I8) :: nlev,k

    call ESMF_LogWrite("Vertical integration coupler initializing", ESMF_LOGMSG_INFO)

    ! Search for the fields with standard name grid_height in import state
    call ESMF_StateGet(importState, "grid_height", grid_height_Field, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(grid_height_Field, farrayPtr=grid_height, localDE=0, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    ! Look through all other fields and do weigthed mean with grid_height,
    ! for now do only water_temperature
    call ESMF_StateGet(importState, "water_temperature", variable_Field, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(variable_Field, farrayPtr=variable, localDE=0, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    nlev = size(variable,3)
    !water_temperature_at_surface = water_temperature(:,:,nlev)
    !water_temperature_at_bottom  = water_temperature(:,:,1)
    bulk_variable(:,:) = 0.0
    do k=1,nlev
      bulk_variable(:,:) = bulk_variable(:,:) &
       + variable(:,:,k) * grid_height(:,:,k)
    enddo

    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_LogWrite("Vertical integration coupler initializing", ESMF_LOGMSG_INFO)

  end subroutine Initialize


  subroutine Run(cplcomp, importState, exportState, externalclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer, intent(out) :: rc

    integer                     :: myrank
    type(ESMF_Time)             :: localtime
    character (len=ESMF_MAXSTR) :: timestring
    character (len=ESMF_MAXSTR) :: message
    type(ESMF_Field)            :: srcfield, dstfield
     
! Print timed log message
    call ESMF_CplCompGet(cplcomp, localPet=myrank, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_ClockGet(externalclock, currtime=localtime, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_TimeGet(localtime, timeString=timestring, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    message = "aocpl_run called at "//trim(timestring)
    call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)
    print *, "Proc ",myrank," time=",trim(timestring)

! Get fields from import and export states
    call ESMF_StateGet(importState, "air_temperature_at_surface", srcfield, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_StateGet(exportState, "air_temperature_at_surface", dstfield, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine Run

  subroutine Finalize(cplcomp, importState, exportState, externalclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer, intent(out) :: rc
     
    call ESMF_LogWrite("Vertical mean coupler finalizing", ESMF_LOGMSG_INFO)

    call ESMF_LogWrite("Vertical mean coupler finalized", ESMF_LOGMSG_INFO)
  end subroutine Finalize

end module vertical_mean_coupler
