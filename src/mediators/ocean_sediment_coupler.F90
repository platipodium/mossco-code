module ocean_sediment_coupler
    
  use esmf
    
  implicit none

  private

  type(ESMF_RouteHandle) :: routeHandle

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

    type(ESMF_Field) :: srcfield, dstfield

    call ESMF_LogWrite("Ocean-sediment coupler initializing", ESMF_LOGMSG_INFO)

    ! Search for the fields with standard name surface_temperature in import
    ! and export states 
    call ESMF_StateGet(importState, "air_temperature_at_surface", srcfield, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_StateGet(exportState, "air_temperature_at_surface", dstfield, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    ! These are fields on different Grids - call RegridStore to set
    ! up the Regrid structure
    call ESMF_FieldRegridStore(srcField=srcfield, dstField=dstfield,&
      routeHandle=routehandle,regridmethod=ESMF_REGRIDMETHOD_BILINEAR,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_LogWrite("Ocean-sediment coupler initializing", ESMF_LOGMSG_INFO)

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
    message = "Ocean-sediment coupler called at "//trim(timestring)
    call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)
    print *, "Proc ",myrank," time=",trim(timestring)

! Get fields from import and export states
    call ESMF_StateGet(importState, "air_temperature_at_surface", srcfield, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_StateGet(exportState, "air_temperature_at_surface", dstfield, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

! Call stored regrid operation
    call ESMF_FieldRegrid(srcfield, dstfield, routehandle, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine Run

  subroutine Finalize(cplcomp, importState, exportState, externalclock, rc)

    type(ESMF_CplComp)   :: cplcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalclock
    integer, intent(out) :: rc
     
    call ESMF_LogWrite("Ocean-sediment coupler finalizing", ESMF_LOGMSG_INFO)

    call ESMF_FieldRegridRelease(routehandle, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_LogWrite("Ocean-sediment coupler finalized", ESMF_LOGMSG_INFO)
  end subroutine Finalize

end module ocean_sediment_coupler
