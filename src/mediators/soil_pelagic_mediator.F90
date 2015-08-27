!> @brief Implementation of an ESMF soil-pelagic coupling
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2014, 2015 Helmholtz-Zentrum Geesthacht
!> @author Richard Hofmeister <richard.hofmeister@hzg.de>
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!> @author Nils Weiher <stu95021@uni-kiel.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "soil_pelagic_mediator.F90"

module soil_pelagic_mediator

    !> @todo: Outsource the default methods to a generic coupler module
    !use mossco_mediator
    use esmf
    use mossco_state
    use mossco_field
    use mossco_component
    use mossco_db


!----------------------------------------------------------------------
!------------------- Soil Pelagic Config ------------------------------
!----------------------------------------------------------------------

    !------------------------------------------------------------------
    implicit none

    private
    !COUPLER CONFIG
    character(len=ESMF_MAXSTR)                    :: rulesets &
                                                   ="'General', &
                                                     'HZG KW'"
    logical                                     :: DEBUG = .true.

    !MODULE VARS
    real(ESMF_KIND_R8),dimension(:,:),   pointer :: ptr_f2=>null(),val1_f2=>null(),val2_f2=>null()
    real(ESMF_KIND_R8),dimension(:,:),   pointer :: DETCflux=>null(),DINflux=>null()
    real(ESMF_KIND_R8) :: dinflux_const=0.0
    real(ESMF_KIND_R8) :: dipflux_const=-1.
    !------------------------------------------------------------------

    public SetServices

    contains

!----------------------------------------------------------------------
!------------------- Soil Pelagic Routines ----------------------------
!----------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "mcpl_InitializeP0"
!> @subsubsection mcpl_InitializeP0 "User Code in Initialization Phase 0"
!> @brief Implementation of User-Code in Phase 0
!> @details Contains untouched States
!> @param cplComp, importState, exportState, parentClock, rc
!! ESMF Coupler Component; ESMF States; Simulation Time; ESMF Return Code
subroutine mcpl_InitializeP0(cplComp, importState, exportState, parentClock, rc)
    !------------------------------------------------------------------
    !INPUTS / OUTPUTS
    type(ESMF_cplComp)          :: cplComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: parentClock
    integer, intent(out)        :: rc

    !LOCAL VARS
    character(len=ESMF_MAXSTR)  :: message
    !------------------------------------------------------------------
    write(message,'(A)') "coupler      User-code init phase 0"
    call ESMF_LogWrite(message, ESMF_LOGMSG_TRACE)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if
    rc = ESMF_SUCCESS

end subroutine mcpl_InitializeP0


#undef  ESMF_METHOD
#define ESMF_METHOD "mcpl_InitializeP1"
!> @subsubsection mcpl_InitializeP1 "User Code in Initialization Phase 1"
!> @brief Implementation of User-Code in Phase 1
!> @details Contains untouched States
!> @param cplComp, importState, exportState, parentClock, rc
!! ESMF Coupler Component; ESMF States; Simulation Time; ESMF Return Code
subroutine mcpl_InitializeP1(cplcomp, importState, exportState, externalclock, rc)
    !------------------------------------------------------------------
    !INPUTS / OUTPUTS
    type(ESMF_cplComp)          :: cplComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: externalclock
    integer, intent(out)        :: rc

    !LOCAL VARS
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_State)            :: paramState
    type(ESMF_Time)             :: currTime
    logical                     :: isPresent
    integer                     :: nmlunit=127, localrc
    !------------------------------------------------------------------
    namelist /soil_pelagic_mediator/ dinflux_const,dipflux_const

    write(message,'(A)') "coupler      User-code init phase 1"
    call ESMF_LogWrite(message, ESMF_LOGMSG_TRACE)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if
    rc = ESMF_SUCCESS

    !> receive coupler component information
    call MOSSCO_CompEntry(cplComp, externalClock, name=name, currTime=currTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !read namelist
    inquire(file=trim(name)//'.nml', exist=isPresent)

    if (isPresent) then
      open(nmlunit,file='soil_pelagic_mediator.nml',action='read',status='old')
      read(nmlunit,soil_pelagic_mediator)
      close(nmlunit)
    endif

    if (dipflux_const < 0.0) dipflux_const=dinflux_const/16.0d0


    paramState=ESMF_StateCreate(name=trim(name)//'Parameters', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(paramState, trim(name)//'::dipflux_const', dipflux_const, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(paramState, trim(name)//'::dinflux_const', dinflux_const, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateAdd(importState, (/paramState/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

end subroutine mcpl_InitializeP1


#undef  ESMF_METHOD
#define ESMF_METHOD "mcpl_Run_pre_recipe"
!> @subsubsection mcpl_Run_pre_recipe "User Code before automatic recipe search"
!> @brief Implementation of User-Code in Run Phase,
!! before automatic recipe search via database
!> @details Contains normalized inventory lists of substances known by database
!> @param cplComp, importState, exportState, parentClock, rc
!! ESMF Coupler Component; ESMF States; Simulation Time; ESMF Return Code
subroutine mcpl_Run_pre_recipe(cplcomp, importState, exportState, externalclock, rc)
    !------------------------------------------------------------------
    !INPUTS / OUTPUTS
    type(ESMF_cplComp)          :: cplComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: externalclock
    integer, intent(out)        :: rc

    !LOCAL VARS
    type(ESMF_State)            :: dba_import, dba_export
    type(ESMF_Time)             :: currTime, stopTime
    type(ESMF_Time)             :: localtime
    type(ESMF_Field)            :: field

    character(len=ESMF_MAXSTR)  :: name, message
    character (len=ESMF_MAXSTR) :: timestring

    integer(ESMF_KIND_R8)       :: advanceCount
    integer(ESMF_KIND_I4)       :: rank, ubnd(2), lbnd(2), &
                                   itemCount, dba_value

    real(ESMF_KIND_R8),parameter:: NC_fdet=0.20d0
    real(ESMF_KIND_R8),parameter:: NC_sdet=0.04d0
    !> @todo read NC_fdet dynamically from fabm model info?  This would not comply with our aim to separate fabm/esmf

    integer                     :: ammrc, nitrc, oxyrc, odurc
    integer                     :: localrc
    integer                     :: myrank

    logical                     :: verbose=.true.
    !------------------------------------------------------------------
    write(message,'(A)') "coupler      User-code run phase, pre-recipe"
    call ESMF_LogWrite(message, ESMF_LOGMSG_TRACE)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if
    rc = ESMF_SUCCESS

    !> receive coupler component information
    call MOSSCO_CompEntry(cplComp, externalClock, name=name, currTime=currTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(externalClock, advanceCount=advanceCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (advanceCount > 0) verbose=.false.


    call ESMF_StateGet(importState, "dba_import", dba_import, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(exportState, "dba_export", dba_export, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)


    !DIN flux:
    !> User code to get nutrients
    call mossco_state_get(exportState,(/'nutrients_upward_flux_at_soil_surface'/),DINflux,verbose=verbose)
    call mossco_state_get(importState, (/'mole_concentration_of_nitrate_upward_flux_at_soil_surface'/), &
      val1_f2, verbose=verbose)
    call mossco_state_get(importState, (/'mole_concentration_of_ammonium_upward_flux_at_soil_surface'/), &
      val2_f2, verbose=verbose, rc=localrc)
    DINflux = val1_f2 + val2_f2

    !> Tell the coupler that the export substance has been found ("found" value=1)
    dba_value=1
    call ESMF_AttributeSet(state=dba_export, name='nutrients_upward_flux_at_soil_surface', value=dba_value, rc=localrc)

    !> Tell the coupler that the two import substances have been used
    if (localrc==ESMF_SUCCESS) then
        call ESMF_AttributeGet(state=dba_import, name='mole_concentration_of_nitrate_upward_flux_at_soil_surface', value=dba_value)
        dba_value=dba_value+1
        call ESMF_AttributeSet(state=dba_import, name='mole_concentration_of_nitrate_upward_flux_at_soil_surface', value=dba_value)

        call ESMF_AttributeGet(state=dba_import, name='mole_concentration_of_ammonium_upward_flux_at_soil_surface', value=dba_value)
        dba_value=dba_value+1
        call ESMF_AttributeSet(state=dba_import, name='mole_concentration_of_ammonium_upward_flux_at_soil_surface', value=dba_value)
    end if

    call MOSSCO_CompExit(cplComp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

end subroutine mcpl_Run_pre_recipe


#undef  ESMF_METHOD
#define ESMF_METHOD "mcpl_Run_pre_log"
!> @subsubsection mcpl_Run_pre_log "User Code before log creation"
!> @brief Implementation of User-Code in Run Phase,
!! after automatic recipe search, before missing log is created
!> @details Contains inventory lists reduced by found substances
!> @param cplComp, importState, exportState, parentClock, rc
!! ESMF Coupler Component; ESMF States; Simulation Time; ESMF Return Code
subroutine mcpl_Run_pre_log(cplcomp, importState, exportState, externalclock, rc)
    !------------------------------------------------------------------
    !INPUTS / OUTPUTS
    type(ESMF_cplComp)          :: cplComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: externalclock
    integer, intent(out)        :: rc

    !LOCAL VARS
    character(len=ESMF_MAXSTR)  :: message
    !------------------------------------------------------------------
    write(message,'(A)') "coupler      User-code run phase, pre-log"
    call ESMF_LogWrite(message, ESMF_LOGMSG_TRACE)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if
    rc = ESMF_SUCCESS

end subroutine mcpl_Run_pre_log


#undef  ESMF_METHOD
#define ESMF_METHOD "mcpl_finalize"
!> @subsubsection mcpl_finalize "User Code at end of execution"
!> @brief Implementation of User-Code in Finalize Phase,
!> @param cplComp, importState, exportState, parentClock, rc
!! ESMF Coupler Component; ESMF States; Simulation Time; ESMF Return Code
subroutine mcpl_finalize(cplcomp, importState, exportState, externalclock, rc)
    !------------------------------------------------------------------
    !INPUTS / OUTPUTS
    type(ESMF_cplComp)          :: cplComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: externalclock
    integer, intent(out)        :: rc

    !LOCAL VARS
    type(ESMF_Time)             :: currTime
    integer                     :: localrc
    character(len=ESMF_MAXSTR)  :: name, message
    !------------------------------------------------------------------
    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, externalClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#if 0
    !! Safely destroy the parameter state if it exists in either import or export states
    paramName=trim(name)//'Parameters'
    call ESMF_StateGet(importState, paramName, itemType=importItemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(exportState, paramName, itemType=exportItemType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if ((exportItemType == ESMF_STATEITEM_STATE) .or. (importItemType == ESMF_STATEITEM_STATE)) then
      if (importItemType == ESMF_STATEITEM_STATE) then
        call ESMF_StateGet(importState, paramName, paramState, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateRemove(importState, (/paramName/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      if (exportItemType == ESMF_STATEITEM_STATE) then
        call ESMF_StateGet(exportState, paramName, paramState, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateRemove(importState, (/paramName/), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif

      call ESMF_StateDestroy(paramState, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif
#endif

    call MOSSCO_CompExit(cplComp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

end subroutine mcpl_finalize




!############################################################################################
!############################################################################################
!############################################################################################







!@dev: To be outsourced (see bottom)
!----------------------------------------------------------------------
!------------------- General MOSSCO coupler Routines ------------------
!----------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
!> @subsubsection SetServices "SetServices Method"
!> @brief Specifies entry points for ESMF methods
!> @details Required public ESMF method
!> @param cplComp, importState, exportState, parentClock, rc
!! ESMF Coupler Component; ESMF States; Simulation Time; ESMF Return Code
subroutine SetServices(cplComp, rc)
    !------------------------------------------------------------------
    implicit none

    !INPUTS/OUTPUTS
    type(ESMF_CplComp)          :: cplComp
    integer, intent(out)        :: rc

    !LOCAL VARS
    character(len=ESMF_MAXSTR)  :: message

    integer                     :: localrc
    !------------------------------------------------------------------
    write(message,'(A)') "coupler      SetServices"
    call ESMF_LogWrite(message, ESMF_LOGMSG_TRACE)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if
    rc = ESMF_SUCCESS

    !>Register phase 0 method for ESMF
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !>Register phase 1 method for ESMF
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !>Register run method for ESMF
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !>Register finalize method for ESMF
    call ESMF_CplCompSetEntryPoint(cplComp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

end subroutine SetServices


#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
!> @subsubsection InitializeP0 "Initialize Method Phase 0"
!> @brief ESMF preperation method
!> @details Allocate Space, open files, set initial conditions
!> @param cplComp, importState, exportState, parentClock, rc
!! ESMF Coupler Component; ESMF States; Simulation Time; ESMF Return Code
subroutine InitializeP0(cplComp, importState, exportState, parentClock, rc)
    !------------------------------------------------------------------
    implicit none
    !INPUTS / OUTPUTS
    type(ESMF_cplComp)          :: cplComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: parentClock
    integer, intent(out)        :: rc

    !LOCAL VARS
    type(ESMF_Time)             :: currTime

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message

    integer                     :: localrc
    !------------------------------------------------------------------
    write(message,'(A)') "coupler      Init phase 0"
    call ESMF_LogWrite(message, ESMF_LOGMSG_TRACE)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if
    rc = ESMF_SUCCESS

    !> Call user-code method
    call mcpl_InitializeP0(cplcomp, importState, exportState, parentClock, rc)

    write(message,'(A)') "coupler      Return to init phase 0"
    call ESMF_LogWrite(message, ESMF_LOGMSG_TRACE)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if

    !> Calls incoming couple/grid component method and receives clock
    call MOSSCO_CompEntry(cplComp, parentClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Defines number of ESMF Initialization phases according to NUOPC standard,
    !! adds the configuration to the Component
    InitializePhaseMap(1) = "IPDv00p1=1"
    call ESMF_AttributeAdd(cplComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_AttributeSet(cplComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Calls outgoing couple/grid component method and passes clock
    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
!> @subsubsection InitializeP1 "Initialize Method Phase 1"
!> @brief ESMF preperation method
!> @details Allocate Space, open files, set initial conditions
!> @param cplComp, importState, exportState, parentClock, rc
!! ESMF Coupler Component; ESMF States; Simulation Time; ESMF Return Code
subroutine InitializeP1(cplcomp, importState, exportState, externalclock, rc)
    !------------------------------------------------------------------
    implicit none

    !INPUTS / OUTPUTS
    type(ESMF_CplComp)          :: cplcomp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: externalclock
    integer, intent(out)        :: rc

    !LOCAL VARS
    type(ESMF_State)                        :: dba_import, dba_export
    type(ESMF_Time)                         :: currTime

    character(len=ESMF_MAXSTR)              :: name, substanceName, &
                                               attributeName, message

    integer                                 :: import_itemCount, &
                                               export_itemCount, &
                                               import_fieldCount=0, &
                                               export_fieldCount=0, &
                                               localrc, dba_rc,i,j, h

    logical                                 :: dba_verbose

    !LOCAL POINTER
    type(ESMF_State), pointer               :: ste =>null()
    type(ESMF_StateItem_Flag), allocatable  :: export_itemTypes(:), &
                                               import_itemTypes(:)

    character(len=ESMF_MAXSTR),dimension(:,:),pointer &
                                            :: dba_substances, &
                                               dba_aliases, &
                                               dba_equivalents
    character(len=ESMF_MAXSTR),allocatable  :: export_itemNames(:), &
                                               import_itemNames(:)


    integer(ESMF_KIND_I4)                   :: dba_value
    !------------------------------------------------------------------
    write(message,'(A)') "coupler      Init phase 1"
    call ESMF_LogWrite(message, ESMF_LOGMSG_TRACE)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if
    rc = ESMF_SUCCESS

    !> receive coupler component information
    call MOSSCO_CompEntry(cplComp, externalClock, name=name, currTime=currTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Call user-code method
    call mcpl_InitializeP1(cplcomp, importState, exportState, externalclock, rc)

    write(message,'(A)') "coupler      Return to init phase 1"
    call ESMF_LogWrite(message, ESMF_LOGMSG_TRACE)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if

    !> @paragraph dba "Database Arrays"
    !> @brief Create database array states (dba) for import and export

    dba_import=ESMF_StateCreate(name='dba_import', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    dba_export=ESMF_StateCreate(name='dba_export', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Get all Substances by unique name from database
    call get_substances_list(dba_substances)
    !> @todo: First execution after State declaration somehow causes wrong results (Array dimension +1)
    call get_substances_list(dba_substances)

    call ESMF_StateGet(importState, itemCount=import_itemCount)
    call ESMF_StateGet(exportState, itemCount=export_itemCount)
    allocate(import_itemNames(import_itemCount))
    allocate(import_itemTypes(import_itemCount))
    allocate(export_itemNames(export_itemCount))
    allocate(export_itemTypes(export_itemCount))

    if (debug .eqv. .true.) then
        write(*,*) ""
        write(*,*) ">*******Import/Export Count********"
        write(*,*) import_itemCount, export_itemCount
        write(*,*) "> **********************************"
        write(*,*) ""
        write(*,*) "> List of Substances (db)"
        write(*,'(A)') dba_substances
        write(*,*) ""
    end if

    !> get list and types of all items found in import / export state
    call ESMF_StateGet(importState, itemTypeList=import_itemTypes, itemNameList=import_itemNames, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_StateGet(exportState, itemTypeList=export_itemTypes, itemNameList=export_itemNames, rc=localRc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Log Items in Import / Export State

    write(message,'(A)') "All import fields"
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if

    do i=1, import_itemCount
        !> Print and count fields in import state
        if (import_itemTypes(i)==ESMF_STATEITEM_FIELD) then
            import_fieldCount=import_fieldCount+1
            write(message,'(A2,A126)') "+ ", import_itemNames(i)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
            if (debug) write(*,'(A)') message
        end if
    end do

    write(message,'(A)') "All export fields"
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if

    do i=1, export_itemCount
        !> Print and count fields in import state
        if (import_itemTypes(i)==ESMF_STATEITEM_FIELD) then
            import_fieldCount=import_fieldCount+1
            write(message,'(A2,A126)') "+ ", export_itemNames(i)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
            if (debug) write(*,'(A)') message
        end if
    end do


    !> Loop all SubstanceName - Appendices Combinations from the database
    do j=1, size(dba_substances)
        call get_substance_aliases_list(dba_substances(j,1), rulesets,dba_aliases)

        if (associated(dba_aliases)) then
            if (debug) then
                write(*,*) ""
                write(*,*) "> Searching list of aliases for ", trim(dba_substances(j,1)), ":"
                write(*,'(A)') ("- " // dba_aliases(i,1), i=1,(size(dba_aliases)))
                write(*,*) ""
            end if

            do i=1, (size(dba_aliases))

!                if (debug) then
!                    write(*,*) "---"
!                    write(*,*) "> searching ", trim(dba_aliases(i,1)), " in import"
!                end if

                !> Search combinations in import
                do h=1, import_itemCount
                    !> If found add them to import inventory
                    if ((import_itemNames(h)==dba_aliases(i,1)) .and. import_itemTypes(h)==ESMF_STATEITEM_FIELD) then
                        if (debug) write(*,*) "> adding ", trim(dba_aliases(i,1)), " to import attributes"

                        call ESMF_AttributeSet(dba_import, dba_aliases(i,1), 0, rc=localrc)
                            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
                                call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
                        exit
                    end if
                end do

!                if (debug) then
!                    write(*,*) "---"
!                    write(*,*) "> searching ", trim(dba_aliases(i,1)), " in export"
!                end if

                !> Search combinations in export
                do h=1, export_itemCount
                    !> If found add them to export inventory
                    if ((export_itemNames(h)==dba_aliases(i,1)) .and. export_itemTypes(h)==ESMF_STATEITEM_FIELD) then
                        if (debug) write(*,*) "> adding ", trim(dba_aliases(i,1)), " to export attributes"

                        dba_value=0
                        call ESMF_AttributeSet(state=dba_export, name=trim(dba_aliases(i,1)), value=dba_value, rc=localrc)
                            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
                                call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

!                        if (debug) then
!                            call ESMF_AttributeGet(state=dba_export, name=trim(dba_aliases(i,1)), value=dba_value, rc=localrc)
!                            if (localrc==ESMF_SUCCESS) then
!                                write(*,*) "> Found export item: ", trim(dba_aliases(i,1)), " used times: ", dba_value
!                            else
!                                write(*,*) "> Not found (export):", trim(dba_aliases(i,1))
!                            end if
!                        end if

                        exit
                    end if
                end do
            end do
        else
            write(message,*) "No aliases found for ", trim(dba_substances(j,1))
            call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)
            if (debug) write(*,*) "> ", message
        end if
    end do

    !> Complete database Arrays
    call ESMF_StateAddReplace(importState, (/dba_import/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateAddReplace(exportState, (/dba_export/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(cplComp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

end subroutine InitializeP1


#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
!> @subsubsection Run "Run Method"
!> @brief ESMF Simulation step execution method
!> @details Executes Transformations between import and export State
!> @param cplComp, importState, exportState, parentClock, rc
!! ESMF Coupler Component; ESMF States; Simulation Time; ESMF Return Code
subroutine Run(cplcomp, importState, exportState, externalclock, rc)
    !------------------------------------------------------------------
    !INPUTS / OUTPUTS
    type(ESMF_CplComp)          :: cplcomp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: externalclock
    integer, intent(out)        :: rc

    !LOCAL VARS
    type(ESMF_State)                        :: dba_import, dba_export
    type(ESMF_Time)                         :: currTime

    character(len=ESMF_MAXSTR),dimension(1:3) &
                                            :: substances_import, &
                                               substances_export
    character(len=ESMF_MAXSTR)              :: val, name, &
                                               attributeName, message
    integer(ESMF_KIND_R8)                   :: advanceCount
    integer(ESMF_KIND_I4)                   :: dba_value
    integer                                 :: c, n_i, n_e, c_f=0, &
                                               i, j, h, n, localrc, &
                                               n_req=0, n_inv, &
                                               used=0, used_max=0, &
                                               checkrc

    logical                                 :: hit

    !LOCAL POINTER
    type(ESMF_StateItem_Flag), allocatable  :: import_itemTypes(:), &
                                               export_itemTypes(:)

    character(len=ESMF_MAXSTR),dimension(:,:),pointer &
                                            :: dba_aliases
    character(len=ESMF_MAXSTR),allocatable  :: import_itemNames(:), &
                                               export_itemNames(:)
    character(len=ESMF_MAXSTR),allocatable,dimension(:) &
                                            :: required, &
                                               inventory, &
                                               arr
    character(len=ESMF_MAXSTR),pointer      :: sa_name

    real(ESMF_KIND_R8),dimension(:,:),pointer :: field=>null(), &
                                                 field_imp=>null(), &
                                                 field_exp=>null()

    logical                                 :: verbose=.true.

    !------------------------------------------------------------------
    write(message,'(A)') "coupler      Run"
    call ESMF_LogWrite(message, ESMF_LOGMSG_TRACE)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if
    rc = ESMF_SUCCESS

    !> receive coupler component information
    call MOSSCO_CompEntry(cplComp, externalClock, name=name, currTime=currTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGet(externalClock, advanceCount=advanceCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (advanceCount > 0) verbose=.false.

    !> Call user-code method
    call mcpl_Run_pre_recipe(cplcomp, importState, exportState, externalclock, rc)

    write(message,'(A)') "coupler      Return to run"
    call ESMF_LogWrite(message, ESMF_LOGMSG_TRACE)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if

    call ESMF_StateGet(importState, "dba_import", dba_import, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateGet(exportState, "dba_export", dba_export, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Get attribute number of import and export dba
    call ESMF_AttributeGet(dba_import, count=n_i)
    call ESMF_AttributeGet(dba_export, count=n_e)

    !> @paragraph: automated_copy  "Automated copy of Substances"
    !> @brief: Substances directly found in Import State are copied to the export State

    !> 1) Search all items directly by alias
    do i=1,n_e
        call ESMF_AttributeGet(dba_export,i,attributeName, rc=localrc)
        !> Check if used and found still are zero
        call ESMF_AttributeGet(dba_export,name=attributeName,value=dba_value)
            !> Count items declared as found by user-code
            if (dba_value/=0) then
                localrc=ESMF_RC_ARG_BAD
                c_f=c_f+1
            end if

        call ESMF_AttributeGet(dba_import,name=attributeName,value=dba_value,rc=checkrc)
            if (dba_value/=0) checkrc=ESMF_RC_ARG_BAD

        !> Add matching attributes with used=0 and found=0
        if ((localrc==ESMF_SUCCESS) .and. (checkrc==ESMF_SUCCESS)) then

            !> Check if field is still available
            call mossco_state_get(importState,(/attributeName/), field_imp, rc=localrc)
            call mossco_state_get(exportState,(/attributeName/), field_exp, rc=checkrc)

            if ((localrc==ESMF_SUCCESS) .and. (checkrc==ESMF_SUCCESS)) then
                if (debug) write(*,*) "> Found value: ", dba_value

                !> If found set used (import) and found (export) to 1
                dba_value=1
                call ESMF_AttributeSet(dba_export,name=attributeName,value=dba_value,rc=localrc)
                call ESMF_AttributeSet(dba_import,name=attributeName,value=dba_value,rc=localrc)

                !> Copy data
                field_exp=field_imp

                !> Add found item
                c_f=c_f+1
            else
                write(message,*) "Field ", attributeName, ", listed in dba_export, is no longer available in import or export state"
                call ESMF_LogWrite(message, ESMF_LOGMSG_ERROR)
                if (debug) write(*,*) "> ", message
            end if

        end if

    end do


    !> 2) Loop used level
    do while ( (c_f<n_e) .and. (used<=used_max) )
        hit=.true.

        !> Loop current used level until no more substances are found
        do while (hit)
            hit=.false.
            if(debug) then
                write(*,*) ""
                write(*,*) "> Start search (used", used, ")"
            end if
            !> Redim the Substance arrays
            if (allocated(required)) deallocate(required)
            if (allocated(inventory)) deallocate(inventory)
            n_req=n_e-c_f
            allocate(required(n_req))
            if (debug) write(*,*) "> ", n_req, " required substances remaining"

            !> Get required substances with found=0
            j=0
            do i=1,n_e
                call ESMF_AttributeGet(dba_export,i,attributeName)
                call ESMF_AttributeGet(dba_export,name=attributeName,value=dba_value)
                !write(*,'(A20, A)') "Searching attribute ", attributeName
                if (dba_value==0) then
                    j=j+1
                    required(j)=attributeName
                    !write(*,'(A20,A)') "Adding attribute ", attributeName
                end if
            end do

            !> Get inventory substances with current used level or below
            n_inv=0
            do i=1,n_i
                call ESMF_AttributeGet(dba_import,i,attributeName)
                call ESMF_AttributeGet(dba_import,name=attributeName,value=dba_value)
                if (dba_value<=used) n_inv=n_inv+1
            end do
            allocate(inventory(n_inv))
            if (debug) write(*,*) "> Found ", n_inv, " substances in inventory for current used level"
            j=0
            do i=1,n_i
                call ESMF_AttributeGet(dba_import,i,attributeName)
                call ESMF_AttributeGet(dba_import,name=attributeName,value=dba_value)
                if (dba_value<=used) then
                    j=j+1
                    inventory(j)=attributeName
                end if
            end do


            !> 3) Loop all export substances
            do i=1,n_req
                if (debug) write(*,*) "> Looping ", required(i)
                call get_equivalent_appendix_name(required(i),rulesets,sa_name)
                    if (.not. associated(sa_name)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
                call get_substance_appendix_aliases_list(sa_name, rulesets, dba_aliases)
                    if (.not. associated(dba_aliases)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

                !> 4) Loop all alias combinations for all substances
                do j=1, size(dba_aliases)

                    !> 5) Check all inventory entries at the current used level for all combinations
                    do h=1,n_inv
                        if (dba_aliases(j,1)==inventory(h)) then
                        !> Check if field is still available
                        call mossco_state_get(importState,(/inventory(h)/), field_imp, rc=localrc)
                        call mossco_state_get(exportState,(/required(i)/), field_exp, rc=checkrc)

                            if ((localrc==ESMF_SUCCESS) .and. (checkrc==ESMF_SUCCESS)) then

                                !> Increase used value of Substance and max used if neccesary
                                call ESMF_AttributeGet(dba_import,name=inventory(h),value=dba_value,rc=localrc)
                                dba_value=dba_value+1
                                call ESMF_AttributeSet(dba_import,name=inventory(h),value=dba_value,rc=localrc)
                                if (dba_value>used_max) used_max=dba_value

                                !> Set found value to 1
                                dba_value=1
                                call ESMF_AttributeSet(dba_export,name=required(i),value=dba_value,rc=localrc)
                                c_f=c_f+1

                                hit=.true.
                                if (debug) write(*,*) "> Found hit"

                                !> Copy data
                                field_exp=field_imp
                            else
                                write(message,*) "Field ", attributeName, ", listed in dba_export, is no longer available in import or export state"
                                call ESMF_LogWrite(message, ESMF_LOGMSG_ERROR)
                                if (debug) write(*,*) "> ", message
                            end if
                        end if
                        if (hit) exit
                    end do
                    if (hit) exit
                end do
                if (hit) exit
            end do
        end do
        if (debug) write(*,*) ">> No more hits found, increasing used level"
        used=used+1
    end do
    if (debug) write(*,*) ">> Everything found or nothing more to find"


    !> @paragraph: automated_recipe "Automatic Recipe Search"
    !> @brief: Recursurve search in the database for fitting recipes

    !> @todo: Implementation
    !> Receive equation as string, read and execute the equation


    call mcpl_Run_pre_log(cplcomp, importState, exportState, externalclock, rc)

    !> Get attribute number of import and export dba
    call ESMF_AttributeGet(dba_import, count=n_i)
    call ESMF_AttributeGet(dba_export, count=n_e)

    c_f=0
    do i=1,n_e
        call ESMF_AttributeGet(dba_export,i,attributeName, rc=localrc)
        call ESMF_AttributeGet(dba_export,name=attributeName,value=dba_value)
        !> Count items declared as found by user-code
        if (dba_value==1) c_f=c_f+1
    end do
    n_req=n_e-c_f


    write(message,'(A)') "coupler      Return to run"
    call ESMF_LogWrite(message, ESMF_LOGMSG_TRACE)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if


    !> @paragraph: final_log "Final log"
    !> @brief: Analyse data, Log found and not found substances

    write(message,'(A)') "Identified the following substances in import state:"
    call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if

    do i=1,n_i
        call ESMF_AttributeGet(dba_import,i,attributeName)
        call ESMF_AttributeGet(dba_import,name=attributeName,value=dba_value)
        call get_equivalent_appendix_name(attributeName,rulesets,sa_name)

        write(message,"(A,A)") "+ ", trim(attributeName)// ", identified as"
        call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)
        if (debug) write(*,*) message
        write(message,"(A2, A111,A13,I2)") "  ", adjustl(sa_name), ", used times ", dba_value
        call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)
        if (debug) write(*,*) message
    end do

    write(message,'(A)') "Identified the following substances in export state:"
    call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if

    do i=1,n_e
        call ESMF_AttributeGet(dba_export,i,attributeName)
        call ESMF_AttributeGet(dba_export,name=attributeName,value=dba_value)
        call get_equivalent_appendix_name(attributeName,rulesets,sa_name)

        if (dba_value == 1) then
            write(message,'(A, A)') "+ ", trim(attributeName) // ", identified as "
            call ESMF_LogWrite(adjustl(message), ESMF_LOGMSG_INFO)
            if (debug) write(*,*) adjustl(message)

            write(message,'(A2, A111, A15)') "  ", adjustl(sa_name), ", found.       "
            call ESMF_LogWrite(adjustl(message), ESMF_LOGMSG_INFO)
            if (debug) write(*,*) "  ", adjustl(message)
        else
            write(message,'(A, A)') "+ ", trim(attributeName) // ", identified as "
            call ESMF_LogWrite(adjustl(message), ESMF_LOGMSG_INFO)
            if (debug) write(*,*) adjustl(message)

            write(message,'(A2, A111, A15)') "  ", adjustl(sa_name), ", not found.   "
            call ESMF_LogWrite(adjustl(message), ESMF_LOGMSG_INFO)
            if (debug) write(*,*) "  ", adjustl(message)
        end if
    end do

    write(message,'(I2,A8,I2,A)') c_f, " of the ", n_e, " export fields have been found."
    call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if

    if (used_max<=1) then
        write(message,'(A)') "All import fields only used up to 1 time."
        call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)
        if (debug) then
            write(*,*) ""
            write(*,*) "> ", message
        end if
    else
        write(message,'(A30,I2,A7)') "Import fields were used up to ", used_max, " times."
        call ESMF_LogWrite(message, ESMF_LOGMSG_INFO)
        if (debug) then
            write(*,*) ""
            write(*,*) "> ", message
        end if
    end if

    call MOSSCO_CompExit(cplComp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

end subroutine Run


#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
!> @subsubsection Finalize "Finalize Method"
!> @brief ESMF End of Execution method
!> @details deallocate space, close files, print results
!> @param cplComp, importState, exportState, parentClock, rc
!! ESMF Coupler Component; ESMF States; Simulation Time; ESMF Return Code
subroutine Finalize(cplcomp, importState, exportState, externalclock, rc)
    !------------------------------------------------------------------
    !INPUTS / OUTPUTS
    type(ESMF_CplComp)          :: cplcomp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: externalclock
    integer,intent(out)         :: rc

    !LOCAL VARS
    character(len=ESMF_MAXSTR)  :: name, message, paramName
    type(ESMF_State)            :: paramState
    type(ESMF_Time)             :: currTime
    integer                     :: localrc
    type(ESMF_StateItem_Flag)   :: exportItemType, importItemType
    !------------------------------------------------------------------
    write(message,'(A)') "coupler      Finalize"
    call ESMF_LogWrite(message, ESMF_LOGMSG_TRACE)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if
    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(cplComp, externalClock, name, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call mcpl_finalize(cplcomp, importState, exportState, externalclock, rc)

    write(message,'(A)') "coupler      Return to finalize"
    call ESMF_LogWrite(message, ESMF_LOGMSG_TRACE)
    if (debug) then
        write(*,*) ""
        write(*,*) "> ", message
    end if

    call MOSSCO_CompExit(cplComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

end subroutine Finalize

end module soil_pelagic_mediator



!@dev: area for all general coupler routines
!Outsource and 'use' in all Coupler modules
!----------------------------------------------------------------------
!------------------- General MOSSCO coupler Routines ------------------
!----------------------------------------------------------------------
!module mossco_mediator
!
!
!end module mossco_mediator


!####### TEMPLATE #####################################################
!#undef  ESMF_METHOD
!#define ESMF_METHOD "User_Code"
!> @brief
!> @param
!subroutine User_Code()
!    !------------------------------------------------------------------
!    !INPUTS / OUTPUTS
!
!    !LOCAL VARS
!
!    !------------------------------------------------------------------
!
!end subroutine User_Code
