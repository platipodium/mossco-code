!> @brief Implementation of an inout ESMF coupler component, all input is
!>        mirrored to output
!> @file inout_coupler.F90
!!
!  This computer program is part of MOSSCO. 
!> @copyright Copyright (C) 2013, 2014 Helmholtz-Zentrum Geesthacht 
!> @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

module inout_coupler

  use esmf
!  use mossco_component

  implicit none

  private
  public SetServices

  contains

  subroutine SetServices(Cplcomp, rc)

    type(ESMF_CplComp)   :: Cplcomp
    integer, intent(out) :: rc

    call ESMF_CplCompSetEntryPoint(Cplcomp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=rc)
    call ESMF_CplCompSetEntryPoint(Cplcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=rc)
    call ESMF_CplCompSetEntryPoint(Cplcomp, ESMF_METHOD_RUN, Run, rc=rc)
    call ESMF_CplCompSetEntryPoint(Cplcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)

  end subroutine SetServices

  subroutine InitializeP0(CplComp, importState, exportState, parentClock, rc)
 
    implicit none
  
    type(ESMF_CplComp)    :: CplComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)       :: currTime

    call MOSSCO_CompEntry(CplComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(CplComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=rc)
    call ESMF_AttributeSet(CplComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=rc)

    call MOSSCO_CompExit(CplComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine InitializeP0

  subroutine InitializeP1(CplComp, importState, exportState, parentClock, rc)
    
    type(ESMF_CplComp)    :: CplComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(ESMF_MAXSTR):: name, message
    type(ESMF_Time)       :: currTime
    
    !! Check whether there is already a clock (it might have been set 
    !! with a prior ESMF_CplCompCreate() call.  If not, then create 
    !! a local clock as a clone of the parent clock, and associate it
    !! with this component.  Finally, set the name of the local clock
    call MOSSCO_CompEntry(CplComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

	  exportState=importState
	  
    !! Finally, log the successful completion of this function
    call MOSSCO_CompExit(CplComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine InitializeP1

  subroutine Run(CplComp, importState, exportState, parentClock, rc)
    
    type(ESMF_CplComp)      :: CplComp
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: parentClock
    integer, intent(out)    :: rc

    character(ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)         :: currTime, stopTime
    type(ESMF_Clock)        :: clock
     
    call MOSSCO_CompEntry(CplComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_CplCompGet(CplComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  	exportState=importState
	  clock=parentClock
	  
    !! Finally, log the successful completion of this function
    call MOSSCO_CompExit(CplComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine Run

  subroutine Finalize(CplComp, importState, exportState, parentClock, rc)
    
    type(ESMF_CplComp)    :: CplComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock

    call MOSSCO_CompEntry(CplComp, parentClock, name, currTime, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_CplCompGet(CplComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
   
    !! Here comes your own finalization code
    !! 1. Destroy all fields that you created, be aware that other components
    !!    might have interfered with your fields, e.g., moved them into a fieldBundle
    !! 2. Deallocate all your model's internal allocated memory    
    !! 3. Destroy your clock

    !! @todo The clockIsPresent statement does not detect if a clock has been destroyed 
    !! previously, thus, we comment the clock destruction code while this has not
    !! been fixed by ESMF
    call ESMF_ClockDestroy(clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    !! Finally, log the successful completion of this function
    call MOSSCO_CompExit(CplComp, rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

  end subroutine Finalize

end module inout_coupler
