!> @file test_ESMF_Vm.F90
!! @brief test accessing the vm object
!! @author Carsten Lemmen
!!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "test_ESMF_Vm.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#define ESMF_METHOD "test_ESMF_Vm"
program test_ESMF_Vm

use esmf

integer       :: rc, localrc, petCount, localPet
type(ESMF_Vm) :: vm

call ESMF_Initialize(rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_VmGetGlobal(vm=vm, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_VmGet(vm=vm, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

call ESMF_Finalize()

end program
