module mossco_state

use esmf
implicit none

interface mossco_state_get
    module procedure mossco_state_get_f2
    module procedure mossco_state_get_f3
end interface

contains



  subroutine mossco_state_get_f2(state,name,fpointer)
    type(ESMF_State) :: state
    character(len=*) :: name
    type(ESMF_Field) :: field
    real(ESMF_KIND_R8),pointer,dimension(:,:) :: fpointer
    integer :: rc
    call ESMF_StateGet(state,name,field,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field,localde=0,farrayPtr=fpointer,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  end subroutine mossco_state_get_f2

  subroutine mossco_state_get_f3(state,name,fpointer)
    type(ESMF_State) :: state
    type(ESMF_Field) :: field
    character(len=*) :: name
    integer :: rc
    real(ESMF_KIND_R8),pointer,dimension(:,:,:) :: fpointer
    call ESMF_StateGet(state,name,field,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_FieldGet(field,localde=0,farrayPtr=fpointer,rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
  end subroutine mossco_state_get_f3

 

end module mossco_state
