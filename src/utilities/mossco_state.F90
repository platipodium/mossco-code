module mossco_state

use esmf
implicit none

interface mossco_state_get
    module procedure mossco_state_get_f2
    module procedure mossco_state_get_f3
end interface

contains

  subroutine mossco_state_get_f2(state,name,fpointer,rc)
    type(ESMF_State) :: state
    character(len=*),dimension(:) :: name
    type(ESMF_Field) :: field
    real(ESMF_KIND_R8),pointer,dimension(:,:) :: fpointer
    integer(ESMF_KIND_I4) :: esmfrc,i
    integer,intent(out) :: rc
    type(ESMF_StateItem_Flag) :: itemType

    rc=1
    do i=1,size(name)
      call ESMF_StateGet(state,trim(name(i)),itemType, rc=esmfrc) ! this is really a call to StateGetInfo
      if(itemtype == ESMF_STATEITEM_NOTFOUND) then
!         write(0,*) 'not found field ',trim(name(i))
        continue
      else
         if(esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!        write(0,*) 'found field ',trim(name(i))
         call ESMF_StateGet(state,trim(name(i)),field,rc=esmfrc)
         if(esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
         call ESMF_FieldGet(field,localde=0,farrayPtr=fpointer,rc=esmfrc)
         if(esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
         rc=0
         exit
      end if
    end do
  end subroutine mossco_state_get_f2

  subroutine mossco_state_get_f3(state,name,fpointer,rc)
    type(ESMF_State) :: state
    type(ESMF_Field) :: field
    character(len=*),dimension(:) :: name
    real(ESMF_KIND_R8),pointer,dimension(:,:,:) :: fpointer
    integer(ESMF_KIND_I4) :: esmfrc,i
    integer,intent(out) :: rc
    type(ESMF_StateItem_Flag) :: itemType

    rc=1
    do i=1,size(name)
      call ESMF_StateGet(state,trim(name(i)),itemType, rc=esmfrc)
      if(itemType == ESMF_STATEITEM_NOTFOUND) then
!         write(0,*) 'not found field ',trim(name(i))
        continue
      else
         if(esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!        write(0,*) 'found field ',trim(name(i))
         call ESMF_StateGet(state,trim(name(i)),field,rc=esmfrc)
         if(esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
         call ESMF_FieldGet(field,localde=0,farrayPtr=fpointer,rc=esmfrc)
         if(esmfrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
         rc=0
         exit
      end if
    end do
  end subroutine mossco_state_get_f3

 

end module mossco_state
