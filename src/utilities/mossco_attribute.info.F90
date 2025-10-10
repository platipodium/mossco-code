module mossco_info_attr_get
  use ESMF
  implicit none
  private
  public :: MOSSCO_AttributeGet, MOSSCO_AttributeGetPtr

  interface MOSSCO_AttributeGet
    module procedure AttrGet_Mesh_Scalar
    module procedure AttrGet_Mesh_Array
    ! add: AttrGet_Field_Scalar, AttrGet_Field_Array, ..., for all object types
  end interface

  interface MOSSCO_AttributeGetPtr
    module procedure AttrGet_Mesh_ScalarPtr
    module procedure AttrGet_Mesh_ArrayPtr
    ! add: AttrGet_Field_ScalarPtr, AttrGet_Field_ArrayPtr, ..., for all object types
  end interface

contains

  pure function normkey(key) result(k)
    character(len=*), intent(in) :: key
    character(len=len_trim(key)) :: k
    k = trim(key)
  end function normkey

  ! Core getters on Info (polymorphic value)
  subroutine MOSSCO_InfoGetScalar(info, key, val, rc)
    type(ESMF_Info), intent(in)  :: info
    character(len=*), intent(in) :: key
    class(*), intent(out)        :: val
    integer, intent(out)         :: rc
    rc = ESMF_SUCCESS
    select type(v => val)
    type is (integer);              call ESMF_InfoGet(info, normkey(key), v, rc=rc)
    type is (integer(ESMF_KIND_I4));call ESMF_InfoGet(info, normkey(key), v, rc=rc)
    type is (integer(ESMF_KIND_I8));call ESMF_InfoGet(info, normkey(key), v, rc=rc)
    type is (real(ESMF_KIND_R8));   call ESMF_InfoGet(info, normkey(key), v, rc=rc)
    type is (real(ESMF_KIND_R4));   call ESMF_InfoGet(info, normkey(key), v, rc=rc)
    type is (logical);              call ESMF_InfoGet(info, normkey(key), v, rc=rc)
    type is (character(len=*));     call ESMF_InfoGet(info, normkey(key), v, rc=rc)
    class default
      rc = ESMF_RC_ARG_BAD
      call ESMF_LogMsgSet(ESMF_LOGMSG_ERROR, msg="Unsupported scalar type in MOSSCO_InfoGetScalar", rc=rc)
    end select
  end subroutine

  subroutine MOSSCO_InfoGetArray(info, key, val, rc)
    type(ESMF_Info), intent(in)  :: info
    character(len=*), intent(in) :: key
    class(*), intent(out)        :: val(:)
    integer, intent(out)         :: rc
    rc = ESMF_SUCCESS
    select type(v => val)
    type is (integer);              call ESMF_InfoGet(info, normkey(key), v, rc=rc)
    type is (integer(ESMF_KIND_I4));call ESMF_InfoGet(info, normkey(key), v, rc=rc)
    type is (integer(ESMF_KIND_I8));call ESMF_InfoGet(info, normkey(key), v, rc=rc)
    type is (real(ESMF_KIND_R8));   call ESMF_InfoGet(info, normkey(key), v, rc=rc)
    type is (real(ESMF_KIND_R4));   call ESMF_InfoGet(info, normkey(key), v, rc=rc)
    type is (logical);              call ESMF_InfoGet(info, normkey(key), v, rc=rc)
    type is (character(len=*));     call ESMF_InfoGet(info, normkey(key), v, rc=rc)
    class default
      rc = ESMF_RC_ARG_BAD
      call ESMF_LogMsgSet(ESMF_LOGMSG_ERROR, msg="Unsupported array type in MOSSCO_InfoGetArray", rc=rc)
    end select
  end subroutine

  ! Pointer helpers (require association)
  subroutine MOSSCO_InfoGetScalarPtr(info, key, val, rc)
    type(ESMF_Info), intent(in)     :: info
    character(len=*), intent(in)    :: key
    class(*), pointer, intent(inout):: val
    integer, intent(out)            :: rc
    if (.not. associated(val)) then
      rc = ESMF_RC_ARG_BAD
      call ESMF_LogMsgSet(ESMF_LOGMSG_ERROR, msg="Disassociated pointer in AttributeGet (scalar)", rc=rc)
      return
    end if
    call MOSSCO_InfoGetScalar(info, key, val, rc)
  end subroutine

  subroutine MOSSCO_InfoGetArrayPtr(info, key, val, rc)
    type(ESMF_Info), intent(in)     :: info
    character(len=*), intent(in)    :: key
    class(*), pointer, intent(inout):: val(:)
    integer, intent(out)            :: rc
    if (.not. associated(val)) then
      rc = ESMF_RC_ARG_BAD
      call ESMF_LogMsgSet(ESMF_LOGMSG_ERROR, msg="Disassociated pointer in AttributeGet (array)", rc=rc)
      return
    end if
    call MOSSCO_InfoGetArray(info, key, val, rc)
  end subroutine

  ! Example: Mesh wrappers (repeat same pattern for Field, GridComp, CplComp, State, Grid)

  subroutine AttrGet_Mesh_Scalar(obj, key, val, rc)
    type(ESMF_Mesh), intent(in) :: obj
    character(len=*), intent(in):: key
    class(*), intent(out)       :: val
    integer, intent(out), optional :: rc
    type(ESMF_Info) :: info; integer :: r
    call ESMF_MeshGet(obj, info=info, rc=r)
    if (r==ESMF_SUCCESS) call MOSSCO_InfoGetScalar(info, key, val, r)
    if (present(rc)) rc=r
  end subroutine

  subroutine AttrGet_Mesh_Array(obj, key, val, rc)
    type(ESMF_Mesh), intent(in) :: obj
    character(len=*), intent(in):: key
    class(*), intent(out)       :: val(:)
    integer, intent(out), optional :: rc
    type(ESMF_Info) :: info; integer :: r
    call ESMF_MeshGet(obj, info=info, rc=r)
    if (r==ESMF_SUCCESS) call MOSSCO_InfoGetArray(info, key, val, r)
    if (present(rc)) rc=r
  end subroutine

  subroutine AttrGet_Mesh_ScalarPtr(obj, key, val, rc)
    type(ESMF_Mesh), intent(in)    :: obj
    character(len=*), intent(in)   :: key
    class(*), pointer, intent(inout):: val
    integer, intent(out), optional :: rc
    type(ESMF_Info) :: info; integer :: r
    call ESMF_MeshGet(obj, info=info, rc=r)
    if (r==ESMF_SUCCESS) call MOSSCO_InfoGetScalarPtr(info, key, val, r)
    if (present(rc)) rc=r
  end subroutine

  subroutine AttrGet_Mesh_ArrayPtr(obj, key, val, rc)
    type(ESMF_Mesh), intent(in)    :: obj
    character(len=*), intent(in)   :: key
    class(*), pointer, intent(inout):: val(:)
    integer, intent(out), optional :: rc
    type(ESMF_Info) :: info; integer :: r
    call ESMF_MeshGet(obj, info=info, rc=r)
    if (r==ESMF_SUCCESS) call MOSSCO_InfoGetArrayPtr(info, key, val, r)
    if (present(rc)) rc=r
  end subroutine

end module mossco_info_attr_get
