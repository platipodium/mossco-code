module mossco_variable_types

#ifdef ESMF
  use esmf
#else
#define ESMF_MAXSTR 256
#define ESMF_MAXPATHLEN 512
#endif

  implicit none

  type, public, abstract :: MOSSCO_VariableInfo
    character(len=ESMF_MAXSTR) :: csdms_name  = ''  ! CSDMS/GSN name
    character(len=ESMF_MAXSTR) :: name  = ''   ! Short name
    character(len=ESMF_MAXSTR) :: units = ''    ! Units
    character(len=ESMF_MAXSTR) :: standard_name = '' ! CF standard name
    character(len=ESMF_MAXSTR) :: creator = '' !
    character(len=ESMF_MAXPATHLEN) :: description = '' ! long description
    logical            :: optional = .false.
    contains
      procedure :: as_yaml
  end type

#ifdef ESMF
integer, parameter :: MOSSCO_KIND_R8=ESMF_KIND_R8
#else
integer, parameter :: MOSSCO_KIND_R8=selected_real_kind(13)
#endif

  type, extends(MOSSCO_VariableInfo), public :: MOSSCO_VariableFloat
    real(MOSSCO_KIND_R8) :: valid_min = -1e30+1.0
    real(MOSSCO_KIND_R8) :: valid_max = huge(1.0_MOSSCO_KIND_R8)
    real(MOSSCO_KIND_R8) :: missing_value = -1.0e30
  end type

  type, extends(MOSSCO_VariableFloat), public :: MOSSCO_VariableFArray0d
     real(MOSSCO_KIND_R8), pointer :: data=>NULL()
     integer :: rank = 0
  end type
  type, extends(MOSSCO_VariableFloat), public :: MOSSCO_VariableFArray1d
     real(MOSSCO_KIND_R8), pointer, dimension(:) :: data=>NULL()
     integer :: rank = 1
  end type
  type, extends(MOSSCO_VariableFloat), public :: MOSSCO_VariableFArray2d
     real(MOSSCO_KIND_R8), pointer, dimension(:,:) :: data=>NULL()
     integer :: rank = 2

  end type
  type, extends(MOSSCO_VariableFloat), public :: MOSSCO_VariableFArray3d
     real(MOSSCO_KIND_R8), pointer, dimension(:,:,:) :: data=>NULL()
     integer :: rank = 3
  end type
  type, extends(MOSSCO_VariableFloat), public :: MOSSCO_VariableFArray4d
     real(MOSSCO_KIND_R8), pointer, dimension(:,:,:,:) :: data=>NULL()
     integer :: rank = 4
  end type

contains

subroutine as_yaml(self)

  class(MOSSCO_VariableInfo) :: self

  if (len(self%name) > 0) then
    write(*,'(A)') self%name
    if (len(self%standard_name) > 0) write(*,'(A,A)') '  standard_name:',self%standard_name
    if (len(self%description) > 0) write(*,'(A,A)') '  description:',self%description
  endif

end subroutine as_yaml

end module mossco_variable_types
