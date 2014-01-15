module mossco_variable_types

#ifdef ESMF
  use esmf
#endif  
  
  implicit none

  type, public :: MOSSCO_VariableInfo
    character(len=255) :: name  = ''   ! Short name
    character(len=255) :: unit = ''    ! Units
    character(len=511) :: standard_name = '' ! CF standard name 
    character(len=511) :: description = '' ! long description 
    
    contains 
      procedure :: as_yaml
    
  end type

#ifdef ESMF 
  type, extends(MOSSCO_VariableInfo), public :: MOSSCO_VariableFArray0d
     real(ESMF_KIND_R8), pointer :: data
  end type
  type, extends(MOSSCO_VariableInfo), public :: MOSSCO_VariableFArray1d
     real(ESMF_KIND_R8), pointer, dimension(:) :: data
  end type
  type, extends(MOSSCO_VariableInfo), public :: MOSSCO_VariableFArray2d
     real(ESMF_KIND_R8), pointer, dimension(:,:) :: data
  end type
  type, extends(MOSSCO_VariableInfo), public :: MOSSCO_VariableFArray3d
     real(ESMF_KIND_R8), pointer, dimension(:,:,:) :: data
  end type
  type, extends(MOSSCO_VariableInfo), public :: MOSSCO_VariableFArray4d
     real(ESMF_KIND_R8), pointer, dimension(:,:,:,:) :: data
  end type
#else
  type, extends(MOSSCO_VariableInfo), public :: MOSSCO_VariableFArray0d
     double precision, pointer :: data
  end type
  type, extends(MOSSCO_VariableInfo), public :: MOSSCO_VariableFArray1d
     double precision, pointer, dimension(:) :: data
  end type
  type, extends(MOSSCO_VariableInfo), public :: MOSSCO_VariableFArray2d
     double precision, pointer, dimension(:,:) :: data
  end type
  type, extends(MOSSCO_VariableInfo), public :: MOSSCO_VariableFArray3d
     double precision, pointer, dimension(:,:,:) :: data
  end type
  type, extends(MOSSCO_VariableInfo), public :: MOSSCO_VariableFArray4d
     double precision, pointer, dimension(:,:,:,:) :: data
  end type
#endif 


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
