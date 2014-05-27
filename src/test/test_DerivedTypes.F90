!> @file test_DerivedTypes.F90
!! @brief Tests derived types of ESMF_GridComp
!! @author Carsten Lemmen
!!

program test_DerivedTypes
use esmf

integer :: rc
type(ESMF_GridComp) :: testComp

#define EXTEND_ESMF

#ifndef EXTEND_ESMF
type :: MOSSCO_GridComp
  type(ESMF_GridComp) :: comp
  integer             :: instance_number
end type

type(MOSSCO_GridComp) :: test

#else
type, extends(ESMF_GridComp) :: MOSSCO_GridComp
  character(len=ESMF_MAXSTR) :: output_filename
  integer                    :: instance_number
end type
#endif

call ESMF_Initialize()

#ifdef EXTEND_ESMF
testcomp=ESMF_GridCompCreate(name='test component')
#else
test%comp=ESMF_GridCompCreate(name='test component')
#endif

call ESMF_Finalize()

end program

