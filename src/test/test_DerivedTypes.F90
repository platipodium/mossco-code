!> @file test_DerivedTypes.F90
!! @brief Tests derived types of ESMF_GridComp
!! @author Carsten Lemmen
!!

program test_DerivedTypes
use esmf

integer :: rc
type(ESMF_GridComp) :: testComp


#ifdef ESMF_NO_SEQUENCE

type, extends(ESMF_GridComp) :: MOSSCO_GridComp
  character(len=ESMF_MAXSTR) :: output_filename
  integer                    :: instance_number
end type

#else

type :: MOSSCO_GridComp
  type(ESMF_GridComp) :: comp
  integer             :: instance_number
end type

#endif


type(MOSSCO_GridComp) :: test

call ESMF_Initialize()

#ifdef ESMF_NO_SEQUENCE
!test=ESMF_GridCompCreate(name='test component')
testcomp=ESMF_GridCompCreate(name='test component')
call ESMF_LogWrite('Created ESMF extended test Component', ESMF_LOGMSG_INFO, rc=rc)

#else
test%comp=ESMF_GridCompCreate(name='test component')
call ESMF_LogWrite('Created ESMF nested Component', ESMF_LOGMSG_INFO, rc=rc)
#endif

call ESMF_LogFlush(rc=rc)
call ESMF_Finalize()

end program

