!> @brief Implementation of generic interface test functions
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2017 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

module mossco_interface

  use esmf
  implicit none

  interface MOSSCO_AttributeSet

    module procedure MOSSCO_StateAttributeSetList1
    module procedure MOSSCO_CplCompAttributeSetList1

  ! module procedure MOSSCO_StateAttributeSetLogical
  ! module procedure MOSSCO_StateAttributeSetList2
  ! module procedure MOSSCO_StateAttributeSetInt4List1
  ! module procedure MOSSCO_CplCompAttributeSetStringListPtr
  ! module procedure MOSSCO_StateAttributeSetStringListPtr
  ! module procedure MOSSCO_CplCompAttributeSetList2
  ! module procedure MOSSCO_GridCompAttributeSetList1
  ! module procedure MOSSCO_GridCompAttributeSetList2
  ! module procedure MOSSCO_GridCompAttributeSetInt4List1
  end interface MOSSCO_AttributeSet

  contains

  subroutine MOSSCO_CplCompAttributeSetList1(cplComp, label, stringList, rc)
    type(ESMF_CplComp), intent(inout)  :: cplComp
    character(len=*), intent(in)  :: label
    character(len=*), intent(in), allocatable :: stringList(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc
  end subroutine MOSSCO_CplCompAttributeSetList1

  subroutine MOSSCO_StateAttributeSetList1(state, label, stringList, rc)
    type(ESMF_State), intent(inout)  :: state
    character(len=*), intent(in)  :: label
    character(len=*), intent(in), allocatable :: stringList(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc
  end subroutine MOSSCO_StateAttributeSetList1

end module

program test_interface

  use ESMF

  implicit none

  call ESMF_Initialize()
  call ESMF_Finalize()

end program
