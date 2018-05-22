!> @brief Implementation of mesh utilities
!!
!! This computer program is part of MOSSCO.
!! @copyright Copyright 2018 Helmholtz-Zentrum Geesthacht
!! @author Carsten Lemmen <carsten.lemmen@hzg.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#define ESMF_FILENAME "mossco_mesh.F90"

#define RANGE2D lbnd(1):ubnd(1),lbnd(2):ubnd(2)

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module mossco_mesh

  use esmf
  use mossco_strings

  implicit none

  public MOSSCO_MeshString

  private

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_MeshString"
subroutine MOSSCO_MeshString(mesh, message, kwe, length, rc)

  type(ESMF_Mesh), intent(in)                    :: mesh
  character(len=ESMF_MAXSTR), intent(inout)      :: message
  logical, intent(in), optional                  :: kwe
  integer(ESMF_KIND_I4), intent(inout), optional :: length
  integer(ESMF_KIND_I4), intent(out), optional   :: rc

  integer(ESMF_KIND_I4)   :: rc_, length_, spatialDim, parametricDim, localrc
  integer(ESMF_KIND_I4)   :: numOwnedNodes, numOwnedElements
  character(ESMF_MAXSTR)  :: string, name, formatString

  logical                            :: isPresent

  rc_ = ESMF_SUCCESS
  if (present(kwe)) rc_ = ESMF_SUCCESS

  name = 'mesh'
  isPresent=.false.
  !> @todo there is no name property in meshes yet, neither are there attributes
  !> there is a ticket requested with ESMF
  !call ESMF_MeshGet(mesh, name=name, rc=localrc)
  !_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  !call ESMF_AttributeGet(mesh, name='creator', isPresent=isPresent, rc=localrc)
  !_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  if (isPresent) then
    !call ESMF_AttributeGet(mesh, name='creator', value=string, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    call MOSSCO_MessageAdd(message, ' ['//string)
    call MOSSCO_MessageAdd(message, ']'//name)
  else
    call MOSSCO_MessageAdd(message,' '//name)
  endif

  call ESMF_MeshGet(mesh, parametricDim=parametricDim, spatialDim=spatialDim, &
    numOwnedNodes=numOwnedNodes, numOwnedElements=numOwnedElements, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)

  write(formatString,'(A,A)') '(A,'//intformat(parametricDim)//',A,',&
    intformat(spatialDim)//')'    
  write(string,formatString) '(d=',parametricDim,'/',spatialDim
  
  write(formatString,'(A,A)') '(A,'//intformat(numOwnedNodes)//',A,', &
    intformat(numOwnedElements)//',A)'
  write(string,formatString) trim(string)//' n=',numOwnedNodes,' e=',numOwnedElements,')'
  call MOSSCO_MessageAdd(message,trim(string))

  length_=len_trim(message)
  if (present(length)) length=length_
  if (present(rc)) rc=rc_

end subroutine MOSSCO_MeshString

end module mossco_mesh
