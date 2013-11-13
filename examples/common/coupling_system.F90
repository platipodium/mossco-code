!> @brief Definition of coupling system as given in yaml dialect
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen, Helmholtz-Zentrum Geesthacht
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

module coupling_system

implicit none

private

type component_description
  character(len=255) :: name
  character(len=511) :: description
  integer, dimension(3) :: grid_size
  character(len=255) :: filename
end type component_description

type coupling_description
  character(len=255), dimension(2) :: component_name
  character(len=255)               :: direction
  character(len=255)               :: filename
  logical                          :: forward, backward
  character(len=255)               :: coupler
  type(component_description), dimension(2) :: components

  contains 
   
  procedure parse

end type coupling_description

contains

subroutine parse(self, filename)
  class (coupling_description) :: self
  character(len=255) :: filename, string
  integer :: rc, lun

  call get_lun(lun)
  open(unit=lun,file=filename,status='old',action='read',iostat=rc) 
  do while (rc == 0)
    read(lun,*) string
    print string
  enddo
  close(lun)
  call free_lun(lun)
                   
end subroutine parse

end module coupling_system
