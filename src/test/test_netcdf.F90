!> @brief Implementation of netcdf test functions
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2015 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

program test_netcdf

  use netcdf

  implicit none

  integer :: ncid, varid, status

  status = nf90_create(path='test_netcdf.nc', cmode=NF90_CLOBBER, ncid=ncid)
  status = nf90_close(ncid)

  status = nf90_open(path='test_netcdf.nc', mode=NF90_WRITE, ncid=ncid)
  status = nf90_close(ncid)

end program
