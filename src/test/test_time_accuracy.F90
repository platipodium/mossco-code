!> @brief Test ESMF-Timeinterval accuracy
!
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2018 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hereon.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

program test_time_accuracy

  use esmf

  implicit none

  real(ESMF_KIND_R8)      :: s_r8, ms_r8, us_r8, real8=0
  integer(ESMF_KIND_I4)   :: localrc
  integer(ESMF_KIND_I8)   :: i, j
  type(ESMF_Time)         :: tic, toc
  type(ESMF_TimeInterval) :: timeInterval

  call ESMF_Initialize(rc=localrc)

  call ESMF_TimeSet(tic, rc=localrc)
  call ESMF_TimeSet(toc, rc=localrc)

  do i=1,20

    call ESMF_TimeSyncToRealTime(tic, rc=localrc)
    do j=1,i*i*10000000
      real8=real8 * j + real8 * i
    enddo
    call ESMF_TimeSyncToRealTime(toc, rc=localrc)
    timeInterval = toc - tic
    call ESMF_TimeIntervalGet(timeInterval, s_r8=s_r8, rc=localrc)
    write(*,*) s_r8,':',ms_r8,':',us_r8

  enddo

  call ESMF_Finalize(rc=localrc)

end program
