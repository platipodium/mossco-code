module mossco_time

use esmf
implicit none



contains

!> Actually, this should be an extension of ESMF_TimeSet
subroutine timeString2ESMF_Time(timestring,time)
  character(len=*), intent(in) :: timestring
  type(ESMF_Time), intent(out) :: time

  integer :: yy,mm,dd,h,m,s

  read(timestring(1:4),'(i4)') yy
  read(timestring(6:7),'(i2)') mm
  read(timestring(9:10),'(i2)') dd
  read(timestring(12:13),'(i2)') h
  read(timestring(15:16),'(i2)') m
  read(timestring(18:19),'(i2)') s

  call ESMF_TimeSet(time,yy=yy,mm=mm,dd=dd,h=h,m=m,s=s)

end subroutine timeString2ESMF_Time



end module mossco_time
