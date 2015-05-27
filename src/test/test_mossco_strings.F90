!> @brief unit tests of string utilities
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2014, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

program test_mossco_strings

!   function only_var_name(longname)
  use mossco_strings

  character(len=255) :: string1, string2
  integer(kind=8)    :: i8, n
  integer(kind=4)    :: i4
  !real(kind=4)       :: r4
  !real(kind=8)       :: r8
  character(len=10)   :: f

  write(string1,'(A)') 'The quick brown fox jumped'
  write(string2,'(A)') 'The_quick_brown_fox_jumped'
  call replace_character(string1,' ','_')

  n=len_trim(string1)
  if (.not.string1(1:n).eq.string2(1:n)) then
    write(0,'(A)') 'Error testing replace_character'
  endif

  call split_string(string1,string2,'e')

  i8=167889716
  if (.not.order(i8).eq.8) then
    write(0,'(A)') 'Error testing order with int*8'
  endif

  i4=-167
  if (.not.order(i4).eq.3) then
    write(0,'(A,I1,A)') 'Error testing order with int*4', order(i4), '/= 3'
  endif

!   r4=-25E15
!   if (.not.order(r4).eq.16) then
!     write(0,'(A,I1,A)') 'Error testing order with real*4', order(r4), '/= 16'
!   endif
!
!   r8=1.00000000004D-10
!   if (.not.order(r4).eq.-10) then
!     write(0,'(A,I1,A)') 'Error testing order with real*8', order(r4), '/= -10'
!   endif

  write(f,'(A)') intformat(i8)

  if (.not.trim(f).eq.'I9.9') then
    write(0,'(A)') 'Error testing intformat from int*8'
    print *, intformat(i8)
  endif

  write(f,'(A)') intformat(-i4)

  if (.not.trim(f).eq.'I3.3') then
    write(0,'(A)') 'Error testing intformat from positive int*4'
    print *, intformat(i4)
  endif

  write(f,'(A)') intformat(i4)

  if (.not.trim(f).eq.'I4.4') then
    write(0,'(A)') 'Error testing intformat from negative int*4'
    print *, intformat(i4)
  endif


end program test_mossco_strings
