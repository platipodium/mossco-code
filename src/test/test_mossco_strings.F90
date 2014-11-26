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
  integer(kind=8)            :: i, n
  character(len=4)   :: f
  
  write(string1,'(A)') 'The quick brown fox jumped'
  write(string2,'(A)') 'The_quick_brown_fox_jumped'
  call replace_character(string1,' ','_')
  
  n=len_trim(string1)
  if (.not.string1(1:n).eq.string2(1:n)) then
    write(0,'(A)') 'Error testing replace_character'
  endif

  call split_string(string1,string2,'e')
  
  i=167889
  if (.not.order(i).eq.5) then
    write(0,'(A)') 'Error testing order'
  endif
    
  write(f,'(A)') intformat(i)
  
  if (.not.trim(f).eq.'I6.6') then
    write(0,'(A)') 'Error testing intformat'
    print *, intformat(i)
  endif

 
end program test_mossco_strings
