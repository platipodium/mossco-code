!> @brief Implementation of string utilities 
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

module mossco_strings

implicit none

contains

   function only_var_name(longname)
      character(:),allocatable     :: only_var_name
      character(len=*), intent(in) :: longname
      character(len=256)           :: words(100)
      integer                      :: pos,pos1=1,pos2,i

      !> remove model name
      pos = INDEX(longname, " ")
!      allocate(character(len=len_trim(longname)-pos)::only_var_name)
      only_var_name = repeat (' ',len_trim(longname)-pos) 
      only_var_name = trim(longname(pos+1:))

      call replace_character(only_var_name,' ','_')
   end function only_var_name

   !> replace char_old by char_new in string
   subroutine replace_character(string,char_old,char_new)
      character(len=*), intent(inout) :: string
      character(len=1), intent(in)    :: char_old,char_new
      integer                         :: pos1,pos2,length

      pos1=1
      pos2=0
      length=len_trim(string)
      do
         pos2 = INDEX(string(pos1:), char_old)
         if (pos1+pos2 > length) exit
         if (pos2 == 0) then
            exit
         else
            string((pos1+pos2-1):(pos1+pos2-1)) = char_new
            pos1 = pos2+pos1
         end if
      end do
   end subroutine replace_character
   
   subroutine split_string(string,remainder, char)
     character(len=*), intent(out)   :: remainder
     character(len=*), intent(inout) :: string
     character(len=1), intent(in)    :: char
     
     integer :: pos
     
     !!@implementation needs to be done
   
     remainder=string
     pos=index(string,char)
     if (pos>0) then 
       do while (pos==1) 
         string=string(pos:)
         pos=index(string,char)
       enddo
       if (pos==0) return
        
       remainder=string(pos+1:)
       string=string(1:pos-1)
     endif   
     return
   end subroutine split_string
   
   integer function order(i)
     integer(kind=8),intent(in) :: i
     order=int(log10(i*1.0))
   end function order
   
   function intformat(i)
     character(len=2) :: intformat
     integer(kind=8), intent(in) :: i
     integer             :: o,j
     character           :: c
     
     o=order(i)    
     write(intformat,'(A,I1)') 'I', order(i)+1

  end function intformat
     
   

end module mossco_strings
