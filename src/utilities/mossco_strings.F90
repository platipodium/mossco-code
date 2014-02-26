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

end module mossco_strings
