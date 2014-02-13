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

      pos1=1
      pos =0
      !> replace white space with underscore
      do
         pos2 = INDEX(only_var_name(pos1:), " ")
         IF (pos2 == 0) THEN
            pos = pos + 1
            words(pos) = only_var_name(pos1:)
            EXIT
         END IF
         pos = pos + 1
         words(pos) = only_var_name(pos1:pos1+pos2-2)
         pos1 = pos2+pos1
      end do
 
      only_var_name=trim(words(1))
      do i = 2, pos
         only_var_name=trim(only_var_name)//"_"//trim(words(i))
      end do

   end function only_var_name

end module mossco_strings
