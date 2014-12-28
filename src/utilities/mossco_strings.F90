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


#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "mossco_strings.F90"

module mossco_strings

  use esmf

implicit none

  public order, intformat

  interface order
    module procedure order_i4
    module procedure order_i8
  end interface
	 
  interface intformat
    module procedure intformat_i4
    module procedure intformat_i8
  end interface

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "only_var_name"
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
#undef  ESMF_METHOD
#define ESMF_METHOD "replace_character"
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
   
#undef  ESMF_METHOD
#define ESMF_METHOD "split_string"
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
   
#undef  ESMF_METHOD
#define ESMF_METHOD "order_i8"
   integer function order_i8(i)
     integer(kind=8),intent(in) :: i
     order_i8=int(log10(i*1.0))
   end function order_i8

#undef  ESMF_METHOD
#define ESMF_METHOD "order_i4"
   integer function order_i4(i)
     integer(kind=4),intent(in) :: i
     order_i4=int(log10(i*1.0))
   end function order_i4

   
#undef  ESMF_METHOD
#define ESMF_METHOD "intformat_i8"
   function intformat_i8(i)
     character(len=4) :: intformat_i8
     integer(kind=8), intent(in) :: i
     integer             :: o
     
     o=order(i)+1
     if (o<1) o=1
     if (o>9) o=9   
     write(intformat_i8,'(A,I1,A,I1)') 'I', o , '.', o

  end function intformat_i8
     
#undef  ESMF_METHOD
#define ESMF_METHOD "intformat_i4"
   function intformat_i4(i)
     character(len=4) :: intformat_i4
     integer(kind=4), intent(in) :: i
     integer             :: o
     
     o=order(i)+1
     if (o<1) o=1
     if (o>9) o=9   
     write(intformat_i4,'(A,I1,A,I1)') 'I', o , '.', o

  end function intformat_i4
   
#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_MessageAdd"
  subroutine MOSSCO_MessageAdd(message, string)
  
    character(ESMF_MAXSTR), intent(inout)  :: message
    character(len=*), intent(in)     :: string
    
    integer(ESMF_KIND_I4)                  :: len1, len2, len0
    
    len0=len(message)
    len1=len_trim(message)
    len2=len_trim(string)
    
    
    
    if (len1 + len2 <= len0) then
      write(message, '(A)') trim(message)//trim(string)
    elseif (len1 > len0 - 2 ) then
      return
    else
      write(message, '(A)') trim(message)//string(1:len0-len1-2)//'..'
    endif
    
    return
  
  end subroutine MOSSCO_MessageAdd

end module mossco_strings
