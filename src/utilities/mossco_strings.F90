!> @brief Implementation of string utilities
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2014, 2015, 2016 Helmholtz-Zentrum Geesthacht
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
  use mossco_memory

implicit none

  public order, intformat

  interface order
    module procedure order_i4
    module procedure order_i8
    module procedure order_r4
    module procedure order_r8
  end interface

  interface intformat
    module procedure intformat_i4
    module procedure intformat_i8
  end interface

  interface MOSSCO_MessageAdd
    module procedure MOSSCO_MessageAddString
    module procedure MOSSCO_MessageAddList
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
   function order_i8(i) result(order)
     integer(kind=8), intent(in)  :: i
     integer(kind=4)              :: order
     integer(kind=8)              :: s
     integer(kind=8), parameter   :: ONE=1

     if ( i .eq. 0 ) then
       order = 0
     else
       s = sign(ONE,i)
       order = int(0.5*(1-s)) + int(log10(1.0*s*i))
     endif
   end function order_i8

#undef  ESMF_METHOD
#define ESMF_METHOD "order_i4"
   function order_i4(i) result(order)
     integer(kind=4), intent(in)  :: i
     integer(kind=4)              :: order

     if ( i .eq. 0 ) then
       order = 0
     else
       order = int(0.5*(1-sign(1,i))) + int(log10(1.0*abs(i)))
     endif
   end function order_i4

#undef  ESMF_METHOD
#define ESMF_METHOD "order_r8"
   function order_r8(r) result(order)
     real(kind=8), intent(in)  :: r
     integer(kind=4)           :: order
     if (r<0) then
       order=int(log10(-r))+1
     else
       order=int(log10(r))
     endif
   end function order_r8

#undef  ESMF_METHOD
#define ESMF_METHOD "order_r4"
   function order_r4(r) result(order)
     real(kind=4), intent(in)  :: r
     integer(kind=4)           :: order
     if (r<0) then
       order=int(log10(-r))+1
     else
       order=int(log10(r))
     endif
   end function order_r4

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
#define ESMF_METHOD "MOSSCO_MessageAddString"
  subroutine MOSSCO_MessageAddString(message, string, rc)

    character(len=*), intent(inout)  :: message
    character(len=*), intent(in)           :: string
    integer(ESMF_KIND_I4), optional        :: rc

    integer(ESMF_KIND_I4)                  :: len1, len2, len0, rc_

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

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

  end subroutine MOSSCO_MessageAddString

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StringMatch"
  subroutine MOSSCO_StringMatch(item, pattern, isMatch, rc)

    character(len=*), intent(in)        :: item
    character(len=*), intent(in)        :: pattern
    logical, intent(out)                :: isMatch
    integer(ESMF_KIND_I4), intent(out)  :: rc

    integer(ESMF_KIND_I4)               :: localrc, i, j

    isMatch = .false.

    i=index(pattern,'*')
    j=index(pattern,'*',back=.true.)

    if (i>0 .and. j>i+1) then  ! found two asterisks with content in between
      if (index(item, pattern(i+1:j-1)) > 0) isMatch=.true.
      !write(0,*)  'Match 1', trim(item), trim(pattern), index(item, pattern(i+1:j-1)), ' ?> 0', isMatch
    elseif (i==1) then         ! found one asterisk at beginning, match end
      j=index(item, pattern(i+1:len_trim(pattern)))
      if (j+len_trim(pattern)-2 == len_trim(item)) isMatch=.true.
      !write(0,*)  'Match 2', trim(item), trim(pattern), j+len_trim(pattern)-2, '?=', len_trim(item) , isMatch
    elseif (i>1 .and. len_trim(item)>=i) then          ! found one asterisk at end, match beginning
      if (item(1:i-1)==pattern(1:i-1)) isMatch=.true.
      !write(0,*)  'Match 3', trim(item), trim(pattern), item(1:i-1), '?=', pattern(1:i-1) , isMatch
    else                       ! found no asterisk
      if (trim(item)==trim(pattern)) isMatch=.true.
      !write(0,*)  'Match 3', trim(item), trim(pattern), trim(item), '?=', trim(pattern) , isMatch
    endif

    return

  end subroutine MOSSCO_StringMatch

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StringClean"
  function MOSSCO_StringClean(string, exclude, kwe, char, rc) result (string_)

    character(len=*), intent(inout)          :: string
    character(len=*), intent(in), optional   :: exclude
    logical, intent(in), optional            :: kwe
    character(len=1), intent(in), optional   :: char
    integer(ESMF_KIND_I4), optional, intent(out)  :: rc

    integer(ESMF_KIND_I4)                    :: localrc, i, n, j
    character(len=ESMF_MAXSTR)               :: exclude_, string_
    character(len=1)                         :: char_

    string_ = trim(string(1:len(string_)))
    rc = ESMF_SUCCESS
    if (present(kwe)) rc = ESMF_SUCCESS
    if (present(char)) then
      char_ = char
    else
      char_ = '_'
    endif
    if (present(exclude)) then
      exclude_ = trim(exclude(1:len(exclude_)))
    else
      exclude_ = '[]()*/+^' !@todo check the disallowed characters from test_FieldName
      ! and ESMF documentation (request sent)
    endif
    if (len(exclude_) < 1) return

    do i = 1, len_trim(string_)
      do j = 1, len_trim(exclude_)
        if (string_(i:i) == exclude_(i:i)) string_(i:i) = char_
      enddo
      if (iachar(string_(i:i)) < 32) string_(i:i) = char_
      if (iachar(string_(i:i)) > 127) string_(i:i) = char_
    enddo

  end function MOSSCO_StringClean

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_MessageAddList"
  subroutine MOSSCO_MessageAddList(message, stringList, rc)

    character(len=*), intent(inout)  :: message
    character(len=*),  intent(in),  allocatable :: stringList(:)
    integer(ESMF_KIND_I4), intent(out), optional :: rc

    integer(ESMF_KIND_I4)                  :: i, rc_, localrc

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    if (.not.allocated(stringList)) return

    call MOSSCO_MessageAdd(message, stringList(lbound(stringList,1)), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i = lbound(stringList,1) + 1, ubound(stringList,1)

      call MOSSCO_MessageAdd(message, ', '//stringList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    enddo
    return

  end subroutine MOSSCO_MessageAddList

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_CheckUnits"
  subroutine MOSSCO_CheckUnits(unit1, unit2, isEqual, rc)

    character(len=*), intent(in)   :: unit1, unit2
    logical, intent(out)           :: isEqual
    integer(ESMF_KIND_I4), optional, intent(out) :: rc

    integer(ESMF_KIND_I4)            :: rc_, count, i, localrc
    integer(ESMF_KIND_I4)            :: chunk = 10
    character(len=ESMF_MAXSTR)       :: unit_
    character(len=10), allocatable, dimension(:) :: unit1List, unit2List

    if (present(rc)) rc = ESMF_SUCCESS
    isEqual = .true.

    !> Assume that all parts of a unit are separated by white space
    if (len(unit1) > len(unit_)) then
      unit_ = adjustl(trim(unit1(1:len(unit_))))
    else
      unit_ = adjustl(trim(unit1(1:len(unit1))))
    endif

    call MOSSCO_Reallocate(unit1List, chunk, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    count = 0
    do
      i=index(unit_,' ')
      if (i<2) exit
      count = count + 1
      if (count > chunk) then
        chunk = chunk + chunk
        call MOSSCO_Reallocate(unit1List, chunk, keep=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
      unit1List(count) = unit_(1:i-1)
      unit_=adjustl(unit_(i:len_trim(unit_)))
    enddo

    !call ESMF_UtilSort(unit1List, rc=localrc)
    !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(unit1List, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine MOSSCO_CheckUnits

end module mossco_strings
