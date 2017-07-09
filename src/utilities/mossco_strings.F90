!> @brief Implementation of string utilities
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2014, 2015, 2016, 2017 Helmholtz-Zentrum Geesthacht
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

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module mossco_strings

  use esmf
  use mossco_memory

  implicit none

  private
  public intformat, order, MOSSCO_MessageAdd, only_var_name, replace_character
  public split_string, MOSSCO_StringMatch, MOSSCO_StringClean
  public MOSSCO_CheckUnits, MOSSCO_CleanUnit

  !> @brief Returns the order of magnitude of its input argument
  !> @param <integer|real>(kind=4|8)
  !> @return integer(kind=4)
  interface order
    module procedure order_i4
    module procedure order_i8
    module procedure order_r4
    module procedure order_r8
  end interface

  !> @brief Returns a formatstring for its argument
  !> @param integer(kind=4|8)
  !> @return character(len=4)
  interface intformat
    module procedure intformat_i4
    module procedure intformat_i8
  end interface

  !> @brief Safely adds a string or list of strings to existing string, observing length
  !> @param character(len=*) | character(len=*),dimension(*)
  interface MOSSCO_MessageAdd
    module procedure MOSSCO_MessageAddString
    module procedure MOSSCO_MessageAddList
  end interface

  interface MOSSCO_StringMatch
    module procedure MOSSCO_StringMatchPattern
    module procedure MOSSCO_StringMatchPatternList
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

      only_var_name = repeat (' ',len_trim(longname)-pos)
      only_var_name = trim(longname(pos+1:))

      !> remove variable description after an equal sign
      pos = index(longname, "=")
      if (pos>1) then
        only_var_name(pos:len(only_var_name)) = ' '
        only_var_name = trim(longname)
      endif

      call replace_character(only_var_name,' ','_')

      ! ESMF does not allow the slash as a character in a field name
      call replace_character(only_var_name,'/','_')
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

     if ( i .eq. 0 ) then
       order = 1
     else
       order = int(log10(abs(real(i)))) + 1
     endif
   end function order_i8

#undef  ESMF_METHOD
#define ESMF_METHOD "order_i4"
!> @brief Returns the order of magnitude of its input argument
!> @return integer(kind=4)
!> @param integer(kind=4)
   function order_i4(i) result(order)
     integer(kind=4), intent(in)  :: i
     integer(kind=4)              :: order

     if ( i .eq. 0 ) then
       order = 1
     else
       order = int(log10(abs(real(i)))) + 1
     endif
   end function order_i4

#undef  ESMF_METHOD
#define ESMF_METHOD "order_r8"
!> @brief Returns the order of magnitude of its input argument
!> @return integer(kind=4)
!> @param integer(kind=8)
   function order_r8(r) result(order)
     real(kind=8), intent(in)  :: r
     integer(kind=4)           :: order
     if (r .eq. 0) then
       order = 1
     elseif ( abs(r) < 1 ) then
       order=-int(log10(abs(r))) + 1
     else
       order=int(log10(abs(r))) + 1
     endif
   end function order_r8

#undef  ESMF_METHOD
#define ESMF_METHOD "order_r4"
   function order_r4(r) result(order)
     real(kind=4), intent(in)  :: r
     integer(kind=4)           :: order
     if (r .eq. 0) then
       order = 1
     elseif ( abs(r) < 1 ) then
       order=-int(log10(abs(r))) + 1
     else
       order=int(log10(abs(r))) + 1
     endif
   end function order_r4

#undef  ESMF_METHOD
#define ESMF_METHOD "intformat_i8"
  function intformat_i8(i)
     character(len=4) :: intformat_i8
     integer(kind=8), intent(in) :: i
     integer             :: o

     o=order(i)
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

     o=order(i)
     if (i<0) o=o+1
     if (o<1) o=1
     if (o>9) o=9
     write(intformat_i4,'(A,I1,A,I1)') 'I', o , '.', o

  end function intformat_i4

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_MessageAddString"
!> @param character(len=*) message : string to add to [inout]
!> @param character(len=*) string: string to add [in]
!> @param integer [rc]: return code
!> @desc Adds onto a string another string, and observes the
!> maximum length of the receiving string
  subroutine MOSSCO_MessageAddString(message, string, rc)

    character(len=*), intent(inout)    :: message
    character(len=*), intent(in)       :: string
    integer(ESMF_KIND_I4), optional    :: rc

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
#define ESMF_METHOD "MOSSCO_StringMatchPattern"
  subroutine MOSSCO_StringMatchPattern(item, pattern, isMatch, rc)

    character(len=*), intent(in)        :: item
    character(len=*), intent(in)        :: pattern
    logical, intent(out)                :: isMatch
    integer(ESMF_KIND_I4), intent(out)  :: rc

    integer(ESMF_KIND_I4)               :: localrc, i, j

    rc = ESMF_SUCCESS
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

  end subroutine MOSSCO_StringMatchPattern

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_StringMatchPatternList"
  subroutine MOSSCO_StringMatchPatternList(itemName, patternList, isMatch, rc)

    character(len=*), intent(in)        :: itemName
    character(len=*), intent(in), allocatable :: patternList(:)
    logical, intent(inout)              :: isMatch
    integer(ESMF_KIND_I4), intent(out), optional  :: rc

    integer(ESMF_KIND_I4)               :: localrc, i, j, rc_

    rc_ = ESMF_SUCCESS
    if (present(rc)) rc = rc_

    ! Return if there is no pattern, isMatch is returned in the state it was
    ! received (thus only set to .false. two lines below)
    if (.not.allocated(patternlist)) return
    isMatch = .false.

    do j=lbound(patternList,1),ubound(patternList,1)
      call MOSSCO_StringMatch(itemName, patternList(j), isMatch, localrc)
      if (localrc /= ESMF_SUCCESS) then
        if (present(rc)) then
          rc = localrc
          return
        else
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
        endif
      endif
      if (isMatch) return
    enddo

  end subroutine MOSSCO_StringMatchPatternList

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
!> @param character(len=*) message : string to add to [inout]
!> @param character(len=*), dimension(:): string to add [in]
!> @param integer [rc]: return code
!> @desc Adds onto a string a list of strings, and observes the
!> maximum length of the receiving string
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
!> @param character(len=*) unit1, unit2: strings to compare [in]
!> @param logical isEqual: result of checking two unit strings for equality [out]
!> @param integer [rc]: return code
!> @desc Compares two unit strings for equality, using unit cleaning and
!> sorting of entries.
!> @todo: detect multiple occurence of the same unit
!> @todo: optionally rely on physunits package
  subroutine MOSSCO_CheckUnits(unit1, unit2, isEqual, rc)

    character(len=*), intent(in)   :: unit1, unit2
    logical, intent(out)           :: isEqual
    integer(ESMF_KIND_I4), optional, intent(out) :: rc

    integer(ESMF_KIND_I4)            :: rc_, count1, count2, i, localrc, chunk
    integer(ESMF_KIND_I4), parameter :: maxChunk = 10
    character(len=ESMF_MAXSTR)       :: unit_, message
    character(len=10), allocatable, dimension(:) :: unit1List, unit2List

    if (present(rc)) rc = ESMF_SUCCESS
    isEqual = .true.

    !> Assume that all parts of a unit1 are separated by white space and put
    !> them in an ordered list
    if (len(unit1) > len(unit_)) then
      unit_ = adjustl(trim(unit1(1:len(unit_))))
    else
      unit_ = adjustl(trim(unit1(1:len(unit1))))
    endif

    chunk = maxChunk
    call MOSSCO_Reallocate(unit1List, chunk, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    count1 = 0
    do
      i=index(unit_,' ')
      if (i<2) exit
      count1 = count1 + 1
      if (count1 > chunk) then
        chunk = chunk + chunk
        call MOSSCO_Reallocate(unit1List, chunk, keep=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
      unit1List(count1) = adjustl(trim(unit_(1:i-1)))
      unit_=adjustl(unit_(i+1:len_trim(unit_)))
    enddo
    call MOSSCO_Reallocate(unit1List, count1, keep=.true., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_UtilSort(unit1List, ESMF_SORTFLAG_ASCENDING, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !> Assume that all parts of a unit2 are separated by white space and put
    !> them in another ordered list
    if (len(unit2) > len(unit_)) then
      unit_ = adjustl(trim(unit2(1:len(unit_))))
    else
      unit_ = adjustl(trim(unit2(1:len(unit2))))
    endif

    chunk = maxChunk
    call MOSSCO_Reallocate(unit2List, chunk, keep=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    count2 = 0
    do
      i=index(unit_,' ')
      if (i<2) exit
      count2 = count2 + 1
      if (count2 > chunk) then
        chunk = chunk + chunk
        call MOSSCO_Reallocate(unit2List, chunk, keep=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      endif
      unit2List(count2) = adjustl(trim(unit_(1:i-1)))
      unit_=adjustl(unit_(i+1:len_trim(unit_)))
    enddo
    call MOSSCO_Reallocate(unit2List, count2, keep=.true., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_UtilSort(unit2List, ESMF_SORTFLAG_ASCENDING, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1, count1
      !call MOSSCO_CleanUnit(unit1List(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    do i=1, count2
      !call MOSSCO_CleanUnit(unit2List(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    if (count1>0) then
      write(unit_,'(A)') trim(unit1List(1))
      do i=2, count1
        call MOSSCO_MessageAdd(unit_,'.'//trim(unit1List(i)))
      enddo
    endif

    if (count2>0) then
      write(message,'(A)') trim(unit2List(1))
      do i=2, count2
        call MOSSCO_MessageAdd(message,'.'//trim(unit2List(i)))
      enddo
    endif

    if (trim(unit_) == trim(message)) then
      isEqual = .true.
      call ESMF_LogWrite('  equal units '//trim(message), ESMF_LOGMSG_INFO)
    else
      isEqual = .false.
      call ESMF_LogWrite('  units '//trim(message), ESMF_LOGMSG_WARNING)
      call ESMF_LogWrite('  differ from '//trim(unit_), ESMF_LOGMSG_WARNING)
    endif

    call MOSSCO_Reallocate(unit1List, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_Reallocate(unit2List, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc_)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine MOSSCO_CheckUnits

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_CleanUnit"
!> @param character(len=*) unit: string to clean [inout]
!> @param integer [rc]: return code
!> @desc cleans a unit string to follow the most simple CF conventions
!> e.g. convert 'kg*m.s^-2 s**-1 to kg m s-2 s-1'
!> @todo implement conversion of slash "/" character to negative exponent
  subroutine MOSSCO_CleanUnit(unit, rc)

    character(len=*), intent(inout)              :: unit
    integer(ESMF_KIND_I4), optional, intent(out) :: rc

    integer(ESMF_KIND_I4)            :: rc_, localrc, chunk, i
    character(len=ESMF_MAXSTR)       :: string

    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    !> Check for items that are not dealt with here
    if (index(unit,'(') > 0) return
    if (index(unit,')') > 0) return

    !> Remove instances of leading, trailing, and double whitespace
    unit=adjustl(trim(unit))
    do
      i=index(unit,'  ')
      if (i<1) exit
      write(unit,'(A)') unit(1:i)//unit(i+2:len_trim(unit))
    enddo

    !> Search for multiplication dot '.'. replace by whitespace
    do
      i=index(unit,'.')
      if (i<1) exit
      unit(i:i)=' '
    enddo

    !>  @todo  Search for division slash '/', replace this by whitespace
    !> and change the next occuring number to negative

    string=unit
    do
      i=index(string,'/')
      if (i<1) exit

      string(i:i) = ' '
      i=i+1
      ! Search for a number
      do
        if (i == len(string)) return
        if (string(i:i) > '9' .or. string(i:i) < '0' ) then
          i=i+1
        else
          exit
        endif
      enddo

      if (string(i-1:i-1) == '-') then
        write(string,'(A)') string(1:i-2)//string(i:len_trim(string))
      else
        write(string,'(A)') string(1:i-1)//'-'//string(i:len_trim(string))
      endif
    enddo

    !> Remove instances of '**'
    do
      i=index(unit,'**')
      if (i<1) exit
      write(unit,'(A)') unit(1:i-1)//unit(i+2:len_trim(unit))
    enddo

    !> Remove instances of '^'
    do
      i=index(unit,'^')
      if (i<1) exit
      write(unit,'(A)') unit(1:i-1)//unit(i+2:len_trim(unit))
    enddo

    !> Remove instances of single '*'
    do
      i=index(unit,'^')
      if (i<1) exit
      write(unit,'(A)') unit(1:i-1)//unit(i+2:len_trim(unit))
    enddo

  end subroutine MOSSCO_CleanUnit

end module mossco_strings

!
! program test_mossco_strings
!
!   use mossco_strings
!
!   character(len=10) :: s1,s2
!   logical           :: isMatch
!   integer(kind=ESMF_KIND_I4) :: rc
!
!   s1 = 'bla *bi d'
!
!   call replace_character(s1,' ','l')
!   if (trim(s1) /= 'blal*bild') then
!     write(0,*) 'Did not pass test for replace_character'
!   endif
!
!   call split_string(s1,s2,'l')
!   call split_string(s2,s1,'l')
!   if (trim(s2) /= '*bi') then
!     write(0,*) 'Did not pass test 1 for split_string'
!   endif
!   if (trim(s1) /= 'd') then
!     write(0,*) 'Did not pass test 2 for split_string'
!   endif
!
!   s1 = 'abcdef'
!   call MOSSCO_MessageAddString(s1, s1, rc)
!   if (trim(s1) /= 'abcdefab..') then
!     write(0,*) 'Did not pass test for MOSSCO_MessageAddString'
!   endif
!
!   call MOSSCO_StringMatchPattern(s1, 'cd', isMatch, rc)
!   if (.not.isMatch)  then
!     write(0,*) 'Did not pass test 1 for MOSSCO_StringMatchPattern'
!   endif
!
!   call MOSSCO_StringMatchPattern(s1, 'cdg', isMatch, rc)
!   if (isMatch)  then
!     write(0,*) 'Did not pass test 2 for MOSSCO_StringMatchPattern'
!   endif
!
!   ! call MOSSCO_StringMatchPatternList(s1, (/'cde*','ab*f'/), isMatch)
!   ! if (.not.isMatch)  then
!   !   write(0,*) 'Did not pass test 1 for MOSSCO_StringMatchPatternList'
!   ! endif
!
!   !call MOSSCO_MessageAddList(message, stringList, rc)
!   ! MOSSCO_CheckUnits(unit1, unit2, isEqual, rc)
!   ! subroutine MOSSCO_CleanUnit(unit, rc)
!
! end
