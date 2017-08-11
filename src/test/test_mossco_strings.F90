!> @brief unit tests of string utilities
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2017, Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#define ESMF_FILENAME "test_mossco_strings.F90"
#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#define ESMF_METHOD "test_mossco_strings"
program test_mossco_strings

  use esmf
  use mossco_strings

  implicit none

  integer(ESMF_KIND_I4)               :: rc, localrc, i
  integer(ESMF_KIND_I4)               :: int4
  integer(ESMF_KIND_I4), allocatable  :: int4list(:)
  integer(ESMF_KIND_I8)               :: int8
  integer(ESMF_KIND_I4), allocatable  :: int8list(:)
  real(ESMF_KIND_R4)                  :: real4
  real(ESMF_KIND_R4), allocatable     :: real4list(:)
  real(ESMF_KIND_R8)                  :: real8
  real(ESMF_KIND_R8 ), allocatable    :: real8list(:)
  logical                             :: boolean, isMatch
  logical, allocatable                :: booleanlist(:)


  character(len=ESMF_MAXSTR)          :: string,format,remainder
  character(len=ESMF_MAXSTR), allocatable :: stringlist(:)

  call ESMF_Initialize(rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  write(*,'(A)') 'Testing interface "order"'
  do i=1,11
    int4 = 3**i
    int8 = 9**i
    real4 = 3.0**(real(i)-6.0)
    real8 = 3.0**(dble(i)-6.0)
    !write(*,*) i,int4,order(int4),int8,order(int8)
    !write(*,*) i,real4,order(real4),real8,order(real8)
  enddo

  write(*,'(A)') 'Testing interface "intformat"'
  do i=1,11
    int4 = (2*mod(i,2)-1)*3**i
    int8 = (2*mod(i,2)-1)*9**i
    write(format,'(A)') '(I2,I11,X,A,X,'//intformat(int4)//',X,I11,X,A,X'//intformat(int8)//')'
    write(*,format) i,int4,intformat(int4),int4,int8,intformat(int8),int8
  enddo

  !--------------
  write(*,'(A)') 'Testing interface "MOSSCO_MessageAdd"'
  string='This is not such a large string'
  do i=1,5
    call MOSSCO_MessageAdd(string, trim(string), localrc)
    write(*,'(I1,X,A)') i,trim(string)
  enddo

  allocate(stringList(5))
  do i=1,5
    stringList(i) = 'This is not such a large string'
  enddo

  string='bla'
  call MOSSCO_MessageAdd(string, stringList, localrc)
  write(*,'(A)') string

  deallocate(stringList)

  !--------------
  write(*,'(A)') 'Testing procedure  "only_var_name"'
  string='bla=6 is a/blubb=7 v:%xariable'
  write(*,'(A,X,A)') trim(string),only_var_name(string)

  !--------------
  write(*,'(A)') 'Testing procedure  "replace_character"'
  string='bla=6 is a/blubb=7 v:%xariable'
  call replace_character(string,' ','_')
  write(*,'(A)') trim(string)

  !--------------
  write(*,'(A)') 'Testing procedure  "split_string"'
  do i=1,5
    call split_string(string,remainder,'_')
    write(*,'(I1,X,A,A)') i,trim(string),trim(remainder)
  enddo

  !--------------
  write(*,'(A)') 'Testing procedure  "CleanUnit"'

  allocate(stringList(10))
  stringList(1) = 'm'
  stringList(2) = 'm/s'
  stringList(3) = 'kg/mmol'
  stringList(4) = 'N / km'
  stringList(5) = 'ms**2/km**2'
  stringList(6) = 'kg.m/s2'
  stringList(7) = '4E10 m/s-1'
  stringList(8) = 'm**-2 * km**1'

  do i=1,8
    string = stringList(i)
    call MOSSCO_CleanUnit(string)
    write(*,'(I2.2,X,A15,A)') i,'"'//trim(stringList(i))//'"',' ?= "'//trim(string)//'"'
  enddo

  !--------------
  write(*,'(A)') 'Testing procedure  "MOSSCO_StringMatch"'

  call MOSSCO_StringMatch('', '', isMatch, rc=localrc)
  if (isMatch) localrc=ESMF_RC_NOT_FOUND
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  string='myVariable_that_is_zero_000'

  !> Define patterns that should match
  stringList(1) = '*'
  stringList(2) = 'myVariable*'
  stringList(3) = '*_000'
  stringList(4) = '*_that_*'
  stringList(5) = 'myVar*_is_*'
  stringList(6) = '*yVar*_is_*0*'

  do i=1,6
    call MOSSCO_StringMatch(string, stringList(i), isMatch=isMatch, rc=localrc)
    if (.not.isMatch) then
      localrc=ESMF_RC_NOT_FOUND
      write(*,'(A)') 'String "'//trim(string)//'" wrongly not matched by  "'//trim(StringList(i))//'"'
      !_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif
  enddo

  !> Define patterns that should not match
  stringList(1) = ''
  stringList(2) = 'myVariable'
  stringList(3) = '_000'
  stringList(4) = '_that_'
  stringList(5) = '*_than_'
  stringList(6) = '*x*'

  do i=1,6
    call MOSSCO_StringMatch(string, stringList(i), isMatch=isMatch, rc=localrc)
    if (isMatch) then
      localrc=ESMF_RC_NOT_FOUND
      write(*,'(A)') 'String "'//trim(string)//'" wrongly matched "'//trim(StringList(i))//'"'
      !_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif
  enddo

  write(*,'(A)') 'All tests done.'

  call ESMF_Finalize(rc=localrc)

end program
