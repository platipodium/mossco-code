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

#define ESMF_CONTEXT  line=__LINE__
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "test_mossco_strings.F90"
#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

program test_mossco_strings

  use esmf
  use mossco_strings

  integer(ESMF_KIND_I4)               :: rc
  integer(ESMF_KIND_I4)               :: int4
  integer(ESMF_KIND_I4), allocatable  :: int4list(:)
  integer(ESMF_KIND_I8)               :: int8
  integer(ESMF_KIND_I4), allocatable  :: int8list(:)
  real(ESMF_KIND_R4)                  :: real4
  real(ESMF_KIND_R4), allocatable     :: real4list(:)
  real(ESMF_KIND_R8)                  :: real8
  real(ESMF_KIND_R8 ), allocatable    :: real8list(:)
  logical                             :: boolean
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

  call ESMF_Finalize(rc=localrc)

end program
