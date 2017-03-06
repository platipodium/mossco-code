!> Test for reading a table from a .cfg file with ESMF_Config utilities
!>
!> This computer program is part of MOSSCO.
!> @copyright Copyright 2016 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME
#define ESMF_ERR_PASSTHRU msg="Test routine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "test_ConfigTable.F90"

program test_ConfigTable

  use ESMF

  implicit none

  character(len=ESMF_MAXSTR)       :: filename
  type(ESMF_Config)                :: config

  integer(ESMF_KIND_I4)                :: localrc, rc, i, j, rowCount, columnCount
  integer(ESMF_KIND_I4)                :: unit=1023
  logical                              :: isPresent, isTableEnd
  character(len=ESMF_MAXSTR)           :: message, string

  filename = 'test_ConfigTable.cfg'

  open(unit,file=trim(filename), action='write')

  write(unit,'(A)') 'my_table_name::'
  write(unit,'(A)') ' 1000     3000     263.0'
  write(unit,'(A)') '  925     3000     263.0'
  write(unit,'(A)') '  850     3000     263.0'
  write(unit,'(A)') '  700     3000     269.0'
  write(unit,'(A)') '  500     3000     287.0'
  write(unit,'(A)') '  400     3000     295.8'
  write(unit,'(A)') '  300     3000     295.8'
  write(unit,'(A)') '::'

  close(unit)

  call ESMF_Initialize(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  config = ESMF_ConfigCreate(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  ! The validate call is erroneous on an empty config, is this correct?
  !call ESMF_ConfigValidate(config, rc=localrc)
  ! if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
  !   call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  !
  call ESMF_ConfigLoadFile(config, trim(filename), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
   call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_ConfigValidate(config, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_ConfigFindLabel(config, label='my_table_name::', rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_ConfigGetDim(config, lineCount=rowCount, columnCount=columnCount, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  write(message,'(A,I1,A,I1,A)') '  reading table (',rowCount,' x ', columnCount,')'
  call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  do i = 1, rowCount
     call ESMF_ConfigNextLine(config, tableEnd=isTableEnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

     if (isTableEnd) exit

     do j = 1, columnCount
       call ESMF_ConfigGetAttribute(config, string, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
         call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
     enddo
     call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
  enddo

  call ESMF_ConfigValidate(config, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
   call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_ConfigDestroy(config, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
   call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_Finalize(endflag=ESMF_END_NORMAL)

end program test_ConfigTable
