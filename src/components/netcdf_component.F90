!> @brief Implementation of an ESMF netcdf output component
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
module netcdf_component

  use esmf
  use mossco_variable_types
  use netcdf

  implicit none
  private

  public :: SetServices

  contains

  !> Provide an ESMF compliant SetServices routine, which defines
  !! entry points for Init/Run/Finalize
  subroutine SetServices(gridcomp, rc)
  
    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, Initialize, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    rc=ESMF_SUCCESS
    
  end subroutine SetServices

  !> Initialize the component
  !!
  subroutine Initialize(gridComp, importState, exportState, parentClock, rc)
    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR) :: timestring, message, name
    type(ESMF_Time)            :: currTime, stopTime, startTime
    type(ESMF_TimeInterval)    :: timeInterval, timeStep
    integer(ESMF_KIND_I4)      :: petCount, localPet
    integer(ESMF_KIND_I8)      :: advanceCount
    type(ESMF_Clock)           :: clock

    clock = ESMF_ClockCreate(parentClock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_GridCompSet(gridComp, clock=clock, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_GridCompGet(gridComp,petCount=petCount,localPet=localPet,name=name, &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
 
    call ESMF_ClockGet(clock,startTime=startTime, currTime=currTime, &
      stopTime=stopTime, advanceCount=advanceCount, timeStep=timeStep, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    write(message,'(A)') trim(timestring)//' '//trim(name)//' initializing ...'
     
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    message = trim(nf90_inq_libvers())
    call ESMF_LogWrite(trim(name)//' uses NetCDF '//trim(message), ESMF_LOGMSG_INFO)

    write(message,'(A)') trim(timestring)//' '//trim(name)//' initialized.'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    rc=ESMF_SUCCESS

  end subroutine Initialize

  subroutine Run(gridComp, importState, exportState, parentClock, rc)
 
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=19)       :: timestring
    type(ESMF_Time)         :: currTime, currentTime, ringTime, time
    type(ESMF_TimeInterval) :: timeInterval
    integer(ESMF_KIND_I8)   :: advanceCount,  i, j
    integer(ESMF_KIND_I4)   :: itemCount, timeSlice, localPet, fieldCount, ii
    type(ESMF_StateItem_Flag), allocatable, dimension(:) :: itemTypeList
    type(ESMF_Field)        :: field
    type(ESMF_Field), allocatable, dimension(:) :: fieldList
    type(ESMF_Array)        :: array
    type(ESMF_FieldBundle)  :: fieldBundle
    type(ESMF_ArrayBundle)  :: arrayBundle
    character(len=ESMF_MAXSTR), allocatable, dimension(:) :: itemNameList
       
    character(len=ESMF_MAXSTR) :: message, fileName, name, numString
    type(ESMF_FileStatus_Flag) :: fileStatus=ESMF_FILESTATUS_REPLACE
    type(ESMF_IOFmt_Flag)      :: ioFmt


    call ESMF_GridCompGet(gridComp, localPet=localPET, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! This output routine only works on PET0
    if (localPET>0) return

    call ESMF_ClockGet(parentClock,currTime=currTime, timestep=timeInterval, &
                       advanceCount=advanceCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(timestring)//' netcdf_component running...'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call ESMF_AttributeGet(importState, name='filename', value=fileName, &
      defaultValue='netcdf_componenent', rc=rc)

    call ESMF_StateGet(importState, itemCount=itemCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !write(numstring,'(I3)') itemCount
    !write(message,'(A)') 'Found '//trim(numstring)//' items in '//trim(name)
    !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    if (advanceCount<huge(timeSlice)) then
      timeSlice=int(advanceCount, ESMF_KIND_I4)
    else
      write(message,'(A)') 'Cannot use this advanceCount for a netcdf timeSlice, failed to convert long int to int'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
    endif

    if (itemcount>0) then
      if (.not.allocated(itemTypeList)) allocate(itemTypeList(itemCount))
      if (.not.allocated(itemNameList)) allocate(itemNameList(itemCount))
      
      call ESMF_StateGet(importState, itemTypeList=itemTypeList, itemNameList=itemNameList, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
      do i=1,itemCount
        
        if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(importState, trim(itemNameList(i)), field, rc=rc) 
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

          call MOSSCO_FieldWriteNc(field, filename, parentClock, rc)
!        elseif (itemTypeList(i) == ESMF_STATEITEM_ARRAY) then
!          call ESMF_StateGet(importState, trim(itemNameList(i)), array, rc=rc) 
!          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!          call ESMF_ArrayWrite(array, fileName, overwrite=.true., status=fileStatus, &
!             ioFmt=ESMF_IOFMT_NETCDF, timeSlice=timeSlice, rc=rc)
! 
        elseif (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
          call ESMF_StateGet(importState, trim(itemNameList(i)), fieldBundle, rc=rc) 
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
           
           !> allocate fieldlist by fieldCount
           if (allocated(fieldList)) deallocate(fieldList)
           call ESMF_FieldBundleGet(fieldBundle,fieldCount=fieldCount,rc=rc)
           allocate(fieldList(fieldCount))
           
           call ESMF_FieldBundleGet(fieldBundle,fieldList=fieldList,rc=rc)
           do ii=1,size(fieldList)
              call MOSSCO_FieldWriteNc(fieldList(ii), filename, parentClock, rc)
           end do
!       elseif (itemTypeList(i) == ESMF_STATEITEM_ARRAYBUNDLE) then
!         call ESMF_StateGet(importState, trim(itemNameList(i)), arrayBundle, rc=rc) 
!         if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!          call ESMF_ArrayBundleWrite(arrayBundle, fileName, singleFile=.true., &
!            overwrite=.true., status=fileStatus, &
!             ioFmt=ESMF_IOFMT_NETCDF, timeSlice=timeSlice, rc=rc)
        else 
          write(message,'(A)') 'Item with name '!//trim(itemNameList(i)!//' not saved to file ' 
        endif
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      enddo

      if (allocated(itemTypeList)) deallocate(itemTypeList)
      if (allocated(itemNameList)) deallocate(itemNameList)
    endif 


    write(message,'(A)') trim(timestring)//' netcdf_component finished running.'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
 
  end subroutine Run

  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    type(ESMF_Time)      :: currTime
    type(ESMF_TimeInterval) :: timeInterval
    integer(ESMF_KIND_I8)   :: advanceCount
    character(len=ESMF_MAXSTR) :: message, timeString


    call ESMF_ClockGet(parentClock,currTime=currTime, timestep=timeInterval, &
                       advanceCount=advanceCount, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call ESMF_TimeGet(currTime,timeStringISOFrac=timestring)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    write(message,'(A)') trim(timestring)//' netcdf_component finalized'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  end subroutine Finalize

  subroutine MOSSCO_FieldWriteNc(field, filename, clock, rc)

    type(ESMF_Field), intent(in) :: field
    character(len=*), intent(in) :: filename  
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out)         :: rc    

    type(ESMF_Grid)              :: grid
    type(ESMF_ArraySpec)         :: arraySpec
    type(ESMF_TypeKind_Flag)     :: coordTypeKind
    integer(ESMF_KIND_I4)        :: dimCount, rank
    integer(ESMF_KIND_I8)        :: totalCount, i, seconds
    type(ESMF_Index_Flag)        :: indexFlag
    real(ESMF_KIND_R8), pointer  :: farrayPtr1(:), farrayPtr2(:,:), farrayPtr3(:,:,:)
    type(ESMF_Time)              :: refTime, currTime
    integer(ESMF_KIND_I4)        :: ubnd(3), lbnd(3)
    
   
    integer                      :: ncStatus, ncid, varid, dimid, udimid
    integer                      :: nDims, nVars, nAtts
    integer, dimension(4)        :: dimlens,dimids
    real(ESMF_KIND_R8)           :: time

    character(len=ESMF_MAXSTR)   :: message, timeString, name
    real(ESMF_KIND_R8),allocatable :: value1(:)

    !! Determine current and reference times, express the time as 
    !! seconds since refTime
    call ESMF_ClockGet(clock, currTime=currTime, refTime=refTime, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(refTime, timeStringISOFrac=timeString, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalGet(currTime-refTime, s_i8=seconds, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    !! Look at the field and get its rank, grid, and arrayspec
    call ESMF_FieldGet(field, grid=grid, arraySpec=arraySpec, rank=rank, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridGet(grid, coordTypeKind=coordTypeKind, dimCount=dimCount, &
       indexFlag=indexFlag, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ncStatus = nf90_open(path=trim(fileName)//'.nc', mode=NF90_WRITE, ncid=ncid)
    if (ncStatus /= NF90_NOERR) then
      
      ncStatus=nf90_create(trim(fileName)//'.nc', NF90_CLOBBER, ncid)
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

      ncStatus = nf90_def_dim(ncid, 'time', NF90_UNLIMITED, udimid)
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

      ncStatus = nf90_def_var(ncid, 'time', NF90_INT, udimid, varid)
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

      ncStatus = nf90_put_att(ncid, varid, 'unit', 'seconds since '//timeString)
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
      
      call ESMF_FieldGet(field, rank=rank, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
!      call ESMF_GridGetCoord(grid,coordDim=1,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
!        computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=farrayPtr1, rc=rc)
!      if (rc /= ESMF_SUCCESS) then
        !! no coordinates defined for this grid, we take x, y, z then

        call ESMF_FieldGetBounds(field, localDE=0, computationalUBound=ubnd, computationalLBound=lbnd, &
          rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        if (rank /= 3) then
          write(message,'(A)') 'Not implemented: writing fields with rank /= 3'
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        endif

        if (rank>0) then
          ncStatus = nf90_def_dim(ncid, 'x', ubnd(1)-lbnd(1)+1, dimid)
          ncStatus = nf90_def_var(ncid, 'x', NF90_INT, dimid, varid)
          ncStatus = nf90_put_att(ncid, varid, 'units', 'unitless')
          ncStatus = nf90_put_att(ncid, varid, 'long_name', 'index_of_x_direction')
        endif

        if (rank>1) then        
          ncStatus = nf90_def_dim(ncid, 'y', ubnd(2)-lbnd(2)+1, dimid)
          ncStatus = nf90_def_var(ncid, 'y', NF90_INT, dimid, varid)
          ncStatus = nf90_put_att(ncid, varid, 'units', 'unitless')
          ncStatus = nf90_put_att(ncid, varid, 'long_name', 'index_of_y_direction')
        endif 
        
        if (rank>2) then
          ncStatus = nf90_def_dim(ncid, 'z', ubnd(3)-lbnd(3)+1, dimid)
          ncStatus = nf90_def_var(ncid, 'z', NF90_INT, dimid, varid)
          ncStatus = nf90_put_att(ncid, varid, 'units', 'unitless')
          ncStatus = nf90_put_att(ncid, varid, 'long_name', 'index_of_z_direction')
        endif


 !     else
!        ncStatus = nf90_def_dim(ncid, 'lon', ubnd(1)-lbnd(1)+1, dimid)
!        if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
!
!        call ESMF_GridGetCoord(grid,coordDim=2,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
!          computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=farrayPtr1, rc=rc)
!        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!        ncStatus = nf90_def_dim(ncid, 'lat', ubnd(1)-lbnd(1)+1, dimid)
!        if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
!
!        call ESMF_GridGetCoord(grid,coordDim=3,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
!          computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=farrayPtr1, rc=rc)
!        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!        ncStatus = nf90_def_dim(ncid, 'z', ubnd(1)-lbnd(1)+1, dimid)
!        if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
!      endif

      ncStatus = nf90_enddef(ncid)
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

      ncStatus = nf90_inquire(ncid, nDims, nVars, nAtts, udimid)
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
    
!      write(*,*) ncid, nDims, nVars, nAtts, udimid

      !call ESMF_GridGetCoord(grid,coordDim=1,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
      !  computationalLBound=lbnd, computationalUBound=ubnd, farrayPtr=farrayPtr1, rc=rc)
      !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      allocate(value1(ubnd(1)-lbnd(1)+1))
      do i=1,ubound(value1,1)
        value1(i) = i
      end do

      ncStatus = nf90_inq_varid(ncid,'x',varid)
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

      !ncStatus = nf90_put_var(ncid, varid, farrayPtr1(lbnd(1):ubnd(1)))
      ncStatus = nf90_put_var(ncid, varid, value1)
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

      write(message,'(A)') 'New file '//trim(fileName)//'.nc created'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    endif

    !! Get general info about file
    ncStatus = nf90_inquire(ncid, nDims, nVars, nAtts, udimid)
    if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

    do i=1,nDims
      dimids(i)=i
      ncStatus = nf90_inquire_dimension(ncid, dimids(i), len=dimlens(i) )
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
    enddo

    ncStatus = nf90_inq_varid(ncid, 'time', varid)
    if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR) 
 
    if (dimlens(udimid)==0) then
      ncStatus = nf90_put_var(ncid, varid, (/seconds/), start=(/1/), count=(/1/))
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
      !write(*,*) ncid, varid, seconds
    else
      ncStatus = nf90_get_var(ncid, varid, time, start=(/dimlens(udimid)/))
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
   
      if (time<seconds) then
        !! append data
        ncStatus = nf90_put_var(ncid, varid, seconds, start=(/dimlens(udimid)+1/))!, count=(/1/))
        if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
      elseif (time>seconds) then
        write(message,'(A)') 'Not implemented: inserting time'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      endif
      if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
    endif

    ncStatus = nf90_inquire_dimension(ncid, dimids(udimid), len=dimlens(udimid) )
    if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
    
    call ESMF_FieldGet(field, rank=rank, name=name, rc=rc)
    if (rank == 3) then
      call ESMF_FieldGet(field, farrayPtr=farrayPtr3, rc=rc)
    else
      write(message,'(A)') 'Not implemented: writing of fields with rank other than 3'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
    endif

    ncStatus = nf90_inq_varid(ncid, trim(name), varid)
    if (ncStatus /= NF90_NOERR) then
       !! Variable not found, so create it
    
        ncStatus = nf90_redef(ncid)
        if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

        ncStatus = nf90_def_var(ncid, trim(name), NF90_DOUBLE, dimids((/4,3,2,1/)), varid)
        if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
        
        !! @todo ask the field for all its attributes and add them here   
        ncStatus = nf90_enddef(ncid)
        if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
            
    endif
    
    ncStatus = nf90_inquire_variable(ncid, varid, ndims=nDims, natts=nAtts, dimids=dimids)
    if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
   
    !write(*,*) trim(name), rank, ubound(farrayPtr3)

    ncStatus = nf90_put_var(ncid, varid, farrayPtr3, start=(/1,1,1,dimlens(1)/))
    if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)

    ncStatus = nf90_close(ncid)
    if (ncStatus /= NF90_NOERR) call ESMF_LogWrite(nf90_strerror(ncStatus),ESMF_LOGMSG_ERROR)
 
    rc=ESMF_SUCCESS

  end subroutine MOSSCO_FieldWriteNc


!  subroutine MOSSCO_Reallocate(variable,dimensions, keepLarge)
!    integer, intent(inout), allocatable :: variable(:)
!    integer :: dimensions
!    logical, optional :: keepLarge
!
!    if (.not.present(keepLarge)) keepLarge=.false.
!
!    if (.not.allocated(variable)) then
!      allocate(variable(dimensions))
!    else
!      if (.not.((size(variable)>=dimensions).and.keepLarge)) then
!        deallocate(variable)
!        allocate(variable(dimensions))
!      endif
!    endif
!
!   end subroutine MOSSCO_Reallocate 


end module netcdf_component
