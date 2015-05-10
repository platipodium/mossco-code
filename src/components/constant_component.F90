!> @brief Implementation of an ESMF component that delivers constant data fields
!
!> @import none
!> @export all variables that are located in a file read by this component
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014, 2015 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!> @author Richard Hofmeister <richard.hofmeister@hzg.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "constant_component.F90"

#define CONSTANT_OLD

module constant_component

  use esmf
  use mossco_variable_types
  use mossco_component
  use mossco_strings
  use mossco_state
  use mossco_field

  implicit none

  private

  type,extends(MOSSCO_VariableInfo) :: variable_item_type
    type(variable_item_type), pointer     :: next => null()
    type(ESMF_Field)                      :: field
    integer :: rank
    real(ESMF_KIND_R8) :: value
  end type

  type(variable_item_type), pointer :: cur_item,variable_items

  public SetServices

  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    integer              :: localrc

    rc=ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_READRESTART, phase=1, &
      userRoutine=ReadRestart, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
  subroutine InitializeP0(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: name
    type(ESMF_Time)             :: currTime
    integer                     :: localrc
    logical                     :: isPresent

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    character(len=ESMF_MAXSTR)     :: name, message, line

    type(ESMF_Mesh)                             :: mesh
    real(ESMF_KIND_R8), pointer :: farrayPtr1(:)
    character(len=ESMF_MAXSTR)                  :: varname, meshname
    integer, parameter                          :: fileunit=21
    logical                                     :: file_readable=.true.
    integer(ESMF_KIND_I4)                       :: start

    character(len=ESMF_MAXSTR)                  :: unitString, foreignGridFieldName
    type(ESMF_Time)                             :: currTime
    real(ESMF_KIND_R8)                          :: floatValue
    integer(ESMF_KIND_I4), dimension(2)  :: computationalUBound2, computationalLBound2
    integer                              :: petCount, localPet

    integer                     :: localrc
    character(ESMF_MAXPATHLEN)  :: configFileName, fileName
    type(ESMF_Config)           :: config
    logical                     :: fileIsPresent, labelIsPresent
    integer(ESMF_KIND_I4)       :: numNodes=0, numElements=0
    integer(ESMF_KIND_I4)       :: localDeCount, localDe

    rc=ESMF_SUCCESS
    localDeCount=0

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, petCount=petCount, localPet=localPet, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importState,'foreignGridFieldName',value=foreignGridFieldName,defaultvalue='none',rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Check whether there is a config file with the same name as this component
    !! If yes, load it.
    configfilename=trim(name)//'.cfg'
    inquire(FILE=trim(configfilename), exist=fileIsPresent)
    if (fileIsPresent) then
      config = ESMF_ConfigCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ConfigLoadFile(config, configfilename, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ConfigFindLabel(config, label='meshname:', isPresent=labelIsPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ConfigGetAttribute(config, meshName, rc = rc, default='sediment_surface_mesh')
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ConfigFindLabel(config, label='ugrid:', isPresent=labelIsPresent, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ConfigGetAttribute(config, fileName, rc = rc, default='constant_ugrid.nc')
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      inquire(file=trim(fileName), exist=fileIsPresent)
      if (fileIsPresent .and. trim(foreignGridFieldName) == 'none') then

        mesh = ESMF_MeshCreate(meshname=trim(meshName),filename=trim(fileName), &
          filetypeflag=ESMF_FILEFORMAT_UGRID, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_MeshGet(mesh, numOwnedElements=numElements, numOwnedNodes=numNodes, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,I6,A)') trim(name)//' uses unstructured grid from '//trim(fileName)//' with ',numElements,' local elements.'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      endif
    endif

    ! TODO: get foreign grid
    if (trim(foreignGridFieldName) /= 'none') then
      write(message,'(A)') trim(name)//' foreign grid not implemented'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    end if

    !> create list of export_variables, that will come from a function
    !> which reads a text file

    allocate(variable_items)
    cur_item => variable_items
    cur_item%next => variable_items

    !> open constant_component.dat
    !! @todo: read filename from configuration namelist/yaml
    open(fileunit,file=trim(name)//'.dat',iostat=rc, action='read', status='old')
    if (rc == 0) then
      write(message,'(A)') trim(name)//' reads from file '//trim(name)//'.dat'
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
    else
      open(fileunit,file='constant.dat',iostat=rc, action='read', status='old')
      if (rc == 0) then
        write(message,'(A)') trim(name)//' reads from file constant.dat'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      else
        open(fileunit,file='constant_component.dat',iostat=rc, action='read', status='old')
        if (rc == 0) then
          write(message,'(A)') trim(name)//' reads from file constant_component.dat.  This feature is deprecated.'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
        else
          write(message,'(A)') trim(name)//' could not open file for reading.'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)
          file_readable=.false.
        endif
      endif
    endif

    if (rc ==0 ) then
      do
        !> read constant_component.dat line by line, maybe add rank later
        !! format of each line is:
        !!   some_standard_name  12.345
        read(fileunit,'(A)', iostat=rc) line
        if (rc /= 0) exit
        line=adjustl(line)
        if (len_trim(line)==0) cycle
        if (line(1:1)=='#' .or. line(1:1)=='%' .or. line(1:1)=='#') cycle

        read(line,*,iostat=rc) varname
        if (rc /= 0) cycle
        line=adjustl(line(len_trim(varname)+1:))
        if (len_trim(line)>0) then
          read(line,*,iostat=rc) floatValue
          if (rc /= 0) cycle
        else
          floatValue=0.0D0
        endif
        line=adjustl(line(len_trim(varname)+1:))
        if (len_trim(line)>0) then
          read(line,*,iostat=rc) unitString
          if (rc /= 0) cycle
        else
          unitString=''
        endif

        !> add item to list of constants
        allocate(cur_item%next)
        cur_item => cur_item%next
        cur_item%standard_name=trim(varname)
        cur_item%rank = 3
        cur_item%value = floatValue
        start=1
        do while (index(cur_item%standard_name(start:),'_at_')>0)
          cur_item%rank=cur_item%rank - 1
          start = index(cur_item%standard_name(start:),'_at_')+1
        enddo
        do while (index(cur_item%standard_name(start:),'_averaged_')>0)
          cur_item%rank=cur_item%rank - 1
          start = index(cur_item%standard_name(start:),'_averaged_')+1
        enddo

        nullify(cur_item%next)
      end do
    close(fileunit)
    end if
!5   continue


    !> now go through list, create fields and add to exportState
    cur_item => variable_items%next
    if (file_readable .and. numNodes==0) then
      do
        if (cur_item%rank==3) then
#if 0
          cur_item%field = ESMF_FieldCreate(grid3, arraySpec3, &
            indexflag=ESMF_INDEX_DELOCAL, &
            staggerloc=ESMF_STAGGERLOC_CENTER, name=cur_item%standard_name, rc=localrc)
#else
          cur_item%field = ESMF_FieldEmptyCreate(name=cur_item%standard_name, rc=localrc)
#endif
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

#if 0
            call ESMF_FieldGet(cur_item%field, localDe=0, farrayPtr=farrayPtr3, &
              computationalLBound=computationalLBound3, computationalUBound=computationalUBound3, rc=localrc)
            farrayPtr3(:,:,:)=cur_item%value
#endif
        elseif (cur_item%rank==2) then
#if 0
          cur_item%field = ESMF_FieldCreate(grid2, arraySpec2, &
            indexflag=ESMF_INDEX_DELOCAL, &
            staggerloc=ESMF_STAGGERLOC_CENTER, name=cur_item%standard_name, rc=localrc)
#else
          cur_item%field = ESMF_FieldEmptyCreate(name=cur_item%standard_name, rc=localrc)
#endif
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#if 0
            call ESMF_FieldGet(cur_item%field, localDe=0, farrayPtr=farrayPtr2, &
              computationalLBound=computationalLBound2, computationalUBound=computationalUBound2, rc=localrc)
            farrayPtr2(:,:)=cur_item%value
#endif
        else
          write(0,*) cur_item%rank, trim(varname), cur_item%rank
          write(message,'(A,I1,A)') trim(name)//' not implemented reading rank(', &
            cur_item%rank,') variable '//trim(varname)
          call ESMF_LogWrite(message,ESMF_LOGMSG_INFO)
        endif
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (len_trim(unitString)>0) then
          call ESMF_AttributeSet(cur_item%field,'units',trim(unitString), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
        call ESMF_AttributeSet(cur_item%field, 'creator', trim(name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_AttributeSet(cur_item%field,'default_value',cur_item%value, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' created field '
        call mossco_fieldstring(cur_item%field, message)
        call ESMF_LogWrite(message,ESMF_LOGMSG_INFO)

        call ESMF_StateAdd(exportState,(/cur_item%field/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (associated(cur_item%next)) then
          cur_item => cur_item%next
        else
          exit
        end if
      end do
    else
      do
        cur_item%field = ESMF_FieldCreate(mesh, typekind=ESMF_TYPEKIND_R8, &
          meshloc=ESMF_MESHLOC_ELEMENT, name=trim(cur_item%standard_name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_AttributeSet(cur_item%field, 'creator', trim(name), rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        do localDe=0,localDeCount-1
          call ESMF_FieldGet(cur_item%field, localDe=0, farrayPtr=farrayPtr1, &
              computationalLBound=computationalLBound2, computationalUBound=computationalUBound2, rc=localrc)
          farrayPtr1(:)=cur_item%value
        enddo

        if (len_trim(unitString)>0) then
            call ESMF_AttributeSet(cur_item%field,'units',trim(unitString))
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif

        call ESMF_AttributeSet(cur_item%field,'default_value',cur_item%value, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_StateAddReplace(exportState,(/cur_item%field/),rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A)') trim(name)//' mesh just created field '
        call mossco_fieldstring(cur_item%field, message)
        call ESMF_LogWrite(message,ESMF_LOGMSG_INFO)

        if (associated(cur_item%next)) then
            cur_item => cur_item%next
        else
          exit
        end if
      enddo
    endif

    call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "ReadRestart"
  subroutine ReadRestart(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer(ESMF_KIND_I4) :: localrc

    rc=ESMF_SUCCESS

    !> It does not make sense to readrestart this component, thus, we
    !> 1. do not log calls to this function with CompEntry/CompExit
    !> 2. use dummy variables to avoid -Wunused

    call ESMF_GridCompValidate(gridComp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockValidate(parentClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine ReadRestart

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)     :: gridComp
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: parentClock
    integer, intent(out)    :: rc

    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime, stopTime
    type(ESMF_Clock)        :: clock

    integer                 :: localrc
    logical                 :: isPresent

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_ClockGet(clock, stopTime=stopTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (stopTime>currTime) then
      call ESMF_ClockAdvance(clock, timeStep=stopTime-currTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (isPresent) call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)   :: gridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: parentClock
    integer, intent(out)  :: rc

    integer                 :: localrc
    character(ESMF_MAXSTR)  :: name
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock
    logical                 :: isPresent

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=name, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Here comes your own finalization code
    !! 1. Destroy all fields that you created, be aware that other components
    !!    might have interfered with your fields, e.g., moved them into a fieldBundle
    !! 2. Deallocate all your model's internal allocated memory
    !! 3. Destroy your clock

    !! @todo The clockIsPresent statement does not detect if a clock has been destroyed
    !! previously, thus, we comment the clock destruction code while this has not
    !! been fixed by ESMF
    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !call ESMF_ClockDestroy(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !if (isPresent) call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !if (isPresent) call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

end module constant_component
