!> @brief Implementation of geom utilities that deal with grid/mesh/locstream equally
!
! This computer program is part of MOSSCO.
!> @copyright 2021-2022 Helmholtz-Zentrum Hereon
!> @copyright 2018-2021 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen <carsten.lemmen@hereon.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#define ESMF_FILENAME "mossco_geom.F90"

#define RANGE2D lbnd(1):ubnd(1),lbnd(2):ubnd(2)
#define RANGE2DDIM lbnd(1):ubnd(1)-1,lbnd(2):ubnd(2)-1
#define RANGE3D lbnd(1):ubnd(1),lbnd(2):ubnd(2),lbnd(3):ubnd(3)
#define RANGE3DDIM lbnd(1):ubnd(1)-1,lbnd(2):ubnd(2)-1,lbnd(3):ubnd(3)

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

module mossco_geom

  use esmf
  use esmf_pointlistmod
  use mossco_strings
  use mossco_grid
  !use mossco_xgrid
  use mossco_mesh
  use mossco_locstream

  implicit none

  public MOSSCO_GeomCreate, MOSSCO_FileFormatFromString, MOSSCO_GeomTypeFromString, MOSSCO_GeomString

  private

  interface MOSSCO_GeomString
    module procedure MOSSCO_MeshString
    module procedure MOSSCO_GridString
    module procedure MOSSCO_LocStreamString
    !module procedure MOSSCO_XGridString
  end interface

  interface MOSSCO_GeomCreate
    module procedure MOSSCO_GeomCreateFromFile
  end interface

contains

#undef  ESMF_METHOD
#define ESMF_METHOD "MOSSCO_GeomCreateFromFile"
subroutine MOSSCO_GeomCreateFromFile(fileName, fileFormat, geomType, &
  kwe, grid, mesh, locStream, maskVariable, owner, writeVtk, rc)

  character(len=*), intent(in)                :: fileName
  type(ESMF_FILEFORMAT_Flag), intent(in)      :: fileFormat
  type(ESMF_GeomType_Flag), intent(in)        :: geomType
  type(ESMF_KeyWordEnforcer), intent(in), optional :: kwe
  type(ESMF_Grid), intent(out), optional      :: grid
  type(ESMF_Mesh), intent(out), optional      :: mesh
  type(ESMF_LocStream), intent(out), optional :: locStream
  character(len=*), intent(in), optional      :: maskVariable
  logical, intent(in), optional               :: writeVtk
  character(len=*), intent(in), optional      :: owner

  integer,  intent(out), optional            :: rc

  character(len=ESMF_MAXSTR)         :: owner_, maskVariable_, baseFileName
  integer(ESMF_KIND_I4)              :: localrc, rc_, i
  logical                            :: writeVtk_, addMask
  character(len=ESMF_MAXSTR)         :: fileFormatString, geomTypeString, message
  type(ESMF_PointList)               :: pointList

  rc_ = ESMF_SUCCESS
  owner_ = '--'
  writeVtk_ = .true.
  addMask = .false.
  maskVariable_ = '(none)'

  if (present(kwe)) rc_ = ESMF_SUCCESS
  if (present(rc)) rc = ESMF_SUCCESS
  if (present(owner)) call MOSSCO_StringCopy(owner_, owner)
  if (present(maskVariable)) call MOSSCO_StringCopy(maskVariable_, maskVariable)
  if (present(writeVtk)) writeVtk_ = writeVtk

  !> Check presence of optional keyword combinations
  do while (.true.)
    if ( (geomType == ESMF_GEOMTYPE_GRID) .and. present(grid)) exit
    if ( (geomType == ESMF_GEOMTYPE_MESH) .and. present(mesh)) exit
    if ( (geomType == ESMF_GEOMTYPE_LOCSTREAM) .and. present(locstream)) exit

    write(message,'(A)') trim(owner_)//' invalid combination of geomType and optional return arguments'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
    if (present(rc)) rc = ESMF_RC_ARG_BAD
    return
  enddo

  !> Look for illegal combinations of geomType and fileFormat
  do while (.true.)
    if ( (geomType == ESMF_GEOMTYPE_GRID) .and. (fileFormat == ESMF_FILEFORMAT_GRIDSPEC)) exit
    if ( (geomType == ESMF_GEOMTYPE_GRID) .and. (fileFormat == ESMF_FILEFORMAT_SCRIP)) exit
    if ( (geomType == ESMF_GEOMTYPE_MESH) .and. (fileFormat == ESMF_FILEFORMAT_SCRIP)) exit
    if ( (geomType == ESMF_GEOMTYPE_MESH) .and. (fileFormat == ESMF_FILEFORMAT_UGRID)) exit
    if ( (geomType == ESMF_GEOMTYPE_MESH) .and. (fileFormat == ESMF_FILEFORMAT_ESMFMESH)) exit
    if ( (geomType == ESMF_GEOMTYPE_LOCSTREAM) .and. (fileFormat == ESMF_FILEFORMAT_SCRIP)) exit
    if ( (geomType == ESMF_GEOMTYPE_LOCSTREAM) .and. (fileFormat == ESMF_FILEFORMAT_UGRID)) exit
    if ( (geomType == ESMF_GEOMTYPE_LOCSTREAM) .and. (fileFormat == ESMF_FILEFORMAT_ESMFMESH)) exit

    write(message,'(A)') trim(owner_)//' invalid combination of geomType and fileFormat'
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)
    if (present(rc)) rc = ESMF_RC_ARG_BAD
    return
  enddo

  call MOSSCO_FileFormatString(fileFormat, fileFormatString, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  call MOSSCO_GeomTypeString(geomType, geomTypeString, rc=localrc)
  _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  ! Try generic interface for grid creation
  if (geomType == ESMF_GEOMTYPE_GRID) then

    ! ESMF_GridCreateFrmNCFile(filename, fileformat, regDecomp, &
    !   decompflag, isSphere, polekindflag, addCornerStagger, addUserArea, indexflag, &
    !   addMask, varname, coordNames, rc)

    addMask = .false.
    if (fileFormat == ESMF_FILEFORMAT_GRIDSPEC .and. (trim(maskVariable) /= '(none)')) addMask = .true.

    grid = ESMF_GridCreate(filename=trim(fileName), fileFormat=fileFormat, &
      isSphere=.false., addmask=addMask, varname=trim(maskVariable), &
      addUserArea=.false., indexFlag=ESMF_INDEX_DELOCAL, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  ! Create SCRIP GRID, mask is automatic from GRID_MASK in file
  elseif (fileFormat == ESMF_FILEFORMAT_SCRIP .and. geomType == ESMF_GEOMTYPE_GRID) then

    grid = ESMF_GridCreate(filename=trim(fileName), &
      fileFormat=fileFormat, isSphere=.false., rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  ! Create CF GRID, mask optional from mask variable
  elseif (fileFormat == ESMF_FILEFORMAT_GRIDSPEC .and. geomType == ESMF_GEOMTYPE_GRID) then

    if (trim(maskVariable) /= '(none)') then

      grid = ESMF_GridCreate(filename=trim(fileName), fileFormat=fileFormat, &
        isSphere=.false., addmask=.true., varname=trim(maskVariable), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A)') trim(owner_)//' created grid masked by ' //trim(maskVariable)
    else
      grid = ESMF_GridCreate(filename=trim(fileName), fileFormat=fileFormat, &
        isSphere=.false., rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message, '(A)') trim(owner_)//' created grid'
    endif

    write(message, '(A)') trim(message)//' from CF '//trim(fileName)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  ! Create UGRID mesh, masking not possible
  elseif (fileFormat == ESMF_FILEFORMAT_UGRID .and. geomType == ESMF_GEOMTYPE_MESH) then

    mesh = ESMF_MeshCreate(trim(fileName), fileFormat=fileFormat, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    write(message, '(A)') trim(owner_)//' created mesh from UGRID '//trim(fileName)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  ! Create ESMF mesh
  elseif (fileFormat == ESMF_FILEFORMAT_ESMFMESH .and. geomType == ESMF_GEOMTYPE_MESH) then

    mesh = ESMF_MeshCreate(trim(fileName), fileformat=fileFormat, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    write(message, '(A)') trim(owner_)//' created mesh from ESMF '//trim(fileName)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  ! Create ESMF mesh
  elseif (fileFormat == ESMF_FILEFORMAT_SCRIP .and. geomType == ESMF_GEOMTYPE_MESH) then

    mesh = ESMF_MeshCreate(trim(fileName), fileformat=fileFormat, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    write(message, '(A)') trim(owner_)//' created mesh from SCRIP '//trim(fileName)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  elseif (geomType == ESMF_GEOMTYPE_LOCSTREAM) then

    ! ESMF_LocStreamCreateFromFile(filename, &
    !            fileformat, varname, indexflag, centerflag, name, rc)
    ! For a grid in ESMF or UGRID format, it can use center coordinates
    ! or  corner coordinates. For SCRIP only center coordinates.

    if (fileFormat == ESMF_FILEFORMAT_SCRIP) then

      locStream = ESMF_LocStreamCreate(filename=trim(fileName), &
        fileformat=fileFormat, name=trim(fileName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    elseif (fileFormat == ESMF_FILEFORMAT_UGRID .and. trim(maskVariable) /= '(none)') then

      locStream = ESMF_LocStreamCreate(filename=trim(fileName), &
        fileformat=fileformat, varname=trim(maskVariable), &
        name=trim(fileName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    elseif (fileFormat == ESMF_FILEFORMAT_UGRID) then
      locStream = ESMF_LocStreamCreate(filename=trim(fileName), &
        fileformat=fileformat, name=trim(fileName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    elseif (fileFormat == ESMF_FILEFORMAT_ESMFMESH) then
      locStream = ESMF_LocStreamCreate(filename=trim(fileName), &
        fileformat=fileformat, name=trim(fileName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    else
      write(message, '(A,A)') trim(owner_)//' unknown file format'
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
      localrc = ESMF_RC_NOT_IMPL
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    endif

    write(message, '(A)') trim(owner_)//' created locstream from ' &
      //trim(fileFormatString)//' '//trim(fileName)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

  else
    write(message, '(A,A)') trim(owner_)//' unknown file format/type ' &
      ,trim(fileFormatString)//'/'//trim(geomTypeString)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
    localrc = ESMF_RC_NOT_IMPL
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
  endif

  if (writeVtk_) then

    i = index(fileName, '.' ,back=.true.)
    baseFileName  = fileName(1:i-1)

    if (geomType == ESMF_GEOMTYPE_GRID) then

      call ESMF_GridWriteVTK(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        filename=trim(baseFileName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    elseif (geomType == ESMF_GEOMTYPE_MESH) then

      call ESMF_MeshWrite(mesh, trim(baseFileName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    elseif (geomType == ESMF_GEOMTYPE_LOCSTREAM) then

      pointList=ESMF_PointListCreate(locStream, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_PointListWriteVTK(pointList, trim(baseFileName), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_PointListDestroy(pointList, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    endif
  endif

end subroutine MOSSCO_GeomCreateFromFile

#undef ESMF_METHOD
#define ESMF_METHOD MOSSCO_GeomTypeString
subroutine MOSSCO_GeomTypeString(geomType, string, kwe, owner, rc)

  type(ESMF_GeomType_Flag), intent(in)             :: geomType
  character(len=ESMF_MAXSTR), intent(out)          :: string
  type(ESMF_KeyWordEnforcer), intent(in), optional :: kwe
  character(len=*), intent(in), optional           :: owner
  integer(ESMF_KIND_I4), intent(out), optional     :: rc

  integer(ESMF_KIND_I4)        :: rc_, localrc
  character(len=ESMF_MAXSTR)   :: message, owner_

  rc_ = ESMF_SUCCESS
  owner_ = '--'
  if (present(kwe)) rc_ = ESMF_SUCCESS
  if (present(rc)) rc = ESMF_SUCCESS
  if (present(owner)) call MOSSCO_StringCopy(owner_, owner)

  if (geomType == ESMF_GEOMTYPE_GRID) then
    string = 'GRID'
  elseif (geomType == ESMF_GEOMTYPE_MESH) then
    string = 'MESH'
  elseif (geomType == ESMF_GEOMTYPE_XGRID) then
    string = 'XGRID'
  elseif (geomType == ESMF_GEOMTYPE_LOCSTREAM) then
    string = 'LOCSTREAM'
  else
    write(message, '(A)') trim(owner_)//' cannot determine string for '// &
      'unknown geometry type'
    !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)

    if (present(rc)) then
      rc = ESMF_RC_ARG_BAD
    else
      localrc = ESMF_RC_ARG_BAD
      !_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif
  endif

end subroutine MOSSCO_GeomTypeString

#undef ESMF_METHOD
#define ESMF_METHOD MOSSCO_GeomTypeFromString
subroutine MOSSCO_GeomTypeFromString(geomType, string, kwe, owner, rc)

  type(ESMF_GeomType_Flag), intent(out)            :: geomType
  character(len=*), intent(in)                     :: string
  type(ESMF_KeyWordEnforcer), intent(in), optional :: kwe
  character(len=*), intent(in), optional           :: owner
  integer(ESMF_KIND_I4), intent(out), optional     :: rc

  integer(ESMF_KIND_I4)        :: rc_, localrc
  character(len=ESMF_MAXSTR)   :: message, owner_
  character(len=len(string))   :: fileFormatString

  rc_ = ESMF_SUCCESS
  owner_ = '--'
  if (present(kwe)) rc_ = ESMF_SUCCESS
  if (present(rc)) rc = ESMF_SUCCESS
  if (present(owner)) call MOSSCO_StringCopy(owner_, owner)

  fileFormatString =  MOSSCO_StringUpper(adjustl(trim(string)))

  if (trim(fileFormatString) == 'GRID') then
    geomType = ESMF_GEOMTYPE_GRID
  elseif (trim(fileFormatString) == 'MESH') then
    geomType = ESMF_GEOMTYPE_MESH
  elseif (trim(fileFormatString) == 'XGRID') then
    geomType = ESMF_GEOMTYPE_XGRID
  elseif (trim(fileFormatString) == 'LOCSTREAM') then
    geomType = ESMF_GEOMTYPE_LOCSTREAM
  else
    write(message, '(A)') trim(owner_)//' cannot determine geomType for'// &
      ' string '//trim(fileFormatString)
    !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)

    if (present(rc)) then
      rc = ESMF_RC_ARG_BAD
    else
      localrc = ESMF_RC_ARG_BAD
      !_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif
  endif

end subroutine MOSSCO_GeomTypeFromString

#undef ESMF_METHOD
#define ESMF_METHOD MOSSCO_FileFormatString
subroutine MOSSCO_FileFormatString(fileFormat, string, kwe, owner, rc)

  type(ESMF_FILEFORMAT_Flag), intent(in)           :: fileFormat
  character(len=ESMF_MAXSTR), intent(out)          :: string
  type(ESMF_KeyWordEnforcer), intent(in), optional :: kwe
  character(len=*), intent(in), optional           :: owner
  integer(ESMF_KIND_I4), intent(out), optional     :: rc

  integer(ESMF_KIND_I4)        :: rc_, localrc
  character(len=ESMF_MAXSTR)   :: message, owner_

  rc_ = ESMF_SUCCESS
  owner_ = '--'
  if (present(kwe)) rc_ = ESMF_SUCCESS
  if (present(rc)) rc = ESMF_SUCCESS
  if (present(owner)) call MOSSCO_StringCopy(owner_, owner)

  if (fileFormat == ESMF_FILEFORMAT_GRIDSPEC &
    .or. fileFormat == ESMF_FILEFORMAT_GRIDSPEC) then
    string = 'GRIDSPEC'
  elseif (fileFormat == ESMF_FILEFORMAT_SCRIP) then
    string = 'SCRIP'
  elseif (fileFormat == ESMF_FILEFORMAT_UGRID) then
    string = 'UGRID'
  elseif (fileFormat == ESMF_FILEFORMAT_ESMFMESH) then
    string = 'ESMF'
  else
    write(message, '(A)') trim(owner_)//' cannot determine string for '// &
      'unknown file format type'
    !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)

    if (present(rc)) then
      rc = ESMF_RC_ARG_BAD
    else
      localrc = ESMF_RC_ARG_BAD
      !_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif
  endif

end subroutine MOSSCO_FileFormatString

#undef ESMF_METHOD
#define ESMF_METHOD MOSSCO_FileFormatFromString
recursive subroutine MOSSCO_FileFormatFromString(fileFormat, string, kwe, owner, rc)

  type(ESMF_FILEFORMAT_Flag), intent(out)          :: fileFormat
  character(len=*), intent(in)                     :: string
  type(ESMF_KeyWordEnforcer), intent(in), optional :: kwe
  character(len=*), intent(in), optional           :: owner
  integer(ESMF_KIND_I4), intent(out), optional     :: rc

  integer(ESMF_KIND_I4)        :: rc_, localrc
  character(len=ESMF_MAXSTR)   :: message, owner_
  character(len=len(string))   :: fileFormatString

  rc_ = ESMF_SUCCESS
  owner_ = '--'
  if (present(kwe)) rc_ = ESMF_SUCCESS
  if (present(rc)) rc = ESMF_SUCCESS
  if (present(owner)) call MOSSCO_StringCopy(owner_, owner)

  fileFormatString =  MOSSCO_StringUpper(adjustl(trim(string)))

  if (trim(fileFormatString) == 'GRIDSPEC' .or. trim(fileFormatString) == 'CF' &
    .or. trim(fileFormatString) == 'GRIDSPEC') then
    fileFormat = ESMF_FILEFORMAT_GRIDSPEC
  elseif (trim(fileFormatString) == 'UGRID') then
    fileFormat = ESMF_FILEFORMAT_UGRID
  elseif (trim(fileFormatString) == 'SCRIP') then
    fileFormat = ESMF_FILEFORMAT_SCRIP
  elseif (trim(fileFormatString) == 'ESMF' .or. trim(string) == 'ESMFMESH') then
    fileFormat = ESMF_FILEFORMAT_ESMFMESH
  else
    write(message, '(A,A)') trim(owner_)//' cannot determine file format for', &
      ' string '//trim(fileFormatString)
    !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR, ESMF_CONTEXT)

    if (present(rc)) then
      rc = ESMF_RC_ARG_BAD
    else
      localrc = ESMF_RC_ARG_BAD
      !!_MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc_)
    endif
  endif

end subroutine MOSSCO_FileFormatFromString

end module mossco_geom
