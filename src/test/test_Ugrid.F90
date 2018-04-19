program test_UGrid

use esmf
type(ESMF_Mesh)      :: mesh
type(ESMF_Field)     :: field
real(ESMF_KIND_R8),pointer,dimension(:) :: farrayptr
integer              :: rc
integer              :: numOwnedElements=-1,numNodes
type(ESMF_FileFormat_Flag) :: fileformat=ESMF_FILEFORMAT_UGRID

call esmf_initialize()

mesh = ESMF_MeshCreate(filename='ugrid_sediment.nc',filetypeflag=ESMF_FILEFORMAT_UGRID,rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_MeshGet(mesh,numOwnedElements=numOwnedElements,numOwnedNodes=numNodes)
write(0,*) 'number of owned elements:',numOwnedElements,', number of owned nodes:',numNodes
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

field = ESMF_FieldCreate(mesh, name='mesh_field', typekind=ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
call ESMF_FieldGet(field,farrayPtr=farrayptr,rc=rc)
farrayptr(1:numOwnedElements)=10.

call ESMF_FieldPrint(field)


call ESMF_MeshFreeMemory(mesh, rc=localrc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

call ESMF_MeshDestroy(mesh, rc=localrc)
if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


call esmf_finalize()

end program
