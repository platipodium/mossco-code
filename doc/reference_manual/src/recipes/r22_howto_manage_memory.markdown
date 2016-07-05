# Recipe #22: How to manage memory

To avoid memory leakage (i.e. occupying memory from your program that cannot be
accessed later), it is important that every allocated memory is balanced by a
deallocation.

## Simple Fortran arrays

For local variables (often `ubnd` and `lbnd`), use the Fortran `allocate` and
`deallocate` in the local scope.  Best practice is to check for the allocation status

    if (allocated(myArray)) deallocate(myArray)
    allocate(myArray, mySize)
    ...
    if  (allocated(myArray)) deallocate(myArray)

Global variables should be deallocated in the `Finalize()` routine of an ESMF component.

## Fortran pointers

For local pointers, make sure that the the pointer is initialized by an association
or by a `null()` assignment.  Balance an associated pointer locally with a `nullify`
statement.

    myPtr => null()
    myPtr => myTarget
    ...
    if (associated(myPtr)) nullify(myPtr)

Global variables should be nullified in the `Finalize()` routine of an ESMF component.

## ESMF objects

All ESMF objects that are created by a component (typically in the `Initialize` phases)
should be destroyed by the same component (in the `Finalize` phase).

      call ESMF_FieldCreate()
      ...
      call ESMF_FieldDestroy()

In the call to ESMF_Destroy(), all resources attached to the object (attributes, memory) are released, but

 - ESMF_FieldDestroy() does not destroy the associated ESMF_Grid
 - ESMF_StateDestroy() does not destroy contained items

We provide utility functions defined in `mossco_state.F90`, that recursively remove all items owned by
a component and destroy them.  A component would typically

    call `MOSSCO_DestroyOwn(myState)`

to recursively destroy all objects in `myState`, followed by destruction of `myState` itself.

## Allocated pointers

Allocated pointers are often used when interacting with ESMF objects (see below).

      if (isPresent) then
        call ESMF_FieldGet(oldField, farrayPtr=myPtr)
      else
        allocate(myPtr,mySize)
      endif

and later in the code

      newField = ESMF_FieldCreate(farrayPtr=myPtr)

This creates the situation that the created field now `owns` the memory pointed to by
the pointer `myPtr`.  When this field is later destroyed with `ESMF_FieldDestroy()`, also
the memory is deallocated.

It is thus an error to deallocate the pointer `myPtr`, it must be nullified only.

    if (associated(myPtr)) nullify(myPtr)


If, however, no field `newField` is created (and thus not destroyed), the user has to
take care that the pointer is deallocated (i.e. memory release and pointer nullification)

    if (associated(myPtr)) deallocate(myPtr)
