import numpy as N
cimport numpy as N
cimport cython

DTYPEf = N.float64
ctypedef N.float64_t DTYPEf_t

@cython.boundscheck(False) # turn of bounds-checking for entire function
@cython.wraparound(False)  # turn of bounds-checking for entire function
cpdef interp3d(N.ndarray[DTYPEf_t, ndim=3] x, N.ndarray[DTYPEf_t, ndim=3] y,
               N.ndarray[DTYPEf_t, ndim=1] x_new):
    """
    interp3d(x, y, new_x)

    Performs linear interpolation over the last dimension of a 3D array,
    according to new values from a 2D array x_new. Thus, interpolate
    y[i, j, :] for new_x[i, j].

    Parameters
    ----------
    x : 3-D ndarray (double type)
        Array containg the x (abcissa) values. Must be monotonically
        increasing.
    y : 3-D ndarray (double type)
        Array containing the y values to interpolate.
    x_new: 1-D ndarray (double type)
        Array with new abcissas to interpolate. Must be monotonically
        increasing

    Returns
    -------
    new_y : 3-D ndarray
        Interpolated values.
    """
    cdef int nx = y.shape[2]
    cdef int ny = y.shape[1]
    cdef int nz = y.shape[0]
    cdef int nz_new = len(x_new)
    cdef int i, j, k, kk
    cdef double dx, dl, dy
    cdef N.ndarray[DTYPEf_t, ndim=3] new_y = N.zeros((nz_new, ny, nx), dtype=DTYPEf)

    for j in range(ny):
        for i in range(nx):
            kk=0
            for k in range(nz_new):
                 if x_new[k]<=x[0,j,i]:
                   new_y[k,j,i] = -9999.
                   #print("%d: %0.2f below %0.2f"%(k,x_new[k],x[0,j,i]))
                 elif x_new[k]>=x[nz-1,j,i]:
                   new_y[k,j,i] = -9999.
                   #print("%d: %0.2f above %0.2f"%(k,x_new[k],x[nz-1,j,i]))
                 else:
                   while x_new[k]>x[kk,j,i]:
                     kk=kk+1
                   #print("%d: %0.2f between layer %d and %d (%0.2f and %0.2f)"%(k,x_new[k],kk-1,kk,x[kk-1,j,i],x[kk,j,i]))
                   dx = x[kk,j,i]-x[kk-1,j,i]
                   dl = x_new[k]-x[kk-1,j,i]
                   dy = y[kk,j,i]-y[kk-1,j,i]
                   new_y[k,j,i] = dl/dx*dy + y[kk-1,j,i]

    return new_y

