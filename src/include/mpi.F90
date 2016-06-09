!> Workaround (proposed by SGI support) for the mpt MPI implementation, where
!> the fortran module file mpi.f90 is missing
!> see bug https://sourceforge.net/p/mossco/tickets/406/

mpi.f90
module mpi
include 'mpif.h'
end module mpi
