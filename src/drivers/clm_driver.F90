!> @brief Implementation of CLM driver
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2013, 2014, 2015, 2016 Helmholtz-Zentrum Geesthacht
!> @author Hartmut Kapitza <hartmut.kapitza@hzg.de>
!> @author Carsten Lemmen <carsten.lemmen@hzg.de>
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!
!! This module contains routines specific to the CLM output format.
!> and encapsulates the CLM atmospheric interface.

module clm_driver

  use netcdf

#ifdef MOSSCO_MPI
  use mpi
#endif

  implicit none

  private

    integer, parameter :: nvars=7        !< number of atm. variables
    integer            :: iunit          !< file identifier
    integer            :: nx             !< number of points in x-dir
    integer            :: ny             !< number of points in y-dir
    integer            :: varid(0:nvars) !< netcdf variable identifiers
    real(4), pointer   :: work(:,:,:,:)  !< work array for reading data
    real(8), pointer   :: dat_time(:)    !< available times
    integer            :: nrec           !< number of time records in data
    integer            :: irec           !< current number of 1st record
    integer            :: orec           !< previous number of 1st record
    real(8)            :: wnew, wold     !< weights for time interpolation

    public CLM_init
    public CLM_getrecord
    public CLM_getdata
    public CLM_final

    contains

    subroutine CLM_init(myrank, app_time_secs, lrc)

      integer, intent(in)  :: myrank
      real(8), intent(in)  :: app_time_secs
      integer, intent(out) :: lrc

      integer              :: ierr, ibuf(4), dimid

! Set return code to no-error
      lrc  = 0
      ibuf = 0

! Open file, read dimensions, get variable ids, broadcast dimensions
      if ( myrank==0 ) then
        print *, "atmos.nc"
        ierr = nf90_open("atmos.nc", NF90_NOWRITE,iunit)
        if ( ierr /= NF90_NOERR ) call cdf_check_err(ierr, ibuf(4))

        if ( ierr == NF90_NOERR ) then  ! Continue reading only if open succeeded
          ierr = nf90_inq_dimid(iunit,'rlon',dimid)
          ierr = nf90_inquire_dimension(iunit,dimid,len=ibuf(1))
          ierr = nf90_inq_dimid(iunit,'rlat',dimid)
          ierr = nf90_inquire_dimension(iunit,dimid,len=ibuf(2))
          ierr = nf90_inq_dimid(iunit,'time',dimid)
          ierr = nf90_inquire_dimension(iunit,dimid,len=ibuf(3))
          ierr = nf90_inq_varid(iunit,'time',     varid(0))
          ierr = nf90_inq_varid(iunit,'PMSL',     varid(1))
          ierr = nf90_inq_varid(iunit,'U_10M',    varid(2))
          ierr = nf90_inq_varid(iunit,'V_10M',    varid(3))
          ierr = nf90_inq_varid(iunit,'T_2M',     varid(4))
          ierr = nf90_inq_varid(iunit,'RELHUM_2M',varid(5))
          ierr = nf90_inq_varid(iunit,'CLCT',     varid(6))
          ierr = nf90_inq_varid(iunit,'TOT_PREC', varid(7))
        endif

      endif

#ifdef MOSSCO_MPI
      call MPI_BCAST( ibuf, 4, MPI_INTEGER,   0, MPI_COMM_WORLD, ierr )
#endif

      nx   = ibuf(1)
      ny   = ibuf(2)
      nrec = ibuf(3)
      lrc  = ibuf(4)

      if ( lrc /= 0 ) return  ! Abort if open failed

! Initialize current data record ID and previous ID
      irec =  1
      orec = -1

      allocate (work(nx,ny,nvars,0:1))
      allocate (dat_time(nrec))

! Read available times and broadcast
      if ( myrank==0 ) ierr = nf90_get_var(iunit,varid(0),dat_time)
#ifdef MOSSCO_MPI
      call MPI_BCAST( dat_time, nrec   , MPI_DOUBLE_PRECISION,   0, MPI_COMM_WORLD, ierr )
#endif

! Check for consistency
      if ( app_time_secs < dat_time(1) .or. app_time_secs >= dat_time(nrec) ) then
        if ( myrank==0 ) then
          print *, "*** ERROR: Application time outside dataset range!"
          print *, "*** ERROR: app_time       =", app_time_secs
          print *, "*** ERROR: clm_time begin =", dat_time(1)
          print *, "*** ERROR: clm_time end   =", dat_time(nrec)
          print *, "*** ERROR: Please check atm.nc"
        endif
        lrc = -9999
      endif

    end subroutine CLM_init

!----------------------------------------------------------------------------------

    subroutine CLM_getrecord(myrank, app_time_secs, lrc)

      integer, intent(in)  :: myrank
      real(8), intent(in)  :: app_time_secs
      integer, intent(out) :: lrc

      integer i

! Set return code to no-error
      lrc = 0

! Check for EOF condition
      if ( app_time_secs > dat_time(nrec) ) then
        if ( myrank==0 ) then
          print *, "*** ERROR: EOF reached on atm.nc"
        endif
        lrc = -9999
        return
      endif

! Find record ID of data window
      do i=irec+1,nrec
        if ( dat_time(i) > app_time_secs ) then
          irec = i - 1
          exit
        endif
      enddo

! Get data
      if ( irec==orec+1 ) call CLM_readdata(myrank, 1)
      if ( irec>=orec+2 ) call CLM_readdata(myrank, 2)

      orec = irec

! Compute weights
      wnew = (app_time_secs - dat_time(irec)) / (dat_time(irec+1)-dat_time(irec))
      wold = 1.d0 - wnew

      if ( myrank==0 ) then
        print *,"weights=",wold,wnew
      endif

    end subroutine CLM_getrecord

!----------------------------------------------------------------------------------

    subroutine CLM_readdata(myrank, num)

      integer, intent(in) :: myrank
      integer, intent(in) :: num

      integer             :: ierr

! Get data records and broadcast
! First case: 2 records
      if ( num==2 ) then

       if ( myrank==0 ) then

        print *, "reading 2 records irec = ", irec, irec+1

        ierr = nf90_get_var(iunit,varid(1),work(:,:,1,:),start=(/ 1, 1, irec /) &
                                                        ,count=(/nx,ny, 2    /))
        ierr = nf90_get_var(iunit,varid(2),work(:,:,2,:),start=(/ 1, 1, 1, irec /) &
                                                        ,count=(/nx,ny, 1, 2    /))
        ierr = nf90_get_var(iunit,varid(3),work(:,:,3,:),start=(/ 1, 1, 1, irec /) &
                                                        ,count=(/nx,ny, 1, 2    /))
        ierr = nf90_get_var(iunit,varid(4),work(:,:,4,:),start=(/ 1, 1, 1, irec /) &
                                                        ,count=(/nx,ny, 1, 2    /))
        ierr = nf90_get_var(iunit,varid(5),work(:,:,5,:),start=(/ 1, 1, 1, irec /) &
                                                        ,count=(/nx,ny, 1, 2    /))
        ierr = nf90_get_var(iunit,varid(6),work(:,:,6,:),start=(/ 1, 1, irec /) &
                                                        ,count=(/nx,ny, 2    /))
        ierr = nf90_get_var(iunit,varid(7),work(:,:,7,:),start=(/ 1, 1, irec /) &
                                                        ,count=(/nx,ny, 2    /))
       endif

#ifdef MOSSCO_MPI
       call MPI_BCAST(work,nx*ny*nvars*2,MPI_REAL,0,MPI_COMM_WORLD,ierr )
#endif

       work(:,:,1,:) = work(:,:,1,:) * 0.01 ! Pa ---> hPa

      endif

! Second case: 1 record only
      if ( num==1 ) then

       work(:,:,:,0) = work(:,:,:,1)

       if ( myrank==0 ) then

         print *, "reading 1 record  irec = ", irec+1

         ierr = nf90_get_var(iunit,varid(1),work(:,:,1,1),start=(/1,  1,irec+1 /) &
                                                         ,count=(/nx,ny,1 /))
         ierr = nf90_get_var(iunit,varid(2),work(:,:,2,1),start=(/1,1,1,irec+1 /) &
                                                         ,count=(/nx,ny,1,1 /))
         ierr = nf90_get_var(iunit,varid(3),work(:,:,3,1),start=(/1,1,1,irec+1 /) &
                                                         ,count=(/nx,ny,1,1 /))
         ierr = nf90_get_var(iunit,varid(4),work(:,:,4,1),start=(/1,1,1,irec+1 /) &
                                                         ,count=(/nx,ny,1,1 /))
         ierr = nf90_get_var(iunit,varid(5),work(:,:,5,1),start=(/1,1,1,irec+1 /) &
                                                         ,count=(/nx,ny,1,1 /))
         ierr = nf90_get_var(iunit,varid(6),work(:,:,6,1),start=(/1,  1,irec+1 /) &
                                                         ,count=(/nx,ny,1 /))
         ierr = nf90_get_var(iunit,varid(7),work(:,:,7,1),start=(/1,  1,irec+1 /) &
                                                         ,count=(/nx,ny,1 /))
       endif

#ifdef MOSSCO_MPI
       call MPI_BCAST(work(:,:,:,1),nx*ny*nvars,MPI_REAL,0,MPI_COMM_WORLD,ierr)
#endif
       work(:,:,1,1) = work(:,:,1,1) * 0.01 ! Pa ---> hPa

      endif

    end subroutine CLM_readdata

!----------------------------------------------------------------------------------

    subroutine CLM_getdata(dout,ib,ie,jb,je,flag)

      integer,           intent(in)  :: ib, ie, jb, je
      real(8),           intent(out) :: dout(ib:ie,jb:je)
      character (len=1), intent(in)  :: flag

      select case ( flag )

      case ('P')
        dout =  wold * work(ib:ie,jb:je,1,0) + wnew * work(ib:ie,jb:je,1,1)
      case ('U')
        dout =  wold * work(ib:ie,jb:je,2,0) + wnew * work(ib:ie,jb:je,2,1)
      case ('V')
        dout =  wold * work(ib:ie,jb:je,3,0) + wnew * work(ib:ie,jb:je,3,1)
      case ('T')
        dout =  wold * work(ib:ie,jb:je,4,0) + wnew * work(ib:ie,jb:je,4,1)
      case ('Q')
        dout =  wold * work(ib:ie,jb:je,5,0) + wnew * work(ib:ie,jb:je,5,1)
      case ('C')
        dout =  wold * work(ib:ie,jb:je,6,0) + wnew * work(ib:ie,jb:je,6,1)
      case ('R')
        dout =  wold * work(ib:ie,jb:je,7,0) + wnew * work(ib:ie,jb:je,7,1)
      case default
        print * , "no such variable ",flag

      end select

    end subroutine CLM_getdata

!----------------------------------------------------------------------------------

    subroutine CLM_final(myrank)

      integer, intent(in) :: myrank

      integer ierr

      if ( myrank==0 ) then
        print *, "Closing atm.nc"
        ierr = nf90_close(iunit)
      endif

      deallocate (work)

    end subroutine CLM_final

!----------------------------------------------------------------------------------

!> Check error code
!! @par Detailed Description:
!! This subroutine checks the error code of a netCDF call.
!! In case of error it returns the error message and stops the run.
!----------------------------------------------------------------------------------
   subroutine cdf_check_err(ierr, lrc)

      integer, intent(in)  :: ierr !< netCDF return code
      integer, intent(out) :: lrc  !< local return code

      print *, nf90_strerror(ierr)
      lrc = -999

   end subroutine cdf_check_err

end module clm_driver
