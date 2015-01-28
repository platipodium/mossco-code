!> @brief Implementation of a driver that delivers flux data from rivers
!
!> @import none
!> @export all upward fluxes of variables in river data file
!
!  This computer program is part of MOSSCO.
!> @copyright Copyright (C) 2015 Helmholtz-Zentrum Geesthacht
!> @author Carsten Lemmen
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

module river_driver

  use netcdf
  use mossco_netcdf

#ifdef MOSSCO_MPI
  use mpi
#endif

  implicit none

  type, extends(type_mossco_netcdf), public :: MOSSCO_RiverNetcdf
    character(len=255), pointer, dimension(:)    :: riverNameList, quantityNameList
  end type MOSSCO_RiverNetcdf

  contains

  function read_river_file(fileName) result(nc)

    character(len=255)       :: fileName
    type(MOSSCO_RiverNetcdf) :: nc

    integer                         :: status, nvar, i, j, k
    !character(len=255), allocatable :: riverNameList(:), quantityNameList(:)
    character(len=255)              :: variableName

    status = nf90_open(trim(filename), mode=NF90_NOWRITE, ncid=nc%ncid)
    status = nf90_inq_dimid(nc%ncid,'time',nc%timeDimId)
    call nc%update_variables()

	  nvar = ubound(nc%variables,1)
	  allocate(nc%riverNameList(nvar))
	  allocate(nc%quantityNameList(nvar))

    do i=1, nvar

      nc%riverNameList(i)='none'
      nc%quantityNameList(i)='none'

      variableName = nc%variables(i)%standard_name

      if (variableName(1:10) == 'Discharge ') then
        nc%riverNameList(i)=trim(variableName(11:len_trim(variableName)))
        nc%quantityNameList(i)='water_discharge'
      else
        j = index(variableName, ' discharge')
        if (j == 0) cycle

        k = index(variableName(1:j-1),' ', back=.true.)
        if (k == 0) cycle

        nc%riverNameList(i)=variableName(k+1:j-1)
        nc%quantityNameList(i)=variableName(1:k-3)
      endif
      !!write(0,*) i,trim(nc%variables(i)%standard_name)//':'//trim(riverNameList(i))//':'//trim(quantityNameList(i)), j,k
    enddo
    call nc%close()

  end function read_river_file

  subroutine unique_rivers_and_quantities(nc, riverCount, quantityCount)

    type(MOSSCO_RiverNetcdf) :: nc
    integer                  :: riverCount, quantityCount, i, j, nvar
    character(len=255)       :: riverName, quantityName

  	riverCount=nvar
  	quantityCount=nvar

    do i=1, nvar-1
      quantityName=nc%quantityNameList(i)
      riverName=nc%riverNameList(i)
      do j=i+1, nvar
        if (trim(nc%quantityNameList(j))==trim(quantityName)) quantityCount = quantityCount - 1
        if (trim(nc%riverNameList(j))==trim(riverName)) riverCount = riverCount - 1
      enddo

      write (0,*) i,j, quantityCount, riverCount
    enddo

  end subroutine unique_rivers_and_quantities

  subroutine  lon_lat_names(nc, lon, lat, riverNameList)

    type(MOSSCO_RiverNetcdf) :: nc
    double precision, dimension(*)   :: lon, lat
    character(len=*), dimension(*)   :: riverNameList

    integer                  :: riverCount, i, j, nvar, status
    character(len=255)       :: riverName

    j=1
    do i=1, nvar
      if (trim(nc%quantityNameList(i)) /= 'water_discharge' ) cycle
      riverNameList(j) = trim(nc%riverNameList(i))
      status = nf90_get_att(nc%ncid, nc%variables(i)%varid, 'lat', lat(j))
      status = nf90_get_att(nc%ncid, nc%variables(i)%varid, 'lon', lat(j))
      j=j+1
    enddo

  end subroutine lon_lat_names

  subroutine  quantity_names_units(nc, quantityNameList, unitNameList)

    type(MOSSCO_RiverNetcdf) :: nc
    character(len=*), dimension(*)   :: unitNameList, quantityNameList

    integer                  :: quantityCount, i, j, nvar, status
    character(len=255)       :: quantityName

    j=1
    do i=1, nvar
      if (trim(nc%riverNameList(i)) /= nc%RiverNameList(1) ) cycle
      quantityNameList(j) = trim(nc%quantityNameList(i))
      status = nf90_get_att(nc%ncid, nc%variables(i)%varid, 'units', unitNameList(j))
      j=j+1
    enddo

  end subroutine  quantity_names_units



#if 0
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

    public river_init
    public river_getrecord
    public river_getdata
    public river_final

    contains

    subroutine river_init(myrank, app_time_secs, lrc)

      integer, intent(in)  :: myrank
      real(8), intent(in)  :: app_time_secs
      integer, intent(out) :: lrc

      character (len=80)   :: timestring
      integer              :: ierr, rc, ibuf(4), dimid

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
          print *, "*** ERROR: river_time begin =", dat_time(1)
          print *, "*** ERROR: river_time end   =", dat_time(nrec)
          print *, "*** ERROR: Please check atm.nc"
        endif
        lrc = -9999
      endif

    end subroutine river_init

!----------------------------------------------------------------------------------

    subroutine river_getrecord(myrank, app_time_secs, lrc)

      integer, intent(in)  :: myrank
      real(8), intent(in)  :: app_time_secs
      integer, intent(out) :: lrc

      integer i, rc

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
      if ( irec==orec+1 ) call river_readdata(myrank, 1)
      if ( irec>=orec+2 ) call river_readdata(myrank, 2)

      orec = irec

! Compute weights
      wnew = (app_time_secs - dat_time(irec)) / (dat_time(irec+1)-dat_time(irec))
      wold = 1.d0 - wnew

      if ( myrank==0 ) then
        print *,"weights=",wold,wnew
      endif

    end subroutine river_getrecord

!----------------------------------------------------------------------------------

    subroutine river_readdata(myrank, num)

      integer, intent(in) :: myrank
      integer, intent(in) :: num

      integer             :: ierr, rc

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

    end subroutine river_readdata

!----------------------------------------------------------------------------------

    subroutine river_getdata(dout,ib,ie,jb,je,flag)

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

    end subroutine river_getdata

#endif

end module river_driver
