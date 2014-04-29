!> @file test_Netcdf.F90
!! @brief test mossco_netcdf module
!! @author Richard Hofmeister
!!
!! creates the netcdf file test.nc for testing writing files,
!! coordinates and variables. The file test.nc is created and opened
!! three times in order to add items to the netcdf file.

program test_Netcdf
use mossco_netcdf
use esmf

type(type_mossco_netcdf)   :: nc
character(len=ESMF_MAXSTR) :: filename='test.nc'
character(len=ESMF_MAXSTR) :: timeUnit='seconds since 2000-01-01 00:00:00'
integer                    :: rc
type(ESMF_Grid)            :: grid,grid2
type(ESMF_Field)           :: field3d,field2d,field3d_c
integer, dimension(:),pointer :: dimids => null()
real(ESMF_KIND_R8),dimension(:,:,:),pointer :: farray3d
real(ESMF_KIND_R8),dimension(:,:),pointer :: farray2d

call esmf_initialize()

grid  = ESMF_GridCreateNoPeriDim( &
         minIndex=(/1,1,1/),maxIndex=(/1,4,25/), regDecomp=(/1,1,1/), &
         name='test_grid',rc=rc)
! dont add coordinates here:
!call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)

allocate(farray3d(1,4,25))
farray3d(:,:,:)=123.4
field3d = ESMF_FieldCreate(grid,name='my_standard_name_3d',farrayPtr=farray3d)
field3d_c = ESMF_FieldCreate(grid,name='my_standard_name_3d_copy',farrayPtr=farray3d)

grid2  = ESMF_GridCreateNoPeriDim( &
         minIndex=(/1,1/),maxIndex=(/1,1/), regDecomp=(/1,1/), &
         name='one_by_one_grid',rc=rc)
call ESMF_GridAddCoord(grid2, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)

allocate(farray2d(1,1))
! write coordinate values:
call ESMF_GridGetCoord(grid2, 1, farrayPtr=farray2d, rc=rc)
farray2d(1,1) = 15.15d0
call ESMF_GridGetCoord(grid2, 2, farrayPtr=farray2d, rc=rc)
farray2d(1,1) = 53.53d0

! create fields to write into netcdf
field2d = ESMF_FieldCreate(grid2,name='my_standard_name_2d',farrayPtr=farray2d)

nc = mossco_netcdfCreate(filename,timeUnit=timeUnit,rc=rc)
write(0,*) 'created netcdf file: ',trim(filename)
call nc%close()

nc = mossco_netcdfOpen(filename,rc=rc)
write(0,*) 'opened netcdf ',trim(filename)
write(0,*) '  ',trim(filename),' has ',size(nc%variables),'variables'
call nc%close()

write(0,*) 'open netcdf and create variable from field3d,field3d_c on 1x4x25 grid'
nc = mossco_netcdfOpen(filename,rc=rc)
call nc%create_variable(field3d)
call nc%add_timestep(3600.d0)
call nc%put_variable(field3d_c)

call nc%update_variables()
write(0,*) '  ',trim(filename),' now has ',size(nc%variables),'variables'
call nc%close()

write(0,*) 'open netcdf and add dimensions of a 1x1 grid'
nc = mossco_netcdfOpen(filename,rc=rc)
dimids => nc%grid_dimensions(grid2)
write(0,*) '  -> grid has',size(dimids-1),'dimensions'
write(0,*) 'put second timestep for field3d_c'
call nc%add_timestep(7200.d0)
farray3d(:,:,:)=721.0
call nc%put_variable(field3d_c)
call nc%close()

call esmf_finalize()

end program
