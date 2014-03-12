program test_Netcdf
use mossco_netcdf
use esmf

type(type_mossco_netcdf)   :: nc
character(len=ESMF_MAXSTR) :: filename='test.nc'
character(len=ESMF_MAXSTR) :: timeUnit='seconds since 2000-01-01 00:00:00'
integer                    :: rc
type(ESMF_Grid)            :: grid,grid2
type(ESMF_Field)           :: field3d,field2d
integer, dimension(:),pointer :: dimids => null()
real(ESMF_KIND_R8),dimension(:,:,:),pointer :: farray3d
real(ESMF_KIND_R8),dimension(:,:),pointer :: farray2d

call esmf_initialize()

grid  = ESMF_GridCreateNoPeriDim( &
         minIndex=(/1,1,1/),maxIndex=(/1,4,25/), regDecomp=(/1,1,1/), &
         name='test_grid',rc=rc)
allocate(farray3d(1,4,25))
field3d = ESMF_FieldCreate(grid,name='my_standard_name_3d',farrayPtr=farray3d)

grid2  = ESMF_GridCreateNoPeriDim( &
         minIndex=(/1,1/),maxIndex=(/1,1/), regDecomp=(/1,1/), &
         name='one_by_one_grid',rc=rc)
allocate(farray2d(1,1))
field2d = ESMF_FieldCreate(grid2,name='my_standard_name_2d',farrayPtr=farray2d)

nc = mossco_netcdfCreate(filename,timeUnit=timeUnit,rc=rc)
write(0,*) 'created netcdf file: ',trim(filename)
call nc%close()

nc = mossco_netcdfOpen(filename,rc=rc)
write(0,*) 'opened netcdf ',trim(filename)
write(0,*) '  ',trim(filename),' has ',size(nc%variables),'variables'
call nc%close()

write(0,*) 'open netcdf and create variable from field on 1x4x25 grid'
nc = mossco_netcdfOpen(filename,rc=rc)
call nc%create_variable(field3d)
call nc%update_variables()
write(0,*) '  ',trim(filename),' now has ',size(nc%variables),'variables'
call nc%close()

write(0,*) 'open netcdf and add dimensions of a 1x1 grid'
nc = mossco_netcdfOpen(filename,rc=rc)
dimids => nc%grid_dimensions(grid2)
write(0,*) '  -> grid has',size(dimids-1),'dimensions'
call nc%close()

call esmf_finalize()

end program
