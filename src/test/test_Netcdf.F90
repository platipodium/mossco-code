program test_Netcdf
use mossco_netcdf
use esmf

type(type_mossco_netcdf)   :: nc
character(len=ESMF_MAXSTR) :: filename='test.nc'
character(len=ESMF_MAXSTR) :: timeUnit='seconds since 2000-01-01 00:00:00'
integer                    :: rc
type(ESMF_Grid)            :: grid

call esmf_initialize()

grid  = ESMF_GridCreateNoPeriDim( &
         minIndex=(/1,1,1/),maxIndex=(/1,4,25/), regDecomp=(/1,1,1/), &
         name='test_grid',rc=rc)

nc = mossco_netcdfCreate(filename,timeUnit=timeUnit,rc=rc)
write(0,*) 'created netcdf file: ',trim(filename)
call nc%close()

nc = mossco_netcdfOpen(filename,rc=rc)
write(0,*) 'opened netcdf ',trim(filename)
write(0,*) '  ',trim(filename),' has ',size(nc%variables),'variables'
call nc%close()

write(0,*) 'open netcdf and add dimensions of a 1x4x25 grid'
nc = mossco_netcdfOpen(filename,rc=rc)
call nc%use_grid_dimensions(grid)
call nc%close()

call esmf_finalize()

end program
