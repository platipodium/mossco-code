program test_Netcdf
use mossco_netcdf
use esmf

type(type_mossco_netcdf)   :: nc
character(len=ESMF_MAXSTR) :: filename='test.nc'
character(len=ESMF_MAXSTR) :: timeUnit='seconds since 2000-01-01 00:00:00'
integer                    :: rc

nc = mossco_netcdfCreate(filename,timeUnit=timeUnit,rc=rc)
write(0,*) 'created netcdf file: ',trim(filename)
call nc%close()

nc = mossco_netcdfOpen(filename,rc=rc)
write(0,*) 'opened netcdf ',trim(filename)
write(0,*) '  ',trim(filename),' has ',size(nc%variables),'variables'
call nc%close()


end program
