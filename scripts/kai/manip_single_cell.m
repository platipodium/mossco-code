ncfile='~/tmp.nc';
outfile=ncfile;

ncid  = netcdf.open(ncfile,'NC_WRITE');

ix =22; iy= 15; 

[ndim nvar natt udimid] = netcdf.inq(ncid); 
for id=0:nvar-1
   [varname,xtype,dimids,natts]=netcdf.inqVar(ncid,id);
   varn{id+1} = varname;
   fprintf('%d %s\n',id,varn{id+1});
end
netcdf.close(ncid);

id=5
data = ncread(ncfile,varn{id+1});

ii=find(data(ix,iy,:)<100)
data(ix,iy,ii)=100;
ncwrite(outfile,varn{id+1}, data);



