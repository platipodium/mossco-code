%
% reads time and grid info from mossco generated netcdf files
%
% kai wirtz Nov2015
%
% needs: ncfile IsSoil IsWater
ncid  = netcdf.open(ncfile,'NC_NOWRITE');
varid = netcdf.inqVarID(ncid,'time');
time_units = netcdf.getAtt(ncid,varid,'units');

t_offset = datenum(time_units(15:end),'yyyy-mm-dd HH:MM:SS');
time= (ncread(ncfile,'time')/86400)+t_offset;
year= floor(time/365.25);

if IsSoil
  soil_dz   = squeeze(ncread(ncfile,'layer_height_in_soil'));
  soil_depth= squeeze(ncread(ncfile,'layer_center_depth_in_soil'));
  soil_dzt  = squeeze(sum(soil_dz,1));
end
if IsWater
  try
    water_dz   = squeeze(ncread(ncfile,'grid_height_in_water'));
  catch exception
     varid=netcdf.inqDimID(ncid,'getmGrid3D_getm_3');
     [id nz]=netcdf.inqDim(ncid,varid);
     fprintf('using equidistant mesh\n');
     water_dz=ones(nz,length(time));
  end
  water_depth= cumsum(water_dz,1);
  water_dzt  = squeeze(sum(water_dz,1));
end
% Get name and length of first dimension
%[dimname, dimlen] = netcdf.inqDim(ncid,0)

%% check for 2D-fields
%for i=nv(1)+1:nv(2)  vinfo = ncinfo(ncfile,[ms var2{i}]);
%  if length(vinfo.Size) < 4, Is2D(i)=1; end
%end
