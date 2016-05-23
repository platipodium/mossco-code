%
% reads time and grid info from mossco generated netcdf files
%
% kai wirtz Nov2015
%
% needs: ncfile IsSoil IsWater
ncid  = netcdf.open(ncfile,'NC_NOWRITE');
varid = netcdf.inqVarID(ncid,'time');
time_units = netcdf.getAtt(ncid,varid,'units');
if Is1D 
  coordn='helgoland3d';
else
  coordn='getmGrid3D_getm';
end

t_offset = datenum(time_units(15:end),'yyyy-mm-dd HH:MM:SS');
time= (ncread(ncfile,'time')/86400)+t_offset;
year= floor(time/365.25);
doy=floor(mod(time,365.25)+1);

if IsSoil
  try
    soil_dz   = squeeze(ncread(ncfile,'layer_height_in_soil'));
    soil_depth= squeeze(ncread(ncfile,'layer_center_depth_in_soil'));
  catch exception
    soil_dz = 0.05*ones(2,length(time));
    soil_depth= cumsum(soil_dz,1);;
  end
  soil_dzt  = squeeze(sum(soil_dz,1));

end
if IsWater
  try
    water_dz   = squeeze(ncread(ncfile,'grid_height_in_water'));
  catch exception
     varid=netcdf.inqDimID(ncid,[coordn '_3']);
     [id nz]=netcdf.inqDim(ncid,varid);
     fprintf('using equidistant mesh\n');
     water_dz = ones(nz,length(time));
  end
  water_depth= cumsum(water_dz,1);
  depth = unique(water_depth);
  water_dzt  = squeeze(sum(water_dz,1));
end
% Get name and length of first dimension
%[dimname, dimlen] = netcdf.inqDim(ncid,0)

%% check for 2D-fields
%for i=nv(1)+1:nv(2)  vinfo = ncinfo(ncfile,[ms var2{i}]);
%  if length(vinfo.Size) < 4, Is2D(i)=1; end
%end
i_loc=[1 1];
if length(locs) >1 | nfigm>0 | (ptag(1)=='T')
 % reading geo-coordinates
 varid=netcdf.inqVarID(ncid,[coordn '_lon']);
% [id loni]=netcdf.inqDim(ncid,varid);
 lon=netcdf.getVar(ncid,varid);

 varid=netcdf.inqVarID(ncid,[coordn '_lat']);
% [id lati]=netcdf.inqDim(ncid,varid);
 lat=netcdf.getVar(ncid,varid);
% limits
 ig=find(lon>0 & lon<9E9 );
 lonlimit=cl_minmax(cl_minmax(lon(ig)));
%lonlimit(1)=lonlimit(1)+1.2; lonlimit(2)=lonlimit(2)-0.0; 
 ig=find(lat>0 & lat<9E9 );
 latlimit=cl_minmax(cl_minmax(lat(ig)));
%% positioning (Helgoland)

 if length(loc) >0
% dr=0.03; [ix_hr iy_hr]=find(abs(lat-54.15)<dr & abs(lon-7.8)<dr);
  for li=1:size(loc,1)
    [m1 i]=min(abs(lat-loc(li,1))+abs(lon-loc(li,2)),[],1);
    [m j]=min(m1);
    i_loc(li,1:2)=[i(j) j];
    fprintf('%s: %1.2f %1.2f\t%d %d\n',locs{li},lon(i_loc(li,1),i_loc(li,2)),lat(i_loc(li,1),i_loc(li,2)),i_loc(li,1),i_loc(li,2));
  end
 end
end %length(locs) >1
if ptag(1)=='T'
  varid=netcdf.inqDimID(ncid,[coordn '_1']);
  [id n1]=netcdf.inqDim(ncid,varid);
  varid=netcdf.inqDimID(ncid,[coordn '_2']);
  [id n2]=netcdf.inqDim(ncid,varid);
  if(n1>n2)
    txi=1:n1;
    tx=lon(1,txi);
    txn='lon';
  else
    txi=1:n2;
    tx=lat(1,txi);
    txn='lat';
  end
  iw=strfind(ncfile,'cutT');
  ncfileD=ncfile; ncfileD(iw:iw+3)='cutD';
  water_d = squeeze(ncread(ncfileD,'water_depth_at_soil_surface'));
%    water_dz = water_dz.*water_d/size(water_dz,1);
  Dmax=round(max(max(water_d))); 
end


