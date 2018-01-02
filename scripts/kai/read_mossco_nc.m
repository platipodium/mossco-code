offtime=[datenum('01-Jan-0000')  datenum('21-May-0001')];
%offtime=[datenum('01-Jan-0009')  datenum('21-May-0001')];
ivalue=[];oduvala=[];time=[];

for fi=1:1

if ~exist(files{fi},'file') warning('File %s does not exist, skipped',files{fi}); continue; end

ncid=netcdf.open(files{fi},'NC_NOWRITE');
[ndim nvar natt udimid] = netcdf.inq(ncid); 
for id=0:nvar-1
   [varname,xtype,dimids,natts]=netcdf.inqVar(ncid,id);
   varn{id+1} = varname;
   iw=strfind(varname,'_in_water');
   if(length(iw)==0) iw=strfind(varname,'_in_soil'); end
   ii=strfind(varname,'_');
   if(length(ii)>1 & length(iw)>0)
    iu0=max(find(ii<iw));
    if length(iu0)==0, 
       vars{id+1} = varname(1:iw-1);
    else
      vars{id+1} = varname(ii(iu0)+1:iw-1);
    end
   else vars{id+1} =varname;
   end
   %dimv(id+1)= natts-2;
   %if (ismember(id,vid_tshow))  
   if (strcmp(varname,'dissolved_oxygen_upward_flux_at_soil_surface') )varid=id; end  
   if (strcmp(varname,'dissolved_reduced_substances_upward_flux_at_soil_surface') )odu_id=id; end
if(id==varid | id==odu_id )
fprintf('\n ******************** \n %s==%s ?\n',varn{id+1},varna);
end
   fprintf('%d %s %s\n',id,varn{id+1},vars{id+1});
   
   %end
end

for vid=0:nvar-1
  [varname,xtype,dimids,natts]=netcdf.inqVar(ncid,vid);

  if (strcmp(varname,'getmGrid3D_y') & fi==1) lat=netcdf.getVar(ncid,vid); 
%  if (strcmp(varname,'lat') ) lat=netcdf.getVar(ncid,vid); 
 size(lat)
  end
  if (strcmp(varname,'getmGrid3D_x') & fi==1) lon=netcdf.getVar(ncid,vid);
%  if (strcmp(varname,'lon') ) lon=netcdf.getVar(ncid,vid);
  size(lon)
  end
  if strcmp(varname,'h' & fi==1)
     hlayer=netcdf.getVar(ncid,vid); 
     hlayer((hlayer<0))=NaN;
  end
  if (strcmp(varname,'level') & fi==1)lev=netcdf.getVar(ncid,vid); end
%  if strcmp(varname,'z') depth=netcdf.getVar(ncid,vid); end
%  if strcmp(varname,'bathymetry') bat=netcdf.getVar(ncid,vid); end
  if strcmp(varname,'time') 
     time=[time; offtime(fi)+netcdf.getVar(ncid,vid)/86400]; 
     timeunits=netcdf.getAtt(ncid,vid,'units');
  end
end
%time';

% if stoich==1,
% iphyc=double(squeeze(netcdf.getVar(ncid,0)));ivalue=[ivalue; iphyc];
% iphyn=double(squeeze(netcdf.getVar(ncid,1)));
% iphyp=double(squeeze(netcdf.getVar(ncid,2))); else
ivalue=cat(3+dim3d,ivalue, double(squeeze(netcdf.getVar(ncid,varid))));  %end
if odu, oduvala=cat(3+dim3d,oduvala, double(squeeze(netcdf.getVar(ncid,odu_id)))); end

end %fi

if ~exist('bat')
  fprintf('reading external topo.nc ....\n');
%  nctop=netcdf.open('~/mossco/mossco-setups/NSBS6nm/topo.nc','NC_NOWRITE');
  nctop=netcdf.open('~/mossco/mossco-setups/sns/Topo/topo.nc','NC_NOWRITE');

  [ndim nvar natt udimid] = netcdf.inq(nctop); 
  for vid=0:nvar-1
   [varname,xtype,dimids,natts]=netcdf.inqVar(nctop,vid);

%   if (strcmp(varname,'lat') ) lat=netcdf.getVar(nctop,vid);  end
%   if (strcmp(varname,'lon') ) lon=netcdf.getVar(nctop,vid);  end
   if strcmp(varname,'bathymetry') 
       bat=netcdf.getVar(nctop,vid);
       size(bat)
       end
  % length(find(bats<-1350))
  end
  netcdf.close(nctop);
end
%size(lon)

%hour=mod(round((time-time(1))./3600),24);
doy=floor(mod(time,365.25)+1);
year=2000+floor(time./(365.25));
years=unique(year);
nti=length(time);
fprintf('time %d\t%1.1f %1.1f ... %1.1f\n',nti,time(1),time(2),time(end));
%nlon=size(lon,1)-1;  nlat=size(lat,2);  
nlon=size(lon,1);  nlat=size(lat,2);  
lon=double(lon(1:nlon,1:nlat));  lat=double(lat(1:nlon,1:nlat));

ig=find(lon>0 & lon<9E9 );
lonlimit=cl_minmax(cl_minmax(lon(ig)));
%lonlimit(1)=lonlimit(1)+1.2; lonlimit(2)=lonlimit(2)-0.0; 
ig=find(lat>0 & lat<9E9 );
latlimit=cl_minmax(cl_minmax(lat(ig)));
%latlimit(1)=latlimit(1)+0.; latlimit(2)=latlimit(2)-0.0; 

%lon(lon<0)=lonlimit(1);lon(lon>9E9)=lonlimit(2);
%lat(lat<0)=latlimit(1);lat(lat>9E9)=latlimit(2);

if ~exist('mossco_visual_boundaries_sns.mat','file') & 0
   m_gshhs('ib','save','mossco_visual_boundaries');
   m_gshhs('hr','save','mossco_visual_rivers');
   m_gshhs('fc','save','mossco_visual_coasts');
end

