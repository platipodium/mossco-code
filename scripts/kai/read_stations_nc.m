%offdatime=[datenum('01-Feb-0000')  datenum('21-May-0001')];

if ~exist(datf,'file') warning('File %s does not exist, skipped',datf); continue; end

ncid=netcdf.open(datf,'NC_NOWRITE');
[ndim ndvar natt udimid] = netcdf.inq(ncid); 
is=0; iv=1; oldstatn='dummy';
for id=0:ndvar-1
  [datvarname,xtype,dimids,natts]=netcdf.inqVar(ncid,id);
  if strcmp(datvarname,'time') 
    datime=netcdf.getVar(ncid,id)/86400; 
    datimeunits=netcdf.getAtt(ncid,id,'units');
%    fprintf(' %d data times found:\t%s\n',length(datime),datimeunits);

  else
%   iw=strfind(datvarname,'DIP');
    iw=strfind(datvarname,'-');
    varid = netcdf.inqVarID(ncid,datvarname);
    if ~strcmpi(datvarname(1:iw-1),oldstatn)
      is=is+1;
      statn{is}=datvarname(1:iw-1);
      oldstatn=  statn{is};
      lat(is) = netcdf.getAtt(ncid,varid,'lat');
      lon(is) = netcdf.getAtt(ncid,varid,'lon');
%      fprintf('\n%d %s \tlat/lon= %1.1f %1.1f\n',is,(statn{is}),lat(is),lon(is));%cell2mat
 %     vars{is,iv}='';
      iv=1;
    end
    vars{is,iv} = datvarname(iw+1:end);
    data{is,iv} = netcdf.getVar(ncid,id);
   %dimv(id+1)= natts-2;
   %if (ismember(id,vid_tshow))  
 %%   fprintf('%d %s found %d values\n',is,(vars{is,iv}),length(find(~isnan(data{is,iv}))));
   %end
    iv=iv+1;
    vars{id,iv}='';
  end
end
%% print list of stations and their location
if 1
for i=1:is, fprintf('''%s'',',statn{i});
 if(mod(i,6)==5)fprintf('\n');end 
 end
fprintf('\n');
for i=1:is, fprintf('[%1.1f,%1.1f];',lat(i),lon(i));
 if(mod(i,6)==5)fprintf('\n'); end
 end
end
fprintf('\n');
%hour=mod(round((datime-datime(1))./3600),24);

t_offset = datenum(datimeunits(15:end),'yyyy-mm-dd HH:MM:SS');
datime= datime+t_offset;
dadoy=floor(mod(datime,365.25)+1);
dayear=floor(datime./(365.25));
dayears=unique(dayear);
for i=1:is
  dval = data{i,1};
  indd  = find(~isnan(dval) & dayear>=2002 & dayear<=2005 );
  fprintf('found %d in 2002-2005 for station %s\n',length(indd),statn{i});
end

% connect station and variable name to data index
for li=1:size(loc,1) 
  show_dati(li)=0;
  for i=1:is
    if strcmpi(locs{li},statn{i})
      show_dati(li)=i;
      break;
    end
  end
end

