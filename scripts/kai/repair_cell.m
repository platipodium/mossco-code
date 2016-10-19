
close all;
ncfile='~/sns/bdy_3d_bio_ecoham_2000-2010_red.nc';
%outfile='~/sns/bdy_3d_bio_ecoham_2000-2010_red.nc';
%ncfile='~/sns/restart144Mar_20.001.nc';
%%ncfile='restart144Mar_20.067.nc';
outfile=ncfile;

ncid  = netcdf.open(ncfile,'NC_WRITE');
%ix1 =1; iy1= 15; ix2 =1; iy2= 14 ; %boundary
ix1 =2; iy1= 7; ix2 =2; iy2= 6 ; %restart
%% ix1 =100; iy1= 53;ix2 =100; iy2= 52 ; % topo 
%% ix1 =2; iy1= 10; ix2 =2; iy2= 9 ; %restart61
%%ix1 =11; iy1= 3; ix2 =11; iy2= 2 ;  %restart144

[ndim nvar natt udimid] = netcdf.inq(ncid); 
for id=0:nvar-1
   [varname,xtype,dimids,natts]=netcdf.inqVar(ncid,id);
   varn{id+1} = varname;
   fprintf('%d %s\n',id,varn{id+1});
end
netcdf.close(ncid);



%for id=1:1  % topo 
for id=0:nvar-1
% for id=[0:12 17:20 22:23]
  data = ncread(ncfile,varn{id+1});
  if(length(size(data))>2)
    if id==1, IsOut=true; else  IsOut=false; end
    if IsOut
     figure(id+1);set(gcf,'Position',[id*20 id*30 1100 550],'Visible','on'); 
     subplot(1,2,1)
     if length(size(data))==3
       map=data(:,:,1);
     else
       map=data(:,:);
     end
     imagesc(map);
%    axis([max(1,ix1-5) min(size(map,1),ix1+5) max(1,iy1-5) min(size(map,2),iy1+5)]);
    end
    tmp=data(ix2,iy2,:);
 %   size(data)
    fprintf('%s: swap %1.2f <-> %1.2f\n',varn{id+1},mean(tmp), mean(data(ix1,iy1,:)));
    data(ix2,iy2,:)=data(ix1,iy1,:);
    data(ix1,iy1,:)=tmp;
    
%%    data(101,53) =9.5;  data(100,53) =25;
    if IsOut
     subplot(1,2,2)   
     if length(size(data))==3
       map=data(:,:,1);
     else
       map=data(:,:);
     end
     imagesc(map);
    end
%    axis([max(1,ix1-5) min(size(map,1),ix1+5) max(1,iy1-5) min(size(map,2),iy1+5)]);
     ncwrite(outfile,varn{id+1}, data);
   end %if(length(size(data))>3)
end



