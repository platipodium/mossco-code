
close all;clear all;
%% ncfile='topo_HR.nc';
%%ncfile='restart61Mar_20.29.nc';
%%ncfile='restart144Mar_20.067.nc';
%%ncfile='/data/wirtz/sns/Forcing/atm_n_EMEP-hybridClim_NWCS20_2000-2010_timmean_cut_Rsns_HR.nc';
ncfile='/data/wirtz/sns/Forcing/bdy_3d_bio_ecoham_2000-2010_linear_ref_Z30SigmaL_Tclim_MWNext_redNP.nc';
ncfile='/data/wirtz/sns/Forcing/River/tmp.nc';
outfile=ncfile;
ncid  = netcdf.open(ncfile,'NC_NOWRITE');

[ndim nvar natt udimid] = netcdf.inq(ncid); 
for id=0:nvar-1
   [varname,xtype,dimids,natts]=netcdf.inqVar(ncid,id);
 %  varn{id+1} =  mat2cell(varname);
   varn{id+1} =  varname;
   fprintf('%d %s\n',id,varn{id+1});
end
netcdf.close(ncid);

%% ix1 =100; iy1= 53;ix2 =100; iy2= 52 ; % topo 
%% ix1 =2; iy1= 10; ix2 =2; iy2= 9 ; %restart61
%%ix1 =11; iy1= 3; ix2 =11; iy2= 2 ;  %restart144
ix1 =102; iy1= 52;ix2 =101; iy2= 53 ; % topo 
ix1 =8; iy1= 92; % river 

%for id=2:3  % topo 
for id=0:1
% for id=[0:12 17:20 22:23]
  data = ncread(ncfile,varn{id+1});
  if(length(size(data))>1)
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
%     axis([max(1,ix1-5) min(size(map,1),ix1+5) max(1,iy1-5) min(size(map,2),iy1+5)]);
    end
%%    tmp=data(ix2,iy2,:);
 %   size(data)
 %%   fprintf('%s: swap %1.2f <-> %1.2f\n',varn{id+1},mean(tmp), mean(data(ix1,iy1,:)));
%    data(ix2,iy2,:)=data(ix1,iy1,:);
%% river
ix1 =8; iy1= 92; % river 
%data(ix1,iy1,1)
fprintf('%s: %1.2f\t',varn{id+1}, mean(mean(data(ix1,iy1,:))));
data(ix1,iy1,:)=data(ix1,iy1,:)*0.2;
fprintf('-> %1.2f\n', mean(mean(data(ix1,iy1,:))));

ix1 =5; iy1= 80; % river 
fprintf('%s: %1.2f\t',varn{id+1}, mean(mean(data(ix1,iy1,:))));
data(ix1,iy1,:)=data(ix1,iy1,:)*0.4;
fprintf('-> %1.2f\n',mean(mean(data(ix1,iy1,:))));
%    if id==3
 %%    data(ind)=-2.e+20;
 %%%   ind=find(data>=-1);
 %%%   data(ind)=data(ind)./(1+99*exp(-(2+(id-2)*10)*data(ind)));
 %%   end
%%     ind=find(data<-2.e+20);
%%     data(ind)=-2.e+20;

 %%   data(ix2+1,iy2)=data(ix1,iy1);
%%    data(ix1,iy1,:)=tmp;
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



