%%
% generic plot script for time variable mossco results
%
% kai wirtz Nov2015
%
clear all;close all;warning ('off','all');
addpath('~/tools/m_map');  % map-toolbox needed for 2D plots
show_data=1; Is1D=0; IsNOAH=0; 
%datf='~/data/DeutscheBucht/Helgoland.nc';
datf='~/data/DeutscheBucht/stations.nc';
datm='~/data/DeutscheBucht/esacci_2003_2013.mat';
%% settings
% locations; at least one site-name (locs) should be given 
%loc =[]; ];%];%
loc =[[54.18,7.86];[55.,8.3];[53.7,7.2];[52.3,4.3];[52.56,3.5];[53.42,5.15];[53.76,4.77]; [54.6,8.4];[54.0,8.7];[54.1,6.3];[54.2,7.5];];%;[53.92,4.6];[55.2,5.0];[55.0,8.0];]; 
% %[54.96,8.4]; 
%  % 17 m 28 m% Noordwijk-10 Noordwijk-70
locs={'Helgoland';'Sylt'; 'Norderney';'NOORDWK10';'NOORDWK70';'TERSLG4';  'TERSLG50';   'SAmrum';  'Norderelbe'; 'T22';    'T26';  'TERSLG70';'T2' ;'T8';}; %
%'Helgoland'; 'Sylt';    'SAmrum';'Norderelbe';'Nordeney',
%  'T36';     'T26' ;    'T41';   'T8'  ;      'T2';
%  'T22';     'T5';      'T12';   'T11'
%[54.1,54.1];[55.0,8.4];[54.6,8.4];[54.0,8.7];[53.7,7.2];
%[53.7,6.4];[54.2,7.5];[54.0,8.1];[55.0,8.0];[55.2,5.0];
%[54.1,6.3];[55.0,6.3];[54.7,7.4];[54.7,6.9];
if IsNOAH
 loc =[[54.18,7.86];[53.989,6.237];	[53.987,6.870];	[54.070,8.019];	[54.173,7.962];	[54.092,7.357];	[54.439,7.425];	[54.468,6.193];	[55.038,6.403];	[54.830,5.575];	[55.257,4.746];	[55.502,4.168];	[54.685,6.737];	[54.688,7.510];	[54.194,7.234]];
 locs={'Helgoland';'NOAH-A-permeable';	'NOAH-B';	'NOAH-C-imperm';	'NOAH-CCPG';	'NOAH-D';	'NOAH-E';	'NOAH-F';	'NOAH-G';	'NOAH-H';	'NOAH-CCPJ';	'NOAH-I';	'NOAH-NSB3';	'NOAH-NSB2';	'NOAH-DB';};	
end
% load and prepare data
if (show_data==1)
    read_stations_nc;  
else
    show_dati=zeros(size(loc,1));
end;
surf=1;
%tags={'_a';'_b'};%'_c';'_3';'_0';tags={'_4';};%'_2';'_3';
%tags={'';'_Zmort';'_n'};%;};%'_0';'_1';'exu';'Ndep';
nrow  = 2; ncol  = 2; 	% number of columns in fig
%%nrow  = 1; ncol  = 1; 	% number of columns in fig
nrowm = 4; ncolm = 6;
nrowm = 2; ncolm = 3;
if Is1D 
  locs={'Helgoland'};
  loc =[54.18,7.82];
%  spath= '/home/wirtz/mossco/setups/hr/';%/local';hrres/
  spath= '/local/home/wirtz/mossco/mossco-setups/hr/'
%tags = {'ref';'mort_zoo0.02';'mort_zoo0.03';'zm_fa_delmax1';};
%tags = {'ref';'vS_det30';'vS_phy0.';'hydrol0.01';'remin0.03';'phi_agg0.001';'phi_agg0.01';};
%tags = {'ref';'V_NC_max0.9';'rFast0.06';'a_water0.8';'phi_agg0.003';'syn_nut-4.6';};
    %'PAds1';'ksNO3denit7.';'rSlow0.001';};'vir_phyC0.5';'vir_spor_C0.00001';
%'mort_zoo0.017';'mort_zoo0.32';'zm_fa_delmax2.5';'zm_fa_inf0.02';'zm_fa_inf0.2';
%'QN_phy_max0.17';'QN_phy_max0.26';'alpha0.22';'AffN0.15';'AffP0.05';
%tags = {'ref';'zm_fa_inf0.01';'zm_fa_inf2';};%'zm_fa_delmax1';'zm_fa_delmax9';%%'vir_infect0.001';'vir_infect0.0035';'vir_loss0.1';'rnit140';'Nqual0.';'vS_det15';};'vir_spor_C0.002';'vir_phyC0.1';'vir_phyC2.';
%tags ={'ref';'vsphy';};% {'_0';'_1';};%'phi_agg5E-4';'agg_doc0.01';'vir_loss0.';};'_2';
tags = {'_0';'_1';};%'_2';
%};%'vir_loss0.02';'vir_loss0.2';vir_phyC0.1';'vir_phyC8.0';'vir_phyC80';'vir_spor_C0.00001';'vir_spor_C0.01';'vir_mu0.003';'vir_mu0.01';'vir_loss0.3';'vir_loss0.8';
%tags = {'ref';'vir_spor_r0.12';'vir_spor_r0.18';};
%    'vir_spor_r0.16';'vir_mu0.003';};'_vir_mu3.5';'_vir_loss1.0';};
%tags = {'ref';'AffP0.05';};
ntags=length(tags);
  ncf0 = 'mossco_1d'; % base file name of input netcdf
  setvar_1D  % defines variables to show - and where/how to do it 
  ncol = 3; nrow = 2; 	% number of columns in fig
else
if show_data
    load(datm);
    [m_lon,m_lat] = meshgrid(lons,lats);
    im0=0;
    timeg = timeg  + datenum('1970-01-01','yyyy-mm-dd');- datenum('2000-01-01','yyyy-mm-dd'); %days after 1/1/2000
end
%  loc =[54.18,7.82];
%'_a_water1.1''_vS_phy2.5';'_vir_mu0.07';'_a_water1.1';
tags ={'';'_2';'_1';};%'_2';'_a1.5';'_2010';'_att';'';'_30_0';'_30_1';'_del2';'';'_genMeth12';
%tags ={'_3';'_20';};%t'_sinking_factor_min0.03';'_sinking_factor_min0.27';'_sinking_factor_min0.39';};
%tags ={'ResAmpl.01';'ResAmpl.19';'ResAmpl.37';'ResAmpl.55';};'_rSlow0.0001';_vir_spor_C0.003
%'_phi_agg0.003';'_remin0.1';'_vir_loss1.0';'_vir_mu3.5';'_mort_zoo0.035';

 ntags=length(tags);
 spath= '/local/home/wirtz/sns/';%  
 %spath= '~/jureca/sns/cut';%   
%%spath  ='/ocean-data/wirtz/';%
%%spath='/data/wirtz/sns/cut';
  ncf0 = 'sns'; 
  if IsNOAH
    setvar_o2flux  % defines variables to show - and where/how to do it %setvar  
   %% nrowm = 1; ncolm = 1;
 %%   ncol = 2; nrow = 1; 	% number of columns in fig
  else
    setvar_sns %Pres Data defines variables to show - and where/how to do it %setvar  
  end
end

dxp = 0.82/(ncol+0.05); dyp = 0.83/(nrow +0.05);
dxpm = 0.86/( ncolm +0.05); dypm= 0.86/(nrowm+0.05);
compn ={'water';'soil'};
fs = 16; colp=prism(5);colj=colp([1 4:5 2:3 1:2],:); coljj=jet(11); colt='kw';
i0=10;coljm=ones(256,3); coljm(i0+1:256,:)=jet(256-i0);

linw=[3 2*ones(1,14)]; lins=['- '; repmat('- ',14,1);]; 

%% check for tag file
tagfile = fullfile(spath,['tag.lst']);
if exist(tagfile)
  tagd =importdata(tagfile,' '); tagn=length(tagd.textdata);
else
  tagn=0;
end

%% open all figures
for np=1:nfig+nfigm, figure(np); set(gcf,'Position',[0 0 1680 850],'Visible','off','Color','w'); end
oldfig=-np; 
ptag=cell2mat(var{1}(9));
occ = zeros(nfig,ncol,nrow); occ0=occ+1;
for ns=1:ntags %% loop over scenarios/stations/layers
 % reset index for map time offset
 moffs=0;moffc=0;mofc=0;figc=[]; varshortm0='';varshortmc0=''; imc=1;
 %% read model output
 tag=cell2mat(tags(ns));
 ncfile = fullfile(spath,[ncf0 tag '.nc']);
 %%ncfile = fullfile(spath,[ncf0 tag '/cut/' ncf0 tag '.nc']);

 read_nc_time_layers
 t0=time(1); t1=time(end);
% t0 = datenum('1962-03-01','yyyy-mm-dd')-1;
% t0 = datenum('1961-02-01','yyyy-mm-dd')-1;
t0 = datenum('2004-02-01','yyyy-mm-dd')-1;
t1 = datenum('2005-09-30','yyyy-mm-dd')-1;
%t1 = datenum('2005-10-20','yyyy-mm-dd')-1;

 ind=find(time>= t0 & time<=t1);
 toffm = min(find(time>= t0))-1;
 year=year(ind);time=time(ind); doy=doy(ind); years= unique(year);
 it=round(1+(0:19)*(length(time)-1)/19);% discrete index for plotting symbols

%% loop over all variables to show
  for i=1:nvar
    ptag=cell2mat(var{i}(9));  % tag with plot type and depth/time index
    varn=cell2mat(var{i}(1));
    varshort=cell2mat(var{i}(2));
    varshort0=varshort;
    if(varn(1:2)=='c ')
      eval(['tmp=' varn(3:end) ';']);
    else
      tmp = ncread(ncfile,varn);
      if surf & size(tmp,3)>2
         tmp2 = squeeze(sum(tmp(:,:,2:end,:),3))/(size(tmp,3)-1); 
         tmp(:,:,2,:) = tmp2;
         tmp = tmp(:,:,1:2,:);
      end
      % stored for later calculation
      if IsStore(i), eval([varshort(find(~isspace(varshort))) '=tmp;']); end
      varid = netcdf.inqVarID(ncid,varn);
      if(varn(2) == 'C' )
        units = '-'; dim=3;
      else
        units = netcdf.getAtt(ncid,varid,'units');
        dims  = netcdf.getAtt(ncid,varid,'coordinates');
        dim   = length(strfind(dims,' '))+1;
      end
    end
%% plotting either over time (incl contour) or time sliced maps  
    if (ptag(1)=='M')
      plotgen_maps
    else
      plotgen_body
    end
 end  %nvar
% close input file
 netcdf.close(ncid);
%% add tag label from file
%name = sprintf('%s%s_%s_%s',cell2mat(vars(:,1)),cell2mat(varn{end)),run,datestr(t0,'yyyy'));
 if ns==1
  for nt=1:tagn
   if(strfind(tag,cell2mat(tagd.textdata(nt,1)))>0)
     annotation('textbox',[ns*0.15-0.1 0.95 0.15 0.04],'String',[cell2mat(tagd.textdata(nt,2)) ':' num2str(tagd.data(nt))],'Color',coljj(ns*2-1,:),'Fontweight','bold','FontSize',fs,'LineStyle','none');
   end 
  end % tagn
 end
end %ns scneario tags

%% create directory (name) for output
tagnam=sprintf('%s%s',cell2mat(tags(1)),cell2mat(tags(end)));
figdir = fullfile('~/sns',['plots' tagnam]);
%figdir = fullfile(spath,'plots');
if ~exist(figdir),  mkdir(figdir); end;

%% plot each figure as EPS & PNG
figc=unique(figc);  % additional figs for map intercomparison
nfigc=length(figc);
for np=1:nfig+nfigm+nfigc
  if(np<=nfig+nfigm)
    figure(np);  
  else
    figure(figc(np-nfig-nfigm));
  end  
  set(gcf,'PaperPositionMode','auto', 'InvertHardCopy', 'off','Visible','off');%,'Visible','off'
%% add site name to each figure/page
  if(np<=nfig)
    li=floor(np/nfig0);
    annotation('textbox',[0.45 0.95 0.2 0.045],'String',locs{max(1,li)},'Color','k','Fontweight','bold','FontSize',fs+2,'LineStyle','none');
%% create base file name
    fnam0=sprintf('%d_%s%s',np,locs{max(1,li)},tagnam);
  else if(np<=nfig+nfigm) 
     fnam0=sprintf('%d_map_%s',np,vt{min(np-nfig,length(vt))});
      else
     fnam0=sprintf('%d_mapc%s%s',np,cell2mat(tags(1)),cell2mat(tags(end)));    
    end 
  end
%  fnam=fullfile(figdir,[fnam0 '.eps']);
%  fprintf('save EPS in %s ...\n',fnam);
%  print(gcf,'-depsc',fnam);
  fnam=fullfile(figdir,[fnam0 '.png']);
  fprintf('save PNG in %s ...\n',fnam);
  print(gcf,'-dpng',fnam);
end


