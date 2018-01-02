%%
% generic plot script for time variable mossco results
%
% kai wirtz Nov2015
%
clear all;warning ('off','all');
addpath('~/tools/m_map');  % map-toolbox needed for 2D plots
addpath('~/tools/export_fig');  % toolbox needed for correct png layout
show_data=1; Is1D=0; IsNOAH=0; show_clim=2; surf=0; vis='off'; %'off' 'Visible' 'on' or 'off'
ShowMore=0;  surround=2;  % show min-max of bordering neighbor cells
tmp=version; VerMat=num2str(tmp(1:3));
%datf='~/data/DeutscheBucht/Helgoland_19622014.nc';
datf='~/data/DeutscheBucht/stations_1960-2015.nc';
datm='~/data/DeutscheBucht/chl_esacciv3.1_2000_2014.mat';
unitv={'';'';'';'';'';'';'';'';'';'';'';'';'';'';'';'';'';'';'';'';'';'';'';};
%% settings
% locations; at least one site-name (locs) shoulNoordwijk-10d be given
%loc =[]; ];%];%54.17,7.88];[55.,8.3][55.5,6.]
% Noordwijk-10[52.3,4.3];[53.77,4.77];];
loc =[[54.25,7.99];[55.05,8.34];[53.7,7.2];[52.31,4.2];[52.55,3.57];[53.42,5.15];[53.74,4.8];[54.0,8.72]; ];%[52.26,4.4];[54.6,8.4];
%%loc =[[54.13,7.98];[55.0,8.3];[53.7,7.2];[52.35,4.1];[52.6,3.5];[52.26,4.4];[53.42,5.15];[53.77,4.77];[54.0,8.7]; ];%
%%loc =[[54.13,7.98];[55.0,8.3];[53.7,7.2];[54.0,8.7]; [54.6,8.4];];%
%[54.6,8.4];[53.9,2.9];[54.14,7.97];[52.1,2.8];[54.1,6.3];[54.2,7.5];[53.92,4.6];;[55.2,5.0];[55.0,8.0];];
% %[54.96,8.4]; ];%
%  % 17 m 28 m% Noordwijk-10 Noordwijk-70'TERSLG50';};
%%locs={'Helgoland';'Sylt';'Norderney'; 'Norderelbe';'SAmrum';}; %
locs={'Helgoland';'Sylt';'Norderney';'NOORDWK10';'NOORDWK70';'TERSLG4';'TERSLG50'; 'Norderelbe';'NOORDWK2';'SAmrum';'EastAngliaPlume'; 'Helg0';'EnglishChannel';'T22';    'T26';  'TERSLG70';'T2' ;'T8';}; %
abc='cigdaebh';
%'Helgoland'; 'Sylt';    'SAmrum';'Norderelbe';'Nordeney',
%  'T36';     'T26' ;    'T41';   'T8'  ;      'T2';
%  'T22';     'T5';      'T12';   'T11'  ; 'NOORDWK2';
%[54.1,54.1];[55.0,8.4];[54.6,8.4];[54.0,8.7];[53.7,7.2];
%[53.7,6.4];[54.2,7.5];[54.0,8.1];[55.0,8.0];[55.2,5.0];
%[54.1,6.3];[55.0,6.3];[54.7,7.4];[54.7,6.9];[52.26,4.4];
if IsNOAH
 loc =[[54.18,7.86];[53.989,6.237];[53.987,6.870];[54.070,8.019];[54.173,7.962];	[54.092,7.357];	[54.439,7.425];	[54.468,6.193];	[55.038,6.403];	[54.830,5.575];	[55.257,4.746];	[55.502,4.168];	[54.685,6.737];	[54.688,7.510];	[54.194,7.234]];
 locs={'Helgoland';'NOAH-A-permeable';	'NOAH-B';	'NOAH-C-imperm';	'NOAH-CCPG';	'NOAH-D';	'NOAH-E';	'NOAH-F';	'NOAH-G';	'NOAH-H';	'NOAH-CCPJ';	'NOAH-I';	'NOAH-NSB3';	'NOAH-NSB2';	'NOAH-DB';};
end
% load and prepare data
if (show_data==1)
    read_stations_nc;
else
    show_dati=zeros(size(loc,1));
end;
%tags={'_a';'_b'};%'_c';'_3';'_0';tags={'_4';};%'_2';'_3';
%tags={'';'_Zmort';'_n'};%;};%'_0';'_1';'exu';'Ndep';
%nrow  = 1; ncol  = 1; 	% number of columns in fig
nrow  = 2; ncol  = 1; 	% number of columns in fig
nrowm = 3; ncolm = 1;%6
%nrowm = 4; ncolm = 5;
%% nrow  = 1; ncolm = 2; nrowm = 3;

ntagsu = 1; % number of constellations of plot series
if Is1D
  locs={'Helgoland'};
  loc =[54.18,7.82];
%  spath= '/home/wirtz/mossco/setups/hr/';%/local';hrres/
  spath= '/local/home/wirtz/mossco/mossco-setups/hr/'
  tags = {'_0';'_1';};%'_2';
%tags = {'ref';'vir_spor_r0.12';'vir_spor_r0.18';};
%    'vir_spor_r0.16';'vir_mu0.003';};'_vir_mu3.5';'_vir_loss1.0';};
%tags = {'ref';'AffP0.05';};
  ntags=length(tags);
  ncf0 = 'mossco_1d'; % base file name of input netcdf
  setvar_1D  % defines variables to show - and where/how to do it
  ncol = 2; nrow = 2; 	% number of columns in fig
else
 if show_data
  load(datm);
%  [m_lon,m_lat] = meshgrid(lons,lats);
   m_lon=lon;m_lat=lat;
  im0=0;
  if 1
    timeg0=timeg;
% tags ={'_vir_spor_0';'_vir_spor_r0.08';};%'_a_wat
    ii=[];
% 2004:{62,82,100,114,136,145,180,189,221,246,277,300}
%      tm={{73,81,93,97,108,119,134,165,191,196,227,257,288},{62,82,100,114,136,145,180,189,211,221,247,263}};
% mean 2000-2002 {21,35,49,63,77,91,105,119,133,147,161,175,189,203,217,231,245,259,273,287,301,315,357}
%2003: ,164,178,180,191,195,200,203,209,216,220,227,248,250,257,261,264,275,288,300,310,311
%2004:{51,54,62,75,82,85,89,100,106,114,119,136,142,145,157,163,174,180,187,201,208,211,215,221,227,235,246,251,254,260,282,286,295}
    tm={{51,98,101,126,134,136,141,151,161,171,217,225,236,263},{71,93,101,112,126,130,143,145,149,161,175,179,185,207,213,227,229,233},{45,87,94,96,100,132,136,143,152,153,168,175,192,196,210,214,226,229,245,255},{45,56,73,81,93,97,105,108,113,119,124,128,131,135,148,160,164,178,180,191,195,200},{85,100,114,145,180,221,254,286},{38,69,73,82,90,95,101,110,113,126,129,135,141,148,159,165,170,174,178,190,200,214,216,230,241,246,251,256,266,278,284,289,303},{42,57,72,82,92,95,100,104,107,124,130,134,138,149,159,162,168,174,181,185,193,196,199,206,218,231,254,264,290},{34,46,67,71,76,80,85,90,95,105,109,119,135,140,150,155,170,179,186,190,200,208,213,217,224,229,241,250,259,265,280,287,303},{40,46,56,74,89,99,101,106,112,123,128,132,139,145,160,166,179,183,196,202,206,212,217,228,236,243,257,262,283,285,313},{45,67,76,83,89,93,100,107,109,113,115,122,128,132,138,140,143,145,150,157,163,170,174,183,194,201,205,215,218,231,234,252,259,262,287,298},{63,66,82,87,98,100,104,107,110,113,117,125,130,134,140,148,154,167,174,179,185,191,197,205,218,231,240,248,264,283,290},{28,34,39,64,67,70,78,81,86,98,102,109,111,115,121,124,130,132,140,145,153,160,165,171,175,178,192,208,219,227,234,272,274,288,296},{38,56,68,71,81,85,88,96,121,134,144,147,172,206,225,229,247,253},{64,73,86,92,96,105,110,113,119,121,123,129,153,155,158,176,186,190,199,210,215,226,235,238,265,273,280},{34,64,68,71,83,101,106,109,135,138,145,150,157,163,177,185,192,198,204,215,239,247,255,261,270}};
%tm{5}={100,145,180,254};
%tm{3}={152};
    for yi=1:size(tm,2)
       for di=1:length(tm{yi})
         ii1=find(yearg==2000+yi-1 & doyg==tm{yi}{di});
         if ii1, ii=[ii; ii1];
         else
          fprintf('%d doy=%d not found\n',2000+yi-1,tm{yi}{di});
         end
       end
    end
    timeg = timeg(ii); doyg = doyg(ii); datat = datat(ii,:,:); yearg = yearg(ii);
  end %1
  timeg = timeg  + datenum('2000-01-01','yyyy-mm-dd');%- datenum('2000-01-01','yyyy-mm-dd'); %days after 1/1/2000
%%  timea=timea(2:size(timser,1)+1) + datenum('1970-01-01','yyyy-mm-dd');
timea=timea+datenum('2000-01-01','yyyy-mm-dd');
timser=timser(1:length(timea),:);
end %1D
end % show
%
% --------------------------------- edit mostly here -------------------------------
%
%tags ={'_20002004';};%'_2009b';'_adap_rub0';;'zm_fa_inf0';
tags ={'_adap0P2';'RefResults';};%
%tags ={'_ref';'_zm_fa_inf0';};
%tags ={'_ref';'_adap_rub0';};
%tags ={'_adap_rub0';};
%tags ={'_ref';'_vir_mu0';};
%;'_sinkp0';'_NH3Ads0.02';'_syn_nut4';'_mort_zoo0.021'
%'_mort_zoo0.023';'_g_max0.6';'_zm_fa_delmax10';'_basal_resp_zoo0.08';'_vir_mu-0.2';'_vir_spor_C0.002';'_vS_det34';'_z0_const0.005';
%'_sinking_factor_min0.05';'_half_sedimentation_tke1E-5';
%'_half_sedimentation_tke2E-4';'_rFast0.05';'_vS_det18';'_sinking_factor0.32';'_sinking_factor_min0.03';'_a_water0.8';bash-4.1$ %
spath= '/local/home/wirtz/sns/cut/';
%%   spath= '~/sns/';
%spath= '~/jureca/sns/cut';%   %%spath  ='/ocean-data/wirtz/';%%spath='/data/wirtz/sns/cut/';
ncf0 = 'sns'; ntags=length(tags);tagn=ntags;fignov=[];

%% check for tag file
tagfile = fullfile(spath,['tag.lst']);ncs=2;
if exist(tagfile) & 0
  tagd =importdata(tagfile,' '); tagn=length(tagd.textdata);
  ntagsu=ceil(tagn/(ncs-1))
  ni=1;
  for nsu=1:ntagsu
   tags{nsu*ncs}='_ref';%   tags{1+(nsu-1)*ncs}='';
   for nt=1:min(ncs-1,tagn-(nsu-1)*(ncs-1))
     tags{0+(nsu-1)*ncs+nt}=['_' cell2mat(tagd.textdata((nsu-1)*(ncs-1)+nt,2)) num2str(tagd.data((nsu-1)*(ncs-1)+nt))];
   end
  end
  tagn=ncs;  % compare three scenarios
  ntags=tagn
else
  tagn=0;
end

dxp = 0.82/(ncol+0.05); dyp = 0.82/(nrow +0.1);
dxpm = 0.86/( ncolm +0.05); dypm= 0.86/(nrowm+0.05);
compn ={'water';'soil'};
fs = 14; colp=prism(5);colp=gray(5);colj=colp([2:5 2:3 1:2],:); coljj=jet(11);
coljj(1,:)=[1 0.5 0];coljj(8,:)=coljj(3,:);coljj(3,:)=coljj(7,:);
colt='kw';
i0=0;coljm=ones(256,3); coljm(i0+1:256,:)=jet(256-i0);
vt{1}='00';
linw=[2 2*ones(1,14)]; lins=['- '; repmat('- ',14,1);];

%linw=ones(1,16);
% REF plot settings
% timp=1;tags ={'_ref';};
% nrowm = 2;
% Clim
% Traits  timp=2; fs = 12; tags ={'_20002004';}; nrowm = 2;tm{5}={100,114};nrowm = 3; ncolm = 2;
% ScenZoo timp=2; fs = 12; tags ={'_ref';'_zm_fa_inf0';};nrow  = 1;ncolm = 2;  tm{5}={100,254} nrowm = 2;
% zoo/vir timp=3; fs = 12; tags ={'_20002004';}; nrow  = 2;
t0y=[2000 2004 2000 2002 2000 2000];t1y=[2006 2004 2004 2004 2014 2005]; 
for timp=[1]  % 2 3 loop over time periods

if IsNOAH
   setvar_o2flux  % defines variables to show - and where/how to do it %setvar  
   %% nrowm = 1; ncolm = 1;
   ncol = 1; nrow =3; 	% number of columns in fig
 else
   setvar_All; % %Data defines variables to show - and where/how to do it %       
%   setvar_snsPOM; 
%   setvar_snsPres; % %Data defines variables to show - and where/how to do it %       
% setvar_snsTrait; %%    setvar_predprey;
%   setvar_chl
end

for nsu=1:ntagsu % loop over scenario configurations

ns2m = min(ntags,length(tags)-(nsu-1)*ncs);
fprintf('%d length(tags)=%d %d\tns2m=%d\t%d/%d\n',ntags,length(tags),nsu-1,ns2m,nsu,ntagsu);
ns1=1+(nsu-1)*ncs; ns2=ns2m+(nsu-1)*ncs;
%% create directory (name) for output
%%tagnam=sprintf('%s%s',cell2mat(tags(ns1)),cell2mat(tags(ns2)));
tagnam=sprintf('%s%s_%s%s',cell2mat(tags(ns1)),cell2mat(tags(ns2)),num2str(t0y(timp)),num2str(t1y(timp)));
fprintf('tagnam=%s\t%d/%d\n',tagnam,ns1,ns2);
figdir = fullfile('~/sns',['plots' tagnam]);
%figdir = fullfile(spath,'plots');
if ~exist(figdir),  mkdir(figdir); end;
close all;
%% open all figures
for np=1:nfig+nfigm, figure(np);
nx=(np<=nfig)*1180+(np>nfig)*(100+ncolm*200);
ny=(np<=nfig)*(80+nrow*180)+(np>nfig)*(80+nrowm*180);
set(gcf,'Position',[0 0 nx ny],'Visible','off','Color','w');clf; end
oldfig=-np;
ptag=cell2mat(var{1}(9));
occ = zeros(nfig,ncol,nrow); occ0=occ+1;

for ns=1:ns2m  %% loop over scenarios/stations/layers
 % reset index for map time offset
 moffs=0;moffc=0;mofc=0;figc=[]; varshortm0='';varshortmc0=''; imc=1;
 %% read model output
 tag=cell2mat(tags((nsu-1)*ncs+ns));
 ncfile = fullfile(spath,[ncf0 tag '.nc']);
% ncfile = fullfile(spath,[ncf0 tag '/cut/' ncf0 tag '.nc']);

 read_nc_time_layers
 t0=time(1);
 if ~exist('t1')
   t1=time(end);
 else
   if time(end) > t1
     t1=time(end);
   end
 end
t0 = datenum([num2str(t0y(timp)) '-01-02'],'yyyy-mm-dd')-1;
t1 = datenum([num2str(t1y(timp)) '-12-30'],'yyyy-mm-dd')-1;
%t0 = datenum('2001-02-01','yyyy-mm-dd')-1;
%t1 = datenum('2003-12-30','yyyy-mm-dd')-1;

 ind=find(time>= t0 & time<=t1);
 if (length(ind)>2)
  toffm = min(find(time>= t0))-1;
  year=year(ind);time=time(ind); doy=doy(ind); years= unique(year);
  it=round(1+(0:29)*(length(time)-1)/29);% iscrete index for plotting symbols

%% loop over all variables to show
  for i=1:nvar
    ptag=cell2mat(var{i}(9));  % tag with plot type and depth/time index
    varn=cell2mat(var{i}(1));
    varshort=cell2mat(var{i}(2));
    varshort0=varshort;
    if(varn(1:2)=='c ')
      eval(['tmp=' varn(3:end) ';']);
 %     units = 'mmol/m^3';
      units = unitv{i};
      dim=3;
    else
      tmp = ncread(ncfile,varn);
      if surf  & strcmp(varshort0,'CHL')
         ncfile2 = fullfile(spath,[ncf0 tag '_chls.nc']);
         tmp2 = ncread(ncfile2,'Chl_chl_in_water');
         kd   = 0.4 + 0.02*squeeze(tmp(:,:,2,:));
         fac  = exp(-2*kd.*squeeze(water_dz(:,:,2,:)));
         fac(fac>0.5) = 0.5;
         f2   = fac.*fac;
 %        tmp2 = squeeze(tmp(:,:,2));
         tmp(:,:,2,:) = (1-fac-f2).*squeeze(tmp(:,:,2,:))+fac.*squeeze(tmp2(:,:,2,:))+f2.*squeeze(tmp2(:,:,1,:));
      end
      % stored for later calculation
      if IsStore(i), eval([varshort(find(~isspace(varshort))) '=tmp;']); end
      varid = netcdf.inqVarID(ncid,varn);
      if(varn(1) == 'c')
        units = 'mmol/m^3'; dim=3;
      else
        units = netcdf.getAtt(ncid,varid,'units');
        units = strrep(units,'**','^');
        dims  = netcdf.getAtt(ncid,varid,'coordinates');
        dim   = length(strfind(dims,' '))+1;
      end
    end
%% plotting either over time (incl contour) or time sliced maps
    switch (ptag(1))
     case{'M'}
       switch (upper(ptag(2)))
         case{'A'}
           IsData=0; plot_stat_map
           if(show_data & strcmp(upper(varshort0),'CHL'))
            day1= doyg;
            IsData=1; plot_stat_map;
           end
         case{'F'}
           plot_vario;%plot_fft;
         otherwise
           plotgen_maps
        end %switch (upper(ptag(2)))
   case{'L','C','P','S','V'}
          plotgen_body
   case{'X'}
          plot_12
    case{'A'}
      ii1=find(~isnan(tmp));
      fprintf('%s: avg = %1.3f\n',varshort0,mean(mean(mean(tmp(ii1)))));
    end
   end  %nvar
% close input file
  end  %length(ind)>2
 netcdf.close(ncid);
%% add tag label from file
%name = sprintf('%s%s_%s_%s',cell2mat(vars(:,1)),cell2mat(varn{end)),run,datestr(t0,'yyyy'));
 if ns==-1
  for nt=1:tagn
   if(strfind(tag,cell2mat(tagd.textdata(nt,1)))>0)
     annotation('textbox',[ns*0.15-0.1 0.95 0.15 0.04],'String',[cell2mat(tagd.textdata(nt,2)) ':' num2str(tagd.data(nt))],'Color',coljj(ns*2-1,:),'Fontweight','bold','FontSize',fs,'LineStyle','none');
   end
  end % tagn
 end
end %ns scneario tags


%% plot each figure as EPS & PNG
figc=unique(figc);  % additional figs for map intercomparison
nfigc=length(figc);
fignov=unique(fignov);
for np=[1:nfig+nfigm+nfigc fignov];
  if(np<=nfig+nfigm | np>=100)
    figure(np);
  else
    figure(figc(np-nfig-nfigm));
  end
  set(gcf,'PaperPositionMode','auto', 'InvertHardCopy', 'off','Visible',vis);%,'Visible','off','Position',1*[0 0 1180 650]
%% add site name to each figure/page
  if(np<=nfig | np>=00)
    li=ceil((np-(np>=100)*100)/nfig0);
    fprintf('np %d/%d=%d\n', np,nfig0,li); %floor ?
    annotation('textbox',[0.45 0.9 0.2 0.045],'String',locs{max(1,li)},'Color','k','Fontweight','bold','FontSize',fs+4,'LineStyle','none');%%0.965
%% create base file name
    fnam0=sprintf('%d_%s%s',np,locs{max(1,li)},tagnam);
  else if(np<=nfig+nfigm)
     fnam0=sprintf('%d_map_%s',np,vt{min(np-nfig,length(vt))});
    else
     fnam0=sprintf('%d_%d_mapc%s%s',np,years(1),cell2mat(tags(ns1)),cell2mat(tags(ns2)));
    end
  end
%  fnam=fullfile(figdir,[fnam0 '.eps']);
%  fprintf('save EPS in %s ...\n',fnam);
%  print(gcf,'-depsc',fnam);
  fnam=fullfile(figdir,[fnam0 '.png']);
  fprintf('save PNG in %s ...\n',fnam);
%  export_fig(fnam,'-eps','-r600');
  export_fig(fnam,'-png','-r600'); %,'PaperUnits','cm','PaperSize',[30,40],'PaperPosition',[0 0 30 40],'-r300'
%  print(gcf,'-dpng',fnam);
end
end %nsu
end
if show_clim & exist('clim_avg')
 climfile = fullfile(spath,['clim.mat']);
 save(climfile,'clim_avg','clim_std','clim_doy','clim_stat');
end

