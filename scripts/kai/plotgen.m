%%
% generic plot script for time variable mossco results
%
% kai wirtz Nov2015
%
clear all;warning ('off','all');
addpath('~/tools/m_map');  % map-toolbox needed for 2D plots
addpath('~/tools/export_fig');  % toolbox needed for correct png layout
show_data=1; Is1D=0; IsNOAH=0; tmp=version; VerMat=num2str(tmp(1:3)); 
%datf='~/data/DeutscheBucht/Helgoland_19622014.nc';
datf='~/data/DeutscheBucht/stations_1960-2015.nc';
datm='~/data/DeutscheBucht/chl_esacci_2000_2013.mat';

%% settings
% locations; at least one site-name (locs) should be given 
%loc =[]; ];%];%54.17,7.88];[55.,8.3][55.5,6.]
loc =[[54.14,7.97];[55.0,8.4];[53.7,7.2];[52.3,4.3];[52.59,3.53];[53.42,5.15];[53.77,4.77]; [54.6,8.4];[54.0,8.7];[53.9,2.9];[54.17,7.88];];%[52.1,2.8];[54.1,6.3];[54.2,7.5];[53.92,4.6];;[55.2,5.0];[55.0,8.0];]; 
% %[54.96,8.4]; ];%
%  % 17 m 28 m% Noordwijk-10 Noordwijk-70
locs={'Helgoland';'Sylt'; 'Norderney';'NOORDWK10';'NOORDWK70';'TERSLG4';  'TERSLG50';   'SAmrum';  'Norderelbe';'EastAngliaPlume'; 'HR0';'EnglishChannel';'T22';    'T26';  'TERSLG70';'T2' ;'T8';}; %
%'Helgoland'; 'Sylt';    'SAmrum';'Norderelbe';'Nordeney',
%  'T36';     'T26' ;    'T41';   'T8'  ;      'T2';
%  'T22';     'T5';      'T12';   'T11'  ; 'NOORDWK2';
%[54.1,54.1];[55.0,8.4];[54.6,8.4];[54.0,8.7];[53.7,7.2];
%[53.7,6.4];[54.2,7.5];[54.0,8.1];[55.0,8.0];[55.2,5.0];
%[54.1,6.3];[55.0,6.3];[54.7,7.4];[54.7,6.9];[52.26,4.4];
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

surf=0; vis='off'; % 'Visible' 'on' or 'off'
surround=2;  % show min-max of bordering neighbor cells
%tags={'_a';'_b'};%'_c';'_3';'_0';tags={'_4';};%'_2';'_3';
%tags={'';'_Zmort';'_n'};%;};%'_0';'_1';'exu';'Ndep';
%nrow  = 1; ncol  = 1; 	% number of columns in fig
nrow  = 2; ncol  = 1; 	% number of columns in fig
nrowm = 4; ncolm = 7;
%nrowm = 3; ncolm = 5;
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
  [m_lon,m_lat] = meshgrid(lons,lats);
  im0=0;
  if 1
    timeg0=timeg;
% tags ={'_vir_spor_0';'_vir_spor_r0.08';};%'_a_wat
    ii=[];
%      tm={{73,81,93,97,108,119,134,165,191,196,227,257,288},{62,82,100,114,136,145,180,189,211,221,247,263}};
    tm={{21,35,49,63,77,91,105,119,133,147,161,175,189,203,217,231,245,259,273,287,301,315,357},{21,35,49,63,77,91,105,119,133,147,161,175,189,203,217,231,245,259,273,287,301,315,357},{21,35,49,63,77,91,105,119,133,147,161,175,189,203,217,231,245,259,273,287,301,315,357},{81,93,97,108,119,135,160,180,195,227,257,288},{62,82,100,114,136,145,180,189,221,246,277,300},{82,90,102,113,126,135,148,160,175,200,214,230,243,266,289},{63,82,85,95,104,107,124,162,181,193,199,206,231,254,264,311},{67,73,85,95,105,119,140,150,179,189,200,208,217,241,250,265,283,303,305},{40,46,56,74,89,99,101,104,106,112,123,128,132,135,145,160,179,183,196,205,217,223,228,243,262,291,303,313}};
    for yi=1:size(tm,2)
       for di=1:length(tm{yi})
         ind=find(yearg==2000+yi-1 & doyg==tm{yi}{di});
         if ind, ii=[ii; ind]; end
       end
    end
    timeg = timeg(ii); doyg = doyg(ii); datat = datat(ii,:,:); yearg = yearg(ii);
  end
  timeg = timeg  + datenum('1970-01-01','yyyy-mm-dd');- datenum('2000-01-01','yyyy-mm-dd'); %days after 1/1/2000
  timea=timea(2:size(timser,1)+1) + datenum('1970-01-01','yyyy-mm-dd');
%
% --------------------------------- edit mostly here -------------------------------
%
  tags ={'';'_const_NC_zoo0.15';'_vir_mu-0.1';};
%'_a_water1.4';'_zm_fa_inf0.5';'_mort_zoo0.014';'_g_max0.45';'_basal_resp_zoo0.08';'_const_PC_zoo0.035';'_const_NC_zoo0.15';'_vir_mu-0.1'
%'_sinking_factor_min0.05';'_half_sedimentation_tke1E-5';
 spath= '/local/home/wirtz/sns/cut/';%   spath= '~/';
% spath= '~/jureca/sns/cut';%   %%spath  ='/ocean-data/wirtz/';%%%sns/cut
 %spath='/data/wirtz/';
 end
 ncf0 = 'sns'; ntags=length(tags);
 if IsNOAH
    setvar_o2flux  % defines variables to show - and where/how to do it %setvar  
   %% nrowm = 1; ncolm = 1;
     ncol = 1; nrow =3; 	% number of columns in fig
 else
    setvar_snsPres; % %Data defines variables to show - and where/how to do it %       
%% setvar_snsTrait; %%    setvar_predprey;
 end
end
%% check for tag file
tagfile = fullfile(spath,['tag.lst']);ncs=3;
if exist(tagfile) & 1
  tagd =importdata(tagfile,' '); tagn=length(tagd.textdata);
  ntagsu=ceil(tagn/(ncs-1)); ni=1;
  for nsu=1:ntagsu
   tags{1+(nsu-1)*ncs}='';
   for nt=1:min(ncs-1,tagn-(nsu-1)*(ncs-1))
    tags{1+(nsu-1)*ncs+nt}=['_' cell2mat(tagd.textdata((nsu-1)*(ncs-1)+nt,2)) num2str(tagd.data((nsu-1)*(ncs-1)+nt))];
    end
  end
  tagn=3;  % compare three scenarios
else
  tagn=0;
end

dxp = 0.82/(ncol+0.05); dyp = 0.83/(nrow +0.05);
dxpm = 0.86/( ncolm +0.05); dypm= 0.86/(nrowm+0.05);
compn ={'water';'soil'};
fs = 12; colp=prism(5);colp=gray(5);colj=colp([2:5 2:3 1:2],:); coljj=jet(11); 
coljj(8,:)=coljj(3,:);coljj(3,:)=coljj(7,:);
colt='kw';
i0=10;coljm=ones(256,3); coljm(i0+1:256,:)=jet(256-i0);
vt{1}='00';
linw=[2 2*ones(1,14)]; lins=['- '; repmat('- ',14,1);]; 
%linw=ones(1,16);

for nsu=1:ntagsu % loop over scenario configurations

ns2m = min(ntags,length(tags)-(nsu-1)*ncs);
ns1=1+(nsu-1)*ncs;ns2=ns2m+(nsu-1)*ncs;
%% create directory (name) for output
tagnam=sprintf('%s%s',cell2mat(tags(ns1)),cell2mat(tags(ns2)));
figdir = fullfile('~/sns',['plots' tagnam]);
%figdir = fullfile(spath,'plots');
if ~exist(figdir),  mkdir(figdir); end;
close all;
%% open all figures
for np=1:nfig+nfigm, figure(np);  set(gcf,'Position',[0 0 1280 560],'Visible','off','Color','w');clf; end
oldfig=-np; 
ptag=cell2mat(var{1}(9));
occ = zeros(nfig,ncol,nrow); occ0=occ+1;

for ns=1:ns2m  %% loop over scenarios/stations/layers
 % reset index for map time offset
 moffs=0;moffc=0;mofc=0;figc=[]; varshortm0='';varshortmc0=''; imc=1;
 %% read model output
 tag=cell2mat(tags((nsu-1)*ncs+ns))
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
% t0 = datenum('1962-03-01','yyyy-mm-dd')-1;
%t0 = datenum('2001-03-01','yyyy-mm-dd')-1;
t1 = datenum('2002-04-30','yyyy-mm-dd')-1;
%t0 = datenum('2010-03-01','yyyy-mm-dd')-1
%t1 = datenum('2008-11-01','yyyy-mm-dd')-1;

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
     if ptag(2)=='a'
      plot_stat_map
     else
      plotgen_maps
     end
    else
      plotgen_body
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
for np=1:nfig+nfigm+nfigc
  if(np<=nfig+nfigm)
    figure(np);  
  else
    figure(figc(np-nfig-nfigm));
  end  
  set(gcf,'PaperPositionMode','auto', 'InvertHardCopy', 'off','Visible',vis);%,'Visible','off','Position',1*[0 0 1180 650]
%% add site name to each figure/page
  if(np<=nfig)
    li=ceil(np/nfig0);
    fprintf('np %d/%d=%d\n', np,nfig0,li); %floor ?
    annotation('textbox',[0.45 0.95 0.2 0.045],'String',locs{max(1,li)},'Color','k','Fontweight','bold','FontSize',fs+2,'LineStyle','none');
%% create base file name
    fnam0=sprintf('%d_%s%s',np,locs{max(1,li)},tagnam);
  else if(np<=nfig+nfigm) 
     fnam0=sprintf('%d_map_%s',np,vt{min(np-nfig,length(vt))});
      else
     fnam0=sprintf('%d_mapc%s%s',np,cell2mat(tags(ns1)),cell2mat(tags(ns2)));    
    end 
  end
%  fnam=fullfile(figdir,[fnam0 '.eps']);
%  fprintf('save EPS in %s ...\n',fnam);
%  print(gcf,'-depsc',fnam);
  fnam=fullfile(figdir,[fnam0 '.png']);
  fprintf('save PNG in %s ...\n',fnam);
%  export_fig(fnam,'-eps','-r600');
  export_fig(fnam,'-png'); %,'PaperUnits','cm','PaperSize',[30,40],'PaperPosition',[0 0 30 40],'-r300'
%  print(gcf,'-dpng',fnam);
end
end %nsu
