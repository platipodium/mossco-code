%%
% generic plot script for time variable mossco results
%
% kai wirtz Nov2015
%
clear all;close all;
addpath('~/tools/m_map');  % map-toolbox needed for 2D plots
show_data=1; Is1D=0; IsNOAH=0;
datf='~/data/DeutscheBucht/stations.nc';
%% settings
% locations; at least one site-name (locs) should be given 
%loc =[]; 
loc =[[54.18,7.86];[55.,8.3];[52.3 4.3];[52.56 3.5];]; %[54.1,6.3];;[54.2,7.5]; %[54.96,8.4]; 
%  % 17 m 28 m
% Noordwijk-10 Noordwijk-70
locs={'Helgoland';'Sylt';'Noordwijk-10';'Noordwijk-70';}; %'T22'; 'T26'; 
%'Helgoland'; 'Sylt';    'SAmrum';'Norderelbe';'Nordeney',
%  'T36';     'T26' ;    'T41';   'T8'  ;      'T2';
%  'T22';     'T5';      'T12';   'T11'
%[54.1,54.1];[55.0,8.4];[54.6,8.4];[54.0,8.7];[53.7,7.2];
%[53.7,6.4];[54.2,7.5];[54.0,8.1];[55.0,8.0];[55.2,5.0];
%[54.1,6.3];[55.0,6.3];[54.7,7.4];[54.7,6.9];
if IsNOAH
 loc =[[53.989,6.237];	[53.987,6.870];	[54.070,8.019];	[54.173,7.962];	[54.092,7.357];	[54.439,7.425];	[54.468,6.193];	[55.038,6.403];	[54.830,5.575];	[55.257,4.746];	[55.502,4.168];	[54.685,6.737];	[54.688,7.510];	[54.194,7.234]];
 locs={'NOAH-A-permeable';	'NOAH-B';	'NOAH-C-imperm';	'NOAH-CCPG';	'NOAH-D';	'NOAH-E';	'NOAH-F';	'NOAH-G';	'NOAH-H';	'NOAH-CCPJ';	'NOAH-I';	'NOAH-NSB3';	'NOAH-NSB2';	'NOAH-DB';};	
end
% load and prepare data
if show_data, read_stations_nc; 
else show_dati=zeros(size(loc,1)); end;
%tags={'_a';'_b'};%'_c';'_3';'_0';tags={'_4';};%'_2';'_3';
%tags={'';'_Zmort';'_n'};%;};%'_0';'_1';'exu';'Ndep';
ncol = 3; nrow = 2; 	% number of columns in fig
nrowm = 3; ncolm = 5;
if Is1D 
  locs={'Helgoland'};
  loc =[54.18,7.82];
  spath= '/local/home/wirtz/mossco/mossco-setups/helgoland/';%';hrres/
%  spath= '/local/home/wirtz/mossco/mossco-setups/hrres/'
%tags = {'ref';'phi_agg0.0004';'phi_agg0.002';};%'agg_doc0.01';'mort_zoo0.015';'PAds0';'rnit140';'Nqual0.';'vS_det15';};
%tags = {'ref';'mort_zoo0.015';'mort_zoo0.025';};%'agg_doc0.01';'PAds0';'rnit140';'Nqual0.';'vS_det15';};
%tags = {'ref';'vir_loss0.0';'vir_loss1.2';};%'rnit140';'Nqual0.';'vS_det15';};
 tags ={'';};% {'_0';'_1';};%'phi_agg5E-4';'agg_doc0.01';'vir_loss0.';};
%tags = {'ref';'Nqual0.';'rnit150';'PAds0';};%'sinking_factor_min0.5';'remNP0.';'remNP-0.2';'Nqual0';
%tags = {'ref';'agg_doc0.01';'agg_doc0.2';};
%tags = {'ref';'AffP0.05';'hydrol0.02';};
ntags=length(tags);
  ncf0 = 'mossco_1d'; % base file name of input netcdf
  setvar_1D  % defines variables to show - and where/how to do it 
 %% graph settings
  ncol = 4; nrow = 2; 	% number of columns in fig
else
%  loc =[54.18,7.82];
%'ref0';'PAds12';'PAdsODU40';bash-4.1$'_genMeth12';'_a_chl0.02';'_mort_zoo0.016';'_rFast0.08';
%'_sinking_factor_min0.3';'_vS_det16';
%%tags = {'';'_Zmorta';'_a_water1.3';'_Q101.8';};
%%tags = {'';'_vS_det16';'_PAdsODU220';'_syn_nut-4.6';};
%%tags = {'';'_rSlow0.005';'_genMeth6';'_mort_zoo0.024';};'_PAdsODU220';
tags = {'';};
 ntags=length(tags);
%  spath= '/home/wirtz/';%sns  
  spath  ='/data/wirtz/';%'/ocean-data/wirtz/';
%% ncfile = fullfile(spath,['sns' tag '/cut/sns' tag '.nc']);
  ncf0 = 'sns_HR'; 
  if IsNOAH
    setvar_o2flux  % defines variables to show - and where/how to do it %setvar  
    nrowm = 1; ncolm = 1;
  else
    setvar_sns  % defines variables to show - and where/how to do it %setvar  
  end
end

dxp = 0.82/(ncol+0.05); dyp = 0.83/(nrow +0.05);
dxpm = 0.86/( ncolm +0.05); dypm= 0.86/(nrowm+0.05);
compn ={'water';'soil'};
fs = 16; colp=prism(5);colj=colp([1 4:5 2:3],:); coljj=jet(10); colt='kw';
i0=10;coljm=ones(256,3); coljm(i0+1:256,:)=jet(256-i0);

linw=[3 2*ones(1,14)]; lins=['- '; repmat('- ',14,1);]; 

%% check for tag file
tagfile = fullfile(spath,['tag.lst']);
if exist(tagfile)
  tagd =importdata(tagfile,' '); tagn=length(tagd.textdata);
else
  tagn=0;
end
% load and prepare data
if show_data, read_stations_nc; end;

%% open all figures
for np=1:nfig+nfigm, figure(np); set(gcf,'Position',[0 0 1680 850],'Visible','off','Color','w'); end
oldfig=-np; 
ptag=cell2mat(var{1}(9));
occ = zeros(nfig,ncol,nrow); occ0=occ+1;
for ns=1:ntags %% loop over scenarios/stations/layers
 % reset index for map time offset
 moffs=0; varshortm0='';
 %% read model output
 tag=cell2mat(tags(ns));
 %ncfile = fullfile(spath,[ncf0 tag '.nc']);
 ncfile = fullfile(spath,[ncf0 tag '/cut/' ncf0 tag '.nc']);

 read_nc_time_layers
 t0=time(1); t1=time(end);
 %t0 = datenum('2003-03-20','yyyy-mm-dd')-1;
 %t1 = datenum('2005-06-01','yyyy-mm-dd')-1;

 ind=find(time>= t0 & time<=t1);
 toffm = min(find(time>= t0))-1;
 year=year(ind);time=time(ind); doy=doy(ind); years= unique(year);
 it=round(1+(0:9)*(length(time)-1)/9);% discrete index for plotting symbols

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

%% create directory (name) for utput
figdir = fullfile(spath,'plots');
if ~exist(figdir),  mkdir(figdir); end;

%% plot each figure as EPS & PNG
for np=1:nfig+nfigm
  figure(np);  
  set(gcf,'PaperPositionMode','auto', 'InvertHardCopy', 'off');%,'Visible','off'
%% add site name to each figure/page
  if(np<=nfig)
    li=floor(np/nfig0);
    annotation('textbox',[0.45 0.95 0.2 0.045],'String',locs{li},'Color','k','Fontweight','bold','FontSize',fs+2,'LineStyle','none');
%% create base file name
    fnam0=sprintf('%s%s%s_%d',locs{li},cell2mat(tags(1)),cell2mat(tags(end)),np);
  else
    fnam0=sprintf('map_%s_%d',vt{np-nfig},np);
  end
%  fnam=fullfile(figdir,[fnam0 '.eps']);
%  fprintf('save EPS in %s ...\n',fnam);
%  print(gcf,'-depsc',fnam);
  fnam=fullfile(figdir,[fnam0 '.png']);
  fprintf('save PNG in %s ...\n',fnam);
  print(gcf,'-dpng',fnam);
end


