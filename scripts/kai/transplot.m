%%
% generic plot script for time variable mossco results
%
% kai wirtz Nov2015
%
clear all;close all;
addpath('~/tools/m_map');  % map-toolbox needed for 2D plots
show_data=1; datf='~/data/DeutscheBucht/stations.nc';
%% settings
% locations; at least one site-name (locs) should be given 
loc =[]; 
%loc =[[54.18,7.82];[54.96,8.4];[54.1,6.3];[54.2,7.5];]; % 
locs={'T1';}; % 'T1'; 

tags={'_new';};%'_res';'_att';
ntags=length(tags);
spath  ='/home/wirtz/sns/';%spath  ='/ocean-data/wirtz/';
%spath  ='/media/archiv/'
setvar_trans;  % defines variables to show - and where/how to do it %setvar  
%setvar_1D  % defines variables to show - and where/how to do it 
%% graph settings
ncol = 3; nrow = 3; 	% number of columns in fig
dxp = 0.83/(ncol+0.05); dyp = 0.83/(nrow +0.05);
compn ={'water';'soil'};
fs = 16; colp=prism(5);colj=colp([1 4:5 2:3],:); coljj=jet(10); colt='kw';
i0=10;coljm=ones(256,3); coljm(i0+1:256,:)=jet(256-i0);

linw=[2 1*ones(1,14)]; lins=['- '; repmat('- ',14,1);]; 

%ncfile = fullfile(spath,['cut_29_' tag '.nc']);
%spath  ='/home/wirtz/mossco/mossco-setups/helgoland/';%

%% check for tag file
tagfile = fullfile(spath,['tag.lst']);
if exist(tagfile)
  tagd =importdata(tagfile,' '); tagn=length(tagd.textdata);
else
  tagn=0;
end

%% open all figures
for np=1:nfig, figure(np); set(gcf,'Position',[0 0 1540 950],'Visible','off','Color','w'); end
oldfig=-np;

occ = zeros(nfig,ncol,nrow); 

for ns=1:ntags
 %% loop over scenarios/stations/layers

 %% read model output
 tag=cell2mat(tags(ns));

% loop over transects (eg from 3D output)

% ncfile = fullfile(spath,['hr' tag '/mossco_1d.nc']);
%% ncfile = fullfile(spath,['sns' tag '/cut/sns' tag '.nc']);
 ncfile = fullfile(spath,['cut' locs{1} '.nc']);
% ncfile = fullfile(spath,['mossco_1d' tag '.nc']);

 read_nc_time_layers
 t0=time(1); t1=time(end);
 t0 = datenum('2003-07-10','yyyy-mm-dd')-1;
 t1 = datenum('2003-07-28','yyyy-mm-dd')-1;

 time0=time;
 ind=find(time>= t0 & time<=t1);
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
    if (ptag(1)=='T')
      plotgen_trans
    else
      plotgen_body
    end
 end  %nvar
% close input file
 netcdf.close(ncid);
%% add tag label from file
%name = sprintf('%s%s_%s_%s',cell2mat(vars(:,1)),cell2mat(varn{end)),run,datestr(t0,'yyyy'));
 for nt=1:tagn
   if(strfind(tag,cell2mat(tagd.textdata(nt,1)))>0)
     annotation('textbox',[ns*0.15-0.1 0.95 0.15 0.04],'String',[cell2mat(tagd.textdata(nt,2)) ':' num2str(tagd.data(nt))],'Color',coljj(ns*2-1,:),'Fontweight','bold','FontSize',fs,'LineStyle','none');
   end 
 end % tagn
end %ns scneario tags

%% create directory (name) for utput
figdir = fullfile(spath,'plots');
if ~exist(figdir),  mkdir(figdir); end;

%% plot each figure as EPS & PNG
for np=1:nfig
  figure(np);  
  set(gcf,'PaperPositionMode','auto', 'InvertHardCopy', 'off','Visible','off');
%% add site name to each figure/page
  li=ceil(np/nfig0);
  annotation('textbox',[0.45 0.95 0.2 0.045],'String',locs{li},'Color','k','Fontweight','bold','FontSize',fs+2,'LineStyle','none');
%% create base file name
  fnam0=sprintf('T%s_%s%s_%d',locs{li},cell2mat(tags(1)),cell2mat(tags(end)),np);

%  fnam=fullfile(figdir,[fnam0 '.eps']);
%  fprintf('save EPS in %s ...\n',fnam);
%  print(gcf,'-depsc',fnam);
  fnam=fullfile(figdir,[fnam0 '.png']);
  fprintf('save PNG in %s ...\n',fnam);
  print(gcf,'-dpng',fnam);
end


