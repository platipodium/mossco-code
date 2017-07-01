% script for quick vizualisation of netcdf model results
%  takes no arguments
addpath('~/tools/mexcdf/mexnc'); addpath('~/tools/m_map');
clear all; close all
addpath('~/tools/export_fig');  % toolbox needed for correct png layout
show_data=1; Is1D=0; IsNOAH=1; tmp=version; VerMat=num2str(tmp(1:3)); 
datm='~/data/DeutscheBucht/esacci_2003_2013.mat';
%% settings
% locations; at least one site-name (locs) should be given 
%loc =[]; ];%];%
%%loc =[[54.18,7.86];[55.,8.3];[53.7,7.2];[52.3,4.3];[52.56,3.5];[53.42,5.15];[53.76,4.77]; [54.6,8.4];[54.0,8.7];[54.1,6.3];[54.2,7.5];[53.92,4.6];[53.9,2.9];];%;;[55.2,5.0];[55.0,8.0];];
%%locs={'Helgoland';'Sylt'; 'Norderney';'NOORDWK10';'NOORDWK70';'TERSLG4';  'TERSLG50';   'SAmrum';  'Norderelbe'; 'T22';    'T26';  'TERSLG70';'EastAngliaPlume';'T2' ;'T8';}; %
 loc =[[54.18,7.86];[53.989,6.237];	[53.987,6.870];	[54.070,8.019];	[54.173,7.962];	[54.092,7.357];	[54.439,7.425];	[54.468,6.193];	[55.038,6.403];	[54.830,5.575];	[55.257,4.746];	[55.502,4.168];	[54.685,6.737];	[54.688,7.510];	[54.194,7.234]];
 locs={'Helgoland';'NOAH-A-permeable';	'NOAH-B';	'NOAH-C-imperm';	'NOAH-CCPG';	'NOAH-D';	'NOAH-E';	'NOAH-F';	'NOAH-G';	'NOAH-H';	'NOAH-CCPJ';	'NOAH-I';	'NOAH-NSB3';	'NOAH-NSB2';	'NOAH-DB';};	
% %[54.96,8.4]; 
%  % 17 m 28 m% Noordwijk-10 Noordwijk-70
%tags ={'';'_half_sedimentation_tke2E-4';'_rFast0.05';'_vS_det18';'_sinking_factor0.32';'_sinking_factor_min0.03';'_a_water0.8'};
tags ={''};
for nsu=1:length(tags)
 tag=cell2mat(tags(nsu));
 files{1} = fullfile('~/sns/cut',['sns' tag '.nc']);
%% Output directory
dirname=['sns_Oflux02' tag]; if ~exist(dirname),  mkdir(dirname); end;
%files={'~/jureca/sns/cut/sns.nc'};%,
files={'~/sns/cut/sns.nc'};%,'~/sns_kai_z2.nc'_ref_20002011

%% Construtcs File and Path names
%vid_tshow=[0]; j=1; 
stoich=0;
if stoich==1,
  varna='N:P';  zlimit=[-1 20];  varid=0;
else
%  varna='Temp (^oC)';  zlimit=[-1 20];varid=7;
%  varna='N_2 flux (mmol/m2d)';  zlimit=[0 2.5];  varid=0;
%  varna='depth (m)';  zlimit=[2 52];  varid=0;
  varna='O_2 downward flux';varun='(mmol/m^2d)';  zlimit=[0 40];  varid=15;
end
dim3d=0; odu=1;odu_id=16;
%lx0=2;lx1=4;ly0=1;ly1=0;
lx0=1;lx1=0;ly0=1;ly1=0;

%% user settings on data to show
%% Plot settings
fs=14;  % font size
%gifobj = moviein(16); fprintf('storing video in %s ..\n',gifname);
i0=10;lins=['--';'- '];
colnj=jet(15);
colj=ones(256,3);colj(i0+1:256,:)=jet(256-i0);
collo=ones(3,1)*0.0;colhi=ones(3,1)*0.7;

read_mossco_nc; % from files{i},  returns ncid and variables lon, lat
read_noah;
latlimit =[51.0402   55.6386];
lonlimit =[0.0029    9.2283];

%% Init animation
fn=vars{varid+1};

%% Loop over time index
ti=1;o2flux=zeros(length(doy),ndn)-1;o2fl_min=zeros(13,1);
msi=o2fl_min; o2fl_max=o2fl_min;o2fl_mean=o2fl_min;
o2fl_var=o2fl_min;o2fl_min=o2fl_min+9E9;
m_proj('equidistant','lat',latlimit,'lon',lonlimit);
%m_gshhs_h('save','sns_coast');


%years(2)
it0=min(find(year==years(2)));it1=max(find(year==years(5)))-1;

dit=20;
for it=1:1:it1 %length(doy)
  %set(gcf,'position',[1 5 350 250],'Visible','off');
 %% extract data from matrix
  if(dim3d)
      value=double(squeeze(:,ivalue(:,:,1,it)));%,md(jm)
  else
      value=double(squeeze(ivalue(1:end,:,it)));%,md(jm)
  end
  value=value(lx0:end-lx1,ly0:end-ly1);
  if odu, oduval=oduvala(:,:,it); value=(oduval(lx0:end-lx1,ly0:end-ly1)-value)*24*3600;
%  else      value=-value*24*3600;
  end

 %%evaluate min-max for stations 
  for i=1:ndn
    o2flux(it,i)=value(nsgi(i,1),nsgi(i,2));
  end  
  mi=min(ceil(doy(it)/30.52),12);
  if it>=it0 & it<it1 & mod(it,dit)==1
 %% Open figure
   fig=figure(1);
   clf;set(fig,'Position',[1 1 500 440]);
   set(fig,'DoubleBuffer','on','Color','w');%
   subplot('Position',[0.03 0.03 0.92 0.92]); 
%        end 
   hold on;

   minv=min(min(min(min(value(value>-1)))));
   maxv=max(max(max(max(value(value>-1)))));
   fprintf('%d %s\t%d\t%1.5e %1.5e\n\n',it,vars{varid+1},minv,maxv);
   value(find(value<1E-4 | value>1E4 ))=-1;

   dx=size(lon,1)-lx0+1-size(value,1); if(dx<0) dx0=dx;dx=0; else dx0=0; end
   dy=size(lat,2)-ly0+1-size(value,2); if(dy<0) dy0=dy;dy=0; else dy0=0; end
   lo=lon(lx0+dx0:end-dx,ly0+dy0:end-dy);   la=lat(lx0+dx0:end-dx,ly0+dy0:end-dy);
   iym=size(value,2)-2;
   m_pcolor(lo(:,1:iym),la(:,1:iym),value(:,1:iym));

   colormap(colj(i0:end,:));
   set(gca,'Clim',zlimit);
   shading flat;
   m_grid('box','off','color','k','backcolor','none','tickdir','out','linestyle','none','xtick',[],'ytick',[],'xticklabel','','yticklabel',''); 
%  bats=bat(lx0:end-lx1,ly0:end-ly1);
%  dx=size(lon,1)-lx0+1-size(bats,1); if(dx<0) dx0=dx;dx=0; else dx0=0; end
%  dy=size(lat,2)-ly0+1-size(bats,2); if(dy<0) dy0=dy;dy=0; else dy0=0; end
%  [c,h]=m_contour(meshgrid(lon(lx0+dx0:end-dx,ly0+dy0:end-dy)),meshgrid(lat(lx0+dx0:end,ly0+dy0:end-dy)),bats,[20 50 100 200],['y:'],'linewidth',1);%clabel(c,h,'LabelSpacing',1440,'rotation',0);
  
   m_usercoast('sns_coast','color',ones(3,1)*0.5,'linewidth',1.0,'linestyle','-');
   cb=colorbar;
   title(cb,varun,'FontSize',fs,'Color','k');%,'FontWeight','bold'
   set(cb, 'Position', [0.82 0.02 .018 .39],'FontSize',fs,'Ytick',[0:10:30]);
   mons=datestr(doy(it));
   ta=sprintf('%s  %d',mons(4:6),year(it));
   m_text(lonlimit(1)-0.1,latlimit(2),varna,'HorizontalAlignment','left','FontSize',fs+6,'FontWeight','bold');
   m_text(lonlimit(2)-7,latlimit(1)+0.5,ta,'FontWeight','bold','HorizontalAlignment','right','FontSize',fs);
   set(gca,'FontSize',fs);
   cc='kw';
    for i=1:ndn
    for rad=8:2:16
      m_plot(lon(nsgi(i,1),nsgi(i,2)),lat(nsgi(i,1),nsgi(i,2)),'o','Color',cc(1+(rad>13 &rad<17)),'MarkerSize',rad,'Linewidth',2)
    end

%     m_text(lon(nsgi(i,1),nsgi(i,2)),lat(nsgi(i,1),nsgi(i,2)),'o','FontSize',15,'FontWeight','bold','HorizontalAlignment','center','Color','w');
   end
   set(gcf,'PaperPositionMode','auto');
   fname=sprintf('%s/%s_%d_%d.png',dirname,fn,mi,year(it));%vars{varid+1}md(jm)
   print(gcf,'-dpng','-r600',fname);% save old figure
  end

  o2fl_min(mi)=min(min(o2flux(it,:)),o2fl_min(mi));
  o2fl_max(mi)=max(max(o2flux(it,:)),o2fl_max(mi));
  o2fl_var(mi)=o2fl_var(mi) + o2flux(it,:)*o2flux(it,:)';
  o2fl_mean(mi)=o2fl_mean(mi) + mean(o2flux(it,:));
  msi(mi)=msi(mi)+1;
  ti=ti+1;
end
o2fl_mean=o2fl_mean./msi;
o2fl_var=  o2fl_var./(ndn*msi)- o2fl_mean.*o2fl_mean;
fig=figure(2);
clf;
set(fig,'Position',[1 1 620 540]);
subplot('Position',[0.12 0.1 0.87 0.89],'Box','on'); 
hold on;
axis([0.95 11.95 0 40]);
%        for i=hipor
%      plot(doy(ind)/30.5,1+0.95*o2flux(ind,i),lins(1+(y==year(it)),:),'Color',colhi,'LineWidth',1+2*(y==year(it)));%colj(i0+i*10-1,:)
ii=1:12;
if 0
yh=zeros(2,12);
yh(1,:) = o2fl_min(ii) ;
yh(2,:) = o2fl_max(ii) ;
pa=area(ii,yh');
set(pa(1),'FaceColor','w'); 
set(pa(2),'FaceColor',ones(3,1)*0.85); 
set(pa,'LineStyle','none')
plot(ii,o2fl_mean(ii),'-','Color',collo,'LineWidth',2)
else
yh=zeros(2,12);
yh(1,:) = o2fl_mean(ii)-sqrt(o2fl_var(ii));
yh(2,:) = o2fl_mean(ii)+sqrt(o2fl_var(ii))-yh(1,:)' ;
pa=area(ii,yh');
set(pa(1),'FaceColor','w'); 
set(pa(2),'FaceColor',ones(3,1)*0.75); 
set(pa,'LineStyle','none')
plot(ii,o2fl_mean(ii),'-','Color','w','LineWidth',4)
if nsu==1
  o2fl_ref = o2fl_mean(ii);
else
  plot(ii,o2fl_ref,'--','Color',collo,'LineWidth',2)
end

end

%text(1,32,tag,'FontSize',fs);
for i=1:ndn
   lg(i)=plot(noahdat(:,1),noahdat(:,1+i),'o','markersize',8,'MarkerFaceColor',colnj(i,:),'Color',colnj(i,:));
end %collo

%le=legend(lg,statlabel,'Location','NorthWest')
%set(le,'Box','off','FontSize',8);
if 0
for y=years(2):years(4)
  it=find(year==y);
  mi=min(ceil(doy(it)/30.52),12);
  for i=1:ndn
    plot(0.5+doy(it)/30.52,o2flux(it,i),'-','Color',colnj(i,:),'LineWidth',1);    
  end
end  
end

set(gca,'Xtick',[1:2:12],'XTicklabel',['Jan';'Mar';'May';'Jul';'Sep';'Nov'],'Box','on');
set(gca,'FontSize',fs,'Ytick',[0:10:40]);
ylabel('O_2 flux (mmol/m^2d)');
xt0=0.9;
%  text(xt0-0.4,62.,'Modular coastal model system (MOSSCO)','FontSize',fs+8,'FontWeight','bold')
%%
%  text(xt0,47,'Data:','FontSize',fs+2);
set(gcf,'PaperPositionMode','auto');
fname=sprintf('%s/o2flux.png',dirname);%vars{varid+1}md(jm)
print(gcf,'-dpng','-r600',fname);% save old figure

end

% convert   -delay 20   -loop 0   frame*.png   animated.gif
