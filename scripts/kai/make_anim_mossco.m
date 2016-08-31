% script for quick vizualisation of netcdf model results
%  takes no arguments
addpath('~/tools/mexcdf/mexnc'); addpath('~/tools/m_map');
clear all; close all
%% Output directory
dirname='sns'; if ~exist(dirname),  mkdir(dirname); end;
files={'~/sns/sns.nc'};%,'~/sns_kai_z2.nc'

%% Construtcs File and Path names
%vid_tshow=[0]; j=1; 
stoich=0;
if stoich==1,
  varna='N:P';  zlimit=[-1 20];  varid=0;
else
%  varna='Temp (^oC)';  zlimit=[-1 20];varid=7;
%  varna='N_2 flux (mmol/m2d)';  zlimit=[0 2.5];  varid=0;
%  varna='depth (m)';  zlimit=[2 52];  varid=0;
  varna='O_2 downward flux';varun='(mmol/m^2d)';  zlimit=[0 32.5];  varid=17;
end
dim3d=0; odu=1;odu_id=18;
lx0=2;lx1=4;ly0=1;ly1=0;
lx0=1;lx1=0;ly0=1;ly1=0;

%% user settings on data to show
%% Plot settings
fs=14;  % font size
%gifobj = moviein(16); fprintf('storing video in %s ..\n',gifname);
i0=10;lins=['--';'- '];colj=ones(256,3);
colj(i0+1:256,:)=jet(256-i0);collo=ones(3,1)*0.0;colhi=ones(3,1)*0.7;

read_mossco_nc; % from files{i},  returns ncid and variables lon, lat

read_noah;

%units{j}=netcdf.getAtt(ncid,varid,'units');

%% Init animation
fn=vars{varid+1};
aviname =['anim_' fn '.avi'];
fprintf('storing video in %s ..\n',aviname);
aviobj = avifile(aviname,'fps',0.4);

%% Loop over time index
ti=1;o2flux=zeros(length(doy),ndn)-1;

m_proj('equidistant','lat',latlimit,'lon',lonlimit);
%m_gshhs_h('save','sns_coast');

doy0=0; 
years(2)
it0=min(find(year==years(2)));
it1=max(find(year==years(3)))-1;

for it=1:it1 %length(doy)
%for it=1:1:20
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
%  bat2=bat(lx0-1:end,ly0:end-ly1-1);  value=double(bat2);

 %%evaluate min-max for stations 
  for i=1:ndn
    o2flux(it,i)=value(nsgi(i,1),nsgi(i,2));
  end  

%  if stoich==1,
%    phyc=double(squeeze(iphyc(:,:,1,it))); phyc=phyc(lx0:end-lx1,ly0:end-ly1)';
%    phyn=double(squeeze(iphyn(:,:,1,it))); phyn=phyn(lx0:end-lx1,ly0:end-ly1)'    value=phyn./(phyp + 0.001);  end
  if it>=it0 & it<it1
 %% Open figure
   fig=figure(ti);
   clf;set(fig,'Position',[1 1 1040 560]);
   set(fig,'DoubleBuffer','on','Color','w');%
   subplot('Position',[0.02 0.18 0.48 0.8]); 
%        end 
   hold on;

   minv=min(min(min(min(value(value>-1)))));
   maxv=max(max(max(max(value(value>-1)))));
   fprintf('%d %s\t%d\t%1.5e %1.5e\n\n',it,vars{varid+1},minv,maxv);
   value(find(value<1E-4 | value>1E4 ))=-1;
%value=value(3:end-2,:);
%value=[value(:,:)'; -ones(size(value,1),1)']';

%  ind=find(value>0.0);
%  if odu, value(ind)=0+value(ind)*1.;  end
   if odu, value=1+value*1.05;  end

   dx=size(lon,1)-lx0+1-size(value,1); if(dx<0) dx0=dx;dx=0; else dx0=0; end
   dy=size(lat,2)-ly0+1-size(value,2); if(dy<0) dy0=dy;dy=0; else dy0=0; end
   lo=lon(lx0+dx0:end-dx,ly0+dy0:end-dy);
   la=lat(lx0+dx0:end-dx,ly0+dy0:end-dy);
%  m_pcolor(lo(ind),la(ind),value(ind));
   m_pcolor(lo,la,value);
%%  fprintf('\nstation\tH:Jana.xls\tH:topo.nc\n\n');

   for i=hipor
%    m_text(lon(nsgi(i,1),nsgi(i,2)),lat(nsgi(i,1),nsgi(i,2)),['$\textcircled{' num2str(i) '}$'], 'Interpreter', 'latex','Color','w','FontSize',14,'FontWeight','bold');
    m_text(lon(nsgi(i,1),nsgi(i,2)),lat(nsgi(i,1),nsgi(i,2)),'o','FontSize',15,'FontWeight','bold','HorizontalAlignment','center','Color','w');
%%    fprintf('%d %s\t\t%1.2f \t%1.2f\n',i,statlabel{i},noah_depth(i),bat(nsgi(i,1),nsgi(i,2)));
   end
   for i=lopor
    m_text(lon(nsgi(i,1),nsgi(i,2)),lat(nsgi(i,1),nsgi(i,2)),'o','FontSize',15,'FontWeight','bold','HorizontalAlignment','center','Color','k');
%    o2flux(it,i)=value(nsgi(i,1),nsgi(i,2));
   end
   

  ind=find(doy<=doy(it)+7 & doy>doy0 & year<=year(it))';
  
  o2fl_min(it)=min(min(o2flux(ind,:)));
  o2fl_max(it)=max(max(o2flux(ind,:)));
  if(year(it+1)~=year(it))
     doy0=0;
  else
     doy0=doy(it);
  end
  
  colormap(colj(i0:end,:));
 %       set(gca, 'Color', 'k')
  set(gca,'Clim',zlimit);
 %        set(p,'MeshStyle','both','EdgeAlpha',0);
  shading flat;
  m_grid('box','off','color','k','backcolor','none','tickdir','out','linestyle','none','xtick',[],'ytick',[],'xticklabel','','yticklabel',''); 
%  bats=bat(lx0:end-lx1,ly0:end-ly1);
%  dx=size(lon,1)-lx0+1-size(bats,1); if(dx<0) dx0=dx;dx=0; else dx0=0; end
%  dy=size(lat,2)-ly0+1-size(bats,2); if(dy<0) dy0=dy;dy=0; else dy0=0; end

%  [c,h]=m_contour(meshgrid(lon(lx0+dx0:end-dx,ly0+dy0:end-dy)),meshgrid(lat(lx0+dx0:end,ly0+dy0:end-dy)),bats,[20 50 100 200],['y:'],'linewidth',1);%clabel(c,h,'LabelSpacing',1440,'rotation',0);
  
  m_usercoast('sns_coast','color',ones(3,1)*0.5,'linewidth',1.0,'linestyle','-');
  cb=colorbar;
  title(cb,varun,'FontSize',fs,'FontWeight','bold','Color','k');
  set(cb, 'Position', [0.4 0.05 .018 .45],'FontSize',fs,'Ytick',[0:10:30]);
  mons=datestr(doy(it));
  ta=sprintf('%s  %d',mons(4:6),year(it));
  m_text(lonlimit(1)-0.3,latlimit(2)-0.2,varna,'HorizontalAlignment','left','FontSize',fs+8,'FontWeight','bold');
  m_text(lonlimit(2)-4.2,latlimit(1)-0.5,ta,'FontWeight','bold','HorizontalAlignment','right','FontSize',fs);
  set(gca,'FontSize',fs);
if 1
  subplot('Position',[0.51 0.05 0.47 0.54]); 
   hold on;
  if 0
  ii=find(doy<=doy(it) & year==year(it));
  yh=zeros(2,length(ii));
  if length(ii)>1
   yh(1,:) = o2fl_min(ii) + 0*4*sin((doy(ii)-40)'/365*pi).^2;
   yh(2,:) = o2fl_max(ii) - 0*12*sin((doy(ii)-80)'/365*pi).^6+ 0E-5*doy(ii)'.^2;
   pa=area(doy(ii)/30.25,yh');
   set(pa(2),'FaceColor',ones(3,1)*0.35); 
   set(pa,'LineStyle','none')
  end
  else
   for y=year(it0):year(it)
    ind=find(year==y & (doy<=doy(it) | (y~=year(it))) ); 
%    for i=1:ndn
    for i=hipor
      plot(doy(ind)/30.5,1+0.95*o2flux(ind,i),lins(1+(y==year(it)),:),'Color',colhi,'LineWidth',1+2*(y==year(it)));%colj(i0+i*10-1,:)
     end
    for i=lopor
      plot(doy(ind)/30.5,1+0.95*o2flux(ind,i),lins(1+(y==year(it)),:),'Color',collo,'LineWidth',1+2*(y==year(it)));%colj(i0+i*10-1,:)
     end
    end
  end
  ind=find( (noahdat(:,1)-0)*30.25 < doy(it)  | (year(it0)~=year(it)) );
  for i=hipor
      plot(noahdat(ind,1),noahdat(ind,1+i),'o','markersize',8,'MarkerFaceColor',colhi,'Color',colhi);
  end
  for i=lopor
      plot(noahdat(ind,1),noahdat(ind,1+i),'o','markersize',8,'MarkerFaceColor',collo,'Color',collo);
  end

  axis([1 12 0 38.5]);
  hold on;
  
  set(gca,'Xtick',[1:2:12],'XTicklabel',['Jan';'Mar';'May';'Jul';'Sep';'Nov'],'Box','on');
  set(gca,'FontSize',fs,'Ytick',[0:10:30]);

  xt0=0.9;
  text(xt0-0.4,62.,'Modular coastal model system (MOSSCO)','FontSize',fs+8,'FontWeight','bold');
%  text(xt0,60.,' (MOSSCO)','FontSize',fs+6,'FontWeight','bold');
  text(xt0+0.6,57,'OMEXDIA, GETM, MAECS, CLM, ...','FontSize',fs);

%%
  text(xt0,47,'NOAH-Data:','FontSize',fs+2);
  text(xt0,44,'Friedrich^#, Naderipour^#, Neumann^+,v Beusekom^#, Janssen^+,Oehler^+','FontSize',fs);
  text(xt0+8.6,41.,'(^#HZG  ^+AWI)','FontSize',fs);
%%  text(9,37,'^*HZG   ^#AWI','FontSize',fs-4)
 end

  set(gcf,'PaperPositionMode','auto');
  fname=sprintf('%s/%s_%d.png',dirname,fn,it);%vars{varid+1}md(jm)
  print(gcf,'-dpng','-r600',fname);% save old figure
 %       end
  frame = getframe(fig);  %gifobj(nif) = frame; 
  aviobj = addframe(aviobj,frame);
  ti=ti+1;
 end

end %it

aviobj = close(aviobj);
fprintf('closing %s ..\n',aviname);

% convert   -delay 20   -loop 0   frame*.png   animated.gif
