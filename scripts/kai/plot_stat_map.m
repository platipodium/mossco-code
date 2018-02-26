% set data matrix

%%fprintf('in figure %d\n',get(gcf,'Number'));
dm=1; % eliminate borders
statm={'mean','std_t','std-z'}; timl={'all','Mar-May','Jun-Aug','Sep-Oct','Nov-Jan'};
ytl=[0.01 0.1 0.5 1 3 10 30 100 1E3 1E4 1E5 1E6];
cc='kw';
%ii=find(time>= datenum('2003-03-01','yyyy-mm-dd') & time<datenum('2004-03-01','yyyy-mm-dd') );
sval=[];
% layer index
di = cell2mat(var{i}(5));
si = cell2mat(var{i}(7));
if IsData
  day1=doyg;
end
if si>1
  mi=min(ceil(4*(day1+30.25)/365.25),4);
  ii=find(mi==1+mod(si-1,4));
else
  ii=1:length(day1);
end

if IsData

   value =datat(ii,:,:);
   %vex=1.2*power(value,3.12)./(8+power(value,3));
   %value=0.35*(1+power(10,vex));
   sval(1,:,:)=nanmean(value,1); % average TODO: layer depth
   length(isnan(value))
   sval(2,:,:)=squeeze(nanstd(value,1))./(squeeze(sval(1,:,:))+1E-3); % average TODO: layer depth
   length(isnan(sval(1,:,:)))
   length(isnan(sval(2,:,:)))

   lo=lon;
   la=lat;
else
% if ptag(2)=='A'
   value = squeeze(tmp(dm:end,dm:end-dm,di,ii));
%else
%   value = squeeze(tmp(dm:end,dm:end-dm,di,ind));
% end

   sval(1,:,:)=mean(value,3); % average TODO: layer depth
   sval(2,:,:)=squeeze(std(value,0,3))./(squeeze(sval(1,:,:))+1E-3);  % time variation
  dx=size(lon,1)-lx0+1-size(value,1); if(dx<0) dx0=dx;dx=0; else dx0=0; end
  dy=size(lat,2)-ly0+1-size(value,2); if(dy<0) dy0=dy;dy=0; else dy0=0; end
%fprintf('x: %d -- %d\t y: %d -- %d\tlx0=%d,dx0=%d,dx=%d\n',lx0+dx0,size(lon,1)-dx,ly0+dy0,size(lon,2)-dy,lx0,dx0,dx);
  lo=lon(lx0+dx0:end-dx,ly0+dy0:end-dy);
  la=lat(lx0+dx0:end-dx,ly0+dy0:end-dy);
end

% sval(3,:,:)=10*mean(std(value,0,3),4);  % vertical variation
 %% print variable & scen name & date
%    ta=sprintf('%s%d %d z%d',mons(4:6),year(ti-toffm),doy(ti-toffm),di);

% goes to new figure (if required)
%vt{np-nfig}=[varshort0 tag];
ncols=cell2mat(var{i}(8));
figure(300+ns*5+i+IsData*50);
set(gcf, 'visible',vis,'Color','w','Position',[0 0 150+ncols*350 470]); hold on

for(ix=1:ncols)
% geometry of sub-plot
 x0=0.04+(ix-1)/ncols; y0=0.1;
%%  else % cmp different scenarios
 axs=subplot('Position',[x0 y0 0.9/ncols 0.89]);

 value=squeeze(sval(ix,:,:));

%% process min-max value
 if ix==1
  minval = cell2mat(var{i}(3)); maxVal = cell2mat(var{i}(4));
 else
  minval = 0; maxVal = 1.5; units='\sigma';
 end
%  if maxVal<-1, maxVal=1.05*max(max(value)); end
  if minval>0 & maxVal/minval >= 10, islog=1; else  islog=0; end

%fprintf('%d %s/%s\t np=%d/%d\t%d\tminax=%1.2f %1.2f\n',i,varshort,varn,np,cell2mat(var{i}(6)),length(find(value<minval*1.1)),minval,maxVal);

%% prepare 2D plot
  m_proj('equidistant','lat',latlimit,'lon',lonlimit);
  hold on
%% plot data

%% plot 2D data
%   value(find(value<1E-4 | value>1E4 ))=-1;
  value(find(value<minval*1.11))=minval*1.11;
  if(islog)
    value=log10(value);
  end
  indm=find(lo>999 | la>999 );lo(indm)=NaN;la(indm)=NaN;value(indm)=NaN;
%  m_pcolor(lo(ind),la(ind),value(ind));
  m_pcolor(lo,la,value);
 %       set(axs, 'Color', 'k')
  if(islog) set(axs,'Clim',log10([minval maxVal]));
  else set(axs,'Clim',[minval maxVal]); end
  colormap(coljm);
  shading flat;

%%  colormap(ssec);  %% choose color map
 %        set(p,'MeshStyle','both','EdgeAlpha',0);
  for ili=1:size(loc,1)*(timp~=2)*(ncols==1)
%    m_plot(loc(ili,2),loc(ili,1),'o','markersize',9,'MarkerFaceColor','none','Color','w','Linewidth',2);normal
 %   m_plot(lon(i_loc(ili,1),i_loc(ili,2)),lat(i_loc(ili,1),i_loc(ili,2)),'o','Color','w','MarkerSize',6,'Linewidth',2)
    for rad=6:2:14
      m_plot(lon(i_loc(ili,1),i_loc(ili,2)),lat(i_loc(ili,1),i_loc(ili,2)),'o','Color',cc(1+(rad>11 &rad<15)),'MarkerSize',rad,'Linewidth',2)
    end
 %   m_text(lon(i_loc(ili,1),i_loc(ili,2)),lat(i_loc(ili,1),i_loc(ili,2)),abc(ili),'Color','w','HorizontalAlignment','center','FontSize',fs-2,'FontWeight','bold')
%
  end
  m_grid('box','off','color','k','backcolor','w','tickdir','out','linestyle','none','xtick',[],'ytick',[],'xticklabel','','yticklabel','');
  m_usercoast('sns_coast','color',ones(3,1)*0.5,'linewidth',1.0,'linestyle','-');

  if(ix>=1 & ~IsData) %2-IsNOAH
%% colorbar settings
    cb=colorbar;
    title(cb,units,'FontSize',fs+2,'Color','k','FontWeight','bold');%
    set(cb, 'Position', [x0+0.72 y0+0.08 .025 0.28],'FontSize',fs+2);
    if(str2num(VerMat)<8.4)
       labAtt='YTicklabels';
    else
       labAtt='Ticklabels';
    end
    if(islog )
%      ctl =ceil(log10(minval)):0.5:ceil(log10(maxVal));
       set(cb,'YTick',log10(ytl),labAtt,ytl);
%    else
%      set(cb,'YTick',ytl,labAtt,ytl);
    end
  end
  if si>=1   %[num2str(min(day1(ii))) '-' num2str(max(day1(ii))) ]
     m_text(lonlimit(1)-0.2,latlimit(2)-0.4,timl{si},'HorizontalAlignment','left','FontSize',fs+6,'FontWeight','bold','FontName','Helvetica','Interpreter','none');
  end
  if(ix==-1) m_text(lonlimit(1)-0.5,latlimit(2)-0.12,[varshort0 ' ' tag],'HorizontalAlignment','left','FontSize',fs+4,'FontWeight','bold','FontName','Helvetica','Interpreter','none'); end
  set(axs,'FontSize',fs);
  iin=find(~isnan(value)); mval=mean(mean(value(iin)));
  fprintf('%s mean=%1.3f\n',varshort0,mval);
%%  mh=m_text(lonlimit(2)-0.84,latlimit(1)+0.5,[statm{ix} ':' num2str(mval,'%1.1f')],'FontWeight','bold','HorizontalAlignment','right','FontSize',fs);
%%  uistack(mh,'top');
%% plot sites of interest
%pause
end
 set(gcf,'PaperPositionMode','auto', 'InvertHardCopy', 'off','Visible','on');
fnam0=sprintf('%d_stat_%s_%s_%d',300+i+ns*5+IsData*50,varshort0,cell2mat(tags(ns)),IsData);
fnam=fullfile(figdir,[fnam0 '.png']);
fprintf('save PNG in %s ...\n',fnam);
%  export_fig(fnam,'-eps','-r600');
pause(0.6)
%export_fig(fnam,'-png','-r600'); %,'PaperUnits','cm','PaperSize',[30,40],'PaperPosition',[0 0 30 40],'-r300'
%fnam=fullfile(figdir,[fnam0 '.pdf']);
print(gcf,'-dpng',fnam);
)
figure(np);
%%fprintf('out figure %d\n',get(gcf,'Number'));
