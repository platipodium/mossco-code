% set data matrix
dm=4; % eliminate borders
statm={'mean','std-t','std-z'};

%ii=find(time>= datenum('2003-03-01','yyyy-mm-dd') & time<datenum('2004-03-01','yyyy-mm-dd') );
value = squeeze(tmp(dm:end,dm:end-dm,:,:)); 
sval=[];
sval(1,:,:)=mean(mean(value,4),3); % average TODO: layer depth
sval(2,:,:)=mean(std(value,0,4),3);  % time variation 
sval(3,:,:)=10*mean(std(value,0,3),4);  % vertical variation
 %% print variable & scen name & date
%    ta=sprintf('%s%d %d z%d',mons(4:6),year(ti-toffm),doy(ti-toffm),di);

% goes to new figure (if required)
%vt{np-nfig}=[varshort0 tag]; 
figure(200+ns*5+str2num(ptag(3))); set(gcf, 'visible','off','Color','w'); hold on

for(ix=1:3)
% geometry of sub-plot
 x0=0.04+(ix-1)*0.32; y0=0.1;
%%  else % cmp different scenarios
 axs=subplot('Position',[x0 y0 0.3 0.89]);

 value=squeeze(sval(ix,:,:));

%% process min-max value
  minval = cell2mat(var{i}(3)); maxVal = cell2mat(var{i}(4)); 
%  if maxVal<-1, maxVal=1.05*max(max(value)); end
  if minval>0 & maxVal/minval > 20, islog=1; else  islog=0; end

%% prepare 2D plot 
  m_proj('equidistant','lat',latlimit,'lon',lonlimit);

%% plot data
  dx=size(lon,1)-lx0+1-size(value,1); if(dx<0) dx0=dx;dx=0; else dx0=0; end
  dy=size(lat,2)-ly0+1-size(value,2); if(dy<0) dy0=dy;dy=0; else dy0=0; end
  lo=lon(lx0+dx0:end-dx,ly0+dy0:end-dy);
  la=lat(lx0+dx0:end-dx,ly0+dy0:end-dy);

%% plot 2D data
%   value(find(value<1E-4 | value>1E4 ))=-1;
  if(islog) 
    value(find(value<minval*1.1))=minval*1.1;
    value=log10(value);
  end
%  m_pcolor(lo(ind),la(ind),value(ind));
  m_pcolor(lo,la,value);
 %       set(gca, 'Color', 'k')
  if(islog) set(gca,'Clim',log10([minval maxVal]));
  else set(gca,'Clim',[minval maxVal]); end

%  colormap(coljm(i0:end,:));
%%  colormap(ssec);  %% choose color map
 %        set(p,'MeshStyle','both','EdgeAlpha',0);
  shading flat;
  m_grid('box','off','color','k','backcolor','none','tickdir','out','linestyle','none','xtick',[],'ytick',[],'xticklabel','','yticklabel','');
  m_usercoast('sns_coast','color',ones(3,1)*0.5,'linewidth',1.0,'linestyle','-');

  if(ix==1) %2-IsNOAH
%% colorbar settings
    cb=colorbar;
    title(cb,units,'FontSize',fs-4,'Color','k');%,'FontWeight','bold'
    set(cb, 'Position', [x0+0.1+0.03*dxpm y0+0.04+dypm*0.04 .015 0.6*dypm],'FontSize',fs-2);
    if(islog & VerMat<8.4)
%      ctl =ceil(log10(minval)):0.5:ceil(log10(maxVal));
       if(islog) set(cb,'YTick',log10(ytl),'YTicklabels',ytl); 
       else set(cb,'YTick',ytl,'YTicklabels',ytl); end
    end
  end
 
  if(ix==1) m_text(lonlimit(1)-0.5,latlimit(2)-0.12,[varshort0 ' ' tag],'HorizontalAlignment','left','FontSize',fs+4,'FontWeight','bold','FontName','Helvetica','Interpreter','none'); end
  set(gca,'FontSize',fs);
  ii=find(~isnan(value)); mval=mean(mean(value(ii)));
  mh=m_text(lonlimit(2)-0.84,latlimit(1)+0.5,[statm{ix} ':' num2str(mval,'%1.1f')],'FontWeight','bold','HorizontalAlignment','right','FontSize',fs);
  uistack(mh,'top');
end
 set(gcf,'PaperPositionMode','auto', 'InvertHardCopy', 'off','Visible',vis);
fnam0=sprintf('%d_stat_%s_%s',200+str2num(ptag(3))+ns*5,varshort0,cell2mat(tags(ns)));
fnam=fullfile(figdir,[fnam0 '.png']);
fprintf('save PNG in %s ...\n',fnam);
%  export_fig(fnam,'-eps','-r600');
export_fig(fnam,'-png'); %,'PaperUnits','cm','PaperSize',[30,40],'PaperPosition',[0 0 30 40],'-r300'


