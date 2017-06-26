% set data matrix
  mons=datestr(doy(ti-toffm));
  dm=3; % eliminate borders
  if length(size(tmp))>3
    value = squeeze(tmp(dm:end,dm:end-dm,di,ti)); 
 %% print variable & scen name & date
%    ta=sprintf('%s%d %d z%d',mons(4:6),year(ti-toffm),doy(ti-toffm),di);
    ta=sprintf('%s%d %d',mons(4:6),year(ti-toffm),doy(ti-toffm));
  else
    value = squeeze(tmp(:,:,ti))*di;  % surface maps
    ta=sprintf('%s%d %d',mons(4:6),year(ti-toffm),doy(ti-toffm));
  end

if(show_data)
  indn=find(~isnan(value));
  fprintf('%d %s/%s\t np=%d/%d mofc=%d im=%d/%d ixy=%d %d/%1.2f\tmean=%1.2f\t%d %d\n',i,varshort,varn,np,cell2mat(var{i}(6)),mofc,im,length(vli),ix,iy,y0,mean(mean(value(indn))),timeg(iig(im)),int32(time(mdi)));
end
%% process min-max value
  minval = cell2mat(var{i}(3)); maxVal = cell2mat(var{i}(4)); 
  if maxVal<-1, maxVal=1.05*max(max(value)); end
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
  fprintf('%d %s/%s\t np=%d\tmean=%1.2f\n',im,varshort,varn,np,mean(mean(value)));
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

  if(im==1) %2-IsNOAH
%% colorbar settings
    cb=colorbar;
    title(cb,units,'FontSize',fs-2,'FontWeight','bold','Color','k');
    set(cb, 'Position', [x0+0.03*dxpm y0+dypm*0.04 .012 0.3*dypm],'FontSize',fs);
    if(islog & VerMat<8.4)
%      ctl =ceil(log10(minval)):0.5:ceil(log10(maxVal));
       if(islog) set(cb,'YTick',log10(ytl),'YTicklabels',ytl); 
       else set(cb,'YTick',ytl,'YTicklabels',ytl); end
    end
  end
 
  if(ix==1) m_text(lonlimit(1)-0.5,latlimit(2)-0.12,[varshort0 ' ' tag],'HorizontalAlignment','left','FontSize',fs+4,'FontWeight','bold','FontName','Helvetica','Interpreter','none'); end
  set(gca,'FontSize',fs);
  mh=m_text(lonlimit(2)-1.,latlimit(1)+0.5,ta,'FontWeight','bold','HorizontalAlignment','right','FontSize',fs);
  uistack(mh,'top');

%% plot sites of interest
  for ili=1:size(loc,1)
%    m_plot(loc(ili,2),loc(ili,1),'o','markersize',9,'MarkerFaceColor','none','Color','w','Linewidth',2);normal
    m_plot(lon(i_loc(ili,1),i_loc(ili,2)),lat(i_loc(ili,1),i_loc(ili,2)),'o','Color','w','MarkerSize',6,'Linewidth',2)
%   m_text(lon(i_loc(ili,1),i_loc(ili,2)),lat(i_loc(ili,1),i_loc(ili,2)),'o','Color','w','HorizontalAlignment','center','FontWeight','bold','FontSize',fs+(nrowm==1)*4)
%[x,y] = m_ll2xy(varargin{1},varargin{2});s = size(varargin,1);h=plot(x,y,varargin{3:s});

%   bold m_text(loc(ili,2),loc(ili,1),'o','Color','w','HorizontalAlignment','center','FontWeight','bold','FontSize',fs)
  end
% ,'VerticalAlignment','center'annotation('textbox',tpos-[0 0.14*dyp 0 0],'String',compn{Zt(i)},'Color',col,'Fontweight','bold','FontSize',fs-2,'LineStyle','none');

