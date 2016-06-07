ili=1; 
if isstrprop(ptag(2), 'xdigit') 
  vli=2:length(ptag); mode='s';
else
  vli=0:nrowm*ncolm-1; mode='v';
  moffs=moffs+1;
  if(~strcmp(varshort,varshortm0))
    moff=moffs;
  end
  varshortm0=varshort;
end

di = cell2mat(var{i}(5)); %depth index

for im=1:length(vli)
 if mode=='s' % loop over time steps 0:first 9:last
  li=vli(im);
  zi=1+str2num(ptag(li)); 
%% index position of sub-plot
  iy=cell2mat(var{i}(7)); 
% zero indicates running index for time slices
  if(iy==0) iy = mod(li-2,nrowm); end
  ix=cell2mat(var{i}(8)); 
  if(ix==0) 
    if(cell2mat(var{i}(7))==0)
      ix = floor((li-1)/nrowm);
    else
      ix = li-1;
    end
  end
  ti=it(zi);
 else
  ix = 1+mod(im-1,ncolm);
  iy = 1+floor((im-1)/ncolm);
  ti = toffm+((cell2mat(var{i}(6))-moff)*(nrowm*ncolm)+im)*2;     
 end
 if ti-toffm<=length(ind)
% goes to new figure (if required)
  np = ntags*(cell2mat(var{i}(6))-1)+ ns + nfig;
  if(im==1) vt{np-nfig}=[varshort0 tag]; end
  figure(np); set(gcf, 'visible','off','Color','w'); hold on
%   set(fig,'DoubleBuffer','on','Color','w');%
% geometry of sub-plot
  x0=0.06+(ix-1)*1.15*dxpm; y0=0.1+(nrowm-iy)*1.03*dypm;
  axs=subplot('Position',[x0 y0 dxpm dypm]);
  hold on

% set data matrix
  mons=datestr(doy(ti-toffm));
  if length(size(tmp))>3
    value = squeeze(tmp(:,:,di,ti)); 
 %% print variable & scen name & date
    ta=sprintf('%s%d %d z%1.0f',mons(4:6),year(ti-toffm),doy(ti-toffm),depth(di));
  else
    value = squeeze(tmp(:,:,ti))*di;  % surface maps
    ta=sprintf('%s%d %d',mons(4:6),year(ti-toffm),doy(ti-toffm));
  end  
  indn=find(~isnan(value));
  fprintf('%d %s/%s\t np=%d/%d im=%d i=%d %d\tmean=%1.2f\n',i,varshort,varn,np,cell2mat(var{i}(6)),im,ix,iy,mean(mean(value(indn))));

%% process min-max value
  minval = cell2mat(var{i}(3)); maxVal = cell2mat(var{i}(4)); 
  if maxVal<-1, maxVal=1.05*max(max(value)); end
  if minval>0 & maxVal/minval > 30, islog=1; else  islog=0; end

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
  colormap(ssec);  %% choose color map
 %        set(p,'MeshStyle','both','EdgeAlpha',0);
  shading flat;
  m_grid('box','off','color','k','backcolor','none','tickdir','out','linestyle','none','xtick',[],'ytick',[],'xticklabel','','yticklabel',''); 
  m_usercoast('sns_coast','color',ones(3,1)*0.5,'linewidth',1.0,'linestyle','-');

  if(im==1) %2-IsNOAH
%% colorbar settings
    cb=colorbar;
    title(cb,units,'FontSize',fs-2,'FontWeight','bold','Color','k');
    set(cb, 'Position', [x0+0.03*dxpm y0+dypm*0.06 .014 0.3*dypm],'FontSize',fs);
    if(islog)
      %ctl=power(10,ceil(log10(minval)):log10(5):ceil(log10(maxVal)))
      ctl =ceil(log10(minval)):0.5:ceil(log10(maxVal));
%      ctl =log10([1E-3 0.01 0.03 0.1 1 3 10 30 100 1E3]);
      set(cb,'YTick',ctl,'YTicklabel',power(10,ctl)); 
    end
  end
 
  m_text(lonlimit(1)-0.3,latlimit(2)-0.2,[varshort0 ' ' tag],'HorizontalAlignment','left','FontSize',fs+8,'FontWeight','bold','FontName','Helvetica','Interpreter','none');
  m_text(lonlimit(2)-1.2,latlimit(1)+0.5,ta,'FontWeight','bold','HorizontalAlignment','right','FontSize',fs);
  set(gca,'FontSize',fs);

%% plot sites of interest
  for ili=1:size(loc,1)
%    m_plot(loc(ili,2),loc(ili,1),'o','markersize',9,'MarkerFaceColor','none','Color','w','Linewidth',2);normal
    m_text(lon(i_loc(ili,1),i_loc(ili,2)),lat(i_loc(ili,1),i_loc(ili,2)),'o','Color','w','HorizontalAlignment','center','FontWeight','bold','FontSize',fs+(nrowm==1)*4)
%   bold m_text(loc(ili,2),loc(ili,1),'o','Color','w','HorizontalAlignment','center','FontWeight','bold','FontSize',fs)
  end
% ,'VerticalAlignment','center'annotation('textbox',tpos-[0 0.14*dyp 0 0],'String',compn{Zt(i)},'Color',col,'Fontweight','bold','FontSize',fs-2,'LineStyle','none');
 end % if
end  % if

