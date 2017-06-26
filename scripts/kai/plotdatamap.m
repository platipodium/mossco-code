x0=0.06+(ix-1)*1.15*dxpm; y0=0.1+(nrowm-iy)*1.03*dypm;
 % cmp different scenarios
axs=subplot('Position',[x0 y0 dxpm dypm]);
hold on

%% prepare 2D plot 
m_proj('equidistant','lat',latlimit,'lon',lonlimit);


%% plot 2D data
% value(find(value<1E-4 | value>1E4 ))=-1;
value=squeeze(datat(ii(mdi),:,:));

% transformation to correct for bias of ESACCI against station CHL
% see kai/cmp_chl.m
value=0.5+power(10,-0.3 + 1.4*log10(value));

if(islog) 
value(find(value<minval*1.1))=minval*1.1;
value=log10(value);
end

m_pcolor(m_lon,m_lat,value');
% set(gca, 'Color', 'k')
if(islog) set(gca,'Clim',log10([minval maxVal]));
else set(gca,'Clim',[minval maxVal]); end
shading flat;
m_grid('box','off','color','k','backcolor','none','tickdir','out','linestyle','none','xtick',[],'ytick',[],'xticklabel','','yticklabel',''); 
m_usercoast('sns_coast','color',ones(3,1)*0.5,'linewidth',1.0,'linestyle','-');

ta=sprintf('    %d',doyg(ii(mdi)));%yearg(ii(mdi)),
m_text(lonlimit(2)-1.2,latlimit(1)+0.5,ta,'FontWeight','bold','HorizontalAlignment','right','FontSize',fs);

