x0=0.06+(ix-1)*1.15*dxpm;
y0=0.1+iy*1.03*dypm;
%y0=0.1+(nrowm-iy)*1.03*dypm;
 % cmp different scenarios
gca=subplot('Position',[x0 y0 dxpm dypm]);
hold on

%% prepare 2D plot
m_proj('equidistant','lat',latlimit,'lon',lonlimit);

%% plot 2D data
% value(find(value<1E-4 | value>1E4 ))=-1;
value=squeeze(datat(ii(mdi),:,:));

% transformation to correct for bias of ESACCI against station CHL
% see kai/cmp_chl.m
%value=0.5+power(10,-0.3 + 1.4*log10(value));
%value=0.5+power(10,-0.2 + 1.6*log10(value));
if 0
lyfv= log10(value);
a1=0.2; b1e=0.4;
value=0.4+power(10,(-a1+lyfv)/b1e);
lyfv0=0.7;
indv=find(lyfv>=lyfv0);
b2e=1.6;
a2=ly0-b2e/b1e*(lyfv0-a1);
value(indv)=power(10,(-a2+lyfv(indv))./b2e);
else
%  value=power(10,0.11+0.84*log10(value));
%%vex=1.2*power(value,3.12)./(8+power(value,3));
%%value=0.35*(1+power(10,vex));
value(55,60:70)
% value=7*(value-1.2)./(sqrt(value)+0.5);
end

value(find(value<minval*1.1))=minval*1.1;
if(islog)
  value=log10(value);
end

m_pcolor(m_lon,m_lat,value);
% set(gca, 'Color', 'k')
if(islog) set(gca,'Clim',log10([minval maxVal]));
else set(gca,'Clim',[minval maxVal]); end
shading flat;
colormap(coljm);
m_grid('box','off','color','k','backcolor','none','tickdir','out','linestyle','none','xtick',[],'ytick',[],'xticklabel','','yticklabel','');
m_usercoast('sns_coast','color',ones(3,1)*0.5,'linewidth',1.0,'linestyle','-');
if ShowMore
ta=sprintf('    %d',doyg(ii(mdi)));%yearg(ii(mdi)),
m_text(lonlimit(2)-1.2,latlimit(1)+0.5,ta,'FontWeight','bold','HorizontalAlignment','right','FontSize',fs);
end
