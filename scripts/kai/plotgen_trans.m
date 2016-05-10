ili=1;
for li=2:length(ptag)  % loop over time steps 0:first 9:last
 if isstrprop(ptag(li), 'xdigit') 
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

% goes to new figure (if required)
  np = ntags*(cell2mat(var{i}(6))-1)+ ns;
  figure(np); set(gcf, 'visible','off','Color','w'); hold on
%   set(fig,'DoubleBuffer','on','Color','w');%
% geometry of sub-plot
  x0=0.06+(ix-1)*1.15*dxp; y0=0.1+(nrow-iy)*1.03*dyp;
  axs=subplot('Position',[x0 y0 dxp dyp]);
  hold on

%res = squeeze(tmp)*cell2mat(var{i}(5)); 
  value = squeeze(tmp(:,:,:,ind(it(zi))));%*cell2mat(var{i}(5)); 

  depth = -water_d(:,it(zi))*(nz-1:-1:0)/nz;

  tpos=[x0+0.25*(0.15)*dxp y0+0.85*dyp 0.3*dxp 0.11*dyp];%occ(np,ix,iy)+

%% process min-max value
  minval = cell2mat(var{i}(3)); maxVal = cell2mat(var{i}(4)); 
  if maxVal<-1, maxVal=1.05*max(max(value)); end
  if minval>0 & maxVal/minval > 30, islog=1; else  islog=0; end
%if minval>0 & maxVal/minval > 30,set(gca,'YScale','Log','YTick',power(10,ceil(log10(minval)):ceil(log10(maxVal))));end
  if(islog) 
    value(find(value<minval*1.1))=minval*1.1;
    value=log10(value);
  end

  set(axs,'FontSize',fs,'box','on');%,,'Xlim',[t0 t1]'Fontweight','bold'
  if(iy<nrow) end

  if(ptag(1)=='T')
  % if(Zt(i)==2)    depth = -soil_depth(:,1);
 %% area settings
   if ix==1
     ylabel('m');
   else
     set(axs,'YTicklabel',[]);
   end
   if nrow==iy
     xlabel(txn);
   else
     set(axs,'XTickLabel',[]); 
   end
% annotation('textbox',[x0+0.9*dxp y0+(0.85-ns*0.15)*dyp 0.3*dxp 0.11*dyp],'String',tag,'Color',coljj(ns*2-1,:),'Fontweight','bold','FontSize',fs,'LineStyle','none');
  if maxVal>0,
   ii = find(value>maxVal);
   if ii, value(ii) = maxVal;end %value=valuehape(value,size(depth));
  end
  if(islog) 
    zlim(log10([minval maxVal]));
  else
    zlim([minval maxVal]);
  end
%% plot model data
  ix=find(tx>0);
  tt=repmat(tx(ix),nz,1)';
  h = pcolor(tt,depth(ix,:),value(ix,:));
  ylim([-Dmax*0.85 0]); xlim([min(tx(ix))+0.02  max(tx(ix))]);
%caxis([minval maxVal])
  set(h,'edgecolor','none');
%% find and print min+max
  col=[0.95 0.94 0.97];
 % annotation('textbox',tpos+[0.1*dxp -0.14*dyp 0 0],'String',[num2str(minval,5) '-' num2str(maxVal,5) units],'Color',col,'Fontweight','bold','FontSize',fs,'LineStyle','none');

  mons=datestr(doy(it(zi)));
  if strfind(tag,'_')
    tagc=tag(2:end);
  else
    tagc=tag;
  end
  ta=sprintf('%s (%d,%d) %s %d',[varshort0 ' ' tagc],doy(it(zi)),ind(it(zi)),mons(4:6),year(it(zi)));
  annotation('textbox',tpos+[0.34*dxp -0.99*dyp 0.02 0.1],'String',ta,'Color','k','Fontweight','bold','FontSize',fs,'LineStyle','none');

%% colorbar settings
%cb = colorbar;%set(cb,'LineWidth',1,'FontSize',fs-2);%,'Fontweight','bold'
%lh = ylabel(cb,units,'FontSize',fs-2);
  end
 end
end
%% colorbar settings
cb=colorbar;
title(cb,units,'FontSize',fs-2,'FontWeight','bold','Color','k');
set(cb, 'Position', [x0+0.18*dxp y0+dyp*0.02 .014 0.3*dyp],'FontSize',fs);
if(islog)
   ctl =([0.01 0.1 1 10 20 100 1E3]);
   set(cb,'YTick',log10(ctl),'YTicklabel',ctl); 
end
