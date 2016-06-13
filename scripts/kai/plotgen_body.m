
% index position of sub-plot
iy=cell2mat(var{i}(7)); ix=cell2mat(var{i}(8));  
ytl=[1E-5 1E-4 1E-3 0.01 0.1 0.3 1 3 10 20 50 100 1E3 1E4 1E5 1E6];  
% geometry of sub-plot
x0=0.05+(ix-1)*1.2*dxp; y0=0.1+(nrow-iy)*1.07*dyp;

% loop over sites (eg from 3D output)
for ili=1:size(i_loc,1)
  if size(loc,1) == 0 
    res = squeeze(tmp)*cell2mat(var{i}(5)); 
  else
    res = squeeze(tmp(i_loc(ili,1),i_loc(ili,2),:,:))*cell2mat(var{i}(5)); 
  end

% goes to new figure if required
  np=cell2mat(var{i}(6)) + nfig0*(ili-1);
  figure(np); oldfig=np; hold on
  axs=subplot('Position',[x0 y0 dxp dyp]);
  hold on
  tpos=[x0+0.223*(min(occ(np,ix,iy),4)+0.11)*dxp y0+0.85*dyp 0.3*dxp 0.11*dyp];

%fprintf('first %s\t np=%d c=%d ili=%d i=%d\t occ=%d\n',varshort,np,1+occ(np,ix,iy)-occ0(np,ix,iy),ili,i,occ(np,ix,iy));


%% process min-max value
  minval = cell2mat(var{i}(3)); maxVal = cell2mat(var{i}(4)); 
  if maxVal<-1, maxVal=1.05*max(max(res)); end
  if minval>0 & maxVal/minval > 20,  set(gca,'YScale','Log','YTick',ytl,'YTicklabel',ytl);  end

  if(ptag(1)=='P')
    set(axs,'FontSize',fs,'Xlim',[minval maxVal],'box','on');
    xlabel(units);
 else
    set(axs,'FontSize',fs,'Xlim',[t0 t1],'box','on');%,'Fontweight','bold'
    if occ(np,ix,iy) ==0, % first action in subplot
     dtim=t1-t0;
     if (dtim>60)
       nm=ceil(dtim/365);
       xt=round( nm*(0:round(dtim/30.25))*30.25);
       set(axs,'XTick',xt+t0);
       datetick(axs,'x','mmm','keepticks','keeplimits');
       if (dtim>360 & iy==nrow)
        for yi=1:length(years)
          xy=(mean(time(find(year==years(yi))))-t0)/dtim;
          annotation('textbox',tpos+[xy*dxp -1.09*dyp 0 0],'String',num2str(years(yi)),'Color','k','Fontweight','normal','FontSize',fs-2,'LineStyle','none');
        end
       end 
     end %if dtim
     if(iy<nrow) set(axs,'XTickLabel',[]); end
   end %if occ
 end

 if(ptag(1)=='C' | ptag(1)=='P')
     if(Zt(i)==2)
       depth = -soil_depth(:,1);
     else
       depth = water_depth(:,1);
     end
  %% area settings
     ylabel('m');
     yy=[mean(depth(end,:)) mean(depth(1))];
     set(axs,'ylim',[min(yy) max(yy)]);
 end

 switch(ptag(1))  % plot type

 case{'L'} %% single lines
  if occ(np,ix,iy) ==0,set(axs,'ylim',[minval maxVal]);ylabel(units); grid on; end
%  set(cb,'position',[x0cb y0cb 0.015 dyp*0.8],'YAxisLocation','right');
  if (occ0(np,ix,iy)==1 & ns>1) occ0(np,ix,iy)=occ(np,ix,iy); end  %
  ci = 1+mod(occ(np,ix,iy),occ0(np,ix,iy)+(ns==1)*99);
  if (occ0(np,ix,iy)==2 & occ(np,ix,iy)==3 & ns==4) ci=1; end
  fprintf('%d %d\tns=%d oc=%d %d  ci=%d\n',ix,iy,ns,occ(np,ix,iy),occ0(np,ix,iy),ci);
%  col=colj(1+occ(np,ix,iy)-(occ0(np,ix,iy)-0)*(ns-1),:); 
  col=colj(ci,:); 
  for li=2:length(ptag)  % loop over given depths
     if isstrprop(ptag(li), 'xdigit') 
       zi=1+str2num(ptag(li));  % depth index from tag list
       % rescale depth index for more than 10 layers
       if(size(res,1)>10) zi=1+round((zi-1)/9*(size(res,1)-1)); end
       y=res(zi,:);
       plot(time(it),y(ind(it)),'o','Color',coljj(li-1,:),'MarkerFaceColor',coljj(li-1,:),'MarkerSize',4+2*mod(ns+1,4));
       annotation('textbox',tpos+[0.05*(li-1)*dxp -0.14*dyp 0 0],'String',[num2str(zi) '/' ptag(li)],'Color',coljj(li-1,:),'Fontweight','bold','FontSize',fs-2,'LineStyle','none');
     else
       if(dim==3)
% y=mean(res,2);
         if(Zt(i)==2)
          dz = soil_dz; dzt=soil_dzt;
         else
          dz = water_dz;dzt=water_dzt;
         end
         y = squeeze(sum(res.*dz,1)./dzt); %sum(res(:,ii)'*dz,1)
       else
         y = res;
       end %dim==3
     end %xdigit
     plot(time,y(ind),lins(ns,:),'Color' ,col,'LineWidth',linw(ns)); 
     if ntags>0
       plot(time(it),y(ind(it)),'o','Color',coljj(ns*2-1,:),'MarkerFaceColor',coljj(ns*2-1,:),'MarkerSize',8);
       annotation('textbox',[x0+0.88*dxp y0+(0.85-ns*0.15)*dyp 0.3*dxp 0.11*dyp],'String',tag,'Color',coljj(ns*2-1,:),'Fontweight','bold','FontSize',fs+2,'LineStyle','none','Interpreter','none');
     end
%     fprintf('%d %d\t%s  %1.3f\n',ns,i,varn,mean(y));
  end
 case{'M'}  %% map in extra window
  if maxVal>0,
     ii = find(res>maxVal);
     if ii, res(ii) = maxVal;  end %res=reshape(res,size(depth));
     caxis([minval maxVal]);
  end
  %% plot model data
  [c,h] = contourf(time,depth,res(:,ind),20);
  %caxis([minval maxVal])
  set(h,'edgecolor','none');
  %% find and print min+max
  col=[0.95 0.94 0.97];
  annotation('textbox',tpos+[0.1*dxp -0.14*dyp 0 0],'String',[num2str(minval,5) '-' num2str(maxVal,5) units],'Color',col,'Fontweight','bold','FontSize',fs-2,'LineStyle','none');

  
 case{'C'}  %% time-depth contour plots
  if maxVal>0,
     ii = find(res>maxVal);
     if ii, res(ii) = maxVal;  end %res=reshape(res,size(depth));
     caxis([minval maxVal]);
  end
  %% plot model data
  [c,h] = contourf(time,depth,res(:,ind),20);
  %caxis([minval maxVal])
  set(h,'edgecolor','none');
  %% find and print min+max
  col=[0.95 0.94 0.97];
  annotation('textbox',tpos+[0.1*dxp -0.14*dyp 0 0],'String',[num2str(minval,5) '-' num2str(maxVal,5) units],'Color',col,'Fontweight','bold','FontSize',fs-2,'LineStyle','none');

  %% colorbar settings
%cb = colorbar;%set(cb,'LineWidth',1,'FontSize',fs-2);%,'Fontweight','bold'
%lh = ylabel(cb,units,'FontSize',fs-2);

 case{'P'}   %% profiles
    col=colj(1+occ(np,ix,iy)-occ0(np,ix,iy)*(ns-1),:); 
    for li=2:length(ptag)  % loop over given times
    ii=1+round((length(ind)-1)*str2num(ptag(li))/9);
     %% plot model data
     plot(res(:,ind(ii)),depth,lins(ns,:),'Color',coljj(li-1,:) ,'LineWidth',linw(ns));
     plot(res(:,ind(ii)),depth,'o','Color',col,'MarkerFaceColor',col);
     annotation('textbox',tpos+[0.09*(li-1)*dxp -0.14*dyp 0 0],'String',[num2str(time(ii)-t_offset)],'Color',coljj(li-1,:),'Fontweight','bold','FontSize',fs-2,'LineStyle','none');
  end
 end
%% plot variable name
%  col='k';

% plot data
%fprintf('%d %d data: %d\t%c\n',i,ili,show_dati(ili),cell2mat(var{i}(9)));

 if (show_dati(ili)>0 & ns==1)%& (cell2mat(var{i}(9))=='L' | cell2mat(var{i}(9))=='M'))
  id=show_dati(ili); iv=1;
%  fprintf('datashow: %d \n',size(vars,2));
  col=colj(1+occ(np,ix,iy)-occ0(np,ix,iy)*(ns-1),:); 
  while length(vars{id,iv})>0 
%   fprintf('%s:%s\n',varshort0,vars{id,iv});
   if strcmpi(vars{id,iv},varshort0)
     dval = data{id,iv}*cell2mat(var{i}(5));
     indd  = find(~isnan(dval));
     if length(indd)>0
 %      fprintf('%s: %d\t%1.1f %1.1f \n',varshort0,length(indd),datime(indd(1)),datime(indd(end)));
       plot(datime(indd),dval(indd),'+','Color',col,'MarkerSize',13,'LineWidth',2);
       break;
     end
   end
   iv=iv+1;
  end %while
 end % if show
 
if(cell2mat(var{i}(9)) ~='N'  )
 if (ns==1 & occ(np,ix,iy)<5)
   fac=abs(cell2mat(var{i}(5))-1);
   if(fac>0.1 & fac<1000) varshort=[varshort0 '*' num2str(cell2mat(var{i}(5)))]; end
   ii=ix*100+iy*10+occ(np,ix,iy);
%     col=colj(1+occ(np,ix,iy)-occ0(np,ix,iy),:); 
   col=colj(1+1*mod(occ(np,ix,iy),occ0(np,ix,iy)+(ns==1)*99),:);
%  col=colj(1+3*floor(occ(np,ix,iy)/(occ0(np,ix,iy)+1)),:); 
%fprintf('%s\t np=%d occ=%d %d\t%d\n',varshort,np,occ(np,ix,iy),occ0(np,ix,iy),1+3*floor(occ(np,ix,iy)/(occ0(np,ix,iy)+1)));
%fprintf('%s\t tpy=%1.1f \t %d\n',varshort,tpos(2),ii);
   th(ii)=annotation('textbox',tpos,'String',[varshort ],'Color',col,'Fontweight','bold','FontSize',fs+2,'LineStyle','none','FitHeightToText','off');%tag
   annotation('textbox',tpos-[0 0.14*dyp 0 0],'String',compn{Zt(i)},'Color',col,'Fontweight','bold','FontSize',fs-2,'LineStyle','none');
 end %if (ns==1 &
 occ(np,ix,iy) = occ(np,ix,iy) + 1;
end  %if 'N'
end  %li 

