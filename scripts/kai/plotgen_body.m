
% index position of sub-plot
iy=cell2mat(var{i}(7)); ix=cell2mat(var{i}(8));
%ytl=[1E-5 1E-4 1E-3 0.01 0.1 0.3 1 3 10 20 50 100 1E3 1E4 1E5 1E6];
ytl=[0.1 0.3 1 3 10 20 50 100 300];
% geometry of sub-plot
x0=0.1+(ix-1)*1.1*dxp; y0=0.2-(nrow-1)*0.07+(nrow-iy)*1.1*dyp;
z0=min(2,size(tmp,3));
% loop over sites (eg from 3D output)
for ili=1:size(i_loc,1)
  if show_clim>0, clim_doy(i,1:12,1:2)=zeros(12,2); end
  if size(loc,1) == 0
    res = squeeze(tmp)*cell2mat(var{i}(5));
  else
    res = squeeze(tmp(i_loc(ili,1),i_loc(ili,2),:,:))*cell2mat(var{i}(5));
    if neighn(ili)>0
%     if exist('resmini') clear resmini resmaxi; end
     resmini=[];resmaxi=[];
     for ni=1:neighn(ili)
       tx=i_loc(ili,1)+neighv(ili,ni,1);
       ty=i_loc(ili,2)+neighv(ili,ni,2);
       if(ndims(tmp)>3)
        resmini(ni,:) = min(squeeze(tmp(tx,ty,z0:end,ind)));
        resmaxi(ni,:) = max(squeeze(tmp(tx,ty,z0:end,ind)));
       else
        resmini(ni,:) = min(squeeze(tmp(tx,ty,ind)));
        resmaxi(ni,:) = max(squeeze(tmp(tx,ty,ind)));
       end
     end
     resmin=min(resmini)'*cell2mat(var{i}(5));
     resmax=max(resmaxi)'*cell2mat(var{i}(5));
    end
  end

% goes to new figure if required
  np=cell2mat(var{i}(6)) + nfig0*(ili-1);
  figure(np);
  set(gcf,'visible',vis);
  oldfig=np; hold on
  axs=subplot('Position',[x0 y0 dxp dyp]);
  hold on
  tpos=[x0+0.223*(min(occ(np,ix,iy),4)+0.11)*dxp y0+0.85*dyp 0.3*dxp 0.11*dyp];

%fprintf('first %s\t np=%d c=%d ili=%d i=%d\t occ=%d\n',varshort,np,1+occ(np,ix,iy)-occ0(np,ix,iy),ili,i,occ(np,ix,iy));


%% process min-max value
  minval = cell2mat(var{i}(3)); maxVal = cell2mat(var{i}(4));
  if maxVal<-1, maxVal=1.05*max(max(res)); end
  if minval>0 & maxVal/minval > 20,  set(gca,'YScale','Log','YTick',ytl,'YTicklabel',ytl);

 end

  if(ptag(1)=='P')
    set(axs,'FontSize',fs,'Xlim',[minval maxVal],'box','on');
    xlabel(units);
  elseif(ptag(1)=='V')
    set(axs,'FontSize',fs,'box','on');%,'Xlim',[minval maxVal]
    if occ(np,ix,iy) ==0, % first action in subplot
     if(iy<nrow) set(axs,'XTickLabel',[]);
     else
       xlabel('Wave number (1/km)');
     end
    end
  else
    set(axs,'FontSize',fs,'Xlim',[t0 t1],'box','on');%,'Fontweight','bold'
    if occ(np,ix,iy) ==0, % first action in subplot
     dtim=t1-t0;
     if (dtim>60)
       nm=ceil(dtim/365);
       xt=round( nm*(0:round(dtim/30.25))*30.25);
       set(axs,'XTick',xt+t0);
       if (dtim<9*360) datetick(axs,'x','mmm','keepticks','keeplimits');
       else set(axs,'XTickLabel',[]);  end
       if (dtim>360 & iy==nrow)
        if (dtim>9*360) dya=0.01;  set(axs,'XTick',t0+(1:nm-1)*365.25);
        else dya=0.0; end
        for yi=1:length(years)
          xy=0.1+(mean(time(find(year==years(yi))))-t0)/dtim;
          annotation('textbox',[x0+(xy-0.13)*dxp 0.0+dya 0.2*dxp 0.2*dyp],'String',num2str(years(yi)),'Color','k','Fontweight','normal','FontSize',fs+2,'LineStyle','none');
        end
       end
     end %if dtim
     if(iy<nrow) set(axs,'XTickLabel',[]); end
   end %if occ
 end


% plot data
%fprintf('%d %d data: %d\t%c\n',i,ili,show_dati(ili),cell2mat(var{i}(9)));
 if (show_dati(ili)>0 & ns==1)%& (cell2mat(var{i}(9))=='L' | cell2mat(var{i}(9))=='M'))
  id=show_dati(ili); iv=1;
%  fprintf('datashow: %d \n',size(vars,2));
%%  col=colj(1+occ(np,ix,iy)-occ0(np,ix,iy)*(ns-1),:);
  col=ones(3,1)*0.3;
  while length(vars{id,iv})>0
%   fprintf('%s:%s\n',varshort0,vars{id,iv});
   if strcmpi(vars{id,iv},varshort0)
     dval = squeeze(data{id,iv})*cell2mat(var{i}(5));
     indd  = find(~isnan(dval));
     if length(indd)>0
       x1=datime(indd);x2=log10(minval+dval(indd));regression
       fprintf('%s:%s\t std=%1.3f trend=%1.3f\t',varshort0,locs{ili},std(x2),c*(x1(end)-x1(1)));
       fprintf('yreg= %1.3f + %1.5f*x\n',mean(x2)-c*mean(x1),c);
       if show_clim>0  % calc climatology
         td=2;
         day1=dadoy(indd);
         make_climatology;
       end
       plot(datime(indd),dval(indd),'+','Color',col,'MarkerFaceColor',col,'MarkerSize',11-(ili==1)*5,'LineWidth',1);
       break;
     end
   end
   iv=iv+1;
  end %while
  if strcmpi('chl',varshort0)  & 1
    indd  = find(~isnan(timser(:,ili)) & timea>= t0 & timea<=t1);
    if length(indd)>0
      dval = timser(indd,ili)*cell2mat(var{i}(5));
%      fprintf('%s: %d\t%1.1f %1.1f \n',varshort0,length(indd),datime(indd(1)),datime(indd(end)));
      plot(timea(indd),dval,'o','Color',col,'MarkerFaceColor','none','MarkerSize',3,'LineWidth',1);
    end
   end

 end % if show

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
  if occ(np,ix,iy) ==0,set(axs,'ylim',[minval maxVal]);ylabel(units); grid off; end
%  set(cb,'position',[x0cb y0cb 0.015 dyp*0.8],'YAxisLocation','right');
  if (occ0(np,ix,iy)==1 & ns>1) occ0(np,ix,iy)=occ(np,ix,iy); end  %
  ci = 1+mod(occ(np,ix,iy),occ0(np,ix,iy)+(ns==1)*99);
  if (occ0(np,ix,iy)==2 & occ(np,ix,iy)==3 & ns==4) ci=1; end
%  fprintf('%d %d\tns=%d oc=%d %d  ci=%d\n',ix,iy,ns,occ(np,ix,iy),occ0(np,ix,iy),ci);
%  col=colj(1+occ(np,ix,iy)-(occ0(np,ix,iy)-0)*(ns-1),:);
%  col=colj(ci,:);
  col=coljj(ns*2-2+ci,:);
  for li=2:length(ptag)  % loop over given depths
     if isstrprop(ptag(li), 'xdigit')
       zi=1+str2num(ptag(li));  % depth index from tag list
       % rescale depth index for more than 10 layers/
       if(size(res,1)>10) zi=1+round((zi-1)/9*(size(res,1)-1)); end
       y=res(zi,:);
       if size(neigh,1)>0 & 0
 %%        Xt=[time,fliplr(time)];                %#create continuous x value array for plotting
 %%        Ym=[resmin-0.5,fliplr(resmax)+2];              %#create y values for out and then back
 %%        fill(Xt,Ym,'w','Color',ones(3,1)*0.9);
         iu=find(resmin>0);
         hp = patch([time(iu); fliplr(time(iu)')';], [resmin(iu); fliplr(resmax(iu)')';], 'r');
         set(hp, 'facecolor',ones(3,1)*0.8, 'edgecolor', 'none'); drawnow
 %%         yvm=0.5*[resmin+resmax]; yve=0.5*[-resmin+resmax];
 %%         hp=errorbar(time(it),yvm,yve,'Color',coljj(li-1,:));
 %%         hp.CapSize = 2;  %  needs matlab2016b

       end
       if(ntags>1 & 1) plot(time(it),y(ind(it)),'o','Color',coljj(li-1,:),'MarkerFaceColor',coljj(li-1,:),'MarkerSize',2+ceil(2*mod(ns+1,4))); end
%%       annotation('textbox',tpos+[0.05*(li-1)*dxp -0.14*dyp 0 0],'String',[num2str(zi) '/' ptag(li)],'Color',coljj(li-1,:),'Fontweight','bold','FontSize',fs-2,'LineStyle','none');
     else
       if(dim==3 && isempty(findstr(varn,'flux')) )
% y=mean(res,2);
         if(Zt(i)==2)
          dz = soil_dz; dzt=soil_dzt;
         else
          if dimdz<3
             dz = water_dz; dzt=water_dzt;
          else
             dz = squeeze(water_dz(i_loc(ili,1),i_loc(ili,2),:,:));
             dzt = squeeze(water_dzt(i_loc(ili,1),i_loc(ili,2),:))';
          end
         end
         y = squeeze(sum(res.*dz,1)./dzt); %sum(res(:,ii)'*dz,1)
       else
         y = res;
       end %dim==3
     end %xdigit
     patchline(time,y(ind),'edgealpha',0.6,'edgecolor',col,'linewidth',linw(ns));

     x1=time;x2=log10(minval+y(ind));regression
     fprintf('model\t\t std=%1.3f trend=%1.3f\t',std(x2),c*(x1(end)-x1(1)));
     fprintf('yreg= %1.3f + %1.5f*x\n',mean(x2)-c*mean(x1),c);
     if show_clim>0 % calc climatology
       td=1;
       day1=doy;
       make_climatology;
       plot_clim
     end
%     plot(time,y(ind),lins(ns,:),'Color' ,col,'LineWidth',linw(ns));
     if ntags>1
       sp=strfind(tag,'y');
       if sp>0, tag1=tag(sp+1:end);
       else tag1=tag; end
     %  plot(time(it),y(ind(it)),'o','Color',coljj(ns*2-1,:),'MarkerFaceColor',coljj(ns*2-1,:),'MarkerSize',8);
       annotation('textbox',[x0+0.9*dxp y0+(0.85-ns*0.15)*dyp 0.3*dxp 0.11*dyp],'String',tag1,'Color',coljj(ns*2-1,:),'Fontweight','bold','FontSize',fs+2,'LineStyle','none','Interpreter','none');
     end
%%     set(gca,'Xlim',[t0 t1]);
%     fprintf('%d %d\t%s  %1.3f\n',ns,i,varn,mean(y));
  end
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
   end

 case{'V'}   %% variogram
  if ili==1 & length(size(tmp))>2 %only for the entire domain and for maps
 %  di = cell2mat(var{i}(5)); %depth index
   for li=2:length(ptag)  % loop over given times
     ti=it(1+str2num(ptag(li)));
     if strfind(tag,'P') ti=it(end-1)+1; fprintf('extra doy %d\n',doy(ti)); end

     dvariofile=[spath num2str(year(ti)) '_' num2str(doy(ti)) '_' varshort(find(~isspace(varshort))) '.mat'];
     fprintf('looking for variogram of data in %s ...\n',dvariofile);
     set(gca,'box','on','YScale','log','XScale','log','Xlim',[1/35 1/2],'XTick',[1/20 1/10 1/4 1/2],'XTickLabel',['1/20';'1/10';'1/4 ';'1/2 '],'Ylim',[10 2E5]);%,'Xlim',[ 40],'Xlim',[0 2.4],'YScale','Log','Ylim',[min(vario.val)*2 max(vario.val)*2]
     if exist(dvariofile)
        clear Pf
        load(dvariofile);   %dvar=dvario.mean^2;
%        plot(dvario.distance,dvario.val/dvar,'o','linewidth',2,'Color','k','MarkerSize',12-0.6*sqrt(length(dvario.val)));
%        set(gca,'Ylim',[0.2*max(dvario.val/dvar)-0.1 1.03*max(dvario.val/dvar)]);
        if exist('Pf')
          fprintf('frad:%1.2e %1.2e \tPf %1.2e %1.2e\n',frad(2),max(frad),min(Pf),max(Pf));

          plot(frad/1.6,Pf,'+-','linewidth',2,'Color','k','MarkerSize',12);%*2E-3
        end
     end;
     % extract data matrix
     if length(size(tmp))>3
       value = squeeze(tmp(:,:,end,ti))*cell2mat(var{i}(5));
     else
       value = squeeze(tmp(:,:,ti))*cell2mat(var{i}(5));  % surface maps
     end
     % adjust coordinate
     dx=size(lon,1)-lx0+1-size(value,1); if(dx<0) dx0=dx;dx=0; else dx0=0; end
     dy=size(lat,2)-ly0+1-size(value,2); if(dy<0) dy0=dy;dy=0; else dy0=0; end
     lo=lon(lx0+dx0:end-dx,ly0+dy0:end-dy);
     la=lat(lx0+dx0:end-dx,ly0+dy0:end-dy);
%     ig=find(~isnan(value) & (lo>5 | length(size(tmp))==4) & water_depth(1:size(value,1),1:size(value,2),end,ti)>15*5/30 );
%     value((lo<5 & length(size(tmp))==3) | water_depth(1:size(value,1),1:size(value,2),end,ti)<15*5/30 )=NaN;
     value(water_depth(1:size(value,1),1:size(value,2),end,ti)<12*5/30 )=NaN;
     value=log10(value);
     spectral
     plot(frad,Pf,'-','linewidth',3,'Color',coljj(ns*2-1,:));

%%   outfilename=['~/' varshort(find(~isspace(varshort))) tag '.mat'];
%%   save(outfilename,'lo','la','value')
%%      dat(dat>=maxVal)=maxVal-1E-3;
%     vario = variogram([lo(ig) la(ig)],value(ig),'plotit',false,'nrbins',50);
%     dvar2=mean(value(ig))^2;
%     plot(vario.distance,1.3*vario.val/dvar2,'-','linewidth',3,'Color',coljj(ns*2-1,:));
     sp=strfind(tag,'y');
     if sp>0, tag1=tag(sp+1:end);
     else tag1=tag; end
     sp=strfind(tag,'l');
     if sp>0, tag1=tag(sp+1:end);
     else tag1=tag; end

     annotation('textbox',[x0+0.88*dxp y0+(0.97-ns*0.08)*dyp 0.3*dxp 0.11*dyp],'String',tag1,'Color',coljj(ns*2-1,:),'Fontweight','bold','FontSize',fs+2,'LineStyle','none','Interpreter','none');
%        axis([0 params.maxdist 0 max(S.val)*1.1]);
     grid on;
%     text(2.,0.06,[num2str(doy(ti)) ' ' num2str(min(vario.val),'%1.2f') '-' num2str(max(vario.val),'%1.1f')],'fontweight','bold','fontsize',16);
    end
   end
 case{'S'}   %% variogram
%  cbins=minval:(maxVal-minval)/20:maxVal;
     y0=0.28; y1=0.52;
     set(gca,'box','on','Xlim',[0.5 ntags+0.5],'XTick',[],'Ylim',[y0 y1]);%,'Xlim',[ 40],'Xlim',[0 2.4],'YScale','Log','Ylim',[min(vario.val)*2 max(vario.val)*2]
   for li=2:length(ptag)  % loop over given times
     ti=it(1+str2num(ptag(li)));
     if strfind(tag,'P') ti=it(end-1)+1; fprintf('extra doy %d\n',doy(ti)); end
     % extract data matrix
     if length(size(tmp))>3
       value = squeeze(tmp(:,:,end,ti))*cell2mat(var{i}(5));
     else
       value = squeeze(tmp(:,:,ti))*cell2mat(var{i}(5));  % surface maps
     end
     value(water_depth(1:size(value,1),1:size(value,2),end,ti)<12*5/30 )=NaN;
     valm=mean(value(~isnan(value)));
     vals=std(value(~isnan(value)));

     h=bar(ns,vals/valm);
     set(h(1),'FaceColor',coljj(ns*2-1,:));

%%      dat(dat>=maxVal)=maxVal-1E-3;
%%     hd=histc(dat,cbins);
%%     stairs(cbins,1+hd,'linewidth',3,'Color',coljj(ns*2-1,:));
     sp=strfind(tag,'y');
     if sp>0, tag1=tag(sp+1:end);
     else tag1=tag; end
     sp=strfind(tag,'l');
     if sp>0, tag1=tag(sp+1:end);
     else tag1=tag; end

     text(ns-0.25,y0-(y1-y0)*0.05,tag1,'Color',coljj(ns*2-1,:),'Fontweight','bold','FontSize',fs+2);
     grid on;
   end

  end

if(cell2mat(var{i}(9)) ~='N'  )
 if (ns==1 & occ(np,ix,iy)<5)
   fac=abs(cell2mat(var{i}(5))-1);
   if(fac>0.1 & fac<1000) varshort=[varshort0 '*' num2str(cell2mat(var{i}(5)))]; end
   ii=ix*100+iy*10+occ(np,ix,iy);
%     col=colj(1+occ(np,ix,iy)-occ0(np,ix,iy),:);
%%   col=colj(1+1*mod(occ(np,ix,iy),occ0(np,ix,iy)+(ns==1)*99),:);  col=coljj(ns*2-2+ci,:);
%  col=colj(1+3*floor(occ(np,ix,iy)/(occ0(np,ix,iy)+1)),:);
%fprintf('%s\t np=%d occ=%d %d\t%d\n',varshort,np,occ(np,ix,iy),occ0(np,ix,iy),1+3*floor(occ(np,ix,iy)/(occ0(np,ix,iy)+1)));
%fprintf('%s\t tpy=%1.1f \t %d\n',varshort,tpos(2),ii);
   ta=strrep(varshort,'ZooC','Zooplankton');
%0.33*dxp*(ptag(1)=='V')
%%   th(ii)=annotation('textbox',tpos+[0.37*dxp 0.02 0 0],'String',[ta ],'Color',col,'Fontweight','bold','FontSize',fs+0,'LineStyle','none','FitHeightToText','off');%tag
%%   annotation('textbox',tpos-[0 0.14*dyp 0 0],'String',compn{Zt(i)},'Color',col,'Fontweight','bold','FontSize',fs-2,'LineStyle','none');
 end %if (ns==1 &
 occ(np,ix,iy) = occ(np,ix,iy) + 1;
end  %if 'N'
end  %li
