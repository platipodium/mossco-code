%%
% generic plot script for time variable mossco results
%
% kai wirtz Nov2015
%
clear all;close all;
%% settings
% set(0, 'DefaulttextInterpreter', 'none');  % underscores ('_') can be used as separators
%setvar_HR  % defines variables to show - and where/how to do it %setvar  
setvar_1D  % defines variables to show - and where/how to do it 
%% graph settings
ncol = 1; nrow = 3; 	% number of columns in fig
ncol = 4; nrow = 2; 	% number of columns in fig
dxp = 0.83/(ncol+0.05); dyp = 0.83/(nrow+0.05);
compn ={'water';'soil'};
fs = 14; colp=prism(5);colj=colp([1 4:5 2:3],:); coljj=jet(10); colt='kw';
linw=[2 1*ones(1,14)]; lins=['- '; repmat('- ',14,1);];
%spath  ='/home/wirtz/cut_074';%spath  ='/data/wirtz/sns_0/cut_29';
%ncfile = fullfile(spath,['cut_29_' tag '.nc']);
spath  ='/local//home/wirtz/mossco/mossco-setups';%/helgoland
%spath  ='/home/wirtz';

%tags={'102';'104';'132';'134';'82';'84';}; %tags={'102';'132';'82';}; 
%tags={'_4';'_1';'_2';};%
tags={'ef';'_5';'_6';};
ntags=length(tags);
%% check for tag file
tagfile = fullfile(spath,['tag.lst']);
if exist(tagfile)
  tagd =importdata(tagfile,' '); tagn=length(tagd.textdata);
else
  tagn=0;
end

for ns=1:ntags
%% loop over scenarios/stations/layers
occ = zeros(ncol,nrow); 
%% read model output
tag=cell2mat(tags(ns));

ncfile = fullfile(spath,['hr' tag '/mossco_1d.nc']);
read_nc_time_layers
t0=time(1); t1=time(end);
t0 = datenum('2001-03-01','yyyy-mm-dd')-1;
t1 = datenum('2003-10-01','yyyy-mm-dd')-1;

ind=find(time>= t0 & time<=t1);
year=year(ind);time=time(ind); years= unique(year);
it=1:round(length(time)/10):length(time); % discrete index for plotting symbols

%% open all figures
for np=1:nfig, figure(np); set(gcf,'Position',[0 0 1440 750],'Visible','on'); end

%% loop over all variables to show
for i=1:nvar
  varn=cell2mat(var{i}(1));  varshort=cell2mat(var{i}(2));
  if(varn(1:2)=='c ')
    eval(['res=' varn(3:end) ';']);
    res = res*cell2mat(var{i}(5));
  else
    res = squeeze(ncread(ncfile,varn))*cell2mat(var{i}(5));
    % stored for later calculation
    if IsStore(i), eval([varshort(find(~isspace(varshort))) '=res;']); end
    varid = netcdf.inqVarID(ncid,varn);
    units = netcdf.getAtt(ncid,varid,'units');
    dims  = netcdf.getAtt(ncid,varid,'coordinates');
    dim   = length(strfind(dims,' '))+1;
  end
%  y = res(:,ii);
  %eval([num2str(a) '+' '2.3'])

% index position of sub-plot
  iy=cell2mat(var{i}(7)); ix=cell2mat(var{i}(8));  
% geometry of sub-plot
  x0=0.06+(ix-1)*1.2*dxp; y0=0.1+(nrow-iy)*1.07*dyp;
  tpos=[x0+0.25*(occ(ix,iy)+0.15)*dxp y0+0.85*dyp 0.3*dxp 0.11*dyp];
  figure(cell2mat(var{i}(6)));
  axs=subplot('Position',[x0 y0 dxp dyp]);
  hold on

%% process min-max value
  minval = cell2mat(var{i}(3)); maxVal = cell2mat(var{i}(4)); 
  if maxVal<-1, maxVal=1.05*max(max(res)); end
  if minval>0 & maxVal/minval > 30, set(gca,'YScale','Log','YTick',power(10,ceil(log10(minval)):ceil(log10(maxVal))));  end

  ptag=cell2mat(var{i}(9));  % tag with plot type and depth/time index
  if(ptag(1)=='P')
    set(axs,'FontSize',fs,'Xlim',[minval maxVal],'box','on');
    xlabel(units);
  else
    set(axs,'FontSize',fs,'Xlim',[t0 t1],'box','on');%,'Fontweight','bold'
    if occ(ix,iy) ==0, % first action in subplot
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
    set(axs,'ylim',[mean(depth(end,:)) mean(depth(1))]);
  end

  switch(ptag(1))  % plot type

  case{'L'} %% single lines
    if occ(ix,iy) ==0,set(axs,'ylim',[minval maxVal]);ylabel(units); grid on; end
%      set(cb,'position',[x0cb y0cb 0.015 dyp*0.8],'YAxisLocation','right');
    col=colj(1+occ(ix,iy),:); 
    for li=2:length(ptag)  % loop over given depths
      if isstrprop(ptag(li), 'xdigit') 
        zi=1+str2num(ptag(li));  % depth index from tag list
        % rescale depth index for more than 10 layers
        if(size(res,1)>10) zi=1+round((zi-1)/9*(size(res,1)-1)); end
        y=res(zi,:);    
        plot(time(it),y(ind(it)),'o','Color',coljj(li-1,:),'MarkerFaceColor',coljj(li-1,:));
        annotation('textbox',tpos+[0.08*(li-1)*dxp -0.14*dyp 0 0],'String',[num2str(zi) '/' ptag(li)],'Color',coljj(li-1,:),'Fontweight','bold','FontSize',fs-2,'LineStyle','none');
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
         y=res;
        end
      end
      plot(time,y(ind),lins(ns,:),'Color' ,col,'LineWidth',linw(ns)); 
      if ntags>2
        plot(time(it),y(ind(it)),'o','Color',coljj(ns*2-1,:),'MarkerFaceColor',coljj(ns*2-1,:));
        annotation('textbox',[x0+0.9*dxp y0+(0.85-ns*0.15)*dyp 0.3*dxp 0.11*dyp],'String',tag,'Color',coljj(ns*2-1,:),'Fontweight','bold','FontSize',fs,'LineStyle','none');
      end
%      fprintf('%d %d\t%s  %1.3f\n',ns,i,var,mean(y));
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
%    cb = colorbar;%    set(cb,'LineWidth',1,'FontSize',fs-2);%,'Fontweight','bold'
%    lh = ylabel(cb,units,'FontSize',fs-2);

  case{'P'}   %% profiles
    for li=2:length(ptag)  % loop over given times
     ii=1+round((length(ind)-1)*str2num(ptag(li))/9);
     %% plot model data
     plot(res(:,ind(ii)),depth,lins(ns,:),'Color' ,col,'LineWidth',linw(ns));
     plot(res(:,ind(ii)),depth,'o','Color',coljj(li-1,:),'MarkerFaceColor',coljj(li-1,:));
     annotation('textbox',tpos+[0.09*(li-1)*dxp -0.14*dyp 0 0],'String',[num2str(time(ii)-t_offset)],'Color',coljj(li-1,:),'Fontweight','bold','FontSize',fs-2,'LineStyle','none');
    end
   end
%% plot variable name
%  col='k';
  if(cell2mat(var{i}(9)) ~='N' & occ(ix,iy)<4)
    fac=abs(cell2mat(var{i}(5))-1);
    if(fac>0.1 & fac<1000) varshort=[varshort '*' num2str(cell2mat(var{i}(5)))]; end
    annotation('textbox',tpos,'String',[varshort ],'Color',col,'Fontweight','bold','FontSize',fs+2,'LineStyle','none');%tag
    annotation('textbox',tpos-[0 0.14*dyp 0 0],'String',compn{Zt(i)},'Color',col,'Fontweight','bold','FontSize',fs-2,'LineStyle','none');
    occ(ix,iy) = occ(ix,iy) + 1;
  end
end 
% close input file
netcdf.close(ncid);
%% add tag label from file
%name = sprintf('%s%s_%s_%s',cell2mat(vars(:,1)),cell2mat(varn{end)),run,datestr(t0,'yyyy'));
for nt=1:tagn
  if(strfind(tag,cell2mat(tagd.textdata(nt,1)))>0)
     annotation('textbox',[ns*0.15-0.1 0.95 0.15 0.04],'String',[cell2mat(tagd.textdata(nt,2)) ':' num2str(tagd.data(nt))],'Color',coljj(ns*2-1,:),'Fontweight','bold','FontSize',fs,'LineStyle','none');
  end 
end
end

%% plot as EPS
for np=1:nfig
  figure(np);  
%  name = ;
  set(gcf,'PaperPositionMode','auto');
  fnam=fullfile(spath,sprintf('HR%s%s_%d_%d.eps',cell2mat(tags(1)),cell2mat(tags(end)),np,ns));
  fprintf('save EPS in %s ...\n',fnam);
  print(gcf,'-depsc',fnam);
end


