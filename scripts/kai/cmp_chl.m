%%
% compare chl data between stations and satellite  
%
% kai wirtz Nov2015
%
clear all;close all; coljj=jet(9); tw=6;
addpath('~/tools/m_map');  % map-toolbox needed for 2D plots
datf='~/data/DeutscheBucht/stations.nc';
datm='~/data/DeutscheBucht/esacci_2003_2013.mat';
%% settings
% locations; at least one site-name (locs) should be given 
%loc =[]; 
loc =[[52.56,3.5];[53.76,4.77];[54.18,7.85];[52.3,4.3];[55.,8.3];[53.7,7.2];[53.42,5.15]; [54.6,8.4];[54.0,8.7];]; 
%nrow  = 2; ncol  = 3; 	% dxp = 0.82/(ncol+0.05); dyp = 0.83/(nrow +0.05);
% Noordwijk-10 Noordwijk-70
locs={'NOORDWK70';'TERSLG50';'Helgoland';'NOORDWK10';'Sylt'; 'Norderney';'TERSLG4';'SAmrum';'Norderelbe';'TERSLG70';}; %
%'Helgoland'; 'Sylt';    'SAmrum';'Norderelbe';'Nordeney',
%  'T36';     'T26' ;    'T41';   'T8'  ;      'T2';
%  'T22';     'T5';      'T12';   'T11'
%[54.1,54.1];[55.0,8.4];[54.6,8.4];[54.0,8.7];[53.7,7.2];
%[53.7,6.4];[54.2,7.5];[54.0,8.1];[55.0,8.0];[55.2,5.0];
%[54.1,6.3];[55.0,6.3];[54.7,7.4];[54.7,6.9];

% load and prepare data
read_stations_nc;  
datime= datime- datenum('2000-01-01','yyyy-mm-dd'); %days after 1/1/2000
load(datm);[lon,lat] = meshgrid(lons,lats); 
timeg = timeg  + datenum('1970-01-01','yyyy-mm-dd')- datenum('2000-01-01','yyyy-mm-dd'); %days after 1/1/2000
set(gcf,'position',[1 5 850 850],'Visible','on');
plot([0.1 100],[0.1 100],'k-');
hold on
di=1;
for li=1:size(loc,1)
  [m1 i]=min(abs(lat-loc(li,1))+abs(lon-loc(li,2)),[],1);
  [m j]=min(m1);
  i_loc(li,1:2)=[i(j) min(j,size(datat,3))];
  
  fprintf('%s: lon %1.3f %1.3f\t lat %1.3f %1.3f\t%d %d\n',locs{li},lon(i_loc(li,1),i_loc(li,2)),loc(li,2),lat(i_loc(li,1),i_loc(li,2)),loc(li,1),i_loc(li,1),i_loc(li,2));
  while length(find(isnan(datat(:,i_loc(li,2),i_loc(li,1)))))==size(datat,1)
%      length(find(isnan(datat(:,i_loc(li,1),i_loc(li,2)))))
      i_loc(li,2)=i_loc(li,2)-1;
      i_loc(li,1)=i_loc(li,1)+1;
  end     
  fprintf('%s: lon %1.3f %1.3f\t lat %1.3f %1.3f\t%d %d\n',locs{li},lon(i_loc(li,1),i_loc(li,2)),loc(li,2),lat(i_loc(li,1),i_loc(li,2)),loc(li,1),i_loc(li,1),i_loc(li,2));
  id=show_dati(li); 
  iv=1;chl_i(li) = -1;ii=[];
  while length(vars{id,iv})>0    
   if strcmpi(vars{id,iv},'CHL')
     chl_i(li) = iv;
     chl=squeeze(data{id,iv});

     ii=find(~isnan(chl));
     fprintf(' %d\t%1.1f\n',length(ii),mean(chl(ii)));
     break;
   end
   iv=iv+1;
  end
  for i=1:length(ii)
    [mdv mdi]=min(abs(timeg-datime(ii(i))));
    if(mdv < tw)
      satvalue=datat(mdi,i_loc(li,2),i_loc(li,1));
%      chl(ii(i))
      if ~isnan(satvalue)
       plot(chl(ii(i)),satvalue,'o','Color',coljj(li,:),'Markersize',tw+1-mdv,'Linewidth',1+(li<4));%,'MarkerFaceColor'
       x1a(di:di+tw-mdv)=chl(ii(i));
       x2a(di:di+tw-mdv)=satvalue;
       source(di:di+tw-mdv)=li;
       di=di+tw+1;
      end
    end
  end
%  text(0.22,37-3*li,locs{li},'Color',coljj(li,:),'fontsize',14,'fontweight','bold');
  text(0.22,66*exp(-0.3*li),locs{li},'Color',coljj(li,:),'fontsize',14,'fontweight','bold');
end 
%    t3 = find(years(t2)>=27);
    hold on
%    plot(x1(t3),x2(t3),'bo');
    % calc regression
ind=find(x1a>1E-8 & x2a>1E-8);  % all positive data 
x1=log10(x1a(ind));x2=log10(x2a(ind));
regression
fprintf('yreg= %1.3f + %1.3f*x\n',mean(x2)-c*mean(x1),c);
ta=sprintf('%1.2f + %1.2f*x',mean(x2)-c*mean(x1),c);
text(22,34,ta,'Color','k','fontsize',14,'fontweight','bold');
plot(power(10,xreg),power(10,yreg),'k-','LineWidth',2)

ind=find(x1a>1E-8 & x2a>1E-8 & source<5);% only case-I
x1=log10(x1a(ind));x2=log10(x2a(ind));
regression
fprintf('yreg= %1.3f + %1.3f*x\n',mean(x2)-c*mean(x1),c);
ta=sprintf('%1.2f + %1.2f*x',mean(x2)-c*mean(x1),c);
text(22,18,ta,'Color','b','fontsize',14,'fontweight','bold');
plot(power(10,xreg),power(10,yreg),'b-','LineWidth',2)

set(gca,'YScale','log','XScale','log');
set(gca,'Box','on','fontsize',14);
xlabel('station chl')
ylabel('ESACCI chl')
axis([0.2 76 0.2 76]);
%t0 = datenum('2003-03-01','yyyy-mm-dd')-1;ind=find(time>= t0 & time<=t1); %toffm = min(find(time>= t0))-1;
fnam='~/data/DeutscheBucht/cmp_chl_log.png';
fprintf('save PNG in %s ...\n',fnam);
print(gcf,'-dpng',fnam);




