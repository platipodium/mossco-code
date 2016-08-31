fdname='~/data/DeutscheBucht/NOAH/NOAH_benthic_O2fluxes.csv';
statlabel={'A-permeable','B','C-imperm','CCPG','D','E','F','G','H','CCPJ','I','NSB3','NSB2','DB','Median','Median high permeable','Median low permeable'};
hipor=[1 2 4 6 13];%11 
lopor=[3 5 7 8 9 10 12 14];

noahcoord = csvread(fdname,0,1,[0 1 2 17]);
noah_depth=noahcoord(1,:);
noah_lat=noahcoord(2,:);
noah_lon=noahcoord(3,:);

noahdat = csvread(fdname,4,0,[4 0 24 16]);
noahdat(noahdat<1E-4)=-1;
noahdat(:,11)=-1;
ndn = 14;

for i=1:ndn
 latv=reshape(lat, numel(lat), 1);
 lonv=reshape(lon, numel(lon), 1);

 [mv, index] = min( abs(latv-noah_lat(i))+abs(lonv-noah_lon(i)) );
 [ci,cj] = ind2sub(size(lat), index);

 fprintf('%d %s\t lat/lon\t%1.4f %1.4f\tfound %1.4f %1.4f\t%d %d\n',i,statlabel{i},noah_lat(i),noah_lon(i),lat(ci,cj),lon(ci,cj),ci,cj); 
 nsgi(i,:)=[ci,cj];
end
fprintf('\n');

for i=1:ndn
 fprintf('[%1.3f,%1.3f],\t',noah_lat(i),noah_lon(i));
end
fprintf('\n');
for i=1:ndn
 fprintf('\"NOAH-%s\";\t',statlabel{i});
end
fprintf('\n');

% export variogram for given month
ind=find(noahdat(:,1)==6); % june
dat=noahdat(ind,2:ndn+1);
lats=repmat(noah_lat(1:end-3),length(ind),1);
lons=repmat(noah_lon(1:end-3),length(ind),1);

i2=find(dat>0);
dvario = variogram([lons((i2)) lats((i2))],dat((i2)),'plotit',true,'nrbins',8);
dvario.mean=mean(dat((i2)))
dvario.std=std(dat((i2)))
text(2.,0.06,[num2str(min(dvario.val),'%1.2f') '-' num2str(max(dvario.val),'%1.1f')],'fontweight','bold','fontsize',16);
outfilename=['noah_O2flux.mat'];
save(outfilename,'dvario')





