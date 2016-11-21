ytl=[1E-5 1E-4 1E-3 0.01 0.1 0.3 1 3 10 20 50 100 1E3 1E4 1E5 1E6];  
ili=1; 
ntagm=ntags+(exist('datat') & strcmp(varshort0,'CHL'));

if(~strcmp(varshort,varshortm0))
   moff=moffs; mofc=0;
else
   mofc=mofc+1;
end
varshortm0=varshort;
if isstrprop(ptag(2), 'xdigit') 
  vli=2:length(ptag); mode='s';
elseif 1==0
  vli=0:nrowm*ncolm-1; mode='c';
  moffs=moffs+1;
else
  iig=find(timeg>=t0 & timeg<=t1);
  vli=0:min(nrowm*ncolm,length(iig)-mofc*(nrowm*ncolm))-1; 
  moffs=moffs+1;
  mode='v';
end


di = cell2mat(var{i}(5)); %depth index

%%for cmpm=1:2  % 2nd mode: directly compare maps of different scenarios

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
  if strfind(tag,'P') ti=it(end-1); end

 elseif mode=='c'
  ix = 1+mod(im-1,ncolm);
  iy = 1+floor((im-1)/ncolm);
  ti = toffm+((cell2mat(var{i}(6))-moff)*(nrowm*ncolm)+im)*2;   
 else
  ix = 1+mod(im-1,ncolm);
  iy = 1+floor((im-1)/ncolm);
  [mdv mdi]=min(abs(timeg(iig(im+mofc*(nrowm*ncolm)))-int32(time)));
  ti =mdi+toffm;
%%  fprintf('%d/%d\t%d %d\t%d %d\n',im,length(vli),ti,mdi,timeg(iig(im+moff*(nrowm*ncolm))),int32(time(mdi)));

 end
 if ti-toffm<=length(ind)
% goes to new figure (if required)
  np = ntagm*(cell2mat(var{i}(6))-1)+ ns + nfig;
  if(im==1) vt{np-nfig}=[varshort0 tag]; end
%%  if cmpm==1
    figure(np); set(gcf, 'visible','off','Color','w'); hold on
    % geometry of sub-plot
    x0=0.06+(ix-1)*1.15*dxpm; y0=0.1+(nrowm-iy)*1.03*dypm;
%%  else % cmp different scenarios

  axs=subplot('Position',[x0 y0 dxpm dypm]);
  hold on
 plot_single_map

 if ntagm>1 % cmp different scenarios
  if abs(ntagm-nrowm)<abs(ntagm-ncolm)
    if(2*ntagm<=nrowm)
      ix = 1+mod(im-1,ncolm); npc=floor((im-1)/ncolm); iy = ns;
    else
      ix = 1+mod(im-1,ncolm); npc=floor((im-1)/ncolm); iy = ns;
    end
%    ax_cmp=subplot(ncolm,ntagm,im);
  else
    iy = 1+mod(im-1,nrowm); npc=floor((im-1)/nrowm); ix = ns;
%    ax_cmp=subplot(ntagm,nrowm,im);
  end

  if(~strcmp(varshort,varshortmc0))
    moffc=moffc+100;
  end
  varshortmc0=varshort;
  npc=moffc+npc+mofc*15;

  figure(npc); set(gcf,'Position',[0 0 980 560], 'visible',vis,'Color','w'); hold on
  figc=[figc npc];
    % geometry of sub-plot
  x0=0.06+(ix-1)*1.15*dxpm; y0=0.1+(iy-1)*1.03*dypm;
  fprintf('copy im=%d ix=%d iy=%d %1.2f npc=%d\t%d\n',im,ix,iy,y0,npc,mofc);
  ax_cmp=subplot('Position',[x0 y0 dxpm dypm]);hold on
  if(VerMat<8.4)
    copyaxes(axs,ax_cmp,true );  
  else
    plot_single_map; % repeat plotting
  end

%% plot satellite map for comparison  
  if(ntagm~=ntags & im0<length(iig))
    % find best date of data
    if mode=='v'
      ii=iig;
      mdi=im+mofc*(nrowm*ncolm);mdv=0;
    else
      ii=find(yearg==year(ti-toffm));
      [mdv mdi]=min(abs(doyg(ii)-doy(ti-toffm)));
    end
    if(mdv < 21)
     iy = max(nrowm-ntags-0,0);
 %%  fprintf('data im=%d iy=%d \t%d %d\n',im,iy,mdi,ii(mdi));
     plotdatamap;
     im0=im0+1;
    end

  end   

  % colormap(ssec); 
  imc=imc+1;
 end
 end % if
end  % for im


