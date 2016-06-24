%%
% calculate power spectrum of irregular grid data
%
% kai wirtz June 2016

% load data
%load('~/CHL_vS_phy0..mat')

% grid dims
snx=size(value,1); sny=size(value,2);
 
% locate optimal non-NaN window
suf=0.333;
six=sum(isnan(value),2);
six0=min(find(six<suf*sny));
six1=max(find(six<suf*sny));
if(mod(six1-six0,2)==0)
  if(six(six0)>six(six1)) six0=six0+1;
  else six1=six1-1; end
end

siy=sum(isnan(value),1);
siy0=min(find(siy<suf*snx));
siy1=max(find(siy<suf*snx));
if(mod(siy1-siy0,2)==0)
  if(siy(siy0)>siy(siy1)) siy0=siy0+1;
  else siy1=siy1-1; end
end

if(siy0+10>siy1 | six0+10>six1)
  fprintf('not enough valid grid data  ...\n');
else
value=value(six0:six1,siy0:siy1)';

Mval = inpaint_nans(value,3);

%[X,Y] = meshgrid(1:six1-six0+1, 1:siy1-siy0+1);
%valid = ~isnan(value);
%Mval = griddata(X(valid),Y(valid),value(valid),X,Y);
%length(find(isnan(Mval)))

%subplot(1,2,1),surf(X,Y,value,'edgecolor','none'), shading interp
%subplot(1,2,2),surf(X,Y,Mval,'edgecolor','none'), shading interp 

%compute the Fourier transform
imf=fftshift(fft2(Mval));

%power spectrum: square of the modulus of the Fourier transform
impf=abs(imf).^2;

% frequency coordinate
snx=size(Mval,1); sny=size(Mval,2);
fx=-snx/2:snx/2-1;
fy=-sny/2:sny/2-1;
% display the log power spectrum 
%imagesc(fx,fy,log10(impf));

% rotational average of the power spectrum:
n=min(snx,sny);
six=find(abs(fx)<=floor(n/2) & fx<floor(n/2));
siy=find(abs(fy)<=floor(n/2) & fy<floor(n/2));

Pf=rotavg(impf(six,siy));

frad=(0:n/2)/n*pi/2;

%loglog(f1,Pf)
end