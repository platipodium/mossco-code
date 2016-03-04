function r=cl_minmax(data,dim)
% minmax=CL_MINMAX(data,[dim]) computes minmax values of data in first non-unit
% size dimension, or along given dimension.

% Carsten Lemmen <carsten.lemmen@hzg.de>
%cl_register_function;

% Default argument for dim: first dimension with len>1
if nargin<2
  dsize=size(data);
  idim=find(dsize>1);
  if isempty(idim) dim=1; else dim=idim(1); end
end

dmin=min(data,[],dim);
dmax=max(data,[],dim);

r=[dmin dmax];

return;
end