%
% generic plot script for time variable mossco results:
% "setvar" defines variables to show - and where/how to do it 
%
% kai wirtz Mar2016
%
%plot types:  L: trajectories
%       L:show layer x only (TODO until now for x<10)  LM: depth average as line  
%       DC:depth contours       N : not shown but stored	
%       P : profiles   	C : Depth-time countours
%	long name		short name  min max fac	fig row col type
%	1			2           3   4   5	6   7	8   9
%						    5:is depth index in 3D for maps
var={
   {'Zooplankton_Carbon_zooC_in_water';'ZooC';0.0;12.5; 1;		1; 1;1;'L1'};
 };

setvar_post
