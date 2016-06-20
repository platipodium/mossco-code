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
%	long name			short name  min max fac	fig row col type
%	1				2           3   4   5	6   7	8   9
%							    5:is depth index in 3D for maps
var={
 %% {'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water';'DIN';0;50; 1.0;	1; 1;1;'L1'};
%%  {'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water';'DIP';0;2.; 1.;	1; 2;1;'L1'};
%%  {'Phytplankton_Carbon_phyC_in_water';'PhyC';4;200; 1.0;		1; 1;2;'N'};
   {'Chl_chl_in_water';'CHL';0.3;28; 1;					1; 1;1;'L4'};
   {'Chl_chl_in_water';'CHL';0.;2; 1;					1; 1;2;'V4'};
%  {'Chl_chl_in_water';'chl';0.2;40; 1;					1; 1;2;'L0'};
   {'Chl_chl_in_water';'CHL';0;15; 5;					1; 1;0;'M4'};
%%  {'Chl_chl_in_water';'CHL';0.5;20; 2;					2; 1;0;'Mi'};
%%  {'Chl_chl_in_water';'CHL';0.5;20; 2;					3; 1;0;'Mi'};
%  {'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water';'DIN';1.;70; 1;	3; 1;0;'Mi'};
% {'Detritus_Carbon_detC_in_water';'DetC';0;8; 0.1;			1; 2;1;'L0'};
%   {'Detritus_Carbon_detC_in_water';'DetC';0.5;50; 1;			1; 2;0;'M0369'};
%%   {'Zooplankton_Carbon_zooC_in_water';'ZooC';0;9; 1;			1; 2;2;'LM'};
%%   {'NC';'N2C';0.08;0.3; 1.0;						1; 2;3;'L0'};
%%   {'PC';'P2C';0;0.3; 16;						1; 2;3;'L0'};
%%   {'c N2C./P2C';'N:P';0;0.3; 0.01;					1; 2;3;'L0'};
%%  {'fraction_of_Rubisco_Rub_in_water';'Rub';0;-9; 1.0;			1; 1;3;'N'};
%%  {'c Rub./(PhyC+1E-2)';'fRub';0.1;0.5; 1.0;				1; 1;3;'LM'};
%%  {'c 0.6*CHL./(PhyC+1E-2)';'fLHC';0.1;0.5; 1.0;			1; 1;3;'LM'};
%  {'c 1-(Rub+0.6*CHL)./(PhyC+1E-2)';'fNut';0;1.; 1.0;			1; 1;3;'LM'};
%  {'denitrification_rate_in_soil';'denit';0;100; 1.0;			1; 3;1;'DC'};
%  {'N2flux';'N2 flux';-0.4;5; 1.; 					1; 2;1;'LM'};
%  {'mole_concentration_of_nitrate_upward_flux_at_soil_surface';'NO3 flux';0;1.; 86400.;1; 2;1;'LM'};
%  {'mole_concentration_of_ammonium_upward_flux_at_soil_surface';'NH4 flux';0;1.; 86400.; 1; 2;1;'LM'};
  {'dissolved_oxygen_upward_flux_at_soil_surface';'O2flux';-30;0; 1.; 1; 1;1;'N'};
  {'dissolved_reduced_substances_upward_flux_at_soil_surface';'ODU flux';-6;0; 1.; 1; 1; 1;'N'};
  {'c ODUflux-O2flux- 0.0001';'O2 flux';10;35; 86400; 			2; 1;0;'M4'};
   {'Chl_chl_in_water';'CHL';0;15; 4;					3; 1;0;'M4'};
  {'c ODUflux-O2flux - 0.0001';'O2 flux';0;45; 86400; 			1; 2;1;'LM'};
  {'c ODUflux-O2flux - 0.0001';'O2 flux';0;0.3; 86400; 			1; 2;2;'V4'};
%  {'fast_detritus_C_in_soil';'TOC fast';2E2;7E4; 1.0; 			1; 2;2;'LM'};
%  {'slow_detritus_C_in_soil';'TOC slow';5E2;5E4; 1.0; 			1; 2;2;'LM'};
%  {'detritus-P_in_soil';'POP';5E2;5E4; 105.0; 			1; 2;2;'LM'};
%  {'mole_concentration_of_phosphate_in_soil';'PO4s';5E2;5E4; 105.0; 			1; 3;2;'LM'};
%  {'fast_detritus_C_in_soil';'TOC fast';2E3;2E4; 1.0; 			1; 3;3;'DC'};
%  {'mole_concentration_of_ammonium_in_soil';'NH4';3E2;2E3; 1.0; 		1; 3;2;'DC'};
 };
%mole_concentration_of_nitrate_in_soildenitrification_rate_in_soil,detritus-P_in_soil,dissolved_oxygen_in_soil,dissolved_reduced_substances_in_soil,Zooplankton_Carbon_zooC_in_water,fraction_of_Rubisco_Rub_in_water,mole_concentration_of_ammonium_in_soil,mole_concentration_of_phosphate_in_soil,porosity_in_soil,radiation_in_water,slow_detritus_C_in_soil,temperature_in_water,detritus-P_upward_flux_at_soil_surface,dissolved_reduced_substances_upward_flux_at_soil_surface,fast_detritus_C_upward_flux_at_soil_surface,mole_concentration_of_nitrate_upward_flux_at_soil_surface,mole_concentration_of_phosphate_upward_flux_at_soil_surface,Detritus_Carbon_detC_in_water
IsWater=false; IsSoil=false; 

% number of variables to plot
nvar=length(var); nfig=1; nfigm=0; IsStore=zeros(nvar,1);

% retrieve or set major meta-info
for i=1:nvar
 Zt(i)=1;
 varn=cell2mat(var{i}(1));
 if strfind(varn,'soil'),  IsSoil=true; Zt(i)=2; end
 if strfind(varn,'water'), IsWater=true; end
 if strfind(varn,'c ')
  for j=1:nvar
    varshort=cell2mat(var{j}(2)); % stored for later calculation ? 
    if strfind(varn,varshort(1:3)), IsStore(j)=1; end
  end
 end
 % number of figures needed
 ptag=cell2mat(var{i}(9));% tag with plot type and depth/time index
 if (ptag(1)~='M' & cell2mat(var{i}(6))>nfig)  nfig=cell2mat(var{i}(6)); end
 if (ptag(1)=='M' & ntags*cell2mat(var{i}(6))>nfigm) nfigm=ntags*cell2mat(var{i}(6)); end
end
nfig0 = nfig;
nfig  = nfig *size(loc,1);  % duplication of figures according to sites
lx0=1;lx1=0;ly0=1;ly1=0;
 
