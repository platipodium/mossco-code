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
  {'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water';'DIN';1.;300; 1.0;	1; 1;1;'L1'};
%{'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water';'DIP';0.;2.3; 1.;	2; 1;1;'L0'};
  {'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water';'DIP';0.;3.3; 1.;	1; 2;1;'L1'};
  {'Chl_chl_in_water';'chl';0.5;60; 1;					1; 3;1;'L1'};
%  {'Chl_chl_in_water';'chl';0.5;50; 1;					2; 1;1;'L3'};
%  {'Chl_chl_in_water';'chl';0.5;40; 1;					1; 1;1;'LM'};
%  {'Chl_chl_in_water';'chl';0.7;40; 1;					2; 1;1;'L0'};
%  {'Phytplankton_Carbon_phyC_in_water';'PhyC';4;200; 0.3;               1; 1;1;'N'};
  {'Chl_chl_in_water';'CHL';1.;30;2;                                  1; 1;0;'Mi'};
  {'Chl_chl_in_water';'CHL';1.;30;2;                                  2; 1;0;'Mi'};
  {'Chl_chl_in_water';'CHL';1.;30;2;                                  3; 1;0;'Mi'};
  {'Chl_chl_in_water';'CHL';1.;30;2;                                  4; 1;0;'Mi'};
  {'Chl_chl_in_water';'CHL';1.;30;2;                                  5; 1;0;'Mi'};
  {'Chl_chl_in_water';'CHL';1.;30;2;                                  6; 1;0;'Mi'};
%  {'Chl_chl_in_water';'CHL';1.;30; 2;                                  9; 1;0;'Mi'};
%  {'Chl_chl_in_water';'CHL';1.;30; 1;                                 10; 1;0;'Mi'};
%  {'Chl_chl_in_water';'CHL';1.;30; 1;                                 11; 1;0;'Mi'};
 %%  {'Chl_chl_in_water';'CHL';1.;30; 2;					5; 1;0;'Mi'};
%%  {'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water';'DIN';1;70; 2;	4; 1;0;'Mi'};
%%  {'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water';'DIP';0.;1.; 2.; 6; 1;0;'Mi'};
%   {'Zooplankton_Carbon_zooC_in_water';'ZooC';0.01;15; 1;		2; 2;1;'L1'};
%   {'Detritus_Carbon_detC_in_water';'DetC';0;8; 0.001;			1; 2;1;'N'};
%   {'c log(1E-1+DetC)';'lDetC';0;8; 2;				3; 2;1;'L0'};
%%   {'Virus_C_density_in_cells_vir_in_water';'VirC';0;2; 1.;		2; 1;1;'N'};
%%   {'c VirC./(PhyC+1E-2)';'vir';0;5; 4;				2; 2;1;'L1'};
%%   {'NC';'N2C';0.08;0.24; 1.0;						1; 2;3;'L1'};
%%   {'PC';'P2C';0;0.3; 16;						1; 2;3;'L1'};
%%  {'c N2C./P2C';'N:P';0;0.3; 0.01;					1; 2;3;'L1'};
%%    {'NC';'N:C';0.06;0.35; 1;						2; 1;0;'M0369'};
%%    {'PC';'P:C';0;0.05; 1;						2; 2;0;'M0369'};
%  {'c PhyP./(PhyC+1E-2)';'P:C';0;0.4; 15;				1; 2;2;'LM'};
%  {'Phytplankton_Nitrogen_phyN_in_water';'PhyN';0;-9; 1.0;		1; 2;3;'N'};
%  {'Phytplankton_Phosphorus_phyP_in_water';'PhyP';0;-9; 1.0;		1; 2;3;'N'};
%  {'c PhyN./(PhyC+1E-2)';'N:C';0;0.4; 1.0;				1; 2;2;'LM'};
%  {'c PhyP./(PhyC+1E-2)';'P:C';0;0.4; 15;				1; 2;2;'LM'};
%%  {'fraction_of_Rubisco_Rub_in_water';'Rub';0;-9; 1.0;			1; 1;1;'N'};
%%    {'c Rub./(PhyC+1E-2)';'fRub';0.1;0.5; 1;				10; 1;0;'Mi'};
%%    {'c chl./(PhyC+1E-2)';'Chl2C';0.1;0.5; 1;				13; 1;0;'Mi'};
%%    {'c chl./(PhyC+1E-2)';'Chl2C';0.1;0.5; 4;				15; 1;0;'Mi'};
%  {'c 1-(Rub+0.6*CHL)./(PhyC+1E-2)';'fNut';0;1.; 1.0;			1; 1;3;'LM'};
%  {'denitrification_rate_in_soil';'denit';0;50; 1.0;			4; 1;1;'DC'};
%  {'N2flux';'N2 flux';0.;6; 1.; 					3; 1;1;'LM'};
%  {'N2flux';'N2 flux';0.1;5; 1;                      6; 1;0;'Mi'};
%  {'mole_concentration_of_nitrate_upward_flux_at_soil_surface';'NO3 flux';0;1.; 86400.;1; 2;1;'LM'};
%  {'mole_concentration_of_ammonium_upward_flux_at_soil_surface';'NH4 flux';0;1.; 86400.; 1; 2;1;'LM'};
%  {'dissolved_oxygen_upward_flux_at_soil_surface';'O2flux';-30;0; 1.; 1; 1;1;'N'};
%  {'dissolved_reduced_substances_upward_flux_at_soil_surface';'ODU flux';-6;0; 1.; 1; 1; 1;'N'};
% {'c ODUflux-O2flux';'O2 flux';5;40; 86400; 				9; 1;0;'Mi'};
% {'c ODUflux-O2flux';'O2 flux';0;50; 86400; 				3; 2;1;'LM'};
%  {'slow_detritus_C_in_soil';'TOC slow';5E2;5E4; 1.0; 			1; 3;2;'LM'};
%%  {'detritus-P_in_soil';'POP';5E2;5E4; 1.0; 			1; 1;3;'L1'};
%%  {'temperature_in_water';'Temw';0;20; 1.0; 		1; 1;1;'N'};
%%  {'c Temw(:,:,4,:)-Temw(:,:,1,:)';'dTemw';-3;7; 1.0; 		5; 1;0;'Mi'};
%%  {'mole_concentration_of_phosphate_in_soil';'PO4s';5E2;5E4; 1.0; 		1; 1;3;'L1'};
%%  {'_datt_in_water';'att';0.1;3; 1.0; 		1; 1;3;'N'};
%%  {'c 1.89/att';'secchi';0.;10; 1.0; 		1; 1;3;'L1'};
%  {'fast_detritus_C_in_soil';'TOC fast';2E3;2E4; 1.0; 			1; 3;3;'DC'};
%  {'mole_concentration_of_ammonium_in_soil';'NH4';3E2;2E3; 1.0; 		1; 3;2;'DC'};
 };

%mole_concentration_of_nitrate_in_soildenitrification_rate_in_soil,detritus-P_in_soil,dissolved_oxygen_in_soil,dissolved_reduced_substances_in_soil,Zooplankton_Carbon_zooC_in_water,fraction_of_Rubisco_Rub_in_water,mole_concentration_of_ammonium_in_soil,mole_concentration_of_phosphate_in_soil,porosity_in_soil,radiation_in_water,slow_detritus_C_in_soil,temperature_in_water,detritus-P_upward_flux_at_soil_surface,dissolved_reduced_substances_upward_flux_at_soil_surface,fast_detritus_C_upward_flux_at_soil_surface,mole_concentration_of_nitrate_upward_flux_at_soil_surface,mole_concentration_of_phosphate_upward_flux_at_soil_surface,Detritus_Carbon_detC_in_water
setvar_post

