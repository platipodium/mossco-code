%
% generic plot script for time variable mossco results:
% "setvar" defines variables to show - and where/how to do it 
%
% kai wirtz Nov2015
%

%plot types:  DC:depth contours SL: surface value as line  LM: depth average as line  
%             N : not shown but stored		P : profiles   	C : Depth-time countours
%	long name				short name  min max fac	fig row col type
%	1					2	    3   4   5	6   7	8   9
var={{'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water';'DIN';0;45; 1.0;	1; 1;1;'L9'};
  {'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water';'DIP';0;-9; 15;	1; 1;1;'L9'};
  {'Chl_chl_in_water';'CHL';0.1;120; 3;					1; 1;2;'L9'};
  {'Chl_chl_in_water';'CHL';0.1;120; 3;					1; 1;2;'L0'};
  {'Phytplankton_Carbon_phyC_in_water';'PhyC';1;80; 1.0;		1; 1;2;'L9'};
  {'Phytplankton_Nitrogen_phyN_in_water';'PhyN';0;-9; 1.0;		1; 2;3;'N'};
  {'Phytplankton_Phosphorus_phyP_in_water';'PhyP';0;-9; 1.0;		1; 2;3;'N'};
  {'c PhyN./(PhyC+1E-2)';'N:C';0.06;0.5; 1.0;				1; 2;2;'L9'};
  {'c PhyP./(PhyC+1E-2)';'P:C';0;0.3; 15;				1; 2;2;'L9'};
  {'Phytoplankton_Exudation_Loss_Rate__phyELR_in_water';'resp';0;0.5; 1.;1; 2;3;'LM'};
%  {'Phytoplankton_C_Uptake_Rate__phyUR_in_water';'GPP';0;0.27; 1.;1; 2;3;'LM'};
%  {'dtheta_dt_due_to_flex_theta__fac4_in_water';'fZm';0;1.; 	1.;	1; 2;3;'L9'};
%  {'Zooplankton_Mortality_rate_in_water';'Zmort';0;0.27; 	1.;	1; 2;3;'L9'};
  {'fraction_of_Rubisco_Rub_in_water';'Rub';0;-9; 1.0;			1; 1;3;'N'};
  {'c Rub./(PhyC+1E-2)';'fRub';0.;0.4; 1.0;				1; 1;3;'L9'};
%  {'c 0.6*CHL./(PhyC+1E-2)';'fLHC';0;1.; 1.0;				1; 1;3;'L9'};
  {'c 1-(Rub+0.6*CHL)./(PhyC+1E-2)';'fNut';0;1.; 0.5;			1; 1;3;'L9'};
%  {'Zooplankton_Carbon_zooC_in_water';'ZooC';0;9.; 1.0;			1; 2;1;'LM'};
%  {'Si-uptake_activity__aVSi_in_water';'att2';0;0.2; 1.0;		1; 2;4;'L9'};
%  {'light_attenuation_att_in_water';'att';0;0.15; 1.0;		1; 2;4;'L0'};
%  {'Photosynthetically_Active_Radiation__dPAR_in_water';'PAR';0;2E2; 1E-2;	1; 2;4;'L02M'};
%  {'denitrification_rate_in_soil';'denit';0;100; 1.0;			1; 3;2;'L0123M'}; 
%  {'denitrification_rate_in_soil';'denit';0;100; 1.0;			1; 3;3;'P049'};
 % {'N2flux';'N2 flux';-0.4;3; 1.; 					1; 2;1;'LM'};
%%  {'mole_concentration_of_nitrate_upward_flux_at_soil_surface';'NO3 flux';0;3.; 86400.;1; 2;1;'LM'};
%%  {'mole_concentration_of_ammonium_upward_flux_at_soil_surface';'NH4 flux';0;1.; 86400.; 1; 2;1;'LM'};
%% {'dissolved_oxygen_upward_flux_at_soil_surface';'O_2 flux';-30;0; 86400.; 1; 2;2;'N'};
%%  {'dissolved_reduced_substances_upward_flux_at_soil_surface';'ODU flux';-6;0; -86400.; 1; 2; 2;'N'};
%%  {'c O_2flux+ODUflux';'tO2 flux';-6;0; -0.1; 			1; 2;1;'LM'};
  {'N-uptake_activity__aVN_in_water';'aVN';0;1.; 1.; 			1; 2;1;'L9'};
  {'P-uptake_activity__aVP_in_water';'aVP';0;1.; 1.; 			1; 2;1;'L9'};
%  {'fast_detritus_C_in_soil';'TOC fast';3E3;2E4; 1.0; 			1; 2;3;'LM'};
%  {'slow_detritus_C_in_soil';'TOC slow';5E2;5E4; 0.1; 			1; 2;3;'LM'};
%  {'detritus-P_in_soil';'POP';5E2;5E4; 50; 				1; 2;3;'LM'};
%  {'mole_concentration_of_phosphate_in_soil';'PO4';5E2;5E4; 90; 	1; 2;3;'LM'};
%%  {'Detritus_Carbon_detC_in_water';'DetC';0.;-9; 1.0;			1; 1;1;'L9'};
%  {'Detritus_Phosphorus_detP_in_water';'DetP';0.1;80; 100.0;		1; 2;4;'L9'};
%  {'fast_detritus_C_in_soil';'TOC fast';2E3;2E4; 1.0; 			1; 3;3;'CD'};
%  {'mole_concentration_of_ammonium_in_soil';'NH4';3E2;2E3; 1.0; 		1; 3;2;'CD'};
 };

%mole_concentration_of_nitrate_in_soildenitrification_rate_in_soil,detritus-P_in_soil,dissolved_oxygen_in_soil,dissolved_reduced_substances_in_soil,Zooplankton_Carbon_zooC_in_water,fraction_of_Rubisco_Rub_in_water,mole_concentration_of_ammonium_in_soil,mole_concentration_of_phosphate_in_soil,porosity_in_soil,radiation_in_water,slow_detritus_C_in_soil,temperature_in_water,detritus-P_upward_flux_at_soil_surface,dissolved_reduced_substances_upward_flux_at_soil_surface,fast_detritus_C_upward_flux_at_soil_surface,mole_concentration_of_nitrate_upward_flux_at_soil_surface,mole_concentration_of_phosphate_upward_flux_at_soil_surface,Detritus_Carbon_detC_in_water
IsWater=false; IsSoil=false; 

% number of variables to plot
nvar=length(var); nfig=1; IsStore=zeros(nvar,1);nfigm=0;

%% retrieve or set major meta-info
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
 if cell2mat(var{i}(6))>nfig,  nfig=cell2mat(var{i}(6)); end
end
nfig0 = nfig;
