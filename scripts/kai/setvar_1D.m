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
var={{'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water';'DIN';0;64; 1.0;	1; 1;1;'L9'};
  {'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water';'DIP';0;1.8; 15;	1; 1;1;'L9'};
%  {'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water';'DIP';0;1.4; 1;	1; 2;1;'L0'};
%%  {'Detritus_Phosphorus_detP_in_water';'POP';0.;1.8; 1;			1; 1;3;'L9'};
%  {'Dissolved_Organic_Phosphorus_domP_in_water';'DOP';0.;2; 1;		1; 2;1;'L9'};
  {'Phytplankton_Phosphorus_phyP_in_water';'PhyP';0;2.5; 1.0;		1; 1;3;'N'};
  {'Chl_chl_in_water';'CHL';0.5;35; 1;				1; 1;2;'L9'};
%  {'Chl_chl_in_water';'CHL';0.5;18; 1;				1; 1;2;'L9'};
%%  {'Chl_chl_in_water';'CHL';0.;15; 1;				1; 3;2;'P0123456789'};
%  {'Chl_chl_in_water';'CHL';0.;8; 1;				1; 2;4;'P579'};
% {'Chl_chl_in_water';'CHL';0.9;120; 3;					1; 1;2;'L0'};
  {'Auxiliary_diagnostic__fac5_in_water';'relax';-0.4;0.35;1; 			1; 1;3;'L9'};
  {'Auxiliary_diagnostic__fac4_in_water';'adap';0;2; 1.0; 			1; 1;3;'L9'};
  {'Auxiliary_diagnostic__fac3_in_water';'growth';0;2; 1; 			1; 1;3;'L9'};
%  {'dtheta_dt_due_to_flex_theta__fac1_in_water';' infect';0;2; 1; 			1; 2;2;'L9'};
%  {'dtheta_dt_due_to_grad_theta__fac2_in_water';'dil';0;2; 1; 			1; 2;2;'L9'};
%%  {'c (fac1)./((reshape(repmat(time'',12,1),1,1,12,length(time))-t_offset)*24*3600+1)';'CoLim';2;9.; 10.0;1; 2;4;'LM'};
%%  {'c (fac2)./((reshape(repmat(time'',12,1),1,1,12,length(time))-t_offset)*24*3600+1)';'Pact';2;9.; 1.0;1; 2;4;'LM'};
 {'Phytplankton_Carbon_phyC_in_water';'PhyC';0.4;40; 0.3;		1; 2;1;'LM'};
  {'Phytplankton_Nitrogen_phyN_in_water';'PhyN';0;-9; 1.0;		1; 2;3;'N'};
  {'c PhyN./(PhyC+1E-2)';'N:C';0.05;0.2; 1.0;				1; 2;2;'L9'};
  {'c PhyP./(PhyC+1E-2)';'P:C';0;0.21; 16;				1; 2;2;'L9'};
%  {'Phytoplankton_C_Uptake_Rate__phyUR_in_water';'prod';0;0.28; 0.2;1; 2;1;'L9'};
%  {'Phytoplankton_Exudation_Loss_Rate__phyELR_in_water';'exu';0.;0.5; -1.;1; 2;1;'L9'};
%  {'Phytoplankton_Aggregation_Loss_Rate__phyALR_in_water';'agg';0;0.5; -1.;1; 2;1;'L9'};
%  {'Phytoplankton_Viral_Loss_Rate__phyVLR_in_water';'vir';0;0.5; -1.;1; 2;1;'L9'};
%  {'Phytoplankton_Grazing_Loss_Rate__phyGLR_in_water';'graz';0;0.27; -1.;1; 2;1;'L9'};
%%  {'Phytoplankton_Respiration_Rate__phyRER_in_water';'res';0;0.27; -1.;1; 2;1;'LM'};
%%  {'c prod./((reshape(repmat(time'',12,1),1,1,12,length(time))-t_offset)*24*3600+1)';'Pro';0;0.2; 1.0;1; 2;1;'LM'};
  {'Virus_C_density_in_cells_vir_in_water';'VirC';5E-5;0.6; 1.;		1; 2;3;'N'};
  {'c VirC./(PhyC+1E-2)';'vir';5E-4;45; 1.0;				1; 2;3;'L8'};
%  {'dtheta_dt_due_to_flex_theta__fac4_in_water';'fZm';0;1.; 	1.;	1; 2;3;'L9'};
%  {'Zooplankton_Mortality_rate_in_water';'Zmort';0;0.27; 	1.;	1; 2;3;'L9'};
  {'fraction_of_Rubisco_Rub_in_water';'Rub';0;-9; 1.0;			1; 2;3;'N'};
  {'c Rub./(PhyC+1E-2)';'fRub';0.1;0.5; 1.0;				1; 2;3;'L9'};
%  {'c 0.6*CHL./(PhyC+1E-2)';'fLHC';0;1.; 1.0;				1; 1;3;'L9'};
%  {'c 1-(Rub+0.6*CHL)./(PhyC+1E-2)';'fNut';0;1.; 0.5;			1; 1;3;'L9'};
  {'Zooplankton_Carbon_zooC_in_water';'ZooC';0;6.;1;		1; 2;1;'LM'};
%  {'Si-uptake_activity__aVSi_in_water';'att2';0;0.2; 1.0;		1; 2;4;'L9'};
%  {'light_attenuation_att_in_water';'att';0;0.15; 1.0;		1; 2;4;'L0'};
%  {'Photosynthetically_Active_Radiation__dPAR_in_water';'PAR';0;2E2; 1E-2;	1; 2;4;'L02M'};
%  {'denitrification_rate_in_soil';'denit';0;100; 1.0;			1; 3;2;'L0123M'}; 

 % {'N2flux';'N2 flux';-0.4;3; 1.; 					1; 2;1;'LM'};
%%  {'mole_concentration_of_nitrate_upward_flux_at_soil_surface';'NO3 flux';0;3.; 86400.;1; 2;1;'LM'};
%%  {'mole_concentration_of_ammonium_upward_flux_at_soil_surface';'NH4 flux';0;1.; 86400.; 1; 2;1;'LM'};
%% {'dissolved_oxygen_upward_flux_at_soil_surface';'O_2 flux';-30;0; 86400.; 1; 2;2;'N'};
%%  {'dissolved_reduced_substances_upward_flux_at_soil_surface';'ODU flux';-6;0; -86400.; 1; 2; 2;'N'};
%%  {'c O_2flux+ODUflux';'tO2 flux';-6;0; -0.1; 			1; 2;1;'LM'};
%  {'N-uptake_activity__aVN_in_water';'aVN';0;1.; 1.; 			1; 2;1;'L9'};
%  {'P-uptake_activity__aVP_in_water';'aVP';0;1.; 1.; 			1; 2;1;'L9'};
%%  {'fast_detritus_C_in_soil';'TOC fast';1E3;2E4; 1.0; 			1; 2;4;'LM'};
%%  {'slow_detritus_C_in_soil';'TOC slow';5E2;5E4; 0.01; 			1; 2;4;'LM'};
%  {'denitrification_rate_in_soil';'denit';0;100; 3E2;			1; 2;4;'LM'};
%  {'detritus-P_in_soil';'POP';5E2;5E4; 50; 				1; 2;3;'LM'};
%%%  {'detritus-P_in_soil';'POPs';0;0.8; 0.003;                           	1; 2;3;'L1'};
%%%  {'mole_concentration_of_phosphate_in_soil';'PO4s';0;3.; 0.006;     	1; 2;3;'LM'};
%%%  {'c (POPs+PO4s)*0.1/18-0.5';'sedP';0;2; 1;                            1; 2;3;'L1'};
%%%  {'c (POPs+PO4s)*0.1/18-0.5';'sedPd';0;2; 2;                           1; 2;3;'L9'};
%%  {'Dissolved_Organic_Carbon_domC_in_water';'DOC';0.;1; 1;		1; 2;1;'LM'};
%%%  {'Detritus_Carbon_detC_in_water';'DetC';0.;36; 1;			1; 1;4;'LM'};
%%  {'Detritus_Carbon_detC_in_water';'DetC';0.;36; 1.;			1; 2;1;'LM'};  %{'Detritus_Phosphorus_detP_in_water';'DetP';0.1;80; 100.0;		1; 2;4;'L9'};
%%  {'dissolved_oxygen_in_soil';'O2s';-1500;0; 1; 		1; 1;4;'N'};
%%  {'dissolved_reduced_substances_in_soil';'ODU';3E2;2E3; 1; 		1; 1;4;'N'};
%%  {'c O2s-ODU';'NetO2';-3E3;10; 1.0; 		1; 1;4;'L1'};
%%  {'c O2s-ODU';'NetO2';-1500;0; 1.0; 		1; 1;4;'L2'};
  {'dissolved_oxygen_upward_flux_at_soil_surface';'O2flux';-30;0; 1.; 1; 1;1;'N'};
  {'dissolved_reduced_substances_upward_flux_at_soil_surface';'ODU flux';-6;0; 1.; 1; 1; 1;'N'};
  {'c ODUflux-O2flux';'O2 flux';5;35; 86400;                            1; 1;1;'LM'};

%%  {'fast_detritus_C_in_soil';'TOC fast';1E2;1E5; 1.0;                  1; 3;1;'C'};
%%  {'Detritus_Carbon_detC_in_water';'DetC';0;20; 1.;		       1;3;3;'P12579'};  
 };

%mole_concentration_of_ammonium_in_soil,mole_concentration_of_nitrate_in_soildenitrification_rate_in_soil,detritus-P_in_soil,dissolved_oxygen_in_soil,dissolved_reduced_substances_in_soil,Zooplankton_Carbon_zooC_in_water,fraction_of_Rubisco_Rub_in_water,mole_concentration_of_ammonium_in_soil,mole_concentration_of_phosphate_in_soil,porosity_in_soil,radiation_in_water,slow_detritus_C_in_soil,temperature_in_water,detritus-P_upward_flux_at_soil_surface,dissolved_reduced_substances_upward_flux_at_soil_surface,fast_detritus_C_upward_flux_at_soil_surface,mole_concentration_of_nitrate_upward_flux_at_soil_surface,mole_concentration_of_phosphate_upward_flux_at_soil_surface,Detritus_Carbon_detC_in_water
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
