#   Makefile generated using program [bldmake]
#
#   Generation date [03/07/2018  13:58:03]
#   Configuration file [cfg.test.bld]
#   CVS archive [/pf/g/g260077/devel/M3HOME/CMAQ5.0.1/models/CCTM]
#

MODEL = CCTM_test_Linux2_x86_64ifort

FC    = mpiifort
CC    = cc

f_FLAGS       = -fixed -132 -qoverride-limits -fno-alias -mp1 -fp-model precise -xCORE-AVX2 -O2 -I /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/ioapi_3.1/Linux2_x86_64ifort -I /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/pario -I /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/se_snl -I.
F_FLAGS       = -fixed -132 -qoverride-limits -fno-alias -mp1 -fp-model precise -xCORE-AVX2 -O2 -I /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/ioapi_3.1/Linux2_x86_64ifort -I /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/pario -I /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/se_snl -I.
f90_FLAGS     = -free -fno-alias -fp-model precise -mp1 -xCORE-AVX2 -O2 -I /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/ioapi_3.1/Linux2_x86_64ifort -I /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/pario -I /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/se_snl -I.
F90_FLAGS     = -free -fno-alias -fp-model precise -mp1 -xCORE-AVX2 -O2 -I /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/ioapi_3.1/Linux2_x86_64ifort -I /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/pario -I /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/se_snl -I.
C_FLAGS       = -O2 -DFLDMN -I /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/mpich/include

LINKER        = mpiifort
LINK_FLAGS    = 

CPP = mpiifort
CPP_FLAGS = \
  -Dparallel \
  -DSUBST_MODULES=SE_MODULES \
  -DSUBST_BARRIER=SE_BARRIER \
  -DSUBST_GLOBAL_MAX=SE_GLOBAL_MAX \
  -DSUBST_GLOBAL_MIN=SE_GLOBAL_MIN \
  -DSUBST_GLOBAL_MIN_DATA=SE_GLOBAL_MIN_DATA \
  -DSUBST_GLOBAL_TO_LOCAL_COORD=SE_GLOBAL_TO_LOCAL_COORD \
  -DSUBST_GLOBAL_SUM=SE_GLOBAL_SUM \
  -DSUBST_GLOBAL_LOGICAL=SE_GLOBAL_LOGICAL \
  -DSUBST_LOOP_INDEX=SE_LOOP_INDEX \
  -DSUBST_SUBGRID_INDEX=SE_SUBGRID_INDEX \
  -DSUBST_HI_LO_BND_PE=SE_HI_LO_BND_PE \
  -DSUBST_SUM_CHK=SE_SUM_CHK \
  -DSUBST_INIT_ARRAY=SE_INIT_ARRAY \
  -DSUBST_COMM=SE_COMM \
  -DSUBST_MY_REGION=SE_MY_REGION \
  -DSUBST_SLICE=SE_SLICE \
  -DSUBST_GATHER=SE_GATHER \
  -DSUBST_DATA_COPY=SE_DATA_COPY \
  -DSUBST_IN_SYN=SE_IN_SYN

SE_SNL =  -L/pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/se_snl -lse_snl
PARIO =  -L/pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/pario -lpario -ldl -lrt -lpthread -libverbs
IOAPI =  -L/pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/ioapi_3.1/Linux2_x86_64ifort -lioapi
NETCDF =  -L/pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/netcdf/lib -lnetcdf -lnetcdff

LIBRARIES = $(SE_SNL) $(PARIO) $(IOAPI) $(NETCDF)

BASE_INC = /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/scripts/cctm/BLD_test

INCLUDES = \
  -DSUBST_GRID_ID= \
  -DSUBST_PE_COMM=\"$(BASE_INC)/PE_COMM.EXT\" \
  -DSUBST_CONST=\"$(BASE_INC)/CONST.EXT\" \
  -DSUBST_FILES_ID=\"$(BASE_INC)/FILES_CTM.EXT\" \
  -DSUBST_EMISPRM=\"$(BASE_INC)/EMISPRM.EXT\" \
  -DSUBST_RXCMMN=\"$(BASE_INC)/RXCM.EXT\" \
  -DSUBST_RXDATA=\"$(BASE_INC)/RXDT.EXT\" \
  -DSUBST_PACTL_ID=\"$(BASE_INC)/PA_CTL.EXT\" \
  -DSUBST_PACMN_ID=\"$(BASE_INC)/PA_CMN.EXT\" \
  -DSUBST_PADAT_ID=\"$(BASE_INC)/PA_DAT.EXT\" \
  -DSUBST_MPI=\"/pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/mpich/include/mpif.h\"

GLOBAL_MODULES = \
  UTILIO_DEFN.o \
  VGRD_DEFN.o \
  HGRD_DEFN.o \
  CGRID_SPCS.o \
  GRID_CONF.o \
  PCGRID_DEFN.o \
  WVEL_DEFN.o \
  BIDI_MOD.o \
  LSM_MOD.o \
  DEPVVARS.o \
  MOSAIC_MOD.o \
  ABFLUX_MOD.o \
  AEROMET_DATA.o \
  BIOG_EMIS.o \
  BEIS_DEFN.o \
  AERO_DATA.o \
  LTNG_DEFN.o \
  UDTYPES.o \
  STK_EMIS.o \
  STK_PRMS.o \
  PTMAP.o \
  PTBILIN.o \
  SSEMIS.o \
  PTMET.o \
  PT3D_DEFN.o \
  DUST_EMIS.o \
  STD_CONC.o \
  SOA_DEFN.o \
  PRECURSOR_DATA.o \
  AERO_EMIS.o \
  EMIS_DEFN.o \
  DEPV_DEFN.o \
  PAGRD_DEFN.o

CTM_YAMO = \
  AVG_CONC.o \
  advstep.o \
  driver.o \
  hveloc.o \
  sciproc.o \
  wr_aconc.o \
  wr_cgrid.o \
  wr_conc.o

PAR_NODISTR = \
  mpcomm_init.o \
  par_term.o

INIT_YAMO = \
  diffmsg.o \
  flcheck.o \
  grdcheck.o \
  initscen.o \
  load_cgrid.o \
  opconc.o

GENCOOR = \
  couple.o \
  ppmv_msmr.o

HYAMO = \
  advbc_map.o \
  hadvyppm.o \
  hcontvel.o \
  hppm.o \
  rdbcon.o \
  x_ppm.o \
  x_yamo.o \
  y_ppm.o \
  y_yamo.o \
  zfdbc.o

VYAMO = \
  vppm.o \
  zadvyppm.o

MULTISCALE = \
  deform.o \
  hcdiff3d.o \
  hdiff.o \
  rho_j.o

ACM2 = \
  VDIFF_MAP.o \
  conv_cgrid.o \
  eddyx.o \
  matrix.o \
  opddep.o \
  opddep_fst.o \
  opddep_mos.o \
  opmet.o \
  rddepv.o \
  rdmet.o \
  tri.o \
  vdiffacm2.o

M3DRY = \
  cgrid_depv.o \
  gas_depv_map.o \
  m3dry.o \
  opdepv_diag.o \
  opdepv_fst.o \
  opdepv_mos.o

EMIS = \
  cropcal.o \
  opemis.o \
  tfabove.o \
  tfbelow.o

BEIS3 = \
  beis3.o \
  checkmem.o \
  chkgrid.o \
  czangle.o \
  getfline.o \
  getparb.o \
  hrno.o \
  parsline.o \
  tmpbeis.o \
  wrdaymsg.o

SMOKE = \
  delta_zs.o \
  fire_plmris.o \
  openlayout.o \
  oppt3d_diag.o \
  plmris.o \
  plsprd.o \
  preplm.o \
  ungridb2.o \
  write3_distr.o

PHOT_INLINE = \
  CSQY_DATA.o \
  PHOT_MOD.o \
  aero_photdata.o \
  o3totcol.o \
  opphot.o \
  phot.o

EBI_CB05TUCL = \
  hrdata_mod.o \
  hrcalcks.o \
  hrdriver.o \
  hrg1.o \
  hrg2.o \
  hrg3.o \
  hrg4.o \
  hrinit.o \
  hrprodloss.o \
  hrrates.o \
  hrsolver.o

AERO6 = \
  aero_depv.o \
  aero_driver.o \
  aero_subs.o \
  coags.o \
  getpar.o \
  hetchem.o \
  isocom.o \
  isofwd.o \
  isorev.o \
  opdiam.o \
  opvis.o \
  poaage.o

CLOUD_ACM_AE6 = \
  AQ_DATA.o \
  acmcld.o \
  aq_map.o \
  aqchem.o \
  cldproc_acm.o \
  convcld_acm.o \
  getalpha.o \
  hlconst.o \
  indexn.o \
  rescld.o \
  scavwdep.o

PA = \
  pa_init.o \
  pa_irr.o \
  pa_irr_ctl.o \
  pa_mkhdr.o \
  pa_output.o \
  pa_update.o

UTIL = \
  cksummer.o \
  findex.o \
  get_envlist.o \
  lstepf.o \
  setup_logdev.o \
  subhdomain.o \
  subhfile.o

OBJS = \
  $(GLOBAL_MODULES) \
  $(CTM_YAMO) \
  $(PAR_NODISTR) \
  $(INIT_YAMO) \
  $(GENCOOR) \
  $(HYAMO) \
  $(VYAMO) \
  $(MULTISCALE) \
  $(ACM2) \
  $(M3DRY) \
  $(EMIS) \
  $(BEIS3) \
  $(SMOKE) \
  $(PHOT_INLINE) \
  $(EBI_CB05TUCL) \
  $(AERO6) \
  $(CLOUD_ACM_AE6) \
  $(PA) \
  $(UTIL)

.SUFFIXES: .F .f .c .F90 .f90

$(MODEL): $(OBJS)
	$(LINKER) $(LINK_FLAGS) $(OBJS) $(LIBRARIES) -o $@

.F.o:
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f.o:
	$(FC) -c $(f_FLAGS) $<

.F90.o:
	$(FC) -c $(F90_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f90.o:
	$(FC) -c $(f90_FLAGS) $<

.c.o:
	$(CC) -c $(C_FLAGS) $<

clean:
	rm -f $(OBJS) $(MODEL) *.mod


# dependencies

AVG_CONC.o:	VGRD_DEFN.F CGRID_SPCS.F UTILIO_DEFN.F
STD_CONC.o:	HGRD_DEFN.F VGRD_DEFN.F CGRID_SPCS.F UTILIO_DEFN.F
WVEL_DEFN.o:	GRID_CONF.F UTILIO_DEFN.F
advstep.o:	GRID_CONF.F UTILIO_DEFN.F $(BASE_INC)/CONST.EXT $(BASE_INC)/FILES_CTM.EXT
driver.o:	PCGRID_DEFN.F CGRID_SPCS.F STD_CONC.F AVG_CONC.F WVEL_DEFN.F \
		PAGRD_DEFN.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT $(BASE_INC)/RXCM.EXT \
		$(BASE_INC)/PA_CTL.EXT /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/mpich/include/mpif.h
hveloc.o:	GRID_CONF.F UTILIO_DEFN.F $(BASE_INC)/PE_COMM.EXT $(BASE_INC)/CONST.EXT \
		$(BASE_INC)/FILES_CTM.EXT
sciproc.o:	UTILIO_DEFN.F $(BASE_INC)/PA_CTL.EXT
wr_aconc.o:	GRID_CONF.F AVG_CONC.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
wr_cgrid.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
wr_conc.o:	GRID_CONF.F STD_CONC.F WVEL_DEFN.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
GRID_CONF.o:	HGRD_DEFN.F VGRD_DEFN.F
HGRD_DEFN.o:	UTILIO_DEFN.F
PAGRD_DEFN.o:	GRID_CONF.F $(BASE_INC)/PA_CTL.EXT $(BASE_INC)/PA_CMN.EXT
PCGRID_DEFN.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F
VGRD_DEFN.o:	UTILIO_DEFN.F
mpcomm_init.o:	UTILIO_DEFN.F /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/mpich/include/mpif.h
par_term.o:	UTILIO_DEFN.F /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/mpich/include/mpif.h
diffmsg.o:	UTILIO_DEFN.F
flcheck.o:	UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
grdcheck.o:	GRID_CONF.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
initscen.o:	HGRD_DEFN.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
load_cgrid.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/CONST.EXT \
		$(BASE_INC)/FILES_CTM.EXT
opconc.o:	GRID_CONF.F WVEL_DEFN.F STD_CONC.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
couple.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
ppmv_msmr.o:	GRID_CONF.F UTILIO_DEFN.F $(BASE_INC)/CONST.EXT $(BASE_INC)/FILES_CTM.EXT
advbc_map.o:	CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
hadvyppm.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F HGRD_DEFN.F \
		$(BASE_INC)/PE_COMM.EXT $(BASE_INC)/FILES_CTM.EXT
hcontvel.o:	GRID_CONF.F UTILIO_DEFN.F $(BASE_INC)/PE_COMM.EXT \
		$(BASE_INC)/FILES_CTM.EXT
hppm.o:	HGRD_DEFN.F UTILIO_DEFN.F
rdbcon.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/CONST.EXT \
		$(BASE_INC)/FILES_CTM.EXT
x_ppm.o:	HGRD_DEFN.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/PE_COMM.EXT \
		$(BASE_INC)/CONST.EXT
x_yamo.o:	HGRD_DEFN.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/PE_COMM.EXT \
		$(BASE_INC)/CONST.EXT
y_ppm.o:	HGRD_DEFN.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/PE_COMM.EXT \
		$(BASE_INC)/CONST.EXT
y_yamo.o:	HGRD_DEFN.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/PE_COMM.EXT \
		$(BASE_INC)/CONST.EXT
vppm.o:	CGRID_SPCS.F UTILIO_DEFN.F
zadvyppm.o:	GRID_CONF.F CGRID_SPCS.F WVEL_DEFN.F UTILIO_DEFN.F \
		$(BASE_INC)/FILES_CTM.EXT
deform.o:	GRID_CONF.F UTILIO_DEFN.F $(BASE_INC)/PE_COMM.EXT $(BASE_INC)/CONST.EXT \
		$(BASE_INC)/FILES_CTM.EXT
hcdiff3d.o:	GRID_CONF.F UTILIO_DEFN.F $(BASE_INC)/PE_COMM.EXT \
		$(BASE_INC)/CONST.EXT $(BASE_INC)/FILES_CTM.EXT
hdiff.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/PE_COMM.EXT \
		$(BASE_INC)/CONST.EXT
rho_j.o:	GRID_CONF.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
VDIFF_MAP.o:	CGRID_SPCS.F AERO_EMIS.F UTILIO_DEFN.F $(BASE_INC)/EMISPRM.EXT
conv_cgrid.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/CONST.EXT \
		$(BASE_INC)/FILES_CTM.EXT
eddyx.o:	GRID_CONF.F UTILIO_DEFN.F $(BASE_INC)/PE_COMM.EXT $(BASE_INC)/CONST.EXT \
		$(BASE_INC)/FILES_CTM.EXT
matrix.o:	VGRD_DEFN.F CGRID_SPCS.F UTILIO_DEFN.F
opddep.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
opddep_fst.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F LSM_MOD.F \
		$(BASE_INC)/FILES_CTM.EXT
opddep_mos.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F LSM_MOD.F \
		$(BASE_INC)/FILES_CTM.EXT
opmet.o:	UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
rddepv.o:	HGRD_DEFN.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT \
		$(BASE_INC)/RXCM.EXT
rdmet.o:	GRID_CONF.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
tri.o:	VGRD_DEFN.F CGRID_SPCS.F
vdiffacm2.o:	CGRID_SPCS.F GRID_CONF.F EMIS_DEFN.F DEPV_DEFN.F \
		VDIFF_MAP.F UTILIO_DEFN.F BIDI_MOD.F LSM_MOD.F $(BASE_INC)/CONST.EXT \
		$(BASE_INC)/FILES_CTM.EXT $(BASE_INC)/PA_CTL.EXT
ABFLUX_MOD.o:	HGRD_DEFN.F UTILIO_DEFN.F BIDI_MOD.F MOSAIC_MOD.F \
		LSM_MOD.F $(BASE_INC)/CONST.EXT $(BASE_INC)/FILES_CTM.EXT
BIDI_MOD.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
DEPVVARS.o:	HGRD_DEFN.F UTILIO_DEFN.F
DEPV_DEFN.o:	HGRD_DEFN.F CGRID_SPCS.F DEPVVARS.F UTILIO_DEFN.F \
		MOSAIC_MOD.F LSM_MOD.F BIDI_MOD.F VGRD_DEFN.F ABFLUX_MOD.F GRID_CONF.F \
		AERO_DATA.F SOA_DEFN.F AEROMET_DATA.F $(BASE_INC)/CONST.EXT \
		$(BASE_INC)/FILES_CTM.EXT $(BASE_INC)/RXCM.EXT
LSM_MOD.o:	HGRD_DEFN.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
MOSAIC_MOD.o:	HGRD_DEFN.F LSM_MOD.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
cgrid_depv.o:	CGRID_SPCS.F HGRD_DEFN.F UTILIO_DEFN.F
gas_depv_map.o:	CGRID_SPCS.F DEPVVARS.F UTILIO_DEFN.F
m3dry.o:	HGRD_DEFN.F DEPVVARS.F VGRD_DEFN.F UTILIO_DEFN.F ABFLUX_MOD.F \
		LSM_MOD.F MOSAIC_MOD.F BIDI_MOD.F $(BASE_INC)/PE_COMM.EXT $(BASE_INC)/CONST.EXT \
		$(BASE_INC)/FILES_CTM.EXT
opdepv_diag.o:	GRID_CONF.F DEPVVARS.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
opdepv_fst.o:	GRID_CONF.F DEPVVARS.F UTILIO_DEFN.F LSM_MOD.F $(BASE_INC)/FILES_CTM.EXT
opdepv_mos.o:	GRID_CONF.F DEPVVARS.F UTILIO_DEFN.F LSM_MOD.F $(BASE_INC)/FILES_CTM.EXT
BEIS_DEFN.o:	HGRD_DEFN.F CGRID_SPCS.F BIOG_EMIS.F UTILIO_DEFN.F \
		$(BASE_INC)/CONST.EXT
BIOG_EMIS.o:	UTILIO_DEFN.F
DUST_EMIS.o:	AERO_DATA.F HGRD_DEFN.F UTILIO_DEFN.F GRID_CONF.F \
		$(BASE_INC)/CONST.EXT $(BASE_INC)/FILES_CTM.EXT
EMIS_DEFN.o:	GRID_CONF.F CGRID_SPCS.F BEIS_DEFN.F LTNG_DEFN.F \
		PT3D_DEFN.F UTILIO_DEFN.F AERO_EMIS.F AERO_DATA.F BIOG_EMIS.F \
		PTMAP.F $(BASE_INC)/CONST.EXT $(BASE_INC)/FILES_CTM.EXT
LTNG_DEFN.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/CONST.EXT
PT3D_DEFN.o:	UDTYPES.F GRID_CONF.F CGRID_SPCS.F STK_PRMS.F STK_EMIS.F \
		PTMET.F PTBILIN.F PTMAP.F UTILIO_DEFN.F VGRD_DEFN.F $(BASE_INC)/CONST.EXT \
		$(BASE_INC)/FILES_CTM.EXT
PTBILIN.o:	UDTYPES.F HGRD_DEFN.F VGRD_DEFN.F STK_PRMS.F
PTMAP.o:	UDTYPES.F CGRID_SPCS.F STK_EMIS.F AERO_DATA.F UTILIO_DEFN.F
PTMET.o:	UDTYPES.F STK_PRMS.F PTBILIN.F UTILIO_DEFN.F VGRD_DEFN.F \
		$(BASE_INC)/FILES_CTM.EXT
SSEMIS.o:	HGRD_DEFN.F AERO_DATA.F UTILIO_DEFN.F GRID_CONF.F AEROMET_DATA.F \
		$(BASE_INC)/FILES_CTM.EXT
STK_EMIS.o:	UDTYPES.F UTILIO_DEFN.F
STK_PRMS.o:	UDTYPES.F HGRD_DEFN.F UTILIO_DEFN.F
cropcal.o:	HGRD_DEFN.F UTILIO_DEFN.F
opemis.o:	VGRD_DEFN.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
tfabove.o:	HGRD_DEFN.F
tfbelow.o:	HGRD_DEFN.F UTILIO_DEFN.F $(BASE_INC)/CONST.EXT $(BASE_INC)/FILES_CTM.EXT
beis3.o:	BIOG_EMIS.F
chkgrid.o:	UTILIO_DEFN.F
czangle.o:	UTILIO_DEFN.F $(BASE_INC)/CONST.EXT
hrno.o:	HGRD_DEFN.F BIOG_EMIS.F UTILIO_DEFN.F
parsline.o:	UTILIO_DEFN.F
tmpbeis.o:	HGRD_DEFN.F UTILIO_DEFN.F BIOG_EMIS.F
wrdaymsg.o:	UTILIO_DEFN.F
fire_plmris.o:	$(BASE_INC)/CONST.EXT
openlayout.o:	HGRD_DEFN.F UTILIO_DEFN.F
oppt3d_diag.o:	GRID_CONF.F UTILIO_DEFN.F
plmris.o:	$(BASE_INC)/CONST.EXT
preplm.o:	UTILIO_DEFN.F
ungridb2.o:	HGRD_DEFN.F PTBILIN.F
CGRID_SPCS.o:	UTILIO_DEFN.F $(BASE_INC)/RXCM.EXT $(BASE_INC)/RXDT.EXT
PHOT_MOD.o:	CSQY_DATA.F UTILIO_DEFN.F $(BASE_INC)/CONST.EXT $(BASE_INC)/RXCM.EXT
aero_photdata.o:	CGRID_SPCS.F UTILIO_DEFN.F AERO_DATA.F SOA_DEFN.F \
		AEROMET_DATA.F $(BASE_INC)/RXCM.EXT
o3totcol.o:	UTILIO_DEFN.F
opphot.o:	GRID_CONF.F UTILIO_DEFN.F PHOT_MOD.F $(BASE_INC)/FILES_CTM.EXT \
		$(BASE_INC)/RXCM.EXT
phot.o:	CGRID_SPCS.F PCGRID_DEFN.F UTILIO_DEFN.F AERO_DATA.F PHOT_MOD.F \
		SOA_DEFN.F AEROMET_DATA.F $(BASE_INC)/FILES_CTM.EXT $(BASE_INC)/RXCM.EXT
hrcalcks.o:	$(BASE_INC)/RXCM.EXT
hrdriver.o:	HGRD_DEFN.F VGRD_DEFN.F CGRID_SPCS.F UTILIO_DEFN.F \
		hrdata_mod.F $(BASE_INC)/CONST.EXT $(BASE_INC)/FILES_CTM.EXT \
		$(BASE_INC)/EMISPRM.EXT $(BASE_INC)/RXCM.EXT $(BASE_INC)/PA_CTL.EXT
hrg1.o:	hrdata_mod.F
hrg2.o:	hrdata_mod.F
hrg3.o:	hrdata_mod.F
hrg4.o:	hrdata_mod.F
hrinit.o:	CGRID_SPCS.F UTILIO_DEFN.F hrdata_mod.F $(BASE_INC)/RXCM.EXT
hrprodloss.o:	hrdata_mod.F
hrrates.o:	hrdata_mod.F
hrsolver.o:	CGRID_SPCS.F UTILIO_DEFN.F hrdata_mod.F
AEROMET_DATA.o:	$(BASE_INC)/CONST.EXT
AERO_DATA.o:	CGRID_SPCS.F AEROMET_DATA.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
AERO_EMIS.o:	AERO_DATA.F GRID_CONF.F CGRID_SPCS.F SSEMIS.F DUST_EMIS.F \
		PRECURSOR_DATA.F UTILIO_DEFN.F PT3D_DEFN.F PTMAP.F $(BASE_INC)/CONST.EXT \
		$(BASE_INC)/FILES_CTM.EXT $(BASE_INC)/RXCM.EXT
PRECURSOR_DATA.o:	CGRID_SPCS.F UTILIO_DEFN.F AEROMET_DATA.F
SOA_DEFN.o:	AERO_DATA.F AEROMET_DATA.F CGRID_SPCS.F UTILIO_DEFN.F \
		$(BASE_INC)/RXCM.EXT
aero_depv.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F AERO_DATA.F \
		SOA_DEFN.F AEROMET_DATA.F MOSAIC_MOD.F LSM_MOD.F DEPV_DEFN.F \
		$(BASE_INC)/FILES_CTM.EXT $(BASE_INC)/RXCM.EXT
aero_driver.o:	GRID_CONF.F AERO_DATA.F PRECURSOR_DATA.F SOA_DEFN.F \
		AEROMET_DATA.F UTILIO_DEFN.F CGRID_SPCS.F $(BASE_INC)/FILES_CTM.EXT \
		$(BASE_INC)/RXCM.EXT $(BASE_INC)/PA_CTL.EXT
aero_subs.o:	AERO_DATA.F SOA_DEFN.F AEROMET_DATA.F UTILIO_DEFN.F \
		PRECURSOR_DATA.F $(BASE_INC)/CONST.EXT
getpar.o:	AERO_DATA.F AEROMET_DATA.F
hetchem.o:	AERO_DATA.F PRECURSOR_DATA.F AEROMET_DATA.F
opdiam.o:	GRID_CONF.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
opvis.o:	GRID_CONF.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
poaage.o:	AERO_DATA.F PRECURSOR_DATA.F $(BASE_INC)/RXCM.EXT
AQ_DATA.o:	AERO_DATA.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/CONST.EXT
aq_map.o:	CGRID_SPCS.F AQ_DATA.F AERO_DATA.F UTILIO_DEFN.F
aqchem.o:	AQ_DATA.F UTILIO_DEFN.F $(BASE_INC)/CONST.EXT $(BASE_INC)/RXCM.EXT
cldproc_acm.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/CONST.EXT \
		$(BASE_INC)/FILES_CTM.EXT
convcld_acm.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F AQ_DATA.F \
		$(BASE_INC)/CONST.EXT $(BASE_INC)/FILES_CTM.EXT
getalpha.o:	AERO_DATA.F $(BASE_INC)/CONST.EXT
hlconst.o:	UTILIO_DEFN.F
rescld.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/CONST.EXT \
		$(BASE_INC)/FILES_CTM.EXT
scavwdep.o:	CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/CONST.EXT
pa_init.o:	GRID_CONF.F PAGRD_DEFN.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT \
		$(BASE_INC)/PA_CTL.EXT $(BASE_INC)/PA_CMN.EXT $(BASE_INC)/PA_DAT.EXT
pa_irr.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F $(BASE_INC)/RXCM.EXT \
		$(BASE_INC)/PA_CMN.EXT
pa_irr_ctl.o:	GRID_CONF.F CGRID_SPCS.F PAGRD_DEFN.F $(BASE_INC)/PA_CMN.EXT
pa_mkhdr.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F PAGRD_DEFN.F \
		$(BASE_INC)/PA_CMN.EXT
pa_output.o:	GRID_CONF.F PAGRD_DEFN.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT \
		$(BASE_INC)/PA_CTL.EXT $(BASE_INC)/PA_CMN.EXT
pa_update.o:	GRID_CONF.F CGRID_SPCS.F PAGRD_DEFN.F AERO_EMIS.F \
		UTILIO_DEFN.F $(BASE_INC)/CONST.EXT $(BASE_INC)/FILES_CTM.EXT \
		$(BASE_INC)/EMISPRM.EXT $(BASE_INC)/PA_CTL.EXT $(BASE_INC)/PA_CMN.EXT
cksummer.o:	GRID_CONF.F CGRID_SPCS.F UTILIO_DEFN.F
lstepf.o:	UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
setup_logdev.o:	UTILIO_DEFN.F /pf/g/g260077/devel/M3HOME/CMAQ5.0.1/lib/x86_64/ifort/mpich/include/mpif.h
subhdomain.o:	UTILIO_DEFN.F
subhfile.o:	GRID_CONF.F UTILIO_DEFN.F $(BASE_INC)/FILES_CTM.EXT
