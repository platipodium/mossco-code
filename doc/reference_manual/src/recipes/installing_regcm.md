# Recipe #4 Installing the RegCM climate model 

	export REGCM_DIR=/your/path/to/regcm
	export ESMF_LIB=/your/path/to/libesmf.a

	export REGCM_ROOT=${REGCM_DIR}/RegCM-4.3.5.7
	export REGCM_GLOBEDAT=${REGCM_DIR}/data
	
	
Create the `REGCM_ROOT` directory (this variable is later used by RegCM), download and install the program, with ESMF enabled in your `./configure` options.
	
	mkdir -p ${REGCM_ROOT}	
	cd ${REGCM_DIR}
	wget --no-check-certificate https://gforge.ictp.it/gf/download/frsrelease/199/1215/RegCM-4.3.5.7.tar.gz
	tar xzf RegCM-4.3.5.7.tar.gz
	cd ${REGCM_ROOT}
	
Unfortunately, RegCM may need to be patched on your work station.  We have provided three patches that should be applied

	patch < makeinc.patch ${REGCM_ROOT}/makeinc
	patch < mod_esmf_atm.patch ${REGCM_ROOT}/Main/mod_esmf_atm.F90
	patch < mod_couplerr.patch ${REGCM_ROOT}/Main/mod_couplerr.F90

After patching, you can `configure` and `make` the climate model
	./configure --with-esmf=yes --with-roms=
	make && make install
	
	
	
	