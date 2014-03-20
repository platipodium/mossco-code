omexdia_p: performs a 10 year simulation running only FABM's omexdia_p model in the fabm_sediment_driver (without ESMF) for a constant input flux of organic material from the sediment surface. The boundary conditions are:

variable                     | value  | unit
-----------------------------|--------|----------
temperature                  |  10.0  | degC
dissolved_phosphate          |   1.0  | mmolP/m**3
dissolved_nitrate            |  10.0  |mmolN/m**3
dissolved_ammonium           |   0.0  | mmolN/m**3
dissolved_oxygen             | 250.0  | mmolO2/m**3
dissolved_reduced_substances |   0.0  | -mmolO2/m**3
fast_detritus_C_upward_flux  |  -5.0  | mmolC/m**2/d
slow_detritus_C_upward_flux  |  -5.0  | mmolC/m**2/d
detritus-P_upward_flux       |  -0.08 | mmolP/m**2/d

All input files are contained in $MOSSCO_DIR/examples/standalone/omexdia_p. The example runs inside the example directory:
    ./omexdia_p
If a python+matplotlib environment are available, then the results can be displayed by:
    python plotsed1d.py
