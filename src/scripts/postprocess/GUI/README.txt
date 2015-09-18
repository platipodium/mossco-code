0) The program includes:
	- folder: lib
	- folder: settings
	- file:   main.pyw
	- file:   matplotlibrc
	- file:   README.txt

1) Specify settings in file .\settings\settings_user.json, otherwise settings
   from file .\settings\settings_default.json will be used
   For help see .\settings\help.txt

2) Run the file main.pyw. In linux type in shell:
   $python main.pyw

3) If something fails, make sure nessesary libraries are installed.
   This program has been tested with following configuration:
   
   OS:
   - SUSE Linux/Windows
   
   Python:
   - Python 2.7.6
   
   Qt:
   - 4.8.5
   
   HDF5:
   - 1.8.13

   netCDF:
   - 4.3.2

   Python modules:
   - json 2.0.9
   - PyQt4 4.10.3
   - scipy 0.12.0
   - numpy 1.9.2
   - netCDF4 1.0.8
   - matplotlib 1.3.0
   - Basemap 1.0.8

4) The file matplotlibrc in current directory will overwrite all
   other matplotlib settings. If you want to use your own settings,
   make sure you have following two parameters in your matplotlibrc file:
   
   backend : qt4agg
   backend.qt4 : PyQt4
