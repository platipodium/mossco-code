# -*- coding: utf-8 -*-
"""
# This script is is part of MOSSCO. It schedules restarts of
# generic MOSSCO examples.
#
# @copyright (C) 2015, 2016, 2017 Helmholtz-Zentrum Geesthacht
# @author Richard Hofmeister <richard.hofmeister@hzg.de>
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#
"""
import os
import sys
import glob
from datetime import datetime,timedelta
import time
import subprocess

start = '2003-01-01 01:00:00' # time to start/restart the simulation
stop  = '2003-01-01 05:00:00'
init  = '2003-01-01 00:00:00' # time for initialization without hotstart

number_of_processors = 2
runid = 'restarttest'
output_directory = os.path.join(os.getcwd(),'Results',runid)

# output will be stored in directories: hot_000,hot_001...hot_$n
# if you like ro start with a certain number $n, then set start_index
start_index = 0

# by default all components will be hotstarted, if the start time is not
# init time. you can switch off to hotstart GETM with the switch hotstart_getm_at_start:
hotstart_getm_at_start=False

# Tested examples
# 1. just getm
#mossco_example='getm--netcdf'
#output_component='netcdf'

# 2. getm + fabm_pelagic
#mossco_example = 'getm--fabm_pelagic--restart'
#output_component = 'mossco_gfr'

# 3. getm + fabm_sediment
#mossco_example = 'getm--fabm_sediment--restart'
#output_component = 'mossco_gfr'

# 3. getm + fabm_pelagic + fabm_sediment
mossco_example = 'gfs'
output_component = 'mossco_gfs'

# if restart_directory is not set, the first simulation period
# will not restart, but initialize completely from namelist
# if restart_directory is set, the script assumes all restart
# input files in the restart_directory, you possibly want to set a
# different runid in order to avoid overwriting the previous results

restart_directory = os.path.join(os.getcwd(),'Results','snstest','hot_000')

# ----------------------
# do not change below this line

def increment_months(T,months=1):
  if T.month+months>12:
    addyear = (months+T.month)/12
    newmonth = (months+T.month)%12
  else:
    addyear = 0
    newmonth = T.month + months
  return datetime(T.year+addyear,newmonth,T.day,T.hour,T.minute,T.second)

def increment_hours(T,hours=1):
  return T+timedelta(0,3600.*hours)

def increment_days(T,days=1):
  return T + timedelta(days)

def replace_line(filename,pattern,newtext):
  backupfile=filename+'.bak'
  rc=os.system('mv %s %s'%(filename,backupfile))
  if rc != 0:
    print('Could not replace line in file %s'%(filename))
    quit()
  f = open(backupfile,'r')
  fn = open(filename,'w')
  for l in f.readlines():
    if pattern in l:
      fn.write("   %s\n"%newtext)
    else:
      fn.write(l)
  f.close()
  fn.close()

def create_cfg(filename, content):
  f = open(filename,'w')
  f.write(content)
  f.close()

def create_mossco_run(start,stop):
  f = open('mossco_run.nml','w')
  f.write('''
&mossco_run
 title = '%s',
 start = '%s',
 stop= '%s',
 loglevelzero = 'all',
 loglevel = 'all',
 logflush = .true.,
/
  '''%(runid,start.strftime(tformat),stop.strftime(tformat)))
  f.close()

tformat = '%Y-%m-%d %H:%M:%S'

Tstart = datetime.strptime(start, tformat)
Tstop  = datetime.strptime(stop, tformat)
Tinit  = datetime.strptime(init, tformat)
Tcurr = Tstart
i = start_index

# prepare filesystem
rc=os.system('mkdir -p %s'%(output_directory))
if (rc != 0):
  print('Could not create directory %s'%(output_directory))
  quit()

while Tcurr<Tstop:
  #Tnext = increment_months(Tcurr,1)
  #Tnext = increment_days(Tcurr,1)
  Tnext = increment_hours(Tcurr,1)
  if Tnext>Tstop:
    Tnext = Tstop

  print("%s: run from %s to %s"%(sys.argv[0],Tcurr.strftime(tformat),Tnext.strftime(tformat)))

  # prepare filesystem
  outdir = '%s/hot_%03d'%(output_directory,i)
  rc=os.system('mkdir -p %s'%(outdir))
  if (rc != 0):
    print('Could not create directory %s'%(outdir))
    quit()

  print("%s: created output directory %s"%(sys.argv[0],outdir))

  # write mossco_run.nml
  create_mossco_run(Tcurr,Tnext)
  print("%s: created mossco_run.nml"%(sys.argv[0]))
  rc=os.system('cp mossco_run.nml %s'%outdir)
  if (rc != 0):
    print('Could not copy mossco_run.nml')
    quit()

  # write output component.cfg
  create_cfg(output_component+'.cfg','filename: %s/%s.nc\n'%(outdir,output_component))
  rc=os.system('cp %s.cfg %s'%(output_component,outdir))
  if (rc != 0):
    print('Could not copy %s.cfg'%(output_component))
    quit()

  # set common GETM parameters:
  replace_line('getm.inp',' out_dir =',"out_dir = '%s',"%outdir)

  # set restart filename for fabm components in restart.cfg
  if Tcurr == Tinit or 'restart_directory' not in locals():
    replace_line('getm.inp',' hotstart =','hotstart = .false.,')
    replace_line('getm.inp',' save_initial =','save_initial = .true.,')
    #rc=os.system('rm -f restart_soil.cfg restart_water.cfg')
    text = 'filename: %s/%s.nc\ninclude: *_in_soil\ncheckFile: .false.\n'%(restart_directory,output_component)
    create_cfg('restart_soil.cfg',text)
    text = 'filename: %s/%s.nc\ninclude: *_in_water\ncheckFile: .false.\n'%(restart_directory,output_component)
    create_cfg('restart_water.cfg',text)
    print("%s: starting from initial conditions"%(sys.argv[0]))
  else:
    if not(Tcurr == Tinit):
      if not(Tcurr == Tstart and 'restart_directory' in locals()):
        restart_directory = output_directory+'/hot_%03d'%(i-1)
      print("%s: starting from restart directory"%(sys.argv[0]))

    text = 'filename: %s/%s.nc\ninclude: *_in_soil\ncheckFile: .true.\n'%(restart_directory,output_component)
    create_cfg('restart_soil.cfg',text)
    rc=os.system('cp restart_soil.cfg %s'%outdir)
    if (rc != 0):
      print('Could not restart_soil.cfg')
      quit()

    text = 'filename: %s/%s.nc\ninclude: *_in_water\ncheckFile: .true.\n'%(restart_directory,output_component)
    create_cfg('restart_water.cfg',text)
    rc=os.system('cp restart_water.cfg %s'%outdir)
    if (rc != 0):
      print('Could not restart_water.cfg')
      quit()

    # Do not spinup sediment in subsequent runs
    replace_line('run_sed.nml','presimulation_years=','  presimulation_years = 0,')
    replace_line('run_sed.nml','presimulation_years =','  presimulation_years = 0,')

    # prepare GETM's namelists:
    if (hotstart_getm_at_start or not(Tcurr == Tstart)):
      print('%s: restart GETM from hotstart files'%(sys.argv[0]))
      print(Tcurr)
      print(Tstart)
      print(hotstart_getm_at_start)
      replace_line('getm.inp',' hotstart =','hotstart = .true.,')
      replace_line('getm.inp',' save_initial =','save_initial = .false.,')
      # copy GETM's restart files
      rc=os.system('cd %s; for f in `ls restart*.out`; do cp ${f} %s/${f%%\\.out}.in; done'%(restart_directory,outdir))
      if (rc != 0):
        print('Could not copy getm restart files from %s to %s'%(restart_directory,outdir))
        quit()
    else:
      replace_line('getm.inp',' hotstart =','hotstart = .false.,')
      replace_line('getm.inp',' save_initial =','save_initial = .true.,')


  # run MOSSCO
  print('calling "mossco -b -sJ -t %s -n%d %s"'%(runid,number_of_processors,mossco_example))
  rc=os.system('mossco -z0:30:0 -b -sJ -t %s -n%d %s'%(runid,number_of_processors,mossco_example))

  replace_line('slurm.sh','copy-from','cp -f %s/*.* . # copy-from\n'%(outdir)) 
  replace_line('slurm.sh','copy-to','cp -f *.nml *.cfg *.sh *.inp %s # copy-to\n'%(outdir)) 

  
  rc=os.system('cp slurm.sh getm.inp mossco_run.nml %s'%(outdir))

  # done with the current period
  Tcurr = Tnext
  i = i+1

# Now iterate again, but this time submit dependen jobs in the queue

Tcurr = Tstart
i = start_index

while Tcurr<Tstop:

  outdir = '%s/hot_%03d'%(output_directory,i)

  if Tcurr == Tstart:
    print('calling "sbatch --parsable %s/slurm.sh"'%(outdir))
    jobid = int(subprocess.check_output(['sbatch','--parsable','%s/slurm.sh'%(outdir)]))
  else:
    print('calling "sbatch --parsable --dependency=afterok:%d %s/slurm.sh"'%(jobid,outdir))
    jobid = int(subprocess.check_output(['sbatch','--parsable','--dependency=afterok:%d'%(jobid),'%s/slurm.sh'%(outdir)]))

  time.sleep(2)
  Tnext = increment_hours(Tcurr,1)
  if Tnext>Tstop:
    Tnext = Tstop

  Tcurr = Tnext
  i = i + 1
