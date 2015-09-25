#!/usr/bin/env python
# This script is is part of MOSSCO. It creates from an ESMF Log file output
# timing diagrams for all components
#
# @copyright (C) 2014, 2015 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the

import sys
import os
import re
import numpy
import pylab

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
     filename = os.environ['MOSSCO_SETUPDIR'] + '/deep_lake/PET0.deep_lake-1x1-r1'

print 'Using ' + filename + ' ...'

fid = file(filename,'rU')
lines=fid.readlines()
fid.close()

timingdict={}

for line in lines:
  if not line.__contains__(' TRACE '):
    continue
  if not line.__contains__(' phase '):
    continue
  words=re.split(' *',line)
  i=words.index('phase')
  pet=words[i-4]
  stage=words[i-1]
  component=words[i-3]
  if component == 'PET0': 
    print line
    continue     
  time=words[1]
  
  msecs=float(time[0:1])*86400000 + float(time[2:3])*3600000 + float(time[4:5])*60000 + float(time[7:9])
  if not timingdict.has_key(component):
    timingdict[component]={}
  if not timingdict[component].has_key(stage):
    timingdict[component][stage]=[]

  timingdict[component][stage].append(msecs)

stats={}
for key,value in timingdict.iteritems():
  stats[key]={}
  if value.has_key('initialized'):
    timediffs=numpy.array(value['initialized'])-numpy.array(value['initializing'] )
    timediff=numpy.nansum(timediffs)
    stats[key]['Initialization']={'all':timediffs, 'total': timediff, 'start': numpy.array(value['initializing'])}      
  if value.has_key('finalized'):
    timediffs=numpy.array(value['finalized'])-numpy.array(value['finalizing'] )
    timediff=numpy.nansum(timediffs)
    stats[key]['Finalization']={'all':timediffs, 'total': timediff, 'start': numpy.array(value['finalizing'])}
  if value.has_key('ran'):
    timediffs=numpy.array(value['ran'])-numpy.array(value['running'] )
    timediff=numpy.nansum(timediffs)
    stats[key]['Run']={'all':timediffs, 'total': timediff, 'start': numpy.array(value['running'])}

# make piecharts
fig=pylab.figure(1, figsize=(6,6))
fig.clf()
ax = pylab.axes([0.1, 0.1, 0.8, 0.8])
labels = timingdict.keys()
labels.remove('toplevel')
fracs=[]
for component in labels:
  compsum=0.0
  for stage in ['Initialization', 'Run', 'Finalization']:
    if stats[component].has_key(stage):
      compsum=compsum + stats[component][stage]['total']
  fracs.append(compsum)
  
pylab.pie(fracs, labels=labels, autopct='%1.1f%%', shadow=True, startangle=90)
pylab.title('Total time spent by component', bbox={'facecolor':'0.8', 'pad':15})
#pylab.show()
pylab.savefig('total_time_spent_by_component.pdf',transparent=True,format='pdf')
pylab.close(fig)

# pycharts separated by phase
labels = timingdict.keys()
labels.remove('toplevel')
for stage in ['Initialization', 'Run']: # , 'Finalization']:
  fig=pylab.figure(figsize=(6,6))
  fig.clf()
  ax = pylab.axes([0.1, 0.1, 0.8, 0.8])
  fracs=[]
  for component in labels:
    if stats[component].has_key(stage):
      fracs.append(stats[component][stage]['total'])
    else :
      fracs.append(0.0)
  pylab.pie(fracs, labels=labels, autopct='%1.1f%%', shadow=True, startangle=90)
  pylab.title(stage + ' time spent by component', bbox={'facecolor':'0.8', 'pad':15})
  #pylab.show()
  pylab.savefig(stage.lower() + '_time_spent_by_component.pdf',transparent=True,format='pdf')
  pylab.close(fig)

#fig=pylab.figure(5)
#fig.clf()
#
#for comp,compval in stats.iteritems():
#  for stage,stageval in compval.iteritems():
#    pylab.plot(stageval['start'],stageval['all'])
#




