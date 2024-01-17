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
import matplotlib.pyplot as plt

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = '/Users/Lemmen/temp/PET00.sns_2050-1x61-gf-empty'
    #filename = os.environ['MOSSCO_SETUPDIR'] + '/deep_lake/PET0.deep_lake-1x1-r1'

print ('Using ' + filename + ' ...')

with open(filename,'r') as fid:
  lines=fid.readlines()

timingdict={}

for line in lines:
  if not line.__contains__(' TRACE '):
    continue
  if not line.__contains__(' phase '):
    continue
  words=re.split('\s+',line)
  i=words.index('phase')
  pet=words[i-4]
  stage=words[i-1]
  component=words[i-3]
  if component == 'PET0':
    print (line)
    continue
  time=words[1]

  msecs=float(time[0:1])*86400000 + float(time[2:3])*3600000 + float(time[4:5])*60000 + float(time[7:9])
  if not component in timingdict:
    timingdict[component]={}
  if not stage in timingdict[component]:
    timingdict[component][stage]=[]

  timingdict[component][stage].append(msecs)

stats={}
for key,value in timingdict.items():
  stats[key]={}
  try:
    if 'initialized' in value:
      timediffs=numpy.array(value['initialized'])-numpy.array(value['initializing'] )
      timediff=numpy.nansum(timediffs)
      stats[key]['Initialization']={'all':timediffs, 'total': timediff, 'start': numpy.array(value['initializing'])}
    if 'finalized' in value:
      timediffs=numpy.array(value['finalized'])-numpy.array(value['finalizing'] )
      timediff=numpy.nansum(timediffs)
      stats[key]['Finalization']={'all':timediffs, 'total': timediff, 'start': numpy.array(value['finalizing'])}
    if 'ran' in value:
      timediffs=numpy.array(value['ran'])-numpy.array(value['running'] )
      timediff=numpy.nansum(timediffs)
      stats[key]['Run']={'all':timediffs, 'total': timediff, 'start': numpy.array(value['running'])}
  except:
      pass
# make piecharts
fig,ax=plt.subplots(figsize=(6,6))
fig.clf()
labels = timingdict.keys()
#labels.pop('toplevel')
fracs=[]
for component in labels:
  compsum=0.0
  for stage in ['Initialization', 'Run', 'Finalization']:
    if stage in stats[component]:
      compsum=compsum + stats[component][stage]['total']
  fracs.append(numpy.abs(compsum))

plt.pie(fracs, labels=labels, autopct='%1.1f%%', shadow=True, startangle=90)
plt.title('Total time spent by component', bbox={'facecolor':'0.8', 'pad':15})
#pylab.show()
plt.savefig('total_time_spent_by_component.pdf',transparent=True,format='pdf')
plt.close(fig)

# pycharts separated by phase
labels = timingdict.keys()
#labels.pop('toplevel')
for stage in ['Initialization', 'Run']: # , 'Finalization']:
  fig,ax=plt.subplots(figsize=(6,6))
  fracs=[]
  for component in labels:
    if stage in stats[component]:
      fracs.append(numpy.abs(stats[component][stage]['total']))
    else :
      fracs.append(0.0)
  plt.pie(fracs, labels=labels, autopct='%1.1f%%', shadow=True, startangle=90)
  plt.title(stage + ' time spent by component', bbox={'facecolor':'0.8', 'pad':15})
  plt.show()
  plt.savefig(stage.lower() + '_time_spent_by_component.pdf',transparent=True,format='pdf')
  plt.close(fig)

#fig=pylab.figure(5)
#fig.clf()
#
#for comp,compval in stats.iteritems():
#  for stage,stageval in compval.iteritems():
#    pylab.plot(stageval['start'],stageval['all'])
#