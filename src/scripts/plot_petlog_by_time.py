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
import numpy as np
import pylab
from matplotlib import cm
import brewer2mpl

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
  filename = '/Volumes/Kiwi/mossco/output/PET0.NSBS153-1x153-3Dpelsedriv_TRACE'
  filename = os.environ['MOSSCO_SETUPDIR'] + '/deep_lake/PET0.deep_lake-1x1-getm--fabm_pelagic--fabm_s'
  #filename = os.environ['MOSSCO_SETUPDIR'] + '/deep_lake/PET0.deep_lake-1x1-river'

print 'Using ' + filename + ' ...'

(file_dir,file_name) = os.path.split(filename)
(file_base, file_ext)= os.path.splitext(file_name)

fid = file(filename,'rU')
lines=fid.readlines()
fid.close()

timingdict={}
mintime=-1

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
  
  msecs=float(time[0:2])*3600000 + float(time[2:4])*60000 + float(time[4:6])*1000 + float(time[7:10])
  if mintime<0 :
    mintime=msecs

  if not timingdict.has_key(component):
    timingdict[component]={}
  if not timingdict[component].has_key(stage):
    timingdict[component][stage]=[]

  timingdict[component][stage].append(msecs-mintime)

maxtime=msecs

fig=pylab.figure(1, figsize=(12,3))
fig.clf()
ax = pylab.axes([0.15, 0.15, 0.7, 0.8])


dtime=maxtime-mintime
n=len(timingdict.keys())
totals=np.ones(n)
totals[:]=0.0

colors=cm.rainbow(np.linspace(0,1,n))

i=0
for key,value in timingdict.iteritems():
  if value.has_key('initialized'):
    n=len(value['initialized'])
    if (len(value['initializing'])<n or len(value['initializing']) > n+1 ): 
      print i,key,'has unbalanced initialize phase, skipped.'
      continue
    timediffs=np.array(value['initialized'][0:n])-np.array(value['initializing'][0:n])   
    ax.bar(left=value['initializing'][0:n],height=np.multiply(value['initializing'][0:n],0.0)+0.8,
           width=timediffs,bottom=1.1+i,color=colors[i], edgecolor=colors[i])  
    print i,key,'spent',np.sum(timediffs),'ms in ',len(timediffs),'initialize calls'

    totals[i] = totals[i] + np.sum(timediffs)
    if key != 'toplevel':
      ax.bar(left=value['initializing'][0:n],height=np.multiply(value['initialized'][0:n],0.0)+0.8,
           width=timediffs,bottom=0.1,color=colors[i], edgecolor=colors[i])  

  else:
    print i,key,'has no init phase'
   
  if value.has_key('readrestarted'):
    n=len(value['readrestarted'])
    if (len(value['readrestarting'])<n or len(value['readrestarting']) > n+1 ): 
      print i,key,'has unbalanced readrestart phase, skipped.'
      continue
    timediffs=np.array(value['readrestarted'][0:n])-np.array(value['readrestarting'][0:n])   
    ax.bar(left=value['readrestarting'][0:n],height=np.multiply(value['readrestarting'][0:n],0.0)+0.8,
           width=timediffs,bottom=1.1+i,color=colors[i], edgecolor=colors[i])  
    print i,key,'spent',np.sum(timediffs),'ms in ',len(timediffs),'readrestart calls'

    totals[i] = totals[i] + np.sum(timediffs)
    if key != 'toplevel':
      ax.bar(left=value['readrestarting'][0:n],height=np.multiply(value['readrestarting'][0:n],0.0)+0.8,
           width=timediffs,bottom=0.1,color=colors[i], edgecolor=colors[i])  

  else:
    print i,key,'has no readrestart phase'
   
  if value.has_key('finalized'):
    n=len(value['finalized'])
    if (len(value['finalizing'])<n or len(value['finalizing']) > n+1 ): 
      print i,key,'has unbalanced finalize phase, skipped.'
      continue
    timediffs=np.array(value['finalized'][0:n])-np.array(value['finalizing'][0:n] )
    ax.bar(left=value['finalizing'][0:n],height=np.multiply(value['finalizing'][0:n],0.0)+0.8,
           width=timediffs,bottom=1.1+i,color=colors[i], edgecolor=colors[i])  
    print i,key,'spent',np.sum(timediffs),'ms in ',len(timediffs),'finalize calls'
    if key != 'toplevel':
      ax.bar(left=value['finalizing'][0:n],height=np.multiply(value['finalizing'][0:n],0.0)+0.8,
           width=timediffs,bottom=0.1,color=colors[i], edgecolor=colors[i])  
    totals[i] = totals[i] + np.sum(timediffs)
  else:
    print i,key,'has no finalize phase'

  if value.has_key('ran'):
    n=len(value['ran'])
    if (len(value['running'])<n or len(value['running']) > n+1 ): 
      print i,key,'has unbalanced run phase, skipped.'
      continue
  
    timediffs=np.array(value['ran'][0:n])-np.array(value['running'][0:n] )
    ax.bar(left=value['running'][0:n],height=np.multiply(value['running'][0:n],0.0)+0.8,
           width=timediffs,bottom=1.1+i,color=colors[i], edgecolor=colors[i])  
    print i,key,'spent',np.sum(timediffs),'ms in ',len(timediffs),'run calls'
    totals[i] = totals[i] + np.sum(timediffs)

    if key != 'toplevel':
      ax.bar(left=value['running'][0:n],height=np.multiply(value['running'][0:n],0.0)+0.8,
           width=timediffs,bottom=0.1,color=colors[i], edgecolor=colors[i])  
  else:
    print i,key,'has no run phase'

  i=i+1

i=0
for key in timingdict.keys():
    
  string="%5.1f %% (" % (totals[i]*100/dtime)
    
  if totals[i] < 1000:
    string += "%3.0f ms)" % (totals[i])
  elif  totals[i] < 1000*1000:
    string += "%3.0f s )" % (totals[i]/1000.0)
  elif  totals[i] < 1000*1000*60:
    string += "%3.0f m )" % (totals[i]/1000.0/60)
  else:
    string += "%3.0f h )" % (totals[i]/1000.0/3600)

    
  ax.text(dtime*1.02,1.5+i,string,verticalalignment='center',
          horizontalalignment='left')
  i=i+1

ax.set_xlim([0-0.01*dtime, 1.01*dtime])
labels = ['all']
labels.extend(timingdict.keys())
ax.set_xlabel('Time since simulation start (ms)')
ax.set_yticks(np.arange(0.5,len(labels),step=1),minor=False)
ax.set_yticklabels(labels)
#pylab.title('Time spent in each component', bbox={'facecolor':'0.8', 'pad':15})
#pylab.show()
pylab.savefig('petlog_by_time_' + file_ext + '.pdf',transparent=True,format='pdf')
pylab.savefig('petlog_by_time_' + file_ext + '.png',transparent=True,format='png')
pylab.close(fig)
