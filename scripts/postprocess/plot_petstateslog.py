#!/usr/bin/env python
# This script is is part of MOSSCO. It creates from an ESMF Log file output
# timing diagrams for all components
#
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the

import sys
import os
import re
import numpy as np
import pylab

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
  filename = '/Volumes/Kiwi/mossco/output/PET0.NSBS153-1x153-3Dpelsedriv_TRACE'
  filename = os.environ['MOSSCO_SETUPDIR'] + '/deep_lake/PET0.states_deep_lake-1x1-getm--fabm_pelagic--fabm_sediment--river--porosity--netcdf'
  #filename = os.environ['MOSSCO_SETUPDIR'] + '/deep_lake/PET0.deep_lake-1x1-river'

print 'Using ' + filename + ' ...'

(file_dir,file_name) = os.path.split(filename)
(file_base, file_ext)= os.path.splitext(file_name)

fid = file(filename,'rU')
lines=fid.readlines()
fid.close()

timingdict={}
mintime=-1
end_index=len(lines)+1

for i,line in enumerate(lines):
  if line.__contains__('====== Status at end of first run loop  ======'):
    start_index=i+1
    break
for i,line in enumerate(lines[start_index:-1]):
  if line.__contains__('====== Status at end of first run loop  ======'):
    end_index=i+start_index+1
    break

print start_index,end_index

componentDict={}
fieldDict={}
componentSet=set([])
for line in lines[start_index:]:  
  words=re.split(' *',line)
  if words[-4] == 'States':
    component=words[-2]
    componentSet.add(component)
    continue

  if not line.__contains__('field'): continue
  if not words[5] == 'field': continue
      
  state=words[4]
  field=words[6]
  if field[-1]=='/':
    field='bundle/'+words[7]
  purpose=state[-6:]

  if not componentDict.has_key(component):
    componentDict[component]={'purpose':{},'size':0}
    
  if not componentDict[component]['purpose'].has_key(purpose):
    componentDict[component]['purpose'][purpose]=[]
    
  componentDict[component]['purpose'][purpose].append(field)
    
  if not fieldDict.has_key(field):
   
    fieldDict[field]={'origin':'', 'states':[]}

  fieldDict[field]['states'].extend(component+purpose)
  fieldDict[field]['origin']=re.split('[\[*\]]',field)[1]
  
maxlen=0
for componentName,component in componentDict.iteritems():
  n=0
  for key,value in component['purpose'].iteritems():
    n=n+len(value)
  component['size']=n
  maxlen=np.max([n,maxlen])      

nComponents=len(componentDict)


fig=pylab.figure(1, figsize=(3*nComponents,int(maxlen/10.)))
#fig=pylab.figure(1, figsize=(12,12))
fig.clf()
ax = pylab.axes([0.05, 0.05, 0.9, 0.9])


# distribute components in circle

colors='rgbcmykrgbocmykrgbcmyk'
colorDict={}

for i,componentName in enumerate(componentDict):
  colorDict[componentName]=colors[i]

#ax.plot(0,0,'ko')
for i,componentName in enumerate(componentDict):

  component=componentDict[componentName]    
  #if componentName != 'getm': continue   
   
  radius=10
  # Cirular layout
  x=radius*np.cos(2*np.pi/nComponents*(i+1))
  y=radius*np.sin(2*np.pi/nComponents*(i+1))

  # Linear layout
  x=-radius + (i-1)*3*radius/(nComponents)
  y=radius
  
  ax.text(x,y,componentName,fontsize=25,color=colors[i]) 
  print '==========='
  print componentName

  offset=0.4

  for key,value in component['purpose'].iteritems():
    for j,field in enumerate(value):
      xpos=x
      ypos=y-j*0.17-offset
      #ax.plot(xpos,ypos,'ko')
      ax.text(xpos,ypos,key[0]+' '+field,fontsize=4,color=
        colorDict[fieldDict[field]['origin']])
      print xpos,ypos, '  '+key[0]+' '+field
    offset=offset+j*0.17
 
ax.set_xlim([-10, 15])
ax.set_ylim([-15, 10])
ax.set_yticks([],minor=False)
ax.set_xticks([],minor=False)
ax.set_axis_off()
      
pylab.savefig('petstateslog' + file_ext + '.pdf',transparent=True,format='pdf')
pylab.savefig('petstateslog' + file_ext + '.png',transparent=True,format='png')
pylab.close(fig)

