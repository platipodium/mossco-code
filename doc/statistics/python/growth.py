# -*- coding: utf-8 -*-

from matplotlib import dates
import datetime
import pylab
import numpy as np
import csv
from matplotlib import pyplot


counters=['lines of code']#, 'files by date']
titles={'lines of code':'total lines of code','files by date':'total files'}

for counter in counters:

  fieldnames=['time',counter]
  with  open('../gitstats/' + counter.replace(' ','_') + '.dat','r') as fid: 
      reader=csv.DictReader(fid,fieldnames=fieldnames,delimiter=' ')
      fields=[]
      for row in reader:
          #print(np.int(row['time']),np.int(row[counter]))
          fields.append([np.int(row['time']),np.int(row[counter])])
          
          
  fields=np.array(fields)
#  fieldnames=row.keys()

  # convert epoch to matplotlib float format
  time=fields[:,0]
  dts = map(datetime.datetime.fromtimestamp, time)
  fds = dates.date2num(list(dts)) # converted
  hfmt = dates.DateFormatter('%y %m %d')

  fig=pylab.figure(1, figsize=(12,6))
  fig.clf()

  ax = pylab.axes([0.1, 0.2, 0.8, 0.7])
  value=fields[:,1]

  pyplot.plot(fds,value,'k-',linewidth=3)

  ax=pylab.gca()
  ax.xaxis.set_major_formatter(hfmt)
  ax.xaxis.set_major_locator(dates.MonthLocator())
  ax.set_ylim(bottom = 0)
  pyplot.xticks(rotation=45)

  pylab.title(titles[counter].capitalize(), bbox={'facecolor':'0.8', 'pad':15})
  #pylab.show()
  pylab.savefig(titles[counter].replace(' ','_') + '.pdf',transparent=True,format='pdf')
