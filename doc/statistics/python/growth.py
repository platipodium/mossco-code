# -*- coding: utf-8 -*-

from matplotlib import dates
import datetime
import pylab
import numpy
import csv
from matplotlib import pyplot

# Map of author names to columns


counters=['lines of code'] #, 'files by date']
titles={'lines of code':'total lines of code','files by date':'total files'}

for counter in counters:

  fieldnames=['time',counter]
  fid=open('../gitstats/' + counter.replace(' ','_') + '.dat','rb')
  reader=csv.DictReader(fid,fieldnames=fieldnames,delimiter=' ')
  fields=[]
  for row in reader:
    fields.append(map(numpy.int,row.values()))
  fid.close()
  fields=numpy.array(fields)
  fieldnames=row.keys()

  # convert epoch to matplotlib float format
  time=fields[:,fieldnames.index('time')]
  dts = map(datetime.datetime.fromtimestamp, time)
  fds = dates.date2num(dts) # converted
  hfmt = dates.DateFormatter('%y %m %d')

  fig=pylab.figure(1, figsize=(12,6))
  fig.clf()

  ax = pylab.axes([0.1, 0.2, 0.8, 0.7])
  value=fields[:,fieldnames.index(counter)]

  pyplot.plot(fds,value,'k-',linewidth=3)  
 
  ax=pylab.gca()
  ax.xaxis.set_major_formatter(hfmt)
  ax.xaxis.set_major_locator(dates.MonthLocator())
  ax.set_ylim(bottom = 0)
  pyplot.xticks(rotation=45)

  pylab.title(titles[counter].capitalize(), bbox={'facecolor':'0.8', 'pad':15})
  pylab.show()
  pylab.savefig(titles[counter].replace(' ','_') + '.pdf',transparent=True,format='pdf')
