# -*- coding: utf-8 -*-

from matplotlib import dates
import datetime
import pylab
import numpy as np
import csv
import sys
from matplotlib import pyplot

# Map of author names to columns

authorinstitute={'Carsten Lemmen':'HZG','Richard Hofmeister':'HZG',
  u'Ulrich Körner':'HZG','Knut Klingbeil':'IOW','Hassan Nasermoaddeli':'BAW',
  'Markus Kreus':'HZG', 'Onur Kerimoglu':'HZG', 'Kai Wirtz':'HZG',
  'Nils Weiher':'HZG','Ryan O\'Kuinghttons':'HZG', 'Johannes Bieser':'HZG'}

authoralias={'Johannes Bieser':'Johannes Bieser', 'Carsten Lemmen':'Carsten Lemmen', 'Richard Hofmeister':
  'Richard Hofmeister','Richard':'Richard Hofmeister','hofmeist':
  'Richard Hofmeister','Richard Hofmeister richard.hofmeister@hzg.de':'Richard Hofmeister','Richard H':'Richard Hofmeister',
  'Ulrich Koerner':u'Ulrich Körner','Ulrich Körner':
  u'Ulrich Körner','mhnaserm':'Hassan Nasermoaddeli','hnaserm':'Hassan Nasermoaddeli','Markus Kreus markus.kreus@baw.de':'Markus Kreus',
  'Mohammed Hassan Nasermoaddeli':'Hassan Nasermoaddeli','Hassan Nasermoaddeli':
  'Hassan Nasermoaddeli','Knut':'Knut Klingbeil','Kai Wirtz':'Kai Wirtz',
  'Onur Kerimoglu':'Onur Kerimoglu','Markus Kreus':'Markus Kreus','Onur':'Onur Kerimoglu',
  'Nils Weiher':'Nils Weiher','Kai Wirtz kai.wirtz@hzg.de':'Kai Wirtz',
  'Ryan O\'Kuinghttons':'Ryan O\'Kuinghttons'}

# Get committer names from one of the output gnuplot files
with open('../gitstats/lines_of_code_by_author.plot', 'r') as fid:
  for line in fid:
    sys.stdout.write('.')
  line=line.split('"')
  authornames=line[1:-1:2]

counters=['lines of code', 'commits']
titles={'lines of code':'lines of code changes','commits':'commits'}
colormap = "bgrcmy"

for counter in counters:

  fieldnames=['time']
  fieldnames.extend(authornames)
  fid=open('../gitstats/' + counter.replace(' ','_') + '_by_author.dat','rb')
  reader=csv.DictReader(fid,fieldnames=fieldnames,delimiter=' ')
  fields=[]
  for row in reader:
    fields.append(map(np.int,row.values()))
  fid.close()
  fields=np.array(fields)
  fieldnames=row.keys()

  authorfields={}
  for author in pylab.unique(authoralias.values()):
    af=[]
    for i in range(1,np.size(fieldnames)):
      if authoralias.has_key(fieldnames[i]):
        if authoralias[fieldnames[i]]==author:
          af.append(i)
    authorfields[author]=af

  print (authorfields)
  institutefields={}
  for institute in pylab.unique(authorinstitute.values()):
    #print institute
    af=[]
    for i in range(0,np.size(fieldnames)):
      if fieldnames[i]=='time':
        continue
      if authorinstitute.has_key(authoralias[fieldnames[i]]):
        if authorinstitute[authoralias[fieldnames[i]]]==institute:
          af.append(i)
    institutefields[institute]=af

  contrib={}
  for key in authorfields:
    if pylab.size(authorfields[key]) < 1: continue
    contrib[key]=fields[:,authorfields[key][0]]
    for i in range(1,pylab.size(authorfields[key])):
      contrib[key]=contrib[key] + fields[:,authorfields[key][i]]

  for key in institutefields:
    contrib[key]=fields[:,institutefields[key][0]]
    for i in range(1,pylab.size(institutefields[key])):
      contrib[key]=contrib[key] + fields[:,institutefields[key][i]]

  # convert epoch to matplotlib float format
  time=fields[:,fieldnames.index('time')]
  dts = map(datetime.datetime.fromtimestamp, time)
  fds = dates.date2num(dts) # converted
  hfmt = dates.DateFormatter('%y %m %d')

  # make a  a piechart
  fig=pylab.figure(1, figsize=(6,6))
  fig.clf()
  ax = pylab.axes([0.1, 0.1, 0.8, 0.8])
  labels = authorfields.keys()
  fracs=np.zeros((len(labels),1))
  for i,author in enumerate(labels):
    if author in contrib: fracs[i]=contrib[author][-1]

  fracs = fracs / sum(fracs) * 100.0
  sort_index = sorted(range(len(fracs)), key=fracs.__getitem__, reverse=True)

  pylab.pie(fracs, labels=labels, autopct='%1.1f%%', shadow=True, startangle=90)
  pylab.title(titles[counter].capitalize() + ' by author', bbox={'facecolor':'0.8', 'pad':15})
  #pylab.show()
  pylab.savefig(titles[counter].replace(' ','_') + '_by_author.pdf',transparent=True,format='pdf')

  # make a  a piechart
  fig=pylab.figure(2, figsize=(6,6))
  fig.clf()
  ax = pylab.axes([0.1, 0.1, 0.8, 0.8])
  labels = institutefields.keys()
  fracs=[]
  for institute in labels:
    fracs.append(contrib[institute][-1])
  pylab.pie(fracs, labels=labels, autopct='%1.1f%%', shadow=True, startangle=90)
  pylab.title(titles[counter].capitalize() + ' by institute', bbox={'facecolor':'0.8', 'pad':15})
  #pylab.show()
  pylab.savefig(titles[counter].replace(' ','_') + '_by_institute.pdf',transparent=True,format='pdf')

  fig=pylab.figure(3, figsize=(12,6))
  fig.clf()

  ax = pylab.axes([0.1, 0.2, 0.8, 0.7])
  labels = institutefields.keys()

  fracs=np.zeros((pylab.size(labels),time.size))
  for i in range(0,pylab.size(labels)):
    fracs[i,:]=contrib[labels[i]]

  pyplot.stackplot(fds,fracs,colors=colormap)

  p = []
  i = 0
  for _ in labels:
    p.append(pyplot.Rectangle((0, 0), 1, 1, fc=colormap[i]))
    i = (i + 1) % len(colormap)

  ax=pylab.gca()
  ax.xaxis.set_major_formatter(hfmt)
  ax.xaxis.set_major_locator(dates.MonthLocator())
  ax.set_ylim(bottom = 0)
  pyplot.xticks(rotation=45)
  ax.legend(p, labels, loc='upper left')

  pylab.title(titles[counter].capitalize(), bbox={'facecolor':'0.8', 'pad':15})
  #pylab.show()
  pylab.savefig(titles[counter].replace(' ','_') + '_by_time_and_institute.pdf',transparent=True,format='pdf')


  fig=pylab.figure(4, figsize=(12,6))
  fig.clf()

  ax = pylab.axes([0.1, 0.2, 0.8, 0.7])
  labels = authorfields.keys()

  fracs=np.zeros((pylab.size(labels),time.size))
  for i in range(0,pylab.size(labels)):
    if labels[i] in contrib: fracs[i,:]=contrib[labels[i]]

  pyplot.stackplot(fds,fracs,colors=colormap)
  p = []
  i = 0
  for _ in labels:
    p.append(pyplot.Rectangle((0, 0), 1, 1, fc=colormap[i]))
    i = (i + 1) % len(colormap)

  ax=pylab.gca()
  ax.xaxis.set_major_formatter(hfmt)
  ax.xaxis.set_major_locator(dates.MonthLocator())
  ax.set_ylim(bottom = 0)
  pyplot.xticks(rotation=45)
  ax.legend(p, labels, loc='upper left')

  pylab.title(titles[counter].capitalize(), bbox={'facecolor':'0.8', 'pad':15})
  #pylab.show()
  pylab.savefig(titles[counter].replace(' ','_') + '_by_time_and_author.pdf',transparent=True,format='pdf')

  fig=pylab.figure(5, figsize=(12,6))
  fig.clf()

  ax = pylab.axes([0.1, 0.2, 0.8, 0.7])
  labels = authorfields.keys()

  fracs=np.zeros((pylab.size(labels),time.size))
  for i in range(0,pylab.size(labels)):
    if labels[i] in contrib: fracs[i,:]=contrib[labels[i]]

  pyplot.plot(fds,np.sum(fracs,0),'k-',linewidth=3)

  ax=pylab.gca()
  ax.xaxis.set_major_formatter(hfmt)
  ax.xaxis.set_major_locator(dates.MonthLocator())
  ax.set_ylim(bottom = 0)
  pyplot.xticks(rotation=45)

  pylab.title(titles[counter].capitalize(), bbox={'facecolor':'0.8', 'pad':15})
  #pylab.show()
  pylab.savefig(titles[counter].replace(' ','_') + '.pdf',transparent=True,format='pdf')
