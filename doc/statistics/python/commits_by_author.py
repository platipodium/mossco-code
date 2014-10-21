# -*- coding: utf-8 -*-

import pylab
import csv

# Map of author names to columns

authorinstitute={'Carsten Lemmen':'HZG','Richard Hofmeister':'HZG',
  'Ulrich Körner':'HZG','Knut Klingbeil':'IOW','Hassan Nasermoaddeli':'BAW'}

authoralias={'Carsten Lemmen':'Carsten Lemmen', 'Richard Hofmeister':
  'Richard Hofmeister','Richard':'Richard Hofmeister','hofmeist':
  'Richard Hofmeister','Ulrich Koerner':'Ulrich Körner','Ulrich Körner':
  'Ulrich Körner','mhnaserm':'Hassan Nasermoaddeli',
  'Mohammed Hassan Nasermoaddeli':'Hassan Nasermoaddeli','Hassan Nasermoaddeli':
  'Hassan Nasermoaddeli','Knut':'Knut Klingbeil'}

fieldnames=['time','Carsten Lemmen','Richard','hofmeist','Knut',
  'Mohammed Hassan Nasermoaddeli','mhnaserm','Hassan Nasermoaddeli',
  'Richard Hofmeister','Ulrich Körner','Ulrich Koerner']
  
# Load data
fid=open('../gitstats/commits_by_author.dat','rb')
reader=csv.DictReader(fid,fieldnames=fieldnames,delimiter=' ')

time=[]
for row in reader:
  time.append(int(row['time']))

fid.close()

# Consolidate by author
contrib={}
# The slices will be ordered and plotted counter-clockwise.
for key,value in row.iteritems():
    if key=='time':
      continue
    if not authoralias.has_key(key):
      print 'Alias ' + key + ' not defined, skipped'
      continue
    else:
      author=authoralias[key]
 
    if contrib.has_key(author):
      contrib[author] = contrib[author] + int(value)
    else:
      contrib[author] = int(value)

authorcontrib=contrib

# Consolidate by institute
contrib={}
# The slices will be ordered and plotted counter-clockwise.
for key,value in row.iteritems():
    if key=='time':
      continue
    if not authoralias.has_key(key):
      print 'Alias ' + key + ' not defined, skipped'
      continue
    else:
      author=authoralias[key]
    
    if not authorinstitute.has_key(author):
      print 'Insitute for ' + author + ' not defined, skipped'
      continue
    else:
      institute=authorinstitute[author]
        
    if contrib.has_key(institute):
      contrib[institute] = contrib[institute] + int(value)
    else:
      contrib[institute] = int(value)

# make a square figure and axes
fig=pylab.figure(1, figsize=(6,6))
fig.clf()
ax = pylab.axes([0.1, 0.1, 0.8, 0.8])
labels = authorcontrib.keys()
fracs = authorcontrib.values()
pylab.pie(fracs, labels=labels, autopct='%1.1f%%', shadow=True, startangle=90)
pylab.title('Commits by author', bbox={'facecolor':'0.8', 'pad':15})
pylab.show()
pylab.savefig('commits_by_author.pdf',transparent=True,format='pdf')

# make a square figure and axes
fig=pylab.figure(2, figsize=(6,6))
fig.clf()
ax = pylab.axes([0.1, 0.1, 0.8, 0.8])
labels = contrib.keys()
fracs = contrib.values()
pylab.pie(fracs, labels=labels, autopct='%1.1f%%', shadow=True, startangle=90)
pylab.title('Commits by institute', bbox={'facecolor':'0.8', 'pad':15})
pylab.show()
pylab.savefig('commits_by_institute.pdf',transparent=True,format='pdf')



