from pylab import *
import numpy as np
from datetime import datetime
import sys

if len(sys.argv)>1:
    datfile=sys.argv[1]
else:
    datfile='output_0d.dat'

a=np.loadtxt(datfile,skiprows=1, comments='#', delimiter='\t',converters={0: datestr2num})

fh=open(datfile,'r')
fh.seek(0)
# remove hashmark and "time"
labels=fh.readlines()[1][1:].split('\t')[1:]
numvar=len(labels)

plottime=a[:,0]-a[0,0]
#labels=['swr','temp','salt','nut','phy','zoo','det']

monthslen=[31,28,31,30,31,30,31,31,30,31,30,31]
monthsnames=['J','F','M','A','M','J','J','A','S','O','N','D']
ticks=cumsum([ i for i in [0]+monthslen+monthslen[:-1]])
tickl=[ '   '+s for s in monthsnames+monthsnames]

f=figure(figsize=(6,10))
f.subplots_adjust(top=0.95,bottom=0.05,hspace=0.5)

for i in range(numvar):
   subplot(numvar,1,i+1)

   plot(plottime,a[:,i+1],'r-')
   title(labels[i],size=7.0)
   xlim(0,730)
   gca().set_xticks(ticks)
   gca().set_xticklabels([])

   ax  = gca()
   yt  = ax.get_yticks()
   ytl = ax.get_yticklabels()
   ax.set_yticks([yt[0],yt[-1]])
   ax.set_yticklabels([str(yt[0]),str(yt[-1])])

   gca().xaxis.grid(color='k',linestyle=':',linewidth=0.5)

gca().set_xticklabels(tickl)
xlabel('')
show()
#savefig('%s.png'%datfile[:-4])
