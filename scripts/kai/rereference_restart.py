import sys
import os
import math as m

def jd(year,month=1,day=1):
   """
   calculate julianday from year, month and day
   """
   igreg=15+31*(10+12*1582)
   if (month > 2):
      jm = month+1
      jy = year
   else:
      jy = year-1
      jm = month+13

   julian = int(m.floor(365.25*jy)+m.floor(30.6001*jm)+day+1720995)
   if (day+31*(month+12*year) >= igreg):
       ja = int(0.01*jy)
       julian = julian+2-ja+int(0.25*ja)
   return julian

if __name__ == '__main__':

  if len(sys.argv)!=3:
    print "rereference_restart.py HOTSTART_PATH YEAR"
    sys.exit(1)
  else:
    fpath=sys.argv[1]
    filelist=os.listdir(sys.argv[1])
    newyear=int(sys.argv[2])
    newjd=jd(newyear,1,1)

    for filen in filelist:
       if filen[-3:]=='.in':
          fullfile=os.path.join(fpath,filen)
          os.system('ncap -O -s "julianday = %d" %s %s' % (newjd,fullfile,fullfile))


