from pylab import *

f=open('output.dat','r')
vars = f.readline().split()
idxdenit = vars.index('hzg_omexdia_p_denit')
idxpor = vars.index('porosity()')

fluxes=[]
time=[]

varn  = ['fastDet','slowDet','Pdet','phosphate','nitrate','ammonium','oxygen/10','ODU']
units = [u'mmolC/m\u00b2/d',u'mmolC/m\u00b2/d',u'mmolP/m\u00b2/d',u'mmolP/m\u00b2/d',u'mmolN/m\u00b2/d',u'mmolN/m\u00b2/d',u'mmolO2/m\u00b2/d',u'-mmolO2/m\u00b2/d']

# get N:C ratios from fabm_sed.nml
NCfdet=0.2
NCsdet=0.01
ff = open('fabm_sed.nml')
for line in ff.readlines():
  st = line.split()
  if len(st)==3:
    if st[0]=='NCrFdet':
      NCfdet=float(st[2])
    elif st[0]=='NCrSdet':
      NCsdet=float(st[2])
ff.close()
#print('NCfdet: %5.2f, NCsdet: %5.2f'%(NCfdet,NCsdet))

for line in f.readlines():
    dat = line.split()
    if dat[1] == 'fluxes':
        time.append(float(dat[0])/86400./365.)
        if len(fluxes)>0:
            fluxes[-1][-1] = sum(asarray(dz)*asarray(denit)*asarray(por))
        fluxes.append([ float(item)*86400. for item in dat[2:10]])
        fluxes[-1].append(0.0)
        dz=[]
        por=[]
        denit=[]
    else:
        dz.append(float(dat[2]))
        por.append(float(dat[idxpor]))
        denit.append(float(dat[idxdenit]))
fluxes[-1][-1] = sum(asarray(dz)*asarray(denit)*asarray(por))

fluxes=asarray(fluxes)
time=asarray(time)

fluxes[:,6] += -fluxes[:,7]
fluxes[:,6] = fluxes[:,6]/10.

f.close()

fig=figure(figsize=(8,8))
for i in range(3,7):

    plot(time,fluxes[:,i],lw=1.5,label=varn[i]+' -> %5.2f [%s]'%(fluxes[-1,i],units[i]))

plot(time,-fluxes[:,8],lw=1.5,label='denitrification -> %5.2f [%s]'%(fluxes[-1,8],u'mmolN/m\u00b2/d'))

ylim(-2,2)
xlim(0,10)
ylabel(u'fluxes [mmol/m\u00b2/d]')
xlabel('years')
legend()
title(u'constant flux of %5.2f [mmolP/m\u00b2/d], %5.2f [mmolN/m\u00b2/d]'%(fluxes[-1,2],fluxes[-1,0]*NCfdet+fluxes[-1,1]*NCsdet))

show()
