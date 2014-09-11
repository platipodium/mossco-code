from pylab import *

f=open('output.dat','r')
vars = f.readline().split()
idxdenit = vars.index('hzg_omexdia_p_denit')

fluxes=[]
time=[]

varn  = ['fastDet','slowDet','Pdet','phosphate','nitrate','ammonium','oxygen/10','ODU']
units = [u'mmolC/m\u00b2/d',u'mmolC/m\u00b2/d',u'mmolP/m\u00b2/d',u'mmolP/m\u00b2/d',u'mmolN/m\u00b2/d',u'mmolN/m\u00b2/d',u'mmolO2/m\u00b2/d',u'-mmolO2/m\u00b2/d']

for line in f.readlines():
    dat = line.split()
    if dat[1] == 'fluxes':
        time.append(float(dat[0])/86400./365.)
        if len(fluxes)>0:
            fluxes[-1][-1] = sum(asarray(dz)*asarray(denit))
        fluxes.append([ float(item)*86400. for item in dat[2:10]])
        fluxes[-1].append(0.0)
        dz=[]
        denit=[]
    else:
        dz.append(float(dat[2]))
        denit.append(float(dat[idxdenit]))
fluxes[-1][-1] = sum(asarray(dz)*asarray(denit))

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

show()
