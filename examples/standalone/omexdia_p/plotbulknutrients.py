from pylab import *

f=open('output.dat','r')
vars = f.readline().split()
idxpor = vars.index('porosity()')

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

pon=[]
poc=[]
pop=[]
din=[]
dip=[]

for line in f.readlines():
    dat = line.split()
    if dat[1] == 'fluxes':
        time.append(float(dat[0])/86400./365.)
        if len(time)>1:
            # fill states
            zz = asarray(dz)
            pp = asarray(por)
            poc.append(sum((asarray(sdet)+asarray(fdet))*zz*pp))
            pon.append(sum((asarray(sdet)*NCsdet+asarray(fdet)*NCfdet)*zz*pp))
            pop.append(sum(asarray(pdet)*zz*pp))
            din.append(sum((asarray(amm)+asarray(nit))*zz*pp))
            dip.append(sum(asarray(pho)*zz*pp))
            
        dz=[]
        por=[]
        fdet=[]
        sdet=[]
        pdet=[]
        amm=[]
        nit=[]
        pho=[]
    else:
        dz.append(float(dat[2]))
        por.append(float(dat[idxpor]))
        fdet.append(float(dat[4]))
        sdet.append(float(dat[5]))
        pdet.append(float(dat[6]))
        pho.append(float(dat[7]))
        nit.append(float(dat[8]))
        amm.append(float(dat[9]))

time=asarray(time)[:-1]
poc=asarray(poc)
pon=asarray(pon)
pop=asarray(pop)
din=asarray(din)
dip=asarray(dip)

f.close()

lw=1.5

fig=figure(figsize=(8,8))
NCscale=0.2*0.5*(NCsdet+NCfdet)
plot(time,poc*NCscale,lw=lw,label='POC*%3.2f'%NCscale)
plot(time,(pon+din),lw=lw,label='total N')
plot(time,(pop+dip)*16,lw=lw,label='total P*16')

xlim(0,10)
ylabel(u'bulk POM [mmol/m\u00b2]')
xlabel('years')
legend(loc='upper left',frameon=False)

show()
