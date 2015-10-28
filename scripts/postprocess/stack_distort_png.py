#!/usr/bin/env python
# This script is is part of MOSSCO. It creates from a set of png maps for different
# variables a stacked and perspectively distorted compilation
#
# You could later animate the resulting files with a command like:
# mencoder mf://stack_distort_*.png  -mf fps=15:type=png -ovc lavc -lavcopts vcodec=mpeg4:mbd=2:trell -oac copy -o stack_distort.avi
#
# @copyright (C) 2015 Helmholtz-Zentrum Geesthacht
# @author Richard Hofmeister <richard.hofmeister@hzg.de>
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the

import os
import sys
import os.path

# User input: choose the variables to arrange (select 3)
variables=[
'mole_concentration_of_phosphate_in_soil',
#'Dissolved_Inorganic_Nitrogen_DIN_nutN_in_water',
'Dissolved_Inorganic_Phosphorus_DIP_nutP_in_water',
'Chl_chl_in_water',
]

# User input: Limit the timesteps (as date in YYYYMMDD format)
timesteps = range(20000301,20020526)
#timesteps = range(20000301,20010000)

for t in timesteps:

  if not os.path.isfile('%s_%8d.png'%(variables[0],t)) : continue
  if not os.path.isfile('%s_%8d.png'%(variables[1],t)) : continue
  if not os.path.isfile('%s_%8d.png'%(variables[2],t)) : continue

  print('  timestep %08d'%t)
  for var in variables:
  #      Each set of four floating point values represent a source image coordinate,
  # followed immediately by the destination image coordinate.
  # U1,V1 X1,Y1 U2,V2 X2,Y2 U3,V3 X3,Y3 ... Un,Vn Xn,Yn
    #persp_str='convert %s_%08d.png -mattecolor none -virtual-pixel transparent -background None -interpolate Spline +distort Perspective "0,0,300,0 0,576,0,350 864,0,914,20 864,576,664,396" %s_%08d_p.png'%(var,t,var,t)
    persp_str='convert %s_%08d.png -mattecolor none -virtual-pixel transparent -background None -interpolate Spline +distort Perspective "0,0,100,0 0,576,0,300 864,0,914,0 864,576,664,298" %s_%08d_p.png'%(var,t,var,t)
    #print(persp_str)
    os.system(persp_str)

  montagestr='convert -page 900x840'
  for i,var in enumerate(variables):
#    xoff=230-i*110
    xoff=180-i*70
#    yoff=600-i*295
    yoff=520-i*225
    montagestr+=' -page +%d+%d %s_%08d_p.png'%(xoff,yoff,var,t)

  montagestr+=' -background white -layers flatten stack_distort_%08d.png'%t
  #print(montagestr)
  os.system(montagestr)

  for var in variables:
    #print('delete %s_%04d_p.png'%(var,t))
    os.system('rm %s_%08d_p.png'%(var,t))
