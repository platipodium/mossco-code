#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# This script is is part of MOSSCO. It creates from YAML descriptions of
# couplings a toplevel_component.F90 source file
#
# @copyright (C) 2014, 2015, 2016, 2017, 2018 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen <carsten.lemmen@hzg.de>
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.

from __future__ import absolute_import, division, unicode_literals
import sys
import os

# Define a generic iterable ver list or dict
def sequential_iterator(obj):
    return obj if isinstance(obj, dict) else xrange(len(obj))

try:
  import yaml

  class Loader(yaml.Loader):
    # The loader class was suggested by David Hall (Oxford)
    # on https://higgshunter.wordpress.com, and adapted to python3
    def __init__(self, stream):
        self._root = os.path.split(stream.name)[0]
        super(Loader, self).__init__(stream)
        Loader.add_constructor('!include', Loader.include)
        Loader.add_constructor('!import',  Loader.include)

    def include(self, node):
        if   isinstance(node, yaml.ScalarNode):
            return self.extractFile(self.construct_scalar(node))

        elif isinstance(node, yaml.SequenceNode):
            result = []
            for filename in self.construct_sequence(node):
                result += self.extractFile(filename)
            return result

        elif isinstance(node, yaml.MappingNode):
            result = {}
            for k,v in self.construct_mapping(node).items():
                result[k] = self.extractFile(v)
            return result

        else:
            print ("Error:: unrecognised node type in !include statement")
            raise yaml.constructor.ConstructorError

    def extractFile(self, filename):
        filepath = os.path.join(self._root, filename)
        with open(filepath, 'r') as f:
            return yaml.load(f, Loader)

except:
    print('Please install the python-yaml package or set your PYTHONPATH variable\n')
    print('to the location of the python yaml package.')
    sys.exit(1)

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
     filename='gotm--fabm_pelagic--fabm_sediment'

if not filename.endswith('yaml'):
  filename = filename + '.yaml'

print (sys.argv, len(sys.argv))
if not os.path.exists(filename):
    print ('File ' + filename + ' does not exist.')
    sys.exit(1)

with open(filename,'rU') as fid:
    print ('Using ' + filename + ' ...')
    config = yaml.load(fid)

# Search for the key with name "coupling".  If part of the filename is the word "coupling" then assume that the first item on the list read is the name of the coupling
coupling_name = os.path.splitext(os.path.basename(filename))[0]
make_path = os.path.dirname( os.path.realpath(__file__) )
#IamLocal = os.path.relpath( os.getcwd() , make_path ) == '.'
if os.getcwd() == make_path:
  coupling_exe = coupling_name
else:
  coupling_exe  = os.path.join ( os.getcwd() , coupling_name )
variables = []
coupling_properties = []

if not type(config) is dict:
  print ('File ' + filename + ' does not contain data or does not contain a dictionary.')
  sys.exit(1)

if 'author' in config.keys():
    author = config.pop('author')
else:
    author = 'Carsten Lemmen <carsten.lemmen@hzg.de>'

if 'copyright' in config.keys():
    copyright = config.pop('copyright')
else:
    copyright = 'Copyright (C) 2014, 2015, 2016, 2017, 2018 Helmholtz-Zentrum Geesthacht'

if 'dependencies' in config.keys():
  dependencies = config.pop('dependencies')
else:
  dependencies=[]

if 'instances' in config.keys():
  instances = config.pop('instances')
else:
  instances=[]

componentList=[]
gridCompList=[]
cplCompList=['link_connector']
couplingList=[]
petList=[]
foreignGrid={}

intervals =[]
directions = []

if not 'coupling' in config.keys():
  print ('File ' + filename + ' must contain a coupling dictionary.')
  print ('Try adding a first line consisting only of the word "coupling:".')
  sys.exit(1)

coupling = config.pop("coupling")

# Make it a list in any case
if not (type(coupling) is list):
  coupling=[coupling]

if len(coupling)<1:
  print ('File ' + filename + ' contains an empty coupling list.')
  print (coupling)
  sys.exit(1)

# Loop over the list of couplings.  Each entry in this list is a dictionary
# that has at least the key 'components:'
# todo: we could shortcut this by allowing comp1:comp2 to
# Set a default coupling alarm interval of 6 minutes
intervals=['6 m'] * len(coupling)

for i, item in enumerate(coupling):
    if type(item) is dict:
        if 'components' in item.keys():
            gridCompList.extend([item["components"][0], item["components"][-1]])
            n=len(item["components"])
            if n>2:
                couplingList.append(item["components"])
            elif n==2:
                couplingList.append([item["components"][0], "link_connector", item["components"][-1]])
                cplCompList.append("link_connector")
            for i in range(1,n-1):
                cplCompList.append(item["components"][i])
            if 'interval' in item.keys():
                intervals[i] = item["interval"]
            if 'direction' in item.keys():
                directions.append(item["direction"])
        else:
          gridComplist.extend(item.keys())
          gridCompList.extend(item.values())
          for key,value in item.items():
             couplingList.append(key, "link_connector",value)
          cplCompList.append("link_connector")

    else:
        print ('Warning, dictionary expected for item ' + item + ', it is of type ',  type(item))

gridCompSet=set(gridCompList)
gridCompList=list(gridCompSet)
cplCompSet=set(cplCompList)
cplCompList=list(cplCompSet)
componentSet=gridCompSet.union(cplCompSet)
componentList=list(componentSet)

# if there are any dependencies specified, go through the list of components
# and sort this list
if type(dependencies) is dict:
  dependencies = list(dependencies)

gridOrder=[]
for item in dependencies:
  for key,value in item.items():
    if type(value) is list:
      value=value[0]
    if type(value) is dict:
      if 'grid' in value.keys():
        donator=value['component']
        if key not in gridOrder:
          gridOrder.append(key)
        if donator not in gridOrder:
          gridOrder.insert(gridOrder.index(key),donator)
        if gridOrder.index(donator) > gridOrder.index(key):
          print ("ERROR: cyclic grid dependencies")
          sys.exit(1)

dependencyDict={}
for component in componentSet:
    for item in dependencies:
        compdeps=[]
        if type(item) is dict:
          if not component in item.keys():
            continue
          for jtem in item.values():
              if type(jtem) is list and len(jtem) == 1:
                  jtem=jtem[0]
              if type(jtem) is str:
                 compdeps.append(jtem)
              elif (type(jtem) is dict) and 'component' in jtem.keys():
                 compdeps.append(jtem['component'])
                 if 'grid' in jtem.keys():
                    foreignGrid[list(item.keys())[0]]=jtem['grid']
          for compdep in compdeps:
            if componentList.index(component)< componentList.index(compdep):
              if component in gridOrder and compdep in gridOrder:
                if gridOrder.index(component) < gridOrder.index(compdep):
                  continue
              c=componentList.pop(componentList.index(component))
              componentList.insert(componentList.index(compdep)+1,c)
          if list(item.keys())[0] in dependencyDict.keys():
            dependencyDict[list(item.keys())[0]].extend(compdeps)
          else:
            dependencyDict[list(item.keys())[0]]=compdeps

for key,value in dependencyDict.items():
    unique=[]
    for item in value:
      if item not in unique:
        unique.append(item)
    dependencyDict[key]=unique


if 'rename_connector' in componentList:
  c=componentList.pop(componentList.index('rename_connector'))
  componentList.insert(0,c)

if 'link_connector' in componentList:
  c=componentList.pop(componentList.index('link_connector'))
  componentList.insert(0,c)

# Create dictionary for component names (instanceDict) and for petLists that
# instances of these components run on.  Get this information from the
# yaml 'instances' dictionary/list
instanceDict={}
instancePetDict={}

if type(instances) is list:
  for i in range(0,len(instances)):
    item=instances[i]
    if 'component' in item.keys():
       instanceDict[list(item.keys())[0]]=item['component']
    else:
      instanceDict[list(item.keys())[0]]=list(item.values())[0]

    if 'petList' in item.keys():
      instancePetDict[list(item.keys())[0]]=item['petList']
else:
  for key,value in instances.items():
    if type(value) is str:
      instanceDict[key] = value
    elif type(value) is dict and 'component' in value.keys():
      instanceDict[key] = value['component']
      if 'petList' in value.keys():
          instancePetDict[key]=value['petList']

if len(instanceDict)>0:
  for key,value in instanceDict.items():
    sys.stdout.write(key + ' is running as an instance of ' + value)
    if key in instancePetDict.keys():
      sys.stdout.write(' on PET ' + str(instancePetDict[key]))
    sys.stdout.write('\n')
if len(dependencyDict)>0:
  for key,value in dependencyDict.items():
    sys.stdout.write(key + ' depends on ')
    print (value)

if len(foreignGrid)>0:
  for key,value in foreignGrid.items():
    print(key + ' obtains grid information from ' + value + ' field')

for item in gridCompList:
  if not item in instanceDict.keys():
    instanceDict[item]=item

cplCompList=[]
gridCompList=[]
petList=[]

for item in componentList:
  i=componentList.index(item)
  if item in dependencyDict.keys():
    for dep in dependencyDict[item]:
      j=componentList.index(dep)
      if j>i:
        componentList.remove(item)
        componentList.insert(j,item)


for item in componentList:
    if item in gridCompSet:
        gridCompList.append(item)
        if item in instanceDict.keys() and item in instancePetDict.keys():
            petList.append(str(instancePetDict[item]))
        else:
            petList.append('all')
    else:
        cplCompList.append(item)

# sort netcdf instances to the beginning of the gridCompList
sortedGridCompList = []
for item in gridCompList:
  if item=='netcdf' or (item in instanceDict.keys() and instanceDict[item]=='netcdf'):
    sortedGridCompList.insert(0,item)
  else:
    sortedGridCompList.append(item)
gridCompList = sortedGridCompList

#if 'rename_connector' in cplCompList:
#  c=cplCompList.pop(cplCompList.index('rename_connector'))
#  cplCompList.insert(0,c)

if 'link_connector' in cplCompList:
  c=cplCompList.pop(cplCompList.index('link_connector'))
  cplCompList.insert(0,c)

for item in cplCompList:
  if not item in instanceDict.keys():
    instanceDict[item]=item

instanceList=list(set(instanceDict.values()))
print ('Components to process:', componentList)
print ('Grid components to process:', gridCompList)
print ('Couple components to process:', cplCompList)
print ('Base instances to process:', instanceList)
#print(' '.join('{}:{}'.format(*k) for k in enumerate(gridCompList)))
#print(' '.join('{}:{}'.format(*k) for k in enumerate(cplCompList)))
#print(' '.join('{}:{}'.format(*k) for k in enumerate(instanceList)))

# Done parsing the list, now write the new toplevel_component file

outfilename = os.path.join( os.path.dirname( os.path.realpath(__file__) ) , 'toplevel_component.F90' )
fid = open(outfilename,'w')

fid.write('''!> @brief Implementation of an ESMF toplevel coupling
!>
!> Do not edit this file, it is automatically generated by
''')
fid.write('!> the call \'python ' + sys.argv[0] + ' ' + filename + '\'')
fid.write('''
!>
!> This computer program is part of MOSSCO.
''')
fid.write('!> @copyright ' + copyright + '\n')
fid.write('!> @author ' + author + '\n')
fid.write('''
!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
!

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMF_ERR_PASSTHRU msg="MOSSCO subroutine call returned error"
#undef ESMF_FILENAME
#define ESMF_FILENAME "toplevel_component.F90"

#define _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(X) if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=X)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

''')

fid.write('module ' + 'toplevel_component\n')
fid.write('''
  use esmf
  use mossco_variable_types
  use mossco_state
  use mossco_component\n
''')

for jtem in instanceList:

    if jtem.find('_mediator')>0 or jtem.find('_connector')>0 or jtem == 'vertical_reduction' or jtem == 'calculator' or jtem.find('_coupler')>0:
      fid.write('  use ' + jtem + ', only : ' + jtem + '_SetServices => SetServices \n')
    else: fid.write('  use ' + jtem + '_component, only : ' + jtem + '_SetServices => SetServices \n')

fid.write('\n  implicit none\n\n  private\n\n  public SetServices\n')
fid.write('''
  type(ESMF_GridComp),dimension(:),save, allocatable :: gridCompList
  type(ESMF_CplComp),dimension(:), save, allocatable :: cplCompList
  type(ESMF_State), dimension(:),  save, allocatable :: gridExportStateList, gridImportStateList
  type(ESMF_Alarm), dimension(:),  save, allocatable :: cplAlarmList
  type(ESMF_Clock), dimension(:),  save, allocatable :: gridCompClockList, cplCompClockList
  type(ESMF_Clock), save                             :: controlClock
  character(len=ESMF_MAXSTR), dimension(:), save, allocatable :: gridCompNameList, cplCompNameList, cplNames
''')

fid.write('''
  contains

#undef  ESMF_METHOD
#define ESMF_METHOD "SetServices"
  subroutine SetServices(gridcomp, rc)

    type(ESMF_GridComp)  :: gridcomp
    integer, intent(out) :: rc

    integer              :: localrc

    rc=ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=0, &
      userRoutine=InitializeP0, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine SetServices

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP0"
  subroutine InitializeP0(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)         :: gridComp
    type(ESMF_State)            :: importState
    type(ESMF_State)            :: exportState
    type(ESMF_Clock)            :: parentClock
    integer, intent(out)        :: rc

    character(len=10)           :: InitializePhaseMap(1)
    character(len=ESMF_MAXSTR)  :: myName
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=myName, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
       call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine InitializeP0

#undef  ESMF_METHOD
#define ESMF_METHOD "InitializeP1"
  subroutine InitializeP1(gridComp, importState, exportState, parentClock, rc)

    implicit none

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=19)       :: timestring
    type(ESMF_Time)         :: startTime, currTime
    type(ESMF_Time)         :: ringTime, time
    type(ESMF_TimeInterval) :: alarmInterval

    integer(ESMF_KIND_I4)  :: numGridComp, numCplComp, petCount
    integer(ESMF_KIND_I4)  :: alarmCount, numCplAlarm, i, localrc, localPet
    type(ESMF_Alarm), dimension(:), allocatable :: alarmList !> @todo shoudl this be a pointer?
    character(ESMF_MAXSTR) :: myName, message, childName, alarmName
    type(ESMF_Alarm)       :: childAlarm
    type(ESMF_Clock)       :: childClock
    type(ESMF_Clock)       :: clock !> This component's internal clock
    logical                :: clockIsPresent
    integer(ESMF_KIND_I4), allocatable :: petList(:)
    type(ESMF_VM)          :: vm
    type(ESMF_Log)         :: stateLog

    integer(ESMF_KIND_I4)  :: phase, phaseCount
    integer(ESMF_KIND_I4), dimension(:), allocatable :: gridCompPhaseCountList,CplCompPhaseCountList
    logical, allocatable   :: GridCompHasPhaseZeroList(:)
    logical                :: hasPhaseZero
    integer(ESMF_KIND_I4), parameter :: maxPhaseCount=9

    integer(ESMF_KIND_I4), allocatable      :: intValueList(:)
    character(len=ESMF_MAXSTR), allocatable :: charValueList(:)
    character(len=ESMF_MAXSTR)              :: stringList(6,2)
    type(ESMF_AttPack)     :: attPack
    character(len=ESMF_MAXSTR) :: convention, purpose

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=myName, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Create a control clock that is later used for deteriming timesteps to
    !! individual calls of coupled components
    !! this is a global variable, might change later
    controlClock = ESMF_ClockCreate(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockSet(controlClock, name=trim(myName)//'Control')
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, vm=vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_VmGet(vm, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Add a CIM component attribute package to this component
    convention='CIM 1.5'
    purpose='ModelComp'

    call ESMF_AttributeAdd(gridComp, convention=convention, purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'ShortName', 'toplevel', convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'LongName', 'toplevel', convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'ModelType', 'framework', convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importState, name='simulation_title', value=message, defaultvalue='Untitled', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'SimulationShortName', message, convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'SimulationLongName', message, convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'SimulationRationale', 'Modular coupled system simulation', convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importState, name='simulation_start', value=message, defaultvalue='Untitled', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'SimulationStartDate', message, convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(importState, name='simulation_stop', value=message, defaultvalue='Untitled', rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'SimulationDuration', message, convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    write(message,'(I4)') petCount
    call ESMF_AttributeSet(gridComp, 'SimulationNumberOfProcessingElements', message, convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    purpose='Platform'
    call ESMF_AttributeGetAttPack(gridComp, convention, purpose, attpack=attpack, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, 'MachineName', 'unknown', convention=convention, &
      purpose=purpose, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Allocate the fields for all gridded components and their names
''')
fid.write('    numGridComp = ' + str(len(gridCompList)) )
fid.write('''
    allocate(gridCompList(numGridComp), stat=localrc)
    allocate(gridCompClockList(numGridComp), stat=localrc)
    allocate(gridCompNameList(numGridComp), stat=localrc)
    allocate(gridImportStateList(numGridComp), stat=localrc)
    allocate(gridExportStateList(numGridComp), stat=localrc)

''')
for i in range(0, len(gridCompList)):
    fid.write('    gridCompNameList(' + str(i+1) + ') = \'' + gridCompList[i] + '\'\n')

fid.write('''
    !! Create all gridded components, and create import and export states for these

    allocate(petList(petCount), stat=localrc)
    do i=1,petCount
      petList(i)=i-1
    enddo

    do i = 1, numGridComp
      gridCompClockList(i) = ESMF_ClockCreate(clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_ClockSet(gridCompClockList(i), name=trim(gridCompNameList(i)), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

''')
for i in range(0, len(gridCompList)):

    if (petList[i]=='all'):
        fid.write('    gridCompList(' + str(i+1) + ') = ESMF_GridCompCreate(name=trim(gridCompNameList(' + str(i+1) + ')),  &\n')
        fid.write('      petList=petList, clock=gridCompClockList(' + str(i+1) + '), rc=localrc)\n')
    else:
       fid.write('    if (petCount<=' + str(max(petList[i])) + ') then\n')
       fid.write('      write(message,\'(A,I4)\') \'This configuration requires more than ' + max(petList[i]) + ' PET, I got only \',petCount\n')
       fid.write('      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)\n')
       fid.write('      write(0,\'(A)\') trim(message)\n')
       fid.write('      call ESMF_Finalize(endflag=ESMF_END_ABORT)\n')
       fid.write('    endif\n')
       fid.write('    gridCompList(' + str(i+1) + ') = ESMF_GridCompCreate(name=trim(gridCompNameList(' + str(i+1) + ')),  &\n')
       fid.write('      petList=(/' + petList[i] + '/), clock=gridCompClockList(' + str(i+1) + '), rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
    fid.write('      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n')
fid.write('''
    do i=1, numGridComp
      gridExportStateList(i) = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED, &
        name=trim(gridCompNameList(i))//'Export')
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_LogWrite('toplevel reconciles '//trim(gridCompNameList(i))//'Export', ESMF_LOGMSG_INFO)
      call ESMF_StateReconcile(gridExportStateList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      gridImportStateList(i) = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED, &
        name=trim(gridCompNameList(i))//'Import')
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_LogWrite('toplevel reconciles '//trim(gridCompNameList(i))//'Import', ESMF_LOGMSG_INFO)
      call ESMF_StateReconcile(gridImportStateList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo
''')

for i in range(0, len(gridCompList)):
  item=gridCompList[i]
  if item in instanceDict.keys():
    if instanceDict[item] != 'netcdf': continue
  else:
    continue

  fid.write('    !! Adding meta information to output component ' + item + '\n')
  fid.write('''
    !>@todo find out why attributeSet does not work

    do i=1, numGridComp
      if (i<10) then
        write(message,'(A,I1)') 'gridded_component_', i
      else
        write(message,'(A,I2)') 'gridded_component_', i
      endif
  ''')
  fid.write('      !call ESMF_AttributeSet(importState(' + str(i+1) + '), trim(message), trim(gridCompNameList(i)), rc=localrc)\n')
  fid.write('      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
  fid.write('        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n')
  fid.write('    enddo\n')

fid.write('''
    !! Now register all setServices routines for the gridded components
''')

for i in range(0, len(gridCompList)):
    item=gridCompList[i]
    if item in instanceDict.keys():
        item=instanceDict[item]
    fid.write('    call ESMF_GridCompSetServices(gridCompList(' + str(i+1) + '), ' +item + '_SetServices, rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
    fid.write('      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n')

fid.write('\n    !! Allocate the fields for all coupler components and their names\n')
fid.write('    numCplComp = ' + str(len(cplCompList)) )
if len(cplCompList)>0:
    fid.write('''
    allocate(cplCompList(numCplComp), stat=localrc)
    allocate(cplCompNameList(numCplComp), stat=localrc)
    allocate(cplCompClockList(numCplComp), stat=localrc)
''')

for i in range(0, len(cplCompList)):
    fid.write('    cplCompNameList(' + str(i+1) + ') = \'' + cplCompList[i] + '\'\n')
fid.write('''

    do i = 1, numCplComp
      cplCompClockList(i) = ESMF_ClockCreate(clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ClockSet(cplCompClockList(i), name=trim(cplCompNameList(i)), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      cplCompList(i) = ESMF_CplCompCreate(name=trim(cplCompNameList(i)), clock=cplCompClockList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

''')
for i in range(0,len(cplCompList)):
    item = cplCompList[i]
    if item in instanceDict.keys():
            item=instanceDict[item]

    fid.write('    call ESMF_CplCompSetServices(cplCompList(' + str(i+1) + '), ' + item + '_SetServices, rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
    fid.write('      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n')

fid.write('''
    !! Initialize all components, both cpl and grid components, do this
    !! in the order specified by dependencies/couplings
    !! Also, try to find coupling/dependency specific export/import states in
    !! the initialization

    !! Establish number of phases and zero phase for all components
    !! @> todo this interface will likely change in the future and will
    !! be integrated with GridCompGet

    allocate(GridCompHasPhaseZeroList(numGridComp), stat=localrc)
    allocate(gridCompPhaseCountList(numGridComp), stat=localrc)

    do i = 1, numGridComp
      call ESMF_GridCompGetEPPhaseCount(gridCompList(i), ESMF_METHOD_INITIALIZE, &
        phaseCount=phaseCount, phaseZeroFlag=hasPhaseZero, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      gridCompPhaseCountList(i)=phaseCount
      gridCompHasPhaseZeroList(i)=hasPhaseZero
    enddo

    !! Go through all phase 0 if components have it
    do i = 1,numGridcomp
      if (.not.GridCompHasPhaseZeroList(i)) cycle
      call ESMF_GridCompInitialize(gridCompList(i), exportState=gridExportStateList(i), phase=0, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !!> @todo expect the Attribute InitializePhaseMap in this state, this attribute
      !! contains information on the phases defined in the component.
    enddo

    allocate(CplCompPhaseCountList(numCplComp), stat=localrc)
    cplCompPhaseCountList(:)=1

    !!> The code below is not working in ESMF 6, thus not executed for now
    !do i = 1, numCplComp
    !  call ESMF_CplCompGetEPPhaseCount(cplCompList(i), ESMF_METHOD_INITIALIZE, &
    !    phaseCount=CplCompPhaseCountList(i), phaseZeroFlag=hasPhaseZero, rc=localrc)
    !  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    !    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !  if (.not.hasPhaseZero) cycle
!     TODO: clock provided during Create() seems to be not recognized?!
      !call ESMF_CplCompInitialize(cplCompList(i), phase=0, rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      !  call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !!> @todo expect the Attribute InitializePhaseMap in this state, this attribute
      !! contains information on the phases defined in the component.
    !end do

    !! Declare all dependencies
''')

for item in gridCompList:
  ifrom=gridCompList.index(item)
  ito=ifrom
  for j in range(0, len(couplingList)):
    jtem=couplingList[j]
    if jtem[-1]==item:
      ifrom=gridCompList.index(jtem[0])
  j=gridCompList.index(item)
  if (item in foreignGrid.keys()):
    #print item
    fid.write('    call ESMF_AttributeSet(gridImportStateList(' + str(ito+1)+'), name="foreign_grid_field_name", value="'+foreignGrid[item]+'", rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
    fid.write('      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
  if (item == 'fabm_pelagic') :
    fid.write('    call ESMF_AttributeSet(gridImportStateList(' + str(ito+1)+'), name="surface_downwelling_photosynthetic_radiative_flux:needed", value=.true., rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
    fid.write('      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
    fid.write('    call ESMF_AttributeSet(gridImportStateList(' + str(ito+1)+'), name="water_depth_at_soil_surface:needed", value=.true., rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
    fid.write('      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')

  if item in dependencyDict.keys() and len(dependencyDict[item]) > 0:
    fid.write('    allocate(charValueList(' + str(len(dependencyDict[item])) + '), intValueList(' + str(len(dependencyDict[item])) + '))\n')
    for i,jtem in enumerate(dependencyDict[item]):
      fid.write('    charValueList(' + str(i+1) + ') = \'' + jtem + '\'\n')
      fid.write('    intValueList (' + str(i+1) + ') = ' + str(ifrom + 1) + '\n')
    fid.write('    call ESMF_AttributeSet(gridImportStateList(' + str(ito+1)+'), name="depends_on", valueList=charValueList, rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
    fid.write('      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
    fid.write('    call ESMF_AttributeSet(gridImportStateList(' + str(ito+1)+'), name="depends_on_id", valueList=intValueList, rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
    fid.write('      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
    fid.write('    deallocate(charValueList)\n')
    fid.write('    deallocate(intValueList)\n')

#for item in cplCompList:
#  if (item == 'pelagic_benthic_coupler') :
#    ito=cplCompList.index(item)
#    fid.write('    call ESMF_AttributeSet(cplImportStateList(' + str(ito+1)+'), name="temperature_in_water:needed", value=.true., rc=localrc)\n')
#    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
#    fid.write('    call ESMF_AttributeSet(cplImportStateList(' + str(ito+1)+'), name="temperature_at_soil_surface:needed", value=.true., rc=localrc)\n')
#    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
#    fid.write('    call ESMF_AttributeSet(cplImportStateList(' + str(ito+1)+'), name="dissolved_oxygen_in_water:needed", value=.true., rc=localrc)\n')
#    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')

fid.write('''
    !! Go through all phases:
    !! IPDv00p1 = phase 1: Advertise Fields in import and export States. These can be
    !!   empty fields that are later completed with FieldEmptyComplete
    !! IPDv00p2 = phase 2: Realize Fields (that have not been completed in phase 1)

''')

maxPhases=2

#for phase in range(1,maxPhases+1):
#  fid.write('    phase = %i \n'%phase)
if (True):
  fid.write('    do phase = 1,' + str(maxPhases) + '\n\n')
  for item in gridCompList:
    fid.write('      !! Initializing ' + item + '\n')
    ito=gridCompList.index(item)

    if item in dependencyDict.keys():
      for jtem in dependencyDict[item]:
        ifrom=gridCompList.index(jtem)
        fid.write('      !! linking ' + jtem + 'Export to ' + item + 'Import\n')
        fid.write('      write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(' + str(ifrom+1) +'))//"Export to "//trim(gridCompNameList(' + str(ito+1)+'))//"Import"\n')
        fid.write('      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)\n')
        fid.write('      call ESMF_CplCompInitialize(cplCompList(1), importState=gridExportStateList(' + str(ifrom+1) + '), &\n')
        fid.write('        exportState=gridImportStateList(' + str(ito+1)+'), clock=clock, rc=localrc)\n')
        fid.write('      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
        fid.write('        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
        fid.write('      call ESMF_LogFlush()\n')

    fid.write('      if (gridCompPhaseCountList( ' + str(ito+1) + ')>= phase) then\n')
#    fid.write('        call MOSSCO_GridCompFieldsTable(gridCompList(' + str(ito+1) + '), importState=gridImportStateList(' + str(ito+1) + '), &\n')
#    fid.write('          exportState=gridExportStateList(' + str(ito+1) + '), rc=localrc)\n')
    fid.write('        call ESMF_GridCompInitialize(gridCompList(' + str(ito+1) + '), importState=gridImportStateList(' + str(ito+1) + '), &\n')
    fid.write('          exportState=gridExportStateList(' + str(ito+1) + '), clock=clock, phase=phase, rc=localrc)\n')
    fid.write('        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
    fid.write('          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
#    fid.write('        call MOSSCO_GridCompFieldsTable(gridCompList(' + str(ito+1) + '), importState=gridImportStateList(' + str(ito+1) + '), &\n')
#    fid.write('          exportState=gridExportStateList(' + str(ito+1) + '), rc=localrc)\n')
    fid.write('        !call ESMF_CplCompInitialize(cplCompList(2), importState=gridImportStateList(' + str(ito+1) + '), &\n')
    fid.write('        !   clock=clock, rc=localrc)\n')
    fid.write('        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
    fid.write('          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
    fid.write('        !call ESMF_CplCompInitialize(cplCompList(2), importState=gridExportStateList(' + str(ito+1) + '), &\n')
    fid.write('        !   clock=clock, rc=localrc)\n')
    fid.write('        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
    fid.write('          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
    fid.write('      endif\n\n')

  fid.write('      !! Linking\n')
  for i,item in enumerate(gridCompList):
    for j,jtem in enumerate(gridCompList):
      if i<j:
        for c in couplingList:
          if (c[0]==item and c[-1]==jtem) or (c[0]==jtem and c[-1]==item):
            break
        else:
          continue
        fid.write('      !! linking ' + item + ' and ' + jtem + '\n')
        fid.write('      if (gridCompPhaseCountList( ' + str(i+1) + ')>= phase .or. gridCompPhaseCountList( ' + str(j+1) + ')>= phase) then\n')
        for c in couplingList:
          if instanceDict[c[1]] == 'nudge_connector' : continue
          if instanceDict[c[1]] == 'regrid_coupler' : continue
          if c[0]==item and c[-1]==jtem:
            fid.write('        !! linking ' + item + 'Export to ' + jtem + 'Import\n')
            fid.write('        write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(' + str(i+1) +'))//"Export to "//trim(gridCompNameList(' + str(j+1)+'))//"Import"\n')
            fid.write('        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)\n')
            fid.write('        call ESMF_CplCompInitialize(cplCompList(1), importState=gridExportStateList(' + str(i+1) + '), &\n')
            fid.write('          exportState=gridImportStateList(' + str(j+1)+'), clock=clock, rc=localrc)\n')
            fid.write('        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
            fid.write('          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
            fid.write('        !! linking ' + jtem + 'Import to ' + item + 'Export\n')
            fid.write('        write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(' + str(j+1) +'))//"Import to "//trim(gridCompNameList(' + str(i+1)+'))//"Export"\n')
            fid.write('        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)\n')
            fid.write('        call ESMF_CplCompInitialize(cplCompList(1), importState=gridImportStateList(' + str(j+1) + '), &\n')
            fid.write('          exportState=gridExportStateList(' + str(i+1)+'), clock=clock, rc=localrc)\n')
            fid.write('        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
            fid.write('          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
          if c[0]==jtem and c[-1]==item:
            fid.write('        !! linking ' + jtem + 'Export to ' + item + 'Import\n')
            fid.write('        write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(' + str(j+1) +'))//"Export to "//trim(gridCompNameList(' + str(i+1)+'))//"Import"\n')
            fid.write('        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)\n')
            fid.write('        call ESMF_CplCompInitialize(cplCompList(1), importState=gridExportStateList(' + str(j+1) + '), &\n')
            fid.write('          exportState=gridImportStateList(' + str(i+1)+'), clock=clock, rc=localrc)\n')
            fid.write('        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
            fid.write('          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
            fid.write('        !! linking ' + item + 'Import to ' + jtem + 'Export\n')
#           Here we require that gridCompList was filled in the order of componentList (ordered by dependencies) !!!
#           For example, here we link wave fields from getmImport to waveExport *before* waveExport is linked to erosedImport...
            fid.write('        write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(' + str(i+1) +'))//"Import to "//trim(gridCompNameList(' + str(j+1)+'))//"Export"\n')
            fid.write('        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)\n')
            fid.write('        call ESMF_CplCompInitialize(cplCompList(1), importState=gridImportStateList(' + str(i+1) + '), &\n')
            fid.write('          exportState=gridExportStateList(' + str(j+1)+'), clock=clock, rc=localrc)\n')
            fid.write('        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
            fid.write('          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
        fid.write('      endif\n\n')

  for i,item in enumerate(cplCompList):
# @todo  dirty hack for now: skip link_connector and rename_connector
    if item=='link_connector': continue
    if item=='rename_connector': continue

    for jtem in couplingList:
      if jtem[1] == item:
        break
    else:
        continue
    fid.write('      !! calling init of ' + item + '\n')
    for j in range(0, len(couplingList)):
      jtem=couplingList[j]
      if jtem[1] != item: continue

      ifrom=gridCompList.index(jtem[0])
      ito=gridCompList.index(jtem[-1])
      icpl=cplCompList.index(item)
      if icpl==0: continue

      if item == 'nudge_connector':
        fid.write('      !! connecting ' + jtem[0] + 'Export to ' + jtem[-1] + 'Export\n')
      else:
        fid.write('      !! connecting ' + jtem[0] + 'Export to ' + jtem[-1] + 'Import\n')

      fid.write('      if (gridCompPhaseCountList( ' + str(ifrom+1) + ')>= phase .or. gridCompPhaseCountList( ' + str(ito+1) + ')>= phase) then\n')
      fid.write('      if (cplCompPhaseCountList( ' + str(icpl+1) + ')>= phase) then\n')
      fid.write('        write(message,"(A,I1,A)") trim(myName)//" "//trim(gridCompNameList(' + str(ifrom+1) +'))//"Export=>"//trim(cplCompNameList('+str(icpl+1)+'))//"(initP",phase,")=>"//trim(gridCompNameList(' + str(ito+1)+'))//"Import"\n')
      fid.write('        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)\n')
      fid.write('        !call MOSSCO_StateLog(gridExportStateList(' + str(ifrom+1) + '), rc=localrc)\n')
      if item == 'nudge_connector':
        fid.write('        call ESMF_CplCompInitialize(cplCompList(' + str(icpl+1) + '), importState=gridExportStateList(' + str(ifrom+1) + '), &\n')
        fid.write('          exportState=gridExportStateList(' + str(ito+1) + '), clock=clock, phase=phase, rc=localrc)\n')
      else:
        fid.write('        call ESMF_CplCompInitialize(cplCompList(' + str(icpl+1) + '), importState=gridExportStateList(' + str(ifrom+1) + '), &\n')
        fid.write('          exportState=gridImportStateList(' + str(ito+1) + '), clock=clock, phase=phase, rc=localrc)\n')
      fid.write('        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
      fid.write('          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
      fid.write('        !call MOSSCO_Log(gridImportStateList(' + str(ito+1) + '), rc=localrc)\n')
      fid.write('      endif\n')
      fid.write('      endif\n')

  fid.write('    enddo  ! of loop over Initialize phases\n\n')

# Go through all output components and link toplevel metadata to it
for item in gridCompList:
  if item in instanceDict.keys():
    if not instanceDict[item] == 'netcdf' : continue
  elif not item == 'netcdf' : continue
  ito=gridCompList.index(item)

  fid.write('    !> Link attributes of exportState of the topLevel component (which contains metadata)\n')
  fid.write('    !> to the netcdf component\'s import state\n')
  fid.write('    call ESMF_AttributeLink(importState, gridImportStateList(' + str(ito+1) + '), rc=localrc)\n')
  fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
  fid.write('      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
  fid.write('    call ESMF_AttributeLink(gridComp, gridImportStateList(' + str(ito+1) + '), rc=localrc)\n')
  fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
  fid.write('      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
  fid.write('    call ESMF_CplCompInitialize(cplCompList(1), importState=importState, &\n')
  fid.write('      exportState=gridImportStateList(' + str(ito+1) + '), clock=clock, rc=localrc)\n')
  fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
  fid.write('      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
  fid.write('    call MOSSCO_StateLog(exportState, rc=localrc)\n')
  fid.write('    call MOSSCO_StateLog(gridImportStateList(' + str(ito+1) + '), rc=localrc)\n')
  fid.write('    call MOSSCO_CompLog(gridComp, rc=localrc)\n')

# Go through all components and log their import and export states
fid.write('''
    !> Go through all components and log their import and export states

    call ESMF_GridCompGet(gridComp, localPet=localPet, rc=localrc)

    if (localPet==0) then
      call ESMF_AttributeGet(importState, name='simulation_title', value=message, defaultvalue='Untitled', rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_LogOpen(stateLog,'states_'//trim(message), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_LogWrite('====== Status at end of child initialization ======', ESMF_LOGMSG_INFO, log=stateLog)

      do i=1,numGridComp
        call ESMF_LogWrite('====== '//trim(gridCompNameList(i))//' ======', ESMF_LOGMSG_INFO, log=stateLog)

        call MOSSCO_CompLog(gridCompList(i), log=stateLog, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_StateLog(gridImportStateList(i), log=stateLog, deep=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call MOSSCO_StateLog(gridExportStateList(i), log=stateLog, deep=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo

      do i=1,numCplComp
        call ESMF_LogWrite('====== '//trim(cplCompNameList(i))//' ======', ESMF_LOGMSG_INFO, log=stateLog)

        call MOSSCO_CompLog(cplCompList(i), log=stateLog, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo

      call ESMF_LogClose(stateLog)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    endif

    !! Establish number of phases for all components
    !! @> todo this interface will likely change in the future and will
    !! be integrated with GridCompGet

    if (.not.allocated(gridCompPhaseCountList)) &
      allocate(gridCompPhaseCountList(numGridComp), stat=localrc)

    do i = 1, numGridComp
      call ESMF_GridCompGetEPPhaseCount(gridCompList(i), ESMF_METHOD_READRESTART, &
        phaseCount=phaseCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      gridCompPhaseCountList(i)=phaseCount
    enddo
''')

# Go through ReadRestart only if (1) a netcdf_input is connected
# and (2) a ReadRestart phase is defined.
for item in gridCompList:
  ito=gridCompList.index(item)
  for coupling in couplingList:
    if coupling[-1] != item: continue
    jtem=coupling[0]
    ifrom=gridCompList.index(jtem)
    if not jtem in instanceDict.keys(): continue
    if not instanceDict[jtem] == 'netcdf_input' : continue
    if coupling[1] == 'nudge_connector' : continue
    fid.write('    !! ReadRestarting ' + item + ' with data from ' + jtem + '\n')
    fid.write('    if (gridCompPhaseCountList(' + str(ito+1) + ') > 0) then\n')
    fid.write('      call ESMF_GridCompReadRestart(gridCompList(' + str(ito+1) + '), importState=gridExportStateList(' + str(ifrom+1) + '), &\n')
    fid.write('        exportState=gridExportStateList(' + str(ito+1) + '), clock=clock, phase=1, rc=localrc)\n')
    fid.write('      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &\n')
    fid.write('        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n')
    fid.write('    endif\n\n')
fid.write('    !! End of ReadRestart \n\n')

fid.write('''
    do i=1, numGridComp
      call ESMF_LogWrite('toplevel reconciles '//trim(gridCompNameList(i))//'Import', ESMF_LOGMSG_INFO)
      call ESMF_StateReconcile(state=gridImportStateList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_LogWrite('toplevel reconciles '//trim(gridCompNameList(i))//'Export', ESMF_LOGMSG_INFO)
      call ESMF_StateReconcile(state=gridExportStateList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo
 ''')

fid.write('''
    !!> Check all states for remaining incomplete fields
    !!>@todo find segfault this is causing
    !call ESMF_LogWrite(trim(myName)//' listing all import and export states', ESMF_LOGMSG_INFO)

    do i=1, numGridComp
      !call MOSSCO_StateCheckFields(gridImportStateList(i), rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !call MOSSCO_StateCheckFields(gridExportStateList(i), rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !call MOSSCO_StateLog(gridImportStateList(i))
      !call MOSSCO_StateLog(gridExportStateList(i))
   enddo
''')

# Go through all components and log their import and export states
fid.write('''
    !> Go through all components and log their import and export states
    call ESMF_LogWrite('====== Status at end of child readrestarting ======', ESMF_LOGMSG_INFO, log=stateLog)

    !do i=1,numGridComp
    !  call ESMF_LogWrite('====== States of '//trim(gridCompNameList(i))//' ======', ESMF_LOGMSG_INFO, log=stateLog)
    !  call MOSSCO_StateLog(gridImportStateList(i))
    !  call MOSSCO_StateLog(gridExportStateList(i))
    !enddo
''')

#fid.write('''
#    do phase=1, -9
#      do i=1, numGridComp
#        if (gridCompPhaseCountList(i) < phase) cycle
#        call ESMF_AttributeGet(gridImportStateList(i), 'depends_on_id', isPresent=isPresent, rc=localrc)
#        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#        if (isPresent) then
#          call ESMF_AttributeGet(gridImportStateList(i), 'depends_on_id', itemCount=itemCount, rc=localrc)
#          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#          allocate(intValueList(itemCount))
#          call ESMF_AttributeGet(gridImportStateList(i), 'depends_on_id', valueList=intValueList, rc=localrc)
#          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#          do j=1, itemCount
#            call ESMF_CplCompInitialize(cplCompList(1), importState=gridExportStateList(intValueList(j)), &
#              exportState=gridImportStateList(i), clock=clock, rc=localrc)
#            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#          enddo
#          deallocate(intValueList)
#        endif
#
#        call ESMF_GridCompInitialize(gridCompList(i), importState=gridImportStateList(i), exportState=gridExportStateList(i), &
#          clock=clock, phase=phase, rc=localrc)
#        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#      enddo
#    enddo
#
# ''')


#for icpl in range(1,len(cplCompList)):
#  item=cplCompList[icpl]
#  fid.write('    !! Initializing ' + item + '\n')
#
#  if dependencyDict.has_key(item):
#    for jtem in dependencyDict[item]:
#      ifrom=gridCompList.index(jtem)
#      fid.write('    call ESMF_CplCompInitialize(cplCompList(1), importState=gridExportStateList(' + str(ifrom+1) + '), &\n')
#      fid.write('      exportState=cplImportStateList(' + str(icpl+1)+'), clock=clock, rc=localrc)\n')

#fid.write('''
#    do i=2, numCplComp
#      call ESMF_AttributeGet(cplImportStateList(i), 'depends_on_id', isPresent=isPresent, rc=localrc)
#      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#
#      if (isPresent) then
#        call ESMF_AttributeGet(cplImportStateList(i), 'depends_on_id', itemCount=itemCount, rc=localrc)
#        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#        allocate(intValueList(itemCount))
#
#        call ESMF_AttributeGet(cplImportStateList(i), 'depends_on_id', valueList=intValueList, rc=localrc)
#        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#        do j=1, itemCount
#            call ESMF_CplCompInitialize(cplCompList(1), importState=gridExportStateList(intValueList(j)), &
#              exportState=cplImportStateList(i), clock=clock, rc=localrc)
#            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#        enddo
#        deallocate(intValueList)
#      endif
#
#      call ESMF_CplCompInitialize(CplCompList(i), importState=cplImportStateList(i), exportState=cplExportStateList(i), &
#        clock=clock, phase=phase, rc=localrc)
#      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
#    enddo
#''')


fid.write('    numCplAlarm = ' + str(len(couplingList)))
if len(cplCompList)>0:
    fid.write('''
    if (allocated(cplAlarmList)) deallocate(cplAlarmList)
    if (allocated(cplNames)) deallocate(cplNames)
    allocate(cplAlarmList(numCplAlarm))
    allocate(cplNames(numCplAlarm))

    !! The default coupler for all cplAlarms is the 'link' connector
    cplNames(:) = 'link'

    !! For other explicitly given couplings, specify connectors
''')
for idx,couplingItem in enumerate(couplingList):
    if couplingItem[1][:4] == 'link':
        continue
    else:
        fid.write("    cplNames(%d)='%s'\n" % (idx+1,couplingItem[1].split('_connector')[0]))
fid.write('''
    !! Set the coupling alarm starting from start time of local clock
    call ESMF_ClockGet(clock,startTime=startTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

''')

for i in range(0,len(couplingList)):

    unit = 'h' # default unit is hours
    value = intervals[i]

    if isinstance(value,str) or isinstance(value,unicode):
      string = value.split()
      number = string[0]
      if len(string)>1: unit = string[1]

    elif isinstance(value,int):
      number = str(value)

    else:
        print ('Unknown interval specification "' + intervals[i] + '"')
        sys.exit(1)

      # Special case infinity
    if (number == 'inf') or (number == 'none') or (number == '0'):
        unit = 'yy'
        number = '99999'

    fid.write('    call ESMF_TimeIntervalSet(alarmInterval, startTime, ' + unit + '=' + number + ' ,rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
    fid.write('    cplAlarmList(' + str(i+1) + ')=ESMF_AlarmCreate(clock=clock,ringTime=startTime+alarmInterval, &\n')
    alarmName = str(couplingList[i][0]) + '--' + str(couplingList[i][-1]) + '--cplAlarm'
    fid.write('      ringInterval=alarmInterval, name=\'' + alarmName + '\', rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n')

    fid.write('''
    !! Copy this alarm to all children as well
    do i=1,numGridComp
      call ESMF_GridCompGet(gridCompList(i),name=childName, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    ''')
    fid.write('  if (trim(childName)==\'' + str(couplingList[i][0]) + '\' .or. trim(childName)==\'' + str(couplingList[i][-1]) + '\') then')
    fid.write('''
        call ESMF_GridCompGet(gridCompList(i), clockIsPresent=clockIsPresent, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (clockIsPresent) then
          call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=localrc)
        else
          call ESMF_LOGWRITE(trim(myName)//' creates clock for '//trim(childName)//', this should have been done by the component.', &
            ESMF_LOGMSG_WARNING)

          childClock=ESMF_ClockCreate(clock=clock, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_GridCompSet(gridCompList(i),clock=childClock, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
    ''')
    fid.write('    childAlarm=ESMF_AlarmCreate(cplAlarmList(' + str(i+1) + '), rc=localrc)')
    fid.write('''
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_AlarmSet(childAlarm, clock=childClock)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      endif
    enddo
    ''')

fid.write('''
    !! Set the default ringTime to the stopTime of local clock, then get all Alarms
    !! from local clock into alarmList, find those that contain the string "cplAlarm"
    !! and look for the earliest ringtime in all coupling alarms.  Save that in the
    !! ringTime
    call ESMF_ClockGet(clock, stopTime=ringTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGetAlarmList(clock, ESMF_ALARMLIST_ALL, alarmCount=alarmCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (allocated(alarmList)) deallocate(alarmList)
    allocate(alarmList(alarmCount))

    call ESMF_ClockGetAlarmList(clock,ESMF_ALARMLIST_ALL,alarmList=alarmList,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1,ubound(alarmList,1)
      call ESMF_AlarmGet(alarmList(i), ringTime=time, name=alarmName, rc=localrc)

      call ESMF_TimeGet(time,timeStringISOFrac=timestring)
      !write(message,'(A)') trim(myName)//' alarm '//trim(alarmName)//' rings at '//trim(timestring)
      !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      if (index(trim(alarmName),'cplAlarm') < 1) cycle
      if (time<ringTime) ringTime=time
    enddo
    if (allocated(alarmList)) deallocate(alarmList)

    !! Set the timestep such that it corresponds to the time until the
    !! first ringing alarm, log that time
    call ESMF_ClockGet(clock,currTime=currTime,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_ClockSet(clock,timeStep=ringTime-currTime,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, name=childName, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_TimeGet(ringTime,timeStringISOFrac=timestring, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    write(message,'(A)') trim(myName)//' '//trim(childName)//' alarms ring next at '//trim(timestring)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    stringList(1,1)='Name';               stringList(1,2)='Carsten Lemmen'
    stringList(2,1)='Abbreviation';       stringList(2,2)='cl'
    stringList(3,1)='PhysicalAddress';    stringList(3,2)='Helmholtz-Zentrum Geesthacht'
    stringList(4,1)='EmailAddress';       stringList(4,2)='carsten.lemmen@hzg.de'
    stringList(5,1)='ResponsiblePartyRole';   stringList(5,2)='Contact'
    stringList(6,1)='URL';   stringList(6,2)='http://www.hzg.de'

    !> @todo te following code throws attribute warnings in ESMF7, this needs
    !> to be investigated and is disabled for now.

#if ESMF_VERSION_MAJOR > 7
    !> Write Responsible party ISO 19115 attributes
    convention = 'ISO 19115'
    purpose    = 'RespParty'

    do i=1,6
      call ESMF_AttributeSet(gridComp, trim(stringList(i,1)), trim(stringList(i,2)), &
        convention=convention, purpose=purpose, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    !> Write Citation ISO 19115 attributes
    convention = 'ISO 19115'
    purpose    = 'Citation'
    stringList(1,1)='ShortTitle';     stringList(1,2)='Lemmen et al. (2013)'
    stringList(2,1)='LongTitle';      stringList(2,2)='  '
    stringList(3,1)='Date';           stringList(3,2)='2013'
    stringList(4,1)='PresentationForm';   stringList(4,2)='Workshop report'
    stringList(5,1)='DOI';            stringList(5,2)='not assigned'
    stringList(6,1)='URL';            stringList(6,2)='http://www.kfki.de/files/kfki-aktuell/0/13-2-DE.pdf'

    !do i=1,6
    !  call ESMF_AttributeSet(gridComp, trim(stringList(i,1)), trim(stringList(i,2)), &
    !    convention=convention, purpose=purpose, rc=localrc)
    !  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    !        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !enddo

    call ESMF_AttributeWrite(gridComp, convention='CIM 1.5', purpose='ModelComp', &
      attwriteflag=ESMF_ATTWRITE_XML, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) then
      if (rc .ne. ESMF_RC_LIB_NOT_PRESENT) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    do i=1,numGridComp
      call ESMF_AttributeWrite(gridCompList(i), 'CIM 1.5', 'ModelComp', &
        attwriteflag=ESMF_ATTWRITE_XML, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) then
        if (rc .ne. ESMF_RC_LIB_NOT_PRESENT) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    enddo
#endif

    !! Populate toplevel state with metadata on simulation
    call MOSSCO_StatePopulateAttributes(exportState, parentClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

''')

fid.write('''

    call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Flush the log at the end of Initialize()
    call ESMF_LogFlush(rc=localrc)

  end subroutine InitializeP1

#undef  ESMF_METHOD
#define ESMF_METHOD "Run"
  subroutine Run(gridComp, importState, exportState, parentClock, rc)

    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    character(len=ESMF_MAXSTR) :: timestring, cplName, myName, childName
    type(ESMF_Time)            :: stopTime, currTime, ringTime, time
    type(ESMF_TimeInterval)    :: timeInterval, ringInterval, zeroInterval
    integer(ESMF_KIND_I8)      :: i, j, k, l, advanceCount
    integer(ESMF_KIND_I4)      :: alarmCount
    integer(ESMF_KIND_I4)      :: numGridComp, numCplComp, numComp
    integer(ESMF_KIND_I4)      :: hours, minutes, seconds, localPet
    real(ESMF_KIND_R8)         :: ms_r8, realValue
    type(ESMF_Log)             :: stateLog

    type(ESMF_Alarm), dimension(:), allocatable :: alarmList
    type(ESMF_Clock)        :: childClock, myClock
    logical                 :: clockIsPresent
    type(ESMF_State)        :: impState, expState
    integer(ESMF_KIND_I4)   :: localrc

    type(ESMF_Time), allocatable, dimension(:) :: wallTimeStart, wallTimeStop

    character(len=ESMF_MAXSTR) :: message, compName, alarmName, name1, name2
    character(len=ESMF_MAXSTR) :: formatString

    integer(ESMF_KIND_I4)  :: phase, phaseCount
    integer(ESMF_KIND_I4), dimension(:), allocatable :: gridCompPhaseCountList,CplCompPhaseCountList
    logical, allocatable   :: GridCompHasPhaseZeroList(:)
    logical                :: hasPhaseZero
    integer(ESMF_KIND_I4), parameter :: maxPhaseCount=9

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=myName, currTime=currTime, importState=importState, &
      exportState=exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_GridCompGet(gridComp, clock=myClock, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    numGridComp = ubound(gridCompList,1)-lbound(gridCompList,1)+1
    numCplComp  = ubound(cplCompList ,1)-lbound(cplCompList ,1)+1
    numComp = numGridComp + numCplComp

    if (allocated(wallTimeStart)) deallocate(wallTimeStart)
    if (allocated(wallTimeStop)) deallocate(wallTimeStop)
    allocate(wallTimeStart(0:numComp))
    allocate(wallTimeStop(0:numComp))

    call ESMF_TimeIntervalSet(zeroInterval, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    do i = 0, numComp
      call ESMF_TimeSet(wallTimeStart(i), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_TimeSet(wallTimeStop(i), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    enddo

    !! Establish number of phases and zero phase for all components
    !! @> todo this interface will likely change in the future and will
    !! be integrated with GridCompGet

    allocate(GridCompHasPhaseZeroList(numGridComp))
    allocate(gridCompPhaseCountList(numGridComp))

    do i = 1, numGridComp
      call ESMF_GridCompGetEPPhaseCount(gridCompList(i), ESMF_METHOD_RUN, &
        phaseCount=phaseCount, phaseZeroFlag=hasPhaseZero, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      gridCompPhaseCountList(i)=phaseCount
      GridCompHasPhaseZeroList(i)=hasPhaseZero
    enddo

    allocate(CplCompPhaseCountList(numCplComp))
    !!> @todo reenable if ESMF new enough
    CplCompPHaseCountList(:)=1

    !do i = 1, numCplComp
    !  call ESMF_CplCompGetEPPhaseCount(cplCompList(i), ESMF_METHOD_RUN, &
    !    phaseCount=CplCompPhaseCountList(i), phaseZeroFlag=hasPhaseZero, rc=localrc)
    !  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    !    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !  if (.not.hasPhaseZero) cycle
    !enddo

    call ESMF_ClockGetAlarmList(myClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
      alarmCount=alarmCount, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)


    !! Run until the clock's stoptime is reached
    do

      call ESMF_TimeSyncToRealTime(wallTimeStart(0), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_ClockGet(myClock,currTime=currTime, stopTime=stopTime, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (currTime>stopTime) then
        call ESMF_LogWrite(trim(myName)//' clock out of scope', ESMF_LOGMSG_ERROR)
        call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=localrc)
      endif

      !! Loop through all components and check whether their clock is currently at the
      !! same time as my own clock's currTime, if yes, then run the respective couplers
      do i=1,numGridComp
        !! Determine for each child the clock
        call ESMF_GridCompGet(gridCompList(i),name=compName, clockIsPresent=clockIsPresent, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (.not.clockIsPresent) then
          call ESMF_LogWrite(trim(myName)//' required clock not found in '//trim(compName), ESMF_LOGMSG_ERROR)
          call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=localrc)
        endif

        call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_ClockGet(childClock, currTime=time, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        ! write(message,'(A)') trim(myName)//' '//trim(compName)//' now at '//trim(timestring)
        !  call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE)

        if (time>currTime) cycle

        !! Find all the alarms in this child and call all the couplers that
        !! have ringing alarms at this stage

        call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
          alarmCount=alarmCount, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (alarmCount==0) then
          timeInterval=stopTime-currTime
          cycle
        endif

        if (allocated(alarmList)) deallocate(alarmList)
        allocate(alarmList(alarmCount))

        call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
           alarmList=alarmList, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        do j=1,alarmCount
          call ESMF_AlarmGet(alarmList(j), name=alarmName, ringTime=ringTime, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          !! Skip this alarm if it is not a cplAlarm
          if (index(trim(alarmName),'cplAlarm') < 1) cycle

          !! Skip this alarm if it is inbound of this component
          if (trim(alarmName(1:index(alarmName,'--')-1))/=trim(compName)) cycle

          !! Skip this alarm if it is not ringing now
          !if (ringTime > currTime) cycle

          call ESMF_TimeGet(ringTime, timeStringISOFrac=timeString)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          !write(message,'(A)') trim(myName)//' '//trim(compName)//' '//trim(alarmName)//' rings at '//trim(timeString)
          !call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

          name1=trim(alarmName(1:index(alarmName,'--')-1))
          name2=trim(alarmName(index(alarmName,'--')+2:index(alarmName,'--cplAlarm')-1))

          do k=1,ubound(cplAlarmList,1)
            if (cplAlarmList(k) == alarmList(j)) then
              cplName = trim(cplNames(k))
              exit
            endif
          enddo

          ! Catch a possible memory leak problem when k overflows
          if (k > ubound(cplAlarmList,1)) then
            write(message,'(A)') 'You have some memory corruption, good luck searching ...'
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          endif

          write(message,'(A)') trim(myName)//' '//trim(timeString)//' '//trim(name1)//' ->'
          if (trim(cplName) /= 'link') then
            write(message,'(A)') trim(message)//' '//trim(cplName)//' ->'
          else
            write(message,'(A)') trim(message)//' ('//trim(cplName)//') ->'
          endif
          write(message,'(A)') trim(message)//' '//trim(name2)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

          !call ESMF_GridCompGet(gridCompList(i), exportState=impState, rc=localrc)
          impState=gridExportStateList(i)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          !! Search the gridCompList for other's name
          do k=1, ubound(gridCompList,1)
              call ESMF_GridCompGet(gridCompList(k), name=childName, rc=localrc)
              _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

              if (trim(childName)==trim(name2)) exit
          enddo

          if (trim(childName) /= trim(name2)) then
            write(message,'(A)') trim(myName)//' did not find component '//trim(name2)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
          endif

          !! Search the cplCompList for cplName
          do l=1, ubound(cplCompNameList,1)
              !write(0,*) l,trim(cplCompNameList(l))//' ?= '//trim(cplName)//'_connector'
              if (trim(cplCompNameList(l))==trim(cplName)//'_connector') exit
              if (trim(cplCompNameList(l))==trim(cplName)//'_mediator') exit
              if (trim(cplCompNameList(l))==trim(cplName)) exit
          enddo

          !! Exit if the name of the connector/mediator was not found
          if (l > ubound(cplCompNameList,1)) then
            write(message,'(A)') trim(myName)//' could not match '//trim(cplName)//' and '//trim(cplCompNameList(l))
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
          endif

          !call ESMF_GridCompGet(gridCompList(k), importState=expState, rc=localrc)
          expState=gridImportStateList(k)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          !> @todo the following is a hack and makes the nudge_connector special by
          !> receiving two export States (the latter to manipulate)
          if (trim(cplCompNameList(l)) == 'nudge_connector') expState=gridExportStateList(k)

          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          write(message,'(A)') trim(myName)//' '//trim(timeString)//' calling '//trim(cplCompNameList(l))

          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

          call ESMF_TimeSyncToRealTime(wallTimeStart(numgridComp + l), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_CplCompRun(cplCompList(l), importState=impState, &
            exportState=expState, clock=controlClock, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_TimeSyncToRealTime(wallTimeStop(numgridComp + l), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message,'(A)') trim(myName)//' executed ' &
            //trim(cplCompNameList(l))//' in '
          call ESMF_TimeIntervalGet(wallTimeStop(numgridComp + l) - &
            wallTimeStart(numgridComp + l), ms_r8=ms_r8, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (ms_r8 < 9999.0d0) then
            write(formatString,'(A)') '(A,X,'//intformat(int(ms_r8))//',X,A)'
            write(message, formatString) trim(message), int(ms_r8), 'mseconds'
          else
            write(formatString,'(A)') '(A,X,'//intformat(int(ms_r8/1000.0d0))//',X,A)'
            write(message, formatString) trim(message), int(ms_r8/1000.0d0), 'seconds'
          endif
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          call ESMF_LogFlush()

        enddo
      enddo

#if 0
      !! Obtain all currently ringing Alarms, and run the components associated with these alarms
      !! When no alarms are ringing anymore, then obtain the minimum of the nextRinging alarms and advance
      !! myself with that timeStep

      call ESMF_ClockGetAlarmList(myClock, alarmListFlag=ESMF_ALARMLIST_RINGING, &
        alarmCount=alarmCount, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (alarmCount>0) then
        if (allocated(alarmList)) deallocate(alarmList)
        allocate(alarmList(alarmCount))
        call ESMF_ClockGetAlarmList(myClock, alarmListFlag=ESMF_ALARMLIST_RINGING, &
          alarmList=alarmList, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_TimeGet(time,timeStringISOFrac=timeString)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message,'(A,I2,A)') trim(myName)//'  '//trim(timeString)//' has',alarmCount,' ringing alarms'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      endif

      do j=1,alarmCount
        call ESMF_AlarmGet(alarmList(j), name=alarmName, ringTime=time, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (index(trim(alarmName),'cplAlarm')<1) cycle

        call ESMF_TimeGet(time,timeStringISOFrac=timeString)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        write(message,'(A)') trim(myName)//'  '//trim(alarmName)//' is ringing now at '//trim(timestring)
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)
      enddo

      call ESMF_ClockGetAlarmList(myClock, alarmListFlag=ESMF_ALARMLIST_NEXTRINGING, &
        alarmCount=alarmCount, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (alarmCount>0) then
        if (allocated(alarmList)) deallocate(alarmList)
        allocate(alarmList(alarmCount))
      endif
#endif

      !! Loop through all components and check whether their clock is currently at the
      !! same time as my own clock's currTime, if yes, then run the component and advance it's time
      !! until the next coupling Alarm of this component
      do i=1,numGridComp
        !! Determine for each child the clock
        call ESMF_GridCompGet(gridCompList(i),name=compName, clockIsPresent=clockIsPresent, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (.not.clockIsPresent) then
          call ESMF_LogWrite(trim(myName)//' required clock not found in '//trim(compName), ESMF_LOGMSG_ERROR)
          call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=localrc)
        endif

        call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_ClockGet(childClock,currTime=time, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_TimeGet(time,timeStringISOFrac=timeString)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (time>currTime) then
          call ESMF_TimeGet(time,timeStringISOFrac=timeString)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          !write(message,'(A)') trim(myName)//' '//trim(compName)//' now at '//trim(timestring)//', but'
          !call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)

          call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          !write(message,'(A)') trim(myName)//' now at '//trim(timestring)//', cycling ...'
          !call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)

          cycle
        endif

        !! Find the child's alarm list, get the interval to the next ringing alarm
        !! and run the component for the interval until that alarm

        call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
          alarmCount=alarmCount, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (alarmCount==0) then
          !! This case seems problematic and causing the non-monotonic time warning in netcdf
          call ESMF_LogWrite('No alarm found in '//trim(compName), ESMF_LOGMSG_WARNING)
          timeInterval=stopTime-currTime
        else
          if (allocated(alarmList)) deallocate(alarmList)
          allocate(alarmList(alarmCount))
          call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
             alarmList=alarmList, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif

        !! Set the default ringTime to the stopTime of local clock, then get all Alarms
        !! from local clock into alarmList, find those that contain the string "cplAlarm"
        !! and look for the earliest ringtime in all coupling alarms.  Save that in the
        !! ringTime
        call ESMF_ClockGet(myClock, stopTime=ringTime, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)


        do j=1,alarmCount
          call ESMF_AlarmGet(alarmList(j), name=alarmName, ringTime=time, &
            ringInterval=ringInterval, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (index(trim(alarmName),'cplAlarm')<1) cycle

          call ESMF_TimeGet(time,timeStringISOFrac=timeString)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          !write(message,'(A)') trim(myName)//' '//trim(compName)//' '//trim(alarmName)//' rings at '//trim(timestring)
          !call ESMF_LogWrite(trim(message),ESMF_LOGMSG_INFO)

          !! This might be problematic for components that need to run multiple times from multiple alarms
          !! For a process model the currTime+ringInterval should be taken if it advances it's own clock
          !! For a non-process model (i.e. output), the clock should only be advanced if all of it's current
          !! alarms are switched off ...
          if (time==currTime) ringTime=currTime+ringInterval
          if (time<ringTime) ringTime=time
        enddo

        !call ESMF_TimeGet(ringTime,timeStringISOFrac=timestring, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        !write(message,'(A)') trim(myName)//' setting child''s stopTime to'//trim(timeString)
        !call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=localrc);

!       TODO: do not modify childClock
!             (components need to inquire stopTime not from their own clock!)
        call ESMF_ClockSet(childClock, stopTime=ringTime, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_ClockGet(childClock, timeStep=timeInterval, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (timeInterval>ringTime-currTime) then
          !call ESMF_ClockSet(childClock, timeStep=ringTime-currTime, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          !call ESMF_LogWrite(trim(myName)//"  must be implemented in "//trim(compName),ESMF_LOGMSG_WARNING)
        endif

        !! Change the controlClock with updated currTime and timeStep (if not zero)
        if (ringTime < currTime) then
          write(message,'(A)') trim(myName)//' should not run components with negative timestep'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING, rc=localrc)
          !call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        elseif (ringTime > currTime) then
          timeInterval=ringTime-currTime

          call ESMF_ClockSet(controlClock, currTime=currTime, timeStep=timeInterval, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_TimeIntervalGet(timeInterval, h=hours, m=minutes, s=seconds, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message,'(A,A,I5.5,A,I2.2,A,I2.2,A)') trim(myName)//' '//trim(timeString)//' calling '//trim(compName), &
            ' to run for ', hours, ':', minutes, ':', seconds, ' hours'
        else
          write(message,'(A,A,I5.5,A,I2.2,A,I2.2,A)') trim(myName)//' '//trim(timeString)//' calling '//trim(compName), &
            ' to run without stepping forward'
        endif
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=localrc)
        call ESMF_ClockSet(controlClock, currTime=currTime,  rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)


        !! Loop over all run phases, disregarding any action that could be taken between
        !! phases
        do phase=1,gridCompPhaseCountList(i)
          !call MOSSCO_GridCompFieldsTable(gridCompList(i), importState=gridImportStateList(i), exportState=gridExportStateList(i),rc=localrc)
          call ESMF_LogWrite('toplevel reconciles '//trim(gridCompNameList(i))//'Import', ESMF_LOGMSG_INFO)
          call ESMF_StateReconcile(gridImportStateList(i), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_LogWrite('toplevel reconciles '//trim(gridCompNameList(i))//'Export', ESMF_LOGMSG_INFO)
          call ESMF_StateReconcile(gridExportStateList(i), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_TimeSyncToRealTime(wallTimeStart(i), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_GridCompRun(gridCompList(i),importState=gridImportStateList(i),&
            exportState=gridExportStateList(i), clock=controlClock, phase=phase, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          call ESMF_TimeSyncToRealTime(wallTimeStop(i), rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          write(message,'(A)') trim(myName)//' executed '//trim(gridCompNameList(i))//' in '
          call ESMF_TimeIntervalGet(wallTimeStop(i) - wallTimeStart(i), &
            ms_r8=ms_r8, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (ms_r8 < 9999.0d0) then
            write(formatString,'(A)') '(A,X,'//intformat(int(ms_r8))//',X,A)'
            write(message, formatString) trim(message), int(ms_r8), 'mseconds'
          else
            write(formatString,'(A)') '(A,X,'//intformat(int(ms_r8/1000.0d0))//',X,A)'
            write(message, formatString) trim(message), int(ms_r8/1000.0d0), 'seconds'
          endif

          call ESMF_TimeIntervalGet(timeInterval, ms_r8=realValue, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (ms_r8>0.0d0) then
            realValue = realValue / ms_r8
            write(formatString,'(A)') '(A,X,'//intformat(int(realValue))//')'
            write(message, formatString) trim(message)//' with speedup ', int(realValue)
          endif
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

          !call MOSSCO_GridCompFieldsTable(gridCompList(i), importState=gridImportStateList(i), exportState=gridExportStateList(i),rc=localrc)
          !call ESMF_LogFlush()
        enddo

        call ESMF_ClockGet(childClock, currTime=time, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        if (time == currTime) then
          !! This child component did not advance its clock in its Run() routine
          !! We do that here
          call ESMF_LogWrite(trim(myName)//' '//trim(compName)//' did not advance its clock',ESMF_LOGMSG_WARNING)
          !call ESMF_LogWrite("... but this assumption is weird - skipping further action!",ESMF_LOGMSG_WARNING)

          !call ESMF_ClockAdvance(childClock, timeStep=timeInterval, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        endif
      enddo

      !! Now that all child components have been started, find out the minimum time
      !! to the next coupling and use this as a time step for my own clock Advance

      call ESMF_ClockGetAlarmList(myClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
        alarmCount=alarmCount, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (alarmCount==0) then
        !call ESMF_LogWrite('No alarm found in '//trim(myName), ESMF_LOGMSG_WARNING)
        timeInterval=stopTime-currTime
      else
        if (allocated(alarmList)) deallocate(alarmList)
        allocate(alarmList(alarmCount))

        call ESMF_ClockGetAlarmList(myClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
          alarmList=alarmList, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_AlarmGet(alarmList(1), ringTime=ringTime, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        do j=2,alarmCount
          call ESMF_AlarmGet(alarmList(j), ringTime=time, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

          if (time<ringTime) ringTime=time
        enddo

        timeInterval=ringTime-currTime
      endif

      !> Log current and next ring time
      call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(myName)//' '//trim(timeString)//' '//trim(myName)//' stepping to'
      call ESMF_TimeGet(ringTime,timeStringISOFrac=timestring, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(message)//' '//trim(timeString)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=localrc)

      call ESMF_ClockGet(myClock, advanceCount=advanceCount, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_GridCompGet(gridComp, localPet=localPet, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      !> If advanceCount==1 then go through all components and log their import and export states

      if (localPet==0 .and. advanceCount==0) then
        call ESMF_AttributeGet(importState, name='simulation_title', value=message, defaultvalue='Untitled', rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_LogOpen(stateLog,'states_'//trim(message), rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_LogWrite('====== Status at end of first run loop  ======', ESMF_LOGMSG_INFO, log=stateLog)

        do i=1,numGridComp
          call ESMF_LogWrite('====== States of '//trim(gridCompNameList(i))//' ======', ESMF_LOGMSG_INFO, log=stateLog)
          call MOSSCO_StateLog(gridImportStateList(i), deep=.true., log=stateLog, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
          call MOSSCO_StateLog(gridExportStateList(i), deep=.true., log=stateLog, rc=localrc)
          _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
        enddo

        call ESMF_LogWrite('====== States of '//trim(myName)//' ======', ESMF_LOGMSG_INFO, log=stateLog)
        call MOSSCO_StateLog(importState, log=stateLog, deep=.true.,  rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call MOSSCO_StateLog(exportState, log=stateLog, deep=.true., rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

        call ESMF_LogClose(stateLog, rc=localrc)
        _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
      endif

      !> Set new time interval and advance clock, stop if end of
      !! simulation reached
      call ESMF_ClockSet(myClock, timeStep=timeInterval, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_ClockAdvance(myClock, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      call ESMF_TimeSyncToRealTime(wallTimeStop(0), rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      write(message,'(A)') trim(myName)//' executed in '
      call ESMF_TimeIntervalGet(wallTimeStop(0) - wallTimeStart(0), &
        ms_r8=ms_r8, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (ms_r8 < 9999.0d0) then
        write(formatString,'(A)') '(A,X,'//intformat(int(ms_r8))//',X,A)'
        write(message, formatString) trim(message), int(ms_r8), 'mseconds'
      else
        write(formatString,'(A)') '(A,X,'//intformat(int(ms_r8/1000.0d0))//',X,A)'
        write(message, formatString) trim(message), int(ms_r8/1000.0d0), 'seconds'
      endif

      call ESMF_TimeIntervalGet(timeInterval, ms_r8=realValue, rc=localrc)
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

      if (ms_r8>0.0d0) then
        realValue = realValue / ms_r8
        write(formatString,'(A)') '(A,X,'//intformat(int(realValue))//')'
        write(message, formatString) trim(message)//' with speedup ', int(realValue)
      endif
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      if (ESMF_ClockIsStopTime(myClock, rc=localrc)) exit
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    enddo

''')

for coupling in couplingList:
  jtem = coupling[-1]

  if jtem in instanceDict.keys():
    if instanceDict[jtem] != 'netcdf': continue

  item = coupling[0]
  ito = gridCompList.index(jtem)
  ifrom = gridCompList.index(item)
  icpl = cplCompList.index(coupling[1])

  fid.write('\n    !! Running final netcdf output coupling ' + item + ' to ' + jtem + '\n')
  fid.write('    call ESMF_CplCompRun(cplCompList(' + str(icpl+1) + '), importState=gridImportStateList(' + str(ifrom + 1) + '), &\n')
  fid.write('      exportState=gridExportStateList(' + str(ito+1) + '), clock=controlClock, rc=localrc)\n')
  fid.write('    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)\n')
  fid.write('    do phase=1,gridCompPhaseCountList(' + str(ito+1) + ')\n')
  fid.write('      call ESMF_GridCompRun(gridCompList(' + str(ito+1) + '), importState=gridImportStateList(' + str(ito + 1) + '), &\n')
  fid.write('        exportState=gridExportStateList(' + str(ito+1) + '), clock=controlClock, rc=localrc)')
  fid.write('''
      _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)
    enddo
''')

fid.write('''
    call ESMF_StateValidate(importState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    call ESMF_StateValidate(exportState, rc=localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

    if (allocated(wallTimeStart)) deallocate(wallTimeStart)
    if (allocated(wallTimeStop)) deallocate(wallTimeStop)

    call MOSSCO_CompExit(gridComp, localrc)
    _MOSSCO_LOG_AND_FINALIZE_ON_ERROR_(rc)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer(ESMF_KIND_I8)   :: i
    integer(ESMF_KIND_I4)   :: numGridComp, numCplComp, localrc
    character(ESMF_MAXSTR)  :: myName
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock

    integer(ESMF_KIND_I4)  :: phase, phaseCount
    integer(ESMF_KIND_I4), dimension(:), allocatable :: gridCompPhaseCountList,CplCompPhaseCountList
    logical, allocatable   :: GridCompHasPhaseZeroList(:)
    logical                :: hasPhaseZero, isPresent
    integer(ESMF_KIND_I4), parameter :: maxPhaseCount=9

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name=myName, currTime=currTime, &
      importState=importState, exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Establish number of phases and zero phase for all components
    !! @> todo this interface will likely change in the future and will
    !! be integrated with GridCompGet

    numGridComp=size(gridCompList)
    numCplComp=size(cplCompList)

    allocate(GridCompHasPhaseZeroList(numGridComp))
    allocate(gridCompPhaseCountList(numGridComp))

    do i = 1, numGridComp
      call ESMF_GridCompGetEPPhaseCount(gridCompList(i), ESMF_METHOD_FINALIZE, &
        phaseCount=phaseCount, phaseZeroFlag=hasPhaseZero, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      gridCompPhaseCountList(i)=phaseCount
      GridCompHasPhaseZeroList(i)=hasPhaseZero
    enddo

    allocate(CplCompPhaseCountList(numCplComp))
    !> @todo reenable if implemented in ESMF versions
    CplCompPhaseCountList(:)=1

    !do i = 1, numCplComp
    !  call ESMF_CplCompGetEPPhaseCount(cplCompList(i), ESMF_METHOD_FINALIZE, &
    !    phaseCount=CplCompPhaseCountList(i), phaseZeroFlag=hasPhaseZero, rc=localrc)
    !  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
    !    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !  if (.not. hasPhaseZero) cycle
    !enddo

    do i=1,ubound(cplCompList,1)
      call ESMF_CplCompFinalize(cplCompList(i), clock=clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    do i=1,ubound(gridCompList,1)
      do phase=1,gridCompPhaseCountList(i)
        !call ESMF_LogWrite(trim(myName)//' tells '//trim(gridCompNameList(i))//' to finalize', ESMF_LOGMSG_INFO)
        call ESMF_GridCompFinalize(gridCompList(i), importState=gridImportStateList(i), exportState= &
          gridExportStateList(i), clock=clock, phase=phase, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo
    enddo

    do i=1,ubound(gridCompList,1)
      !!@todo destroy any remaining fields/arrays in states
      call MOSSCO_DestroyOwn(gridExportStateList(i), owner=trim(myName),  rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call MOSSCO_DestroyOwn(gridImportStateList(i), owner=trim(myName),  rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

''')
i=0
for item in gridCompList:
# The clock is already destroyed locally
#    fid.write('    call ESMF_ClockDestroy(gridCompClockList(' + str(i+1) + '), rc=localrc)\n')
    fid.write('    call ESMF_GridCompDestroy(gridCompList(' + str(i+1) + '), rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n')
    i += 1
i=0
for item in cplCompList:
    fid.write('    call ESMF_CplCompDestroy(cplCompList(' + str(i+1) + '), rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n')
    i += 1
fid.write('''
    if (allocated(gridCompClockList)) deallocate(gridCompClockList)
    if (allocated(gridCompList)) deallocate(gridCompList)
    if (allocated(cplCompList))  deallocate(cplCompList)
    if (allocated(gridExportStateList)) deallocate(gridExportStateList)
    if (allocated(gridImportStateList)) deallocate(gridImportStateList)
    if (allocated(cplAlarmList)) deallocate(cplAlarmList)

    call ESMF_ClockDestroy(controlClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockDestroy(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, importStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !if (isPresent) call ESMF_StateValidate(importState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, exportStateIsPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !if (isPresent) call ESMF_StateValidate(exportState, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

end module toplevel_component
''')

fid.close()

outfilename = os.path.join( os.path.dirname( os.path.realpath(__file__) ) , 'Makefile.coupling' )
fid = open(outfilename,'w')

fid.write('# This Makefile is part of MOSSCO\n#\n')
fid.write('# Do not edit this file, it is automatically generated by\n')
fid.write('# the call \'python ' + sys.argv[0] + ' ' + filename + '\'\n#\n')
fid.write('# @copyright ' + copyright + '\n')
fid.write('# @author ' + author + '\n')
fid.write('''
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
# LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.
#

ifndef MOSSCO_DIR
export MOSSCO_DIR=$(subst /examples/generic$,,$(CURDIR))
endif

include $(MOSSCO_DIR)/src/Rules.make

''')

# Place conditionals for building this coupled system
conditionals = {'gotm' : 'GOTM', 'fabm' : 'FABM', 'erosed' : 'EROSED',
                'fabm_gotm' : 'GOTM_FABM', 'getm' : 'GETM', 'gotmfabm' : 'GOTM_FABM'}
for item in gridCompSet.union(cplCompSet):
    if item in conditionals.keys():
        fid.write('ifneq ($(MOSSCO_' + conditionals[item] + '),true)\n')
        fid.write('$(error This example only works with MOSSCO_' + conditionals[item] + ' = true)\n')
        fid.write('endif\n')

libs = {'gotm'       : ['solver', 'mossco_gotm'] ,
        'gotmfabm'   : ['mossco_gotmfabm','mossco_fabmpelagic', 'mossco_gotm', 'solver'],
        'fabm_gotm'                : ['mossco_fabmgotm','solver'],
        'fabm_sediment' : ['sediment', 'mossco_sediment', 'solver'],
        'fabm_pelagic' : ['mossco_fabmpelagic', 'util', 'solver'],
        'fabm_benthic' : ['mossco_fabmbenthic', 'util', 'solver'],
        'constant'   : ['constant', 'mossco_util'],
        'default'   :  ['default'],
        'clm_netcdf' : ['mossco_clm'],
        'benthos'    : ['mossco_benthos'],
        'grid'       : ['mossco_grid'],
        'location'   : ['mossco_location'],
        'erosed'     : ['mossco_erosed'],
        'filtration'     : ['mossco_filtration'],
        'schism'     : ['mossco_schism'],
        'hamsom'     : ['mossco_hamsom'],
        'tracer'     : ['mossco_tracer'],
        'netcdf'     : ['mossco_netcdf'],
        'netcdf_input'     : ['mossco_netcdf'],
        'test'       : ['mossco_test'],
        'simplewave' : ['mossco_simplewave'],
        'river'      : ['mossco_river'],
        'time_aggregation'      : ['mossco_aggregation'],
        'empty'      : ['mossco_technical'],
        'dummy'      : ['mossco_technical'],
        'inout'      : ['mossco_technical'],
        'info'       : ['mossco_info'],
        'fabm0d'     : ['mossco_fabm0d', 'solver', 'airsea',
                        'input', 'util', 'fabm'],
#        'pelagic_benthic_mediator' : ['mossco_mediator'],
        'pelagic_soil_connector' : ['mossco_mediator'],
        'soil_pelagic_connector' : ['mossco_mediator'],
        'vertical_reduction' : ['verticalreduction'],
        'calculator' : ['mossco_calculator'],
        'pelagic_benthic_coupler' : ['pelagicbenthiccoupler'],
        'benthic_pelagic_coupler' : ['pelagicbenthiccoupler'],
        'xgrid_coupler' : ['xgridcoupler'],
        'nudge_connector' : ['mossco_connector'],
        'link_connector' : ['mossco_connector'],
        'flux_connector' : ['mossco_connector'],
        'rename_connector' : ['mossco_connector'],
        'transport_connector' : ['mossco_connector'],
        'copy_coupler' : ['copycoupler'],
        'regrid_coupler' : ['regridcoupler'],
        'remtc_atmosphere' : ['remtc'],
        'remtc_atmosphere' : ['remtc'],
        'remtc_ocean' : ['remtc'],
        'getm' : ['mossco_getm'],
}

deps = {'clm_netcdf' : ['libmossco_clm'],
        'benthos'    : ['libmossco_benthos'],
        'hamsom'     : ['libmossco_hamsom'],
        'schism'     : ['libmossco_schism'],
        'tracer'     : ['libmossco_tracer'],
        'erosed'     : ['libmossco_erosed'],
        'fabm0d'     : ['libmossco_fabm0d'],
        'fabm_sediment' : ['libsediment', 'libmossco_sediment', 'libsolver'],
        'fabm_pelagic' : ['libmossco_fabmpelagic', 'libsolver'],
        'fabm_benthic' : ['libmossco_fabmbenthic', 'libsolver'],
        'simplewave' : ['libmossco_simplewave'],
        'netcdf'      : ['libmossco_netcdf'],
        'netcdf_input'      : ['libmossco_netcdf'],
        'test'       : ['libmossco_test'],
        'info'       : ['libmossco_info'],
        'time_aggregation'      : ['libmossco_aggregation'],
        'empty'      : ['libmossco_technical'],
        'river'      : ['libmossco_river'],
        'inout'      : ['libmossco_technical'],
        'dummy'      : ['libmossco_technical'],
        'constant'   : ['libconstant libmossco_util'],
        'default'  : ['libdefault'],
        'gotm'       : ['libmossco_gotm', 'libsolver'],
        'fabm_gotm'                : ['libmossco_fabmgotm'],
        'gotmfabm'       : ['libmossco_gotmfabm', 'libsolver'],
#        'pelagic_benthic_mediator' : ['libmossco_mediator'],
        'pelagic_soil_connector' : ['libmossco_mediator'],
        'soil_pelagic_connector' : ['libmossco_mediator'],
        'pelagic_benthic_coupler' : ['libpelagicbenthiccoupler'],
        'benthic_pelagic_coupler' : ['libpelagicbenthiccoupler'],
        'vertical_reduction' : ['libverticalreduction'],
        'calculator' : ['libmossco_calculator'],
        'xgrid_coupler' : ['libxgridcoupler'],
        'link_connector' : ['libmossco_connector'],
        'nudge_connector' : ['libmossco_connector'],
        'rename_connector' : ['libmossco_connector'],
        'flux_connector' : ['libmossco_connector'],
        'transport_connector' : ['libmossco_connector'],
        'filtration' : ['libmossco_filtration'],
        'copy_coupler' : ['libcopycoupler'],
        'regrid_coupler' : ['libregridcoupler'],
        'remtc_atmosphere' : ['libremtc'],
        'remtc_ocean' : ['libremtc'],
        'getm' : ['libmossco_getm'],
        'grid' : ['libmossco_grid'],
        'location' : ['libmossco_location'],
}

#fid.write('\nNC_LIBS += $(shell nf-config --flibs)\n\n')
fid.write('LDFLAGS += -L$(MOSSCO_LIBRARY_PATH)\n')
for item in gridCompSet.union(cplCompSet):
    if item in instanceDict.keys():
        item=instanceDict[item]
    if item in libs.keys():
        fid.write('LDFLAGS +=')
        for lib in libs[item]:
            fid.write(' -l' + lib)
        if item=='gotm':
            fid.write(' $(GOTM_LDFLAGS)')
        if item=='getm':
            fid.write(' $(GETM_LDFLAGS)')
        if item=='fabm_sediment' or item=='fabm_pelagic' or item=='fabm_benthic':
            fid.write(' $(FABM_LDFLAGS)')
        if item=='fabm' or item=='fabm0d':
            fid.write(' $(FABM_LDFLAGS) -L$(GOTM_LIBRARY_PATH)')
        if item=='fabm_gotm':
            fid.write(' $(GOTM_LDFLAGS) $(FABM_LDFLAGS)')
        if item=='gotmfabm':
            fid.write(' $(GOTM_LDFLAGS) $(FABM_LDFLAGS)')
        fid.write('\n')

#fid.write('LDFLAGS += $(LIBS) -lmossco_util -lesmf $(ESMF_NETCDF_LIBS)  -llapack\n\n')
fid.write('LDFLAGS += -lmossco_util $(ESMF_F90LDFLAGS)  \n\n')

#for item in gridCompSet.union(cplCompSet):
#    if libs.has_key(item):
#        if item=='gotm':
#            fid.write(' $(NC_LIBS)\n\n')
#        if item=='fabm_gotm':
#            fid.write(' $(NC_LIBS)\n\n')


fid.write('.PHONY: all exec ' + coupling_name + '\n\n')
fid.write('all: exec\n\n')
fid.write('exec: libmossco_util libmossco_connector ')
for item in gridCompSet.union(cplCompSet):
    if item in instanceDict.keys():
        item=instanceDict[item]
    if item in deps.keys():
        for dep in deps[item]:
            fid.write(' ' + dep)
fid.write(' ' + coupling_name + '\n\n')
fid.write(coupling_name + ': toplevel_component.o ../common/main.o\n')
fid.write('\t$(F90) $(F90FLAGS) $^ $(LDFLAGS) -o $(PWD)/$@\n')
fid.write('\t@echo "Created example binary $(PWD)/$@"\n')
fid.write('''

# Other subsidiary targets that might not be needed, these should evetually
# end up in some global Rules.make

libmossco_gotmfabm libmossco_gotm libmossco_fabmgotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

libmossco_util libsolver:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libsediment libconstant libdefault libmossco_clm libmossco_erosed \
libmossco_fabm0d libmossco_fabmpelagic libmossco_filtration libmossco_grid \
libmossco_fabmbenthic:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_technical libmossco_getm libmossco_simplewave libmossco_netcdf libmossco_benthos:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_aggregation:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_info libmossco_test libmossco_river libmossco_hamsom libmossco_location libmossco_schism:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_sediment:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers $@

libsurfacescouplerlibaocoupler liblinkcoupler libxgridcoupler libregridcoupler libcopycoupler libmossco_coupler libmossco_mediator:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators $@

libmossco_connector:
	$(MAKE) -C $(MOSSCO_DIR)/src/connectors $@

libverticalreduction libmossco_calculator:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators $@

libremtc:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/remtc $@

libpelagicbenthiccoupler:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators pelagicbenthiccoupler benthicpelagiccoupler

atmos.nc:
	@-ln -s /media/data/forcing/CLM/cDII.00.kss.2003.nc $@ || \
	ln -s /h/ksedata02/data/model/CLM/cDII.00.kss.2003.nc $@ || \
	echo "Could not find data file cDII.00.kss.2003.nc."

clean: extraclean
extraclean:
''')

if os.getcwd() == make_path:
  fid.write('	@-rm -f ' + coupling_exe)
fid.close()
