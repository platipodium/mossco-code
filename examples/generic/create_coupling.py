#!/usr/bin/env python
# This script is is part of MOSSCO. It creates from YAML descriptions of
# couplings a toplevel_component.F90 source file
#
# @copyright (C) 2014, 2015 Helmholtz-Zentrum Geesthacht
# @author Carsten Lemmen
#
# MOSSCO is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License v3+.  MOSSCO is distributed in the

import sys
import os

# Define a generic iterable ver list or dict
def sequential_iterator(obj):
  return obj if isinstance(obj, dict) else xrange(len(obj))

try:
    import yaml
except:
    sys.path.append('/home/lemmen/opt/lib64/python2.6/site-packages/')
    import yaml

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
     filename = 'fabm_benthic_pelagic+wave.yaml'
     #filename = 'constant_fabm_sediment_netcdf.yaml'
     filename = 'constant_constant_netcdf.yaml'
     filename = 'getm--fabm_pelagic--netcdf.yaml'
     filename='getm--fabm_pelagic--netcdf'
     filename='pelagic_3d'

if not filename.endswith('yaml'):
  filename = filename + '.yaml'

print sys.argv, len(sys.argv)
if not os.path.exists(filename):
    print 'File ' + filename + ' does not exist.'
    exit(1)

print 'Using ' + filename + ' ...'

fid = file(filename,'rU')
config = yaml.load(fid)
fid.close()

# Search for the key with name "coupling".  If part of the filename is the word "coupling" then assume that the first item on the list read is the name of the coupling
coupling_name = os.path.splitext(os.path.basename(filename))[0]
variables = []
coupling_properties = []

if not type(config) is dict:
  print 'File ' + filename + ' does not contain data or does not contain a'
  print 'dictionary.'
  exit(1)

if config.has_key('author'):
    author = config.pop('author')
else:
    author = 'Carsten Lemmen, <carsten.lemmen@hzg.de>'

if config.has_key('copyright'):
    copyright = config.pop('copyright')
else:
    copyright = 'Copyright (C) 2014, 2015, Helmholtz-Zentrum Geesthacht'

if config.has_key('dependencies'):
  dependencies = config.pop('dependencies')
else:
  dependencies=[]

if config.has_key('instances'):
  instances = config.pop('instances')
else:
  instances=[]

componentList=[]
gridCompList=[]
cplCompList=['link_connector','rename_connector']
#cplCompList=['link_connector']
couplingList=[]
petList=[]
foreignGrid={}

intervals =[]
directions = []

if not config.has_key('coupling'):
  print 'File ' + filename + ' must contain a coupling dictionary.'
  print 'Try adding a first line consisting only of the word "coupling:".'
  exit(1)

coupling = config.pop("coupling")

# Make it a list in any case
if not (type(coupling) is list):
  coupling=[coupling]

if len(coupling)<1:
  print 'File ' + filename + ' contains an empty coupling list.'
  print coupling
  exit(1)

# Loop over the list of couuplings.  Each entry in this list is a dictionary
# that has at least the key 'components:'
# todo: we could shortcut this by allowing comp1:comp2 to
for item in coupling:
    if type(item) is dict:
        if item.has_key("components"):
            gridCompList.extend([item["components"][0], item["components"][-1]])
            n=len(item["components"])
            if n>2:
                couplingList.append(item["components"])
            elif n==2:
                couplingList.append([item["components"][0], "link_connector", item["components"][-1]])
                cplCompList.append("link_connector")
            for i in range(1,n-1):
                cplCompList.append(item["components"][i])
            if item.has_key("interval"):
                intervals.append(item["interval"])
            else:
                intervals.append("6 m")
            if item.has_key("direction"):
                directions.append(item["direction"])
        else:
          gridComplist.extend(item.keys())
          gridCompList.extend(item.values())
          for key,value in item.iteritems():
             couplingList.append(key, "link_connector",value)
          cplCompList.append("link_connector")

    else:
        print 'Warning, dictionary expected for item ' + item + ', it is of type ',  type(item)

gridCompSet=set(gridCompList)
gridCompList=list(gridCompSet)
cplCompSet=set(cplCompList)
cplCompList=list(cplCompSet)
componentSet=gridCompSet.union(cplCompSet)
componentList=list(componentSet)

# Set a default coupling alarm interval of 6 minutes
if len(intervals) == 0:
    intervals=len(cplCompList) * ['6 m']

# if there are any dependencies specified, go through the list of components
# and sort this list
if type(dependencies) is dict:
  dependencies = list(dependencies)

gridOrder=[]
for item in dependencies:
  for key,value in item.iteritems():
    if type(value) is list:
      value=value[0]
    if type(value) is dict:
      if value.has_key('grid'):
        donator=value['component']
        if key not in gridOrder:
          gridOrder.append(key)
        if donator not in gridOrder:
          gridOrder.insert(gridOrder.index(key),donator)
        if gridOrder.index(donator) > gridOrder.index(key):
          print "ERROR: cyclic grid dependencies"
          sys.exit(1)

dependencyDict={}
for component in componentSet:
    for item in dependencies:
        compdeps=[]
        if type(item) is dict:
          if not item.has_key(component):
            continue
          for jtem in item.values():
              if type(jtem) is list and len(jtem) == 1:
                  jtem=jtem[0]
              if type(jtem) is str:
                 compdeps.append(jtem)
              elif (type(jtem) is dict) and jtem.has_key('component'):
                 compdeps.append(jtem['component'])
                 if jtem.has_key('grid'):
                    foreignGrid[item.keys()[0]]=jtem['grid']
          for compdep in compdeps:
            if componentList.index(component)< componentList.index(compdep):
              if component in gridOrder and compdep in gridOrder:
                if gridOrder.index(component) < gridOrder.index(compdep):
                  continue
              c=componentList.pop(componentList.index(component))
              componentList.insert(componentList.index(compdep)+1,c)
          if dependencyDict.has_key(item.keys()[0]):
            dependencyDict[item.keys()[0]].extend(compdeps)
          else:
            dependencyDict[item.keys()[0]]=compdeps

for key,value in dependencyDict.iteritems():
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
    if item.has_key('component'):
       instanceDict[item.keys()[0]]=item['component']
    else:
      instanceDict[item.keys()[0]]=item.values()[0]

    if item.has_key('petList'):
      instancePetDict[item.keys()[0]]=item['petList']
    if item.has_key('petlist'):
      instancePetDict[item.keys()[0]]=item['petlist']
else:
  for key,value in instances.iteritems():
    if type(value) is str:
      instanceDict[key] = value
    elif type(value) is dict and value.has_key('component'):
      instanceDict[key] = value['component']
      if value.has_key('petList'):
          instancePetDict[key]=value['petList']

if len(instanceDict)>0:
  for key,value in instanceDict.iteritems():
    sys.stdout.write(key + ' is running as an instance of ' + value)
    if instancePetDict.has_key(key):
      sys.stdout.write(' on PET ' + str(instancePetDict[key]))
    sys.stdout.write('\n')
if len(dependencyDict)>0:
  for key,value in dependencyDict.iteritems():
    sys.stdout.write(key + ' depends on ')
    print value

if len(foreignGrid)>0:
  for key,value in foreignGrid.iteritems():
    print(key + ' obtains grid information from ' + value + ' field')

for item in gridCompList:
  if not instanceDict.has_key(item):
    instanceDict[item]=item

cplCompList=[]
gridCompList=[]
petList=[]
for item in componentList:
    if item in gridCompSet:
        gridCompList.append(item)
        if instanceDict.has_key(item) and instancePetDict.has_key(item):
            petList.append(str(instancePetDict[item]))
        else:
            petList.append('all')
    else:
        cplCompList.append(item)

if 'rename_connector' in cplCompList:
  c=cplCompList.pop(cplCompList.index('rename_connector'))
  cplCompList.insert(0,c)

if 'link_connector' in cplCompList:
  c=cplCompList.pop(cplCompList.index('link_connector'))
  cplCompList.insert(0,c)

instanceList=list(set(instanceDict.values()))
print 'Components to process:', componentList
print 'Grid components to process:', componentList
print 'Couple components to process:', cplCompList
print 'Base instances to process:', instanceList

# Done parsing the list, now write the new toplevel_component file

outfilename = 'toplevel_component.F90'
fid = file(outfilename,'w')

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
''')

fid.write('module ' + 'toplevel_component\n')
fid.write('''
  use esmf
  use mossco_variable_types
  use mossco_state
  use mossco_component\n
''')

for jtem in instanceList:

    if jtem.find('mediator')>0:
      fid.write('  use ' + jtem + ', only : ' + jtem + '_SetServices => SetServices \n')
    else: fid.write('  use ' + jtem + '_component, only : ' + jtem + '_SetServices => SetServices \n')

for jtem in cplCompList:
    fid.write('  use ' + jtem + ', only : ' + jtem + '_SetServices => SetServices \n')

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

for item in cplCompList:
    fid.write('  type(ESMF_CplComp), save  :: ' + item + 'Comp\n')
for item in gridCompList:
    fid.write('  type(ESMF_GridComp), save :: ' + item + 'Comp\n')
for item in gridCompList:
    fid.write('  type(ESMF_State), save    :: ' + item + 'ExportState, ' + item + 'ImportState\n')

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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, phase=1, &
      userRoutine=InitializeP1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, Run, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_FINALIZE, Finalize, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    character(len=ESMF_MAXSTR)  :: myName, message
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, myName, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    InitializePhaseMap(1) = "IPDv00p1=1"

    call ESMF_AttributeAdd(gridComp, convention="NUOPC", purpose="General", &
      attrList=(/"InitializePhaseMap"/), rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(gridComp, name="InitializePhaseMap", valueList=InitializePhaseMap, &
      convention="NUOPC", purpose="General", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
    type(ESMF_Time)         :: clockTime, startTime, stopTime, currTime
    type(ESMF_Time)         :: ringTime, time
    type(ESMF_TimeInterval) :: timeInterval, timeStep, alarmInterval
    real(ESMF_KIND_R8)      :: dt

    integer(ESMF_KIND_I4)  :: numGridComp, numCplComp, petCount
    integer(ESMF_KIND_I4)  :: alarmCount, numCplAlarm, i, localrc
    type(ESMF_Alarm), dimension(:), allocatable :: alarmList !> @todo shoudl this be a pointer?
    character(ESMF_MAXSTR) :: myName, message, childName, alarmName
    type(ESMF_Alarm)       :: childAlarm
    type(ESMF_Clock)       :: childClock
    type(ESMF_Clock)       :: clock !> This component's internal clock
    logical                :: clockIsPresent
    integer(ESMF_KIND_I4), allocatable :: petList(:)
    type(ESMF_VM)          :: vm

    integer(ESMF_KIND_I4)  :: phase, phaseCount, j, itemCount
    integer(ESMF_KIND_I4), dimension(:), allocatable :: gridCompPhaseCountList,CplCompPhaseCountList
    logical, allocatable   :: GridCompHasPhaseZeroList(:)
    logical                :: hasPhaseZero, isPresent
    integer(ESMF_KIND_I4), parameter :: maxPhaseCount=9

    integer(ESMF_KIND_I4), allocatable      :: intValueList(:)
    character(len=ESMF_MAXSTR), allocatable :: charValueList(:)

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, myName, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Create a control clock that is later used for deteriming timesteps to individual calls of coupled components
    !! this is a global variable, might change later
    controlClock = ESMF_ClockCreate(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    
    call ESMF_ClockSet(controlClock, name=trim(myName)//'Control')    
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    !! Allocate the fields for all gridded components and their names
''')
fid.write('    numGridComp = ' + str(len(gridCompList)) )
fid.write('''
    allocate(gridCompList(numGridComp))
    allocate(gridCompClockList(numGridComp))
    allocate(gridCompNameList(numGridComp))
    allocate(gridImportStateList(numGridComp))
    allocate(gridExportStateList(numGridComp))

''')
for i in range(0, len(gridCompList)):
    fid.write('    gridCompNameList(' + str(i+1) + ') = \'' + gridCompList[i] + '\'\n')

fid.write('''
    !! Create all gridded components, and create import and export states for these
    call ESMF_GridCompGet(gridComp, vm=vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    call ESMF_VmGet(vm, petCount=petCount, rc=localrc)
    allocate(petList(petCount))
    do i=1,petCount
      petList(i)=i-1
    enddo

    do i = 1, numGridComp
      gridCompClockList(i) = ESMF_ClockCreate(clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_ClockSet(gridCompClockList(i), name=trim(gridCompNameList(i)), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
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
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n')
fid.write('''
    do i=1, numGridComp
      gridExportStateList(i) = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED, &
        name=trim(gridCompNameList(i))//'Export')
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      gridImportStateList(i) = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED, &
        name=trim(gridCompNameList(i))//'Import')
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    !! Now register all setServices routines for the gridded components
''')

for i in range(0, len(gridCompList)):
    item=gridCompList[i]
    if instanceDict.has_key(item):
        item=instanceDict[item]
    fid.write('    call ESMF_GridCompSetServices(gridCompList(' + str(i+1) + '), ' +item + '_SetServices, rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n')

fid.write('\n    !! Allocate the fields for all coupler components and their names\n')
fid.write('    numCplComp = ' + str(len(cplCompList)) )
if len(cplCompList)>0:
    fid.write('''
    allocate(cplCompList(numCplComp))
    allocate(cplCompNameList(numCplComp))
    allocate(cplCompClockList(numCplComp))
''')

for i in range(0, len(cplCompList)):
    fid.write('    cplCompNameList(' + str(i+1) + ') = \'' + cplCompList[i] + '\'\n')
fid.write('''

    do i = 1, numCplComp
      cplCompClockList(i) = ESMF_ClockCreate(clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      call ESMF_ClockSet(cplCompClockList(i), name=trim(cplCompNameList(i)), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      cplCompList(i) = ESMF_CplCompCreate(name=trim(cplCompNameList(i)), clock=cplCompClockList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

''')
for i in range(0,len(cplCompList)):
    fid.write('    call ESMF_CplCompSetServices(cplCompList(' + str(i+1) + '), ' + cplCompList[i] + '_SetServices, rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n')

fid.write('''
    !! Initialize all components, both cpl and grid components, do this
    !! in the order specified by dependencies/couplings
    !! Also, try to find coupling/dependency specific export/import states in
    !! the initialization

    !! Establish number of phases and zero phase for all components
    !! @> todo this interface will likely change in the future and will
    !! be integrated with GridCompGet

    allocate(GridCompHasPhaseZeroList(numGridComp))
    allocate(gridCompPhaseCountList(numGridComp))

    do i = 1, numGridComp
      call ESMF_GridCompGetEPPhaseCount(gridCompList(i), ESMF_METHOD_INITIALIZE, &
        phaseCount=phaseCount, phaseZeroFlag=hasPhaseZero, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      gridCompPhaseCountList(i)=phaseCount
      GridCompHasPhaseZeroList(i)=hasPhaseZero
    enddo

    !! Go through all phase 0 if components have it
    do i = 1,numGridcomp
      if (.not.GridCompHasPhaseZeroList(i)) cycle
      call ESMF_GridCompInitialize(gridCompList(i), exportState=gridExportStateList(i), phase=0, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !!> @todo expect the Attribute InitializePhaseMap in this state, this attribute
      !! contains information on the phases defined in the component.
    enddo

    allocate(CplCompPhaseCountList(numCplComp))
    cplCompPhaseCountList(:)=1

    !!> The code below is not working in ESMF 6, thus not executed for now
    !do i = 1, numCplComp
    !  call ESMF_CplCompGetEPPhaseCount(cplCompList(i), ESMF_METHOD_INITIALIZE, &
    !    phaseCount=CplCompPhaseCountList(i), phaseZeroFlag=hasPhaseZero, rc=localrc)
    !  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !  if (.not.hasPhaseZero) cycle
!     TODO: clock provided during Create() seems to be not recognized?!
      !call ESMF_CplCompInitialize(cplCompList(i), phase=0, rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
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
  if (foreignGrid.has_key(item)):
    #print item
    fid.write('    call ESMF_AttributeSet(gridImportStateList(' + str(ito+1)+'), name="foreign_grid_field_name", value="'+foreignGrid[item]+'", rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
  if (item == 'fabm_pelagic') :
    fid.write('    call ESMF_AttributeSet(gridImportStateList(' + str(ito+1)+'), name="surface_downwelling_photosynthetic_radiative_flux:needed", value=.true., rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')

  if dependencyDict.has_key(item) and len(dependencyDict[item]) > 0:
    fid.write('    allocate(charValueList(' + str(len(dependencyDict[item])) + '), intValueList(' + str(len(dependencyDict[item])) + '))\n')
    for i,jtem in enumerate(dependencyDict[item]):
      fid.write('    charValueList(' + str(i+1) + ') = \'' + jtem + '\'\n')
      fid.write('    intValueList (' + str(i+1) + ') = ' + str(ifrom + 1) + '\n')
    fid.write('    call ESMF_AttributeSet(gridImportStateList(' + str(ito+1)+'), name="depends_on", valueList=charValueList, rc=localrc)\n')
    fid.write('    call ESMF_AttributeSet(gridImportStateList(' + str(ito+1)+'), name="depends_on_id", valueList=intValueList, rc=localrc)\n')
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

    if dependencyDict.has_key(item):
      for jtem in dependencyDict[item]:
        ifrom=gridCompList.index(jtem)
        fid.write('      !! linking ' + jtem + 'Export to ' + item + 'Import\n')
        fid.write('      write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(' + str(ifrom+1) +'))//"Export to "//trim(gridCompNameList(' + str(ito+1)+'))//"Import"\n')
        fid.write('      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)\n')
        fid.write('      call ESMF_CplCompInitialize(cplCompList(1), importState=gridExportStateList(' + str(ifrom+1) + '), &\n')
        fid.write('        exportState=gridImportStateList(' + str(ito+1)+'), clock=clock, rc=localrc)\n')
        fid.write('      call ESMF_LogFlush()\n')

    fid.write('      if (gridCompPhaseCountList( ' + str(ito+1) + ')>= phase) then\n')
#    fid.write('        call MOSSCO_GridCompFieldsTable(gridCompList(' + str(ito+1) + '), importState=gridImportStateList(' + str(ito+1) + '), &\n')
#    fid.write('          exportState=gridExportStateList(' + str(ito+1) + '), rc=localrc)\n')
    fid.write('        call ESMF_GridCompInitialize(gridCompList(' + str(ito+1) + '), importState=gridImportStateList(' + str(ito+1) + '), &\n')
    fid.write('          exportState=gridExportStateList(' + str(ito+1) + '), clock=clock, phase=phase, rc=localrc)\n')
#    fid.write('        call MOSSCO_GridCompFieldsTable(gridCompList(' + str(ito+1) + '), importState=gridImportStateList(' + str(ito+1) + '), &\n')
#    fid.write('          exportState=gridExportStateList(' + str(ito+1) + '), rc=localrc)\n')
    fid.write('        !call ESMF_CplCompInitialize(cplCompList(2), importState=gridImportStateList(' + str(ito+1) + '), &\n')
    fid.write('        !   clock=clock, rc=localrc)\n')
    fid.write('        !call ESMF_CplCompInitialize(cplCompList(2), importState=gridExportStateList(' + str(ito+1) + '), &\n')
    fid.write('        !   clock=clock, rc=localrc)\n')
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
          if c[0]==item and c[-1]==jtem:
            fid.write('        !! linking ' + item + 'Export to ' + jtem + 'Import\n')
            fid.write('        write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(' + str(i+1) +'))//"Export to "//trim(gridCompNameList(' + str(j+1)+'))//"Import"\n')
            fid.write('        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)\n')
            fid.write('        call ESMF_CplCompInitialize(cplCompList(1), importState=gridExportStateList(' + str(i+1) + '), &\n')
            fid.write('          exportState=gridImportStateList(' + str(j+1)+'), clock=clock, rc=localrc)\n')
            fid.write('        !! linking ' + jtem + 'Import to ' + item + 'Export\n')
            fid.write('        write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(' + str(j+1) +'))//"Import to "//trim(gridCompNameList(' + str(i+1)+'))//"Export"\n')
            fid.write('        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)\n')
            fid.write('        call ESMF_CplCompInitialize(cplCompList(1), importState=gridImportStateList(' + str(j+1) + '), &\n')
            fid.write('          exportState=gridExportStateList(' + str(i+1)+'), clock=clock, rc=localrc)\n')
          if c[0]==jtem and c[-1]==item:
            fid.write('        !! linking ' + jtem + 'Export to ' + item + 'Import\n')
            fid.write('        write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(' + str(j+1) +'))//"Export to "//trim(gridCompNameList(' + str(i+1)+'))//"Import"\n')
            fid.write('        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)\n')
            fid.write('        call ESMF_CplCompInitialize(cplCompList(1), importState=gridExportStateList(' + str(j+1) + '), &\n')
            fid.write('          exportState=gridImportStateList(' + str(i+1)+'), clock=clock, rc=localrc)\n')
            fid.write('        !! linking ' + item + 'Import to ' + jtem + 'Export\n')
#           Here we require that gridCompList was filled in the order of componentList (ordered by dependencies) !!!
#           For example, here we link wave fields from getmImport to waveExport *before* waveExport is linked to erosedImport...
            fid.write('        write(message,"(A)") trim(myName)//" linking "//trim(gridCompNameList(' + str(i+1) +'))//"Import to "//trim(gridCompNameList(' + str(j+1)+'))//"Export"\n')
            fid.write('        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)\n')
            fid.write('        call ESMF_CplCompInitialize(cplCompList(1), importState=gridImportStateList(' + str(i+1) + '), &\n')
            fid.write('          exportState=gridExportStateList(' + str(j+1)+'), clock=clock, rc=localrc)\n')
        fid.write('      endif\n\n')

  for i,item in enumerate(cplCompList):
#   dirty hack for now: skip link_connector and rename_connector
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

      fid.write('      !! connecting ' + jtem[0] + 'Export to ' + jtem[-1] + 'Import\n')
      fid.write('      if (gridCompPhaseCountList( ' + str(ifrom+1) + ')>= phase .or. gridCompPhaseCountList( ' + str(ito+1) + ')>= phase) then\n')
      fid.write('      if (cplCompPhaseCountList( ' + str(icpl+1) + ')>= phase) then\n')
      fid.write('        write(message,"(A,I1,A)") trim(myName)//" "//trim(gridCompNameList(' + str(ifrom+1) +'))//"Export=>"//trim(cplCompNameList('+str(icpl+1)+'))//"(initP",phase,")=>"//trim(gridCompNameList(' + str(ito+1)+'))//"Import"\n')
      fid.write('        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)\n')
      fid.write('        !call MOSSCO_StateLog(gridExportStateList(' + str(ifrom+1) + '), rc=localrc)\n')
      fid.write('        call ESMF_CplCompInitialize(cplCompList(' + str(icpl+1) + '), importState=gridExportStateList(' + str(ifrom+1) + '), &\n')
      fid.write('          exportState=gridImportStateList(' + str(ito+1) + '), clock=clock, phase=phase, rc=localrc)\n')
      fid.write('        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n')
      fid.write('        !call MOSSCO_StateLog(gridImportStateList(' + str(ito+1) + '), rc=localrc)\n')
      fid.write('      endif\n')
      fid.write('      endif\n')

  fid.write('    enddo  ! of loop over Initialize phases\n\n')




# Go through ReadRestart (assumed only phase 1)
for item in gridCompList:
  fid.write('    !! ReadRestarting ' + item + '\n')
  ifrom=gridCompList.index(item)
  ito=ifrom
  for j in range(0, len(couplingList)):
    jtem=couplingList[j]
    if jtem[-1]==item:
      ifrom=gridCompList.index(jtem[0])
  j=gridCompList.index(item)
  fid.write('    call ESMF_GridCompReadRestart(gridCompList(' + str(ito+1) + '), importState=gridImportStateList(' + str(ito+1) + '), &\n')
  fid.write('          exportState=gridExportStateList(' + str(ito+1) + '), clock=clock, phase=1, rc=localrc)\n')
fid.write('    !! End of ReadRestart \n\n')

fid.write('''
    do i=1, numGridComp
      call ESMF_StateReconcile(state=gridImportStateList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateReconcile(state=gridExportStateList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo
 ''')

fid.write('''
    !!> Check all states for remaining incomplete fields
    !!>@todo find segfault this is causing
    call ESMF_LogWrite(trim(myName)//' listing all import and export states', ESMF_LOGMSG_INFO)

    do i=1, numGridComp
      call MOSSCO_StateCheckFields(gridImportStateList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !call MOSSCO_StateCheckFields(gridExportStateList(i), rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !call MOSSCO_StateLog(gridImportStateList(i))
      !call MOSSCO_StateLog(gridExportStateList(i))
   enddo
''')

fid.write('''
    do phase=1, -9
      do i=1, numGridComp
        if (gridCompPhaseCountList(i) < phase) cycle
        call ESMF_AttributeGet(gridImportStateList(i), 'depends_on_id', isPresent=isPresent, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (isPresent) then
          call ESMF_AttributeGet(gridImportStateList(i), 'depends_on_id', itemCount=itemCount, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          allocate(intValueList(itemCount))
          call ESMF_AttributeGet(gridImportStateList(i), 'depends_on_id', valueList=intValueList, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          do j=1, itemCount
            call ESMF_CplCompInitialize(cplCompList(1), importState=gridExportStateList(intValueList(j)), &
              exportState=gridImportStateList(i), clock=clock, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          enddo
          deallocate(intValueList)
        endif

        call ESMF_GridCompInitialize(gridCompList(i), importState=gridImportStateList(i), exportState=gridExportStateList(i), &
          clock=clock, phase=phase, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo
    enddo

 ''')


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
    if (.not.allocated(cplAlarmList)) allocate(cplAlarmList(numCplAlarm))
    if (.not.allocated(cplNames)) allocate(cplNames(numCplAlarm))
    cplNames(:) = 'link'
''')
for idx,couplingItem in enumerate(couplingList):
    if couplingItem[1][:4] == 'link':
        continue
    else:
        fid.write("    cplNames(%d)='%s'\n" % (idx+1,couplingItem[1].split('_connector')[0]))
fid.write('''
    !! Set the coupling alarm starting from start time of local clock
    call ESMF_ClockGet(clock,startTime=startTime, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

''')

for i in range(0,len(couplingList)):

    string = intervals[i].split()
    number = string[0]
    if (number == 'inf') or (number == 'none') or (number == '0'):
    	  unit = 'yy'
    	  number = '99999'
    else:
      if len(string)>1:
        unit = string[1]
      else:
        unit = 'h'


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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockGetAlarmList(clock,ESMF_ALARMLIST_ALL,alarmCount=alarmCount,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (.not.allocated(alarmList)) allocate(alarmList(alarmCount))
    call ESMF_ClockGetAlarmList(clock,ESMF_ALARMLIST_ALL,alarmList=alarmList,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    do i=1,ubound(alarmList,1)
      call ESMF_AlarmGet(alarmList(i), ringTime=time, name=alarmName, rc=localrc)

      call ESMF_TimeGet(time,timeStringISOFrac=timestring)
      write(message,'(A)') trim(myName)//' alarm '//trim(alarmName)//' rings at '//trim(timestring)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

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
    type(ESMF_TimeInterval)    :: timeInterval, ringInterval
    integer(ESMF_KIND_I8)      :: advanceCount,  i, j, k, l
    integer(ESMF_KIND_I4)      :: alarmCount, petCount, localPet
    integer(ESMF_KIND_I4)      :: numGridComp, numCplComp
    integer(ESMF_KIND_I4)      :: hours, minutes, seconds

    type(ESMF_Alarm), dimension(:), allocatable :: alarmList
    type(ESMF_Alarm)        :: childAlarm
    type(ESMF_Clock)        :: childClock, myClock
    logical                 :: clockIsPresent
    type(ESMF_State)        :: impState, expState
    type(ESMF_Field)        :: field
    type(ESMF_FieldBundle)  :: fieldBundle
    type(ESMF_Array)        :: array
    type(ESMF_ArrayBundle)  :: arrayBundle
    type(ESMF_StateItem_Flag), dimension(:), allocatable :: itemTypeList
    character(len=ESMF_MAXSTR), dimension(:), allocatable:: itemNameList
    integer(ESMF_KIND_I4)   :: itemCount, localrc

    character(len=ESMF_MAXSTR) :: message, compName, alarmName, name1, name2

    integer(ESMF_KIND_I4)  :: phase, phaseCount
    integer(ESMF_KIND_I4), dimension(:), allocatable :: gridCompPhaseCountList,CplCompPhaseCountList
    logical, allocatable   :: GridCompHasPhaseZeroList(:)
    logical                :: hasPhaseZero
    integer(ESMF_KIND_I4), parameter :: maxPhaseCount=9

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, myName, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=myClock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    if (.not.allocated(alarmList)) allocate(alarmList(20))

    numGridComp=ubound(gridCompList,1)-lbound(gridCompList,1)+1
    numCplComp =ubound(cplCompList ,1)-lbound(cplCompList ,1)+1

    !! Establish number of phases and zero phase for all components
    !! @> todo this interface will likely change in the future and will
    !! be integrated with GridCompGet

    allocate(GridCompHasPhaseZeroList(numGridComp))
    allocate(gridCompPhaseCountList(numGridComp))

    do i = 1, numGridComp
      call ESMF_GridCompGetEPPhaseCount(gridCompList(i), ESMF_METHOD_RUN, &
        phaseCount=phaseCount, phaseZeroFlag=hasPhaseZero, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      gridCompPhaseCountList(i)=phaseCount
      GridCompHasPhaseZeroList(i)=hasPhaseZero
    enddo

    allocate(CplCompPhaseCountList(numCplComp))
    !!> @todo reenable if ESMF new enough
    CplCompPHaseCountList(:)=1

    !do i = 1, numCplComp
    !  call ESMF_CplCompGetEPPhaseCount(cplCompList(i), ESMF_METHOD_RUN, &
    !    phaseCount=CplCompPhaseCountList(i), phaseZeroFlag=hasPhaseZero, rc=localrc)
    !  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !  if (.not.hasPhaseZero) cycle
    !enddo

    call ESMF_ClockGetAlarmList(myClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
      alarmCount=alarmCount, rc=localrc)

    if (allocated(alarmList)) then
      if (size(alarmList)<alarmCount) then
        deallocate(alarmList)
        allocate(alarmList(alarmCount))
      endif
    else
      allocate(alarmList(alarmCount))
    endif

    !! Run until the clock's stoptime is reached
    do

      call ESMF_ClockGet(myClock,currTime=currTime, stopTime=stopTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (currTime>stopTime) then
        call ESMF_LogWrite(trim(myName)//' clock out of scope', ESMF_LOGMSG_ERROR)
        call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=localrc)
      endif

      !! Loop through all components and check whether their clock is currently at the
      !! same time as my own clock's currTime, if yes, then run the respective couplers
      do i=1,numGridComp
        !! Determine for each child the clock
        call ESMF_GridCompGet(gridCompList(i),name=compName, clockIsPresent=clockIsPresent, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (.not.clockIsPresent) then
          call ESMF_LogWrite(trim(myName)//' required clock not found in '//trim(compName), ESMF_LOGMSG_ERROR)
          call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=localrc)
        endif

        call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_ClockGet(childClock,currTime=time, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        ! write(message,'(A)') trim(myName)//' '//trim(compName)//' now at '//trim(timestring)
        !  call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE)

        if (time>currTime) cycle

        !! Find all the alarms in this child and call all the couplers that
        !! have ringing alarms at this stage

        call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
          alarmCount=alarmCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (alarmCount>ubound(alarmList,1)) then
          deallocate(alarmList)
          allocate(alarmList(alarmCount))
        endif

        if (alarmCount==0) then
          timeInterval=stopTime-currTime
          !call ESMF_LogWrite(trim(myName)//' '//trim(compName)//' has not ringing alarm at '//trim(timestring),ESMF_LOGMSG_WARNING)
        else
          call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
             alarmList=alarmList, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif

        do j=1,alarmCount
          call ESMF_AlarmGet(alarmList(j), name=alarmName, ringTime=ringTime, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !! Skip this alarm if it is not a cplAlarm
          if (index(trim(alarmName),'cplAlarm') < 1) cycle

          !! Skip this alarm if it is inbound of this component
          if (trim(alarmName(1:index(alarmName,'--')-1))/=trim(compName)) cycle

          !! Skip this alarm if it is not ringing now
          !if (ringTime > currTime) cycle

          call ESMF_TimeGet(ringTime,timeStringISOFrac=timeString)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !write(0,*) trim(compName)//' ', i,'/',alarmCount,' '//trim(alarmName)//' rings at '//trim(timeString)
          write(message,'(A)') trim(myName)//' '//trim(compName)//' '//trim(alarmName)//' rings at '//trim(timeString)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

          name1=trim(alarmName(1:index(alarmName,'--')-1))
          name2=trim(alarmName(index(alarmName,'--')+2:index(alarmName,'--cplAlarm')-1))

          do k=1,ubound(cplAlarmList,1)
            if (cplAlarmList(k) == alarmList(j)) then
              cplName = trim(cplNames(k))
              exit
            endif
          enddo

          write(message,'(A)') trim(myName)//' '//trim(timeString)//' '//trim(name1)//' ->'
          if (trim(cplName) /= 'link') then
            write(message,'(A)') trim(message)//' '//trim(cplName)//' ->'
          else
            write(message,'(A)') trim(message)//' ('//trim(cplName)//') ->'
          endif
          write(message,'(A)') trim(message)//' '//trim(name2)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

          call ESMF_GridCompGet(gridCompList(i), exportState=impState, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !! Search the gridCompList for other's name
          do k=1, ubound(gridCompList,1)
              call ESMF_GridCompGet(gridCompList(k), name=childName, rc=localrc)
              if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
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
          enddo

          call ESMF_GridCompGet(gridCompList(k), importState=expState, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          write(message,'(A)') trim(myName)//' '//trim(timeString)//' calling '//trim(cplCompNameList(l))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)
''')

fid.write('''
          call ESMF_CplCompRun(cplCompList(l), importState=impState, &
            exportState=expState, clock=controlClock, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_LogFlush()

        enddo
      enddo

      !! Loop through all components and check whether their clock is currently at the
      !! same time as my own clock's currTime, if yes, then run the component
      do i=1,numGridComp
        !! Determine for each child the clock
        call ESMF_GridCompGet(gridCompList(i),name=compName, clockIsPresent=clockIsPresent, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (.not.clockIsPresent) then
          call ESMF_LogWrite(trim(myName)//' required clock not found in '//trim(compName), ESMF_LOGMSG_ERROR)
          call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=localrc)
        endif

        call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_ClockGet(childClock,currTime=time, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_TimeGet(time,timeStringISOFrac=timeString)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (time>currTime) then
          call ESMF_TimeGet(time,timeStringISOFrac=timeString)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          write(message,'(A)') trim(myName)//' '//trim(compName)//' now at '//trim(timestring)//', but'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)

          call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          write(message,'(A)') trim(myName)//' now at '//trim(timestring)//', cycling ...'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)

          cycle
        endif

        !! Find the child's alarm list, get the interval to the next ringing alarm
        !! and run the component for the interval until that alarm

        call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
          alarmCount=alarmCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (alarmCount==0) then
          !call ESMF_LogWrite('No alarm found in '//trim(compName), ESMF_LOGMSG_WARNING)
          timeInterval=stopTime-currTime
        else
          call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
             alarmList=alarmList, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif

        !! Set the default ringTime to the stopTime of local clock, then get all Alarms
        !! from local clock into alarmList, find those that contain the string "cplAlarm"
        !! and look for the earliest ringtime in all coupling alarms.  Save that in the
        !! ringTime
        call ESMF_ClockGet(myClock, stopTime=ringTime, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        do j=1,alarmCount
          call ESMF_AlarmGet(alarmList(j), name=alarmName, ringTime=time, &
            ringInterval=ringInterval, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          if (index(trim(alarmName),'cplAlarm')<1) cycle

          if (time==currTime) ringTime=currTime+ringInterval
          if (time<ringTime) ringTime=time
        enddo

        !call ESMF_TimeGet(ringTime,timeStringISOFrac=timestring, rc=localrc)
        !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        !write(message,'(A)') trim(myName)//' setting child''s stopTime to'//trim(timeString)
        !call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=localrc);

!       TODO: do not modify childClock
!             (components need to inquire stopTime not from their own clock!)
        call ESMF_ClockSet(childClock, stopTime=ringTime, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_ClockGet(childClock, timeStep=timeInterval, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (timeInterval>ringTime-currTime) then
          !call ESMF_ClockSet(childClock, timeStep=ringTime-currTime, rc=localrc)
          !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_LogWrite(trim(myName)//" adaptive timestep must be implemented in "//trim(compName),ESMF_LOGMSG_WARNING)
        endif

        timeInterval=ringTime-currTime

        call ESMF_TimeIntervalGet(timeInterval, h=hours, m=minutes, s=seconds, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,A,I5.5,A,I2.2,A,I2.2,A)') trim(myName)//' '//trim(timeString)//' calling '//trim(compName), &
          ' to run for ', hours, ':', minutes, ':', seconds, ' hours'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=localrc)

        !! Change the controlClock with updated currTime and timeStep
        call ESMF_ClockSet(controlClock, currTime=currTime, timeStep=timeInterval, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
          call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        !! Loop over all run phases, disregarding any action that could be taken between
        !! phases
        do phase=1,gridCompPhaseCountList(i)
          !call MOSSCO_GridCompFieldsTable(gridCompList(i), importState=gridImportStateList(i), exportState=gridExportStateList(i),rc=localrc)
          call ESMF_GridCompRun(gridCompList(i),importState=gridImportStateList(i),&
            exportState=gridExportStateList(i), clock=controlClock, phase=phase, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          !call MOSSCO_GridCompFieldsTable(gridCompList(i), importState=gridImportStateList(i), exportState=gridExportStateList(i),rc=localrc)
          !call ESMF_LogFlush()
        enddo

        call ESMF_ClockGet(childClock, currTime=time, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (time == currTime) then
          !! This child component did not advance its clock in its Run() routine
          !! We do that here
          call ESMF_LogWrite(trim(myName)//' '//trim(compName)//' did not advance its clock',ESMF_LOGMSG_WARNING)
          call ESMF_LogWrite("... but this assumption is weird - skipping further action!",ESMF_LOGMSG_WARNING)

          !call ESMF_ClockAdvance(childClock, timeStep=timeInterval, rc=localrc)
          !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      enddo

      !! Now that all child components have been started, find out the minimum time
      !! to the next coupling and use this as a time step for my own clock Advance

      call ESMF_ClockGetAlarmList(myClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
        alarmCount=alarmCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (alarmCount==0) then
        !call ESMF_LogWrite('No alarm found in '//trim(myName), ESMF_LOGMSG_WARNING)
        timeInterval=stopTime-currTime
      else
        call ESMF_ClockGetAlarmList(myClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
          alarmList=alarmList, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_AlarmGet(alarmList(1), ringTime=ringTime, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        do j=2,alarmCount
          call ESMF_AlarmGet(alarmList(j), ringTime=time, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          if (time<ringTime) ringTime=time
        enddo

        timeInterval=ringTime-currTime
      endif

      !> Log current and next ring time
      call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      write(message,'(A)') trim(myName)//' '//trim(timeString)//' '//trim(myName)//' stepping to'
      call ESMF_TimeGet(ringTime,timeStringISOFrac=timestring, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      write(message,'(A)') trim(message)//' '//trim(timeString)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=localrc);

      !> Set new time interval and advance clock, stop if end of
      !! simulation reached
      call ESMF_ClockSet(myClock, timeStep=timeInterval, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_ClockAdvance(myClock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (ESMF_ClockIsStopTime(myClock, rc=localrc)) exit
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Run

#undef  ESMF_METHOD
#define ESMF_METHOD "Finalize"
  subroutine Finalize(gridComp, importState, exportState, parentClock, rc)
    type(ESMF_GridComp)  :: gridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: parentClock
    integer, intent(out) :: rc

    integer(ESMF_KIND_I8)   :: i
    integer(ESMF_KIND_I4)   :: petCount, localPet,numGridComp, numCplComp, localrc
    character(ESMF_MAXSTR)  :: myName, message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock

    integer(ESMF_KIND_I4)  :: phase, phaseCount
    integer(ESMF_KIND_I4), dimension(:), allocatable :: gridCompPhaseCountList,CplCompPhaseCountList
    logical, allocatable   :: GridCompHasPhaseZeroList(:)
    logical                :: hasPhaseZero
    integer(ESMF_KIND_I4), parameter :: maxPhaseCount=9

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, myName, currTime, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

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
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      gridCompPhaseCountList(i)=phaseCount
      GridCompHasPhaseZeroList(i)=hasPhaseZero
    enddo

    allocate(CplCompPhaseCountList(numCplComp))
    !> @todo reenable if implemented in ESMF versions
    CplCompPhaseCountList(:)=1

    !do i = 1, numCplComp
    !  call ESMF_CplCompGetEPPhaseCount(cplCompList(i), ESMF_METHOD_FINALIZE, &
    !    phaseCount=CplCompPhaseCountList(i), phaseZeroFlag=hasPhaseZero, rc=localrc)
    !  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    !  if (.not. hasPhaseZero) cycle
    !enddo

    do i=1,ubound(cplCompList,1)
      call ESMF_CplCompFinalize(cplCompList(i), clock=clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo
    do i=1,ubound(gridCompList,1)
      do phase=1,gridCompPhaseCountList(i)
        call ESMF_GridCompFinalize(gridCompList(i), clock=clock, phase=phase, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo
    enddo
    do i=1,ubound(gridCompList,1)
      !!@todo destroy any remaining fields/arrays in states
      call ESMF_StateDestroy(gridExportStateList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateDestroy(gridImportStateList(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
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

    call MOSSCO_CompExit(gridComp, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  end subroutine Finalize

end module toplevel_component
''')

fid.close()

outfilename='Makefile.coupling'
fid = file(outfilename,'w')

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
export MOSSCO_DIR=$(subst /examples/generic$,,$(PWD))
endif

include $(MOSSCO_DIR)/src/Rules.make

''')

# Place conditionals for building this coupled system
conditionals = {'gotm' : 'GOTM', 'fabm' : 'FABM', 'erosed' : 'EROSED',
                'fabm_gotm' : 'GOTM_FABM', 'getm' : 'GETM', 'gotmfabm' : 'GOTM_FABM'}
for item in gridCompSet.union(cplCompSet):
    if conditionals.has_key(item):
        fid.write('ifneq ($(MOSSCO_' + conditionals[item] + '),true)\n')
        fid.write('$(error This example only works with MOSSCO_' + conditionals[item] + ' = true)\n')
        fid.write('endif\n')

libs = {'gotm'       : ['solver', 'mossco_gotm'] ,
        'gotmfabm'   : ['mossco_gotmfabm','mossco_fabmpelagic', 'mossco_gotm', 'solver'],
        'fabm_gotm'                : ['mossco_fabmgotm','solver'],
        'fabm_sediment' : ['sediment', 'mossco_sediment', 'solver'],
        'fabm_pelagic' : ['mossco_fabmpelagic', 'util', 'solver'],
        'constant'   : ['constant'],
        'constant_grid'   : ['constant_grid'],
        'clm_netcdf' : ['mossco_clm'],
        'benthos'    : ['mossco_benthos'],
        'erosed'     : ['mossco_erosed'],
        'hamsom'     : ['mossco_hamsom'],
        'tracer'     : ['mossco_tracer'],
        'netcdf'     : ['mossco_netcdf'],
        'netcdf_input'     : ['mossco_netcdf'],
        'test'       : ['mossco_test'],
        'simplewave' : ['mossco_simplewave'],
        'river'      : ['mossco_river'],
        'empty'      : ['empty'],
        'inout'      : ['mossco_inout'],
        'info'       : ['mossco_info'],
        'fabm0d'     : ['mossco_fabm0d', 'solver', 'airsea',
                        'input', 'util', 'fabm'],
        'pelagic_benthic_mediator' : ['mossco_mediator'],
        'pelagic_soil_connector' : ['mossco_mediator'],
        'soil_pelagic_connector' : ['mossco_mediator'],
        'pelagic_benthic_coupler' : ['pelagicbenthiccoupler'],
        'benthic_pelagic_coupler' : ['pelagicbenthiccoupler'],
        'xgrid_coupler' : ['xgridcoupler'],
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
        'tracer'     : ['libmossco_tracer'],
        'erosed'     : ['libmossco_erosed'],
        'fabm0d'     : ['libmossco_fabm0d'],
        'fabm_sediment' : ['libsediment', 'libmossco_sediment', 'libsolver'],
        'fabm_pelagic' : ['libmossco_fabmpelagic', 'libsolver'],
        'simplewave' : ['libmossco_simplewave'],
        'netcdf'      : ['libmossco_netcdf'],
        'netcdf_input'      : ['libmossco_netcdf'],
        'test'       : ['libmossco_test'],
        'info'       : ['libmossco_info'],
        'empty'      : ['libempty'],
        'river'      : ['libmossco_river'],
        'inout'      : ['libmossco_inout'],
        'constant'   : ['libconstant'],
        'constant_grid'  : ['libconstant_grid'],
        'gotm'       : ['libmossco_gotm', 'libsolver'],
        'fabm_gotm'                : ['libmossco_fabmgotm'],
        'gotmfabm'       : ['libmossco_gotmfabm', 'libsolver'],
        'pelagic_benthic_mediator' : ['libmossco_mediator'],
        'pelagic_soil_connector' : ['libmossco_mediator'],
        'soil_pelagic_connector' : ['libmossco_mediator'],
        'pelagic_benthic_coupler' : ['libpelagicbenthiccoupler'],
        'benthic_pelagic_coupler' : ['libpelagicbenthiccoupler'],
        'xgrid_coupler' : ['libxgridcoupler'],
        'link_connector' : ['libmossco_connector'],
        'rename_connector' : ['libmossco_connector'],
        'flux_connector' : ['libmossco_connector'],
        'transport_connector' : ['libmossco_connector'],
        'copy_coupler' : ['libcopycoupler'],
        'regrid_coupler' : ['libregridcoupler'],
        'remtc_atmosphere' : ['libremtc'],
        'remtc_ocean' : ['libremtc'],
        'getm' : ['libmossco_getm'],
}

#fid.write('\nNC_LIBS += $(shell nf-config --flibs)\n\n')
fid.write('LDFLAGS += -L$(MOSSCO_LIBRARY_PATH)\n')
for item in gridCompSet.union(cplCompSet):
    if instanceDict.has_key(item):
        item=instanceDict[item]
    if libs.has_key(item):
        fid.write('LDFLAGS +=')
        for lib in libs[item]:
            fid.write(' -l' + lib)
        if item=='gotm':
            fid.write(' $(GOTM_LDFLAGS)')
        if item=='getm':
            fid.write(' $(GETM_LDFLAGS)')
        if item=='fabm_sediment':
            fid.write(' $(FABM_LDFLAGS)')
        if item=='fabm_pelagic':
            fid.write(' $(FABM_LDFLAGS)')
        if item=='fabm':
            fid.write(' $(FABM_LDFLAGS) -L$(GOTM_LIBRARY_PATH)')
        if item=='fabm_gotm':
            fid.write(' $(GOTM_LDFLAGS) $(FABM_LDFLAGS)')
        if item=='gotmfabm':
            fid.write(' $(GOTM_LDFLAGS) $(FABM_LDFLAGS)')
        if item=='fabm0d':
            fid.write(' $(FABM_LDFLAGS) -L$(GOTM_LIBRARY_PATH)')
        fid.write('\n')

#fid.write('LDFLAGS += $(LIBS) -lmossco_util -lesmf $(ESMF_NETCDF_LIBS)  -llapack\n\n')
fid.write('LDFLAGS += -lmossco_util $(ESMF_LDFLAGS)  \n\n')

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
    if instanceDict.has_key(item):
        item=instanceDict[item]
    if deps.has_key(item):
        for dep in deps[item]:
            fid.write(' ' + dep)
fid.write(' ' + coupling_name + '\n\n')
fid.write(coupling_name + ': toplevel_component.o ../common/main.o\n')
fid.write('\t$(F90) $(F90FLAGS) $^ $(LDFLAGS) -o $@\n')
fid.write('\t@echo "Created example binary $(PWD)/$@"\n')
fid.write('''

# Other subsidiary targets that might not be needed, these should evetually
# end up in some global Rules.make

libmossco_gotmfabm libmossco_gotm libmossco_fabmgotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

libmossco_util libsolver:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libsediment libconstant libconstant_grid libmossco_clm libmossco_erosed \
libmossco_fabm0d libmossco_fabmpelagic:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libempty libmossco_inout libmossco_getm libmossco_simplewave libmossco_netcdf libmossco_benthos:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_info libmossco_test libmossco_river:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_sediment:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers $@

libsurfacescoupler libaocoupler liblinkcoupler libxgridcoupler libregridcoupler libcopycoupler libmossco_coupler:
	$(MAKE) -C $(MOSSCO_DIR)/src/mediators $@

libmossco_connector:
	$(MAKE) -C $(MOSSCO_DIR)/src/connectors $@

libmossco_mediator:
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
	@-rm -f %s

'''%coupling_name)
fid.close()



