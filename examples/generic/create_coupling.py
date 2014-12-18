#!/usr/bin/env python
# This script is is part of MOSSCO. It creates from YAML descriptions of
# couplings a toplevel_component.F90 source file
#
# @copyright (C) 2014 Helmholtz-Zentrum Geesthacht
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
     filename='reference_3d'

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
    copyright = 'Copyright (C) 2014, Helmholtz-Zentrum Geesthacht'

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
cplCompList=[]
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
                couplingList.append([item["components"][0], "link_coupler", item["components"][-1]])
                cplCompList.append("link_coupler")
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
             couplingList.append(key, "link_coupler",value)
          cplCompList.append("link_coupler")

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

dependencyDict={}
for component in componentSet:
  if type(dependencies) is dict:
    for item in dependencies:
        compdeps=[]
        if type(item) is dict:
          for jtem in item.values():
              if type(jtem) is str:
                 compdeps.append(jtem)
              elif (type(jtem) is dict) and jtem.has_key('component'):
                 compdeps.append(jtem['component'])
                 if jtem.has_key('grid'):
                    foreignGrid[item.keys()[0]]=jtem['grid']
        if type(compdeps) is list:
          for compdep in compdeps:
            if componentList.index(component)< componentList.index(compdep):
                   c=componentList.pop(componentList.index(compdep))
                   componentList.insert(componentList.index(component),c)
        elif componentList.index(component)< componentList.index(compdeps):
              c=componentList.pop(componentList.index(compdeps))
              componentList.insert(componentList.index(component),c)
    if dependencyDict.has_key(item):
      dependencyDict[item].extend(compdeps)
    else:
      dependencyDict[item]=compdeps

  elif type(dependencies) is list:
    for i in range(0,len(dependencies)):
        item=dependencies[i]
        compdeps=[]
        if type(item) is dict:
          for jtem in item.values():
              if type(jtem) is list and len(jtem) == 1:
                  jtem=jtem[0]
              if type(jtem) is str:
                 compdeps.append(jtem)
              elif (type(jtem) is dict) and jtem.has_key('component'):
                 compdeps.append(jtem['component'])
                 if jtem.has_key('grid'):
                    foreignGrid[item.keys()[0]]=jtem['grid']
        if type(compdeps) is list:
          for compdep in compdeps:
            if componentList.index(component)< componentList.index(compdep):
                   c=componentList.pop(componentList.index(compdep))
                   componentList.insert(componentList.index(component),c)
        elif componentList.index(component)< componentList.index(compdeps):
              c=componentList.pop(componentList.index(compdeps))
              componentList.insert(componentList.index(component),c)
        if dependencyDict.has_key(item.keys()[0]):
          dependencyDict[item.keys()[0]].extend(compdeps)
        else:
          dependencyDict[item.keys()[0]]=compdeps
  else:
    print 'The dependencies specification must be list or dictionary'

for key, value in dependencyDict.iteritems():
    dependencyDict[key]=list(set(value))


if 'link_coupler' in componentList:
    c=componentList.pop(componentList.index('link_coupler'))
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

print 'Components to process:', componentList
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

# print gridCompList, cplCompList

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

for jtem in gridCompList:
    if instanceDict.has_key(jtem):
        item=instanceDict[jtem]
    else: item=jtem
    fid.write('  use ' + item + '_component, only : ' + item + '_SetServices => SetServices \n')
for jtem in cplCompList:
    if instanceDict.has_key(jtem):
        item=instanceDict[jtem]
    else: item=jtem
    fid.write('  use ' + item + ', only : ' + item + '_SetServices => SetServices \n')

fid.write('\n  implicit none\n\n  private\n\n  public SetServices\n')
fid.write('''
  type(ESMF_GridComp),dimension(:),save, allocatable :: gridCompList
  type(ESMF_CplComp),dimension(:), save, allocatable :: cplCompList
  type(ESMF_State), dimension(:),  save, allocatable :: exportStates, importStates
  type(ESMF_Alarm), dimension(:),  save, allocatable :: cplAlarmList
  type(ESMF_Clock), dimension(:),  save, allocatable :: gridCompClockList, cplCompClockList
  character(len=ESMF_MAXSTR), dimension(:), save, allocatable :: gridCompNames, cplCompNames, cplNames
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
    character(len=ESMF_MAXSTR)  :: name, message
    type(ESMF_Time)             :: currTime
    integer                     :: localrc

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
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
    character(ESMF_MAXSTR) :: name, message
    type(ESMF_Alarm)       :: childAlarm
    type(ESMF_Clock)       :: childClock
    type(ESMF_Clock)       :: clock !> This component's internal clock
    logical                :: clockIsPresent
    integer(ESMF_KIND_I4), allocatable :: petList(:)
    type(ESMF_VM)          :: vm

    integer(ESMF_KIND_I4)  :: phase, phaseCount, j, itemCount
    integer(ESMF_KIND_I4), dimension(:), allocatable :: phaseCountList
    logical, allocatable   :: hasPhaseZeroList(:)
    logical                :: hasPhaseZero, isPresent
    integer(ESMF_KIND_I4), parameter :: maxPhaseCount=9
    
    integer(ESMF_KIND_I4), allocatable      :: intValueList(:)
    character(len=ESMF_MAXSTR), allocatable :: charValueList(:)

    rc = ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

    !! Allocate the fields for all gridded components and their names
''')
fid.write('    numGridComp = ' + str(len(gridCompList)) )
fid.write('''
    allocate(gridCompList(numGridComp))
    allocate(gridCompClockList(numGridComp))
    allocate(gridCompNames(numGridComp))
    allocate(importStates(numGridComp))
    allocate(exportStates(numGridComp))

''')
for i in range(0, len(gridCompList)):
    fid.write('    gridCompNames(' + str(i+1) + ') = \'' + gridCompList[i] + '\'\n')

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
      call ESMF_ClockSet(gridCompClockList(i), name=trim(gridCompNames(i)), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

''')
for i in range(0, len(gridCompList)):

    if (petList[i]=='all'):
        fid.write('    gridCompList(' + str(i+1) + ') = ESMF_GridCompCreate(name=trim(gridCompNames(' + str(i+1) + ')),  &\n')
        fid.write('      petList=petList, clock=gridCompClockList(' + str(i+1) + '), rc=localrc)\n')
    else:
       fid.write('    if (petCount<=' + str(max(petList[i])) + ') then\n')
       fid.write('      write(message,\'(A,I4)\') \'This configuration requires more than ' + max(petList[i]) + ' PET, I got only \',petCount\n')
       fid.write('      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)\n')
       fid.write('      write(0,\'(A)\') trim(message)\n')
       fid.write('      call ESMF_Finalize(endflag=ESMF_END_ABORT)\n')
       fid.write('    endif\n')
       fid.write('    gridCompList(' + str(i+1) + ') = ESMF_GridCompCreate(name=trim(gridCompNames(' + str(i+1) + ')),  &\n')
       fid.write('      petList=(/' + petList[i] + '/), clock=gridCompClockList(' + str(i+1) + '), rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n')
fid.write('''
    do i=1, numGridComp
      exportStates(i) = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED, &
        name=trim(gridCompNames(i))//'ExportState')
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      importStates(i) = ESMF_StateCreate(stateintent=ESMF_STATEINTENT_UNSPECIFIED, &
        name=trim(gridCompNames(i))//'ImportState')
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
    allocate(cplCompNames(numCplComp))
    allocate(cplCompClockList(numCplComp))
''')

for i in range(0, len(cplCompList)):
    fid.write('    cplCompNames(' + str(i+1) + ') = \'' + cplCompList[i] + '\'\n')
fid.write('''

    do i = 1, numCplComp
      cplCompClockList(i) = ESMF_ClockCreate(clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_ClockSet(cplCompClockList(i), name=trim(cplCompNames(i)), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      cplCompList(i) = ESMF_CplCompCreate(name=trim(cplCompNames(i)), clock=cplCompClockList(i), rc=localrc)
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

    allocate(hasPhaseZeroList(numGridComp))
    allocate(phaseCountList(numGridComp))

    do i = 1, numGridComp
      call ESMF_GridCompGetEPPhaseCount(gridCompList(i), ESMF_METHOD_INITIALIZE, &
        phaseCount=phaseCount, phaseZeroFlag=hasPhaseZero, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      phaseCountList(i)=phaseCount
      hasPhaseZeroList(i)=hasPhaseZero
    enddo

    !! Go through all phase 0 if components have it
    do i = 1,numGridcomp
      if (.not.hasPhaseZeroList(i)) cycle
      call ESMF_GridCompInitialize(gridCompList(i), exportState=exportStates(i), phase=0, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      !!> @todo expect the Attribute InitializePhaseMap in this state, this attribute
      !! contains information on the phases defined in the component.
    enddo

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
    print item
    fid.write('    call ESMF_AttributeSet(importStates(' + str(ito+1)+'), name="foreign_grid_field_name", value="'+foreignGrid[item]+'", rc=localrc)\n')
    fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
  
  if dependencyDict.has_key(item):
    fid.write('    allocate(charValueList(' + str(len(dependencyDict[item])) + '), intValueList(' + str(len(dependencyDict[item])) + '))\n')
    for i,jtem in enumerate(dependencyDict[item]):
      fid.write('    charValueList(' + str(i+1) + ') = \'' + jtem + '\'\n')
      fid.write('    intValueList (' + str(i+1) + ') = ' + str(ifrom + 1) + '\n')
    fid.write('    call ESMF_AttributeSet(importStates(' + str(ito+1)+'), name="depends_on", valueList=charValueList, rc=localrc)\n')  
    fid.write('    call ESMF_AttributeSet(importStates(' + str(ito+1)+'), name="depends_on_id", valueList=intValueList, rc=localrc)\n')  
    fid.write('    deallocate(charValueList)\n')
    fid.write('    deallocate(intValueList)\n')

fid.write('''
    !! Go through all phases:
    !! IPDv00p1 = phase 1: Advertise Fields in import and export States. These can be
    !!   empty fields that are later completed with FieldEmptyComplete
    !! IPDv00p2 = phase 2: Realize Fields (that have not been completed in phase 1)

''')

maxPhases=2

for phase in range(1,maxPhases+1):
  fid.write('    phase = %i \n'%phase)
  for item in gridCompList:
    fid.write('    !! Initializing phase '  + str(phase) + ' of ' + item + '\n')
    ifrom=gridCompList.index(item)
    ito=ifrom
    for j in range(0, len(couplingList)):
        jtem=couplingList[j]
        if jtem[-1]==item:
            ifrom=gridCompList.index(jtem[0])
    j=gridCompList.index(item)
    if foreignGrid.has_key(item):
      fid.write('    if (    (phase.eq.1 .and. phaseCountList( ' + str(ito+1) + ').eq.1) &\n')
      fid.write('        .or.(phase.eq.2 .and. phaseCountList( ' + str(ito+1) + ').gt.1) ) then\n')
      fid.write('    call ESMF_AttributeSet(importStates(' + str(ito+1)+'), name="foreign_grid_field_name", value="'+foreignGrid[item]+'", rc=localrc)\n')
      fid.write('    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)\n\n')
      
      if dependencyDict.has_key(item):
        for jtem in dependencyDict[item]:
          ifrom=gridCompList.index(jtem)
          fid.write('    call ESMF_CplCompInitialize(cplCompList(1), importState=exportStates(' + str(ifrom+1) + '), &\n')
          fid.write('      exportState=importStates(' + str(ito+1)+'), clock=clock, rc=localrc)\n')
          fid.write('    call ESMF_LogFlush()\n')

      fid.write('    end if\n\n')
          
    fid.write('    if (phaseCountList( ' + str(ito+1) + ')>=' + str(phase) + ') then\n')
    fid.write('      call ESMF_GridCompInitialize(gridCompList(' + str(ito+1) + '), importState=importStates(' + str(ito+1) + '), &\n')
    fid.write('        exportState=exportStates(' + str(ito+1) + '), clock=clock, phase=' + str(phase) + ', rc=localrc)\n')
    fid.write('    endif\n')
    fid.write('''
''')

fid.write('''
    do phase=1, -9
      do i=1, numGridComp
        if (phaseCountList(i) < phase) cycle
        call ESMF_AttributeGet(importStates(i), 'depends_on_id', isPresent=isPresent, rc=localrc)  
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        if (isPresent) then
          call ESMF_AttributeGet(importStates(i), 'depends_on_id', itemCount=itemCount, rc=localrc)  
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          allocate(intValueList(itemCount))
          call ESMF_AttributeGet(importStates(i), 'depends_on_id', valueList=intValueList, rc=localrc)  
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)          
          do j=1, itemCount
            call ESMF_CplCompInitialize(cplCompList(1), importState=exportStates(intValueList(j)), &
              exportState=importStates(i), clock=clock, rc=localrc)
            if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)          
          enddo
          deallocate(intValueList)
        endif
      
        call ESMF_GridCompInitialize(gridCompList(i), importState=importStates(i), exportState=exportStates(i), &
          clock=clock, phase=phase, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo
    enddo
''')

for icpl in range(1,len(cplCompList)):
    item=cplCompList[icpl]
    for i in range(0, len(couplingList)):
        jtem=couplingList[i]
        if item==jtem[1]:
          ifrom=gridCompList.index(jtem[0])
          ito  =gridCompList.index(jtem[2])
          break
    fid.write('    !! Initializing ' + jtem[1] + '\n')
    fid.write('    call ESMF_CplCompInitialize(cplCompList(' + str(icpl+1) + '), importState=exportStates(' + str(ifrom+1) + '), &\n')
    fid.write('      exportState=importStates(' + str(ito+1) + '), clock=clock, phase=' + str(1) + ', rc=localrc)\n')
    fid.write('''
    if (rc /= ESMF_SUCCESS) then
      if ((rc == ESMF_RC_ARG_SAMECOMM .or. rc==506) .and. phase>1) then
        write(message,'(A,I4)') 'There is no initialization defined for phase=', phase
        write(message,'(A,A)') trim(message),' For now, ignore errors  immediately above'
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_WARNING)
      else
        write(message,'(A,I4)') 'Initializing failed with error code ', rc
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
        call ESMF_LogFlush()
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    endif
''')


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
        fid.write("    cplNames(%d)='%s'\n" % (idx+1,couplingItem[1].split('_coupler')[0]))
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
    fid.write('    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)\n\n')
    fid.write('    cplAlarmList(' + str(i+1) + ')=ESMF_AlarmCreate(clock=clock,ringTime=startTime+alarmInterval, &\n')
    alarmName = str(couplingList[i][0]) + '--' + str(couplingList[i][-1]) + '--cplAlarm'
    fid.write('      ringInterval=alarmInterval, name=\'' + alarmName + '\', rc=localrc)\n')
    fid.write('    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)\n')

    fid.write('''
    !! Copy this alarm to all children as well
    do i=1,numGridComp
      call ESMF_GridCompGet(gridCompList(i),name=name, rc=localrc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

    ''')
    fid.write('  if (trim(name)==\'' + str(couplingList[i][0]) + '\' .or. trim(name)==\'' + str(couplingList[i][-1]) + '\') then')
    fid.write('''
        call ESMF_GridCompGet(gridCompList(i), clockIsPresent=clockIsPresent, rc=localrc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

        if (clockIsPresent) then
          call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=localrc)
        else
          call ESMF_LOGWRITE('Creating clock for '//trim(name)//', this should have been done by the component.', &
            ESMF_LOGMSG_WARNING)

          childClock=ESMF_ClockCreate(clock=clock, rc=localrc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

          call ESMF_GridCompSet(gridCompList(i),clock=childClock, rc=localrc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
        endif
    ''')
    fid.write('    childAlarm=ESMF_AlarmCreate(cplAlarmList(' + str(i+1) + '), rc=localrc)')
    fid.write('''
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

        call ESMF_AlarmSet(childAlarm, clock=childClock)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

      endif
    enddo
    ''')


fid.write('''
    !! Set the default ringTime to the stopTime of local clock, then get all Alarms
    !! from local clock into alarmList, find those that contain the string "cplAlarm"
    !! and look for the earliest ringtime in all coupling alarms.  Save that in the
    !! ringTime
    call ESMF_ClockGet(clock, stopTime=ringTime, rc=localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

    call ESMF_ClockGetAlarmList(clock,ESMF_ALARMLIST_ALL,alarmCount=alarmCount,rc=localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    if (.not.allocated(alarmList)) allocate(alarmList(alarmCount))
    call ESMF_ClockGetAlarmList(clock,ESMF_ALARMLIST_ALL,alarmList=alarmList,rc=localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

    do i=1,ubound(alarmList,1)
      call ESMF_AlarmGet(alarmList(i), ringTime=time, name=name, rc=localrc)

      call ESMF_TimeGet(time,timeStringISOFrac=timestring)
      write(message,'(A)') trim(name)//' rings at '//trim(timestring)
      call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

      if (index(trim(name),'cplAlarm') < 1) cycle
      if (time<ringTime) ringTime=time
    enddo
    if (allocated(alarmList)) deallocate(alarmList)

    !! Set the timestep such that it corresponds to the time until the
    !! first ringing alarm, log that time
    call ESMF_ClockGet(clock,currTime=currTime,rc=localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    call ESMF_ClockSet(clock,timeStep=ringTime-currTime,rc=localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

    call ESMF_GridCompGet(gridComp, name=name, rc=localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

    call ESMF_TimeGet(ringTime,timeStringISOFrac=timestring, rc=localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
    write(message,'(A)') trim(name)//' alarms ring next at '//trim(timestring)
    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

    call MOSSCO_CompExit(gridComp, localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

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

    character(len=ESMF_MAXSTR) :: timestring, cplName, myName
    type(ESMF_Time)            :: stopTime, currTime, ringTime, time
    type(ESMF_TimeInterval)    :: timeInterval, ringInterval
    integer(ESMF_KIND_I8)      :: advanceCount,  i, j, k, l
    integer(ESMF_KIND_I4)      :: alarmCount, petCount, localPet
    integer(ESMF_KIND_I4)      :: numGridComp, numCplComp
    integer(ESMF_KIND_I4)      :: hours, minutes, seconds

    type(ESMF_Alarm), dimension(:), allocatable :: alarmList
    type(ESMF_Alarm)        :: childAlarm
    type(ESMF_Clock)        :: childClock, clock
    logical                 :: clockIsPresent
    type(ESMF_State)        :: impState, expState
    type(ESMF_Field)        :: field
    type(ESMF_FieldBundle)  :: fieldBundle
    type(ESMF_Array)        :: array
    type(ESMF_ArrayBundle)  :: arrayBundle
    type(ESMF_StateItem_Flag), dimension(:), allocatable :: itemTypeList
    character(len=ESMF_MAXSTR), dimension(:), allocatable:: itemNameList
    integer(ESMF_KIND_I4)   :: itemCount, localrc

    character(len=ESMF_MAXSTR) :: message, compName, name, alarmName, otherName

    integer(ESMF_KIND_I4)  :: phase, phaseCount
    integer(ESMF_KIND_I4), dimension(:), allocatable :: phaseCountList
    logical, allocatable   :: hasPhaseZeroList(:)
    logical                :: hasPhaseZero
    integer(ESMF_KIND_I4), parameter :: maxPhaseCount=9

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

    if (.not.allocated(alarmList)) allocate(alarmList(20))

    numGridComp=ubound(gridCompList,1)-lbound(gridCompList,1)+1

    !! Establish number of phases and zero phase for all components
    !! @> todo this interface will likely change in the future and will
    !! be integrated with GridCompGet

    allocate(hasPhaseZeroList(numGridComp))
    allocate(phaseCountList(numGridComp))

    do i = 1, numGridComp
      call ESMF_GridCompGetEPPhaseCount(gridCompList(i), ESMF_METHOD_RUN, &
        phaseCount=phaseCount, phaseZeroFlag=hasPhaseZero, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      phaseCountList(i)=phaseCount
      hasPhaseZeroList(i)=hasPhaseZero
    enddo

    call ESMF_ClockGetAlarmList(clock, alarmListFlag=ESMF_ALARMLIST_ALL, &
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

      call ESMF_ClockGet(clock,currTime=currTime, stopTime=stopTime, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

      if (currTime>stopTime) then
        call ESMF_LogWrite('Clock out of scope in '//trim(compName), ESMF_LOGMSG_ERROR)
        call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=localrc)
      endif

      !! Loop through all components and check whether their clock is currently at the
      !! same time as my own clock's currTime, if yes, then run the respective couplers
      do i=1,numGridComp
        !! Determine for each child the clock
        call ESMF_GridCompGet(gridCompList(i),name=compName, clockIsPresent=clockIsPresent, rc=localrc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

        if (.not.clockIsPresent) then
          call ESMF_LogWrite('Required clock not found in '//trim(compName), ESMF_LOGMSG_ERROR)
          call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=localrc)
        endif

        call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=localrc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

        call ESMF_ClockGet(childClock,currTime=time, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        ! write(message,'(A)') trim(compName)//' now at '//trim(timestring)
        !  call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE)

        if (time>currTime) cycle

        !! Find all the alarms in this child and call all the couplers that
        !! have ringing alarms at this stage

        call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
          alarmCount=alarmCount, rc=localrc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

        if (alarmCount>ubound(alarmList,1)) then
          deallocate(alarmList)
          allocate(alarmList(alarmCount))
        endif

        if (alarmCount==0) then
          timeInterval=stopTime-currTime
          !call ESMF_LogWrite(trim(compName)//' has not ringing alarm at '//trim(timestring),ESMF_LOGMSG_WARNING)
        else
          call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
             alarmList=alarmList, rc=localrc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
        endif

        do j=1,alarmCount
          call ESMF_AlarmGet(alarmList(j), name=alarmName, ringTime=ringTime, rc=localrc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

          !! Skip this alarm if it is not a cplAlarm
          if (index(trim(alarmName),'cplAlarm') < 1) cycle

          !! Skip this alarm if it is inbound of this component
          if (trim(alarmName(1:index(alarmName,'--')-1))/=trim(compName)) cycle

          !! Skip this alarm if it is not ringing now
          !if (ringTime > currTime) cycle

          call ESMF_TimeGet(ringTime,timeStringISOFrac=timeString)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

          !write(0,*) trim(compName)//' ', i,'/',alarmCount,' '//trim(alarmName)//' rings at '//trim(timeString)
          write(message,'(A)') trim(compName)//' '//trim(alarmName)//' rings at '//trim(timeString)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

          myName=trim(alarmName(1:index(alarmName,'--')-1))
          otherName=trim(alarmName(index(alarmName,'--')+2:index(alarmName,'--cplAlarm')-1))

          do k=1,ubound(cplAlarmList,1)
            if (cplAlarmList(k) == alarmList(j)) then
              cplName = trim(cplNames(k))
              exit
            endif
          enddo

          write(message,'(A)') trim(timeString)//' '//trim(myName)//' ->'
          if (trim(cplName) /= 'link') then
            write(message,'(A)') trim(message)//' '//trim(cplName)//' ->'
          else
            write(message,'(A)') trim(message)//' ('//trim(cplName)//') ->'
          endif
          write(message,'(A)') trim(message)//' '//trim(otherName)
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)

          call ESMF_GridCompGet(gridCompList(i), exportState=impState, rc=localrc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

          !! Search the gridCompList for other's name
          do k=1, ubound(gridCompList,1)
              call ESMF_GridCompGet(gridCompList(k), name=name, rc=localrc)
              if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
              if (trim(name)==trim(otherName)) exit
          enddo

          if (trim(name) /= trim(otherName)) then
            write(message,'(A)') 'Did not find component '//trim(otherName)
            call ESMF_LogWrite(trim(message), ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
          endif

          !! Search the cplCompList for cplName
          do l=1, ubound(cplCompNames,1)
              !write(0,*) l,trim(cplCompNames(l))//' ?= '//trim(cplName)//'_coupler'
              if (trim(cplCompNames(l))==trim(cplName)//'_coupler') exit
          enddo

          call ESMF_GridCompGet(gridCompList(k), importState=expState, rc=localrc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

          call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          write(message,'(A)') trim(timeString)//' Calling '//trim(cplCompNames(l))
          call ESMF_LogWrite(trim(message), ESMF_LOGMSG_TRACE)
''')

fid.write('''
          call ESMF_CplCompRun(cplCompList(l), importState=impState, &
            exportState=expState, clock=clock, rc=localrc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
          call ESMF_LogFlush()

        enddo
      enddo

      !! Loop through all components and check whether their clock is currently at the
      !! same time as my own clock's currTime, if yes, then run the component
      do i=1,numGridComp
        !! Determine for each child the clock
        call ESMF_GridCompGet(gridCompList(i),name=compName, clockIsPresent=clockIsPresent, rc=localrc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

        if (.not.clockIsPresent) then
          call ESMF_LogWrite('Required clock not found in '//trim(compName), ESMF_LOGMSG_ERROR)
          call ESMF_FINALIZE(endflag=ESMF_END_ABORT, rc=localrc)
        endif

        call ESMF_GridCompGet(gridCompList(i), clock=childClock, rc=localrc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

        call ESMF_ClockGet(childClock,currTime=time, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        call ESMF_TimeGet(time,timeStringISOFrac=timeString)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (time>currTime) then
          call ESMF_TimeGet(time,timeStringISOFrac=timeString)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          write(message,'(A)') trim(compName)//' now at '//trim(timestring)//', but'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)

          call ESMF_TimeGet(currTime,timeStringISOFrac=timeString)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          write(message,'(A)') trim(name)//' now at '//trim(timestring)//', cycling ...'
          call ESMF_LogWrite(trim(message),ESMF_LOGMSG_WARNING)

          cycle
        endif

        !! Find the child's alarm list, get the interval to the next ringing alarm
        !! and run the component for the interval until that alarm

        call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
          alarmCount=alarmCount, rc=localrc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

        if (alarmCount==0) then
          !call ESMF_LogWrite('No alarm found in '//trim(compName), ESMF_LOGMSG_WARNING)
          timeInterval=stopTime-currTime
        else
          call ESMF_ClockGetAlarmList(childClock, alarmListFlag=ESMF_ALARMLIST_ALL, &
             alarmList=alarmList, rc=localrc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
        endif

        !! Set the default ringTime to the stopTime of local clock, then get all Alarms
        !! from local clock into alarmList, find those that contain the string "cplAlarm"
        !! and look for the earliest ringtime in all coupling alarms.  Save that in the
        !! ringTime
        call ESMF_ClockGet(clock, stopTime=ringTime, rc=localrc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

        do j=1,alarmCount
          call ESMF_AlarmGet(alarmList(j), name=alarmName, ringTime=time, &
            ringInterval=ringInterval, rc=localrc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
          if (index(trim(alarmName),'cplAlarm')<1) cycle

          if (time==currTime) ringTime=currTime+ringInterval
          if (time<ringTime) ringTime=time
        enddo

        !call ESMF_TimeGet(ringTime,timeStringISOFrac=timestring, rc=localrc)
        !if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
        !write(message,'(A)') 'Setting child''s stopTime to'//trim(timeString)
        !call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=localrc);


        call ESMF_ClockSet(childClock, stopTime=ringTime, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        call ESMF_ClockGet(childClock, timeStep=timeInterval, rc=localrc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
        if (timeInterval>ringTime-currTime) then
          call ESMF_ClockSet(childClock, timeStep=ringTime-currTime, rc=localrc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
        endif

        timeInterval=ringTime-currTime

        call ESMF_TimeIntervalGet(timeInterval, h=hours, m=minutes, s=seconds, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        write(message,'(A,A,I5.5,A,I2.2,A,I2.2,A)') trim(timeString)//' calling '//trim(compName), &
          ' to run for ', hours, ':', minutes, ':', seconds, ' hours'
        call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=localrc);

        !! Loop over all run phases, disregarding any action that could be taken between
        !! phases
        do phase=1,phaseCountList(i)
          call ESMF_GridCompRun(gridCompList(i),importState=importStates(i),&
            exportState=exportStates(i), clock=clock, phase=phase, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
          call ESMF_LogFlush()
        enddo

        call ESMF_ClockGet(childClock, currTime=time, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

        if (time == currTime) then
          !! This child component did not advance its clock in its Run() routine
          !! We do that here
          call ESMF_LogWrite(trim(compName)//' did not advance its clock',ESMF_LOGMSG_WARNING)

          call ESMF_ClockAdvance(childClock, timeStep=timeInterval, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
        endif
      enddo

      !! Now that all child components have been started, find out the minimum time
      !! to the next coupling and use this as a time step for my own clock Advance
      call ESMF_GridCompGet(gridComp, name=name, rc=localrc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

      call ESMF_ClockGetAlarmList(clock, alarmListFlag=ESMF_ALARMLIST_ALL, &
        alarmCount=alarmCount, rc=localrc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

      if (alarmCount==0) then
        !call ESMF_LogWrite('No alarm found in '//trim(name), ESMF_LOGMSG_WARNING)
        timeInterval=stopTime-currTime
      else
        call ESMF_ClockGetAlarmList(clock, alarmListFlag=ESMF_ALARMLIST_ALL, &
          alarmList=alarmList, rc=localrc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

        call ESMF_AlarmGet(alarmList(1), ringTime=ringTime, rc=localrc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

        do j=2,alarmCount
          call ESMF_AlarmGet(alarmList(j), ringTime=time, rc=localrc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

          if (time<ringTime) ringTime=time
        enddo

        timeInterval=ringTime-currTime
      endif

      !> Log current and next ring time
      call ESMF_TimeGet(currTime,timeStringISOFrac=timestring, rc=localrc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
      write(message,'(A)') trim(timeString)//' '//trim(name)//' stepping to'
      call ESMF_TimeGet(ringTime,timeStringISOFrac=timestring, rc=localrc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)
      write(message,'(A)') trim(message)//' '//trim(timeString)
      call ESMF_LogWrite(trim(message),ESMF_LOGMSG_TRACE, rc=localrc);

      !> Set new time interval and advance clock, stop if end of
      !! simulation reached
      call ESMF_ClockSet(clock, timeStep=timeInterval, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_ClockAdvance(clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      if (ESMF_ClockIsStopTime(clock, rc=localrc)) exit
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo

    call MOSSCO_CompExit(gridComp, localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

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
    character(ESMF_MAXSTR)  :: name, message, timeString
    logical                 :: clockIsPresent
    type(ESMF_Time)         :: currTime
    type(ESMF_Clock)        :: clock

    integer(ESMF_KIND_I4)  :: phase, phaseCount
    integer(ESMF_KIND_I4), dimension(:), allocatable :: phaseCountList
    logical, allocatable   :: hasPhaseZeroList(:)
    logical                :: hasPhaseZero
    integer(ESMF_KIND_I4), parameter :: maxPhaseCount=9

    rc=ESMF_SUCCESS

    call MOSSCO_CompEntry(gridComp, parentClock, name, currTime, localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

    call ESMF_GridCompGet(gridComp, clock=clock, rc=localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

    !! Establish number of phases and zero phase for all components
    !! @> todo this interface will likely change in the future and will
    !! be integrated with GridCompGet

    numGridComp=size(gridCompList)

    allocate(hasPhaseZeroList(numGridComp))
    allocate(phaseCountList(numGridComp))

    do i = 1, numGridComp
      call ESMF_GridCompGetEPPhaseCount(gridCompList(i), ESMF_METHOD_FINALIZE, &
        phaseCount=phaseCount, phaseZeroFlag=hasPhaseZero, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      phaseCountList(i)=phaseCount
      hasPhaseZeroList(i)=hasPhaseZero
    enddo

    do i=1,ubound(cplCompList,1)
      call ESMF_CplCompFinalize(cplCompList(i), clock=clock, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    enddo
    do i=1,ubound(gridCompList,1)
      do phase=1,phaseCountList(i)
        call ESMF_GridCompFinalize(gridCompList(i), clock=clock, phase=phase, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      enddo
    enddo
    do i=1,ubound(gridCompList,1)
      !!@todo destroy any remaining fields/arrays in states
      call ESMF_StateDestroy(exportStates(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
      call ESMF_StateDestroy(importStates(i), rc=localrc)
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
    if (allocated(exportStates)) deallocate(exportStates)
    if (allocated(importStates)) deallocate(importStates)
    if (allocated(cplAlarmList)) deallocate(cplAlarmList)

    call ESMF_ClockDestroy(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call MOSSCO_CompExit(gridComp, localrc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=localrc)

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

libs = {'gotm'       : ['solver', 'gotm'] ,
        'gotmfabm'   : ['mossco_gotmfabm','mossco_fabmpelagic', 'gotm', 'solver'],
        'fabm_gotm'       : ['gotm', 'mossco_fabmgotm', 'solver',
                  'gotm', 'gotm_prod', 'airsea_prod', 'meanflow_prod', 'seagrass_prod',
                  'output_prod', 'observations_prod', 'input_prod', 'turbulence_prod', 'util_prod'],
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
        'test'       : ['mossco_test'],
        'simplewave' : ['mossco_simplewave'],
        'empty'      : ['empty'],
        'inout'      : ['mossco_inout'],
        'info'       : ['mossco_info'],
        'fabm0d'     : ['mossco_fabm0d', 'solver', 'airsea_prod',
                        'input_prod', 'util_prod', 'fabm'],
        'pelagic_benthic_coupler' : ['pelagicbenthiccoupler'],
        'benthic_pelagic_coupler' : ['pelagicbenthiccoupler'],
        'xgrid_coupler' : ['xgridcoupler'],
        'link_coupler' : ['linkcoupler'],
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
        'test'       : ['libmossco_test'],
        'info'       : ['libmossco_info'],
        'empty'      : ['libempty'],
        'inout'      : ['libmossco_inout'],
        'constant'   : ['libconstant'],
        'constant_grid'  : ['libconstant_grid'],
        'gotm'       : ['libgotm', 'libsolver'],
        'fabm_gotm'       : ['libmossco_fabmgotm', 'libsolver', 'libgotm'],
        'gotmfabm'       : ['libmossco_gotmfabm', 'libsolver'],
        'pelagic_benthic_coupler' : ['libpelagicbenthiccoupler'],
        'benthic_pelagic_coupler' : ['libpelagicbenthiccoupler'],
        'xgrid_coupler' : ['libxgridcoupler'],
        'link_coupler' : ['liblinkcoupler'],
        'copy_coupler' : ['libcopycoupler'],
        'regrid_coupler' : ['libregridcoupler'],
        'remtc_atmosphere' : ['libremtc'],
        'remtc_ocean' : ['libremtc'],
        'getm' : ['libmossco_getm'],
}

#fid.write('\nNC_LIBS += $(shell nf-config --flibs)\n\n')
fid.write('LDFLAGS += $(MOSSCO_LDFLAGS) $(LIBRARY_PATHS)\n')
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
            fid.write(' $(FABM_LDFLAGS) -L$(GOTM_LIBRARY_PATH)')
        if item=='gotmfabm':
            fid.write(' $(GOTM_LDFLAGS) $(FABM_LDFLAGS)')
        if item=='fabm0d':
            fid.write(' $(FABM_LDFLAGS) -L$(GOTM_LIBRARY_PATH)')
        fid.write('\n')

#fid.write('LDFLAGS += $(LIBS) -lmossco_util -lesmf $(ESMF_NETCDF_LIBS)  -llapack\n\n')
fid.write('LDFLAGS += $(LIBS) -lmossco_util -lesmf $(ESMF_NETCDF_LIBS) \n\n')

#for item in gridCompSet.union(cplCompSet):
#    if libs.has_key(item):
#        if item=='gotm':
#            fid.write(' $(NC_LIBS)\n\n')
#        if item=='fabm_gotm':
#            fid.write(' $(NC_LIBS)\n\n')


fid.write('.PHONY: all exec ' + coupling_name + '\n\n')
fid.write('all: exec\n\n')
fid.write('exec: libmossco_util ')
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

libmossco_gotmfabm libgotm libmossco_fabmgotm:
	$(MAKE) -C $(MOSSCO_DIR)/src/components/gotm $@

libmossco_util libsolver:
	$(MAKE) -C $(MOSSCO_DIR)/src/utilities $@

libsediment libconstant libconstant_grid libmossco_clm libmossco_erosed \
libmossco_fabm0d libmossco_fabmpelagic:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libempty libmossco_inout libmossco_getm libmossco_simplewave libmossco_netcdf libmossco_benthos:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_info libmossco_test:
	$(MAKE) -C $(MOSSCO_DIR)/src/components $@

libmossco_sediment:
	$(MAKE) -C $(MOSSCO_DIR)/src/drivers $@

libsurfacescoupler libaocoupler liblinkcoupler libxgridcoupler libregridcoupler libcopycoupler:
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



