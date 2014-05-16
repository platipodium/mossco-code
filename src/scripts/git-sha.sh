#! /bin/bash

SHA=none

if [ $1 ] ; then  
  if [ -d $1 ] ; then
    SHA=`(cd $1 ; git log) | head -1 | awk '{print $2}'`
  fi
fi
 
echo $SHA
