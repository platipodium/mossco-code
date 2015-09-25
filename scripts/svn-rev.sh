#! /bin/bash

REV=none

if [ $1 ] ; then  
  if [ -d $1 ] ; then
    REV=`(cd $1 ; svn log) | head -2| tail -1 | awk '{print $1}'`
  fi
fi
 
echo $REV
