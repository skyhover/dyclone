#!/bin/bash

if [ $# -ne 2 ]; then
   echo "Usage: $0 <a list of file names> <source dir>" 1>&2
   exit 65
fi

scriptdir=`dirname $0`

while read fn; do
   fn=${fn%%.*}.c
   a=`find $2 -name "$fn" -print -quit`  # too slow
   a=${a#$2/}
   a=${a%%/*}
   if [ "$a" != "" ]; then
      echo $a   # this is the dir name we want
   else
      echo "ERROR: $fn"
   fi
done < $1

