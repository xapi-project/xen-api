#!/bin/bash

for f in `cat ../xapi-consts/api_errors.ml | grep let | awk '{print $2}' | sort ` ; 
do 
  if grep $f datamodel.ml >/dev/null 2>&1 ; 
      then echo "$f yes" ; 
      else echo "$f no" ; 
  fi ; 
done | grep " no" 