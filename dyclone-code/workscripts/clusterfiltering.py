#!/usr/bin/python2.4

import sys, os

import re
linesreg = re.compile("(.*)_(\d+)-(\d+)_(\d+)-(\d+)\.foo\.c.*")

def getlocations( line ):
    rotatedirs = line.split('/')
    dirs = []
    for d in rotatedirs:
        if not d.startswith("DIRRTT"):
           dirs.append(d)
    if len(dirs)>=3:
       filename = dirs[len(dirs)-3]
    else:
       filename = ''
    if len(dirs)>=2:
       funname = dirs[len(dirs)-2]
    else:
       funname = ''
    if len(dirs)>=1:
       m = linesreg.match(dirs[len(dirs)-1])
       if m != None:
          l1 = int(m.group(2))
          b1 = int(m.group(3))
          l2 = int(m.group(4))
          b2 = int(m.group(5))
          return (filename, funname, (min(l1,l2), min(b1,b2), max(l1,l2), max(b1,b2)), dirs[len(dirs)-1])
       else:
          return (filename, funname, (-1, -1, -1, -1), dirs[len(dirs)-1])


def is_loc_overlapping( loc1, loc2 ):
    #print loc1, loc2
    if loc1[1]!=loc2[1]: # different functions
       return False
    elif loc1[0]!=loc2[0]: # the same function but different files
       return True
    elif (loc1[2][1]<=loc2[2][1] and loc1[2][3]>=loc2[2][1]) \
             or (loc1[2][1]>=loc2[2][1] and loc1[2][1]<=loc2[2][3]): # line numbers overlap
       return True
    return False

def filter_one_cluster( cluster ):
    results = {}
    for c in cluster:
        if c in results:
           continue
        else:
           loc1 = getlocations(c)
           filtered = False
           for r in results:
               if is_loc_overlapping(loc1, results[r]):
                  filtered = True
                  break
           if filtered:
              continue
           else:
              results[c] = loc1
    if len(results)>1:
       return results.keys()
    else:
       return []

def is_loc_duplicated_function( loc1, loc2 ):
    if loc1[1]!=loc2[1]: # different functions
       return False
    elif loc1[0]!=loc2[0]: # the same function but different files
       return True
    return False

def filter_duplicated_functions( cluster ):
    results = {}
    for c in cluster:
        if c in results:
           continue
        else:
           loc1 = getlocations(c)
           filtered = False
           for r in results:
               if is_loc_duplicated_function(loc1, results[r]):
                  filtered = True
                  break
           if filtered:
              continue
           else:
              results[c] = loc1
    if len(results)>1:
       return results.keys()
    else:
       return []


def main():
    f = open(sys.argv[1])  # a L* file name
    cluster = f.readlines()
    cluster = filter_one_cluster(cluster)
    cluster.sort()
    f.close()
    for c in cluster:
        print c,

if __name__ == '__main__':
   main()

