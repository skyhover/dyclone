#!/usr/bin/python2.4

import sys, os

import re
linesreg = re.compile("(.*)_(\d+)-(\d+)_(\d+)-(\d+)\.foo\.c.*")

def getlocations( fn ):
    fn = fn.splitlines()
    if len(fn)<1:
       return (None, None, None)
    fn = fn[len(fn)-1].split()
    if len(fn)<1:
       return (None, (-1,-1,-1,-1), None)
    fn = fn[len(fn)-1]
    m = linesreg.match(fn)
    if m != None:
       l1 = int(m.group(2))
       b1 = int(m.group(3))
       l2 = int(m.group(4))
       b2 = int(m.group(5))
       return (m.group(1), (min(l1,l2), min(b1,b2), max(l1,l2), max(b1,b2)), fn)
    else:
       return (None, (-1, -1, -1, -1), fn)


def is_loc_overlapping( loc1, loc2 ):
    #print loc1, loc2
    if loc1[2]==loc2[2]: # the same loc
       return True
    if loc1[0]!=loc2[0]: # different functions
       return False
    elif (loc1[1][1]<=loc2[1][1] and loc1[1][3]>=loc2[1][1]) \
             or (loc1[1][1]>=loc2[1][1] and loc1[1][1]<=loc2[1][3]): # line numbers overlap
       return True
    return False

def filter_trunknames( cluster ):
    results = {}
    for c in cluster:
        loc1 = getlocations(c)
        filtered = False
        for r in results:
            if is_loc_overlapping(loc1, results[r]):
               filtered = True
               break
        if filtered:
           continue
        else:
           results[loc1[2]] = loc1
    return results.keys()

def main():
    f = open(sys.argv[1])  # a list of trunk names
    cluster = f.readlines()
    cluster = filter_trunknames(cluster)
    cluster.sort()
    f.close()
    for c in cluster:
        print c

if __name__ == '__main__':
   main()

