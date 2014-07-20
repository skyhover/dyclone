#!/usr/bin/python2.4

import sys, os
import clusterfiltering

def getfilesinfilteredcluster(c):
    fn = c.split()
    trunktofiles = {}  # to avoid duplications
    if len(fn)<1:
       return trunktofiles

    fn = fn[len(fn)-1]
    if not os.path.basename(fn).startswith("filter"):
       fn = os.path.join(os.path.dirname(fn), "filtered_" + os.path.basename(fn))
    f = open(fn)
    for n in f:
        n = n.splitlines()
        if len(n)<1:
           continue
        else:
           n = n[len(n)-1]
           loc = clusterfiltering.getlocations(n)
           trunktofiles[loc[3]] = loc[0]
    return trunktofiles

def main():
    fn = sys.argv[1].split()
    if len(fn)<1:
       print >> sys.stderr, "Error: invalid L* file:", sys.argv[1]
       sys.exit(1)

    results = getfilesinfilteredcluster(fn)
    for k in results:
       print results[k]


if __name__ == '__main__':
   main()

