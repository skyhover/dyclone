#!/usr/bin/python2.4

import sys, os

trunkcount = 0
def codetrunkname( line ):
    global trunkcount
    dirs = line.split('/')
    if len(dirs)>=1:
       trunkcount += 1
       return dirs[len(dirs)-1]
    else:
       return None

def gettrunknames ( cluster ):
    results = set()
    for c in cluster:
	c = c.splitlines()
        if len(c)<1:
           continue
        c = c[len(c)-1].split()
        if len(c)<1:
           continue
        c = c[len(c)-1]
        tn = codetrunkname(c)
        if tn != None:
           results.add(tn)
    return results

def main1():
    f = open(sys.argv[1])  # a L* file name
    names = f.readlines()
    names = list(gettrunknames(names))
    names.sort()
    f.close()
    for n in names:
        print n

def main():
    f = open(sys.argv[1])  # a list of L* file names
    nameset = set()
    for c in f:
        c = c.splitlines()
        if len(c)<1:
           continue
        c = c[len(c)-1].split()
        if len(c)<1:
           continue
        c = c[len(c)-1]
        if not os.path.isfile(c):
           print >> sys.stderr, "invalid L* file:", c
           continue
        ff = open(c)  # a L* file name
        cluster = ff.readlines()
        nameset = nameset | gettrunknames(cluster)
        ff.close()
    names = list(nameset)
    names.sort()
    for n in names:
        print n
    print >> sys.stderr, "Total trunknames w/ duplications:", trunkcount
    print >> sys.stderr, "                 w/o duplications:", len(names)


if __name__ == '__main__':
   main()

