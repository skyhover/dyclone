#!/usr/bin/python2.4

import sys
import trunknamefiltering

def readlineset( file ):
    f = open(file)
    ns = set()
    for n in f:
        n = n.splitlines()
        if len(n)<1:
           continue
        n = n[len(n)-1].split()
        if len(n)<1:
           continue
        n = n[len(n)-1]
        ns.add(n)
    return ns

def readlinehash( file ):
    f = open(file)
    ns = {}  # function_name -> line numbers
    for n in f:
        n = n.splitlines()
        if len(n)<1:
           continue
        n = n[len(n)-1].split()
        if len(n)<1:
           continue
        n = n[len(n)-1]
        loc = trunknamefiltering.getlocations(n)
        if loc[0] not in ns:
           ns[loc[0]] = (1, set())
        else:
           ns[loc[0]][0] = ns[loc[0]][0]+1
        #for i in range(loc[1][0],loc[1][2]+1):
        ns[loc[0]][1].update(range(loc[1][0],loc[1][2]+1))
    return ns



def main():
    if len(sys.argv) != 3:
       print >> sys.stderr, "Usage:", sys.argv[0], "<name set 1> <name set 2>"
       sys.exit(1)
    nameset1 = readlineset(sys.argv[1])
    nameset2 = readlineset(sys.argv[2])
    namediff = list(nameset1 - nameset2)
    namediff.sort()
    print >> sys.stderr, "Difference:"
    for n in namediff:
        print n
    #print >> sys.stderr, "Intersection:"
    #nameinter = list(nameset1 & nameset2)
    #nameinter.sort()
    #for n in nameinter:
    #    print >> sys.stderr, n


def mainhash():
    if len(sys.argv) != 3:
       print >> sys.stderr, "Usage:", sys.argv[0], "<name set 1> <name set 2>"
       sys.exit(1)
    namehash1 = readlinehash(sys.argv[1])
    namehash2 = readlinehash(sys.argv[2])
    namediff = {}
    for k1 in namehash1: # TODO: not useful...
        keep = False
        if k1 not in namehash2:
           continue
    namediff = list(namehash1 - namehash2)
    namediff.sort()
    print >> sys.stderr, "Difference:"
    for n in namediff:
        print n
    #print >> sys.stderr, "Intersection:"
    #nameinter = list(nameset1 & nameset2)
    #nameinter.sort()
    #for n in nameinter:
    #    print >> sys.stderr, n


if __name__ == '__main__':
   main()

