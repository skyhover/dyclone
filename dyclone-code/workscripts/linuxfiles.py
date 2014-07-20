#!/usr/bin/python2.4

import re
import sys

def filesdist( cfile ):
    f = open(cfile)  # a list of C files
    files = {}
    for c in f:
        c = c.splitlines()
        if len(c)<1:
           continue
        c = c[len(c)-1].split('/')
        if len(c)<3: # assume the first one is linux-x.x.x
           continue 
        # the second is the module, the last one is the C file name 
        if c[len(c)-1] not in files:  # there are hundreds of files with a same name. e.g., alloc.c
           # To be consistent, use the first found by 'find'
           files[c[len(c)-1]] = c[1]
    f.close()
    return files

def main():
    if len(sys.argv) != 2:
       print >> sys.stderr, "Usage: ", sys.argv[0], " <a full list of original C files>"
       sys.exit(1)
    files = filesdist(sys.argv[1])
    for k in files:
        print k, files[k]


if __name__ == '__main__':
   main()

