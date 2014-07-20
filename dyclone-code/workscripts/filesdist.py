#!/usr/bin/python2.4

import re
import sys
import linuxfiles

def main():
    if len(sys.argv) != 3:
       print >> sys.stderr, "Usage: ", sys.argv[0], " <a full list of original C files> <a list of file names>"
       sys.exit(1)
    files = linuxfiles.filesdist(sys.argv[1])
    f = open(sys.argv[2])
    for fn in f:
        fn = fn.splitlines()
        if len(fn)<1:
           continue
        fn = fn[len(fn)-1]
        cn = fn.split('.')
        cn = cn[0] + ".c"
        print fn, files[cn]
    f.close()


if __name__ == '__main__':
   main()

