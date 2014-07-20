#!/usr/bin/python

import sys

def readinfilelist (file):
    f = open(file, 'r')
    filelist=[]
    lc = 0
    #for line in f.readlines():
    for line in f:
        lc += 1
        if line=='\n':
            continue
        data=line.split('|')
        if len(data)==5:
            filelist.append((data[0], int(data[2]), int(data[3]), int(data[4])))
        elif len(data)==9:
            filelist.append((data[0], int(data[2]), int(data[3]), int(data[4]), int(data[6]), int(data[7]), int(data[8])))
        else:
            print >> sys.stderr, "Warning:", sys.argv[0], "Unrecognized line at", lc, "in", file
            continue
    f.close()
    return filelist


# remove code without out-vars
def filterfilelist(f1):
    if len(f1)==7:
        if f1[4]>0:
            return True
    return False

def cmpfilelist(f1, f2):
    ins1=(f1[1], f1[2], f1[3])
    ins2=(f2[1], f2[2], f2[3])
    rsl = cmp(ins2, ins1)  # reversed order
    if rsl==0:
        if len(f1)==7:
            rds1=(f1[4], f1[5], f1[6])
        else:
            rds1=(0,0,0)
        if len(f2)==7:
            rds2=(f2[4], f2[5], f2[6])
        else:
            rds2=(0,0,0)
        return cmp(rds1, rds2)  # normal order
    else:
        return rsl


def dumpfilelist(filelist, oc=sys.stdout):
    lc = 0
    for f in filelist:
        lc += 1
        for x in f:
            print >> oc, x,
        print >> oc
    if lc<2:
        print >> sys.stderr, "Warning:", sys.argv[0], "not enough code for clustering?"

def main():
    numbers = readinfilelist(sys.argv[1])
    dumpfilelist(numbers)


if __name__ == '__main__':
   main()

