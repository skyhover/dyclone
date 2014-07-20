#!/bin/bash

scriptdir=$(cd `dirname $0`; pwd)

pushd . >& /dev/null

cd $scriptdir

#make clean
#rm *.o libdyc.a

make
#flex -oinputformat.lex.c inputformat.l
#bison -d inputformat.y
#gcc -c inputformat.tab.c -O3
#gcc -c randomvalues.c -O3
#gcc -c dycmain.c -O3
#ar -qcs libdyc.a inputformat.tab.o randomvalues.o

popd >& /dev/null

