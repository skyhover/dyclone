#include <stdio.h>
#include <stdlib.h>
#include "dycmain.h"
#include "inputformat.tab.h"

extern void __dyc_foo(); /* the code trunk */

void __dyc_readinputs(FILE * inf) {
    /* read numbers/pointers from 'inf' to the arrays */
    yyin = inf;
    yyrestart(yyin);
    yyparse();
}

void __dyc_printinputs(FILE * outf) {
    /* output inputs (including history) in an easier format for value permutation */
    int i;
    for(i=0; i<__dyc_bytenum; i++) {
        fprintf(outf, "%d\n", __dyc_byteinputs[i]);
    }
    for(i=0; i<__dyc_ptrnum; i++) {
        /* p0 or p1 */
        fprintf(outf, "p%d\n", __dyc_ptrinputs[i]);
    }
}

int main(int argc, char *argv[] ) {
    FILE * infile = stdin;

    if(argc==1)
        ;
    else if ( argc==2 ) {
        infile = fopen(argv[1], "r");
        if ( infile==NULL ) {
            fprintf(stderr, "Can't open file '%s'\n", argv[1]);
            exit(1);
        }
    } else {
        fprintf(stderr, "Usage: %s <input>\n", argv[0]);
        exit(1);
    }

    __dyc_readinputs(infile);
    fclose(infile);
    infile = NULL;

    __dyc_foo();
    //__dyc_printinputs(stdout);

    return 0;
}

