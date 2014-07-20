#ifndef __DYCMAIN_H__
#define __DYCMAIN_H__

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

extern FILE * yyin;
extern void yyrestart(FILE * new_file);
extern int yyparse();

/* use the array to store all number/pointer inputs */
#define MAXBYTENUM 100
#define MAXPTRNUM 50
/* Q: better to use queues? not really, 
 *    since I'd like to keep the history as well. */
/* for numbers */
extern int __dyc_byteinputs[MAXBYTENUM];
extern int __dyc_bytenum;
extern int __dyc_nextbyte;
/* for pointers */
extern int __dyc_ptrinputs[MAXPTRNUM];
extern int __dyc_ptrnum;
extern int __dyc_nextptr;

extern void __dyc_initrandomseed();
extern unsigned char __dyc_randompre_byte(unsigned int exp);
extern int __dyc_randompre_ptr(unsigned int exp);
extern int __dyc_readpre_byte();
extern int __dyc_readpre_ptr();
extern void __dyc_printpre_byte(const unsigned char i);
extern void __dyc_printpre_ptr(const void const * p);

#define PUSHINPUT(q, c, n, i)  \
    if ( n<c ) {    \
        q[n] = i;   \
        n = n+1;    \
    } else {    \
        /* overflow silently assert(n<c); */   \
    }
#define POPINPUT(q, n, i, x)    \
    if ( n>i ) {    \
        x = q[i];   \
        i = i+1;    \
    } else {    \
        x = 0; /* 0 for extra inputs */    \
    }

#endif /* __DYCMAIN_H__ */

