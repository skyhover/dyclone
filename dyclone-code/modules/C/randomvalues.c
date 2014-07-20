#include <stdio.h>
#include <stdlib.h>
#include <time.h>
// The "include"s may cause duplicate definitions with .i files

#include "dycmain.h"

/* for numbers */
int __dyc_byteinputs[MAXBYTENUM];
int __dyc_bytenum = 0;
int __dyc_nextbyte = 0;
/* for pointers */
int __dyc_ptrinputs[MAXPTRNUM];
int __dyc_ptrnum = 0;
int __dyc_nextptr = 0;

/* NOTE: better use srandom only once in one execution;
 *      otherwise the random numbers would be the same.
#define RAND_SEEDING(r)     \
    srandom(time(NULL));    \
    r = random();
*/

/* should not use the seeding more than once closely in time */
void __dyc_initrandomseed() {
    srandom(time(NULL));
}

/* NOTE: a second design for random value generation:
 * only treat two types of data: bytes and pointers.
 * random values are within the range [-128, 127].
 * Hope it not cause false negatives in practice.
 * ..unfortunately not always, if some type is unsigned..
 * Update: changed to use unsigned char. Better to generate values that satisfy input constraints... */

unsigned char __dyc_randompre_byte(unsigned int exp) {
    unsigned char r = random();
    return r;
}

int __dyc_randompre_ptr(unsigned int exp) {
    /* NOTE: 50%!=NULL for each pointer is too high if a recursive data structure contains several pointer members.
     * so, we use the exponential decade kind of probability. exp=0->50%, exp=8->0%, exp=1->25%, ... */
    int r = random();
    if ( exp>=8 ) return 0; // pointers more than 8 levels deep are all null.
    do {
        if ( (r&0x01) == 0 )
            return 0;
        else {
            r >>= 1;
        }
    } while ( exp-- > 0 );
    return 1;
}

/*
char __dyc_random_char() {
    char r;
    RAND_SEEDING(r);
    return r;
}

int __dyc_random_int() {
    int r;
    RAND_SEEDING(r);
    return r;
}

long int __dyc_random_long(int n, ...) {
    long int r;
    RAND_SEEDING(r);
    return r;
}
*/


/* For user-defined types, we may need to generate such function definitions dynamically (during the execution of CodeChopper) */


/*******************************************
 * A set of funtions for getting input data
 * from a pre-defined input array
 *******************************************/
/* return a number in the range of [-128,127] */
int __dyc_readpre_byte() {
    int x;
    POPINPUT(__dyc_byteinputs, __dyc_bytenum, __dyc_nextbyte, x);
    return x;
}

/* return 0 or 1 for a pointer */
int __dyc_readpre_ptr() {
    int p;
    POPINPUT(__dyc_ptrinputs, __dyc_ptrnum, __dyc_nextptr, p);
    return p;
}

/* output to stdout. Don't use fprintf, which may cause trouble for CIL's function construction */
void __dyc_printpre_byte(const unsigned char i) {
    printf("%d\n", i);
}

void __dyc_printpre_ptr(const void const * p) {
    printf("p%d\n", p==NULL?0:1);
}


