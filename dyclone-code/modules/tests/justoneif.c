#include <stdio.h>

typedef int bool ;
int(*g)(void);

void f() {
    bool i,j=0;
    //printf("else branch: %p\n", g);
    if ( i == j ) {
        i = j*j;
    } else {
        printf("else branch: %p\n", g);
    }
}
