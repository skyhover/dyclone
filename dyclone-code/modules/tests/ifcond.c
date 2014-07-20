#include <stdio.h>

typedef int bool ;

int f() {
    return 1;
}

int(*g)(void);

int main(void) {
    bool i,j=0;
    i = i+j;
    g=f;
    if ( i == j ) {
        i = j*j;
        printf("then branch: %p\n", f);
        j = i * i;
    } else {
        printf("else branch: %p\n", g);
    }
    return 0;
}
