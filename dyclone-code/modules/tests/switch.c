#include <stdio.h>

int main(void) {
    int i,j=0;
    i = i+j;
    switch ( i ) {
        case 3: case 5:
            i = j*j;
            printf("a branch\n");
            j = i * i;
        case 2:
            i = atoi("c");
            break;
        case 11:
            printf("nothing\n");
        default:
            printf("default\n");
    }
    return 0;
}

