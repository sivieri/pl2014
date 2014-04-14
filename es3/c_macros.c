#include <stdio.h>

#define INCI(i) {int a = 0; ++i;}

int main(int argc, char **argv) {
    int a = 0, b = 0;
    
    INCI(a);
    INCI(b);
    printf("a is now %d, b is now %d\n", a, b);
    
    return 0;
}
