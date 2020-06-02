#include <stdio.h>

void cmp(int x) {
    if (x > 20) {
        puts("Greater");
    } else if (x < 20) {
        puts("Less");
    } else {
        puts("Equal");
    }
}

int main() {
    cmp(20);
    cmp(1);
    cmp(100);
    
    return 0;
}
