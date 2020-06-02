#include <stdio.h>

int main() {
    int x = 20;
    
    if (x > 10) {
        puts("Greater");
        
        if (x > 20) {
            puts("X greater than 20");
            puts("");
        }
    } else if (x > 10) {
        puts("Less");
    } else {
        puts("IDK");
    }
    
    return 0;
}
