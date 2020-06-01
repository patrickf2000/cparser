#include <stdio.h>

int main() {
    int a = 1;
    int b = a;
    int c = a + b;
    int d = a * b + 2;
    int e = 20 + 3 * a + b;
    
    printf("%d\n", a);
    printf("%d\n", b);
    printf("%d\n", c);
    printf("%d\n", d);
    printf("%d\n", e);
    
    a = b;
    b = 100;
    c = e;
    d = 1 + 2 + a + 3 + 4 + b;
    e = -1 * a * b * c;
    
    printf("%d\n", a);
    printf("%d\n", b);
    printf("%d\n", c);
    printf("%d\n", d);
    printf("%d\n", e);

    int x = 2;
    return 10 * x;
}
