#include <stdio.h>

int main() {
    int a;
    a = 1;
    int b;
    b = a;
    int c;
    c = a + b;
    int d;
    d = a * b + 2;
    int e;
    e = 20 + 3 * a + b;
    printf("%d\n", a);
    printf("%d\n", b);
    printf("%d\n", c);
    printf("%d\n", d);
    printf("%d\n", e);
    a = b;
    b = 100;
    c = e;
    d = 1 + 2 + a + 3 + 4 + b;
    e =  - 1 * a * b * c;
    printf("%d\n", a);
    printf("%d\n", b);
    printf("%d\n", c);
    printf("%d\n", d);
    printf("%d\n", e);
    int x;
    x = 2;
    return 10 * x;
}

