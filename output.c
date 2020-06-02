#include <stdio.h>

int add(int x, int y) {
    int answer;
    answer = x + y;
    return answer;
}
void main() {
    char v1;
    v1 = 'a';
    short v2;
    int v3;
    v3 = add(2, 20);
    float v4;
    v4 = 1.23000E+00;
    double v5;
    printf("%c\n", v1);
    printf("%d\n", v3);
    printf("%f\n", v4);
}
