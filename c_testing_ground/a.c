#include <stdio.h>
int main() {
    int a = 2;
    int *b = &a;
    int c = a + *b;
    printf("%d\n", c);
}
