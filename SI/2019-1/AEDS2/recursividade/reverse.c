#include <stdio.h>

void
reverse()
{
    char ch = getchar();
    if(ch != '\n') {
        reverse();
        putchar(ch);
    }
}

int
main(void)
{
    reverse();
    putchar(10);
    return 0;
}

