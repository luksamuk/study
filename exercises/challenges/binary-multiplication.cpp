#include <cstdio>
unsigned int mult(unsigned int a, unsigned int b)
{
    unsigned int res = 0x00u;
    while(b != 0u) {
        res += ((b & 0x01u) != 0x00u) ? a : 0x00u;
        a <<= 0x01u;
        b >>= 0x01u;
    }
    return res;
}

int main(void)
{
    unsigned int input1, input2;
    while(true)
    {
        scanf("%u %u", &input1, &input2);
        printf("%u * %u = %u\n", input1, input2, mult(input1, input2));
    }
    return 0;
}
