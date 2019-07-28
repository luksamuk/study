#include <stdio.h>

int
main(void)
{
    const float menu[] = {
	1.1f, 1.3f, 1.5f, 1.1f, 1.3f, 1.0f
    };

    int item_code, amount;
    scanf("%d %d", &item_code, &amount);
    item_code -= 100;
    printf("%0.2f\n", menu[item_code] * amount);
    return 0;
}
