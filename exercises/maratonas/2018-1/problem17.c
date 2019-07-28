#include <stdio.h>
#include <string.h>

int
main(void)
{
    char word[80]; // should be enough
    scanf("%s", word); // get string
    size_t string_size = strlen(word);
    size_t i;
    // Careful with null terminators...
    for(i = string_size; i > 0; i--)
	printf("%c", word[i - 1]);
    printf("\n");
    return 0;
}
