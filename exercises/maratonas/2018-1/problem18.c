#include <stdio.h>
#include <string.h>
#include <ctype.h>

char
transform(char c)
{
    if(c < 'a' || c > 'z') return c;
    c -= 'a';
    c = (c + 4) % ('z' - 'a');
    return c + 'a';
}

int
main(void)
{
    char text[255];
    fgets(text, 255, stdin); // receive an entire line
    // Now loop around and transform each character
    size_t string_size = strlen(text), i;
    for(i = 0; i < string_size; i++)
	text[i] = transform(tolower(text[i]));
    printf("%s\n", text);
    return 0;
}
