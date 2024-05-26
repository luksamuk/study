#include "utils.h"
#include "types.h"
#include <stdlib.h>
#include <stdio.h>
#include <libcd.h>

char *
file_read(char *filename, u_long *length)
{
    CdlFILE filepos;
    int numsectors;
    char *buffer;

    buffer = NULL;

    if(CdSearchFile(&filepos, filename) == NULL) {
        printf("File %s not found!", filename);
        return NULL;
    }

    numsectors = (filepos.size + 2047) / 2048;
    buffer = (char*) malloc3(2048 * numsectors);
    if(!buffer) {
        printf("Error allocating %d sectors.\n", numsectors);
        return NULL;
    }

    CdControl(CdlSetloc, (u_char*) &filepos.pos, 0);
    CdRead(numsectors, (u_long*) buffer, CdlModeSpeed);
    CdReadSync(0, 0);

    *length = filepos.size;
    return buffer;
}

char
get_byte(u_char *bytes, u_long *b)
{
    return (char) bytes[(*b)++];
}

short
get_short_le(u_char *bytes, u_long *b)
{
    unsigned short value = 0;
    value |= bytes[(*b)++];
    value |= bytes[(*b)++] << 8;
    return (short) value;
}

short
get_short_be(u_char *bytes, u_long *b)
{
    unsigned short value = 0;
    value |= bytes[(*b)++] << 8;
    value |= bytes[(*b)++];
    return (short) value;
}

long
get_long_le(u_char *bytes, u_long *b)
{
    unsigned long value = 0;
    value |= bytes[(*b)++];
    value |= bytes[(*b)++] << 8;
    value |= bytes[(*b)++] << 16;
    value |= bytes[(*b)++] << 24;
    return (long) value;
}

long
get_long_be(u_char *bytes, u_long *b)
{
    unsigned long value = 0;
    value |= bytes[(*b)++] << 24;
    value |= bytes[(*b)++] << 16;
    value |= bytes[(*b)++] << 8;
    value |= bytes[(*b)++];
    return (long) value;
}