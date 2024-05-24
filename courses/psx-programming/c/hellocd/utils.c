#include "utils.h"
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