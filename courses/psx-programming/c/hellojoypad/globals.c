#include "globals.h"

#include <libgpu.h>

static u_long ot[2][OT_LEN];
static char primbuff[2][PRIMBUFF_LEN];
static char *nextprim;


void
empty_ot(u_short currbuff)
{
    ClearOTagR(ot[currbuff], OT_LEN);
}

void
set_ot_at(u_short currbuff, u_int i, u_long value)
{
    ot[currbuff][i] = value;
}

u_long *
get_ot_at(u_short currbuff, u_int i)
{
    return &ot[currbuff][i];
}

void
increment_next_prim(u_int size)
{
    nextprim += size;
}

void
set_next_prim(char *value)
{
    nextprim = value;
}

void
reset_next_prim(u_short currbuff)
{
    nextprim = primbuff[currbuff];
}

char *
get_next_prim(void)
{
    return nextprim;
}
