#include "joypad.h"

static u_long padstate;

void
joypad_init(void)
{
    PadInit(0);
}

void
joypad_reset(void)
{
    padstate = 0;
}

void
joypad_update(void)
{
    u_long pad;
    pad = PadRead(0);
    padstate = pad;
}

int
joypad_check(int p)
{
    return padstate & p;
}