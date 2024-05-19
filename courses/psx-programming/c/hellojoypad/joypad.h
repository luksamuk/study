#ifndef JOYPAD_H
#define JOYPAD_H

#include <libetc.h>

#define PAD1_SELECT    _PAD(0, PADselect)
#define PAD1_START     _PAD(0, PADstart)
#define PAD1_UP        _PAD(0, PADLup)
#define PAD1_RIGHT     _PAD(0, PADLright)
#define PAD1_DOWN      _PAD(0, PADLdown)
#define PAD1_LEFT      _PAD(0, PADLleft)
#define PAD1_TRIANGLE  _PAD(0, PADRup)
#define PAD1_CIRCLE    _PAD(0, PADRright)
#define PAD1_CROSS     _PAD(0, PADRdown)
#define PAD1_SQUARE    _PAD(0, PADRleft)
#define PAD1_L2        _PAD(0, PADL2)
#define PAD1_R2        _PAD(0, PADR2)
#define PAD1_L1        _PAD(0, PADL1)
#define PAD1_R1        _PAD(0, PADR1)

#define PAD2_SELECT    _PAD(1, PADselect)
#define PAD2_START     _PAD(1, PADstart)
#define PAD2_UP        _PAD(1, PADLup)
#define PAD2_RIGHT     _PAD(1, PADLright)
#define PAD2_DOWN      _PAD(1, PADLdown)
#define PAD2_LEFT      _PAD(1, PADLleft)
#define PAD2_TRIANGLE  _PAD(1, PADRup)
#define PAD2_CIRCLE    _PAD(1, PADRright)
#define PAD2_CROSS     _PAD(1, PADRdown)
#define PAD2_SQUARE    _PAD(1, PADRleft)
#define PAD2_L2        _PAD(1, PADL2)
#define PAD2_R2        _PAD(1, PADR2)
#define PAD2_L1        _PAD(1, PADL1)
#define PAD2_R1        _PAD(1, PADR1)

void joypad_init(void);
void joypad_reset(void);
void joypad_update(void);

int  joypad_check(int);

#endif