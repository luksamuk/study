#ifndef GLOBALS_H
#define GLOBALS_H

#include <sys/types.h>

#define OT_LEN          8192
#define PRIMBUFF_LEN  131072

void    empty_ot(u_short currbuff);
void    set_ot_at(u_short currbuff, u_int i, u_long value);
u_long *get_ot_at(u_short currbuff, u_int i);

void    increment_next_prim(u_int size);
void    set_next_prim(char *value);
void    reset_next_prim(u_short currbuff);
char   *get_next_prim(void);

#endif
