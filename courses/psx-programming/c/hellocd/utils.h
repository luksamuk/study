#ifndef UTILS_H
#define UTILS_H

#include <sys/types.h>

char *file_read(char *filename, u_long *length);

char  get_byte(u_char *bytes, u_long *b);
short get_short_le(u_char *bytes, u_long *b);
short get_short_be(u_char *bytes, u_long *b);
long  get_long_le(u_char *bytes, u_long *b);
long  get_long_be(u_char *bytes, u_long *b);

#endif
