#include "include/gsvalue.h"
#include "include/gc.h"

#include <stdio.h>

void printLn(const char *s, void *fv)
{
    puts(s);
}

void putChar(const char c, void *fv)
{
    putchar(c);
}


