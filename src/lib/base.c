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

void putNumber(const uint32_t n, void *fv)
{
    printf("%d", n);
    fflush(stdout);
}

void putFloat(const double f, void *fv)
{
    printf("%lf", f);
}
