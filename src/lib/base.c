#include "include/gsvalue.h"
#include "include/gc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printLn(const char *s, void *fv)
{
    (void)fv;
    if (s == NULL) {
        puts("");
    } else {
        puts(s);
    }
    fflush(stdout);
}

void putChar(const char c, void *fv)
{
    (void)fv;
    putchar((unsigned char)c);
    fflush(stdout);
}

void putNumber(const int32_t n, void *fv)
{
    (void)fv;
    printf("%d", n);
    fflush(stdout);
}

void putFloat(const double f, void *fv)
{
    (void)fv;
    printf("%g", f);
    fflush(stdout);
}

int32_t gs_str_cmp(const char *a, const char *b)
{
    if (a == b) return 0;
    if (a == NULL) return -1;
    if (b == NULL) return 1;
    int r = strcmp(a, b);
    return (r < 0) ? -1 : (r > 0 ? 1 : 0);
}

void gs_match_fail(void)
{
    fprintf(stderr, "Runtime error: non-exhaustive pattern match failed\n");
    abort();
}

void gs_bounds_fail(int32_t idx, int32_t len)
{
    fprintf(stderr, "Runtime error: array index %d out of bounds (length %d)\n", idx, len);
    abort();
}
