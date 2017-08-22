/**
 * Geisha value representation definitions
 */

#ifndef GSVALUE_H
#define GSVALUE_H

#include <stdint.h>

/* Representations

  Currently for 64-bit arch only.

  primitive values: int32 / char / double

  struct: A struct has a header first, subsequent pointers and
          primitive values fields. Structs should be allocated in
          GC heap and managed by GC.

  closure: A closure also has a head, and contains a pointer to a function,
           a collection of pointers to free variables.

  structure of the head:
            +-------+-------+--------------------------+
            | type  | mark  | count of pointer fields  |
            +-------+-------+--------------------------+
        low     1       1                14              high

        So we could hold 2^14 pointers, which means 16MB data
        in single structure lol
*/

typedef uint32_t header_t;

// `type` field values 1 means this is a closure type
#define Is_struct(h) (((h) & 1) == 0)
#define Is_closure(h) (((h) & 1) != 0)

#define Set_fields_count(h, c) (((h) & 3) | (c << 2))
#define Fields_count(h) (h >> 2)

// mark operations
#define Mark(h) ((h) | (1 << 1))
#define Unmark(h) ((h) & ~(1 << 1))

#endif