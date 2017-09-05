#ifndef GSVALUE_H
#define GSVALUE_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/**
 * Geisha value representation definitions
 */


/* Representations

  primitive values: int32 / char (8bit) / double (64bit)

  struct: A struct has a header first, followed by fields.

          low
          +--------+
          | header |
          +--------+
          |        |
          | fields |
          |        |
          +--------+
          high

  closure: A closure also has a head, and contains a pointer to a function,
           a collection of pointers to free variables.

          low
          +--------+
          | header |
          +--------+
          |   fn   |
          +--------+
          |   fv   |
          +--------+
          high


  structure of the head:
        +--------+------+
        |  size  | type |
        +--------+------+
     max|  rest      1  |0

        size of struct is words of fields
        size of closure is words of fv + fn
        type 1 is closure, type 0 is struct
*/

// hope that size_t is synonymous with uintptr_t, which size is a word
typedef uintptr_t header_t;
typedef header_t value_t;

#define WORD_SIZE (sizeof(void*))

#define Get_head(p) (*((header_t *)(p) - 1))
#define Header_size (sizeof(header_t))

// `type` field values 1 means this is a closure type
#define Is_struct(h) ((Get_head(h) & 1) == 0)
#define Is_closure(h) ((Get_head(h) & 1) != 0)

#define Size_mask (-1 << 1)   // clear size bits

#define Set_size(h, c) (Get_head(h) = (Get_head(h) & Size_mask) | (c << 1))
#define Get_size(h) (Get_head(h) >> 1)


inline void gsvalue_init(value_t *ptr, size_t size, bool type)
{
    Set_size(ptr, size);
    Get_head(ptr) &= -1 << 1;
    Get_head(ptr) |= type ? 1 : 0;
}

#endif