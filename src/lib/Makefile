# Makefile for Geisha runtime library

CC = cc
CFLAGS = -c -O2 -Wall
DEBUG_FLAGS = -D__DEBUG

# Source files
SRCS = base.c runtime/gc_bitmap.c
OBJS = base.o gc_bitmap.o

# Output directory (project root)
OUT_DIR = ../..

.PHONY: all clean debug

all: $(OUT_DIR)/runtime.o

# Compile base.c
base.o: base.c include/gsvalue.h include/gc.h
	$(CC) $(CFLAGS) -o $@ base.c

# Compile gc_bitmap.c
gc_bitmap.o: runtime/gc_bitmap.c include/gc_bitmap.h include/gc.h include/gsvalue.h
	$(CC) $(CFLAGS) -o $@ runtime/gc_bitmap.c

# Combine into single runtime object using partial linking
$(OUT_DIR)/runtime.o: base.o gc_bitmap.o
	ld -r -o $@ base.o gc_bitmap.o

# Debug build with tracing output
debug: CFLAGS += $(DEBUG_FLAGS)
debug: clean all

clean:
	rm -f $(OBJS) $(OUT_DIR)/runtime.o
