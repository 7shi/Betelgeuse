TARGET  = $(SOURCES:.c=)
CC      = alpha-elf-gcc
CFLAGS  = -mcpu=21164a -fno-builtin
LDFLAGS = -nostdlib -e main -Ttext 0x200 -s
SOURCES = 1.c 2.c 3.c 4.c 5.c 6.c 7t.c 7d.c 7a.c

all: $(TARGET)

clean:
	rm -f $(TARGET) test.* *.bin *.asm *.out
