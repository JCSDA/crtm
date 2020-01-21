CC = gcc
CFLAGS = -g -O1
COBJS = main.o

all: endian

endian: $(COBJS)
	$(CC) -o $@ $^ $(LDFLAGS)

clean:
	$(RM) $(COBJS) endian
