FCC=gfortran
CFLAGS=-c -O0 -g -std=f2003 -w -Wno-unused-parameter
SOURCES=main.f90 
OBJECTS=$(SOURCES:.f90=.o)
EXECUTABLE=kmatrix_svd.out
UNAME_S=$(shell uname -s)

ifeq ($(UNAME_S),Linux)
	CFLAGS+=-L/usr/local/lib
	LIBS+=-llapack -lblas -lm
endif
ifeq ($(UNAME_S),Darwin)
	LIBS+=-framework accelerate
endif

all: $(SOURCES) $(EXECUTABLE)
 
$(EXECUTABLE): $(OBJECTS)
	$(FCC) $(OBJECTS) $(LIBS) -o $@

%.o : %.f90
	$(FCC) $(CFLAGS) $< -o $@

clean: 
	@rm *.o *.mod
