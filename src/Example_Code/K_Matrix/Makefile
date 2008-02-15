#==============================================================================
#
# Makefile for CRTM K-matrix model example program
#
#==============================================================================

# Macro definitions
# -----------------
# The location of the CRTM library
# *** CHANGE THIS TO THE LOCATION ON YOUR SYSTEM ***
CRTM_DIR = $(HOME)/local/CRTM

# The Fortran95 compiler+flags, and linker+flags
# *** CHANGE THESE FOR THE COMPILER YOU USE ON YOUR SYSTEM ***
FC = gfortran
FC_FLAGS = -c \
           -fbounds-check \
           -fconvert=big-endian \
           -ffpe-trap=overflow,zero \
           -ffree-form \
	   -fno-second-underscore \
           -frecord-marker=4 \
	   -static \
           -Wall \
           -I$(CRTM_DIR)/include
FL = $(FC)
FL_FLAGS = -L$(CRTM_DIR)/lib -lCRTM -o

# The executable and object files
EXE_FILE = Example_K_Matrix
OBJ_FILE = $(EXE_FILE).o


# Target definitions
# ------------------
# The main target
all: $(OBJ_FILE)
	$(FL) $(OBJ_FILE) $(FL_FLAGS) $(EXE_FILE)

# A clean target
clean:
	rm -f $(OBJ_FILE) $(EXE_FILE)

# Local dependencies
$(EXE_FILE).o: $(EXE_FILE).f90


# Suffix rule definitions
# -----------------------
.SUFFIXES:
.SUFFIXES: .f90 .o
.f90.o:
	$(FC) $(FC_FLAGS) $<
