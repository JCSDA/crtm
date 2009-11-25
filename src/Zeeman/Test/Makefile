#==============================================================================
#
# Makefile for CRTM Forward model example program
#
#==============================================================================

# Macro definitions
# -----------------
# The location of the CRTM library
# *** CHANGE THIS TO THE LOCATION ON YOUR SYSTEM ***
CRTM_DIR = $(CRTM_SOURCE_ROOT)/Build

# The Fortran95 compiler+flags, and linker+flags
# *** CHANGE THESE FOR THE COMPILER YOU USE ON YOUR SYSTEM ***
# *** SEE THE END OF FILE FOR THE ADDITIONAL DEFINITIONS        ***
#FC = $(FC_GFORTRAN)
#FC_FLAGS = $(FC_GFORTRAN_FLAGS) -I$(CRTM_DIR)/include
FC = $(FC_XLF)
FC_FLAGS = $(FC_XLF_FLAGS) -I$(CRTM_DIR)/include
FL = $(FC)
FL_FLAGS = -L$(CRTM_DIR)/lib -lCRTM -o

# The executable and object files
EXE_FILE = test_Zeeman_Utility
OBJ_FILE = $(EXE_FILE).o


# Target definitions
# ------------------
# The main target
all: $(OBJ_FILE)
	$(FL) $(OBJ_FILE) $(FL_FLAGS) $(EXE_FILE)

# A clean target
clean:
	rm -f $(OBJ_FILE) $(EXE_FILE) *.output *.output.diff

# Local dependencies
$(EXE_FILE).o: $(EXE_FILE).f90


# Suffix rule definitions
# -----------------------
.SUFFIXES:
.SUFFIXES: .f90 .o
.f90.o:
	$(FC) $(FC_FLAGS) $<


# Some extra compilers to play with
# ---------------------------------
# gfortran
FC_GFORTRAN = gfortran
FC_GFORTRAN_FLAGS = -c \
                    -fbounds-check \
                    -fconvert=big-endian \
                    -ffpe-trap=overflow,zero \
                    -ffree-form \
	            -fno-second-underscore \
                    -frecord-marker=4 \
	            -static \
                    -Wall
           
# g95
FC_G95 = g95
FC_G95_FLAGS = -c \
               -fbounds-check \
               -fendian=big \
               -ffree-form \
               -fno-second-underscore \
               -ftrace=frame \
               -malign-double \
               -Wall
 
# Intel
FC_INTEL = ifort
FC_INTEL_FLAGS = -c \
                 -g \
                 -check bounds \
                 -convert big_endian \
                 -e95 \
                 -traceback \
                 -warn errors \
                 -free \
                 -assume byterecl \
                 -fpstkchk \
                 -mp

# PGI
FC_PGI = pgf95
FC_PGI_FLAGS = -c \
               -g \
               -byteswapio \
               -Ktrap=ovf,divz \
               -Mbounds \
               -Mchkstk \
               -Mdclchk \
               -Minform,inform \
               -Mnosave \
               -Mstandard \
               -Mref_externals

# Lahey f95
FC_LAHEY = lf95
FC_LAHEY_FLAGS = -c \
                 -g \
                 --chk aesu \
                 --f95 \
                 --trace \
                 --trap \
                 --ninfo --warn

# AIX XLF
FC_XLF = xlf95
FC_XLF_FLAGS = -c \
               -qcheck \
               -qdbg \
               -qextchk \
               -qfloat=nomaf:rndsngl:nans \
               -qflttrap=ov:zero:inv:en \
               -qinitauto \
               -qfree=f90 \
               -qhalt=W \
               -qlanglvl=95pure \
               -qmaxmem=-1 \
               -qsuffix=f=f90:cpp=F90

