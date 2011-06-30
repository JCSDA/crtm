#==============================================================================
#
# Makefile for Test_EmisCoeff program
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# Executable files
EXE_FILE = Test_EmisCoeff

# This makefile
MAKEFILE = Makefile

# Source files for linking
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Binary_File_Utility.f90 \
            Endian_Utility.f90 \
            netCDF_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            EmisCoeff_Define.f90 \
            EmisCoeff_Binary_IO.f90 \
            EmisCoeff_netCDF_IO.f90

# Object files for link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} $(EXE_FILE).o

# Include and library definitions
INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf


# ------------------
# Define dependecies
# ------------------
# Make based on OS type
all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")   make -f $(MAKEFILE) test_program $(SUNOS_FLAGS) ;; \
	  "AIX")     make -f $(MAKEFILE) test_program $(AIX_FLAGS) ;; \
	  "IRIX64" ) make -f $(MAKEFILE) test_program $(IRIX64_FLAGS) ;; \
	  "Linux" )  make -f $(MAKEFILE) test_program $(LINUX_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
       esac

# Build the test program
test_program: create_links $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(FL_FLAGS) $(EXE_FILE)

# Clean up
clean:
	$(REMOVE) $(OBJ_FILES) *.mod *.MOD *.stb

realclean: remove_links clean

# Create links for compilation
create_links:
	@linkfiles $(CRTM_SOURCE_ROOT) $(SRC_FILES)
                   
remove_links:
	@unlinkfiles $(SRC_FILES)


# ---------------------------
# Source code dependency list
# ---------------------------
include make.dependencies


# --------------------
# Define default rules
# --------------------
include $(CRTM_SOURCE_ROOT)/make.rules

