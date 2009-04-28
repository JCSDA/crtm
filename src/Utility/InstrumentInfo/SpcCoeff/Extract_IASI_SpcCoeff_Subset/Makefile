#==============================================================================
#
# Makefile for Extract_IASI_SpcCoeff_Subset program
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKEFILE = Makefile

# Executable files
EXE_FILE = Extract_IASI_SpcCoeff_Subset

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Fundamental_Constants.f90 \
            List_File_Utility.f90 \
            Sort_Utility.f90 \
            String_Utility.f90 \
            Channel_Subset_Define.f90 \
            IASI_Define.f90 \
            IASI_Subset.f90 \
            SensorInfo_Parameters.f90 \
            AntCorr_Define.f90 \
            AntCorr_netCDF_IO.f90 \
            SpcCoeff_Define.f90 \
            SpcCoeff_netCDF_IO.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

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
	  "SunOS")  make -f $(MAKEFILE) build $(SUNOS_FLAGS) ;; \
	  "AIX")    make -f $(MAKEFILE) build $(AIX_FLAGS) ;; \
	  "IRIX64") make -f $(MAKEFILE) build $(IRIX64_FLAGS) ;; \
	  "Linux")  make -f $(MAKEFILE) build $(LINUX_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
	esac

# Build the program
build: create_links $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(FL_FLAGS) $(EXE_FILE)

# Clean up
clean:
	$(REMOVE) $(OBJ_FILES) *.mod *.MOD *.stb

realclean: remove_links clean

# Install it
install:
	@if [ -d $(HOME)/bin ]; then \
	    $(MOVE) $(EXE_FILE) $(HOME)/bin; \
	fi


# Source file link creation and removal
create_links:
	@linkfiles $(CRTM_SOURCE_ROOT) $(SRC_FILES)

remove_links:
	@unlinkfiles $(SRC_FILES)

# Source dependency lists
include make.dependencies


# --------------------
# Define default rules
# --------------------
include $(CRTM_SOURCE_ROOT)/make.rules
