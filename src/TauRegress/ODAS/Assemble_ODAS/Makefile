include $(CRTM_SOURCE_ROOT)/make.macros
 
# -------------
# This makefile
# -------------

MAKE_FILE = Makefile


# ----------------------
# Executable load module
# ----------------------

EXE_FILE = Cat_ODAS

# -----------------
# Log file for make
# -----------------

LOG_FILE = log.make.$(EXE_FILE)

# ------------
# Source files
# ------------
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Sort_Utility.f90 \
            netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            ODAS_Define.f90 \
            ODAS_netCDF_IO.f90
EXE_SRC_FILE=Cat_ODAS.f90

# Obj files used in link phase
 
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            ${EXE_SRC_FILE:.f90=.o} 


# Include and library definitions
INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -lessl -L$(HOME)/local/netcdf/lib -lnetcdf 
#LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf


# ------------------
# Define dependecies
# ------------------
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Create and remove source links
create_links:
	@linkfiles $(CRTM_SOURCE_ROOT) $(SRC_FILES)
                   
remove_links:
	@unlinkfiles $(SRC_FILES)

# Source dependencies
include make.dependencies


# --------------------
# Define default rules
# --------------------
include $(CRTM_SOURCE_ROOT)/make.rules
 
