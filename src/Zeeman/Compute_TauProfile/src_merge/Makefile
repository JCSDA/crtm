#==============================================================================
#
# Makefile for Merge TauProd program
#
#==============================================================================
ROOT=$(CRTM_SOURCE_ROOT)
   
# -------------
# Define macros
# -------------
include $(ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = merge 
 
# Source files 
SRC_FILES =  Type_Kinds.f90 \
             merge.f90
  
# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o}
               

# Include and library definitions
#INCLUDES = -I$(HOME)/local/netcdf/include
#LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf

INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -lessl -L$(HOME)/local/netcdf/lib -lnetcdf 


# ------------------
# Define dependecies
# ------------------
include $(ROOT)/make.common_targets

# Create and remove source links
create_links:
	@linkfiles $(ROOT) $(SRC_FILES)
                      
remove_links:
	@unlinkfiles $(SRC_FILES)
  
# Squeaky clean target
realclean:: clean remove_links

# Source dependencies
include make.dependencies

# --------------------
# Define default rules
# --------------------
include $(ROOT)/make.rules
