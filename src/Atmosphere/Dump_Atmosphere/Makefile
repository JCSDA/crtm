#==============================================================================
#
# Makefile for Dump_Atmosphere program
#
# $Id: Makefile,v 1.1 2006/05/08 13:26:08 wd20pd Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

REPOSITORY_DIR = $(HOME)/src

include $(REPOSITORY_DIR)/make.macros


# -------------
# This makefile
# -------------

MAKE_FILE = Makefile


# ---------------
# Executable file
# ---------------

EXE_FILE = Dump_Atmosphere


# ------------
# Object files
# ------------

OBJ_FILES = $(EXE_FILE).o


# --------------------------
# Library and include macros
# --------------------------

# Include and library location
# *** MODIFY THESE TO THE LOCATION OF THE PROFILE_UTILITY LIBRARY ON YOUR SYSTEM ***
#INCLUDE_DIR = $(HOME)/local/include
#LIBRARY_DIR = $(HOME)/local/lib
INCLUDE_DIR = $(REPOSITORY_DIR)/CRTM/Build/include
LIBRARY_DIR = $(REPOSITORY_DIR)/CRTM/Build/lib

# Library name
LIBRARY_NAME = CRTM

# Definitions used in compilation and linking
INCLUDES = -I$(INCLUDE_DIR)
LIBRARIES = -L$(LIBRARY_DIR) -l$(LIBRARY_NAME)



#-----------------------------------------------------------------------------
#                          -- Define dependecies --
#-----------------------------------------------------------------------------

# ---------------------
# Make based on OS type
# ---------------------

all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")   make -f $(MAKE_FILE) test_program $(SUNOS_FLAGS) ;; \
	  "AIX")     make -f $(MAKE_FILE) test_program $(AIX_FLAGS) ;; \
	  "IRIX64" ) make -f $(MAKE_FILE) test_program $(IRIX64_FLAGS) ;; \
	  "Linux" )  make -f $(MAKE_FILE) test_program $(LINUX_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
       esac


# -- Targets for specific Linux compilers.
#
# *** NOTE: The PGI compiler must be v6 or later but even ***
# ***       that may not work due to compiler bugs        ***
intel_debug:
	make -f $(MAKE_FILE) test_program $(LINUX_FLAGS_INTEL_DEBUG)

intel:
	make -f $(MAKE_FILE) test_program $(LINUX_FLAGS_INTEL_PROD)

lahey_debug:
	make -f $(MAKE_FILE) test_program $(LINUX_FLAGS_LAHEY_DEBUG)

lahey:
	make -f $(MAKE_FILE) test_program $(LINUX_FLAGS_LAHEY_PROD)

pgi_debug:
	make -f $(MAKE_FILE) test_program $(LINUX_FLAGS_PGI_DEBUG)

pgi:
	make -f $(MAKE_FILE) test_program $(LINUX_FLAGS_PGI_PROD)

g95_debug:
	make -f $(MAKE_FILE) test_program $(LINUX_FLAGS_G95_DEBUG)

g95:
	make -f $(MAKE_FILE) test_program $(LINUX_FLAGS_G95_PROD)


# ----------------
# Make the program
# ----------------

test_program: create_links $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(EXTRA_FL_FLAGS) $(FL_FLAGS) $(EXE_FILE)



# --------
# Clean up
# --------

clean:
	$(REMOVE) $(OBJ_FILES) $(EXE_FILE) *.mod *.MOD *.stb

realclean: remove_links clean


# ----------------
# Create the links
# ----------------

ATMOSPHERE_DATA_DIR = $(REPOSITORY_DIR)/CRTM/Test_Code/Test_DataFiles
ATMOSPHERE_DATA_FILE = ECMWF-Atmosphere.Cloud.Aerosol.bin

DATA_ENDIAN_TAGS = Big_Endian

create_links:
	@if [ ! -f ${ATMOSPHERE_DATA_FILE} ]; then \
	  link_file=${ATMOSPHERE_DATA_FILE}.${DATA_ENDIAN_TAGS} ; \
	  $(LINK) ${ATMOSPHERE_DATA_DIR}/${DATA_ENDIAN_TAGS}/${ATMOSPHERE_DATA_FILE} $$link_file ; \
	fi


# ----------------
# Remove the links
# ----------------

remove_links:
	@$(REMOVE) *.bin.${DATA_ENDIAN_TAGS}


# ---------------
# Dependency list
# ---------------

$(EXE_FILE).o : $(EXE_FILE).f90


#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(REPOSITORY_DIR)/make.rules

