#==============================================================================
#
# Makefile for Atmospheric_Properties_Test
#
# $Id: Makefile,v 1.5 2006/06/05 14:52:11 wd20pd Exp $
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

EXE_FILE = Atmospheric_Properties_Test


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
INCLUDE_DIR = $(REPOSITORY_DIR)/Utility/Profile_Utility/Build/include
LIBRARY_DIR = $(REPOSITORY_DIR)/Utility/Profile_Utility/Build/lib

# Library name
LIBRARY_NAME = Profile_Utility

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


# ----------------
# Make the program
# ----------------

test_program: $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(EXTRA_FL_FLAGS) $(FL_FLAGS) $(EXE_FILE)



# --------
# Clean up
# --------

clean:
	$(REMOVE) $(OBJ_FILES) $(EXE_FILE) *.mod *.MOD *.stb


# ---------------
# Dependency list
# ---------------

$(EXE_FILE).o : $(EXE_FILE).f90



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(REPOSITORY_DIR)/make.rules

