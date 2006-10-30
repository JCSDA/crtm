#==============================================================================
#
# Makefile for Sort_Utility test
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Test_Sort_Utility

# Source file to link
SRC_FILES = Type_Kinds.f90 Sort_Utility.f90

# Obj files for link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} $(EXE_FILE).o


# -------------------
# Define dependencies
# -------------------
# Make based on OS type
all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")  make -f $(MAKE_FILE) test_program $(SUNOS_FLAGS) ;; \
	  "AIX")    make -f $(MAKE_FILE) test_program $(AIX_FLAGS) ;; \
	  "IRIX64") make -f $(MAKE_FILE) test_program $(IRIX64_FLAGS) ;; \
	  "Linux")  make -f $(MAKE_FILE) test_program $(LINUX_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
       esac

# Make the program
test_program: create_links $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(EXTRA_FL_FLAGS) $(FL_FLAGS) $(EXE_FILE)

# Clean up
clean:
	$(REMOVE) $(OBJ_FILES) $(EXE_FILE) *.mod *.MOD *.stb

realclean: remove_links clean

# Create the links for compilation
create_links:
	@linkfiles $(CRTM_SOURCE_ROOT) $(SRC_FILES)

remove_links:
	@unlinkfiles $(SRC_FILES)

# Source file dependency list
include make.dependencies


# --------------------
# Define default rules
# --------------------
include $(CRTM_SOURCE_ROOT)/make.rules

