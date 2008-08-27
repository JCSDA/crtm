#==============================================================================
#
# Makefile for JCSDA CRTM Build
#
# $Id$
#
#==============================================================================

# -------------
# Define macros
# -------------
include ../make.macros

# This makefile
MAKE_FILE = makefile

# The library name
LIBRARY_TAG = CRTM
LIBRARY_NAME = lib$(LIBRARY_TAG).a

# The filelist
include make.filelist


# ------------------
# Define dependecies
# ------------------
# Default build based on OS type
all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")          make -f $(MAKE_FILE) library $(SUNOS_FLAGS) ;; \
	  "AIX")            make -f $(MAKE_FILE) library $(AIX_FLAGS) ;; \
	  "IRIX64")         make -f $(MAKE_FILE) library $(IRIX64_FLAGS) ;; \
	  "HP-UX")          make -f $(MAKE_FILE) library $(HPUX_FLAGS) ;; \
	  "Linux"|"Darwin") make -f $(MAKE_FILE) library $(LINUX_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
       esac

# IBM AIX Compiler
# ----------------
ibm_debug:
	make -f $(MAKE_FILE) library $(AIX_FLAGS_DEBUG)

ibm:
	make -f $(MAKE_FILE) library $(AIX_FLAGS_PROD)


# HP-UX Compiler
# --------------

hp_debug:
	make -f $(MAKE_FILE) library $(HPUX_FLAGS_DEBUG)

hp:
	make -f $(MAKE_FILE) library $(HPUX_FLAGS_PROD)


# SGI MIPSpro Compiler
# --------------------
sgi_debug:
	make -f $(MAKE_FILE) library $(IRIX64_FLAGS_DEBUG)

sgi:
	make -f $(MAKE_FILE) library $(IRIX64_FLAGS_PROD)


# SunOS Compiler
# --------------
sun_debug:
	make -f $(MAKE_FILE) library $(SUNOS_FLAGS_DEBUG)

sun:
	make -f $(MAKE_FILE) library $(SUNOS_FLAGS_PROD)


# Linux compilers
# ---------------
gfortran_debug:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_GFORTRAN_DEBUG)

gfortran:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_GFORTRAN_PROD)


intel_debug:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_INTEL_DEBUG)

intel:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_INTEL_PROD)


lahey_debug:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_LAHEY_DEBUG)

lahey:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_LAHEY_PROD)


pgi_debug:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_PGI_DEBUG)

pgi:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_PGI_PROD)


g95_debug:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_G95_DEBUG)

g95:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_G95_PROD)


absoft_debug:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_ABSOFT_DEBUG)

absoft:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_ABSOFT_PROD)


# Build the library and include files
library: $(OBJ_FILES)
	$(ARCHIVER) $(ARCHIVER_FLAGS) $(LIBRARY_NAME) $(OBJ_FILES)

# Clean up the chaff
clean:
	$(REMOVE) $(OBJ_FILES) *.mod *.MOD *.stb *.a

# Define build dependencies and rules
include make.dependencies
include make.rules
