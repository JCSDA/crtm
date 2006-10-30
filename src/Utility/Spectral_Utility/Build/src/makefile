#==============================================================================
#
# Makefile for Spectral Utility Build
#
# $Id: makefile,v 1.1 2006/05/23 15:31:05 wd20pd Exp $
#
#==============================================================================

#-------------------------------------------------------------------------------#
#                               -- DEFINE MACROS --                             # 
#-------------------------------------------------------------------------------#

include ../make.macros


# -------------
# This makefile
# -------------

MAKE_FILE = makefile


# ----------------
# The library name
# ----------------

LIBRARY_TAG = Spectral_Utility
LIBRARY_NAME = lib$(LIBRARY_TAG).a


# -----------------------------
# Spectral_Utility Object files
# -----------------------------

OBJ_FILES = Type_Kinds.o \
            Fundamental_Constants.o \
            Spectral_Units_Conversion.o \
            Spectral_Utility.o



#-------------------------------------------------------------------------------#
#                             -- DEFINE DEPENDECIES --                          # 
#-------------------------------------------------------------------------------#

# ------------------------------
# Default build based on OS type
# ------------------------------

all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")   make -f $(MAKE_FILE) library $(SUNOS_FLAGS) ;; \
	  "AIX")     make -f $(MAKE_FILE) library $(AIX_FLAGS) ;; \
	  "IRIX64" ) make -f $(MAKE_FILE) library $(IRIX64_FLAGS) ;; \
	  "Linux" )  make -f $(MAKE_FILE) library $(LINUX_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
       esac


# ----------------
# IBM AIX Compiler
# ----------------

ibm_debug:
	make -f $(MAKE_FILE) library $(AIX_FLAGS_DEBUG)

ibm:
	make -f $(MAKE_FILE) library $(AIX_FLAGS_PROD)


# --------------------
# SGI MIPSpro Compiler
# --------------------

sgi_debug:
	make -f $(MAKE_FILE) library $(IRIX64_FLAGS_DEBUG)

sgi:
	make -f $(MAKE_FILE) library $(IRIX64_FLAGS_PROD)


# --------------
# SunOS Compiler
# --------------

sun_debug:
	make -f $(MAKE_FILE) library $(SUNOS_FLAGS_DEBUG)

sun:
	make -f $(MAKE_FILE) library $(SUNOS_FLAGS_PROD)


# -----------------------------------------------------------
# Linux compilers.
#
# *** NOTE: The PGI compiler must be v6 or later but even ***
# ***       that may not work due to compiler bugs        ***
# -----------------------------------------------------------

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



# -----------------------------------
# Build the library and include files
# -----------------------------------

library: $(OBJ_FILES)
	$(ARCHIVER) $(ARCHIVER_FLAGS) $(LIBRARY_NAME) $(OBJ_FILES)


# ------------------
# Clean up the chaff
# ------------------

clean:
	$(REMOVE) $(OBJ_FILES) *.mod *.MOD *.m *.M *.stb *.a


# -------------------------
# Define build dependencies
# -------------------------

include make.dependencies


# --------------------------
# Define default build rules
# --------------------------

include make.rules
