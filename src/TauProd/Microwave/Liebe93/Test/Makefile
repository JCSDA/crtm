#==============================================================================
#
# Makefile for Liebe93 test program
#
# $Id: Makefile,v 1.3 2004/12/16 18:56:05 paulv Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

F90_DIR = $(HOME)/f90

include $(F90_DIR)/make.macros


# -------------
# This makefile
# -------------

MAKEFILE = Makefile


# ----------------
# Executable files
# ----------------

EXE_FILE = Liebe93_test


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = type_kinds.o \
                    file_utility.o \
                    error_handler.o
MWLBL_OBJ_FILES = Liebe92_Coefficients.o \
                  Liebe93_Coefficients.o \
                  MWLBL_Liebe93.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(MWLBL_OBJ_FILES) \
            $(EXE_FILE).o


# -------------------------------
# Include and library definitions
# -------------------------------

INCLUDES = 
LIBRARIES = 


# -----------------------
# Extra compilation flags
# -----------------------

EXTRA_FC_FLAGS = -r8



#-----------------------------------------------------------------------------
#                          -- Define dependecies --
#-----------------------------------------------------------------------------



# ---------------------
# Make based on OS type
# ---------------------

all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")    make -f $(MAKEFILE) the_program $(SUNOS_FLAGS) ;; \
	   "AIX")     make -f $(MAKEFILE) the_program $(AIX_FLAGS) ;; \
	   "IRIX64" ) make -f $(MAKEFILE) the_program $(IRIX64_FLAGS) ;; \
	   "Linux" )  make -f $(MAKEFILE) the_program $(Linux_FLAGS_PGI) ;; \
	   *) echo "This system is not supported" ;; \
       esac


# ---------------------
# Make the test program
# ---------------------

the_program: create_links $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(FL_FLAGS) $(EXE_FILE)


# --------
# Clean up
# --------

clean:
	$(REMOVE) $(OBJ_FILES) $(EXE_FILE) *.mod *.MOD *.stb

realclean: remove_links clean


# ----------------------------
# Create links for compilation
# ----------------------------


UTILITY_SOURCE_DIR = $(F90_DIR)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

MWLBL_SOURCE_DIR = $(F90_DIR)/LBL/Microwave
MWLBL_SOURCE_FILES = ${MWLBL_OBJ_FILES:.o=.f90}

create_links:
	@for source_file in ${UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${MWLBL_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${MWLBL_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(MWLBL_SOURCE_FILES)
                   
remove_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done


# ---------------
# Dependency list
# ---------------

type_kinds.o : type_kinds.f90
file_utility.o : file_utility.f90 type_kinds.o
error_handler.o : error_handler.f90 file_utility.o

Liebe92_Coefficients.o : Liebe92_Coefficients.f90
Liebe93_Coefficients.o : Liebe93_Coefficients.f90
MWLBL_Liebe93.o : MWLBL_Liebe93.f90 type_kinds.o error_handler.o Liebe92_Coefficients.o Liebe93_Coefficients.o

$(EXE_FILE).o : $(EXE_FILE).f90 type_kinds.o MWLBL_Liebe93.o f90coef92.inc f90coef93.inc


# -------------------------------
# Create tarball for distribution
# -------------------------------

TAR_FILE = $(EXE_FILE)_software.tar

tar: create_links
	$(TARBALL_CREATE) $(TAR_FILE) ${OBJ_FILES:.o=.f90}
	@$(COPY) makefile.dist makefile
	$(TARBALL_APPEND) $(TAR_FILE) makefile
	@$(REMOVE) makefile
	@$(COPY) $(F90_DIR)/make.macros .
	@$(COPY) $(F90_DIR)/make.rules .
	$(TARBALL_APPEND) $(TAR_FILE) make.macros make.rules
	@$(REMOVE) make.macros make.rules
	compress $(TAR_FILE)



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(F90_DIR)/make.rules

