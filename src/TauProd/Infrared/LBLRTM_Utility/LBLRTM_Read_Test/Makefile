#==============================================================================
#
# Makefile for LBLRTM utility test programs
#
# $Id: Makefile,v 1.2 2001/10/17 21:25:22 paulv Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

include $(HOME)/f90/make.macros

# -------------
# This makefile
# -------------

MAKEFILE = Makefile


# ----------------
# Executable files
# ----------------

EXE_FILE = lblrtm_read_test


# ------------
# Object files
# ------------

GENERAL_OBJ_FILES = type_kinds.o
UTILITY_OBJ_FILES = file_utility.o \
                    error_handler.o
INTERPOLATE_OBJ_FILES = interpolate.o
MATH_OBJ_FILES = integrate.o
SPECTRAL_OBJ_FILES = spectral.o
LBLRTM_OBJ_FILES = lblrtm_utility.o

OBJ_FILES = $(GENERAL_OBJ_FILES) \
            $(UTILITY_OBJ_FILES) \
            $(INTERPOLATE_OBJ_FILES) \
            $(MATH_OBJ_FILES) \
            $(SPECTRAL_OBJ_FILES) \
            $(LBLRTM_OBJ_FILES) \
            $(EXE_FILE).o




#-----------------------------------------------------------------------------
#                          -- Define dependecies --
#-----------------------------------------------------------------------------



# ---------------------
# Make based on OS type
# ---------------------

all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")    make -f $(MAKEFILE) read_test $(SUNOS_FLAGS) ;; \
	   "AIX")     make -f $(MAKEFILE) read_test $(AIX_FLAGS) ;; \
	   "IRIX64" ) make -f $(MAKEFILE) read_test $(IRIX64_FLAGS) ;; \
	   "Linux" )  make -f $(MAKEFILE) read_test $(Linux_FLAGS_PGI) ;; \
	   *) echo "This system is not supported" ;; \
       esac


# -----------------------------
# Make the LBLRTM test programs
# -----------------------------

read_test: create_links $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(FL_FLAGS) $(EXE_FILE)


# --------
# Clean up
# --------

clean:
	$(REMOVE) $(OBJ_FILES) $(EXE_FILE) *.mod *.MOD

realclean: remove_links clean


# ----------------------------
# Create links for compilation
# ----------------------------

create_links: create_source_links create_datafile_links

remove_links: remove_source_links remove_datafile_links

GENERAL_SOURCE_DIR = $(HOME)/f90/General
GENERAL_SOURCE_FILES = ${GENERAL_OBJ_FILES:.o=.f90}

UTILITY_SOURCE_DIR = $(HOME)/f90/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

INTERPOLATE_SOURCE_DIR = $(HOME)/f90/Interpolation
INTERPOLATE_SOURCE_FILES = ${INTERPOLATE_OBJ_FILES:.o=.f90}

MATH_SOURCE_DIR = $(HOME)/f90/Math
MATH_SOURCE_FILES = ${MATH_OBJ_FILES:.o=.f90}

SPECTRAL_SOURCE_DIR = $(HOME)/f90/Spectral
SPECTRAL_SOURCE_FILES = ${SPECTRAL_OBJ_FILES:.o=.f90}

LBLRTM_SOURCE_DIR = $(HOME)/f90/LBLRTM
LBLRTM_SOURCE_FILES = ${LBLRTM_OBJ_FILES:.o=.f90}

create_source_links:
	@for source_file in ${GENERAL_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${GENERAL_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${INTERPOLATE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${INTERPOLATE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${MATH_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${MATH_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${SPECTRAL_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SPECTRAL_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${LBLRTM_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${LBLRTM_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(GENERAL_SOURCE_FILES) \
                   $(UTILITY_SOURCE_FILES) \
                   $(INTERPOLATE_SOURCE_FILES) \
                   $(MATH_SOURCE_FILES) \
                   $(SPECTRAL_SOURCE_FILES) \
                   $(LBLRTM_SOURCE_FILES)
                   
remove_source_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done

# -- data file linking
LBLRTM_DATA_FILES = upwelling_tau.band001 upwelling_tau.band002
LBLRTM_DATA_DIR = $(HOME)/f90/LBLRTM/LBLRTM_Test_Data

create_datafile_links:
	@$(LINK) $(LBLRTM_DATA_DIR)/*endian .
	@if [ -n "$(ENDIAN)" ]; then \
	  for data_file in $(LBLRTM_DATA_FILES); do \
	    if [ ! -f $$data_file ]; then \
	      $(LINK) $(LBLRTM_DATA_DIR)/$$data_file.$(ENDIAN) $$data_file ; \
	    fi ; \
	  done ; \
	else \
	  echo $@": nothing done. ENDIAN not defined." ; \
	fi

remove_datafile_links:
	@for data_file in $(LBLRTM_DATA_FILES); do \
	  $(REMOVE) $$data_file ; \
	done

# ---------------
# Dependency list
# ---------------

type_kinds.o : type_kinds.f90 
spectral.o : spectral.f90 type_kinds.o error_handler.o file_utility.o interpolate.o integrate.o 
lblrtm_utility.o : lblrtm_utility.f90 type_kinds.o file_utility.o error_handler.o spectral.o 
$(EXE_FILE).o : $(EXE_FILE).f90 type_kinds.o error_handler.o lblrtm_utility.o 
interpolate.o : interpolate.f90 type_kinds.o error_handler.o 
integrate.o : integrate.f90 type_kinds.o error_handler.o interpolate.o 
file_utility.o : file_utility.f90 
error_handler.o : error_handler.f90 file_utility.o 



# -------------------------------
# Create tarball for distribution
# -------------------------------

TAR_FILE = $(EXE_FILE)_software.tar

tar: create_source_links
	$(TARBALL_CREATE) $(TAR_FILE) ${OBJ_FILES:.o=.f90}
	@$(COPY) makefile.dist makefile
	$(TARBALL_APPEND) $(TAR_FILE) makefile
	@$(REMOVE) makefile
	@$(COPY) $(HOME)/f90/make.macros .
	@$(COPY) $(HOME)/f90/make.rules .
	$(TARBALL_APPEND) $(TAR_FILE) make.macros make.rules
	@$(REMOVE) make.macros make.rules
	@$(LINK) $(LBLRTM_DATA_DIR)/*endian .
	$(TARBALL_APPEND) $(TAR_FILE) *endian
	@$(REMOVE) *endian
	compress $(TAR_FILE)
	make -f $(MAKEFILE) remove_source_links



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(HOME)/f90/make.rules

