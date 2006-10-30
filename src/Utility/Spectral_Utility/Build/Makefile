#==============================================================================
#
# Makefile for local Spectral Utility Build package
#
# $Id: Makefile,v 1.1 2006/05/23 15:31:05 wd20pd Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

REPOSITORY_DIR = $(HOME)/src

include $(REPOSITORY_DIR)/make.macros


# ----------------
# The library name
# ----------------

LIBRARY_TAG = Spectral_Utility
LIBRARY_NAME = lib$(LIBRARY_TAG).a


# -----------------------
# The library directories
# -----------------------

SOURCE_DIR = src
LIBRARY_DIR = lib
INCLUDE_DIR = include


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    Fundamental_Constants.o

SPECTRAL_UTILITY_OBJ_FILES = Spectral_Units_Conversion.o \
                             Spectral_Utility.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(SPECTRAL_UTILITY_OBJ_FILES)



#-----------------------------------------------------------------------------
#                          -- Define dependecies --
#-----------------------------------------------------------------------------

# ---------------------
# Make based on OS type
# ---------------------

all: create_links
	@make -f makefile.dist all


# --------------------------------------------------------------------
# Install the library and include files in their repective directories
# --------------------------------------------------------------------

install:
	@make -f makefile.dist install


# --------
# Clean up
# --------

clean: remove_links
	@make -f makefile.dist clean

distclean: remove_links
	@make -f makefile.dist distclean


# --------------------------------
# Create the links for compilation
# --------------------------------

# -- The source code links
UTILITY_SOURCE_DIR = $(REPOSITORY_DIR)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

SPECTRAL_UTILITY_SOURCE_DIR = $(REPOSITORY_DIR)/Utility/Spectral_Utility
SPECTRAL_UTILITY_SOURCE_FILES = ${SPECTRAL_UTILITY_OBJ_FILES:.o=.f90}


create_links:
	@if [ ! -f make.macros ]; then \
	  $(LINK) $(REPOSITORY_DIR)/make.macros . ; \
	fi; \
	cd $(SOURCE_DIR); \
	if [ ! -f make.rules ]; then \
	  $(LINK) $(REPOSITORY_DIR)/make.rules . ; \
	fi; \
	for source_file in ${UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done; \
	for source_file in ${SPECTRAL_UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SPECTRAL_UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done; \
	cd ..


# ----------------
# Remove the links
# ----------------

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(SPECTRAL_UTILITY_SOURCE_FILES)

remove_links:
	@cd $(SOURCE_DIR); \
	for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done; cd ..



# -------------------------------
# Create tarball for distribution
# -------------------------------

TAR_FILE = $(LIBRARY_TAG).tar

tar: clean create_links
	$(TARBALL_CREATE) $(TAR_FILE) $(SOURCE_DIR)
	@$(COPY) makefile.dist makefile
	$(TARBALL_APPEND) $(TAR_FILE) makefile
	@$(REMOVE) makefile
	$(TARBALL_APPEND) $(TAR_FILE) make.macros
	compress $(TAR_FILE)

