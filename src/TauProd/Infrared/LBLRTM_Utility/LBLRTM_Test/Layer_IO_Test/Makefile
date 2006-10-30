#==============================================================================
#
# Makefile for LBLRTM Layer_IO_Test program
#
# $Id: Makefile,v 1.1 2002/04/12 20:31:17 paulv Exp $
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

EXE_FILE = Layer_IO_Test


# ------------
# Object files
# ------------

GENERAL_OBJ_FILES = type_kinds.o
UTILITY_OBJ_FILES = file_utility.o \
                    error_handler.o
LBL_OBJ_FILES = LBL_Define.o
LBLRTM_OBJ_FILES = LBLRTM_Parameters.o \
                   LBLRTM_Utility.o \
                   LBLRTM_Fhdr_IO.o \
                   LBLRTM_Phdr_IO.o \
                   LBLRTM_Panel_IO.o \
                   LBLRTM_Layer_IO.o

OBJ_FILES = $(GENERAL_OBJ_FILES) \
            $(UTILITY_OBJ_FILES) \
            $(LBL_OBJ_FILES) \
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


GENERAL_SOURCE_DIR = $(F90_DIR)/General
GENERAL_SOURCE_FILES = ${GENERAL_OBJ_FILES:.o=.f90}

UTILITY_SOURCE_DIR = $(F90_DIR)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

LBL_SOURCE_DIR = $(F90_DIR)/LBL
LBL_SOURCE_FILES = ${LBL_OBJ_FILES:.o=.f90}

LBLRTM_SOURCE_DIR = $(F90_DIR)/LBLRTM
LBLRTM_SOURCE_FILES = ${LBLRTM_OBJ_FILES:.o=.f90}

create_links:
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
	@for source_file in ${LBL_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${LBL_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${LBLRTM_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${LBLRTM_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(GENERAL_SOURCE_FILES) \
                   $(UTILITY_SOURCE_FILES) \
                   $(LBL_SOURCE_FILES) \
                   $(LBLRTM_SOURCE_FILES)
                   
remove_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done


# ---------------
# Dependency list
# ---------------

type_kinds.o : type_kinds.f90 
file_utility.o : file_utility.f90 
error_handler.o : error_handler.f90 file_utility.o 

LBL_Define.o : LBL_Define.f90 type_kinds.o error_handler.o 

LBLRTM_Parameters.o : LBLRTM_Parameters.f90 type_kinds.o 
LBLRTM_Utility.o : LBLRTM_Utility.f90 type_kinds.o file_utility.o error_handler.o LBLRTM_Parameters.o 
LBLRTM_Fhdr_IO.o : LBLRTM_Fhdr_IO.f90 type_kinds.o file_utility.o error_handler.o LBLRTM_Parameters.o 
LBLRTM_Phdr_IO.o : LBLRTM_Phdr_IO.f90 type_kinds.o file_utility.o error_handler.o LBLRTM_Parameters.o 
LBLRTM_Panel_IO.o : LBLRTM_Panel_IO.f90 type_kinds.o file_utility.o error_handler.o LBLRTM_Parameters.o 
LBLRTM_Layer_IO.o : LBLRTM_Layer_IO.f90 type_kinds.o file_utility.o error_handler.o LBL_Define.o LBLRTM_Parameters.o LBLRTM_Utility.o LBLRTM_Fhdr_IO.o LBLRTM_Phdr_IO.o LBLRTM_Panel_IO.o 

$(EXE_FILE).o : $(EXE_FILE).f90 type_kinds.o file_utility.o error_handler.o LBLRTM_Parameters.o LBLRTM_Utility.o LBLRTM_Layer_IO.o 


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

