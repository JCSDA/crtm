#==============================================================================
#
# Makefile for Rosenkranz03 test program
#
# $Id: Makefile,v 1.1 2004/11/15 21:19:53 paulv Exp $
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

EXE_FILE = Rosenkranz03_test


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Error_Handler.o
CONSTANTS_OBJ_FILES = Fundamental_Constants.o
PROFILE_UTILITY_OBJ_FILES = Profile_Utility_Parameters.o \
                            Atmospheric_Properties.o \
                            Units_Conversion.o
MWLBL_OBJ_FILES = Rosenkranz03_Coefficients.o \
                  MWLBL_Rosenkranz03.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(CONSTANTS_OBJ_FILES) \
            $(PROFILE_UTILITY_OBJ_FILES) \
            $(MWLBL_OBJ_FILES) \
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
	  "SunOS")   make -f $(MAKEFILE) the_program $(SUNOS_FLAGS) ;; \
	  "AIX")     make -f $(MAKEFILE) the_program $(AIX_FLAGS) ;; \
	  "IRIX64" ) make -f $(MAKEFILE) the_program $(IRIX64_FLAGS) ;; \
	  "Linux" )  make -f $(MAKEFILE) the_program $(Linux_FLAGS) ;; \
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

CONSTANTS_SOURCE_DIR = $(F90_DIR)/Constants
CONSTANTS_SOURCE_FILES = ${CONSTANTS_OBJ_FILES:.o=.f90}
 
PROFILE_UTILITY_SOURCE_DIR = $(F90_DIR)/Profile_Utility
PROFILE_UTILITY_SOURCE_FILES = ${PROFILE_UTILITY_OBJ_FILES:.o=.f90}

MWLBL_SOURCE_DIR = $(F90_DIR)/LBL/Microwave
MWLBL_SOURCE_FILES = ${MWLBL_OBJ_FILES:.o=.f90}

create_links:
	@for source_file in ${UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CONSTANTS_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CONSTANTS_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${PROFILE_UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${PROFILE_UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${MWLBL_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${MWLBL_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(CONSTANTS_SOURCE_FILES) \
                   $(PROFILE_UTILITY_SOURCE_FILES) \
                   $(MWLBL_SOURCE_FILES)
                   
remove_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done


# ---------------
# Dependency list
# ---------------

Type_Kinds.o : Type_Kinds.f90
File_Utility.o : File_Utility.f90 Type_Kinds.o
Error_Handler.o : Error_Handler.f90 File_Utility.o

Fundamental_Constants.o : Fundamental_Constants.f90 Type_Kinds.o
 
Profile_Utility_Parameters.o : Profile_Utility_Parameters.f90 Type_Kinds.o Fundamental_Constants.o
Units_Conversion.o : Units_Conversion.f90 Type_Kinds.o Error_Handler.o Profile_Utility_Parameters.o Atmospheric_Properties.o
Atmospheric_Properties.o : Atmospheric_Properties.f90 Type_Kinds.o Error_Handler.o Profile_Utility_Parameters.o

Rosenkranz03_Coefficients.o : Rosenkranz03_Coefficients.f90
MWLBL_Rosenkranz03.o : MWLBL_Rosenkranz03.f90 Type_Kinds.o Error_Handler.o Fundamental_Constants.o Units_Conversion.o Rosenkranz03_Coefficients.o

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o Units_Conversion.o MWLBL_Rosenkranz03.o


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

