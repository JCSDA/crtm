#==============================================================================
#
# Makefile for AerosolCoeff_IO_Test
#
# $Id: Makefile,v 1.1 2006/05/23 21:54:40 wd20pd Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

include ${REPOSITORY_SOURCE_ROOT}/make.macros


# -------------
# This makefile
# -------------

MAKE_FILE = Makefile


# ---------------
# Executable file
# ---------------

EXE_FILE = AerosolCoeff_IO_Test


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Message_Handler.o \
                    Compare_Float_Numbers.o \
                    Endian_Utility.o \
                    Binary_File_Utility.o
AEROSOLCOEFF_OBJ_FILES = AerosolCoeff_Define.o \
                         AerosolCoeff_Binary_IO.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(AEROSOLCOEFF_OBJ_FILES) \
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
	  "SunOS")   make -f $(MAKE_FILE) test_program $(SUNOS_FLAGS) ;; \
	  "AIX")     make -f $(MAKE_FILE) test_program $(AIX_FLAGS) ;; \
	  "IRIX64" ) make -f $(MAKE_FILE) test_program $(IRIX64_FLAGS) ;; \
	  "Linux" )  make -f $(MAKE_FILE) test_program $(LINUX_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
       esac



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


# --------------------------------
# Create the links for compilation
# --------------------------------

create_links: create_source_links create_data_links

remove_links: remove_source_links remove_data_links

# Source links
UTILITY_SOURCE_DIR = $(REPOSITORY_SOURCE_ROOT)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

AEROSOLCOEFF_SOURCE_DIR = $(REPOSITORY_SOURCE_ROOT)/CRTM/Coefficients/AerosolCoeff
AEROSOLCOEFF_SOURCE_FILES = ${AEROSOLCOEFF_OBJ_FILES:.o=.f90}


create_source_links:
	@for source_file in ${UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${AEROSOLCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${AEROSOLCOEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(AEROSOLCOEFF_SOURCE_FILES)
                   
remove_source_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done

# Datafile links links
create_data_links:
	@$(LINK) $(REPOSITORY_FIXFILE_ROOT)/CRTM/AerosolCoeff/*.bin .
                   
remove_data_links:
	@$(REMOVE) *AerosolCoeff*.bin


# ---------------
# Dependency list
# ---------------

Type_Kinds.o : Type_Kinds.f90
File_Utility.o : File_Utility.f90
Message_Handler.o : Message_Handler.f90 File_Utility.o
Compare_Float_Numbers.o : Compare_Float_Numbers.f90 Type_Kinds.o
Endian_Utility.o : Endian_Utility.f90 Type_Kinds.o
Binary_File_Utility.o : Binary_File_Utility.f90 Type_Kinds.o File_Utility.o Message_Handler.o Endian_Utility.o

AerosolCoeff_Define.o : AerosolCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o
AerosolCoeff_Binary_IO.o : AerosolCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o AerosolCoeff_Define.o Binary_File_Utility.o

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o Message_Handler.o AerosolCoeff_Define.o AerosolCoeff_Binary_IO.o


# -------------------------------
# Create tarball for distribution
# -------------------------------

TAR_FILE = $(EXE_FILE)_software.tar

tar: create_links
	$(TARBALL_CREATE) $(TAR_FILE) ${OBJ_FILES:.o=.f90}
	@$(COPY) makefile.dist makefile
	$(TARBALL_APPEND) $(TAR_FILE) makefile
	@$(REMOVE) makefile
	@$(COPY) $(REPOSITORY_SOURCE_ROOT)/make.macros .
	@$(COPY) $(REPOSITORY_SOURCE_ROOT)/make.rules .
	$(TARBALL_APPEND) $(TAR_FILE) make.macros make.rules
	@$(REMOVE) make.macros make.rules
	compress $(TAR_FILE)



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(REPOSITORY_SOURCE_ROOT)/make.rules

