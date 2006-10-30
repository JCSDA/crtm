#==============================================================================
#
# Makefile for CRTM Surface Test program
#
# $Id: Makefile,v 1.5 2006/05/02 14:58:35 dgroff Exp $
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

MAKE_FILE = Makefile


# ---------------
# Executable file
# ---------------

EXE_FILE = Test_Surface


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Message_Handler.o \
                    Endian_Utility.o \
                    Binary_File_Utility.o
CRTM_SURFACE_OBJ_FILES = CRTM_SensorData_Define.o \
                         CRTM_Surface_Define.o \
                         CRTM_Surface_Binary_IO.o
CRTM_OBJ_FILES = CRTM_Parameters.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(CRTM_SURFACE_OBJ_FILES) \
            $(CRTM_OBJ_FILES) \
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
	  "Linux" )  make -f $(MAKE_FILE) test_program $(Linux_FLAGS) ;; \
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
	$(REMOVE) $(OBJ_FILES) *.mod *.MOD *.stb

realclean: remove_links clean


# --------------------------------
# Create the links for compilation
# --------------------------------


UTILITY_SOURCE_DIR = $(F90_DIR)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

CRTM_SURFACE_SOURCE_DIR = $(F90_DIR)/CRTM/Surface
CRTM_SURFACE_SOURCE_FILES = ${CRTM_SURFACE_OBJ_FILES:.o=.f90}

CRTM_SOURCE_DIR = $(F90_DIR)/CRTM
CRTM_SOURCE_FILES = ${CRTM_OBJ_FILES:.o=.f90}

create_links:
	@for source_file in ${UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_SURFACE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_SURFACE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(CRTM_SURFACE_SOURCE_FILES) \
                   $(CRTM_SOURCE_FILES)
                   
remove_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done


# ---------------
# Dependency list
# ---------------

Type_Kinds.o : Type_Kinds.f90
File_Utility.o : File_Utility.f90
Message_Handler.o : Message_Handler.f90 File_Utility.o
Endian_Utility.o : Endian_Utility.f90 Type_Kinds.o
Binary_File_Utility.o : Binary_File_Utility.f90 Type_Kinds.o File_Utility.o Message_Handler.o Endian_Utility.o

CRTM_Parameters.o : CRTM_Parameters.f90 Type_Kinds.o

CRTM_Surface_Define.o : CRTM_Surface_Define.f90 Type_Kinds.o Message_Handler.o CRTM_SensorData_Define.o
CRTM_Surface_Binary_IO.o : CRTM_Surface_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o CRTM_Surface_Define.o
CRTM_SensorData_Define.o : CRTM_SensorData_Define.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o

Test_Surface.o : Test_Surface.f90 Type_Kinds.o Message_Handler.o CRTM_Surface_Define.o CRTM_Surface_Binary_IO.o

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

