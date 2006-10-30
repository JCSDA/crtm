#==============================================================================
#
# Makefile for AtmProfile_NC2BIN program
#
# $Id: Makefile,v 1.5 2004/12/30 18:52:29 paulv Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

F90_DIR = $(HOME)/f90

include $(F90_DIR)/make.macros


# ----------------
# Executable files
# ----------------

EXE_FILE = AtmProfile_NC2BIN


# -------------
# This makefile
# -------------

MAKEFILE = Makefile


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Error_Handler.o \
                    Compare_Float_Numbers.o \
                    Endian_Utility.o \
                    Binary_File_Utility.o
NETCDF_OBJ_FILES = netCDF_Utility.o \
                   netCDF_Dimension_Utility.o \
                   netCDF_Attribute_Utility.o \
                   netCDF_Variable_Utility.o
ATMPROFILE_OBJ_FILES = AtmProfile_Define.o \
                       AtmProfile_Binary_IO.o \
                       AtmProfile_netCDF_IO.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(NETCDF_OBJ_FILES) \
            $(ATMPROFILE_OBJ_FILES) \
            $(EXE_FILE).o


# -------------------------------
# Include and library definitions
# -------------------------------

INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf



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
	$(REMOVE) $(OBJ_FILES) *.mod *.MOD *.stb

realclean: remove_links clean


# ----------
# Install it
# ----------

install:
	@if [ -d $(HOME)/bin ]; then \
	  $(MOVE) $(EXE_FILE) $(HOME)/bin; \
	fi


# ----------------------------
# Create links for compilation
# ----------------------------


UTILITY_SOURCE_DIR = $(F90_DIR)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

NETCDF_SOURCE_DIR = $(F90_DIR)/NETCDF
NETCDF_SOURCE_FILES = ${NETCDF_OBJ_FILES:.o=.f90}

ATMPROFILE_SOURCE_DIR = $(F90_DIR)/AtmProfile
ATMPROFILE_SOURCE_FILES = ${ATMPROFILE_OBJ_FILES:.o=.f90}

create_links:
	@for source_file in ${UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${NETCDF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${NETCDF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${ATMPROFILE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${ATMPROFILE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(NETCDF_SOURCE_FILES) \
                   $(ATMPROFILE_SOURCE_FILES)
                   
remove_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done


# ---------------
# Dependency list
# ---------------

Type_Kinds.o : Type_Kinds.f90
File_Utility.o : File_Utility.f90
Error_Handler.o : Error_Handler.f90 File_Utility.o
Compare_Float_Numbers.o : Compare_Float_Numbers.f90 Type_Kinds.o
Endian_Utility.o : Endian_Utility.f90 Type_Kinds.o
Binary_File_Utility.o : Binary_File_Utility.f90 Type_Kinds.o File_Utility.o Error_Handler.o Endian_Utility.o

netCDF_Variable_Utility.o : netCDF_Variable_Utility.f90 Type_Kinds.o Error_Handler.o
netCDF_Dimension_Utility.o : netCDF_Dimension_Utility.f90 Type_Kinds.o Error_Handler.o
netCDF_Attribute_Utility.o : netCDF_Attribute_Utility.f90 Type_Kinds.o Error_Handler.o
netCDF_Utility.o : netCDF_Utility.f90 Type_Kinds.o Error_Handler.o netCDF_Dimension_Utility.o netCDF_Variable_Utility.o netCDF_Attribute_Utility.o

AtmProfile_Define.o : AtmProfile_Define.f90 Type_Kinds.o Error_Handler.o Compare_Float_Numbers.o
AtmProfile_netCDF_IO.o : AtmProfile_netCDF_IO.f90 Type_Kinds.o Error_Handler.o AtmProfile_Define.o netCDF_Utility.o
AtmProfile_Binary_IO.o : AtmProfile_Binary_IO.f90 Type_Kinds.o Error_Handler.o Binary_File_Utility.o AtmProfile_Define.o                                                                                                                        

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o File_Utility.o Error_Handler.o AtmProfile_Define.o AtmProfile_Binary_IO.o AtmProfile_netCDF_IO.o


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


# -------------------------
# Create html documentation
# -------------------------

PRO_FILE = main.pro

html:
	@echo "create_f90_html_doc,'${EXE_FILE}.f90'" > ${PRO_FILE}; \
	echo "exit" >> ${PRO_FILE}; \
	idl ${PRO_FILE}; \
	$(REMOVE) $(PRO_FILE)



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(F90_DIR)/make.rules

