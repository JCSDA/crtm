#==============================================================================
#
# Makefile for LBLRTM_to_netCDF program
#
# $Id: Makefile,v 1.9 2005/12/20 18:42:47 paulv Exp $
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

EXE_FILE = LBLRTM_to_netCDF


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Error_Handler.o \
                    Compare_Float_Numbers.o
NETCDF_OBJ_FILES = netCDF_Utility.o \
                   netCDF_Dimension_Utility.o \
                   netCDF_Attribute_Utility.o \
                   netCDF_Variable_Utility.o
LBLRTM_OBJ_FILES = LBLRTM_Parameters.o \
                   LBLRTM_Utility.o \
                   LBLRTM_Fhdr_IO.o \
                   LBLRTM_Phdr_IO.o \
                   LBLRTM_Panel_IO.o \
                   LBLRTM_Layer_IO.o \
                   LBLRTM_netCDF_IO.o
TAU_PRODUCTION_OBJ_FILES = Tau_Production_Parameters.o \
                           Tau_Production_Utility.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(NETCDF_OBJ_FILES) \
            $(LBLRTM_OBJ_FILES) \
            $(TAU_PRODUCTION_OBJ_FILES) \
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
	  "Linux" )  make -f $(MAKEFILE) the_program $(LINUX_FLAGS) ;; \
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

LBLRTM_SOURCE_DIR = $(F90_DIR)/LBLRTM
LBLRTM_SOURCE_FILES = ${LBLRTM_OBJ_FILES:.o=.f90}

TAU_PRODUCTION_SOURCE_DIR = $(F90_DIR)/Transmittance_Production
TAU_PRODUCTION_SOURCE_FILES = ${TAU_PRODUCTION_OBJ_FILES:.o=.f90}

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
	@for source_file in ${LBLRTM_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${LBLRTM_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${TAU_PRODUCTION_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${TAU_PRODUCTION_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(NETCDF_SOURCE_FILES) \
                   $(LBLRTM_SOURCE_FILES) \
                   $(TAU_PRODUCTION_SOURCE_FILES)
                   
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

netCDF_Dimension_Utility.o : netCDF_Dimension_Utility.f90 Type_Kinds.o Error_Handler.o 
netCDF_Variable_Utility.o : netCDF_Variable_Utility.f90 Type_Kinds.o Error_Handler.o 
netCDF_Attribute_Utility.o : netCDF_Attribute_Utility.f90 Error_Handler.o Type_Kinds.o 
netCDF_Utility.o : netCDF_Utility.f90 netCDF_Attribute_Utility.o netCDF_Variable_Utility.o netCDF_Dimension_Utility.o Error_Handler.o Type_Kinds.o 

LBLRTM_Parameters.o : LBLRTM_Parameters.f90 Type_Kinds.o 
LBLRTM_Utility.o : LBLRTM_Utility.f90 Type_Kinds.o File_Utility.o Error_Handler.o LBLRTM_Parameters.o 
LBLRTM_Fhdr_IO.o : LBLRTM_Fhdr_IO.f90 Type_Kinds.o File_Utility.o Error_Handler.o LBLRTM_Parameters.o 
LBLRTM_Phdr_IO.o : LBLRTM_Phdr_IO.f90 Type_Kinds.o File_Utility.o Error_Handler.o LBLRTM_Parameters.o 
LBLRTM_Panel_IO.o : LBLRTM_Panel_IO.f90 Type_Kinds.o File_Utility.o Error_Handler.o LBLRTM_Parameters.o 
LBLRTM_Layer_IO.o : LBLRTM_Layer_IO.f90 Type_Kinds.o File_Utility.o Error_Handler.o LBLRTM_Parameters.o LBLRTM_Utility.o LBLRTM_Fhdr_IO.o LBLRTM_Phdr_IO.o LBLRTM_Panel_IO.o 
LBLRTM_netCDF_IO.o : LBLRTM_netCDF_IO.f90 Type_Kinds.o Error_Handler.o netCDF_Utility.o 

Tau_Production_Parameters.o : Tau_Production_Parameters.f90 Type_Kinds.o LBLRTM_Parameters.o 
Tau_Production_Utility.o : Tau_Production_Utility.f90 Type_Kinds.o File_Utility.o Error_Handler.o Compare_Float_Numbers.o

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o File_Utility.o Error_Handler.o LBLRTM_Parameters.o LBLRTM_Utility.o LBLRTM_Fhdr_IO.o LBLRTM_Layer_IO.o LBLRTM_netCDF_IO.o Tau_Production_Parameters.o Tau_Production_Utility.o


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

