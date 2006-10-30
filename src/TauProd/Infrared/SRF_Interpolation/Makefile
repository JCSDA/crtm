#==============================================================================
#
# Makefile for Interpolate_SRFs program
#
# $Id: Makefile,v 1.12 2005/12/20 18:42:47 paulv Exp $
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

EXE_FILE = Interpolate_SRFs


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Error_Handler.o \
                    Compare_Float_Numbers.o
INTERPOLATE_OBJ_FILES = Interpolate.o
INTEGRATE_OBJ_FILES = Integrate.o
SENSORINFO_OBJ_FILES = SensorInfo_Define.o \
                       SensorInfo_LinkedList.o \
                       SensorInfo_IO.o
SRF_OBJ_FILES = SRF_Define.o \
                SRF_netCDF_IO.o
NETCDF_OBJ_FILES = netCDF_Utility.o \
                   netCDF_Dimension_Utility.o \
                   netCDF_Attribute_Utility.o \
                   netCDF_Variable_Utility.o
LBLRTM_OBJ_FILES = LBLRTM_Parameters.o
TAU_PRODUCTION_OBJ_FILES = Tau_Production_Parameters.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(INTERPOLATE_OBJ_FILES) \
            $(INTEGRATE_OBJ_FILES) \
            $(SENSORINFO_OBJ_FILES) \
            $(SRF_OBJ_FILES) \
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

INTERPOLATE_SOURCE_DIR = $(F90_DIR)/Interpolation
INTERPOLATE_SOURCE_FILES = ${INTERPOLATE_OBJ_FILES:.o=.f90}

INTEGRATE_SOURCE_DIR = $(F90_DIR)/Integration
INTEGRATE_SOURCE_FILES = ${INTEGRATE_OBJ_FILES:.o=.f90}

SENSORINFO_SOURCE_DIR = $(F90_DIR)/Instrument_Information/SensorInfo
SENSORINFO_SOURCE_FILES = ${SENSORINFO_OBJ_FILES:.o=.f90}

SRF_SOURCE_DIR = $(F90_DIR)/Instrument_Information/SRF
SRF_SOURCE_FILES = ${SRF_OBJ_FILES:.o=.f90}

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
	@for source_file in ${INTERPOLATE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${INTERPOLATE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${INTEGRATE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${INTEGRATE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${SENSORINFO_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SENSORINFO_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${SRF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SRF_SOURCE_DIR}/$$source_file . ; \
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
                   $(INTERPOLATE_SOURCE_FILES) \
                   $(INTEGRATE_SOURCE_FILES) \
                   $(SENSORINFO_SOURCE_FILES) \
                   $(SRF_SOURCE_FILES) \
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

netCDF_Utility.o : netCDF_Utility.f90 Type_Kinds.o Error_Handler.o netCDF_Dimension_Utility.o netCDF_Variable_Utility.o netCDF_Attribute_Utility.o 
netCDF_Dimension_Utility.o : netCDF_Dimension_Utility.f90 Type_Kinds.o Error_Handler.o 
netCDF_Attribute_Utility.o : netCDF_Attribute_Utility.f90 Type_Kinds.o Error_Handler.o 
netCDF_Variable_Utility.o : netCDF_Variable_Utility.f90 Type_Kinds.o Error_Handler.o 

Interpolate.o : Interpolate.f90 Type_Kinds.o Error_Handler.o 
Integrate.o : Integrate.f90 Type_Kinds.o Error_Handler.o Compare_Float_Numbers.o Interpolate.o 

SensorInfo_Define.o : SensorInfo_Define.f90 Type_Kinds.o Error_Handler.o 
SensorInfo_LinkedList.o : SensorInfo_LinkedList.f90 Type_Kinds.o Error_Handler.o SensorInfo_Define.o 
SensorInfo_IO.o : SensorInfo_IO.f90 Type_Kinds.o File_Utility.o Error_Handler.o SensorInfo_Define.o SensorInfo_LinkedList.o 

SRF_Define.o : SRF_Define.f90 Type_Kinds.o Error_Handler.o Integrate.o 
SRF_netCDF_IO.o : SRF_netCDF_IO.f90 Type_Kinds.o Error_Handler.o SRF_Define.o netCDF_Utility.o 

LBLRTM_Parameters.o : LBLRTM_Parameters.f90 Type_Kinds.o
 
Tau_Production_Parameters.o : Tau_Production_Parameters.f90 Type_Kinds.o LBLRTM_Parameters.o

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o File_Utility.o Error_Handler.o Interpolate.o SensorInfo_Define.o SensorInfo_LinkedList.o SensorInfo_IO.o SRF_Define.o SRF_netCDF_IO.o Tau_Production_Parameters.o 


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

