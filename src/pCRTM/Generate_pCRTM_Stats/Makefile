#==============================================================================
#
# Makefile for pCRTM statistics generation program
#
# $Id: Makefile,v 1.7 2006/05/02 14:58:35 dgroff Exp $
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

EXE_FILE = Generate_pCRTM_Stats


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Message_Handler.o \
                    Compare_Float_Numbers.o \
                    Endian_Utility.o \
                    Binary_File_Utility.o \
                    List_File_Utility.o
NETCDF_OBJ_FILES = netCDF_Utility.o \
                   netCDF_Dimension_Utility.o \
                   netCDF_Variable_Utility.o \
                   netCDF_Attribute_Utility.o
SENSORINFO_OBJ_FILES = SensorInfo_Define.o \
                       SensorInfo_LinkedList.o \
                       SensorInfo_IO.o
ATMPROFILE_OBJ_FILES = AtmProfile_Define.o \
                       AtmProfile_netCDF_IO.o
SPCCOEFF_OBJ_FILES = SpcCoeff_Define.o \
                     SpcCoeff_Binary_IO.o
TAUCOEFF_OBJ_FILES = TauCoeff_Define.o \
                     TauCoeff_Binary_IO.o
TAUPROFILE_OBJ_FILES = TauProfile_Define.o \
                       TauProfile_netCDF_IO.o
pCRTMSTATS_OBJ_FILES = pCRTMstats_Define.o \
                       pCRTMstats_netCDF_IO.o
pCRTM_OBJ_FILES = parameters.o \
                  sensor_planck_routines.o \
                  spectral_coefficients.o \
                  transmittance_coefficients.o \
                  initialize.o \
                  absorber_profile.o \
                  predictors.o \
                  transmittance.o \
                  radiance.o \
                  forward_model.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(NETCDF_OBJ_FILES) \
            $(SENSORINFO_OBJ_FILES) \
            $(ATMPROFILE_OBJ_FILES) \
            $(SPCCOEFF_OBJ_FILES) \
            $(TAUCOEFF_OBJ_FILES) \
            $(TAUPROFILE_OBJ_FILES) \
            $(pCRTMSTATS_OBJ_FILES) \
            $(pCRTM_OBJ_FILES) \
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
	  "SunOS")   make -f $(MAKE_FILE) the_program $(SUNOS_FLAGS) ;; \
	  "AIX")     make -f $(MAKE_FILE) the_program $(AIX_FLAGS) ;; \
	  "IRIX64" ) make -f $(MAKE_FILE) the_program $(IRIX64_FLAGS) ;; \
	  "Linux" )  make -f $(MAKE_FILE) the_program $(Linux_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
	esac



# ----------------
# Make the program
# ----------------

the_program: create_links $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(EXTRA_FL_FLAGS) $(FL_FLAGS) $(EXE_FILE)



# --------
# Clean up
# --------

clean:
	$(REMOVE) $(OBJ_FILES) $(EXE_FILE) *.mod *.MOD *.stb

realclean: remove_links clean


# ----------
# Install it
# ----------

install:
	@if [ -d $(HOME)/bin ]; then \
	    $(MOVE) $(EXE_FILE) $(HOME)/bin; \
	fi


# --------------------------------
# Create the links for compilation
# --------------------------------

UTILITY_SOURCE_DIR = $(F90_DIR)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

NETCDF_SOURCE_DIR = $(F90_DIR)/NETCDF
NETCDF_SOURCE_FILES = ${NETCDF_OBJ_FILES:.o=.f90}

SENSORINFO_SOURCE_DIR = $(F90_DIR)/Instrument_Information/SensorInfo
SENSORINFO_SOURCE_FILES = ${SENSORINFO_OBJ_FILES:.o=.f90}

ATMPROFILE_SOURCE_DIR = $(F90_DIR)/AtmProfile
ATMPROFILE_SOURCE_FILES = ${ATMPROFILE_OBJ_FILES:.o=.f90}

SPCCOEFF_SOURCE_DIR = $(F90_DIR)/Instrument_Information/SpcCoeff
SPCCOEFF_SOURCE_FILES = ${SPCCOEFF_OBJ_FILES:.o=.f90}

TAUCOEFF_SOURCE_DIR = $(F90_DIR)/CRTM/Coefficients/TauCoeff
TAUCOEFF_SOURCE_FILES = ${TAUCOEFF_OBJ_FILES:.o=.f90}

TAUPROFILE_SOURCE_DIR = $(F90_DIR)/TauProfile
TAUPROFILE_SOURCE_FILES = ${TAUPROFILE_OBJ_FILES:.o=.f90}

pCRTMSTATS_SOURCE_DIR = $(F90_DIR)/CRTM/pCRTM/pCRTMstats
pCRTMSTATS_SOURCE_FILES = ${pCRTMSTATS_OBJ_FILES:.o=.f90}

pCRTM_SOURCE_DIR = $(F90_DIR)/CRTM/pCRTM/Source_Code
pCRTM_SOURCE_FILES = ${pCRTM_OBJ_FILES:.o=.f90}

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
	@for source_file in ${SENSORINFO_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SENSORINFO_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${ATMPROFILE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${ATMPROFILE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${SPCCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SPCCOEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${TAUCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${TAUCOEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${TAUPROFILE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${TAUPROFILE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${pCRTMSTATS_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${pCRTMSTATS_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${pCRTM_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${pCRTM_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(NETCDF_SOURCE_FILES) \
                   $(SENSORINFO_SOURCE_FILES) \
                   $(ATMPROFILE_SOURCE_FILES) \
                   $(SPCCOEFF_SOURCE_FILES) \
                   $(TAUCOEFF_SOURCE_FILES) \
                   $(TAUPROFILE_SOURCE_FILES) \
                   $(pCRTMSTATS_SOURCE_FILES) \
                   $(pCRTM_SOURCE_FILES)
                   
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
Compare_Float_Numbers.o : Compare_Float_Numbers.f90 Type_Kinds.o 
Endian_Utility.o : Endian_Utility.f90 Type_Kinds.o
Binary_File_Utility.o : Binary_File_Utility.f90 Type_Kinds.o File_Utility.o Message_Handler.o Endian_Utility.o
List_File_Utility.o : List_File_Utility.f90 Type_Kinds.o File_Utility.o Message_Handler.o 

netCDF_Variable_Utility.o : netCDF_Variable_Utility.f90 Type_Kinds.o Message_Handler.o 
netCDF_Dimension_Utility.o : netCDF_Dimension_Utility.f90 Type_Kinds.o Message_Handler.o 
netCDF_Attribute_Utility.o : netCDF_Attribute_Utility.f90 Type_Kinds.o Message_Handler.o 
netCDF_Utility.o : netCDF_Utility.f90 Type_Kinds.o Message_Handler.o netCDF_Dimension_Utility.o netCDF_Variable_Utility.o netCDF_Attribute_Utility.o 

parameters.o : parameters.f90 Type_Kinds.o 
sensor_planck_routines.o : sensor_planck_routines.f90 Type_Kinds.o parameters.o spectral_coefficients.o 
spectral_coefficients.o : spectral_coefficients.f90 Type_Kinds.o Message_Handler.o parameters.o SpcCoeff_Define.o SpcCoeff_Binary_IO.o 
transmittance_coefficients.o : transmittance_coefficients.f90 Type_Kinds.o Message_Handler.o parameters.o TauCoeff_Define.o TauCoeff_Binary_IO.o 
initialize.o : initialize.f90 Message_Handler.o transmittance_coefficients.o spectral_coefficients.o 
absorber_profile.o : absorber_profile.f90 Type_Kinds.o parameters.o 
predictors.o : predictors.f90 Type_Kinds.o parameters.o Message_Handler.o 
transmittance.o : transmittance.f90 Type_Kinds.o parameters.o transmittance_coefficients.o 
radiance.o : radiance.f90 Type_Kinds.o parameters.o spectral_coefficients.o sensor_planck_routines.o 
forward_model.o : forward_model.f90 Type_Kinds.o Message_Handler.o parameters.o spectral_coefficients.o absorber_profile.o predictors.o transmittance.o radiance.o 

SensorInfo_LinkedList.o : SensorInfo_LinkedList.f90 Type_Kinds.o Message_Handler.o SensorInfo_Define.o 
SensorInfo_IO.o : SensorInfo_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o SensorInfo_Define.o SensorInfo_LinkedList.o 
SensorInfo_Define.o : SensorInfo_Define.f90 Type_Kinds.o Message_Handler.o 

AtmProfile_Define.o : AtmProfile_Define.f90 Type_Kinds.o Message_Handler.o 
AtmProfile_netCDF_IO.o : AtmProfile_netCDF_IO.f90 Type_Kinds.o Message_Handler.o AtmProfile_Define.o netCDF_Utility.o 

TauProfile_netCDF_IO.o : TauProfile_netCDF_IO.f90 Type_Kinds.o Message_Handler.o TauProfile_Define.o netCDF_Utility.o 
TauProfile_Define.o : TauProfile_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o 

SpcCoeff_Define.o : SpcCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o 
SpcCoeff_Binary_IO.o : SpcCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o SpcCoeff_Define.o Binary_File_Utility.o 

TauCoeff_Define.o : TauCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o 
TauCoeff_Binary_IO.o : TauCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o TauCoeff_Define.o Binary_File_Utility.o 

pCRTMstats_Define.o : pCRTMstats_Define.f90 Type_Kinds.o Message_Handler.o 
pCRTMstats_netCDF_IO.o : pCRTMstats_netCDF_IO.f90 Type_Kinds.o Message_Handler.o pCRTMstats_Define.o netCDF_Utility.o 

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o File_Utility.o Message_Handler.o List_File_Utility.o AtmProfile_Define.o AtmProfile_netCDF_IO.o SensorInfo_Define.o SensorInfo_LinkedList.o SensorInfo_IO.o TauProfile_Define.o TauProfile_netCDF_IO.o pCRTMstats_Define.o pCRTMstats_netCDF_IO.o initialize.o parameters.o forward_model.o spectral_coefficients.o radiance.o 


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

