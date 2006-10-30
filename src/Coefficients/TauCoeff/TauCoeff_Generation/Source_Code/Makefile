#==============================================================================
#
# Makefile for TauCoeff generation program
#
# $Id: Makefile,v 1.2 2004/05/28 20:49:39 paulv Exp $
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

EXE_FILE = GenTransCoef


# -------------
# This makefile
# -------------

MAKEFILE = Makefile


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = type_kinds.o \
                    file_utility.o \
                    error_handler.o \
                    Compare_Float_Numbers.o \
                    Coeff_Binary_Utility.o
CONSTANTS_OBJ_FILES = fundamental_constants.o
SENSORINFO_OBJ_FILES = SensorInfo_Define.o \
                       SensorInfo_LinkedList.o \
                       SensorInfo_IO.o
NETCDF_OBJ_FILES = netCDF_Utility.o \
                   netCDF_Dimension_Utility.o \
                   netCDF_Variable_Utility.o \
                   netCDF_Attribute_Utility.o
SPCCOEFF_OBJ_FILES = SpcCoeff_Define.o \
                     SpcCoeff_netCDF_IO.o
TAUCOEFF_OBJ_FILES = TauCoeff_Define.o \
                     TauCoeff_netCDF_IO.o
PROFILE_UTILITY_OBJ_FILES = Units_Conversion.o \
                            Profile_Utility_Parameters.o \
                            Atmospheric_Properties.o
ATMPROFILE_OBJ_FILES = AtmProfile_Define.o \
                       AtmProfile_netCDF_IO.o
TAUPROFILE_OBJ_FILES = TauProfile_Define.o \
                       TauProfile_netCDF_IO.o
NCEP_RTM_OBJ_FILES = absorber_profile.o \
                     predictors.o \
                     parameters.o
TAUCOEFF_GENERATION_OBJ_FILES = ParametersGenCoef.o \
                                ChanType.o \
                                ReadParameters.o \
                                ConvLevelLayer.o \
                                ReadProfile_netCDF.o \
                                ReadProfile_Bin.o \
                                AbsorberAmount.o \
                                PredictandPredictor.o \
                                PlanckFunc.o \
                                CalcRegWeight.o \
                                CalcRegCoef.o \
                                PredAbsCoefTransTemp.o \
                                Utilities_Statistics.o \
                                CalcStatTransTemp.o \
                                WriteTransTable.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(CONSTANTS_OBJ_FILES) \
            $(SENSORINFO_OBJ_FILES) \
            $(NETCDF_OBJ_FILES) \
            $(SPCCOEFF_OBJ_FILES) \
            $(TAUCOEFF_OBJ_FILES) \
            $(PROFILE_UTILITY_OBJ_FILES) \
            $(ATMPROFILE_OBJ_FILES) \
            $(TAUPROFILE_OBJ_FILES) \
            $(NCEP_RTM_OBJ_FILES) \
            $(TAUCOEFF_GENERATION_OBJ_FILES) \
            $(EXE_FILE).o


# -------------------------------
# Include and library definitions
# -------------------------------

INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf -L$(HOME)/local/lapack/lib -llapack -lblas


# ------------------------
# Call compiler and linker
# ------------------------

all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")   make -f $(MAKEFILE) the_program $(SUNOS_FLAGS) ;; \
	  "AIX")     make -f $(MAKEFILE) the_program $(AIX_FLAGS) ;; \
	  "IRIX64" ) make -f $(MAKEFILE) the_program $(IRIX64_FLAGS) ;; \
	  "Linux" )  make -f $(MAKEFILE) the_program $(Linux_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
	esac


# ----------------
# Make the program
# ----------------

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

CONSTANTS_SOURCE_DIR = $(F90_DIR)/Constants
CONSTANTS_SOURCE_FILES = ${CONSTANTS_OBJ_FILES:.o=.f90}

SENSORINFO_SOURCE_DIR = $(F90_DIR)/Instrument_Information/SensorInfo
SENSORINFO_SOURCE_FILES = ${SENSORINFO_OBJ_FILES:.o=.f90}

NETCDF_SOURCE_DIR = $(F90_DIR)/NETCDF
NETCDF_SOURCE_FILES = ${NETCDF_OBJ_FILES:.o=.f90}

SPCCOEFF_SOURCE_DIR = $(F90_DIR)/Instrument_Information/SpcCoeff
SPCCOEFF_SOURCE_FILES = ${SPCCOEFF_OBJ_FILES:.o=.f90}

TAUCOEFF_SOURCE_DIR = $(F90_DIR)/NCEP_RTM/Coefficients/TauCoeff
TAUCOEFF_SOURCE_FILES = ${TAUCOEFF_OBJ_FILES:.o=.f90}

PROFILE_UTILITY_SOURCE_DIR = $(F90_DIR)/Profile_Utility
PROFILE_UTILITY_SOURCE_FILES = ${PROFILE_UTILITY_OBJ_FILES:.o=.f90}

ATMPROFILE_SOURCE_DIR = $(F90_DIR)/AtmProfile
ATMPROFILE_SOURCE_FILES = ${ATMPROFILE_OBJ_FILES:.o=.f90}

TAUPROFILE_SOURCE_DIR = $(F90_DIR)/TauProfile
TAUPROFILE_SOURCE_FILES = ${TAUPROFILE_OBJ_FILES:.o=.f90}

NCEP_RTM_SOURCE_DIR = $(F90_DIR)/NCEP_RTM/Source_Code
NCEP_RTM_SOURCE_FILES = ${NCEP_RTM_OBJ_FILES:.o=.f90}

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
	@for source_file in ${SENSORINFO_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SENSORINFO_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${NETCDF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${NETCDF_SOURCE_DIR}/$$source_file . ; \
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
	@for source_file in ${PROFILE_UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${PROFILE_UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${ATMPROFILE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${ATMPROFILE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${TAUPROFILE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${TAUPROFILE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${NCEP_RTM_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${NCEP_RTM_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_LINKED_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                          $(CONSTANTS_SOURCE_FILES) \
                          $(SENSORINFO_SOURCE_FILES) \
                          $(NETCDF_SOURCE_FILES) \
                          $(SPCCOEFF_SOURCE_FILES) \
                          $(TAUCOEFF_SOURCE_FILES) \
                          $(PROFILE_UTILITY_SOURCE_FILES) \
                          $(ATMPROFILE_SOURCE_FILES) \
                          $(TAUPROFILE_SOURCE_FILES) \
                          $(NCEP_RTM_SOURCE_FILES)

remove_links:
	@for source_file in ${ALL_LINKED_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done


# ---------------
# Dependency list
# ---------------

type_kinds.o : type_kinds.f90 
predictors.o : predictors.f90 type_kinds.o error_handler.o parameters.o 
parameters.o : parameters.f90 type_kinds.o 
netCDF_Variable_Utility.o : netCDF_Variable_Utility.f90 type_kinds.o error_handler.o 
netCDF_Utility.o : netCDF_Utility.f90 type_kinds.o error_handler.o netCDF_Dimension_Utility.o netCDF_Variable_Utility.o netCDF_Attribute_Utility.o 
netCDF_Dimension_Utility.o : netCDF_Dimension_Utility.f90 type_kinds.o error_handler.o 
netCDF_Attribute_Utility.o : netCDF_Attribute_Utility.f90 type_kinds.o error_handler.o 
fundamental_constants.o : fundamental_constants.f90 type_kinds.o 
file_utility.o : file_utility.f90 
error_handler.o : error_handler.f90 file_utility.o 
absorber_profile.o : absorber_profile.f90 type_kinds.o parameters.o 
WriteTransTable.o : WriteTransTable.f90 type_kinds.o error_handler.o ParametersGenCoef.o TauCoeff_Define.o TauCoeff_netCDF_IO.o 
Utilities_Statistics.o : Utilities_Statistics.f90 type_kinds.o ParametersGenCoef.o 
Units_Conversion.o : Units_Conversion.f90 type_kinds.o error_handler.o Profile_Utility_Parameters.o Atmospheric_Properties.o 
TauProfile_netCDF_IO.o : TauProfile_netCDF_IO.f90 type_kinds.o error_handler.o TauProfile_Define.o netCDF_Utility.o 
TauProfile_Define.o : TauProfile_Define.f90 type_kinds.o error_handler.o Compare_Float_Numbers.o 
TauCoeff_netCDF_IO.o : TauCoeff_netCDF_IO.f90 type_kinds.o error_handler.o TauCoeff_Define.o netCDF_Utility.o 
TauCoeff_Define.o : TauCoeff_Define.f90 type_kinds.o error_handler.o Compare_Float_Numbers.o 
SpcCoeff_netCDF_IO.o : SpcCoeff_netCDF_IO.f90 type_kinds.o error_handler.o SpcCoeff_Define.o netCDF_Utility.o 
SpcCoeff_Define.o : SpcCoeff_Define.f90 type_kinds.o error_handler.o Compare_Float_Numbers.o 
SensorInfo_LinkedList.o : SensorInfo_LinkedList.f90 type_kinds.o error_handler.o SensorInfo_Define.o 
SensorInfo_IO.o : SensorInfo_IO.f90 type_kinds.o file_utility.o error_handler.o SensorInfo_Define.o SensorInfo_LinkedList.o 
SensorInfo_Define.o : SensorInfo_Define.f90 type_kinds.o error_handler.o 
ReadProfile_netCDF.o : ReadProfile_netCDF.f90 type_kinds.o error_handler.o ParametersGenCoef.o AtmProfile_Define.o AtmProfile_netCDF_IO.o TauProfile_netCDF_IO.o Units_Conversion.o ConvLevelLayer.o 
ReadProfile_Bin.o : ReadProfile_Bin.f90 type_kinds.o ParametersGenCoef.o ConvLevelLayer.o 
ReadParameters.o : ReadParameters.f90 type_kinds.o error_handler.o SensorInfo_Define.o SensorInfo_IO.o SpcCoeff_Define.o SpcCoeff_netCDF_IO.o ParametersGenCoef.o ChanType.o PredictandPredictor.o SensorInfo_LinkedList.o 
Profile_Utility_Parameters.o : Profile_Utility_Parameters.f90 type_kinds.o fundamental_constants.o 
PredictandPredictor.o : PredictandPredictor.f90 type_kinds.o ParametersGenCoef.o predictors.o 
PredAbsCoefTransTemp.o : PredAbsCoefTransTemp.f90 type_kinds.o ParametersGenCoef.o PredictandPredictor.o PlanckFunc.o 
PlanckFunc.o : PlanckFunc.f90 type_kinds.o ParametersGenCoef.o 
ParametersGenCoef.o : ParametersGenCoef.f90 type_kinds.o SpcCoeff_Define.o 
LeastSquareProblem.o : LeastSquareProblem.f90 type_kinds.o ParametersGenCoef.o 
GenTransCoef.o : GenTransCoef.f90 type_kinds.o file_utility.o ParametersGenCoef.o ReadParameters.o ReadProfile_netCDF.o ReadProfile_Bin.o AbsorberAmount.o PredictandPredictor.o CalcRegWeight.o CalcRegCoef.o PredAbsCoefTransTemp.o CalcStatTransTemp.o WriteTransTable.o 
ConvLevelLayer.o : ConvLevelLayer.f90 type_kinds.o ParametersGenCoef.o 
Compare_Float_Numbers.o : Compare_Float_Numbers.f90 type_kinds.o 
Coeff_Binary_Utility.o : Coeff_Binary_Utility.f90 type_kinds.o file_utility.o error_handler.o 
ChanType.o : ChanType.f90 type_kinds.o ParametersGenCoef.o 
CalcStatTransTemp.o : CalcStatTransTemp.f90 type_kinds.o ParametersGenCoef.o Utilities_Statistics.o 
CalcRegWeight.o : CalcRegWeight.f90 type_kinds.o ParametersGenCoef.o PlanckFunc.o 
CalcRegCoef.o : CalcRegCoef.f90 type_kinds.o ParametersGenCoef.o PredictandPredictor.o 
Atmospheric_Properties.o : Atmospheric_Properties.f90 type_kinds.o error_handler.o Profile_Utility_Parameters.o 
AtmProfile_netCDF_IO.o : AtmProfile_netCDF_IO.f90 type_kinds.o error_handler.o AtmProfile_Define.o netCDF_Utility.o 
AtmProfile_Define.o : AtmProfile_Define.f90 type_kinds.o error_handler.o 
AbsorberAmount.o : AbsorberAmount.f90 type_kinds.o ParametersGenCoef.o absorber_profile.o 


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

