#==============================================================================
#
# Makefile for Create_IR_SpcCoeff_Sensor program
#
# $Id: Makefile,v 1.22 2006/05/02 16:58:03 dgroff Exp $
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

EXE_FILE = Create_IR_SpcCoeff_Sensor


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Message_Handler.o \
                    Compare_Float_Numbers.o \
                    Fundamental_Constants.o
INTERPOLATE_OBJ_FILES = Interpolate.o
INTEGRATE_OBJ_FILES = Integrate.o
PLANCK_OBJ_FILES = Planck_Functions.o
SPECTRAL_OBJ_FILES = Spectral_Units_Conversion.o
SENSORINFO_OBJ_FILES = SensorInfo_Define.o \
                       SensorInfo_LinkedList.o \
                       SensorInfo_IO.o
SRF_OBJ_FILES = SRF_Define.o \
                SRF_netCDF_IO.o
SOLAR_OBJ_FILES = Solar_Define.o \
                  Solar_netCDF_IO.o
SPCCOEFF_OBJ_FILES = SpcCoeff_Define.o \
                     SpcCoeff_netCDF_IO.o
NETCDF_OBJ_FILES = netCDF_Utility.o \
                   netCDF_Dimension_Utility.o \
                   netCDF_Variable_Utility.o \
                   netCDF_Attribute_Utility.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(INTERPOLATE_OBJ_FILES) \
            $(INTEGRATE_OBJ_FILES) \
            $(PLANCK_OBJ_FILES) \
            $(SPECTRAL_OBJ_FILES) \
            $(SENSORINFO_OBJ_FILES) \
            $(SRF_OBJ_FILES) \
            $(SOLAR_OBJ_FILES) \
            $(SPCCOEFF_OBJ_FILES) \
            $(NETCDF_OBJ_FILES) \
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

# -- Main targets

create_links: create_source_links create_data_links

remove_links: remove_source_links remove_data_links


# -- Source code linking

UTILITY_SOURCE_DIR = $(F90_DIR)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

NETCDF_SOURCE_DIR = $(F90_DIR)/NETCDF
NETCDF_SOURCE_FILES = ${NETCDF_OBJ_FILES:.o=.f90}

INTERPOLATE_SOURCE_DIR = $(F90_DIR)/Interpolation
INTERPOLATE_SOURCE_FILES = ${INTERPOLATE_OBJ_FILES:.o=.f90}

INTEGRATE_SOURCE_DIR = $(F90_DIR)/Integration
INTEGRATE_SOURCE_FILES = ${INTEGRATE_OBJ_FILES:.o=.f90}

PLANCK_SOURCE_DIR = $(F90_DIR)/Radiance
PLANCK_SOURCE_FILES = ${PLANCK_OBJ_FILES:.o=.f90}

SPECTRAL_SOURCE_DIR = $(F90_DIR)/Spectral
SPECTRAL_SOURCE_FILES = ${SPECTRAL_OBJ_FILES:.o=.f90}

SOLAR_SOURCE_DIR = $(F90_DIR)/Solar_Spectrum
SOLAR_SOURCE_FILES = ${SOLAR_OBJ_FILES:.o=.f90}

SENSORINFO_SOURCE_DIR = $(F90_DIR)/Instrument_Information/SensorInfo
SENSORINFO_SOURCE_FILES = ${SENSORINFO_OBJ_FILES:.o=.f90}

SRF_SOURCE_DIR = $(F90_DIR)/Instrument_Information/SRF
SRF_SOURCE_FILES = ${SRF_OBJ_FILES:.o=.f90}

SPCCOEFF_SOURCE_DIR = $(F90_DIR)/Instrument_Information/SpcCoeff
SPCCOEFF_SOURCE_FILES = ${SPCCOEFF_OBJ_FILES:.o=.f90}

create_source_links:
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
	@for source_file in ${PLANCK_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${PLANCK_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${SPECTRAL_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SPECTRAL_SOURCE_DIR}/$$source_file . ; \
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
	@for source_file in ${SOLAR_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SOLAR_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${SPCCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SPCCOEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${NETCDF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${NETCDF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done


# -- Source code removal

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(INTERPOLATE_SOURCE_FILES) \
                   $(INTEGRATE_SOURCE_FILES) \
                   $(PLANCK_SOURCE_FILES) \
                   $(SPECTRAL_SOURCE_FILES) \
                   $(SENSORINFO_SOURCE_FILES) \
                   $(SRF_SOURCE_FILES) \
                   $(SOLAR_SOURCE_FILES) \
                   $(SPCCOEFF_SOURCE_FILES) \
                   $(NETCDF_SOURCE_FILES)

remove_source_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done


# -- Test data file linking

SENSORINFO_DATA_DIR = $(F90_DIR)/Instrument_Information/SensorInfo
SENSORINFO_DATA_FILE = SensorInfo

SOLAR_DATA_DIR = $(HOME)/Solar_Spectrum
SOLAR_DATA_FILE = solar.nc

SRF_DATA_DIR = $(HOME)/SRF_Data/netCDF/Interpolated
SRF_DATA_FILES = hirs3_n17.srf.nc airsM9_aqua.srf.nc

create_data_links:
	@if [ ! -f ${SENSORINFO_DATA_FILE} ]; then \
	    $(LINK) ${SENSORINFO_DATA_DIR}/${SENSORINFO_DATA_FILE} . ; \
	fi
	@if [ ! -f ${SOLAR_DATA_FILE} ]; then \
	    $(LINK) ${SOLAR_DATA_DIR}/${SOLAR_DATA_FILE} . ; \
	fi
	@for srf_file in ${SRF_DATA_FILES}; do \
	  if [ ! -f $$srf_file ]; then \
	    $(LINK) ${SRF_DATA_DIR}/$$srf_file . ; \
	  fi ; \
	done


# -- Test data file removal

remove_data_links:
	$(REMOVE) ${SENSORINFO_DATA_FILE}
	$(REMOVE) ${SOLAR_DATA_FILE}
	$(REMOVE) ${SRF_DATA_FILES}



# ----------------
# Dependency lists
# ----------------

Type_Kinds.o : Type_Kinds.f90 
File_Utility.o : File_Utility.f90 
Message_Handler.o : Message_Handler.f90 File_Utility.o 
Compare_Float_Numbers.o : Compare_Float_Numbers.f90 Type_Kinds.o Message_Handler.o 
Fundamental_Constants.o : Fundamental_Constants.f90 Type_Kinds.o 
Planck_Functions.o : Planck_Functions.f90 Type_Kinds.o Fundamental_Constants.o Message_Handler.o 

Interpolate.o : Interpolate.f90 Type_Kinds.o Message_Handler.o 
Integrate.o : Integrate.f90 Type_Kinds.o Message_Handler.o Interpolate.o 

Spectral_Units_Conversion.o : Spectral_Units_Conversion.f90 Type_Kinds.o Fundamental_Constants.o 

netCDF_Dimension_Utility.o : netCDF_Dimension_Utility.f90 Type_Kinds.o Message_Handler.o 
netCDF_Variable_Utility.o : netCDF_Variable_Utility.f90 Type_Kinds.o Message_Handler.o 
netCDF_Attribute_Utility.o : netCDF_Attribute_Utility.f90 Type_Kinds.o Message_Handler.o 
netCDF_Utility.o : netCDF_Utility.f90 Type_Kinds.o Message_Handler.o netCDF_Dimension_Utility.o netCDF_Variable_Utility.o netCDF_Attribute_Utility.o

SensorInfo_Define.o : SensorInfo_Define.f90 Type_Kinds.o Message_Handler.o
SensorInfo_LinkedList.o : SensorInfo_LinkedList.f90 Type_Kinds.o Message_Handler.o SensorInfo_Define.o 
SensorInfo_IO.o : SensorInfo_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o SensorInfo_Define.o 

SRF_netCDF_IO.o : SRF_netCDF_IO.f90 Type_Kinds.o Message_Handler.o SRF_Define.o netCDF_Utility.o 
SRF_Define.o : SRF_Define.f90 Type_Kinds.o Message_Handler.o 

Solar_netCDF_IO.o : Solar_netCDF_IO.f90 Type_Kinds.o Message_Handler.o Solar_Define.o netCDF_Utility.o 
Solar_Define.o : Solar_Define.f90 Type_Kinds.o Message_Handler.o File_Utility.o 

SpcCoeff_netCDF_IO.o : SpcCoeff_netCDF_IO.f90 Type_Kinds.o Message_Handler.o SpcCoeff_Define.o netCDF_Utility.o 
SpcCoeff_Define.o : SpcCoeff_Define.f90 Type_Kinds.o File_Utility.o Message_Handler.o Compare_Float_Numbers.o

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o File_Utility.o Message_Handler.o Compare_Float_Numbers.o Fundamental_Constants.o Planck_Functions.o Interpolate.o Integrate.o Spectral_Units_Conversion.o SensorInfo_Define.o SensorInfo_LinkedList.o SensorInfo_IO.o SRF_Define.o SRF_netCDF_IO.o Solar_Define.o Solar_netCDF_IO.o SpcCoeff_Define.o SpcCoeff_netCDF_IO.o


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
	$(TARBALL_APPEND) $(TAR_FILE) $(SENSORINFO_DATA_FILE)
	$(TARBALL_APPEND) $(TAR_FILE) $(SOLAR_DATA_FILE)
	$(TARBALL_APPEND) $(TAR_FILE) $(SRF_DATA_FILES)
	compress -f $(TAR_FILE)


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

