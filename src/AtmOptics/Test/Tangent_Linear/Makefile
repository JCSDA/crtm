#==============================================================================
#
# Makefile for CRTM AtmAbsorption Tangent-Linear Test program
#
# $Id: Makefile,v 1.3 2006/05/19 20:06:49 dgroff Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

REPOSITORY_DIR = $(HOME)/src
FIX_DIR = $(HOME)/fix
include $(REPOSITORY_DIR)/make.macros

#REMEMBER TO CHANGE COEFF DIRECTORIES TO FIX/COEFF etc.
# -------------
# This makefile
# -------------

MAKE_FILE = Makefile


# ---------------
# Executable file
# ---------------

EXE_FILE = Test_Tangent_Linear


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Message_Handler.o \
                    Compare_Float_Numbers.o \
                    Endian_Utility.o \
                    Binary_File_Utility.o
SPCCOEFF_OBJ_FILES = SpcCoeff_Define.o \
                     SpcCoeff_Binary_IO.o
TAUCOEFF_OBJ_FILES = TauCoeff_Define.o \
                     TauCoeff_Binary_IO.o
AEROSOLCOEFF_OBJ_FILES = AerosolCoeff_Define.o \
                         AerosolCoeff_Binary_IO.o
CLOUDCOEFF_OBJ_FILES = CloudCoeff_Define.o \
		       CloudCoeff_Binary_IO.o
EMISCOEFF_OBJ_FILES = EmisCoeff_Define.o \
                      EmisCoeff_Binary_IO.o
CRTM_COEFF_OBJ_FILES = CRTM_SpcCoeff.o \
                       CRTM_TauCoeff.o \
                       CRTM_AerosolCoeff.o \
                       CRTM_CloudCoeff.o \
                       CRTM_EmisCoeff.o
CRTM_ATMOSPHERE_OBJ_FILES = CRTM_Atmosphere_Define.o \
                            CRTM_Cloud_Define.o \
                            CRTM_Aerosol_Define.o \
                            CRTM_Atmosphere_Binary_IO.o \
                            CRTM_Cloud_Binary_IO.o \
                            CRTM_Aerosol_Binary_IO.o
CRTM_CHANNELINFO_OBJ_FILES = CRTM_ChannelInfo_Define.o \
                             CRTM_ChannelInfo.o
CRTM_GEOMETRYINFO_OBJ_FILES = CRTM_GeometryInfo_Define.o

CRTM_ATMABSORPTION_OBJ_FILES = CRTM_AtmAbsorption_IntAbsorber.o \
                               CRTM_AtmAbsorption_Predictor.o \
                               CRTM_AtmAbsorption_Define.o \
                               CRTM_AtmAbsorption.o
CRTM_ATMSCATTER_OBJ_FILES = CRTM_AtmScatter_Define.o \
			    CRTM_AerosolScatter.o \
			    CRTM_CloudScatter.o
CRTM_ATMOPTICS_OBJ_FILES = CRTM_AtmOptics.o
CRTM_OBJ_FILES = CRTM_Parameters.o \
                 CRTM_LifeCycle.o
CRTM_COMPONENTTEST_OBJ_FILES = ComponentTest_Define.o \
                            ComponentTest_netCDF_IO.o 
NETCDF_OBJ_FILES = netCDF_Utility.o \
                   netCDF_Dimension_Utility.o \
                   netCDF_Variable_Utility.o \
                   netCDF_Attribute_Utility.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(SPCCOEFF_OBJ_FILES) \
            $(TAUCOEFF_OBJ_FILES) \
            $(AEROSOLCOEFF_OBJ_FILES) \
	    $(CLOUDCOEFF_OBJ_FILES) \
            $(EMISCOEFF_OBJ_FILES) \
            $(CRTM_COEFF_OBJ_FILES) \
            $(CRTM_ATMOSPHERE_OBJ_FILES) \
            $(CRTM_SURFACE_OBJ_FILES) \
            $(CRTM_CHANNELINFO_OBJ_FILES) \
            $(CRTM_GEOMETRYINFO_OBJ_FILES) \
            $(CRTM_ATMABSORPTION_OBJ_FILES) \
	    $(CRTM_ATMSCATTER_OBJ_FILES) \
	    $(CRTM_ATMOPTICS_OBJ_FILES) \
            $(CRTM_OBJ_FILES) \
            $(CRTM_COMPONENTTEST_OBJ_FILES) \
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
	$(REMOVE) $(OBJ_FILES) *.mod *.MOD *.stb

realclean: remove_links clean


# --------------------------------
# Create the links for compilation
# --------------------------------

create_links: create_source_links create_data_links

# -- The source code links
UTILITY_SOURCE_DIR = $(REPOSITORY_DIR)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

SPCCOEFF_SOURCE_DIR = $(REPOSITORY_DIR)/Instrument_Information/SpcCoeff
SPCCOEFF_SOURCE_FILES = ${SPCCOEFF_OBJ_FILES:.o=.f90}

TAUCOEFF_SOURCE_DIR = $(REPOSITORY_DIR)/CRTM/Coefficients/TauCoeff
TAUCOEFF_SOURCE_FILES = ${TAUCOEFF_OBJ_FILES:.o=.f90}

AEROSOLCOEFF_SOURCE_DIR = $(REPOSITORY_DIR)/CRTM/Coefficients/AerosolCoeff
AEROSOLCOEFF_SOURCE_FILES = ${AEROSOLCOEFF_OBJ_FILES:.o=.f90}

CLOUDCOEFF_SOURCE_DIR = $(REPOSITORY_DIR)/CRTM/Coefficients/CloudCoeff
CLOUDCOEFF_SOURCE_FILES = ${CLOUDCOEFF_OBJ_FILES:.o=.f90}

EMISCOEFF_SOURCE_DIR = $(REPOSITORY_DIR)/CRTM/Coefficients/EmisCoeff/Spectral
EMISCOEFF_SOURCE_FILES = ${EMISCOEFF_OBJ_FILES:.o=.f90}

CRTM_COEFF_SOURCE_DIR = $(REPOSITORY_DIR)/CRTM/Coefficients
CRTM_COEFF_SOURCE_FILES = ${CRTM_COEFF_OBJ_FILES:.o=.f90}

CRTM_ATMOSPHERE_SOURCE_DIR = $(REPOSITORY_DIR)/CRTM/Atmosphere
CRTM_ATMOSPHERE_SOURCE_FILES = ${CRTM_ATMOSPHERE_OBJ_FILES:.o=.f90}

CRTM_CHANNELINFO_SOURCE_DIR = $(REPOSITORY_DIR)/CRTM/ChannelInfo
CRTM_CHANNELINFO_SOURCE_FILES = ${CRTM_CHANNELINFO_OBJ_FILES:.o=.f90}

CRTM_GEOMETRYINFO_SOURCE_DIR = $(REPOSITORY_DIR)/CRTM/GeometryInfo
CRTM_GEOMETRYINFO_SOURCE_FILES = ${CRTM_GEOMETRYINFO_OBJ_FILES:.o=.f90}

CRTM_ATMABSORPTION_SOURCE_DIR = $(REPOSITORY_DIR)/CRTM/AtmAbsorption
CRTM_ATMABSORPTION_SOURCE_FILES = ${CRTM_ATMABSORPTION_OBJ_FILES:.o=.f90}

CRTM_ATMSCATTER_SOURCE_DIR = $(REPOSITORY_DIR)/CRTM/AtmScatter
CRTM_ATMSCATTER_SOURCE_FILES = $(CRTM_ATMSCATTER_OBJ_FILES:.o=.f90)

CRTM_ATMOPTICS_SOURCE_DIR = $(REPOSITORY_DIR)/CRTM/AtmOptics
CRTM_ATMOPTICS_SOURCE_FILES =${CRTM_ATMOPTICS_OBJ_FILES:.o=.f90}

CRTM_SOURCE_DIR = $(REPOSITORY_DIR)/CRTM
CRTM_SOURCE_FILES = ${CRTM_OBJ_FILES:.o=.f90}

CRTM_COMPONENTTEST_SOURCE_DIR = $(REPOSITORY_DIR)/CRTM/Test_Code/Utility/ComponentTest
CRTM_COMPONENTTEST_SOURCE_FILES = ${CRTM_COMPONENTTEST_OBJ_FILES:.o=.f90}

NETCDF_SOURCE_DIR = $(REPOSITORY_DIR)/NETCDF
NETCDF_SOURCE_FILES = ${NETCDF_OBJ_FILES:.o=.f90}


create_source_links:
	@for source_file in ${UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UTILITY_SOURCE_DIR}/$$source_file . ; \
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
	@for source_file in ${AEROSOLCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${AEROSOLCOEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CLOUDCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CLOUDCOEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${EMISCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${EMISCOEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_COEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_COEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_ATMOSPHERE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_ATMOSPHERE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_CHANNELINFO_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_CHANNELINFO_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_GEOMETRYINFO_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_GEOMETRYINFO_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_ATMABSORPTION_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_ATMABSORPTION_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_ATMSCATTER_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_ATMSCATTER_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_ATMOPTICS_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_ATMOPTICS_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_COMPONENTTEST_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_COMPONENTTEST_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${NETCDF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${NETCDF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

# -- The data file links
ATMOSPHERE_DATA_DIR = $(REPOSITORY_DIR)/CRTM/Test_Code/Test_DataFiles
ATMOSPHERE_DATA_FILE = ECMWF-Atmosphere.Cloud.Aerosol.bin

SURFACE_DATA_DIR = $(REPOSITORY_DIR)/CRTM/Test_Code/Test_DataFiles
SURFACE_DATA_FILE = ECMWF-Surface.bin


SPCCOEFF_DATA_DIR = $(FIX_DIR)/CRTM/SpcCoeff/Current
TAUCOEFF_DATA_DIR = $(FIX_DIR)/CRTM/TauCoeff/Current
AEROSOLCOEFF_DATA_DIR = $(FIX_DIR)/CRTM/AerosolCoeff/Current
CLOUDCOEFF_DATA_DIR = $(FIX_DIR)/CRTM/CloudCoeff/Current
EMISCOEFF_DATA_DIR = $(FIX_DIR)/CRTM/EmisCoeff/Current

DATA_FILE_TAGS = amsua_n17 \
                 hirs3_n17 \
                 ssmis_f16

DATA_ENDIAN_TAGS = Big_Endian

create_data_links:
	@if [ ! -f ${ATMOSPHERE_DATA_FILE} ]; then \
	  link_file=${ATMOSPHERE_DATA_FILE}.${DATA_ENDIAN_TAGS} ; \
	  $(LINK) ${ATMOSPHERE_DATA_DIR}/${DATA_ENDIAN_TAGS}/${ATMOSPHERE_DATA_FILE} $$link_file ; \
	fi
	@if [ ! -f ${SURFACE_DATA_FILE} ]; then \
	  link_file=${SURFACE_DATA_FILE}.${DATA_ENDIAN_TAGS} ; \
	  $(LINK) ${SURFACE_DATA_DIR}/${DATA_ENDIAN_TAGS}/${SURFACE_DATA_FILE} $$link_file ; \
	fi
	@for file_tag in ${DATA_FILE_TAGS}; do \
	  spccoeff_file=$$file_tag.SpcCoeff.bin ; \
	  link_file=$$spccoeff_file.${DATA_ENDIAN_TAGS} ; \
	  if [ ! -f $$link_file ]; then \
	    $(LINK) ${SPCCOEFF_DATA_DIR}/${DATA_ENDIAN_TAGS}/$$spccoeff_file $$link_file ; \
	  fi ; \
	done
	@for file_tag in ${DATA_FILE_TAGS}; do \
	  taucoeff_file=$$file_tag.TauCoeff.bin ; \
	  link_file=$$taucoeff_file.${DATA_ENDIAN_TAGS} ; \
	  if [ ! -f $$link_file ]; then \
	    $(LINK) ${TAUCOEFF_DATA_DIR}/${DATA_ENDIAN_TAGS}/$$taucoeff_file $$link_file ; \
	  fi ; \
	done
	@emiscoeff_file=EmisCoeff.bin ; \
	  link_file=$$emiscoeff_file.${DATA_ENDIAN_TAGS} ; \
	  if [ ! -f $$link_file ]; then \
	    $(LINK) ${EMISCOEFF_DATA_DIR}/${DATA_ENDIAN_TAGS}/$$emiscoeff_file $$link_file ; \
	  fi
	@aerosolcoeff_file=AerosolCoeff.bin ; \
	  link_file=$$aerosolcoeff_file.${DATA_ENDIAN_TAGS} ; \
	  if [ ! -f $$link_file ]; then \
	    $(LINK) ${AEROSOLCOEFF_DATA_DIR}/${DATA_ENDIAN_TAGS}/$$aerosolcoeff_file $$link_file ; \
	  fi
	@cloudcoeff_file=CloudCoeff.bin ; \
	  link_file=$$cloudcoeff_file.${DATA_ENDIAN_TAGS} ; \
	  if [ ! -f $$link_file ]; then \
	    $(LINK) ${CLOUDCOEFF_DATA_DIR}/${DATA_ENDIAN_TAGS}/$$cloudcoeff_file $$link_file ; \
	  fi






# ----------------
# Remove the links
# ----------------

remove_links: remove_source_links remove_data_links

# -- The source code links
ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(SPCCOEFF_SOURCE_FILES) \
                   $(TAUCOEFF_SOURCE_FILES) \
                   $(AEROSOLCOEFF_SOURCE_FILES) \
                   $(CLOUDCOEFF_SOURCE_FILES) \
                   $(EMISCOEFF_SOURCE_FILES) \
                   $(CRTM_COEFF_SOURCE_FILES) \
                   $(CRTM_ATMOSPHERE_SOURCE_FILES) \
                   $(CRTM_CHANNELINFO_SOURCE_FILES) \
                   $(CRTM_GEOMETRYINFO_SOURCE_FILES) \
                   $(CRTM_ATMABSORPTION_SOURCE_FILES) \
		   $(CRTM_ATMSCATTER_SOURCE_FILES) \
                   $(CRTM_ATMOPTICS_SOURCE_FILES) \
		   $(CRTM_SOURCE_FILES) \
                   $(CRTM_COMPONENTTEST_SOURCE_FILES) \
                   $(NETCDF_SOURCE_FILES)
                   
remove_source_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done

# -- The data file links
remove_data_links:
	@for file_tag in ${DATA_FILE_TAGS}; do \
	  $(REMOVE) $$file_tag.*.bin* ; \
	done
	@$(REMOVE) CloudCoeff.bin*
	@$(REMOVE) AerosolCoeff.bin*
	@$(REMOVE) EmisCoeff.bin*


# ---------------
# Dependency list
# ---------------

include make.dependencies


# -------------------------------
# Create tarball for distribution
# -------------------------------

TAR_FILE = $(EXE_FILE)_software.tar

tar: create_links
	$(TARBALL_CREATE) $(TAR_FILE) ${OBJ_FILES:.o=.f90}
	@$(COPY) makefile.dist makefile
	$(TARBALL_APPEND) $(TAR_FILE) makefile
	@$(REMOVE) makefile
	@$(COPY) $(REPOSITORY_DIR)/make.macros .
	@$(COPY) $(REPOSITORY_DIR)/make.rules .
	$(TARBALL_APPEND) $(TAR_FILE) make.macros make.rules
	@$(REMOVE) make.macros make.rules
	@for file_tag in ${DATA_FILE_TAGS}; do \
	  $(TARBALL_APPEND) $(TAR_FILE) $$file_tag.*.bin.*Endian ; \
	done
	compress $(TAR_FILE)



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(REPOSITORY_DIR)/make.rules

