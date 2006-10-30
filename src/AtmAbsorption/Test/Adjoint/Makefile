#==============================================================================
#
# Makefile for CRTM AtmAbsorption Adjoint Test program
#
# $Id: Makefile,v 1.7 2006/05/02 14:58:34 dgroff Exp $
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

EXE_FILE = Test_Adjoint


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
SCATTERCOEFF_OBJ_FILES = ScatterCoeff_Define.o \
                         ScatterCoeff_Binary_IO.o
EMISCOEFF_OBJ_FILES = EmisCoeff_Define.o \
                      EmisCoeff_Binary_IO.o
CRTM_COEFF_OBJ_FILES = CRTM_SpcCoeff.o \
                       CRTM_TauCoeff.o \
                       CRTM_AerosolCoeff.o \
                       CRTM_ScatterCoeff.o \
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
CRTM_OBJ_FILES = CRTM_Parameters.o \
                 CRTM_LifeCycle.o
CRTM_TLADMTEST_OBJ_FILES = TLADMtest_Define.o \
                           TLADMtest_netCDF_IO.o 
NETCDF_OBJ_FILES = netCDF_Utility.o \
                   netCDF_Dimension_Utility.o \
                   netCDF_Variable_Utility.o \
                   netCDF_Attribute_Utility.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(SPCCOEFF_OBJ_FILES) \
            $(TAUCOEFF_OBJ_FILES) \
            $(AEROSOLCOEFF_OBJ_FILES) \
            $(SCATTERCOEFF_OBJ_FILES) \
            $(EMISCOEFF_OBJ_FILES) \
            $(CRTM_COEFF_OBJ_FILES) \
            $(CRTM_ATMOSPHERE_OBJ_FILES) \
            $(CRTM_SURFACE_OBJ_FILES) \
            $(CRTM_CHANNELINFO_OBJ_FILES) \
            $(CRTM_GEOMETRYINFO_OBJ_FILES) \
            $(CRTM_ATMABSORPTION_OBJ_FILES) \
            $(CRTM_OBJ_FILES) \
            $(CRTM_TLADMTEST_OBJ_FILES) \
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

create_links: create_source_links create_data_links

# -- The source code links
UTILITY_SOURCE_DIR = $(F90_DIR)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

SPCCOEFF_SOURCE_DIR = $(F90_DIR)/Instrument_Information/SpcCoeff
SPCCOEFF_SOURCE_FILES = ${SPCCOEFF_OBJ_FILES:.o=.f90}

TAUCOEFF_SOURCE_DIR = $(F90_DIR)/CRTM/Coefficients/TauCoeff
TAUCOEFF_SOURCE_FILES = ${TAUCOEFF_OBJ_FILES:.o=.f90}

AEROSOLCOEFF_SOURCE_DIR = $(F90_DIR)/CRTM/Coefficients/AerosolCoeff
AEROSOLCOEFF_SOURCE_FILES = ${AEROSOLCOEFF_OBJ_FILES:.o=.f90}

SCATTERCOEFF_SOURCE_DIR = $(F90_DIR)/CRTM/Coefficients/ScatterCoeff
SCATTERCOEFF_SOURCE_FILES = ${SCATTERCOEFF_OBJ_FILES:.o=.f90}

EMISCOEFF_SOURCE_DIR = $(F90_DIR)/CRTM/Coefficients/EmisCoeff/Spectral
EMISCOEFF_SOURCE_FILES = ${EMISCOEFF_OBJ_FILES:.o=.f90}

CRTM_COEFF_SOURCE_DIR = $(F90_DIR)/CRTM/Coefficients
CRTM_COEFF_SOURCE_FILES = ${CRTM_COEFF_OBJ_FILES:.o=.f90}

CRTM_ATMOSPHERE_SOURCE_DIR = $(F90_DIR)/CRTM/Atmosphere
CRTM_ATMOSPHERE_SOURCE_FILES = ${CRTM_ATMOSPHERE_OBJ_FILES:.o=.f90}

CRTM_CHANNELINFO_SOURCE_DIR = $(F90_DIR)/CRTM/ChannelInfo
CRTM_CHANNELINFO_SOURCE_FILES = ${CRTM_CHANNELINFO_OBJ_FILES:.o=.f90}

CRTM_GEOMETRYINFO_SOURCE_DIR = $(F90_DIR)/CRTM/GeometryInfo
CRTM_GEOMETRYINFO_SOURCE_FILES = ${CRTM_GEOMETRYINFO_OBJ_FILES:.o=.f90}

CRTM_ATMABSORPTION_SOURCE_DIR = $(F90_DIR)/CRTM/AtmAbsorption
CRTM_ATMABSORPTION_SOURCE_FILES = ${CRTM_ATMABSORPTION_OBJ_FILES:.o=.f90}

CRTM_SOURCE_DIR = $(F90_DIR)/CRTM
CRTM_SOURCE_FILES = ${CRTM_OBJ_FILES:.o=.f90}

CRTM_TLADMTEST_SOURCE_DIR = $(F90_DIR)/CRTM/Test_Code/Utility/TLADMtest
CRTM_TLADMTEST_SOURCE_FILES = ${CRTM_TLADMTEST_OBJ_FILES:.o=.f90}

NETCDF_SOURCE_DIR = $(F90_DIR)/NETCDF
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
	@for source_file in ${SCATTERCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SCATTERCOEFF_SOURCE_DIR}/$$source_file . ; \
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
	@for source_file in ${CRTM_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_TLADMTEST_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_TLADMTEST_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${NETCDF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${NETCDF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

# -- The data file links
SPCCOEFF_DATA_DIR = $(HOME)/CRTM/SpcCoeff/Current
TAUCOEFF_DATA_DIR = $(HOME)/CRTM/TauCoeff/Current
AEROSOLCOEFF_DATA_DIR = $(HOME)/CRTM/AerosolCoeff/Current
SCATTERCOEFF_DATA_DIR = $(HOME)/CRTM/ScatterCoeff/Current
EMISCOEFF_DATA_DIR = $(HOME)/CRTM/EmisCoeff/Current

DATA_FILE_TAGS = amsua_n17 \
                 hirs3_n17 \
                 ssmis_f16

DATA_ENDIAN_TAGS = Little_Endian

create_data_links:
	@for file_tag in ${DATA_FILE_TAGS}; do \
	  spccoeff_file=$$file_tag.SpcCoeff.bin ; \
	  for endian_tag in ${DATA_ENDIAN_TAGS}; do \
	    link_file=$$spccoeff_file.$$endian_tag ; \
	    if [ ! -f $$link_file ]; then \
	      $(LINK) ${SPCCOEFF_DATA_DIR}/$$endian_tag/$$spccoeff_file $$link_file ; \
	    fi ; \
	  done; \
	done
	@for file_tag in ${DATA_FILE_TAGS}; do \
	  taucoeff_file=$$file_tag.TauCoeff.bin ; \
	  for endian_tag in ${DATA_ENDIAN_TAGS}; do \
	    link_file=$$taucoeff_file.$$endian_tag ; \
	    if [ ! -f $$link_file ]; then \
	      $(LINK) ${TAUCOEFF_DATA_DIR}/$$endian_tag/$$taucoeff_file $$link_file ; \
	    fi ; \
	  done; \
	done
	@scattercoeff_file=ScatterCoeff.bin ; \
	for endian_tag in ${DATA_ENDIAN_TAGS}; do \
	  link_file=$$scattercoeff_file.$$endian_tag ; \
	  if [ ! -f $$link_file ]; then \
	    $(LINK) ${SCATTERCOEFF_DATA_DIR}/$$endian_tag/$$scattercoeff_file $$link_file ; \
	  fi ; \
	done
	@aerosolcoeff_file=AerosolCoeff.bin ; \
	for endian_tag in ${DATA_ENDIAN_TAGS}; do \
	  link_file=$$aerosolcoeff_file.$$endian_tag ; \
	  if [ ! -f $$link_file ]; then \
	    $(LINK) ${AEROSOLCOEFF_DATA_DIR}/$$endian_tag/$$aerosolcoeff_file $$link_file ; \
	  fi ; \
	done
	@emiscoeff_file=EmisCoeff.bin ; \
	for endian_tag in ${DATA_ENDIAN_TAGS}; do \
	  link_file=$$emiscoeff_file.$$endian_tag ; \
	  if [ ! -f $$link_file ]; then \
	    $(LINK) ${EMISCOEFF_DATA_DIR}/$$endian_tag/$$emiscoeff_file $$link_file ; \
	  fi ; \
	done


# ----------------
# Remove the links
# ----------------

remove_links: remove_source_links remove_data_links

# -- The source code links
ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(SPCCOEFF_SOURCE_FILES) \
                   $(TAUCOEFF_SOURCE_FILES) \
                   $(AEROSOLCOEFF_SOURCE_FILES) \
                   $(SCATTERCOEFF_SOURCE_FILES) \
                   $(EMISCOEFF_SOURCE_FILES) \
                   $(CRTM_COEFF_SOURCE_FILES) \
                   $(CRTM_ATMOSPHERE_SOURCE_FILES) \
                   $(CRTM_CHANNELINFO_SOURCE_FILES) \
                   $(CRTM_GEOMETRYINFO_SOURCE_FILES) \
                   $(CRTM_ATMABSORPTION_SOURCE_FILES) \
                   $(CRTM_SOURCE_FILES) \
                   $(CRTM_TLADMTEST_SOURCE_FILES) \
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
	@$(REMOVE) ScatterCoeff.bin*
	@$(REMOVE) AerosolCoeff.bin*
	@$(REMOVE) EmisCoeff.bin*


# ---------------
# Dependency list
# ---------------

Type_Kinds.o : Type_Kinds.f90
File_Utility.o : File_Utility.f90
Message_Handler.o : Message_Handler.f90 File_Utility.o
Endian_Utility.o : Endian_Utility.f90 Type_Kinds.o
Compare_Float_Numbers.o : Compare_Float_Numbers.f90 Type_Kinds.o
Binary_File_Utility.o : Binary_File_Utility.f90 Type_Kinds.o File_Utility.o Message_Handler.o Endian_Utility.o

TauCoeff_Define.o : TauCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o
TauCoeff_Binary_IO.o : TauCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o TauCoeff_Define.o

SpcCoeff_Define.o : SpcCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o
SpcCoeff_Binary_IO.o : SpcCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o SpcCoeff_Define.o

AerosolCoeff_Define.o : AerosolCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o
AerosolCoeff_Binary_IO.o : AerosolCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o AerosolCoeff_Define.o Binary_File_Utility.o

ScatterCoeff_Define.o : ScatterCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o
ScatterCoeff_Binary_IO.o : ScatterCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o ScatterCoeff_Define.o

EmisCoeff_Define.o : EmisCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o
EmisCoeff_Binary_IO.o : EmisCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o EmisCoeff_Define.o

CRTM_TauCoeff.o : CRTM_TauCoeff.f90 Type_Kinds.o Message_Handler.o TauCoeff_Define.o TauCoeff_Binary_IO.o CRTM_Parameters.o
CRTM_SpcCoeff.o : CRTM_SpcCoeff.f90 Type_Kinds.o Message_Handler.o SpcCoeff_Define.o SpcCoeff_Binary_IO.o CRTM_Parameters.o
CRTM_AerosolCoeff.o : CRTM_AerosolCoeff.f90 Type_Kinds.o Message_Handler.o AerosolCoeff_Define.o AerosolCoeff_Binary_IO.o CRTM_Parameters.o
CRTM_ScatterCoeff.o : CRTM_ScatterCoeff.f90 Type_Kinds.o Message_Handler.o ScatterCoeff_Define.o ScatterCoeff_Binary_IO.o CRTM_Parameters.o
CRTM_EmisCoeff.o : CRTM_EmisCoeff.f90 Type_Kinds.o Message_Handler.o EmisCoeff_Define.o EmisCoeff_Binary_IO.o

CRTM_Parameters.o : CRTM_Parameters.f90 Type_Kinds.o
CRTM_LifeCycle.o : CRTM_LifeCycle.f90 Message_Handler.o CRTM_SpcCoeff.o CRTM_TauCoeff.o CRTM_AerosolCoeff.o CRTM_ScatterCoeff.o CRTM_EmisCoeff.o CRTM_ChannelInfo.o
CRTM_GeometryInfo_Define.o : CRTM_GeometryInfo_Define.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o
CRTM_ChannelInfo_Define.o : CRTM_ChannelInfo_Define.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o
CRTM_ChannelInfo.o : CRTM_ChannelInfo.f90 Type_Kinds.o Message_Handler.o CRTM_ChannelInfo_Define.o

CRTM_Cloud_Define.o : CRTM_Cloud_Define.f90 Type_Kinds.o Message_Handler.o
CRTM_Cloud_Binary_IO.o : CRTM_Cloud_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o CRTM_Cloud_Define.o
CRTM_Aerosol_Define.o : CRTM_Aerosol_Define.f90 Type_Kinds.o Message_Handler.o
CRTM_Aerosol_Binary_IO.o : CRTM_Aerosol_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o CRTM_Aerosol_Define.o
CRTM_Atmosphere_Define.o : CRTM_Atmosphere_Define.f90 Type_Kinds.o Message_Handler.o CRTM_Cloud_Define.o CRTM_Aerosol_Define.o
CRTM_Atmosphere_Binary_IO.o : CRTM_Atmosphere_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o CRTM_Atmosphere_Define.o CRTM_Cloud_Binary_IO.o CRTM_Aerosol_Binary_IO.o

CRTM_AtmAbsorption_Predictor.o : CRTM_AtmAbsorption_Predictor.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_Atmosphere_Define.o CRTM_AtmAbsorption_Define.o
CRTM_AtmAbsorption_IntAbsorber.o : CRTM_AtmAbsorption_IntAbsorber.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_Atmosphere_Define.o
CRTM_AtmAbsorption_Define.o : CRTM_AtmAbsorption_Define.f90 Type_Kinds.o Message_Handler.o
CRTM_AtmAbsorption.o : CRTM_AtmAbsorption.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_TauCoeff.o CRTM_Atmosphere_Define.o CRTM_GeometryInfo_Define.o CRTM_AtmAbsorption_Define.o CRTM_AtmAbsorption_IntAbsorber.o CRTM_AtmAbsorption_Predictor.o

netCDF_Dimension_Utility.o : netCDF_Dimension_Utility.f90 Type_Kinds.o Message_Handler.o 
netCDF_Variable_Utility.o : netCDF_Variable_Utility.f90 Type_Kinds.o Message_Handler.o 
netCDF_Attribute_Utility.o : netCDF_Attribute_Utility.f90 Type_Kinds.o Message_Handler.o 
netCDF_Utility.o : netCDF_Utility.f90 Type_Kinds.o Message_Handler.o netCDF_Dimension_Utility.o netCDF_Variable_Utility.o netCDF_Attribute_Utility.o 

TLADMtest_Define.o : TLADMtest_Define.f90 Type_Kinds.o Message_Handler.o
TLADMtest_netCDF_IO.o : TLADMtest_netCDF_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o TLADMtest_Define.o netCDF_Utility.o

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_LifeCycle.o CRTM_GeometryInfo_Define.o CRTM_ChannelInfo_Define.o CRTM_Atmosphere_Define.o CRTM_Atmosphere_Binary_IO.o CRTM_AtmAbsorption.o TLADMtest_Define.o TLADMtest_netCDF_IO.o


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
	@for file_tag in ${DATA_FILE_TAGS}; do \
	  $(TARBALL_APPEND) $(TAR_FILE) $$file_tag.*.bin.*Endian ; \
	done
	compress $(TAR_FILE)



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(F90_DIR)/make.rules

