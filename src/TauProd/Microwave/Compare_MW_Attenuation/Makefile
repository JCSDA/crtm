#==============================================================================
#
# Makefile for Compare_MW_Attenuation program
#
# $Id: Makefile,v 1.2 2006/06/30 16:47:16 dgroff Exp $
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

EXE_FILE = Compare_MW_Attenuation


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Message_Handler.o \
                    Compare_Float_Numbers.o \
                    Fundamental_Constants.o
NETCDF_OBJ_FILES = netCDF_Utility.o \
                   netCDF_Dimension_Utility.o \
                   netCDF_Variable_Utility.o \
                   netCDF_Attribute_Utility.o
PROFILE_UTILITY_OBJ_FILES = Profile_Utility_Parameters.o \
                            Geopotential.o \
                            Units_Conversion.o \
                            Atmospheric_Properties.o
SPECTRA_OBJ_FILES = Spectra_Define.o \
                    Spectra_netCDF_IO.o
ATMPROFILE_OBJ_FILES = AtmProfile_Define.o \
                       AtmProfile_netCDF_IO.o
MWLBL_OBJ_FILES = Liebe89_Coefficients.o \
                  Liebe92_Coefficients.o \
                  Liebe93_Coefficients.o \
                  Rosenkranz03_Coefficients.o \
                  MWLBL_Liebe89.o \
                  MWLBL_Liebe93.o \
                  MWLBL_Rosenkranz03.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(NETCDF_OBJ_FILES) \
            $(PROFILE_UTILITY_OBJ_FILES) \
            $(SPECTRA_OBJ_FILES) \
            $(ATMPROFILE_OBJ_FILES) \
            $(MWLBL_OBJ_FILES) \
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

PROFILE_UTILITY_SOURCE_DIR = $(F90_DIR)/Profile_Utility
PROFILE_UTILITY_SOURCE_FILES = ${PROFILE_UTILITY_OBJ_FILES:.o=.f90}

SPECTRA_SOURCE_DIR = $(F90_DIR)/Spectral
SPECTRA_SOURCE_FILES = ${SPECTRA_OBJ_FILES:.o=.f90}

ATMPROFILE_SOURCE_DIR = $(F90_DIR)/AtmProfile
ATMPROFILE_SOURCE_FILES = ${ATMPROFILE_OBJ_FILES:.o=.f90}

MWLBL_SOURCE_DIR = $(F90_DIR)/LBL/Microwave
MWLBL_SOURCE_FILES = ${MWLBL_OBJ_FILES:.o=.f90}

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
	@for source_file in ${PROFILE_UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${PROFILE_UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${SPECTRA_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SPECTRA_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${ATMPROFILE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${ATMPROFILE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${MWLBL_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${MWLBL_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(NETCDF_SOURCE_FILES) \
                   ${PROFILE_UTILITY_SOURCE_FILES} \
                   ${SPECTRA_SOURCE_FILES} \
                   ${ATMPROFILE_SOURCE_FILES} \
                   $(MWLBL_SOURCE_FILES)
                   
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
Fundamental_Constants.o : Fundamental_Constants.f90 Type_Kinds.o 

netCDF_Variable_Utility.o : netCDF_Variable_Utility.f90 Type_Kinds.o Message_Handler.o 
netCDF_Dimension_Utility.o : netCDF_Dimension_Utility.f90 Type_Kinds.o Message_Handler.o 
netCDF_Attribute_Utility.o : netCDF_Attribute_Utility.f90 Type_Kinds.o Message_Handler.o 
netCDF_Utility.o : netCDF_Utility.f90 Type_Kinds.o Message_Handler.o netCDF_Dimension_Utility.o netCDF_Variable_Utility.o netCDF_Attribute_Utility.o 

Profile_Utility_Parameters.o : Profile_Utility_Parameters.f90 Type_Kinds.o Fundamental_Constants.o 
Units_Conversion.o : Units_Conversion.f90 Type_Kinds.o Message_Handler.o Profile_Utility_Parameters.o Atmospheric_Properties.o 
Atmospheric_Properties.o : Atmospheric_Properties.f90 Type_Kinds.o Message_Handler.o Profile_Utility_Parameters.o 
Geopotential.o : Geopotential.f90 Type_Kinds.o Message_Handler.o Fundamental_Constants.o Atmospheric_Properties.o 

MWLBL_Liebe93.o : MWLBL_Liebe93.f90 Type_Kinds.o Message_Handler.o Liebe92_Coefficients.o 
MWLBL_Liebe89.o : MWLBL_Liebe89.f90 Type_Kinds.o Message_Handler.o Liebe89_Coefficients.o 
MWLBL_Rosenkranz03.o : MWLBL_Rosenkranz03.f90 Type_Kinds.o Message_Handler.o Fundamental_Constants.o Units_Conversion.o Rosenkranz03_Coefficients.o 

Liebe93_Coefficients.o : Liebe93_Coefficients.f90 Type_Kinds.o 
Liebe92_Coefficients.o : Liebe92_Coefficients.f90 Type_Kinds.o 
Liebe89_Coefficients.o : Liebe89_Coefficients.f90 Type_Kinds.o 
Rosenkranz03_Coefficients.o : Rosenkranz03_Coefficients.f90 Type_Kinds.o 

Spectra_Define.o : Spectra_Define.f90 Type_Kinds.o Message_Handler.o
Spectra_netCDF_IO.o : Spectra_netCDF_IO.f90 Type_Kinds.o Message_Handler.o Spectra_Define.o netCDF_Utility.o 

AtmProfile_Define.o : AtmProfile_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o
AtmProfile_netCDF_IO.o : AtmProfile_netCDF_IO.f90 Type_Kinds.o Message_Handler.o AtmProfile_Define.o netCDF_Utility.o 

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o Message_Handler.o Profile_Utility_Parameters.o Units_Conversion.o AtmProfile_Define.o AtmProfile_netCDF_IO.o Spectra_Define.o Spectra_netCDF_IO.o MWLBL_Liebe89.o MWLBL_Liebe93.o MWLBL_Rosenkranz03.o



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
	$(TARBALL_APPEND) $(TAR_FILE) $(SENSORINFO_FILE)
	compress $(TAR_FILE)



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(F90_DIR)/make.rules

