#==============================================================================
#
# Makefile for Create_AtmProfile program
#
# $Id: Makefile,v 1.10 2005/01/04 22:49:29 paulv Exp $
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

EXE_FILE = Create_AtmProfile


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
                    Fundamental_Constants.o
INTERPOLATE_OBJ_FILES = Interpolate.o
NETCDF_OBJ_FILES = netCDF_Utility.o \
                   netCDF_Dimension_Utility.o \
                   netCDF_Variable_Utility.o \
                   netCDF_Attribute_Utility.o
PROFILE_UTILITY_OBJ_FILES = Profile_Utility_Parameters.o \
                            Atmospheric_Properties.o \
                            Units_Conversion.o \
                            Geopotential.o
ATMPROFILE_OBJ_FILES = AtmProfile_Parameters.o \
                       AtmProfile_Define.o \
                       AtmProfile_netCDF_IO.o
UMBC_PROFILE_OBJ_FILES =  UMBC_Profile_Set.o
CIMSS_PROFILE_OBJ_FILES = CIMSS_Profile_Set.o
ECMWF_PROFILE_OBJ_FILES = ECMWF_Profile_Set.o
MODEL_PROFILE_OBJ_FILES = Model_Profile_Set.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(INTERPOLATE_OBJ_FILES) \
            $(NETCDF_OBJ_FILES) \
            $(PROFILE_UTILITY_OBJ_FILES) \
            $(ATMPROFILE_OBJ_FILES) \
            $(UMBC_PROFILE_OBJ_FILES) \
            $(CIMSS_PROFILE_OBJ_FILES) \
            $(ECMWF_PROFILE_OBJ_FILES) \
            $(MODEL_PROFILE_OBJ_FILES) \
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

INTERPOLATE_SOURCE_DIR = $(F90_DIR)/Interpolation
INTERPOLATE_SOURCE_FILES = ${INTERPOLATE_OBJ_FILES:.o=.f90}

NETCDF_SOURCE_DIR = $(F90_DIR)/NETCDF
NETCDF_SOURCE_FILES = ${NETCDF_OBJ_FILES:.o=.f90}

PROFILE_UTILITY_SOURCE_DIR = $(F90_DIR)/Profile_Utility
PROFILE_UTILITY_SOURCE_FILES = ${PROFILE_UTILITY_OBJ_FILES:.o=.f90}

ATMPROFILE_SOURCE_DIR = $(F90_DIR)/AtmProfile
ATMPROFILE_SOURCE_FILES = ${ATMPROFILE_OBJ_FILES:.o=.f90}

UMBC_PROFILE_SOURCE_DIR = $(F90_DIR)/AtmProfile/Create_AtmProfile/UMBC_Profile_Set
UMBC_PROFILE_SOURCE_FILES = ${UMBC_PROFILE_OBJ_FILES:.o=.f90}

CIMSS_PROFILE_SOURCE_DIR = $(F90_DIR)/AtmProfile/Create_AtmProfile/CIMSS_Profile_Set
CIMSS_PROFILE_SOURCE_FILES = ${CIMSS_PROFILE_OBJ_FILES:.o=.f90}

ECMWF_PROFILE_SOURCE_DIR = $(F90_DIR)/AtmProfile/Create_AtmProfile/ECMWF_Profile_Set
ECMWF_PROFILE_SOURCE_FILES = ${ECMWF_PROFILE_OBJ_FILES:.o=.f90}

MODEL_PROFILE_SOURCE_DIR = $(F90_DIR)/AtmProfile/Create_AtmProfile/Model_Profile_Set
MODEL_PROFILE_SOURCE_FILES = ${MODEL_PROFILE_OBJ_FILES:.o=.f90}


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
	@for source_file in ${ATMPROFILE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${ATMPROFILE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${UMBC_PROFILE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UMBC_PROFILE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CIMSS_PROFILE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CIMSS_PROFILE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${ECMWF_PROFILE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${ECMWF_PROFILE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${MODEL_PROFILE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${MODEL_PROFILE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(CONSTANTS_SOURCE_FILES) \
                   $(INTERPOLATE_SOURCE_FILES) \
                   $(NETCDF_SOURCE_FILES) \
                   $(PROFILE_UTILITY_SOURCE_FILES) \
                   $(ATMPROFILE_SOURCE_FILES) \
                   $(UMBC_PROFILE_SOURCE_FILES) \
                   $(CIMSS_PROFILE_SOURCE_FILES) \
                   $(ECMWF_PROFILE_SOURCE_FILES) \
                   $(MODEL_PROFILE_SOURCE_FILES)
                   
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
Fundamental_Constants.o : Fundamental_Constants.f90 Type_Kinds.o 

Interpolate.o : Interpolate.f90 Error_Handler.o Type_Kinds.o 

netCDF_Dimension_Utility.o : netCDF_Dimension_Utility.f90 Error_Handler.o Type_Kinds.o 
netCDF_Variable_Utility.o : netCDF_Variable_Utility.f90 Error_Handler.o Type_Kinds.o 
netCDF_Attribute_Utility.o : netCDF_Attribute_Utility.f90 Error_Handler.o Type_Kinds.o 
netCDF_Utility.o : netCDF_Utility.f90 netCDF_Variable_Utility.o netCDF_Dimension_Utility.o netCDF_Attribute_Utility.o Error_Handler.o Type_Kinds.o 

AtmProfile_Parameters.o : AtmProfile_Parameters.f90 Type_Kinds.o 
AtmProfile_Define.o : AtmProfile_Define.f90 Error_Handler.o Type_Kinds.o Compare_Float_Numbers.o
AtmProfile_netCDF_IO.o : AtmProfile_netCDF_IO.f90 netCDF_Utility.o AtmProfile_Define.o Error_Handler.o Type_Kinds.o 

UMBC_Profile_Set.o : UMBC_Profile_Set.f90 Error_Handler.o File_Utility.o Profile_Utility_Parameters.o Units_Conversion.o
CIMSS_Profile_Set.o : CIMSS_Profile_Set.f90 Error_Handler.o Type_Kinds.o 
ECMWF_Profile_Set.o : ECMWF_Profile_Set.f90 Error_Handler.o File_Utility.o Type_Kinds.o 
Model_Profile_Set.o : Model_Profile_Set.f90 Error_Handler.o File_Utility.o Profile_Utility_Parameters.o Units_Conversion.o

Profile_Utility_Parameters.o : Profile_Utility_Parameters.f90 Fundamental_Constants.o Type_Kinds.o 
Atmospheric_Properties.o : Atmospheric_Properties.f90 Profile_Utility_Parameters.o Error_Handler.o Type_Kinds.o 
Units_Conversion.o : Units_Conversion.f90 Atmospheric_Properties.o Profile_Utility_Parameters.o Error_Handler.o Type_Kinds.o 
Geopotential.o : Geopotential.f90 Type_Kinds.o Error_Handler.o Fundamental_Constants.o Atmospheric_Properties.o

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o File_Utility.o Error_Handler.o Interpolate.o AtmProfile_Define.o AtmProfile_netCDF_IO.o AtmProfile_Parameters.o UMBC_Profile_Set.o CIMSS_Profile_Set.o ECMWF_Profile_Set.o Model_Profile_Set.o Units_Conversion.o Geopotential.o Profile_Utility_Parameters.o 


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

