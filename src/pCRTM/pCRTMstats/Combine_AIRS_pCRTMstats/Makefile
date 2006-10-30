#==============================================================================
#
# Makefile for Combine_AIRS_RTMstats program
#
# $Id: Makefile,v 1.1 2004/02/12 19:57:15 paulv Exp $
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

EXE_FILE = Combine_AIRS_RTMstats


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = type_kinds.o \
                    file_utility.o \
                    error_handler.o
NETCDF_OBJ_FILES = netCDF_Utility.o \
                   netCDF_Dimension_Utility.o \
                   netCDF_Variable_Utility.o \
                   netCDF_Attribute_Utility.o
AIRS_OBJ_FILES = AIRS_Define.o
RTMSTATS_OBJ_FILES = RTMstats_Define.o \
                     RTMstats_netCDF_IO.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(NETCDF_OBJ_FILES) \
            $(AIRS_OBJ_FILES) \
            $(RTMSTATS_OBJ_FILES) \
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

AIRS_SOURCE_DIR = $(F90_DIR)/AIRS
AIRS_SOURCE_FILES = ${AIRS_OBJ_FILES:.o=.f90}

RTMSTATS_SOURCE_DIR = $(F90_DIR)/NCEP_RTM/RTMstats
RTMSTATS_SOURCE_FILES = ${RTMSTATS_OBJ_FILES:.o=.f90}

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
	@for source_file in ${AIRS_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${AIRS_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${RTMSTATS_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${RTMSTATS_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(NETCDF_SOURCE_FILES) \
                   $(AIRS_SOURCE_FILES) \
                   $(RTMSTATS_SOURCE_FILES)
                   
remove_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done



# ---------------
# Dependency list
# ---------------

type_kinds.o : type_kinds.f90 
file_utility.o : file_utility.f90 
error_handler.o : error_handler.f90 file_utility.o 

netCDF_Dimension_Utility.o : netCDF_Dimension_Utility.f90 error_handler.o type_kinds.o 
netCDF_Variable_Utility.o : netCDF_Variable_Utility.f90 error_handler.o type_kinds.o 
netCDF_Attribute_Utility.o : netCDF_Attribute_Utility.f90 error_handler.o type_kinds.o 
netCDF_Utility.o : netCDF_Utility.f90 netCDF_Attribute_Utility.o netCDF_Variable_Utility.o netCDF_Dimension_Utility.o error_handler.o type_kinds.o 

AIRS_Define.o : AIRS_Define.f90 type_kinds.o error_handler.o 

RTMstats_Define.o : RTMstats_Define.f90 type_kinds.o file_utility.o error_handler.o
RTMstats_netCDF_IO.o : RTMstats_netCDF_IO.f90 type_kinds.o error_handler.o RTMstats_Define.o netCDF_Utility.o 

$(EXE_FILE).o : $(EXE_FILE).f90 type_kinds.o error_handler.o AIRS_Define.o RTMstats_Define.o RTMstats_netCDF_IO.o 


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

