#==============================================================================
#
# Makefile for Test_GeometryInfo program
#
# $Id: Makefile,v 1.2 2006/05/02 14:58:34 dgroff Exp $
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

EXE_FILE = Test_GeometryInfo


# -------------
# This makefile
# -------------

MAKEFILE = Makefile


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Message_Handler.o
CRTM_OBJ_FILES = CRTM_Parameters.o
CRTM_GEOMETRYINFO_OBJ_FILES = CRTM_GeometryInfo_Define.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(CRTM_OBJ_FILES) \
            $(CRTM_GEOMETRYINFO_OBJ_FILES) \
            $(EXE_FILE).o



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



# ----------------------------
# Create links for compilation
# ----------------------------

# -- Source code linking

UTILITY_SOURCE_DIR = $(F90_DIR)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

CRTM_SOURCE_DIR = $(F90_DIR)/CRTM
CRTM_SOURCE_FILES = ${CRTM_OBJ_FILES:.o=.f90}

CRTM_GEOMETRYINFO_SOURCE_DIR = $(F90_DIR)/CRTM/GeometryInfo
CRTM_GEOMETRYINFO_SOURCE_FILES = ${CRTM_GEOMETRYINFO_OBJ_FILES:.o=.f90}


create_links:
	@for source_file in ${UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_GEOMETRYINFO_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_GEOMETRYINFO_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done


# -- Source code removal

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(CRTM_SOURCE_FILES) \
                   $(CRTM_GEOMETRYINFO_SOURCE_FILES)
                   
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

CRTM_Parameters.o : CRTM_Parameters.f90 Type_Kinds.o
CRTM_GeometryInfo_Define.o : CRTM_GeometryInfo_Define.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o Message_Handler.o CRTM_GeometryInfo_Define.o


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
	compress -f $(TAR_FILE)



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(F90_DIR)/make.rules

