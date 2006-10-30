#==============================================================================
#
# Makefile for SRF_ASCII_IO_Test program
#
# $Id: Makefile,v 1.7 2006/05/02 16:58:02 dgroff Exp $
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

EXE_FILE = SRF_ASCII_IO_Test


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Message_Handler.o \
                    String_Utility.o \
                    Compare_Float_Numbers.o
INTERPOLATE_OBJ_FILES = Interpolate.o
INTEGRATE_OBJ_FILES = Integrate.o
SRF_OBJ_FILES = SRF_Define.o \
                SRF_ASCII_IO.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(INTERPOLATE_OBJ_FILES) \
            $(INTEGRATE_OBJ_FILES) \
            $(SRF_OBJ_FILES) \
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

SRF_SOURCE_DIR = $(F90_DIR)/Instrument_Information/SRF
SRF_SOURCE_FILES = ${SRF_OBJ_FILES:.o=.f90}

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
	@for source_file in ${SRF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SRF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(INTERPOLATE_SOURCE_FILES) \
                   $(INTEGRATE_SOURCE_FILES) \
                   $(SRF_SOURCE_FILES)
                   
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
String_Utility.o : String_Utility.f90 
Compare_Float_Numbers.o : Compare_Float_Numbers.f90 Type_Kinds.o 

Interpolate.o : Interpolate.f90 Type_Kinds.o Message_Handler.o 
Integrate.o : Integrate.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o Interpolate.o

SRF_Define.o : SRF_Define.f90 Type_Kinds.o Message_Handler.o Integrate.o 
SRF_ASCII_IO.o : SRF_ASCII_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o String_Utility.o SRF_Define.o 

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o Message_Handler.o SRF_Define.o SRF_ASCII_IO.o 


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

