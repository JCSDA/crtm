#==============================================================================
#
# Makefile for Test_Interpolate_Utility program
#
# $Id: Makefile,v 1.2 2006/06/07 20:17:21 wd20pd Exp $
#
#==============================================================================

# -------------
# Define macros
# -------------

include $(REPOSITORY_SOURCE_ROOT)/make.macros

# This makefile
MAKEFILE = Makefile

# Executable files
EXE_FILE = Test_Interpolate_Utility

# Object files
UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Message_Handler.o
INTERPOLATE_UTILITY_OBJ_FILES = Interpolate_Utility.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(INTERPOLATE_UTILITY_OBJ_FILES) \
            $(EXE_FILE).o


# ------------------
# Define dependecies
# ------------------
# Make based on OS type
all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")   make -f $(MAKEFILE) build $(SUNOS_FLAGS) ;; \
	  "AIX")     make -f $(MAKEFILE) build $(AIX_FLAGS) ;; \
	  "IRIX64" ) make -f $(MAKEFILE) build $(IRIX64_FLAGS) ;; \
	  "Linux" )  make -f $(MAKEFILE) build $(LINUX_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
       esac

# Build the test program
build: create_links $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(FL_FLAGS) $(EXE_FILE)

# Clean up
clean:
	$(REMOVE) $(OBJ_FILES) $(EXE_FILE) *.mod *.MOD *.stb *.dat $(EXE_FILE).test

realclean: remove_links clean

# Run the test
test: all
	@$(EXE_FILE) | tee $(EXE_FILE).test
	@nl=`diff $(EXE_FILE).output $(EXE_FILE).test | wc -l`; \
	if [ $$nl -eq 0 ]; then \
	  echo; echo "  Test successful!"; \
	else \
	  echo; echo "  Test run results different!"; \
	  diff $(EXE_FILE).output $(EXE_FILE).test ; \
	fi

# Create links for compilation
UTILITY_SOURCE_DIR = $(REPOSITORY_SOURCE_ROOT)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}
INTERPOLATE_UTILITY_SOURCE_DIR = $(REPOSITORY_SOURCE_ROOT)/Utility/Interpolate_Utility
INTERPOLATE_UTILITY_SOURCE_FILES = ${INTERPOLATE_UTILITY_OBJ_FILES:.o=.f90}

create_links:
	@for source_file in ${UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${INTERPOLATE_UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${INTERPOLATE_UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(INTERPOLATE_UTILITY_SOURCE_FILES)

remove_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done

# Source dependency list
Type_Kinds.o : Type_Kinds.f90 
File_Utility.o : File_Utility.f90 
Message_Handler.o : Message_Handler.f90 File_Utility.o 
Interpolate_Utility.o : Interpolate_Utility.f90 Type_Kinds.o Message_Handler.o 
$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o Message_Handler.o File_Utility.o Interpolate_Utility.o 


# -------------------------------
# Create tarball for distribution
# -------------------------------
TAR_FILE = $(EXE_FILE)_software.tar
tar: create_links
	$(TARBALL_CREATE) $(TAR_FILE) ${OBJ_FILES:.o=.f90}
	@$(COPY) makefile.dist makefile
	$(TARBALL_APPEND) $(TAR_FILE) makefile
	@$(REMOVE) makefile
	@$(COPY) $(REPOSITORY_SOURCE_ROOT)/make.macros .
	@$(COPY) $(REPOSITORY_SOURCE_ROOT)/make.rules .
	$(TARBALL_APPEND) $(TAR_FILE) make.macros make.rules
	@$(REMOVE) make.macros make.rules



# --------------------
# Define default rules
# --------------------
include $(REPOSITORY_SOURCE_ROOT)/make.rules

