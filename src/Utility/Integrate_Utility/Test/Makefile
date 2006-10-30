#==============================================================================
#
# Makefile for Test_Integrate_Utility program
#
# $Id: Makefile,v 1.1 2006/06/08 19:46:40 wd20pd Exp $
#
#==============================================================================

# -------------
# Define macros
# -------------

include $(REPOSITORY_SOURCE_ROOT)/make.macros

# This makefile
MAKEFILE = Makefile

# Executable files
EXE_FILE = Test_Integrate_Utility

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Interpolate_Utility.f90 \
            Integrate_Utility.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o


# ------------------
# Define dependecies
# ------------------
# Make based on OS type
all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")  make -f $(MAKEFILE) build $(SUNOS_FLAGS) ;; \
	  "AIX")    make -f $(MAKEFILE) build $(AIX_FLAGS) ;; \
	  "IRIX64") make -f $(MAKEFILE) build $(IRIX64_FLAGS) ;; \
	  "Linux")  make -f $(MAKEFILE) build $(LINUX_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
	esac

# Build the test program
build: create_links $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(FL_FLAGS) $(EXE_FILE)

# Clean up
clean:
	$(REMOVE) $(OBJ_FILES) $(EXE_FILE) *.mod *.MOD *.stb $(EXE_FILE).test

realclean: remove_links clean

# Run the test
test: all
	@$(EXE_FILE) | tee $(EXE_FILE).test
	@nl=`diff $(EXE_FILE).output $(EXE_FILE).test | wc -l`; \
	if [ $$nl -eq 0 ]; then \
	  echo; echo "  Test successful!"; \
	else \
	  echo; echo "  Test run results different!"; \
	  diff $(EXE_FILE).output $(EXE_FILE).test; \
	fi; exit 0


# Source file link creation and removal
create_links:
	@linkfiles $(REPOSITORY_SOURCE_ROOT) $(SRC_FILES)

remove_links:
	@unlinkfiles $(SRC_FILES)

# Source dependency list
Type_Kinds.o : Type_Kinds.f90 
File_Utility.o : File_Utility.f90 
Message_Handler.o : Message_Handler.f90 File_Utility.o 
Compare_Float_Numbers.o : Compare_Float_Numbers.f90 Type_Kinds.o
Interpolate_Utility.o : Interpolate_Utility.f90 Type_Kinds.o Message_Handler.o 
Integrate_Utility.o : Integrate_Utility.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o Interpolate_Utility.o
$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o Message_Handler.o File_Utility.o Integrate_Utility.o 


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
	$(TARBALL_APPEND) $(TAR_FILE) $(EXE_FILE).output
	@compress $(TAR_FILE)


# --------------------
# Define default rules
# --------------------
include $(REPOSITORY_SOURCE_ROOT)/make.rules
