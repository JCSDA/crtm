#==============================================================================
#
# Makefile for dependent set TAPE5 creation program
#
# $Id: Makefile,v 1.2 2000/07/14 17:32:13 paulv Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

include $(HOME)/f90/make.macros


# -------------
# This makefile
# -------------

MAKE_FILE = Makefile


# ------------
# Object files
# ------------


OBJ_FILES = type_kinds.o \
            get_lun.o \
            error_handler.o \
            fundamental_constants.o \
            meteorology.o \
            dependent_profile_set.o \
            lblrtm_utility.o \
            create_dependent_set_tape5.o


# ----------------
# Executable files
# ----------------

EXE_FILE = create_dependent_set_tape5



#-----------------------------------------------------------------------------
#                          -- Define dependecies --
#-----------------------------------------------------------------------------

# ---------------------
# Make based on OS type
# ---------------------


# Make based on OS type
all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")    make -f $(MAKE_FILE) create_tape5 $(SUNOS_FLAGS) ;; \
	   "AIX")     make -f $(MAKE_FILE) create_tape5 $(AIX_FLAGS) ;; \
	   "IRIX64" ) make -f $(MAKE_FILE) create_tape5 $(IRIX64_FLAGS) ;; \
	   "Linux" )  make -f $(MAKE_FILE) create_tape5 $(Linux_FLAGS_PGI) ;; \
	   *) echo "This system is not supported" ;; \
       esac



# -------------------------------
# Make the TAPE5 creation program
# -------------------------------

create_tape5: $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(FL_FLAGS) $(EXE_FILE)


# --------
# Clean up
# --------

clean:
	$(REMOVE) $(OBJ_FILES) $(EXE_FILE)
	$(REMOVE) *.mod *.MOD



# --------------------------------
# Create the links for compilation
# (This is a kludge for now...)
# --------------------------------

create_links:
	ln -sf $(HOME)/f90/General/type_kinds.f90
	ln -sf $(HOME)/f90/General/get_lun.f90
	ln -sf $(HOME)/f90/Utility/error_handler.f90
	ln -sf $(HOME)/f90/Constants/fundamental_constants.f90
	ln -sf $(HOME)/f90/Meteorology/meteorology.f90
	ln -sf $(HOME)/f90/Readers/dependent_profile_set.f90
	ln -sf $(HOME)/f90/LBLRTM/lblrtm_utility.f90

remove_links:
	$(REMOVE) type_kinds.f90
	$(REMOVE) get_lun.f90
	$(REMOVE) error_handler.f90
	$(REMOVE) fundamental_constants.f90
	$(REMOVE) meteorology.f90
	$(REMOVE) dependent_profile_set.f90
	$(REMOVE) lblrtm_utility.f90


# -------------------------------
# Create tarball for distribution
# -------------------------------

TAR_FILE = create_dependent_set_tape5_software.tar

tar:
	tar cvhf $(TAR_FILE) ${OBJ_FILES:.o=.f90}
	tar rvhf $(TAR_FILE) Makefile
	tar rvhf $(TAR_FILE) dependent_profile_set.nc
	compress $(TAR_FILE)



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(HOME)/f90/make.rules

