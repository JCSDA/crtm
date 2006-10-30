#==============================================================================
#
# Makefile for LBLRTM output spectral averaging program
#
# $Id: Makefile,v 1.4 2000/06/30 15:55:33 paulv Exp $
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
            netcdf_utility.o \
            lblrtm_utility.o \
            spectral.o \
            average_tape13_tau.o


# ----------------
# Executable files
# ----------------

EXE_FILE = average_tape13_tau



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
	  "SunOS")    make -f $(MAKE_FILE) spectral_average $(SUNOS_FLAGS) ;; \
	   "AIX")     make -f $(MAKE_FILE) spectral_average $(AIX_FLAGS) ;; \
	   "IRIX64" ) make -f $(MAKE_FILE) spectral_average $(IRIX64_FLAGS) ;; \
	   "Linux" )  make -f $(MAKE_FILE) spectral_average $(Linux_FLAGS_PGI) ;; \
	   *) echo "This system is not supported" ;; \
       esac



# ----------------
# Make the program
# ----------------

spectral_average: $(OBJ_FILES)
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
	ln -sf $(HOME)/f90/General/type_kinds.f90 .
	ln -sf $(HOME)/f90/General/get_lun.f90 .
	ln -sf $(HOME)/f90/Utility/error_handler.f90 .
	ln -sf $(HOME)/f90/Readers/netcdf_utility.f90 .
	ln -sf $(HOME)/f90/LBLRTM/lblrtm_utility.f90 .
	ln -sf $(HOME)/f90/Spectral/spectral.f90 .

remove_links:
	rm type_kinds.f90 > /dev/null
	rm get_lun.f90 > /dev/null
	rm error_handler.f90 > /dev/null
	rm netcdf_utility.f90 > /dev/null
	rm lblrtm_utility.f90 > /dev/null
	rm spectral.f90 > /dev/null


# -------------------------------
# Create tarball for distribution
# -------------------------------

TAR_FILE = average_tape13_tau_software.tar

tar:
	tar cvhf $(TAR_FILE) ${OBJ_FILES:.o=.f90}
	tar rvhf $(TAR_FILE) Makefile
	compress $(TAR_FILE)



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(HOME)/f90/make.rules

