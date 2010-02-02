#==============================================================================
#
# Makefile for CRTM K-Matrix model example programs
#
#==============================================================================

# Added CRTM executable names to this list.
# Naming convention is:
#   Example<N>_<DESCRIPTION>
# where N increments with each addition, and
#       DESCRIPTION is a short description of the program.
EXE_FILES = Example1_Simple \
            Example2_SSU \
            Example3_Zeeman \
            Example4_ODPS


# Target definitions
# ------------------
# The main target
all:
	@for EXE_FILE in $(EXE_FILES); do \
	  make -f makefile.single "EXE_FILE=$$EXE_FILE"; \
	done

# A clean target
clean:
	@for EXE_FILE in $(EXE_FILES); do \
	  make -f makefile.single clean "EXE_FILE=$$EXE_FILE"; \
	done
