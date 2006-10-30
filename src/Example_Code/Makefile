#==============================================================================
#
# Master makefile for building, running, and tarballing the CRTM
# example test codes
#
#==============================================================================

# -------------
# Define macros
# -------------
include make.macros


# -----------
# Main target
# -----------
all: build run

# Build targets
build: build_test_forward \
       build_test_tangent_linear \
       build_test_adjoint \
       build_test_k_matrix

build_test_forward:
	cd Forward; make; cd ..

build_test_tangent_linear:
	cd Tangent_Linear; make; cd ..

build_test_adjoint:
	cd Adjoint; make; cd ..

build_test_k_matrix:
	cd K_Matrix; make; cd ..


# run targets
run: run_test_forward \
     run_test_tangent_linear \
     run_test_adjoint \
     run_test_k_matrix

run_test_forward: build_test_forward
	cd Forward; run_Test_Forward; cd ..

run_test_tangent_linear: build_test_tangent_linear
	cd Tangent_Linear; run_Test_Tangent_Linear; cd ..

run_test_adjoint: build_test_adjoint
	cd Adjoint; run_Test_Adjoint; cd ..

run_test_k_matrix: build_test_k_matrix
	cd K_Matrix; run_Test_K_Matrix; cd ..


# Clean targets
clean: clean_test_forward \
       clean_test_tangent_linear \
       clean_test_adjoint \
       clean_test_k_matrix

clean_test_forward:
	cd Forward; make clean; cd ..

clean_test_tangent_linear:
	cd Tangent_Linear; make clean; cd ..

clean_test_adjoint:
	cd Adjoint; make clean; cd ..

clean_test_k_matrix:
	cd K_Matrix; make clean; cd ..


# Tarball target
TAR_FILE = CRTM_Test_software.tar

tar:
	$(TARBALL_CREATE) $(TAR_FILE) make.macros make.rules make.common
	$(TARBALL_APPEND) $(TAR_FILE) Makefile
	$(TARBALL_APPEND) $(TAR_FILE) Forward Tangent_Linear Adjoint K_Matrix
	compress $(TAR_FILE)

