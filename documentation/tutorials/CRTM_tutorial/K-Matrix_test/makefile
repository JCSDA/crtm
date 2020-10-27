all:
	gfortran -c -I/usr/local/crtm_v2.3.0/include KMatrix_test.f90
	gfortran -L/usr/local/crtm_v2.3.0/lib -lCRTM -o K.x KMatrix_test.o

clean:
	@rm *.o *.mod
