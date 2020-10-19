all:
	gfortran -c -I/usr/local/crtm_v2.3.0/include Example_NakajimaKing.f90
	gfortran -L/usr/local/crtm_v2.3.0/lib -lCRTM -o NK.x Example_NakajimaKing.o

clean:
	@rm *.o *.mod
