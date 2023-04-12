<h1> pfft Library </h1>

Copyright UCAR JCSDA, 2021-2022

This version of the pfft library is part of the JCSDA Community Radiative Transfer Model (CRTM).

<h2> Description </h2>

The *pfft* routines are the updated `.f90` Fast-Fourier-Transform
routines for real and complex transforms in lines, x-array format
and in y-array format. They relace the nfft suite, which pre-dates
the availability of the useful `.f90` features now included. 

- `cfft`, `dfft`: complex data transform and its inverse
- `rfft`, `hfft`: real data transform and its inverse

<h2> Build Process </h2>

<h3> Dependencies </h3>

A Fortran 90 compiler is required.

<h3> Compilation </h3>

Enter the directory `pfft/Build/` and type `make`.

<h3> Installation </h3>

From the same directory, type `make install`. This creates the `include/` and `lib/` directories in the same location.

<h3> Testing </h3>

The following tests are included:

- test2:      Examples showing idioms for Fourier differentiation and integration
- test3:      Example showing the idiom needed to convolve a pair of 1D arrays
- test4:      Example showing how to use slow fourier evaluation, hsfe, (real)
- test5:      Example, like test4, but use complex slow evaluation routines

<h2> Applications </h2>

This library is required for processing IR Fourier spectrometer instruments such as IASI and CrIS within the CRTM.
This includes the apodization and non-LTE treatment of these sensors.


