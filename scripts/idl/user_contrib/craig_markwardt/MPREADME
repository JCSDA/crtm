
MPFIT PACKAGE
MARKWARDT IDL PROGRAMS

Craig Markwardt
craigm@cow.physics.wisc.edu
31 Jan 2000

The following instructions apply to the MPFIT and TNMIN packages of
functions for curve fitting under IDL, available from the Markwardt
IDL Library.

MPFIT is a set of routines for robust least-squares minimization
(curve fitting), using arbitrary user written IDL functions or
procedures.  MPFIT is based on the well-known and tested MINPACK-1
FORTRAN package of routines available at www.netlib.org.  The relevant
sections of code have been translated almost directly from the FORTRAN
equivalent.

MPFIT functions are designed for consistent usage and have some
special features not found in the standard IDL routines.  MPFIT
functions permit you to fix any function parameters, as well as to set
simple upper and lower parameter bounds.  See the documentation under
PARINFO for instructions on how to use this facility.

TNMIN solves general minimization problems (generally not curve
fitting), and has similar features to the MPFIT family of functions.
As of this writing, TNMIN requires the user function to supply
derivatives.


DOWNLOADING

Download new versions of from Craig Markwardt's web page:

http://cow.physics.wisc.edu/~craigm/idl/idl.html

Program modification dates appear on the web page, which you can
compare agains your own copy.  You can also check the modification
history of the file itself to see how recent it is.

Please see the file INSTALL for installation instructions.


MANIFEST

The following functions are included:

 INSTALL     - installation instructions
 MPREADME    - this file

 MPFIT       - main fitting engine, required for other driver functions
 MPFITFUN    - driver function for 1D function fitting
 MPFIT2DFUN  - driver function for 2D function fitting (images)
 MPCURVEFIT  - drop-in replacement for IDL's CURVEFIT, requires MPFIT
 MPFITEXPR   - driver function for fitting expressions interactively
 MPFITPEAK   - driver function for fitting Gaussian, Lorentzian or Moffat peaks
 MPFIT2DPEAK - driver function for fitting 2D peaks

 GAUSS1      - example 1D gaussian function
 GAUSS1P     - example 2D gaussian *procedure*
 fakedata.sav - example 1D gaussian data 


USAGE

The general theory of curve fitting is beyond the scope of this
document.  However, I can offer you a few suggestions.  First, read
the fitting tutorial found on my web page here:

http://cow.physics.wisc.edu/~craigm/idl/idl.html

(and click through to the Fitting section).  This should get you
started on the basics of 1D fitting, and in fact 2D fitting too since
the principles are almost the same.

Second, read the documentation!  Each of the program files is
extensively self-documented in their comment headers.  You can either
read the files directly or download the documentation from my web page
in the documentation section.

Finally, don't be afraid to experiment.


RECOMMENDATIONS

There are a lot of fitting functions available, each one optimized for
a specific task.  Allow me to suggest which one to use:

 * For 1D curve fitting, use MPFITFUN;
 * For 2D surface fitting, use MPFIT2DFUN;
 * For existing programs which already use CURVEFIT, use MPCURVEFIT;
 * For general non-linear minimization problems, use TNMIN

The main engine, MPFIT, is required in all cases, since the driver
functions call it.

IDL is a registered trademark of RSI

