;+
; NAME:
;   LINFITEX
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Model function for fitting line with errors in X and Y
;
; MAJOR TOPICS:
;   Curve and Surface Fitting
;
; CALLING SEQUENCE:
;   parms = MPFIT('LINFITEX', start_parms, $
;             FUNCTARGS={X: X, Y: Y, SIGMA_X: SIGMA_X, SIGMA_Y: SIGMA_Y}, $
;             ...)
;
; DESCRIPTION:
;
;  LINFITEX is a model function to be used with MPFIT in order to 
;  fit a line to data with errors in both "X" and "Y" directions.  
;  LINFITEX follows the methodology of Numerical Recipes, in the
;  section entitled, "Straight-Line Data with Errors in Both
;  Coordinates."
;
;  The user is not meant to call LINFITEX directly.  Rather, the
;  should pass LINFITEX as a user function to MPFIT, and MPFIT will in
;  turn call LINFITEX.
;
;  Each data point will have an X and Y position, as well as an error
;  in X and Y, denoted SIGMA_X and SIGMA_Y.  The user should pass
;  these values using the FUNCTARGS convention, as shown above.  I.e.
;  the FUNCTARGS keyword should be set to a single structure
;  containing the fields "X", "Y", "SIGMA_X" and "SIGMA_Y".  Each
;  field should have a vector of the same length.
;
;  Upon return from MPFIT, the best fit parameters will be,
;      P[0] - Y-intercept of line on the X=0 axis.
;      P[1] - slope of the line
;
;  NOTE that LINFITEX requires that AUTODERIVATIVE=1, i.e. MPFIT
;  should compute the derivatives associated with each parameter
;  numerically.
;
; INPUTS:
;   P - parameters of the linear model, as described above.
;
; KEYWORD INPUTS:
;   (as described above, these quantities should be placed in
;    a FUNCTARGS structure)
;   X - vector, X position of each data point
;   Y - vector, Y position of each data point
;   SIGMA_X - vector, X uncertainty of each data point
;   SIGMA_Y - vector, Y uncertainty of each data point
;
; RETURNS:
;   Returns a vector of residuals, of the same size as X.
;
; EXAMPLE:
;   
;   ; X and Y values
;   XS = [2.9359964E-01,1.0125043E+00,2.5900450E+00,2.6647639E+00,3.7756164E+00,4.0297413E+00,4.9227958E+00,6.4959011E+00]
;   YS = [6.0932738E-01,1.3339731E+00,1.3525699E+00,1.4060204E+00,2.8321848E+00,2.7798350E+00,2.0494456E+00,3.3113062E+00]
;   
;   ; X and Y errors
;   XE = [1.8218818E-01,3.3440986E-01,3.7536234E-01,4.5585755E-01,7.3387712E-01,8.0054945E-01,6.2370265E-01,6.7048335E-01]
;   YE = [8.9751285E-01,6.4095122E-01,1.1858428E+00,1.4673588E+00,1.0045623E+00,7.8527629E-01,1.2574003E+00,1.0080348E+00]
;
;   ; Best fit line
;   p = mpfit('LINFITEX', [1d, 1d], $
;                FUNCTARGS={X: XS, Y: YS, SIGMA_X: XE, SIGMA_Y: YE}, $
;                perror=dp, bestnorm=chi2)
;   yfit = p[0] + p[1]*XS
;
;
; REFERENCES:
;
;   Press, W. H. 1992, *Numerical Recipes in C*, 2nd Ed., Cambridge
;      University Press
;
; MODIFICATION HISTORY:
;   Written, Feb 2009
;   Documented, 14 Apr 2009, CM
;  $Id$
;
;-
; Copyright (C) 2009, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
;

function linfitex, p, $
                   x=x, y=y, sigma_x=sigma_x, sigma_y=sigma_y, $
                   _EXTRA=extra

  a = p[0]   ;; Intercept
  b = p[1]   ;; Slope

  f = a + b*x

  resid = (y - f)/sqrt(sigma_y^2 + (b*sigma_x)^2)

  return, resid
end
