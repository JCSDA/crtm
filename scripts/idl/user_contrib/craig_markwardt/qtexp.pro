;+
; NAME:
;   QTEXP
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Compute "exponentiation" of a non-unit quaternion
;
; MAJOR TOPICS:
;   Geometry
;
; CALLING SEQUENCE:
;   Q = QTEXP(QLOG)
;
; DESCRIPTION:
;
;   The function QTEXP computes the "exponentiation" of a quaternion.
;
;   The exponential is only defined for a non-unit quaternion with a
;   *zero* rotation angle.  Specifically, the expression
;
;      QTEXP([VAXIS * PHI/2, 0])    
;
;   becomes
; 
;      [VAXIS*SIN(PHI/2), COS(PHI/2)]
;
;   where VAXIS is the unit vector rotation axis and PHI is the
;   rotation angle.  Note that since VAXIS is a unit vector, the
;   product VAXIS*PHI can have an arbitrary direction and magnitude.
;
;   Typically the input to QTEXP is found by taking the logarithm of a
;   unit quaternion using QTLOG, and the identity QTEXP(QTLOG(Q)) is
;   the same as Q.
;   
;  Conventions for storing quaternions vary in the literature and from
;  library to library.  This library uses the convention that the
;  first three components of each quaternion are the 3-vector axis of
;  rotation, and the 4th component is the rotation angle.  Expressed
;  in formulae, a single quaternion is given by:
;
;     Q(0:2) = [VX, VY, VZ]*SIN(PHI/2)
;     Q(3)   =              COS(PHI/2)
;
;  where PHI is the rotation angle, and VAXIS = [VX, VY, VZ] is the
;  rotation eigen axis expressed as a unit vector.  This library
;  accepts quaternions of both signs, but by preference returns
;  quaternions with a positive 4th component.
;
; INPUTS:
;
;   QLOG - a non-unit quaternion of the form [VX, VY, VZ, 0]; or,
;          N quaternions of the same form, as a 4xN array.
;
;
; RETURNS:
;
;   The exponentiated unit quaternion(s).  For a single input
;   quaternion, returns a 4-vector; for N input quaternions, returns a
;   4xN array.
;
;
; KEYWORD PARAMETERS:
;
;   NONE
;
; EXAMPLE:
;
;   IDL> print, qtlog(qtcompose([0d,1,0], !dpi/4))
;         0.0000000      0.39269908       0.0000000       0.0000000
;
;   Prints the logarithm of the quaternion composed of a rotation of
;   !dpi/4 radians around the axis [0,1,0]
;
;
; SEE ALSO
;   QTANG, QTAXIS, QTCOMPOSE, QTERP, QTEXP, QTFIND, QTINV, QTLOG,
;   QTMAT, QTMULT, QTPOW, QTVROT
;
; MODIFICATION HISTORY:
;   Written, July 2001, CM
;   Documented, Dec 2001, CM
;   Documentation corrected, 27 Jan 2002, CM
;   Usage message, error checking, 15 Mar 2002, CM
;
;  $Id$
;
;-
; Copyright (C) 2001, 2002, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

function qtexp, q

  if n_params() EQ 0 then begin
      info = 1
      USAGE:
      message, 'USAGE:', /info
      message, 'QNEW = QTEXP(Q)', info=info
      return, 0
  endif
  nq = n_elements(q)/4
  if nq LT 1 then goto, USAGE

  v = q(0:2,*)
  th = sqrt(total(v^2,1))
  wh = where(th NE 0, ct)

  if ct GT 0 then v(*,wh) = v(*,wh)/rebin(reform(th(wh),1,ct),3,ct)

  q1 = q

  q1(3,*) = cos(th)
  q1(0:2,*) = rebin(reform([sin(th)],1,nq),3,nq)*v

  return, q1
end
