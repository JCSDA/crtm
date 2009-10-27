;+
; NAME:
;   QTLOG
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Compute "logarithm" of a unit quaternion
;
; MAJOR TOPICS:
;   Geometry
;
; CALLING SEQUENCE:
;   QLOG = QTLOG(Q)
;
; DESCRIPTION:
;
;   The function QTLOG computes the "logarithm" of a unit quaternion.
;
;   The logarithm of a quaternion is defined for any unit quaternion,
;   such that the expression
;
;     QTLOG([VAXIS*SIN(PHI/2), COS(PHI/2)] 
;
;   becomes
;
;     [VAXIS * PHI/2, 0]
;
;   where VAXIS is the unit vector rotation axis and PHI is the
;   rotation angle.  Note that the output quaternion is not a *unit*
;   quaternion.  The output of QTLOG is also commonly known as an
;   *axial vector*, for a rotation axis VAXIS and rotation angle
;   PHI/2.
;
;   Typically the output to QTLOG is eventually exponentiated with the
;   QTEXP function, and the identity QTEXP(QTLOG(Q)) is the same as Q.
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
;
; INPUTS:
;
;  Q - array of one or more unit quaternions.  For a single
;      quaternion, Q should be a 4-vector.  For N quaternions, Q
;      should be a 4xN array.
;
;
; RETURNS:
;
;   The non-unit quaternion logarithm(s).  For a single input
;   quaternion, returns a 4-vector of the form [VX, VY, VZ, 0].  For N
;   input quaternions, returns N quaternions of the same form as a 4xN
;   array.
;
;
; KEYWORD PARAMETERS:
;
;   NONE
;
; EXAMPLE:
;
;   IDL> q = qtlog(qtcompose([0d,1,0], !dpi/4))
;   IDL> print, qtexp(2 * q)
;          0.0000000      0.70710678       0.0000000      0.70710678
;
;   First, computes the logarithm Q of the quaternion composed of a
;   rotation of !dpi/4 radians around the axis [0,1,0].  Second,
;   computes the exponentiation of 2*Q.  This is the same as raising
;   the original quaternion to the second power.
;
;
; SEE ALSO
;   QTANG, QTAXIS, QTCOMPOSE, QTERP, QTEXP, QTFIND, QTINV, QTLOG,
;   QTMAT, QTMULT, QTPOW, QTVROT
;
; MODIFICATION HISTORY:
;   Written, July 2001, CM
;   Documented, Dec 2001, CM
;   Documentation clarified & corrected, 27 Jan 2002, CM
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
function qtlog, q

  if n_params() EQ 0 then begin
      info = 1
      USAGE:
      message, 'USAGE:', /info
      message, 'QNEW = QTLOG(Q)', info=info
      return, 0
  endif
  nq = n_elements(q)/4
  if nq LT 1 then goto, USAGE

  v = q(0:2,*)
  sinth = sqrt(total(v^2,1))
  wh = where(sinth NE 0, ct)
  if ct GT 0 then v(*,wh) = v(*,wh)/rebin(reform(sinth(wh),1,ct),3,ct)
  costh = q(3,*)

  q1 = q

  q1(3,*) = 0
  q1(0,0) = rebin(reform(atan(sinth,costh),1,nq),3,nq) * v

  return, q1
end
