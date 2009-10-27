;+
; NAME:
;   QTINV
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Compute inverse of QUATERNION
;
; MAJOR TOPICS:
;   Geometry
;
; CALLING SEQUENCE:
;   QINV = QTINV(Q)
;
; DESCRIPTION:
;
;   The function QTINV computes the inverse of the quaternion Q.  The
;   inverse of a quaternion is equivalent to a rotation about the same
;   axis but the opposite direction.
;
;   The inverse is also defined mathematically such that
;
;     QTMULT( Q, QTINV(Q) )   
;
;   becomes [0, 0, 0, 1], which is the identity quaternion.
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
;   The resulting inverse unit quaternions.  For a single input
;   quaternion, returns a 4-vector.  For N input quaternions, returns
;   N quaternions as a 4xN array.
;
;
; KEYWORD PARAMETERS:
;
;   NONE
;
; EXAMPLE:
;
;   IDL> print, qtcompose([0d,1,0], !dpi/4)
;          0.0000000      0.38268343       0.0000000      0.92387953
;   IDL> print, qtinv(qtcompose([0d,1,0], !dpi/4))
;          0.0000000      0.38268343       0.0000000     -0.92387953
;
;   Prints the quaternion composed of a rotation of !dpi/4 radians
;   around the axis [0,1,0], then the inverse of the same quaternion.
;
;
; SEE ALSO
;   QTANG, QTAXIS, QTCOMPOSE, QTERP, QTEXP, QTFIND, QTINV, QTLOG,
;   QTMAT, QTMULT, QTPOW, QTVROT
;
; MODIFICATION HISTORY:
;   Written, July 2001, CM
;   Documented, Dec 2001, CM
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

function qtinv, q

  if n_params() EQ 0 then begin
      info = 1
      USAGE:
      message, 'USAGE:', /info
      message, 'QNEW = QTINV(Q)', info=info
      return, 0
  endif
  nq = n_elements(q)/4
  if nq LT 1 then goto, USAGE

  return, [-q(0:2,*), q(3,*)]
end
