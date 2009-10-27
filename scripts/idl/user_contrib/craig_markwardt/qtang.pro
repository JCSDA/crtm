;+
; NAME:
;   QTANG
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Find rotation angle(s) of unit quaternion
;
; MAJOR TOPICS:
;   Geometry
;
; CALLING SEQUENCE:
;   PHI = QTANG(Q)
;
; DESCRIPTION:
;
;  The function QTANG accepts a unit quaternion Q and returns the
;  rotation angle PHI of the quaternion.
;
;  Use QTAXIS and QTANG to extract the properties of an existing
;  quaternion.  Use QTCOMPOSE to combine a rotation axis and angle
;  into a new quaternion.
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
;  Q - array of one or more unit quaternions.  For a single
;      quaternion, Q should be a 4-vector.  For N quaternions, Q
;      should be a 4xN array.
;
; RETURNS:
;
;  For a single quaternion, returns the scalar quaternion rotation
;  angle in radians.  For N quaternions, returns an N-vector of
;  rotation angles.
;
; KEYWORD PARAMETERS:
;
;  NONE
;
; EXAMPLE:
;
;   IDL> print, qtang(qtcompose([0d,1,0], !dpi/4))
;         0.78539816
;
;   Prints the angle part of the quaternion composed of a rotation of
;   !dpi/4 radians around the axis [0,1,0]
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
function qtang, q

  if n_params() EQ 0 then begin
      info = 1
      USAGE:
      message, 'USAGE:', /info
      message, 'PHI = QTANG(Q)', info=info
      return, 0
  endif
  nq = n_elements(q)/4
  if nq LT 1 then goto, USAGE

  return, atan(sqrt(total(q(0:2,*)^2,1)),(q(3,*))(*))*2
end
