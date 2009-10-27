;+
; NAME:
;   QTMAT
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Find direction cosine matrix from quaternion(s)
;
; MAJOR TOPICS:
;   Geometry
;
; CALLING SEQUENCE:
;   MATRIX = QTMAT(Q)
;
; DESCRIPTION:
;
;  The function QTMAT computes one or more direction cosine matrices
;  (i.e., rotation matrices) from unit quaternions.
;
;  The usage of the resulting matrix on a 3-vector X is either 
;  MATRIX # X, or MATRIX ## X, depdending on the meaning of the
;  rotation (i.e., body-fixed or coordinate-fixed, see QTVROT).
;
;  QTFIND and QTMAT are functional inverses: use QTFIND to convert a
;  known direction cosine matrix to a new quaternion; use QTMAT to
;  convert a known quaternion to matrix representation.
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
; KEYWORDS:
;
;  INVERT - if set, compute the matrix of QTINV(Q) instead Q
;
;
; RETURNS:
;
;   The direction cosine matrices.  For a single input quaternion,
;   retuns a 3x3 array.  For N input quaternions, returns a 3x3xN
;   array.
;
;
; KEYWORD PARAMETERS:
;
;   NONE
;
; EXAMPLE:
;
;   print, qtmat(qtcompose([0d,1,0], !dpi/4)) 
;        0.70710678       0.0000000      0.70710678
;         0.0000000       1.0000000       0.0000000
;       -0.70710678       0.0000000      0.70710678
;
;   Form a quaternion composed of a rotation of !dpi/4 radians around
;   the axis [0,1,0], and then print the corresponding rotation
;   matrix.
;
;
; SEE ALSO
;   QTANG, QTAXIS, QTCOMPOSE, QTERP, QTEXP, QTFIND, QTINV, QTLOG,
;   QTMAT, QTMULT, QTPOW, QTVROT
;
; MODIFICATION HISTORY:
;   Written, July 2001, CM
;   Documented, Dec 2001, CM
;   Documentation clarifications, 28 Jan 2002, CM
;   Allow multiple quaternions, 28 Jan 2002, CM
;   Usage message, error checking, 15 Mar 2002, CM
;   Add INVERT keyword, 05 Oct 2007, CM
;
;  $Id$
;
;-
; Copyright (C) 2002, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
function qtmat, q, invert=invert

; THIS IS ADAPTED FROM CHAPTER 12 BY F.L.MARKLEY
  if n_params() EQ 0 then begin
      info = 1
      USAGE:
      message, 'USAGE:', /info
      message, 'MATRIX = QTMAT(Q)', info=info
      return, 0
  endif
  nq = n_elements(q)/4
  if nq LT 1 then goto, USAGE

  if NOT keyword_set(invert) then begin
      q1 = q(0,*) & q2 = q(1,*) & q3 = q(2,*) & q4 = q(3,*)
  endif else begin
      q1 = -q(0,*) & q2 = -q(1,*) & q3 = -q(2,*) & q4 = q(3,*)
  endelse

  a = dblarr(3,3,nq)
  A(0,0,*)=Q1*Q1-Q2*Q2-Q3*Q3+Q4*Q4
  A(0,1,*)=2.D0*(Q1*Q2+Q3*Q4)
  A(0,2,*)=2.D0*(Q1*Q3-Q2*Q4)
  A(1,0,*)=2.D0*(Q1*Q2-Q3*Q4)
  A(1,1,*)=-Q1*Q1+Q2*Q2-Q3*Q3+Q4*Q4
  A(1,2,*)=2.D0*(Q2*Q3+Q1*Q4)
  A(2,0,*)=2.D0*(Q1*Q3+Q2*Q4)
  A(2,1,*)=2.D0*(Q2*Q3-Q1*Q4)
  A(2,2,*)=-Q1*Q1-Q2*Q2+Q3*Q3+Q4*Q4

  return, a
end

