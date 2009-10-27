;+
; NAME:
;   QTFIND
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Find quaternion(s) from direction cosine matrix
;
; MAJOR TOPICS:
;   Geometry
;
; CALLING SEQUENCE:
;   Q = QTFIND(MATRIX)
;
; DESCRIPTION:
;
;   The function QTFIND determines one or more unit quaternions from
;   direction cosine matrices.
;
;   This routine is optimized to avoid singularities which occur when
;   any one of the quaternion components is nearly zero.  Up to four
;   different transformations are attempted to maximize the precision
;   of all four quaternion components.
;
;   QTFIND and QTMAT are functional inverses: use QTFIND to convert a
;   known direction cosine matrix to a new quaternion; use QTMAT to
;   convert a known quaternion to matrix representation.
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
;   MATRIX - array of one or more direction cosine matrices.  For a
;            single matrix, MATRIX should be a 3x3 array.  For N
;            matrices, MATRIX should be a 3x3xN array.  The arrays are
;            assumed to be valid rotation matrices.
;
;
; RETURNS:
;
;   The resulting unit quaternions.  For a single matrix, returns a
;   single quaternion as a 4-vector.  For N matrices, returns N
;   quaternions as a 4xN array.
;
;
; KEYWORD PARAMETERS:
;
;   NONE
;
; EXAMPLE:
;
;   ;; Form a rotation matrix about the Z axis by 32 degrees
;   th1 = 32d*!dpi/180         
;   mat1 = [[cos(th1),-sin(th1),0],[sin(th1),cos(th1),0],[0,0,1]]
;   
;   ;; Form a rotation matrix about the X axis by 116 degrees
;   th2 = 116d*!dpi/180
;   mat2 = [[1,0,0],[0,cos(th2),-sin(th2)],[0,sin(th2),cos(th2)]]
;
;   ;; Find the quaternion that represents MAT1, MAT2 and the
;   composition of the two, MAT2 ## MAT1.
;
;    print, qtfind(mat1), qtfind(mat2), qtfind(mat2 ## mat1)
;       0.0000000       0.0000000      0.27563736      0.96126170
;      0.84804810       0.0000000       0.0000000      0.52991926
;      0.81519615     -0.23375373      0.14606554      0.50939109
;
;
; SEE ALSO
;   QTANG, QTAXIS, QTCOMPOSE, QTERP, QTEXP, QTFIND, QTINV, QTLOG,
;   QTMAT, QTMULT, QTPOW, QTVROT
;
; MODIFICATION HISTORY:
;   Written, July 2001, CM
;   Documented, Dec 2001, CM
;   Re-added check to enforce q(3) GE 0, 15 Mar 2002, CM
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

function qtfind, amat

; THIS ROUTINE CONVERTS ROTATION MATRIX AMAT INTO QUATERNION AQT
; IT ASSUMES AMAT IS A VALID ROTATION MATRIX
; THIS IS ADAPTED FROM CHAPTER 12 BY F.L.MARKLEY
;
; MODIFIED 11/22/95 TO AVOID SINGULARITIES (E.G., Q4=0.)
; THE SQUARE OF ONE OF THE QUATERNION ELEMENTS MUST BE >= 0.25
;	SINCE THE 4 SUM TO 1.
; MOD 11/24/95 TO MAKE SURE Q4 >= 0
; MOD 14-DEC-95 TO FIX BUG OF WRONG SIGN OF Q4 IF Q1,Q3,&Q4 < .5

  if n_params() EQ 0 then begin
      info = 1
      USAGE:
      message, 'USAGE:', /info
      message, 'Q = QTFIND(MATRIX)', /info
      message, '   MATRIX must be a 3x3xN array of direction cosines', $
        info=info
      return, 0
  endif

  sz = size(amat)
  if sz(0) LT 2 then begin
      DIM_ERROR:
      
      message, 'ERROR: MATRIX must be a 3x3xN array', /info
      return, 0
  endif
  if sz(1) NE 3 OR sz(2) NE 3 then goto, DIM_ERROR

  nq = n_elements(amat)/9
  ad0 = amat(0,0,*) & ad1 = amat(1,1,*) & ad2 = amat(2,2,*)
  a12 = amat(1,2,*) & a21 = amat(2,1,*)
  a20 = amat(2,0,*) & a02 = amat(0,2,*)
  a01 = amat(0,1,*) & a10 = amat(1,0,*)

  n1 = nq
  q0 = replicate(amat(0)*0+0., nq) & q1 = q0 & q2 = q0 & q3 = q0

  qd = 1. + ad0 + ad1 + ad2
  wh = where(qd GE 0.99, ct)
  if ct GT 0 then begin
      qx = 0.5*sqrt(qd(wh))
      q3(wh) = qx
      qx = qx * 4
      q0(wh) = (a12-a21)(wh)/qx
      q1(wh) = (a20-a02)(wh)/qx
      q2(wh) = (a01-a10)(wh)/qx
      n1 = n1 - ct
  endif 
  if n1 GT 0 then begin
      qd = 1. + ad0 - ad1 - ad2
      wh = where(qd GE 0.99, ct)
      if ct GT 0 then begin
          qx = 0.5*sqrt(qd(wh))
          q0(wh) = qx
          qx = qx * 4
          q3(wh) = (a12-a21)(wh)/qx
          q2(wh) = (a20+a02)(wh)/qx
          q1(wh) = (a01+a10)(wh)/qx
          n1 = n1 - ct
      endif
  endif
  if n1 GT 0 then begin
      qd = 1. - ad0 - ad1 + ad2
      wh = where(qd GE 0.99, ct)
      if ct GT 0 then begin
          qx = 0.5*sqrt(qd(wh))
          q2(wh) = qx
          qx = qx * 4
          q1(wh) = (a12+a21)(wh)/qx
          q0(wh) = (a20+a02)(wh)/qx
          q3(wh) = (a01-a10)(wh)/qx
          n1 = n1 - ct
      endif
  endif
  if n1 GT 0 then begin
      qd = 1. - ad0 + ad1 - ad2
      wh = where(qd GE 0.99, ct)
      if ct GT 0 then begin
          qx = 0.5*sqrt(qd(wh))
          q1(wh) = qx
          qx = qx * 4
          q2(wh) = (a12+a21)(wh)/qx
          q3(wh) = (a20-a02)(wh)/qx
          q0(wh) = (a01+a10)(wh)/qx
          n1 = n1 - ct
      endif
  endif

  wh = where(q3 LT 0, ct)
  if ct GT 0 then begin
      q0(wh) = -q0(wh)
      q1(wh) = -q1(wh)
      q2(wh) = -q2(wh)
      q3(wh) = -q3(wh)
  endif

  return, transpose([[q0],[q1],[q2],[q3]])
end
