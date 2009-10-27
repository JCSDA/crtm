;+
; NAME:
;   QTMULTN
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Multiply several quaternions
;
; MAJOR TOPICS:
;   Geometry
;
; CALLING SEQUENCE:
;   QRESULT = QTMULTN(Q1, Q2, ...,  [/INV1, /INV2, ...] )
;
; DESCRIPTION:
;
;   The function QTMULTN performs multiplication of quaternions.
;   It is a convenience routine to simplify the multiplication
;   of a chain of several quaternions.  
;  
;   For example, 
;     QTMULTN(Q1,Q2,Q3,/INV3,Q4)
;   is the same as,
;     QTMULT(Q1,QTMULT(Q2,QTMULT(QTINV(Q3),Q4)))
;
;   Up to eight quaternions may be multiplied with this routine.
;
;   As for QTMULT(), Qn may be 'vectors' of quaternions, if the Qn are
;   4xN arrays.  In that case the input arrays must be of the same
;   dimension.
;
;   Note that quaternion multiplication is not commutative.
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
;  Qn - array of one or more unit quaternions, the nth operand in the
;       multiplication.  For a single quaternion, Qn should be a
;       4-vector.  For N quaternions, Qn should be a 4xN array.
;       If INVn is set, then the inverse of Qn is used.
;
;  INVn - if set, use QTINV(Qn) in place of Qn.
;
;
; RETURNS:
;
;   The resulting multiplied unit quaternions.  For a single inputs,
;   returns a 4-vector.  For N input quaternions, returns N
;   quaternions as a 4xN array.
;
;
; KEYWORD PARAMETERS:
;
;   NONE
;
; EXAMPLE:
;
;   Q1 = qtcompose([0,0,1],  32d*!dpi/180d)
;   Q2 = qtcompose([1,0,0], 116d*!dpi/180d)
;
;   IDL> print, qtmult(q1, q2)
;        0.81519615      0.23375373      0.14606554      0.50939109
;
;   Form a rotation quaternion of 32 degrees around the Z axis, and 
;   116 degrees around the X axis, then multiply the two quaternions.
;   
; SEE ALSO
;   QTANG, QTAXIS, QTCOMPOSE, QTERP, QTEXP, QTFIND, QTINV, QTLOG,
;   QTMAT, QTMULT, QTMULTN, QTPOW, QTVROT
;
; MODIFICATION HISTORY:
;   Written, 30 Aug 2007, CM
;
;  $Id$
;
;-
; Copyright (C) 2007, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

function qtmultn, qt1, qt2, qt3, qt4, qt5, qt6, qt7, qt8, $
                  inv1=inv1, inv2=inv2, inv3=inv3, inv4=inv4, $
                  inv5=inv5, inv6=inv6, inv7=inv7, inv8=inv8

  if n_params() LT 2 then begin
      info = 1
      USAGE:
      message, 'USAGE:', /info
      message, 'QNEW = QTMULTN(Q1, Q2, ...)', info=info
      return, 0
  endif

  rqt = qtmult(qt1, qt2, inv1=inv1, inv2=inv2)
  if n_params() GE 3 then rqt = qtmult(rqt, qt3, inv2=inv3)
  if n_params() GE 4 then rqt = qtmult(rqt, qt4, inv2=inv4)
  if n_params() GE 5 then rqt = qtmult(rqt, qt5, inv2=inv5)
  if n_params() GE 6 then rqt = qtmult(rqt, qt6, inv2=inv6)
  if n_params() GE 7 then rqt = qtmult(rqt, qt7, inv2=inv7)
  if n_params() GE 8 then rqt = qtmult(rqt, qt8, inv2=inv8)
    
  return, rqt
end
