;+
; NAME:
;   QTPOW
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Raise quaternion Q to the "power" POW
;
; MAJOR TOPICS:
;   Geometry
;
; CALLING SEQUENCE:
;   QNEW = QTPOW(Q, POW)
;
; DESCRIPTION:
;
;   The function QTPOW raises a quaterion Q to the power P.  The
;   operation 
;
;      QNEW = QTPOW(Q, POW)
;
;   is equivalent to
;
;      QNEW = QTEXP( POW * QTLOG(Q))
;
;   which is the same as the definition of raising a real number to
;   any power (however, QTPOW is faster than using QTLOG and QTEXP).
;
;   For integer values of POW, this form of exponentiation is also
;   directly equivalent to the multiplication of that many Q's
;   together.
;
;   Geometrically, raising Q to any power between 0 and 1 realizes a
;   rotation that smoothly interpolates between the identity
;   quaternion and Q.  Thus, QTPOW is useful for interpolation of
;   quaternions or SLERPing (spherical linear interpolation).
;
;   When raising more than one quaternion to a power at a time, the
;   number of quaternions and powers must be equal.
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
;  POW - array of N powers, where N is the number of quaternions.
;
;
; RETURNS:
;
;   The resulting exponentiated unit quaternions.  For a single
;   inputs, returns a 4-vector.  For N input quaternions, returns N
;   quaternions as a 4xN array.
;
;
; KEYWORD PARAMETERS:
;
;   NONE
;
; EXAMPLE:
;
;   ;; Form a rotation quaternion of 45 degrees about the X axis
;   Q = qtcompose([1,0,0], !dpi/4)
;
;   ;; Make an array of 1001 values smoothly varying from 0 to 1
;   P = dindgen(1001)/1000d
;
;   ;; Perform spherical linear interpolation
;   QNEW = QTERP(Q, P)
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

function qtpow, q, pow

  if n_params() EQ 0 then begin
      info = 1
      USAGE:
      message, 'USAGE:', /info
      message, 'QNEW = QTPOW(Q, POW)', info=info
      return, 0
  endif
  nq = n_elements(q)/4
  np = n_elements(pow)
  if nq LT 1 OR np LT 1 then goto, USAGE

  v = q(0:2,*)
  sinth = sqrt(total(v^2,1))

  th = atan(sinth, q(3,*))
  rat = th*0
  wh = where(sinth NE 0, ct)
  if ct GT 0 then rat(wh) = (sin(pow*th))(wh)/sinth(wh)

  q1 = q

  q1(3,*)   = cos(th*pow)
  q1(0:2,*) = rebin(reform(rat,1,nq),3,nq)*temporary(v)

  return, q1
end
