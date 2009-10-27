;+
; NAME:
;   SRVADD
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Add velocity 3-vectors according to special relativity
;
; MAJOR TOPICS:
;   Physics, Geometry
;
; CALLING SEQUENCE:
;   U0 = SRVADD(U1, V)
;
; DESCRIPTION:
;
;  The function SRVADD performs addition of velocity 3-vectors
;  according to special relativity.
;
;  Consider two inertial coordinate frames.  Frame "0" is a "lab" or
;  rest frame.  Frame "1" is a "rocket" or moving frame, moving at
;  velocity V with respect to the lab frame.  The velocity V is
;  allowed to be an arbitrary 3-vector.
;
;    * An observer in the rocket frame sees a body moving at velocity U1.
;
;    * An observer in the lab frame sees the same body moving at
;      velocity U0.
;
;    * This function solves for U0 in terms of V and U1.
;
;  U1 and V are allowed to be 3xN arrays, which means more than one
;  vector can be computed in a single call.  If the dimensions of
;  either U1 or V are 3x1, then it will be expanded to match the
;  dimensions of the other vector.  This simulates addition by a
;  "scalar" vector.  Because V can be a 3xN array, this means that
;  multiple "rocket" frames can be computed at one time.
;
;  NOTE: Velocities passed to SRVADD are measured as a *fraction of
;        the speed of light*.  Therefore, if the velocities are
;        measured in some physical units, and CLIGHT is the speed of
;        light in those same units, then the following statement:
;
;           U0 = SRVADD(U1/CLIGHT, V/CLIGHT)*CLIGHT
;
;        will compute the velocity U0, also in the same units.
;
;
;  The formula for computing the velocity in the lab frame is:
;
;         ( (1-1/GAMMA)*(U1 . VUNIT)*VUNIT + U1/GAMMA + V )
;    U0 = -------------------------------------------------
;                           (1 - U1 . V)
;
;  where 
;    GAMMA is the Lorentz factor = 1/SQRT(1 - |V|^2)
;    VUNIT is the unit vector in the direction of V, = V/|V|
;    "." is the vector dot product
;
;  [ IDL notation is not strictly adhered to in this formula, for
;  clarity of presentation. ]
;
;
; INPUTS:
;
;   U1 - 3-vector or 3xN array, the velocity of a body as seen in the
;        rocket frame (frame 1).  The velocity is normalized such that
;        the speed of light is 1.
;
;
;   V - 3-vector or 3xN array, the velocity of the rocket frame as
;       seen by an observer in the lab.  The velocity is normalized
;       such that the speed of light is 1.
;
; RETURNS:
;
;   A 3xN array, containing the velocity of the body as seen in the
;   lab frame.  The velocity is normalized such that the speed of
;   light is 1.
;
; KEYWORD PARAMETERS:
;
;   CLASSICAL - if set, then classical velocity addition is performed,
;               and the relativistic form is disabled.
;               Default: not set (i.e., relativity is applied)
;
; EXAMPLE:
;
;   IDL> print, srvadd([0.1d,0,0],   [0.5d,0,0])
;         0.56504883       0.0000000       0.0000000
;
;   Adds velocities of 0.1 and 0.5 times the speed of light.  The
;   result is slightly less than the arithmetic sum.
;
;
;   IDL> print, srvadd([0.,0.1,0],[0.5d,0,0])
;         0.50000000     0.086602542       0.0000000
;
;   Adds velocities in two orthogonal directions.  Demonstrates the
;   relativistic aberration of velocities (i.e., velocities in the
;   perpendicular direction are affected).
;
;
; MODIFICATION HISTORY:
;   Written, 28 Jan 2002, CM
;   More documentation, 29 Jan 2002, CM
;   Add CLASSICAL keyword, 29 Jul 2002, CM
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

function srvadd, u, v, classical=classical

  nu = n_elements(u)/3
  nv = n_elements(v)/3

  if nu EQ 0 OR nv EQ 0 then begin
      message, 'USAGE: U0 = SRVADD(U1, V)', /info
      message, '   U1 = vel. of body in rocket frame; V is velocity of rocket in lab', $
        /info
      message, '   U0 = vel. of body in lab frame', /info
      return, -1d
  endif

  if nu NE nv AND nu NE 1 AND nv NE 1 then begin
      message, 'ERROR: U and V must have the same number of vectors'
  endif else if nu EQ 1 then begin
      v1 = v
      u1 = v*0
      u1(0,*) = u(0) & u1(1,*) = u(1) & u1(2,*) = u(2)
  endif else if nv EQ 1 then begin
      u1 = u
      v1 = u*0
      v1(0,*) = v(0) & v1(1,*) = v(1) & v1(2,*) = v(2)
  endif else begin
      u1 = u
      v1 = v
  endelse

  if keyword_set(classical) then begin
      return, u1+v1
  endif

  ;; Compute unit vector v, along with 1/gamma and 1 - 1/gamma
  vunit = v1
  vnorm = total(vunit^2,1)        ;; Momentarily = |V|^2

  oogamma = sqrt(1 - vnorm)       ;; 1/gamma
  omoogamma = 1 - oogamma         ;; 1 - 1/gamma
  vnorm = sqrt(temporary(vnorm))  ;; Now vnorm = |V|
  wh = where(vnorm EQ 0, ct)      ;; Avoid overflow
  if ct GT 0 then vnorm(wh) = 1

  ;; Normalize the unit vector
  if ct GT 0 then $
    for i = 0, 2 do vunit(i,*) = vunit(i,*) / vnorm

  ;; Compute elements of numerator and denominator
  udv  = total(u1*v1,1)        ;; Dot product U1 . V
  denom = 1/(1 + udv)          ;; Denominator of expression
  udvu = temporary(udv)/vnorm  ;; Dot product U1 . VUNIT
  uu = [1,1,1]                 ;; Used to expand N-vector to 3xN array
  
  ;; U0 = ( (1-1/GAMMA)*(U1 . VUNIT)*VUNIT + U1/GAMMA + V ) / (1 - U1 . V)
  return, ((uu#(omoogamma*udvu))*vunit + (uu#oogamma)*u1 + v1)*(uu#denom)

end
