;+
; NAME:
;   SRVDOPP
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Compute relativistic doppler shift (arbitrary velocity & photon dir.)
;
; MAJOR TOPICS:
;   Physics, Geometry
;
; CALLING SEQUENCE:
;   NU1_NU0 = SRVDOPP(U0, V)
;
; DESCRIPTION:
;
;  The function SRVDOPP computes the relativistic doppler shift
;  between two inertial reference frames.
;
;  Consider two inertial coordinate frames.  Frame "0" is a "lab" or
;  rest frame.  Frame "1" is a "rocket" or moving frame, moving at
;  velocity V with respect to the lab frame.  The velocity V is
;  allowed to be an arbitrary 3-vector.
;
;    * An observer in the lab frame sees a photon of frequency NU0
;      propagating in the direction U0. (U0 is a unit 3-vector)
;
;    * An observer in the rocket frame observes the same photon with
;      frequency NU1.
;
;    * This function computes the ratio NU1 / NU0.
;
;  U0 and V are allowed to be 3xN arrays, which means more than one
;  set of values can be computed in a single call.  If the dimensions
;  of either U0 or V are 3x1, then it will be expanded to match the
;  dimensions of the other vector.
;
;  NOTE: Velocities passed to SRVDOPP are measured as a *fraction of
;        the speed of light*.
;
;  The formula for computing the relativistic doppler shift is:
;              
;    NU1_NU0 =  (1 - U0 . V) * GAMMA
;              
;  where 
;    GAMMA is the Lorentz factor = 1/SQRT(1 - |V|^2)
;    "." is the vector dot product
;
;  [ IDL notation is not strictly adhered to in this formula, for
;  clarity of presentation. ]
;
;
; INPUTS:
;
;   U0 - 3-vector or 3xN array, the unit vector of the photon
;        propagation direction, as seen in the lab frame.
;
;   V - 3-vector or 3xN array, the velocity of the rocket frame as
;       seen by an observer in the lab.  The velocity is normalized
;       such that the speed of light is 1.
;
; RETURNS:
;
;   A N-vector giving the ratio, NU1/NU0, which is the ratio of the
;   frequency observed in the rocket frame to the frequency seen in
;   the lab frame.
;
; KEYWORD PARAMETERS:
;
;   CLASSICAL - if set, then classical Doppler shift is performed,
;               and the relativistic form is disabled.
;               Default: not set (i.e., relativity is applied)
;
; EXAMPLE:
;
;   IDL> RATIO = SRVDOPP([-1d,0,0], [0.1d,0,0])
;
;   A photon of frequency NU0 is moving along the -x axis in the lab
;   frame; a rocket observer is moving with speed 0.1 c along the +x
;   axis.  NU0 * RATIO is the frequency seen by the rocket observer.
;
;
;   IDL> RATIO = SRVDOPP([0,-1d,0], [0.1,0,0])
;
;   The observer is the same, but the photon is moving along the -y
;   axis.  NU0 * RATIO is the frequency seen by the rocket observer.
;   This is the relativistic transverse doppler shift.
;
;
; MODIFICATION HISTORY:
;   Written, 05 May 2002, CM
;   Documentation, 12 May 2002, CM
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

function srvdopp, u, v, classical=classical

  nu = n_elements(u)/3
  nv = n_elements(v)/3

  if nu EQ 0 OR nv EQ 0 then begin
      message, 'USAGE: NU_NU0 = SRVDOPP(U, V)', /info
      message, '   U = unit vector of photon in lab; '+$
        'V is velocity of rocket in lab', /info
      message, '   NU_NU0 = (freq. in rocket frame) / '+$
        '(freq. in lab frame)', /info
      return, -1d
  endif

  ;; Expand either of the arguments
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

  ;; Compute unit vector v, along with 1/gamma and 1 - 1/gamma
  vunit = v1
  vnorm = total(vunit^2,1)        ;; Momentarily = |V|^2

  if NOT keyword_set(classical) then begin
      oogamma = sqrt(1 - vnorm)       ;; 1/gamma
      ;; (1 - v . u) * gamma
      return, (1-total(v1*u1,1))/oogamma
  endif else begin
      ;; Classical Doppler shift
      return, (1-total(v1*u1,1))
  endelse

end
