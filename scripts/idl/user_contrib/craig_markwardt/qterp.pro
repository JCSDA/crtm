;+
; NAME:
;   QTERP
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Smoothly interpolate from a grid of quaternions (spline or slerp)
;
; MAJOR TOPICS:
;   Geometry
;
; CALLING SEQUENCE:
;   QNEW = QTERP(TGRID, QGRID, TNEW, [/SLERP], QDIFF=, [/RESET])
;
; DESCRIPTION:
;
;  The function QTERP is used to interplate from a set of known unit
;  quaternions specified on a grid of independent values, to a new set
;  of independent values.  For example, given a set of quaternions at
;  specified key times, QTERP can interpolate at any points between
;  those times.  This has applications for computer animation and
;  spacecraft attitude control.
;
;  The "grid" of quaternions can be regularly or irregularly sampled.
;  The new values can also be regularly or irregularly sampled.
;
;  The simplest case comes when one wants to interpolate between two
;  quaternions Q1 and Q2.  In that case the user should specify the
;  gridded quaterion as QGRID = [[Q1], [Q2]], with grid points at
;  TGRID = [0d, 1d].  Then the user can sample any intermediate
;  orientation by specifying TNEW anywhere between 0 and 1.
;
;  The user has the option of performing pure spline interpolation of
;  the quaternion components (the default technique).  The resulting
;  interpolants are normalized to be unit quaternions.  This option is
;  useful for fast interpolation of quaternions, but suffers if the
;  grid is not well sampled enough.  Spline interpolation will not
;  strictly find the shortest path between two orientations.
;
;  The second option is to use Spherical Linear IntERPolation, or
;  SLERPing, to interpolate between quaternions (by specifying the
;  SLERP keyword).  This technique is guaranteed to find the shortest
;  path between two orientations, but is somewhat slower than spline
;  interpolation.  This approach involves computing a finite
;  difference of the data.  To avoid repeated computation of the
;  difference on every call, users can pass a named variable in the
;  QDIFF keyword.  This value can be reset with the RESET keyword.
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
;  Users must have the VALUE_LOCATE() function, available either in
;  IDL 5.3 or later, or from the Markwardt web page.
;
; INPUTS:
;
;   TGRID - a vector of N independent variable values.  In the
;           simplest case, this can be [0, 1, ...] up to the number of
;           quaternions in the grid.  The grid sampling does not have
;           to be uniform.
;
;   QGRID - an 4xN array of unit quaternions specified on the grid.
;
;   TNEW - a vector of M desired independent variable values which
;          sample the grid specified by TGRID.  The desired values do
;          not have to be uniformly sampled.
;
; RETURNS:
;
;   A 4xM array of unit quaternions, where is M is the number of
;   desired samples.
;
;
; KEYWORD PARAMETERS:
;
;   SLERP - if set, then spherical linear interpolation is performed.
;           The default is to perform spline interpolation on the
;           quaternion coefficients.
;
;   QDIFF - upon return, QDIFF is filled with finite difference values
;           which can be used to speed computations in subsequent
;           calls.  Users should be aware that QDIFF may be
;           inadvertently reused from one call to the next.  When the
;           difference data should no longer be reused, the named
;           variable passed to the QDIFF keyword should be set to a
;           scalar, or the /RESET keyword should be used.
;
;   RESET - if set, then the QDIFF finite difference will be forced to
;           be recalculated, even if there is already data present and
;           passed to the QDIFF keyword.
;
;
; EXAMPLE:
;
;   This example starts with two quaternions representing rotations of
;   0 degrees and 45 degrees, and forms 1001 quaternions which are
;   smooth interpolations between 0 and 45 degrees.
;
;   ;; Create a grid of two quaternions at times 0 and 1
;   Q0 = qtcompose([1,0,0], 0D)       & T0 = 0D
;   Q1 = qtcompose([1,0,0], !dpi/4)   & T1 = 1D
;
;   ;; Put the grid elements into an array
;   TGRID = [T0, T1]
;   QGRID = [[Q0], [Q1]]
;
;   ;; Make an array of 11 values smoothly varying from 0 to 1
;   TNEW = dindgen(11)/10d
;
;   ;; Perform spherical linear interpolation
;   QNEW = QTERP(TGRID, QGRID, TNEW, /SLERP)
;
;   --->  (interpolated results in QNEW)
;  
;       0.0000000       0.0000000       0.0000000       1.0000000
;     0.039259816       0.0000000       0.0000000      0.99922904
;     0.078459096       0.0000000       0.0000000      0.99691733
;      0.11753740       0.0000000       0.0000000      0.99306846
;      0.15643447       0.0000000       0.0000000      0.98768834
;      0.19509032       0.0000000       0.0000000      0.98078528
;      0.23344536       0.0000000       0.0000000      0.97236992
;      0.27144045       0.0000000       0.0000000      0.96245524
;      0.30901699       0.0000000       0.0000000      0.95105652
;      0.34611706       0.0000000       0.0000000      0.93819134
;      0.38268343       0.0000000       0.0000000      0.92387953
;   
;
; SEE ALSO
;   QTANG, QTAXIS, QTCOMPOSE, QTERP, QTEXP, QTFIND, QTINV, QTLOG,
;   QTMAT, QTMULT, QTPOW, QTVROT
;
; MODIFICATION HISTORY:
;   Written, July 2001, CM
;   Documented, Dec 2001, CM
;   Usage message; check for 0- and 1-length quaternions; handle case
;      when quaternions are GE 180 degrees apart; handle case of
;      interpolating beyond end of known grid, 15 Mar 2002, CM
;   Use simplified QTMULT with /INV, 21 Sep 2007, CM
;   Added sample output, 29 Sep 2008, CM
;
;  $Id$
;
;-
; Copyright (C) 2001, 2002, 2007, 2008, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

function qterp, t0, q0, t1, qdiff=qdiff, reset=reset, slerp=slerp

  if n_params() EQ 0 then begin
      info = 1
      USAGE:
      message, 'USAGE:', /info
      message, 'QNEW = QTERP(TGRID, QGRIDJ, TNEW, [/SLERP, QDIFF=, /RESET])',$
        info=info
      return, 0
  endif

  nq = n_elements(q0)/4
  if nq EQ 0 then goto, USAGE

  ;; If there is only one quaternion, replicate it, since there is
  ;; nothing to interpolate
  if nq EQ 1 then $
    return, rebin(reform(q0,4,1),4,n_elements(t1))

  if keyword_set(slerp) then begin
      if n_elements(qdiff)/4 NE nq-1 OR keyword_set(reset) then begin
          qdiff = qtmult(q0(*,0:nq-2), /inv1, q0(*,1:*))

          ;; Normalize the quaternions to get the smallest path
          wh = where(qdiff(3,*) LT 0, ct)
          if ct GT 0 then qdiff(*,wh) = -qdiff(*,wh)
      endif
      ii = value_locate(t0, t1) < (nq-2) > 0
      hh = (t1-t0(ii))/(t0(ii+1)-t0(ii))

      return, qtmult(q0(*,ii), qtpow(qdiff(*,ii),hh))
  endif
  
  q1 = (q0(*,0) # t1) * 0
  for i = 0, 3 do $
    q1(i,*) = spl_interp(t0, q0(i,*), spl_init(t0, q0(i,*)), t1)
  tot = sqrt(total(q1^2,1))
  for i = 0, 3 do $
    q1(i,*) = q1(i,*) / tot

  return, q1
end
