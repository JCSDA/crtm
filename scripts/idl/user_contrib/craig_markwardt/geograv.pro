;+
; NAME:
;   GEOGRAV
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Estimate gravitational potential and acceleration by harmonic expansion
;
; MAJOR TOPICS:
;   Physics, Gravity, Geodesy, Spacecraft Navigation
;
; CALLING SEQUENCE:
;   GEOGRAV, GEOGMOD, R, PHI, A [, NMAX=NMAX, MMAX=MMAX, UNITS=UNITS]
;
; DESCRIPTION:
;
;   GEOGRAV estimates the gravitational potential and acceleration due
;   to a non-point central body such as the Earth.  The computation is
;   based on an expansion of the potential spherical harmonics.  The
;   coefficients of the expansion, the Cnm and Snm, are assumed to be
;   known, and available in the GEOGMOD structure (see GEOGREAD).
;   Various gravity solutions are available.
;
;   The user specifies the geocentric position of interest, referred
;   to the earth-fixed coordinates.  The result is the *inertial*
;   gravitational potential and acceleration, expressed in earth-fixed
;   coordinates (i.e., no fictitious potentials or accelerations are
;   applied).  Users should normally rotate the acceleration into
;   inertial coordinates.
;
;   Users can restrict the degree and order of the potential
;   evaluation using the NMAX (order) and MMAX (degree) keywords.
;
;   Input *and* output units are specified using the UNITS keyword,
;   which is an integer value between 1 and 3. The allowed values are:
;
;      UNITS        Accel.       Pot.      Position
;        1  (cgs)   cm/s^2      (cm/s)^2      cm
;        2  (mks)   m/s^2       (m/s)^2        m
;        3  (km)    km/s^2      (km/s)^2      km
;   Note that the input coordinate units must match the desired output
;   units.
;
; INPUTS:
;
;   GEOGMOD - gravity model structure, as returned by GEOGREAD.
;
;   R - earth-fixed position(s) of interest.  Either a 3-vector, for a
;       single evaluation, or a 3xN array, for evaluations of N
;       vectors.
;
;   PHI - upon return, the potential(s). Either a scalar or an
;         N-vector, depending on R.
;
;   A - upon return, the acceleration(s).  Either a 3-vector or a 3xN
;       array, depending on R.
;
;
; KEYWORD PARAMETERS:
;
;   NMAX - maximum spherical harmonic order to evaluate
;
;   MMAX - maximum spherical harmonic degree to evaluate
;
;   UNITS - specifies input and output physical units (see above).
;
;
; IMPLEMENTATION NOTE:
;
;   The computations in this routine are based on recursion relations
;   for fully-normalized associated Legendre polynomials.  They should
;   be stable (and avoid underflow) for evaluations of high order
;   expansions.
;
; EXAMPLE:
;   GEOGREAD, 'egm96', egm96
;   GEOGRAV, egm96, r, phi, a
;
;   Read the gravity model "EGM96" and evaluate it at position "R" in
;   body coordinates.  The potential and acceleration are returned in
;   PHI and A.
;
; REFERENCES:
;
;   Holmes, S. A. & Featherstone, W. E. 2002, "A unified approach to
;     the Clenshaw summation and the recursive computation of very
;     high degree and order normalised associated Legendre functions,"
;     J. Geodesy, 76, 279
;
;   McCarthy, D. D. (ed.) 1996: IERS Conventions, IERS T.N. 21.
;     http://maia.usno.navy.mil/conventions.html
;
;   Pines, S. 1973, "Uniform Representation of the Gravitational
;     Potential and its Derivatives," AIAA J., 11, 1508
;
;   Roithmayr, C. 1996, "Contributions of Spherical Harmonics to
;     Magnetic and Gravitational Fields," NASA Memo, NASA Johnson
;     Space Center, Houston, Texas, USA, 23 Jan 1996
;     (Republished as: NASA/TM2004213007, March 2004
;      URL: http://nssdcftp.gsfc.nasa.gov/models/geomagnetic/igrf/old_matlab_igrf/Contributions.pdf )
;
;   Seidelmann, P.K. 1992, *Explanatory Supplement to the Astronomical
;     Almanac*, ISBN 0-935702-68-7
;
;
; MODIFICATION HISTORY:
;   Written and documented, 05 Jan 2004, CM
;   Documentation additions, CM, 26 Sep 2004
;   Add missing UNITIZE function, CM, 19 Nov 2004
;
; TODO:
;   Allow perturbations of the main coefficients, because of tides.
;
;  $Id$
;
;-
; Copyright (C) 2004, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

; Utility routine to compute unit vector for each vector
function geograv_unitize, u
  uu = sqrt(total(u^2,1))
  nu = n_elements(uu)
  if nu EQ 1 then return, u/uu(0)
  
  return, u / rebin(reform(uu,1,nu),3,nu)
end

; Main routine to compute gravity for one position
pro geograv_one, geogmod, r, phi, a, $
                 nmax=nmax, mmax=mmax, $
                 unitfact=unitfact, C=C, S=S

  ;; Units        Accel.       Pot.      Position
  ;;  1 - cgs,    cm/s^2      (cm/s)^2      cm
  ;;  2 - mks,    m/s^2       (m/s)^2        m
  ;;  3 - km,     km/s^2      (km/s)^2      km

  rmean = geogmod.a   ;; Mean equatorial radius, in meters always
  mu    = geogmod.mu  ;; GM, in m^3/s^2 always

  rhat = geograv_unitize(r)
  rr = sqrt(total(r^2,1)) / unitfact
  u = rhat(2)

  rho = rmean/rr
  zero = rho*0.

  ;; Pines r_m, i_m.  Uninitialized array, plus the first two
  ;; components to start the recursion relation
  rm = fltarr(mmax+2) + zero & im = rm
  rm(0) = 1 & rm(1) = rhat(0)
  im(0) = 0 & im(1) = rhat(1)

  ;; NORMALIZED Pines Anm matrix.  We only keep one column at a time,
  ;; and the previous one.  Initialize as if we were doing n=1
  Anm = rm*0   & An_1m = Anm
  Anm(0:1) = [u,1]*sqrt(3d)
  An_1m(0) = 1

  ;; Index counters
  nn = lindgen(nmax+2)
  mm = lindgen(mmax+2)

  ;; rho_(n+1) - is overall radial scale factor.  NOTE: (mu/rr) term
  ;;             comes at the end.  We start at n = 1
  rho_n1 = (rmean/rr)^2
  rho_n  = (rmean/rr)

  ;; n = 0 term of Pines eqns (30)
  ax = 0d & ay = 0d & az = 0d

  ;; Pines eqns (30b) -- dominant spherical term (NOTE again, -mu/rr
  ;;                     term comes at the end)
  ar = - C(0,0) / rr  ;; radial accel
  phi = C(0,0)        ;; potential

  ;; Note: start at n = 1
  n = 1L
  while (n LE nmax) do begin
      ;; Extract normalized Cnm and Snm coefficients
      mmax1 = n < mmax
      Cnm = reform(C(n,0:mmax1)) & Snm = reform(S(n,0:mmax1))

      ;; Also the rm and im coefficients
      rmm = rm(0:mmax1)  & imm = im(0:mmax1)
      rm1 = [0,rm(0:mmax1-1)] & im1 = [0,im(0:mmax1-1)]

      ;; Pines eqns (27), multipliers for potential and gradient
      Dnm = Cnm*rmm + Snm*imm
      Enm = Cnm*rm1 + Snm*im1
      Fnm = Snm*rm1 - Cnm*im1

      ;; This is the derivative of Anm with respect to u, NORMALIZED
      Apnm = [Anm(1:mmax), 0]
      Apnm = Apnm * sqrt((n+mm+1d)*(n-mm)>0d)
      Apnm(0) = Apnm(0) / sqrt(2d)  ;; Special normalization for m=0

      ;; Compute accelerations for this order.
      ;; Pines eqns (30) - NOTE: Anm is NORMALIZED
      ;; Note the sum over m
      ax = ax + (rho_n1/rmean) * total(Enm*Anm*mm)
      ay = ay + (rho_n1/rmean) * total(Fnm*Anm*mm)
      az = az + (rho_n1/rmean) * total(Dnm*Apnm)

      ;; Compute radial accel, Pines eqn (30a).  Note, can't use eqn
      ;; (30b) because of normalization issues.
      ar = ar - (rho_n1/rmean) * total(Dnm*((n+1d +mm)*Anm + u*Apnm))

      ;; Potential, Pines eqn (11)
      phi = phi + rho_n * total(Dnm*Anm)


      ;; =================
      ;; Increment to next value of n
      n = n + 1

      ;; ----- Find next values of important matrices 
      tn = 2d*n & tnm1 = 2d*n-1d & tnp1 = 2d*n+1d
          
      ;; === Anm
      An_2m = An_1m ;; Move "n-1" column to "n-2" column 
      An_1m = Anm   ;; Move "n"   column to "n-1" column 
      
      ;; Initialize the recurrence (Pines eqns 23) - NORMALIZED
      ;; note: keeping these sqrt()'s distinct improves precision
      Anm(n) = sqrt(tnp1)/sqrt(tn)*An_1m(n-1)

      ;; Holmes & Featherstone eqns (12) - NORMALIZED
      ;; note: keeping these sqrt()'s distinct improves precision
      mmm = mm(0:n-1)
      xm = sqrt(tnm1*tnp1)/sqrt(double((n-mmm)*(n+mmm)))
      ym = sqrt(tnp1*(n+mmm-1d)*(n-mmm-1d))/sqrt((n-mmm)*(n+mmm)*(2d*n-3d))

      ;; Holmes & Featherstone eqn (11) - NORMALIZED
      Anm(0:n-1) = xm*u*An_1m(0:n-1) - ym*An_2m(0:n-1)

      ;; == rm and im, next terms in recurrence
      rm(n) = rm(1) * rm(n-1) - im(1) * im(n-1)
      im(n) = im(1) * rm(n-1) + rm(1) * im(n-1)

      ;; == rho_n1 ... simple geometric sequence
      rho_n1 = rho_n1 * (rmean/rr)
      rho_n  = rho_n  * (rmean/rr)
      
  endwhile
  
  ;; NOTE: now must normalize by (mu/rr)
  phi = phi * (-mu/rr) * unitfact^2

  ;; Compose the cartesian and radial components of the acceleration
  a = [ax,ay,az] + ar*rhat
  a = a * (mu/rr) * unitfact

  return
end

pro geograv, geogmod, r, phi, a, nmax=nmax0, mmax=mmax0, units=units0

  sz = size(geogmod)
  if sz(sz(0)+1) NE 8 then begin
      GEOGMOD_ERROR:
      message, 'ERROR: GEOGMOD must be a gravity model structure', /info
      return
  endif

  ;; Be sure it is a gravity structure
  isgrav = 0
  catch, catcherr
  if catcherr EQ 0 then isgrav = (geogmod.type EQ 'GRAVITY')
  catch, /cancel
  if isgrav EQ 0 then goto, GEOGMOD_ERROR

  if n_elements(nmax0) EQ 0 then nmax = geogmod.nmax else nmax = floor(nmax0(0))
  if n_elements(mmax0) EQ 0 then mmax = geogmod.mmax else mmax = floor(mmax0(0))
  nmax = nmax < geogmod.nmax
  mmax = mmax < geogmod.mmax < nmax

  if n_elements(units0) EQ 0 then begin
      units = 2
  endif else begin
      units = floor(units0(0))
  endelse
  case units of 
      1: unitfact = 100d
      2: unitfact = 1d
      3: unitfact = 0.001d
      else: begin
          message, 'ERROR: UNITS must be one of 1, 2, or 3', /info
          return
      end
  endcase

  ;; Retrieve the normalized coefficients
  C = *(geogmod.Cnm)
  S = *(geogmod.Snm)

  nv = n_elements(r)/3
  a   = fltarr(3,nv) + r(0)*0
  phi = fltarr(nv) + r(0)*0   & if nv EQ 1 then phi = phi(0)
  for i = 0L, nv-1 do begin
      geograv_one, geogmod, r(*,i), phi1, a1, C=C, S=S, $
        nmax=nmax, mmax=mmax, unitfact=unitfact
      
      a(*,i) = a1
      phi(i) = phi1
  endfor

  return
end
