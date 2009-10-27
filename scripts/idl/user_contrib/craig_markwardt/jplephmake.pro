;+
; NAME:
;   JPLEPHMAKE
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Make a new ephemeris suitable for interpolation by JPLEPHINTERP
;
; MAJOR TOPICS:
;   Planetary Orbits, Interpolation
;
; CALLING SEQUENCE:
;   JPLEPHREAD, INFO, RAW, OBJ, T, CX, CY, CZ, $
;               [ POSUNITS=, AUTHOR=, DATE=, OBJECTNAME=, ]
;               [ KEYWORDS=, KEYVALUES=, /RESET ]
;
; DESCRIPTION:
;
;   JPLEPHMAKE is a utility routine which forms an ephemeris table
;   suitable for interpolation with JPLEPHINTERP.  This is a way for
;   users to make or augment an ephemeris of solar system bodies not
;   already present in the JPL planetary ephemeris.  This routine only
;   creates new ephemerides in memory.  No facility is provided to
;   write to disk.
;
;   The user must have already estimated the Chebyshev polynomial
;   coefficients for the body of interest.  One way to do this is with
;   CHEBGRID from the Markwardt library.  
;
;   The two options are either to create a new ephemeris or to augment
;   an existing one.  Augmentation merely means that new columns are
;   added to an existing ephemeris table.  The JPL ephemeris itself
;   can be augmented.
;
;   Even when creating a new ephemeris from scratch, passing an
;   existing INFO structure based on another epehemeris is strongly
;   recommended, because the structure usually contains planetary
;   masses, physical constants, etc. which are relevant.
;
;
;
; PARAMETERS: 
;
;
;   INFO - upon input, an existing INFO structure based on a known
;          ephemeris.  Upon output, a modified INFO structure.
;
;          If INFO is undefined upon input, or the RESET keyword is
;          set, then the returned INFO is set to a generic header.
;
;   RAW - upon input, an existing set of Chebyshev coefficients.  Upon
;         output, the new or augmented set of coefficients.
;
;         If RAW is undefined upon input, or if the RESET keyword is
;         set, then the returned RAW variable is initialized to a new
;         set of keywords.
;
;   OBJ - scalar string, name of the object.
;
;   T - array of times, in Julian Days (TDB), which refer to the
;       *start* epoch of each granule.  [ In the terminology of the
;       JPL ephemeris and CHEBGRID, a "granule" is a single
;       subinterval over which a Chebyshev polynomial is fitted. ] If
;       an existing ephemeris is to be augmented, then T must overlap
;       exactly.
;
;   CX, CY, CZ - arrays of Chebyshev polynomial coefficients.
;
;
;
; KEYWORD PARAMETERS:
;
;   POSUNITS - a scalar string, the units of position as fitted by CX,
;              CY, and CZ.  Allowed values:
;                 'KM' - kilometers  (default)
;                 'CM' - centimeters
;                 'AU' - astronomical units
;                 'LT-S' - light seconds
;
;   NSUBINTERVALS - Number of granules per time sample.
;                   Default: 1
;
;   RESET - if set, then a new ephemeris table is created.  Any
;           Chebyshev coefficients in RAW are overwritten.
;
;   AUTHOR - a scalar string, an identifier giving the author of the
;            new ephemeris.
;            Default: ''
;
;   DATE - a scalar string, the creation date of the ephemeris.
;          Default: SYSTIME(0)
;
;   KEYWORDS - an optional string array, giving any header keywords to
;              be added to the ephemeris (in conjunction with
;              KEYVALUES).
;              Default: (none)
;
;   KEYVALUES - an optional double array, giving any header values for
;               the keywords specified by KEYWORDS.
;
;               Default: (none)
;
;
; EXAMPLE:
;
;
; REFERENCES:
;
;   JPL Export Ephmeris FTP Site
;      ftp://navigator.jpl.nasa.gov/pub/ephem/export/
;      (ephemeris files are available here, however, they must be
;      converted to FITS format using the "bin2eph" utility found in
;      AXBARY)
;
;
; SEE ALSO
;   JPLEPHREAD, JPLEPHINTERP, JPLEPHTEST
;   
; MODIFICATION HISTORY:
;   Written and Documented, CM, Mar 2002
;   Corrected way that ephemerides are merged, also
;     way that AUTHOR field is filled, 29 May 2002, CM
;
;  $Id$
;
;-
; Copyright (C) 2002, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy and distribute unmodified copies for
; non-commercial purposes, and to modify and use for personal or
; internal use, is granted.  All other rights are reserved.
;-

pro jplephmake, info, raw, obj, t, cx, cy, cz, reset=reset, $
                posunits=posunits, author=author, date=date, $
                keywords=newkeywords, keyvalues=newkeyvalues, $
                nsubintervals=ns0

  if n_elements(info) GT 0 then begin
      c = info.c
      au = info.au
      keywords = info.keywords
      keyvalues = info.keyvalues
  endif

  if n_elements(author) EQ 0 then author = ''
  if n_elements(date) EQ 0 then date = systime()

  fresh = n_elements(info) EQ 0 OR keyword_set(reset)

  if fresh then begin
      nrows = n_elements(t)-1
      tstart = min(t) & tstop = max(t)
      timedel = t(1)-t(0)
      format= 'JPLEPHMAKE'
      denum = 0L
      if n_elements(c) EQ 0 then $
        c = 299792458d                ;; Speed of light in km/s
      if n_elements(au) EQ 0 then $
        au = 499.00478380613566287138 ;; AU in lt-sec
      jdlimits = [tstart, tstop]
      jdrows = nrows
  endif else begin
      nrows = info.nrows
      tstart = info.tstart & tstop = info.tstop
      timedel = info.timedel
      format = info.format
      denum = info.denum
      jdlimits = info.jdlimits
      jdrows = info.jdrows
      
      objname = info.objname
      ptr = info.ptr
      ncoeff = info.ncoeff
      nsub = info.nsub
      if author EQ '' then author = info.author
  endelse

  ;; Figure the number of subintervals.  If it's specified exactly,
  ;; well okay.  If this is an augmented ephemeris, then it may be
  ;; possible to assign the number of subintervals.
  if n_elements(ns0) EQ 0 then begin
      if fresh then begin
          ns = 1L
      endif else begin
          ns = round(timedel/(t(1)-t(0)))
      endelse
  endif else begin
      ns = round(ns0(0))
  endelse

  sz = size(cx)
  nc = sz(1)
  nr = sz(2) / ns

  xraw = transpose([[[cx]],[[cy]],[[cz]]], [0,2,1])
  xraw = reform(xraw,ns*nc*3L,nr, /overwrite) 

  c_km  = c/1000d
  au_km = au*c_km
  if n_elements(posunits) GT 0 then begin
      case strupcase(strtrim(posunits(0),2)) of
          'KM':   km = 1 ;; Dummy statement
          'CM':   xraw = xraw / 1d5   ;; Convert from CM to KM
          'AU':   xraw = xraw * au_km ;; Convert from AU to KM
          'LT-S': xraw = xraw * c_km  ;; Convert from LT-S to KM
          ELSE: message, 'ERROR: Unrecognized position units'
      endcase
  endif

  if fresh then begin
      objname = [strupcase(strtrim(obj(0),2))]
      ptr = [1L]
      ncoeff = [nc]
      nsub = [ns]

      raw = temporary(xraw)
  endif else begin

      if abs((t(1)-t(0))*ns - timedel) GT 1d-6 then begin
          message, 'ERROR: time sampling does not match'
      endif
      
      if abs(jdlimits(0) - min(t)) GT 1d-6 $
        OR abs(jdlimits(1) - max(t)) GT 1d-6 then begin
          message, 'ERROR: start and stop times do not match'
      endif

      if jdrows NE nr then begin
          message, 'ERROR: number of rows does not match'
      endif

      iprev = n_elements(ncoeff)-1
      objname = [objname, strupcase(strtrim(obj(0),2))]
      ptr = [ptr, max(ptr)+ncoeff(iprev)*nsub(iprev)*3L]
      ncoeff = [ncoeff, nc]
      nsub = [nsub, ns]
      
      szr = size(raw)
      newraw = make_array(szr(1)+nc*3, nr)
      newraw(0,0) = raw
      newraw(szr(1),0) = xraw
      raw = temporary(newraw)
  endelse

  if n_elements(newkeywords) EQ 0 then begin
      newkeywords = ['']
      newkeyvalues = ['']
  endif 
  if n_elements(keywords) EQ 0 then begin
      keywords = newkeywords
      keyvalues = newkeyvalues
  endif else if newkeywords(0) NE '' then begin
      keywords = [newkeywords, keywords]
      keyvalues = [newkeyvalues, keyvalues]
  endif

  auth = author
  info = {filename: '', edited: 1L, $
          creation_date: date, author: auth, $
          nrows: nrows, tstart: tstart, tstop: tstop, $
          timedel: timedel, format: format, denum: denum, $
          c: c, au: au, jdlimits: jdlimits, jdrows: jdrows, $
          objname: objname, ptr: ptr, ncoeff: ncoeff, $
          nsub: nsub, keywords: keywords, keyvalues: keyvalues}

  return
end

