;+
; NAME:
;   GEOGREAD
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Read gravity model from disk file
;
; MAJOR TOPICS:
;   Physics, Gravity, Geodesy, Spacecraft Navigation
;
; CALLING SEQUENCE:
;   GEOGREAD, ROOTFILE, MODEL [, STATUS=STATUS, ERRMSG=ERRMSG,
;                               /COEFF_ERR]
;
; DESCRIPTION:
;
;   GEOGREAD reads a gravity model from a disk file.  The gravity
;   model must have already been prepared.  There are a number of
;   freely available models.
;
;   Each model must have a "description" file which describes, in IDL
;   syntax, the name, content and format of the model file.  The
;   ROOTFILE parameter is the name of this description file.  The
;   description must provide the name of the model file (the FILENAME
;   field), which must reside in the same directory.
;
; FILE FORMAT:
;
;   The format of the description file (and hence also the format of
;   the structure returned in GEOGMOD), is as follows, an example
;   modified from egm96.desc:
;
; { $
;   name: 'EGM96', $              ;; Title of the model
;   type: 'GRAVITY', $            ;; Type of model 'GRAVITY' or 'BFIELD'
;   filename: 'EGM96.GEO', $      ;; Model coefficient file name (same dir)
;   reference: 'Lemoine, ...'     ;; Complete literature reference
;   url: 'ftp://ftp.csr.utexas.edu/pub/grav/EGM96.GEO.Z', $   ;; Source URL
;   nmax: 360L, $                 ;; Maximum order (inclusive)
;   mmax: 360L, $                 ;; Maximum degree (inclusive)
;   normalized: 1, $              ;; Coefficients are normalized (1=yes, 0=no)
;   mu: 398600.44150D+09, $       ;; GM for central body [m^3/s^2]
;   a: 6378136.30d, $             ;; Mean equatorial radius [m]
;   tide: 'ZERO', $               ;; Tide system (ZERO, FREE, or MEAN)
;   epoch: 1986.0d, $             ;; Epoch of model coefficients (Julian year)
;   C21: -.1869876359548955D-09,$ ;; C21 coefficient (if not in Cnm table)
;   S21:  .1195280120306540D-08,$ ;; S21 coefficient (if not in Cnm table)
;   C20_dot: 1.16275534D-11,$     ;; C20 rate (unitless; yr^-1)
;   C21_dot: -0.32d-11, $         ;; C21 rate (unitless; yr^-1)
;   S21_dot: +1.62d-11, $         ;; S21 rate (unitless; yr^-1)
;   rowstart: 4L, $               ;; Coefficient starting row (first row = 0)
;   nrows: 65338L, $              ;; Number of coefficient rows in file
;   ncolrange: [6,8], $           ;; Column range for degree (first col = 0)
;   mcolrange: [9,11], $          ;;    "     "    "  order 
;   Ccolrange: [12,30], $         ;;    "     "    "  C coefficients
;   Scolrange: [31,49], $         ;;    "     "    "  S coefficients
;   dCcolrange: [50,62], $        ;;    "     "    "  C std deviation
;   dScolrange: [63,75] $         ;;    "     "    "  S std deviation
; }
;
;   The xCOLRANGE fields describe which character columns in the model
;   file, inclusive, contain the quantity of interest.  You can use a
;   text editor which reports the column number to find these values.
;   Exclude any character columns that contain field delimiters such
;   as commas.
;
;   Since the C21 and S21 coefficients are commonly not included in
;   the table itself, their values are allowed to be specified in the
;   description file.  If the coefficients *are* in the table, then
;   they must be set to zero in the description file to avoid double
;   computations.  The coefficient rates can be used to extrapolate to
;   different epochs from the reference epoch (specified by EPOCH).
; 
;
; INPUTS:
;
;   ROOTFILE - scalar string, the name of the model description file.
;
;   GEOGMOD - upon return, an IDL structure containing the model
;             information.  In addition to the fields listed above,
;             other fields are appended which contain (pointers to)
;             the coefficient data, etc.
;
; KEYWORD PARAMETERS:
;
;   STATUS - upon return, a status indicator.  A value of 1 is OK, 0
;            indicates an error condition.
;
;   ERRMSG - upon return, an error message, if any.  If no error
;            occurred, then ERRMSG is set to ''.
;
;   CEOFF_ERR - if set, then coefficient standard deviations are also
;               read in.q
;
;
; EXAMPLE:
;   GEOGREAD, 'egm96', egm96
;   GEOGRAV, egm96, r, phi, a
;
;   Read the gravity model "EGM96" and evaluate it at position "R" in
;   body coordinates.  The potential and acceleration are returned in
;   PHI and A.
;
; MODIFICATION HISTORY:
;   Documentation additions, CM, 26 Sep 2004
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
pro geogread, rootfile, geogmod, status=status, errmsg=errmsg, $
              coeff_err=cerr

  status = 0
  errmsg = ''

  if n_params() EQ 0 then begin
      message, 'USAGE: GEOGREAD, ROOTFILE, GEOGMOD [, STATUS=, ERRMSG=]', /info
      return
  endif

  openr, unit, rootfile, /get_lun, error=err
  if err NE 0 then begin
      errmsg = 'ERROR: could not open '+rootfile
      return
  endif

  rootstr = ''
  on_ioerror, DONE_ROOTFILE
  while NOT eof(unit) do begin
      s = ''
      readf, unit, s
      s = strtrim(s,2)
      if s NE '' then begin
          p = strpos(s, '$')
          if p GE 0 then s = strtrim(strmid(s,0,p),2)
          p = strpos(s, ';')
          if p GE 0 then s = strtrim(strmid(s,0,p),2)

          if s NE '' then rootstr = rootstr + s
      endif
  endwhile
  DONE_ROOTFILE:
  free_lun, unit

  if rootstr EQ '' then begin
      errmsg = 'ERROR: no data found in '+rootfile
      return
  endif

  temp = strlowcase(rootstr)
  if strpos(temp,'execute') GE 0 OR strpos(temp,'call_') GE 0 $
    OR strpos(temp,'spawn') GE 0 OR strpos(temp,'openw') GE 0 then begin
      errmsg = 'ERROR: '+rootfile+' contains insecure commands'
      return
  endif
  
  cmdstr = 'geogmod = '+rootstr
  dummy = execute(cmdstr)
  if dummy NE 1 then begin
      errmsg = 'ERROR: could not parse structure in '+rootfile
      return
  endif
  sz = size(geogmod) 
  if sz(sz(0)+1) NE 8 then begin
      errmsg = 'ERROR: structure not found in '+rootfile
      return
  endif

  tn = strlowcase(tag_names(geogmod))
  if max(tn EQ 'filename') EQ 0 OR max(tn EQ 'type') EQ 0 OR $
    max(tn EQ 'a') EQ 0 OR max(tn EQ 'mu') EQ 0 OR $
    max(tn EQ 'nmax') EQ 0 OR max(tn EQ 'mmax') EQ 0 OR $
    max(tn EQ 'nrows') EQ 0 OR max(tn EQ 'rowstart') EQ 0 OR $
    max(tn EQ 'ncolrange') EQ 0 OR max(tn EQ 'mcolrange') EQ 0 OR $
    max(tn EQ 'ccolrange') EQ 0 OR max(tn EQ 'scolrange') EQ 0 then begin
      errmsg = 'ERROR: structure did not contain required fields in '+rootfile
      return
  endif

  ps = path_sep()

  p = rstrpos(rootfile, ps)
  if p GE 0 then path = strmid(rootfile,0,p+1) $
  else           path = ''

  file = path + geogmod.filename

  openr, unit, file, /get_lun, error=err
  if err NE 0 then begin
      errmsg = 'ERROR: could not open '+file
      return
  endif

  nrows = geogmod.nrows

  reading = 1
  on_ioerror, DONE_READING
  str = strarr(geogmod.rowstart)
  readf, unit, str

  str = strarr(nrows)
  readf, unit, str
  reading = 0
  
  DONE_READING:
  free_lun, unit
  if reading then begin
      errmsg = 'ERROR: could not read coefficient rows from '+file
      return
  endif

  rng = geogmod.ncolrange & n = long(strmid(str,rng(0),rng(1)-rng(0)+1))
  rng = geogmod.mcolrange & m = long(strmid(str,rng(0),rng(1)-rng(0)+1))

  rng = geogmod.Ccolrange & C = double(strmid(str,rng(0), rng(1)-rng(0)+1))
  rng = geogmod.Scolrange & S = double(strmid(str,rng(0), rng(1)-rng(0)+1))

  if keyword_set(cerr) then begin
      rng = geogmod.dCcolrange & dC = double(strmid(str,rng(0), rng(1)-rng(0)+1))
      rng = geogmod.dScolrange & dS = double(strmid(str,rng(0), rng(1)-rng(0)+1))
  endif
  str = 0
  
  Cnm = dblarr(geogmod.nmax+1,geogmod.mmax+1)
  Snm = Cnm

  if keyword_set(cerr) then begin
      dCnm = Cnm
      dSnm = Cnm
      dCnm(n,m) = temporary(dC) & dSnm(n,m) = temporary(dS)
  endif

  Cnm(0,0) = 1
  Cnm(n,m) = temporary(C)  &  Snm(n,m) = temporary(S)

  geogmod = create_struct(geogmod, $
                          'Cnm', ptr_new(Cnm, /no_copy), $
                          'Snm', ptr_new(Snm, /no_copy))
  if keyword_set(cerr) then begin
      geogmod = create_struct(geogmod, $
                              'dCnm', ptr_new(dCnm, /no_copy), $
                              'dSnm', ptr_new(dSnm, /no_copy))
  endif

  return
end
