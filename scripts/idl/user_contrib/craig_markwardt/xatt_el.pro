;+
; NAME:
;   XATT_EL
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Compute X-ray attenuation coefficient from NIST tables
;
; MAJOR TOPICS:
;   Physics
;
; CALLING SEQUENCE:
;   MU_RHO = XATT_EL(ELEMENT, ENERGY)
;
; DESCRIPTION:
;
;  This function computes the X-ray mass absorption coefficient for a
;  given element, based on the NIST mass absorption coefficients.
;  These tables are most well sampled for energies between 1 keV and
;  100 MeV.  These coefficients are from the tables at
;
;     http://physics.nist.gov/PhysRefData/XrayMassCoef/cover.html
;
;  The user specifies the atomic elements, either by the atomic symbol
;  (as a character string), or by the atomic number (Z).  Values are
;  interpolated in log-log space.
;
;  For a compound or mixture, the ELEMENT parameter can be an array.
;  The elements are weighted according to the WEIGHTS keyword.
;  According to the NIST documentation, the elements should be
;  weighted in proportion to their masses.
;
;  The energy is specified as either a scalar value or an array, and
;  the same number of absorption coefficients are returned.
;
;  The transmission of a medium is then defined by:
;
;     TRANS = EXP(- MU_RHO * RHO * THICKNESS )
;
;  where MU_RHO is the value returned by this function (cm^2/g), RHO
;  is the mass density of the medium (g/cm^3), and THICKNESS is the
;  thickness of the material (cm).
;
;  The data are stored within this function.  The routine should be in
;  the user's path so that the data can be located.
;
;
; INPUTS:
;
;   EL - scalar or vector quantity specifying atomic elements.  Either
;        a string giving the atomic symbol, or an integer giving the
;        atomic number, Z.  A vector indicates a compound/mixture
;        consisting of multiple elements to be combined.
;
;   ENERGY - real scalar or array, gives the energy in keV of the
;            X-ray being absorbed.
;
; KEYWORDS:
;
;   WEIGHTS - an array of weights to be used in combining multiple
;             elements.  Elements should be combined in proportion to
;             their masses or number, according to the 'BY' keyword.
;             Weights are normalized to unity if necessary.
;             Default: equal weight to each element
;
;   BY - indicates how multiple elements weighting factors are to be
;        used.  If BY='WEIGHT' or BY='MASS', the WEIGHTS are assumed
;        to be the proportion of each element by mass.  If BY='NUMBER',
;        the WEIGHTS are assumed to be the number of atoms of each
;        element.  The atomic weight of each element is assumed to be
;        the mean atomic weight, averaged over the natural isotopic
;        abundences (See NIST web page
;        http://physics.nist.gov/PhysRefData/Compositions/).
;        Default: 'WEIGHT'
;
;   ATTENTYPE - type of scattering cross section to compute.  One of
;               the following:
;                   0 = coherent scattering
;                   1 = incoherent scattering
;                   2 = photoelectric absorption
;                   3 = pair production by nuclear field
;                   4 = pair production by electron field
;                   5 = total attenuation/scattering cross section
;                   6 = total non-coherent scattering cross section
;               NOTE: ATTENTYPE always overrides the shortcut keywords
;               below.
;               Default: 5 
;
;   COHERENT_SCATTERING   - if set, equivalent to ATTENTYPE=0
;   INCOHERENT_SCATTERING - if set, equivalent to ATTENTYPE=1
;   PHOTOELECTRIC         - if set, equivalent to ATTENTYPE=2
;   PAIR_NUCLEAR          - if set, equivalent to ATTENTYPE=3
;   PAIR_ELECTRON         - if set, equivalent to ATTENTYPE=4
;   TOTAL                 - if set, equivalent to ATTENTYPE=5
;   NO_COHERENT           - if set, equivalent to ATTENTYPE=6
; 
;
; RETURNS:
;
;   MU_RHO - the mass attenuation coefficient, with units of cm^2/g.
;
;
; EXAMPLE:
;
;   MU_RHO = XATT_EL('Al', [20.,30,40])
;
;     compute coefficient for Aluminum at 20, 30 and 40 keV.
;
;   MU_RHO = XATT_EL(47, 60)
;
;     compute coefficient for Silver (Z=47) at 60 keV.
;
;   MU_RHO = XATT_EL(['H','O'], WEIGHTS=[2,1], BY='NUMBER', 60)
;   
;     compute coefficient for water (H2O) at 60 keV.
;
; REFERENCES:
;
;   Hubbell, J.H. and Seltzer, S.M. (2004), 
;     Tables of X-Ray Mass Attenuation Coefficients and Mass
;     Energy-Absorption Coefficients (version 1.4). 
;     [Online] Available: http://physics.nist.gov/xaamdi 
;     National Institute of Standards and Technology, Gaithersburg, MD.
;
;     Originally published as NISTIR 5632, National Institute of
;     Standards and Technology, Gaithersburg, MD (1995).
;
;   Coursey, J. S., Schwab, D. J. and Dragoset, R. A.
;     Atomic Weights and Isotopic Compositions (with Relative Atomic
;       Masses)
;     Web page http://physics.nist.gov/PhysRefData/Compositions/
;     Visited March 2006
;
; SEE ALSO:
;
;
; MODIFICATION HISTORY:
;   Written, 17 Jul 2002, CM
;   Documented, 24 Jul 2002, CM
;   Changed to more robust (?) way to determine path, 24 Jan 2003, CM
;   Use ROUTINE_INFO to determine path, 10 Mar 2003, CM
;   Add ability to process compounds and mixtures, 30 Jun 2005, CM
;   Completely revamp to handle the full XCOM cross section list, 19
;     Jul 2005, CM
;   Add the BY keyword for handling molecules by atomic number instead
;     of weight, 16 May 2006
;
;  $Id$
;
;-
; Copyright (C) 2002,2003,2005, 2006, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

function xatt_el_encode_float, strs
  bbase = (byte('@'))(0)
  bzero = (byte('0'))(0)

  bb = (byte(strs) - bzero) AND 15b

  b0 = (bb(0,*) OR ishft(bb(2,*) AND 3b,4))  + bbase
  b1 = (bb(3,*) OR ishft(bb(2,*) AND 12b,2)) + bbase

  ;; Encode the exponent
  ex = fix(strmid(strs,5,3))
  exb = bbase + byte(ex+18)

  return, string(transpose([[b0(*)],[b1(*)],[exb(*)]]))
end

function xatt_el_decode_float, strs
  bbase = (byte('@'))(0)
  bzero = (byte('0'))(0)

  tens = 10.^(lindgen(32)-18-2)

  bb = (byte(strs) - bbase) AND 63b
  
  b0 = (bb(0,*) AND 15b)
  b1 = (ishft(bb(0,*),-4) AND 3b) OR (ishft(bb(1,*),-2) AND 12b)
  b2 = (bb(1,*) AND 15b)

  ex = bb(2,*)

  flt = (b0*100 + b1*10 + b2)*tens(ex)
  return, flt(*)
end


pro xatt_el_parse, encode_file=encode_file, decode_file=decode_file
  common xatt_el_common, el, en, mu, ind, elsym, amass
  forward_function filepath

  if keyword_set(decode_file) then begin
      file = decode_file
  endif else begin
      file = 'xatt_el.pro' 
  endelse

  get_lun, unit
  openr, unit, file, error=err
  if err NE 0 then begin
      paths = routine_info('xatt_el', /functions, /source)
      fname = paths.path

      openr, unit, fname, error=err
      if err NE 0 then begin
          free_lun, unit
          message, 'ERROR: could not find attenuation data from xatt_el.pro'
      endif

  endif

  FOUND_FILE:
  el = [0l] & en = [0d] & mu = [0d]
  reading = 0
  ss = strarr(20000)
  on_ioerror, DONE_IO
  readf, unit, ss
DONE_IO:
  cc = (fstat(unit)).transfer_count
  free_lun, unit
  if cc LT 9392 then begin
      message, 'ERROR: could not read xatt_el.pro'
  endif

  wh1 = where(strpos(ss,';====  START TABLE') EQ 0, ct1)
  wh2 = where(strpos(ss,';====  END TABLE')   EQ 0, ct2)
  wh1 = wh1(0)
  wh2 = wh2(0)

  if (ct1 EQ 0 OR ct2 EQ 0) then begin
      BAD_FORMAT:
      message, 'ERROR: the file xatt_el.pro did not contain table data'
  endif

  ;; If the output is encoded
  encoded = (strpos(ss(wh1),'ENCODED') GE 0)

  ss = ss(wh1+1:wh2-1)
  wh = where(strmid(ss,0,6) NE ';==== ', ct)
  if ct EQ 0 then goto, BAD_FORMAT

  if keyword_set(encode_file) then begin
      if encoded then message, 'ERROR: input is already encoded'
      ss1 = ss(wh)
      outstr = strmid(ss1,0,17)
      
      outstr = outstr + xatt_el_encode_float(strmid(ss1,17,8))
      outstr = outstr + xatt_el_encode_float(strmid(ss1,26,8))
      outstr = outstr + xatt_el_encode_float(strmid(ss1,35,8))
      outstr = outstr + xatt_el_encode_float(strmid(ss1,44,8))
      outstr = outstr + xatt_el_encode_float(strmid(ss1,53,8))
      outstr = outstr + xatt_el_encode_float(strmid(ss1,62,8))
      outstr = outstr + xatt_el_encode_float(strmid(ss1,71,8))

      ss(wh) = outstr

      openw, unit, encode_file, /get_lun
      printf, unit, ';====  START TABLE (ENCODED)'
      printf, unit, ss, format='(A)'
      printf, unit, ';====  END TABLE'
      free_lun, unit

      return
  endif

  ss = ss(wh)

;         1         2         3         4         5         6         7
;123456789012345678901234567890123456789012345678901234567890123456789012345678
; 1    1.000E-03 3.47E-01 5.03E-02 6.82E+00 0.00E+00 0.00E+00 7.21E+00 6.87E+00

  el = round(fix(strmid(ss,1,2)))
  ed = strtrim(strmid(ss,4,2))
  en = float(strmid(ss,7,9))*1000
  if NOT encoded then begin
      mu_cohscat = float(strmid(ss,17,8))
      mu_incscat = float(strmid(ss,26,8))
      mu_photo   = float(strmid(ss,35,8))
      mu_pairnuc = float(strmid(ss,44,8))
      mu_paire   = float(strmid(ss,53,8))
      mu_total   = float(strmid(ss,62,8))
      mu_totxcoh = float(strmid(ss,71,8))
  endif else begin
      mu_cohscat = xatt_el_decode_float(strmid(ss,17,3))
      mu_incscat = xatt_el_decode_float(strmid(ss,20,3))
      mu_photo   = xatt_el_decode_float(strmid(ss,23,3))
      mu_pairnuc = xatt_el_decode_float(strmid(ss,26,3))
      mu_paire   = xatt_el_decode_float(strmid(ss,29,3))
      mu_total   = xatt_el_decode_float(strmid(ss,32,3))
      mu_totxcoh = xatt_el_decode_float(strmid(ss,35,3))
  endelse
  
  mu = transpose([[mu_cohscat],[mu_incscat],[mu_photo],[mu_pairnuc],$
                  [mu_paire],[mu_total],[mu_totxcoh]])

  ind = lonarr(2,max(el)+1)
  for i = 0, max(el) do begin
      ii = where(el EQ i)
      ind(*,i) = [min(ii), max(ii)]
  endfor

  elsym = ['','H', 'He','Li','Be','B','C', 'N', 'O', 'F', 'Ne', 'Na', 'Mg', $
           'Al', 'Si', 'P', 'S', 'Cl', 'Ar', 'K', 'Ca', 'Sc', 'Ti', 'V', $
           'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', 'Ga', 'Ge', 'As', 'Se', $
           'Br', 'Kr', 'Rb', 'Sr', 'Y', 'Zr', 'Nb', 'Mo', 'Tc', 'Ru', 'Rh', $
           'Pd', 'Ag', 'Cd', 'In', 'Sn', 'Sb', 'Te', 'I', 'Xe', 'Cs', 'Ba']
  elsym = [elsym, $
           'La', 'Ce', 'Pr', 'Nd', 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', $
           'Er', 'Tm', 'Yb', 'Lu', 'Hf', 'Ta', 'W', 'Re', 'Os', 'Ir', 'Pt', $
           'Au', 'Hg', 'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', $
           'Th', 'Pa', 'U', 'Np', 'Pu', 'Am']

  ;; Atomic weights and isotopic compositions
  ;; http://physics.nist.gov/PhysRefData/Compositions/
  amass = dblarr(256)
  amass(*) = !values.d_nan

  amass(1)  = 1.00794
  amass(2)  = 4.002602
  amass(3)  = 6.941
  amass(4)  = 9.012182
  amass(5)  = 10.811
  amass(6)  = 12.0107
  amass(7)  = 14.0067
  amass(8)  = 15.9994
  amass(9)  = 18.9984032
  amass(10) = 20.1797
  amass(11) = 22.989770
  amass(12) = 24.3050
  amass(13) = 26.981538
  amass(14) = 28.0855
  amass(15) = 30.973761
  amass(16) = 32.065
  amass(17) = 35.453
  amass(18) = 39.948
  amass(19) = 39.0983
  amass(20) = 40.078
  amass(21) = 44.955910
  amass(22) = 47.867
  amass(23) = 50.9415
  amass(24) = 51.9961
  amass(25) = 54.938049
  amass(26) = 55.845
  amass(27) = 58.933200
  amass(28) = 58.6934
  amass(29) = 63.546
  amass(30) = 65.409
  amass(31) = 69.723
  amass(32) = 72.64
  amass(33) = 74.92160
  amass(34) = 78.96
  amass(35) = 79.904
  amass(36) = 83.798
  amass(37) = 85.4678
  amass(38) = 87.62
  amass(39) = 88.90585
  amass(40) = 91.224
  amass(41) = 92.90638
  amass(42) = 95.94
  amass(44) = 101.07
  amass(45) = 102.90550
  amass(46) = 106.42
  amass(47) = 107.8682
  amass(48) = 112.411
  amass(49) = 114.818
  amass(50) = 118.710
  amass(51) = 121.760
  amass(52) = 127.60
  amass(53) = 126.90447
  amass(54) = 131.293
  amass(55) = 132.90545
  amass(56) = 137.327
  amass(57) = 138.9055
  amass(58) = 140.116
  amass(59) = 140.90765
  amass(60) = 144.24
  amass(62) = 150.36
  amass(63) = 151.964
  amass(64) = 157.25
  amass(65) = 158.92534
  amass(66) = 162.500
  amass(67) = 164.93032
  amass(68) = 167.259
  amass(69) = 168.93421
  amass(70) = 173.04
  amass(71) = 174.967
  amass(72) = 178.49
  amass(73) = 180.9479
  amass(74) = 183.84
  amass(75) = 186.207
  amass(76) = 190.23
  amass(77) = 192.217
  amass(78) = 195.078
  amass(79) = 196.96655
  amass(80) = 200.59
  amass(81) = 204.3833
  amass(82) = 207.2
  amass(83) = 208.98038
  amass(90) = 232.0381
  amass(91) = 231.03588
  amass(92) = 238.02891

  return
end

function xatt_el_value_locate, xbins, x
  nbins = n_elements(xbins)
  sz = size(xbins)

  ;; The values are computed by spline interpolation.  Here is the "y"
  ;; value of the spline, which is just the bin position.
  tp = sz(sz(0)+1)
  if tp EQ 1 OR tp EQ 2 OR tp EQ 12 then begin
      yy = findgen(nbins) - 0.5
      eps = (machar()).eps
  endif else begin
      yy = dindgen(nbins) - 0.5D
      eps = (machar(/double)).eps
  endelse      

  ii = round(spl_interp(xbins, yy, yy*0, x) + eps)
  ii = ii > (-1L) < (nbins-1)

  return, ii
end

pro xatt_el_tabinv, XARR, X, IEFF
  Npoints = N_elements(xarr) & npt= npoints - 1
  ieff = float(xatt_el_value_locate(xarr,x))
  g = where( (ieff LT npt) and (ieff GE 0), Ngood)
  if Ngood GT 0 then begin
      neff = ieff(g)
      diff = x(g) - xarr(neff) + 0.
      ieff(g) = neff +  diff / (xarr(neff+1) - xarr(neff) ) 
  endif
  return
end

pro xatt_el_linterp, Xtab, Ytab, Xint, Yint
  xatt_el_tabinv, xtab, xint, r
  yint = interpolate(ytab, r)
  return
end

function xatt_el, ellist, e, weights=wtlist0, by=by0, $
                  attentype=attentype0, $
                  coherent_scattering=cohscat, $
                  incoherent_scattering=incscat, $
                  photoelectric=photoel, $
                  pair_nuclear=pairnuc, $
                  pair_electron=paire, $
                  total=totatten, no_coherent=totnocoh

  common xatt_el_common, el, en, mu, ind, elsym, amass

  if n_elements(el) EQ 0 then xatt_el_parse
  if n_elements(by0) EQ 0 then by = 'WEIGHT' $
  else by = strupcase(strtrim(by0(0),2))

  if by NE 'WEIGHT' AND by NE 'MASS' AND by NE 'NUMBER' then $
    message, 'ERROR: the keyword BY must be one of "WEIGHT" or "NUMBER"'

  if n_elements(attentype0) GT 0 then attentype = round(attentype0(0)) $
  else begin
      if keyword_set(cohscat) then attentype = 0
      if keyword_set(incscat) then attentype = 1
      if keyword_set(photoel) then attentype = 2
      if keyword_set(pairnuc) then attentype = 3
      if keyword_set(paire)   then attentype = 4
      if keyword_set(totatten) then attentype = 5
      if keyword_set(totnocoh) then attentype = 6
      if n_elements(attentype) EQ 0 then attentype = 5  ;; Default TOTAL
  endelse

  sz = size(ellist)
  if n_elements(wtlist0) EQ 0 then begin
      wtlist = dblarr(n_elements(ellist))*0 + 1
  endif else begin
      wtlist = double(wtlist0)
  endelse

  elnum = lonarr(n_elements(ellist))
  if sz(sz(0)+1) NE 7 then begin
      elnum(*) = round(ellist)
  endif else begin
      for i = 0, n_elements(ellist)-1 do begin
          el0 = ellist(i)
          el1 = where(el0 EQ elsym, ct)
          if ct EQ 0 then $
            message, 'ERROR: Element '+el0(0)+' not found'
          elnum(i) = el1(0)
      endfor
  endelse

  if n_elements(wtlist) NE n_elements(ellist) then $
    message, 'ERROR: number of weights does not match number of elements'

  ;; If the user specified the composition by number, then convert to
  ;; a by-weight value by multiplying by the atomic weight for each
  ;; species.
  if by EQ 'NUMBER' then wtlist = wtlist * amass(elnum)

  ;; Normalize the composition weights
  wtlist = wtlist / total(wtlist)

  result = 0
  for i = 0, n_elements(ellist)-1 do begin

      el1 = elnum(i)
      
      en1 = en(ind(0,el1):ind(1,el1))
      mu1 = mu(attentype,ind(0,el1):ind(1,el1))
      mu1 = mu1(*)

      xatt_el_linterp, alog10(en1), alog10(mu1>1e-37), alog10(e), x
      resulti = 10d^x
      wh = where(resulti LT 2e-37, ct)
      if ct GT 0 then resulti(wh) = 0
      
      result = result + resulti * wtlist(i)
  endfor
  
  return, result
end

; ====================================================================
; Here is the cross section data.  It is arranged by element number,
; and then by energy.  The ENCODED table is not encoded for security
; reasons, but rather to make the file smaller in size. See above
; for the encoding strategy.
; ====================================================================
; Column layout
; EL  |ENERGY   |Encoded Data
; EL - atomic number
; ENERGY - energy in MeV
; Encoded data - cross section data
; ====================================================================


;====  START TABLE (ENCODED)
;==== ELEMENT  1
; 1    1.000E-03 CWQECPFbR@@R@@RgARFgR
; 1    1.500E-03 RhQIfPqUR@@R@@RRERAeR
; 1    2.000E-03 BWQAXQfTQ@@R@@RAFRXBQ
; 1    3.000E-03 aUQbHQaXQ@@R@@ReQQSfQ
; 1    4.000E-03 QBQrYQfIP@@R@@RTUQCRQ
; 1    5.000E-03 HAPS@QRcP@@R@@RTIQsIQ
; 1    6.000E-03 UgPcIQQWP@@R@@RDDQCTQ
; 1    8.000E-03 cVPCYQEfO@@R@@RSaQSUQ
; 1    1.000E-02 BVPSXQrRO@@R@@RCeQcQQ
; 1    1.500E-02 QFPcTQvTN@@R@@RsVQcUQ
; 1    2.000E-02 fYOcSQRPN@@R@@RcYQcSQ
; 1    3.000E-02 CBOSTQVGM@@R@@RSWQSTQ
; 1    4.000E-02 qQOCTQbHM@@R@@RCVQCTQ
; 1    5.000E-02 Q@OsDQAFM@@R@@RsFQsDQ
; 1    6.000E-02 gUNcEQeUL@@R@@RcFQcEQ
; 1    8.000E-02 tANCIQR@L@@R@@RCIQCIQ
; 1    1.000E-01 rVNRdQIbK@@R@@RRdQRdQ
; 1    1.500E-01 aCNbUQRPK@@R@@RbUQbUQ
; 1    2.000E-01 VaMBSQiSJ@@R@@RBSQBSQ
; 1    3.000E-01 CGMRAQbTJ@@R@@RRAQRAQ
; 1    4.000E-01 qSMAiQQAJ@@R@@RAiQAiQ
; 1    5.000E-01 QAMqSQUcI@@R@@RqSQqSQ
; 1    6.000E-01 gXLaPQcXI@@R@@RaPQaPQ
; 1    8.000E-01 tBLAPQAfI@@R@@RAPQAPQ
; 1    1.000E+00 rVLaFQQGI@@R@@RaFQaFQ
; 1    1.022E+00 bULaEQQBI@@R@@RaEQaEQ
; 1    1.250E+00 qWLQCQwYHdVL@@RQCQQCQ
; 1    1.500E+00 aCLACQEaHbRM@@RACQACQ
; 1    2.000E+00 VaKxVPsPHAEN@@RxWPxWP
; 1    2.044E+00 fQKhUPSYHQCN@@RhVPhVP
; 1    3.000E+00 CGKFiPRAHCBNBQMVbPVbP
; 1    4.000E+00 qSKuUPAVHT`NIdMEaPEaP
; 1    5.000E+00 QAKTfPQAHVXNQfNEEPEEP
; 1    6.000E+00 gXJtIPIAGX@NCANTPPTPP
; 1    7.000E+00 eTJSdPWUGITNDFNDHPDHP
; 1    8.000E+00 tBJSYPVPGAGOEHNsUPsUP
; 1    9.000E+00 CQJs@PuPGQHOFFNCXPCXP
; 1    1.000E+01 rVJCFPEHGaHOViNcEPcEP
; 1    1.100E+01 bHJBePTXGqGOGgNCGPCGP
; 1    1.200E+01 QbJbWPTGGAUOxQNRaPRaP
; 1    1.300E+01 aTJRRPCbGQSOYPNrWPrWP
; 1    1.400E+01 AQJrHPSSGaQOACObUPbUP
; 1    1.500E+01 aCJbFPcHGaWOQ@ORTPRTP
; 1    1.600E+01 AHJREPCGGqTOQGOBUPBUP
; 1    1.800E+01 XSIQgPrQGAfOaIObHPbHP
; 1    2.000E+01 VaIAbPBRGQgOAQOREPREP
; 1    2.200E+01 uQIaYPRIGBGOQROBDPBDP
; 1    2.400E+01 D`IQXPB@GRFOaROQePQeP
; 1    2.600E+01 DIIAXPAdGbDOqQOAgPAgP
; 1    2.800E+01 SRIAPPqQGrBOA`OAaPAaP
; 1    3.000E+01 CGIqBPQYGrIOAhOqUPqUP
; 1    4.000E+01 qSIAEPQHGbYObBOQTPQTP
; 1    5.000E+01 QAIxWOIRFRcOBYOARPARP
; 1    6.000E+01 gWHWUOGcFSBOrROqDPqDP
; 1    8.000E+01 tAHUfOEeFCROCGOaEPaEP
; 1    1.000E+02 rVHTdOdWFcUOsDOQIPQIP
; 1    1.500E+02 aCHSROS@FDEOCcOQDPQDP
; 1    2.000E+02 V`GrVOrBFtAOTGOQBPQBP
; 1    3.000E+02 CGGQfOQUFdSOdROQBPQBP
; 1    4.000E+02 qSGQSOQFFDcOTaOQCPQCP
; 1    5.000E+02 Q@GaGOiFETfOUBOQDPQDP
; 1    6.000E+02 gWFAIOwQEEFOeHOQDPQDP
; 1    8.000E+02 tAFHXNuXEUIOUQOQEPQEP
; 1    1.000E+03 rVFVhNdSEeGOeWOQFPQFP
; 1    1.500E+03 aCFDgNCHEEPOUaOQHPQHP
; 1    2.000E+03 V`EsWNrAEEWOFEOQIPQIP
; 1    3.000E+03 CGEbRNQTEUTOf@Oa@Pa@P
; 1    4.000E+03 qSEBBNQFEUYOfIOaAPaAP
; 1    5.000E+03 Q@EaVNiEDeQOvEOaAPaAP
; 1    6.000E+03 gWDAPNwQDeSOvIOaBPaBP
; 1    8.000E+03 tADAHNuXDeVOFUOaBPaBP
; 1    1.000E+04 rVDHcMdRDeWOFXOaBPaBP
; 1    1.500E+04 aCDV@MCHDeYOVSOaCPaCP
; 1    2.000E+04 V`CdYMrADuQOVUOaCPaCP
; 1    3.000E+04 CGCcDMQTDuROVXOaCPaCP
; 1    4.000E+04 qSCBXMQFDuROfPOaDPaDP
; 1    5.000E+04 Q@CBBMiDCuSOfQOaDPaDP
; 1    6.000E+04 gWBqQMwPCuSOfROaDPaDP
; 1    8.000E+04 tABqAMuXCuSOfSOaDPaDP
; 1    1.000E+05 rVBAGMdRCuTOfSOaDPaDP
;==== ELEMENT  2
; 2    1.000E-03 sYQABPFDS@@R@@RFHSFDS
; 2    1.500E-03 SUQRCPaTS@@R@@RaXSaTS
; 2    2.000E-03 cEQCYPVPR@@R@@RFfRVTR
; 2    3.000E-03 bSQvFPaXR@@R@@RBARqTR
; 2    4.000E-03 BFQXePvGQ@@R@@RyCQgGQ
; 2    5.000E-03 aRQQ@QCEQ@@R@@RuWQTEQ
; 2    6.000E-03 aHQaFQaUQ@@R@@Rd@QRbQ
; 2    8.000E-03 HSPAWQVHP@@R@@RRcQBIQ
; 2    1.000E-02 UaPQYQRbP@@R@@RBXQAiQ
; 2    1.500E-02 RePqRQwCO@@R@@RBIQA`Q
; 2    2.000E-02 qVPqVQrUO@@R@@RQfQqXQ
; 2    3.000E-02 XHOqUQFgN@@R@@RAdQqVQ
; 2    4.000E-02 dYOqQQRWN@@R@@RqVQqRQ
; 2    5.000E-02 CCOaWQa@N@@R@@RqPQaWQ
; 2    6.000E-02 RAOaSQFQM@@R@@RaUQaSQ
; 2    8.000E-02 QIOQUQBPM@@R@@RQVQQUQ
; 2    1.000E-01 gVNAXQQCM@@R@@RAYQAXQ
; 2    1.500E-01 CQNqCQBhL@@R@@RqDQqCQ
; 2    2.000E-01 QbNaBQQBL@@R@@RaBQaBQ
; 2    3.000E-01 XTMAFQCHK@@R@@RAFQAFQ
; 2    4.000E-01 DaMYSPq@K@@R@@RYSPYSP
; 2    5.000E-01 CHMxPPVgJ@@R@@RxQPxPP
; 2    6.000E-01 RDMHEPtCJ@@R@@RHEPHEP
; 2    8.000E-01 a@MGGPRHJ@@R@@RGHPGGP
; 2    1.000E+00 gYLvFPqFJ@@R@@RvFPvFP
; 2    1.022E+00 wFLfIPaEJ@@R@@RfIPfIP
; 2    1.250E+00 TbLeYPXRItPL@@ReYPeYP
; 2    1.500E+00 CRLUGPfCIbTM@@RUGPUGP
; 2    2.000E+00 QbLDQPSfIAFN@@RDRPDRP
; 2    2.044E+00 AdLtFPCdIQDN@@RtGPtGP
; 2    3.000E+00 XUKCWPbDICDNaAMSPPSPP
; 2    4.000E+00 DaKBiPQUITdNTfMRePReP
; 2    5.000E+00 CHKRPPQIIfSNIhMRXPRXP
; 2    6.000E+00 RDKbAPYWHXENQRNrAPrAP
; 2    7.000E+00 QWKQiPHBHYQNBENR@PR@P
; 2    8.000E+00 a@KAaPV`HAGORVNQdPQdP
; 2    9.000E+00 YPJaVPFEHQIOCENAaPAaP
; 2    1.000E+01 gYJQTPuIHaIOSRNqPPqPP
; 2    1.100E+01 vFJATPDfHqHOSfNaQPaQP
; 2    1.200E+01 uDJqEPDRHAVOtINQTPQTP
; 2    1.300E+01 TUJaGPDEHQTOtXNAWPAWP
; 2    1.400E+01 SbJa@PsTHaROUFNAQPAQP
; 2    1.500E+01 CRJQDPCXHaYOURNqFPqFP
; 2    1.600E+01 C@JAHPcEHqUOEgNqBPqBP
; 2    1.800E+01 rGJYbOBgHAgOVQNaDPaDP
; 2    2.000E+01 QbJYDORWHQhOW@NQHPQHP
; 2    2.200E+01 QYJHYOrBHBHOgTNQCPQCP
; 2    2.400E+01 qDJWdORBHRGOXDNAIPAIP
; 2    2.600E+01 QDJGUOQeHbEOhQNAFPAFP
; 2    2.800E+01 IaIGCOAaHrCOIENACPACP
; 2    3.000E+01 XUIfVOaXHBPOIVNA@PA@P
; 2    4.000E+01 DaIeIOaEHrQOQBOYAOYAO
; 2    5.000E+01 CHIDROYgGRdOaFOhQOhQO
; 2    6.000E+01 RDICaOhHGSBOqGOx@Ox@O
; 2    8.000E+01 a@IC@OVIGCQOQTOWeOWeO
; 2    1.000E+02 gYHBYOTdGcROaXOwXOwXO
; 2    1.500E+02 CRHqWOcHGSfOQaOgUOgUO
; 2    2.000E+02 QbHqIOBVGTHOBGOgSOgSO
; 2    3.000E+02 XTGIfNaTGDSObGOgXOgXO
; 2    4.000E+02 DaGwRNaCGTXOBPOwTOwTO
; 2    5.000E+02 CHGvINI`FdWOBXOwYOwYO
; 2    6.000E+02 RDGEWNXFFtTORUOGdOGdO
; 2    8.000E+02 a@GdGNVBFDdObTOW`OW`O
; 2    1.000E+03 gYFSQNDiFT`OrPOWeOWeO
; 2    1.500E+03 CRFBVNcFFTiOrYOHBOHBO
; 2    2.000E+03 QbFQ`NBUFECOBdOHFOHFO
; 2    3.000E+03 XTEqBNaSFEIOR`OXBOXBO
; 2    4.000E+03 DaEABNaBFUBORcOXEOXEO
; 2    5.000E+03 CHExDMyXEUDOReOXGOXGO
; 2    6.000E+03 RDEGGMXEEUEORgOXHOXHO
; 2    8.000E+03 a@EEUMVAEUFORiOh@Oh@O
; 2    1.000E+04 gYDDUMDiEUGOC@OhBOhBO
; 2    1.500E+04 CRDCGMcFEUIOCAOhCOhCO
; 2    2.000E+04 QbDrFMBTEe@OCBOhDOhDO
; 2    3.000E+04 XTCaSMaSEe@OCCOhEOhEO
; 2    4.000E+04 DaCaEMaBEeAOCDOhFOhFO
; 2    5.000E+04 CHCABMyXDeAOCDOhFOhFO
; 2    6.000E+04 RDChQLXEDeAOCDOhFOhFO
; 2    8.000E+04 a@CfPLVADeAOCEOhGOhGO
; 2    1.000E+05 gYBuGLDiDeAOCEOhGOhGO
;==== ELEMENT  3
; 3    1.000E-03 TAQCHPrCT@@R@@RrDTrCT
; 3    1.500E-03 CRQTWPfSS@@R@@RfWSfSS
; 3    2.000E-03 RbQUSPbWS@@R@@RrQSbXS
; 3    3.000E-03 rBQVaPgER@@R@@RWURwBR
; 3    4.000E-03 QdQXBPBdR@@R@@RSARRbR
; 3    5.000E-03 aTQiBPqFR@@R@@RaRRAVR
; 3    6.000E-03 qIQABQGVQ@@R@@RIhQHYQ
; 3    8.000E-03 AAQQHQBgQ@@R@@REEQDDQ
; 3    1.000E-02 GXPaIQqFQ@@R@@RCPQbUQ
; 3    1.500E-02 DAPASQCXP@@R@@RRHQqWQ
; 3    2.000E-02 BWPAXQqBP@@R@@RAfQaQQ
; 3    3.000E-02 a@PAYQsCO@@R@@RaTQQRQ
; 3    4.000E-02 GAOAWQaEO@@R@@RQUQAXQ
; 3    5.000E-02 TWOATQEgN@@R@@RAYQATQ
; 3    6.000E-02 cAOAPQSFN@@R@@RATQAQQ
; 3    8.000E-02 AcOqDQQIN@@R@@RqFQqDQ
; 3    1.000E-01 QHOaHQeQM@@R@@RaIQaHQ
; 3    1.500E-01 eFNQEQAUM@@R@@RQFQQEQ
; 3    2.000E-01 RfNAFQeRL@@R@@RAFQAFQ
; 3    3.000E-01 qBNi@PQVL@@R@@RiAPi@P
; 3    4.000E-01 GRMhDPfQK@@R@@RhEPhDP
; 3    5.000E-01 tUMWSPSTK@@R@@RWSPWSP
; 3    6.000E-01 s@MVfPb@K@@R@@RVgPVfP
; 3    8.000E-01 AfMVBPQAK@@R@@RVBPVBP
; 3    1.000E+00 QIMUPPFiJ@@R@@RUPPUPP
; 3    1.022E+00 QDMETPVAJ@@R@@RETPETP
; 3    1.250E+00 gPLTbPTEJVBL@@RTbPTbP
; 3    1.500E+00 eHLDWPCBJCSM@@RDXPDXP
; 3    2.000E+00 RgLCbPQbJqGN@@RCcPCcP
; 3    2.044E+00 BdLsWPAfJAXN@@RsXPsXP
; 3    3.000E+00 qBLC@PAHJSdNAEMCDPCDP
; 3    4.000E+00 GSKRPPWPIFQNdIMRWPRWP
; 3    5.000E+00 tUKRFPuQIhPNXTMbFPbFP
; 3    6.000E+00 s@KQaPdQIAFOqANBCPBCP
; 3    7.000E+00 BRKqRPCfIaCOqWNAfPAfP
; 3    8.000E+00 AfKQVPsBIqIObANqRPqRP
; 3    9.000E+00 AWKATPRaIQTObTNaRPaRP
; 3    1.000E+01 QIKqCPRYIaWOCDNQSPQSP
; 3    1.100E+01 IbJaDPrCIqYOCSNAUPAUP
; 3    1.200E+01 hEJQFPRBIQ`OsYNqIPqIP
; 3    1.300E+01 GCJQ@PQeIB@OTDNqDPqDP
; 3    1.400E+01 FFJADPA`IR@ODWNaIPaIP
; 3    1.500E+01 eHJIfOaWIRIOtXNaEPaEP
; 3    1.600E+01 dTJyIOQVIbGOEHNaBPaBP
; 3    1.800E+01 cWJXXOqHIBSOeSNQFPQFP
; 3    2.000E+01 RgJWaOaCIRWOVDNQAPQAP
; 3    2.200E+01 BUJwEOQBIbYOfQNAGPAGP
; 3    2.400E+01 BFJFgOABIBaOGDNADPADP
; 3    2.600E+01 qVJFUOyHHRbOGUNAAPAAP
; 3    2.800E+01 QRJFHOhXHCBOGcNIhOIhO
; 3    3.000E+01 qBJuVOHIHSAOXHNiYOiYO
; 3    4.000E+01 GSITXOFAHCYOiWNICOICO
; 3    5.000E+01 tUICbOtYHsXOAHOhXOhXO
; 3    6.000E+01 s@IcIOShHDAOQHOHXOHXO
; 3    8.000E+01 AfIbPORgHtFOqCOhIOhIO
; 3    1.000E+02 QIIREOrGHdROATOhAOhAO
; 3    1.500E+02 eHHQSOQWHECOaTOhAOhAO
; 3    2.000E+02 RgHa@OQHHeIOqWOhGOhGO
; 3    3.000E+02 qBHXSNGdGeQOQdOHPOHPO
; 3    4.000E+02 GRGfXNEhGE`OBEOXROXRO
; 3    5.000E+02 tUGUSNtPGUcORCOhQOhQO
; 3    6.000E+02 s@GtSNSaGFBORIOhXOhXO
; 3    8.000E+02 AfGsPNRdGVEObGOxYOxYO
; 3    1.000E+03 QIGCDNrEGfCOrCOHgOHgO
; 3    1.500E+03 eHFRBNQVGvEOBROXiOXiO
; 3    2.000E+03 RgFaTNQGGFROBXOIGOIGO
; 3    3.000E+03 qBFQDNGbFVPORTOYEOYEO
; 3    4.000E+03 GREHbMEfFVTORXOiAOiAO
; 3    5.000E+03 tUEgAMdYFVWObPOiDOiDO
; 3    6.000E+03 s@EVBMSaFVYObROiFOiFO
; 3    8.000E+03 AfEtQMRcFfQObTOy@Oy@O
; 3    1.000E+04 QIECeMrEFfRObUOyBOyBO
; 3    1.500E+04 eHDbVMQVFfUObWOyEOyEO
; 3    2.000E+04 RgDBDMQGFfVObYOyFOyFO
; 3    3.000E+04 qBDAQMGbEfWOrPOyHOyHO
; 3    4.000E+04 GRCAHMEfEfXOrPOyIOyIO
; 3    5.000E+04 tUCHaLdYEfXOrQOIPOIPO
; 3    6.000E+04 s@CGULSaEfXOrQOIPOIPO
; 3    8.000E+04 AfCuQLRcEfYOrQOIQOIQO
; 3    1.000E+05 QICdULrEEfYOrROIQOIQO
;==== ELEMENT  4
; 4    1.000E-03 UbQBIPFDT@@R@@RFDTFDT
; 4    1.500E-03 TeQsYPqYT@@R@@RA`TqYT
; 4    2.000E-03 T@QeHPGRS@@R@@RGWSGSS
; 4    3.000E-03 RfQwEPBIS@@R@@RRCSR@S
; 4    4.000E-03 rBQhSPxGR@@R@@RhXRHUR
; 4    5.000E-03 QcQYVPDHR@@R@@RtGRTHR
; 4    6.000E-03 aUQACQbFR@@R@@RRSRrFR
; 4    8.000E-03 aEQQFQHbQ@@R@@RQBRYhQ
; 4    1.000E-02 yUPaFQdCQ@@R@@RFWQEYQ
; 4    1.500E-02 UXPAQQQ@Q@@R@@RCGQRQQ
; 4    2.000E-02 STPAXQdAP@@R@@RbEQQ`Q
; 4    3.000E-02 qWPQQQAHP@@R@@RqYQaRQ
; 4    4.000E-02 AEPAYQDHO@@R@@RaTQQSQ
; 4    5.000E-02 VfOAWQQbO@@R@@RQUQAXQ
; 4    6.000E-02 TbOASQADO@@R@@RAYQATQ
; 4    8.000E-02 BbOqGQSdN@@R@@RAPQqGQ
; 4    1.000E-01 AbOqAQAfN@@R@@RqCQqAQ
; 4    1.500E-01 XINQHQDbM@@R@@RQIQQHQ
; 4    2.000E-01 dRNAHQAhM@@R@@RAIQAHQ
; 4    3.000E-01 BFNITPeCL@@R@@RIVPITP
; 4    4.000E-01 QFNHVPbBL@@R@@RHWPHVP
; 4    5.000E-01 GRMwSPQIL@@R@@RwTPwSP
; 4    6.000E-01 UFMWEPGQK@@R@@RWFPWEP
; 4    8.000E-01 R`MfHPsTK@@R@@RfIPfHP
; 4    1.000E+00 AfMeUPrBK@@R@@ReUPeUP
; 4    1.022E+00 qXMUYPR@K@@R@@RUYPUYP
; 4    1.250E+00 QIMEEPASKHRL@@REEPEEP
; 4    1.500E+00 hELTYPADKtQM@@RdPPdPP
; 4    2.000E+00 dTLSbPVXJAhN@@RSdPSdP
; 4    2.044E+00 DTLCgPvGJBCN@@RCiPCiP
; 4    3.000E+00 BFLCHPsQJEPNAHMSDPSDP
; 4    4.000E+00 QFLRWPRVJxWNDPMbVPbVP
; 4    5.000E+00 GSKbBPQeJQHOxWMrEPrEP
; 4    6.000E+00 UFKQfPQWJAUOqENRBPRBP
; 4    7.000E+00 sYKqVPqBJaYOAbNQePQeP
; 4    8.000E+00 R`KaQPQCJQaObGNAbPAbP
; 4    9.000E+00 bIKAXPY`IR@OrQNqQPqQP
; 4    1.000E+01 AfKqGPHaIbIOSCNaSPaSP
; 4    1.100E+01 QSKaHPWeIBUOSRNQVPQVP
; 4    1.200E+01 aIKa@PgBIbPOS`NQPPQPP
; 4    1.300E+01 Q@KQCPfSIrTOdENATPATP
; 4    1.400E+01 IXJAGPVBIBgOTYNAPPAPP
; 4    1.500E+01 hEJAAPeXIRiOTaNqFPqFP
; 4    1.600E+01 gEJiTOuAISAOeANqCPqCP
; 4    1.800E+01 uSJHaOdXIsBOuXNaGPaGP
; 4    2.000E+01 dTJXCOTIISQOvANaCPaCP
; 4    2.200E+01 CdJWTOsYIcXOvYNQIPQIP
; 4    2.400E+01 cBJGEOCVICdOgCNQFPQFP
; 4    2.600E+01 rUJfROSIISiOgTNQDPQDP
; 4    2.800E+01 rGJfEOReITBOHCNQBPQBP
; 4    3.000E+01 BFJUaOrUIdDOxINQ@PQ@P
; 4    4.000E+01 QFJtPOBDItVOYbNAEPAEP
; 4    5.000E+01 GRISbOaSIUDOQAOABPABP
; 4    6.000E+01 UFIsHOqEIEUOaAOA@PA@P
; 4    8.000E+01 R`IbWOAAIUbOqFOYdOYdO
; 4    1.000E+02 AfIbAOHDHfEOAXOYdOYdO
; 4    1.500E+02 hEHQXOuDHFaOaXOAAPAAP
; 4    2.000E+02 dTHaCOD@HWFOAaOABPABP
; 4    3.000E+02 BFHxUNbVHgPOQhOAEPAEP
; 4    4.000E+02 QFHFfNQiHGfOBIOAFPAFP
; 4    5.000E+02 GRGeXNQYHHDORGOAHPAHP
; 4    6.000E+02 UFGDfNqCHXGObCOAIPAIP
; 4    8.000E+02 R`GC`NYfGxEOrAOQ@PQ@P
; 4    1.000E+03 AfGSBNWgGHWOrGOQBPQBP
; 4    1.500E+03 hEFRHNuAGhTOBVOQCPQCP
; 4    2.000E+03 dTFaYNShGxSORQOQDPQDP
; 4    3.000E+03 BFFQGNbUGHdORWOQEPQEP
; 4    4.000E+03 QFFIFMQiGHiObPOQFPQFP
; 4    5.000E+03 GREGPMQYGXcObSOQFPQFP
; 4    6.000E+03 UFEfHMqCGXfObTOQGPQGP
; 4    8.000E+03 R`EDdMYeFXiObVOQGPQGP
; 4    1.000E+04 AfESeMWfFIAObWOQGPQGP
; 4    1.500E+04 hEDrSMuAFIDObYOQHPQHP
; 4    2.000E+04 dTDR@MShFIFOrPOQHPQHP
; 4    3.000E+04 BFDAUMbUFIGOrQOQHPQHP
; 4    4.000E+04 QFDQAMQiFIIOrROQHPQHP
; 4    5.000E+04 GRCIELQYFIIOrROQHPQHP
; 4    6.000E+04 UFCgULqCFIIOrSOQHPQHP
; 4    8.000E+04 R`CEgLYdEY@OrSOQHPQHP
; 4    1.000E+05 AfCtWLWfEY@OrSOQHPQHP
;==== ELEMENT  5
; 5    1.000E-03 HEQaQPaCU@@R@@RaCUaCU
; 5    1.500E-03 VeQCHPsVT@@R@@RsWTsVT
; 5    2.000E-03 EgQTWPQYT@@R@@RaPTQYT
; 5    3.000E-03 d@QGDPdRS@@R@@RdWSdSS
; 5    4.000E-03 SGQxSPAiS@@R@@RQcSAiS
; 5    5.000E-03 RRQIhPyCR@@R@@RiXRISR
; 5    6.000E-03 BIQAGQeBR@@R@@RUTRuCR
; 5    8.000E-03 QVQa@QBGR@@R@@RrERRIR
; 5    1.000E-02 aCQaIQA@R@@R@@RaERQCR
; 5    1.500E-02 wDPATQbUQ@@R@@RDcQDIQ
; 5    2.000E-02 tWPQQQABQ@@R@@RCAQRTQ
; 5    3.000E-02 BTPQUQbVP@@R@@RBFQAbQ
; 5    4.000E-02 AWPQUQAAP@@R@@RqYQaUQ
; 5    5.000E-02 yYOQRQtYO@@R@@RaVQQWQ
; 5    6.000E-02 VgOAYQbPO@@R@@RQXQQQQ
; 5    8.000E-02 DCOARQIiN@@R@@RAWQASQ
; 5    1.000E-01 bROqFQtPN@@R@@RqIQqFQ
; 5    1.500E-01 QHOaCQaBN@@R@@RaDQaCQ
; 5    2.000E-01 fWNQCQtYM@@R@@RQDQQCQ
; 5    3.000E-01 RhNIcPqDM@@R@@RIfPIcP
; 5    4.000E-01 aXNHbPuPL@@R@@RHdPHbP
; 5    5.000E-01 AGNHEPCFL@@R@@RHGPHFP
; 5    6.000E-01 GVMGUPQ`L@@R@@RGVPGUP
; 5    8.000E-01 d@MVUPYYK@@R@@RVUPVUP
; 5    1.000E+00 bYMEiPUgK@@R@@REiPEiP
; 5    1.022E+00 RWMEcPEUK@@R@@REcPEcP
; 5    1.250E+00 qRMeFPsQKQ@M@@ReGPeGP
; 5    1.500E+00 QIMtXPbYKVEM@@RtYPtYP
; 5    2.000E+00 vRLDHPqPKBVN@@RTAPTAP
; 5    2.044E+00 FSLDCPaUKbUN@@RDFPDFP
; 5    3.000E+00 RhLcAPYXJGDNQBMcHPcHP
; 5    4.000E+00 aXLbXPfPJQDOTYMB`PB`P
; 5    5.000E+00 AGLrAPEBJQSOYDMBXPBXP
; 5    6.000E+00 GVKBEPDDJAiOAPNbEPbEP
; 5    7.000E+00 EXKAdPsHJb@OAiNBHPBHP
; 5    8.000E+00 d@KaWPR`JBXOrGNQdPQdP
; 5    9.000E+00 sBKQTPRUJrTOBbNAdPAdP
; 5    1.000E+01 bYKARPbGJRhOcFNqVPqVP
; 5    1.100E+01 bBKqCPBDJSIOcWNaXPaXP
; 5    1.200E+01 AgKaEPAfJsHODFNaSPaSP
; 5    1.300E+01 QYKQGPqPJSWODSNQXPQXP
; 5    1.400E+01 qGKQAPQWJsTOtXNQSPQSP
; 5    1.500E+01 QIKAEPAVJCiOUANQPPQPP
; 5    1.600E+01 AEKA@PqFJDDOESNAVPAVP
; 5    1.800E+01 hIJYHOa@JtBOFCNAQPAQP
; 5    2.000E+01 vRJHVOAHJTVOVWNqGPqGP
; 5    2.200E+01 UUJGfOySItYOGGNqDPqDP
; 5    2.400E+01 dVJwEOHhITiOWTNqAPqAP
; 5    2.600E+01 SgJV`OXGIUHOWgNaIPaIP
; 5    2.800E+01 CSJVQOWVIuEOxGNaGPaGP
; 5    3.000E+01 RhJVFOGDIUQOxUNaEPaEP
; 5    4.000E+01 aXJT`OeDIVGOACOaAPaAP
; 5    5.000E+01 AGJDIOTGIfVOQFOQIPQIP
; 5    6.000E+01 GVISROCVIGFOaFOQHPQHP
; 5    8.000E+01 d@IrXORXIgUOAROQHPQHP
; 5    1.000E+02 bYIrAOBFIHHOQSOQIPQIP
; 5    1.500E+02 QIIaTOqGIHaOqTOaBPaBP
; 5    2.000E+02 vQHaIOABIiGOAhOaDPaDP
; 5    3.000E+02 RhHYBNFbHIdOBFOaHPaHP
; 5    4.000E+02 aXHWENUAHABPRGOqAPqAP
; 5    5.000E+02 AGHUbNDIHADPbEOqBPqBP
; 5    6.000E+02 GVGEFNCPHAFPrAOqDPqDP
; 5    8.000E+02 d@GSeNRUHAHPrIOqFPqFP
; 5    1.000E+03 bYGcENBDHAIPBUOqGPqGP
; 5    1.500E+03 QIGbGNqFHQBPRTOqIPqIP
; 5    2.000E+03 vQFqVNABHQCPRYOAQPAQP
; 5    3.000E+03 RhFaBNF`GQDPbTOARPARP
; 5    4.000E+03 aXFITMU@GQEPbXOASPASP
; 5    5.000E+03 AGFwQMDHGQEPrPOASPASP
; 5    6.000E+03 GVEVUMCPGQFPrQOASPASP
; 5    8.000E+03 d@EEDMRUGQFPrSOATPATP
; 5    1.000E+04 bYETBMBDGQFPrTOATPATP
; 5    1.500E+04 QIEBdMqFGQGPrVOAUPAUP
; 5    2.000E+04 vQDRIMABGQGPrWOAUPAUP
; 5    3.000E+04 RhDQQMF`FQGPrXOAUPAUP
; 5    4.000E+04 aXDQFMU@FQGPrXOAUPAUP
; 5    5.000E+04 AGDISLDHFQGPrYOAUPAUP
; 5    6.000E+04 GVCWgLCPFQGPrYOAUPAUP
; 5    8.000E+04 d@CVALRUFQGPrYOAUPAUP
; 5    1.000E+05 bYCTgLBDFQHPrYOAVPAVP
;==== ELEMENT  6
; 6    1.000E-03 AHRaFPbAU@@R@@RbAUbAU
; 6    1.500E-03 YYQRQPViT@@R@@RG@TViT
; 6    2.000E-03 xBQCfPCBT@@R@@RCCTCBT
; 6    3.000E-03 VCQFQPXfS@@R@@RICSXgS
; 6    4.000E-03 dPQHUPsRS@@R@@RsXSsSS
; 6    5.000E-03 SYQYePAgS@@R@@RQaSAhS
; 6    6.000E-03 RbQQ@QAES@@R@@RAISAGS
; 6    8.000E-03 R@QaEQdDR@@R@@RTXRtGR
; 6    1.000E-02 aRQqEQBHR@@R@@RrGRbAR
; 6    1.500E-02 yYPQQQUYQ@@R@@RHGQW@Q
; 6    2.000E-02 FXPaPQRHQ@@R@@RDRQsWQ
; 6    3.000E-02 sFPaUQuQP@@R@@RRVQbCQ
; 6    4.000E-02 BEPaUQRIP@@R@@RBHQAgQ
; 6    5.000E-02 qGPaSQADP@@R@@RAgQqSQ
; 6    6.000E-02 IaOaPQeWO@@R@@RqUQaVQ
; 6    8.000E-02 uQOQSQRGO@@R@@RaQQQUQ
; 6    1.000E-01 sROAWQACO@@R@@RQQQAXQ
; 6    1.500E-01 aXOqCQrQN@@R@@RqEQqCQ
; 6    2.000E-01 YTNaBQAFN@@R@@RaCQaBQ
; 6    3.000E-01 dFNAFQRhM@@R@@RAGQAFQ
; 6    4.000E-01 BPNYRPaGM@@R@@RYUPYRP
; 6    5.000E-01 QTNxPPFdL@@R@@RxRPxPP
; 6    6.000E-01 AGNHEPdEL@@R@@RHFPHEP
; 6    8.000E-01 FBMGGPRDL@@R@@RGHPGGP
; 6    1.000E+00 CeMvFPqCL@@R@@RvFPvFP
; 6    1.022E+00 cYMfIPaCL@@R@@Rv@PfIP
; 6    1.250E+00 BWMeYPxEKATM@@ReYPeYP
; 6    1.500E+00 qQMUGPFFKWiM@@RUHPUHP
; 6    2.000E+00 iSLDQPCcKSIN@@RDTPDTP
; 6    2.044E+00 iBLtFPsPKCTN@@RtIPtIP
; 6    3.000E+00 dHLCWPREKYCNaAMSVPSVP
; 6    4.000E+00 BQLBiPAXKAXOTfMCEPCEP
; 6    5.000E+00 QTLRPPQBKQiOIhMrQPrQP
; 6    6.000E+00 AGLbAPICJBTOQRNBWPBWP
; 6    7.000E+00 GfKQiPWVJBeOBENbIPbIP
; 6    8.000E+00 FBKAaPFYJcBORVNREPREP
; 6    9.000E+00 tVKaVPeYJSUOCENBEPBEP
; 6    1.000E+01 CeKQTPEFJCeOSRNQfPQfP
; 6    1.100E+01 SHKATPTUJTCOSfNAiPAiP
; 6    1.200E+01 bXKqEPTDJtHOtHNAcPAcP
; 6    1.300E+01 bHKaGPC`JdROtXNqXPqXP
; 6    1.400E+01 QgKa@PSPJDdOUFNqTPqTP
; 6    1.500E+01 qQKQDPcEJEDOUSNqPPqPP
; 6    1.600E+01 QQKAHPCDJeCOEgNaWPaWP
; 6    1.800E+01 QIKYaObXJUYOVQNaQPaQP
; 6    2.000E+01 iSJYDOBPJU`OGINQXPQXP
; 6    2.200E+01 WfJHYORGJVIOgTNQTPQTP
; 6    2.400E+01 fYJWdOQhJFUOXDNQRPQRP
; 6    2.600E+01 uPJGUOAbJfYOhPNQPPQPP
; 6    2.800E+01 TbJGCOaYJVaOICNAXPAXP
; 6    3.000E+01 dHJfUOQWJWBOITNAWPAWP
; 6    4.000E+01 BQJeIOQGJWfOQBOATPATP
; 6    5.000E+01 QTJDQOiHIXYOaEOASPASP
; 6    6.000E+01 AGJC`OwQIY@OqFOASPASP
; 6    8.000E+01 FBIC@OuVIIfOQROATPATP
; 6    1.000E+02 CeIBYOTYIADPaUOAVPAVP
; 6    1.500E+02 qQIqWOCEIQDPAgOQPPQPP
; 6    2.000E+02 iSHqIObHIa@PBBOQTPQTP
; 6    3.000E+02 dHHIeNQRIaGPb@OQYPQYP
; 6    4.000E+02 BQHwRNQDIqAPrBOaRPaRP
; 6    5.000E+02 QTHvINY@HqDPBPOaTPaTP
; 6    6.000E+02 AGHEWNWXHqFPBVOaVPaVP
; 6    8.000E+02 FBGdGNeXHqIPRUOaYPaYP
; 6    1.000E+03 CeGSQNTUHAQPbQOqPPqPP
; 6    1.500E+03 qQGBUNCCHASPbYOqSPqSP
; 6    2.000E+03 iSFQ`NbGHAUPrTOqTPqTP
; 6    3.000E+03 dHFqBNQQHAWPB`OqVPqVP
; 6    4.000E+03 BQFABNQDHAWPBcOqWPqWP
; 6    5.000E+03 QTFxDMIHGAXPBeOqWPqWP
; 6    6.000E+03 AGFGGMWWGAXPBfOqXPqXP
; 6    8.000E+03 FBEEUMeXGAYPBhOqXPqXP
; 6    1.000E+04 CeEDUMTTGAYPBiOqYPqYP
; 6    1.500E+04 qQECGMCCGQPPRaOqYPqYP
; 6    2.000E+04 iSDrFMbGGQPPRbOqYPqYP
; 6    3.000E+04 dHDaSMQQGQPPRcOA`PA`P
; 6    4.000E+04 BQDaEMQDGQPPRdOA`PA`P
; 6    5.000E+04 QTDABMIHFQPPRdOA`PA`P
; 6    6.000E+04 AGDhQLWWFQPPRdOA`PA`P
; 6    8.000E+04 FBCfPLeXFQPPRdOA`PA`P
; 6    1.000E+05 CeCuGLTTFQQPRdOA`PA`P
;==== ELEMENT  7
; 7    1.000E-03 aIRQ@PsAU@@R@@RsAUsAU
; 7    1.500E-03 QHRbCPAHU@@R@@RAHUAHU
; 7    2.000E-03 AERSQPtVT@@R@@RtWTtVT
; 7    3.000E-03 H@QUhPAUT@@R@@RAVTAUT
; 7    4.000E-03 VAQHBPV@S@@R@@RVGSV@S
; 7    5.000E-03 tWQYWPCIS@@R@@RSDSS@S
; 7    6.000E-03 CdQAGQqVS@@R@@RAaSqWS
; 7    8.000E-03 bYQaCQWGR@@R@@RWVRgIR
; 7    1.000E-02 BCQqCQSTR@@R@@RChRcXR
; 7    1.500E-02 aAQAXQiWQ@@R@@RaDRQBR
; 7    2.000E-02 HDPQWQCaQ@@R@@RVHQuGQ
; 7    3.000E-02 dCPaSQAAQ@@R@@RCGQbTQ
; 7    4.000E-02 RXPaTQSaP@@R@@RbIQBCQ
; 7    5.000E-02 qTPaRQAgP@@R@@RQhQAaQ
; 7    6.000E-02 aEPQYQABP@@R@@RAbQaYQ
; 7    8.000E-02 w@OQSQSbO@@R@@RaTQQWQ
; 7    1.000E-01 tWOAVQAgO@@R@@RQSQAXQ
; 7    1.500E-01 RGOqCQTbN@@R@@RqEQqCQ
; 7    2.000E-01 aCOaBQQdN@@R@@RaCQaBQ
; 7    3.000E-01 UQNAFQEVM@@R@@RAGQAFQ
; 7    4.000E-01 S@NYRPrCM@@R@@RYVPYSP
; 7    5.000E-01 QiNxPPaFM@@R@@RxRPxPP
; 7    6.000E-01 qHNHEPGbL@@R@@RHFPHEP
; 7    8.000E-01 wXMGGPSeL@@R@@RGHPGGP
; 7    1.000E+00 ThMvFPBUL@@R@@RvFPvFP
; 7    1.022E+00 tWMfIPbHL@@R@@Rv@PfIP
; 7    1.250E+00 SIMeYPQTLaYM@@ReYPeYP
; 7    1.500E+00 bAMUGPQBLyFM@@RUHPUHP
; 7    2.000E+00 aEMDQPGFKsSN@@RDUPDUP
; 7    2.044E+00 QIMtFPFdKDBN@@RDPPDPP
; 7    3.000E+00 USLCWPSfKAGOaAMSXPSXP
; 7    4.000E+00 SALR`PrRKqSOTfMCGPCGP
; 7    5.000E+00 QiLRPPBFKrBOIhMrTPrTP
; 7    6.000E+00 qHLbAPaVKBeOQRNRQPRQP
; 7    7.000E+00 ABLQiPqIKsCOBENrDPrDP
; 7    8.000E+00 wYKAaPQIKsUORVNbAPbAP
; 7    9.000E+00 VEKaVPADKTDOCENRAPRAP
; 7    1.000E+01 ThKQTPiGJDYOSRNBBPBBP
; 7    1.100E+01 TBKATPxEJDbOSgNQfPQfP
; 7    1.200E+01 CVKqEPWYJUAOtINQ`PQ`P
; 7    1.300E+01 ReKaGPVfJuHOtYNAePAeP
; 7    1.400E+01 RTKa@PFRJeTOUFNAbPAbP
; 7    1.500E+01 bAKQDPUfJEhOURNqXPqXP
; 7    1.600E+01 QeKAIPUWJV@OEgNqUPqUP
; 7    1.800E+01 QTKYbOTaJVQOVQNqQPqQP
; 7    2.000E+01 aEKYDOtIJFgOGINaWPaWP
; 7    2.200E+01 ACKXPOSgJgAOgTNaUPaUP
; 7    2.400E+01 hUJWdOcSJWQOXCNaSPaSP
; 7    2.600E+01 wGJGVOsDJwYOhPNaQPaQP
; 7    2.800E+01 vEJGCOCIJHDOICNaPPaPP
; 7    3.000E+01 UTJfVOBgJhHOITNQYPQYP
; 7    4.000E+01 SAJeIORDJiFOQAOQWPQWP
; 7    5.000E+01 QiJDROqPJA@PaEOQWPQWP
; 7    6.000E+01 qHJCaOAQJAFPqEOQWPQWP
; 7    8.000E+01 wXIC@OAEJQEPQROaPPaPP
; 7    1.000E+02 ThIBYOHPIaAPaUOaSPaSP
; 7    1.500E+02 bAIqWOUXIqBPAgOaXPaXP
; 7    2.000E+02 aEIqIOTHIqIPBAOqSPqSP
; 7    3.000E+02 USHIfNrXIAWPRIOqYPqYP
; 7    4.000E+02 SAHwRNBHIQRPrAOAcPAcP
; 7    5.000E+02 QiHvINaWIQUPrIOAePAeP
; 7    6.000E+02 qHHEWNqIIQWPBUOAgPAgP
; 7    8.000E+02 wXGdGNADIaPPRSOQ`PQ`P
; 7    1.000E+03 ThGSRNxBHaRPRYOQbPQbP
; 7    1.500E+03 bAGBVNUTHaUPbWOQePQeP
; 7    2.000E+03 aEGQ`NTFHaWPrROQfPQfP
; 7    3.000E+03 USFqBNrWHaYPrWOQhPQhP
; 7    4.000E+03 SAFABNBHHqPPB`OQiPQiP
; 7    5.000E+03 QiFxDMaVHqPPBbOB@PB@P
; 7    6.000E+03 qHFGGMqHHqQPBcOB@PB@P
; 7    8.000E+03 wXEEUMADHqQPBeOBAPBAP
; 7    1.000E+04 ThEDUMxAGqRPBfOBAPBAP
; 7    1.500E+04 bAECGMUTGqRPBhOBAPBAP
; 7    2.000E+04 aEErFMTEGqSPBiOBBPBBP
; 7    3.000E+04 USDaSMrWGqSPR`OBBPBBP
; 7    4.000E+04 SADaEMBHGqSPR`OBBPBBP
; 7    5.000E+04 QiDABMaVGqSPR`OBBPBBP
; 7    6.000E+04 qHDhQLqHGqSPRaOBBPBBP
; 7    8.000E+04 wXCfPLADGqSPRaOBBPBBP
; 7    1.000E+05 ThCuGLxAFqSPRaOBBPBBP
;==== ELEMENT  8
; 8    1.000E-03 QPRXQOTYU@@R@@RTYUTYU
; 8    1.500E-03 qIRqWPQUU@@R@@RQUUQUU
; 8    2.000E-03 aFRBePVdT@@R@@RVeTVdT
; 8    3.000E-03 A@REIPRFT@@R@@RRGTRFT
; 8    4.000E-03 GcQW@PiCS@@R@@RyASiDS
; 8    5.000E-03 VHQxTPtRS@@R@@RtYStSS
; 8    6.000E-03 ThQA@QrQS@@R@@RrWSrRS
; 8    8.000E-03 CUQQHQQBS@@R@@RQFSQCS
; 8    1.000E-02 RVQaIQUWR@@R@@RUeRuPR
; 8    1.500E-02 AYQAUQQTR@@R@@RAdRaYR
; 8    2.000E-02 IiPQTQVBQ@@R@@RhUQgVQ
; 8    3.000E-02 eEPaQQaTQ@@R@@RsXQcEQ
; 8    4.000E-02 cAPaRQvIP@@R@@RRYQbFQ
; 8    5.000E-02 RGPaQQCGP@@R@@RRCQQbQ
; 8    6.000E-02 QVPQXQaXP@@R@@RQaQqUQ
; 8    8.000E-02 YDOQRQVPO@@R@@RaXQQYQ
; 8    1.000E-01 UiOAVQSAO@@R@@RQUQAYQ
; 8    1.500E-01 rSOqCQhCN@@R@@RqFQqCQ
; 8    2.000E-01 QUOaBQcEN@@R@@RaDQaBQ
; 8    3.000E-01 VfNAFQYIM@@R@@RAGQAFQ
; 8    4.000E-01 ScNYRPScM@@R@@RYWPYSP
; 8    5.000E-01 RRNxPPRBM@@R@@RxSPxPP
; 8    6.000E-01 qUNHEPqBM@@R@@RHGPHEP
; 8    8.000E-01 IdMGHPfWL@@R@@RGIPGHP
; 8    1.000E+00 v@MvFPTDL@@R@@RvGPvGP
; 8    1.022E+00 FCMv@PCfL@@R@@Rv@Pv@P
; 8    1.250E+00 DCMeYPbRLQdM@@RuPPeYP
; 8    1.500E+00 B`MUGPQ`LAGN@@RUIPUHP
; 8    2.000E+00 QXMDRPa@LdGN@@RDVPDVP
; 8    2.044E+00 QQMtFPQFLdPN@@RDQPDPP
; 8    3.000E+00 G@LCWPfXKaBOaBMcPPcPP
; 8    4.000E+00 SdLR`PTYKQhOTfMS@PS@P
; 8    5.000E+00 RRLRPPCXKbUOIhMrXPrXP
; 8    6.000E+00 qULbAPB`KcFOQRNRUPRUP
; 8    7.000E+00 aILQiPrDKC`OBENrIPrIP
; 8    8.000E+00 IeKAaPB@KdIORVNbFPbFP
; 8    9.000E+00 wXKaVPqVKtSOCENRGPRGP
; 8    1.000E+01 v@KQTPQVKUCOSRNBIPBIP
; 8    1.100E+01 eAKATPAPKUPOSgNBCPBCP
; 8    1.200E+01 tHKqEPaHKEcOtINQgPQgP
; 8    1.300E+01 sSKaGPQGKVDOtYNQcPQcP
; 8    1.400E+01 cBKa@PAHKFSOUGNQ`PQ`P
; 8    1.500E+01 B`KQDPA@KvPOUSNAgPAgP
; 8    1.600E+01 BVKAIPyFJVfOEgNAdPAdP
; 8    1.800E+01 QeKYbOhEJGSOVQNA`PA`P
; 8    2.000E+01 QXKYEOwHJGdOW@NqWPqWP
; 8    2.200E+01 q@KXPOfXJhBOgTNqUPqUP
; 8    2.400E+01 AIKWdOFIJXWOXCNqSPqSP
; 8    2.600E+01 yCJGVOePJHiOhPNqRPqRP
; 8    2.800E+01 HDJGCOUIJYHOICNqQPqQP
; 8    3.000E+01 G@JfVODcJIUOISNqQPqQP
; 8    4.000E+01 SdJeIOSYJAFPQAOqPPqPP
; 8    5.000E+01 RRJDROBeJQDPaEOqQPqQP
; 8    6.000E+01 qUJCaOrGJaAPqEOqRPqRP
; 8    8.000E+01 IeIC@OqWJqAPQROqVPqVP
; 8    1.000E+02 v@IBYOAQJqHPaTOqYPqYP
; 8    1.500E+02 B`IqWOyFIQPPAfOAgPAgP
; 8    2.000E+02 QXIqIOGAIQXPB@OQbPQbP
; 8    3.000E+02 G@HIfNdVIaWPRHOQiPQiP
; 8    4.000E+02 SdHwRNSPIqRPbIOBCPBCP
; 8    5.000E+02 RRHvINrYIqVPrGOBFPBFP
; 8    6.000E+02 qUHEXNrCIqXPBROBHPBHP
; 8    8.000E+02 IeGdHNqUIAbPRPORAPRAP
; 8    1.000E+03 v@GSRNAPIAdPRUORCPRCP
; 8    1.500E+03 B`GBVNy@HAgPbSORFPRFP
; 8    2.000E+03 QXGQ`NVgHAiPbXORGPRGP
; 8    3.000E+03 G@FqBNdUHQaPrSORIPRIP
; 8    4.000E+03 SdFABNCYHQbPrUOb@Pb@P
; 8    5.000E+03 RRFxDMrYHQbPrWObAPbAP
; 8    6.000E+03 qUFGHMrBHQcPrXObAPbAP
; 8    8.000E+03 IeEEUMqTHQcPB`ObBPbBP
; 8    1.000E+04 v@EDUMqIHQdPBaObBPbBP
; 8    1.500E+04 B`ECHMiIGQdPBbObCPbCP
; 8    2.000E+04 QXErFMVgGQePBcObCPbCP
; 8    3.000E+04 G@DaSMdUGQePBdObCPbCP
; 8    4.000E+04 SdDaEMCYGQePBeObDPbDP
; 8    5.000E+04 RRDABMrYGQePBeObDPbDP
; 8    6.000E+04 qUDhRLrBGQePBeObDPbDP
; 8    8.000E+04 IeCfQLqTGQePBeObDPbDP
; 8    1.000E+05 v@CuGLqIGQePBeObDPbDP
;==== ELEMENT  9
; 9    1.000E-03 aRRFSOeUU@@R@@ReUUeUU
; 9    1.500E-03 QRRqEPQhU@@R@@RQhUQhU
; 9    2.000E-03 APRbAPICT@@R@@RIETICT
; 9    3.000E-03 QERT@PBhT@@R@@RBiTBhT
; 9    4.000E-03 i@QU`PaET@@R@@RaFTaET
; 9    5.000E-03 wGQGVPFSS@@R@@RVQSFTS
; 9    6.000E-03 UhQxVPsRS@@R@@RsYSsSS
; 9    8.000E-03 TDQAFQQUS@@R@@RaPSQVS
; 9    1.000E-02 CFQQHQwXR@@R@@RhARW`R
; 9    1.500E-02 qSQqEQRHR@@R@@RBYRrBR
; 9    2.000E-02 QDQASQxUQ@@R@@RQCRABR
; 9    3.000E-02 FIPQQQrGQ@@R@@RDYQChQ
; 9    4.000E-02 sTPQRQy@P@@R@@RBcQBUQ
; 9    5.000E-02 RSPQQQDXP@@R@@RbAQQfQ
; 9    6.000E-02 AbPAYQBVP@@R@@RQbQqTQ
; 9    8.000E-02 AGPATQYVO@@R@@RaTQQSQ
; 9    1.000E-01 GAOqHQTYO@@R@@RQPQASQ
; 9    1.500E-01 cAOaEQaBO@@R@@Rq@QaGQ
; 9    2.000E-01 AcOQEQDdN@@R@@RQHQQFQ
; 9    3.000E-01 XINAAQqGN@@R@@RABQAAQ
; 9    4.000E-01 dRNIBPEiM@@R@@RIGPICP
; 9    5.000E-01 RfNhDPSHM@@R@@RhGPhDP
; 9    6.000E-01 BFNgSPQhM@@R@@RgUPgSP
; 9    8.000E-01 QFNvPPYiL@@R@@RvRPvQP
; 9    1.000E+00 GRMFCPfAL@@R@@RFDPFCP
; 9    1.022E+00 WAMUgPEaL@@R@@RUgPUgP
; 9    1.250E+00 tUMuIPSdLBFM@@REPPuIP
; 9    1.500E+00 s@MT`PBfLQDN@@RTbPTaP
; 9    2.000E+00 AfMTHPA`LTUN@@RdCPdCP
; 9    2.044E+00 qXMTCPqTLT`N@@RTHPTHP
; 9    3.000E+00 hELcIPA@Lq@OQEMCRPCRP
; 9    4.000E+00 dTLrTPFgKRAOtPMRfPRfP
; 9    5.000E+00 RgLrGPe@KBcOyFMbVPbVP
; 9    6.000E+00 BFLBIPTHKCWOATNBVPBVP
; 9    7.000E+00 QRLAhPCYKDEOQdNrAPrAP
; 9    8.000E+00 QFLqQPRiKTWOBSNRIPRIP
; 9    9.000E+00 YGKQXPbRKEDOBiNRAPRAP
; 9    1.000E+01 GSKAVPrCKEVOsDNBDPBDP
; 9    1.100E+01 VDKqFPR@KEeOsVNQhPQhP
; 9    1.200E+01 UFKaHPQ`KfAOTFNQdPQdP
; 9    1.300E+01 DPKa@PqUKVTOTSNQ`PQ`P
; 9    1.400E+01 sYKQDPaQKFeODiNAgPAgP
; 9    1.500E+01 s@KAHPQPKWDOeDNAePAeP
; 9    1.600E+01 R`KACPAPKGPOUVNAbPAbP
; 9    1.800E+01 bIKIPOaCKW`OVGNqYPqYP
; 9    2.000E+01 AfKhWOQ@KxEOvRNqWPqWP
; 9    2.200E+01 QSKHEOYeJxUOgCNqUPqUP
; 9    2.400E+01 aIKWSOIHJYAOwPNqTPqTP
; 9    2.600E+01 Q@KGGOxEJIUOXDNqSPqSP
; 9    2.800E+01 IWJfWOwSJyVOXUNqSPqSP
; 9    3.000E+01 hEJvAOg@JA@PXcNqSPqSP
; 9    4.000E+01 dTJEAOuDJQBPAEOqSPqSP
; 9    5.000E+01 RgJTIOdEJaAPQHOqUPqUP
; 9    6.000E+01 BFJcQOSSJaHPaHOqWPqWP
; 9    8.000E+01 QFJBdObSJqIPATOAbPAbP
; 9    1.000E+02 GSIrFOR@JAWPQUOAfPAfP
; 9    1.500E+02 s@IaXOqIJQYPqUOQdPQdP
; 9    2.000E+02 AfIqBOADJaWPAiOQiPQiP
; 9    3.000E+02 hEHyDNVeIqVPBEOBFPBFP
; 9    4.000E+02 dTHwBNe@IAbPREORAPRAP
; 9    5.000E+02 RgHFFNTFIAePbBORDPRDP
; 9    6.000E+02 BFHUINCWIAhPbGORFPRFP
; 9    8.000E+02 QFHDENbPIQaPrDORIPRIP
; 9    1.000E+03 GSGsCNBHIQdPrIObAPbAP
; 9    1.500E+03 s@GrCNqHIQgPBVObDPbDP
; 9    2.000E+03 AfGA`NADIQiPRPObEPbEP
; 9    3.000E+03 hEFaENVbHBAPRTObGPbGP
; 9    4.000E+03 dTFiWMUIHBBPRWObHPbHP
; 9    5.000E+03 RgFWaMTEHBBPRXObIPbIP
; 9    6.000E+03 BFFvPMCVHBCPRYObIPbIP
; 9    8.000E+03 QFFUFMRYHBCPbQOr@Pr@P
; 9    1.000E+04 GSEdBMBHHBDPbROr@Pr@P
; 9    1.500E+04 s@ERaMqHHBDPbSOrAPrAP
; 9    2.000E+04 AfEbDMADHBEPbTOrAPrAP
; 9    3.000E+04 hEDQTMVbGBEPbTOrAPrAP
; 9    4.000E+04 dTDQIMUIGBEPbUOrBPrBP
; 9    5.000E+04 RgDiVLTEGBEPbUOrBPrBP
; 9    6.000E+04 BFDXGLCVGBEPbUOrBPrBP
; 9    8.000E+04 QFDfFLRYGBEPbUOrBPrBP
; 9    1.000E+05 GSCEILBHGBEPbUOrBPrBP
;==== ELEMENT  10
;10    1.000E-03 Q`REXOGQU@@R@@RGQUGQU
;10    1.500E-03 A`RQFPbVU@@R@@RbWUbVU
;10    2.000E-03 aXRQcPaDU@@R@@RaDUaDU
;10    3.000E-03 ARRcTPDDT@@R@@RDETDDT
;10    4.000E-03 QFRuFPqWT@@R@@RqXTqWT
;10    5.000E-03 IUQVbPiDS@@R@@RyDSiDS
;10    6.000E-03 wTQhHPuHS@@R@@REWSuIS
;10    8.000E-03 EPQADQbFS@@R@@RrCSbGS
;10    1.000E-02 ShQQHQQES@@R@@Ra@SQFS
;10    1.500E-02 bCQqGQcER@@R@@RcQRsIR
;10    2.000E-02 AVQAWQqAR@@R@@RaQRAVR
;10    3.000E-02 wSPQVQSYQ@@R@@RUbQUEQ
;10    4.000E-02 tVPQXQARQ@@R@@RCWQC@Q
;10    5.000E-02 cBPQWQFgP@@R@@RRXQbFQ
;10    6.000E-02 rAPQUQsYP@@R@@RRFQQcQ
;10    8.000E-02 qFPQPQAXP@@R@@RqXQaTQ
;10    1.000E-01 XeOATQWBO@@R@@RaPQQQQ
;10    1.500E-01 TAOqAQQ`O@@R@@RqGQqCQ
;10    2.000E-01 rDOaAQWWN@@R@@RaDQaAQ
;10    3.000E-01 AEOAEQREN@@R@@RAFQAEQ
;10    4.000E-01 UcNISPiEM@@R@@RYPPITP
;10    5.000E-01 C`NhRPE@M@@R@@RhVPhSP
;10    6.000E-01 bTNWhPSAM@@R@@RHAPWhP
;10    8.000E-01 AYNGAPQWM@@R@@RGCPGAP
;10    1.000E+00 YRMvAPyXL@@R@@RvBPvAP
;10    1.022E+00 YBMfDPYGL@@R@@RfEPfDP
;10    1.250E+00 V@MeTPfCLBQM@@ReUPeTP
;10    1.500E+00 dCMUCPTQLqCN@@RUDPUDP
;10    2.000E+00 rHMtGPBcLu@N@@RDSPDSP
;10    2.044E+00 bHMtBPrTLuQN@@RtHPtHP
;10    3.000E+00 AFMCTPQWLQQOa@MSYPSYP
;10    4.000E+00 UfLBgPAHLBUOTbMSBPSBP
;10    5.000E+00 CaLBXPXFKcIOI`MBbPBbP
;10    6.000E+00 bULRIPVUKDDOQPNbQPbQP
;10    7.000E+00 QeLQgPEWKtPOBCNBVPBVP
;10    8.000E+00 AYLqYPdYKuAORTNrEPrEP
;10    9.000E+00 QHLaUPT@KEeOCCNbFPbFP
;10    1.000E+01 YSKQSPcTKvEOCYNb@Pb@P
;10    1.100E+01 GhKARPcHKF`OScNRDPRDP
;10    1.200E+01 fRKqDPRhKgAOtENR@PR@P
;10    1.300E+01 eTKaFPrSKWYOtTNBFPBFP
;10    1.400E+01 DfKQIPRRKWeOUBNBDPBDP
;10    1.500E+01 dCKQCPrDKhHOEXNBAPBAP
;10    1.600E+01 sRKAHPRHKXYOEbNQiPQiP
;10    1.800E+01 RdKIcOQbKYGOFUNQfPQfP
;10    2.000E+01 rHKIGOqRKiXOGCNQePQeP
;10    2.200E+01 QgKHROQVKAAPWVNQcPQcP
;10    2.400E+01 aUKGgOARKAFPHENQcPQcP
;10    2.600E+01 AQKwIOqAKQ@PXQNQbPQbP
;10    2.800E+01 aBKVgOaAKQCPXcNQbPQbP
;10    3.000E+01 AFKfPOQBKQGPyCNQbPQbP
;10    4.000E+01 UfJeEOxEJq@PQ@OQdPQdP
;10    5.000E+01 CaJtHOfTJAPPaCOQfPQfP
;10    6.000E+01 bUJsWOUQJAYPqDOB@PB@P
;10    8.000E+01 AYJRhOTAJaQPQPOBFPBFP
;10    1.000E+02 YSIBWOcHJqPPaRORAPRAP
;10    1.500E+02 dCIqVORHJAdPAcOb@Pb@P
;10    2.000E+02 rHIqHOaSJQcPQfObGPbGP
;10    3.000E+02 AFIyWNAHJBDPRCOrEPrEP
;10    4.000E+02 UfHgUNXCIR@PbCOrIPrIP
;10    5.000E+02 CaHvDNVPIRCPr@OBSPBSP
;10    6.000E+02 bUHESNEQIRFPrEOBUPBUP
;10    8.000E+02 AYHdDNDFIb@PBROBYPBYP
;10    1.000E+03 YSGCYNcDIbCPBVORQPRQP
;10    1.500E+03 dCGBSNRFIbFPRSORTPRTP
;10    2.000E+03 rHGAhNaRIbHPRWORVPRVP
;10    3.000E+03 AFGqANAHIr@PbQORXPRXP
;10    4.000E+03 UfFAANX@HrAPbTORYPRYP
;10    5.000E+03 CaFhGMFXHrBPbUORYPRYP
;10    6.000E+03 bUFGAMEPHrCPbVObPPbPP
;10    8.000E+03 AYFEPMDEHrCPbXObPPbPP
;10    1.000E+04 YSEDQMcDHrDPbXObQPbQP
;10    1.500E+04 dCECEMRFHrDPrPObQPbQP
;10    2.000E+04 rHErDMaRHrDPrPObRPbRP
;10    3.000E+04 AFEaRMAHHrEPrQObRPbRP
;10    4.000E+04 UfDaDMX@GrEPrQObRPbRP
;10    5.000E+04 CaDAAMFXGrEPrRObRPbRP
;10    6.000E+04 bUDXTLEPGrEPrRObRPbRP
;10    8.000E+04 AYDVULDEGrEPrRObSPbSP
;10    1.000E+05 YSCuCLcDGrEPrRObSPbSP
;==== ELEMENT  11
;11    1.000E-03 QbRaGPVRT@@R@@RVTTVRT
;11    1.035E-03 QaRqCPUdT@@R@@RUfTUdT
;11    1.072E-03 Q`RqHPEQT@@R@@RESTEQT
;11 K  1.072E-03 Q`RqHPFSU@@R@@RFTUFSU
;11    1.500E-03 qWRQiPSIU@@R@@RSIUSIU
;11    2.000E-03 aTRbTPQRU@@R@@RQRUQRU
;11    3.000E-03 APRSePEFT@@R@@REGTEFT
;11    4.000E-03 QIReHPbET@@R@@RbFTbET
;11    5.000E-03 YcQVVPQHT@@R@@RQITQHT
;11    6.000E-03 xAQwRPVdS@@R@@RGCSVeS
;11    8.000E-03 UcQiRPReS@@R@@RCBSRfS
;11    1.000E-02 DQQQ@QQPS@@R@@RQVSQQS
;11    1.500E-02 BWQq@QtBR@@R@@RdYRDUR
;11    2.000E-02 aQQAPQqVR@@R@@RBFRQ`R
;11    3.000E-02 hPPAXQDeQ@@R@@Rg@QvDQ
;11    4.000E-02 uCPQQQQcQ@@R@@RSgQCTQ
;11    5.000E-02 cQPQQQyGP@@R@@RB`QBTQ
;11    6.000E-02 bPPAYQUIP@@R@@RbGQBAQ
;11    8.000E-02 QSPATQBCP@@R@@RA`QaTQ
;11    1.000E-01 AAPqIQIcO@@R@@RQYQAXQ
;11    1.500E-01 dTOaFQbTO@@R@@RqDQaIQ
;11    2.000E-01 bUOQFQAEO@@R@@Ra@QQGQ
;11    3.000E-01 QIOAAQC@N@@R@@RACQABQ
;11    4.000E-01 vSNYAPaIN@@R@@RYIPYBP
;11    5.000E-01 tANxBPViM@@R@@RxGPxCP
;11    6.000E-01 C@NwPPtFM@@R@@RwTPwQP
;11    8.000E-01 aYNvWPb@M@@R@@RvYPvWP
;11    1.000E+00 AHNFIPqGM@@R@@RV@PFIP
;11    1.022E+00 ADNFBPaIM@@R@@RFCPFBP
;11    1.250E+00 VbMETPxSLRYM@@REUPEUP
;11    1.500E+00 DaMTePvCLARN@@RTgPTfP
;11    2.000E+00 rQMdBPSfLeTN@@RdHPdHP
;11    2.044E+00 RYMTGPCcLFHN@@RdCPdCP
;11    3.000E+00 a@MsBPb@LaQOQFMCYPCYP
;11    4.000E+00 vVLrWPQPLbPOtUMCDPCDP
;11    5.000E+00 tCLrIPQDLCYOIVMrUPrUP
;11    6.000E+00 CALRBPYCKdIOAUNRVPRVP
;11    7.000E+00 bALQ`PgRKTiOQfNBRPBRP
;11    8.000E+00 aYLqSPVSKeSOBUNrBPrBP
;11    9.000E+00 qDLQYPuQKfAORbNbDPbDP
;11    1.000E+01 AHLAWPEGKvSOsGNRHPRHP
;11    1.100E+01 XeKqGPTWKgAOC`NRCPRCP
;11    1.200E+01 WRKaIPTEKgUOd@NR@PR@P
;11    1.300E+01 FPKaAPC`KHEOTXNBGPBGP
;11    1.400E+01 URKQEPSPKHSOTdNBDPBDP
;11    1.500E+01 DaKAIPcEKxYOeINBBPBBP
;11    1.600E+01 dCKADPCDKYBOeQNBAPBAP
;11    1.800E+01 sDKYPObWKyROfBNQhPQhP
;11    2.000E+01 rQKxVOrIKACPvXNQgPQgP
;11    2.200E+01 bDKXCORFKAHPw@NQfPQfP
;11    2.400E+01 AhKgPOQgKQBPwWNQfPQfP
;11    2.600E+01 aPKWDOAaKQFPhANQfPQfP
;11    2.800E+01 qHKvSOaXKa@PhRNQfPQfP
;11    3.000E+01 a@KvHOQVKaCPIANQfPQfP
;11    4.000E+01 vVJEGOQFKqHPAFOQiPQiP
;11    5.000E+01 tCJdCOiCJAYPQIOBCPBCP
;11    6.000E+01 CAJcTOgVJQWPaIOBGPBGP
;11    8.000E+01 aYJBgOuRJqPPATORCPRCP
;11    1.000E+02 AHJrHOTVJA`PQVORIPRIP
;11    1.500E+02 DaIqPOCCJQePqVObIPbIP
;11    2.000E+02 rQIqCObFJBDPAhOrFPrFP
;11    3.000E+02 a@IITNQQJREPBDOBUPBUP
;11    4.000E+02 vVHwINQCJbAPRDORPPRPP
;11    5.000E+02 tCHVBNIBIbEPb@ORSPRSP
;11    6.000E+02 CAHeDNWRIbHPbEORVPRVP
;11    8.000E+02 aYHDINeSIrBPrBORYPRYP
;11    1.000E+03 AHHsGNTQIrEPrGObRPbRP
;11    1.500E+03 DaGrENC@IrHPBTObUPbUP
;11    2.000E+03 rQGAbNbEIBPPBXObWPbWP
;11    3.000E+03 a@GaGNQPIBSPRSObYPbYP
;11    4.000E+03 vVFyWMQCIBTPRUOrPPrPP
;11    5.000E+03 tCFWhMIAHBUPRWOrQPrQP
;11    6.000E+03 CAFvWMWPHBUPRXOrRPrRP
;11    8.000E+03 aYFeBMeSHBVPbPOrRPrRP
;11    1.000E+04 AHFdFMTPHBVPbQOrSPrSP
;11    1.500E+04 DaERdMC@HBWPbROrSPrSP
;11    2.000E+04 rQEbFMbEHBWPbSOrTPrTP
;11    3.000E+04 a@EQVMQPHBXPbTOrTPrTP
;11    4.000E+04 vVDa@MQCHBXPbTOrTPrTP
;11    5.000E+04 tCDyULI@GBXPbTOrTPrTP
;11    6.000E+04 CADhELWPGBXPbTOrTPrTP
;11    8.000E+04 aYDvBLeSGBXPbUOrUPrUP
;11    1.000E+05 AHDUDLTPGBXPbUOrUPrUP
;==== ELEMENT  12
;12    1.000E-03 RERQTPi@T@@R@@RiBTi@T
;12    1.142E-03 BIRAdPFTT@@R@@RFVTFTT
;12    1.305E-03 BCRRGPTQT@@R@@RTSTTQT
;12 K  1.305E-03 BCRRGPETU@@R@@RETUETU
;12    1.500E-03 QfRRSPD@U@@R@@RD@UD@U
;12    2.000E-03 qXRsAPQcU@@R@@RQcUQcU
;12    3.000E-03 QPRTVPVWT@@R@@RVXTVWT
;12    4.000E-03 aHRuSPRfT@@R@@RRgTRfT
;12    5.000E-03 AIRFdPQWT@@R@@RQXTQWT
;12    6.000E-03 y@QGiPiHS@@R@@RyHSiIS
;12    8.000E-03 F`QiXPShS@@R@@RDFSSiS
;12    1.000E-02 UBQQAQBDS@@R@@RRASBES
;12    1.500E-02 BhQqBQUdR@@R@@RvFRFGR
;12    2.000E-02 AhQARQBSR@@R@@RrVRRXR
;12    3.000E-02 A@QQRQvXQ@@R@@Ry@Qx@Q
;12    4.000E-02 fFPQTQrQQ@@R@@RDhQdEQ
;12    5.000E-02 dEPQTQqBQ@@R@@RcIQBgQ
;12    6.000E-02 CGPQSQwEP@@R@@RRWQbFQ
;12    8.000E-02 AbPAXQBiP@@R@@RQeQqWQ
;12    1.000E-01 a@PASQAPP@@R@@RaYQQWQ
;12    1.500E-01 UROq@QsYO@@R@@RqIQqDQ
;12    2.000E-01 SFOa@QQQO@@R@@RaDQaAQ
;12    3.000E-01 AROAEQtCN@@R@@RAFQAEQ
;12    4.000E-01 HCNyIPAgN@@R@@RIYPIQP
;12    5.000E-01 UENXYPAAN@@R@@RhUPhPP
;12    6.000E-01 SXNWePvBM@@R@@RWiPWeP
;12    8.000E-01 BBNVhPSIM@@R@@RGAPViP
;12    1.000E+00 aINfHPQhM@@R@@Rv@PfHP
;12    1.022E+00 aDNfAPAgM@@R@@RfCPfBP
;12    1.250E+00 hGMeRPaGMRdM@@ReSPeRP
;12    1.500E+00 uTMUAPYHLaQN@@RUCPUBP
;12    2.000E+00 cCMtFPuTLvGN@@RDSPDRP
;12    2.044E+00 CIMt@PUULFfN@@RtHPtGP
;12    3.000E+00 ATMCSPSHLAaOa@McQPcQP
;12    4.000E+00 HHLBfPRGLRcOT`MSFPSFP
;12    5.000E+00 UGLBWPaTLScOyVMBgPBgP
;12    6.000E+00 SYLRHPqBLDbOQPNbXPbXP
;12    7.000E+00 bTLQfPQ@LeROBBNRTPRTP
;12    8.000E+00 BBLqYPIQKvDORSNBUPBTP
;12    9.000E+00 aPLaTPhCKVhOCANrGPrGP
;12    1.000E+01 aILQRPwAKWWOCXNrAPrAP
;12    1.100E+01 AGLARPVWKXAOSaNbGPbGP
;12    1.200E+01 XhKqCPUgKhPOtCNbCPbCP
;12    1.300E+01 gUKaEPEWKIFOtRNbAPbAP
;12    1.400E+01 fPKQIPEDKIXOU@NRHPRHP
;12    1.500E+01 uUKQCPdXKIhOEUNRGPRGP
;12    1.600E+01 EEKAGPtGKABPuYNREPREP
;12    1.800E+01 SiKI`OCeKAIPFRNRDPRDP
;12    2.000E+01 cCKIDOCTKQEPViNRCPRCP
;12    2.200E+01 bWKxIOSAKaAPWRNRBPRBP
;12    2.400E+01 bDKGdOBdKaFPHANRBPRBP
;12    2.600E+01 QaKwFObQKqAPHWNRCPRCP
;12    2.800E+01 aUKVeOBQKqEPHiNRCPRCP
;12    3.000E+01 ATKVXObEKqIPiHNRDPRDP
;12    4.000E+01 HHJeCOaWKQUPAIORHPRHP
;12    5.000E+01 UGJtFOqCKaWPaBObCPbCP
;12    6.000E+01 SYJsVOQ@KqWPqCObGPbGP
;12    8.000E+01 BBJRgOhAJQaPAYOrFPrFP
;12    1.000E+02 aIJBVOVUJBAPaPOBRPBRP
;12    1.500E+02 uUIqUOtEJRHPA`ORTPRTP
;12    2.000E+02 cCIqGOcEJbHPQcObQPbQP
;12    3.000E+02 ATIyTNRFJBPPR@OrQPrQP
;12    4.000E+02 HHHgSNaRJBWPb@OrWPrWP
;12    5.000E+02 UGHvANq@JRRPbGOBaPBaP
;12    6.000E+02 SYHEQNAHJRUPrBOBdPBdP
;12    8.000E+02 BBHdBNHIIbPPrIOBhPBhP
;12    1.000E+03 aIHCWNFWIbRPBTOR`PR`P
;12    1.500E+03 uUGBSNtAIbWPRQORdPRdP
;12    2.000E+03 cCGAhNcDIbYPRVORgPRgP
;12    3.000E+03 ATGqANRFIrRPbQORiPRiP
;12    4.000E+03 HHFAANaRIrSPbTOC@PC@P
;12    5.000E+03 UGFhDMaIIrTPbUOCAPCAP
;12    6.000E+03 SYFViMAHIrTPbWOCBPCBP
;12    8.000E+03 BBFuHMHHHrUPbXOCCPCCP
;12    1.000E+04 aIFDPMFWHrVPbYOCCPCCP
;12    1.500E+04 uUECDMtAHrVPrQOCDPCDP
;12    2.000E+04 cCErDMcCHrWPrROCDPCDP
;12    3.000E+04 ATEaQMRFHrWPrROCDPCDP
;12    4.000E+04 HHDaDMaRHrWPrSOCEPCEP
;12    5.000E+04 UGDAAMaIHrXPrSOCEPCEP
;12    6.000E+04 SYDXQLAHHrXPrTOCEPCEP
;12    8.000E+04 BBDVSLHHGrXPrTOCEPCEP
;12    1.000E+05 aIDuALFWGrXPrTOCEPCEP
;==== ELEMENT  13
;13    1.000E-03 bFRASPQHU@@R@@RQIUQHU
;13    1.500E-03 BDRBXPD@T@@R@@RDBTD@T
;13    1.560E-03 BARRYPcPT@@R@@RcRTcPT
;13 K  1.560E-03 BARRYPSfU@@R@@RSfUSfU
;13    2.000E-03 AdRsGPbFU@@R@@RbFUbFU
;13    3.000E-03 QRRtSPGgT@@R@@RGhTGgT
;13    4.000E-03 q@REaPSYT@@R@@RcPTSYT
;13    5.000E-03 QBRvYPQbT@@R@@RQcTQbT
;13    6.000E-03 iTQwPPQDT@@R@@RQETQDT
;13    8.000E-03 gCQiIPTeS@@R@@RECSTfS
;13    1.000E-02 UQQAFQRVS@@R@@RbRSRWS
;13    1.500E-02 SDQaGQWQR@@R@@RWfRgTR
;13    2.000E-02 BEQqGQS@R@@R@@RCTRcDR
;13    3.000E-02 Q@QAVQxRQ@@R@@RQCRABR
;13    4.000E-02 FfPAYQSPQ@@R@@ReXQE@Q
;13    5.000E-02 dXPQPQqRQ@@R@@RcXQcAQ
;13    6.000E-02 sIPAXQYVP@@R@@RrXQBTQ
;13    8.000E-02 B@PATQsXP@@R@@RBBQAbQ
;13    1.000E-01 qBPqIQAdP@@R@@RqPQQWQ
;13    1.500E-01 VBOaGQTiO@@R@@RqHQqBQ
;13    2.000E-01 SPOQGQB@O@@R@@RaBQQIQ
;13    3.000E-01 QXOABQuTN@@R@@RADQACQ
;13    4.000E-01 XcNYFPBXN@@R@@RiHPYIP
;13    5.000E-01 uSNxGPqDN@@R@@RHUPxIP
;13    6.000E-01 SiNwUPHPM@@R@@RG`PwVP
;13    8.000E-01 bENFaPdEM@@R@@RFdPFbP
;13    1.000E+00 ATNVCPbTM@@R@@RVEPVCP
;13    1.022E+00 qHNFFPBYM@@R@@RFHPFGP
;13    1.250E+00 iAMEXPaYMSCM@@RUPPEYP
;13    1.500E+00 vIMThPaBMqQN@@REAPE@P
;13    2.000E+00 cPMdEPgSLvUN@@RtBPtBP
;13    2.044E+00 CTMd@PwHLgGN@@RdHPdGP
;13    3.000E+00 aPMsEPdBLQbOQGMSTPSTP
;13    4.000E+00 I@LrYPBhLS@OtXMSAPSAP
;13    5.000E+00 uVLBQPRHLTEOYRMBdPBdP
;13    6.000E+00 D@LRCPqTLU@OAVNbVPbUP
;13    7.000E+00 RdLQaPAULUdOQgNRSPRSP
;13    8.000E+00 bELqTPaDLfYOBWNBTPBTP
;13    9.000E+00 qXLaPPAILwHORdNrGPrGP
;13    1.000E+01 ATLAXPiVKH@OsINrBPrBP
;13    1.100E+01 QILqHPhYKXVOCbNbHPbHP
;13    1.200E+01 A@Lq@PGiKIHOdCNbEPbEP
;13    1.300E+01 XRKaBPgCKYVOdQNbCPbCP
;13    1.400E+01 wEKQFPfWKA@PTgNbAPbAP
;13    1.500E+01 FPKQ@PVIKADPuBNRIPRIP
;13    1.600E+01 eRKAEPuWKAHPeUNRHPRHP
;13    1.800E+01 DTKYVOEIKQEPfFNRGPRGP
;13    2.000E+01 cPKHbOTUKaBPFbNRGPRGP
;13    2.200E+01 RhKXIOTAKaHPwDNRGPRGP
;13    2.400E+01 RPKgUOsUKqCPGaNRGPRGP
;13    2.600E+01 RCKWIOCUKqHPhFNRHPRHP
;13    2.800E+01 AdKvXOSIKARPhWNRIPRIP
;13    3.000E+01 aPKFRORgKAVPIENb@Pb@P
;13    4.000E+01 I@JU@Ob@KaSPAGObEPbEP
;13    5.000E+01 uVJdFOqUKqVPQIOrAPrAP
;13    6.000E+01 D@JcWOAUKAfPaIOrFPrFP
;13    8.000E+01 bEJBiOAHKBAPAUOBUPBUP
;13    1.000E+02 ATJBPOhTJRBPQVORRPRRP
;13    1.500E+02 FPIqQOuTJbIPqVObTPbTP
;13    2.000E+02 cPIqDOdIJBPPAhOrRPrRP
;13    3.000E+02 aPIYPNBfJRSPBDOBcPBcP
;13    4.000E+02 I@HGTNRDJbPPRDOBiPBiP
;13    5.000E+02 uVHVFNqQJbUPb@ORcPRcP
;13    6.000E+02 D@HeHNASJbXPbFORfPRfP
;13    8.000E+02 bEHTBNAGJrSPrCOC@PC@P
;13    1.000E+03 ATHsINXTIrVPrGOCCPCCP
;13    1.500E+03 FPGrGNeYIBaPBUOCGPCGP
;13    2.000E+03 cPGAcNdGIBcPBYOS@PS@P
;13    3.000E+03 aPGaGNBeIBfPRTOSBPSBP
;13    4.000E+03 I@FIcMRCIBgPRVOSDPSDP
;13    5.000E+03 uVFHDMqQIBhPRXOSEPSEP
;13    6.000E+03 D@FFbMARIBiPRYOSEPSEP
;13    8.000E+03 bEFeEMAGIBiPbQOSFPSFP
;13    1.000E+04 ATFdIMXSHR`PbROSGPSGP
;13    1.500E+04 FPERfMeYHRaPbSOSGPSGP
;13    2.000E+04 cPEbHMdGHRaPbTOSHPSHP
;13    3.000E+04 aPEQWMBeHRaPbUOSHPSHP
;13    4.000E+04 I@DaAMRCHRbPbVOSHPSHP
;13    5.000E+04 uVDIcLqQHRbPbVOSIPSIP
;13    6.000E+04 D@DxALARHRbPbVOSIPSIP
;13    8.000E+04 bEDvGLAGHRbPbVOSIPSIP
;13    1.000E+05 ATDUHLXSGRbPbVOSIPSIP
;==== ELEMENT  14
;14    1.000E-03 RSRqBPQWU@@R@@RQWUQWU
;14    1.500E-03 bIRrIPuCT@@R@@RuFTuCT
;14    1.839E-03 RBRCHPCGT@@R@@RCITCGT
;14 K  1.839E-03 RBRCHPSIU@@R@@RSIUSIU
;14    2.000E-03 BERsIPrWU@@R@@RrXUrWU
;14    3.000E-03 aWRTfPyWT@@R@@RyXTyWT
;14    4.000E-03 APRVCPTQT@@R@@RTSTTQT
;14    5.000E-03 aARWAPBTT@@R@@RBUTBTT
;14    6.000E-03 AERWhPAVT@@R@@RAWTAVT
;14    8.000E-03 HDQYQPvHS@@R@@RFWSvIS
;14    1.000E-02 fBQAHQsAS@@R@@RsISsCS
;14    1.500E-02 SYQaIQIeR@@R@@RACSYhR
;14    2.000E-02 rDQAPQDIR@@R@@RDVRdCR
;14    3.000E-02 aEQQPQQFR@@R@@RATRqAR
;14    4.000E-02 GiPQSQdYQ@@R@@RGAQfBQ
;14    5.000E-02 EPPQTQrAQ@@R@@RtHQCeQ
;14    6.000E-02 SbPQSQaIQ@@R@@RcAQBaQ
;14    8.000E-02 rBPAXQUBP@@R@@RbCQB@Q
;14    1.000E-01 QTPASQRPP@@R@@RAdQaXQ
;14    1.500E-01 WCOqAQFaO@@R@@RAUQqHQ
;14    2.000E-01 DHOaAQrTO@@R@@RaHQaCQ
;14    3.000E-01 AdOAFQGhN@@R@@RAHQAFQ
;14    4.000E-01 ADOIXPCQN@@R@@RiQPYQP
;14    5.000E-01 vPNhVPAeN@@R@@RxUPhXP
;14    6.000E-01 dVNHBPQFN@@R@@RHHPHCP
;14    8.000E-01 bRNGEPEeM@@R@@RGHPGFP
;14    1.000E+00 aXNvDPcTM@@R@@RvFPvDP
;14    1.022E+00 aQNfGPCSM@@R@@RfIPfHP
;14    1.250E+00 AHNeWPrCMSRM@@ReYPeXP
;14    1.500E+00 GWMUEPaXMQaN@@RUHPUHP
;14    2.000E+00 d@MDPPAEMWSN@@RDXPDXP
;14    2.044E+00 DCMtEPABMXBN@@RDSPDSP
;14    3.000E+00 AgMCVPE`LRDOaAMcXPcXP
;14    4.000E+00 AEMBiPSeLCVOTdMcDPcDP
;14    5.000E+00 vSLBYPRhLdSOIeMRgPRgP
;14    6.000E+00 dWLb@PrILeXOQQNrYPrYP
;14    7.000E+00 CTLQhPQiLfQOBDNbVPbVP
;14    8.000E+00 bSLA`PqPLGUORUNRWPRWP
;14    9.000E+00 BHLaVPAYLhAOCDNRQPRQP
;14    1.000E+01 aXLQTPqBLX`OSQNBVPBVP
;14    1.100E+01 qILASPQILYSOSeNBSPBSP
;14    1.200E+01 QGLqDPAHLAAPtGNBPPBPP
;14    1.300E+01 YfKaGPIiKAFPtWNrHPrHP
;14    1.400E+01 XYKa@PYBKQAPUDNrFPrFP
;14    1.500E+01 GXKQDPHVKQFPUPNrEPrEP
;14    1.600E+01 VWKAHPGiKa@PEdNrDPrDP
;14    1.800E+01 UIKIiOVeKaHPFXNrDPrDP
;14    2.000E+01 dAKYBOfBKqEPGENrDPrDP
;14    2.200E+01 CXKHWOeRKARPWYNrDPrDP
;14    2.400E+01 RbKWbOUBKAXPHHNrEPrEP
;14    2.600E+01 BYKGTOtQKQSPXTNrFPrFP
;14    2.800E+01 REKGAOtFKQXPXfNrGPrGP
;14    3.000E+01 AgKfTODFKaSPyFNrHPrHP
;14    4.000E+01 AEKeHOCAKAbPQ@OBUPBUP
;14    5.000E+01 vSJDQOrIKQfPaCORRPRRP
;14    6.000E+01 dWJC`OQiKBGPqCORXPRXP
;14    8.000E+01 bSJRiOAXKbDPAYObYPbYP
;14    1.000E+02 aXJBYOQHKrEPaQOrVPrVP
;14    1.500E+02 GXIqWOGdJRUPAaORaPRaP
;14    2.000E+02 dAIqIOEfJbVPQdOC@PC@P
;14    3.000E+02 AgIIcNS`JB`PR@OSAPSAP
;14    4.000E+02 AEIwPNRbJBhPb@OSHPSHP
;14    5.000E+02 vSHvGNrDJRdPbGOcCPcCP
;14    6.000E+02 dWHEVNQeJRhPrCOcFPcFP
;14    8.000E+02 bSHdFNAVJCCPBPOsAPsAP
;14    1.000E+03 aXHSQNQGJCFPBUOsDPsDP
;14    1.500E+03 GXGBUNwWISAPRROsIPsIP
;14    2.000E+03 dAGQ`NEcISDPRWOCQPCQP
;14    3.000E+03 AgGqBNCiISGPbQOCTPCTP
;14    4.000E+03 AEGABNRaISIPbTOCVPCVP
;14    5.000E+03 vSFxBMrCISIPbVOCWPCWP
;14    6.000E+03 dWFGEMQdIc@PbWOCXPCXP
;14    8.000E+03 bSFESMAVIcAPbYOCYPCYP
;14    1.000E+04 aXFDTMQGIcBPrPOCYPCYP
;14    1.500E+04 GXECGMwWHcBPrQOSPPSPP
;14    2.000E+04 dAErFMEcHcCPrROSPPSPP
;14    3.000E+04 AgEaSMCiHcDPrSOSQPSQP
;14    4.000E+04 AEEaEMRaHcDPrSOSQPSQP
;14    5.000E+04 vSDABMrCHcDPrTOSQPSQP
;14    6.000E+04 dWDXYLQdHcDPrTOSQPSQP
;14    8.000E+04 bSDVYLAVHcDPrTOSQPSQP
;14    1.000E+05 aXDuFLQGHcDPrTOSRPSRP
;==== ELEMENT  15
;15    1.000E-03 bVRQAPQaU@@R@@RQaUQaU
;15    1.500E-03 BQRBHPVRT@@R@@RVUTVRT
;15    2.000E-03 RFRCCPC@T@@R@@RCBTC@T
;15    2.145E-03 R@RcIPBWT@@R@@RBYTBWT
;15 K  2.145E-03 R@RcIPBWU@@R@@RBWUBWU
;15    3.000E-03 qTRdTPQBU@@R@@RQBUQBU
;15    4.000E-03 AUREiPeCT@@R@@ReDTeCT
;15    5.000E-03 aDRFhPBeT@@R@@RBfTBeT
;15    6.000E-03 AHRwRPqQT@@R@@RqSTqRT
;15    8.000E-03 xCQYCPWWS@@R@@RgVSWXS
;15    1.000E-02 VTQACQSfS@@R@@RDDSSgS
;15    1.500E-02 CbQaCQQIS@@R@@RaDSa@S
;15    2.000E-02 RPQqEQTgR@@R@@RuERU@R
;15    3.000E-02 qDQAUQARR@@R@@RqPRQWR
;15    4.000E-02 HUPAXQuWQ@@R@@RX@QgEQ
;15    5.000E-02 EaPAXQBeQ@@R@@RTbQtDQ
;15    6.000E-02 dBPAXQaPQ@@R@@RCYQCGQ
;15    8.000E-02 RQPATQvGP@@R@@RrBQBGQ
;15    1.000E-01 aVPqIQSBP@@R@@RAgQqPQ
;15    1.500E-01 wROaGQXSO@@R@@RASQqFQ
;15    2.000E-01 DSOQGQCTO@@R@@RaEQaAQ
;15    3.000E-01 B@OABQYdN@@R@@RAEQACQ
;15    4.000E-01 QCOi@PtAN@@R@@RyFPiEP
;15    5.000E-01 gINHQPrDN@@R@@RXQPHTP
;15    6.000E-01 EGNwYPAVN@@R@@RGePG`P
;15    8.000E-01 BfNFePGQM@@R@@RFhPFfP
;15    1.000E+00 AcNVFPdQM@@R@@RVHPVFP
;15    1.022E+00 qUNV@PtEM@@R@@RVBPV@P
;15    1.250E+00 QGNUQPReMsPM@@RUSPUQP
;15    1.500E+00 XDMEAPRCMB@N@@REDPECP
;15    2.000E+00 TXMdGPqCMGfN@@RtFPtEP
;15    2.044E+00 tHMdBPaHMHWN@@RtAPtAP
;15    3.000E+00 BDMsFPwCLbCOQHMSYPSYP
;15    4.000E+00 QDMBaPTiLcPOD`MSGPSGP
;15    5.000E+00 wCLBRPsVLDbOYWMRbPRaP
;15    6.000E+00 EILRDPCALUaOAWNrUPrUP
;15    7.000E+00 sTLQbPRQLFhOQhNbSPbSP
;15    8.000E+00 BfLqUPRELwUOBXNRUPRUP
;15    9.000E+00 bFLaQPAhLXTORfNBYPBYP
;15    1.000E+01 AcLAYPaWLiFOCQNBUPBUP
;15    1.100E+01 QQLqIPQPLYaOCdNBRPBRP
;15    1.200E+01 aGLq@PqFLAEPdDNBPPBPP
;15    1.300E+01 AHLaCPaDLQAPdSNrHPrHP
;15    1.400E+01 yEKQFPQELQFPTiNrGPrGP
;15    1.500E+01 XDKQ@PAFLaAPuDNrFPrFP
;15    1.600E+01 WFKAEPYcKaEPeWNrFPrFP
;15    1.800E+01 eUKiQOxUKqCPfINrFPrFP
;15    2.000E+01 TXKHfOGbKAQPFeNrFPrFP
;15    2.200E+01 sYKhCOGGKAWPwGNrGPrGP
;15    2.400E+01 SHKgYOFTKQTPGdNrHPrHP
;15    2.600E+01 rQKgBOUbKQYPhINBPPBPP
;15    2.800E+01 rDKFaOEXKaTPxPNBQPBQP
;15    3.000E+01 BDKFUOU@KaYPIHNBSPBSP
;15    4.000E+01 QDKUCOsXKAiPAGORQPRQP
;15    5.000E+01 wCJdHOCAKBCPQIORXPRXP
;15    6.000E+01 EIJcYOBYKREPaIObUPbUP
;15    8.000E+01 BfJRaOAfKrBPAUOrVPrVP
;15    1.000E+02 AcJBQOAXKBTPQVOBdPBdP
;15    1.500E+02 XDIqROIeJbTPqUORiPRiP
;15    2.000E+02 TXIqEOwGJrVPAhOCHPCHP
;15    3.000E+02 BDIYUNT`JR`PBDOc@Pc@P
;15    4.000E+02 QDIGXNcWJRiPRCOcHPcHP
;15    5.000E+02 wCHVINRdJCDPb@OsCPsCP
;15    6.000E+02 EIHu@NBUJCIPbEOsFPsFP
;15    8.000E+02 BfHTDNAcJSDPrBOCQPCQP
;15    1.000E+03 AcHCQNAWJSGPrGOCUPCUP
;15    1.500E+03 XDGrHNyWIcCPBTOSPPSPP
;15    2.000E+03 TXGAdNwBIcEPBXOSRPSRP
;15    3.000E+03 BDGaHNDhIcIPRROSUPSUP
;15    4.000E+03 QDGIhMcVIs@PRUOSWPSWP
;15    5.000E+03 wCFHHMRcIsAPRVOSXPSXP
;15    6.000E+03 EIFFeMBTIsBPRXOSXPSXP
;15    8.000E+03 BfFeHMAcIsCPRYOSYPSYP
;15    1.000E+04 AcFtAMAVIsCPbPOcPPcPP
;15    1.500E+04 XDERhMyVHsDPbROcQPcQP
;15    2.000E+04 TXEbIMwBHsEPbROcQPcQP
;15    3.000E+04 BDEQXMDhHsEPbSOcRPcRP
;15    4.000E+04 QDEaAMcVHsFPbSOcRPcRP
;15    5.000E+04 wCDIgLRcHsFPbTOcRPcRP
;15    6.000E+04 EIDxELBTHsFPbTOcRPcRP
;15    8.000E+04 BfDFPLAcHsFPbTOcRPcRP
;15    1.000E+05 AcDeALAVHsFPbTOcRPcRP
;==== ELEMENT  16
;16    1.000E-03 ReRAAPBSU@@R@@RBSUBSU
;16    1.500E-03 bYRQePxAT@@R@@RxDTxAT
;16    2.000E-03 BSRRbPCcT@@R@@RCeTCcT
;16    2.472E-03 RHRsWPRET@@R@@RRGTRET
;16 K  2.472E-03 RHRsWPBGU@@R@@RBGUBGU
;16    3.000E-03 QeRdTPqDU@@R@@RqDUqDU
;16    4.000E-03 aPRF@PvBT@@R@@RvDTvBT
;16    5.000E-03 qERGGPCWT@@R@@RCYTCWT
;16    6.000E-03 QGRWePR@T@@R@@RRBTR@T
;16    8.000E-03 YAQyFPyFS@@R@@RIVSyGS
;16    1.000E-02 gCQAEQTcS@@R@@REASTdS
;16    1.500E-02 t@QaEQAYS@@R@@RQUSQQS
;16    2.000E-02 BbQqGQfIR@@R@@RvQRFSR
;16    3.000E-02 QQQAXQAaR@@R@@RRARQfR
;16    4.000E-02 YTPQQQGPQ@@R@@RIgQXbQ
;16    5.000E-02 VXPQRQcWQ@@R@@REeQUIQ
;16    6.000E-02 D`PQQQBFQ@@R@@RDEQSWQ
;16    8.000E-02 BfPAWQhFP@@R@@RRYQr@Q
;16    1.000E-01 AiPASQDEP@@R@@RBBQAcQ
;16    1.500E-01 HbOqAQQAP@@R@@RQQQARQ
;16    2.000E-01 EGOaAQTQO@@R@@Rq@QaEQ
;16    3.000E-01 bIOAFQqAO@@R@@RAIQAGQ
;16    4.000E-01 q@OIXPeWN@@R@@RiWPYTP
;16    5.000E-01 xENhWPCHN@@R@@RxXPxPP
;16    6.000E-01 EaNHBPQcN@@R@@RX@PHDP
;16    8.000E-01 cHNGFPyXM@@R@@RW@PGGP
;16    1.000E+00 R@NvEPFHM@@R@@RvGPvEP
;16    1.022E+00 BANfHPuTM@@R@@Rv@PfHP
;16    1.250E+00 qDNeXPCiMTAM@@RuPPeXP
;16    1.500E+00 yCMUFPBbMbAN@@RUIPUHP
;16    2.000E+00 eEMDPPqUMhWN@@RTPPDYP
;16    2.044E+00 ECMtEPaYMyDN@@RDUPDTP
;16    3.000E+00 rCMCVPiTLBUOaAMsQPsQP
;16    4.000E+00 qAMBiPVULSfOTeMcIPcIP
;16    5.000E+00 HPLRPPTdLeIOIfMCDPCDP
;16    6.000E+00 EdLbAPSeLFYOQQNBgPBgP
;16    7.000E+00 dILQhPcILWVOBDNrVPrVP
;16    8.000E+00 cHLA`PBbLXQORUNbXPbXP
;16    9.000E+00 RYLaVPBVLyHOCDNbSPbSP
;16    1.000E+01 R@LQTPRHLABPSQNRYPRYP
;16    1.100E+01 qTLASPQfLAIPSfNRVPRVP
;16    1.200E+01 AVLqDPqXLQEPtGNRTPRTP
;16    1.300E+01 aDLaGPaSLaAPtWNRSPRSP
;16    1.400E+01 AGLa@PQPLaGPUENRRPRRP
;16    1.500E+01 yDKQDPqILqBPUPNRRPRRP
;16    1.600E+01 hAKAHPq@LqGPEdNRRPRRP
;16    1.800E+01 FXKY`OQELAVPFXNRRPRRP
;16    2.000E+01 eEKYCOABLQTPGENRSPRSP
;16    2.200E+01 tDKHXOiEKaRPWYNRTPRTP
;16    2.400E+01 cUKWcOHTKaXPHHNRVPRVP
;16    2.600E+01 SAKGTOwUKqUPXSNRXPRXP
;16    2.800E+01 bXKGBOWHKA`PXeNRYPRYP
;16    3.000E+01 rCKfUOfXKAePyENbQPbQP
;16    4.000E+01 qAKeHOTeKBGPQ@OrQPrQP
;16    5.000E+01 HPJDQOScKbCPaCOrYPrYP
;16    6.000E+01 EdJC`OcFKrFPqCOBgPBgP
;16    8.000E+01 cHJC@OBSKRTPAYORiPRiP
;16    1.000E+02 R@JBYOQdKbWPaPOCHPCHP
;16    1.500E+02 yDIqWOaIKBiPA`OcEPcEP
;16    2.000E+02 eEIqIOiTJCBPQcOsFPsFP
;16    3.000E+02 rCIIdNFQJSHPBIOCYPCYP
;16    4.000E+02 qAIwQND`JcGPRIOSWPSWP
;16    5.000E+02 HPHvHNCdJsCPbFOcRPcRP
;16    6.000E+02 EdHEVNc@JsHPrAOcVPcVP
;16    8.000E+02 cHHdGNBPJCTPrHOsRPsRP
;16    1.000E+03 R@HSQNQbJCWPBSOsUPsUP
;16    1.500E+03 yDGBUNaHJSSPRPOCaPCaP
;16    2.000E+03 eEGQ`NYXISVPRTOCdPCdP
;16    3.000E+03 rCGqBNvIISYPRYOCgPCgP
;16    4.000E+03 qAGABNtYIcQPbQOChPChP
;16    5.000E+03 HPFxCMCcIcRPbSOS`PS`P
;16    6.000E+03 EdFGFMSIIcSPbTOS`PS`P
;16    8.000E+03 cHFETMrIIcTPbUOSaPSaP
;16    1.000E+04 R@FDTMQbIcUPbVOSbPSbP
;16    1.500E+04 yDECGMaHIcVPbXOScPScP
;16    2.000E+04 eEErFMYWHcVPbYOScPScP
;16    3.000E+04 rCEaSMvHHcWPbYOSdPSdP
;16    4.000E+04 qAEaEMtYHcWPrPOSdPSdP
;16    5.000E+04 HPDABMCcHcWPrPOSdPSdP
;16    6.000E+04 EdDhPLSIHcWPrPOSdPSdP
;16    8.000E+04 cHDVYLrIHcXPrPOSePSeP
;16    1.000E+05 R@DuGLQbHcXPrQOSePSeP
;==== ELEMENT  17
;17    1.000E-03 CCRXVOBcU@@R@@RBcUBcU
;17    1.500E-03 B`RaYPyTT@@R@@RyWTyTT
;17    2.000E-03 RSRRYPDYT@@R@@RTRTDYT
;17    2.822E-03 RBRSgPqUT@@R@@RqWTqUT
;17 K  2.822E-03 RBRSgPaSU@@R@@RaTUaSU
;17    3.000E-03 BDRdEPAWU@@R@@RAWUAWU
;17    4.000E-03 aVReRPGBT@@R@@RGDTGBT
;17    5.000E-03 qIRvQPCiT@@R@@RS`TCiT
;17    6.000E-03 a@RWXPrGT@@R@@RrHTrGT
;17    8.000E-03 iGQXcPAFT@@R@@RAHTAGT
;17    1.000E-02 GQQA@QeTS@@R@@RuSSeUS
;17    1.500E-02 DYQQIQqSS@@R@@RqXSqTS
;17    2.000E-02 RfQqAQwAR@@R@@RwTRGTR
;17    3.000E-02 QXQAQQRCR@@R@@RBSRbGR
;17    4.000E-02 A@QAUQxRQ@@R@@RQBRABR
;17    5.000E-02 VaPAUQtDQ@@R@@RFXQuYQ
;17    6.000E-02 EEPAUQBTQ@@R@@RtIQCiQ
;17    8.000E-02 CBPAQQIbP@@R@@RrPQrIQ
;17    1.000E-01 B@PqGQDcP@@R@@RBEQAeQ
;17    1.500E-01 yDOaEQqDP@@R@@RAXQqIQ
;17    2.000E-01 uHOQFQERO@@R@@RaGQaAQ
;17    3.000E-01 BTOAAQQWO@@R@@RAEQACQ
;17    4.000E-01 qHOY@PFeN@@R@@RyAPYGP
;17    5.000E-01 HhNxCPsSN@@R@@RHUPxFP
;17    6.000E-01 VHNwQPrCN@@R@@RG`PwSP
;17    8.000E-01 CXNvXPQHN@@R@@RFcPvYP
;17    1.000E+00 bCNV@PwFM@@R@@RVCPVAP
;17    1.022E+00 RDNFCPVfM@@R@@RFFPFDP
;17    1.250E+00 ASNEUPtQMdDM@@REXPEVP
;17    1.500E+00 YbMTfPCQMbGN@@RTiPThP
;17    2.000E+00 UXMdCPRBMHhN@@RtCPtBP
;17    2.044E+00 uEMTHPBEMYWN@@RdHPdHP
;17    3.000E+00 BXMsCPQFMRQOQGMSYPSXP
;17    4.000E+00 APMrXPWaLDDOtVMSIPSIP
;17    5.000E+00 XdLBPPUeLEQOIXMRePReP
;17    6.000E+00 fALRBPtVLfROAVNB`PB`P
;17    7.000E+00 TVLQaPSfLwQOQfNrPPrPP
;17    8.000E+00 CYLqSPsILhYOBUNbSPbSP
;17    9.000E+00 rVLQYPRfLYWORcNRXPRXP
;17    1.000E+01 bCLAXPbSLADPsGNRUPRUP
;17    1.100E+01 AeLqHPrFLQAPC`NRSPRSP
;17    1.200E+01 QULaIPRDLQHPd@NRQPRQP
;17    1.300E+01 qBLaBPQfLaDPTXNRPPRPP
;17    1.400E+01 QDLQEPAaLq@PTdNRPPRPP
;17    1.500E+01 YcKAIPaXLqEPeINRPPRPP
;17    1.600E+01 xSKADPQVLAPPeQNRPPRPP
;17    1.800E+01 V`KYROqHLAYPfBNRQPRQP
;17    2.000E+01 UYKxXOaCLQWPvXNRRPRRP
;17    2.200E+01 dRKXEOQALaUPgINRTPRTP
;17    2.400E+01 ChKgROAALqRPwVNRVPRVP
;17    2.600E+01 sAKWEOyBKqXPXINRXPRXP
;17    2.800E+01 BeKvUOhRKAdPhPNbPPbPP
;17    3.000E+01 BXKvIOHBKAiPXhNbRPbRP
;17    4.000E+01 APKEHOUeKRAPAFOrRPrRP
;17    5.000E+01 XdJdDOtSKbGPQHOBaPBaP
;17    6.000E+01 fAJcUOSbKBPPaHOBiPBiP
;17    8.000E+01 CYJBhORcKRYPASOCBPCBP
;17    1.000E+02 bCJrIOrCKrRPQTOSAPSAP
;17    1.500E+02 YcIqPOQUKRdPqSOcIPcIP
;17    2.000E+02 UYIqCOQFKCHPAeOCPPCPP
;17    3.000E+02 BXIIVNwPJcDPB@OSSPSSP
;17    4.000E+02 APIGQNuWJsCPR@OcQPcQP
;17    5.000E+02 XdHVCNdQJsIPRFOcWPcWP
;17    6.000E+02 fAHeENCdJCTPbAOsQPsQP
;17    8.000E+02 CYHT@NBhJSPPbHOsVPsVP
;17    1.000E+03 bCHsGNr@JSSPrBOC`PC`P
;17    1.500E+03 YcGrFNQSJSYPrIOCePCeP
;17    2.000E+03 UYGAbNQEJcRPBSOChPChP
;17    3.000E+03 BXGaGNgWIcUPBWOSaPSaP
;17    4.000E+03 APGyYMuUIcWPRPOScPScP
;17    5.000E+03 XdFH@MdPIcXPRQOSdPSdP
;17    6.000E+03 fAFvXMCcIcYPRROSePSeP
;17    8.000E+03 CYFeCMBgIsPPRSOSfPSfP
;17    1.000E+04 bCFdGMr@IsQPRTOSgPSgP
;17    1.500E+04 YcEReMQSIsRPRVOShPShP
;17    2.000E+04 UYEbGMQEIsRPRVOShPShP
;17    3.000E+04 BXEQVMgWHsSPRWOSiPSiP
;17    4.000E+04 APEa@MuUHsSPRWOSiPSiP
;17    5.000E+04 XdDyXLdPHsSPRXOSiPSiP
;17    6.000E+04 fADhGLCcHsSPRXOSiPSiP
;17    8.000E+04 CYDvDLBgHsTPRXOSiPSiP
;17    1.000E+05 bCDUFLr@HsTPRXOSiPSiP
;==== ELEMENT  18
;18    1.000E-03 CDRGHOSHU@@R@@RSHUSHU
;18    1.500E-03 BbRARPQ@U@@R@@RQ@UQ@U
;18    2.000E-03 RWRb@PEIT@@R@@RUBTEIT
;18    3.000E-03 BHRsRPaXT@@R@@RqPTaXT
;18    3.203E-03 QiRD@PAPT@@R@@RARTAPT
;18 K  3.203E-03 QiRD@PaGU@@R@@RaGUaGU
;18    4.000E-03 aYREBPWUT@@R@@RWWTWUT
;18    5.000E-03 AQRV@PdAT@@R@@RdCTdAT
;18    6.000E-03 a@RVgPRXT@@R@@RRYTRXT
;18    8.000E-03 iEQhIPQGT@@R@@RQHTQGT
;18    1.000E-02 GQQiIPfCS@@R@@RvBSfDS
;18    1.500E-02 TVQQ@QQcS@@R@@RQhSQdS
;18    2.000E-02 CBQaAQhAR@@R@@RhSRxCR
;18    3.000E-02 aRQqBQBPR@@R@@RrPRRSR
;18    4.000E-02 ABQqEQYaQ@@R@@RaCRQCR
;18    5.000E-02 GGPqFQTeQ@@R@@RGAQv@Q
;18    6.000E-02 UHPqEQrYQ@@R@@RdVQTEQ
;18    8.000E-02 SAPqBQQCQ@@R@@RrVQBUQ
;18    1.000E-01 BFPaHQUVP@@R@@RBDQAdQ
;18    1.500E-01 iSOQHQQTP@@R@@RASQqCQ
;18    2.000E-01 UUOAIQfHO@@R@@RaAQQEQ
;18    3.000E-01 RROYRPAcO@@R@@RYePyPP
;18    4.000E-01 ASOXUPWhN@@R@@RxXPhSP
;18    5.000E-01 YHNGbPtDN@@R@@RWfPGgP
;18    6.000E-01 vINgDPrRN@@R@@RwCPgGP
;18    8.000E-01 cPNvGPqHN@@R@@RFRPvHP
;18    1.000E+00 rANuSPXYM@@R@@RuVPuTP
;18    1.022E+00 bANeWPXBM@@R@@RuPPeXP
;18    1.250E+00 AXNUCPUPMdFM@@RUEPUDP
;18    1.500E+00 ACNdVPShMbGN@@RtPPdXP
;18    2.000E+00 uXMShPBWMHfN@@RDGPDGP
;18    2.044E+00 USMScPrIMYUN@@RDCPDBP
;18    3.000E+00 RWMSCPqFMRPOAIMsHPsHP
;18    4.000E+00 ATMbQPi@LDCODWMCBPCBP
;18    5.000E+00 iELbEPVbLuHOXaMB`PB`P
;18    6.000E+00 FRLQiPUSLVYOqGNbWPbWP
;18    7.000E+00 tRLqYPdPLgWOAdNRXPRXP
;18    8.000E+00 cQLaSPScLhTOrANRRPRRP
;18    9.000E+00 BfLQPPCTLYQOrUNBXPBXP
;18    1.000E+01 rALqIPCELACPSGNBUPBUP
;18    1.100E+01 QaLaIPrTLQ@PSWNBSPBSP
;18    1.200E+01 aQLaAPBYLQGPSeNBRPBRP
;18    1.300E+01 qGLQDPbGLaCPt@NBRPBRP
;18    1.400E+01 QHLAHPR@LaIPdTNBRPBRP
;18    1.500E+01 ACLACPQdLqDPTgNBRPBRP
;18    1.600E+01 ICKyXOAaLqIPeGNBRPBRP
;18    1.800E+01 WDKXdOaPLAXPEdNBTPBTP
;18    2.000E+01 uXKhEOASLQVPvFNBUPBUP
;18    2.200E+01 tXKgVOaILaTPFdNBWPBWP
;18    2.400E+01 DAKWFOQHLqQPgHNRPPRPP
;18    2.600E+01 CRKvROAHLqWPgYNRRPRRP
;18    2.800E+01 ReKvDOYiKAbPHGNRTPRTP
;18    3.000E+01 RWKF@Oy@KAhPHSNRVPRVP
;18    4.000E+01 AUKtWOFiKBIPYaNbWPbWP
;18    5.000E+01 iEJShOEXKbEPQAOrVPrVP
;18    6.000E+01 FRJCSOTTKrHPa@OBdPBdP
;18    8.000E+01 cQJrQOsIKRWPqDORgPRgP
;18    1.000E+02 rAJbEOrPKrPPATOCGPCGP
;18    1.500E+02 ACJaPOqYKRbPaROcDPcDP
;18    2.000E+02 uXIaEOqDKCEPqSOsEPsEP
;18    3.000E+02 RWIHiNXbJcAPAhOCYPCYP
;18    4.000E+02 AUIVfNfXJs@PQfOSWPSWP
;18    5.000E+02 iEHuVNuDJsFPBBOcRPcRP
;18    6.000E+02 FRHTcNDUJCQPBGOcVPcVP
;18    8.000E+02 cQHCeNsCJCVPRCOsRPsRP
;18    1.000E+03 rAHSGNbWJSPPRGOsUPsUP
;18    1.500E+03 ACHbANqXJSVPbCOC`PC`P
;18    2.000E+03 uXGqQNqCJSYPbGOCcPCcP
;18    3.000E+03 RWGQINHhIcRPr@OCfPCfP
;18    4.000E+03 AUGi@MfVIcTPrCOChPChP
;18    5.000E+03 iEFWRMuCIcUPrDOCiPCiP
;18    6.000E+03 FRFvHMDTIcVPrEOS`PS`P
;18    8.000E+03 cQFTaMsCIcWPrFOSaPSaP
;18    1.000E+04 rAFDAMbVIcWPrGOSaPSaP
;18    1.500E+04 ACFrWMqXIcXPrHOSbPSbP
;18    2.000E+04 uXERCMqCIcYPrIOScPScP
;18    3.000E+04 RWEAWMHhHcYPBPOScPScP
;18    4.000E+04 AUEQCMfVHsPPBPOSdPSdP
;18    5.000E+04 iEDYILuCHsPPBPOSdPSdP
;18    6.000E+04 FRDwWLDTHsPPBPOSdPSdP
;18    8.000E+04 cQDUeLsCHsPPBPOSdPSdP
;18    1.000E+05 rADDeLbVHsPPBQOSdPSdP
;==== ELEMENT  19
;19    1.000E-03 sERaBPDEU@@R@@RDFUDEU
;19    1.500E-03 CHRQfPAQU@@R@@RARUAQU
;19    2.000E-03 BbRrPPVVT@@R@@RVYTVVT
;19    3.000E-03 rCRTFPRGT@@R@@Rb@TRHT
;19    3.607E-03 BGRThPqAT@@R@@RqCTqAT
;19 K  3.607E-03 BGRThPa@U@@R@@Ra@Ua@U
;19    4.000E-03 QbREVPiDT@@R@@RiFTiDT
;19    5.000E-03 aPRVXPUGT@@R@@RUITUGT
;19    6.000E-03 qFRWQPSIT@@R@@Rc@TSIT
;19    8.000E-03 ADRXcPAVT@@R@@RAWTAVT
;19    1.000E-02 xFQYhPGaS@@R@@RWaSGbS
;19    1.500E-02 eCQQHQBTS@@R@@RRPSBUS
;19    2.000E-02 CYQq@QAES@@R@@RAISAFS
;19    3.000E-02 AgQAQQCHR@@R@@RCQRcBR
;19    4.000E-02 QHQAUQaHR@@R@@RQTRARR
;19    5.000E-02 hAPAVQFPQ@@R@@RhXQGfQ
;19    6.000E-02 FCPAUQcRQ@@R@@ReXQEHQ
;19    8.000E-02 cSPARQAWQ@@R@@RcEQBiQ
;19    1.000E-01 BQPqHQgFP@@R@@RrDQR@Q
;19    1.500E-01 QCPaGQBCP@@R@@RQXQAWQ
;19    2.000E-01 VQOQGQhFO@@R@@RqBQaEQ
;19    3.000E-01 RfOACQBRO@@R@@RAHQAEQ
;19    4.000E-01 aXOiBPAEO@@R@@RIYPyCP
;19    5.000E-01 AHOHSPuUN@@R@@RhPPHYP
;19    6.000E-01 WQNGaPcPN@@R@@RWbPGeP
;19    8.000E-01 dDNFgPAcN@@R@@RVcPFiP
;19    1.000E+00 rRNVHPQDN@@R@@RfBPVIP
;19    1.022E+00 bPNVAPAHN@@R@@RVEPVBP
;19    1.250E+00 qTNUSPgIMTaM@@RUVPUTP
;19    1.500E+00 aANEBPeGMbPN@@REGPEFP
;19    2.000E+00 F`MdIPcGMAAO@@RDPPtIP
;19    2.044E+00 VQMdDPSFMAIO@@RtEPtEP
;19    3.000E+00 CBMsGPqYMBeOQHMcWPcVP
;19    4.000E+00 qPMBbPaAMTYODbMcHPcHP
;19    5.000E+00 AIMBSPYBLVBOiQMCEPCEP
;19    6.000E+00 WVLREPgILWPOAWNRaPRaP
;19    7.000E+00 UULQcPFFLxROQiNBbPBbP
;19    8.000E+00 dELqVPUHLIbOBYNrWPrWP
;19    9.000E+00 sFLaRPTRLAHPRfNrSPrSP
;19    1.000E+01 rRLQPPDALQGPCRNrPPrPP
;19    1.100E+01 bELAPPcPLaEPCeNbYPbYP
;19    1.200E+01 AiLqAPcGLqCPdFNbXPbXP
;19    1.300E+01 aQLaCPRiLAPPdTNbXPbXP
;19    1.400E+01 qILQGPrVLAVPEANbXPbXP
;19    1.500E+01 aALQAPRVLQRPuENbYPbYP
;19    1.600E+01 AFLAFPrHLQXPeXNbYPbYP
;19    1.800E+01 HPKiTOR@LaYPv@NrQPrQP
;19    2.000E+01 F`KHiOAhLqXPFfNrTPrTP
;19    2.200E+01 eRKhFOaYLAfPwGNrVPrVP
;19    2.400E+01 tRKwROQTLQdPGeNrYPrYP
;19    2.600E+01 DBKgEOARLBAPhINBbPBbP
;19    2.800E+01 CWKFdOqALBGPxPNBdPBdP
;19    3.000E+01 CBKFWOaBLRCPIHNBgPBgP
;19    4.000E+01 qPKUDOIFKrHPAGOC@PC@P
;19    5.000E+01 AIKdIOg@KRVPQIOSAPSAP
;19    6.000E+01 WVJsPOUgKrPPaIOc@Pc@P
;19    8.000E+01 dEJRbODUKRaPATOsEPsEP
;19    1.000E+02 rRJBROSUKCGPQUOCVPCVP
;19    1.500E+02 aAJqSOrEKsAPqTOcVPcVP
;19    2.000E+02 F`IqEOqVKCVPAgOsXPsXP
;19    3.000E+02 CBIYYNQGKcTPBBOSdPSdP
;19    4.000E+02 qPIWQNxWJsUPRAODCPDCP
;19    5.000E+02 AIIfANGAJCaPRHODIPDIP
;19    6.000E+02 WVHuBNEdJCfPbCOTDPTDP
;19    8.000E+02 dEHTFNtHJScPbIOd@Pd@P
;19    1.000E+03 rRHCRNSPJSgPrDOdDPdDP
;19    1.500E+03 aAHrINrCJDDPBQOt@Pt@P
;19    2.000E+03 F`GAeNqUJDGPBUOtDPtDP
;19    3.000E+03 CBGaHNQGJTAPBYOtGPtGP
;19    4.000E+03 qPGYbMxUITCPRROtIPtIP
;19    5.000E+03 AIGXAMG@ITDPRSODPPDPP
;19    6.000E+03 WVFFhMEcITEPRTODQPDQP
;19    8.000E+03 dEFu@MtGITFPRVODRPDRP
;19    1.000E+04 rRFtCMSPITGPRWODSPDSP
;19    1.500E+04 aAFRiMrCITHPRXODTPDTP
;19    2.000E+04 F`Er@MqUITIPRYODUPDUP
;19    3.000E+04 CBEQXMQGITIPbPODUPDUP
;19    4.000E+04 qPEaBMxTHd@PbPODVPDVP
;19    5.000E+04 AIEYaLViHd@PbPODVPDVP
;19    6.000E+04 WVDxHLEcHd@PbQODVPDVP
;19    8.000E+04 dEDFRLtGHd@PbQODVPDVP
;19    1.000E+05 rRDeBLSPHd@PbQODVPDVP
;==== ELEMENT  20
;20    1.000E-03 SXRAYPDfU@@R@@RDgUDfU
;20    1.500E-03 cFRrFPqQU@@R@@RqQUqQU
;20    2.000E-03 RfRS@PWgT@@R@@RH@TWgT
;20    3.000E-03 BVRDXPbUT@@R@@RbXTbUT
;20    4.000E-03 BERuRPa@T@@R@@RaBTa@T
;20    4.038E-03 BDRuVPQGT@@R@@RQITQGT
;20 K  4.038E-03 BDRuVPABU@@R@@RABUABU
;20    5.000E-03 qRRvYPFAT@@R@@RFCTFAT
;20    6.000E-03 AVRwRPsRT@@R@@RsSTsRT
;20    8.000E-03 QBRYGPqQT@@R@@RqSTqRT
;20    1.000E-02 XeQABQiDS@@R@@RyDSiES
;20    1.500E-02 eWQaAQRaS@@R@@RRhSRbS
;20    2.000E-02 CbQqBQaES@@R@@RqASaGS
;20    3.000E-02 BFQATQsSR@@R@@RDHRCgR
;20    4.000E-02 q@QAXQQUR@@R@@RAcRqPR
;20    5.000E-02 IDPAYQG`Q@@R@@RABRiIQ
;20    6.000E-02 fUPAYQDSQ@@R@@RVXQUaQ
;20    8.000E-02 DAPAUQA`Q@@R@@RcVQcEQ
;20    1.000E-01 bWPAQQXcP@@R@@RRWQr@Q
;20    1.500E-01 aEPq@QRPP@@R@@RaWQQUQ
;20    2.000E-01 gCOa@QABP@@R@@RqHQq@Q
;20    3.000E-01 cIOAEQC@O@@R@@RQBQAHQ
;20    4.000E-01 AgOIVPqAO@@R@@RyXPiPP
;20    5.000E-01 a@OhVPWFN@@R@@RHePxSP
;20    6.000E-01 xFNHBPDYN@@R@@RXEPHFP
;20    8.000E-01 tRNGEPbHN@@R@@RWBPGGP
;20    1.000E+00 CBNvDPARN@@R@@RvIPvFP
;20    1.022E+00 BiNfHPqDN@@R@@RvBPfIP
;20    1.250E+00 QdNeXPIIMuFM@@RuQPeYP
;20    1.500E+00 qDNUFPVVMBbN@@ReAPUIP
;20    2.000E+00 WWMDPPDGMQ@O@@RTRPTRP
;20    2.044E+00 gEMtEPScMQIO@@RDXPDWP
;20    3.000E+00 sFMCWPbCMCHOaAMsXPsXP
;20    4.000E+00 AiMBiPQQMTfOTeMCPPsIP
;20    5.000E+00 aAMRPPQCMfROIfMSGPSGP
;20    6.000E+00 HQLbAPIELX@OQQNCCPCCP
;20    7.000E+00 VHLQhPWRLIROBDNRePReP
;20    8.000E+00 tSLA`PFSLAFPRUNBiPBiP
;20    9.000E+00 sTLaVPeQLQGPCDNBfPBfP
;20    1.000E+01 CCLQTPTgLaGPSQNBdPBdP
;20    1.100E+01 RPLASPDWLqEPSeNBcPBcP
;20    1.200E+01 R@LqDPDELATPtGNBbPBbP
;20    1.300E+01 qYLaGPsQLQQPtVNBcPBbP
;20    1.400E+01 QTLa@PCRLQXPUDNBcPBcP
;20    1.500E+01 qELQDPSGLaUPUPNBdPBdP
;20    1.600E+01 QHLAHPRfLqQPEcNBePBeP
;20    1.800E+01 yEKY`ObPLAbPFVNBhPBgP
;20    2.000E+01 WWKYCOrBLQbPGDNR`PR`P
;20    2.200E+01 fFKHXOR@LBAPWWNRcPRcP
;20    2.400E+01 eFKWcOQaLBIPHFNRgPRfP
;20    2.600E+01 DXKGTOqVLRGPXQNC@PC@P
;20    2.800E+01 CfKGBOaSLbDPXcNCCPCCP
;20    3.000E+01 sGKfUOQQLr@PyBNCFPCFP
;20    4.000E+01 AiKeHOQBLRVPQ@Oc@Pc@P
;20    5.000E+01 aAKDQOXaKrVPaBOsBPsBP
;20    6.000E+01 HQJC`OwIKRaPqBOCRPCRP
;20    8.000E+01 tSJC@OUQKSDPAXOSYPSYP
;20    1.000E+02 CCJBYOtIKs@PQYOsQPsQP
;20    1.500E+02 qEJqWORaKSWPqYOScPScP
;20    2.000E+02 WWIqIORHKsSPQaODFPDFP
;20    3.000E+02 sFIIdNAUKSbPBGOdCPdCP
;20    4.000E+02 AiIwQNAIKDDPRGOtCPtCP
;20    5.000E+02 aAIvHNhXJTAPbCODPPDPP
;20    6.000E+02 HQHEVNgCJTGPbHODUPDUP
;20    8.000E+02 tSHdGNERJdDPrEOTRPTRP
;20    1.000E+03 CCHSQNtCJdHPBPOTVPTVP
;20    1.500E+03 qEHBUNBiJtEPBWOdRPdRP
;20    2.000E+03 WWGQ`NRGJtIPRROdVPdVP
;20    3.000E+03 sFGqBNATJDSPRVOtPPtPP
;20    4.000E+03 AiGABNAHJDUPRYOtRPtRP
;20    5.000E+03 aAGxCMhVIDWPbQOtTPtTP
;20    6.000E+03 HQFGFMgAIDXPbROtUPtUP
;20    8.000E+03 tSFETMEQIDYPbSOtVPtVP
;20    1.000E+04 CCFDTMtCITPPbTOtWPtWP
;20    1.500E+04 qEFCGMBiITQPbVOtXPtXP
;20    2.000E+04 WWErFMRFITRPbWOtXPtXP
;20    3.000E+04 sFEaSMATITRPbWOtYPtYP
;20    4.000E+04 AiEaEMAHITRPbXOtYPtYP
;20    5.000E+04 aAEABMhVHTSPbXOD`PD`P
;20    6.000E+04 HQDhPLgAHTSPbXOD`PD`P
;20    8.000E+04 tSDVYLEQHTSPbYOD`PD`P
;20    1.000E+05 CCDuGLtCHTSPbYOD`PD`P
;==== ELEMENT  21
;21    1.000E-03 SURq@PeCU@@R@@ReDUeCU
;21    1.500E-03 cDRR@PAeU@@R@@RAfUAeU
;21    2.000E-03 ReRB`PhXT@@R@@RxPThXT
;21    3.000E-03 BVRDFPR`T@@R@@RRbTR`T
;21    4.000E-03 BERe@PqAT@@R@@RqCTqAT
;21    4.493E-03 AhRuQPIYS@@R@@RiYSYPS
;21 K  4.493E-03 AhRuQPXCT@@R@@RXETXCT
;21    5.000E-03 qRRf@PfIT@@R@@RvATfIT
;21    6.000E-03 AVRGGPSbT@@R@@RScTSbT
;21    8.000E-03 QARHVPAbT@@R@@RAcTAbT
;21    1.000E-02 HdQIXPIeS@@R@@RYeSIfS
;21    1.500E-02 eRQQBQSCS@@R@@Rc@SSES
;21    2.000E-02 CbQaCQqFS@@R@@RAQSqGS
;21    3.000E-02 BFQqDQDGR@@R@@RDQRd@R
;21    4.000E-02 q@QqHQqPR@@R@@RQgRAdR
;21    5.000E-02 IFPqIQXXQ@@R@@RAIRYgQ
;21    6.000E-02 fXPqIQDhQ@@R@@RVcQfGQ
;21    8.000E-02 DDPqFQQiQ@@R@@RsUQsEQ
;21    1.000E-01 rPPqBQYaP@@R@@RRXQrAQ
;21    1.500E-01 aGPaAQrXP@@R@@RaRQAYQ
;21    2.000E-01 wAOQBQQDP@@R@@RqAQaDQ
;21    3.000E-01 sCOIePsFO@@R@@RAEQABQ
;21    4.000E-01 AiOHfPAWO@@R@@RYIPI@P
;21    5.000E-01 aBOX@PHDN@@R@@RxAPXHP
;21    6.000E-01 HXNWPPEDN@@R@@RgTPWUP
;21    8.000E-01 tXNfPPRVN@@R@@RfWPfSP
;21    1.000E+00 CGNUdPQYN@@R@@RUhPUeP
;21    1.022E+00 RdNEhPQQN@@R@@RUbPEiP
;21    1.250E+00 QfNuAPABNuDM@@RuEPuCP
;21    1.500E+00 qGNDcPwHMrYN@@RDhPDfP
;21    2.000E+00 gXMTBPTWMAIO@@RdDPdDP
;21    2.044E+00 wEMDGPDRMQGO@@Rd@PTIP
;21    3.000E+00 CQMcDPRPMCDOQDMSUPSUP
;21    4.000E+00 QbMrQPaYMDhOdSMc@Pc@P
;21    5.000E+00 aCMrDPaGMVQOiCMC@PC@P
;21    6.000E+00 XTLBGPAAMWfOARNBhPBhP
;21    7.000E+00 fGLAfPHRLiFOQaNB`PB`P
;21    8.000E+00 D`LaYPWILADPrINrVPrVP
;21    9.000E+00 sYLQUPfGLQEPBeNrSPrSP
;21    1.000E+01 CGLATPUVLaDPcHNrRPrRP
;21    1.100E+01 RTLqDPE@LqCPsPNrQPrQP
;21    1.200E+01 RCLaFPTSLAQPDINrQPrQP
;21    1.300E+01 AbLQIPTELAXPDVNrQPrQP
;21    1.400E+01 QWLQBPCbLQUPDaNrRPrRP
;21    1.500E+01 qGLAGPSTLaRPUDNrSPrSP
;21    1.600E+01 a@LAAPs@LaXPEVNrUPrUP
;21    1.800E+01 IYKiGORaLqYPFENrWPrWP
;21    2.000E+01 gXKXUObPLAhPVYNB`PB`P
;21    2.200E+01 vEKWdOrELQgPGHNBdPBdP
;21    2.400E+01 uDKGRORDLBEPWTNBgPBgP
;21    2.600E+01 TUKVgOQgLRCPWfNR`PR`P
;21    2.800E+01 SbKVWOAbLRIPxENRdPRdP
;21    3.000E+01 CRKfBOaYLbFPxRNRgPRgP
;21    4.000E+01 QbKTeOaELRQPABOSAPSAP
;21    5.000E+01 aCKTCOYeKrQPQDOcCPcCP
;21    6.000E+01 XTJSVOhEKBfPaDOsDPsDP
;21    8.000E+01 D`JBaOVEKCHPqHOSPPSPP
;21    1.000E+02 CGJrCOT`KcDPAYOcRPcRP
;21    1.500E+02 qGJaVOcEKSPPaWOCcPCcP
;21    2.000E+02 gXIq@OBSKcVPqYOSgPSgP
;21    3.000E+02 CQIiANaRKCePQcOTCPTCP
;21    4.000E+02 QbIgBNaAKSePBBOdCPdCP
;21    5.000E+02 aCIUgNiYJDCPBHOt@Pt@P
;21    6.000E+02 XTHUANHGJDHPRCOtDPtDP
;21    8.000E+02 D`HSiNFEJTEPRIODQPDQP
;21    1.000E+03 CGHcINDdJd@PbDODUPDUP
;21    1.500E+03 qGHbINcBJdFPr@OTRPTRP
;21    2.000E+03 gXGqXNBRJt@PrDOTUPTUP
;21    3.000E+03 CQGaDNaQJtDPrHOTYPTYP
;21    4.000E+03 QbGYSMaAJtFPBQOdQPdQP
;21    5.000E+03 aCGwYMiVItHPBROdSPdSP
;21    6.000E+03 XTFfQMHEItHPBTOdSPdSP
;21    8.000E+03 D`FEIMFDIDPPBUOdUPdUP
;21    1.000E+04 CGFTFMDcIDPPBVOdUPdUP
;21    1.500E+04 qGFBgMcBIDRPBWOdWPdWP
;21    2.000E+04 gXEbAMBRIDRPBXOdWPdWP
;21    3.000E+04 CQEQRMaQIDSPBYOdXPdXP
;21    4.000E+04 QbEQGMaAIDSPBYOdXPdXP
;21    5.000E+04 aCEYRLiVHDSPRPOdXPdXP
;21    6.000E+04 XTDHELHEHDSPRPOdXPdXP
;21    8.000E+04 D`DVGLFDHDTPRPOdYPdYP
;21    1.000E+05 CGDEBLDcHDTPRPOdYPdYP
;==== ELEMENT  22
;22    1.000E-03 cYRQHPEfU@@R@@REgUEfU
;22    1.500E-03 sIRQePBIU@@R@@RR@UBIU
;22    2.000E-03 CIRbRPIcT@@R@@RIfTIcT
;22    3.000E-03 RXRCdPs@T@@R@@RsBTs@T
;22    4.000E-03 RFRTdPQPT@@R@@RQRTQPT
;22    4.966E-03 AbREhPXIS@@R@@RxHSh@S
;22 K  4.966E-03 AbREhPFfT@@R@@RFhTFfT
;22    5.000E-03 AaRUaPFbT@@R@@RFdTFbT
;22    6.000E-03 QTRvWPtAT@@R@@RtBTtAT
;22    8.000E-03 QFRXFPBAT@@R@@RBBTBAT
;22    1.000E-02 i@QYIPQ@T@@R@@RQATQ@T
;22    1.500E-02 EeQAIQSRS@@R@@RSYSSSS
;22    2.000E-02 DAQa@QQSS@@R@@RQYSQUS
;22    3.000E-02 RGQqAQdRR@@R@@RTgRtUR
;22    4.000E-02 qGQqEQQdR@@R@@RbARBHR
;22    5.000E-02 YSPqFQIbQ@@R@@RaARQBR
;22    6.000E-02 GBPqFQePQ@@R@@RgVQVfQ
;22    8.000E-02 dFPqCQr@Q@@R@@RDEQcSQ
;22    1.000E-01 BePaIQQDQ@@R@@RrRQBTQ
;22    1.500E-01 qDPQIQcCP@@R@@RaUQQQQ
;22    2.000E-01 wUOQ@QqCP@@R@@RqAQaDQ
;22    3.000E-01 SSOiXPSbO@@R@@RADQAAQ
;22    4.000E-01 BAOxQPqRO@@R@@RIHPHhP
;22    5.000E-01 aIOWgPIPN@@R@@RXIPHFP
;22    6.000E-01 IANwHPU`N@@R@@RWSPGTP
;22    8.000E-01 EHNFYPC@N@@R@@RVWPVRP
;22    1.000E+00 cFNEdPAgN@@R@@REiPEfP
;22    1.022E+00 SBNuXPqWN@@R@@REcPE`P
;22    1.250E+00 BINeBPa@NUWM@@ReFPeDP
;22    1.500E+00 AUNtUPhTMR`N@@RD`PtYP
;22    2.000E+00 XFMDEPuEMQBO@@RTHPTGP
;22    2.044E+00 GaMD@PUGMaAO@@RTDPTCP
;22    3.000E+00 cSMSIPRbMSCOQBMSQPSQP
;22    4.000E+00 BDMbVPQgMECOTVMSGPSGP
;22    5.000E+00 qAMr@PAXMvQOIHMRhPRhP
;22    6.000E+00 IGLBCPQHMh@OqINBgPBgP
;22    7.000E+00 fWLAcPIaLYSOAhNB`PB`P
;22    8.000E+00 U@LaVPxHLAGPrENrVPrVP
;22    9.000E+00 DCLQSPwALQHPB`NrTPrTP
;22    1.000E+01 cGLAQPFXLaHPcCNrSPrSP
;22    1.100E+01 rPLqBPEbLqGPcTNrSPrSP
;22    1.200E+01 bGLaDPeHLAUPDBNrSPrSP
;22    1.300E+01 QcLQGPDcLQSPtHNrTPrTP
;22    1.400E+01 aWLQ@PDULaPPtSNrUPrUP
;22    1.500E+01 AULAEPTBLaVPEFNrVPrVP
;22    1.600E+01 aHLYhOCdLqRPuGNrXPrXP
;22    1.800E+01 AALYBOsHLAdPUeNBaPBaP
;22    2.000E+01 XGKHQOCBLQdPFWNBdPBdP
;22    2.200E+01 vUKGaOrSLBCPVfNBhPBhP
;22    2.400E+01 eWKw@OBYLRAPGQNRbPRbP
;22    2.600E+01 DcKFeObILRIPGbNRePReP
;22    2.800E+01 TGKFVORALbFPhANRiPRiP
;22    3.000E+01 cSKVBOQgLrBPXWNCBPCBP
;22    4.000E+01 BDKDgOAVLRYPAAOSGPSGP
;22    5.000E+01 qAKDFOQFLrXPQBOs@Ps@P
;22    6.000E+01 IGJSPOYYKRdPaAOCQPCQP
;22    8.000E+01 U@JrVOWEKSGPqFOSXPSXP
;22    1.000E+02 cGJbIOuPKsCPAVOsQPsQP
;22    1.500E+02 AUJaSOsXKcPPaTOSbPSbP
;22    2.000E+02 XGIaHOBcKsVPqUODFPDFP
;22    3.000E+02 cSIIFNAhKSePAiOdCPdCP
;22    4.000E+02 BDIW@NAQKDFPQhOtCPtCP
;22    5.000E+02 qAIEhNQCKTDPBDODPPDPP
;22    6.000E+02 IGHECNyHJTIPBIODUPDUP
;22    8.000E+02 U@HScNGCJdFPREOTRPTRP
;22    1.000E+03 cGHcCNeRJtAPRIOTVPTVP
;22    1.500E+03 AUHbFNsUJtHPbEOdRPdRP
;22    2.000E+03 XGGqUNBaJDQPbIOdVPdVP
;22    3.000E+03 cSGaANAgJDUPrCOtPPtPP
;22    4.000E+03 BDGyHMAPJDXPrFOtRPtRP
;22    5.000E+03 qAGgWMQBJDYPrGOtSPtSP
;22    6.000E+03 IGFVPMyFITPPrHOtTPtTP
;22    8.000E+03 U@FEAMGBITQPrIOtVPtVP
;22    1.000E+04 cGFDIMeQITRPBPOtVPtVP
;22    1.500E+04 AUFBcMsTITSPBROtWPtWP
;22    2.000E+04 XGERGMBaITTPBROtXPtXP
;22    3.000E+04 cSEQPMAgITTPBSOtYPtYP
;22    4.000E+04 BDEQEMAPITUPBTOtYPtYP
;22    5.000E+04 qAEyGLQBITUPBTOtYPtYP
;22    6.000E+04 IGDWbLyFHTUPBTOtYPtYP
;22    8.000E+04 U@DFGLGBHTUPBTOD`PD`P
;22    1.000E+05 cGDTdLeQHTUPBTOD`PD`P
;==== ELEMENT  23
;23    1.000E-03 CbRAGPFYU@@R@@RVPUFYU
;23    1.500E-03 SRRA`PrDU@@R@@RrDUrDU
;23    2.000E-03 cCRBSPQ@U@@R@@RQAUQ@U
;23    3.000E-03 rQRSYPsRT@@R@@RsTTsRT
;23    4.000E-03 bGRdUPaYT@@R@@RqQTaYT
;23    5.000E-03 QaRePPIIS@@R@@RiISY@S
;23    5.465E-03 qWRF@PGIS@@R@@RgHSW@S
;23 K  5.465E-03 qWRF@PEeT@@R@@REgTEeT
;23    6.000E-03 aRRFSPdWT@@R@@RdYTdWT
;23    8.000E-03 aBRGaPb@T@@R@@RbBTb@T
;23    1.000E-02 iQQHfPaAT@@R@@RaBTaAT
;23    1.500E-02 FIQAFQSaS@@R@@RShSSbS
;23    2.000E-02 TIQQFQqQS@@R@@RqWSqSS
;23    3.000E-02 bHQaHQeAR@@R@@RUVRuDR
;23    4.000E-02 ATQqBQb@R@@R@@RBWRrCR
;23    5.000E-02 YiPqCQQAR@@R@@RqERaER
;23    6.000E-02 wGPqCQvGQ@@R@@RHTQwPQ
;23    8.000E-02 DXPq@QbRQ@@R@@RtGQSbQ
;23    1.000E-01 C@PaGQqAQ@@R@@RBhQRXQ
;23    1.500E-01 AQPQGQsQP@@R@@RaXQQTQ
;23    2.000E-01 XHOAHQQSP@@R@@RqBQaDQ
;23    3.000E-01 sSOYQPTSO@@R@@RACQYfP
;23    4.000E-01 RBOXUPQiO@@R@@RXgPxUP
;23    5.000E-01 qGOGcPAIO@@R@@RHGPWdP
;23    6.000E-01 YSNgEPFdN@@R@@RGQPwBP
;23    8.000E-01 uHNvHPCXN@@R@@RFWPFQP
;23    1.000E+00 CUNuTPRFN@@R@@RuYPuVP
;23    1.022E+00 s@NeXPBEN@@R@@RuSPuPP
;23    1.250E+00 bANUCPqINuYM@@RUGPUEP
;23    1.500E+00 QSNdWPA@NC@N@@RtRPtQP
;23    2.000E+00 hTMShPf@MQFO@@RTAPTAP
;23    2.044E+00 hGMScPUiMaEO@@RDGPDGP
;23    3.000E+00 CdMSDPsHMcBOQ@MCWPCVP
;23    4.000E+00 RFMbQPbHMUGODXMSDPSDP
;23    5.000E+00 qHMbFPqQMFiOXbMRfPRfP
;23    6.000E+00 iPLB@PqFMHROqGNBfPBeP
;23    7.000E+00 GELqYPQCMyYOAeNrYPrYP
;23    8.000E+00 EPLaSPiWLQ@PrANrVPrVP
;23    9.000E+00 dGLQPPHSLaAPrUNrTPrTP
;23    1.000E+01 CVLqIPGWLqAPSGNrTPrTP
;23    1.100E+01 BfLq@PvQLAPPSWNrTPrTP
;23    1.200E+01 BPLaBPFILAYPSeNrUPrTP
;23    1.300E+01 BELQEPUWLQWPtANrVPrVP
;23    1.400E+01 qVLAHPUCLaTPdTNrWPrWP
;23    1.500E+01 QTLACPtULqQPTgNrYPrYP
;23    1.600E+01 qELI`ODSLqWPeGNB`PB`P
;23    1.800E+01 AGLXfOS`LAhPEdNBdPBdP
;23    2.000E+01 hTKhFOCXLQiPvFNBhPBhP
;23    2.200E+01 WDKgWOSDLBHPFdNRbPRbP
;23    2.400E+01 F@KWGOBgLRGPgHNRfPRfP
;23    2.600E+01 UAKvSObSLbDPgXNRiPRiP
;23    2.800E+01 DQKvEOBTLrAPHFNCCPCCP
;23    3.000E+01 CdKFAObGLrHPHQNCGPCGP
;23    4.000E+01 RFKtXOaXLbUPIhNcCPcCP
;23    5.000E+01 qHKSiOqCLBePQ@OsFPsFP
;23    6.000E+01 iPJCTOQ@LCAPQIOCWPCWP
;23    8.000E+01 EPJrQOhCKcDPqCOcUPcUP
;23    1.000E+02 CVJbEOVVKCQPASOsXPsXP
;23    1.500E+02 QTJaPOtEKcXPaQODAPDAP
;23    2.000E+02 hTIaFOcFKCePqROTEPTEP
;23    3.000E+02 CdIXaNRGKDEPAeOtBPtBP
;23    4.000E+02 RFIVgNaRKTFPQdODRPDRP
;23    5.000E+02 qHIuWNq@KdCPB@ODYPDYP
;23    6.000E+02 iPHTdNAHKdIPBDOTTPTTP
;23    8.000E+02 EPHCfNX@JtFPR@OdQPdQP
;23    1.000E+03 CVHSHNFWJDQPRDOdUPdUP
;23    1.500E+03 QTHbBNtAJDXPb@OtRPtRP
;23    2.000E+03 hTGqRNcCJTQPbDOtUPtUP
;23    3.000E+03 CdGQINRFJTUPbHOtYPtYP
;23    4.000E+03 RFGiAMaRJTXPr@ODbPDbP
;23    5.000E+03 qHGWSMaIJTYPrAODcPDcP
;23    6.000E+03 iPFvIMAHJdPPrBODdPDdP
;23    8.000E+03 EPFTbMHHIdQPrDODePDeP
;23    1.000E+04 CVFDBMFWIdRPrDODfPDfP
;23    1.500E+04 QTFrXMtAIdSPrFODgPDgP
;23    2.000E+04 hTERCMcCIdTPrFODgPDgP
;23    3.000E+04 CdEAWMRFIdTPrGODhPDhP
;23    4.000E+04 RFEQCMaRIdUPrGODhPDhP
;23    5.000E+04 qHEiALaIIdUPrHODiPDiP
;23    6.000E+04 iPDwXLAHIdUPrHODiPDiP
;23    8.000E+04 EPDUgLHHHdUPrHODiPDiP
;23    1.000E+05 CVDDfLFVHdUPrHODiPDiP
;==== ELEMENT  24
;24    1.000E-03 TDRXTOGPU@@R@@RGPUGPU
;24    1.500E-03 CfRAYPbYU@@R@@RbYUbYU
;24    2.000E-03 SWRRAPaGU@@R@@RaHUaGU
;24    3.000E-03 CBRcHPtAT@@R@@RtDTtAT
;24    4.000E-03 RSRtFPQfT@@R@@RQiTQfT
;24    5.000E-03 RCRuCPAFT@@R@@RAHTAFT
;24    5.989E-03 AaRVIPvIS@@R@@RVWSvIS
;24 K  5.989E-03 AaRVIPUDT@@R@@RUFTUDT
;24    6.000E-03 A`Rf@PUDT@@R@@RUFTUDT
;24    8.000E-03 qERgVPRPT@@R@@RRQTRPT
;24    1.000E-02 AERxXPqHT@@R@@RqITqHT
;24    1.500E-02 fSQAFQDYS@@R@@RTWSTQS
;24    2.000E-02 TWQQGQQhS@@R@@RBDSQiS
;24    3.000E-02 BYQaIQFFR@@R@@RFSRVHR
;24    4.000E-02 QWQqDQRVR@@R@@RBfRrPR
;24    5.000E-02 AIQqFQqAR@@R@@RQURATR
;24    6.000E-02 HEPqEQGXQ@@R@@RiTQHcQ
;24    8.000E-02 T`PqCQCIQ@@R@@RT`QDQQ
;24    1.000E-01 cIPaIQQUQ@@R@@RSGQBdQ
;24    1.500E-01 QUPQIQDPP@@R@@RqYQaSQ
;24    2.000E-01 XhOQAQAbP@@R@@RqHQaIQ
;24    3.000E-01 T@OyRPEPO@@R@@RAGQACQ
;24    4.000E-01 rCOxTPrHO@@R@@RiAPXhP
;24    5.000E-01 QPOH@Pq@O@@R@@RhHPXCP
;24    6.000E-01 AEOGQPXHN@@R@@RgPPGYP
;24    8.000E-01 UaNVRPTFN@@R@@RfRPVVP
;24    1.000E+00 sYNEgPRYN@@R@@RUcPEiP
;24    1.022E+00 cSNE`PBVN@@R@@REfPEcP
;24    1.250E+00 BSNeEPaVNfFM@@ReIPeGP
;24    1.500E+00 aYNtWPa@NcBN@@RDcPDaP
;24    2.000E+00 YPMDGPGQMaDO@@RdAPd@P
;24    2.044E+00 IIMDBPWFMqDO@@RTGPTFP
;24    3.000E+00 dBMc@PDCMCUOQBMSVPSUP
;24    4.000E+00 rHMbWPrRMUROTXMcCPcCP
;24    5.000E+00 QRMrAPBDMwEOYBMCFPCFP
;24    6.000E+00 AFMBDPaRMXhOAPNRfPReP
;24    7.000E+00 wVLAcPqEMADPAiNR`PR`P
;24    8.000E+00 UdLaWPQEMQGPrFNBgPBgP
;24    9.000E+00 dYLQSPA@MaIPBaNBfPBfP
;24    1.000E+01 C`LARPX`LAPPcDNBfPBeP
;24    1.100E+01 SDLqCPWiLQPPcUNBfPBfP
;24    1.200E+01 bTLaDPgDLQYPDDNBgPBgP
;24    1.300E+01 bELQGPfSLaWPDPNBhPBhP
;24    1.400E+01 QdLQAPV@LqUPtUNR`PR`P
;24    1.500E+01 aYLAEPeVLAbPEHNRbPRbP
;24    1.600E+01 AXLA@PeGLAhPuINRdPRdP
;24    1.800E+01 QGLYFOdTLBAPUgNRhPRhP
;24    2.000E+01 YPKHUOTDLRBPVPNCCPCCP
;24    2.200E+01 GeKGeOsTLbBPVhNCGPCGP
;24    2.400E+01 fPKwCOCQLrAPGSNSAPSAP
;24    2.600E+01 eRKFhOSCLrIPGeNSFPSFP
;24    2.800E+01 DeKFYOR`LBVPhCNc@Pc@P
;24    3.000E+01 dBKVEObYLRTPXYNcDPcDP
;24    4.000E+01 rHKDiOB@LBbPAAOCQPCQP
;24    5.000E+01 QRKDHOQXLCDPQBOSVPSVP
;24    6.000E+01 AFKSQOqALc@PaBOcXPcXP
;24    8.000E+01 UdJrWOyYKCUPqFOCgPCgP
;24    1.000E+02 C`Jr@OG`KcSPAVODAPDAP
;24    1.500E+02 aYJaTOUGKSbPaTOdEPdEP
;24    2.000E+02 YPIaHOCgKT@PqUODPPDPP
;24    3.000E+02 dBIY@NRWKt@PAiOTXPTXP
;24    4.000E+02 rHIWCNQcKDRPQgOdYPdYP
;24    5.000E+02 QRIU`NQTKTPPBCOtVPtVP
;24    6.000E+02 AFIEENaHKTVPBGODbPDbP
;24    8.000E+02 UdHSeNiRJdSPRCODiPDiP
;24    1.000E+03 C`HcENgYJdXPRGOTcPTcP
;24    1.500E+03 aYHbGNUCJtUPbCOE@PE@P
;24    2.000E+03 YPGqUNCdJtYPbFOEDPEDP
;24    3.000E+03 dBGaBNRVJDdPr@OEHPEHP
;24    4.000E+03 rHGIRMQbJDfPrBOU@PU@P
;24    5.000E+03 QRGwPMQTJDgPrCOUAPUAP
;24    6.000E+03 AFGVSMaHJDhPrDOUBPUBP
;24    8.000E+03 UdFECMiPIT`PrFOUDPUDP
;24    1.000E+04 C`FTAMgXIT`PrGOUDPUDP
;24    1.500E+04 aYFBdMUBITbPrHOUFPUFP
;24    2.000E+04 YPERHMCdITbPrHOUFPUFP
;24    3.000E+04 dBEQQMRVITcPrIOUGPUGP
;24    4.000E+04 rHEQFMQbITcPrIOUGPUGP
;24    5.000E+04 QREIQLQTITcPBPOUGPUGP
;24    6.000E+04 AFEWfLaHITdPBPOUHPUHP
;24    8.000E+04 UdDV@LiPHTdPBPOUHPUHP
;24    1.000E+05 C`DTfLgXHTdPBPOUHPUHP
;==== ELEMENT  25
;25    1.000E-03 dDRiDOHIU@@R@@RHIUHIU
;25    1.500E-03 SeRQYPRhU@@R@@RRhURhU
;25    2.000E-03 cURRIPARU@@R@@RARUARU
;25    3.000E-03 CIRcHPDbT@@R@@RDeTDbT
;25    4.000E-03 bQRdHPb@T@@R@@RbCTb@T
;25    5.000E-03 bARUIPQIT@@R@@RaATQIT
;25    6.000E-03 AhRFAPWFS@@R@@RwESWFS
;25    6.539E-03 qTRFRPeRS@@R@@RE`SeSS
;25 K  6.539E-03 qTRFRPTPT@@R@@RTRTTPT
;25    8.000E-03 AQRGQPrRT@@R@@RrSTrRT
;25    1.000E-02 Q@RXRPQPT@@R@@RQQTQPT
;25    1.500E-02 FhQADQTeS@@R@@RECSTfS
;25    2.000E-02 tVQQEQRIS@@R@@RbESb@S
;25    3.000E-02 bQQaGQvUR@@R@@RWDRFhR
;25    4.000E-02 aUQqBQBgR@@R@@RSGRC@R
;25    5.000E-02 QDQqCQAWR@@R@@RqQRaPR
;25    6.000E-02 HTPqCQHRQ@@R@@RAFRyUQ
;25    8.000E-02 UEPqAQCYQ@@R@@RuAQtYQ
;25    1.000E-01 CUPaGQqUQ@@R@@RsGQCBQ
;25    1.500E-01 aSPQHQE@P@@R@@RAdQaXQ
;25    2.000E-01 IUOAIQBGP@@R@@RqIQq@Q
;25    3.000E-01 tBOYWPVGO@@R@@RAFQABQ
;25    4.000E-01 BVOhQPrRO@@R@@RYCPHiP
;25    5.000E-01 QYOGhPAYO@@R@@RXIPHCP
;25    6.000E-01 Q@Ow@PyIN@@R@@RWQPGPP
;25    8.000E-01 fDNFSPtXN@@R@@RVTPFWP
;25    1.000E+00 D@NuXPRgN@@R@@REePEaP
;25    1.022E+00 CcNuRPBbN@@R@@RuYPuUP
;25    1.250E+00 RVNUGPQaNVRM@@ReBPe@P
;25    1.500E+00 qXNtPPqHNsDN@@RtWPtUP
;25    2.000E+00 A@NDBPHYMaHO@@RTFPTEP
;25    2.044E+00 iPMSfPh@MqHO@@RTBPTAP
;25    3.000E+00 DVMSFPdRMSUOQAMSRPSRP
;25    4.000E+00 RQMbTPSAMeWOTQMcAPcAP
;25    5.000E+00 aPMbHPrCMWUOXiMCDPCDP
;25    6.000E+00 QAMBAPAfMiBOqHNRePReP
;25    7.000E+00 XILAaPQTMAGPAfNR`PR`P
;25    8.000E+00 fGLaUPqBMa@PrCNBhPBgP
;25    9.000E+00 TeLQQPQEMqCPrWNBgPBgP
;25    1.000E+01 DALAPPABMATPc@NBgPBgP
;25    1.100E+01 sBLqAPYBLQTPcPNBhPBhP
;25    1.200E+01 rYLaCPhGLaSPShNBiPBiP
;25    1.300E+01 rHLQFPWVLqQPtDNRaPRaP
;25    1.400E+01 BELAIPVgLqYPdXNRcPRcP
;25    1.500E+01 qXLADPFVLAfPE@NRePReP
;25    1.600E+01 QWLIhOFBLQcPuANRgPRgP
;25    1.800E+01 aDLICOeILBFPEhNCBPCBP
;25    2.000E+01 A@LxCOtRLRGPFPNCGPCGP
;25    2.200E+01 hIKwTOdGLbGPFhNSAPSAP
;25    2.400E+01 VgKgCOCiLrFPwBNSFPSFP
;25    2.600E+01 UdKvYOSWLBUPwSNcAPcAP
;25    2.800E+01 UBKFPOs@LRSPXANcEPcEP
;25    3.000E+01 DVKFFOCGLbPPHWNcIPcIP
;25    4.000E+01 RQKDbObGLBiPYdNCWPCWP
;25    5.000E+01 aQKDBOAaLSAPQAOcSPcSP
;25    6.000E+01 QAKCVOQPLcHPa@OsUPsUP
;25    8.000E+01 fGJrSOQBLSTPqDOSePSeP
;25    1.000E+02 DAJbGOHiKsRPATODIPDIP
;25    1.500E+02 qXJaQOU`KDBPaQOtDPtDP
;25    2.000E+02 A@JaGODQKd@PqRODYPDYP
;25    3.000E+02 DVIXhNRcKDPPAeOdXPdXP
;25    4.000E+02 RQIGCNb@KTSPQdOtYPtYP
;25    5.000E+02 aQIEbNqVKdQPQiODfPDfP
;25    6.000E+02 QAIThNAVKdVPBCOTbPTbP
;25    8.000E+02 fGHCiNQ@KtTPBIOTiPTiP
;25    1.000E+03 DAHc@NxWJtYPRCOEDPEDP
;25    1.500E+03 qXHbDNEdJDfPRIOU@PU@P
;25    2.000E+03 A@HqSNtHJT`PbCOUDPUDP
;25    3.000E+03 DVGa@NRbJTdPbFOUHPUHP
;25    4.000E+03 RQGiIMRIJTgPbHOeAPeAP
;25    5.000E+03 aQGWYMqUJThPr@OeBPeBP
;25    6.000E+03 QAGFTMAVJTiPrAOeCPeCP
;25    8.000E+03 fGFTfMAIJEAPrBOeDPeDP
;25    1.000E+04 DAFDEMxUIEAPrCOeEPeEP
;25    1.500E+04 qXFB`MEdIEBPrDOeFPeFP
;25    2.000E+04 A@FREMtHIECPrDOeGPeGP
;25    3.000E+04 DVEAXMRbIEDPrEOeGPeGP
;25    4.000E+04 RQEQDMRIIEDPrEOeHPeHP
;25    5.000E+04 aQEiHLqUIEDPrFOeHPeHP
;25    6.000E+04 QAEGdLAVIEDPrFOeHPeHP
;25    8.000E+04 fGDFALAIIEEPrFOeHPeHP
;25    1.000E+05 DADDiLxUHEEPrFOeHPeHP
;==== ELEMENT  26
;26    1.000E-03 TTRxXOIHU@@R@@RIIUIHU
;26    1.500E-03 dDRQSPCPU@@R@@RCPUCPU
;26    2.000E-03 ScRRBPaRU@@R@@RaSUaRU
;26    3.000E-03 sERcAPUTT@@R@@RUXTUTT
;26    4.000E-03 BeRdAPRTT@@R@@RRWTRTT
;26    5.000E-03 BRRUCPqGT@@R@@RAPTqGT
;26    6.000E-03 BFRUgPhGS@@R@@RHXShHS
;26    7.112E-03 qTRF`PUDS@@R@@RuBSUES
;26 K  7.112E-03 qTRF`PDFT@@R@@RDHTDFT
;26    8.000E-03 QTRGPPCDT@@R@@RCFTCDT
;26    1.000E-02 a@RXTPaYT@@R@@RqQTaYT
;26    1.500E-02 GVQAEQeRS@@R@@RuQSeSS
;26    2.000E-02 UGQQFQRPS@@R@@RRWSRRS
;26    3.000E-02 BeQaIQwVR@@R@@RXHRGiR
;26    4.000E-02 A`QqDQsBR@@R@@RcSRCUR
;26    5.000E-02 aDQqFQqPR@@R@@RQfRAcR
;26    6.000E-02 YHPqFQyXQ@@R@@Ra@RQAR
;26    8.000E-02 ePPqCQDFQ@@R@@RUeQuIQ
;26    1.000E-01 sWPq@QBDQ@@R@@RsRQsDQ
;26    1.500E-01 qXPa@QEfP@@R@@RQfQqYQ
;26    2.000E-01 ACPQAQBSP@@R@@RAVQqFQ
;26    3.000E-01 tSOyYPgGO@@R@@RQ@QAEQ
;26    4.000E-01 bYOHaPcAO@@R@@RIPPYCP
;26    5.000E-01 qTOHFPqVO@@R@@RHQPhDP
;26    6.000E-01 aAOGWPQAO@@R@@RwPPWXP
;26    8.000E-01 FcNVWPeUN@@R@@RvPPfSP
;26    1.000E+00 tHNUbPSQN@@R@@RUiPUeP
;26    1.022E+00 d@NEePsDN@@R@@RUcPEiP
;26    1.250E+00 BaNeIPbFNGCM@@RuEPuBP
;26    1.500E+00 QeNDaPaSNSXN@@RDhPDfP
;26    2.000E+00 Q@NTAPA@NqFO@@RdFPdEP
;26    2.044E+00 AENDFPiYMAWO@@RdBPdAP
;26    3.000E+00 DhMcCPEUMsXOQCMcRPcRP
;26    4.000E+00 rUMrPPcWMFDOdRMsAPsAP
;26    5.000E+00 qVMrCPrUMHCOi@MSEPSDP
;26    6.000E+00 aBMBFPRIMIaOAQNCFPCFP
;26    7.000E+00 XgLAePAaMQDPQ`NCAPCAP
;26    8.000E+00 FgLaXPQUMaHPrHNRiPRiP
;26    9.000E+00 ESLQUPqEMAQPBdNRiPRiP
;26    1.000E+01 DPLASPa@MQSPcGNRiPRiP
;26    1.100E+01 cSLqDPAGMaSPcXNCAPCAP
;26    1.200E+01 CELaEPySLqSPDGNCBPCBP
;26    1.300E+01 bPLQHPHiLAbPDTNCDPCDP
;26    1.400E+01 bDLQBPXILQ`PtXNCGPCGP
;26    1.500E+01 QeLAFPWYLQhPUANCIPCIP
;26    1.600E+01 qRLAAPGGLBEPESNSBPSBP
;26    1.800E+01 qFLiDOfBLRIPFANSGPSGP
;26    2.000E+01 Q@LXROUULrAPVUNcBPcBP
;26    2.200E+01 IHKWaOEBLBQPGDNcHPcHP
;26    2.400E+01 gSKGPOTWLRQPGYNsCPsCP
;26    2.600E+01 VPKVdOd@LbPPW`NsHPsHP
;26    2.800E+01 eQKVUOChLbYPhINCRPCRP
;26    3.000E+01 DiKf@OcQLrVPhUNCWPCWP
;26    4.000E+01 rUKTcObWLCGPABOcWPcWP
;26    5.000E+01 qVKTAORBLs@PQCOCcPCcP
;26    6.000E+01 aBKSTOqVLCXPaBOSfPSfP
;26    8.000E+01 FgJB`OqALsVPqGOTGPTGP
;26    1.000E+02 DPJrBOADLSePAWOtCPtCP
;26    1.500E+02 QeJaUOVcKdFPaTOTYPTYP
;26    2.000E+02 Q@Jq@OUHKDUPqUOtVPtVP
;26    3.000E+02 DhIYHNCUKdWPAiOTePTeP
;26    4.000E+02 rUIWINRXKD`PQgOEGPEGP
;26    5.000E+02 qVIUeNBFKDhPBCOUEPUEP
;26    6.000E+02 aBIU@NqRKTdPBGOe@Pe@P
;26    8.000E+02 FgHShNaIKECPRCOeHPeHP
;26    1.000E+03 DPHcGNACKEHPRGOuCPuCP
;26    1.500E+03 QeHbINFfJUEPbCOEPPEPP
;26    2.000E+03 Q@HqWNUDJUIPbFOETPETP
;26    3.000E+03 DhGaCNCSJeDPr@OEXPEXP
;26    4.000E+03 rUGYPMRWJeFPrBOUPPUPP
;26    5.000E+03 qVGwWMBFJeHPrCOURPURP
;26    6.000E+03 aBGVYMqQJeIPrDOUSPUSP
;26    8.000E+03 FgFEHMaIJu@PrFOUTPUTP
;26    1.000E+04 DPFTDMACJuAPrFOUUPUUP
;26    1.500E+04 QeFBfMFeIuBPrHOUWPUWP
;26    2.000E+04 Q@Fb@MUDIuCPrHOUWPUWP
;26    3.000E+04 DhEQRMCSIuDPrIOUXPUXP
;26    4.000E+04 rUEQGMRWIuDPrIOUXPUXP
;26    5.000E+04 qVEIYLBFIuDPrIOUXPUXP
;26    6.000E+04 aBEHBLqQIuEPBPOUYPUYP
;26    8.000E+04 FgDVELaIIuEPBPOUYPUYP
;26    1.000E+05 DPDEALACIuEPBPOUYPUYP
;==== ELEMENT  27
;27    1.000E-03 dVRHDOyYU@@R@@RI`UyYU
;27    1.500E-03 tGRARPcYU@@R@@RsPUcYU
;27    2.000E-03 DGRQhPqWU@@R@@RqXUqWU
;27    3.000E-03 CYRCBPFIT@@R@@RVCTFIT
;27    4.000E-03 RhRSiPB`T@@R@@RBcTB`T
;27    5.000E-03 RTRDhPQRT@@R@@RQTTQRT
;27    6.000E-03 RGReYPYES@@R@@RyGSYES
;27    7.709E-03 aYRVaPTTS@@R@@RtQSTTS
;27 K  7.709E-03 aYRVaPSTT@@R@@RSVTSTT
;27    8.000E-03 aSRGIPcCT@@R@@RcETcCT
;27    1.000E-02 aFRhCPAcT@@R@@RAdTAcT
;27    1.500E-02 wXQABQVAS@@R@@Rf@SVBS
;27    2.000E-02 uIQQCQrTS@@R@@RB`SrUS
;27    3.000E-02 RhQaFQXTR@@R@@RXfRhVR
;27    4.000E-02 AhQqAQcVR@@R@@RShRsYR
;27    5.000E-02 q@QqCQAhR@@R@@RRDRBAR
;27    6.000E-02 iPPqCQAIR@@R@@RqARaBR
;27    8.000E-02 EfPqAQTRQ@@R@@RFQQEcQ
;27    1.000E-01 SePaGQbHQ@@R@@RSeQSVQ
;27    1.500E-01 AgPQHQVWP@@R@@RBBQAdQ
;27    2.000E-01 AHPAIQrSP@@R@@RAXQqGQ
;27    3.000E-01 TgOiSPXHO@@R@@RAIQADQ
;27    4.000E-01 BcOhWPcRO@@R@@RyAPICP
;27    5.000E-01 AbOWcPQiO@@R@@RxBPXCP
;27    6.000E-01 aGOwEPaEO@@R@@RgPPGXP
;27    8.000E-01 WINFWPvIN@@R@@RfPPVSP
;27    1.000E+00 dQNEbPShN@@R@@RUaPEfP
;27    1.022E+00 DQNuVPsWN@@R@@REdPE`P
;27    1.250E+00 ReNeAPRUNgIM@@ReGPeDP
;27    1.500E+00 BENtSPAdNcYN@@RDaPtYP
;27    2.000E+00 QFNDDPQCNAPO@@Rd@PTIP
;27    2.044E+00 QANSiPQ@NQQO@@RTFPTEP
;27    3.000E+00 UDMSHPVEMCgOQAMSXPSXP
;27    4.000E+00 BiMbUPTDMVHOTTMcHPcHP
;27    5.000E+00 AeMbIPS@MhAOIEMSCPSCP
;27    6.000E+00 aHMBCPBVMA@PqINCEPCDP
;27    7.000E+00 ITLAbPBDMQFPAgNCAPC@P
;27    8.000E+00 gCLaVPqTMqAPrDNRiPRiP
;27    9.000E+00 uQLQRPQRMATPrYNRiPRiP
;27    1.000E+01 dRLAQPqEMQVPcBNC@PC@P
;27    1.100E+01 CbLqBPaAMaWPcRNCBPCBP
;27    1.200E+01 cALaCPAIMqVPD@NCDPCDP
;27    1.300E+01 rTLQFPA@MAePtFNCFPCFP
;27    1.400E+01 rFLQ@PiALQdPtQNCIPCIP
;27    1.500E+01 BFLADPXTLBBPECNSAPSAP
;27    1.600E+01 AaLYeOWfLBIPuDNSDPSDP
;27    1.800E+01 ASLIIOG@LbCPUbNc@Pc@P
;27    2.000E+01 QFLxHOfELrEPFTNcFPcFP
;27    2.200E+01 YVKwYOeTLBVPVbNsAPsAP
;27    2.400E+01 HCKgHOUDLRVPwFNsFPsFP
;27    2.600E+01 FdKFcOtRLbUPwWNCRPCRP
;27    2.800E+01 U`KFUOtFLrTPXFNCWPCWP
;27    3.000E+01 UDKV@ODFLBbPXQNSQPSQP
;27    4.000E+01 BiKDeOC@LSCPYiNsRPsRP
;27    5.000E+01 AeKDEOrILsGPQAOChPChP
;27    6.000E+01 aHKCYOQhLSUPa@ODBPDBP
;27    8.000E+01 gCJrUOAWLCcPqDOdDPdDP
;27    1.000E+02 dRJbHOQGLDCPATODPPDPP
;27    1.500E+02 BFJaSOwXKtDPaQOdWPdWP
;27    2.000E+02 QFJaHOEbKTSPqRODcPDcP
;27    3.000E+02 UDIIDNCgKtVPAeOECPECP
;27    4.000E+02 BiIGHNR`KDiPQcOUEPUEP
;27    5.000E+02 AeIEfNrBKTgPQiOeCPeCP
;27    6.000E+02 aHIEBNQcKECPBCOeHPeHP
;27    8.000E+02 gCHSbNAUKUAPBIOuFPuFP
;27    1.000E+03 dRHcBNQFKUFPRCOEQPEQP
;27    1.500E+03 BFHbENwQJeDPRHOEXPEXP
;27    2.000E+03 QFHqTNuXJeHPbAOURPURP
;27    3.000E+03 UDGaANCeJuCPbEOUVPUVP
;27    4.000E+03 BiGyEMBiJuEPbGOUYPUYP
;27    5.000E+03 AeGgTMrAJuGPbHOePPePP
;27    6.000E+03 aHGFXMQcJuHPbIOeQPeQP
;27    8.000E+03 gCFTiMATJuIPr@OeSPeSP
;27    1.000E+04 dRFDHMQEJEPPrAOeTPeTP
;27    1.500E+04 BFFBbMwPIEQPrBOeUPeUP
;27    2.000E+04 QFFRGMuWIERPrCOeUPeUP
;27    3.000E+04 UDEAYMCeIESPrCOeVPeVP
;27    4.000E+04 BiEQEMBiIESPrDOeVPeVP
;27    5.000E+04 AeEyDLrAIESPrDOeWPeWP
;27    6.000E+04 aHEW`LQcIESPrDOeWPeWP
;27    8.000E+04 gCDFELATIETPrDOeWPeWP
;27    1.000E+05 dRDTcLQEIETPrDOeWPeWP
;==== ELEMENT  28
;28    1.000E-03 EERGaOIeU@@R@@RIfUIeU
;28    1.004E-03 EERGfOyUU@@R@@RyUUyUU
;28    1.008E-03 EERWaOiUU@@R@@RiVUiUU
;28 L1 1.008E-03 EERWaOQ@V@@R@@RQ@VQ@V
;28    1.500E-03 tVRqIPdCU@@R@@RdCUdCU
;28    2.000E-03 DURQfPBDU@@R@@RBEUBDU
;28    3.000E-03 CdRC@PGFT@@R@@RW@TGFT
;28    4.000E-03 cIRShPcET@@R@@RcHTcET
;28    5.000E-03 BbRT`PqVT@@R@@RqYTqVT
;28    6.000E-03 BRRuSPAGT@@R@@RAITAGT
;28    8.000E-03 AaRWGPtVS@@R@@RTeStWS
;28    8.333E-03 qSRwIPdES@@R@@RDSSdES
;28 K  8.333E-03 qSRwIPcHT@@R@@RcITcHT
;28    1.000E-02 AQRxFPBGT@@R@@RBITBHT
;28    1.500E-02 hPQADQVhS@@R@@RGHSG@S
;28    2.000E-02 UeQQFQSES@@R@@RcBSSFS
;28    3.000E-02 s@Qq@QIhR@@R@@RACSA@S
;28    4.000E-02 BHQqFQdFR@@R@@RdPRtIR
;28    5.000E-02 ATQqHQRIR@@R@@RBWRrCR
;28    6.000E-02 AFQqHQaGR@@R@@RQQRAQR
;28    8.000E-02 FXPqFQu@Q@@R@@RwAQfVQ
;28    1.000E-01 tGPqBQbXQ@@R@@RDTQD@Q
;28    1.500E-01 BGPaCQwUP@@R@@RbAQB@Q
;28    2.000E-01 a@PQDQcCP@@R@@RQXQAVQ
;28    3.000E-01 UQOA@QyPO@@R@@RQEQQ@Q
;28    4.000E-01 SDOIBPt@O@@R@@RyVPIUP
;28    5.000E-01 BCOhFPrGO@@R@@RxPPXPP
;28    6.000E-01 AQOgUPAYO@@R@@RWdPG`P
;28    8.000E-01 WhNvTPgPN@@R@@RFiPFaP
;28    1.000E+00 UBNFFPtSN@@R@@RVFPVAP
;28    1.022E+00 T`NF@PDYN@@R@@RFIPFDP
;28    1.250E+00 cHNERPCDNWhM@@REYPEVP
;28    1.500E+00 bHNTcPRINDBN@@REBPTiP
;28    2.000E+00 aHNdAPqENQRO@@RtIPtGP
;28    2.044E+00 aCNTFPq@NaSO@@RtEPtCP
;28    3.000E+00 uQMsAPw@MTIOQFMsUPsTP
;28    4.000E+00 cAMrVPTaMfWOtSMCTPCTP
;28    5.000E+00 BFMrIPcWMHgOIRMcIPcIP
;28    6.000E+00 ASMRAPRbMAHPAUNcAPcAP
;28    7.000E+00 AEMQ`PBRMaEPQeNSGPSGP
;28    8.000E+00 HCLqSPBGMAQPBTNSFPSFP
;28    9.000E+00 vELQYPA`MQUPR`NSGPSGP
;28    1.000E+01 UDLAWPQYMaXPsENSHPSHP
;28    1.100E+01 dELqGPASMA`PsWNcAPcAP
;28    1.200E+01 SWLaIPq@MQ`PTGNcCPcCP
;28    1.300E+01 CDLaAPQHMB@PTTNcFPcFP
;28    1.400E+01 bRLQEPAIMBIPT`NcIPcIP
;28    1.500E+01 bHLAIPAAMRHPeDNsBPsBP
;28    1.600E+01 BALADPIRLbFPUVNsEPsEP
;28    1.800E+01 QYLIWOhHLBQPVFNCQPCQP
;28    2.000E+01 aHLxSOwILRTPvPNCXPCXP
;28    2.200E+01 AFLXAOfWLbUPg@NSTPSTP
;28    2.400E+01 XbKWXOFHLrVPgVNcPPcPP
;28    2.600E+01 gPKWBOUXLBfPHINcUPcUP
;28    2.800E+01 VVKvQOUFLRePHYNsQPsQP
;28    3.000E+01 uQKvFOD`LCCPHfNsVPsVP
;28    4.000E+01 cAKEEOSULsGPADOShPShP
;28    5.000E+01 BFKdBOBbLcSPQFOTGPTGP
;28    6.000E+01 ASKcSOrDLCcPaEOtBPtBP
;28    8.000E+01 HCJBfOqTLTCPqIOTUPTUP
;28    1.000E+02 UDJrHOqILtDPQPOtSPtSP
;28    1.500E+02 bHJaYOi@KdXPaWOEAPEAP
;28    2.000E+02 aHJqCOFhKDhPqYOUIPUIP
;28    3.000E+02 uQIIQNTXKUBPQbOEQPEQP
;28    4.000E+02 cAIwGNCSKeFPB@OUSPUSP
;28    5.000E+02 BFIV@NrTKuEPBFOeQPeQP
;28    6.000E+02 ASIeBNbHKEQPR@OeWPeWP
;28    8.000E+02 HCHDHNqQKUPPRFOuUPuUP
;28    1.000E+03 UDHsFNqGKUUPb@OEaPEaP
;28    1.500E+03 bHHrDNYAJeSPbFOEhPEhP
;28    2.000E+03 aHHAaNFcJeXPbIOUbPUbP
;28    3.000E+03 uQGaFNTUJuRPrCOUgPUgP
;28    4.000E+03 cAGyTMCQJuUPrEOUiPUiP
;28    5.000E+03 BFGWfMrSJuWPrFOFAPFAP
;28    6.000E+03 ASGvUMbHJuXPrGOFBPFBP
;28    8.000E+03 HCFe@MqQJuYPrHOFCPFCP
;28    1.000E+04 UDFdEMqGJE`PrIOFDPFDP
;28    1.500E+04 bHFRcMY@IEaPBPOFFPFFP
;28    2.000E+04 aHFbFMFcIEbPBQOFFPFFP
;28    3.000E+04 uQEQVMTUIEcPBQOFGPFGP
;28    4.000E+04 cAEQIMCQIEcPBROFGPFGP
;28    5.000E+04 BFEySLrSIEcPBROFHPFHP
;28    6.000E+04 ASEhBLbHIEcPBROFHPFHP
;28    8.000E+04 HCDvALqQIEdPBROFHPFHP
;28    1.000E+05 UDDUCLqGIEdPBROFHPFHP
;==== ELEMENT  29
;29    1.000E-03 EERUaOAFV@@R@@RAFVAFV
;29    1.047E-03 ECRvFOyCU@@R@@RyCUyCU
;29    1.096E-03 EARFdOhDU@@R@@RhEUhDU
;29 L1 1.096E-03 EARFdOyDU@@R@@RyEUyDU
;29    1.500E-03 DaRAIPDQU@@R@@RDRUDQU
;29    2.000E-03 TSRQYPREU@@R@@RREUREU
;29    3.000E-03 SeRRYPGUT@@R@@RGYTGUT
;29    4.000E-03 CPRSSPCTT@@R@@RCWTCTT
;29    5.000E-03 RaRtIPAgT@@R@@RQ`TAgT
;29    6.000E-03 RPRUHPQCT@@R@@RQFTQCT
;29    8.000E-03 AgRVWPEFS@@R@@ReFSEGS
;29    8.979E-03 aURWFPcVS@@R@@RCcScVS
;29 K  8.979E-03 aURWFPrWT@@R@@RrXTrWT
;29    1.000E-02 AURwSPRDT@@R@@RRFTRET
;29    1.500E-02 H`QyVPwAS@@R@@RGQSwBS
;29    2.000E-02 FFQQ@QsAS@@R@@RsHSsBS
;29    3.000E-02 sGQaCQAES@@R@@RAISAFS
;29    4.000E-02 RBQaIQTRR@@R@@RDfRdUR
;29    5.000E-02 AWQqAQrDR@@R@@RbQRBWR
;29    6.000E-02 AHQqAQqER@@R@@RQYRAXR
;29    8.000E-02 VYPaIQeXQ@@R@@RgSQVgQ
;29    1.000E-01 DUPaFQBhQ@@R@@RTXQTDQ
;29    1.500E-01 RAPQGQxEP@@R@@RbBQBAQ
;29    2.000E-01 aCPAIQCYP@@R@@RQVQATQ
;29    3.000E-01 eROYXPAEP@@R@@RQBQAFQ
;29    4.000E-01 cAOhSPdVO@@R@@RIQPIIP
;29    5.000E-01 BGOW`PRWO@@R@@RxFPXFP
;29    6.000E-01 ATOwBPaRO@@R@@RgSPGXP
;29    8.000E-01 XENFTPhFN@@R@@RfQPVRP
;29    1.000E+00 eCNE`PUDN@@R@@RU`PEeP
;29    1.022E+00 EANuTPDiN@@R@@REcPuXP
;29    1.250E+00 sENUIPs@NHBM@@ReFPeCP
;29    1.500E+00 rCNtRPrHNDBN@@RD`PtXP
;29    2.000E+00 qANDCPAVNQQO@@Rd@PTIP
;29    2.044E+00 aFNShPAQNaRO@@RTGPTEP
;29    3.000E+00 EcMSGPWbMTFOQAMcPPSYP
;29    4.000E+00 cHMbTPuBMfROTSMsBPsAP
;29    5.000E+00 R@MbHPShMxYOIAMSHPSGP
;29    6.000E+00 AVMBBPSGMAGPqHNSAPSAP
;29    7.000E+00 AGMAaPbRMaDPAgNCHPCHP
;29    8.000E+00 h@LaUPbDMAPPrCNCGPCGP
;29    9.000E+00 FXLQRPQeMQTPrXNCHPCHP
;29    1.000E+01 eELAQPqRMaVPc@NS@PS@P
;29    1.100E+01 tDLqAPQUMqXPcPNSCPSCP
;29    1.200E+01 cULaCPAPMAhPSiNSEPSEP
;29    1.300E+01 SALQFPaHMQhPtDNSHPSHP
;29    1.400E+01 bXLQ@PQHMBGPdXNcBPcBP
;29    1.500E+01 rCLADPAIMRFPEANcEPcEP
;29    1.600E+01 BELYaOABMbCPuBNcHPcHP
;29    1.800E+01 aRLIFOXfLrHPEiNsDPsDP
;29    2.000E+01 qALxEOWiLRQPFQNCQPCQP
;29    2.200E+01 AILwVOgALbSPFiNCWPCWP
;29    2.400E+01 YAKgEOVWLrSPwCNSSPSSP
;29    2.600E+01 wWKFaOFDLBcPwSNSYPSYP
;29    2.800E+01 vPKFROUXLRbPXANcTPcTP
;29    3.000E+01 EcKFHOUILC@PHVNcYPcYP
;29    4.000E+01 cHKDcOCdLsDPYcNSbPSbP
;29    5.000E+01 R@KDCOCELSYPQAOT@PT@P
;29    6.000E+01 AVKCXORSLsYPa@OdEPdEP
;29    8.000E+01 h@JrTOAhLDHPqCODYPDYP
;29    1.000E+02 eEJbGOQPLdIPASOdVPdVP
;29    1.500E+02 rCJaROYdKdRPaPOTdPTdP
;29    2.000E+02 qAJaGOGTKDbPqPOUBPUBP
;29    3.000E+02 EcII@NTdKEEPAcOuCPuCP
;29    4.000E+02 cHIGENsPKUIPQaOEUPEUP
;29    5.000E+02 R@IEdNRfKeGPQfOUSPUSP
;29    6.000E+02 AVIE@NBVKuDPB@OUYPUYP
;29    8.000E+02 h@HS`NAeKERPBEOeWPeWP
;29    1.000E+03 eEHcANAXKEXPBIOuRPuRP
;29    1.500E+03 rCHbDNIeJUUPRDOuYPuYP
;29    2.000E+03 qAHqTNwHJePPRGOEcPEcP
;29    3.000E+03 EcGaANTbJeTPb@OEgPEgP
;29    4.000E+03 cHGyAMcYJeWPbBOU`PU`P
;29    5.000E+03 R@GgRMReJeXPbCOUaPUaP
;29    6.000E+03 AVGFVMBVJeYPbDOUbPUbP
;29    8.000E+03 h@FThMAdJuQPbEOUdPUdP
;29    1.000E+04 eEFDFMAXJuRPbFOUePUeP
;29    1.500E+04 rCFBaMIdIuSPbGOUfPUfP
;29    2.000E+04 qAFRFMwHIuTPbHOUgPUgP
;29    3.000E+04 EcEAYMTbIuTPbHOUgPUgP
;29    4.000E+04 cHEQDMcYIuUPbHOUhPUhP
;29    5.000E+04 R@EyALReIuUPbIOUhPUhP
;29    6.000E+04 AVEGgLBVIuUPbIOUhPUhP
;29    8.000E+04 h@DFCLAdIuUPbIOUhPUhP
;29    1.000E+05 eEDTaLAWIuUPbIOUhPUhP
;==== ELEMENT  30
;30    1.000E-03 eDRVXOQUU@@R@@RQUUQUU
;30    1.010E-03 eDRfXOQQU@@R@@RQRUQQU
;30    1.020E-03 eCRvYOAXU@@R@@RAXUAXU
;30 L3 1.020E-03 eCRvYOSRU@@R@@RSSUSRU
;30    1.031E-03 eCRVaOtXU@@R@@RtYUtXU
;30    1.043E-03 eBRGCOFYU@@R@@RFYUFYU
;30 L2 1.043E-03 eBRGCOhCU@@R@@RhDUhCU
;30    1.116E-03 UIRG`OwYU@@R@@RwYUwYU
;30    1.194E-03 UDRhSOwGU@@R@@RwGUwGU
;30 L1 1.194E-03 UDRhSOxIU@@R@@RHPUxIU
;30    1.500E-03 ThRQIPDbU@@R@@RDbUDbU
;30    2.000E-03 dXRqPPrGU@@R@@RrGUrGU
;30    3.000E-03 DHRbTPhGT@@R@@RxAThGT
;30    4.000E-03 STRSRPCcT@@R@@RCgTCcT
;30    5.000E-03 CERtEPBIT@@R@@RRBTBIT
;30    6.000E-03 bSRUBPaFT@@R@@RaITaFT
;30    8.000E-03 QiRFXPeWS@@R@@REgSeWS
;30    9.659E-03 aQRGTPsDS@@R@@RSQSsDS
;30 K  9.659E-03 aQRGTPRRT@@R@@RRTTRRT
;30    1.000E-02 QTRgQPrAT@@R@@RrCTrBT
;30    1.500E-02 yCQiVPHAS@@R@@RXBSHBS
;30    2.000E-02 FQQAIQcTS@@R@@RsRScVS
;30    3.000E-02 SXQaCQQFS@@R@@RaASQGS
;30    4.000E-02 bFQaIQECR@@R@@RuHRUFR
;30    5.000E-02 QVQqAQbQR@@R@@RBiRrTR
;30    6.000E-02 QEQqBQQQR@@R@@RqVRaUR
;30    8.000E-02 GAPq@QvGQ@@R@@RxFQgVQ
;30    1.000E-01 tSPaGQcDQ@@R@@RTgQTPQ
;30    1.500E-01 bEPQHQIQP@@R@@RrDQRBQ
;30    2.000E-01 qAPAIQSdP@@R@@RaRQAYQ
;30    3.000E-01 UiOiRPQIP@@R@@RQDQAHQ
;30    4.000E-01 CROhWPu@O@@R@@RYTPi@P
;30    5.000E-01 bAOWdPRbO@@R@@RHUPhCP
;30    6.000E-01 QTOwFPAdO@@R@@RgYPWTP
;30    8.000E-01 xPNFXPIQN@@R@@RfVPVWP
;30    1.000E+00 UXNEcPEfN@@R@@RUdPEiP
;30    1.022E+00 uDNuWPUVN@@R@@REgPEbP
;30    1.250E+00 SXNeAPsVNHUM@@Ru@PeFP
;30    1.500E+00 BYNtTPrQNdBN@@RDcPDaP
;30    2.000E+00 APNDEPaWNQXO@@RdDPdBP
;30    2.044E+00 qDND@PaQNqPO@@Rd@PTHP
;30    3.000E+00 fBMSHPI@MtCOQAMcSPcSP
;30    4.000E+00 SPMbVPFDMFiOTUMsFPsFP
;30    5.000E+00 bDMr@PTQMYDOIFMcBPcBP
;30    6.000E+00 QVMBCPSYMQAPqINSFPSFP
;30    7.000E+00 QDMAbPRgMaIPAgNSDPSDP
;30    8.000E+00 xVLaVPRTMAUPrDNSDPSDP
;30    9.000E+00 VbLQSPbAMaPPrYNSEPSEP
;30    1.000E+01 ePLAQPQeMqSPcBNSHPSGP
;30    1.100E+01 dSLqBPqUMAePcRNc@Pc@P
;30    1.200E+01 CiLaDPQYMQfPD@NcCPcCP
;30    1.300E+01 sBLQGPAUMBFPtFNcGPcGP
;30    1.400E+01 BfLQ@PqDMREPtQNs@Ps@P
;30    1.500E+01 BYLAEPaDMbDPECNsDPsCP
;30    1.600E+01 RILYfOQEMrBPuDNsGPsGP
;30    1.800E+01 qSLY@OAAMBWPUbNCTPCTP
;30    2.000E+01 APLHPOIELbPPFTNSQPSQP
;30    2.200E+01 QFLG`OXGLrSPVbNSXPSXP
;30    2.400E+01 ySKgIOGTLBdPwFNcTPcTP
;30    2.600E+01 hIKFdOFcLRdPwWNsPPsPP
;30    2.800E+01 WEKFUOvBLCCPXENsVPsVP
;30    3.000E+01 fCKVAOEgLSAPXPNCaPCaP
;30    4.000E+01 SPKDfOtELCVPYgNDEPDEP
;30    5.000E+01 bDKDEOCULsSPQAOdDPdDP
;30    6.000E+01 QVKCYOBfLScPa@ODPPDPP
;30    8.000E+01 xVJrUORCLdCPqDOdTPdTP
;30    1.000E+02 ePJbIOqPLDUPATODbPDbP
;30    1.500E+02 BYJaSOQBLtYPaPOUAPUAP
;30    2.000E+02 APJaHOHQKE@PqQOu@Pu@P
;30    3.000E+02 fCIIENUYKeDPAcOUQPUQP
;30    4.000E+02 SPIGINTIKuGPQaOeTPeTP
;30    5.000E+02 bDIEgNsEKEVPQfOuRPuRP
;30    6.000E+02 QVIEBNrYKUSPB@OuXPuXP
;30    8.000E+02 xVHSbNBIKeRPBFOEfPEfP
;30    1.000E+03 ePHcCNaWKeWPBIOUaPUaP
;30    1.500E+03 BYHbENQAKuUPREOUiPUiP
;30    2.000E+03 APHqTNxEJE`PRHOFCPFCP
;30    3.000E+03 fCGaANUVJEdPbAOFHPFHP
;30    4.000E+03 SPGyGMTGJEgPbCOV@PV@P
;30    5.000E+03 bDGgVMsDJEhPbDOVBPVBP
;30    6.000E+03 QVGFYMrXJU`PbEOVCPVCP
;30    8.000E+03 xVFE@MBIJUaPbFOVDPVDP
;30    1.000E+04 ePFDHMaWJUbPbGOVEPVEP
;30    1.500E+04 BYFBbMQAJUcPbHOVFPVFP
;30    2.000E+04 APFRGMxDIUdPbHOVGPVGP
;30    3.000E+04 fCEQPMUVIUePbIOVHPVHP
;30    4.000E+04 SPEQEMTGIUePbIOVHPVHP
;30    5.000E+04 bDEyFLsDIUePbIOVHPVHP
;30    6.000E+04 QVEWaLrXIUePbIOVHPVHP
;30    8.000E+04 xVDFFLBIIUfPr@OVIPVIP
;30    1.000E+05 ePDTcLaWIUfPr@OVIPVIP
;==== ELEMENT  31
;31    1.000E-03 eBRFXOaYU@@R@@RqPUaYU
;31    1.056E-03 UIRGFOAYU@@R@@RAYUAYU
;31    1.115E-03 UFRgXOqAU@@R@@RqAUqAU
;31 L3 1.115E-03 UFRgXOCaU@@R@@RCbUCaU
;31    1.129E-03 UERGbOdTU@@R@@RdTUdTU
;31    1.142E-03 UDRWfOeTU@@R@@ReUUeTU
;31 L2 1.142E-03 UDRWfOwDU@@R@@RwDUwDU
;31    1.218E-03 U@RxVOFcU@@R@@RFcUFcU
;31    1.298E-03 EFRiROvEU@@R@@RvFUvEU
;31 L1 1.298E-03 EFRiROg@U@@R@@RgAUg@U
;31    1.500E-03 TdRQHPEHU@@R@@REIUEHU
;31    2.000E-03 dSRaYPRQU@@R@@RRRURQU
;31    3.000E-03 DDRbPPHbT@@R@@RHfTHbT
;31    4.000E-03 SRRCSPDIT@@R@@RTCTDIT
;31    5.000E-03 CFRd@PbCT@@R@@RbGTbCT
;31    6.000E-03 bVRTbPqFT@@R@@RqHTqFT
;31    8.000E-03 BBRVIPFIS@@R@@Rv@SV@S
;31    1.000E-02 QWRgGPcFS@@R@@RCRScFS
;31    1.037E-02 QQRGTPRdS@@R@@RS@SReS
;31 K  1.037E-02 QQRGTPb@T@@R@@RbATb@T
;31    1.500E-02 YPQiDPHSS@@R@@RXTSHTS
;31    2.000E-02 VRQAEQCeS@@R@@RScSCfS
;31    3.000E-02 cUQQHQaCS@@R@@RaHSaDS
;31    4.000E-02 rAQaDQuGR@@R@@RuSREYR
;31    5.000E-02 QYQaGQrYR@@R@@RCHRRbR
;31    6.000E-02 QGQaGQaRR@@R@@RAgRqUR
;31    8.000E-02 WFPaEQFeQ@@R@@RHbQXAQ
;31    1.000E-01 DcPaBQCYQ@@R@@Re@QtQQ
;31    1.500E-01 r@PQDQABQ@@R@@RrIQRFQ
;31    2.000E-01 qDPAFQdHP@@R@@RaRQAYQ
;31    3.000E-01 VEOyBPq@P@@R@@RQBQAFQ
;31    4.000E-01 SQOHPPuWO@@R@@RyBPXgP
;31    5.000E-01 bFOgYPSIO@@R@@RhDPHAP
;31    6.000E-01 QXOWCPBAO@@R@@RGYPwCP
;31    8.000E-01 XcNfGPACO@@R@@RFWPvHP
;31    1.000E+00 uSNeUPFPN@@R@@RuWPuQP
;31    1.022E+00 EYNUYPFHN@@R@@RuPPeUP
;31    1.250E+00 cWNEEPTANXXM@@RUDPU@P
;31    1.500E+00 RUNTYPRfNdGN@@RdYPdWP
;31    2.000E+00 ATNSbPAbNQYO@@RTAPT@P
;31    2.044E+00 qHNCgPqVNqQO@@RDHPDFP
;31    3.000E+00 vIMCIPIaMtEOAHMSTPSSP
;31    4.000E+00 cPMRWPVYMV`ODQMcHPcHP
;31    5.000E+00 r@MbBPTbMYFOxXMSFPSEP
;31    6.000E+00 aPMQgPSaMQAPqENS@PS@P
;31    7.000E+00 QGMqWPcDMaIPAbNCHPCHP
;31    8.000E+00 XiLaQPrVMAUPbGNCIPCHP
;31    9.000E+00 WALAXPBPMaPPrQNS@PS@P
;31    1.000E+01 uVLqGPRBMqSPSBNSCPSCP
;31    1.100E+01 tVLaHPQaMAePSQNSFPSFP
;31    1.200E+01 D@La@PqSMQePChNSIPSIP
;31    1.300E+01 CQLQCPQXMBEPdCNcCPcCP
;31    1.400E+01 RdLAGPAUMREPTVNcFPcFP
;31    1.500E+01 RVLAAPqEMbDPDhNs@Ps@P
;31    1.600E+01 bELiVOaEMrBPUGNsDPsDP
;31    1.800E+01 qXLHcOQ@MBWPuSNCQPCQP
;31    2.000E+01 ATLXDOIcLbPPfDNCXPCXP
;31    2.200E+01 QILWVOHgLrRPvPNSUPSUP
;31    2.400E+01 YiKGFOHILBcPWCNcQPcQP
;31    2.600E+01 XRKfSOGRLRcPWRNcWPcWP
;31    2.800E+01 wDKfFOFfLCCPGiNsSPsSP
;31    3.000E+01 FPKUbOvHLSAPhCNsYPsYP
;31    4.000E+01 cPKtQOtRLCVPiVNDCPDCP
;31    5.000E+01 r@KScOsULsRPAGOdBPdBP
;31    6.000E+01 aPKsIOS@LScPQFOtHPtHP
;31    8.000E+01 XiJbWOrALdCPaIOdRPdRP
;31    1.000E+02 uVJbBOAdLDTPqIOD`PD`P
;31    1.500E+02 RVJQXOaBLtXPQUOU@PU@P
;31    2.000E+02 ATJaDOYCKTiPaUOeHPeHP
;31    3.000E+02 FPIxWNFGKeCPqWOEYPEYP
;31    4.000E+02 cPIFgNTUKuFPAeOeRPeRP
;31    5.000E+02 r@IeYNcSKEUPQ`OuPPuPP
;31    6.000E+02 aPIDgNCCKURPQdOuVPuVP
;31    8.000E+02 XiHC`NbGKePPQiOEdPEdP
;31    1.000E+03 uVHSCNAaKeVPBBOEiPEiP
;31    1.500E+03 RVHRHNaAKuTPBGOUgPUgP
;31    2.000E+03 ATHaYNIFJuXPR@OFAPFAP
;31    3.000E+03 FPGQHNFDJEcPRCOFEPFEP
;31    4.000E+03 cPGIHMTSJEePREOFHPFHP
;31    5.000E+03 r@GGRMcRJEgPRFOFIPFIP
;31    6.000E+03 aPGfIMCBJEhPRGOV@PV@P
;31    8.000E+03 XiFDeMbFJEiPRHOVBPVBP
;31    1.000E+04 uVFSfMAaJU`PRIOVCPVCP
;31    1.500E+04 RVFrTMaAJUbPb@OVDPVDP
;31    2.000E+04 ATFR@MIFIUbPbAOVEPVEP
;31    3.000E+04 FPEAUMFDIUcPbAOVEPVEP
;31    4.000E+04 cPEQAMTSIUcPbAOVFPVFP
;31    5.000E+04 r@EIGLcRIUdPbBOVFPVFP
;31    6.000E+04 aPEgVLCBIUdPbBOVFPVFP
;31    8.000E+04 XiDEhLbFIUdPbBOVFPVFP
;31    1.000E+05 uVDtXLAaIUdPbBOVFPVFP
;==== ELEMENT  32
;32    1.000E-03 uDRVIOAiU@@R@@RAiUAiU
;32    1.103E-03 eHRgCOQPU@@R@@RQPUQPU
;32    1.217E-03 eARHSOQIU@@R@@RQIUQIU
;32 L3 1.217E-03 eARHSOtFU@@R@@RtFUtFU
;32    1.232E-03 e@RXYOdUU@@R@@RdVUdUU
;32    1.248E-03 UIRxVOTgU@@R@@RTgUTgU
;32 L2 1.248E-03 UIRxVOfUU@@R@@RfVUfUU
;32    1.328E-03 UDRiROFGU@@R@@RFHUFGU
;32    1.414E-03 EIRAEPUUU@@R@@RUUUUUU
;32 L1 1.414E-03 EIRAEPfHU@@R@@RfIUfHU
;32    1.500E-03 EDRQEPEWU@@R@@REWUEWU
;32    2.000E-03 tQRaWPrQU@@R@@RrQUrQU
;32    3.000E-03 T@RbPPYWT@@R@@RiQTYWT
;32    4.000E-03 SXRCRPDVT@@R@@RTPTDVT
;32    5.000E-03 SBRTGPBTT@@R@@RBWTBTT
;32    6.000E-03 rSRDfPAXT@@R@@RQQTAXT
;32    8.000E-03 BIRFHPfWS@@R@@RFiSfXS
;32    1.000E-02 aTRWBPSWS@@R@@RsTSSXS
;32    1.110E-02 AURgQPbVS@@R@@RBaSbWS
;32 K  1.110E-02 AURgQPQgT@@R@@RQhTQgT
;32    1.500E-02 IhQIEPIDS@@R@@RYESIES
;32    2.000E-02 vWQACQTDS@@R@@RdBSTES
;32    3.000E-02 C`QQFQqDS@@R@@RqHSqES
;32    4.000E-02 BQQaBQEdR@@R@@RfARUgR
;32    5.000E-02 aVQaEQCDR@@R@@RsDRSGR
;32    6.000E-02 aBQaEQqWR@@R@@RBBRQ`R
;32    8.000E-02 GWPaDQWRQ@@R@@RYPQxUQ
;32    1.000E-01 EEPaAQCdQ@@R@@RUUQEEQ
;32    1.500E-01 BQPQCQQBQ@@R@@RBYQbEQ
;32    2.000E-01 APPAEQtSP@@R@@RaVQQRQ
;32    3.000E-01 FSOiCPATP@@R@@RQCQAGQ
;32    4.000E-01 cWOxBPFQO@@R@@RyCPXfP
;32    5.000E-01 rGOgRPSTO@@R@@RhAPWgP
;32    6.000E-01 aVOGFPbDO@@R@@RGUPgIP
;32    8.000E-01 yFNfBPQDO@@R@@RFSPvCP
;32    1.000E+00 FANePPWBN@@R@@RuSPeWP
;32    1.022E+00 uUNUTPvWN@@R@@ReVPePP
;32    1.250E+00 CeNEAPTWNX`M@@RU@PEFP
;32    1.500E+00 bXNTUPcINDQN@@RdVPdSP
;32    2.000E+00 QQNCiPBBNaSO@@RDIPDGP
;32    2.044E+00 ATNCdPQeNqUO@@RDEPDCP
;32    3.000E+00 vPMCFPAINDVOAGMSRPSRP
;32    4.000E+00 sWMRUPwAMGGOtGMcGPcGP
;32    5.000E+00 BQMb@PEUMyGOxPMSFPSFP
;32    6.000E+00 aXMQePtCMQDPqDNSAPSAP
;32    7.000E+00 aCMqUPSYMqBPA`NCIPCIP
;32    8.000E+00 ISLQYPCFMAXPbENS@PS@P
;32    9.000E+00 GULAWPbVMaSPbXNSCPSCP
;32    1.000E+01 FCLqFPrEMqVPCINSFPSFP
;32    1.100E+01 TiLaGPRAMAiPCXNSIPSIP
;32    1.200E+01 TILQIPQaMB@PCeNcCPcCP
;32    1.300E+01 SWLQBPqUMR@PTINcFPcFP
;32    1.400E+01 CHLAFPaQMb@PTRNs@Ps@P
;32    1.500E+01 bXLAAPAYMbHPDcNsDPsDP
;32    1.600E+01 rFLYWOqIMrGPUCNsHPsHP
;32    1.800E+01 AfLxTOaBMRRPeXNCUPCUP
;32    2.000E+01 QQLHFOAIMbVPVHNSSPSSP
;32    2.200E+01 aELGYOIbLrXPfTNcPPcPP
;32    2.400E+01 AELG@OXeLBiPGFNcWPcWP
;32    2.600E+01 XbKVWOhBLC@PGUNsSPsSP
;32    2.800E+01 wPKf@OgPLCIPGbNsYPsYP
;32    3.000E+01 vQKEgOGFLSHPXFNCePCeP
;32    4.000E+01 sWKdWOeCLSSPYVNT@PT@P
;32    5.000E+01 BQKCiOTELC`PAFOt@Pt@P
;32    6.000E+01 aXKsFOCTLDAPQEODVPDVP
;32    8.000E+01 ISJbUORVLtAPaHOtQPtQP
;32    1.000E+02 FCJb@OBDLTSPqGODiPDiP
;32    1.500E+02 bXJQVOqELDhPQSOUIPUIP
;32    2.000E+02 QQJaCOAALEIPaSOuGPuGP
;32    3.000E+02 vPIhYNvRKuCPqUOUYPUYP
;32    4.000E+02 sWIFaNECKEWPAcOuRPuRP
;32    5.000E+02 BQIeTNDBKUVPAhOE`PE`P
;32    6.000E+02 aXIDcNsEKeRPQaOEfPEfP
;32    8.000E+02 ISHsWNRQKuQPQfOUePUeP
;32    1.000E+03 FCHS@NBAKuWPB@OF@PF@P
;32    1.500E+03 bXHRGNqDKEePBEOFHPFHP
;32    2.000E+03 QQHaXNA@KEiPBHOVBPVBP
;32    3.000E+03 vPGQGNfXJUdPRAOVFPVFP
;32    4.000E+03 sWGXiMEAJUgPRCOVIPVIP
;32    5.000E+03 BQGwEMDAJUhPRDOfAPfAP
;32    6.000E+03 aXGfDMsDJUiPREOfBPfBP
;32    8.000E+03 ISFD`MRPJFAPRFOfCPfCP
;32    1.000E+04 FCFSbMB@JFBPRFOfDPfDP
;32    1.500E+04 bXFrQMqDJFCPRGOfEPfEP
;32    2.000E+04 QQFBHMA@JFDPRHOfFPfFP
;32    3.000E+04 vPEATMfXIFEPRHOfGPfGP
;32    4.000E+04 sWEQ@MEAIFEPRIOfGPfGP
;32    5.000E+04 BQEXhLDAIFEPRIOfGPfGP
;32    6.000E+04 aXEgPLsDIFEPRIOfGPfGP
;32    8.000E+04 ISDEbLRPIFFPRIOfHPfHP
;32    1.000E+05 FCDtTLB@IFFPRIOfHPfHP
;==== ELEMENT  33
;33    1.000E-03 UQREaORBU@@R@@RRBURBU
;33    1.150E-03 ERRgHOQRU@@R@@RQRUQRU
;33    1.323E-03 uARIGOAIU@@R@@RAIUAIU
;33 L3 1.323E-03 uARIGOTSU@@R@@RTSUTSU
;33    1.341E-03 u@RiEODXU@@R@@RDYUDXU
;33    1.359E-03 eIRITODTU@@R@@RDUUDTU
;33 L2 1.359E-03 eIRITOFHU@@R@@RFHUFHU
;33    1.500E-03 UIRAIPeBU@@R@@ReCUeBU
;33    1.526E-03 UHRQBPTiU@@R@@RE@UTiU
;33 L1 1.526E-03 UHRQBPeUU@@R@@ReUUeUU
;33    2.000E-03 DeRaPPRcU@@R@@RRcURcU
;33    3.000E-03 dARRVPADU@@R@@RAEUADU
;33    4.000E-03 cWRCPPDhT@@R@@RTbTDhT
;33    5.000E-03 cARTEPbXT@@R@@RrQTbXT
;33    6.000E-03 BaRDcPaST@@R@@RaVTaST
;33    8.000E-03 RHRFBPwES@@R@@RWWSwFS
;33    1.000E-02 qQRGDPSdS@@R@@RTASSdS
;33    1.187E-02 APRGdPBSS@@R@@RRXSBTS
;33 K  1.187E-02 APRGdPqXT@@R@@RqYTqXT
;33    1.500E-02 ACRXdPyTS@@R@@RIeSyUS
;33    2.000E-02 GGQABQDXS@@R@@RTVSDYS
;33    3.000E-02 ShQQEQAUS@@R@@RQQSAWS
;33    4.000E-02 RSQaBQvIR@@R@@RvVRVQR
;33    5.000E-02 qUQaDQsDR@@R@@RcSRCVR
;33    6.000E-02 aHQaEQQeR@@R@@Rb@RBGR
;33    8.000E-02 GdPaDQhHQ@@R@@RACRYQQ
;33    1.000E-01 u@PaAQdCQ@@R@@RUgQETQ
;33    1.500E-01 RSPQBQaEQ@@R@@RbRQrGQ
;33    2.000E-01 AWPAEQeEP@@R@@RqRQQWQ
;33    3.000E-01 vXOiBPaPP@@R@@RQEQAHQ
;33    4.000E-01 CgOxAPWEO@@R@@RIQPICP
;33    5.000E-01 RPOgQPSeO@@R@@RhFPHAP
;33    6.000E-01 qUOGFPRPO@@R@@RGXPwAP
;33    8.000E-01 IgNfAPaHO@@R@@RFTPvDP
;33    1.000E+00 vDNUYPWfN@@R@@RuSPeWP
;33    1.022E+00 FGNUSPWVN@@R@@ReWPeQP
;33    1.250E+00 DFNE@PUANyAM@@RUAPEFP
;33    1.500E+00 BbNTUPcXNTYN@@RdVPdSP
;33    2.000E+00 QYNChPbFNaYO@@RDIPDHP
;33    2.044E+00 QRNCdPRHNAbO@@RDFPDDP
;33    3.000E+00 GGMCFPaBNdQOAGMSTPSSP
;33    4.000E+00 ShMRUPXEMgIOtGMs@PcIP
;33    5.000E+00 RUMb@PFHMiVOxPMSIPSHP
;33    6.000E+00 qWMQePDcMQGPqDNSDPSDP
;33    7.000E+00 q@MqUPSiMqFPA`NSCPSCP
;33    8.000E+00 YeLQYPCPMQSPbENSEPSEP
;33    9.000E+00 GfLAVPRfMaXPbXNSGPSGP
;33    1.000E+01 vGLqFPbRMAbPCINcAPcAP
;33    1.100E+01 eFLaGPrEMQdPCXNcDPcDP
;33    1.200E+01 DRLQIPRCMBFPCdNcHPcHP
;33    1.300E+01 sWLQBPQdMRFPTINsBPsBP
;33    1.400E+01 cELAFPqYMbFPTQNsFPsFP
;33    1.500E+01 BcLA@PaVMrEPDcNCQPCPP
;33    1.600E+01 BYLYWOQTMBTPUBNCUPCUP
;33    1.800E+01 QgLxTOqFMRYPeWNSSPSSP
;33    2.000E+01 QYLHFOaAMrSPVGNcPPcPP
;33    2.200E+01 qBLGYOAIMBfPfSNcXPcXP
;33    2.400E+01 QALG@OYeLRhPGENsUPsUP
;33    2.600E+01 IRKVWOYDLCHPGTNCaPCaP
;33    2.800E+01 XBKf@OHULSHPGaNChPChP
;33    3.000E+01 GGKEgOGeLcGPXDNSdPSdP
;33    4.000E+01 ShKdVOEaLcSPYUNd@Pd@P
;33    5.000E+01 RUKCiOdQLSaPAFODPPDPP
;33    6.000E+01 qWKsEOCbLTBPQEOTWPTWP
;33    8.000E+01 YeJbUOBdLDSPaHODcPDcP
;33    1.000E+02 vGJb@ObGLdVPqGOEAPEAP
;33    1.500E+02 BcJQVOQPLEAPQSOuBPuBP
;33    2.000E+02 QYJaCOQBLeCPaSOUQPUQP
;33    3.000E+02 GGIhYNGVKEWPqUOuTPuTP
;33    4.000E+02 ShIF`NUYKeRPAbOEgPEgP
;33    5.000E+02 RUIeSNDWKuQPAgOUePUeP
;33    6.000E+02 qWIDbNsRKuXPQaOFAPFAP
;33    8.000E+02 YeHsWNrYKEgPQfOV@PV@P
;33    1.000E+03 vGHS@NbCKUbPQiOVEPVEP
;33    1.500E+03 BcHRFNAYKFAPBDOfCPfCP
;33    2.000E+03 QYHaXNQAKFEPBGOfHPfHP
;33    3.000E+03 GGGQFNGRJV@PR@OvBPvBP
;33    4.000E+03 ShGXiMUWJVCPRBOvEPvEP
;33    5.000E+03 RUGwEMDUJVDPRCOvFPvFP
;33    6.000E+03 qWGfCMsQJVFPRDOvHPvHP
;33    8.000E+03 YeFD`MrXJVGPREOvIPvIP
;33    1.000E+04 vGFSbMbCJVHPREOFPPFPP
;33    1.500E+04 BcFrQMAXJVIPRFOFQPFQP
;33    2.000E+04 QYFBHMQAJf@PRGOFRPFRP
;33    3.000E+04 GGEATMGRIfAPRGOFSPFSP
;33    4.000E+04 ShEQ@MUWIfAPRHOFSPFSP
;33    5.000E+04 RUEXhLDUIfAPRHOFSPFSP
;33    6.000E+04 qWEWYLsQIfBPRHOFTPFTP
;33    8.000E+04 YeDEbLrXIfBPRHOFTPFTP
;33    1.000E+05 vGDtTLbCIfBPRHOFTPFTP
;==== ELEMENT  34
;34    1.000E-03 UVRuIOrAU@@R@@RrBUrAU
;34    1.198E-03 ETRgHOQPU@@R@@RQQUQPU
;34    1.436E-03 eIRiXOyVT@@R@@RIbTyVT
;34 L3 1.436E-03 eIRiXOtDU@@R@@RtEUtDU
;34    1.456E-03 eHRIhOTBU@@R@@RTBUTBU
;34    1.476E-03 eFRAAPS`U@@R@@RSaUS`U
;34 L2 1.476E-03 eFRAAPUYU@@R@@RUYUUYU
;34    1.500E-03 eERACPuCU@@R@@RuDUuCU
;34    1.654E-03 UDRQIPtDU@@R@@RtDUtDU
;34 L1 1.654E-03 UDRQIPTaU@@R@@RTbUTaU
;34    2.000E-03 T`RQTPCIU@@R@@RS@UCIU
;34    3.000E-03 dCRRPPQAU@@R@@RQBUQAU
;34    4.000E-03 cXRsDPeAT@@R@@ReETeAT
;34    5.000E-03 cBRDGPBfT@@R@@RR`TBfT
;34    6.000E-03 BcRtSPqTT@@R@@RqWTqTT
;34    8.000E-03 bAREfPGiS@@R@@RXASGiS
;34    1.000E-02 qTRFcPdCS@@R@@RDQSdDS
;34    1.266E-02 qARW`PRHS@@R@@RrBSRIS
;34 K  1.266E-02 qARW`PQWT@@R@@RQYTQXT
;34    1.500E-02 AFRhVPABT@@R@@RACTABT
;34    2.000E-02 gAQIePtTS@@R@@RDbStUS
;34    3.000E-02 DGQQBQQTS@@R@@RaPSQVS
;34    4.000E-02 bPQQHQFaR@@R@@RWHRVbR
;34    5.000E-02 qYQaAQSVR@@R@@RCfRcXR
;34    6.000E-02 qBQaBQBIR@@R@@RrDRbAR
;34    8.000E-02 HDPa@QHiQ@@R@@RAIRAAR
;34    1.000E-01 ETPQHQTVQ@@R@@RfHQuSQ
;34    1.500E-01 bPPQ@QqEQ@@R@@RrPQBTQ
;34    2.000E-01 QRPABQeYP@@R@@RqTQQYQ
;34    3.000E-01 VgOIAPqTP@@R@@RQDQAGQ
;34    4.000E-01 SiOXBPwWO@@R@@Ry@PX`P
;34    5.000E-01 RWOGTPt@O@@R@@RXCPGgP
;34    6.000E-01 A`OV`PrRO@@R@@RwEPWGP
;34    8.000E-01 ABOFGPqIO@@R@@RvAPfAP
;34    1.000E+00 VSNEWPhWN@@R@@ReRPUUP
;34    1.022E+00 fENEQPhDN@@R@@RUUPEYP
;34    1.250E+00 TINDiPUWNYPM@@RE@PTfP
;34    1.500E+00 RaNDUPDANdWN@@RTVPTSP
;34    2.000E+00 aTNC`PBVNqRO@@RDAPSiP
;34    2.044E+00 QWNsUPrGNAdO@@RSgPSfP
;34    3.000E+00 gIMRiPqBNdUOAEMCXPCWP
;34    4.000E+00 T@MBYPHeMwEOdGMcEPcDP
;34    5.000E+00 bRMREPfPMySOXPMSDPSDP
;34    6.000E+00 AbMQ`PeDMQHPq@NSAPS@P
;34    7.000E+00 qDMqQPtDMqGPqVNS@PS@P
;34    8.000E+00 ACMQVPcYMQTPb@NSBPSBP
;34    9.000E+00 X@LASPcAMaYPbRNSEPSEP
;34    1.000E+01 VVLqCPBdMAcPCBNSIPSIP
;34    1.100E+01 ERLaDPRUMQePCPNcCPcCP
;34    1.200E+01 TVLQFPrAMBGPsUNcGPcGP
;34    1.300E+01 ChLAIPRAMRGPDINsAPsAP
;34    1.400E+01 sELACPQdMbGPDQNsEPsEP
;34    1.500E+01 RbLIbOA`MrFPtRNCPPsIP
;34    1.600E+01 RVLyEOaWMBUPEANCTPCTP
;34    1.800E+01 BBLXTOAWMbQPUTNSRPSRP
;34    2.000E+01 aTLGhOqAMrUPFCNcPPcPP
;34    2.200E+01 qFLwBOQHMBhPFXNcWPcWP
;34    2.400E+01 QDLFdOAHMRiPFiNsUPsUP
;34    2.600E+01 yQKFROYaLS@PgGNCbPCbP
;34    2.800E+01 xGKFFOYELc@PgSNChPChP
;34    3.000E+01 gIKuTOXQLcIPWeNSdPSdP
;34    4.000E+01 T@KTVOfILcUPyCNd@Pd@P
;34    5.000E+01 bRKCaOTiLScPADODQPDQP
;34    6.000E+01 AbKcHOTDLTDPQBOTXPTXP
;34    8.000E+01 ACKRYOCHLDUPaEODdPDdP
;34    1.000E+02 VVJREOBULdXPqDOECPECP
;34    1.500E+02 RbJQSOaSLEDPAYOuDPuDP
;34    2.000E+02 aTJa@OaBLeEPQYOUSPUSP
;34    3.000E+02 gIIXPNHHKUPPqPOuUPuUP
;34    4.000E+02 T@IfUNFEKeTPqXOEhPEhP
;34    5.000E+02 bRIUQNDdKuSPAbOUgPUgP
;34    6.000E+02 AbItQNDCKE`PAfOFCPFCP
;34    8.000E+02 ACIcXNCBKEiPQaOVAPVAP
;34    1.000E+03 VVHCCNBRKUePQdOVGPVGP
;34    1.500E+03 RbHRBNaQKFCPQiOfEPfEP
;34    2.000E+03 aTHaTNaAKFGPBBOfIPfIP
;34    3.000E+03 gIGQDNHDJVBPBEOvDPvDP
;34    4.000E+03 T@GxYMFCJVEPBGOvGPvGP
;34    5.000E+03 bRGWIMDbJVGPBHOvHPvHP
;34    6.000E+03 AbGFIMDBJVHPBHOvIPvIP
;34    8.000E+03 ACGdYMCAJVIPBIOFQPFQP
;34    1.000E+04 VVFCcMBQJf@PR@OFRPFRP
;34    1.500E+04 RbFbUMaQJfBPRAOFSPFSP
;34    2.000E+04 aTFBDMaAJfBPRAOFTPFTP
;34    3.000E+04 gIEAPMHDIfCPRBOFTPFTP
;34    4.000E+04 T@EAHMFCIfCPRBOFUPFUP
;34    5.000E+04 bRExXLDbIfDPRBOFUPFUP
;34    6.000E+04 AbEGRLDBIfDPRBOFUPFUP
;34    8.000E+04 ACEeYLCAIfDPRCOFUPFUP
;34    1.000E+05 VVDdSLBQIfDPRCOFVPFVP
;==== ELEMENT  35
;35    1.000E-03 EcRUGObRU@@R@@RbRUbRU
;35    1.500E-03 UQRAAPYfT@@R@@RA@UYfT
;35    1.550E-03 EXRAFPi@T@@R@@RiFTi@T
;35 L3 1.550E-03 EXRAFPtAU@@R@@RtAUtAU
;35    1.573E-03 EVRAHPScU@@R@@RSdUScU
;35    1.596E-03 ETRQAPSYU@@R@@RSYUSYU
;35 L2 1.596E-03 ETRQAPU@U@@R@@RU@UU@U
;35    1.686E-03 uHRa@PDYU@@R@@RTPUDYU
;35    1.782E-03 uARq@PSfU@@R@@RSgUSfU
;35 L1 1.782E-03 uARq@PDYU@@R@@RDYUDYU
;35    2.000E-03 UERQSPCPU@@R@@RCQUCPU
;35    3.000E-03 DTRRRPaCU@@R@@RaCUaCU
;35    4.000E-03 CdRsIPuXT@@R@@REaTuXT
;35    5.000E-03 sFRTEPSHT@@R@@RcATSHT
;35    6.000E-03 RfRDaPQdT@@R@@RQgTQdT
;35    8.000E-03 rBRUePxYS@@R@@RICSxYS
;35    1.000E-02 AeRVaPtRS@@R@@RTaStSS
;35    1.347E-02 aIRhEPBDS@@R@@RRHSBES
;35 K  1.347E-02 aIRhEPAVT@@R@@RAWTAVT
;35    1.500E-02 QBRxSPQAT@@R@@RQBTQAT
;35    2.000E-02 gUQYdPUHS@@R@@ReGSUIS
;35    3.000E-02 tCQQCQqPS@@R@@RqUSqQS
;35    4.000E-02 rWQa@QWPR@@R@@RW`RgRR
;35    5.000E-02 QaQaBQSeR@@R@@RdFRDGR
;35    6.000E-02 APQaCQrBR@@R@@RRXRBTR
;35    8.000E-02 XWPaBQY`Q@@R@@Ra@RQAR
;35    1.000E-01 E`PQIQEIQ@@R@@RFfQfHQ
;35    1.500E-01 rXPQAQQQQ@@R@@RR`QbRQ
;35    2.000E-01 aRPADQvIP@@R@@RAdQaXQ
;35    3.000E-01 GVOYFPQfP@@R@@RQIQQAQ
;35    4.000E-01 dGOhFPxWO@@R@@RYVPYDP
;35    5.000E-01 rUOWWPDeO@@R@@RxCPHEP
;35    6.000E-01 QbOGBPCGO@@R@@RWRPwBP
;35    8.000E-01 AIOVHPQWO@@R@@RFTPvCP
;35    1.000E+00 ViNUVPI`N@@R@@RuSPeVP
;35    1.022E+00 fYNUPPyBN@@R@@ReVPUYP
;35    1.250E+00 DXNThPfINAAN@@REIPEEP
;35    1.500E+00 SANTRPTRNTdN@@RdUPdRP
;35    2.000E+00 qUNCfPrWNAaO@@RDIPDGP
;35    2.044E+00 aXNCbPbXNQdO@@RDEPDDP
;35    3.000E+00 G`MCDPAYNDhOAFMSUPSTP
;35    4.000E+00 tIMRTPYiMwQOtDMsCPsBP
;35    5.000E+00 BaMRIPGTMABPhTMcCPcCP
;35    6.000E+00 QeMQdPU`MaDPqCNSIPSIP
;35    7.000E+00 ASMqTPDhMASPqYNc@PSIP
;35    8.000E+00 Q@MQXPTFMaQPbDNcBPcBP
;35    9.000E+00 hWLAVPcRMqVPbVNcEPcEP
;35    1.000E+01 GBLqEPc@MQaPCGNcIPcIP
;35    1.100E+01 EaLaFPBgMBDPCUNsDPsDP
;35    1.200E+01 DhLQHPbPMRFPCbNsHPsHP
;35    1.300E+01 TFLQAPrGMbGPTFNCSPCSP
;35    1.400E+01 SXLAEPRHMrHPDYNCXPCXP
;35    1.500E+01 SBLYiOBBMBWPD`NSRPSRP
;35    1.600E+01 rTLYQOAhMRVPEINSWPSWP
;35    1.800E+01 RGLhYOaUMrSPeTNcUPcUP
;35    2.000E+01 qVLHBOAXMBgPVCNsTPsTP
;35    2.200E+01 AULGUOqCMCAPVYNCbPCbP
;35    2.400E+01 aBLVfOaAMSCPGANS`PS`P
;35    2.600E+01 ADLVSOQAMcDPwINSgPSgP
;35    2.800E+01 XfKVFOACMsDPwVNDDPDDP
;35    3.000E+01 GaKEdOYVLCSPHINT@PT@P
;35    4.000E+01 tIKdTOGGLCbPIXNtHPtHP
;35    5.000E+01 BaKCgOeQLT@PAEOdPPdPP
;35    6.000E+01 QeKsCOdULtBPQDOtWPtWP
;35    8.000E+01 Q@KbSOCVLdUPaGOEDPEDP
;35    1.000E+02 GBJRHOrVLDiPqFOeDPeDP
;35    1.500E+02 SBJQUOAcLeFPQQOUVPUVP
;35    2.000E+02 qVJaBOqGLEXPaQOuVPuVP
;35    3.000E+02 GaIhTNIHKuTPqSOF@PF@P
;35    4.000E+02 tIIvWNF`KEhPA`OVCPVCP
;35    5.000E+02 BaIePNETKUhPAeOfBPfBP
;35    6.000E+02 QeID`NTSKFEPAiOfIPfIP
;35    8.000E+02 Q@IsUNsIKVDPQdOvHPvHP
;35    1.000E+03 GBHCHNrQKf@PQgOFSPFSP
;35    1.500E+03 SBHRENAaKfIPBBOVRPVRP
;35    2.000E+03 qVHaWNqFKvDPBEOVVPVVP
;35    3.000E+03 GaGQFNIDJvIPBHOfQPfQP
;35    4.000E+03 tIGXdMvWJFRPR@OfTPfTP
;35    5.000E+03 BaGwAMERJFTPRAOfUPfUP
;35    6.000E+03 QeGf@MTRJFUPRAOfWPfWP
;35    8.000E+03 Q@GtXMsIJFVPRBOfXPfXP
;35    1.000E+04 GBFS`MrQJFWPRCOfYPfYP
;35    1.500E+04 SBFbYMAaJFYPRDOvPPvPP
;35    2.000E+04 qVFBGMqEJVPPRDOvQPvQP
;35    3.000E+04 GaEASMICIVPPREOvRPvRP
;35    4.000E+04 tIEQ@MvWIVQPREOvRPvRP
;35    5.000E+04 BaEXcLERIVQPREOvSPvSP
;35    6.000E+04 QeEWULTQIVQPREOvSPvSP
;35    8.000E+04 Q@EuYLsIIVQPRFOvSPvSP
;35    1.000E+05 GBDtQLrQIVQPRFOvSPvSP
;==== ELEMENT  36
;36    1.000E-03 U`RtVOBeU@@R@@RBeUBeU
;36    1.500E-03 UXRyGOAIU@@R@@RAIUAIU
;36    1.675E-03 EURQAPxAT@@R@@RxFTxAT
;36 L3 1.675E-03 EURQAPSaU@@R@@RSbUSaU
;36    1.701E-03 ESRQCPSQU@@R@@RSRUSQU
;36    1.727E-03 EQRQFPSFU@@R@@RSGUSFU
;36 L2 1.727E-03 EQRQFPTVU@@R@@RTVUTVU
;36    1.822E-03 uERaFPShU@@R@@RSiUShU
;36    1.921E-03 eHRqEPCXU@@R@@RCXUCXU
;36 L1 1.921E-03 eHRqEPSdU@@R@@RSeUSdU
;36    2.000E-03 eBRASPSYU@@R@@RcPUSYU
;36    3.000E-03 DYRBPPq@U@@R@@RqAUq@U
;36    4.000E-03 ChRcGPVET@@R@@RVITVET
;36    5.000E-03 sIRDCPsIT@@R@@RCSTsIT
;36    6.000E-03 RhRdYPBGT@@R@@RR@TBGT
;36    8.000E-03 rEREaPIQS@@R@@RiUSIQS
;36    1.000E-02 AhRvSPEFS@@R@@ReFSEGS
;36    1.433E-02 aBRhIPAdS@@R@@RQgSAeS
;36 K  1.433E-02 aBRhIPq@T@@R@@RqATq@T
;36    1.500E-02 QERHYPQFT@@R@@RQGTQFT
;36    2.000E-02 GaQiWPEVS@@R@@RUUSEWS
;36    3.000E-02 DRQQ@QA`S@@R@@RAeSAaS
;36    4.000E-02 BdQQGQWiR@@R@@RxIRXAR
;36    5.000E-02 QfQa@QdAR@@R@@RTRRtCR
;36    6.000E-02 ATQaAQBWR@@R@@RrTRRYR
;36    8.000E-02 H`Pa@QAFR@@R@@RaGRQHR
;36    1.000E-01 UePQGQEVQ@@R@@RgBQfSQ
;36    1.500E-01 BfPAIQaRQ@@R@@RC@QrQQ
;36    2.000E-01 aWPABQFiP@@R@@RAgQqPQ
;36    3.000E-01 gWOXhPRBP@@R@@RQIQQAQ
;36    4.000E-01 tIOHIPIYO@@R@@RIXPIDP
;36    5.000E-01 BdOGRPeGO@@R@@RhCPWdP
;36    6.000E-01 QhOFhPsDO@@R@@RGQPgAP
;36    8.000E-01 QBOFFPqQO@@R@@RvDPfCP
;36    1.000E+00 g@NEUPAFO@@R@@ReSPUVP
;36    1.022E+00 V`NEPPAAO@@R@@RUWPUPP
;36    1.250E+00 dRNDhPFcNACN@@RE@PTfP
;36    1.500E+00 cANDTPTaNEDN@@RTWPTTP
;36    2.000E+00 AaNsYPCANAcO@@RDBPD@P
;36    2.044E+00 qSNsTPRaNQgO@@RShPSgP
;36    3.000E+00 HDMRhPaRNTdOADMSPPCYP
;36    4.000E+00 TRMBYPAHNwXOdFMcIPcHP
;36    5.000E+00 R`MREPHFMACPHXMc@PSIP
;36    6.000E+00 BAMQ`PvIMaEPq@NSGPSGP
;36    7.000E+00 AXMqQPeHMATPqUNSGPSGP
;36    8.000E+00 QCMQUPTPMaRPRINc@Pc@P
;36    9.000E+00 XdLASPSaMqXPbQNcDPcDP
;36    1.000E+01 gDLqBPCVMQbPCANcHPcHP
;36    1.100E+01 UhLaCPS@MBFPsINsCPsCP
;36    1.200E+01 ECLQFPBaMRHPsTNsHPsHP
;36    1.300E+01 dILAIPRVMbIPDHNCRPCRP
;36    1.400E+01 sPLACPrFMrIPDPNCWPCWP
;36    1.500E+01 cBLyYORIMBYPtPNSRPSRP
;36    1.600E+01 BcLyCOBDMRXPTiNSVPSVP
;36    1.800E+01 bCLXROqYMrUPURNcUPcUP
;36    2.000E+01 AaLGfOQYMBiPFANsTPsTP
;36    2.200E+01 QPLw@OATMCCPFVNCbPCbP
;36    2.400E+01 aFLFbOqAMSEPFgNS`PS`P
;36    2.600E+01 AGLFQOa@McFPgDNShPShP
;36    2.800E+01 iCKFDOQAMsFPgPNDEPDEP
;36    3.000E+01 HEKuROACMCVPWcNTAPTAP
;36    4.000E+01 TSKTUOgTLCdPiHNtIPtIP
;36    5.000E+01 R`KC`OFFLTCPACOdQPdQP
;36    6.000E+01 BAKcGOEBLtEPQAOtYPtYP
;36    8.000E+01 QCKRXOsTLdXPaDOEFPEFP
;36    1.000E+02 gDJRDORhLTaPqCOeFPeFP
;36    1.500E+02 cBJQROQgLeIPAXOUYPUYP
;36    2.000E+02 AaJa@OAXLUQPQXOuYPuYP
;36    3.000E+02 HEIHWNIaKuWPaYOFBPFBP
;36    4.000E+02 TSIfTNwDKUbPqVOVFPVFP
;36    5.000E+02 R`IEYNEgKFAPAaOfEPfEP
;36    6.000E+02 BAItPNDiKFHPAeOvBPvBP
;36    8.000E+02 QCIcWNcVKVHPAiOFPPFPP
;36    1.000E+03 gDHCBNRcKfDPQcOFVPFVP
;36    1.500E+03 cBHRANQeKvCPQgOVTPVTP
;36    2.000E+03 AaHaSNAVKvGPB@OVYPVYP
;36    3.000E+03 HEGQDNyUJFSPBCOfTPfTP
;36    4.000E+03 TSGxWMwBJFUPBEOfWPfWP
;36    5.000E+03 R`GWGMEeJFWPBFOfXPfXP
;36    6.000E+03 BAGFHMDhJFXPBGOvPPvPP
;36    8.000E+03 QCGdXMcVJVPPBHOvQPvQP
;36    1.000E+04 gDFCbMRbJVQPBHOvRPvRP
;36    1.500E+04 cBFbTMQeJVRPBIOvSPvSP
;36    2.000E+04 AaFBCMAVJVSPBIOvTPvTP
;36    3.000E+04 HEEAPMyUIVTPR@OvUPvUP
;36    4.000E+04 TSEAHMwAIVTPR@OvUPvUP
;36    5.000E+04 R`ExVLEeIVTPR@OvVPvVP
;36    6.000E+04 BAEGPLDgIVUPRAOvVPvVP
;36    8.000E+04 QCEeXLcVIVUPRAOvVPvVP
;36    1.000E+05 gDDdRLRbIVUPRAOvVPvVP
;==== ELEMENT  37
;37    1.000E-03 F@RVgOSGU@@R@@RSGUSGU
;37    1.500E-03 eURQEPaAU@@R@@RaBUaAU
;37    1.804E-03 ETRASPwST@@R@@RwXTwST
;37 L3 1.804E-03 ETRASPCIU@@R@@RS@UCIU
;37    1.834E-03 ERRAVPRgU@@R@@RRhURgU
;37    1.864E-03 EPRAYPBfU@@R@@RBfUBfU
;37 L2 1.864E-03 EPRAYPSeU@@R@@RSfUSeU
;37    2.000E-03 u@RaQPCPU@@R@@RCQUCPU
;37    2.065E-03 eERaWPSEU@@R@@RSEUSEU
;37 L1 2.065E-03 eERaWPcPU@@R@@RcQUcPU
;37    3.000E-03 dPRRRPAQU@@R@@RARUAQU
;37    4.000E-03 SiRsFPvQT@@R@@RvUTvQT
;37    5.000E-03 CXRTAPsQT@@R@@RsTTsQT
;37    6.000E-03 CGRtWPbGT@@R@@Rr@TbGT
;37    8.000E-03 BSREgPACT@@R@@RAFTACT
;37    1.000E-02 QeRvWPUVS@@R@@RuWSUWS
;37    1.500E-02 a@RXPPqXS@@R@@RQaSqYS
;37    1.520E-02 QHRXVPqRS@@R@@RAdSqRS
;37 K  1.520E-02 QHRXVPa@T@@R@@RaATa@T
;37    2.000E-02 XGQiXPEiS@@R@@RUhSU`S
;37    3.000E-02 dSQQ@QQeS@@R@@RBASQfS
;37    4.000E-02 RhQQGQhYR@@R@@RYARHaR
;37    5.000E-02 BFQa@QTYR@@R@@RTbRtQR
;37    6.000E-02 QRQaAQrQR@@R@@RRhRBcR
;37    8.000E-02 iEPa@QQFR@@R@@RqHRaHR
;37    1.000E-01 fFPQHQF@Q@@R@@RG`QWGQ
;37    1.500E-01 CAPQ@QqYQ@@R@@RSIQBiQ
;37    2.000E-01 qVPABQgRP@@R@@RQfQqXQ
;37    3.000E-01 X@OIDPrDP@@R@@RaBQQDQ
;37    4.000E-01 dTOXEPAEP@@R@@RiWPiAP
;37    5.000E-01 C@OGXPEeO@@R@@RxFPHFP
;37    6.000E-01 BIOVcPsQO@@R@@RWQPw@P
;37    8.000E-01 QIOV@PQ`O@@R@@RFQPfIP
;37    1.000E+00 gQNEYPQHO@@R@@ReYPeQP
;37    1.022E+00 gINETPQCO@@R@@ReRPUUP
;37    1.250E+00 DhNTbPgPNAIN@@REEPE@P
;37    1.500E+00 sINDWPEVNeGN@@RdQPTXP
;37    2.000E+00 QaNCbPsENQaO@@RDFPDDP
;37    2.044E+00 AcNsWPcCNBEO@@RDCPDAP
;37    3.000E+00 XPMCAPA`NUCOAEMSUPSTP
;37    4.000E+00 tXMRQPa@NHGOdIMsCPsCP
;37    5.000E+00 CFMRGPXcMAFPXUMcEPcEP
;37    6.000E+00 RCMQaPGIMaIPqANcCPcBP
;37    7.000E+00 QVMqRPEfMAYPqWNcDPcDP
;37    8.000E+00 a@MQWPTiMaWPbANcGPcGP
;37    9.000E+00 IULATPtDMAdPbSNsAPsAP
;37    1.000E+01 gULqCPCcMQiPCCNsFPsFP
;37    1.100E+01 vBLaDPCTMRCPCQNCQPCQP
;37    1.200E+01 uALQGPSAMbEPsWNCVPCVP
;37    1.300E+01 TSLQ@PBdMrGPTANSQPSQP
;37    1.400E+01 S`LADPbQMBWPDSNSVPSVP
;37    1.500E+01 CPLIgOBRMRWPtTNcQPcQP
;37    1.600E+01 RiLIPObEMbWPECNcVPcVP
;37    1.800E+01 rFLXYOQhMBdPUVNsUPsUP
;37    2.000E+01 QaLWbOqWMRiPFENCePCeP
;37    2.200E+01 QXLwFOQYMSCPVPNScPScP
;37    2.400E+01 qCLFhOAUMcFPVbNDAPDAP
;37    2.600E+01 QCLFVOqCMsGPw@NDIPDIP
;37    2.800E+01 yVKFIOaCMCXPgUNTFPTFP
;37    3.000E+01 XPKuWOQDMSWPWhNdCPdCP
;37    4.000E+01 tXKTXOHVLSgPyENTRPTRP
;37    5.000E+01 CFKCcOvQLdGPADOtUPtUP
;37    6.000E+01 RCKs@OUVLTPPQBOTdPTdP
;37    8.000E+01 a@KbPOTDLDdPaEOeBPeBP
;37    1.000E+02 gUJRFOcILEHPqDOESPESP
;37    1.500E+02 CPJQTORHLEVPAYOuVPuVP
;37    2.000E+02 QaJa@OaSLeYPQYOUgPUgP
;37    3.000E+02 XPIXTNAILUfPqPOfAPfAP
;37    4.000E+02 tXIfYNXBKVAPqWOvEPvEP
;37    5.000E+02 CFIUTNVPKfAPAbOFUPFUP
;37    6.000E+02 RCItTNEQKfHPAfOVRPVRP
;37    8.000E+02 a@IsPNDEKvHPQaOfQPfQP
;37    1.000E+03 gUHCENcDKFTPQdOfWPfWP
;37    1.500E+03 CPHRCNRFKVSPQiOvUPvUP
;37    2.000E+03 QaHaUNaRKVXPBBOF`PF`P
;37    3.000E+03 XPGQDNAHKfSPBEOFePFeP
;37    4.000E+03 tXGHdMX@JfVPBGOFhPFhP
;37    5.000E+03 CFGgBMFWJfXPBHOFiPFiP
;37    6.000E+03 RCGVCMuIJfYPBIOVaPVaP
;37    8.000E+03 a@GtRMDEJvQPR@OVbPVbP
;37    1.000E+04 gUFCeMcDJvRPR@OVcPVcP
;37    1.500E+04 CPFbVMRFJvSPRAOVePVeP
;37    2.000E+04 QaFBEMaRJvTPRBOVePVeP
;37    3.000E+04 XPEAQMAHJvUPRBOVfPVfP
;37    4.000E+04 tXEAHMHIIvUPRCOVgPVgP
;37    5.000E+04 CFEHcLFWIvVPRCOVgPVgP
;37    6.000E+04 RCEGVLuIIvVPRCOVgPVgP
;37    8.000E+04 a@EuRLDDIvVPRCOVgPVgP
;37    1.000E+05 gUDdULcDIvVPRCOVhPVhP
;==== ELEMENT  38
;38    1.000E-03 VBRHQOCYU@@R@@RCYUCYU
;38    1.500E-03 uSRqCPqDU@@R@@RqEUqDU
;38    1.940E-03 EPRqTPWET@@R@@RgATWET
;38 L3 1.940E-03 EPRqTPBfU@@R@@RBfUBfU
;38    2.000E-03 uFRqYPRXU@@R@@RRYURXU
;38    2.007E-03 uERA`PRWU@@R@@RRWURWU
;38 L2 2.007E-03 uERA`PSWU@@R@@RSXUSWU
;38    2.109E-03 eHRAiPSHU@@R@@RSIUSHU
;38    2.216E-03 e@RQhPBdU@@R@@RBdUBdU
;38 L1 2.216E-03 e@RQhPcDU@@R@@RcDUcDU
;38    3.000E-03 dWRbVPQRU@@R@@RQSUQRU
;38    4.000E-03 DFRCVPgFT@@R@@Rw@TgFT
;38    5.000E-03 SURTHPDBT@@R@@RDFTDBT
;38    6.000E-03 SDRDcPBVT@@R@@RRPTBVT
;38    8.000E-03 BYRUaPQBT@@R@@RQETQCT
;38    1.000E-02 BARvYPFGS@@R@@RfGSFGS
;38    1.500E-02 aERHXPQeS@@R@@RBHSQeS
;38    1.610E-02 QDRxXPQYS@@R@@RqQSaPS
;38 K  1.610E-02 QDRxXPQ@T@@R@@RQATQ@T
;38    2.000E-02 XPQiTPfIS@@R@@RvISv@S
;38    3.000E-02 DaQQ@QR@S@@R@@RRFSRAS
;38    4.000E-02 S@QQGQyIR@@R@@RIbRYQR
;38    5.000E-02 RFQa@QTgR@@R@@RuAREIR
;38    6.000E-02 QXQaAQRcR@@R@@RcARCFR
;38    8.000E-02 iVPa@QaFR@@R@@RAXRqHR
;38    1.000E-01 VTPQHQVTQ@@R@@RxGQwQQ
;38    1.500E-01 SEPQ@QQfQ@@R@@RsGQCEQ
;38    2.000E-01 AdPABQxDP@@R@@RBDQAfQ
;38    3.000E-01 HXOIDPRXP@@R@@RaEQQFQ
;38    4.000E-01 DfOXGPQFP@@R@@RIaPyCP
;38    5.000E-01 SDOGXPFTO@@R@@RHTPXCP
;38    6.000E-01 RIOVdPDIO@@R@@RWWPwEP
;38    8.000E-01 aDOVAPR@O@@R@@RFUPvBP
;38    1.000E+00 WhNUPPqAO@@R@@RuQPeSP
;38    1.022E+00 gTNEUPaDO@@R@@ReUPUWP
;38    1.250E+00 UBNTcPxINQCN@@REGPEBP
;38    1.500E+00 SVNDXPFCNEXN@@RdSPTYP
;38    2.000E+00 B@NCbPcYNQhO@@RDHPDFP
;38    2.044E+00 QbNsXPSVNRBO@@RDDPDCP
;38    3.000E+00 XaMCAPQhNeIOAEMSWPSVP
;38    4.000E+00 EBMRQPqBNxAOt@MsFPsFP
;38    5.000E+00 cAMRGPIcMQ@PXVMcIPcHP
;38    6.000E+00 bCMQbPwYMqCPqANcGPcGP
;38    7.000E+00 aTMqRPFTMQSPqWNcHPcHP
;38    8.000E+00 aEMQWPEXMqRPbANsBPsBP
;38    9.000E+00 YaLATPtWMAiPbSNsFPsFP
;38    1.000E+01 HCLqDPdAMBDPCDNCQPCQP
;38    1.100E+01 fTLaEPsWMRHPCRNCWPCWP
;38    1.200E+01 UXLQGPCRMrAPsXNSRPSRP
;38    1.300E+01 tULQ@PSBMBSPTANSXPSXP
;38    1.400E+01 T@LADPBgMRTPDTNcSPcSP
;38    1.500E+01 SWLIiObVMbTPtTNcXPcXP
;38    1.600E+01 SDLIROBXMrTPECNsSPsSP
;38    1.800E+01 BXLhQORGMRaPUWNCcPCcP
;38    2.000E+01 BALWdOQdMCGPFFNScPScP
;38    2.200E+01 aVLwGOqUMcAPVQNDBPDBP
;38    2.400E+01 qILFiOQYMsDPVbNT@PT@P
;38    2.600E+01 QILFWOAVMCVPwANTHPTHP
;38    2.800E+01 ABLV@OqEMSWPgVNdFPdFP
;38    3.000E+01 XbKuXOaFMcWPWiNtCPtCP
;38    4.000E+01 EBKTYOiILDHPyENdSPdSP
;38    5.000E+01 cAKCcOwFLtHPADODgPDgP
;38    6.000E+01 bCKs@OV@LdQPQBOEFPEFP
;38    8.000E+01 aEKbPOTTLTfPaEOuEPuEP
;38    1.000E+02 HCJRFOcQLeAPqDOUVPUVP
;38    1.500E+02 SWJQTOBPLePPAYOU`PU`P
;38    2.000E+02 BAJaAOqYLEdPQYOVAPVAP
;38    3.000E+02 XbIXVNQILVAPqPOvFPvFP
;38    4.000E+02 EBIvPNXaKfGPqWOVQPVQP
;38    5.000E+02 cAIUUNWCKvGPAbOfQPfQP
;38    6.000E+02 bCItUNUcKFTPAfOfXPfXP
;38    8.000E+02 aEIsQNDUKVTPQaOvWPvWP
;38    1.000E+03 HCHCENSVKfQPQdOFcPFcP
;38    1.500E+03 SWHRCNrGKvPPQiOVbPVbP
;38    2.000E+03 BAHaUNqXKvUPBBOVgPVgP
;38    3.000E+03 XbGQENQHKF`PBFOGBPGBP
;38    4.000E+03 EBGHeMHhJFcPBGOGEPGEP
;38    5.000E+03 cAGgDMW@JFePBIOGGPGGP
;38    6.000E+03 bCGVDMUbJFfPBIOGHPGHP
;38    8.000E+03 aEGtSMDTJFhPR@OW@PW@P
;38    1.000E+04 HCFCfMSUJFiPRAOWAPWAP
;38    1.500E+04 SWFbWMrGJVaPRBOWBPWBP
;38    2.000E+04 BAFBEMqWJVaPRCOWCPWCP
;38    3.000E+04 XbEAQMQHJVbPRCOWDPWDP
;38    4.000E+04 EBEAIMHgIVcPRDOWDPWDP
;38    5.000E+04 cAEHeLW@IVcPRDOWDPWDP
;38    6.000E+04 bCEGXLUbIVcPRDOWEPWEP
;38    8.000E+04 aEEuSLDTIVcPRDOWEPWEP
;38    1.000E+05 HCDdVLSUIVcPRDOWEPWEP
;==== ELEMENT  39
;39    1.000E-03 vGRHPOCfU@@R@@RCfUCfU
;39    1.500E-03 UeRqFPAYU@@R@@RAYUAYU
;39    2.000E-03 UURAcPwFT@@R@@RGRTwFT
;39    2.080E-03 EYRQ`PfXT@@R@@RvTTfXT
;39 L3 2.080E-03 EYRQ`PbRU@@R@@RbSUbRU
;39    2.117E-03 EVRQdPBWU@@R@@RBXUBWU
;39    2.155E-03 ESRQgPrDU@@R@@RrDUrDU
;39 L2 2.155E-03 ESRQgPcFU@@R@@RcFUcFU
;39    2.261E-03 uERBGPRaU@@R@@RRaURaU
;39    2.373E-03 eFRRFPRYU@@R@@RbPURYU
;39 L1 2.373E-03 eFRRFPRfU@@R@@RRfURfU
;39    3.000E-03 DbRrPPaUU@@R@@RaUUaUU
;39    4.000E-03 d@RSPPGiT@@R@@RWcTGiT
;39    5.000E-03 cWRdCPtIT@@R@@RDRTtIT
;39    6.000E-03 cDRDhPbYT@@R@@RrSTbYT
;39    8.000E-03 RXRUhPaCT@@R@@RaFTaCT
;39    1.000E-02 BIRFfPfVS@@R@@RFgSfVS
;39    1.500E-02 qARXTPRDS@@R@@RbHSRES
;39    1.704E-02 QARIFPAYS@@R@@RaQSQPS
;39 K  1.704E-02 QARIFPABT@@R@@RACTABT
;39    2.000E-02 X`QyPPvVS@@R@@RFfSvWS
;39    3.000E-02 EDQQAQbGS@@R@@RrCSbHS
;39    4.000E-02 cFQQGQABS@@R@@RAFSACS
;39    5.000E-02 bGQaAQERR@@R@@RuVRUTR
;39    6.000E-02 aWQaBQc@R@@R@@RCYRsCR
;39    8.000E-02 ABQaAQqHR@@R@@RaQRQQR
;39    1.000E-01 FhPQIQWGQ@@R@@RIEQxFQ
;39    1.500E-01 sBPQAQREQ@@R@@RcPQcFQ
;39    2.000E-01 QdPACQiAP@@R@@RREQQeQ
;39    3.000E-01 XeOYDPBeP@@R@@RaIQa@Q
;39    4.000E-01 UCOhFPaHP@@R@@RAAQYTP
;39    5.000E-01 sBOWWPWEO@@R@@RhQPhHP
;39    6.000E-01 rBOGBPTTO@@R@@RwPPGWP
;39    8.000E-01 qAOVHPrCO@@R@@RVUPFRP
;39    1.000E+00 HSNUWPAUO@@R@@RE`PuQP
;39    1.022E+00 HHNUQPqHO@@R@@RuSPeUP
;39    1.250E+00 EQNThPyANQIN@@RUDPEIP
;39    1.500E+00 sVNTSPfYNuUN@@RdYPdUP
;39    2.000E+00 RBNCgPDINBGO@@RTDPTBP
;39    2.044E+00 BCNCbPSeNbAO@@RT@PDHP
;39    3.000E+00 ISMCDPRINUQOAGMcSPcRP
;39    4.000E+00 u@MRTPAVNhTOtEMCSPCRP
;39    5.000E+00 CPMRIPAINQDPhVMsEPsEP
;39    6.000E+00 rFMQdPhSMqHPqCNsDPsDP
;39    7.000E+00 qSMqTPWCMQYPqYNsFPsFP
;39    8.000E+00 qCMQYPFGMqXPbDNCPPCPP
;39    9.000E+00 AEMAVPeGMQfPbVNCUPCUP
;39    1.000E+01 HYLqEPdVMRBPCGNSPPSPP
;39    1.100E+01 GBLaFPTHMbFPCVNSVPSVP
;39    1.200E+01 U`LQHPsXMBPPCbNcRPcRP
;39    1.300E+01 EBLQAPCUMRRPTFNcXPcXP
;39    1.400E+01 tCLAEPSHMbSPDYNsSPsSP
;39    1.500E+01 sWLA@PRdMrTPD`NsYPsYP
;39    1.600E+01 sBLYROrTMBdPEINCdPCdP
;39    1.800E+01 bRLxPOBPMCBPeSNSePSeP
;39    2.000E+01 RBLHCORDMSHPVCNDEPDEP
;39    2.200E+01 qULGVOQcMsCPVXNTDPTDP
;39    2.400E+01 AWLVgOqVMCVPG@NdCPdCP
;39    2.600E+01 aFLVTOaRMSYPwHNtBPtBP
;39    2.800E+01 AHLVGOAYMsPPwTNtIPtIP
;39    3.000E+01 ITKEdOqIMC`PHGNDWPDWP
;39    4.000E+01 uAKdTOACMdBPIVNtXPtXP
;39    5.000E+01 CPKChOXDLTSPAEOECPECP
;39    6.000E+01 rFKsDOvTLtXPQCOeCPeCP
;39    8.000E+01 qCKbSOEBLUDPaFOUSPUSP
;39    1.000E+02 HYJRIOSiLuIPqEOuUPuUP
;39    1.500E+02 sWJQVObULE`PQQOV@PV@P
;39    2.000E+02 RBJaBOQhLFDPaPOvBPvBP
;39    3.000E+02 ITIhUNqALvBPqROVXPVXP
;39    4.000E+02 uAIvWNIeKFYPqYOvSPvSP
;39    5.000E+02 CPIeQNGgKVYPAdOFcPFcP
;39    6.000E+02 rFID`NVUKfWPAhOV`PV`P
;39    8.000E+02 qCIsUNTaKvWPQcOG@PG@P
;39    1.000E+03 HYHCINScKFcPQfOGFPGFP
;39    1.500E+03 sWHRFNbRKVcPBAOWEPWEP
;39    2.000E+03 RBHaWNQfKVhPBDOg@Pg@P
;39    3.000E+03 ITGQFNqAKGDPBHOgFPgFP
;39    4.000E+03 uAGXeMIaJGGPBIOgIPgIP
;39    5.000E+03 CPGwBMGdJGIPRAOwAPwAP
;39    6.000E+03 rFGfAMVTJWAPRAOwBPwBP
;39    8.000E+03 qCGtXMT`JWBPRBOwDPwDP
;39    1.000E+04 HYFS`MSbJWCPRCOwEPwEP
;39    1.500E+04 sWFrPMbQJWEPRDOwFPwFP
;39    2.000E+04 RBFBGMQfJWEPREOwGPwGP
;39    3.000E+04 ITEASMqAJWGPREOwHPwHP
;39    4.000E+04 uAEQ@MI`IWGPRFOwHPwHP
;39    5.000E+04 CPEXdLGdIWGPRFOwIPwIP
;39    6.000E+04 rFEWVLVTIWGPRFOwIPwIP
;39    8.000E+04 qCEE`LT`IWGPRFOwIPwIP
;39    1.000E+05 HYDtRLSbIWHPRFOGPPGPP
;==== ELEMENT  40
;40    1.000E-03 VURX@Od@U@@R@@RdAUd@U
;40    1.500E-03 VCRqDPaSU@@R@@RaSUaSU
;40    2.000E-03 uQRAaPHFT@@R@@RXBTHFT
;40    2.222E-03 URRBBPf@T@@R@@RfFTf@T
;40 L3 2.222E-03 URRBBPrIU@@R@@RrIUrIU
;40    2.264E-03 EYRBFPbEU@@R@@RbEUbEU
;40    2.307E-03 EVRBIPRAU@@R@@RRBURAU
;40 L2 2.307E-03 EVRBIPReU@@R@@RReUReU
;40    2.417E-03 uGRRIPbSU@@R@@RbTUbSU
;40    2.532E-03 eIRbIPrEU@@R@@RrFUrEU
;40 L1 2.532E-03 eIRbIPbYU@@R@@RbYUbYU
;40    3.000E-03 TdRbYPqWU@@R@@RqWUqWU
;40    4.000E-03 t@RCXPHVT@@R@@RXQTHVT
;40    5.000E-03 sVRdAPtRT@@R@@RtVTtRT
;40    6.000E-03 sARDfPR`T@@R@@RRcTR`T
;40    8.000E-03 bTRUfPqCT@@R@@RqFTqCT
;40    1.000E-02 RERFePg@S@@R@@RGRSg@S
;40    1.500E-02 qERXPPrBS@@R@@RBVSrCS
;40    1.800E-02 AFRiDPqIS@@R@@RQPSqIS
;40 K  1.800E-02 AFRiDPyES@@R@@RIWSyFS
;40    2.000E-02 iBQiTPWDS@@R@@RgDSWES
;40    3.000E-02 eAQQ@QBRS@@R@@RBYSBSS
;40    4.000E-02 sHQQGQAIS@@R@@RQDSQ@S
;40    5.000E-02 rFQa@QEbR@@R@@RVGRUdR
;40    6.000E-02 qSQaAQCUR@@R@@RsTRSWR
;40    8.000E-02 AFQaAQAYR@@R@@RqRRaRR
;40    1.000E-01 WFPQHQwVQ@@R@@RiVQXdQ
;40    1.500E-01 CVPQAQrDQ@@R@@RsYQCTQ
;40    2.000E-01 BBPACQA@Q@@R@@RbDQBCQ
;40    3.000E-01 yDOYDPSAP@@R@@RqBQaBQ
;40    4.000E-01 uEOhEPAPP@@R@@RABQiUP
;40    5.000E-01 CVOWWPGbO@@R@@RhYPxEP
;40    6.000E-01 BROGBPTfO@@R@@RwVPWQP
;40    8.000E-01 qGOVHPRUO@@R@@RVWPFSP
;40    1.000E+00 HaNUVPQYO@@R@@REaPuRP
;40    1.022E+00 HTNUPPQQO@@R@@RuTPeVP
;40    1.250E+00 eUNThPABOaDN@@RUEPEIP
;40    1.500E+00 ScNTSPwBNUeN@@RtPPdVP
;40    2.000E+00 bANCgPDXNRCO@@RTEPTBP
;40    2.044E+00 RBNCbPtBNbHO@@RTAPDIP
;40    3.000E+00 IeMCDPBPNeVOAFMcTPcSP
;40    4.000E+00 UTMRTPaPNHgOtEMCUPCUP
;40    5.000E+00 SUMRIPQINQGPhUMsHPsHP
;40    6.000E+00 BVMQdPIRMAQPqCNsGPsGP
;40    7.000E+00 AaMqTPwXMaSPqYNCPPCPP
;40    8.000E+00 qIMQYPfQMAcPbDNCTPCTP
;40    9.000E+00 Q@MAVPuUMB@PbVNCYPCYP
;40    1.000E+01 HgLqEPEHMRGPCGNSUPSUP
;40    1.100E+01 wCLaFPTUMrBPCUNcRPcQP
;40    1.200E+01 VFLQHPTBMBUPCbNcXPcXP
;40    1.300E+01 eELQAPsVMRXPTFNsTPsTP
;40    1.400E+01 TSLAEPCVMrPPDXNC`PC`P
;40    1.500E+01 SdLYiOcAMB`PtYNCePCeP
;40    1.600E+01 CWLYRORhMRaPEHNSaPSaP
;40    1.800E+01 rTLxPObRMCIPeSNDBPDBP
;40    2.000E+01 bBLHBOrDMcFPVBNTBPTBP
;40    2.200E+01 AcLGUORAMCQPVWNdBPdBP
;40    2.400E+01 QTLVfOQbMSTPViNtAPtAP
;40    2.600E+01 qALVTOqVMcWPwGNDPPDPP
;40    2.800E+01 QCLVGOaSMsXPwSNDXPDXP
;40    3.000E+01 IfKEdOQQMCiPHGNTVPTVP
;40    4.000E+01 UTKdTOQBMtBPITNDhPDhP
;40    5.000E+01 SUKChOHfLdTPAEOUCPUCP
;40    6.000E+01 BVKsDOwDLDhPQCOuCPuCP
;40    8.000E+01 qIKbSOEVLeEPaFOeTPeTP
;40    1.000E+02 HgJRIOtELUQPqEOEgPEgP
;40    1.500E+02 SdJQVOBhLUcPQPOfCPfCP
;40    2.000E+02 bBJaBORELVGPaPOFVPFVP
;40    3.000E+02 IfIhUNASLFVPqROvRPvRP
;40    4.000E+02 UTIvWNAGLfSPqYOFgPFgP
;40    5.000E+02 SUIeQNXWKvTPAdOVhPVhP
;40    6.000E+02 BVID`NWDKFbPAgOGEPGEP
;40    8.000E+02 qIIsUNuEKVbPQbOWEPWEP
;40    1.000E+03 HgHCHNdHKViPQfOgBPgBP
;40    1.500E+03 SdHRENBeKGHPBAOwAPwAP
;40    2.000E+03 bBHaWNRDKWDPBDOwFPwFP
;40    3.000E+03 IfGQFNARKg@PBGOGQPGQP
;40    4.000E+03 UTGXeMAGKgCPBIOGUPGUP
;40    5.000E+03 SUGwAMXTJgEPR@OGWPGWP
;40    6.000E+03 BVGf@MWBJgFPRAOGXPGXP
;40    8.000E+03 qIGtXMuDJgHPRBOWPPWPP
;40    1.000E+04 HgFS`MdGJgIPRCOWPPWPP
;40    1.500E+04 SdFrPMBeJwAPRDOWRPWRP
;40    2.000E+04 bBFBGMRCJwAPRDOWSPWSP
;40    3.000E+04 IfEASMARJwBPREOWTPWTP
;40    4.000E+04 UTEQ@MAGJwCPREOWTPWTP
;40    5.000E+04 SUEXdLXTIwCPREOWUPWUP
;40    6.000E+04 BVEWVLWBIwCPREOWUPWUP
;40    8.000E+04 qIEuYLuDIwCPREOWUPWUP
;40    1.000E+05 HgDtQLdGIwCPRFOWUPWUP
;==== ELEMENT  41
;41    1.000E-03 FaRFdOTYU@@R@@RdPUTYU
;41    1.500E-03 FPRQHPqXU@@R@@RqYUqXU
;41    2.000E-03 UgRaUPHcT@@R@@RHiTHdT
;41    2.371E-03 eVRQiPuYT@@R@@REdTuYT
;41 L3 2.371E-03 eVRQiPRHU@@R@@RRHURHU
;41    2.417E-03 eRRBCPBEU@@R@@RBEUBEU
;41    2.465E-03 UYRBGPQcU@@R@@RQdUQcU
;41 L2 2.465E-03 UYRBGPbYU@@R@@RbYUbYU
;41    2.579E-03 UPRRGPBQU@@R@@RBQUBQU
;41    2.698E-03 EPRbHPRFU@@R@@RRFURFU
;41 L1 2.698E-03 EPRbHPBVU@@R@@RBWUBVU
;41    3.000E-03 UFRRTPQ`U@@R@@RQaUQ`U
;41    4.000E-03 DVRsEPYBT@@R@@RYGTYBT
;41    5.000E-03 CiRT@PEIT@@R@@RUCTEIT
;41    6.000E-03 CRRtWPSDT@@R@@RSGTSDT
;41    8.000E-03 rSRUbPATT@@R@@RAWTATT
;41    1.000E-02 bCRFcPGaS@@R@@RHDSGbS
;41    1.500E-02 AQRHYPRRS@@R@@RbWSRSS
;41    1.899E-02 ACRITPq@S@@R@@RAQSqAS
;41 K  1.899E-02 ACRITPhWS@@R@@RxXShXS
;41    2.000E-02 iRQiUPgPS@@R@@RwQSgQS
;41    3.000E-02 ETQQ@QbPS@@R@@RbWSbQS
;41    4.000E-02 SSQQGQQHS@@R@@RaBSQIS
;41    5.000E-02 BWQa@QfHR@@R@@RfTRFPR
;41    6.000E-02 AbQaBQsSR@@R@@RDCRCeR
;41    8.000E-02 QAQaAQaRR@@R@@RAeRqTR
;41    1.000E-01 WPPQIQHSQ@@R@@RADRiRQ
;41    1.500E-01 cRPQAQRUQ@@R@@RDBQcVQ
;41    2.000E-01 RBPADQAIQ@@R@@RrDQRCQ
;41    3.000E-01 IaOYIPCPP@@R@@RqFQaFQ
;41    4.000E-01 eROx@PQTP@@R@@RADQIdP
;41    5.000E-01 cTOgQPXXO@@R@@RHcPHWP
;41    6.000E-01 RTOGFPEUO@@R@@RGfPgPP
;41    8.000E-01 ATOfBPB`O@@R@@RfTPVPP
;41    1.000E+00 iFNePPqUO@@R@@REgPuWP
;41    1.022E+00 HgNUTPaVO@@R@@RuYPuQP
;41    1.250E+00 UdNEAPQBOq@N@@Re@PUDP
;41    1.500E+00 TCNTVPHENfAN@@RtTPtPP
;41    2.000E+00 rCNCiPTbNbAO@@RTHPTFP
;41    2.044E+00 bCNCdPtUNrGO@@RTEPTCP
;41    3.000E+00 ADNCFPbSNEeOAGMcYPcXP
;41    4.000E+00 EcMRVPqUNYEOtGMSPPCYP
;41    5.000E+00 sSMbAPq@Na@PxQMCTPCSP
;41    6.000E+00 RYMQePACNAUPqDNCSPCSP
;41    7.000E+00 Q`MqUPXRMaXPA`NCVPCVP
;41    8.000E+00 AVMaPPgEMAhPbENSQPSQP
;41    9.000E+00 QEMAWPv@MBFPbXNSVPSVP
;41    1.000E+01 yBLqFPUWMbCPCINcSPcSP
;41    1.100E+01 wQLaGPTiMrHPCWNcYPcYP
;41    1.200E+01 FXLQIPTQMRSPCdNsVPsVP
;41    1.300E+01 URLQBPTBMbVPTHNCbPCbP
;41    1.400E+01 tVLAFPsYMrWPTQNChPChP
;41    1.500E+01 TDLAAPSQMBiPDbNSdPSdP
;41    1.600E+01 cTLYXOcGMRiPUAND@PD@P
;41    1.800E+01 BhLxVOBgMSHPeVNTBPTBP
;41    2.000E+01 rCLHHORVMsEPVFNdBPdBP
;41    2.200E+01 QcLWPOrAMSQPfQNtCPtCP
;41    2.400E+01 aRLGAOR@McUPGCNDRPDRP
;41    2.600E+01 qHLVXOQcMsXPGRNTQPTQP
;41    2.800E+01 QILfAOqXMCiPwXNdPPdPP
;41    3.000E+01 ADLEhOaUMD@PXANdWPdWP
;41    4.000E+01 EcKdWOaBMDTPYPNEAPEAP
;41    5.000E+01 sSKS`OiYLtWPAEOeGPeGP
;41    6.000E+01 RYKsFOHCLECPQDOEXPEXP
;41    8.000E+01 AVKbUOUhLEPPaFOuYPuYP
;41    1.000E+02 yCJb@OtVLeWPqFOFCPFCP
;41    1.500E+02 TDJQWOSELV@PQQOFPPFPP
;41    2.000E+02 rCJaCOrFLvEPaQOfSPfSP
;41    3.000E+02 ADJxQNQWLfTPqROV`PV`P
;41    4.000E+02 EcIFbNQGLFbPqYOGGPGGP
;41    5.000E+02 sSIeTNyGKVcPAdOWGPWGP
;41    6.000E+02 RYIDcNG`KGAPAhOgEPgEP
;41    8.000E+02 AVIsWNEeKWBPQcOwEPwEP
;41    1.000E+03 yBHS@NdXKWIPQfOGRPGRP
;41    1.500E+03 TDHRGNSBKgIPBAOWQPWQP
;41    2.000E+03 rCHaXNrDKwDPBDOWVPWVP
;41    3.000E+03 ADHQGNQVKGPPBGOgRPgRP
;41    4.000E+03 EcGIAMQGKGSPBIOgUPgUP
;41    5.000E+03 sSGwFMyDJGUPR@OgWPgWP
;41    6.000E+03 RYGfDMwXJGWPRAOgXPgXP
;41    8.000E+03 AVGDaMEdJGYPRBOwPPwPP
;41    1.000E+04 yBFScMdWJWPPRCOwRPwRP
;41    1.500E+04 TDFrQMSAJWQPRDOwSPwSP
;41    2.000E+04 rCFBIMrCJWSPRDOwTPwTP
;41    3.000E+04 ADFATMQVJWSPREOwUPwUP
;41    4.000E+04 EcEQAMQGJWTPREOwUPwUP
;41    5.000E+04 sSEI@LyDIWTPREOwUPwUP
;41    6.000E+04 RYEgPLwXIWTPREOwUPwUP
;41    8.000E+04 AVEEcLEdIWTPREOwVPwVP
;41    1.000E+05 yBDtTLdWIWTPREOwVPwVP
;==== ELEMENT  42
;42    1.000E-03 VdRFSOTdU@@R@@RTdUTdU
;42    1.500E-03 VTRQBPQbU@@R@@RQbUQbU
;42    2.000E-03 VARQXPYST@@R@@RiPTYST
;42    2.520E-03 eWRBEPuFT@@R@@RERTuFT
;42 L3 2.520E-03 eWRBEPQgU@@R@@RQhUQgU
;42    2.572E-03 eRRBIPAfU@@R@@RAfUAfU
;42    2.625E-03 UXRRDPqTU@@R@@RqUUqTU
;42 L2 2.625E-03 UXRRDPBSU@@R@@RBSUBSU
;42    2.743E-03 EXRbDPRHU@@R@@RRHURHU
;42    2.866E-03 uHRrDPQfU@@R@@RQfUQfU
;42 L1 2.866E-03 uHRrDPbDU@@R@@RbDUbDU
;42    3.000E-03 eGRBUPBAU@@R@@RBAUBAU
;42    4.000E-03 TVRcEPiVT@@R@@RyQTiVT
;42    5.000E-03 SgRShPEQT@@R@@REUTEQT
;42    6.000E-03 CYRdUPsDT@@R@@RsGTsDT
;42    8.000E-03 rWRE`PQTT@@R@@RQWTQTT
;42    1.000E-02 bGRvRPxDS@@R@@RXXSxES
;42    1.500E-02 AURxGPrPS@@R@@RBeSrQS
;42    2.000E-02 IiQYQPa@S@@R@@RqASaAS
;42 K  2.000E-02 IiQYQPGeS@@R@@RWeSGfS
;42    2.000E-02 IiQYQPGeS@@R@@RWeSGfS
;42    3.000E-02 UXQAIQrTS@@R@@RBaSrUS
;42    4.000E-02 cTQQFQaES@@R@@RaISaFS
;42    5.000E-02 RTQQIQfWR@@R@@RGDRvYR
;42    6.000E-02 AgQa@QSgR@@R@@RdGRDIR
;42    8.000E-02 QDQa@QqSR@@R@@RQfRAeR
;42    1.000E-01 wSPQHQIAQ@@R@@RQ@RABR
;42    1.500E-01 sTPQ@QrSQ@@R@@RdAQCcQ
;42    2.000E-01 RIPACQQHQ@@R@@RBRQb@Q
;42    3.000E-01 AAPYAPcWP@@R@@RqHQaHQ
;42    4.000E-01 EbOhCPaVP@@R@@RAEQIiP
;42    5.000E-01 sVOWTPiFO@@R@@RHePHWP
;42    6.000E-01 bSOG@PEiO@@R@@RGePWYP
;42    8.000E-01 AYOVGPCCO@@R@@RfRPFWP
;42    1.000E+00 YXNUUPAiO@@R@@REdPuTP
;42    1.022E+00 YHNEYPA`O@@R@@RuWPeWP
;42    1.250E+00 VENTgPaAOqDN@@RUGPU@P
;42    1.500E+00 dHNTRPxPNvGN@@RtQPdWP
;42    2.000E+00 BQNCfPuBNbFO@@RTFPTDP
;42    2.044E+00 rANCaPUCNBRO@@RTCPTAP
;42    3.000E+00 AGNCDPBdNUgOAFMcWPcVP
;42    4.000E+00 FCMRTPAiNyBOtDMSPPCYP
;42    5.000E+00 CfMRIPAQNaBPhTMCTPCTP
;42    6.000E+00 bXMQdPQANAXPqCNCTPCTP
;42    7.000E+00 QgMqTPYIMqPPqYNCWPCWP
;42    8.000E+00 QQMQXPGaMQaPbCNSRPSRP
;42    9.000E+00 QIMAVPvYMBIPbVNSXPSXP
;42    1.000E+01 iULqEPF@MbFPCFNcUPcUP
;42    1.100E+01 WhLaFPuGMBRPCUNsRPsRP
;42    1.200E+01 vPLQHPDfMRVPCaNsYPsXP
;42    1.300E+01 uQLQAPDTMbYPTENCePCeP
;42    1.400E+01 TcLAEPDHMBbPDWNSbPSbP
;42    1.500E+01 dILYhOsXMRcPtXNShPShP
;42    1.600E+01 sWLYPOSRMCCPEGNDDPDDP
;42    1.800E+01 RhLhYOCIMcCPeQNTEPTEP
;42    2.000E+01 BQLHAOrUMCPPV@NdFPdFP
;42    2.200E+01 B@LGTOBXMSVPVUNtGPtGP
;42    2.400E+01 aXLVeObFMsPPVgNDWPDWP
;42    2.600E+01 ASLVSOBHMCcPwENTVPTVP
;42    2.800E+01 aCLVFOQbMSePwQNdTPdTP
;42    3.000E+01 AGLEcOqXMDFPHDNtRPtRP
;42    4.000E+01 FDKdSOqBMTQPIQNEFPEFP
;42    5.000E+01 CfKCgOADMDdPAEOuCPuCP
;42    6.000E+01 bXKsCOhTLEIPQCOUTPUTP
;42    8.000E+01 QQKbSOFSLEXPaEOEfPEfP
;42    1.000E+02 iUJRHOUBLuUPqDOV@PV@P
;42    1.500E+02 dIJQUOsILVHPAYOFXPFXP
;42    2.000E+02 BQJaBORTLFSPQYOvQPvQP
;42    3.000E+02 AGJhTNaYLvTPqQOViPViP
;42    4.000E+02 FCIvVNaFLVaPqXOWFPWFP
;42    5.000E+02 CfIePNAALGBPAbOgFPgFP
;42    6.000E+02 bXItYNHPKWAPAfOwDPwDP
;42    8.000E+02 QQIsTNv@KgAPQaOGTPGTP
;42    1.000E+03 iUHCHNECKgHPQdOWQPWQP
;42    1.500E+03 dIHRENsEKwHPQiOgPPgPP
;42    2.000E+03 BQHaVNRQKGTPBBOgVPgVP
;42    3.000E+03 AGHQFNaXKWPPBEOwRPwRP
;42    4.000E+03 FCGXcMaFKWSPBGOwUPwUP
;42    5.000E+03 CfGwAMA@KWUPBHOwWPwWP
;42    6.000E+03 bXGVIMxGJWVPBHOwXPwXP
;42    8.000E+03 QQGtWMfHJWXPBIOG`PG`P
;42    1.000E+04 iUFS`MEBJgPPR@OGaPGaP
;42    1.500E+04 dIFbYMsEJgQPRAOGcPGcP
;42    2.000E+04 BQFBGMRQJgRPRBOGcPGcP
;42    3.000E+04 AGFASMaWJgSPRBOGePGeP
;42    4.000E+04 FCEQ@MaFJgSPRBOGePGeP
;42    5.000E+04 CfEXcLA@JgTPRBOGePGeP
;42    6.000E+04 bXEWTLxGIgTPRCOGePGeP
;42    8.000E+04 QQEuYLfHIgUPRCOGfPGfP
;42    1.000E+05 iUDtQLEBIgUPRCOGfPGfP
;==== ELEMENT  43
;43    1.000E-03 WBRGBOuEU@@R@@RuFUuEU
;43    1.500E-03 fYRaAPBIU@@R@@RBIUBIU
;43    2.000E-03 fDRaWPADU@@R@@RADUADU
;43    2.677E-03 eVRbFPEAT@@R@@REGTEBT
;43 L3 2.677E-03 eVRbFPAaU@@R@@RAaUAaU
;43    2.734E-03 eRRrAPqPU@@R@@RqPUqPU
;43    2.793E-03 UWRrEPaPU@@R@@RaPUaPU
;43 L2 2.793E-03 UWRrEPbBU@@R@@RbBUbBU
;43    3.000E-03 EPRRRPAfU@@R@@RAfUAfU
;43    3.043E-03 uGRRVPqYU@@R@@RA`UqYU
;43 L1 3.043E-03 uGRRVPBEU@@R@@RBFUBEU
;43    4.000E-03 dWRs@PACU@@R@@RADUACU
;43    5.000E-03 DGRDAPuYT@@R@@REdTE`T
;43    6.000E-03 SXRdVPSXT@@R@@RcRTSXT
;43    8.000E-03 BdRE`PaUT@@R@@RaXTaUT
;43    1.000E-02 rCRvRPXiS@@R@@RiCSI@S
;43    1.500E-02 QPRxGPRbS@@R@@RCHSRcS
;43    2.000E-02 ACRYPPq@S@@R@@RAQSqAS
;43    2.104E-02 YVQiXPQBS@@R@@RaCSQCS
;43 K  2.104E-02 YVQiXPwGS@@R@@RGXSwHS
;43    3.000E-02 uXQAIQRbS@@R@@RRiSRcS
;43    4.000E-02 sWQQEQqCS@@R@@RqHSqDS
;43    5.000E-02 bTQQIQWDR@@R@@RWRRgFR
;43    6.000E-02 QeQa@QdFR@@R@@RTWRtHR
;43    8.000E-02 QIQa@QAfR@@R@@RR@RQhR
;43    1.000E-01 HEPQHQyQQ@@R@@RQGRAIR
;43    1.500E-01 CiPQ@QRfQ@@R@@RDUQDFQ
;43    2.000E-01 bIPACQaGQ@@R@@RRSQrAQ
;43    3.000E-01 AFPYCPShP@@R@@RARQqAQ
;43    4.000E-01 FGOhEPAaP@@R@@RAGQAAQ
;43    5.000E-01 ScOWWPAAP@@R@@RXgPXWP
;43    6.000E-01 rUOGBPFRO@@R@@RWcPgVP
;43    8.000E-01 QVOVIPsAO@@R@@RfWPVRP
;43    1.000E+00 A@OUWPBFO@@R@@REhPuXP
;43    1.022E+00 YXNUQPQfO@@R@@RE`PuQP
;43    1.250E+00 FRNTiPqBOAPN@@Re@PUCP
;43    1.500E+00 DWNTSPYPNfRN@@RtTPtPP
;43    2.000E+00 RRNCgPE`NrDO@@RTIPTFP
;43    2.044E+00 BQNCbPePNRPO@@RTEPTCP
;43    3.000E+00 QBNCEPS@NVDOAGMsQPcYP
;43    4.000E+00 v@MRTPBFNYXOtEMSSPSSP
;43    5.000E+00 DCMb@PQSNaFPhVMCXPCXP
;43    6.000E+00 B`MQdPaANQRPqCNCYPCXP
;43    7.000E+00 BFMqTPA@NqUPqYNSRPSRP
;43    8.000E+00 QXMQYPXPMQfPbDNSXPSXP
;43    9.000E+00 aDMAVPwIMREPbWNcTPcTP
;43    1.000E+01 AAMqEPVSMrBPCGNsQPsQP
;43    1.100E+01 xCLaFPEdMBXPCVNsXPsXP
;43    1.200E+01 G@LQHPeIMbSPCbNCePCeP
;43    1.300E+01 UgLQAPDcMrVPTFNSbPSbP
;43    1.400E+01 UDLAEPDTMBiPDYNSiPSiP
;43    1.500E+01 DXLA@PTAMC@PtYNDFPDFP
;43    1.600E+01 SdLYSOCbMSAPEINTBPTBP
;43    1.800E+01 SALxROsFMsAPeSNdDPdDP
;43    2.000E+01 RRLHDORiMCXPVBNtEPtEP
;43    2.200E+01 BHLGWOrPMcUPVXNDVPDVP
;43    2.400E+01 qULVhOBVMsYPViNTVPTVP
;43    2.600E+01 AYLVUObEMSbPwGNdVPdVP
;43    2.800E+01 aILVHOBHMDEPwSNtTPtTP
;43    3.000E+01 QBLEeOQdMTFPHFNDcPDcP
;43    4.000E+01 v@KdUOASMdRPISNUHPUHP
;43    5.000E+01 DCKChOQCMTfPAEOEUPEUP
;43    6.000E+01 B`KsDOyILeBPQCOeWPeWP
;43    8.000E+01 QXKbTOVhLeQPaEOF@PF@P
;43    1.000E+02 AAKRIOUVLEiPqEOfDPfDP
;43    1.500E+02 DXJQVOcXLvCPQPOfTPfTP
;43    2.000E+02 RRJaBOrULVYPQYOFgPFgP
;43    3.000E+02 QBJhVNAcLV`PqQOWEPWEP
;43    4.000E+02 v@IvXNqGLGGPqXOwBPwBP
;43    5.000E+02 DCIeRNAILWIPAcOGSPGSP
;43    6.000E+02 B`IDaNYBKgHPAfOWQPWQP
;43    8.000E+02 QXIsVNFcKwIPQaOgRPgRP
;43    1.000E+03 AAICINEWKGUPQeOgXPgXP
;43    1.500E+03 DXHRFNcTKWVPQiOwXPwXP
;43    2.000E+03 RRHaWNrSKgRPBBOGdPGdP
;43    3.000E+03 QBHQFNAbKgXPBEOW`PW`P
;43    4.000E+03 v@GXfMqFKwQPBGOWcPWcP
;43    5.000E+03 DCGwCMAIKwSPBHOWePWeP
;43    6.000E+03 B`GfAMIIJwUPBIOWgPWgP
;43    8.000E+03 QXGtYMFbJwWPR@OWhPWhP
;43    1.000E+04 AAGSaMEVJwXPRAOH@PH@P
;43    1.500E+04 DXFrPMcTJwYPRBOHAPHAP
;43    2.000E+04 RRFBHMrSJGaPRBOHBPHBP
;43    3.000E+04 QBFASMAbJGaPRCOHCPHCP
;43    4.000E+04 v@EQ@MqFJGbPRCOHCPHCP
;43    5.000E+04 DCEXfLAIJGbPRCOHDPHDP
;43    6.000E+04 B`EWWLIIIGbPRCOHDPHDP
;43    8.000E+04 QXEE`LFbIGbPRCOHDPHDP
;43    1.000E+05 AAEtRLEUIGcPRDOHDPHDP
;==== ELEMENT  44
;44    1.000E-03 gHRuYOuQU@@R@@RuRUuQU
;44    1.500E-03 FhRADPbCU@@R@@RbDUbCU
;44    2.000E-03 FTRQPPQAU@@R@@RQBUQAU
;44    2.838E-03 uQRbCPdUT@@R@@RtPTdUT
;44 L3 2.838E-03 uQRbCPaTU@@R@@RaTUaTU
;44    2.902E-03 eVRbHPQTU@@R@@RQUUQTU
;44    2.967E-03 ePRrCPAUU@@R@@RAUUAUU
;44 L2 2.967E-03 ePRrCPQgU@@R@@RQgUQgU
;44    3.000E-03 UWRrFPQfU@@R@@RQfUQfU
;44    3.224E-03 uIRRTPaSU@@R@@RaTUaSU
;44 L1 3.224E-03 uIRRTPAgU@@R@@RAgUAgU
;44    4.000E-03 DaRSDPAIU@@R@@RAIUAIU
;44    5.000E-03 TGRCfPVCT@@R@@RVGTVCT
;44    6.000E-03 cVRTRPsYT@@R@@RCcTsYT
;44    8.000E-03 R`ReWPqVT@@R@@RqXTqVT
;44    1.000E-02 rGRfPPYVS@@R@@RI`SYVS
;44    1.500E-02 QSRhEPSAS@@R@@RcGSSBS
;44    2.000E-02 AERyFPqHS@@R@@RQPSqIS
;44    2.212E-02 YHQyRPADS@@R@@RQDSAES
;44 K  2.212E-02 YHQyRPvWS@@R@@RFhSvXS
;44    3.000E-02 UcQAGQCGS@@R@@RSDSCHS
;44    4.000E-02 CgQQDQAPS@@R@@RAUSAQS
;44    5.000E-02 rRQQGQWSR@@R@@RWbRgUR
;44    6.000E-02 B@QQIQTPR@@R@@RDbRdRR
;44    8.000E-02 aBQQIQQgR@@R@@RbARBIR
;44    1.000E-01 hHPQGQACR@@R@@RaCRQER
;44    1.500E-01 DAPAIQSEQ@@R@@RdUQdEQ
;44    2.000E-01 rEPABQqFQ@@R@@RbRQrHQ
;44    3.000E-01 AIPIDPdGP@@R@@RATQqCQ
;44    4.000E-01 fFOXGPQdP@@R@@RAGQAAQ
;44    5.000E-01 DEOWPPAIP@@R@@RXiPXYP
;44    6.000E-01 BcOVfPVaO@@R@@RWcPgUP
;44    8.000E-01 aQOVCPSVO@@R@@RfUPFYP
;44    1.000E+00 ACOURPbBO@@R@@REePuTP
;44    1.022E+00 Y`NEVPRAO@@R@@RuWPeWP
;44    1.250E+00 fSNTdPAROATN@@RUFPU@P
;44    1.500E+00 dQNDYPABOvXN@@RtQPdVP
;44    2.000E+00 bPNCdPfCNrHO@@RTFPTDP
;44    2.044E+00 BYNsYPFBNRUO@@RTCPTAP
;44    3.000E+00 QFNCBPsCNfEOAFMcYPcXP
;44    4.000E+00 VQMRRPbANySOtAMSSPSRP
;44    5.000E+00 TFMRHPaTNaHPXYMCXPCXP
;44    6.000E+00 BiMQbPq@NQTPqBNCYPCYP
;44    7.000E+00 RCMqSPAGNqWPqWNSSPSSP
;44    8.000E+00 aSMQWPYBMQhPbBNSYPSYP
;44    9.000E+00 aIMAUPWbMRGPbTNcVPcVP
;44    1.000E+01 ADMqDPG@MrEPCDNsSPsSP
;44    1.100E+01 hQLaEPfFMRQPCRNC`PC`P
;44    1.200E+01 gCLQGPeWMbVPsXNChPChP
;44    1.300E+01 VFLQAPUGMB`PTBNSePSeP
;44    1.400E+01 uALAEPtVMRbPDTNDBPDBP
;44    1.500E+01 dSLYcODPMCDPtUNDHPDHP
;44    1.600E+01 DGLIUOT@MSEPEDNTEPTEP
;44    1.800E+01 cALhTOcPMsEPUXNdGPdGP
;44    2.000E+01 bPLWgOcAMSSPFGNtIPtIP
;44    2.200E+01 RELGPOBiMcYPVQNTPPTPP
;44    2.400E+01 AaLVbObSMCdPVbNdPPdPP
;44    2.600E+01 QTLFYOBQMSgPw@NtPPtPP
;44    2.800E+01 qCLVCObCMT@PgVNtYPtYP
;44    3.000E+01 QFLE`OBGMdAPWhNDgPDgP
;44    4.000E+01 VQKdQOQSMdWPyDNeCPeCP
;44    5.000E+01 TGKCeOaAMEAPADOUPPUPP
;44    6.000E+01 BiKsAOA@MeHPQBOuRPuRP
;44    8.000E+01 aSKbQOGXLeWPaDOFFPFFP
;44    1.000E+02 ADKRGOUeLUePqCOv@Pv@P
;44    1.500E+02 dSJQUOSdLFPPAXOvPPvPP
;44    2.000E+02 bPJaAOReLfWPQXOVePVeP
;44    3.000E+02 QFJXYNQfLVhPaYOgCPgCP
;44    4.000E+02 VQIvSNAWLWFPqVOGPPGPP
;44    5.000E+02 TGIUWNQGLgGPAaOWQPWQP
;44    6.000E+02 BiItWNyWKwFPAdOWYPWYP
;44    8.000E+02 aSIsRNwBKGWPAiOgYPgYP
;44    1.000E+03 ADICFNEeKWTPQbOwWPwWP
;44    1.500E+03 dSHRDNS`KgTPQgOGfPGfP
;44    2.000E+03 bPHaVNRbKwPPB@OWbPWbP
;44    3.000E+03 QFHQENQeKwVPBCOWhPWhP
;44    4.000E+03 VQGHhMAVKG`PBDOHAPHAP
;44    5.000E+03 TGGgFMQGKGbPBEOHCPHCP
;44    6.000E+03 BiGVFMyTJGdPBFOHEPHEP
;44    8.000E+03 aSGtUMw@JGePBGOHFPHFP
;44    1.000E+04 ADGCgMEdJGgPBHOHHPHHP
;44    1.500E+04 dSFbXMCiJGhPBIOHIPHIP
;44    2.000E+04 bPFBFMRbJGiPBIOX@PX@P
;44    3.000E+04 QFFARMQeJW`PBIOXAPXAP
;44    4.000E+04 VQEAIMAVJWaPR@OXBPXBP
;44    5.000E+04 TGEHhLQGJWaPR@OXBPXBP
;44    6.000E+04 BiEWPLySIWaPR@OXBPXBP
;44    8.000E+04 aSEuULw@IWaPR@OXBPXBP
;44    1.000E+05 ADEdXLEdIWaPR@OXBPXBP
;==== ELEMENT  45
;45    1.000E-03 GYRUSOVFU@@R@@RVGUVFU
;45    1.500E-03 W@RA@PBRU@@R@@RBSUBRU
;45    2.000E-03 fVRAUPaAU@@R@@RaAUaAU
;45    3.000E-03 uWRrAPtHT@@R@@RDTTtHT
;45    3.004E-03 uWRrBPtGT@@R@@RDSTtGT
;45 L3 3.004E-03 uWRrBPQQU@@R@@RQQUQQU
;45    3.074E-03 uQRrGPARU@@R@@RARUARU
;45    3.146E-03 eURBSPqCU@@R@@RqDUqCU
;45 L2 3.146E-03 eURBSPAdU@@R@@RAeUAdU
;45    3.276E-03 UTRRTPaWU@@R@@RaWUaWU
;45    3.412E-03 ESRbTPQQU@@R@@RQQUQQU
;45 L1 3.412E-03 ESRbTPqSU@@R@@RqSUqSU
;45    4.000E-03 ThRCIPQGU@@R@@RQGUQGU
;45    5.000E-03 tBRC`PVTT@@R@@RVYTVTT
;45    6.000E-03 sXRDVPDFT@@R@@RT@TDFT
;45    8.000E-03 RiReRPAhT@@R@@RQaTAhT
;45    1.000E-02 BTRVWPACT@@R@@RAETACT
;45    1.500E-02 QYRhDPsES@@R@@RSRSsFS
;45    2.000E-02 AIRyFPQPS@@R@@RaQSQPS
;45    2.322E-02 XaQIiPI`R@@R@@RAHSY`R
;45 K  2.322E-02 XaQIiPvAS@@R@@RFQSvBS
;45    3.000E-02 VEQAGQcFS@@R@@RsCScGS
;45    4.000E-02 DBQQDQAYS@@R@@RQTSQPS
;45    5.000E-02 BcQQHQHER@@R@@RHURXFR
;45    6.000E-02 BIQQIQDbR@@R@@RUERTdR
;45    8.000E-02 aGQQIQRBR@@R@@RrFRbDR
;45    1.000E-01 hRPQGQQAR@@R@@RqARaCR
;45    1.500E-01 THPQ@QCPQ@@R@@RTbQTPQ
;45    2.000E-01 BVPABQAWQ@@R@@RrTQRPQ
;45    3.000E-01 QDPIHPdSP@@R@@RAXQqGQ
;45    4.000E-01 VTOhAPRAP@@R@@RQ@QACQ
;45    5.000E-01 dCOWSPQHP@@R@@RYCPxQP
;45    6.000E-01 RfOViPWQO@@R@@RHDPwTP
;45    8.000E-01 aXOVFPCgO@@R@@RvQPVTP
;45    1.000E+00 AHOUTPBQO@@R@@REiPuYP
;45    1.022E+00 ACOEYPr@O@@R@@REbPuRP
;45    1.250E+00 VcNTfPQUOQPN@@Re@PUCP
;45    1.500E+00 DbNTQPQAOGEN@@RtTPtPP
;45    2.000E+00 rRNCfPvXNBWO@@Rd@PTGP
;45    2.044E+00 bPNCaPVTNbTO@@RTFPTDP
;45    3.000E+00 aANCCPcRNFTOAFMsSPsRP
;45    4.000E+00 F`MRSPBQNA@PtCMSWPSVP
;45    5.000E+00 tEMRIPqXNqAPhSMSSPSRP
;45    6.000E+00 CBMQcPAQNQXPqBNSTPSTP
;45    7.000E+00 bBMqTPQFNAbPqXNSYPSYP
;45    8.000E+00 qPMQXPIiMBCPbCNcUPcUP
;45    9.000E+00 qDMAUPXYMbCPbUNsRPsRP
;45    1.000E+01 AIMqEPWXMBQPCFNC`PC`P
;45    1.100E+01 I@LaFPvYMRXPCTNCgPCgP
;45    1.200E+01 WVLQHPVDMrSPC`NSePSeP
;45    1.300E+01 FTLQAPeQMBgPTDNDCPDBP
;45    1.400E+01 UVLAEPUFMC@PDVNT@PT@P
;45    1.500E+01 DdLYgOtWMSBPtWNTGPTGP
;45    1.600E+01 dELIYODTMcCPEFNdCPdCP
;45    1.800E+01 sFLhXOS`MCSPePNtFPtFP
;45    2.000E+01 rRLH@OCWMcRPFINDXPDXP
;45    2.200E+01 bELGSOSCMsXPVTNTYPTYP
;45    2.400E+01 AiLVeOBeMScPVeNtPPtPP
;45    2.600E+01 aQLVSObRMDGPwCND`PD`P
;45    2.800E+01 qILVEOBRMd@PgXNDiPDiP
;45    3.000E+01 aALEcObEMtAPHANThPThP
;45    4.000E+01 FaKdSOaVMtYPyHNuEPuEP
;45    5.000E+01 tFKCfOqAMUDPADOeSPeSP
;45    6.000E+01 CBKsCOAIMEQPQBOEfPEfP
;45    8.000E+01 qPKbSOX@LEaPaEOf@Pf@P
;45    1.000E+02 AIKRHOFULV@PqDOFUPFUP
;45    1.500E+02 DdJQUOdGLVUPAYOFfPFfP
;45    2.000E+02 rRJaBOSILFcPQXOWAPWAP
;45    3.000E+02 aAJhSNRBLWEPaYOGPPGPP
;45    4.000E+02 FaIvUNQYLwCPqVOWWPWWP
;45    5.000E+02 tFIUYNaGLGUPAaOgYPgYP
;45    6.000E+02 CBItYNAFLWSPAeOwVPwVP
;45    8.000E+02 qPIsTNWbKgUPAiOGhPGhP
;45    1.000E+03 AIICHNvDKwRPQbOWdPWdP
;45    1.500E+03 DdHRENdBKGcPQgOHEPHEP
;45    2.000E+03 rRHaVNSFKGiPB@OXAPXAP
;45    3.000E+03 aAHQFNRAKWePBCOXFPXFP
;45    4.000E+03 FaGXbMQXKWhPBDOh@Ph@P
;45    5.000E+03 tFGw@MaGKHAPBFOhBPhBP
;45    6.000E+03 CBGVIMAEKHBPBFOhDPhDP
;45    8.000E+03 qPGtWMWaJHDPBGOhEPhEP
;45    1.000E+04 AIGCiMvCJHEPBHOhFPhFP
;45    1.500E+04 DdFbYMdBJHGPBIOhHPhHP
;45    2.000E+04 rRFBGMSFJHHPBIOhIPhIP
;45    3.000E+04 aAFASMRAJHIPR@Ox@Px@P
;45    4.000E+04 FaEAIMQXJHIPR@Ox@Px@P
;45    5.000E+04 tFEXbLaFJHIPR@Ox@Px@P
;45    6.000E+04 CBEWTLAEJX@PR@OxAPxAP
;45    8.000E+04 qPEuXLWaIX@PR@OxAPxAP
;45    1.000E+05 AIEtPLvCIXAPRAOxBPxBP
;==== ELEMENT  46
;46    1.000E-03 gSRdGOVSU@@R@@RVTUVSU
;46    1.500E-03 gGRHWORWU@@R@@RRXURWU
;46    2.000E-03 FdRq@PaIU@@R@@RaIUaIU
;46    3.000E-03 UdRREPdWT@@R@@RtSTdWT
;46    3.173E-03 uYRbIPDET@@R@@RTATDET
;46 L3 3.173E-03 uYRbIPqEU@@R@@RqFUqEU
;46    3.251E-03 uRRrEPaHU@@R@@RaHUaHU
;46    3.330E-03 eVRBQPaAU@@R@@RaBUaAU
;46 L2 3.330E-03 eVRBQPaVU@@R@@RaVUaVU
;46    3.465E-03 UTRRRPQQU@@R@@RQRUQQU
;46    3.604E-03 ESRbRPqGU@@R@@RqHUqGU
;46 L1 3.604E-03 ESRbRPQXU@@R@@RQXUQXU
;46    4.000E-03 UBRRbPaBU@@R@@RaCUaBU
;46    5.000E-03 DRRcRPFgT@@R@@RVaTFgT
;46    6.000E-03 CfRdGPdGT@@R@@RtATdGT
;46    8.000E-03 CDRETPQiT@@R@@RBBTQiT
;46    1.000E-02 BXRvIPAHT@@R@@RQATAIT
;46    1.500E-02 aRRHHPSTS@@R@@RsQSSUS
;46    2.000E-02 QBRYIPQXS@@R@@RqPSQYS
;46    2.435E-02 XRQIhPIHR@@R@@RA@SYHR
;46 K  2.435E-02 XRQIhPEaS@@R@@RU`SEbS
;46    3.000E-02 fHQAEQsIS@@R@@RCVSCPS
;46    4.000E-02 T@QQBQQVS@@R@@RaQSQWS
;46    5.000E-02 BiQQFQHTR@@R@@RHeRXVR
;46    6.000E-02 RDQQGQEGR@@R@@REPRUHR
;46    8.000E-02 qAQQGQbCR@@R@@RBXRrER
;46    1.000E-01 HcPQEQQGR@@R@@RqHRaIR
;46    1.500E-01 dIPAHQcPQ@@R@@RUBQdYQ
;46    2.000E-01 RRPAAQQVQ@@R@@RBcQRXQ
;46    3.000E-01 QGPXgPTcP@@R@@RQQQqIQ
;46    4.000E-01 vROXAPbEP@@R@@RQ@QADQ
;46    5.000E-01 tEOGTPaFP@@R@@RYCPxPP
;46    6.000E-01 CDOV`PHBO@@R@@RHAPwQP
;46    8.000E-01 qSOFHPTDO@@R@@RfWPVPP
;46    1.000E+00 QAOEXPRXO@@R@@REePuTP
;46    1.022E+00 AFOERPBUO@@R@@RuWPeWP
;46    1.250E+00 WCNTaPaVOQTN@@RUFPEIP
;46    1.500E+00 TfNDVPQIOg@N@@RtPPdUP
;46    2.000E+00 rYNCaPgDNRQO@@RTFPTCP
;46    2.044E+00 bWNsVPViNbYO@@RTCPT@P
;46    3.000E+00 aDNC@PCfNVROAEMsPPcYP
;46    4.000E+00 ViMRPPRWNAAPdHMSUPSUP
;46    5.000E+00 DXMRFPQ`NqBPXRMSRPSQP
;46    6.000E+00 SAMQaPQPNQYPqANSTPSSP
;46    7.000E+00 bIMqRPaDNAdPqVNSYPSXP
;46    8.000E+00 qUMQVPAENBEPb@NcUPcUP
;46    9.000E+00 qHMATPYEMbEPbRNsRPsRP
;46    1.000E+01 QBMqCPHHMBSPCBNC`PC`P
;46    1.100E+01 iFLaDPgCMbPPCPNChPChP
;46    1.200E+01 wXLQFPVTMrUPsUNSfPSfP
;46    1.300E+01 fSLQ@PUhMBiPDINDDPDDP
;46    1.400E+01 uRLADPEYMCBPDQNTAPTAP
;46    1.500E+01 ThLIfOEHMSDPtQNTHPTHP
;46    1.600E+01 tGLyHOtSMcFPE@NdEPdEP
;46    1.800E+01 CVLXXOTEMCVPUSNtHPtHP
;46    2.000E+01 B`LWaOsPMcUPFBNTPPTPP
;46    2.200E+01 rALwEOsDMCaPFVNdRPdRP
;46    2.400E+01 QdLFfOCDMSgPFfNtRPtRP
;46    2.600E+01 aVLFUOrYMT@PgDNDbPDbP
;46    2.800E+01 ASLFHORWMdCPWYNTbPTbP
;46    3.000E+01 aDLuVOrIMtEPWbNEAPEAP
;46    4.000E+01 G@KTXOqWMDbPiFNuHPuHP
;46    5.000E+01 DXKCbOAPMUHPACOeVPeVP
;46    6.000E+01 SAKcIOQFMEUPQAOEiPEiP
;46    8.000E+01 qUKbPOhRLEfPaCOfDPfDP
;46    1.000E+02 QBKREOFfLVEPqBOFYPFYP
;46    1.500E+02 ThJQSOTTLfPPAWOV`PV`P
;46    2.000E+02 B`Ja@OCPLFhPQVOWEPWEP
;46    3.000E+02 aDJXSNbFLg@PaWOGUPGUP
;46    4.000E+02 G@IfXNaYLwHPqTOgRPgRP
;46    5.000E+02 DXIUSNqELWPPqXOwSPwSP
;46    6.000E+02 SAItSNQBLWYPAbOGbPGbP
;46    8.000E+02 qUIsPNHSKwPPAfOWcPWcP
;46    1.000E+03 QBICDNvTKwXPAiOH@PH@P
;46    1.500E+03 ThHRBNDYKGhPQdOX@PX@P
;46    2.000E+03 B`HaTNsGKWePQgOXFPXFP
;46    3.000E+03 aDHQDNbDKHAPQiOhBPhBP
;46    4.000E+03 G@GHbMaXKHDPBAOhEPhEP
;46    5.000E+03 DXGgBMqEKHFPBBOhGPhGP
;46    6.000E+03 SAGVBMQBKHHPBCOhHPhHP
;46    8.000E+03 qUGtQMHQJX@PBDOxAPxAP
;46    1.000E+04 QBGCeMvSJXAPBDOxBPxBP
;46    1.500E+04 ThFbVMDYJXCPBEOxCPxCP
;46    2.000E+04 B`FBDMsFJXCPBEOxDPxDP
;46    3.000E+04 aDFAQMbDJXDPBFOxEPxEP
;46    4.000E+04 G@EAHMaXJXEPBFOxFPxFP
;46    5.000E+04 DXEHbLqEJXEPBFOxFPxFP
;46    6.000E+04 SAEGULQBJXEPBFOxFPxFP
;46    8.000E+04 qUEuRLHQIXEPBFOxFPxFP
;46    1.000E+05 QBEdULvSIXFPBGOxGPxGP
;==== ELEMENT  47
;47    1.000E-03 GcRThOGCU@@R@@RGDUGCU
;47    1.500E-03 GURYGOrXU@@R@@RrYUrXU
;47    2.000E-03 GBRqEPqIU@@R@@RAPUqIU
;47    3.000E-03 VARRGPEGT@@R@@RUDTEHT
;47    3.351E-03 EaRBUPCcT@@R@@RCiTCcT
;47 L3 3.351E-03 EaRBUPaGU@@R@@RaGUaGU
;47    3.436E-03 uTRRQPQIU@@R@@Ra@UQIU
;47    3.524E-03 eWRRXPQBU@@R@@RQCUQBU
;47 L2 3.524E-03 eWRRXPQTU@@R@@RQUUQTU
;47    3.662E-03 UURbXPAPU@@R@@RAQUAPU
;47    3.806E-03 ETRrYPaHU@@R@@RaHUaHU
;47 L1 3.806E-03 ETRrYPAVU@@R@@RAWUAVU
;47    4.000E-03 eHRRcPq@U@@R@@RqAUq@U
;47    5.000E-03 TXRcRPwDT@@R@@RwITwDT
;47    6.000E-03 D@RdGPTWT@@R@@RdQTTWT
;47    8.000E-03 SDRERPRCT@@R@@RRFTRCT
;47    1.000E-02 RVRvIPQGT@@R@@RQITQGT
;47    1.500E-02 aXRX@PCbS@@R@@RD@SCcS
;47    2.000E-02 QFRiBPqQS@@R@@RAdSqRS
;47    2.551E-02 hGQAAQhPR@@R@@RYSRxPR
;47 K  2.551E-02 hGQAAQEUS@@R@@RUTSEVS
;47    3.000E-02 VTQAFQSYS@@R@@RcWScPS
;47    4.000E-02 dGQQCQaWS@@R@@RqRSaXS
;47    5.000E-02 CBQQFQICR@@R@@RIURYDR
;47    6.000E-02 bCQQHQERR@@R@@RuWRUTR
;47    8.000E-02 qFQQHQBPR@@R@@RbURRQR
;47    1.000E-01 iCPQFQaFR@@R@@RAWRqHR
;47    1.500E-01 DXPAIQCiQ@@R@@RESQThQ
;47    2.000E-01 bTPABQaYQ@@R@@RRgQrQQ
;47    3.000E-01 aBPIDPuDP@@R@@RQVQATQ
;47    4.000E-01 GCOXGPBTP@@R@@RQCQAFQ
;47    5.000E-01 TVOWPPqGP@@R@@RyBPHgP
;47    6.000E-01 SIOVfPxRO@@R@@RXEPGcP
;47    8.000E-01 AaOVDPTPO@@R@@RvWPVYP
;47    1.000E+00 QFOURPBaO@@R@@RUbPE`P
;47    1.022E+00 QAOEWPbWO@@R@@REdPuSP
;47    1.250E+00 GXNTdPA`OaPN@@ReBPUDP
;47    1.500E+00 e@NTPPaIOWPN@@RtUPtPP
;47    2.000E+00 RcNCdPGhNbPO@@RdAPTHP
;47    2.044E+00 B`NsYPgPNrXO@@RTHPTEP
;47    3.000E+00 q@NCBPTINvTOAFMsUPsTP
;47    4.000E+00 wDMRRPrYNADPtAMcQPcPP
;47    5.000E+00 tPMRHPBGNqFPXYMSXPSWP
;47    6.000E+00 cFMQcPaSNaTPqBNcPPcPP
;47    7.000E+00 BPMqSPqENAiPqXNcUPcUP
;47    8.000E+00 AdMQXPQDNRAPbBNsRPsRP
;47    9.000E+00 AUMAUPYcMrAPbTNC`PC`P
;47    1.000E+01 QGMqDPxWMRPPCDNChPChP
;47    1.100E+01 yQLaEPGdMbWPCRNSgPSfP
;47    1.200E+01 XFLQGPW@MBcPsXNDEPDEP
;47    1.300E+01 VeLQAPFXMRgPTBNTCPTCP
;47    1.400E+01 UiLAEPUeMSAPDTNd@Pd@P
;47    1.500E+01 eBLYcOUQMcCPtUNdHPdHP
;47    1.600E+01 TYLIVOUCMsEPEDNtEPtEP
;47    1.800E+01 cRLhUOTPMSVPUWNDXPDXP
;47    2.000E+01 RdLWgODAMsUPFFNdQPdQP
;47    2.200E+01 BSLGQOcQMSbPVPNtSPtSP
;47    2.400E+01 BDLVbOcIMDHPVbNDdPDdP
;47    2.600E+01 qTLVPOCBMdBPw@NTdPTdP
;47    2.800E+01 QPLVCOrYMtEPgTNEDPEDP
;47    3.000E+01 qALEaORYMDWPWgNUCPUCP
;47    4.000E+01 wDKdQOQaMTfPyBNUQPUQP
;47    5.000E+01 tPKCeOQRMuBPADOEaPEaP
;47    6.000E+01 cFKsBOaEMePPQBOFDPFDP
;47    8.000E+01 AdKbROyCLFBPaDOFPPFPP
;47    1.000E+02 QGKRGOGSLvAPqCOfVPfVP
;47    1.500E+02 eBJQUOTbLvXPAXOGIPGIP
;47    2.000E+02 RdJaAOcXLGFPQWOwDPwDP
;47    3.000E+02 qAJXYNBTLwIPaXOgUPgUP
;47    4.000E+02 wDIvSNAcLWXPqUOGbPGbP
;47    5.000E+02 tPIUWNAVLwPPqYOWcPWcP
;47    6.000E+02 cFItWNaBLwYPAcOHBPHBP
;47    8.000E+02 AdIsSNYCKWaPAhOXCPXCP
;47    1.000E+03 QGICGNw@KWhPQaOh@Ph@P
;47    1.500E+03 eBHRDNDfKX@PQeOxAPxAP
;47    2.000E+03 RdHaVNcUKXEPQhOxGPxGP
;47    3.000E+03 qAHQENBSKhBPBAOHSPHSP
;47    4.000E+03 wDGHiMAbKhEPBBOHVPHVP
;47    5.000E+03 tPGgGMAVKhGPBCOHXPHXP
;47    6.000E+03 cFGVFMaAKhIPBDOXPPXPP
;47    8.000E+03 AdGtUMYAJxAPBEOXRPXRP
;47    1.000E+04 QGGChMgIJxBPBFOXSPXSP
;47    1.500E+04 eBFbXMDfJxDPBFOXUPXUP
;47    2.000E+04 RdFBFMcTJxEPBGOXVPXVP
;47    3.000E+04 qAFARMBSJxFPBGOXWPXWP
;47    4.000E+04 wDEAIMAbJxFPBHOXWPXWP
;47    5.000E+04 tPEHhLAVJxGPBHOXXPXXP
;47    6.000E+04 cFEWQLaAJxGPBHOXXPXXP
;47    8.000E+04 AdEuVLYAIxGPBHOXXPXXP
;47    1.000E+05 QGEdYLgIIxGPBHOXXPXXP
;==== ELEMENT  48
;48    1.000E-03 GcRuHOwDU@@R@@RwEUwDU
;48    1.500E-03 GTRiYORbU@@R@@RRcURbU
;48    2.000E-03 G@RqHPAWU@@R@@RAWUAWU
;48    3.000E-03 VARRGPuET@@R@@REQTuET
;48    3.538E-03 eVRRVPSRT@@R@@RSXTSRT
;48 L3 3.538E-03 eVRRVPQEU@@R@@RQEUQEU
;48    3.631E-03 UXRbSPAHU@@R@@RAHUAHU
;48    3.727E-03 UQRrPPAAU@@R@@RAAUAAU
;48 L2 3.727E-03 UQRrPPqHU@@R@@RqIUqHU
;48    4.000E-03 eIRBiPQFU@@R@@RQGUQFU
;48    4.018E-03 eHRR`PQEU@@R@@RQFUQEU
;48 L1 4.018E-03 eHRR`PqBU@@R@@RqCUqBU
;48    5.000E-03 TYRSUPgTT@@R@@RgYTgTT
;48    6.000E-03 DARTGPtUT@@R@@RtYTtUT
;48    8.000E-03 SEReHPbBT@@R@@RbETbBT
;48    1.000E-02 RWRfAPaBT@@R@@RaDTaBT
;48    1.500E-02 aYRW`PD@S@@R@@RTHSDAS
;48    2.000E-02 QHRXiPqYS@@R@@RQbSA`S
;48    2.671E-02 GbQYgPWcR@@R@@RHaRHCR
;48 K  2.671E-02 GbQYgPThS@@R@@REFSTiS
;48    3.000E-02 fQQACQcYS@@R@@RsVSsPS
;48    4.000E-02 tBQQ@QqRS@@R@@RqXSqSS
;48    5.000E-02 CEQQDQyFR@@R@@RyXRIWR
;48    6.000E-02 bFQQEQeTR@@R@@RUhRuUR
;48    8.000E-02 qHQQEQRPR@@R@@RrURbQR
;48    1.000E-01 yFPQCQqBR@@R@@RQRRASR
;48    1.500E-01 TUPAGQDGQ@@R@@RUYQUDQ
;48    2.000E-01 bXPYhPqWQ@@R@@RCDQrWQ
;48    3.000E-01 aDPHePeQP@@R@@RQWQAUQ
;48    4.000E-01 WEOH@PRWP@@R@@RQCQAFQ
;48    5.000E-01 dSOwDPATP@@R@@RiEPxYP
;48    6.000E-01 cDOFbPi@O@@R@@RHFPwTP
;48    8.000E-01 AdOFAPtUO@@R@@RfWPFYP
;48    1.000E+00 QHOEQPRgO@@R@@REcPuQP
;48    1.022E+00 QCOuFPBbO@@R@@RuUPeTP
;48    1.250E+00 gPNDePQ`OaSN@@RUCPEEP
;48    1.500E+00 eINDQPqFOgPN@@RdWPdRP
;48    2.000E+00 RhNsVPxANbRO@@RTDPTAP
;48    2.044E+00 BeNsRPHCNBaO@@RTAPDHP
;48    3.000E+00 qCNRfPDRNvWOADMsPPcXP
;48    4.000E+00 GWMBWPRdNAEPdCMSVPSUP
;48    5.000E+00 tXMRDPRHNqGPHRMSTPSSP
;48    6.000E+00 sBMAiPqRNaTPaINSVPSVP
;48    7.000E+00 BTMqPPARNAiPqTNcRPcRP
;48    8.000E+00 AgMQTPa@NRAPRGNcYPcYP
;48    9.000E+00 AXMARPAENrAPRYNsWPsWP
;48    1.000E+01 a@MqBPiCMRPPRhNCePCeP
;48    1.100E+01 IhLaCPhFMbWPsENSdPSdP
;48    1.200E+01 x@LQEPGWMBbPsQNDBPDBP
;48    1.300E+01 GGLAHPFaMRgPDDNT@PT@P
;48    1.400E+01 V@LACPfGMS@PtENTHPTHP
;48    1.500E+01 uALySOE`McCPdUNdEPdEP
;48    1.600E+01 dWLiGOuIMsDPTcNtBPtBP
;48    1.800E+01 cYLHXOtSMSUPEVNDVPDVP
;48    2.000E+01 RiLGbOdBMsTPUdNTYPTYP
;48    2.200E+01 BWLgFOC`MSaPvHNtQPtQP
;48    2.400E+01 BGLvXOCVMDGPvWNDbPDbP
;48    2.600E+01 qWLvGOSHMdAPWENTbPTbP
;48    2.800E+01 QRLFAORcMtDPGYNEBPEBP
;48    3.000E+01 qCLeYOrSMDVPGaNUAPUAP
;48    4.000E+01 GWKTROBAMTePYCNEYPEYP
;48    5.000E+01 tXKsWOQYMuAPAAOuYPuYP
;48    6.000E+01 sBKcEOqBMUYPAIOFBPFBP
;48    8.000E+01 AgKRVOIaLFAPaAOvHPvHP
;48    1.000E+02 a@KRCOGaLv@Pq@OfTPfTP
;48    1.500E+02 uAJQROUGLvWPATOGFPGFP
;48    2.000E+02 RiJQIOCgLGEPQSOwBPwBP
;48    3.000E+02 qCJHRNRWLwGPaUOgRPgRP
;48    4.000E+02 GWIVYNQbLWVPqQOG`PG`P
;48    5.000E+02 tXIEVNQTLgXPqVOWaPWaP
;48    6.000E+02 sBIdXNaHLwWPqYOH@PH@P
;48    8.000E+02 AgIcUNYYKGiPAdOXAPXAP
;48    1.000E+03 a@IC@NgWKWgPAgOXHPXHP
;48    1.500E+03 uAHR@NUAKHGPQaOhIPhIP
;48    2.000E+03 RiHaRNCcKXDPQdOxEPxEP
;48    3.000E+03 qCHQCNRUKh@PQfOHQPHQP
;48    4.000E+03 GWGxRMQbKhCPQhOHTPHTP
;48    5.000E+03 tXGWCMQSKhFPQiOHVPHVP
;48    6.000E+03 sBGFDMaHKhGPB@OHXPHXP
;48    8.000E+03 AgGdUMYWJhIPBAOXPPXPP
;48    1.000E+04 a@GC`MgVJx@PBAOXQPXQP
;48    1.500E+04 uAFbSMUAJxBPBBOXRPXRP
;48    2.000E+04 RiFBBMCcJxCPBCOXTPXTP
;48    3.000E+04 qCFqIMRUJxDPBCOXUPXUP
;48    4.000E+04 GWEAGMQaJxEPBCOXUPXUP
;48    5.000E+04 tXExQLQSJxEPBDOXUPXUP
;48    6.000E+04 sBEwFLaHJxEPBDOXVPXVP
;48    8.000E+04 AgEeTLYWIxEPBDOXVPXVP
;48    1.000E+05 a@ETYLgVIxFPBDOXVPXVP
;==== ELEMENT  49
;49    1.000E-03 WeREUOG`U@@R@@RGaUG`U
;49    1.500E-03 WTRIgOSBU@@R@@RSCUSBU
;49    2.000E-03 GIRARPQWU@@R@@RQXUQWU
;49    3.000E-03 VIRb@PuUT@@R@@REaTuUT
;49    3.730E-03 ePRrSPs@T@@R@@RsFTs@T
;49 L3 3.730E-03 ePRrSPADU@@R@@RAEUADU
;49    3.833E-03 URRB`PIaT@@R@@RIgTIbT
;49    3.938E-03 ESRBgPiFT@@R@@RyATiFT
;49 L2 3.938E-03 ESRBgPaEU@@R@@RaEUaEU
;49    4.000E-03 uIRRbPaCU@@R@@RaCUaCU
;49    4.238E-03 eARCHPAFU@@R@@RAGUAFU
;49 L3 4.238E-03 eARCHPaBU@@R@@RaBUaBU
;49    5.000E-03 dYRSWPHIT@@R@@RXCTHIT
;49    6.000E-03 TARTGPECT@@R@@REGTECT
;49    8.000E-03 cCReDPrFT@@R@@RrITrFT
;49    1.000E-02 bSRVHPaIT@@R@@RqBTq@T
;49    1.500E-02 qSRGfPdFS@@R@@RDUSdGS
;49    2.000E-02 aARXePQaS@@R@@RBDSQbS
;49    2.794E-02 WUQAAQGVR@@R@@RxARWVR
;49 K  2.794E-02 WUQAAQdUS@@R@@RtSSdVS
;49    3.000E-02 FaQACQCgS@@R@@RSeSChS
;49    4.000E-02 DTQQ@QAbS@@R@@RAgSAcS
;49    5.000E-02 SEQQCQIhR@@R@@RACSYiR
;49    6.000E-02 rDQQEQUfR@@R@@RvARFGR
;49    8.000E-02 ASQQEQbUR@@R@@RRaRrVR
;49    1.000E-01 iWPQCQAPR@@R@@RaQRQQR
;49    1.500E-01 tPPAFQtDQ@@R@@REhQEQQ
;49    2.000E-01 rWPYfPAiQ@@R@@RSGQBiQ
;49    3.000E-01 aIPHdPFBP@@R@@RaQQAYQ
;49    4.000E-01 GQOWiPrVP@@R@@RQEQAGQ
;49    5.000E-01 D`OwDPQUP@@R@@RyGPHiP
;49    6.000E-01 sFOFaPIiO@@R@@RXDPG`P
;49    8.000E-01 QaOFAPUAO@@R@@RvQPVRP
;49    1.000E+00 aCOEQPSIO@@R@@REePuSP
;49    1.022E+00 QHOuEPCCO@@R@@RuXPeVP
;49    1.250E+00 GhNDdPBEOaYN@@RUDPEFP
;49    1.500E+00 EXNDPPAWOGeN@@RdXPdSP
;49    2.000E+00 CINsVPXdNbYO@@RTEPTBP
;49    2.044E+00 RfNsRPhRNBhO@@RTBPDIP
;49    3.000E+00 qHNRfPtUNVaOADMsQPsPP
;49    4.000E+00 wTMBWPSENAGPdCMSXPSWP
;49    5.000E+00 TeMRCPrDNqIPHQMSVPSVP
;49    6.000E+00 CTMAiPAdNaWPaINcPPSYP
;49    7.000E+00 RSMqPPQRNQcPqTNcVPcUP
;49    8.000E+00 QdMQTPaINREPRGNsSPsSP
;49    9.000E+00 QSMARPQBNrFPRYNCaPCaP
;49    1.000E+01 aDMqAPIiMRTPRhNS`PS`P
;49    1.100E+01 ABMaCPHeMrRPsENSiPShP
;49    1.200E+01 hQLQEPH@MBhPsPNDGPDGP
;49    1.300E+01 wCLAHPw@MCBPDCNTFPTEP
;49    1.400E+01 vBLABPvQMSFPtENdDPdCP
;49    1.500E+01 UQLySOfAMcIPdUNtAPtAP
;49    1.600E+01 DdLiGOuXMCPPTcNtIPtHP
;49    1.800E+01 CbLHWOEGMcRPEUNTRPTRP
;49    2.000E+01 S@LGaOTRMCaPUcNdUPdUP
;49    2.200E+01 RVLgEODGMShPvGNtXPtXP
;49    2.400E+01 RELvXOsQMTDPvWNDiPDiP
;49    2.600E+01 AcLvGOCPMdIPWCNE@PE@P
;49    2.800E+01 QXLFAOSDMDRPGXNU@PU@P
;49    3.000E+01 qHLeYORbMTTPG`NUIPUIP
;49    4.000E+01 wUKTROREMECPYBNUXPUXP
;49    5.000E+01 TfKsWOqQMEPPAAOEhPEhP
;49    6.000E+01 CTKcEOAQMeYPAIOVBPVBP
;49    8.000E+01 QdKRVOAEMVAPaAOFYPFYP
;49    1.000E+02 aDKRCOxGLFQPq@OvUPvUP
;49    1.500E+02 UQJQQOUTLFiPATOWHPWHP
;49    2.000E+02 S@JQIOTDLWGPQSOGTPGTP
;49    3.000E+02 qHJHRNrULWPPaTOwUPwUP
;49    4.000E+02 wUIVYNBFLgYPqQOWcPWcP
;49    5.000E+02 TfIEVNaULGaPqUOHDPHDP
;49    6.000E+02 CTIdWNqGLW`PqYOXCPXCP
;49    8.000E+02 QdIcUNACLHBPAcOhDPhDP
;49    1.000E+03 aDIC@NhAKX@PAfOxBPxBP
;49    1.500E+03 UQHR@NEWKhAPQaOHSPHSP
;49    2.000E+03 S@HaRNT@KhHPQcOHYPHYP
;49    3.000E+03 qHHQCNrSKxDPQfOXUPXUP
;49    4.000E+03 wUGxQMBEKxHPQhOXXPXXP
;49    5.000E+03 TfGWBMaTKHPPQiOhPPhPP
;49    6.000E+03 CTGFDMqGKHQPQiOhRPhRP
;49    8.000E+03 QdGdUMABKHSPB@OhTPhTP
;49    1.000E+04 aDGC`Mh@JHUPBAOhUPhUP
;49    1.500E+04 UQFbSMEWJHWPBBOhWPhWP
;49    2.000E+04 S@FBBMT@JHXPBBOhXPhXP
;49    3.000E+04 qHFqIMrSJHYPBCOhYPhYP
;49    4.000E+04 wUEAGMBEJHYPBCOxPPxPP
;49    5.000E+04 TfExPLaTJHYPBCOxPPxPP
;49    6.000E+04 CTEwELqGJXPPBCOxPPxPP
;49    8.000E+04 QdEeTLABJXPPBCOxPPxPP
;49    1.000E+05 aDETYLh@IXPPBDOxQPxQP
;==== ELEMENT  50
;50    1.000E-03 H@ReGOXEU@@R@@RXFUXEU
;50    1.500E-03 WWRiXOcIU@@R@@Rs@UcIU
;50    2.000E-03 W@RAQPaVU@@R@@RaVUaVU
;50    3.000E-03 f@Rb@PFHT@@R@@RVDTFHT
;50    3.929E-03 EVRBfPCFT@@R@@RSATCFT
;50 L3 3.929E-03 EVRBfPi@T@@R@@RiFTi@T
;50    4.000E-03 EQRRaPyDT@@R@@RyITyDT
;50    4.156E-03 u@RCAPHRT@@R@@RHWTHRT
;50 L2 4.156E-03 u@RCAPQDU@@R@@RQDUQDU
;50    4.308E-03 UIRSAPAEU@@R@@RAEUAEU
;50    4.465E-03 EHRcAPiVT@@R@@RyQTiVT
;50 L3 4.465E-03 EHRcAPQAU@@R@@RQBUQAU
;50    5.000E-03 tRRSTPHRT@@R@@RHWTHRT
;50    6.000E-03 TDRTBPeET@@R@@ReITeET
;50    8.000E-03 cFRUFPBWT@@R@@RRPTBWT
;50    1.000E-02 bURFGPqFT@@R@@RqHTqFT
;50    1.500E-02 qTRwSPDXS@@R@@RdVSDYS
;50    2.000E-02 aCRHaPBAS@@R@@RRESBBS
;50    2.920E-02 WIQA@QVdR@@R@@RwVRGDR
;50 K  2.920E-02 WIQA@QdHS@@R@@RtFSdIS
;50    3.000E-02 VaQAAQDDS@@R@@RTBSDES
;50    4.000E-02 TQQAHQAiS@@R@@RQdSQ`S
;50    5.000E-02 c@QQAQACS@@R@@RAGSADS
;50    6.000E-02 rHQQCQfAR@@R@@RVWRvCR
;50    8.000E-02 AVQQCQrWR@@R@@RCCRBhR
;50    1.000E-01 IfPQBQAWR@@R@@RaXRQXR
;50    1.500E-01 tYPAEQTVQ@@R@@RFIQeQQ
;50    2.000E-01 BcPIbPB@Q@@R@@RcFQRhQ
;50    3.000E-01 qBPxRPvEP@@R@@RaTQQQQ
;50    4.000E-01 WVOGiPRaP@@R@@RQFQAHQ
;50    5.000E-01 TaOgDPaTP@@R@@RyGPHhP
;50    6.000E-01 CTOvRPAEP@@R@@RXAPwWP
;50    8.000E-01 QeOUcPERO@@R@@RfVPFWP
;50    1.000E+00 aFOuDPsHO@@R@@RE`PeWP
;50    1.022E+00 a@OeHPcBO@@R@@RuRPePP
;50    1.250E+00 HFNtXPRGOqRN@@REIPEAP
;50    1.500E+00 eQNtEPQUOH@N@@RdTPTXP
;50    2.000E+00 SFNsQPIWNrSO@@RTAPDHP
;50    2.044E+00 CCNcWPYDNRbO@@RDHPDEP
;50    3.000E+00 AQNRbPECNViOABMcYPcWP
;50    4.000E+00 WbMBTPsDNAHPTGMSVPSUP
;50    5.000E+00 EGMRAPBWNAPPx@MSUPSTP
;50    6.000E+00 SRMAfPQeNaYPaGNSXPSXP
;50    7.000E+00 RYMaWPaQNQdPqRNcUPcTP
;50    8.000E+00 QhMQRPqFNRFPRDNsRPsRP
;50    9.000E+00 QWMAPPQHNrGPRUNCaPCaP
;50    1.000E+01 aGMq@PAENRVPRdNS`PCiP
;50    1.100E+01 AEMaAPyEMrSPsANShPShP
;50    1.200E+01 H`LQCPHVMBiPcUNDGPDGP
;50    1.300E+01 WPLAGPwRMCDPShNTFPTFP
;50    1.400E+01 FWLAAPGIMSHPdINdDPdDP
;50    1.500E+01 eTLiPOVVMs@PTXNtBPtAP
;50    1.600E+01 TeLYEOVAMCRPDfNtIPtIP
;50    1.800E+01 SaLxFOuFMcSPuHNTSPTSP
;50    2.000E+01 SGLwQOtWMCcPEeNdVPdVP
;50    2.200E+01 bRLWFOt@MD@PfHNtYPtXP
;50    2.400E+01 b@LfYOSaMTFPfXNT`PT`P
;50    2.600E+01 AhLfHOSYMtAPGDNEAPEAP
;50    2.800E+01 aRLUcOsBMDTPwHNUAPUAP
;50    3.000E+01 AQLeQOCHMTVPgYNe@Pe@P
;50    4.000E+01 WbKDVObGMEFPXiNePPePP
;50    5.000E+01 EGKsROA`MERPYhNU`PU`P
;50    6.000E+01 SRKcAOAYMuQPAHOVDPVDP
;50    8.000E+01 QhKRSOQAMVCPQIOVQPVQP
;50    1.000E+02 aGKR@OHcLFTPaHOvXPvXP
;50    1.500E+02 eTJQPOEdLVaPAROgAPgAP
;50    2.000E+02 SGJQGOtGLg@PQQOGWPGWP
;50    3.000E+02 AQJxANR`LWSPaROwXPwXP
;50    4.000E+02 WbIVQNRGLwRPaXOWePWeP
;50    5.000E+02 EGIuINqTLGePqSOHGPHGP
;50    6.000E+02 SRIdQNAULWdPqVOXFPXFP
;50    8.000E+02 QhIcPNAHLHFPAaOhHPhHP
;50    1.000E+03 aGIRfNhWKXDPAdOxEPxEP
;50    1.500E+03 eTHBGNuWKhEPAhOHVPHVP
;50    2.000E+03 SGHaPNtCKxAPQaOXRPXRP
;50    3.000E+03 AQHQANBiKxHPQcOXXPXXP
;50    4.000E+03 WbGhPMRFKHQPQeOhQPhQP
;50    5.000E+03 EGGGCMqSKHSPQfOhSPhSP
;50    6.000E+03 SRGUfMATKHUPQgOhUPhUP
;50    8.000E+03 QhGTYMAHKHWPQhOhWPhWP
;50    1.000E+04 aGGsUMhUJHXPQhOhXPhXP
;50    1.500E+04 eTFRYMuWJXPPQiOxPPxPP
;50    2.000E+04 SGFQiMtCJXQPQiOxQPxQP
;50    3.000E+04 AQFqGMBhJXRPB@OxRPxRP
;50    4.000E+04 WbEAEMRFJXSPB@OxSPxSP
;50    5.000E+04 EGEXYLqSJXSPB@OxSPxSP
;50    6.000E+04 SREgFLATJXSPB@OxSPxSP
;50    8.000E+04 QhEUWLAHJXSPB@OxSPxSP
;50    1.000E+05 aGETSLhUIXTPBAOxTPxTP
;==== ELEMENT  51
;51    1.000E-03 XARECOXXU@@R@@RXYUXXU
;51    1.500E-03 gWRyBOCXU@@R@@RCYUCXU
;51    2.000E-03 WIRqGPqVU@@R@@RqWUqVU
;51    3.000E-03 fGRRHPFWT@@R@@RVSTFWT
;51    4.000E-03 EWRR`PSAT@@R@@RSGTSAT
;51    4.132E-03 uGRRiPBfT@@R@@RRbTBfT
;51 L3 4.132E-03 uGRRiPhTT@@R@@RxPThTT
;51    4.254E-03 eHRCGPXGT@@R@@RhBTXGT
;51    4.380E-03 UIRSEPwRT@@R@@RwWTwRT
;51 L2 4.380E-03 UIRSEPADU@@R@@RAEUADU
;51    4.537E-03 EIRcEPiTT@@R@@RiYTiTT
;51    4.698E-03 ThRsEPHiT@@R@@RXdTHiT
;51 L3 4.698E-03 ThRsEPABU@@R@@RACUABU
;51    5.000E-03 tXRSSPH`T@@R@@RHeTH`T
;51    6.000E-03 d@RT@PUST@@R@@RUWTUST
;51    8.000E-03 sARUBPbPT@@R@@RbSTbPT
;51    1.000E-02 bYRF@PAST@@R@@RAVTAST
;51    1.500E-02 qWRgVPtTS@@R@@RTbStUS
;51    2.000E-02 aERxSPRCS@@R@@RbGSRDS
;51    3.000E-02 GHQA@QFbR@@R@@RgSRVbR
;51    3.049E-02 VaQA@QVQR@@R@@RwARfQR
;51 K  3.049E-02 VaQA@QSiS@@R@@RDGSD@S
;51    4.000E-02 dRQAGQQgS@@R@@RBCSQhS
;51    5.000E-02 cGQQ@QAHS@@R@@RQBSAIS
;51    6.000E-02 BTQQBQVRR@@R@@RFhRfSR
;51    8.000E-02 QPQQBQRaR@@R@@RSHRCCR
;51    1.000E-01 AAQQAQQUR@@R@@RqVRaVR
;51    1.500E-01 TbPADQDcQ@@R@@RvFQEgQ
;51    2.000E-01 RaPyVPRAQ@@R@@RsHQCIQ
;51    3.000E-01 qEPhWPvUP@@R@@RaXQQTQ
;51    4.000E-01 wYOGdPS@P@@R@@RQGQAIQ
;51    5.000E-01 EEOg@PqUP@@R@@RIUPXeP
;51    6.000E-01 STOfXPQBP@@R@@RXEPG`P
;51    8.000E-01 BAOEiPuXO@@R@@RfWPFWP
;51    1.000E+00 aIOuAPcQO@@R@@RE`PeWP
;51    1.022E+00 aDOeEPCSO@@R@@RuRPePP
;51    1.250E+00 x@NtUPrAOqWN@@REHPE@P
;51    1.500E+00 uXNtBPaVOhAN@@RdSPTWP
;51    2.000E+00 cFNcYPAAOrYO@@RT@PDGP
;51    2.044E+00 SBNcUPyTNRiO@@RDGPDDP
;51    3.000E+00 AUNRaPuFNWAOABMcYPcWP
;51    4.000E+00 XFMBSPSUNAIPTEMSWPSVP
;51    5.000E+00 eBMBIPbSNARPhFMSVPSUP
;51    6.000E+00 cSMAePBHNqQPaGNcPPSYP
;51    7.000E+00 bWMaVPqQNQfPqQNcVPcVP
;51    8.000E+00 BDMQRPAUNRIPRCNsTPsTP
;51    9.000E+00 aQMqIPaFNBPPRTNCcPCcP
;51    1.000E+01 qAMaIPQANRYPRbNSbPSbP
;51    1.100E+01 AHMa@PYeMrVPcINDAPDAP
;51    1.200E+01 IGLQCPXiMRcPcSNT@PT@P
;51    1.300E+01 wSLAFPhAMCHPSfNTIPTIP
;51    1.400E+01 fVLAAPWTMcBPdGNdGPdGP
;51    1.500E+01 EaLYUOVhMsDPTVNtEPtEP
;51    1.600E+01 U@LY@OFYMCVPDcNDSPDSP
;51    1.800E+01 DCLxAOeYMcXPuENTWPTWP
;51    2.000E+01 cGLgWOEGMCgPEaNtPPtPP
;51    2.200E+01 rPLWBOTWMDEPfDNDcPDcP
;51    2.400E+01 bGLfUOTFMdAPfSNTePTeP
;51    2.600E+01 QcLfEOCbMtFPViNEFPEFP
;51    2.800E+01 aWLU`OSRMDYPwCNUFPUFP
;51    3.000E+01 AULUXOcGMdQPgTNeEPeEP
;51    4.000E+01 XFKDSOBRMUAPXcNeUPeUP
;51    5.000E+01 eBKsPOQaMEYPYaNUfPUfP
;51    6.000E+01 cSKSIOQXMuXPAGOf@Pf@P
;51    8.000E+01 BDKRROQHMfAPQIOVXPVXP
;51    1.000E+02 qAKBIOyHLVQPaGOFePFeP
;51    1.500E+02 EaJAYOfALViPAQOgHPgHP
;51    2.000E+02 cGJQGOdTLgHPQPOWUPWUP
;51    3.000E+02 AUJhFNCHLgRPaQOGfPGfP
;51    4.000E+02 XFIFWNrALGaPaWOHDPHDP
;51    5.000E+02 eBIuFNAeLWdPqROXFPXFP
;51    6.000E+02 cSITYNQTLHCPqUOhEPhEP
;51    8.000E+02 BDISXNQELXEPqYOxGPxGP
;51    1.000E+03 qAIReNiAKhCPAbOHTPHTP
;51    1.500E+03 EaHBFNVCKxDPAgOXUPXUP
;51    2.000E+03 cGHQYNdPKHPPAiOhQPhQP
;51    3.000E+03 AUHQANCFKHWPQbOhXPhXP
;51    4.000E+03 XFGXUMr@KXQPQcOxQPxQP
;51    5.000E+03 eBGViMAdKXSPQdOxSPxSP
;51    6.000E+03 cSGUcMQSKXUPQeOxUPxUP
;51    8.000E+03 BDGTWMQEKXWPQfOxWPxWP
;51    1.000E+04 qAGsSMYIJXXPQgOxXPxXP
;51    1.500E+04 EaFRXMVCJhPPQgOH`PH`P
;51    2.000E+04 cGFQhMTYJhQPQhOHaPHaP
;51    3.000E+04 AUFqGMCFJhRPQhOHbPHbP
;51    4.000E+04 XFEAEMr@JhRPQiOHbPHbP
;51    5.000E+04 eBEXTLAdJhSPQiOHcPHcP
;51    6.000E+04 cSEgBLQSJhSPQiOHcPHcP
;51    8.000E+04 BDEUSLQEJhSPQiOHcPHcP
;51    1.000E+05 qAETPLYIIhTPQiOHdPHdP
;==== ELEMENT  52
;52    1.000E-03 HERtTOHRU@@R@@RHSUHRU
;52    1.003E-03 HERtWOxGU@@R@@RxHUxGU
;52    1.006E-03 HERtYOxBU@@R@@RxCUxBU
;52 M1 1.006E-03 HERtYOhWU@@R@@RhXUhWU
;52    1.500E-03 gQRXeOcPU@@R@@RcQUcPU
;52    2.000E-03 WBRqCPAcU@@R@@RAcUAcU
;52    3.000E-03 f@RRDPvST@@R@@RvYTvST
;52    4.000E-03 EPRBePcDT@@R@@Rs@TcDT
;52    4.341E-03 UFRCFPbST@@R@@RbXTbST
;52 L3 4.341E-03 UFRCFPGcT@@R@@RGhTGcT
;52    4.475E-03 EGRSEPwHT@@R@@RGSTwHT
;52    4.612E-03 ThRcCPVeT@@R@@RG@TVeT
;52 L2 4.612E-03 ThRcCPIPT@@R@@RIUTIPT
;52    4.773E-03 DhRsCPhXT@@R@@RxSThXT
;52    4.939E-03 tWRCSPHAT@@R@@RHFTHAT
;52 L1 4.939E-03 tWRCSPiET@@R@@RiITiET
;52    5.000E-03 tSRCVPXgT@@R@@RIATXgT
;52    6.000E-03 TGRDAPeXT@@R@@RuRTeXT
;52    8.000E-03 cIRThPbWT@@R@@RrPTbWT
;52    1.000E-02 bWREbPAWT@@R@@RQPTAWT
;52    1.500E-02 qVRGRPDiS@@R@@REHST`S
;52    2.000E-02 aERHVPbAS@@R@@RrDSbBS
;52    3.000E-02 GGQyPPGGR@@R@@RGhRWGR
;52    3.181E-02 FYQIdPUiR@@R@@RvTRFIR
;52 K  3.181E-02 FYQIdPcTS@@R@@RsRScUS
;52    4.000E-02 dQQACQBAS@@R@@RBFSBBS
;52    5.000E-02 cGQAGQQ@S@@R@@RQDSQAS
;52    6.000E-02 BTQAIQfYR@@R@@RGDRF`R
;52    8.000E-02 QPQAIQC@R@@R@@RcERSAR
;52    1.000E-01 AAQAHQQYR@@R@@RA`RqPR
;52    1.500E-01 TcPAAQTiQ@@R@@RFYQF@Q
;52    2.000E-01 RbPIYPRIQ@@R@@RCSQSDQ
;52    3.000E-01 qFPHRPG@P@@R@@RaXQQTQ
;52    4.000E-01 GcOgSPcBP@@R@@RQFQAIQ
;52    5.000E-01 EGOG@PAbP@@R@@RyCPHbP
;52    6.000E-01 SVOVPPQFP@@R@@RHBPgWP
;52    8.000E-01 BBOuSPFBO@@R@@RVTPvDP
;52    1.000E+00 q@OUFPsVO@@R@@ReWPUTP
;52    1.022E+00 aEOUAPSXO@@R@@RUYPEWP
;52    1.250E+00 xENdRPBQOqXN@@RTgPDhP
;52    1.500E+00 EaNdAPqSOhDN@@RTRPDVP
;52    2.000E+00 cHNSYPAEOrYO@@RDAPShP
;52    2.044E+00 SDNSUPAAORhO@@RShPSeP
;52    3.000E+00 AVNBcPUXNGGOIiLcQPSYP
;52    4.000E+00 hAMrFPsPNAIPDCMCYPCYP
;52    5.000E+00 eFMBDPrSNAQPHCMCYPCYP
;52    6.000E+00 cUMA`PRFNaYPaCNSSPSSP
;52    7.000E+00 bXMaRPqXNQePaVNcPPcPP
;52    8.000E+00 BEMAWPQQNRGPBGNcXPcXP
;52    9.000E+00 aRMqEPqANrHPBWNsWPsWP
;52    1.000E+01 qAMaFPQENRVPBdNCfPCfP
;52    1.100E+01 AIMQGPACNrTPc@NSePSeP
;52    1.200E+01 YCLQ@PyDMR`PSSNDDPDDP
;52    1.300E+01 wXLACPXRMCEPCeNTCPTCP
;52    1.400E+01 vQLyYOGcMSHPTENdAPdAP
;52    1.500E+01 EdLiIOgDMsAPDSNdIPdIP
;52    1.600E+01 UCLHeOvTMCSPtPNtGPtGP
;52    1.800E+01 DFLHHOUaMcTPe@NTQPTQP
;52    2.000E+01 cILGVOeGMCcPeUNdTPdTP
;52    2.200E+01 rRLVcOtUMDAPFGNtWPtWP
;52    2.400E+01 bHLFWOtBMTGPFUNDhPDhP
;52    2.600E+01 QdLFHOSfMtAPF`NTiPTiP
;52    2.800E+01 aXLuSOcVMDTPWCNEIPEIP
;52    3.000E+01 AVLESOCPMTWPGSNUIPUIP
;52    4.000E+01 hBKtAORQMEFPhXNUXPUXP
;52    5.000E+01 eFKcPOQiMESPiTNEiPEiP
;52    6.000E+01 cUKS@OaTMuRPADOVCPVCP
;52    8.000E+01 BEKBUOaBMVDPQEOVPPVPP
;52    1.000E+02 qAKBCOySLFTPaDOvWPvWP
;52    1.500E+02 EdJAUOFTLVaPqGOg@Pg@P
;52    2.000E+02 cIJQCODaLg@PAVOGVPGVP
;52    3.000E+02 AVJHDNc@LWSPQVOwWPwWP
;52    4.000E+02 hBIv@NBPLwSPaROWePWeP
;52    5.000E+02 eFIeANQaLGePaWOHGPHGP
;52    6.000E+02 cUIDVNQYLWdPqPOXFPXFP
;52    8.000E+02 BEICXNQILHFPqTOhGPhGP
;52    1.000E+03 qAIBgNYUKXDPqWOxEPxEP
;52    1.500E+03 EdHB@NvFKhEPAaOHUPHUP
;52    2.000E+03 cIHQUNtWKxAPAdOXQPXQP
;52    3.000E+03 AVHAHNSHKxHPAfOXWPXWP
;52    4.000E+03 hBGxBMrHKHQPAhOhQPhQP
;52    5.000E+03 eFGF`MQaKHTPAiOhSPhSP
;52    6.000E+03 cUGuWMQYKHUPQ`OhUPhUP
;52    8.000E+03 BEGDTMQIKHWPQaOhWPhWP
;52    1.000E+04 qAGcSMYSJHYPQaOhXPhXP
;52    1.500E+04 EdFRQMvFJXPPQbOxPPxPP
;52    2.000E+04 cIFQcMtWJXQPQbOxQPxQP
;52    3.000E+04 AVFqCMSHJXRPQcOxRPxRP
;52    4.000E+04 hBEABMrHJXSPQcOxRPxRP
;52    5.000E+04 eFExALQaJXSPQcOxSPxSP
;52    6.000E+04 cUEGBLQYJXSPQcOxSPxSP
;52    8.000E+04 BEEuHLQIJXTPQcOxSPxSP
;52    1.000E+05 qAEtHLYSIXTPQcOxSPxSP
;==== ELEMENT  53
;53    1.000E-03 HRRdYOIIU@@R@@RY@UIIU
;53    1.035E-03 xIRTgOHUU@@R@@RHVUHUU
;53    1.072E-03 xFReGOGfU@@R@@RGgUGfU
;53 M1 1.072E-03 xFReGOh@U@@R@@Rh@Uh@U
;53    1.500E-03 WfRXgOSaU@@R@@RSbUSaU
;53    2.000E-03 GURqDPQiU@@R@@RB@UQiU
;53    3.000E-03 FVRRIPwFT@@R@@RGRTwFT
;53    4.000E-03 eSRRcPSUT@@R@@RcQTSUT
;53    4.557E-03 eCRs@PRTT@@R@@RRYTRTT
;53 L3 4.557E-03 eCRs@PWPT@@R@@RWUTWPT
;53    4.702E-03 UCRsIPGCT@@R@@RGHTGCT
;53    4.852E-03 ECRCXPVYT@@R@@RfTTVYT
;53 L2 4.852E-03 ECRCXPHhT@@R@@RXcTHhT
;53    5.000E-03 TdRSWPxHT@@R@@RHSTxHT
;53    5.188E-03 DbRcXPgRT@@R@@RgVTgRT
;53 L1 5.188E-03 DbRcXPxYT@@R@@RHdTxYT
;53    6.000E-03 tFRTCPVCT@@R@@RVHTVCT
;53    8.000E-03 CURUAPBiT@@R@@RRbTBiT
;53    1.000E-02 B`RUfPaPT@@R@@RaSTaPT
;53    1.500E-02 AdRWXPuBS@@R@@RUQSuCS
;53    2.000E-02 qARhUPBPS@@R@@RRTSBQS
;53    3.000E-02 GVQYaPwRR@@R@@RXVRGbR
;53    3.317E-02 FSQABQEaR@@R@@RVURUaR
;53 K  3.317E-02 FSQABQSQS@@R@@RSXSSRS
;53    4.000E-02 DfQAFQRES@@R@@RbASRFS
;53    5.000E-02 CUQAIQQIS@@R@@RaCSa@S
;53    6.000E-02 RXQQAQgAR@@R@@RWXRwBR
;53    8.000E-02 QXQQBQcDR@@R@@RSQRsER
;53    1.000E-01 AGQQ@QqRR@@R@@RQdRAcR
;53    1.500E-01 eBPADQERQ@@R@@RVhQFVQ
;53    2.000E-01 CIPyQPrHQ@@R@@RcVQsEQ
;53    3.000E-01 ATPhSPgUP@@R@@RqWQaSQ
;53    4.000E-01 hIOGaPSSP@@R@@RaBQQCQ
;53    5.000E-01 uHOWHPQiP@@R@@RyPPYFP
;53    6.000E-01 sWOfVPaGP@@R@@RxAPWdP
;53    8.000E-01 RDOEgPfPO@@R@@RvUPVSP
;53    1.000E+00 qHOeIPTBO@@R@@REdPuPP
;53    1.022E+00 qBOeCPSbO@@R@@RuVPeSP
;53    1.250E+00 HfNtTPbTOAhN@@RUAPEBP
;53    1.500E+00 VFNtAPAiOxQN@@RdUPTYP
;53    2.000E+00 CXNcXPQEORdO@@RTBPDIP
;53    2.044E+00 sCNcTPQAOSDO@@RDIPDFP
;53    3.000E+00 QUNR`PVANGQOAAMsRPsPP
;53    4.000E+00 xQMBRPDENQDPTCMcQPcPP
;53    5.000E+00 UXMBIPRiNAXPhCMcQPcPP
;53    6.000E+00 CgMAePrFNqWPaFNcVPcUP
;53    7.000E+00 BeMaVPQdNBCPqPNsSPsSP
;53    8.000E+00 RHMQQPaUNbFPRBNCaPCaP
;53    9.000E+00 qRMqIPASNBXPRSNSaPSaP
;53    1.000E+01 qIMaIPaFNbWPRaND@PD@P
;53    1.100E+01 QEMa@PQCNBePcHNT@PT@P
;53    1.200E+01 iYLQCPABNCBPcRNTIPTIP
;53    1.300E+01 hELAFPyAMSGPSdNdHPdHP
;53    1.400E+01 WBLA@PXVMsBPdENtGPtGP
;53    1.500E+01 f@LYROWbMCUPTTNDUPDUP
;53    1.600E+01 EULIGOwFMSWPDbNTSPTSP
;53    1.800E+01 tALhIOFVMsYPuBNdXPdXP
;53    2.000E+01 CYLgTOuUMD@PuYNDbPDbP
;53    2.200E+01 BhLW@OUHMTHPfBNTePTeP
;53    2.400E+01 BRLfSOtRMtDPfQNEHPEHP
;53    2.600E+01 BFLfCOtCMDYPVgNUIPUIP
;53    2.800E+01 qXLEgOD@MdSPw@Nu@Pu@P
;53    3.000E+01 QULUVOsQMtVPgQNEPPEPP
;53    4.000E+01 xRKDROrTMeGPHiNEaPEaP
;53    5.000E+01 UXKcYORGMeVPIgNVCPVCP
;53    6.000E+01 CgKSHOA`MUfPAFOvHPvHP
;53    8.000E+01 RHKRQOqCMvIPQHOvVPvVP
;53    1.000E+02 qIKBHOAFMvQPaFOGDPGDP
;53    1.500E+02 f@JAXOGCLg@PAPOGYPGYP
;53    2.000E+02 CYJQFOeFLWPPAYOwWPwWP
;53    3.000E+02 QUJhDNCYLGePaPOHIPHIP
;53    4.000E+02 xRIFUNbRLHEPaVOhHPhHP
;53    5.000E+02 UXIuDNBILXHPqQOHPPHPP
;53    6.000E+02 CgITWNqTLhGPqTOHYPHYP
;53    8.000E+02 RHISWNq@LxIPqXOhQPhQP
;53    1.000E+03 qIIRdNADLHXPAaOhYPhYP
;53    1.500E+03 f@HBENVeKXYPAfOH`PH`P
;53    2.000E+03 CYHQYNeAKhVPAhOHfPHfP
;53    3.000E+03 QUHQ@NCWKxSPQaOXcPXcP
;53    4.000E+03 xRGXRMbPKxVPQbOXgPXgP
;53    5.000E+03 UXGVgMBHKxYPQcOXiPXiP
;53    6.000E+03 CgGUaMqSKH`PQdOI@PI@P
;53    8.000E+03 RHGTUMq@KHcPQeOICPICP
;53    1.000E+04 qIGsRMADKHdPQfOIDPIDP
;53    1.500E+04 f@FRWMVdJHfPQfOIFPIFP
;53    2.000E+04 CYFQhMe@JHgPQgOIGPIGP
;53    3.000E+04 QUFqFMCWJHhPQgOIHPIHP
;53    4.000E+04 xREAEMbPJHhPQhOIHPIHP
;53    5.000E+04 UXEXRLBHJHiPQhOIIPIIP
;53    6.000E+04 CgEg@LqSJHiPQhOIIPIIP
;53    8.000E+04 RHEURLq@JHiPQhOIIPIIP
;53    1.000E+05 qIEDYLADJHiPQhOIIPIIP
;==== ELEMENT  54
;54    1.000E-03 HVRDROIPU@@R@@RIQUIPU
;54    1.072E-03 xIRTgOXCU@@R@@RXDUXCU
;54    1.149E-03 xCRUXOGCU@@R@@RGDUGCU
;54 M1 1.149E-03 xCRUXOwCU@@R@@RwDUwCU
;54    1.500E-03 H@RXRODHU@@R@@RDHUDHU
;54    2.000E-03 GXRaHPBHU@@R@@RBIUBHU
;54    3.000E-03 FXRRBPwRT@@R@@RwXTwRT
;54    4.000E-03 eTRBgPsST@@R@@RsYTsST
;54    4.782E-03 EIRsHPrFT@@R@@RBQTrFT
;54 L3 4.782E-03 EIRsHPFiT@@R@@RVdTFiT
;54    5.000E-03 TdRSRPvDT@@R@@RvITvDT
;54    5.104E-03 DhRSXPF@T@@R@@RFDTF@T
;54 L2 5.104E-03 DhRSXPXCT@@R@@RXHTXCT
;54    5.275E-03 tXRcXPWQT@@R@@RWVTWRT
;54    5.453E-03 dWRsXPVdT@@R@@RViTVdT
;54 L1 5.453E-03 dWRsXPHBT@@R@@RHFTHBT
;54    6.000E-03 tGRDGPvCT@@R@@RvGTvCT
;54    8.000E-03 CVRECPC@T@@R@@RCCTC@T
;54    1.000E-02 BaREePaVT@@R@@RaYTaVT
;54    1.500E-02 AeRGTPUUS@@R@@RuTSUVS
;54    2.000E-02 qBRHYPRQS@@R@@RbUSRRS
;54    3.000E-02 WUQySPHHR@@R@@RXcRXGR
;54    3.456E-02 VBQAAQERR@@R@@RVCRURR
;54 K  3.456E-02 VBQAAQcDS@@R@@RsBScES
;54    4.000E-02 TaQADQbAS@@R@@RbGSbBS
;54    5.000E-02 CYQAGQaCS@@R@@RaGSaDS
;54    6.000E-02 bQQAIQGUR@@R@@RGbRWVR
;54    8.000E-02 aQQQ@QsFR@@R@@RcSRCWR
;54    1.000E-01 AIQAHQqYR@@R@@RBARQ`R
;54    1.500E-01 u@PABQeUQ@@R@@Rg@QfWQ
;54    2.000E-01 SDPYUPBYQ@@R@@RsVQCUQ
;54    3.000E-01 AVPHYPHAP@@R@@RA`QaUQ
;54    4.000E-01 HTOgYPsPP@@R@@RaBQQDQ
;54    5.000E-01 EWOGFPBIP@@R@@RyPPYEP
;54    6.000E-01 CdOVVPqDP@@R@@RhHPW`P
;54    8.000E-01 RHOuXPVdO@@R@@RvPPFXP
;54    1.000E+00 APOeAPtDO@@R@@RuXPeTP
;54    1.022E+00 qDOUFPTCO@@R@@RuPPUWP
;54    1.250E+00 IBNdVPrXOQaN@@REEPTfP
;54    1.500E+00 fHNdDPQiOHeN@@RTYPTSP
;54    2.000E+00 STNcRPaAORgO@@RDHPDDP
;54    2.044E+00 sINSXPQGOSGO@@RDEPDAP
;54    3.000E+00 QXNBePFRNGVOYhLcXPcWP
;54    4.000E+00 HgMrHPdENQDPDGMSXPSWP
;54    5.000E+00 eXMBFPSDNAXPX@MSXPSXP
;54    6.000E+00 SdMAbPBXNqWPaDNcSPcSP
;54    7.000E+00 R`MaSPBDNBCPaWNsQPsQP
;54    8.000E+00 bBMAYPqSNbGPBINC`PsYP
;54    9.000E+00 qUMqGPQPNBXPBYNCiPCiP
;54    1.000E+01 ARMaGPqBNbXPBgNSiPSiP
;54    1.100E+01 QGMQHPQHNBfPcCNDHPDHP
;54    1.200E+01 IfLQAPAGNCBPSVNTHPTHP
;54    1.300E+01 HPLADPyWMSHPChNdGPdGP
;54    1.400E+01 gELIhOXgMsBPTHNtFPtFP
;54    1.500E+01 vALyHOx@MCUPDWNDTPDTP
;54    1.600E+01 UULXcOwRMSXPtTNTRPTRP
;54    1.800E+01 tHLXFOvWMC`PeDNdWPdWP
;54    2.000E+01 SULWSOFCMD@PuPNDbPDbP
;54    2.200E+01 RcLViOETMTHPVBNTePTeP
;54    2.400E+01 BWLVSOTdMtEPVPNEGPEGP
;54    2.600E+01 R@LVCOTTMTPPFfNUHPUHP
;54    2.800E+01 AaLuYOTIMdSPWHNeIPeIP
;54    3.000E+01 QXLEXOCiMtVPGYNuIPuIP
;54    4.000E+01 HhKtEOBgMeGPxUNE`PE`P
;54    5.000E+01 eXKcSObGMeVPyQNVBPVBP
;54    6.000E+01 SeKSCOAhMUfPAEOvHPvHP
;54    8.000E+01 bBKBWOAPMFPPQFOvVPvVP
;54    1.000E+02 ARKBEOQAMvQPaDOGDPGDP
;54    1.500E+02 vAJAVOwGLgAPqHOGYPGYP
;54    2.000E+02 SUJQDOUQLWPPAWOwWPwWP
;54    3.000E+02 QXJXANcVLGePQWOHIPHIP
;54    4.000E+02 HhIvENrTLHEPaTOhHPhHP
;54    5.000E+02 eXIeFNRILXHPaXOHPPHPP
;54    6.000E+02 SeITPNAbLhGPqQOHYPHYP
;54    8.000E+02 bBISRNqGLHPPqUOhQPhQP
;54    1.000E+03 ARIBiNAILHXPqXOhYPhYP
;54    1.500E+03 vAHBBNgGKhPPAcOH`PH`P
;54    2.000E+03 SUHQVNEUKhVPAeOHgPHgP
;54    3.000E+03 QXHAINcTKxSPAhOXcPXcP
;54    4.000E+03 HhGxIMrSKxWPAiOXgPXgP
;54    5.000E+03 eXGFfMRHKxYPQ`OXiPXiP
;54    6.000E+03 SeGEbMAbKHaPQaOIAPIAP
;54    8.000E+03 bBGDXMqFKHcPQbOICPICP
;54    1.000E+04 ARGcVMAIKHdPQbOIDPIDP
;54    1.500E+04 vAFRSMgGJHfPQcOIFPIFP
;54    2.000E+04 SUFQeMEUJHgPQcOIGPIGP
;54    3.000E+04 QXFqDMcSJHhPQdOIHPIHP
;54    4.000E+04 HhEACMrSJHiPQdOIHPIHP
;54    5.000E+04 eXExHLRHJHiPQdOIIPIIP
;54    6.000E+04 SeEGILAbJHiPQdOIIPIIP
;54    8.000E+04 bBEETLqFJX`PQdOIIPIIP
;54    1.000E+05 AREDRLAIJX`PQeOIIPIIP
;==== ELEMENT  55
;55    1.000E-03 XUREhOyFU@@R@@RyGUyFU
;55    1.032E-03 XRRVCOxVU@@R@@RxWUxVU
;55    1.065E-03 HYRvIOhAU@@R@@RhAUhAU
;55 M2 1.065E-03 HYRvIOhXU@@R@@RhYUhXU
;55    1.139E-03 HRRVgOWUU@@R@@RWVUWUU
;55    1.217E-03 xERWYOVXU@@R@@RVYUVXU
;55 M1 1.217E-03 xERWYOFhU@@R@@RFiUFhU
;55    1.500E-03 HGRIfOtCU@@R@@RtDUtCU
;55    2.000E-03 WXRAPPbBU@@R@@RbCUbBU
;55    3.000E-03 VYRb@PhET@@R@@RxBThET
;55    4.000E-03 uURRdPD@T@@R@@RDETD@T
;55    5.000E-03 EDRSXPbET@@R@@Rr@TbET
;55    5.012E-03 ECRSYPbDT@@R@@RbITbDT
;55 L3 5.012E-03 ECRSYPfRT@@R@@RfXTfRT
;55    5.183E-03 TbRcYPFIT@@R@@RVDTFIT
;55    5.359E-03 DbRsYPePT@@R@@ReTTePT
;55 L2 5.359E-03 DbRsYPgTT@@R@@RgYTgTT
;55    5.534E-03 tRRCiPGET@@R@@RW@TGET
;55    5.714E-03 dRRShPVQT@@R@@RVVTVQT
;55 L1 5.714E-03 dRRShPWPT@@R@@RWUTWPT
;55    6.000E-03 DVRTDPfWT@@R@@RvQTfWT
;55    8.000E-03 SUREHPSHT@@R@@RcATSHT
;55    1.000E-02 BiREiPqVT@@R@@RqYTqVT
;55    1.500E-02 Q`RGVPUaS@@R@@RVASUbS
;55    2.000E-02 qFRXRPbXS@@R@@RBbSbYS
;55    3.000E-02 wYQyVPhSR@@R@@RYQRxSR
;55    3.598E-02 UdQABQUGR@@R@@REfReGR
;55 K  3.598E-02 UdQABQCGS@@R@@RSDSCHS
;55    4.000E-02 EGQADQrBS@@R@@RrHSrCS
;55    5.000E-02 cPQAHQaIS@@R@@RqDSq@S
;55    6.000E-02 rPQAIQGgR@@R@@RhERWhR
;55    8.000E-02 aVQQ@QSVR@@R@@RCdRcWR
;55    1.000E-01 QCQAIQQ`R@@R@@RRBRBAR
;55    1.500E-01 EYPABQFBQ@@R@@RWYQGDQ
;55    2.000E-01 cEPiPPbVQ@@R@@RSdQcRQ
;55    3.000E-01 QRPXTPXVP@@R@@RAfQqQQ
;55    4.000E-01 xUOwSPSfP@@R@@RaFQQGQ
;55    5.000E-01 eYOW@PbDP@@R@@RYaPyDP
;55    6.000E-01 ShOfPPATP@@R@@RHSPHCP
;55    8.000E-01 bGOEbPGUO@@R@@RvYPVVP
;55    1.000E+00 AVOeDPdUO@@R@@REePuQP
;55    1.022E+00 APOUIPDUO@@R@@RuWPeSP
;55    1.250E+00 yGNdYPC@OQiN@@RUAPEAP
;55    1.500E+00 VRNdGPREOYIN@@RdTPTXP
;55    2.000E+00 cXNcUPqAOCGO@@RTBPDHP
;55    2.044E+00 SRNcPPaFOcHO@@RDIPDFP
;55    3.000E+00 aTNBgPVbNgWOA@MsSPsQP
;55    4.000E+00 iBMBPPTXNQGPT@McSPcRP
;55    5.000E+00 U`MBGPsHNQRPXEMcTPcSP
;55    6.000E+00 T@MAcPbWNAbPaENcYPcYP
;55    7.000E+00 CAMaTPb@NBHPaXNsWPsWP
;55    8.000E+00 rAMQPPAfNrBPR@NCfPCfP
;55    9.000E+00 AbMqHPaRNRTPRPNSfPSfP
;55    1.000E+01 AXMaGPASNrTPBhNDFPDFP
;55    1.100E+01 aBMQIPaGNRbPcDNTFPTFP
;55    1.200E+01 ACMQBPQENCIPSXNdEPdEP
;55    1.300E+01 xTLAEPAENcEPS`NtEPtEP
;55    1.400E+01 WSLYdOiUMCPPdANDTPDTP
;55    1.500E+01 VVLISOXcMSSPDYNTSPTSP
;55    1.600E+01 uWLXiOxAMcVPtWNdQPdQP
;55    1.800E+01 TVLhAOgHMChPeGNtWPtWP
;55    2.000E+01 cYLWWOFXMDIPuSNTaPTaP
;55    2.200E+01 CELGCOEeMdGPVENEDPEDP
;55    2.400E+01 RVLVWOuBMDTPVTNUGPUGP
;55    2.600E+01 RHLVGODhMTYPV`NeIPeIP
;55    2.800E+01 AhLEbOTPMtTPgBNuIPuIP
;55    3.000E+01 aTLUQOTHMDgPWSNUPPUPP
;55    4.000E+01 iCKtHOCHMuIPH`NUbPUbP
;55    5.000E+01 U`KcVOBTMuXPyVNfEPfEP
;55    6.000E+01 T@KSEOBBMFIPAEOVQPVQP
;55    8.000E+01 rAKBXOQPMVTPQGOVaPVaP
;55    1.000E+02 AXKBFOa@MFfPaEOWIPWIP
;55    1.500E+02 VVJAWOWbLwFPqIOgUPgUP
;55    2.000E+02 cYJQEOUbLgWPAWOWcPWcP
;55    3.000E+02 aTJXFNScLHBPQXOhFPhFP
;55    4.000E+02 iCIvINRdLhBPaTOHUPHUP
;55    5.000E+02 U`IeINrELxFPaYOXXPXXP
;55    6.000E+02 T@ITSNQfLHVPqROhWPhWP
;55    8.000E+02 rAISTNAWLXXPqVOxYPxYP
;55    1.000E+03 AXIRaNQGLhWPqYOHhPHhP
;55    1.500E+03 VVHBCNGbKxYPAdOXiPXiP
;55    2.000E+03 cYHQWNEfKHePAfOIEPIEP
;55    3.000E+03 aTHAINSaKXbPAiOYBPYBP
;55    4.000E+03 iCGHUMRcKXfPQaOYFPYFP
;55    5.000E+03 U`GVaMrDKXiPQbOYHPYHP
;55    6.000E+03 T@GEeMQeKI@PQbOi@Pi@P
;55    8.000E+03 rAGTQMAVKIBPQcOiBPiBP
;55    1.000E+04 AXGcXMQGKIDPQdOiCPiCP
;55    1.500E+04 VVFRUMGaJIFPQeOiEPiEP
;55    2.000E+04 cYFQfMEfJIGPQeOiFPiFP
;55    3.000E+04 aTFqEMS`JIHPQfOiGPiGP
;55    4.000E+04 iCEADMRcJIHPQfOiHPiHP
;55    5.000E+04 U`EHTLrDJIHPQfOiHPiHP
;55    6.000E+04 T@EWCLQeJIIPQfOiIPiIP
;55    8.000E+04 rAEEWLAVJIIPQfOiIPiIP
;55    1.000E+05 AXEDULQGJIIPQfOiIPiIP
;==== ELEMENT  56
;56    1.000E-03 XRRFfOXSU@@R@@RXTUXSU
;56    1.031E-03 HYRWBOWhU@@R@@RWhUWhU
;56    1.062E-03 HVRwIOGVU@@R@@RGWUGVU
;56 M3 1.062E-03 HVRwIOXTU@@R@@RXUUXTU
;56    1.099E-03 HRRwPOWeU@@R@@RWeUWeU
;56    1.137E-03 xHRHBOGPU@@R@@RGQUGPU
;56 M2 1.137E-03 xHRHBOGcU@@R@@RGdUGcU
;56    1.212E-03 xARhROFdU@@R@@RFeUFdU
;56    1.293E-03 hCRiFOUhU@@R@@RUiUUhU
;56 M1 1.293E-03 hCRiFOfEU@@R@@RfFUfEU
;56    1.500E-03 HBRAIPDYU@@R@@RTPUDYU
;56    2.000E-03 WQRAYPrAU@@R@@RrBUrAU
;56    3.000E-03 VVRbEPhST@@R@@RxPThST
;56    4.000E-03 uSRRePTIT@@R@@RdETTIT
;56    5.000E-03 ECRSXPrFT@@R@@RBQTrFT
;56    5.247E-03 DhRsRPBIT@@R@@RRDTBIT
;56 L3 5.247E-03 DhRsRPFET@@R@@RV@TFET
;56    5.432E-03 tWRCbPUWT@@R@@ReQTUWT
;56    5.624E-03 dVRScPUBT@@R@@RUGTUBT
;56 L2 5.624E-03 dVRScPVgT@@R@@RGBTVgT
;56    5.803E-03 TURDBPFUT@@R@@RVPTFUT
;56    5.989E-03 DVRTBPUgT@@R@@RFATUgT
;56 L1 5.989E-03 DVRTBPFiT@@R@@RVcTFiT
;56    6.000E-03 DVRTBPFeT@@R@@RV`TFeT
;56    8.000E-03 SURECPs@T@@R@@RsCTs@T
;56    1.000E-02 R`REaPAcT@@R@@RAfTAcT
;56    1.500E-02 QaRwDPVES@@R@@RvESVFS
;56    2.000E-02 qGRxHPrYS@@R@@RRdSB`S
;56    3.000E-02 GgQYYPIBR@@R@@RY`RYBR
;56    3.744E-02 eVQAAQDcR@@R@@RUPRTcR
;56 K  3.744E-02 eVQAAQBeS@@R@@RRbSBfS
;56    4.000E-02 UBQABQBPS@@R@@RBVSBQS
;56    5.000E-02 cTQAFQqCS@@R@@RqHSqDS
;56    6.000E-02 rSQAHQXCR@@R@@RXQRhDR
;56    8.000E-02 aYQAHQcYR@@R@@RSfRsYR
;56    1.000E-01 QDQAGQQgR@@R@@Rb@RBHR
;56    1.500E-01 UVPAAQfFQ@@R@@RGcQgGQ
;56    2.000E-01 s@PIUPrWQ@@R@@RDEQsRQ
;56    3.000E-01 QTPHQPXeP@@R@@RAiQqTQ
;56    4.000E-01 X`OgRPTEP@@R@@RaGQQHQ
;56    5.000E-01 uXOG@PrEP@@R@@RYbPyEP
;56    6.000E-01 DEOVPPQQP@@R@@RHQPHAP
;56    8.000E-01 r@OuSPGbO@@R@@RvTPVQP
;56    1.000E+00 AXOUGPDiO@@R@@RE`PeUP
;56    1.022E+00 AROUAPdWO@@R@@RuRPUXP
;56    1.250E+00 YTNdSPSEOBBN@@REFPTfP
;56    1.500E+00 fTNdAPbFOyCN@@RTYPTSP
;56    2.000E+00 sTNSYPqGOS@O@@RDHPDDP
;56    2.044E+00 SXNSUPqBOsAO@@RDEPDAP
;56    3.000E+00 aWNBcPgFNwROIiLcYPcXP
;56    4.000E+00 yHMrFPD`NQHPDCMcPPSYP
;56    5.000E+00 FAMBDPSUNQRPHCMcQPcQP
;56    6.000E+00 TGMA`PB`NAbPaCNcWPcVP
;56    7.000E+00 CGMaRPr@NBIPaVNsUPsUP
;56    8.000E+00 rEMAWPQeNrCPBGNCdPCdP
;56    9.000E+00 AeMqFPaYNRTPBWNSdPSdP
;56    1.000E+01 QPMaFPAYNrTPBdNDDPDDP
;56    1.100E+01 aDMQGPqCNRbPc@NTDPTDP
;56    1.200E+01 ADMQ@PaANCIPSSNdDPdDP
;56    1.300E+01 HiLACPQ@NcEPCdNtDPtDP
;56    1.400E+01 gWLyYOAANCPPTDNDSPDSP
;56    1.500E+01 fXLy@OyDMSSPDSNTRPTRP
;56    1.600E+01 EgLHeOhYMcVPtPNdPPdPP
;56    1.800E+01 dTLHIOgRMCiPUINtVPtVP
;56    2.000E+01 sVLGVOvYMDIPeTNT`PT`P
;56    2.200E+01 S@LVcOVAMdHPFFNEDPEDP
;56    2.400E+01 bQLFXOUVMDUPFTNUFPUFP
;56    2.600E+01 bBLFHOU@MdPPvYNeHPeHP
;56    2.800E+01 QbLuTOtQMtTPWANuIPuIP
;56    3.000E+01 aWLESOtHMDgPGRNEYPEYP
;56    4.000E+01 yIKtBOcCMEPPhWNUbPUbP
;56    5.000E+01 FAKcPORVMuYPiQNfEPfEP
;56    6.000E+01 TGKS@ORAMFIPADOVQPVQP
;56    8.000E+01 rEKBUOQWMVTPQEOV`PV`P
;56    1.000E+02 QPKBCOaEMFfPaCOWIPWIP
;56    1.500E+02 fXJAUOhHLwGPqGOgUPgUP
;56    2.000E+02 sVJQCOVILgWPAUOWcPWcP
;56    3.000E+02 aWJHDNTALHCPQVOhFPhFP
;56    4.000E+02 yIIv@NCHLhCPaROHUPHUP
;56    5.000E+02 FAIeANBVLxFPaVOXXPXXP
;56    6.000E+02 TGIDVNBELHVPaYOhWPhWP
;56    8.000E+02 rEICYNQTLXYPqTOH`PH`P
;56    1.000E+03 QPIBgNaCLhWPqWOHhPHhP
;56    1.500E+03 fXHB@NXHKxYPAaOXiPXiP
;56    2.000E+03 sVHQUNVCKHePAdOIEPIEP
;56    3.000E+03 aWHAHNDIKXbPAgOYBPYBP
;56    4.000E+03 yIGxBMCFKXfPAhOYFPYFP
;56    5.000E+03 FAGFaMBUKXiPAiOYHPYHP
;56    6.000E+03 TGGuWMBDKI@PQ`Oi@Pi@P
;56    8.000E+03 rEGDUMQSKIBPQaOiBPiBP
;56    1.000E+04 QPGcSMaCKIDPQaOiCPiCP
;56    1.500E+04 fXFRQMXGJIFPQbOiEPiEP
;56    2.000E+04 sVFQcMVCJIGPQcOiFPiFP
;56    3.000E+04 aWFqCMDHJIHPQcOiGPiGP
;56    4.000E+04 yIEABMCFJIIPQcOiHPiHP
;56    5.000E+04 FAExALBUJIIPQdOiIPiIP
;56    6.000E+04 TGEGCLBDJIIPQdOiIPiIP
;56    8.000E+04 rEEuILQSJY@PQdOiIPiIP
;56    1.000E+05 QPEtHLaBJY@PQdOiIPiIP
;==== ELEMENT  57
;57    1.000E-03 xTRFhOIHU@@R@@RIIUIHU
;57    1.060E-03 hWRGQOWgU@@R@@RWhUWgU
;57    1.123E-03 hQRWgOViU@@R@@RG@UViU
;57 M3 1.123E-03 hQRWgOHBU@@R@@RHBUHBU
;57    1.163E-03 XVRxBOGTU@@R@@RGUUGTU
;57    1.204E-03 XRRhWOV`U@@R@@RVaUV`U
;57 M2 1.204E-03 XRRhWOwAU@@R@@RwBUwAU
;57    1.280E-03 HTRy@OFRU@@R@@RFSUFRU
;57    1.361E-03 xERYeOeTU@@R@@ReUUeTU
;57 M1 1.361E-03 xERYeOU`U@@R@@RU`UU`U
;57    1.500E-03 h@RQAPtVU@@R@@RtWUtVU
;57    2.000E-03 gWRQQPBVU@@R@@RBVUBVU
;57    3.000E-03 fYRbHPi@T@@R@@RiGTi@T
;57    4.000E-03 EdRRhPDWT@@R@@RTSTDWT
;57    5.000E-03 UCRcQPRST@@R@@RRXTRST
;57    5.483E-03 DdRCiPQiT@@R@@RBDTQiT
;57 L3 5.483E-03 DdRCiPuQT@@R@@RuVTuQT
;57    5.683E-03 tRRD@PeDT@@R@@ReITeDT
;57    5.891E-03 dQRTAPD`T@@R@@RDeTD`T
;57 L2 5.891E-03 dQRTAPVPT@@R@@RVUTVPT
;57    6.000E-03 TURTGPfGT@@R@@RvBTfGT
;57    6.266E-03 DQRt@PeQT@@R@@ReUTeQT
;57 L1 6.266E-03 DQRt@PFWT@@R@@RVRTFWT
;57    8.000E-03 cTREHPCYT@@R@@RSSTCYT
;57    1.000E-02 RgREePQdT@@R@@RQgTQdT
;57    1.500E-02 QeRwGPVSS@@R@@RvSSVTS
;57    2.000E-02 APRHQPRgS@@R@@RSBSRhS
;57    3.000E-02 XBQiSPiRR@@R@@RAESyQR
;57    3.892E-02 UPQABQdRR@@R@@ReGRtRR
;57 K  3.892E-02 UPQABQrQS@@R@@RrWSrRS
;57    4.000E-02 eHQACQRRS@@R@@RRXSRSS
;57    5.000E-02 sVQAFQAPS@@R@@RAUSAQS
;57    6.000E-02 BbQAHQXWR@@R@@RXfRhXR
;57    8.000E-02 qTQAIQCiR@@R@@RTHRD@R
;57    1.000E-01 QHQAGQBIR@@R@@RrBRb@R
;57    1.500E-01 uVPAAQfUQ@@R@@RhDQgVQ
;57    2.000E-01 CRPYPPReQ@@R@@RdDQS`Q
;57    3.000E-01 aPPHVPYUP@@R@@RQfQA`Q
;57    4.000E-01 iCOgVPDSP@@R@@Rq@QaAQ
;57    5.000E-01 F@OGDPRQP@@R@@RABQYUP
;57    6.000E-01 d@OVTPaQP@@R@@RXWPXEP
;57    8.000E-01 rIOuWPxHO@@R@@RFdPfPP
;57    1.000E+00 QTOe@PeDO@@R@@REhPuRP
;57    1.022E+00 AXOUDPEAO@@R@@RuYPeTP
;57    1.250E+00 Y`NdUPsHOBIN@@RUAPEAP
;57    1.500E+00 FiNdCPBROiXN@@RdTPTWP
;57    2.000E+00 CiNcRPAWOc@O@@RTBPDHP
;57    2.044E+00 sRNSWPAROCRO@@RDIPDFP
;57    3.000E+00 qSNBePwWNWdOYeLsTPsRP
;57    4.000E+00 yTMrHPUCNaAPDFMcUPcTP
;57    5.000E+00 fDMBEPsYNQVPHHMcVPcVP
;57    6.000E+00 tCMAaPRiNAgPaDNsSPsRP
;57    7.000E+00 SHMaSPBVNRDPaWNCaPCaP
;57    8.000E+00 BTMAXPBHNrHPBHNSaPS`P
;57    9.000E+00 QcMqFPAaNbPPBXNDAPDAP
;57    1.000E+01 QVMaFPQYNB`PBfNTAPTAP
;57    1.100E+01 aIMQHPARNRiPcBNdBPdAP
;57    1.200E+01 AHMQAPaINSFPSUNtBPtBP
;57    1.300E+01 iCLADPQGNsBPCgNDRPDRP
;57    1.400E+01 WfLIeOAHNCWPTGNTQPTQP
;57    1.500E+01 VcLyFOYhMcQPDUNdPPdPP
;57    1.600E+01 V@LXaOiHMsTPtSNdYPdYP
;57    1.800E+01 DbLXDOXDMSgPeBNDePDeP
;57    2.000E+01 S`LWQOgDMTHPeXNE@PE@P
;57    2.200E+01 cBLVhOVRMtGPV@NUDPUDP
;57    2.400E+01 rQLVROUdMTTPFXNeGPeGP
;57    2.600E+01 rALVBOEUMtPPFcNuIPuIP
;57    2.800E+01 QiLuWOECMDdPWENUPPUPP
;57    3.000E+01 qSLEWOdWMTgPGVNePPePP
;57    4.000E+01 yUKtDOCTMUQPxQNFDPFDP
;57    5.000E+01 fDKcSOrSMUaPiVNvGPvGP
;57    6.000E+01 tCKSBObFMfBPADOfTPfTP
;57    8.000E+01 BTKBVOaXMfXPQEOGDPGDP
;57    1.000E+02 QVKBEOqCMGAPaDOwDPwDP
;57    1.500E+02 VcJAVOHdLWRPqGOGaPGaP
;57    2.000E+02 S`JQDOfPLGcPAVOHIPHIP
;57    3.000E+02 qSJHINtILXIPQVOHSPHSP
;57    4.000E+02 yUIvDNcHLHPPaSOhSPhSP
;57    5.000E+02 fDIeENbRLXTPaWOxVPxVP
;57    6.000E+02 tCIDYNRILhTPqPOHePHeP
;57    8.000E+02 BTISQNaTLxWPqUOXhPXhP
;57    1.000E+03 QVIBiNqALHePqXOIFPIFP
;57    1.500E+03 VcHBBNxRKXgPAbOYHPYHP
;57    2.000E+03 S`HQVNVTKIDPAeOiDPiDP
;57    3.000E+03 qSHAINtFKYAPAhOyAPyAP
;57    4.000E+03 yUGxHMcGKYEPAiOyEPyEP
;57    5.000E+03 fDGFeMbQKYHPQ`OyHPyHP
;57    6.000E+03 tCGEaMRHKi@PQaOyIPyIP
;57    8.000E+03 BTGDWMaSKiBPQbOIQPIQP
;57    1.000E+04 QVGcUMqAKiCPQbOISPISP
;57    1.500E+04 VcFRRMxQJiEPQcOIUPIUP
;57    2.000E+04 S`FQdMVSJiFPQdOIVPIVP
;57    3.000E+04 qSFqDMtFJiGPQdOIWPIWP
;57    4.000E+04 yUEACMcGJiHPQeOIWPIWP
;57    5.000E+04 fDExGLbQJiHPQeOIXPIXP
;57    6.000E+04 tCEGGLRHJiHPQeOIXPIXP
;57    8.000E+04 BTEERLaSJiIPQeOIXPIXP
;57    1.000E+05 QVEDQLqAJiIPQeOIXPIXP
;==== ELEMENT  58
;58    1.000E-03 XiRvSOyPU@@R@@RyQUyPU
;58    1.089E-03 HiRWROWhU@@R@@RWhUWhU
;58    1.185E-03 xYRxCOVUU@@R@@RVVUVUU
;58 M3 1.185E-03 xYRxCOWSU@@R@@RWTUWSU
;58    1.228E-03 xURhWOVhU@@R@@RVhUVhU
;58    1.273E-03 xPRIBOFVU@@R@@RFVUFVU
;58 M2 1.273E-03 xPRIBOFdU@@R@@RFeUFdU
;58    1.352E-03 hRRiWOFAU@@R@@RFBUFAU
;58    1.437E-03 XRRADPeIU@@R@@Ru@UeIU
;58 M1 1.437E-03 XRRADPURU@@R@@RUSUURU
;58    1.500E-03 HURAIPEBU@@R@@RECUEBU
;58    2.000E-03 WbRAYPbPU@@R@@RbQUbPU
;58    3.000E-03 VbRbEPyYT@@R@@RIfTyYT
;58    4.000E-03 FERRfPtUT@@R@@RDaTtUT
;58    5.000E-03 uARSYPbYT@@R@@RrTTbYT
;58    5.723E-03 DgRD@PAiT@@R@@RQdTAiT
;58 L3 5.723E-03 DgRD@PEQT@@R@@REVTEQT
;58    6.000E-03 tQRTEPDfT@@R@@RTaTDfT
;58    6.164E-03 dRRdCPTQT@@R@@RTVTTQT
;58 L2 6.164E-03 dRRdCPVDT@@R@@RVITVDT
;58    6.354E-03 TQRtCPuPT@@R@@RuTTuPT
;58    6.549E-03 DQRDRPeIT@@R@@RuCTeIT
;58 L1 6.549E-03 DQRDRPV@T@@R@@RVETV@T
;58    8.000E-03 sVREGPcYT@@R@@RsSTcYT
;58    1.000E-02 CGREePBET@@R@@RBHTBET
;58    1.500E-02 BBRwHPVcS@@R@@RWDSVdS
;58    2.000E-02 AURHSPSFS@@R@@RsASSGS
;58    3.000E-02 HPQiXPACS@@R@@RQBSACS
;58    4.000E-02 EWQACQTVR@@R@@ReARdWR
;58    4.044E-02 uHQADQDSR@@R@@REGRTSR
;58 K  4.044E-02 uHQADQRWS@@R@@RbSSRXS
;58    5.000E-02 CiQAGQAWS@@R@@RQRSAXS
;58    6.000E-02 RbQAIQIER@@R@@RIURYFR
;58    8.000E-02 AaQAIQTBR@@R@@RDQRdCR
;58    1.000E-01 aCQAHQbAR@@R@@RBURrBR
;58    1.500E-01 UhPABQGGQ@@R@@RhYQHIQ
;58    2.000E-01 SUPYXPSDQ@@R@@RDUQT@Q
;58    3.000E-01 aVPXSPABQ@@R@@RBDQAgQ
;58    4.000E-01 iPOwRPtTP@@R@@RqDQaEQ
;58    5.000E-01 fDOW@PbYP@@R@@RADQyYP
;58    6.000E-01 tGOVYPqSP@@R@@RxVPxBP
;58    8.000E-01 BYOEbPXhO@@R@@RVfPvQP
;58    1.000E+00 aPOeDPeQO@@R@@RUfPE`P
;58    1.022E+00 QTOUIPuGO@@R@@REhPuRP
;58    1.250E+00 ACOdYPcRORHN@@RUHPEHP
;58    1.500E+00 WHNdGPRYOAAO@@RtPPdSP
;58    2.000E+00 DENcUPQWOsBO@@RTHPTDP
;58    2.044E+00 ChNcPPQROSTO@@RTEPTAP
;58    3.000E+00 A`NBgPxBNXGOA@MsYPsWP
;58    4.000E+00 AANBPPUPNaDPT@MsPPcYP
;58    5.000E+00 FYMBGPDFNaPPXEMsSPsRP
;58    6.000E+00 TQMAcPc@NQaPaENsYPsYP
;58    7.000E+00 sBMaTPbSNRIPaXNChPChP
;58    8.000E+00 RTMQPPbCNBTPR@NShPShP
;58    9.000E+00 BAMqHPQcNbVPRPNDIPDHP
;58    1.000E+01 aRMaHPqPNBgPBhNTIPTIP
;58    1.100E+01 qDMQIPQRNCFPcDNt@Pt@P
;58    1.200E+01 QCMQBPqHNcDPSXNDQPDPP
;58    1.300E+01 iQLAEPaFNCPPS`NTQPTQP
;58    1.400E+01 hILYdOQENSVPd@NdQPdPP
;58    1.500E+01 gBLITOAGNsPPDYNtPPtPP
;58    1.600E+01 vELXiOYbMCcPtVNtYPtYP
;58    1.800E+01 EBLhAOxPMDGPeGNTePTeP
;58    2.000E+01 DFLWWOwTMdHPuRNU@PU@P
;58    2.200E+01 sFLGDOVhMDWPVENeEPeEP
;58    2.400E+01 BbLVXOvDMdUPVSNuHPuHP
;58    2.600E+01 BPLVGOEbMDaPFiNUPPUPP
;58    2.800E+01 BGLEbOuGMTfPgANeRPeRP
;58    3.000E+01 AaLUQOTiMEIPWRNuRPuRP
;58    4.000E+01 ABLtHOcXMeTPxYNVGPVGP
;58    5.000E+01 VPKcVORaMFEPyTNVRPVRP
;58    6.000E+01 TQKSEOBQMvGPAEOvYPvYP
;58    8.000E+01 RTKBYOqYMFdPQFOg@Pg@P
;58    1.000E+02 aRKBFOASMWGPaEOWPPWPP
;58    1.500E+02 gBJAWOITLwPPqHOWiPWiP
;58    2.000E+02 DFJQEOGELHBPAWOhHPhHP
;58    3.000E+02 AaJXGNdXLxIPQXOhSPhSP
;58    4.000E+02 ABJFPNSQLhPPaTOHcPHcP
;58    5.000E+02 VPIu@NB`LxTPaXOXfPXfP
;58    6.000E+02 TQITSNrCLHdPqQOIFPIFP
;58    8.000E+02 RTISTNqULXgPqVOYIPYIP
;58    1.000E+03 aRIRaNAPLIFPqYOiGPiGP
;58    1.500E+03 gBHBCNyBKYHPAcOyIPyIP
;58    2.000E+03 DFHQWNVhKiEPAfOIVPIVP
;58    3.000E+03 AaHAINdUKyCPAiOYSPYSP
;58    4.000E+03 ABHHUMCYKyGPQ`OYVPYVP
;58    5.000E+03 VPGVaMrYKyIPQaOYYPYYP
;58    6.000E+03 TQGEfMrCKIQPQbOiQPiQP
;58    8.000E+03 RTGTQMqTKISPQcOiSPiSP
;58    1.000E+04 aRGcXMAPKIUPQdOiTPiTP
;58    1.500E+04 gBFRUMyAJIVPQdOiVPiVP
;58    2.000E+04 DFFQfMVhJIXPQeOiWPiWP
;58    3.000E+04 AaFqEMdUJIYPQeOiXPiXP
;58    4.000E+04 ABFADMCYJIYPQfOiYPiYP
;58    5.000E+04 VPEHTLrYJYPPQfOyPPyPP
;58    6.000E+04 TQEWCLrCJYPPQfOyPPyPP
;58    8.000E+04 RTEEWLqTJYPPQfOyPPyPP
;58    1.000E+05 aREDULAPJYPPQfOyPPyPP
;==== ELEMENT  59
;59    1.000E-03 iHRVSOAFV@@R@@RAFVAFV
;59    1.115E-03 YFRWPOXDU@@R@@RXEUXDU
;59    1.242E-03 ICRXSOfGU@@R@@RfHUfGU
;59 M3 1.242E-03 ICRXSOgAU@@R@@RgBUgAU
;59    1.289E-03 XiRHhOfUU@@R@@RfVUfUU
;59    1.337E-03 XeRiEOVCU@@R@@RVDUVCU
;59 M2 1.337E-03 XeRiEOVPU@@R@@RVQUVPU
;59    1.500E-03 xWRAFPEHU@@R@@REIUEHU
;59    1.511E-03 xVRAFPE@U@@R@@REAUE@U
;59 M1 1.511E-03 xVRAFPeCU@@R@@ReDUeCU
;59    2.000E-03 hERAUPrVU@@R@@RrWUrVU
;59    3.000E-03 gDRb@PADU@@R@@RAEUADU
;59    4.000E-03 vDRR`PEGT@@R@@RUCTEGT
;59    5.000E-03 UWRSSPBgT@@R@@RRbTBgT
;59    5.964E-03 TeRDFPAbT@@R@@RAgTAbT
;59 L3 5.964E-03 TeRDFPUBT@@R@@RUGTUBT
;59    6.000E-03 TcRDHPEIT@@R@@RUDTEIT
;59    6.440E-03 dXRtAPdFT@@R@@RtATdFT
;59 L2 6.440E-03 dXRtAPEaT@@R@@REfTEaT
;59    6.635E-03 TXRDPPEPT@@R@@RETTEPT
;59    6.835E-03 DWRTPPEAT@@R@@REFTEAT
;59 L1 6.835E-03 DWRTPPuYT@@R@@REdTuYT
;59    8.000E-03 ScREBPSaT@@R@@RSeTSaT
;59    1.000E-02 c@RE`PRHT@@R@@RbATRHT
;59    1.500E-02 BIRwFPwHS@@R@@RgPSwIS
;59    2.000E-02 QPRHUPsGS@@R@@RSSSsHS
;59    3.000E-02 xTQyTPAIS@@R@@RQISQ@S
;59    4.000E-02 eYQADQDhR@@R@@RUVRTiR
;59    4.199E-02 eHQAEQdFR@@R@@RDiRtFR
;59 K  4.199E-02 eHQAEQBUS@@R@@RRRSBVS
;59    5.000E-02 DDQAHQQUS@@R@@RaPSQVS
;59    6.000E-02 CCQQ@QYVR@@R@@RYhRiWR
;59    8.000E-02 AhQQ@QtFR@@R@@RdVRDWR
;59    1.000E-01 aHQAIQrER@@R@@RRYRBVR
;59    1.500E-01 fCPACQWSQ@@R@@RYHQXVQ
;59    2.000E-01 sPPiXPsEQ@@R@@RdYQtBQ
;59    3.000E-01 qSPhRPAIQ@@R@@RRCQQeQ
;59    4.000E-01 A@PGaPEHP@@R@@RqIQaIQ
;59    5.000E-01 VPOWHPBhP@@R@@RAGQAAQ
;59    6.000E-01 TVOfWPAeP@@R@@RXhPXRP
;59    8.000E-01 bPOEhPiTO@@R@@RW@PFdP
;59    1.000E+00 aWOu@PFCO@@R@@RFGPU`P
;59    1.022E+00 aPOeDPuWO@@R@@RUhPEbP
;59    1.250E+00 AHOtUPCiObGN@@ReGPUFP
;59    1.500E+00 GYNtBPrXOAEO@@RtXPtPP
;59    2.000E+00 dCNcYPaYOCTO@@RdDPd@P
;59    2.044E+00 DENcTPaSOcWO@@RdAPTGP
;59    3.000E+00 AhNR`PXcNHTOABMCfPCdP
;59    4.000E+00 AFNBRPU`NaHPTDMsWPsVP
;59    5.000E+00 vYMBIPtENaUPhDMC`PsYP
;59    6.000E+00 tQMAePCSNQgPaFNCgPCgP
;59    7.000E+00 CVMaVPBbNbEPqPNSfPSfP
;59    8.000E+00 bUMQQPrINRQPRCNDGPDFP
;59    9.000E+00 R@MqIPBGNrTPRSNTHPTGP
;59    1.000E+01 qPMaIPAcNRePRbNdIPdIP
;59    1.100E+01 APMa@PaSNSDPcHNDPPDPP
;59    1.200E+01 QHMQCPAWNsCPcRNTQPTQP
;59    1.300E+01 A@MAFPqDNSPPSdNdQPdQP
;59    1.400E+01 hVLAAPaDNcUPdENtQPtQP
;59    1.500E+01 WTLYUOQDNC`PTTNDaPDaP
;59    1.600E+01 fSLIIOAFNScPDbNT`PT`P
;59    1.800E+01 eDLxAOyAMTHPuCNEGPEGP
;59    2.000E+01 dDLgVOhIMtIPuYNeCPeCP
;59    2.200E+01 SQLWBOGWMTYPfANuHPuHP
;59    2.400E+01 ReLfUOvYMtWPfPNUQPUQP
;59    2.600E+01 RQLfDOfCMTdPVfNeTPeTP
;59    2.800E+01 RFLEiOuUMEIPgINuUPuUP
;59    3.000E+01 AiLUXOuDMeCPgPNEgPEgP
;59    4.000E+01 AFLDSOSdMuYPHhNvCPvCP
;59    5.000E+01 vYKsPOSBMfAPIdNfXPfXP
;59    6.000E+01 tQKSIORXMVSPAFOVfPVfP
;59    8.000E+01 bUKRQOQbMGBPQHOwIPwIP
;59    1.000E+02 qPKBIOQSMwFPaFOgYPgYP
;59    1.500E+02 WTJAYOAAMW`PAPOXIPXIP
;59    2.000E+02 dDJQGOWULhBPAXOHYPHYP
;59    3.000E+02 AiJhFNEALhPPQYOHePHeP
;59    4.000E+02 AFJFWNsULHbPaUOIEPIEP
;59    5.000E+02 vYIuFNC@LXfPqPOYIPYIP
;59    6.000E+02 tQITYNRPLIFPqSOiHPiHP
;59    8.000E+02 bUISXNAgLi@PqWOIQPIQP
;59    1.000E+03 qPIReNQPLiIPA`OYPPYPP
;59    1.500E+03 WTHBFNYgKIRPAeOiRPiRP
;59    2.000E+03 dDHQYNGWKIYPAgOiYPiYP
;59    3.000E+03 AiHQANThKYVPQ`OyWPyWP
;59    4.000E+03 AFHXUMsSKiQPQbOIaPIaP
;59    5.000E+03 vYGViMRiKiSPQcOIcPIcP
;59    6.000E+03 tQGUbMBYKiUPQcOIePIeP
;59    8.000E+03 bUGTVMAgKiWPQdOIgPIgP
;59    1.000E+04 qPGsSMAYKiYPQeOIiPIiP
;59    1.500E+04 WTFRXMYeJyQPQfOY`PY`P
;59    2.000E+04 dDFQhMGWJyRPQfOYbPYbP
;59    3.000E+04 AiFqGMThJySPQgOYcPYcP
;59    4.000E+04 AFFAEMsSJyTPQgOYcPYcP
;59    5.000E+04 vYEXTLRiJyTPQgOYdPYdP
;59    6.000E+04 tQEgBLBYJyTPQgOYdPYdP
;59    8.000E+04 bUEUSLAgJyTPQgOYdPYdP
;59    1.000E+05 qPETPLAYJyUPQgOYePYeP
;==== ELEMENT  60
;60    1.000E-03 IPRvBOfRU@@R@@RfSUfRU
;60    1.002E-03 IPRvDOvWU@@R@@RvXUvWU
;60    1.005E-03 yIRvFOVbU@@R@@RVcUVbU
;60 M4 1.005E-03 yIRvFOXQU@@R@@RXRUXQU
;60    1.142E-03 iDRGVOW@U@@R@@RWAUW@U
;60    1.297E-03 YARhUOUbU@@R@@RUcUUbU
;60 M3 1.297E-03 YARhUOFbU@@R@@RFcUFbU
;60    1.349E-03 IERIFOfFU@@R@@RfGUfFU
;60    1.403E-03 I@RIXOuTU@@R@@RuUUuTU
;60 M2 1.403E-03 I@RIXOFHU@@R@@RFIUFHU
;60    1.500E-03 HiRABPeFU@@R@@ReGUeFU
;60    1.575E-03 HaRAHPtSU@@R@@RtTUtSU
;60 M1 1.575E-03 HaRAHPTdU@@R@@RTeUTdU
;60    2.000E-03 xHRAQPBgU@@R@@RBhUBgU
;60    3.000E-03 wGRREPAIU@@R@@RAIUAIU
;60    4.000E-03 FVRBcPu@T@@R@@RuGTu@T
;60    5.000E-03 eXRCUPC@T@@R@@RCFTC@T
;60    6.000E-03 ECRD@PAhT@@R@@RQcTAhT
;60    6.208E-03 TaRT@PqRT@@R@@RqWTqRT
;60 L3 6.208E-03 TaRT@PDdT@@R@@RDiTDdT
;60    6.460E-03 tVRdCPtHT@@R@@RDSTtHT
;60    6.721E-03 dRRtEPSgT@@R@@RDATSgT
;60 L2 6.721E-03 dRRtEPEQT@@R@@REVTEQT
;60    6.921E-03 TQRDUPECT@@R@@REHTECT
;60    7.126E-03 DQRTTPdXT@@R@@RtRTdXT
;60 L1 7.126E-03 DQRTTPEPT@@R@@REUTEPT
;60    8.000E-03 DARTbPDET@@R@@RDITDET
;60    1.000E-02 cFRuPPbGT@@R@@Rr@TbGT
;60    1.500E-02 RCRgDPwPS@@R@@RWbSwQS
;60    2.000E-02 QSRxCPSRS@@R@@RcXSSSS
;60    3.000E-02 XaQiSPQES@@R@@RaESQFS
;60    4.000E-02 E`QACQUCR@@R@@REaReCR
;60    4.357E-02 EIQAEQDCR@@R@@RdTRTCR
;60 K  4.357E-02 EIQAEQr@S@@R@@RrGSrAS
;60    5.000E-02 TBQAGQaPS@@R@@RaUSaQS
;60    6.000E-02 CIQAIQYaR@@R@@RACSA@S
;60    8.000E-02 QbQAIQTTR@@R@@RDdRdUR
;60    1.000E-01 qAQAHQBUR@@R@@RbYRRVR
;60    1.500E-01 vFPABQGfQ@@R@@RYRQHiQ
;60    2.000E-01 sXPiQPSQQ@@R@@RDdQDWQ
;60    3.000E-01 qWPXVPQDQ@@R@@RRHQB@Q
;60    4.000E-01 ABPwVPuDP@@R@@RAQQqAQ
;60    5.000E-01 fVOWCPCCP@@R@@RAHQABQ
;60    6.000E-01 dWOfRPQeP@@R@@RIDPXWP
;60    8.000E-01 bVOEePABP@@R@@RWCPFfP
;60    1.000E+00 qQOeFPvEO@@R@@RFGPU`P
;60    1.022E+00 aTOeAPFHO@@R@@RUhPEbP
;60    1.250E+00 Q@OtRPT@OrBN@@ReFPUEP
;60    1.500E+00 gXNdIPRcOAGO@@RtWPdYP
;60    2.000E+00 tCNcVPqXOSPO@@RdDPTIP
;60    2.044E+00 TENcRPqROsSO@@RdAPTFP
;60    3.000E+00 QcNBiPIPNXUOAAMCfPCdP
;60    4.000E+00 AINBQPf@NaIPTAMsXPsWP
;60    5.000E+00 VfMBHPTXNaWPXIMCaPC`P
;60    6.000E+00 DcMAdPcQNQiPaFNChPCgP
;60    7.000E+00 SUMaUPRfNbGPaYNSgPSgP
;60    8.000E+00 rRMQPPRQNRSPRANDHPDHP
;60    9.000E+00 REMqHPRHNrVPRQNTIPTIP
;60    1.000E+01 qTMaHPQbNRgPR`Nt@Pt@P
;60    1.100E+01 ATMQIPqRNSGPcFNDRPDQP
;60    1.200E+01 aAMQBPQUNsEPcPNTSPTRP
;60    1.300E+01 ACMAFPAQNSRPSbNdSPdSP
;60    1.400E+01 HhLYiOq@NcXPdBNtSPtSP
;60    1.500E+01 wSLIXOa@NCcPTQNDcPDcP
;60    1.600E+01 F`LICOQBNSfPtXNTbPTbP
;60    1.800E+01 uGLhEOyYMdAPeINU@PU@P
;60    2.000E+01 tELgQOxQMDSPuUNeFPeFP
;60    2.200E+01 SYLGGOGdMdSPVGNEPPEPP
;60    2.400E+01 CBLfPOWDMDaPVUNUTPUTP
;60    2.600E+01 RWLf@OVTMTgPVaNeWPeWP
;60    2.800E+01 bBLEeOFDMUCPgDNuYPuYP
;60    3.000E+01 QcLUTOeQMeFPWUNU`PU`P
;60    4.000E+01 AILDPOTDMEcPHaNvGPvGP
;60    5.000E+01 VfKcXOcGMfEPyWNvRPvRP
;60    6.000E+01 DcKSGOrQMVXPAEOGAPGAP
;60    8.000E+01 rRKRPOBAMGGPQGOGTPGTP
;60    1.000E+02 qTKBGOaPMGQPaEOwTPwTP
;60    1.500E+02 wSJAXOAFMWfPqIOhDPhDP
;60    2.000E+02 tEJQFOWbLhHPAWOXUPXUP
;60    3.000E+02 QcJh@NeFLhVPQXOX`PX`P
;60    4.000E+02 AIJFSNSdLHhPaTOYAPYAP
;60    5.000E+02 VfIuBNSELICPaXOiEPiEP
;60    6.000E+02 DcITVNbRLYCPqQOyEPyEP
;60    8.000E+02 rRISVNQfLiGPqVOIXPIXP
;60    1.000E+03 qTIRcNQWLyFPqYOYVPYVP
;60    1.500E+03 wSHBDNAELIYPAcOiYPiYP
;60    2.000E+03 tEHQXNGdKYVPAeOyVPyVP
;60    3.000E+03 QcHQ@NeCKiSPAhOIcPIcP
;60    4.000E+03 AIHHYMSbKiWPQ`OIgPIgP
;60    5.000E+03 VfGVdMSDKyPPQaOY`PY`P
;60    6.000E+03 DcGEiMbQKyRPQaOYaPYaP
;60    8.000E+03 rRGTSMQfKyTPQbOYdPYdP
;60    1.000E+04 qTGsPMQWKyUPQcOYePYeP
;60    1.500E+04 wSFRVMAEKyWPQdOYgPYgP
;60    2.000E+04 tEFQgMGdJyYPQdOYhPYhP
;60    3.000E+04 QcFqFMeBJI`PQeOYiPYiP
;60    4.000E+04 AIFADMSbJI`PQeOA@QA@Q
;60    5.000E+04 VfEHXLSCJIaPQeOA@QA@Q
;60    6.000E+04 DcEWGLbQJIaPQeOA@QA@Q
;60    8.000E+04 rREUPLQfJIbPQeOA@QA@Q
;60    1.000E+05 qTEDWLQWJIbPQeOA@QA@Q
;==== ELEMENT  61
;61    1.000E-03 iYRfCOBEU@@R@@RBFUBEU
;61    1.013E-03 iXRvCOB@U@@R@@RBAUB@U
;61    1.027E-03 iWRFTOQeU@@R@@RQfUQeU
;61 M5 1.027E-03 iWRFTORTU@@R@@RRUURTU
;61    1.039E-03 iURVTOCeU@@R@@RCfUCeU
;61    1.051E-03 iTRfTOuYU@@R@@RE`UuYU
;61 M4 1.051E-03 iTRfTOWIU@@R@@Rg@UWIU
;61    1.194E-03 YPRwVOvIU@@R@@RFPUvIU
;61    1.357E-03 yCRIAOeXU@@R@@ReYUeXU
;61 M3 1.357E-03 yCRIAOVUU@@R@@RVUUVUU
;61    1.413E-03 iHRITOUhU@@R@@RUiUUhU
;61    1.471E-03 iARIiOEVU@@R@@REWUEVU
;61 M2 1.471E-03 iARIiOuYU@@R@@RE`UuYU
;61    1.500E-03 YHRAAPUTU@@R@@RUUUUTU
;61    1.653E-03 IBRQCPDYU@@R@@RTPUDYU
;61 M1 1.653E-03 IBRQCPdYU@@R@@RtPUdYU
;61    2.000E-03 hVRqIPCDU@@R@@RCEUCDU
;61    3.000E-03 gTRRCPQEU@@R@@RQFUQEU
;61    4.000E-03 vQRBaPeTT@@R@@RuQTeTT
;61    5.000E-03 U`RCSPc@T@@R@@RcFTc@T
;61    6.000E-03 eBRShPB@T@@R@@RBETB@T
;61    6.459E-03 TeRdAPaUT@@R@@RqPTaUT
;61 L3 6.459E-03 TeRdAPdST@@R@@RdXTdST
;61    6.730E-03 tYRtEPTGT@@R@@RdBTTGT
;61    7.013E-03 dTRDXPsVT@@R@@RC`TsVT
;61 L2 7.013E-03 dTRDXPUCT@@R@@RUHTUCT
;61    7.217E-03 TTRTWPtXT@@R@@RDbTtXT
;61    7.428E-03 DSRdWPDUT@@R@@RTPTDUT
;61 L1 7.428E-03 DSRdWPUDT@@R@@RUITUDT
;61    8.000E-03 TFRTbPdGT@@R@@RtATdGT
;61    1.000E-02 sIRuPPBQT@@R@@RBTTBQT
;61    1.500E-02 bARgFPXHS@@R@@RHQSXIS
;61    2.000E-02 QXRxGPsUS@@R@@RSbSsVS
;61    3.000E-02 iEQyPPaBS@@R@@RqCSaCS
;61    4.000E-02 FBQADQEWR@@R@@RVHRUXR
;61    4.518E-02 E@QAFQCiR@@R@@RDYRSiR
;61 K  4.518E-02 E@QAFQb@S@@R@@RbGSbBS
;61    5.000E-02 dHQAHQaXS@@R@@RqSSaYS
;61    6.000E-02 cAQQ@QADS@@R@@RAISAFS
;61    8.000E-02 B@QQAQD`R@@R@@RUARTaR
;61    1.000E-01 qFQAIQRYR@@R@@RBdRrPR
;61    1.500E-01 fRPACQxFQ@@R@@RAARyIQ
;61    2.000E-01 ScPyRPsSQ@@R@@RU@QtQQ
;61    3.000E-01 AePhVPaBQ@@R@@RbGQBIQ
;61    4.000E-01 AGPGePuQP@@R@@RAVQqFQ
;61    5.000E-01 VdOgAPcEP@@R@@RQBQAEQ
;61    6.000E-01 DgOvPPBIP@@R@@RiHPxYP
;61    8.000E-01 rXOUaPAIP@@R@@RgHPG@P
;61    1.000E+00 qYOuCPFbO@@R@@RVIPFAP
;61    1.022E+00 qQOeGPVRO@@R@@RV@PUcP
;61    1.250E+00 QEOtWPDPOBQN@@RuEPeCP
;61    1.500E+00 HANtDPSDOQBO@@RDePtWP
;61    2.000E+00 TRNsQPQaOcSO@@RtAPdFP
;61    2.044E+00 tCNcVPAdOCgO@@RdHPdCP
;61    3.000E+00 BANRbPAAOHcOABMSbPS`P
;61    4.000E+00 QCNBTPfTNqCPTFMCePCdP
;61    5.000E+00 gFMR@PT`NqQPhIMChPChP
;61    6.000E+00 EDMAfPCfNBDPaGNSfPSeP
;61    7.000E+00 sPMaWPSGNrCPqQNDFPDFP
;61    8.000E+00 BdMQRPbYNbPPRDNTGPTGP
;61    9.000E+00 bDMAPPrCNBcPRTNdHPdHP
;61    1.000E+01 AbMq@PBENCEPRcNDPPDPP
;61    1.100E+01 QPMaAPAcNcEPcINTRPTQP
;61    1.200E+01 aFMQCPaVNCTPcTNdSPdSP
;61    1.300E+01 AGMAGPQQNcRPSfNtTPtTP
;61    1.400E+01 iFLAAPqINsXPdGNDePDdP
;61    1.500E+01 HGLiPOaHNScPTVNTePTeP
;61    1.600E+01 GILYDOQINDGPDdNEDPEDP
;61    1.800E+01 ePLxEOAENtBPuENeBPeBP
;61    2.000E+01 TTLwPOyAMTTPEaNuHPuHP
;61    2.200E+01 sULWFOxIMtUPfDNUTPUTP
;61    2.400E+01 SELfYOgSMTcPfSNeXPeXP
;61    2.600E+01 bYLfHOViMU@PViNEaPEaP
;61    2.800E+01 rBLUbOFVMeFPwBNUcPUcP
;61    3.000E+01 BBLeQOF@MEPPgSNFEPFEP
;61    4.000E+01 QCLDUODRMUhPXaNVRPVRP
;61    5.000E+01 gFKsROSPMFRPIhNFiPFiP
;61    6.000E+01 EEKcAOBiMvUPAFOWHPWHP
;61    8.000E+01 BdKRSOREMgEPQHOgRPgRP
;61    1.000E+02 AbKR@OqQMgPPaFOWdPWdP
;61    1.500E+02 HGJAYOQCMXFPAPOHUPHUP
;61    2.000E+02 TTJQGOHWLXPPAYOxWPxWP
;61    3.000E+02 BBJx@NeRLHhPQYOYCPYCP
;61    4.000E+02 QCJVPNdALYAPaUOyDPyDP
;61    5.000E+02 gFIuHNsFLiFPqPOIXPIXP
;61    6.000E+02 EEIdQNB`LyFPqSOYXPYXP
;61    8.000E+02 BdIcPNR@LYPPqWOyRPyRP
;61    1.000E+03 AbIRfNaXLiPPA`OIaPIaP
;61    1.500E+03 HGHBGNQBLyRPAeOYcPYcP
;61    2.000E+03 TTHaPNxHKI`PAgOA@QA@Q
;61    3.000E+03 BBHQANUYKIgPQ`OAAQAAQ
;61    4.000E+03 QCHXYMTIKYbPQaOAAQAAQ
;61    5.000E+03 gFGGBMsEKYdPQbOAAQAAQ
;61    6.000E+03 EEGUfMrYKYfPQcOABQABQ
;61    8.000E+03 BdGTYMBIKYiPQdOABQABQ
;61    1.000E+04 AbGsUMaWKA@QQeOABQABQ
;61    1.500E+04 HGFRYMQBKA@QQeOABQABQ
;61    2.000E+04 TTFQiMxGJA@QQfOABQABQ
;61    3.000E+04 BBFqGMUXJA@QQfOABQABQ
;61    4.000E+04 QCFAEMTHJA@QQgOABQABQ
;61    5.000E+04 gFEXXLsEJAAQQgOACQACQ
;61    6.000E+04 EEEgFLrYJAAQQgOACQACQ
;61    8.000E+04 BdEUVLBIJAAQQgOACQACQ
;61    1.000E+05 AbETSLaWJAAQQgOACQACQ
;==== ELEMENT  62
;62    1.000E-03 iWRUdOR@U@@R@@RRAUR@U
;62    1.039E-03 iSRfDOQeU@@R@@RQfUQeU
;62    1.080E-03 YYRVVOAbU@@R@@RAcUAbU
;62 M5 1.080E-03 YYRVVORQU@@R@@RRRURQU
;62    1.093E-03 YXRfVOsTU@@R@@RsUUsTU
;62    1.106E-03 YVRvVOUUU@@R@@RUVUUUU
;62 M4 1.106E-03 YVRvVOvXU@@R@@RvYUvXU
;62    1.253E-03 ISRGfOUgU@@R@@RUhUUgU
;62    1.420E-03 iFRIHOeEU@@R@@ReFUeEU
;62 M3 1.420E-03 iFRIHOFFU@@R@@RFGUFFU
;62    1.500E-03 YGRiWOuEU@@R@@RuFUuEU
;62    1.541E-03 YCRYhOEDU@@R@@REEUEDU
;62 M2 1.541E-03 YCRYhOuEU@@R@@RuFUuEU
;62    1.629E-03 IDRAFPtSU@@R@@RtTUtSU
;62    1.723E-03 XdRQCPTHU@@R@@RTIUTHU
;62 M1 1.723E-03 XdRQCPtGU@@R@@RtHUtGU
;62    2.000E-03 hVRqCPSAU@@R@@RSBUSAU
;62    3.000E-03 gVRBDPQIU@@R@@RQIUQIU
;62    4.000E-03 vTRrPPE`T@@R@@REgTE`T
;62    5.000E-03 UcRs@Ps@T@@R@@RsFTs@T
;62    6.000E-03 eERCdPBFT@@R@@RRBTBFT
;62    6.716E-03 DcRTIPQTT@@R@@RQYTQTT
;62 L3 6.716E-03 DcRTIPdIT@@R@@RtCTdIT
;62    7.008E-03 dWRtBPCeT@@R@@RCiTCeT
;62    7.312E-03 TRRDUPCUT@@R@@RSPTCUT
;62 L2 7.312E-03 TRRDUPtQT@@R@@RtVTtQT
;62    7.521E-03 DQRTUPtIT@@R@@RDTTtIT
;62    7.737E-03 tARdTPT@T@@R@@RTDTT@T
;62 L1 7.737E-03 tARdTPtST@@R@@RtXTtST
;62    8.000E-03 TIRtUPtFT@@R@@RDPTtFT
;62    1.000E-02 CQRUQPBVT@@R@@RRPTBWT
;62    1.500E-02 bBRGCPHPS@@R@@RhSSHQS
;62    2.000E-02 QYRXCPCfS@@R@@RDCSCgS
;62    3.000E-02 y@QIUPaFS@@R@@RqGSaGS
;62    4.000E-02 FEQAAQeVR@@R@@RvFRuVR
;62    4.683E-02 tUQADQcSR@@R@@RdARsSR
;62 K  4.683E-02 tUQADQBDS@@R@@RR@SBES
;62    5.000E-02 dIQAEQqRS@@R@@RqWSqSS
;62    6.000E-02 cCQAGQAFS@@R@@RQASAGS
;62    8.000E-02 BAQAHQT`R@@R@@ReAREAR
;62    1.000E-01 qGQAGQbVR@@R@@RR`RrVR
;62    1.500E-01 fVPAAQXYQ@@R@@RACRiPQ
;62    2.000E-01 SfPYQPCeQ@@R@@RUIQD`Q
;62    3.000E-01 AfPHWPaFQ@@R@@Rr@QRAQ
;62    4.000E-01 AHPgXPU`P@@R@@RAWQqFQ
;62    5.000E-01 G@OGFPsFP@@R@@RQAQADQ
;62    6.000E-01 TbOVVPRGP@@R@@RiBPxSP
;62    8.000E-01 B`OuYPQCP@@R@@Rg@PVbP
;62    1.000E+00 AaOeBPGGO@@R@@RVAPUcP
;62    1.022E+00 qSOUFPvVO@@R@@RFAPEdP
;62    1.250E+00 QFOdWPTVOBSN@@ReGPUEP
;62    1.500E+00 HINdEPcFOQBO@@RtWPdYP
;62    2.000E+00 TWNcSPQhOcUO@@RdDPTIP
;62    2.044E+00 tGNSYPQaOChO@@RdAPTGP
;62    3.000E+00 BCNBfPADOHbOA@MCgPCeP
;62    4.000E+00 QENrIPFhNqCPDHMC`PsYP
;62    5.000E+00 wCMBFPEGNqQPXAMCcPCcP
;62    6.000E+00 EIMAbPSiNBCPaDNSaPSaP
;62    7.000E+00 sTMaTPcHNrBPaXNDAPDAP
;62    8.000E+00 BfMAYPrXNRXPBINTBPTBP
;62    9.000E+00 bFMqGPBQNBbPBYNdDPdDP
;62    1.000E+01 AcMaGPRBNCCPBgNtEPtEP
;62    1.100E+01 QRMQHPQ`NcCPcCNDWPDWP
;62    1.200E+01 aGMQAPqQNCRPSVNTXPTXP
;62    1.300E+01 AHMAEPQVNSYPChNdYPdYP
;62    1.400E+01 yFLY`OATNsUPTHND`PD`P
;62    1.500E+01 XELIPOqCNS`PDWNT`PT`P
;62    1.600E+01 WFLXeOaCNDDPtTNTiPTiP
;62    1.800E+01 eVLXHOAHNdIPeDNUGPUGP
;62    2.000E+01 TYLWUOiRMTQPeYNuDPuCP
;62    2.200E+01 sYLGAOhWMtQPVANEYPEXP
;62    2.400E+01 SHLVUOGhMT`PFYNeSPeSP
;62    2.600E+01 rQLVEOgCMEGPFdNuVPuVP
;62    2.800E+01 rDLE`OfWMeBPWGNEhPEhP
;62    3.000E+01 BDLUPOf@MuFPGWNUiPUiP
;62    4.000E+01 QELtGOTWMUdPxRNFWPFWP
;62    5.000E+01 wCKcTOcRMvGPiWNFdPFdP
;62    6.000E+01 EIKSDORiMvPPADOWCPWCP
;62    8.000E+01 BgKBXObBMg@PQEOWVPWVP
;62    1.000E+02 AcKBFOqWMWUPaDOGhPGhP
;62    1.500E+02 XEJAVOQGMX@PqGOxIPxIP
;62    2.000E+02 TYJQEOxULHSPAUOxPPxPP
;62    3.000E+02 BDJXCNEaLHbPQVOIFPIFP
;62    4.000E+02 QEJvGNtELIDPaROiGPiGP
;62    5.000E+02 wCIeGNCWLYIPaVOIQPIQP
;62    6.000E+02 EIITQNBiLiIPaYOYQPYQP
;62    8.000E+02 BgISSNRGLISPqSOiTPiTP
;62    1.000E+03 AcIR`NqSLYRPqVOySPySP
;62    1.500E+03 XEHBCNQELiUPA`OIePIeP
;62    2.000E+03 TYHQWNhVKyRPAcOYbPYbP
;62    3.000E+03 BDHAINuWKI`PAeOYiPYiP
;62    4.000E+03 QEHHQMtCKIdPAgOA@QA@Q
;62    5.000E+03 wCGFhMCVKIfPAhOAAQAAQ
;62    6.000E+03 EIGEdMBhKIhPAhOAAQAAQ
;62    8.000E+03 BgGDYMRFKY`PAiOAAQAAQ
;62    1.000E+04 AcGcWMqSKYbPQ`OAAQAAQ
;62    1.500E+04 XEFRTMQEKYdPQaOAAQAAQ
;62    2.000E+04 TYFQeMhUJYePQaOAAQAAQ
;62    3.000E+04 BDFqDMuWJYfPQbOABQABQ
;62    4.000E+04 QEFACMtCJYgPQbOABQABQ
;62    5.000E+04 wCEHQLCVJYgPQbOABQABQ
;62    6.000E+04 EIEWALBhJYhPQbOABQABQ
;62    8.000E+04 BgEEULRFJYhPQbOABQABQ
;62    1.000E+05 AcEDSLqSJYhPQbOABQABQ
;==== ELEMENT  63
;63    1.000E-03 Y`REaObAU@@R@@RbBUbAU
;63    1.063E-03 IdRfIOQgU@@R@@RQhUQgU
;63    1.131E-03 yWRFaOqUU@@R@@RqVUqUU
;63 M5 1.131E-03 yWRFaOrCU@@R@@RrDUrCU
;63    1.146E-03 yURVbOSSU@@R@@RSTUSSU
;63    1.161E-03 yTRGCOuEU@@R@@RuFUuEU
;63 M4 1.161E-03 yTRGCOFWU@@R@@RFXUFWU
;63    1.311E-03 YYRXBOuPU@@R@@RuQUuPU
;63    1.481E-03 IRRyEOEBU@@R@@RECUEBU
;63 M3 1.481E-03 IRRyEOE`U@@R@@REaUE`U
;63    1.500E-03 IPRIYOeRU@@R@@ReRUeRU
;63    1.614E-03 iHRACPtVU@@R@@RtWUtVU
;63 M2 1.614E-03 iHRACPEFU@@R@@REGUEFU
;63    1.704E-03 i@RQ@PDXU@@R@@RDYUDXU
;63    1.800E-03 Y@RQGPSgU@@R@@RShUSgU
;63 M1 1.800E-03 Y@RQGPTEU@@R@@RTFUTEU
;63    2.000E-03 HiRqAPcGU@@R@@RcHUcGU
;63    3.000E-03 GhRBBPaEU@@R@@RaFUaEU
;63    4.000E-03 VdRbWPVBT@@R@@RVITVBT
;63    5.000E-03 VBRcFPCXT@@R@@RSTTCXT
;63    6.000E-03 ERRC`PRHT@@R@@RbCTRHT
;63    6.977E-03 DdRdFPAWT@@R@@RQRTAWT
;63 L3 6.977E-03 DdRdFPDHT@@R@@RTCTDHT
;63    7.290E-03 dWRDQPcTT@@R@@RcYTcTT
;63    7.617E-03 TQRTUPcFT@@R@@Rs@TcFT
;63 L2 7.617E-03 TQRTUPDUT@@R@@RDYTDUT
;63    8.000E-03 tBRtPPSeT@@R@@RSiTSeT
;63    8.052E-03 t@RtSPChT@@R@@RSbTChT
;63 L1 8.052E-03 t@RtSPDXT@@R@@RTSTDXT
;63    1.000E-02 SRREWPRYT@@R@@RbSTRYT
;63    1.500E-02 bHRViPHeS@@R@@RIISHfS
;63    2.000E-02 aSRX@PDGS@@R@@RdDSDHS
;63    3.000E-02 YYQITPqDS@@R@@RATSqDS
;63    4.000E-02 fEQAAQUiR@@R@@RvRRFIR
;63    4.852E-02 dTQAEQCXR@@R@@RDERSYR
;63 K  4.852E-02 dTQAEQQdS@@R@@RB@SQeS
;63    5.000E-02 DSQAEQA`S@@R@@RAeSAaS
;63    6.000E-02 sCQAGQQAS@@R@@RQFSQBS
;63    8.000E-02 BHQAHQUDR@@R@@REVReER
;63    1.000E-01 AQQAGQrYR@@R@@RCDRR`R
;63    1.500E-01 FhPABQIFQ@@R@@RAHRAAR
;63    2.000E-01 DIPYUPDFQ@@R@@RESQEBQ
;63    3.000E-01 QcPXQPqDQ@@R@@RrHQRIQ
;63    4.000E-01 QAPwRPfFP@@R@@RQQQAPQ
;63    5.000E-01 gEOW@PSWP@@R@@RQDQAGQ
;63    6.000E-01 EIOVYPr@P@@R@@RIPPHiP
;63    8.000E-01 R`OEbPa@P@@R@@RwAPGBP
;63    1.000E+00 AgOeEPWRO@@R@@RVIPF@P
;63    1.022E+00 qYOUIPg@O@@R@@RFIPUaP
;63    1.250E+00 a@OtPPDeORRN@@RuCPeAP
;63    1.500E+00 xHNdHPCVOQFO@@RDbPtTP
;63    2.000E+00 tSNcUPR@OsUO@@RdHPdDP
;63    2.044E+00 TSNcQPBCOD@O@@RdEPdAP
;63    3.000E+00 RANBhPQAOIDOAAMSaPCiP
;63    4.000E+00 QINBPPwANqFPT@MCePCcP
;63    5.000E+00 gPMBGPuINqTPXFMCiPChP
;63    6.000E+00 eGMAcPdDNBGPaENSgPSfP
;63    7.000E+00 ChMaUPCXNrGPaXNDGPDGP
;63    8.000E+00 RgMQPPReNbSPR@NTIPTHP
;63    9.000E+00 rEMqHPRVNBgPRPNt@Pt@P
;63    1.000E+01 Q`MaHPbENCIPBhNDRPDRP
;63    1.100E+01 QWMQIPBANs@PcDNTTPTTP
;63    1.200E+01 qBMQBPAbNCXPSXNdVPdUP
;63    1.300E+01 QBMAEPaVNcVPS`NtWPtWP
;63    1.400E+01 iYLYeOQRNCbPd@NDhPDhP
;63    1.500E+01 HTLIUOAQNSgPDYNThPThP
;63    1.600E+01 GRLI@OqANTBPtVNEHPEHP
;63    1.800E+01 EgLhCOQENtGPeFNeFPeFP
;63    2.000E+01 tULWXOABNdPPuRNERPERP
;63    2.200E+01 ScLGEOYIMD`PVDNUXPUXP
;63    2.400E+01 s@LVYOxFMTiPVRNuSPuSP
;63    2.600E+01 BaLVHOgVMUFPFhNEfPEfP
;63    2.800E+01 BRLEcOGGMuBPg@NUhPUhP
;63    3.000E+01 RALUROVWMEVPWQNV@PV@P
;63    4.000E+01 QILtIODdMFEPxVNVXPVXP
;63    5.000E+01 gPKcVOCcMFYPyQNVfPVfP
;63    6.000E+01 eHKSFOSGMFcPAEOgFPgFP
;63    8.000E+01 RgKBYOrFMwCPQFOwPPwPP
;63    1.000E+02 Q`KBGOAgMgYPaDOHBPHBP
;63    1.500E+02 HTJAWOaDMhEPqGOXTPXTP
;63    2.000E+02 tUJQEOiGLXYPAVOHePHeP
;63    3.000E+02 RAJXHNVELXhPQVOiBPiBP
;63    4.000E+02 QIJFPNdQLiAPaROISPISP
;63    5.000E+02 gPIu@NcXLyEPaVOYWPYWP
;63    6.000E+02 eHITTNCGLIVPaYOiWPiWP
;63    8.000E+02 RgISUNr@LiPPqTOIaPIaP
;63    1.000E+03 Q`IRbNAdLiYPqVOY`PY`P
;63    1.500E+03 HTHBDNaBLIbPAaOA@QA@Q
;63    2.000E+03 tUHQXNYGKY`PAcOAAQAAQ
;63    3.000E+03 RAHQ@NVAKYgPAfOABQABQ
;63    4.000E+03 QIHHVMTYKA@QAgOABQABQ
;63    5.000E+03 gPGVbMcWKA@QAhOABQABQ
;63    6.000E+03 eHGEgMCFKAAQAiOACQACQ
;63    8.000E+03 RgGTRMbIKAAQQ`OACQACQ
;63    1.000E+04 Q`GcYMAcKAAQQ`OACQACQ
;63    1.500E+04 HTFRUMaBKAAQQaOACQACQ
;63    2.000E+04 tUFQfMYFJAAQQbOACQACQ
;63    3.000E+04 RAFqEMVAJAAQQbOACQACQ
;63    4.000E+04 QIFADMTXJAAQQbOACQACQ
;63    5.000E+04 gPEHULcVJABQQbOACQACQ
;63    6.000E+04 eHEWELCEJABQQbOACQACQ
;63    8.000E+04 RgEEXLbIJABQQcOACQACQ
;63    1.000E+05 Q`EDVLAcJABQQcOADQADQ
;==== ELEMENT  64
;64    1.000E-03 IhReSObHU@@R@@RbIUbHU
;64    1.089E-03 yYRfIOQeU@@R@@RQfUQeU
;64    1.185E-03 iYRGBOaVU@@R@@RaWUaVU
;64 M5 1.185E-03 iYRGBOAcU@@R@@RAdUAcU
;64    1.201E-03 iXRWDOrQU@@R@@RrRUrQU
;64    1.217E-03 iVRgFOSiU@@R@@RD@USiU
;64 M4 1.217E-03 iVRgFODaU@@R@@RDbUDaU
;64    1.500E-03 yHRiHOECU@@R@@REDUECU
;64    1.544E-03 yCRiPOdYU@@R@@RtPUdYU
;64 M3 1.544E-03 yCRiPOERU@@R@@RESUERU
;64    1.615E-03 iERAAPDiU@@R@@RT`UDiU
;64    1.688E-03 YHRAFPDQU@@R@@RDRUDQU
;64 M2 1.688E-03 YHRAFPdXU@@R@@RdYUdXU
;64    1.782E-03 IHRQCPTEU@@R@@RTFUTEU
;64    1.881E-03 XhRa@PcXU@@R@@RcYUcXU
;64 M1 1.881E-03 XhRa@PCdU@@R@@RCeUCdU
;64    2.000E-03 HeRaHPsEU@@R@@RsFUsEU
;64    3.000E-03 GeRQfPaHU@@R@@RaIUaHU
;64    4.000E-03 VbRbPPvAT@@R@@RvHTvAT
;64    5.000E-03 VARSHPSYT@@R@@RcUTSYT
;64    6.000E-03 ERRsQPbET@@R@@RrATbET
;64    7.243E-03 tPRdHPqHT@@R@@RASTqHT
;64 L3 7.243E-03 tPRdHPC`T@@R@@RCdTC`T
;64    7.579E-03 TSRDSPsHT@@R@@RCRTsHT
;64    7.930E-03 tFRTWPCAT@@R@@RCETCAT
;64 L2 7.930E-03 tFRTWPT@T@@R@@RTETT@T
;64    8.000E-03 tCRdPPDBT@@R@@RDGTDCT
;64    8.376E-03 TFRtUPSYT@@R@@RcSTSYT
;64 L1 8.376E-03 TFRtUPTET@@R@@RTITTET
;64    1.000E-02 SSRuEPbVT@@R@@RbYTbVT
;64    1.500E-02 bIRFbPY@S@@R@@RyDSYAS
;64    2.000E-02 aTRWaPTIS@@R@@RtFSd@S
;64    3.000E-02 iTQiCPqHS@@R@@RAXSqIS
;64    4.000E-02 fHQYcPVIR@@R@@RVbRfIR
;64    5.000E-02 DUQACQsAR@@R@@RCfRCQR
;64    5.024E-02 DRQACQcGR@@R@@RCaRsGR
;64 K  5.024E-02 DRQACQAaS@@R@@RAfSAbS
;64    6.000E-02 sDQAEQQCS@@R@@RQHSQDS
;64    8.000E-02 BIQAFQeFR@@R@@RUWRuFR
;64    1.000E-01 ARQAEQBfR@@R@@RSARRgR
;64    1.500E-01 VdPYfPyAQ@@R@@RQ@RACR
;64    2.000E-01 TCPyGPTIQ@@R@@RUTQUBQ
;64    3.000E-01 QePxEPqHQ@@R@@RBQQbBQ
;64    4.000E-01 QCPWXPFXP@@R@@RQRQAQQ
;64    5.000E-01 wBOVgPcYP@@R@@RQDQAGQ
;64    6.000E-01 UDOFWPrHP@@R@@RyGPHfP
;64    8.000E-01 RcOuQPaEP@@R@@RgEPVfP
;64    1.000E+00 AiOUEPG`O@@R@@RVBPUcP
;64    1.022E+00 AaOEIPGVO@@R@@RFBPEdP
;64    1.250E+00 aBOdQPECORTN@@ReFPUDP
;64    1.500E+00 HWNd@PSYOQGO@@RtVPdWP
;64    2.000E+00 tXNSXPRHOsWO@@RdCPTHP
;64    2.044E+00 TXNSTPR@ODBO@@Rd@PTEP
;64    3.000E+00 RCNBbPQEOIDOIgLCfPCdP
;64    4.000E+00 a@NrFPWXNqEPDBMC`PsYP
;64    5.000E+00 gXMBDPUXNqTPHAMCdPCdP
;64    6.000E+00 uCMA`PtINBGPaCNScPSbP
;64    7.000E+00 SbMaRPcQNrFPaUNDCPDCP
;64    8.000E+00 C@MAWPCENbRPBFNTEPTDP
;64    9.000E+00 rGMqEPbUNBfPBVNdGPdFP
;64    1.000E+01 QbMaEPrCNCHPBcNtHPtHP
;64    1.100E+01 QYMQGPBHNcHPSHNTPPTPP
;64    1.200E+01 qCMQ@PAhNCWPSQNdRPdRP
;64    1.300E+01 QDMACPqQNcTPCcNtSPtSP
;64    1.400E+01 I`LyWOQWNC`PTBNDdPDdP
;64    1.500E+01 XTLiHOAVNSfPDPNTdPTdP
;64    1.600E+01 WQLHdOqENDIPdWNEDPEDP
;64    1.800E+01 UcLHHOQINtEPUGNeBPeBP
;64    2.000E+01 D`LGUOAFNTWPeQNuHPuHP
;64    2.200E+01 SgLVbOYQMtXPFBNUTPUTP
;64    2.400E+01 sDLFVOhTMTfPFPNeXPeXP
;64    2.600E+01 BdLFGOWbMUDPvUNEbPEbP
;64    2.800E+01 BULuSOwAMeIPGGNUdPUdP
;64    3.000E+01 RCLEROvYMESPwFNFFPFFP
;64    4.000E+01 a@LtAOEAMFBPhPNVTPVTP
;64    5.000E+01 gYKcPOSfMFUPYRNVaPVaP
;64    6.000E+01 uDKS@OcHMvYPACOgAPgAP
;64    8.000E+01 C@KBTOBSMgIPQDOgUPgUP
;64    1.000E+02 QbKBCOQdMgTPaBOWgPWgP
;64    1.500E+02 XTJATOaHMhAPqEOHYPHYP
;64    2.000E+02 D`JQCOYXLXTPASOH`PH`P
;64    3.000E+02 RCJHCNvFLXcPQSOYFPYFP
;64    4.000E+02 a@JfINtVLYEPQYOyHPyHP
;64    5.000E+02 gYIe@NCaLy@PaSOYQPYQP
;64    6.000E+02 uCIDVNSGLIPPaVOiQPiQP
;64    8.000E+02 C@ICXNrGLYTPqPOyTPyTP
;64    1.000E+03 QbIBfNQ`LiSPqSOIcPIcP
;64    1.500E+03 XTHB@NaFLyVPqWOYfPYfP
;64    2.000E+03 D`HQUNIXKIcPqYOA@QA@Q
;64    3.000E+03 RCHAHNvBKYaPAbOAAQAAQ
;64    4.000E+03 a@HxAMtTKYePAcOAAQAAQ
;64    5.000E+03 gYGvYMsYKYhPAdOABQABQ
;64    6.000E+03 uCGuVMSFKA@QAeOABQABQ
;64    8.000E+03 C@GDTMrGKA@QAfOABQABQ
;64    1.000E+04 QbGcRMAiKA@QAfOABQABQ
;64    1.500E+04 XTFRPMaFKAAQAgOABQABQ
;64    2.000E+04 D`FQbMIWJAAQAhOACQACQ
;64    3.000E+04 RCFqCMvBJAAQAhOACQACQ
;64    4.000E+04 a@FABMtTJAAQAhOACQACQ
;64    5.000E+04 gYEx@LsYJAAQAhOACQACQ
;64    6.000E+04 uCEGBLSFJAAQAiOACQACQ
;64    8.000E+04 C@EuHLrGJAAQAiOACQACQ
;64    1.000E+05 QbEtHLAiJAAQAiOACQACQ
;==== ELEMENT  65
;65    1.000E-03 AASUQOrIU@@R@@RBPUrIU
;65    1.114E-03 YhRFPOQdU@@R@@RQeUQdU
;65    1.241E-03 IhRgIOQXU@@R@@RQYUQXU
;65 M5 1.241E-03 IhRgIObHU@@R@@RbIUbHU
;65    1.258E-03 IgRGPOsFU@@R@@RsGUsFU
;65    1.275E-03 IfRWPOTfU@@R@@RTgUTfU
;65 M4 1.275E-03 IfRWPOEgU@@R@@REhUEgU
;65    1.500E-03 iSRYBOuAU@@R@@RuAUuAU
;65    1.611E-03 YQRYaODVU@@R@@RDWUDVU
;65 M3 1.611E-03 YQRYaOUEU@@R@@RUFUUEU
;65    1.688E-03 ITRADPdSU@@R@@RdTUdSU
;65    1.768E-03 yGRQ@PTFU@@R@@RTGUTFU
;65 M2 1.768E-03 yGRQ@PDQU@@R@@RDRUDQU
;65    1.865E-03 iGRQGPSaU@@R@@RSbUSaU
;65    1.967E-03 YFRaDPCWU@@R@@RCXUCWU
;65 M1 1.967E-03 YFRaDPcSU@@R@@RcTUcSU
;65    2.000E-03 YCRaFPSPU@@R@@RSQUSPU
;65    3.000E-03 XCRQcPqEU@@R@@RqEUqEU
;65    4.000E-03 WHRRVPfRT@@R@@RvPTfRT
;65    5.000E-03 vERSEPsWT@@R@@RCdTsWT
;65    6.000E-03 eSRcWPrGT@@R@@RBRTrGT
;65    7.514E-03 tTRtGPqBT@@R@@RqGTqBT
;65 L3 7.514E-03 tTRtGPcQT@@R@@RcVTcQT
;65    8.000E-03 DYRTWPCIT@@R@@RSCTCIT
;65    8.252E-03 tHRdWPBdT@@R@@RBhTBdT
;65 L2 8.252E-03 tHRdWPChT@@R@@RSbTChT
;65    8.477E-03 dGRtVPcST@@R@@RcWTcST
;65    8.708E-03 TGRDePCPT@@R@@RCTTCPT
;65 L1 8.708E-03 TGRDePScT@@R@@RSgTScT
;65    1.000E-02 cVRuBPrXT@@R@@RBbTrXT
;65    1.500E-02 rGRF`PYVS@@R@@RI`SYVS
;65    2.000E-02 aYRGiPDQS@@R@@RTYSDRS
;65    3.000E-02 YeQiCPAUS@@R@@RQVSAVS
;65    4.000E-02 FXQYdPVTR@@R@@RgIRfTR
;65    5.000E-02 TYQACQSPR@@R@@RDFRcPR
;65    5.200E-02 tBQADQSDR@@R@@RcWRcDR
;65 K  5.200E-02 tBQADQqRS@@R@@RqXSqSS
;65    6.000E-02 CUQAEQQHS@@R@@RaBSQIS
;65    8.000E-02 RFQAFQUPR@@R@@REbReQR
;65    1.000E-01 AWQAEQC@R@@R@@RcERSAR
;65    1.500E-01 WGPA@QyYQ@@R@@RQERAHR
;65    2.000E-01 dGPIQPDQQ@@R@@RuWQuEQ
;65    3.000E-01 BAPxIPAVQ@@R@@RRPQr@Q
;65    4.000E-01 QFPgQPFeP@@R@@RQVQAUQ
;65    5.000E-01 WXOG@PSaP@@R@@RQGQAIQ
;65    6.000E-01 uBOVPPRSP@@R@@RYVPICP
;65    8.000E-01 CDOuTPqBP@@R@@RwGPGFP
;65    1.000E+00 QfOUGPhHO@@R@@Rf@PF@P
;65    1.022E+00 AgOUBPWbO@@R@@RV@PUaP
;65    1.250E+00 aFOdSPuDObRN@@RuBPUIP
;65    1.500E+00 xXNdBPCaOaAO@@RDaPtRP
;65    2.000E+00 TeNcPPrAOChO@@RdGPdBP
;65    2.044E+00 tTNSVPbCOTCO@@RdDPTIP
;65    3.000E+00 bANBdPaBOiFOYbLSaPCiP
;65    4.000E+00 aDNrGPHCNqHPDDMCePCdP
;65    5.000E+00 WfMBEPUaNqWPHDMCiPCiP
;65    6.000E+00 URMAaPdUNRAPaCNShPShP
;65    7.000E+00 DFMaRPCbNBPPaVNDIPDHP
;65    8.000E+00 SAMAXPcCNbWPBGNdAPd@P
;65    9.000E+00 BVMqFPB`NRaPBWNtCPtBP
;65    1.000E+01 QiMaFPBWNSCPBdNDUPDUP
;65    1.100E+01 aTMQHPb@NsDPc@NTWPTWP
;65    1.200E+01 qHMQ@PQiNSSPSSNdYPdYP
;65    1.300E+01 QHMADPAaNsQPCeND`PD`P
;65    1.400E+01 ABMIbOaWNCgPTDNTaPTaP
;65    1.500E+01 HeLyCOQTNDBPDSNEBPEBP
;65    1.600E+01 wXLHhOASNTGPdYNUBPUBP
;65    1.800E+01 VDLXBOaENDSPUINu@Pu@P
;65    2.000E+01 ThLGXOQBNdUPeTNEWPEWP
;65    2.200E+01 TALVeOAANDfPFENeSPeSP
;65    2.400E+01 CVLFYOYDMEEPFSNuWPuWP
;65    2.600E+01 RdLV@OxHMeCPvXNUaPUaP
;65    2.800E+01 RTLuVOwTMuHPW@NFDPFDP
;65    3.000E+01 bALEUOWHMUSPGPNVEPVEP
;65    4.000E+01 aDLtCOeIMVCPhSNfUPfUP
;65    5.000E+01 WfKcQOTIMVWPYVNGCPGCP
;65    6.000E+01 USKSAOCVMVaPACOwCPwCP
;65    8.000E+01 SAKBVORWMGRPQDOwXPwXP
;65    1.000E+02 QiKBDOBEMwXPaBOXAPXAP
;65    1.500E+02 HeJAUOqFMxEPqEOhSPhSP
;65    2.000E+02 ThJQDOAAMhYPASOXePXeP
;65    3.000E+02 bAJHGNvSLIHPQSOyBPyBP
;65    4.000E+02 aDJvBNEDLyAPQYOYSPYSP
;65    5.000E+02 WfIeCNDBLIVPaSOiWPiWP
;65    6.000E+02 USIDXNsELYVPaVOyXPyXP
;65    8.000E+02 SAISPNRQLyPPqQOYaPYaP
;65    1.000E+03 QiIBhNBALI`PqSOA@QA@Q
;65    1.500E+03 HeHBANqDLYcPqWOAAQAAQ
;65    2.000E+03 ThHQVNA@LA@QA`OABQABQ
;65    3.000E+03 bAHAHNfXKAAQAbOACQACQ
;65    4.000E+03 aDHxEMEAKAAQAdOACQACQ
;65    5.000E+03 WfGFbMDAKAAQAeOACQACQ
;65    6.000E+03 USGuYMsDKABQAeOADQADQ
;65    8.000E+03 SAGDVMRPKABQAfOADQADQ
;65    1.000E+04 QiGcTMB@KABQAgOADQADQ
;65    1.500E+04 HeFRRMqCKABQAhOADQADQ
;65    2.000E+04 ThFQcMA@KABQAhOADQADQ
;65    3.000E+04 bAFqCMfWJABQAhOADQADQ
;65    4.000E+04 aDFABMEAJACQAiOADQADQ
;65    5.000E+04 WfExDLDAJACQAiOADQADQ
;65    6.000E+04 USEGELsDJACQAiOADQADQ
;65    8.000E+04 SAEEPLRPJACQAiOAEQAEQ
;65    1.000E+05 QiEDPLB@JACQAiOAEQAEQ
;==== ELEMENT  66
;66    1.000E-03 ABSeGOBXU@@R@@RBYUBXU
;66    1.138E-03 AASf@OQdU@@R@@RQeUQdU
;66    1.295E-03 YdRgGOQQU@@R@@RQRUQQU
;66 M5 1.295E-03 YdRgGORAU@@R@@RRBURAU
;66    1.314E-03 YbRGPOSEU@@R@@RSFUSEU
;66    1.333E-03 Y`RWSOdXU@@R@@RdYUdXU
;66 M4 1.333E-03 Y`RWSOUPU@@R@@RUQUUPU
;66    1.500E-03 yTRhWOUTU@@R@@RUUUUTU
;66    1.676E-03 YVRIfOdBU@@R@@RdCUdBU
;66 M3 1.676E-03 YVRIfODhU@@R@@RDiUDhU
;66    1.757E-03 IYRADPtGU@@R@@RtHUtGU
;66    1.842E-03 IQRQ@PSaU@@R@@RSbUSaU
;66 M2 1.842E-03 IQRQ@PTFU@@R@@RTGUTFU
;66    2.000E-03 iDRa@PCVU@@R@@RCWUCVU
;66    2.047E-03 YIRaCPcHU@@R@@RcIUcHU
;66 M1 2.047E-03 YIRaCPCRU@@R@@RCSUCRU
;66    3.000E-03 hERAfPAPU@@R@@RAPUAPU
;66    4.000E-03 w@RBXPFhT@@R@@RVeTFhT
;66    5.000E-03 FVRCEPSbT@@R@@RSiTSbT
;66    6.000E-03 uSRSVPBVT@@R@@RRRTBVT
;66    7.790E-03 dYRtFPaET@@R@@Rq@TaET
;66 L3 7.790E-03 dYRtFPsIT@@R@@RCTTsIT
;66    8.000E-03 TXRDUPcBT@@R@@RcGTcBT
;66    8.581E-03 tARdXPbUT@@R@@RbYTbUT
;66 L2 8.581E-03 tARdXPcST@@R@@RcWTcST
;66    8.810E-03 dARtVPCPT@@R@@RCTTCPT
;66    9.046E-03 TARDePSIT@@R@@RcCTSIT
;66 L1 9.046E-03 TARDePcXT@@R@@RsSTcXT
;66    1.000E-02 sSRUIPBfT@@R@@RR`TBfT
;66    1.500E-02 BQRfWPYbS@@R@@RABTYbS
;66    2.000E-02 qRRwVPTXS@@R@@RtVSTYS
;66    3.000E-02 AARYAPQQS@@R@@RaSSQRS
;66    4.000E-02 fQQIcPFbR@@R@@RWXRVbR
;66    5.000E-02 dXQABQcVR@@R@@RdCRsVR
;66    5.379E-02 TGQACQRhR@@R@@RSPRCHR
;66 K  5.379E-02 TGQACQaRS@@R@@RaXSaSS
;66    6.000E-02 SQQADQaAS@@R@@RaFSaBS
;66    8.000E-02 b@QAEQeYR@@R@@RFARuYR
;66    1.000E-01 QPQAEQSAR@@R@@RsFRcAR
;66    1.500E-01 wAPYbPABR@@R@@RQIRQBR
;66    2.000E-01 tEPyCPTXQ@@R@@RUeQURQ
;66    3.000E-01 BEPxCPQRQ@@R@@RRVQrEQ
;66    4.000E-01 QIPWUPWFP@@R@@RQYQAWQ
;66    5.000E-01 wUOVePDIP@@R@@RQHQQ@Q
;66    6.000E-01 ETOFVPbTP@@R@@RiTPY@P
;66    8.000E-01 S@OuPPqHP@@R@@RwIPGHP
;66    1.000E+00 B@OUDPhWO@@R@@Rf@PF@P
;66    1.022E+00 QbOEHPx@O@@R@@RVAPUaP
;66    1.250E+00 aIOdPPUYObWN@@RuBPUIP
;66    1.500E+00 XgNTIPSiOaDO@@RD`PtQP
;66    2.000E+00 EGNSXPBROSeO@@RdFPdAP
;66    2.044E+00 DeNSSPrDOd@O@@RdDPTIP
;66    3.000E+00 bFNBbPaHOyFOIeLS`PChP
;66    4.000E+00 aGNrEPHPNAPPDAMCePCdP
;66    5.000E+00 XDMBCPVHNqYPWiMS`PCiP
;66    6.000E+00 eUMA`PDfNRBPaCNSiPShP
;66    7.000E+00 TEMaQPSiNBRPaUNT@PDIP
;66    8.000E+00 SHMAWPsHNbYPBFNdAPdAP
;66    9.000E+00 RQMqEPRcNRcPBUNtDPtCP
;66    1.000E+01 BDMaEPRXNSEPBbNDVPDVP
;66    1.100E+01 aXMQGPr@NsFPSGNTXPTXP
;66    1.200E+01 AQMAIPBHNSUPSQNtPPtPP
;66    1.300E+01 a@MACPQ`NsSPCbNDbPDbP
;66    1.400E+01 ADMyUOqTNCiPTANTcPTcP
;66    1.500E+01 IELiFOaQNDEPDPNECPECP
;66    1.600E+01 WeLHbOQPNTIPdVNUDPUCP
;66    1.800E+01 fILHFOqANDUPUENuBPuBP
;66    2.000E+01 EILGSOQGNdXPePNEYPEYP
;66    2.200E+01 dALV`OAENDiPFANeUPeUP
;66    2.400E+01 STLFUOYUMEHPvHNE`PE`P
;66    2.600E+01 CALFFOxUMeFPvSNUdPUdP
;66    2.800E+01 bPLuQOHHMEQPGDNFFPFFP
;66    3.000E+01 bFLEQOWPMUVPwDNVHPVHP
;66    4.000E+01 aGLt@OUSMVFPXWNfXPfXP
;66    5.000E+01 XEKSYOtGMfPPIYNGFPGFP
;66    6.000E+01 eVKCIOcRMVePABOwGPwGP
;66    8.000E+01 SHKBTObYMGVPQCOGbPGbP
;66    1.000E+02 BDKBBORDMGbPaAOXEPXEP
;66    1.500E+02 IEJATOAQMxIPqDOhWPhWP
;66    2.000E+02 EIJQCOAFMxSPAROXiPXiP
;66    3.000E+02 bFJHANGBLYCPQROyFPyFP
;66    4.000E+02 aGJfGNeFLyEPQXOYWPYWP
;66    5.000E+02 XEIe@Nd@LYPPaROyRPyRP
;66    6.000E+02 eVIDUNSPLiQPaUOIbPIbP
;66    8.000E+02 SHICWNbRLyUPaYOYePYeP
;66    1.000E+03 BDIBfNBILIdPqROA@QA@Q
;66    1.500E+03 IEHB@NAPLYgPqVOABQABQ
;66    2.000E+03 EIHQTNAELA@QqXOABQABQ
;66    3.000E+03 bFHAGNVgKAAQA`OACQACQ
;66    4.000E+03 aGHhIMeCKABQAbOADQADQ
;66    5.000E+03 XEGvXMTHKABQAcOADQADQ
;66    6.000E+03 eVGuUMCYKABQAcOADQADQ
;66    8.000E+03 SHGDSMbQKABQAdOADQADQ
;66    1.000E+04 BDGcRMBIKABQAeOADQADQ
;66    1.500E+04 IEFRPMqIKACQAeOAEQAEQ
;66    2.000E+04 EIFQbMAEKACQAfOAEQAEQ
;66    3.000E+04 bFFqBMVgJACQAfOAEQAEQ
;66    4.000E+04 aGFABMeCJACQAgOAEQAEQ
;66    5.000E+04 XEEhHLTHJACQAgOAEQAEQ
;66    6.000E+04 eVEG@LCXJACQAgOAEQAEQ
;66    8.000E+04 SHEuGLbQJACQAgOAEQAEQ
;66    1.000E+05 BDEtGLBIJACQAgOAEQAEQ
;==== ELEMENT  67
;67    1.000E-03 ADSUDObQU@@R@@RbRUbQU
;67    1.162E-03 ABSfBOQeU@@R@@RQfUQeU
;67    1.351E-03 A@SGYOAUU@@R@@RAVUAUU
;67 M5 1.351E-03 A@SGYOBFU@@R@@RBGUBFU
;67    1.371E-03 A@SgROCDU@@R@@RCEUCDU
;67    1.392E-03 YiRwUODXU@@R@@RDYUDXU
;67 M4 1.392E-03 YiRwUOUHU@@R@@RUIUUHU
;67    1.500E-03 IhRHXOEdU@@R@@REeUEdU
;67    1.741E-03 iTRAAPDCU@@R@@RDDUDCU
;67 M3 1.741E-03 iTRAAPdWU@@R@@RdXUdWU
;67    1.830E-03 YURAFPTEU@@R@@RTFUTEU
;67    1.923E-03 IURQCPsPU@@R@@RsPUsPU
;67 M2 1.923E-03 IURQCPScU@@R@@RScUScU
;67    2.000E-03 yGRQHPSXU@@R@@RSYUSXU
;67    2.128E-03 iCRaFPSBU@@R@@RSCUSBU
;67 M1 2.128E-03 iCRaFPcFU@@R@@RcGUcFU
;67    3.000E-03 xFRAcPAVU@@R@@RAVUAVU
;67    4.000E-03 GRRBTPWIT@@R@@RgFTWIT
;67    5.000E-03 VWRC@PT@T@@R@@RTGTT@T
;67    6.000E-03 EdRSQPRXT@@R@@RbTTRXT
;67    8.000E-03 dXRtIPaBT@@R@@RaGTaBT
;67    8.071E-03 dTRDRPa@T@@R@@RaDTa@T
;67 L3 8.071E-03 dTRDRPcDT@@R@@RcHTcDT
;67    8.484E-03 DTRTXPBdT@@R@@RBiTBdT
;67    8.918E-03 dERtUPRPT@@R@@RRTTRPT
;67 L2 8.918E-03 dERtUPCRT@@R@@RCVTCRT
;67    9.153E-03 TERDcPcAT@@R@@RcETcAT
;67    9.394E-03 DERTbPCAT@@R@@RCETCAT
;67 L1 9.394E-03 DERTbPCXT@@R@@RSRTCXT
;67    1.000E-02 CaRUCPRgT@@R@@RCATRgT
;67    1.500E-02 BWRfPPADT@@R@@RAFTADT
;67    2.000E-02 qURgYPD`S@@R@@RThSDaS
;67    3.000E-02 ADRIGPQYS@@R@@RqPSaPS
;67    4.000E-02 vWQyYPWFR@@R@@RWdRgFR
;67    5.000E-02 tYQABQCdR@@R@@RDSRSeR
;67    5.562E-02 DEQACQBeR@@R@@RsFRRfR
;67 K  5.562E-02 DEQACQQTS@@R@@RQYSQUS
;67    6.000E-02 cPQADQaFS@@R@@RqASaGS
;67    8.000E-02 bEQAEQUaR@@R@@RfDRFBR
;67    1.000E-01 QTQADQcCR@@R@@RCYRsDR
;67    1.500E-01 WPPYaPAFR@@R@@RaDRQFR
;67    2.000E-01 DWPyCPD`Q@@R@@RVHQuSQ
;67    3.000E-01 RAPxBPaPQ@@R@@RbTQBSQ
;67    4.000E-01 aBPWUPWRP@@R@@RaSQQQQ
;67    5.000E-01 WfOVdPt@P@@R@@Ra@QQBQ
;67    6.000E-01 UYOFVPrXP@@R@@RI`PiDP
;67    8.000E-01 SIOuPPAVP@@R@@RGXPWFP
;67    1.000E+00 BFOUDPYDO@@R@@RfFPFEP
;67    1.022E+00 QgOEHPxUO@@R@@RVEPUfP
;67    1.250E+00 qCOdPPU`OrTN@@RuEPeBP
;67    1.500E+00 iCNTIPdAOaGO@@RDcPtTP
;67    2.000E+00 eANSXPRUODDO@@RdIPdDP
;67    2.044E+00 TiNSSPBVOt@O@@RdFPdAP
;67    3.000E+00 rBNBbPqDOYTOIeLScPSaP
;67    4.000E+00 qANrEPHdNARPDBMChPCfP
;67    5.000E+00 xHMBCPVQNAbPWiMScPSbP
;67    6.000E+00 EbMA`PUBNRFPaCNDBPDAP
;67    7.000E+00 dHMaQPd@NBVPaUNTCPTCP
;67    8.000E+00 cGMAWPSVNrRPBFNdEPdEP
;67    9.000E+00 RYMqEPCHNRgPBUNtHPtHP
;67    1.000E+01 R@MaEPrQNSIPBbNTPPTPP
;67    1.100E+01 qSMQGPBRNCPPSGNdSPdSP
;67    1.200E+01 AVMAIPRINcPPSQNtUPtUP
;67    1.300E+01 aDMACPQiNsXPCbNDgPDfP
;67    1.400E+01 AGMyVOAcNSdPTANThPThP
;67    1.500E+01 yALiFOaYNT@PtINEIPEIP
;67    1.600E+01 XILHbOQWNdEPdVNUIPUIP
;67    1.800E+01 FWLHFOqHNTQPUENuHPuHP
;67    2.000E+01 eDLGSOaCNtTPePNUUPUUP
;67    2.200E+01 tCLV`OQ@NTePF@NuQPuQP
;67    2.400E+01 cTLFUOA@NUDPvHNEfPEfP
;67    2.600E+01 S@LFFOi@MuBPvRNF@PF@P
;67    2.800E+01 bWLuQOHYMEXPGDNVCPVCP
;67    3.000E+01 rCLEQOGiMeSPwDNfEPfEP
;67    4.000E+01 qALt@OEaMfDPXVNvVPvVP
;67    5.000E+01 xHKSYOdPMfYPIXNWDPWDP
;67    6.000E+01 EbKCIOC`MGDPABOGUPGUP
;67    8.000E+01 cHKBTOBbMWUPQCOWaPWaP
;67    1.000E+02 R@KBBObEMWbPaAOhDPhDP
;67    1.500E+02 yBJATOAYMHYPqDOxWPxWP
;67    2.000E+02 eDJQCOQAMHdPAROIIPIIP
;67    3.000E+02 rCJHANwHLiDPQROIWPIWP
;67    4.000E+02 qAJfHNURLIVPQXOiYPiYP
;67    5.000E+02 xHIe@NDQLiQPaQOIcPIcP
;67    6.000E+02 EbIDUNcWLyRPaTOYcPYcP
;67    8.000E+02 cHICWNrULIfPaXOAAQAAQ
;67    1.000E+03 R@IBfNb@LYfPqQOABQABQ
;67    1.500E+03 yBHB@NAWLAAQqUOACQACQ
;67    2.000E+03 eDHQTNQ@LABQqWOADQADQ
;67    3.000E+03 rCHAGNwBKABQA`OADQADQ
;67    4.000E+03 qAHhIMEYKACQAaOAEQAEQ
;67    5.000E+03 xHGvXMtIKACQAbOAEQAEQ
;67    6.000E+03 EbGuUMcVKACQAcOAEQAEQ
;67    8.000E+03 cHGDSMrUKADQAdOAEQAEQ
;67    1.000E+04 R@GcRMb@KADQAdOAFQAFQ
;67    1.500E+04 yBFRPMAVKADQAeOAFQAFQ
;67    2.000E+04 eDFQbMQ@KADQAeOAFQAFQ
;67    3.000E+04 rCFqCMwBJADQAfOAFQAFQ
;67    4.000E+04 qAFABMEYJADQAfOAFQAFQ
;67    5.000E+04 xHEhHLtIJADQAfOAFQAFQ
;67    6.000E+04 EbEG@LcVJADQAfOAFQAFQ
;67    8.000E+04 cHEuGLrTJADQAfOAFQAFQ
;67    1.000E+05 R@EtGLb@JADQAfOAFQAFQ
;==== ELEMENT  68
;68    1.000E-03 AFSEBOrTU@@R@@RrUUrTU
;68    1.187E-03 ADSfDOQfU@@R@@RQgUQfU
;68    1.409E-03 ABSwPOAPU@@R@@RAQUAPU
;68 M5 1.409E-03 ABSwPOBCU@@R@@RBDUBCU
;68    1.431E-03 ABSGdORfU@@R@@RRgURfU
;68    1.453E-03 AASWiOtBU@@R@@RtCUtBU
;68 M4 1.453E-03 AASWiODfU@@R@@RDgUDfU
;68    1.500E-03 AASx@OFFU@@R@@RFGUFFU
;68    1.812E-03 I`RACPCcU@@R@@RCdUCcU
;68 M3 1.812E-03 I`RACPDTU@@R@@RDUUDTU
;68    2.000E-03 iQRQEPSQU@@R@@RSRUSQU
;68    2.006E-03 iPRQFPCYU@@R@@RSPUCYU
;68 M2 2.006E-03 iPRQFPsQU@@R@@RsRUsQU
;68    2.104E-03 IYRaBPsCU@@R@@RsDUsCU
;68    2.206E-03 yIRaIPRiU@@R@@RC@URiU
;68 M1 2.206E-03 yIRaIPSBU@@R@@RSCUSBU
;68    3.000E-03 hQRqYPQRU@@R@@RQSUQRU
;68    4.000E-03 gVRBPPWQT@@R@@RWYTWQT
;68    5.000E-03 vYRRePdIT@@R@@RtFTdIT
;68    6.000E-03 FDRCVPrPT@@R@@RrVTrPT
;68    8.000E-03 DcRtCPaHT@@R@@RqCTaHT
;68    8.358E-03 dURDXPQDT@@R@@RQITQDT
;68 L3 8.358E-03 dURDXPCHT@@R@@RSCTCHT
;68    8.799E-03 DTRdUPbYT@@R@@RrTTbYT
;68    9.264E-03 dDRDbPrET@@R@@RBPTrFT
;68 L2 9.264E-03 dDRDbPcCT@@R@@RcGTcCT
;68    9.505E-03 TCRT`PCCT@@R@@RCGTCCT
;68    9.751E-03 DCRTiPBeT@@R@@RBiTBeT
;68 L1 9.751E-03 DCRTiPcIT@@R@@RsCTcIT
;68    1.000E-02 SdREGPCIT@@R@@RSCTCIT
;68    1.500E-02 RTRVTPAHT@@R@@RQATAHT
;68    2.000E-02 A`RgSPEBS@@R@@Re@SEBS
;68    3.000E-02 AFRIBPaVS@@R@@RqXSaWS
;68    4.000E-02 VfQyVPWRR@@R@@RxARgRR
;68    5.000E-02 TbQABQDDR@@R@@RdSRTDR
;68    5.749E-02 SeQACQrSR@@R@@RcCRBdR
;68 K  5.749E-02 SeQACQAVS@@R@@RQQSAWS
;68    6.000E-02 cYQADQqAS@@R@@RqFSqBS
;68    8.000E-02 rBQAEQVDR@@R@@RFXRfER
;68    1.000E-01 QXQADQsGR@@R@@RcSRCWR
;68    1.500E-01 wRPYaPQAR@@R@@RaIRaAR
;68    2.000E-01 TYPyCPEBQ@@R@@RFQQUfQ
;68    3.000E-01 RGPxBPaWQ@@R@@RrRQRQQ
;68    4.000E-01 aFPWUPW`P@@R@@RaWQQUQ
;68    5.000E-01 h@OVePTSP@@R@@RaCQQEQ
;68    6.000E-01 uVOFVPRcP@@R@@RYgPyIP
;68    8.000E-01 cIOuPPQTP@@R@@RWWPgDP
;68    1.000E+00 RBOUDPiTO@@R@@RvBPVAP
;68    1.022E+00 BCOEIPiBO@@R@@RfAPFAP
;68    1.250E+00 qGOdPPfBOBbN@@RuIPeEP
;68    1.500E+00 YRNTIPDTOqAO@@RDfPtWP
;68    2.000E+00 uHNSXPbYOTDO@@RtBPdFP
;68    2.044E+00 UENSTPRYODPO@@RdIPdDP
;68    3.000E+00 BPNBbPAQOyROIeLSfPScP
;68    4.000E+00 qENrEPyANATPDBMSaPCiP
;68    5.000E+00 hTMBCPFeNAdPWiMSfPSeP
;68    6.000E+00 F@MA`PuHNRIPaCNDFPDEP
;68    7.000E+00 DQMaRPDRNBYPaUNTGPTGP
;68    8.000E+00 sHMAWPsTNrVPBFNdIPdIP
;68    9.000E+00 bWMqEPcDNCAPBUNDRPDRP
;68    1.000E+01 RFMaEPBeNcDPBbNTUPTUP
;68    1.100E+01 qYMQGPRUNCUPSHNdXPdWP
;68    1.200E+01 QPMQ@Pr@NcTPSQND`PD`P
;68    1.300E+01 aHMACPR@NCcPCbNTbPTbP
;68    1.400E+01 Q@MyVOQbND@PTBNECPECP
;68    1.500E+01 iQLiGOqXNTEPDPNUDPUDP
;68    1.600E+01 HTLHbOaUNt@PdVNeEPeDP
;68    1.800E+01 fWLHGOAUNTWPUENETPETP
;68    2.000E+01 EPLGTOaIND`PePNeQPeQP
;68    2.200E+01 DWLVaOQFNEBPFANuXPuXP
;68    2.400E+01 sULFVOAENeAPvHNUcPUcP
;68    2.600E+01 c@LFFOiWMuIPvSNFGPFGP
;68    2.800E+01 rVLuROXcMUUPGDNf@Pf@P
;68    3.000E+01 BPLEROhIMuPPwDNvCPvCP
;68    4.000E+01 qELt@OV@MvBPXVNFdPFdP
;68    5.000E+01 hUKSYODcMvWPIXNgCPgCP
;68    6.000E+01 FAKS@OSiMWCPABOWTPWTP
;68    8.000E+01 sHKBTORgMgUPQCOHAPHAP
;68    1.000E+02 RFKBCOrFMHBPaAOxDPxDP
;68    1.500E+02 iQJATOQVMhPPqDOHhPHhP
;68    2.000E+02 EPJQCOQGMXePAROi@Pi@P
;68    3.000E+02 BPJHBNwULyEPQQOYXPYXP
;68    4.000E+02 qEJfHNE`LYXPQWOI`PI`P
;68    5.000E+02 hUIe@NdSLySPaQOYePYeP
;68    6.000E+02 FAIDUNCfLIdPaTOA@QA@Q
;68    8.000E+02 sHICXNBiLYhPaXOABQABQ
;68    1.000E+03 RFIBfNrALAAQqQOACQACQ
;68    1.500E+03 iQHB@NQTLABQqUOADQADQ
;68    2.000E+03 EPHQUNQELACQqWOAEQAEQ
;68    3.000E+03 BPHAHNgYKADQqYOAFQAFQ
;68    4.000E+03 qEHx@MuWKADQAaOAFQAFQ
;68    5.000E+03 hUGvXMdRKADQAbOAFQAFQ
;68    6.000E+03 FAGuUMCeKADQAbOAFQAFQ
;68    8.000E+03 sHGDSMBhKAEQAcOAGQAGQ
;68    1.000E+04 RFGcRMrAKAEQAdOAGQAGQ
;68    1.500E+04 iQFRPMQTKAEQAdOAGQAGQ
;68    2.000E+04 EPFQbMQEKAEQAeOAGQAGQ
;68    3.000E+04 BPFqCMgYJAEQAeOAGQAGQ
;68    4.000E+04 qEFABMuWJAEQAeOAGQAGQ
;68    5.000E+04 hUEhILdQJAEQAfOAGQAGQ
;68    6.000E+04 FAEGALCeJAEQAfOAGQAGQ
;68    8.000E+04 sHEuGLBhJAEQAfOAGQAGQ
;68    1.000E+05 RFEtGLrAJAFQAfOAGQAGQ
;==== ELEMENT  69
;69    1.000E-03 AHSTbOBiU@@R@@RR`UBiU
;69    1.211E-03 AFSfFOQhU@@R@@RQiUQhU
;69    1.468E-03 ADSWdOqEU@@R@@RqFUqEU
;69 M5 1.468E-03 ADSWdOAdU@@R@@RAfUAdU
;69    1.500E-03 ACSXEOSbU@@R@@RSdUSbU
;69    1.515E-03 ACShDOTDU@@R@@RTEUTDU
;69 M4 1.515E-03 ACShDODbU@@R@@RDcUDbU
;69    1.689E-03 AASyEOd@U@@R@@RdAUd@U
;69    1.885E-03 YeRAFPcVU@@R@@RcWUcVU
;69 M3 1.885E-03 YeRAFPdDU@@R@@RdEUdDU
;69    2.000E-03 IcRQCPcXU@@R@@RcYUcXU
;69    2.090E-03 yTRQIPsBU@@R@@RsCUsBU
;69 M2 2.090E-03 yTRQIPSRU@@R@@RSSUSRU
;69    2.196E-03 iSRaFPSEU@@R@@RSFUSEU
;69    2.307E-03 YQRqCPBaU@@R@@RBbUBaU
;69 M1 2.307E-03 YQRqCPRdU@@R@@RReURdU
;69    3.000E-03 HcRqWPQXU@@R@@RQYUQXU
;69    4.000E-03 GgRrGPGgT@@R@@RWeTGgT
;69    5.000E-03 ViRRbPTPT@@R@@RTWTTPT
;69    6.000E-03 fBRCRPBcT@@R@@RBiTBcT
;69    8.000E-03 ThRt@PqET@@R@@RAPTqET
;69    8.648E-03 dURTUPQ@T@@R@@RQETQ@T
;69 L3 8.648E-03 dURTUPRdT@@R@@RRiTRdT
;69    9.120E-03 DSRtRPRVT@@R@@RbQTRVT
;69    9.617E-03 dBRT`PbCT@@R@@RbGTbCT
;69 L2 9.617E-03 dBRT`PCFT@@R@@RS@TCFT
;69    1.000E-02 DFRECPrYT@@R@@RBcTrYT
;69    1.012E-02 DAREGPrPT@@R@@RrTTrPT
;69 L1 1.012E-02 DAREGPSCT@@R@@RSGTSCT
;69    1.500E-02 bRRVPPQCT@@R@@RQFTQCT
;69    2.000E-02 AeRgPPeFS@@R@@REUSeGS
;69    3.000E-02 Q@RI@PqUS@@R@@RAgSqVS
;69    4.000E-02 WGQyVPWbR@@R@@RxTRHBR
;69    5.000E-02 EGQABQdFR@@R@@RDgRtFR
;69    5.939E-02 CfQADQbSR@@R@@RSBRrTR
;69 K  5.939E-02 CfQADQAQS@@R@@RAVSARS
;69    6.000E-02 C`QADQqFS@@R@@RAQSqGS
;69    8.000E-02 rIQAEQFPR@@R@@RvTRVPR
;69    1.000E-01 aSQAEQSQR@@R@@RsXRcRR
;69    1.500E-01 WfPYdPQFR@@R@@RqDRaFR
;69    2.000E-01 tTPyFPeGQ@@R@@RfXQfAQ
;69    3.000E-01 bDPxFPqVQ@@R@@RBbQbPQ
;69    4.000E-01 q@PWYPxCP@@R@@RqRQQYQ
;69    5.000E-01 HWOVhPtXP@@R@@RaFQQHQ
;69    6.000E-01 UeOFYPS@P@@R@@RABQYXP
;69    8.000E-01 CPOuSPaRP@@R@@RgYPwEP
;69    1.000E+00 RIOUGPABP@@R@@RFPPVHP
;69    1.022E+00 R@OUAPyUO@@R@@Rv@PFIP
;69    1.250E+00 AQOdSPVWOR`N@@REUPuAP
;69    1.500E+00 IeNdAPdYOqEO@@RTaPDaP
;69    2.000E+00 UVNcPPBdOdEO@@RtFPtAP
;69    2.044E+00 uBNSUPrTOTRO@@RtCPdHP
;69    3.000E+00 BXNBcPAYOYdOY`LD@PShP
;69    4.000E+00 APNrFPIcNAWPDDMSePSdP
;69    5.000E+00 XdMBDPgCNAhPHCMDAPD@P
;69    6.000E+00 fAMAaPeXNbCPaCNTAPT@P
;69    7.000E+00 TVMaRPdVNRTPaVNdCPdBP
;69    8.000E+00 CYMAXPSeNBaPBGNtEPtEP
;69    9.000E+00 rVMqFPCRNCFPBVNDXPDXP
;69    1.000E+01 bDMaFPCANcIPBdNdQPdQP
;69    1.100E+01 AeMQGPbYNSQPSINtTPtTP
;69    1.200E+01 QUMQ@PBSNsPPSRNDgPDfP
;69    1.300E+01 qBMADPbANCiPCdNTiPTiP
;69    1.400E+01 QDMIaOBCNDFPTCNUAPUAP
;69    1.500E+01 YdLyAOAhNdBPDQNeBPeAP
;69    1.600E+01 xSLHgOqTNtGPdXNuBPuBP
;69    1.800E+01 V`LXAOQSNdTPUGNURPURP
;69    2.000E+01 UYLGXOqFNDhPeRNuPPuPP
;69    2.200E+01 dRLVdOaBNU@PFCNEfPEfP
;69    2.400E+01 ChLFYOQANu@PFQNFBPFBP
;69    2.600E+01 sALFIOABNEXPvUNVFPVFP
;69    2.800E+01 BeLuUOIQMeTPGGNv@Pv@P
;69    3.000E+01 BXLETOxTME`PwGNFRPFRP
;69    4.000E+01 APLtBOFSMFRPXYNVePVdP
;69    5.000E+01 XdKcQOEIMFhPYRNwEPwDP
;69    6.000E+01 fAKSAOdAMgDPABOgVPgVP
;69    8.000E+01 CYKBUOSCMwWPQCOXDPXDP
;69    1.000E+02 bDKBDOBYMXEPaAOHXPHXP
;69    1.500E+02 YdJAUOaUMxTPqDOIBPIBP
;69    2.000E+02 UYJQDOaCMIIPAROyEPyEP
;69    3.000E+02 BXJHFNXFLYPPQROySPySP
;69    4.000E+02 APJvANVALySPQXOYePYeP
;69    5.000E+02 XdIeBNDhLIiPaROAAQAAQ
;69    6.000E+02 fAIDWNDGLA@QaTOABQABQ
;69    8.000E+02 CYICYNCELAAQaXOACQACQ
;69    1.000E+03 bDIBgNBTLABQqQOADQADQ
;69    1.500E+03 YdHBANaRLADQqUOAFQAFQ
;69    2.000E+03 UYHQUNaBLADQqWOAFQAFQ
;69    3.000E+03 BXHAHNXAKAEQA`OAGQAGQ
;69    4.000E+03 APHxCMFHKAFQAaOAHQAHQ
;69    5.000E+03 XdGFbMDfKAFQAbOAHQAHQ
;69    6.000E+03 fAGuXMDEKAFQAcOAHQAHQ
;69    8.000E+03 CYGDUMCDKAFQAcOAHQAHQ
;69    1.000E+04 bDGcTMBSKAGQAdOAHQAHQ
;69    1.500E+04 YdFRQMaRKAGQAeOAIQAIQ
;69    2.000E+04 UYFQcMaBKAGQAeOAIQAIQ
;69    3.000E+04 BXFqCMX@JAGQAeOAIQAIQ
;69    4.000E+04 APFABMFGJAGQAfOAIQAIQ
;69    5.000E+04 XdExCLDfJAGQAfOAIQAIQ
;69    6.000E+04 fAEGDLDEJAGQAfOAIQAIQ
;69    8.000E+04 CYEEPLCDJAGQAfOAIQAIQ
;69    1.000E+05 bDEtILBSJAGQAfOAIQAIQ
;==== ELEMENT  70
;70    1.000E-03 AHStUOCAU@@R@@RCBUCAU
;70    1.500E-03 ADSW`OqDU@@R@@RqEUqDU
;70    1.528E-03 ADSHGOaIU@@R@@Rq@UaIU
;70 M5 1.528E-03 ADSHGOQdU@@R@@RQeUQdU
;70    1.552E-03 ACShBOrUU@@R@@RrVUrUU
;70    1.576E-03 ACSxGOCiU@@R@@RS`UCiU
;70 M4 1.576E-03 ACSxGOTSU@@R@@RTTUTSU
;70    1.753E-03 AASIWOSgU@@R@@RShUSgU
;70    1.950E-03 YfRAGPCYU@@R@@RSPUCYU
;70 M3 1.950E-03 YfRAGPDDU@@R@@RDEUDDU
;70    2.000E-03 YaRQ@PsYU@@R@@RC`UsYU
;70    2.173E-03 ySRaAPSBU@@R@@RSCUSBU
;70 M2 2.173E-03 ySRaAPsAU@@R@@RsBUsAU
;70    2.283E-03 iRRaHPRfU@@R@@RRgURfU
;70    2.398E-03 YQRqEPbTU@@R@@RbUUbTU
;70 M1 2.398E-03 YQRqEPrVU@@R@@RrWUrVU
;70    3.000E-03 XcRqRPaSU@@R@@RaTUaSU
;70    4.000E-03 WfRr@PXAT@@R@@RXITXAT
;70    5.000E-03 GIRBePdUT@@R@@RtRTdUT
;70    6.000E-03 vARsDPRbT@@R@@RRiTRbT
;70    8.000E-03 EFRd@PqIT@@R@@RATTqIT
;70    8.944E-03 TYRTUPADT@@R@@RAITADT
;70 L3 8.944E-03 TYRTUPrXT@@R@@RBbTrXT
;70    9.447E-03 tDRtQPBQT@@R@@RBUTBQT
;70    9.978E-03 TCRTaPBIT@@R@@RRCTBIT
;70 L2 9.978E-03 TCRTaPRdT@@R@@RRhTRdT
;70    1.000E-02 TCRTbPR`T@@R@@RRdTR`T
;70    1.049E-02 SdREHPRTT@@R@@RRXTRTT
;70 L1 1.049E-02 SdREHPRcT@@R@@RRgTRcT
;70    1.500E-02 bVRvGPQGT@@R@@RQITQGT
;70    2.000E-02 AhRGVPESS@@R@@ReSSETS
;70    3.000E-02 QARHfPAaS@@R@@RQcSAbS
;70    4.000E-02 gHQiRPhBR@@R@@RIDRxAR
;70    5.000E-02 UEQA@QDRR@@R@@REDRTRR
;70    6.000E-02 CfQACQbVR@@R@@RSERrVR
;70    6.133E-02 sSQACQRPR@@R@@RRhRbPR
;70 K  6.133E-02 sSQACQqBS@@R@@RqGSqCS
;70    8.000E-02 BRQADQVVR@@R@@RVaRfWR
;70    1.000E-01 aVQACQcQR@@R@@RChRsRR
;70    1.500E-01 HIPIdPa@R@@R@@RqHRq@R
;70    2.000E-01 DbPiFPEUQ@@R@@RFfQvHQ
;70    3.000E-01 bHPhGPAcQ@@R@@RBhQbUQ
;70    4.000E-01 qBPWQPhUP@@R@@RqUQaRQ
;70    5.000E-01 hSOVaPTgP@@R@@RaGQQIQ
;70    6.000E-01 FGOFRPcBP@@R@@RACQiUP
;70    8.000E-01 CVOeWPaYP@@R@@RwQPwFP
;70    1.000E+00 bCOUAPAFP@@R@@RFPPVGP
;70    1.022E+00 RDOEFPABP@@R@@RfIPFHP
;70    1.250E+00 ATOTXPFeOReN@@RETPeIP
;70    1.500E+00 A@OTGPDiOqGO@@RT`PtYP
;70    2.000E+00 eWNSVPRfOtAO@@RtEPdIP
;70    2.044E+00 ESNSRPBfOTXO@@RtBPdFP
;70    3.000E+00 RSNBaPQVOA@PIaLSiPSfP
;70    4.000E+00 ARNrDPABOAXPD@MSdPScP
;70    5.000E+00 YAMBBPWRNAiPWeMD@PSiP
;70    6.000E+00 vCMqYPUaNbDPaBNT@PT@P
;70    7.000E+00 dUMaQPDeNRTPaTNdBPdBP
;70    8.000E+00 SVMAVPT@NBbPBENtEPtDP
;70    9.000E+00 BaMqDPSUNCGPBTNDXPDXP
;70    1.000E+01 bHMaEPSCNs@PBaNdQPdQP
;70    1.100E+01 AhMQFPrYNSRPSFNtTPtTP
;70    1.200E+01 QXMAIPRRNsQPCYNDgPDfP
;70    1.300E+01 qEMACPr@NS`PC`NTiPTiP
;70    1.400E+01 QFMyQORANDGPDINUAPUAP
;70    1.500E+01 AAMiBOQeNdCPtGNeBPeBP
;70    1.600E+01 XaLxXOAaNtHPdSNuCPuBP
;70    1.800E+01 GDLHCOQYNdUPUBNURPURP
;70    2.000E+01 uPLGPOAQNDiPUVNuPPuPP
;70    2.200E+01 tQLFhOaGNUAPUgNEgPEgP
;70    2.400E+01 SfLFROQFNu@PvDNFBPFBP
;70    2.600E+01 sGLFCOAFNEYPfXNVGPVGP
;70    2.800E+01 RaLeYOyWMeUPG@Nv@Pv@P
;70    3.000E+01 RSLuIOIGME`PgINFSPFSP
;70    4.000E+01 ARLdHOfXMFSPXQNVePVeP
;70    5.000E+01 YBKSWOeHMFiPIRNwEPwEP
;70    6.000E+01 vCKCHOtGMgFPAAOgWPgWP
;70    8.000E+01 SVKBSOcEMwYPQBOXDPXDP
;70    1.000E+02 bHKBBORXMXFPa@OHYPHYP
;70    1.500E+02 AAKATOqQMxUPqCOICPICP
;70    2.000E+02 uPJQCOaHMY@PAPOyFPyFP
;70    3.000E+02 RSJWhNHWLYQPQPOyTPyTP
;70    4.000E+02 ARJfENvDLyTPQVOYfPYfP
;70    5.000E+02 YBIUGNEGLY`PaPOAAQAAQ
;70    6.000E+02 vCIDSNdBLA@QaROABQABQ
;70    8.000E+02 SVICVNSFLABQaVOADQADQ
;70    1.000E+03 bHIBeNRSLABQaYOADQADQ
;70    1.500E+03 AAIQiNaXLADQqSOAFQAFQ
;70    2.000E+03 uPHQTNaFLAEQqUOAFQAFQ
;70    3.000E+03 RSHAGNHRKAEQqWOAGQAGQ
;70    4.000E+03 ARHhFMvAKAFQqYOAHQAHQ
;70    5.000E+03 YBGvUMEEKAFQA`OAHQAHQ
;70    6.000E+03 vCGuRMdAKAFQA`OAHQAHQ
;70    8.000E+03 SVGDQMSEKAFQAaOAHQAHQ
;70    1.000E+04 bHGcPMRRKAGQAbOAIQAIQ
;70    1.500E+04 AAGBYMaXKAGQAbOAIQAIQ
;70    2.000E+04 uPFQaMaFKAGQAcOAIQAIQ
;70    3.000E+04 RSFqBMHQJAGQAcOAIQAIQ
;70    4.000E+04 ARFAAMvAJAGQAcOAIQAIQ
;70    5.000E+04 YBEhELEEJAGQAcOAIQAIQ
;70    6.000E+04 vCEVgLd@JAGQAdOAIQAIQ
;70    8.000E+04 SVEuELSEJAGQAdOAIQAIQ
;70    1.000E+05 bHEtELRRJAGQAdOAIQAIQ
;==== ELEMENT  71
;71    1.000E-03 Q@StTOSHU@@R@@RSIUSHU
;71    1.500E-03 AFSWeOAQU@@R@@RARUAQU
;71    1.588E-03 AESXROaEU@@R@@RaFUaEU
;71 M5 1.588E-03 AESXROQVU@@R@@RQWUQVU
;71    1.614E-03 ADShWOr@U@@R@@RrAUr@U
;71    1.639E-03 ADSHcOCPU@@R@@RCQUCPU
;71 M4 1.639E-03 ADSHcOCiU@@R@@RS`UCiU
;71    2.000E-03 AASQ@PCTU@@R@@RCUUCTU
;71    2.024E-03 A@SQBPsDU@@R@@RsEUsDU
;71 M3 2.024E-03 A@SQBPChU@@R@@RCiUChU
;71    2.140E-03 YaRQIPsIU@@R@@RCPUsIU
;71    2.263E-03 yYRaGPRfU@@R@@RRgURfU
;71 M2 2.263E-03 yYRaGPSEU@@R@@RSFUSEU
;71    2.375E-03 iYRqDPBbU@@R@@RBcUBbU
;71    2.491E-03 YXRAQPRRU@@R@@RRSURRU
;71 M1 2.491E-03 YXRAQPbSU@@R@@RbTUbSU
;71    3.000E-03 IFRqQPqPU@@R@@RqQUqPU
;71    4.000E-03 X@RbIPHXT@@R@@RXVTHXT
;71    5.000E-03 gBRBcPDfT@@R@@RTdTDfT
;71    6.000E-03 FTRsBPCFT@@R@@RSCTCFT
;71    8.000E-03 UHRTHPAVT@@R@@RQQTAVT
;71    9.244E-03 TVRdUPA@T@@R@@RAETA@T
;71 L3 9.244E-03 TVRdUPbVT@@R@@RrPTbVT
;71    1.000E-02 dCRTaPRGT@@R@@RbATRGT
;71    1.035E-02 DIRECPQhT@@R@@RBBTQhT
;71 L2 1.035E-02 DIRECPrRT@@R@@RrVTrRT
;71    1.061E-02 SiRUAPRVT@@R@@RbPTRVT
;71    1.087E-02 CiRUIPBQT@@R@@RBUTBQT
;71 L1 1.087E-02 CiRUIPrYT@@R@@RBcTrYT
;71    1.500E-02 rSRvEPaBT@@R@@RaETaBT
;71    2.000E-02 QcRGSPeXS@@R@@REhSeYS
;71    3.000E-02 QDRHdPQ`S@@R@@RBBSQaS
;71    4.000E-02 GXQiQPhSR@@R@@RIWRxRR
;71    5.000E-02 eIQA@QdUR@@R@@ReHRtUR
;71    6.000E-02 SfQACQB`R@@R@@Rs@RR`R
;71    6.331E-02 cSQACQBQR@@R@@RBgRRQR
;71 K  6.331E-02 cSQACQaFS@@R@@RqASaGS
;71    8.000E-02 BYQADQFaR@@R@@RWFRVaR
;71    1.000E-01 qPQACQsVR@@R@@RDCRCfR
;71    1.500E-01 xBPIePaER@@R@@RASRqER
;71    2.000E-01 TfPiHPuQQ@@R@@RWCQfSQ
;71    3.000E-01 rEPhIPQbQ@@R@@RRhQrUQ
;71    4.000E-01 qFPWSPIIP@@R@@RA`QaVQ
;71    5.000E-01 HiOVcPeCP@@R@@Rq@QaBQ
;71    6.000E-01 fEOFTPsIP@@R@@RAEQIdP
;71    8.000E-01 SWOeYPqXP@@R@@RGcPGWP
;71    1.000E+00 r@OUCPQBP@@R@@RFXPfEP
;71    1.022E+00 bAOEHPAGP@@R@@RvGPVEP
;71    1.250E+00 AXOTYPgBOCCN@@RUPPuEP
;71    1.500E+00 ACOTHPUEOAQO@@RTdPDdP
;71    2.000E+00 EdNSWPSBODRO@@RtIPtCP
;71    2.044E+00 ePNSSPCAOdYO@@RtEPt@P
;71    3.000E+00 bQNBaPaTOABPIdLDCPD@P
;71    4.000E+00 AWNrEPAHOQQPDAMShPSgP
;71    5.000E+00 IPMBCPWbNQbPWhMDEPDDP
;71    6.000E+00 VSMqYPfBNbGPaBNTEPTDP
;71    7.000E+00 D`MaQPU@NRXPaUNdGPdFP
;71    8.000E+00 cWMAWPtBNBfPBFNDPPtIP
;71    9.000E+00 R`MqEPsSNSBPBUNTSPTSP
;71    1.000E+01 rEMaEPcINsEPBbNdVPdVP
;71    1.100E+01 QdMQGPRdNSWPSGNtYPtYP
;71    1.200E+01 aSMAIPbUNsWPSPNTcPTbP
;71    1.300E+01 qIMACPBRNSePCaNEEPEEP
;71    1.400E+01 a@MyTObBNTCPT@NUGPUGP
;71    1.500E+01 ADMiEOBENdIPtHNeHPeHP
;71    1.600E+01 YILHaOQ`NDTPdUNuIPuIP
;71    1.800E+01 gFLHEOaWNtRPUDNUYPUYP
;71    2.000E+01 EhLGROAXNTfPUXNuWPuWP
;71    2.200E+01 DfLV`OqDNUHPUiNUdPUdP
;71    2.400E+01 DHLFTOaANuHPvFNV@PV@P
;71    2.600E+01 CXLFEOQANUWPvPNfEPfEP
;71    2.800E+01 C@LuQOACNuSPGBNvIPvIP
;71    3.000E+01 bQLEQOYTMEiPwANVQPVQP
;71    4.000E+01 AWLt@OGBMVRPXSNGDPGDP
;71    5.000E+01 IQKSYOUUMViPITNGUPGUP
;71    6.000E+01 VSKCIOTYMwFPABOwWPwWP
;71    8.000E+01 cXKBTOCQMW`PQBOhFPhFP
;71    1.000E+02 rEKBBOrQMhHPa@OhPPhPP
;71    1.500E+02 AEKATOqYMHhPqCOYFPYFP
;71    2.000E+02 EhJQCOqDMiCPAQOIYPIYP
;71    3.000E+02 bQJHANX`LiTPQPOIhPIhP
;71    4.000E+02 AWJfGNfVLIhPQVOAAQAAQ
;71    5.000E+02 IQIUINuBLA@QaPOABQABQ
;71    6.000E+02 VSIDTNDSLAAQaSOADQADQ
;71    8.000E+02 cXICWNsBLACQaWOAEQAEQ
;71    1.000E+03 rEIBeNbVLADQaYOAFQAFQ
;71    1.500E+03 AEIQiNqWLAEQqSOAGQAGQ
;71    2.000E+03 EhHQTNqCLAFQqUOAHQAHQ
;71    3.000E+03 bQHAGNHdKAGQqXOAIQAIQ
;71    4.000E+03 AWHhHMfSKAGQqYOAIQAIQ
;71    5.000E+03 IQGvWMu@KAHQA`OAIQAIQ
;71    6.000E+03 VSGuTMDRKAHQA`OQ@QQ@Q
;71    8.000E+03 cXGDRMsAKAHQAaOQ@QQ@Q
;71    1.000E+04 rEGcQMbUKAHQAbOQ@QQ@Q
;71    1.500E+04 AEGRPMqWKAHQAbOQ@QQ@Q
;71    2.000E+04 EhFQbMqCKAHQAcOQ@QQ@Q
;71    3.000E+04 bQFqBMHdJAIQAcOQ@QQ@Q
;71    4.000E+04 AWFABMfSJAIQAcOQ@QQ@Q
;71    5.000E+04 IQEhGLu@JAIQAdOQAQQAQ
;71    6.000E+04 VSEViLDRJAIQAdOQAQQAQ
;71    8.000E+04 cXEuFLsAJAIQAdOQAQQAQ
;71    1.000E+05 rEEtFLbUJAIQAdOQAQQAQ
;==== ELEMENT  72
;72    1.000E-03 QASdROsBU@@R@@RsDUsBU
;72    1.500E-03 AGSGdOAXU@@R@@RAYUAXU
;72    1.662E-03 AESHeOQIU@@R@@Ra@UQIU
;72 M5 1.662E-03 AESHeOAXU@@R@@RAYUAXU
;72    1.689E-03 AESIBORHU@@R@@RRIURHU
;72    1.716E-03 ADSYIOcBU@@R@@RcCUcBU
;72 M4 1.716E-03 ADSYIOcSU@@R@@RcTUcSU
;72    2.000E-03 AASAIPSYU@@R@@RcPUSYU
;72    2.108E-03 A@SQFPSDU@@R@@RSEUSDU
;72 M3 2.108E-03 A@SQFPcUU@@R@@RcVUcUU
;72    2.233E-03 IiRaCPSGU@@R@@RSHUSGU
;72    2.365E-03 yVRqAPrVU@@R@@RrWUrVU
;72 M2 2.365E-03 yVRqAPRcU@@R@@RRdURcU
;72    2.480E-03 iURqHPbSU@@R@@RbTUbSU
;72    2.601E-03 YSRAUPrEU@@R@@RrFUrEU
;72 M1 2.601E-03 YSRAUPBVU@@R@@RBWUBVU
;72    3.000E-03 YBRaYPqVU@@R@@RqWUqVU
;72    4.000E-03 XFRbFPxXT@@R@@RHfTxXT
;72    5.000E-03 gHRrYPEDT@@R@@RUATEDT
;72    6.000E-03 VQRcHPSHT@@R@@RcDTSHT
;72    8.000E-03 eDRTDPQRT@@R@@RQWTQRT
;72    9.561E-03 DXRtQPYTS@@R@@RA@TYUS
;72 L3 9.561E-03 DXRtQPRQT@@R@@RRVTRQT
;72    1.000E-02 dIRDfPbFT@@R@@Rr@TbFT
;72    1.074E-02 D@REIPAeT@@R@@RAiTAeT
;72 L2 1.074E-02 D@REIPRUT@@R@@RRYTRUT
;72    1.100E-02 S`RUHPBPT@@R@@RBTTBPT
;72    1.127E-02 CaReFPbGT@@R@@RrATbGT
;72 L1 1.127E-02 CaReFPbRT@@R@@RbVTbRT
;72    1.500E-02 rWRfHPaFT@@R@@RaITaFT
;72    2.000E-02 QfRwDPEhS@@R@@RFISEiS
;72    3.000E-02 QFRxUPQgS@@R@@RR@SQhS
;72    4.000E-02 gPQYRPXgR@@R@@RIcRIGR
;72    5.000E-02 uGQYePDdR@@R@@REXRTdR
;72    6.000E-02 DCQABQRbR@@R@@RCRRCBR
;72    6.535E-02 SRQACQr@R@@R@@RrURBPR
;72 K  6.535E-02 SRQACQQIS@@R@@RaDSa@S
;72    8.000E-02 RSQACQViR@@R@@RwERW@R
;72    1.000E-01 qSQACQChR@@R@@RTERShR
;72    1.500E-01 HXPyXPaIR@@R@@RAXRqIR
;72    2.000E-01 EEPiBPUaQ@@R@@RwDQFcQ
;72    3.000E-01 rIPhDPQiQ@@R@@RCEQBbQ
;72    4.000E-01 qIPGXPIVP@@R@@RAcQaYQ
;72    5.000E-01 IGOFiPEUP@@R@@RqBQaCQ
;72    6.000E-01 vHOFPPSTP@@R@@RAFQYdP
;72    8.000E-01 cTOeUPAfP@@R@@RGhPWRP
;72    1.000E+00 rEOU@PQGP@@R@@RVPPfGP
;72    1.022E+00 bEOEDPQBP@@R@@RvIPVFP
;72    1.250E+00 QROTWPWTOCIN@@RUPPuEP
;72    1.500E+00 AFOTFPuGOATO@@RTdPDdP
;72    2.000E+00 UgNSUPcFODYO@@RtIPtCP
;72    2.044E+00 uRNSQPSDOtWO@@RtFPt@P
;72    3.000E+00 bVNB`PqQOACPyXLDCPD@P
;72    4.000E+00 QPNrCPQBOQRPShMSiPSgP
;72    5.000E+00 iQMBBPhFNQcPWcMDEPDDP
;72    6.000E+00 fWMqXPFXNbIPaBNTEPTEP
;72    7.000E+00 T`MaPPuBNbPPaTNdHPdGP
;72    8.000E+00 sVMAVPTPNBhPBDNDQPDQP
;72    9.000E+00 RgMqDPCiNSDPBSNTTPTTP
;72    1.000E+01 BPMaDPCSNsGPB`NdXPdXP
;72    1.100E+01 QiMQFPCFNSYPSENDaPDaP
;72    1.200E+01 aWMAIPrVNsYPCXNTdPTdP
;72    1.300E+01 ARMABPRRNShPsYNEGPEFP
;72    1.400E+01 aCMiYOrANTEPDHNUIPUIP
;72    1.500E+01 AGMi@ORCNtBPtFNu@Pu@P
;72    1.600E+01 yILxVOQhNDWPdRNEQPEQP
;72    1.800E+01 GRLH@OqTNtTPU@NeQPeQP
;72    2.000E+01 FALwHOQTNTiPUTNE`PE`P
;72    2.200E+01 TgLFfOqINeAPUeNUgPUgP
;72    2.400E+01 TGLFQOaFNEQPvBNVCPVBP
;72    2.600E+01 SVLFBOQFNUYPfVNfGPfGP
;72    2.800E+01 CGLeWOAGNuVPVgNFQPFQP
;72    3.000E+01 bWLuGOYcMUbPgFNVTPVTP
;72    4.000E+01 QPLdGOwAMVVPHWNGGPGGP
;72    5.000E+01 iRKSWOuXMGCPyGNGXPGXP
;72    6.000E+01 fXKCGOtXMGPPAAOGaPGaP
;72    8.000E+01 sVKBROSUMWdPQBOhIPhIP
;72    1.000E+02 BPKBAOBbMxBPQIOhTPhTP
;72    1.500E+02 AGKASOAgMXbPqBOYIPYIP
;72    2.000E+02 FAJQBOAPMiGPAPOYSPYSP
;72    3.000E+02 bWJWfNiFLiYPAYOYbPYbP
;72    4.000E+02 QPJfCNVdLYcPQUOAAQAAQ
;72    5.000E+02 iQIUFNUTLAAQQYOACQACQ
;72    6.000E+02 fXIDRNdRLABQaQOADQADQ
;72    8.000E+02 sVICUNCVLACQaUOAEQAEQ
;72    1.000E+03 BPIBdNrVLADQaXOAFQAFQ
;72    1.500E+03 AGIQhNAdLAFQqROAHQAHQ
;72    2.000E+03 FAHQSNqHLAFQqTOAHQAHQ
;72    3.000E+03 bWHAGNi@KAGQqVOAIQAIQ
;72    4.000E+03 QPHhCMV`KAHQqXOQ@QQ@Q
;72    5.000E+03 iQGvSMURKAHQqXOQ@QQ@Q
;72    6.000E+03 fXGuQMdPKAHQqYOQ@QQ@Q
;72    8.000E+03 sVGDPMCUKAHQA`OQ@QQ@Q
;72    1.000E+04 BPGSYMrVKAIQA`OQ@QQ@Q
;72    1.500E+04 AGGBXMAdKAIQAaOQAQQAQ
;72    2.000E+04 FAFQaMqHKAIQAaOQAQQAQ
;72    3.000E+04 bWFqBMYIJAIQAbOQAQQAQ
;72    4.000E+04 QPFAAMV`JAIQAbOQAQQAQ
;72    5.000E+04 iQEhCLURJAIQAbOQAQQAQ
;72    6.000E+04 fXEVeLdPJAIQAbOQAQQAQ
;72    8.000E+04 sVEuCLCUJAIQAbOQAQQAQ
;72    1.000E+05 BPEtDLrVJAIQAbOQAQQAQ
;==== ELEMENT  73
;73    1.000E-03 QCSDYOSPU@@R@@RSQUSPU
;73    1.500E-03 AHSwPOQVU@@R@@RQWUQVU
;73    1.735E-03 AFSYDOQDU@@R@@RQEUQDU
;73 M5 1.735E-03 AFSYDOqIU@@R@@RAPUqIU
;73    1.764E-03 AFSyAOBFU@@R@@RBGUBFU
;73    1.793E-03 AESIYOCCU@@R@@RCDUCCU
;73 M4 1.793E-03 AESIYOsGU@@R@@RsHUsGU
;73    2.000E-03 ACSAGPsVU@@R@@RsWUsVU
;73    2.194E-03 AASQIPRgU@@R@@RRiURhU
;73 M3 2.194E-03 AASQIPCUU@@R@@RCVUCUU
;73    2.327E-03 YeRaGPRiU@@R@@RC@URiU
;73    2.469E-03 IaRqFPRYU@@R@@RbPURYU
;73 M2 2.469E-03 IaRqFPrVU@@R@@RrWUrVU
;73    2.586E-03 iYRARPBXU@@R@@RBYUBXU
;73    2.708E-03 YURQPPbBU@@R@@RbCUbBU
;73 M1 2.708E-03 YURQPPrBU@@R@@RrCUrBU
;73    3.000E-03 iERaWPAcU@@R@@RAdUAcU
;73    4.000E-03 hGRbCPYDT@@R@@RiBTYDT
;73    5.000E-03 wIRrUPeFT@@R@@RuCTeFT
;73    6.000E-03 fQRcEPsBT@@R@@RsHTsBT
;73    8.000E-03 uDRTAPQYT@@R@@RaTTQYT
;73    9.881E-03 DSRtYPYES@@R@@RiPSYFS
;73 L3 9.881E-03 DSRtYPrIT@@R@@RBTTrIT
;73    1.000E-02 tHRDcPrCT@@R@@RrHTrDT
;73    1.114E-02 ScRUIPqUT@@R@@RqYTqUT
;73 L2 1.114E-02 ScRUIPBQT@@R@@RBUTBQT
;73    1.141E-02 CdReGPbGT@@R@@RrATbGT
;73    1.168E-02 sTRuFPRDT@@R@@RRHTRET
;73 L1 1.168E-02 sTRuFPBXT@@R@@RRRTBXT
;73    1.500E-02 BcRfDPqAT@@R@@RqDTqAT
;73    2.000E-02 B@Rw@PVCS@@R@@RvCSVCS
;73    3.000E-02 QHRxQPBFS@@R@@RRISBGS
;73    4.000E-02 wXQIYPyHR@@R@@RACSIXR
;73    5.000E-02 UPQYbPEGR@@R@@RuRRUGR
;73    6.000E-02 TBQABQCER@@R@@RSWRSFR
;73    6.742E-02 CRQABQbAR@@R@@RbURrAR
;73 K  6.742E-02 CRQABQQDS@@R@@RQHSQES
;73    8.000E-02 RYQACQgCR@@R@@RWYRwCR
;73    1.000E-01 qXQABQDBR@@R@@Rt@RTBR
;73    1.500E-01 hYPyWPqER@@R@@RQSRATR
;73    2.000E-01 UHPiAPVFQ@@R@@RgPQGHQ
;73    3.000E-01 BVPhDPBHQ@@R@@RSEQR`Q
;73    4.000E-01 ASPGXPY`P@@R@@RAhQqTQ
;73    5.000E-01 yAOFiPuQP@@R@@RqEQaFQ
;73    6.000E-01 VUOFPPsQP@@R@@RAHQAAQ
;73    8.000E-01 sTOeUPQeP@@R@@RWhPgQP
;73    1.000E+00 BROU@PaCP@@R@@RVWPvBP
;73    1.022E+00 rBOEEPQGP@@R@@RFUPfBP
;73    1.250E+00 QVOTWPWaOSFN@@RUTPuIP
;73    1.500E+00 AIOTFPeTOAXO@@RThPDgP
;73    2.000E+00 VDNSUPCROTYO@@RDQPtEP
;73    2.044E+00 EhNSQPs@ODhO@@RtHPtBP
;73    3.000E+00 rTNB`PqYOAEPyXLDFPDCP
;73    4.000E+00 QTNrCPQHOQUPSiMDBPD@P
;73    5.000E+00 IgMBBPhUNQfPWcMDHPDGP
;73    6.000E+00 FfMqXPvYNrBPaBNTIPTHP
;73    7.000E+00 EDMaPPUWNbSPaTNtAPtAP
;73    8.000E+00 CfMAVPtQNRbPBDNDUPDTP
;73    9.000E+00 CEMqDPDHNSGPBSNTXPTXP
;73    1.000E+01 BWMaDPSYNCQPB`NtRPtQP
;73    1.100E+01 BDMQFPc@NcSPSENDePDeP
;73    1.200E+01 qRMAIPBiNCcPCWNThPThP
;73    1.300E+01 AVMABPbSNDBPsXNUAPUAP
;73    1.400E+01 aFMiYOBRNd@PDHNeDPeCP
;73    1.500E+01 Q@Mi@ObCNtFPtENuEPuEP
;73    1.600E+01 iULxVOBHNTRPdRNEVPEVP
;73    1.800E+01 gRLH@OAbND`PU@NeWPeWP
;73    2.000E+01 VHLwHOaRNEDPUTNEePEeP
;73    2.200E+01 UALFfOAVNeGPUdNFCPFBP
;73    2.400E+01 dILFQOqBNEWPvANVIPVIP
;73    2.600E+01 cULFBOaANeUPfUNvDPvCP
;73    2.800E+01 SELeWOQBNEcPVgNFXPFXP
;73    3.000E+01 rULuGOADNUhPgFNfPPfPP
;73    4.000E+01 QTLdGOgUMfSPHVNWEPWEP
;73    5.000E+01 IhKSVOFEMWAPyGNWVPWVP
;73    6.000E+01 FfKCGOE@MGXPAAOGiPGiP
;73    8.000E+01 CfKBROsQMHBPQAOxHPxHP
;73    1.000E+02 BWKBAOReMHQPQIOxSPxSP
;73    1.500E+02 Q@KASOQeMIAPqBOiIPiIP
;73    2.000E+02 VHJQBOAVMyHPqIOiSPiSP
;73    3.000E+02 rUJWfNiYLyYPAYOA@QA@Q
;73    4.000E+02 QTJfCNgFLA@QQTOABQABQ
;73    5.000E+02 IhIUFNE`LABQQXOADQADQ
;73    6.000E+02 FfIDRNDcLACQaQOAEQAEQ
;73    8.000E+02 CfICUNcRLAEQaUOAGQAGQ
;73    1.000E+03 BWIBdNBiLAEQaXOAGQAGQ
;73    1.500E+03 Q@IQhNQcLAGQqQOAIQAIQ
;73    2.000E+03 VHHQSNATLAHQqSOAIQAIQ
;73    3.000E+03 rUHAGNiRKAHQqVOQ@QQ@Q
;73    4.000E+03 QTHhCMgBKAIQqWOQAQQAQ
;73    5.000E+03 IhGvSMuWKAIQqXOQAQQAQ
;73    6.000E+03 FfGuQMDaKAIQqYOQAQQAQ
;73    8.000E+03 CfGDPMcQKQ@QqYOQAQQAQ
;73    1.000E+04 BWGSYMBiKQ@QA`OQBQQBQ
;73    1.500E+04 Q@GBXMQbKQ@QAaOQBQQBQ
;73    2.000E+04 VHFQaMATKQ@QAaOQBQQBQ
;73    3.000E+04 rUFqBMiRJQ@QAaOQBQQBQ
;73    4.000E+04 QTFAAMgAJQ@QAbOQBQQBQ
;73    5.000E+04 IhEhCLuWJQ@QAbOQBQQBQ
;73    6.000E+04 FfEVeLDaJQ@QAbOQBQQBQ
;73    8.000E+04 CfEuCLcQJQ@QAbOQBQQBQ
;73    1.000E+05 BWEtDLBiJQ@QAbOQBQQBQ
;==== ELEMENT  74
;74    1.000E-03 QDStDOcWU@@R@@RcXUcWU
;74    1.500E-03 Q@SWQOaSU@@R@@RaTUaSU
;74    1.809E-03 AFSyHOQ@U@@R@@RQAUQ@U
;74 M5 1.809E-03 AFSyHOq@U@@R@@RqBUq@U
;74    1.840E-03 AFSYWOQcU@@R@@RQdUQcU
;74    1.872E-03 AFSyUOBeU@@R@@RBfUBeU
;74 M4 1.872E-03 AFSyUOSAU@@R@@RSBUSAU
;74    2.000E-03 ADSAEPSaU@@R@@RSbUSaU
;74    2.281E-03 AASaBPBbU@@R@@RBcUBbU
;74 M3 2.281E-03 AASaBPcGU@@R@@RcHUcGU
;74    2.423E-03 YgRq@PBbU@@R@@RBcUBbU
;74    2.575E-03 IbRqIPBTU@@R@@RBUUBTU
;74 M2 2.575E-03 IbRqIPRYU@@R@@RbPURYU
;74    2.694E-03 iYRAVPrCU@@R@@RrDUrCU
;74    2.820E-03 YURQSPBIU@@R@@RR@UBIU
;74 M1 2.820E-03 YURQSPRHU@@R@@RRIURHU
;74    3.000E-03 yFRaSPAiU@@R@@RQ`UAiU
;74    4.000E-03 xGRRIPIXT@@R@@RYVTIXT
;74    5.000E-03 GXRrQPEVT@@R@@RUSTEVT
;74    6.000E-03 vPRc@PCUT@@R@@RSQTCUT
;74    8.000E-03 ERRDGPaUT@@R@@RqQTaUT
;74    1.000E-02 DURtYPiDS@@R@@RiYSiES
;74    1.021E-02 tFRDfPxVS@@R@@Ri@SxVS
;74 L3 1.021E-02 tFRDfPbIT@@R@@RrCTbIT
;74    1.085E-02 TAREGPQdT@@R@@RQhTQdT
;74    1.154E-02 CfReHPaUT@@R@@RaYTaUT
;74 L2 1.154E-02 CfReHPbGT@@R@@RrATbGT
;74    1.182E-02 sWRuFPRET@@R@@RRITRET
;74    1.210E-02 cXRETPBCT@@R@@RBGTBCT
;74 L1 1.210E-02 cXRETPrDT@@R@@RrHTrDT
;74    1.500E-02 BiRf@PqFT@@R@@RqITqFT
;74    2.000E-02 BDRgEPvFS@@R@@RVWSvGS
;74    3.000E-02 a@RhTPRDS@@R@@RbGSRES
;74    4.000E-02 WdQISPyXR@@R@@RAGSIhR
;74    5.000E-02 eQQIfPeIR@@R@@RUeRuIR
;74    6.000E-02 dAQAAQSIR@@R@@RsQRcIR
;74    6.953E-02 sBQABQRBR@@R@@RRURbBR
;74 K  6.953E-02 sBQABQAHS@@R@@RQBSAIS
;74    8.000E-02 bTQACQGTR@@R@@RGaRWTR
;74    1.000E-01 AaQABQTER@@R@@RDTRdFR
;74    1.500E-01 HhPyTPAPR@@R@@RQXRAYR
;74    2.000E-01 eIPYHPFPQ@@R@@RGdQwBQ
;74    3.000E-01 RQPhAPRGQ@@R@@RcDQRiQ
;74    4.000E-01 AVPGVPACQ@@R@@RQbQqXQ
;74    5.000E-01 YTOFgPUfP@@R@@RqHQaHQ
;74    6.000E-01 vQOvHPChP@@R@@RAIQACQ
;74    8.000E-01 CdOeTPBDP@@R@@RHGPgXP
;74    1.000E+00 BXOEIPaHP@@R@@RfRPvGP
;74    1.022E+00 rGOECPaCP@@R@@RVPPfFP
;74    1.250E+00 aPOTVPhHOcCN@@RUXPERP
;74    1.500E+00 QAOTEPUaOQQO@@RE@PDiP
;74    2.000E+00 fINSTPSWOdXO@@RDSPtGP
;74    2.044E+00 FBNSPPCUOTgO@@RDPPtDP
;74    3.000E+00 BaNrYPAhOAGPyULDGPDEP
;74    4.000E+00 QXNrCPaCOQWPShMDDPDBP
;74    5.000E+00 AANBAPIDNQhPWaMT@PDIP
;74    6.000E+00 GCMqXPW@NrDPaANdAPd@P
;74    7.000E+00 UGMaPPEbNbVPaSNtDPtCP
;74    8.000E+00 SfMAVPTbNRdPBDNDWPDWP
;74    9.000E+00 SCMqDPdFNc@PBRNdQPdQP
;74    1.000E+01 RSMaDPsUNCTPrYNtUPtTP
;74    1.100E+01 BIMQFPsDNcVPSDNDhPDhP
;74    1.200E+01 qVMAHPCBNCgPCWNEBPEAP
;74    1.300E+01 QPMABPrUNDFPsWNUDPUDP
;74    1.400E+01 aIMiWORRNdDPDFNeGPeGP
;74    1.500E+01 QCMYGOrCNDPPtDNuHPuHP
;74    1.600E+01 Y`LxTORGNTVPdPNUPPUPP
;74    1.800E+01 GbLWiOQ`NDcPEINuPPuPP
;74    2.000E+01 vCLwFOaYNEHPUSNEiPEiP
;74    2.200E+01 eCLFdOQRNuAPUcNFGPFGP
;74    2.400E+01 DPLvIOqHNURPv@NfCPfCP
;74    2.600E+01 sULF@OaGNuPPfSNvHPvHP
;74    2.800E+01 cCLeVOQGNEhPVeNVRPVRP
;74    3.000E+01 BaLuFOAHNFCPgDNfUPfUP
;74    4.000E+01 QXLdFOWhMfXPHSNg@Pg@P
;74    5.000E+01 AALSVOvAMWFPyDNgRPgRP
;74    6.000E+01 GDKCFOeBMWTPA@OWePWeP
;74    8.000E+01 SfKBROChMHIPQAOHTPHTP
;74    1.000E+02 RSKBAOCHMHWPQIOH`PH`P
;74    1.500E+02 QCKASOBDMIIPqAOyFPyFP
;74    2.000E+02 vCJQBOQRMIUPqIOyPPyPP
;74    3.000E+02 BaJWdNAAMIgPAXOAAQAAQ
;74    4.000E+02 QXJfBNWWLAAQQTOACQACQ
;74    5.000E+02 AAJUENFELACQQXOAEQAEQ
;74    6.000E+02 GDIDQNECLADQaPOAFQAFQ
;74    8.000E+02 SfICTNsWLAEQaTOAGQAGQ
;74    1.000E+03 RSIBcNCBLAFQaWOAHQAHQ
;74    1.500E+03 QCIQhNBALAHQqQOQ@QQ@Q
;74    2.000E+03 vCHQSNQQLAHQqSOQ@QQ@Q
;74    3.000E+03 BaHAFNA@LAIQqUOQAQQAQ
;74    4.000E+03 QXHhBMWSKQ@QqVOQBQQBQ
;74    5.000E+03 AAHvQMFBKQ@QqWOQBQQBQ
;74    6.000E+03 GDGuPMEBKQ@QqXOQBQQBQ
;74    8.000E+03 SfGtIMsVKQ@QqYOQBQQBQ
;74    1.000E+04 RSGSXMCAKQAQqYOQBQQBQ
;74    1.500E+04 QCGBXMBAKQAQA`OQCQQCQ
;74    2.000E+04 vCFQ`MQPKQAQA`OQCQQCQ
;74    3.000E+04 BaFqAMA@KQAQA`OQCQQCQ
;74    4.000E+04 QXFAAMWRJQAQAaOQCQQCQ
;74    5.000E+04 AAFhALFBJQAQAaOQCQQCQ
;74    6.000E+04 GDEVdLEAJQAQAaOQCQQCQ
;74    8.000E+04 SfEuBLsVJQAQAaOQCQQCQ
;74    1.000E+05 RSEtCLCAJQAQAaOQCQQCQ
;==== ELEMENT  75
;75    1.000E-03 QFSdAOCfU@@R@@RCgUCfU
;75    1.500E-03 QASwDOqRU@@R@@RqSUqRU
;75    1.822E-03 AHSiHOQDU@@R@@RQEUQDU
;75 M5 1.822E-03 AHSiHOQDU@@R@@RQEUQDU
;75    1.885E-03 AGSiUOqSU@@R@@RqUUqSU
;75    1.949E-03 AGSA@PbUU@@R@@RbVUbUU
;75 M4 1.949E-03 AGSA@PrSU@@R@@RrTUrSU
;75    2.000E-03 AFSACPsVU@@R@@RsWUsVU
;75    2.367E-03 ABSaEPbXU@@R@@RrPUbXU
;75 M3 2.367E-03 ABSaEPSAU@@R@@RSBUSAU
;75    2.520E-03 A@SqCPbXU@@R@@RbYUbXU
;75    2.682E-03 IeRARPr@U@@R@@RrAUr@U
;75 M2 2.682E-03 IeRARPBTU@@R@@RBUUBTU
;75    2.804E-03 yRRAYPb@U@@R@@RbAUb@U
;75    2.932E-03 YXRQWPQhU@@R@@RQiUQhU
;75 M1 2.932E-03 YXRQWPBGU@@R@@RBHUBGU
;75    3.000E-03 YQRaPPQfU@@R@@RQgUQfU
;75    4.000E-03 HYRREPIfT@@R@@RYdTIfT
;75    5.000E-03 WYRbWPeXT@@R@@RuVTeXT
;75    6.000E-03 F`RSFPSYT@@R@@RcVTSYT
;75    8.000E-03 UQRDDPqRT@@R@@RqXTqRT
;75    1.000E-02 TSRtWPiUS@@R@@RAATiVS
;75    1.054E-02 tARTdPHRS@@R@@RHfSHSS
;75 L3 1.054E-02 tARTdPRIT@@R@@RbCTRIT
;75    1.122E-02 DERUFPAeT@@R@@RAiTAeT
;75    1.196E-02 sYRuHPQVT@@R@@RaPTQVT
;75 L2 1.196E-02 sYRuHPRET@@R@@RRITRET
;75    1.224E-02 sPREVPBDT@@R@@RBGTBDT
;75    1.253E-02 cQRUTPQbT@@R@@RQfTQcT
;75 L1 1.253E-02 cQRUTPbCT@@R@@RbFTbCT
;75    1.500E-02 ReRVGPAQT@@R@@RATTAQT
;75    2.000E-02 BHRgAPfRS@@R@@RFdSfSS
;75    3.000E-02 aCRhQPbDS@@R@@RrGSbDS
;75    4.000E-02 XBQIPPABS@@R@@RQASACS
;75    5.000E-02 uTQIdPUSR@@R@@RfAReSR
;75    6.000E-02 t@QAAQsDR@@R@@RCgRCTR
;75    7.168E-02 cDQABQBDR@@R@@RBVRRDR
;75 K  7.168E-02 cDQABQACS@@R@@RAGSADS
;75    8.000E-02 rPQABQwPR@@R@@RHGRG`R
;75    1.000E-01 AfQABQt@R@@R@@RTYRDPR
;75    1.500E-01 Y@PySPAUR@@R@@RaTRQUR
;75    2.000E-01 ERPYHPfVQ@@R@@RXBQWXQ
;75    3.000E-01 RXPhAPbFQ@@R@@RsDQCHQ
;75    4.000E-01 QPPGVPAHQ@@R@@RQhQAcQ
;75    5.000E-01 yYOFgPfDP@@R@@RAQQqAQ
;75    6.000E-01 FiOvIPDFP@@R@@RQAQADQ
;75    8.000E-01 SdOeTPRDP@@R@@RXHPwYP
;75    1.000E+00 RTOEIPqEP@@R@@RfYPFSP
;75    1.022E+00 BTOEDPaIP@@R@@RVWPvBP
;75    1.250E+00 aTOTVPhYOsAN@@ReSPEVP
;75    1.500E+00 QDOTEPVIOQUO@@REDPTbP
;75    2.000E+00 FWNSTPsUOtYO@@RDVPDPP
;75    2.044E+00 VINSPPcROEHO@@RDSPtGP
;75    3.000E+00 BhNrYPQgOAIPyVLTAPDHP
;75    4.000E+00 aRNrCPaIOQYPShMDGPDEP
;75    5.000E+00 ADNBAPIWNBAPWaMTDPTCP
;75    6.000E+00 gCMqXPGTNrGPaANdEPdDP
;75    7.000E+00 uAMaPPV@NbYPaSNtGPtGP
;75    8.000E+00 DGMAVPUENRhPBDNTQPTQP
;75    9.000E+00 cAMqDPDVNcDPBSNdUPdUP
;75    1.000E+01 bPMaDPSbNCXPrYNtYPtYP
;75    1.100E+01 REMQFPSPNsPPSDNTcPTcP
;75    1.200E+01 AaMAIPSFNSaPCWNEFPEFP
;75    1.300E+01 QTMABPBhNT@PsWNUIPUIP
;75    1.400E+01 qCMiWObTNdHPDGNuBPuBP
;75    1.500E+01 QFMYHOBTNDUPtDNETPETP
;75    1.600E+01 ABMxUObGNdQPdQNUUPUUP
;75    1.800E+01 HDLWiOQiNDiPEINuVPuVP
;75    2.000E+01 VQLwGOqWNUDPUSNUePUeP
;75    2.200E+01 uHLFeOQYNuGPUcNVCPVCP
;75    2.400E+01 TRLFPOATNUXPv@NfIPfIP
;75    2.600E+01 CeLFAOqBNuWPfTNFUPFUP
;75    2.800E+01 sBLeWOaBNUdPVeNVYPVYP
;75    3.000E+01 BiLuGOQCNV@PgDNvRPvRP
;75    4.000E+01 aSLdFOxEMvVPHSNgHPgHP
;75    5.000E+01 ADLSVOfPMgDPyCNwPPwPP
;75    6.000E+01 gCKCGOEVMgRPA@OHDPHDP
;75    8.000E+01 DGKBRODEMXHPQAOXSPXSP
;75    1.000E+02 bPKBAOcBMXWPQIOHiPHiP
;75    1.500E+02 QFKASORCMYHPqAOIVPIVP
;75    2.000E+02 VQJQBOQYMYUPqIOI`PI`P
;75    3.000E+02 BiJWeNAFMYhPAXOABQABQ
;75    4.000E+02 aSJfBNWaLABQQTOADQADQ
;75    5.000E+02 ADJUENvBLADQQXOAFQAFQ
;75    6.000E+02 gCIDQNeGLAEQaPOAGQAGQ
;75    8.000E+02 DGICTNSeLAFQaTOAHQAHQ
;75    1.000E+03 bPIBcNSELAGQaWOAIQAIQ
;75    1.500E+03 QFIQhNR@LAIQqPOQAQQAQ
;75    2.000E+03 VQHQSNQXLQ@QqROQAQQAQ
;75    3.000E+03 BiHAFNAELQ@QqUOQBQQBQ
;75    4.000E+03 aSHhBMGgKQAQqVOQCQQCQ
;75    5.000E+03 ADHvRMv@KQAQqWOQCQQCQ
;75    6.000E+03 gCGuPMeEKQAQqWOQCQQCQ
;75    8.000E+03 DGGtIMScKQBQqXOQCQQCQ
;75    1.000E+04 bPGSYMSEKQBQqYOQDQQDQ
;75    1.500E+04 QFGBXMR@KQBQqYOQDQQDQ
;75    2.000E+04 VQFQ`MQWKQBQA`OQDQQDQ
;75    3.000E+04 BiFqAMAEKQBQA`OQDQQDQ
;75    4.000E+04 aSFAAMGgJQBQA`OQDQQDQ
;75    5.000E+04 ADFhALfIJQBQA`OQDQQDQ
;75    6.000E+04 gCEVdLeEJQBQAaOQDQQDQ
;75    8.000E+04 DGEuBLScJQBQAaOQDQQDQ
;75    1.000E+05 bPEtCLSEJQBQAaOQDQQDQ
;==== ELEMENT  76
;76    1.000E-03 QGSDEODBU@@R@@RDCUDBU
;76    1.500E-03 QBSWDOqYU@@R@@RA`UqYU
;76    1.960E-03 AGSIgOAAU@@R@@RABUAAU
;76 M5 1.960E-03 AGSIgOADU@@R@@RAEUADU
;76    2.000E-03 AGSAAPbAU@@R@@RbBUbAU
;76    2.031E-03 AFSACPRXU@@R@@RRYURXU
;76 M4 2.031E-03 AFSACPBcU@@R@@RBdUBcU
;76    2.234E-03 ADSQEPbXU@@R@@RbYUbXU
;76    2.457E-03 ABSaGPRSU@@R@@RRTURSU
;76 M3 2.457E-03 ABSaGPRdU@@R@@RReURdU
;76    2.619E-03 YiRqFPRQU@@R@@RRRURQU
;76    2.792E-03 I`RAVPREU@@R@@RRFUREU
;76 M2 2.792E-03 I`RAVPbIU@@R@@Rr@UbIU
;76    3.000E-03 YWRQXPQcU@@R@@RQdUQcU
;76    3.049E-03 YRRaPPAfU@@R@@RAgUAfU
;76 M1 3.049E-03 YRRaPPQdU@@R@@RQeUQdU
;76    4.000E-03 XURRBPAAU@@R@@RABUAAU
;76    5.000E-03 gTRbSPEfT@@R@@RUcTEfT
;76    6.000E-03 FeRSAPsQT@@R@@RsXTsQT
;76    8.000E-03 UVRShPqXT@@R@@RAdTqXT
;76    1.000E-02 TXRtQPYiS@@R@@RADTYiS
;76    1.087E-02 dCRTiPHCS@@R@@RHVSHCS
;76 L3 1.087E-02 dCRTiPBHT@@R@@RRBTBHT
;76    1.160E-02 SfReBPqTT@@R@@RqYTqUT
;76    1.238E-02 sPRETPAWT@@R@@RQPTAWT
;76 L2 1.238E-02 sPRETPBBT@@R@@RBFTBBT
;76    1.267E-02 cPRUQPQaT@@R@@RQeTQaT
;76    1.297E-02 SQRUYPAaT@@R@@RAeTAaT
;76 L1 1.297E-02 SQRUYPBIT@@R@@RRCTBIT
;76    1.500E-02 RhRFIPAUT@@R@@RAXTAUT
;76    2.000E-02 R@RWBPFbS@@R@@RGDSFcS
;76    3.000E-02 aDRXPPrAS@@R@@RBTSrBS
;76    4.000E-02 hCQiIPAFS@@R@@RQESAGS
;76    5.000E-02 EcQySPuSR@@R@@RFQREcR
;76    6.000E-02 tFQYgPCWR@@R@@RD@RSWR
;76    7.387E-02 SBQAAQQeR@@R@@RrFRBER
;76 K  7.387E-02 SBQAAQyUR@@R@@RABSIeR
;76    8.000E-02 rTQAAQWaR@@R@@RhIRHAR
;76    1.000E-01 AiQAAQDQR@@R@@RtPRTQR
;76    1.500E-01 iDPiTPAYR@@R@@RaXRQYR
;76    2.000E-01 UQPY@PFgQ@@R@@RxCQwXQ
;76    3.000E-01 bRPXDPrDQ@@R@@RCQQSEQ
;76    4.000E-01 QSPGPPQBQ@@R@@RBAQAfQ
;76    5.000E-01 YfOFaPFWP@@R@@RASQqCQ
;76    6.000E-01 GAOvDPdAP@@R@@RQCQAEQ
;76    8.000E-01 DAOePPbBP@@R@@RhBPGbP
;76    1.000E+00 RYOEEPAPP@@R@@RvPPFUP
;76    1.022E+00 BXOTiPqDP@@R@@RVXPvCP
;76    1.250E+00 aWOTRPICOsEN@@ReRPEVP
;76    1.500E+00 QGOTBPFTOQXO@@RECPTbP
;76    2.000E+00 VYNSRPCiODeO@@RDVPtIP
;76    2.044E+00 vANCWPsVOUEO@@RDSPtFP
;76    3.000E+00 RdNrWPBDOAIPiXLT@PDGP
;76    4.000E+00 aVNrAPqDOaPPSeMDFPDEP
;76    5.000E+00 AFNB@PIcNBBPGeMTCPTBP
;76    6.000E+00 wGMqWPwQNrHPa@NdDPdDP
;76    7.000E+00 EQMQYPvBNrPPaRNtGPtGP
;76    8.000E+00 TDMATPuDNRiPBBNTQPTQP
;76    9.000E+00 cHMqCPdRNcEPBQNdUPdUP
;76    1.000E+01 bUMaCPDGNCYPrWNtYPtYP
;76    1.100E+01 RIMQEPcSNsQPSANTcPTcP
;76    1.200E+01 AdMAHPcHNSbPCTNEGPEFP
;76    1.300E+01 QWMAAPRhNTAPsTNe@PUIP
;76    1.400E+01 qEMYYOrTNdIPDCNuBPuBP
;76    1.500E+01 QHMYAORSNDVPtANETPETP
;76    1.600E+01 ADMhXOrENdRPTWNUUPUUP
;76    1.800E+01 XILWcOBFNT`PEENuWPuWP
;76    2.000E+01 fTLwAOAcNUEPEXNUfPUfP
;76    2.200E+01 EXLvYOaUNuHPEhNVCPVCP
;76    2.400E+01 dQLvEOQPNUYPfDNv@Pv@P
;76    2.600E+01 ScLUfOqGNuXPVXNFUPFUP
;76    2.800E+01 sHLeROaGNUePFiNfPPfPP
;76    3.000E+01 ReLuBOQHNVAPWHNvSPvSP
;76    4.000E+01 aVLdCOhUMvWPxFNgIPgIP
;76    5.000E+01 AFLSSOFdMgFPiENwQPwQP
;76    6.000E+01 wGKCDOeUMgTPYeNHEPHEP
;76    8.000E+01 TEKBPOd@MXIPQ@OXUPXUP
;76    1.000E+02 bUKQiOsDMXXPQHOX`PX`P
;76    1.500E+02 QHKARObAMi@Pq@OIXPIXP
;76    2.000E+02 fTJQAOaUMYWPqHOIbPIbP
;76    3.000E+02 ReJGhNQ@MA@QAWOABQABQ
;76    4.000E+02 aVJVGNh@LABQQROAEQAEQ
;76    5.000E+02 AFJUANVULADQQVOAFQAFQ
;76    6.000E+02 wGItHNEVLAEQQYOAGQAGQ
;76    8.000E+02 TEICRNDILAGQaROAIQAIQ
;76    1.000E+03 bUIBaNcGLAHQaUOQ@QQ@Q
;76    1.500E+03 QHIQfNRHLAIQaYOQAQQAQ
;76    2.000E+03 fTHQRNaSLQ@QqQOQBQQBQ
;76    3.000E+03 ReHAFNAILQAQqSOQBQQBQ
;76    4.000E+03 aVHXFMXEKQAQqTOQCQQCQ
;76    5.000E+03 AFHfWMVRKQAQqUOQCQQCQ
;76    6.000E+03 wGGeUMESKQBQqVOQCQQCQ
;76    8.000E+03 TEGtFMDGKQBQqVOQDQQDQ
;76    1.000E+04 bUGSVMcFKQBQqWOQDQQDQ
;76    1.500E+04 QHGBVMRGKQBQqXOQDQQDQ
;76    2.000E+04 fTFAiMaSKQBQqXOQDQQDQ
;76    3.000E+04 ReFq@MAIKQBQqXOQDQQDQ
;76    4.000E+04 aVFA@MXEJQBQqYOQDQQDQ
;76    5.000E+04 AFFXELVRJQCQqYOQDQQDQ
;76    6.000E+04 wGEFiLESJQCQqYOQDQQDQ
;76    8.000E+04 TEEeHLDGJQCQqYOQDQQDQ
;76    1.000E+05 bUEt@LcFJQCQqYOQDQQDQ
;==== ELEMENT  77
;77    1.000E-03 QISScOdCU@@R@@RdDUdCU
;77    1.500E-03 QDSG@OAiU@@R@@RQ`UAiU
;77    2.000E-03 AISYgOABU@@R@@RACUABU
;77    2.040E-03 AHSABPyWT@@R@@RIhTyWT
;77 M5 2.040E-03 AHSABPAEU@@R@@RAFUAEU
;77    2.078E-03 AHSADPaPU@@R@@RaQUaPU
;77    2.116E-03 AGSAFPBSU@@R@@RBUUBSU
;77 M4 2.116E-03 AGSAFPRWU@@R@@RRXURWU
;77    2.323E-03 AESQHPBYU@@R@@RRPUBYU
;77    2.551E-03 ACSqAPBQU@@R@@RBRUBQU
;77 M3 2.551E-03 ACSqAPrYU@@R@@RB`UrYU
;77    2.724E-03 AASAQPrHU@@R@@RrIUrHU
;77    2.909E-03 IdRQQPBCU@@R@@RBDUBCU
;77 M2 2.909E-03 IdRQQPRFU@@R@@RRGURFU
;77    3.000E-03 yTRQVPB@U@@R@@RBAUB@U
;77    3.174E-03 YVRaVPqVU@@R@@RqWUqVU
;77 M1 3.174E-03 YVRaVPAcU@@R@@RAdUAcU
;77    4.000E-03 xPRR@PAEU@@R@@RAFUAEU
;77    5.000E-03 wWRbQPV@T@@R@@RVHTV@T
;77    6.000E-03 VfRS@PCgT@@R@@RSdTCgT
;77    8.000E-03 eVRSgPAfT@@R@@RQaTAfT
;77    1.000E-02 dWRtPPADT@@R@@RAITADT
;77    1.122E-02 THREIPwSS@@R@@RXFSwTS
;77 L3 1.122E-02 THREIPB@T@@R@@RBDTB@T
;77    1.199E-02 S`RuAPaWT@@R@@RqPTaWT
;77    1.282E-02 cSRUTPqIT@@R@@RASTqIT
;77 L2 1.282E-02 cSRUTPQbT@@R@@RQfTQbT
;77    1.312E-02 STReRPAbT@@R@@RAeTAbT
;77    1.342E-02 CVRuPPqRT@@R@@RqVTqRT
;77 L1 1.342E-02 CVRuPPQiT@@R@@RBBTQiT
;77    1.500E-02 CERFIPQPT@@R@@RQSTQPT
;77    2.000E-02 RERWAPW@S@@R@@RwBSW@S
;77    3.000E-02 aGRHYPBQS@@R@@RRUSBRS
;77    4.000E-02 HSQiHPQAS@@R@@Ra@SQAS
;77    5.000E-02 UgQySPF@R@@R@@RfYRV@R
;77    6.000E-02 DWQYhPcSR@@R@@RTGRsSR
;77    7.611E-02 CEQAAQAhR@@R@@RbHRQhR
;77 K  7.611E-02 CEQAAQyBR@@R@@RySRIRR
;77    8.000E-02 BaQAAQh@R@@R@@RXXRx@R
;77    1.000E-01 QcQAAQTVR@@R@@RDfRdVR
;77    1.500E-01 IXPiVPQUR@@R@@RqTRaUR
;77    2.000E-01 eUPYBPWEQ@@R@@RhSQHFQ
;77    3.000E-01 bYPXFPBTQ@@R@@RSSQcFQ
;77    4.000E-01 QWPGQPQGQ@@R@@RBGQQaQ
;77    5.000E-01 ABPFcPvXP@@R@@RAVQqFQ
;77    6.000E-01 gAOvEPDRP@@R@@RQEQAHQ
;77    8.000E-01 TCOeQPrCP@@R@@RxFPWdP
;77    1.000E+00 bWOEFPAWP@@R@@RvYPVSP
;77    1.022E+00 RVOEAPAQP@@R@@RfWPFQP
;77    1.250E+00 qROTSPIXOCTN@@ReYPURP
;77    1.500E+00 a@OTCPvUOaRO@@REHPTfP
;77    2.000E+00 vXNSSPDIOTgO@@RTPPDSP
;77    2.044E+00 FYNCXPSdOeGO@@RDWPDPP
;77    3.000E+00 CCNrXPRDOQBPyQLTDPTAP
;77    4.000E+00 qPNrBPAPOaRPSfMT@PDIP
;77    5.000E+00 AINB@PACOBEPGgMTGPTFP
;77    6.000E+00 WXMqWPHHNBRPaANdIPdHP
;77    7.000E+00 UWMQYPfSNrTPaRNDRPDQP
;77    8.000E+00 dGMAUPePNCCPBCNTVPTUP
;77    9.000E+00 sGMqCPDdNcIPBQNtPPtPP
;77    1.000E+01 rSMaCPdFNSTPrXNDdPDdP
;77    1.100E+01 bFMQEPC`NsVPSBNThPThP
;77    1.200E+01 Q`MAHPCSNSgPCUNUBPUBP
;77    1.300E+01 aRMABPSCNTFPsUNeEPeEP
;77    1.400E+01 qIMiROBgNtEPDDNuHPuHP
;77    1.500E+01 aAMYCObUNTRPtBNUPPUPP
;77    1.600E+01 AGMxPOBVNdWPTXNeRPeQP
;77    1.800E+01 HTLWeORFNTfPEFNEcPEcP
;77    2.000E+01 FcLwCOQbNeBPEYNFBPFBP
;77    2.200E+01 eULFaOqRNEUPEiNfAPfAP
;77    2.400E+01 tULvFOQWNeVPfFNvGPvGP
;77    2.600E+01 DDLUgOATNEePVYNVSPVSP
;77    2.800E+01 CYLeTOqCNFCPV`NfWPfWP
;77    3.000E+01 CDLuDOaCNVIPWINFaPFaP
;77    4.000E+01 qQLdDOIEMFePxHNwGPwGP
;77    5.000E+01 AILSTOWFMwEPiGNG`PG`P
;77    6.000E+01 WYKCEOUbMwSPYgNXDPXDP
;77    8.000E+01 dGKBQODPMhIPQ@OhUPhUP
;77    1.000E+02 rSKB@OCYMhYPQHOIAPIAP
;77    1.500E+02 aAKAROrAMyBPq@OYYPYYP
;77    2.000E+02 FcJQAOqSMiYPqHOYdPYdP
;77    3.000E+02 CDJW`NQEMAAQAWOACQACQ
;77    4.000E+02 qQJVINXXLADQQROAFQAFQ
;77    5.000E+02 AIJUBNFeLAEQQVOAGQAGQ
;77    6.000E+02 WYItINuQLAFQQYOAHQAHQ
;77    8.000E+02 dGICRNdHLAHQaSOQ@QQ@Q
;77    1.000E+03 rSIBbNCRLAIQaUOQAQQAQ
;77    1.500E+03 aAIQgNbHLQ@QaYOQBQQBQ
;77    2.000E+03 FcHQRNqQLQAQqQOQCQQCQ
;77    3.000E+03 CDHAFNQDLQBQqSOQDQQDQ
;77    4.000E+03 qQHXHMXSKQBQqTOQDQQDQ
;77    5.000E+03 AIHfYMFbKQCQqUOQEQQEQ
;77    6.000E+03 WYGeWMeYKQCQqVOQEQQEQ
;77    8.000E+03 dGGtGMdFKQCQqWOQEQQEQ
;77    1.000E+04 rSGSWMCQKQCQqWOQEQQEQ
;77    1.500E+04 aAGBVMbGKQDQqXOQEQQEQ
;77    2.000E+04 FcFAiMqQKQDQqXOQEQQEQ
;77    3.000E+04 CDFqAMQDKQDQqXOQFQQFQ
;77    4.000E+04 qQFA@MXRJQDQqYOQFQQFQ
;77    5.000E+04 AIFXGLFbJQDQqYOQFQQFQ
;77    6.000E+04 WYEV`LeXJQDQqYOQFQQFQ
;77    8.000E+04 dGEeILdFJQDQqYOQFQQFQ
;77    1.000E+05 rSEtALCQJQDQqYOQFQQFQ
;==== ELEMENT  78
;78    1.000E-03 aASsGODRU@@R@@RDSUDRU
;78    1.500E-03 QFSfAOQgU@@R@@RQiUQgU
;78    2.000E-03 QASYCOAGU@@R@@RAHUAGU
;78    2.122E-03 AISIdOIPT@@R@@RYQTIPT
;78 M5 2.122E-03 AISIdOABU@@R@@RACUABU
;78    2.161E-03 AISAAPQSU@@R@@RQTUQSU
;78    2.202E-03 AHSACPbIU@@R@@Rr@UbIU
;78 M4 2.202E-03 AHSACPBTU@@R@@RBUUBTU
;78    2.413E-03 AFSQEPrGU@@R@@RrHUrGU
;78    2.645E-03 ACSaHPbIU@@R@@Rr@UbIU
;78 M3 2.645E-03 ACSaHPbUU@@R@@RbVUbUU
;78    3.000E-03 YbRAXPQfU@@R@@RQgUQfU
;78    3.026E-03 IiRQPPQaU@@R@@RQbUQaU
;78 M2 3.026E-03 IiRQPPBCU@@R@@RBDUBCU
;78    3.158E-03 yTRQWPAdU@@R@@RAeUAdU
;78    3.296E-03 YYRaTPaVU@@R@@RaWUaVU
;78 M1 3.296E-03 YYRaTPqSU@@R@@RqTUqSU
;78    4.000E-03 HdRBBPAIU@@R@@RQ@UAIU
;78    5.000E-03 GiRRSPvBT@@R@@RFPTvBT
;78    6.000E-03 GFRCBPDAT@@R@@RDHTDAT
;78    8.000E-03 uTRSaPQcT@@R@@RQiTQcT
;78    1.000E-02 tTRdUPAHT@@R@@RQCTAHT
;78    1.156E-02 TBRUEPGSS@@R@@RGdSGSS
;78 L3 1.156E-02 TBRUEPQ`T@@R@@RQeTQaT
;78    1.239E-02 CdRuHPQXT@@R@@RaRTQXT
;78    1.327E-02 SVReQPqAT@@R@@RqETqAT
;78 L2 1.327E-02 SVReQPAbT@@R@@RAeTAbT
;78    1.357E-02 CXReYPqRT@@R@@RqVTqRT
;78    1.388E-02 sIRuWPaST@@R@@RaWTaST
;78 L1 1.388E-02 sIRuWPAiT@@R@@RQbTAiT
;78    1.500E-02 SARFDPQUT@@R@@RQXTQUT
;78    2.000E-02 RIRGFPwES@@R@@RWWSwES
;78    3.000E-02 aIRHTPRPS@@R@@RbTSRQS
;78    4.000E-02 XYQiCPQES@@R@@RaDSQFS
;78    5.000E-02 FIQiXPfER@@R@@RVeRvDR
;78    6.000E-02 TVQYcPsXR@@R@@RtDRChR
;78    7.839E-02 RfQAAQAaR@@R@@Rb@RQaR
;78 K  7.839E-02 RfQAAQXhR@@R@@RyHRIHR
;78    8.000E-02 BfQAAQxDR@@R@@RxSRHUR
;78    1.000E-01 QgQAAQtPR@@R@@RTiRD`R
;78    1.500E-01 iYPiSPaPR@@R@@RqYRqPR
;78    2.000E-01 uXPIIPGQQ@@R@@RX`QxBQ
;78    3.000E-01 rUPXDPRTQ@@R@@RcRQsEQ
;78    4.000E-01 aPPGPPaBQ@@R@@RRBQQfQ
;78    5.000E-01 AEPFaPGFP@@R@@RAYQqIQ
;78    6.000E-01 wHOvCPdQP@@R@@RQGQAIQ
;78    8.000E-01 dCOePPBTP@@R@@RHVPHCP
;78    1.000E+00 rSOEEPQSP@@R@@RFfPVXP
;78    1.022E+00 bROE@PAWP@@R@@RvSPFWP
;78    1.250E+00 qVOTSPY`OSQN@@RuSPUUP
;78    1.500E+00 aCOTBPGEOaVO@@RUAPTiP
;78    2.000E+00 VeNSRPdGOEGO@@RTRPDUP
;78    2.044E+00 fVNCXPTAOuGO@@RDYPDRP
;78    3.000E+00 S@NrWPbCOQCPiYLTFPTCP
;78    4.000E+00 qUNrAPAWOaTPSeMTBPTAP
;78    5.000E+00 QBNB@PAGOBGPGeMd@PTIP
;78    6.000E+00 wXMqWPHSNBTPa@NtAPt@P
;78    7.000E+00 uQMQYPVaNrVPaRNDTPDTP
;78    8.000E+00 tHMAUPEdNCFPBBNTXPTXP
;78    9.000E+00 CVMqCPEENsBPBQNtSPtSP
;78    1.000E+01 B`MaCPDTNSWPrWNDgPDgP
;78    1.100E+01 rBMQEPSfNsYPSANEAPEAP
;78    1.200E+01 QeMAHPSWND@PCTNUEPUEP
;78    1.300E+01 aVMAAPcFNd@PsTNeHPeHP
;78    1.400E+01 ASMiPORiNtHPDCNERPEQP
;78    1.500E+01 aEMYBOrVNTUPtANUTPUTP
;78    1.600E+01 AIMhXORVNtQPTWNeUPeUP
;78    1.800E+01 hULWcObENE@PEDNEgPEgP
;78    2.000E+01 G@LwBOB@NeFPEXNFFPFFP
;78    2.200E+01 uYLvYOA`NEYPEhNfEPfEP
;78    2.400E+01 DgLvEOaSNuPPfDNFRPFRP
;78    2.600E+01 TELUfOQPNU`PVXNVWPVWP
;78    2.800E+01 SWLeROqHNFHPFhNvRPvRP
;78    3.000E+01 SALuCOaHNfDPWGNFfPFfP
;78    4.000E+01 qULdCOISMVaPxENGSPGRP
;78    5.000E+01 QBLSSOGUMGQPiDNGfPGfP
;78    6.000E+01 wYKCDOVFMwYPYdNh@Ph@P
;78    8.000E+01 tHKBPOTWMxFPQ@OxQPxQP
;78    1.000E+02 B`KQiOcTMxVPQGOIHPIHP
;78    1.500E+02 aEKAROBQMyIPq@OiVPiVP
;78    2.000E+02 G@JQAOA`MyVPqGOA@QA@Q
;78    3.000E+02 SAJGiNQIMABQAVOADQADQ
;78    4.000E+02 qUJVHNXcLADQQROAGQAGQ
;78    5.000E+02 QBJUANWDLAFQQVOAHQAHQ
;78    6.000E+02 wYItHNUdLAGQQXOAIQAIQ
;78    8.000E+02 tHICRNDULAIQaROQAQQAQ
;78    1.000E+03 B`IBaNSVLQ@QaTOQBQQBQ
;78    1.500E+03 aEIQfNrGLQAQaXOQCQQCQ
;78    2.000E+03 G@HQRNqXLQBQqPOQDQQDQ
;78    3.000E+03 SAHAFNQHLQCQqROQEQQEQ
;78    4.000E+03 qUHXFMHhKQCQqSOQEQQEQ
;78    5.000E+03 QBHfWMW@KQDQqTOQEQQEQ
;78    6.000E+03 wYGeVMUbKQDQqUOQFQQFQ
;78    8.000E+03 tHGtFMDTKQDQqUOQFQQFQ
;78    1.000E+04 B`GSVMSUKQDQqVOQFQQFQ
;78    1.500E+04 aEGBVMrGKQDQqWOQFQQFQ
;78    2.000E+04 G@FAiMqXKQEQqWOQFQQFQ
;78    3.000E+04 SAFq@MQHKQEQqWOQFQQFQ
;78    4.000E+04 qUFA@MHhJQEQqWOQGQQGQ
;78    5.000E+04 QBFXELW@JQEQqXOQGQQGQ
;78    6.000E+04 wYEFiLUbJQEQqXOQGQQGQ
;78    8.000E+04 tHEeHLDTJQEQqXOQGQQGQ
;78    1.000E+05 B`Et@LSUJQEQqXOQGQQGQ
;==== ELEMENT  79
;79    1.000E-03 aCScGOdTU@@R@@RdUUdTU
;79    1.500E-03 QHSFGOBHU@@R@@RBIUBHU
;79    2.000E-03 QCSXfOQCU@@R@@RQDUQCU
;79    2.206E-03 Q@SAAPIHT@@R@@RYITIHT
;79 M5 2.206E-03 Q@SAAPIcT@@R@@RYdTIcT
;79    2.248E-03 Q@SADPAXU@@R@@RAYUAXU
;79    2.291E-03 AISAFPbAU@@R@@RbBUbAU
;79 M4 2.291E-03 AISAFPrEU@@R@@RrFUrEU
;79    2.507E-03 AGSQHPbGU@@R@@RbHUbGU
;79    2.743E-03 ADSqBPRIU@@R@@Rb@URIU
;79 M3 2.743E-03 ADSqBPRSU@@R@@RRTURSU
;79    3.000E-03 AASAVPBDU@@R@@RBEUBDU
;79    3.148E-03 YdRQTPAaU@@R@@RAbUAaU
;79 M2 3.148E-03 YdRQTPQbU@@R@@RQcUQbU
;79    3.283E-03 yYRaQPqTU@@R@@RqUUqTU
;79    3.425E-03 iSRaYPQXU@@R@@RQXUQXU
;79 M1 3.425E-03 iSRaYPaTU@@R@@RaUUaTU
;79    4.000E-03 IARQiPQCU@@R@@RQDUQCU
;79    5.000E-03 HCRRPPVXT@@R@@RfVTVXT
;79    6.000E-03 WIRRiPTHT@@R@@RdETTHT
;79    8.000E-03 EeRChPBAT@@R@@RBGTBAT
;79    1.000E-02 DdRdSPQCT@@R@@RQHTQCT
;79    1.192E-02 DHReDPWGS@@R@@RWXSWGS
;79 L3 1.192E-02 DHReDPAcT@@R@@RAgTAcT
;79    1.279E-02 sYREXPQQT@@R@@RQUTQQT
;79    1.373E-02 SQRuRPaET@@R@@RaHTaET
;79 L2 1.373E-02 SQRuRPqST@@R@@RqVTqST
;79    1.404E-02 CSRE`PaTT@@R@@RaWTaTT
;79    1.435E-02 sDREhPQUT@@R@@RQYTQUT
;79 L1 1.435E-02 sDREhPA`T@@R@@RAcTA`T
;79    1.500E-02 SHRFDPaPT@@R@@RaTTaQT
;79    2.000E-02 bDRGEPgUS@@R@@RGhSgVS
;79    3.000E-02 qBRHSPbQS@@R@@RrUSbRS
;79    4.000E-02 H`QiCPa@S@@R@@Rq@SaAS
;79    5.000E-02 fDQiXPVTR@@R@@RgFRfSR
;79    6.000E-02 dWQYdPSfR@@R@@RTSRDFR
;79    8.000E-02 RdQAAQqYR@@R@@RRIRAiR
;79    8.072E-02 BiQAAQqUR@@R@@RRDRAeR
;79 K  8.072E-02 BiQAAQXQR@@R@@RX`RhQR
;79    1.000E-01 BBQAAQDfR@@R@@RUFRTfR
;79    1.500E-01 YePiUPaVR@@R@@RAfRqVR
;79    2.000E-01 UcPYAPwQQ@@R@@RiBQhRQ
;79    3.000E-01 BcPXFPbUQ@@R@@RsTQCVQ
;79    4.000E-01 aUPGRPaGQ@@R@@RRHQBAQ
;79    5.000E-01 AHPFcPwIP@@R@@RQSQARQ
;79    6.000E-01 WYOvEPDcP@@R@@RQIQQBQ
;79    8.000E-01 tEOeQPRVP@@R@@RhPPXGP
;79    1.000E+00 BaOEFPaQP@@R@@RVePfWP
;79    1.022E+00 bYOEAPQTP@@R@@RFbPVUP
;79    1.250E+00 AaOTTPADPcPN@@RuYPeQP
;79    1.500E+00 aGOTCPGPOqPO@@RUGPEDP
;79    2.000E+00 WFNSSPDXOUIO@@RTWPTPP
;79    2.044E+00 FeNCYPtBOUPO@@RTTPDWP
;79    3.000E+00 c@NrXPrDOQEPyRLd@PTGP
;79    4.000E+00 A`NrBPQTOaWPSfMTGPTEP
;79    5.000E+00 QENBAPQCOR@PGhMdDPdCP
;79    6.000E+00 HAMqWPHcNBWPaANtFPtEP
;79    7.000E+00 EiMQYPgCNB`PaSNDYPDXP
;79    8.000E+00 TQMAUPVANS@PBCNdSPdSP
;79    9.000E+00 SVMqCPeINsGPBQNtXPtXP
;79    1.000E+01 BiMaDPdUNcQPrXNTcPTbP
;79    1.100E+01 rHMQEPTENCdPSBNEGPEFP
;79    1.200E+01 B@MAHPsTNDEPCUNeAPeAP
;79    1.300E+01 qQMABPCQNdEPsUNuDPuDP
;79    1.400E+01 AWMiSOSCNDTPDDNEXPEWP
;79    1.500E+01 aHMYDOBiNdQPtBNePPePP
;79    1.600E+01 QCMxQObXNtWPTXNuRPuRP
;79    1.800E+01 XaLWfOrENEGPEFNUdPUdP
;79    2.000E+01 gBLwDOBINuCPEYNVDPVDP
;79    2.200E+01 UfLFbOAhNUVPEiNvBPvBP
;79    2.400E+01 EALvGOqQNuXPfFNFYPFYP
;79    2.600E+01 dGLUhOQWNUgPVYNfUPfUP
;79    2.800E+01 cXLeTOATNVEPV`NF`PF`P
;79    3.000E+01 cALuDOqDNvBPWINVdPVdP
;79    4.000E+01 A`LdEOIfMG@PxGNWRPWRP
;79    5.000E+01 QELSTOG`MWPPiFNWePWeP
;79    6.000E+01 HBKCEOFUMGiPYfNx@Px@P
;79    8.000E+01 TQKBQOtXMHVPQ@OHbPHbP
;79    1.000E+02 BiKB@OCaMHgPQHOYIPYIP
;79    1.500E+02 aHKARORRMYQPq@OyXPyXP
;79    2.000E+02 gBJQBOAhMIhPqGOAAQAAQ
;79    3.000E+02 cAJWaNaEMACQAWOAFQAFQ
;79    4.000E+02 A`Jf@NyDLAFQQROAHQAHQ
;79    5.000E+02 QEJUCNGVLAGQQVOAIQAIQ
;79    6.000E+02 HBItINfALAIQQXOQAQQAQ
;79    8.000E+02 TQICSNdVLQ@QaROQBQQBQ
;79    1.000E+03 BiIBbNsRLQAQaTOQCQQCQ
;79    1.500E+03 aHIQgNBXLQCQaXOQDQQDQ
;79    2.000E+03 gBHQSNAfLQCQqPOQEQQEQ
;79    3.000E+03 cAHAFNaDLQDQqROQFQQFQ
;79    4.000E+03 A`HXHMiIKQEQqTOQGQQGQ
;79    5.000E+03 QEHfYMGSKQEQqTOQGQQGQ
;79    6.000E+03 HBGeWMVIKQEQqUOQGQQGQ
;79    8.000E+03 TQGtGMdTKQEQqVOQGQQGQ
;79    1.000E+04 BiGSWMsQKQFQqVOQGQQGQ
;79    1.500E+04 aHGBWMBXKQFQqWOQHQQHQ
;79    2.000E+04 gBFQ`MAfKQFQqWOQHQQHQ
;79    3.000E+04 cAFqAMaDKQFQqWOQHQQHQ
;79    4.000E+04 A`FA@MiHJQFQqXOQHQQHQ
;79    5.000E+04 QEFXHLGSJQFQqXOQHQQHQ
;79    6.000E+04 HBEVaLVIJQFQqXOQHQQHQ
;79    8.000E+04 TQEu@LdTJQFQqXOQHQQHQ
;79    1.000E+05 BiEtALsQJQFQqXOQHQQHQ
;==== ELEMENT  80
;80    1.000E-03 aCSSVODbU@@R@@RDcUDbU
;80    1.500E-03 QISFVORFU@@R@@RRGURFU
;80    2.000E-03 QCSyBOQGU@@R@@RQHUQGU
;80    2.295E-03 Q@SQ@PhVT@@R@@RxWThVT
;80 M5 2.295E-03 Q@SQ@PyXT@@R@@RIiTyXT
;80    2.339E-03 AISQBPATU@@R@@RAUUATU
;80    2.385E-03 AISQEPRBU@@R@@RRCURBU
;80 M4 2.385E-03 AISQEPbHU@@R@@RbIUbHU
;80    2.606E-03 AFSaGPRGU@@R@@RRHURGU
;80    2.847E-03 ACSAPPBGU@@R@@RBHUBGU
;80 M3 2.847E-03 ACSAPPrIU@@R@@RBPUrIU
;80    3.000E-03 ABSAXPRAU@@R@@RRBURAU
;80    3.278E-03 IeRaSPaYU@@R@@RqPUaYU
;80 M2 3.278E-03 IeRaSPA`U@@R@@RAaUA`U
;80    3.417E-03 yPRqPPaSU@@R@@RaTUaSU
;80    3.562E-03 YTRqXPAXU@@R@@RAYUAXU
;80 M1 3.562E-03 YTRqXPQTU@@R@@RQUUQTU
;80    4.000E-03 IFRB@PQGU@@R@@RQHUQGU
;80    5.000E-03 HIRRPPvYT@@R@@RFgTvYT
;80    6.000E-03 gDRRgPtAT@@R@@RtITtAT
;80    8.000E-03 EiRCePBHT@@R@@RRDTBHT
;80    1.000E-02 DhRTYPQGT@@R@@RaBTQGT
;80    1.228E-02 D@Ru@PFfS@@R@@RgGSFgS
;80 L3 1.228E-02 D@Ru@PqTT@@R@@RqXTqTT
;80    1.321E-02 sQRUUPAST@@R@@RAWTAST
;80    1.421E-02 CRRE`PQGT@@R@@RaATQGT
;80 L2 1.421E-02 CRRE`PaST@@R@@RaVTaST
;80    1.452E-02 sDREgPQUT@@R@@RQXTQUT
;80    1.484E-02 cFRUePAWT@@R@@RQPTAWT
;80 L1 1.484E-02 cFRUePqPT@@R@@RqSTqPT
;80    1.500E-02 cBRUiPaUT@@R@@RaXTaUT
;80    2.000E-02 bGRViPGiS@@R@@RXBSW`S
;80    3.000E-02 qDRxEPrPS@@R@@RBdSrQS
;80    4.000E-02 XcQYDPaDS@@R@@RqDSaES
;80    5.000E-02 vCQiPPvWR@@R@@RWPRFgR
;80    6.000E-02 tTQIfPTAR@@R@@RdXRdAR
;80    8.000E-02 RhQA@QAfR@@R@@RbFRQfR
;80    8.310E-02 B`QA@QaWR@@R@@RBERqXR
;80 K  8.310E-02 B`QA@QHHR@@R@@RHWRXIR
;80    1.000E-01 BFQA@QTgR@@R@@ReHREGR
;80    1.500E-01 AAQYXPqQR@@R@@RQaRAaR
;80    2.000E-01 FCPIEPWeQ@@R@@RIVQHeQ
;80    3.000E-01 BhPXAPrTQ@@R@@RCcQSUQ
;80    4.000E-01 aXPwGPqBQ@@R@@RbBQBFQ
;80    5.000E-01 Q@PvYPgVP@@R@@RQVQAUQ
;80    6.000E-01 wTOvBPEAP@@R@@RaAQQCQ
;80    8.000E-01 DSOUXPbUP@@R@@RhXPhDP
;80    1.000E+00 BgOECPaWP@@R@@RViPvQP
;80    1.022E+00 rUOThPaPP@@R@@RFfPVXP
;80    1.250E+00 AeOTQPAHPcVN@@REaPeSP
;80    1.500E+00 aIOTAPgYOqTO@@RUHPEEP
;80    2.000E+00 w@NSQPdUOeGO@@RTWPTPP
;80    2.044E+00 G@NCWPDYOUXO@@RTTPDWP
;80    3.000E+00 cFNrWPBSOQGPiVLdAPTGP
;80    4.000E+00 AdNrAPQYOaXPSdMTGPTEP
;80    5.000E+00 QHNQiPQGORBPGcMdEPdCP
;80    6.000E+00 XHMqVPYGNBYPa@NtFPtEP
;80    7.000E+00 FAMQXPWQNBbPaRNTPPDYP
;80    8.000E+00 dPMATPvDNSAPBBNdTPdTP
;80    9.000E+00 cTMqCPEYNsHPBPNtYPtXP
;80    1.000E+01 ReMaCPDbNcSPrVNTdPTcP
;80    1.100E+01 BSMQEPtANCfPS@NEHPEHP
;80    1.200E+01 BEMAGPChNDGPCSNeBPeBP
;80    1.300E+01 qTMAAPSTNdGPsSNuFPuEP
;80    1.400E+01 QPMYXOcENDVPDBNEYPEYP
;80    1.500E+01 qAMIIOC@NdSPdINeQPeQP
;80    1.600E+01 QEMhVOrXNtYPTUNuSPuSP
;80    1.800E+01 IILWaOBTNEIPECNUePUeP
;80    2.000E+01 wFLw@ORGNuEPEVNVEPVEP
;80    2.200E+01 FILvXOQeNUXPEfNvDPvDP
;80    2.400E+01 UBLvCOqWNE`PfBNVQPVQP
;80    2.600E+01 tFLUeOaRNF@PVUNfWPfWP
;80    2.800E+01 sVLeQOQPNVHPFfNFbPFbP
;80    3.000E+01 cGLuAOqINvDPWENVfPVfP
;80    4.000E+01 AdLdBOABNGBPxBNWTPWTP
;80    5.000E+01 QHLSROHHMWSPi@NWhPWhP
;80    6.000E+01 XHKCDOfXMWbPY`NxCPxCP
;80    8.000E+01 dPKrIOTfMXPPAIOHePHeP
;80    1.000E+02 ReKQiOSdMX`PQGOiBPiBP
;80    1.500E+02 qAKARObQMYTPaIOIaPIaP
;80    2.000E+02 wFJQAOQeMYbPqGOABQABQ
;80    3.000E+02 cGJGgNaIMADQAVOAFQAFQ
;80    4.000E+02 AdJVFNiXLAFQQQOAHQAHQ
;80    5.000E+02 QHJU@NwSLAHQQUOQ@QQ@Q
;80    6.000E+02 XHItGNFTLAIQQWOQAQQAQ
;80    8.000E+02 dPICQNDbLQAQaQOQBQQBQ
;80    1.000E+03 ReIBaNCfLQBQaSOQCQQCQ
;80    1.500E+03 qAIQfNRWLQCQaWOQEQQEQ
;80    2.000E+03 wFHQRNQcLQDQaYOQFQQFQ
;80    3.000E+03 cGHAENaHLQEQqQOQFQQFQ
;80    4.000E+03 AdHXDMiSKQEQqROQGQQGQ
;80    5.000E+03 QHHfVMwPKQEQqSOQGQQGQ
;80    6.000E+03 XHGeTMFRKQFQqTOQGQQGQ
;80    8.000E+03 dPGtEMDaKQFQqUOQHQQHQ
;80    1.000E+04 ReGSUMCeKQFQqUOQHQQHQ
;80    1.500E+04 qAGBUMRWKQFQqVOQHQQHQ
;80    2.000E+04 wFFAiMQbKQFQqVOQHQQHQ
;80    3.000E+04 cGFq@MaHKQGQqVOQHQQHQ
;80    4.000E+04 AdFYiLiRJQGQqWOQHQQHQ
;80    5.000E+04 QHFXCLgYJQGQqWOQHQQHQ
;80    6.000E+04 XHEFhLFQJQGQqWOQHQQHQ
;80    8.000E+04 dPEeGLDaJQGQqWOQHQQHQ
;80    1.000E+05 ReEdILCeJQGQqWOQHQQHQ
;==== ELEMENT  81
;81    1.000E-03 aDScROTiU@@R@@REAUTiU
;81    1.500E-03 QISVYObEU@@R@@RbFUbEU
;81    2.000E-03 QCSYROaBU@@R@@RaCUaBU
;81    2.389E-03 AISQGPhDT@@R@@RxEThDT
;81 M5 2.389E-03 AISQGPQBU@@R@@RQCUQBU
;81    2.437E-03 AHSa@PQQU@@R@@RQRUQQU
;81    2.485E-03 AHSaBPBCU@@R@@RBDUBCU
;81 M4 2.485E-03 AHSaBPrDU@@R@@RrFUrDU
;81    2.711E-03 AESqDPRDU@@R@@RREURDU
;81    2.957E-03 ABSAXPQeU@@R@@RQfUQeU
;81 M3 2.957E-03 ABSAXPbFU@@R@@RbGUbFU
;81    3.000E-03 ABSQPPRHU@@R@@RRIURHU
;81    3.416E-03 ySRqRPQXU@@R@@RQYUQXU
;81 M2 3.416E-03 ySRqRPaXU@@R@@RaYUaXU
;81    3.557E-03 YWRqYPQRU@@R@@RQSUQRU
;81    3.704E-03 IQRAfPqHU@@R@@RqIUqHU
;81 M1 3.704E-03 IQRAfPATU@@R@@RAUUATU
;81    4.000E-03 Y@RBAPa@U@@R@@RaAUa@U
;81    5.000E-03 XCRRPPViT@@R@@RGGTViT
;81    6.000E-03 gIRRfPDTT@@R@@RTRTDTT
;81    8.000E-03 UcRCbPRET@@R@@RbATRET
;81    1.000E-02 TbRTUPaAT@@R@@RaFTaAT
;81    1.266E-02 SaRuEPVVS@@R@@RVfSVVS
;81 L3 1.266E-02 SaRuEPaUT@@R@@RaYTaUT
;81    1.364E-02 cRReQPqET@@R@@RqITqET
;81    1.470E-02 sCREgPQ@T@@R@@RQDTQAT
;81 L2 1.470E-02 sCREgPQTT@@R@@RQWTQTT
;81    1.500E-02 cFRUdPAVT@@R@@RQPTAVT
;81    1.535E-02 SGRFBPqHT@@R@@RARTqHT
;81 L1 1.535E-02 SGRFBPaPT@@R@@RaSTaPT
;81    2.000E-02 r@RVbPXBS@@R@@RxFSXCS
;81    3.000E-02 qFRhGPrYS@@R@@RRcSrYS
;81    4.000E-02 IDQIFPaIS@@R@@RqISaIS
;81    5.000E-02 FSQYRPGAR@@R@@RwURWAR
;81    6.000E-02 DaQyWPdFR@@R@@RDdRtFR
;81    8.000E-02 CBQYePQcR@@R@@RrCRBCR
;81    8.553E-02 rQQYfPaQR@@R@@RQhRqPR
;81 K  8.553E-02 rQQYfPgXR@@R@@RHERwXR
;81    1.000E-01 BIQYbPEIR@@R@@REPRUIR
;81    1.500E-01 ACQYQPqVR@@R@@RQfRAeR
;81    2.000E-01 VCPXiPXHQ@@R@@RiYQIHQ
;81    3.000E-01 RbPHEPBbQ@@R@@RSbQcSQ
;81    4.000E-01 qQPwBPqFQ@@R@@RbGQR@Q
;81    5.000E-01 QBPvTPWdP@@R@@RQXQAWQ
;81    6.000E-01 GhOfHPUIP@@R@@RaCQQEQ
;81    8.000E-01 TQOUUPrUP@@R@@RxUPx@P
;81    1.000E+00 RbOE@PqTP@@R@@RGCPvTP
;81    1.022E+00 B`OTePaVP@@R@@RFiPfQP
;81    1.250E+00 AiODXPQBPsQN@@REcPeTP
;81    1.500E+00 qBODHPWhOqWO@@RUIPEFP
;81    2.000E+00 GUNCYPDcOuEO@@RTXPTQP
;81    2.044E+00 WCNCTPdVOeVO@@RTUPDXP
;81    3.000E+00 sBNrUPRSOQHPiPLdAPTHP
;81    4.000E+00 AgNbIPaUOaYPSaMTGPTFP
;81    5.000E+00 a@NQhPaAORCPwXMdEPdDP
;81    6.000E+00 xDMqUPYPNRPPQINtGPtFP
;81    7.000E+00 VCMQWPwXNBcPaPNTPPTPP
;81    8.000E+00 dYMASPVWNSBPB@NdUPdTP
;81    9.000E+00 sQMqBPeXNsIPrHND`PtYP
;81    1.000E+01 C@MaBPE@NcTPrTNTdPTdP
;81    1.100E+01 BXMQDPDVNCgPCHNEIPEHP
;81    1.200E+01 BIMAGPDBNDHPCQNeCPeCP
;81    1.300E+01 qXMAAPcVNdHPsQNuGPuFP
;81    1.400E+01 QSMYROsFNDWPSiNUPPUPP
;81    1.500E+01 qDMICOS@NdTPdFNeRPeRP
;81    1.600E+01 QGMhPOBhNDaPTRNuTPuTP
;81    1.800E+01 iGLGfORRNU@PTiNUfPUfP
;81    2.000E+01 WQLgEObDNuFPERNVFPVFP
;81    2.200E+01 fALvTOBBNePPEbNvEPvEP
;81    2.400E+01 eBLfIOAcNEaPVHNVRPVRP
;81    2.600E+01 DTLUaOaXNFAPVQNfYPfYP
;81    2.800E+01 CcLUWOQUNVIPFaNFdPFcP
;81    3.000E+01 sDLeHOATNvFPW@NVhPVgP
;81    4.000E+01 AhLd@OAFNGDPhFNWVPWUP
;81    5.000E+01 a@LSPOxGMWUPYDNH@PH@P
;81    6.000E+01 xDKCBOVbMWdPIcNxEPxEP
;81    8.000E+01 dYKrHOUDMXRPAIOHgPHgP
;81    1.000E+02 CAKQhODHMXbPQFOiDPiDP
;81    1.500E+02 qDKAQOrPMYWPaHOIdPIdP
;81    2.000E+02 WQJQ@OBBMYePqFOABQABQ
;81    3.000E+02 sDJGbNqDMADQAUOAFQAFQ
;81    4.000E+02 AhJVBNA@MAFQQPOAIQAIQ
;81    5.000E+02 a@JEGNHALAHQQTOQ@QQ@Q
;81    6.000E+02 xDItDNfVLAIQQVOQAQQAQ
;81    8.000E+02 dYIsINTiLQAQaPOQCQQCQ
;81    1.000E+03 CAIrYNSiLQBQaROQDQQDQ
;81    1.500E+03 qDIQeNbVLQCQaVOQEQQEQ
;81    2.000E+03 WQHQQNQiLQDQaXOQFQQFQ
;81    3.000E+03 sDHAENqCLQEQqPOQGQQGQ
;81    4.000E+03 AhHHIMYfKQEQqQOQGQQGQ
;81    5.000E+03 a@HfQMWgKQFQqROQGQQGQ
;81    6.000E+03 xDGeQMfTKQFQqSOQHQQHQ
;81    8.000E+03 dYGtBMThKQFQqSOQHQQHQ
;81    1.000E+04 CAGSSMShKQFQqTOQHQQHQ
;81    1.500E+04 qDGBTMbVKQGQqTOQHQQHQ
;81    2.000E+04 WQFAgMQiKQGQqUOQHQQHQ
;81    3.000E+04 sDFaIMqCKQGQqUOQIQQIQ
;81    4.000E+04 AhFYbLYfJQGQqUOQIQQIQ
;81    5.000E+04 a@FHHLWfJQGQqUOQIQQIQ
;81    6.000E+04 xDEFcLfTJQGQqVOQIQQIQ
;81    8.000E+04 dYEeDLThJQGQqVOQIQQIQ
;81    1.000E+05 CAEdFLShJQGQqVOQIQQIQ
;==== ELEMENT  82
;82    1.000E-03 aESSYOe@U@@R@@ReAUe@U
;82    1.500E-03 a@SfPOrDU@@R@@RrFUrDU
;82    2.000E-03 QDSiROaGU@@R@@RaIUaGU
;82    2.484E-03 AISaDPW`T@@R@@RHATW`T
;82 M5 2.484E-03 AISaDPqHU@@R@@RAPUqHU
;82    2.534E-03 AHSaGPaTU@@R@@RaUUaTU
;82    2.586E-03 AHSq@PQcU@@R@@RQdUQcU
;82 M4 2.586E-03 AHSq@PBTU@@R@@RBUUBTU
;82    3.000E-03 ACSQRPQeU@@R@@RQfUQeU
;82    3.066E-03 ABSQVPAeU@@R@@RAfUAeU
;82 M3 3.066E-03 ABSQVPRDU@@R@@RREURDU
;82    3.301E-03 YcRaXPqXU@@R@@RqYUqXU
;82    3.554E-03 iURAaPAYU@@R@@RQPUAYU
;82 M2 3.554E-03 iURAaPQWU@@R@@RQXUQXU
;82    3.699E-03 YPRAiPASU@@R@@RATUASU
;82    3.851E-03 yDRQfPq@U@@R@@RqAUq@U
;82 M1 3.851E-03 yDRQfPqFU@@R@@RqGUqFU
;82    4.000E-03 YHRBDPaDU@@R@@RaEUaDU
;82    5.000E-03 hARRRPgBT@@R@@Rw@TgBT
;82    6.000E-03 wFRRgPdPT@@R@@RdWTdPT
;82    8.000E-03 F@RCaPbCT@@R@@RbITbCT
;82    1.000E-02 ThRTTPaFT@@R@@RqATaFT
;82    1.304E-02 CeRETPvAS@@R@@RvPSvBS
;82 L3 1.304E-02 CeRETPQXT@@R@@RaRTQXT
;82    1.500E-02 sARUbPAHT@@R@@RQBTAHT
;82    1.520E-02 cFRUfPADT@@R@@RAHTAET
;82 L2 1.520E-02 cFRUfPAUT@@R@@RAYTAUT
;82    1.553E-02 SHRFDPqHT@@R@@RAQTqHT
;82    1.586E-02 S@RVAPqAT@@R@@RqDTqAT
;82 L1 1.586E-02 S@RVAPQRT@@R@@RQUTQRT
;82    2.000E-02 rDRV`PHPS@@R@@RhTSHPS
;82    3.000E-02 qHRhCPBiS@@R@@RCCSBiS
;82    4.000E-02 i@QIBPqCS@@R@@RATSqDS
;82    5.000E-02 VUQIXPgIR@@R@@RHDRwIR
;82    6.000E-02 T`QySPDSR@@R@@REBRTSR
;82    8.000E-02 CHQYbPBAR@@R@@RBRRRAR
;82    8.800E-02 bSQYcPQUR@@R@@RQaRaUR
;82 K  8.800E-02 bSQYcPwBR@@R@@RgXRGRR
;82    1.000E-01 RCQIiPeDR@@R@@RUURuDR
;82    1.500E-01 AEQIXPAaR@@R@@RBARQaR
;82    2.000E-01 fFPXgPHVQ@@R@@RYiQyFQ
;82    3.000E-01 RiPHDPRcQ@@R@@RDCQsSQ
;82    4.000E-01 qUPwAPARQ@@R@@RrBQREQ
;82    5.000E-01 QDPvSPhFP@@R@@RaQQQPQ
;82    6.000E-01 HFOfFPEQP@@R@@RaEQQGQ
;82    8.000E-01 dROUTPBgP@@R@@RHgPHQP
;82    1.000E+00 RiOTiPAaP@@R@@RW@PF`P
;82    1.022E+00 BgOTdPqSP@@R@@RVfPfXP
;82    1.250E+00 QcODXPQGPsXN@@REhPeXP
;82    1.500E+00 qEODGPxBOAaO@@ReBPEIP
;82    2.000E+00 gSNCXPECOEUO@@RdQPTSP
;82    2.044E+00 w@NCTPDeOuWO@@RTXPTPP
;82    3.000E+00 CQNrTPbSOQIPYYLdCPd@P
;82    4.000E+00 QbNbIPqROqQPSaMd@PTHP
;82    5.000E+00 aCNQhPaFOREPwWMdGPdFP
;82    6.000E+00 XTMqUPIiNRRPQINtIPtHP
;82    7.000E+00 fHMQWPX@NBePaPNTSPTRP
;82    8.000E+00 DaMASPFdNSEPB@NdWPdWP
;82    9.000E+00 C`MqBPUaNCRPrHNDbPDbP
;82    1.000E+01 CHMaBPe@NcWPrTNTgPTgP
;82    1.100E+01 RTMQDPdTNS`PCHNUBPUAP
;82    1.200E+01 RDMAGPTINTBPCPNeFPeFP
;82    1.300E+01 AbMA@PCaNtBPsPNEPPEPP
;82    1.400E+01 QWMYPOSPNTPPSiNUSPUSP
;82    1.500E+01 qGMIBOcCNdXPdFNeVPeVP
;82    1.600E+01 a@MXYOC@NDdPTQNuXPuXP
;82    1.800E+01 YPLGeObSNUDPThNF@PF@P
;82    2.000E+01 wPLgDOrCNEPPEQNfAPf@P
;82    2.200E+01 vFLvSOR@NeTPE`NvIPvIP
;82    2.400E+01 uDLfIOQaNEfPVFNVWPVWP
;82    2.600E+01 TULU`OqUNFFPFYNvSPvSP
;82    2.800E+01 ScLUWOaQNfDPF`NFhPFhP
;82    3.000E+01 CRLeGOQPNFQPGHNGBPGBP
;82    4.000E+01 QbLTIOQ@NW@PhENgQPgQP
;82    5.000E+01 aCLSPOxPMgQPYBNHFPHFP
;82    6.000E+01 XUKCAOWIMH@PIaNHQPHQP
;82    8.000E+01 DaKrHOuDMXXPAHOXcPXcP
;82    1.000E+02 CHKQgOdDMXiPQFOyAPyAP
;82    1.500E+02 qGKAPOBaMiTPaHOYaPYaP
;82    2.000E+02 wPJQ@OR@MA@QqEOACQACQ
;82    3.000E+02 CRJGaNqIMAEQATOAGQAGQ
;82    4.000E+02 QbJVANADMAGQQPOAIQAIQ
;82    5.000E+02 aCJEFNxBLAIQQSOQAQQAQ
;82    6.000E+02 XUItCNVcLQ@QQVOQBQQBQ
;82    8.000E+02 DaIsHNUILQBQaPOQDQQDQ
;82    1.000E+03 CHIrXNTELQCQaROQEQQEQ
;82    1.500E+03 qGIQdNrVLQDQaUOQFQQFQ
;82    2.000E+03 wPHQPNBGLQEQaWOQGQQGQ
;82    3.000E+03 CRHAENqHLQFQqPOQHQQHQ
;82    4.000E+03 QbHHHMADLQFQqQOQHQQHQ
;82    5.000E+03 aCHfPMhHKQGQqROQHQQHQ
;82    6.000E+03 XUGePMV`KQGQqROQIQQIQ
;82    8.000E+03 DaGtAMUHKQGQqSOQIQQIQ
;82    1.000E+04 CHGSRMTDKQGQqSOQIQQIQ
;82    1.500E+04 qGGBSMrVKQGQqTOQIQQIQ
;82    2.000E+04 wPFAgMBGKQHQqTOQIQQIQ
;82    3.000E+04 CRFaIMqHKQHQqUOa@Qa@Q
;82    4.000E+04 QbFYaLACKQHQqUOa@Qa@Q
;82    5.000E+04 aCFHGLhHJQHQqUOa@Qa@Q
;82    6.000E+04 XUEFbLV`JQHQqUOa@Qa@Q
;82    8.000E+04 DaEeCLUGJQHQqUOa@Qa@Q
;82    1.000E+05 CHEdFLTDJQHQqUOa@Qa@Q
;==== ELEMENT  83
;83    1.000E-03 aGSSPOESU@@R@@RETUESU
;83    1.500E-03 aBSFYOBVU@@R@@RBWUBVU
;83    2.000E-03 QFSYUOqDU@@R@@RqEUqDU
;83    2.580E-03 AISq@PgRT@@R@@RwSTgRT
;83 M5 2.580E-03 AISq@PqWU@@R@@RqXUqWU
;83    2.633E-03 AHSqCPAaU@@R@@RAbUAaU
;83    2.688E-03 AHSqFPAeU@@R@@RAfUAeU
;83 M4 2.688E-03 AHSqFPRWU@@R@@RRXURWU
;83    3.000E-03 ADSQTPBDU@@R@@RBEUBDU
;83    3.177E-03 ABSaSPqVU@@R@@RqWUqVU
;83 M3 3.177E-03 ABSaSPBDU@@R@@RBEUBDU
;83    3.427E-03 YeRqWPaYU@@R@@RqPUaYU
;83    3.696E-03 iRRQaPAPU@@R@@RAQUAPU
;83 M2 3.696E-03 iRRQaPAYU@@R@@RQPUAYU
;83    3.845E-03 IPRQhPqFU@@R@@RqFUqFU
;83    3.999E-03 y@RBFPaCU@@R@@RaDUaCU
;83 M1 3.999E-03 y@RBFPaIU@@R@@Rq@UaIU
;83    4.000E-03 y@RBFPaIU@@R@@Rq@UaIU
;83    5.000E-03 xBRRTPWPT@@R@@RWXTWPT
;83    6.000E-03 GWRRiPtXT@@R@@RDfTtXT
;83    8.000E-03 V@RCbPrBT@@R@@RrHTrBT
;83    1.000E-02 EGRTUPqAT@@R@@RqFTqAT
;83    1.342E-02 CaRUUPV@S@@R@@RFYSVAS
;83 L3 1.342E-02 CaRUUPQRT@@R@@RQVTQRT
;83    1.500E-02 sGRUcPQCT@@R@@RQFTQCT
;83    1.571E-02 c@RFIPYdS@@R@@RACTYeS
;83 L2 1.571E-02 c@RFIPqHT@@R@@RARTqHT
;83    1.605E-02 SBRVFPqBT@@R@@RqETqBT
;83    1.639E-02 CERfDPaET@@R@@RaHTaET
;83 L1 1.639E-02 CERfDPAUT@@R@@RAXTAUT
;83    2.000E-02 rIRVaPxQS@@R@@RXeSxQS
;83    3.000E-02 AQRhCPC@S@@R@@RSESCAS
;83    4.000E-02 IQQICPqIS@@R@@RQPSAPS
;83    5.000E-02 vPQIYPgQR@@R@@RxHRwQR
;83    6.000E-02 EBQyUPdSR@@R@@ReCRtSR
;83    8.000E-02 SEQYdPRAR@@R@@RRRRbAR
;83    9.053E-02 RWQYdPQPR@@R@@RAfRaPR
;83 K  9.053E-02 RWQYdPGBR@@R@@RwHRWBR
;83    1.000E-01 RHQYaPERR@@R@@RuTRURR
;83    1.500E-01 AHQYQPAhR@@R@@RBHRQgR
;83    2.000E-01 FRPXiPxYQ@@R@@RACRiYQ
;83    3.000E-01 CGPHFPCEQ@@R@@RTFQCfQ
;83    4.000E-01 qYPwCPAXQ@@R@@RrIQbAQ
;83    5.000E-01 QGPvUPhSP@@R@@RaVQQTQ
;83    6.000E-01 hHOfHPeUP@@R@@RaHQQIQ
;83    8.000E-01 tUOUVPCAP@@R@@RIDPXVP
;83    1.000E+00 CGOEAPQ`P@@R@@RgAPVaP
;83    1.022E+00 ReOTfPAaP@@R@@RGGPvWP
;83    1.250E+00 QiODYPaBPCgN@@RUePuVP
;83    1.500E+00 qIODIPxROAfO@@ReHPUEP
;83    2.000E+00 GeNSPPeGOUXO@@RdVPTXP
;83    2.044E+00 WRNCUPEHOU`O@@RdSPTUP
;83    3.000E+00 SQNrUPrUOaAPiRLdHPdDP
;83    4.000E+00 QhNr@PA`OqTPSbMdDPdBP
;83    5.000E+00 aGNQiPqBORHPwYMtBPt@P
;83    6.000E+00 xYMqUPACORVPa@NDTPDSP
;83    7.000E+00 FVMQXPHWNBiPaQNTXPTWP
;83    8.000E+00 TeMATPWFNSIPBANtSPtRP
;83    9.000E+00 SaMqBPVHNCWPrINDhPDgP
;83    1.000E+01 SGMaBPETNsRPrUNECPEBP
;83    1.100E+01 bRMQDPDeNSePCINUGPUGP
;83    1.200E+01 b@MAGPtGNTGPCQNuBPuAP
;83    1.300E+01 AhMAAPShNtGPsQNEVPEVP
;83    1.400E+01 aRMYTOcUNTVPD@NUYPUYP
;83    1.500E+01 AQMIEOsGNtTPdGNuRPuRP
;83    1.600E+01 aDMhROSDNT`PTSNEdPEdP
;83    1.800E+01 yXLGhOrTNe@PE@NFGPFGP
;83    2.000E+01 WbLgGOBTNEWPESNfHPfGP
;83    2.200E+01 VULvUORINuQPEbNFWPFWP
;83    2.400E+01 UPLvAOQiNUcPVHNfUPfUP
;83    2.600E+01 dYLUbOAcNVDPVQNFaPFaP
;83    2.800E+01 DDLUYOaXNvBPFbNVfPVfP
;83    3.000E+01 SRLeIOQVNFYPW@NWAPWAP
;83    4.000E+01 QhLd@OQENWHPhGNwPPwPP
;83    5.000E+01 aGLSQOIIMwPPYENXEPXEP
;83    6.000E+01 H`KCBOWQMX@PIdNXQPXQP
;83    8.000E+01 TeKrHOUXMhYPAIOIDPIDP
;83    1.000E+02 SGKQhODSMY@PQFOIRPIRP
;83    1.500E+02 AQKAQORcMyVPaHOA@QA@Q
;83    2.000E+02 WbJQAORIMAAQqFOADQADQ
;83    3.000E+02 SRJGdNAUMAFQAUOAHQAHQ
;83    4.000E+02 QhJVDNAIMAIQQPOQAQQAQ
;83    5.000E+02 aGJEHNhYLQ@QQTOQBQQBQ
;83    6.000E+02 H`ItENgDLQAQQVOQCQQCQ
;83    8.000E+02 TeICPNERLQCQaPOQEQQEQ
;83    1.000E+03 SGIrYNtCLQDQaROQFQQFQ
;83    1.500E+03 AQIQeNBiLQFQaVOQGQQGQ
;83    2.000E+03 WbHQQNRFLQFQaXOQHQQHQ
;83    3.000E+03 SRHAENATLQGQqPOQIQQIQ
;83    4.000E+03 QhHXAMAHLQHQqQOa@Qa@Q
;83    5.000E+03 aGHfSMhUKQHQqROa@Qa@Q
;83    6.000E+03 H`GeRMgAKQHQqSOa@Qa@Q
;83    8.000E+03 TeGtCMEQKQIQqSOa@Qa@Q
;83    1.000E+04 SGGSTMtCKQIQqTOa@Qa@Q
;83    1.500E+04 AQGBTMBhKQIQqTOaAQaAQ
;83    2.000E+04 WbFAhMRFKQIQqUOaAQaAQ
;83    3.000E+04 SRFq@MATKQIQqUOaAQaAQ
;83    4.000E+04 QhFYdLAHKQIQqUOaAQaAQ
;83    5.000E+04 aGFX@LhUJQIQqVOaAQaAQ
;83    6.000E+04 H`EFeLgAJQIQqVOaAQaAQ
;83    8.000E+04 TeEeELEPJQIQqVOaAQaAQ
;83    1.000E+05 SGEdGLtBJQIQqVOaAQaAQ
;==== ELEMENT  84
;84    1.000E-03 q@SCYOuQU@@R@@RuRUuQU
;84    1.500E-03 aESVVORYU@@R@@RbPURYU
;84    2.000E-03 QHSyROAQU@@R@@RARUAQU
;84    2.683E-03 Q@SqIPwGT@@R@@RGXTwGT
;84 M5 2.683E-03 Q@SqIPRIU@@R@@RbAURIU
;84    2.740E-03 AISARPQgU@@R@@RQhUQgU
;84    2.798E-03 AISAVPqVU@@R@@RqXUqVU
;84 M4 2.798E-03 AISAVPrPU@@R@@RrQUrPU
;84    3.000E-03 AFSQXPRDU@@R@@RREURDU
;84    3.302E-03 ACSqTPaXU@@R@@RaYUaXU
;84 M3 3.302E-03 ACSqTPQeU@@R@@RQfUQeU
;84    3.567E-03 YfRAiPaQU@@R@@RaRUaQU
;84    3.854E-03 iURBDPqCU@@R@@RqDUqCU
;84 M2 3.854E-03 iURBDPAQU@@R@@RARUAQU
;84    4.000E-03 IYRRAPaIU@@R@@Rq@UaIU
;84    4.149E-03 yCRRIPQHU@@R@@RQIUQHU
;84 M1 4.149E-03 yCRRIPaCU@@R@@RaDUaCU
;84    5.000E-03 XQRbPPGeT@@R@@RWcTGeT
;84    6.000E-03 gURCEPEAT@@R@@REITEAT
;84    8.000E-03 fERCgPBST@@R@@RBYTBST
;84    1.000E-02 UIRdPPqGT@@R@@RASTqHT
;84    1.381E-02 sYRuQPUeS@@R@@RvDSUfS
;84 L3 1.381E-02 sYRuQPAWT@@R@@RQQTAXT
;84    1.500E-02 CWRF@PQHT@@R@@RaBTQHT
;84    1.624E-02 SGRfGPYSS@@R@@RIeSYSS
;84 L2 1.624E-02 SGRfGPqCT@@R@@RqFTqCT
;84    1.659E-02 CIRvEPaFT@@R@@RaITaFT
;84    1.694E-02 CBRFRPa@T@@R@@RaCTa@T
;84 L1 1.694E-02 CBRFRPqIT@@R@@RARTqIT
;84    2.000E-02 BVRVhPY@S@@R@@RyESYAS
;84    3.000E-02 AURx@PSES@@R@@Rs@SSFS
;84    4.000E-02 iYQYAPAVS@@R@@RQWSAWS
;84    5.000E-02 VaQYXPHAR@@R@@RH`RXAR
;84    6.000E-02 UHQIdPDhR@@R@@RUPRThR
;84    8.000E-02 cEQA@QbBR@@R@@RbURrBR
;84    9.311E-02 RSQA@QAWR@@R@@RAbRQWR
;84 K  9.311E-02 RSQA@QvYR@@R@@RWDRFiR
;84    1.000E-01 bEQA@QeWR@@R@@RUiRuWR
;84    1.500E-01 QAQiQPQfR@@R@@RRGRBFR
;84    2.000E-01 fTPIIPi@Q@@R@@RAHRAAR
;84    3.000E-01 SGPXEPc@Q@@R@@RtCQDBQ
;84    4.000E-01 AfPGRPQVQ@@R@@RBXQr@Q
;84    5.000E-01 aBPFcPIIP@@R@@RqQQQYQ
;84    6.000E-01 XYOvFPUfP@@R@@RqBQaCQ
;84    8.000E-01 TbOeRPSGP@@R@@RiHPxYP
;84    1.000E+00 SIOEGPB@P@@R@@RwIPGGP
;84    1.022E+00 CFOEBPQbP@@R@@RgDPVcP
;84    1.250E+00 BFOTUPaIPD@N@@RFIPEhP
;84    1.500E+00 ATOTDPi@OQbO@@RuIPeEP
;84    2.000E+00 XDNSTPUVOuVO@@RtUPdWP
;84    2.044E+00 G`NCYPuFOFIO@@RtRPdTP
;84    3.000E+00 cTNrYPRaOaEPySLtFPtCP
;84    4.000E+00 BENrCPQ`OqXPSgMtBPt@P
;84    5.000E+00 qANBAPqIObCPGiMDPPtIP
;84    6.000E+00 YCMqXPAIObRPaANTRPTQP
;84    7.000E+00 vQMaPPXcNRfPaSNdWPdVP
;84    8.000E+00 UDMAUPWTNcFPBCNDbPDaP
;84    9.000E+00 DFMqDPVRNSTPBRNTgPTgP
;84    1.000E+01 cIMaDPuSNC`PrXNUBPUBP
;84    1.100E+01 rRMQEPUANDCPSCNeGPeGP
;84    1.200E+01 bHMAHPdQNdFPCUNERPERP
;84    1.300E+01 QeMABPd@NDVPsUNUWPUVP
;84    1.400E+01 aXMiUOCeNdVPDENuPPuPP
;84    1.500E+01 AVMYFOSVNDdPtBNEcPEcP
;84    1.600E+01 aIMxSOs@NEAPTXNUfPUfP
;84    1.800E+01 ABMWhOBiNuAPEFNVIPVIP
;84    2.000E+01 hBLwEORWNUXPEYNFPPFPP
;84    2.200E+01 F`LFcOrANEcPEiNfPPVYP
;84    2.400E+01 uQLvHOR@NFEPfENvXPvXP
;84    2.600E+01 DgLUiOQbNfFPVYNVePVeP
;84    2.800E+01 d@LeUOqWNFUPV`NW@PW@P
;84    3.000E+01 cVLuEOaUNfRPWHNgEPgEP
;84    4.000E+01 BFLdFOaANwCPxFNGfPGfP
;84    5.000E+01 qBLSUOYWMGfPiENxBPxBP
;84    6.000E+01 YDKCFOWaMhGPYdNhXPhXP
;84    8.000E+01 UDKBQOEgMHgPQ@OiCPiCP
;84    1.000E+02 cIKB@OdWMiIPQGOiQPiQP
;84    1.500E+02 AVKASOCIMYfPq@OABQABQ
;84    2.000E+02 hBJQBOr@MADQqGOAFQAFQ
;84    3.000E+02 cVJWcNQSMAHQAVOQ@QQ@Q
;84    4.000E+02 BFJfANQDMQAQQROQCQQCQ
;84    5.000E+02 qBJUDNYELQCQQUOQEQQEQ
;84    6.000E+02 YDIDPNgRLQDQQXOQFQQFQ
;84    8.000E+02 UDICTNuQLQEQaROQGQQGQ
;84    1.000E+03 cIIBcNTVLQFQaTOQHQQHQ
;84    1.500E+03 AVIQgNCDLQHQaXOa@Qa@Q
;84    2.000E+03 hBHQSNbHLQIQqPOaAQaAQ
;84    3.000E+03 cVHAFNQRLa@QqROaBQaBQ
;84    4.000E+03 BFHh@MQDLa@QqSOaBQaBQ
;84    5.000E+03 qBHvQMYAKaAQqTOaBQaBQ
;84    6.000E+03 YDGeYMWYKaAQqUOaCQaCQ
;84    8.000E+03 UDGtHMeYKaAQqUOaCQaCQ
;84    1.000E+04 cIGSXMTUKaAQqVOaCQaCQ
;84    1.500E+04 AVGBWMCCKaAQqVOaCQaCQ
;84    2.000E+04 hBFQ`MbHKaBQqWOaCQaCQ
;84    3.000E+04 cVFqAMQRKaBQqWOaDQaDQ
;84    4.000E+04 BFFAAMQDKaBQqWOaDQaDQ
;84    5.000E+04 qBFh@LY@JaBQqXOaDQaDQ
;84    6.000E+04 YDEVcLWXJaBQqXOaDQaDQ
;84    8.000E+04 UDEuALeYJaBQqXOaDQaDQ
;84    1.000E+05 cIEtBLTUJaBQqXOaDQaDQ
;==== ELEMENT  85
;85    1.000E-03 qBSCTOEeU@@R@@REgUEeU
;85    1.021E-03 qBSSVOeTU@@R@@ReUUeTU
;85    1.042E-03 qBScXOESU@@R@@RETUESU
;85 N1 1.042E-03 qBScXOUTU@@R@@RUUUUTU
;85    1.500E-03 aGSVTOrRU@@R@@RrSUrRU
;85    2.000E-03 a@SyXOAXU@@R@@RAYUAXU
;85    2.787E-03 Q@SAWPWBT@@R@@RgCTWBT
;85 M5 2.787E-03 Q@SAWPBRU@@R@@RBSUBRU
;85    2.847E-03 Q@SQQPBAU@@R@@RBBUBAU
;85    2.909E-03 AISQUPaXU@@R@@RaYUaXU
;85 M4 2.909E-03 AISQUPrXU@@R@@RrYUrXU
;85    3.000E-03 AHSaPPbDU@@R@@RbEUbDU
;85    3.426E-03 ACSAdPaPU@@R@@RaQUaPU
;85 M3 3.426E-03 ACSAdPAfU@@R@@RAgUAfU
;85    4.000E-03 iTRREPaGU@@R@@RaHUaGU
;85    4.008E-03 iSRRFPaFU@@R@@RaGUaFU
;85 M2 4.008E-03 iSRRFPqDU@@R@@RqEUqDU
;85    4.160E-03 IWRbCPaBU@@R@@RaCUaBU
;85    4.317E-03 yARrAPQBU@@R@@RQCUQBU
;85 M1 4.317E-03 yARrAPQGU@@R@@RQGUQGU
;85    5.000E-03 hURbTPXFT@@R@@RhETXGT
;85    6.000E-03 wXRCIPeBT@@R@@Ru@TeBT
;85    8.000E-03 vGRSaPRTT@@R@@RbPTRTT
;85    1.000E-02 u@RdTPATT@@R@@RAYTATT
;85    1.421E-02 sVREePuXS@@R@@RVFSuYS
;85 L3 1.421E-02 sVREePART@@R@@RAVTART
;85    1.500E-02 STRFDPaDT@@R@@RaHTaDT
;85    1.678E-02 SBRFRPY@S@@R@@RIRSY@S
;85 L2 1.678E-02 SBRFRPaGT@@R@@Rq@TaGT
;85    1.714E-02 CDRFYPaAT@@R@@RaDTaAT
;85    1.749E-02 RgRVVPQET@@R@@RQHTQET
;85 L1 1.749E-02 RgRVVPqCT@@R@@RqFTqCT
;85    2.000E-02 RRRGAPITS@@R@@RyPSIUS
;85    3.000E-02 AXRxDPcIS@@R@@RCTScIS
;85    4.000E-02 YcQYEPQSS@@R@@RaTSQTS
;85    5.000E-02 W@QiRPxIR@@R@@RYIRHXR
;85    6.000E-02 uBQIiPUBR@@R@@RuUReBR
;85    8.000E-02 sDQAAQrCR@@R@@RrWRBSR
;85    9.573E-02 BYQAAQASR@@R@@RqXRQSR
;85 K  9.573E-02 BYQAAQVTR@@R@@RFiRfUR
;85    1.000E-01 rAQAAQEdR@@R@@RVGRUdR
;85    1.500E-01 QDQiWPBDR@@R@@RbERRCR
;85    2.000E-01 FcPYEPYXQ@@R@@RQBRAER
;85    3.000E-01 cGPh@PsDQ@@R@@RDYQTFQ
;85    4.000E-01 QaPGWPaSQ@@R@@RRVQrGQ
;85    5.000E-01 aEPFhPYRP@@R@@RqWQaTQ
;85    6.000E-01 HeOFPPfEP@@R@@RqEQaGQ
;85    8.000E-01 EHOeVPsCP@@R@@RYPPXiP
;85    1.000E+00 cIOUAPR@P@@R@@RWTPgAP
;85    1.022E+00 SEOEEPBAP@@R@@RwHPGFP
;85    1.250E+00 RCOTXPqFPTAN@@RVIPUhP
;85    1.500E+00 AXOTGPiVOQhO@@REXPuCP
;85    2.000E+00 HQNSVPEdOUaO@@RDbPtTP
;85    2.044E+00 HENSRPeSOfEO@@RtYPtQP
;85    3.000E+00 sVNBaPCEOaGPI`LDRPtIP
;85    4.000E+00 RBNrDPB@OAbPSiMtHPtFP
;85    5.000E+00 qFNBBPAVObGPWdMDVPDUP
;85    6.000E+00 ISMqYPQDObVPaBNTYPTXP
;85    7.000E+00 VcMaQPyGNCAPaTNtSPtRP
;85    8.000E+00 uAMAVPWaNsBPBDNDhPDhP
;85    9.000E+00 TIMqEPFcNcPPBSNEDPECP
;85    1.000E+01 CPMaEPFANCfPB`Ne@PUIP
;85    1.100E+01 BaMQFPuFNT@PSENuEPuEP
;85    1.200E+01 rFMAIPDcNtBPCWNUPPUPP
;85    1.300E+01 BAMACPDPNTSPsXNeUPeTP
;85    1.400E+01 qSMyRODDNtSPDGNuXPuXP
;85    1.500E+01 QQMiCOsSNTaPtENUbPUbP
;85    1.600E+01 qCMxYOCVNEHPdQNFEPFDP
;85    1.800E+01 AEMHCOCCNuIPEINfHPfHP
;85    2.000E+01 XPLGQObYNeWPUSNFYPFYP
;85    2.200E+01 GBLFhOBRNUbPUcNfYPfYP
;85    2.400E+01 U`LFSOb@NVEPfINFhPFhP
;85    2.600E+01 ECLFDOBANvFPfSNGEPGEP
;85    2.800E+01 tDLuPOAfNVUPVdNgAPgAP
;85    3.000E+01 sXLuIOqSNvSPgCNwFPwFP
;85    4.000E+01 RBLdHOaGNGUPHRNWgPWgP
;85    5.000E+01 qFLSXOA@NWhPyANHTPHTP
;85    6.000E+01 ITKCHOhHMHPPA@OHaPHaP
;85    8.000E+01 uAKBSOVEMIAPQ@OyGPyGP
;85    1.000E+02 CPKBBODiMITPQHOyVPyVP
;85    1.500E+02 QQKATOcCMAAQq@OADQADQ
;85    2.000E+02 XPJQCOBQMAEQqHOAHQAHQ
;85    3.000E+02 sXJWhNaPMQ@QAWOQBQQBQ
;85    4.000E+02 RBJfENa@MQCQQSOQEQQEQ
;85    5.000E+02 qFJUHNYXLQDQQVOQFQQFQ
;85    6.000E+02 ITIDSNWhLQFQQYOQHQQHQ
;85    8.000E+02 uAICVNUhLQGQaSOQIQQIQ
;85    1.000E+03 CPIBeNtXLQHQaUOa@Qa@Q
;85    1.500E+03 QQIQiNSHLa@QaYOaBQaBQ
;85    2.000E+03 XPHQTNrILaAQqQOaCQaCQ
;85    3.000E+03 sXHAGNQYLaBQqSOaCQaCQ
;85    4.000E+03 RBHhFMQILaBQqTOaDQaDQ
;85    5.000E+03 qFHvUMYTKaBQqUOaDQaDQ
;85    6.000E+03 ITGuSMWdKaCQqVOaDQaDQ
;85    8.000E+03 uAGDQMUfKaCQqWOaEQaEQ
;85    1.000E+04 CPGcPMtWKaCQqWOaEQaEQ
;85    1.500E+04 QQGBYMSHKaCQqXOaEQaEQ
;85    2.000E+04 XPFQaMrHKaCQqXOaEQaEQ
;85    3.000E+04 sXFqBMQYKaDQqXOaEQaEQ
;85    4.000E+04 RBFAAMQIKaDQqYOaFQaFQ
;85    5.000E+04 qFFhELYSJaDQqYOaFQaFQ
;85    6.000E+04 ITEVhLWdJaDQqYOaFQaFQ
;85    8.000E+04 uAEuELUfJaDQqYOaFQaFQ
;85    1.000E+05 CPEtELtWJaDQqYOaFQaFQ
;==== ELEMENT  86
;86    1.000E-03 aHSSIOEaU@@R@@REcUEaU
;86    1.047E-03 aHSCUOuDU@@R@@RuEUuDU
;86    1.097E-03 aGSsSOT`U@@R@@RTaUT`U
;86 N1 1.097E-03 aGSsSOE@U@@R@@REBUE@U
;86    1.500E-03 aCSVAOrQU@@R@@RrRUrQU
;86    2.000E-03 QGSYIOAXU@@R@@RAYUAXU
;86    2.892E-03 AESAVPVST@@R@@RfSTVST
;86 M5 2.892E-03 AESAVPrAU@@R@@RrBUrAU
;86    3.000E-03 ADSQRPQTU@@R@@RQUUQTU
;86    3.022E-03 ADSQSPQQU@@R@@RQRUQQU
;86 M4 3.022E-03 ADSQSPBVU@@R@@RBWUBVU
;86    3.270E-03 AASaXPQ`U@@R@@RQaUQ`U
;86    3.538E-03 I`RAbPAWU@@R@@RAXUAWU
;86 M3 3.538E-03 I`RAbPqPU@@R@@RqQUqPU
;86    4.000E-03 yARBGPaFU@@R@@RaGUaFU
;86    4.159E-03 YERREPQDU@@R@@RQEUQDU
;86 M2 4.159E-03 YERREPaAU@@R@@RaBUaAU
;86    4.317E-03 XiRbBPQ@U@@R@@RQAUQ@U
;86    4.482E-03 HcRr@PAAU@@R@@RABUAAU
;86 M1 4.482E-03 HcRr@PAEU@@R@@RAFUAEU
;86    5.000E-03 xERRTPHHT@@R@@RXFTHHT
;86    6.000E-03 WRRRgPUFT@@R@@ReDTUFT
;86    8.000E-03 VGRsUPRQT@@R@@RRXTRQT
;86    1.000E-02 UCRDTPART@@R@@RAXTAST
;86    1.462E-02 STReYPuDS@@R@@RuPSuDS
;86 L3 1.462E-02 STReYPqAT@@R@@RqDTqAT
;86    1.500E-02 CTRuWPaBT@@R@@RaFTaBT
;86    1.734E-02 RaRfDPhFS@@R@@RXVShGS
;86 L2 1.734E-02 RaRfDPQFT@@R@@RQHTQFT
;86    1.769E-02 BdRvAPQ@T@@R@@RQCTQ@T
;86    1.805E-02 rXRvGPAET@@R@@RAHTAET
;86 L1 1.805E-02 rXRvGPaAT@@R@@RaDTaAT
;86    2.000E-02 BURvPPyAS@@R@@RYVSyBS
;86    3.000E-02 ATRWfPcES@@R@@RCQScFS
;86    4.000E-02 iXQxSPQRS@@R@@RaRSQSS
;86    5.000E-02 VbQYHPxDR@@R@@RYBRHSR
;86    6.000E-02 UIQITPEIR@@R@@RuQRUIR
;86    8.000E-02 cFQiTPrCR@@R@@RrURBRR
;86    9.840E-02 rBQiSPqBR@@R@@RaURARR
;86 K  9.840E-02 rBQiSPUcR@@R@@RfFRFCR
;86    1.000E-01 bFQiRPuVR@@R@@RFIREfR
;86    1.500E-01 QBQiDPBAR@@R@@RbARR@R
;86    2.000E-01 fXPxTPIWQ@@R@@RQ@RACR
;86    3.000E-01 c@PGePsAQ@@R@@RDRQT@Q
;86    4.000E-01 AgPWDPaRQ@@R@@RRRQrCQ
;86    5.000E-01 aCPVXPIWP@@R@@RqSQaPQ
;86    6.000E-01 hWOVBPfBP@@R@@RqBQaCQ
;86    8.000E-01 ThOEQPsBP@@R@@RiCPxSP
;86    1.000E+00 cCODiPR@P@@R@@Rw@PVhP
;86    1.022E+00 CIODdPBAP@@R@@RWEPFdP
;86    1.250E+00 BIOtHPqEPDAN@@RUhPuWP
;86    1.500E+00 AVOSiPiTOQdO@@ReIPUDP
;86    2.000E+00 hENCQPEcOuWO@@RdUPTWP
;86    2.044E+00 W`NsGPeQOV@O@@RdRPTTP
;86    3.000E+00 cYNbYPCDOaDPyHLdFPdCP
;86    4.000E+00 BHNbDPQiOqVPCbMdBPd@P
;86    5.000E+00 qCNQdPAVOb@PgPMt@PdHP
;86    6.000E+00 iEMqQPQDORWPQGNDRPDQP
;86    7.000E+00 F`MQTPyCNR`PQWNTVPTUP
;86    8.000E+00 eAMAPPGgNc@PQfNtPPtPP
;86    9.000E+00 TAMaIPF`NCWPrCNDePDeP
;86    1.000E+01 sCMQIPUhNsRPbXNEAPE@P
;86    1.100E+01 rVMQAPuCNSfPCANUFPUEP
;86    1.200E+01 rBMADPDaNTGPsBNu@Pu@P
;86    1.300E+01 QgMIcOtHNtHPcRNETPETP
;86    1.400E+01 qPMy@ODBNTWPS`NUXPUWP
;86    1.500E+01 AXMHcOsQNtTPTFNuPPuPP
;86    1.600E+01 q@MHQOCTNTaPDQNEcPEcP
;86    1.800E+01 ACMgYOCANeAPDgNFEPFEP
;86    2.000E+01 xDLGIObXNEWPeINfFPfFP
;86    2.200E+01 FiLVXOBQNuQPeWNFUPFUP
;86    2.400E+01 uYLVEORINUcPFBNfSPfSP
;86    2.600E+01 TcLuXOB@NVDPvDNF`PF`P
;86    2.800E+01 dFLEUOAeNvBPfTNVePVeP
;86    3.000E+01 sQLUFOqRNFYPVaNGIPGIP
;86    4.000E+01 BILT@OaFNWIPHENgYPgYP
;86    5.000E+01 qCLCROYgMwPPX`NXDPXDP
;86    6.000E+01 iGKReOhDMX@PYWNXPPXPP
;86    8.000E+01 eAKrCOVBMhYPAFOIDPIDP
;86    1.000E+02 sDKQcODfMYAPQCOIRPIRP
;86    1.500E+02 AXKqGOcAMyVPaEOA@QA@Q
;86    2.000E+02 xDJAHOBPMAAQqBOADQADQ
;86    3.000E+02 sQJgTNQYMAFQAQOAHQAHQ
;86    4.000E+02 BIJUhNQIMAIQAVOQAQQAQ
;86    5.000E+02 qCJTeNYSLQ@QAYOQBQQBQ
;86    6.000E+02 iGIdDNWcLQBQQROQCQQCQ
;86    8.000E+02 eAIsANUdLQCQQVOQEQQEQ
;86    1.000E+03 sDIrSNtULQDQQXOQFQQFQ
;86    1.500E+03 AXIQ`NSGLQFQaQOQGQQGQ
;86    2.000E+03 xDHAWNrGLQFQaSOQHQQHQ
;86    3.000E+03 sQHABNQXLQGQaUOQIQQIQ
;86    4.000E+03 BIHWaMQILQHQaWOa@Qa@Q
;86    5.000E+03 qCHFVMIXKQHQaWOa@Qa@Q
;86    6.000E+03 iGGEXMW`KQHQaXOa@Qa@Q
;86    8.000E+03 eAGdBMUcKQIQaYOa@Qa@Q
;86    1.000E+04 sDGCUMtTKQIQaYOaAQaAQ
;86    1.500E+04 AXGrHMSFKQIQqPOaAQaAQ
;86    2.000E+04 xDFAcMrGKQIQqPOaAQaAQ
;86    3.000E+04 sQFaFMQXKQIQqPOaAQaAQ
;86    4.000E+04 BIFyPLQHKQIQqQOaAQaAQ
;86    5.000E+04 qCFW`LIXJQIQqQOaAQaAQ
;86    6.000E+04 iGEfXLW`JQIQqQOaAQaAQ
;86    8.000E+04 eAEUBLUbJa@QqQOaAQaAQ
;86    1.000E+05 sDETGLtTJa@QqQOaAQaAQ
;==== ELEMENT  87
;87    1.000E-03 q@SDIOFGU@@R@@RFHUFGU
;87    1.074E-03 aISTPOuBU@@R@@RuCUuBU
;87    1.153E-03 aHSTeOdVU@@R@@RdXUdVU
;87 N1 1.153E-03 aHSTeOtVU@@R@@RtWUtVU
;87    1.500E-03 aDSVeOBcU@@R@@RBdUBcU
;87    2.000E-03 QHSYcOQUU@@R@@RQVUQUU
;87    3.000E-03 AFSQXPvAT@@R@@RFQTvAT
;87 M5 3.000E-03 AFSQXPQhU@@R@@RB@UQhU
;87    3.000E-03 AFSQXPQhU@@R@@RQiUQhU
;87    3.136E-03 ADSaVPATU@@R@@RAUUATU
;87 M4 3.136E-03 ADSaVPbDU@@R@@RbEUbDU
;87    3.389E-03 AASAbPqXU@@R@@RqYUqXU
;87    3.663E-03 I`RQePAQU@@R@@RARUAQU
;87 M3 3.663E-03 I`RQePaSU@@R@@RaTUaSU
;87    4.000E-03 ITRRBPqAU@@R@@RqBUqAU
;87    4.327E-03 YARbHPAHU@@R@@RAIUAHU
;87 M2 4.327E-03 YARbHPQDU@@R@@RQEUQDU
;87    4.487E-03 XfRrFPAEU@@R@@RAFUAEU
;87    4.652E-03 H`RBTPYXT@@R@@RiWTYXT
;87 M1 4.652E-03 H`RBTPYiT@@R@@RAAUYiT
;87    5.000E-03 HXRbPPHPT@@R@@RHYTHPT
;87    6.000E-03 gTRCBPuGT@@R@@REUTuGT
;87    8.000E-03 fHRsYPbRT@@R@@RbXTbRT
;87    1.000E-02 eCRDXPAYT@@R@@RQTTAYT
;87    1.500E-02 SRREaPeBS@@R@@RUWSeBS
;87    1.503E-02 SQREbPUIS@@R@@RUTSUIS
;87 L3 1.503E-02 SQREbPaGT@@R@@Rq@TaGT
;87    1.641E-02 SHRFIPA@T@@R@@RACTA@T
;87    1.791E-02 BgRvIPGiS@@R@@RXHSW`S
;87 L2 1.791E-02 BgRvIPQAT@@R@@RQDTQAT
;87    1.827E-02 B`RFUPAET@@R@@RAHTAET
;87    1.864E-02 rSRVRPA@T@@R@@RACTA@T
;87 L1 1.864E-02 rSRVRPQFT@@R@@RQITQFT
;87    2.000E-02 RQRvUPiXS@@R@@RYcSiXS
;87    3.000E-02 AXRH@PsIS@@R@@RSUSCPS
;87    4.000E-02 YaQxWPQYS@@R@@RaYSQYS
;87    5.000E-02 W@QiCPxRR@@R@@RYRRHaR
;87    6.000E-02 uBQIYPuCR@@R@@RUfRESR
;87    8.000E-02 sDQiYPBTR@@R@@RBgRRTR
;87    1.000E-01 rBQiWPqCR@@R@@RaVRARR
;87    1.011E-01 bGQiWPaIR@@R@@RaQRqHR
;87 K  1.011E-01 bGQiWPuWR@@R@@RFIREfR
;87    1.500E-01 QEQiIPBIR@@R@@RbIRRHR
;87    2.000E-01 FgPH`PIeQ@@R@@RQDRAGR
;87    3.000E-01 cIPW`PCVQ@@R@@RTXQdEQ
;87    4.000E-01 QcPWIPaYQ@@R@@RbPQBQQ
;87    5.000E-01 aGPfSPYaP@@R@@RqXQaUQ
;87    6.000E-01 XdOVFPVRP@@R@@RqFQaGQ
;87    8.000E-01 UDOEUPCXP@@R@@RITPXcP
;87    1.000E+00 sCOTbPb@P@@R@@RGUPWBP
;87    1.022E+00 SIODgPR@P@@R@@RgIPVgP
;87    1.250E+00 REODQPARPTBN@@RFIPEgP
;87    1.500E+00 QPODBPAAPB@O@@RuHPeCP
;87    2.000E+00 XQNCSPVAOUcO@@RtRPdTP
;87    2.044E+00 XENsIPEiOfGO@@RdYPdPP
;87    3.000E+00 CaNrPPSIOaFPIULtBPdIP
;87    4.000E+00 RENbFPBHOqYPCeMdHPdFP
;87    5.000E+00 qGNQePQSObCPgUMtFPtDP
;87    6.000E+00 YVMqRPQIObQPQGNDXPDWP
;87    7.000E+00 GBMQUPyWNRePQXNdRPdQP
;87    8.000E+00 uHMAQPhENcEPQgNtWPtVP
;87    9.000E+00 dEMq@PWCNSSPrDNTbPTbP
;87    1.000E+01 CTMa@PfFNsXPrPNEHPEGP
;87    1.100E+01 BeMQBPUYNDBPCCNeCPeBP
;87    1.200E+01 rIMAEPEDNdDPsENuGPuGP
;87    1.300E+01 BDMY`OTYNDTPcTNURPUQP
;87    1.400E+01 qVMyGOd@NdSPSbNeUPeUP
;87    1.500E+01 QSMHiOChNDaPTINuYPuXP
;87    1.600E+01 qEMHWOcPNThPDTNUaPUaP
;87    1.800E+01 AFMwTOSENeHPT`NVDPVDP
;87    2.000E+01 hQLWDOB`NUUPuBNvEPvEP
;87    2.200E+01 WBLfSORRNE`PuQNVUPVUP
;87    2.400E+01 UhLVIObINFBPFFNvSPvSP
;87    2.600E+01 U@LEbOR@NfCPvHNV`PV`P
;87    2.800E+01 tILEYOQdNFRPfXNGEPGEP
;87    3.000E+01 CcLe@OA`NVYPVfNg@Pg@P
;87    4.000E+01 RELTCOqBNw@PX@NG`PG`P
;87    5.000E+01 qHLCUOADNGbPXfNhFPhFP
;87    6.000E+01 YWKRgOhRMhCPiSNhSPhSP
;87    8.000E+01 uHKrDOFPMHbPAFOYGPYGP
;87    1.000E+02 CUKQdOEIMiDPQDOYVPYVP
;87    1.500E+02 QSKqHOsFMYaPaEOABQABQ
;87    2.000E+02 hQJAIORQMACQqCOAEQAEQ
;87    3.000E+02 CcJwPNaWMAHQAROQ@QQ@Q
;87    4.000E+02 REJFCNaEMQ@QAWOQBQQBQ
;87    5.000E+02 qHJTiNYgLQBQQPOQDQQDQ
;87    6.000E+02 YWIdGNx@LQCQQSOQEQQEQ
;87    8.000E+02 uHIsCNfBLQEQQWOQGQQGQ
;87    1.000E+03 CUIrTNTgLQFQQYOQHQQHQ
;87    1.500E+03 QSIQbNsALQGQaSOQIQQIQ
;87    2.000E+03 hQHAXNBXLQHQaUOa@Qa@Q
;87    3.000E+03 CcHACNaULQIQaWOaAQaAQ
;87    4.000E+03 REHWfMaDLa@QaXOaAQaAQ
;87    5.000E+03 qHHVQMYbKa@QaYOaBQaBQ
;87    6.000E+03 YWGURMhGKa@QaYOaBQaBQ
;87    8.000E+03 uHGdEMf@Ka@QqPOaBQaBQ
;87    1.000E+04 CUGCWMTfKaAQqQOaBQaBQ
;87    1.500E+04 QSGBPMsAKaAQqQOaCQaCQ
;87    2.000E+04 hQFAdMBXKaAQqROaCQaCQ
;87    3.000E+04 CcFaGMaUKaAQqROaCQaCQ
;87    4.000E+04 REFyWLaDKaAQqROaCQaCQ
;87    5.000E+04 qHFWeLYbJaAQqROaCQaCQ
;87    6.000E+04 YWEvRLhFJaAQqROaCQaCQ
;87    8.000E+04 uHEUELf@JaAQqSOaCQaCQ
;87    1.000E+05 CUETILTfJaAQqSOaCQaCQ
;==== ELEMENT  88
;88    1.000E-03 q@StYOVIU@@R@@Rf@UVIU
;88    1.028E-03 q@STfOEhU@@R@@REiUEhU
;88    1.058E-03 q@SUCOUYU@@R@@RePUUYU
;88 N2 1.058E-03 q@SUCOeWU@@R@@ReXUeWU
;88    1.130E-03 aISUWOE@U@@R@@REBUE@U
;88    1.208E-03 aHSFCODQU@@R@@RDRUDQU
;88 N1 1.208E-03 aHSFCOTQU@@R@@RTRUTQU
;88    1.500E-03 aDSwQORdU@@R@@RReURdU
;88    2.000E-03 QHSAFPaQU@@R@@RaRUaQU
;88    3.000E-03 AFSaSPVVT@@R@@RfVTVVT
;88    3.105E-03 AESaYPFFT@@R@@RVGTFFT
;88 M5 3.105E-03 AESaYPqPU@@R@@RqQUqPU
;88    3.176E-03 ADSqSPQRU@@R@@RQSUQRU
;88    3.248E-03 ACSqWPqFU@@R@@RqGUqFU
;88 M4 3.248E-03 ACSqWPBCU@@R@@RBDUBCU
;88    3.510E-03 A@SQaPaTU@@R@@RaUUaTU
;88    3.792E-03 yQRBFPqCU@@R@@RqDUqCU
;88 M3 3.792E-03 yQRBFPQUU@@R@@RQVUQUU
;88    4.000E-03 IYRRFPqFU@@R@@RqGUqFU
;88    4.490E-03 I@RBPPAAU@@R@@RABUAAU
;88 M2 4.490E-03 I@RBPPAHU@@R@@RAIUAHU
;88    4.653E-03 HdRBXPIgT@@R@@RYfTIgT
;88    4.822E-03 hYRRUPIDT@@R@@RYCTIDT
;88 M1 4.822E-03 hYRRUPIST@@R@@RYRTIST
;88    5.000E-03 XRRbSPhVT@@R@@RxTThVT
;88    6.000E-03 gYRCFPUTT@@R@@ReQTUTT
;88    8.000E-03 vCRCaPrPT@@R@@RrWTrPT
;88    1.000E-02 eHRDXPQTT@@R@@RQYTQTT
;88    1.500E-02 SVRE`PEPS@@R@@RuVSEPS
;88    1.544E-02 CURU`PE@S@@R@@RuESEAS
;88 L3 1.544E-02 CURU`PaBT@@R@@RaETaBT
;88    1.690E-02 SARVIPYTS@@R@@RIfSYUS
;88    1.848E-02 B`RFWPGXS@@R@@RwWSGYS
;88 L2 1.848E-02 B`RFWPAET@@R@@RAHTAET
;88    1.886E-02 rSRVTPA@T@@R@@RACTA@T
;88    1.924E-02 bWRfPPYSS@@R@@RI`SYSS
;88 L1 1.924E-02 bWRfPPQ@T@@R@@RQCTQ@T
;88    2.000E-02 RTRvSPYgS@@R@@RABTYgS
;88    3.000E-02 QPRWgPSQS@@R@@RcVSSQS
;88    4.000E-02 AARxSPaTS@@R@@RqUSaUS
;88    5.000E-02 gAQYIPIDR@@R@@RIeRYCR
;88    6.000E-02 EQQIUPUSR@@R@@RVGReSR
;88    8.000E-02 CPQiUPRSR@@R@@RRgRbSR
;88    1.000E-01 rFQiTPqHR@@R@@RqQRAXR
;88    1.039E-01 bAQiRPaDR@@R@@RQVRqDR
;88 K  1.039E-01 bAQiRPUQR@@R@@REcReQR
;88    1.500E-01 QGQiGPRER@@R@@RrFRbDR
;88    2.000E-01 G@PxXPAAR@@R@@RQGRQ@R
;88    3.000E-01 sEPGhPSWQ@@R@@RtPQtFQ
;88    4.000E-01 QgPWGPqUQ@@R@@RbVQBWQ
;88    5.000E-01 aIPfQPACQ@@R@@RAbQaYQ
;88    6.000E-01 YCOVEPvVP@@R@@RqHQaIQ
;88    8.000E-01 eEOETPcQP@@R@@RYXPIEP
;88    1.000E+00 CPOTaPbHP@@R@@RWSPWIP
;88    1.022E+00 cFODfPRIP@@R@@RwGPGEP
;88    1.250E+00 b@ODPPAXPTIN@@RVDPUbP
;88    1.500E+00 QTODAPAEPBDO@@RERPeFP
;88    2.000E+00 xQNCSPvEOFCO@@RtUPdVP
;88    2.044E+00 xDNsHPVBOvGO@@RtRPdSP
;88    3.000E+00 S`NrPPsAOaHPISLtEPtAP
;88    4.000E+00 b@NbEPRFOAaPCdMt@PdHP
;88    5.000E+00 AQNQePQXObEPgTMtHPtFP
;88    6.000E+00 yXMqRPaDObSPQGNTPPDYP
;88    7.000E+00 WIMQUPAAORgPQWNdTPdSP
;88    8.000E+00 UPMAQPXVNcGPQgNtYPtYP
;88    9.000E+00 tEMaIPwINSUPrDNTePTdP
;88    1.000E+01 SRMa@PVPNCaPbYNU@PU@P
;88    1.100E+01 RaMQBPuYNDDPCBNeEPeEP
;88    1.200E+01 BUMAEPeBNdFPsDNEPPEPP
;88    1.300E+01 BIMIhOtVNDWPcSNUTPUTP
;88    1.400E+01 A`MyEOtFNdVPSaNeXPeXP
;88    1.500E+01 QWMHgODCNDdPTHNEaPEaP
;88    1.600E+01 qHMHUOsTNEAPDSNUdPUdP
;88    1.800E+01 AIMwROcGNuBPDiNVGPVGP
;88    2.000E+01 HaLWBORaNUYPuANvHPvHP
;88    2.200E+01 gHLfRObQNEcPeYNVXPVXP
;88    2.400E+01 VBLVHOrHNFFPFENvVPvVP
;88    2.600E+01 eALEaORHNfGPvGNVcPVcP
;88    2.800E+01 DYLEXOBANFVPfWNGIPGIP
;88    3.000E+01 SbLUIOAfNfSPVdNgDPgDP
;88    4.000E+01 b@LTBOqGNwDPHHNGePGeP
;88    5.000E+01 AQLCTOAHNGgPXcNxAPxAP
;88    6.000E+01 yYKRgOXdMhHPiPNhXPhXP
;88    8.000E+01 UQKrDOfSMHhPAFOiBPiBP
;88    1.000E+02 SRKQdOeGMy@PQCOiQPiQP
;88    1.500E+02 QWKqHOCXMYgPaEOABQABQ
;88    2.000E+02 HaJAHObPMADQqBOAFQAFQ
;88    3.000E+02 SbJgXNqSMAHQAQOQ@QQ@Q
;88    4.000E+02 b@JFANaIMQAQAWOQCQQCQ
;88    5.000E+02 AQJThNACMQCQQPOQEQQEQ
;88    6.000E+02 yYIdFNhPLQDQQSOQFQQFQ
;88    8.000E+02 UQIsCNFULQFQQVOQGQQGQ
;88    1.000E+03 SRIrTNUELQGQQYOQIQQIQ
;88    1.500E+03 QWIQaNCSLQHQaROa@Qa@Q
;88    2.000E+03 HaHAXNRWLQIQaTOaAQaAQ
;88    3.000E+03 SbHACNqQLa@QaWOaBQaBQ
;88    4.000E+03 b@HWeMaILa@QaXOaBQaBQ
;88    5.000E+03 AQHVPMACLaAQaYOaCQaCQ
;88    6.000E+03 yYGUQMXWKaAQaYOaCQaCQ
;88    8.000E+03 UQGdDMFSKaAQqPOaCQaCQ
;88    1.000E+04 SRGCWMUDKaAQqQOaCQaCQ
;88    1.500E+04 QWGrIMCSKaBQqQOaCQaCQ
;88    2.000E+04 HaFAdMRWKaBQqROaDQaDQ
;88    3.000E+04 SbFaGMqQKaBQqROaDQaDQ
;88    4.000E+04 b@FyULaHKaBQqROaDQaDQ
;88    5.000E+04 AQFWdLACKaBQqROaDQaDQ
;88    6.000E+04 yYEvQLXWJaBQqROaDQaDQ
;88    8.000E+04 UQEUDLFRJaBQqSOaDQaDQ
;88    1.000E+05 SRETILUDJaBQqSOaDQaDQ
;==== ELEMENT  89
;89    1.000E-03 qCSDgOFVU@@R@@RFWUFVU
;89    1.039E-03 qBSUBOFBU@@R@@RFCUFBU
;89    1.080E-03 qBSuHOeQU@@R@@ReRUeQU
;89 N2 1.080E-03 qBSuHOeYU@@R@@RuPUeYU
;89    1.171E-03 qASUcODhU@@R@@RT`UDhU
;89    1.269E-03 aISVQOTIU@@R@@Rd@UTIU
;89 N1 1.269E-03 aISVQOdHU@@R@@RdIUdHU
;89    1.500E-03 aFSWaOCGU@@R@@RCHUCGU
;89    2.000E-03 a@SAIPaXU@@R@@RqPUaXU
;89    3.000E-03 AGSaVPFgT@@R@@RVhTFgT
;89    3.219E-03 AESqXPEeT@@R@@RUfTEeT
;89 M5 3.219E-03 AESqXPQSU@@R@@RQTUQSU
;89    3.294E-03 ADSAbPAQU@@R@@RARUAQU
;89    3.370E-03 ACSAgPq@U@@R@@RqAUq@U
;89 M4 3.370E-03 ACSAgPAhU@@R@@RAiUAhU
;89    3.630E-03 A@SB@PQVU@@R@@RQWUQVU
;89    3.909E-03 yRRREPaIU@@R@@Rq@UaIU
;89 M3 3.909E-03 yRRREPAYU@@R@@RQPUAYU
;89    4.000E-03 iRRb@PARU@@R@@RASUARU
;89    4.656E-03 XgRRRPiVT@@R@@RyUTiVT
;89 M2 4.656E-03 XgRRRPABU@@R@@RACUABU
;89    5.000E-03 hURbXPhPT@@R@@RhYThPT
;89    5.002E-03 hURbXPXYT@@R@@RhXTXYT
;89 M1 5.002E-03 hURbXPXgT@@R@@RIETXgT
;89    6.000E-03 GaRS@PuUT@@R@@REcTuUT
;89    8.000E-03 FSRCePBaT@@R@@RBhTBaT
;89    1.000E-02 uHRTRPaPT@@R@@RaUTaPT
;89    1.500E-02 cSREdPeSS@@R@@RF@SeTS
;89    1.587E-02 CQRFCPDfS@@R@@ReASDgS
;89 L3 1.587E-02 CQRFCPQHT@@R@@RaATQHT
;89    1.740E-02 CGRvBPYHS@@R@@RIYSYHS
;89    1.908E-02 rURfRPWDS@@R@@RGSSWES
;89 L2 1.908E-02 rURfRPAAT@@R@@RADTAAT
;89    1.946E-02 bYRfXPYYS@@R@@RIgSiPS
;89    1.984E-02 bSRvTPYCS@@R@@RIPSYDS
;89 L1 1.984E-02 bSRvTPAET@@R@@RAHTAET
;89    2.000E-02 bPRvWPACT@@R@@RAFTADT
;89    3.000E-02 QSRHAPcUS@@R@@RCaScVS
;89    4.000E-02 ACRxWPqQS@@R@@RAbSqRS
;89    5.000E-02 wIQiCPITR@@R@@RACSYSR
;89    6.000E-02 UUQYPPuXR@@R@@RFSREhR
;89    8.000E-02 CYQyPPbVR@@R@@RS@RrUR
;89    1.000E-01 BRQiYPAUR@@R@@RqYRQTR
;89    1.068E-01 RGQiVPaAR@@R@@RQRRqAR
;89 K  1.068E-01 RGQiVPuAR@@R@@ReRREPR
;89    1.500E-01 a@QyBPbBR@@R@@RBSRrAR
;89    2.000E-01 g@PHcPAER@@R@@RaARQDR
;89    3.000E-01 CUPWcPsRQ@@R@@RDfQTQQ
;89    4.000E-01 BCPgBPAbQ@@R@@RrUQRUQ
;89    5.000E-01 qCPfUPAGQ@@R@@RAgQqTQ
;89    6.000E-01 IPOVIPGHP@@R@@RARQqCQ
;89    8.000E-01 EQOEXPsYP@@R@@RI`PiFP
;89    1.000E+00 SQOTdPrIP@@R@@RgYPwDP
;89    1.022E+00 sFODiPbIP@@R@@RWRPWHP
;89    1.250E+00 bGODSPQUPdIN@@RfEPFBP
;89    1.500E+00 QXODCPQ@PBIO@@RUPPuEP
;89    2.000E+00 XhNCUPfVOVIO@@RDbPtSP
;89    2.044E+00 hQNCQPFROVTO@@RtYPtPP
;89    3.000E+00 DBNrRPCWOqAPIYLDQPtGP
;89    4.000E+00 bGNbGPbGOAdPCgMtFPtDP
;89    5.000E+00 AUNQfPaVObIPgYMDTPDRP
;89    6.000E+00 AANqSPq@ObXPQHNTVPTUP
;89    7.000E+00 GRMQVPAFOCBPQYNtPPtPP
;89    8.000E+00 eXMARPXfNsBPQhNDfPDeP
;89    9.000E+00 DYMq@PwTNcPPrENEAPEAP
;89    1.000E+01 cTMaAPF`NCfPrQNUGPUGP
;89    1.100E+01 CAMQCPFFNT@PCENuBPuBP
;89    1.200E+01 RSMAFPEWNtCPsFNEXPEWP
;89    1.300E+01 REMYeOTgNTTPcVNeRPeRP
;89    1.400E+01 AfMIQOTVNtSPSdNuVPuVP
;89    1.500E+01 aRMXdOdANTbPd@NEiPEiP
;89    1.600E+01 ARMXQOSaNEIPDVNFBPFBP
;89    1.800E+01 QBMwXOCRNEPPTbNfFPfFP
;89    2.000E+01 Y@LWGOCDNeWPuDNFWPFWP
;89    2.200E+01 WRLfVOrSNUbPuSNfWPfWP
;89    2.400E+01 vBLfCOBXNVEPFHNFfPFfP
;89    2.600E+01 uHLEeObHNvFPFQNGCPGCP
;89    2.800E+01 dTLUQOR@NVUPvQNWIPWIP
;89    3.000E+01 DDLeBOQeNvSPViNwDPwDP
;89    4.000E+01 bGLTEOASNGUPXCNWfPWfP
;89    5.000E+01 AVLCVOQCNWhPXiNHSPHSP
;89    6.000E+01 AALRhOyEMHPPiVNH`PH`P
;89    8.000E+01 eXKrEOVcMIAPAGOyFPyFP
;89    1.000E+02 cTKQeOUQMITPQDOyUPyUP
;89    1.500E+02 aRKqIOcTMAAQaFOADQADQ
;89    2.000E+02 Y@JAIOrRMAEQqCOAHQAHQ
;89    3.000E+02 DDJwSNAaMQ@QAROQBQQBQ
;89    4.000E+02 bGJFFNqEMQCQAWOQEQQEQ
;89    5.000E+02 AVJEANAHMQDQQQOQFQQFQ
;89    6.000E+02 AAJdINXiLQFQQTOQHQQHQ
;89    8.000E+02 eXIsENvSLQGQQWOQIQQIQ
;89    1.000E+03 cTIrVNuHLQHQaPOa@Qa@Q
;89    1.500E+03 aRIQcNSYLa@QaSOaBQaBQ
;89    2.000E+03 Y@HAYNbYLaAQaVOaCQaCQ
;89    3.000E+03 DDHADNqYLaBQaXOaDQaDQ
;89    4.000E+03 bGHH@MqDLaBQaYOaDQaDQ
;89    5.000E+03 AVHVTMAGLaCQqPOaDQaDQ
;89    6.000E+03 AAHUUMXfKaCQqQOaEQaEQ
;89    8.000E+03 eXGdGMvRKaCQqQOaEQaEQ
;89    1.000E+04 cTGCYMuGKaCQqROaEQaEQ
;89    1.500E+04 aRGBQMSXKaDQqROaEQaEQ
;89    2.000E+04 Y@FAeMbYKaDQqSOaEQaEQ
;89    3.000E+04 DDFaHMqYKaDQqSOaFQaFQ
;89    4.000E+04 bGFIbLqDKaDQqSOaFQaFQ
;89    5.000E+04 AVFWiLAGKaDQqTOaFQaFQ
;89    6.000E+04 AAFvVLXeJaDQqTOaFQaFQ
;89    8.000E+04 eXEUHLvQJaDQqTOaFQaFQ
;89    1.000E+05 cTEdALuGJaDQqTOaFQaFQ
;==== ELEMENT  90
;90    1.000E-03 qCSDaOfPU@@R@@RfQUfPU
;90    1.081E-03 qBSuBOuRU@@R@@RuTUuRU
;90    1.168E-03 qASEgOTfU@@R@@RThUTfU
;90 N2 1.168E-03 qASEgOECU@@R@@REEUECU
;90    1.246E-03 q@SvDODUU@@R@@RDVUDUU
;90    1.329E-03 aISFdOScU@@R@@RSdUScU
;90 N1 1.329E-03 aISFdOD@U@@R@@RDBUD@U
;90    1.500E-03 aFSGhOSEU@@R@@RSFUSEU
;90    2.000E-03 a@SAIPqSU@@R@@RqTUqSU
;90    3.000E-03 AGSaVPGGT@@R@@RWHTGGT
;90    3.332E-03 ACSAdPUWT@@R@@ReWTUWT
;90 M5 3.332E-03 ACSAdPqHU@@R@@RqIUqHU
;90    3.410E-03 ABSAhPq@U@@R@@RqAUq@U
;90    3.491E-03 AASQbPaBU@@R@@RaCUaBU
;90 M4 3.491E-03 AASQbPqTU@@R@@RqUUqTU
;90    4.000E-03 YYRRIPaDU@@R@@RaEUaDU
;90    4.046E-03 YTRbAPaAU@@R@@RaBUaAU
;90 M3 4.046E-03 YTRbAPAQU@@R@@RAQUAQU
;90    4.421E-03 YGRrIPQCU@@R@@RQCUQCU
;90    4.830E-03 xXRRYPIBT@@R@@RY@TIBT
;90 M2 4.830E-03 xXRRYPYWT@@R@@RiUTYWT
;90    5.000E-03 hRRbWPxYT@@R@@RHhTxYT
;90    5.182E-03 HVRrUPHET@@R@@RXCTHET
;90 M1 5.182E-03 HVRrUPxIT@@R@@RHXTxIT
;90    6.000E-03 wYRCIPEgT@@R@@RUeTEgT
;90    8.000E-03 FSRCcPBgT@@R@@RRdTBgT
;90    1.000E-02 uHRDXPaTT@@R@@RaYTaTT
;90    1.500E-02 cTRuXPuWS@@R@@RVDSuXS
;90    1.630E-02 sBRFEPdUS@@R@@RTiSdVS
;90 L3 1.630E-02 sBRFEPQBT@@R@@RQFTQBT
;90    1.792E-02 RhRvEPhXS@@R@@RXhShXS
;90    1.969E-02 bVRfTPvQS@@R@@RVhSvRS
;90 L2 1.969E-02 bVRfTPiPS@@R@@RIgSiPS
;90    2.000E-02 bQRfYPY@S@@R@@RyGSYAS
;90    2.047E-02 RTRvVPXYS@@R@@RHeShPS
;90 L1 2.047E-02 RTRvVPYbS@@R@@RABTYcS
;90    3.000E-02 QTRWaPsSS@@R@@RCiSsTS
;90    4.000E-02 ACRhVPqUS@@R@@RAfSqVS
;90    5.000E-02 GTQYAPiXR@@R@@RAESyWR
;90    6.000E-02 UYQyHPUdR@@R@@RVYRFCR
;90    8.000E-02 SQQYXPrSR@@R@@RSHRBcR
;90    1.000E-01 BTQYWPAYR@@R@@RAcRQYR
;90    1.097E-01 BIQYSPQFR@@R@@RAVRaFR
;90 K  1.097E-01 BIQYSPECR@@R@@RuDRUCR
;90    1.500E-01 aAQiAPbFR@@R@@RBWRrER
;90    2.000E-01 gGPxSPAGR@@R@@RaCRQFR
;90    3.000E-01 CXPGdPCaQ@@R@@RTdQTYQ
;90    4.000E-01 BEPWDPAgQ@@R@@RrYQRXQ
;90    5.000E-01 qEPVXPQ@Q@@R@@RQ`QqVQ
;90    6.000E-01 YQOVBPgGP@@R@@RASQqDQ
;90    8.000E-01 EWOERPS`P@@R@@RIfPyAP
;90    1.000E+00 SUODiPBVP@@R@@RwQPwEP
;90    1.022E+00 CPODdPrFP@@R@@RWTPg@P
;90    1.250E+00 r@OtHPQYPtBN@@RfEPFBP
;90    1.500E+00 aQOSiPQCPRBO@@RUPPuDP
;90    2.000E+00 Y@NCQPFeOfCO@@RDaPtRP
;90    2.044E+00 xRNsGPfQOVXO@@RtXPdYP
;90    3.000E+00 DGNbYPSWOqAPyILDPPtFP
;90    4.000E+00 r@NbDPrCOAdPCcMtEPtBP
;90    5.000E+00 AWNQdPqQObIPgQMDRPDQP
;90    6.000E+00 ABNqQPqCObWPQGNTTPTSP
;90    7.000E+00 WRMQTPAIOCAPQWNdXPdXP
;90    8.000E+00 uVMAPPiANsBPQfNDdPDcP
;90    9.000E+00 TUMaIPWeNcPPrCNTiPTiP
;90    1.000E+01 cYMQIPViNCePbXNUEPUEP
;90    1.100E+01 CEMQAPfCNDIPCANu@Pu@P
;90    1.200E+01 RVMADPeRNtBPsBNEUPEUP
;90    1.300E+01 RHMIdOUANTRPcRNePPePP
;90    1.400E+01 AhMyAOdYNtRPS`NuTPuTP
;90    1.500E+01 aTMHdOtCNT`PTFNEgPEgP
;90    1.600E+01 ATMHRODBNEGPDQNF@PF@P
;90    1.800E+01 QDMwPOSRNuHPDgNfCPfCP
;90    2.000E+01 iBLW@OSBNeUPeINFUPFUP
;90    2.200E+01 gRLVYOBaNU`PeWNfUPfUP
;90    2.400E+01 FQLVFORUNVCPFBNFdPFcP
;90    2.600E+01 EVLuXOrDNvDPvDNGAPGAP
;90    2.800E+01 tQLEVORFNVSPfSNWGPWGP
;90    3.000E+01 T@LUGOB@NvQPVaNwBPwAP
;90    4.000E+01 rALTAOAWNGSPHDNWcPWcP
;90    5.000E+01 AXLCSOQFNWfPHiNHPPHPP
;90    6.000E+01 ABLReOYYMxGPYUNxWPxWP
;90    8.000E+01 uVKrCOWBMXhPAEOyCPyCP
;90    1.000E+02 cYKQcOeVMIQPQCOyRPyRP
;90    1.500E+02 aTKqHOsTMAAQaDOADQADQ
;90    2.000E+02 iBJAHOrYMAEQqBOAGQAGQ
;90    3.000E+02 T@JgUNAeMQ@QAQOQBQQBQ
;90    4.000E+02 rAJUiNqIMQBQAVOQDQQDQ
;90    5.000E+02 AXJTfNQAMQDQAYOQFQQFQ
;90    6.000E+02 ABJdENiCLQEQQROQGQQGQ
;90    8.000E+02 uVIsBNVbLQGQQVOQIQQIQ
;90    1.000E+03 cYIrSNUSLQHQQXOa@Qa@Q
;90    1.500E+03 aTIQaNcXLa@QaROaAQaAQ
;90    2.000E+03 iBHAWNrVLaAQaTOaBQaBQ
;90    3.000E+03 T@HACNAdLaAQaVOaCQaCQ
;90    4.000E+03 rAHWbMqHLaBQaWOaDQaDQ
;90    5.000E+03 AXHFWMQ@LaBQaXOaDQaDQ
;90    6.000E+03 ABHEYMi@KaBQaYOaDQaDQ
;90    8.000E+03 uVGdCMV`KaCQaYOaDQaDQ
;90    1.000E+04 cYGCUMURKaCQqPOaEQaEQ
;90    1.500E+04 aTGrIMcXKaCQqQOaEQaEQ
;90    2.000E+04 iBFAcMrVKaCQqQOaEQaEQ
;90    3.000E+04 T@FaFMAdKaCQqQOaEQaEQ
;90    4.000E+04 rAFyQLqHKaDQqROaEQaEQ
;90    5.000E+04 AXFWaLQ@KaDQqROaEQaEQ
;90    6.000E+04 ABFfYLYIJaDQqROaEQaEQ
;90    8.000E+04 uVEUCLFiJaDQqROaEQaEQ
;90    1.000E+05 cYETGLURJaDQqROaEQaEQ
;==== ELEMENT  91
;91    1.000E-03 qGStPOVRU@@R@@RVSUVRU
;91    1.003E-03 qGStROFXU@@R@@RFYUFXU
;91    1.007E-03 qGStTOFTU@@R@@RFUUFTU
;91 N3 1.007E-03 qGStTOFfU@@R@@RFgUFfU
;91    1.110E-03 qESuIOuRU@@R@@RuTUuRU
;91    1.224E-03 qDSFHOtXU@@R@@RD`UtXU
;91 N2 1.224E-03 qDSFHODeU@@R@@RDfUDeU
;91    1.303E-03 qCSVTOdIU@@R@@RtAUdIU
;91    1.387E-03 qBSGDOC`U@@R@@RCbUC`U
;91 N1 1.387E-03 qBSGDOChU@@R@@RCiUChU
;91    1.500E-03 q@SwROsAU@@R@@RsCUsAU
;91    2.000E-03 aDSAGPAbU@@R@@RAcUAbU
;91    3.000E-03 QASaTPGUT@@R@@RWVTGUT
;91    3.442E-03 AESAiPETT@@R@@RUUTETT
;91 M5 3.442E-03 AESAiPqEU@@R@@RqFUqEU
;91    3.525E-03 ADSQcPaFU@@R@@RaGUaFU
;91    3.611E-03 ACSQhPQGU@@R@@RQIUQGU
;91 M4 3.611E-03 ACSQhPaXU@@R@@RaYUaXU
;91    4.000E-03 YaRRHPq@U@@R@@RqAUq@U
;91    4.174E-03 yRRbGPQGU@@R@@RQHUQGU
;91 M3 4.174E-03 yRRbGPqFU@@R@@RqGUqFU
;91    5.000E-03 HiRbVPhWT@@R@@RxVThWT
;91    5.001E-03 HiRbVPhWT@@R@@RxVThWT
;91 M2 5.001E-03 HiRbVPi@T@@R@@RiITi@T
;91    5.181E-03 xXRBiPHST@@R@@RXRTHST
;91    5.367E-03 XVRBbPwST@@R@@RGaTwST
;91 M1 5.367E-03 XVRBbPHFT@@R@@RXDTHFT
;91    6.000E-03 HBRCIPVDT@@R@@RfBTVDT
;91    8.000E-03 fQRCdPCAT@@R@@RCGTCAT
;91    1.000E-02 USRTQPqQT@@R@@RqWTqQT
;91    1.500E-02 sUREdPFFS@@R@@RFTSFGS
;91    1.673E-02 sCRf@PTVS@@R@@RT`STWS
;91 L3 1.673E-02 sCRf@PQ@T@@R@@RQCTQ@T
;91    2.000E-02 rPRvWPvUS@@R@@RGBSvVS
;91    2.031E-02 bURFbPFWS@@R@@RvUSFXS
;91 L2 2.031E-02 bURFbPYHS@@R@@RIUSYIS
;91    2.071E-02 RXRFhPxTS@@R@@RI@SxTS
;91    2.110E-02 RSRVdPxBS@@R@@RXXSxBS
;91 L1 2.110E-02 RSRVdPiPS@@R@@RIfSiQS
;91    3.000E-02 QYRH@PSaS@@R@@RDHSSbS
;91    4.000E-02 AGRxWPAdS@@R@@RQfSAeS
;91    5.000E-02 gYQiCPABS@@R@@RQ@SACS
;91    6.000E-02 uXQYPPfFR@@R@@RVcRvER
;91    8.000E-02 cSQyQPBhR@@R@@RsDRRhR
;91    1.000E-01 RRQyQPQXR@@R@@RQbRaWR
;91    1.126E-01 BFQiUPQDR@@R@@RATRaDR
;91 K  1.126E-01 BFQiUPT`R@@R@@Re@RTiR
;91    1.500E-01 aFQyDPrFR@@R@@RRXRBUR
;91    2.000E-01 WSPHfPQBR@@R@@RaIRaAR
;91    3.000E-01 cQPWfPD@Q@@R@@RUEQtYQ
;91    4.000E-01 RCPgEPQgQ@@R@@RR`QbYQ
;91    5.000E-01 APPfXPQFQ@@R@@RQgQAcQ
;91    6.000E-01 IhOfBPgWP@@R@@RAYQqIQ
;91    8.000E-01 eYOUPPTAP@@R@@RABQiQP
;91    1.000E+00 cYOTgPbPP@@R@@RWdPWWP
;91    1.022E+00 STOTaPBYP@@R@@RwVPGQP
;91    1.250E+00 rIODUPaXPDWN@@RFRPVHP
;91    1.500E+00 aWODEPa@Pb@O@@ReTPEWP
;91    2.000E+00 IWNCVPgDOFUO@@RTcPDcP
;91    2.044E+00 IHNCRPVhOFaO@@RDiPD`P
;91    3.000E+00 dDNrSPsWOqEPYSLTPPDUP
;91    4.000E+00 rINbHPBVOAiPChMDTPDRP
;91    5.000E+00 QSNQgPA`OrEPwRMTRPTPP
;91    6.000E+00 AGNqTPAQOrTPQHNdTPdSP
;91    7.000E+00 GcMQVPQEOCHPQYNtYPtXP
;91    8.000E+00 F@MARPyQNCPPQiNTdPTdP
;91    9.000E+00 tTMqAPxINcXPrFNU@PU@P
;91    1.000E+01 CdMaAPwGNSdPrRNeFPeFP
;91    1.100E+01 SGMQCPVWNTIPCFNERPERP
;91    1.200E+01 bWMAFPUbNDRPsHNUWPUWP
;91    1.300E+01 bGMA@PuINdSPcWNuRPuRP
;91    1.400E+01 QfMIVOTdNDcPSeNEfPEfP
;91    1.500E+01 qQMXhOTVNEAPdBNF@PF@P
;91    1.600E+01 QPMXUOdDNUIPDXNVCPVCP
;91    1.800E+01 QIMGaOsQNUPPTdNvGPvGP
;91    2.000E+01 iPLgAOcINuXPuGNVYPVYP
;91    2.200E+01 WdLfYORfNFDPuUNF`PF`P
;91    2.400E+01 fWLfFObYNfGPVANViPViP
;91    2.600E+01 eXLEhOBVNFYPFSNWFPWFP
;91    2.800E+01 T`LUTObGNfXPvSNwCPwCP
;91    3.000E+01 dGLeEORANFfPGANGXPGXP
;91    4.000E+01 BPLTGOQUNgPPXFNXAPXAP
;91    5.000E+01 QTLCXOaBNXEPIBNhPPhPP
;91    6.000E+01 AGLC@OAANXWPiYNXgPXgP
;91    8.000E+01 F@KrGOWPMYIPAGOYTPYTP
;91    1.000E+02 CdKQfOUfMiSPQDOYePYeP
;91    1.500E+02 qQKAPOSdMACQaFOAFQAFQ
;91    2.000E+02 iPJQ@ORdMAGQqDOQ@QQ@Q
;91    3.000E+02 dGJwWNQeMQBQASOQDQQDQ
;91    4.000E+02 BPJFHNAVMQEQAXOQGQQGQ
;91    5.000E+02 QTJEDNQGMQGQQQOQIQQIQ
;91    6.000E+02 AGJtANySLQHQQTOa@Qa@Q
;91    8.000E+02 F@IsGNgILa@QQXOaBQaBQ
;91    1.000E+03 CdIrWNEbLaAQaPOaCQaCQ
;91    1.500E+03 qQIQdNChLaBQaTOaDQaDQ
;91    2.000E+03 iPHQPNRaLaCQaVOaEQaEQ
;91    3.000E+03 dGHADNQdLaDQaXOaFQaFQ
;91    4.000E+03 BPHHDMAULaEQaYOaGQaGQ
;91    5.000E+03 QTHVWMQFLaEQqPOaGQaGQ
;91    6.000E+03 AGHUWMiYKaEQqQOaGQaGQ
;91    8.000E+03 F@GdIMgFKaFQqROaGQaGQ
;91    1.000E+04 CdGSQMEaKaFQqROaHQaHQ
;91    1.500E+04 qQGBRMCgKaFQqSOaHQaHQ
;91    2.000E+04 iPFAfMR`KaFQqSOaHQaHQ
;91    3.000E+04 dGFaHMQdKaFQqSOaHQaHQ
;91    4.000E+04 BPFIfLAUKaFQqTOaHQaHQ
;91    5.000E+04 QTFHCLQFKaFQqTOaHQaHQ
;91    6.000E+04 AGFvYLiXJaGQqTOaHQaHQ
;91    8.000E+04 F@EeALgFJaGQqTOaHQaHQ
;91    1.000E+05 CdEdDLEaJaGQqTOaHQaHQ
;==== ELEMENT  92
;92    1.000E-03 qFSTSOfQU@@R@@RfSUfQU
;92    1.022E-03 qESdVOvFU@@R@@RvGUvFU
;92    1.045E-03 qEStYOVBU@@R@@RVCUVBU
;92 N3 1.045E-03 qEStYOVQU@@R@@RVRUVQU
;92    1.153E-03 qDSESOERU@@R@@RESUERU
;92    1.273E-03 qBSVBOTQU@@R@@RTSUTQU
;92 N2 1.273E-03 qBSVBOTXU@@R@@RTYUTXU
;92    1.354E-03 qASfPODEU@@R@@RDFUDEU
;92    1.441E-03 q@SWAOSYU@@R@@RcPUSYU
;92 N1 1.441E-03 q@SWAOcVU@@R@@RcWUcVU
;92    1.500E-03 aISGUOsGU@@R@@RsHUsGU
;92    2.000E-03 aCSACPAeU@@R@@RAgUAeU
;92    3.000E-03 Q@SaPPWXT@@R@@RgYTWXT
;92    3.552E-03 ADSAiPUET@@R@@ReFTUET
;92 M5 3.552E-03 ADSAiPaFU@@R@@RaGUaFU
;92    3.639E-03 ACSQcPQHU@@R@@RQIUQHU
;92    3.728E-03 ABSQhPQ@U@@R@@RQAUQ@U
;92 M4 3.728E-03 ABSQhPQWU@@R@@RQXUQWU
;92    4.000E-03 IeRRBPqBU@@R@@RqCUqBU
;92    4.303E-03 YSRbFPQ@U@@R@@RQAUQ@U
;92 M3 4.303E-03 YSRbFPaHU@@R@@RaIUaHU
;92    5.000E-03 HdRRYPH`T@@R@@RHiTH`T
;92    5.182E-03 hWRbWPHCT@@R@@RXBTHCT
;92 M2 5.182E-03 hWRbWPXRT@@R@@RhQTXRT
;92    5.362E-03 XQRrTPGcT@@R@@RWbTGcT
;92    5.548E-03 xERBbPg@T@@R@@RgHTg@T
;92 M1 5.548E-03 xERBbPWQT@@R@@RWYTWQT
;92    6.000E-03 WgRCAPf@T@@R@@RfHTf@T
;92    8.000E-03 VWRsUPCDT@@R@@RSATCDT
;92    1.000E-02 UPRDPPqTT@@R@@RqYTqTT
;92    1.500E-02 sSRuQPVES@@R@@RVSSVES
;92    1.717E-02 cBRVDPtCS@@R@@RdVStDS
;92 L3 1.717E-02 cBRVDPADT@@R@@RAGTADT
;92    2.000E-02 bYRfSPFcS@@R@@RWASFdS
;92    2.095E-02 RTRvWPFDS@@R@@Rv@SFES
;92 L2 2.095E-02 RTRvWPXXS@@R@@RHdSXXS
;92    2.135E-02 BXRFcPXGS@@R@@RHRSXGS
;92    2.176E-02 BRRFiPwWS@@R@@RHBSwXS
;92 L1 2.176E-02 BRRFiPXgS@@R@@RiBSXhS
;92    3.000E-02 QXRGcPSfS@@R@@RTCSSgS
;92    4.000E-02 AGRXXPAgS@@R@@RQhSAhS
;92    5.000E-02 gXQICPADS@@R@@RQBSADS
;92    6.000E-02 uXQyAPvFR@@R@@RGCRFVR
;92    8.000E-02 cSQYRPRdR@@R@@RCPRCCR
;92    1.000E-01 RRQYQPaQR@@R@@RQeRqPR
;92    1.156E-01 QgQISPAIR@@R@@RqHRQHR
;92 K  1.156E-01 QgQISPdPR@@R@@RDiRtPR
;92    1.500E-01 aFQYFPrGR@@R@@RRYRBVR
;92    2.000E-01 WTPhXPQDR@@R@@Rq@RaBR
;92    3.000E-01 cRPGaPDEQ@@R@@RUIQDcQ
;92    4.000E-01 RCPWAPB@Q@@R@@RRbQrQQ
;92    5.000E-01 APPVUPQHQ@@R@@RQhQAdQ
;92    6.000E-01 YbOV@PGaP@@R@@RAYQqIQ
;92    8.000E-01 uQOEPPTIP@@R@@RABQYYP
;92    1.000E+00 sQODgPbUP@@R@@RW`PWRP
;92    1.022E+00 SUODbPRTP@@R@@RwRPwFP
;92    1.250E+00 BPOtGPqRPDVN@@RvGPVCP
;92    1.500E+00 aXOShPaBPb@O@@RUYPERP
;92    2.000E+00 YRNCPPwHOFTO@@RDhPtXP
;92    2.044E+00 YBNsFPWAOF`O@@RDdPtUP
;92    3.000E+00 dFNbXPCeOqDPyFLDUPDPP
;92    4.000E+00 BPNbDPRQOAhPCaMtIPtGP
;92    5.000E+00 QTNQcPAcOrCPWXMDVPDUP
;92    6.000E+00 AGNqQPASOrQPQFNTXPTWP
;92    7.000E+00 GgMQTPQGOCEPQVNtSPtRP
;92    8.000E+00 FCMAPPIiNsFPQeNDhPDgP
;92    9.000E+00 tWMaHPXTNcTPrBNEDPECP
;92    1.000E+01 CfMQIPWQNS`PbWNUIPUIP
;92    1.100E+01 SIMQAPfYNTDPC@NuEPuEP
;92    1.200E+01 bXMADPFCNtFPsANUPPUPP
;92    1.300E+01 bIMIaOEYNTWPcPNeUPeUP
;92    1.400E+01 QgMiHOECNtWPChNuYPuYP
;92    1.500E+01 qRMHaOdUNTfPTDNUcPUcP
;92    1.600E+01 QQMxIOtANUCPtINFFPFEP
;92    1.800E+01 QIMgWOsWNETPDeNfIPfIP
;92    2.000E+01 iVLGGOsENuRPeFNVQPVQP
;92    2.200E+01 WhLVWOCANUgPeTNvQPvQP
;92    2.400E+01 vQLVDOrTNf@PUiNV`PV`P
;92    2.600E+01 uRLuVORQNFQPvANGHPGHP
;92    2.800E+01 TcLETOrANfQPfPNgDPgDP
;92    3.000E+01 dILUEORENvYPFhNwIPwIP
;92    4.000E+01 BQLDIOQXNWQPH@NHBPHBP
;92    5.000E+01 QULCROaENHEPHdNHYPHYP
;92    6.000E+01 AGLRdOACNHWPYQNHgPHgP
;92    8.000E+01 FDKrBOgTMIIPAEOISPISP
;92    1.000E+02 CfKQcOFGMYRPQBOIcPIcP
;92    1.500E+02 qRKqGODAMABQaDOAEQAEQ
;92    2.000E+02 iVJAHOC@MAFQqAOAIQAIQ
;92    3.000E+02 dIJgRNQiMQAQAPOQCQQCQ
;92    4.000E+02 BQJUgNAYMQDQAUOQFQQFQ
;92    5.000E+02 QUJTdNQIMQEQAXOQGQQGQ
;92    6.000E+02 AGJdCNIiLQGQQQOQIQQIQ
;92    8.000E+02 FDIs@NGQLQHQQUOa@Qa@Q
;92    1.000E+03 CfIrRNUcLQIQQWOaAQaAQ
;92    1.500E+03 qRIQ`NSeLaAQaQOaCQaCQ
;92    2.000E+03 iVHAWNRfLaBQaSOaDQaDQ
;92    3.000E+03 dIHABNQgLaCQaUOaEQaEQ
;92    4.000E+03 BQHGiMAXLaCQaVOaEQaEQ
;92    5.000E+03 QUHFUMQHLaDQaWOaEQaEQ
;92    6.000E+03 AGHEWMIeKaDQaWOaFQaFQ
;92    8.000E+03 FDGdAMwIKaDQaXOaFQaFQ
;92    1.000E+04 CfGCTMUaKaDQaYOaFQaFQ
;92    1.500E+04 qRGrHMSdKaEQaYOaFQaFQ
;92    2.000E+04 iVFAcMRfKaEQqPOaGQaGQ
;92    3.000E+04 dIFaFMQgKaEQqPOaGQaGQ
;92    4.000E+04 BQFiXLAXKaEQqPOaGQaGQ
;92    5.000E+04 QUFGhLQHKaEQqPOaGQaGQ
;92    6.000E+04 AGFfVLIeJaEQqPOaGQaGQ
;92    8.000E+04 FDEUALwIJaEQqPOaGQaGQ
;92    1.000E+05 CfETFLUaJaEQqQOaGQaGQ
;==== ELEMENT  93
;93    1.000E-03 qISTROVdU@@R@@RVeUVdU
;93    1.042E-03 qIStXOFTU@@R@@RFVUFTU
;93    1.087E-03 qHSEEOUhU@@R@@RF@UUhU
;93 N3 1.087E-03 qHSEEOvGU@@R@@RvHUvGU
;93    1.201E-03 qGSeYOeGU@@R@@ReIUeGU
;93    1.328E-03 qESFUOtGU@@R@@RtHUtGU
;93 N2 1.328E-03 qESFUODSU@@R@@RDTUDSU
;93    1.500E-03 qCSGWOCXU@@R@@RCYUCXU
;93    1.501E-03 qCSGWOCWU@@R@@RCYUCWU
;93 N1 1.501E-03 qCSGWOSTU@@R@@RSUUSTU
;93    2.000E-03 aFSADPQeU@@R@@RQfUQeU
;93    3.000E-03 QCSaPPWhT@@R@@RHITWhT
;93    3.666E-03 AESQfPEDT@@R@@RUETEDT
;93 M5 3.666E-03 AESQfPaAU@@R@@RaBUaAU
;93    3.757E-03 ADSB@PQCU@@R@@RQDUQCU
;93    3.850E-03 ACSBEPAGU@@R@@RAHUAGU
;93 M4 3.850E-03 ACSBEPQQU@@R@@RQRUQQU
;93    4.000E-03 AASRCPqHU@@R@@RqIUqHU
;93    4.435E-03 iWRrDPAGU@@R@@RAHUAGU
;93 M3 4.435E-03 iWRrDPaEU@@R@@RaFUaEU
;93    5.000E-03 IIRbPPiCT@@R@@RyBTiCT
;93    5.366E-03 xTRrVPwQT@@R@@RG`TwQT
;93 M2 5.366E-03 xTRrVPXHT@@R@@RhGTXHT
;93    5.542E-03 XYRBdPWUT@@R@@RgTTWUT
;93    5.723E-03 HSRRaPVgT@@R@@RGFTVgT
;93 M1 5.723E-03 HSRRaPgGT@@R@@RwFTgGT
;93    6.000E-03 XIRCCPFYT@@R@@RVWTFYT
;93    8.000E-03 vURsXPSHT@@R@@RcETSHT
;93    1.000E-02 eURDTPAbT@@R@@RAgTAbT
;93    1.500E-02 CdRuWPFUS@@R@@RFdSFUS
;93    1.761E-02 cBRfIPdFS@@R@@RTYSdFS
;93 L3 1.761E-02 cBRfIPAAT@@R@@RAETAAT
;93    2.000E-02 rWRvQPWFS@@R@@RGUSWGS
;93    2.160E-02 RRRVePEbS@@R@@RFHSEcS
;93 L2 2.160E-02 RRRVePhIS@@R@@RXUSx@S
;93    2.201E-02 BVRGAPW`S@@R@@RXESW`S
;93    2.243E-02 BQRGGPWQS@@R@@RwVSWRS
;93 L1 2.243E-02 BQRGGPhWS@@R@@RXbShXS
;93    3.000E-02 aSRWcPTDS@@R@@RtASTES
;93    4.000E-02 Q@RhXPQfS@@R@@RBHSQgS
;93    5.000E-02 WcQYEPAIS@@R@@RQHSQ@S
;93    6.000E-02 UgQISPfYR@@R@@RwIRvYR
;93    8.000E-02 sUQiTPCIR@@R@@RSWRSIR
;93    1.000E-01 bPQiTPqPR@@R@@RBERqYR
;93    1.187E-01 QeQYTPAGR@@R@@RqFRQFR
;93 K  1.187E-01 QeQYTPDXR@@R@@RtWRTWR
;93    1.500E-01 q@QiIPBWR@@R@@RrPRRWR
;93    2.000E-01 GaPHaPQIR@@R@@RqERaGR
;93    3.000E-01 sUPWbPdEQ@@R@@REQQEDQ
;93    4.000E-01 bAPgAPR@Q@@R@@RCDQBbQ
;93    5.000E-01 AUPfUPaDQ@@R@@RBEQQaQ
;93    6.000E-01 ACPVIPhBP@@R@@RQTQATQ
;93    8.000E-01 UcOEXPDRP@@R@@RAEQY`P
;93    1.000E+00 CeOTdPB`P@@R@@RXCPwTP
;93    1.022E+00 cYODiPbXP@@R@@RWdPWXP
;93    1.250E+00 BYODSPAaPdPN@@RVTPfIP
;93    1.500E+00 qTODDPaIPbHO@@RuSPUUP
;93    2.000E+00 Y`NCUPwYOfUO@@RTiPDiP
;93    2.044E+00 IXNCQPWPOGBO@@RTfPDfP
;93    3.000E+00 DSNrRPDEOqHPYPLTUPTPP
;93    4.000E+00 RPNbGPbTOQbPCgMDYPDVP
;93    5.000E+00 aPNQfPQcOrHPgYMTVPTTP
;93    6.000E+00 QANqSPQQOrWPQHNdXPdWP
;93    7.000E+00 XIMQVPaDOSBPQYNDcPDbP
;93    8.000E+00 fGMARPADOCSPQhNThPTgP
;93    9.000E+00 TfMq@PI@NsRPrENUDPUDP
;93    1.000E+01 DBMaAPWaNSiPrQNu@Pu@P
;93    1.100E+01 sBMQCPGENdCPCDNEVPEVP
;93    1.200E+01 rYMAFPvENDVPsFNeRPeQP
;93    1.300E+01 rHMYfOuXNdWPcVNuWPuVP
;93    1.400E+01 BEMIROu@NDhPSdNUaPUaP
;93    1.500E+01 qYMXdODiNEFPd@NFEPFEP
;93    1.600E+01 QWMXROTTNeDPDUNVHPVHP
;93    1.800E+01 aDMwXOSgNUVPTbNFSPFSP
;93    2.000E+01 A@MWHOSSNEdPuDNfUPfUP
;93    2.200E+01 x@LfWOSGNV@PuRNFfPFfP
;93    2.400E+01 VhLfCOBhNvDPFHNGEPGEP
;93    2.600E+01 UdLEeObTNVUPFPNgCPgCP
;93    2.800E+01 UCLUROBSNvUPvPNGPPGPP
;93    3.000E+01 DVLeCObFNVdPVhNWUPWUP
;93    4.000E+01 RQLTEOaVNgXPXBNXIPXIP
;93    5.000E+01 aQLCWOqANhCPXgNhXPhXP
;93    6.000E+01 QBLRiOAHNhVPiTNIGPIGP
;93    8.000E+01 fHKrFOHDMiIPAFOiTPiTP
;93    1.000E+02 DBKQfOvIMyTPQDOAAQAAQ
;93    1.500E+02 qYKqIOdBMADQaEOAGQAGQ
;93    2.000E+02 A@KAIOSEMAIQqCOQAQQAQ
;93    3.000E+02 DWJwTNBIMQCQAROQFQQFQ
;93    4.000E+02 RQJFFNQWMQFQAWOQHQQHQ
;93    5.000E+02 aQJEBNaEMQHQQPOa@Qa@Q
;93    6.000E+02 QBJt@NADMQIQQSOaAQaAQ
;93    8.000E+02 fHIsFNG`LaAQQWOaCQaCQ
;93    1.000E+03 DBIrVNfDLaBQQYOaDQaDQ
;93    1.500E+03 qYIQcNTELaDQaSOaFQaFQ
;93    2.000E+03 A@IAYNSALaEQaUOaFQaFQ
;93    3.000E+03 DWHADNBGLaFQaWOaGQaGQ
;93    4.000E+03 RQHHAMQVLaFQaXOaHQaHQ
;93    5.000E+03 aQHVUMaDLaFQaYOaHQaHQ
;93    6.000E+03 QBHUUMADLaGQqPOaHQaHQ
;93    8.000E+03 fHGdHMwXKaGQqPOaIQaIQ
;93    1.000E+04 DBGCYMfBKaGQqQOaIQaIQ
;93    1.500E+04 qYGBQMTEKaGQqQOaIQaIQ
;93    2.000E+04 A@GAfMSAKaHQqROaIQaIQ
;93    3.000E+04 DWFaHMBGKaHQqROaIQaIQ
;93    4.000E+04 RQFIbLQVKaHQqROq@Qq@Q
;93    5.000E+04 aQFH@LaDKaHQqROq@Qq@Q
;93    6.000E+04 QBFvVLADKaHQqSOq@Qq@Q
;93    8.000E+04 fHEUILwWJaHQqSOq@Qq@Q
;93    1.000E+05 DBEdBLfBJaHQqSOq@Qq@Q
;==== ELEMENT  94
;94    1.000E-03 ARSDTOWHU@@R@@RWIUWHU
;94    1.056E-03 AQStWOVRU@@R@@RVTUVRU
;94    1.115E-03 APSUAOUcU@@R@@RUdUUcU
;94 N3 1.115E-03 APSUAOv@U@@R@@RvBUv@U
;94    1.237E-03 qISEaOUHU@@R@@RUIUUHU
;94    1.372E-03 qGSVYOdEU@@R@@RdGUdEU
;94 N2 1.372E-03 qGSVYOtAU@@R@@RtBUtAU
;94    1.500E-03 qESwCOcQU@@R@@RcRUcQU
;94    1.559E-03 qESgWOsDU@@R@@RsEUsDU
;94 N1 1.559E-03 qESgWOCPU@@R@@RCRUCPU
;94    2.000E-03 aISABPBBU@@R@@RBDUBBU
;94    3.000E-03 QFSQXPhHT@@R@@RxIThHT
;94    3.778E-03 AFSQhPDhT@@R@@RTiTDhT
;94 M5 3.778E-03 AFSQhPQGU@@R@@RQHUQGU
;94    3.874E-03 AESBCPAIU@@R@@RQ@UAIU
;94    3.973E-03 ADSBGPABU@@R@@RACUABU
;94 M4 3.973E-03 ADSBGPqIU@@R@@RAPUqIU
;94    4.000E-03 ADSBIPARU@@R@@RASUARU
;94    4.557E-03 ySRrEPACU@@R@@RADUACU
;94 M3 4.557E-03 ySRrEPa@U@@R@@RaAUa@U
;94    5.000E-03 iGRRUPYVT@@R@@RiUTYVT
;94    5.541E-03 xURrYPwDT@@R@@RGSTwDT
;94 M2 5.541E-03 xURrYPwYT@@R@@RGhTwYT
;94    5.734E-03 XWRBgPWFT@@R@@RgETWFT
;94    5.933E-03 HPRRePVXT@@R@@RfWTVXT
;94 M1 5.933E-03 HPRRePFfT@@R@@RVeTFgT
;94    6.000E-03 xDRRhPfXT@@R@@RvVTfXT
;94    8.000E-03 FfRsSPcHT@@R@@RsETcHT
;94    1.000E-02 uTRtIPAhT@@R@@RQdTAhT
;94    1.500E-02 SaRuSPfWS@@R@@RGFSfWS
;94    1.806E-02 SHRvEPTCS@@R@@RDUSTDS
;94 L3 1.806E-02 SHRvEPyYS@@R@@RAATyYS
;94    2.000E-02 BbRfYPGRS@@R@@RwQSGSS
;94    2.227E-02 BXRGCPUUS@@R@@RE`SUVS
;94 L2 2.227E-02 BXRGCPWbS@@R@@RXGSWbS
;94    2.268E-02 BRRGIPWTS@@R@@RwYSWUS
;94    2.310E-02 rFRWEPWIS@@R@@RGTSg@S
;94 L1 2.310E-02 rFRWEPx@S@@R@@RXTSx@S
;94    3.000E-02 aWRWbPdHS@@R@@RDUSdIS
;94    4.000E-02 QBRhXPBCS@@R@@RRESBDS
;94    5.000E-02 HIQYDPQCS@@R@@RaBSQDS
;94    6.000E-02 V@QIRPVeR@@R@@RgURGDR
;94    8.000E-02 CcQiUPcBR@@R@@RsPRsBR
;94    1.000E-01 bVQiUPqWR@@R@@RRCRAfR
;94    1.218E-01 QaQYSPADR@@R@@RqBRQCR
;94 K  1.218E-01 QaQYSPt@R@@R@@RTYRDPR
;94    1.500E-01 qCQy@PRTR@@R@@RrWRbSR
;94    2.000E-01 WiPHbPaBR@@R@@RqIRqAR
;94    3.000E-01 CdPWcPtIQ@@R@@RUWQUHQ
;94    4.000E-01 bFPgBPRHQ@@R@@RSBQR`Q
;94    5.000E-01 AYPfVPaIQ@@R@@RR@QQfQ
;94    6.000E-01 AEPf@PXUP@@R@@RQXQAWQ
;94    8.000E-01 FHOEYPdPP@@R@@RAGQAAQ
;94    1.000E+00 SeOTePRaP@@R@@RhFPGgP
;94    1.022E+00 sYOT`PrYP@@R@@RHGPwPP
;94    1.250E+00 RVODTPAiPdXN@@RfSPvHP
;94    1.500E+00 qYODEPqDPrCO@@RE`PeRP
;94    2.000E+00 ABOCVPXAOvXO@@REEPTeP
;94    2.044E+00 ySNCRPGaOWEO@@REAPTaP
;94    3.000E+00 TUNrSPdBOAPPYRLTYPTUP
;94    4.000E+00 RWNbGPrUOQePChMTSPTPP
;94    5.000E+00 aUNQgPBAOBQPwQMdPPTXP
;94    6.000E+00 QDNqTPQWOB`PQHNtRPtQP
;94    7.000E+00 HQMQVPaIOSEPQYNDfPDfP
;94    8.000E+00 FTMARPAHOCVPQhNEBPEAP
;94    9.000E+00 EIMqAPyFNsUPrFNUHPUHP
;94    1.000E+01 TBMaAPhBNDBPrQNuEPuDP
;94    1.100E+01 CQMQCPwCNdGPCENUQPUPP
;94    1.200E+01 BfMAFPfPNTPPsGNeVPeVP
;94    1.300E+01 BTMYhOFANtRPcVNEaPEaP
;94    1.400E+01 RAMITOUQNTbPSeNUfPUfP
;94    1.500E+01 AcMXfOEINUAPdANV@PV@P
;94    1.600E+01 aQMXTOtRNeIPDVNfCPfCP
;94    1.800E+01 aGMG`OTCNeQPTcNFXPFXP
;94    2.000E+01 ACMWIOcWNEiPuENvQPvPP
;94    2.200E+01 XSLfXOs@NVFPuTNVbPVbP
;94    2.400E+01 WFLfEOC@NFPPFINWAPWAP
;94    2.600E+01 V@LEfOrTNfRPFQNgIPgIP
;94    2.800E+01 eGLUSORSNFaPvQNGVPGVP
;94    3.000E+01 TXLeDOrENG@PViNgRPgRP
;94    4.000E+01 RXLTFOqRNwUPXCNhFPhFP
;94    5.000E+01 aULCWOqFNxAPXhNxVPxVP
;94    6.000E+01 QELC@OQCNxTPiVNYEPYEP
;94    8.000E+01 FUKrFOxEMyHPAGOySPySP
;94    1.000E+02 TCKQfOfSMIcPQDOAAQAAQ
;94    1.500E+02 AcKAPOtHMAEQaFOAHQAHQ
;94    2.000E+02 ACKAIOcGMQ@QqCOQBQQBQ
;94    3.000E+02 TXJwVNRGMQDQAROQGQQGQ
;94    4.000E+02 RXJFGNaSMQGQAWOQIQQIQ
;94    5.000E+02 aUJECNq@MQIQQPOaAQaAQ
;94    6.000E+02 QEJtANAHMa@QQSOaBQaBQ
;94    8.000E+02 FUIsFNX@LaBQQWOaDQaDQ
;94    1.000E+03 TCIrWNFXLaCQQYOaEQaEQ
;94    1.500E+03 AcIQcNtALaEQaSOaGQaGQ
;94    2.000E+03 ACIQPNcCLaFQaUOaHQaHQ
;94    3.000E+03 TXHADNRFLaGQaWOaIQaIQ
;94    4.000E+03 RXHHCMaRLaGQaXOaIQaIQ
;94    5.000E+03 aUHVVMaILaHQaYOaIQaIQ
;94    6.000E+03 QEHUVMAHLaHQaYOq@Qq@Q
;94    8.000E+03 FUGdIMHHKaHQqPOq@Qq@Q
;94    1.000E+04 TCGSPMFVKaHQqQOq@Qq@Q
;94    1.500E+04 AcGBRMtAKaIQqQOq@Qq@Q
;94    2.000E+04 ACGAfMcCKaIQqROqAQqAQ
;94    3.000E+04 TXFaHMREKaIQqROqAQqAQ
;94    4.000E+04 RXFIdLaRKaIQqROqAQqAQ
;94    5.000E+04 aUFHBLaIKaIQqROqAQqAQ
;94    6.000E+04 QEFvXLAHKaIQqROqAQqAQ
;94    8.000E+04 FUEe@LHGJaIQqSOqAQqAQ
;94    1.000E+05 TCEdCLFVJaIQqSOqAQqAQ
;==== ELEMENT  95
;95    1.000E-03 ARStCOwEU@@R@@RwGUwEU
;95    1.066E-03 ARStQOVXU@@R@@RVYUVXU
;95    1.136E-03 AQSUAOEiU@@R@@RU`UEiU
;95 N3 1.136E-03 AQSUAOfFU@@R@@RfGUfFU
;95    1.266E-03 qISEdOEIU@@R@@RU@UEIU
;95    1.412E-03 qHSfVOTDU@@R@@RTEUTDU
;95 N2 1.412E-03 qHSfVOTIU@@R@@Rd@UTIU
;95    1.500E-03 qFSWFOsQU@@R@@RsSUsQU
;95    1.617E-03 qESGcOSIU@@R@@Rc@USIU
;95 N1 1.617E-03 qESGcOcEU@@R@@RcFUcEU
;95    2.000E-03 q@SA@PBHU@@R@@RR@UBHU
;95    3.000E-03 QGSQUPXRT@@R@@RhTTXRT
;95    3.887E-03 AFSB@PtPT@@R@@RDaTtPT
;95 M5 3.887E-03 AFSB@PQ@U@@R@@RQAUQ@U
;95    4.000E-03 ADSBEPADU@@R@@RAEUADU
;95    4.092E-03 ACSR@PyUT@@R@@RIeTyUT
;95 M4 4.092E-03 ACSR@PqHU@@R@@RqIUqHU
;95    4.370E-03 A@SbCPQGU@@R@@RQHUQGU
;95    4.667E-03 iYRrFPYhT@@R@@RAAUYhT
;95 M3 4.667E-03 iYRrFPQFU@@R@@RQGUQFU
;95    5.000E-03 yDRRQPI`T@@R@@RY`TI`T
;95    5.710E-03 hVRBaPVhT@@R@@RGGTVhT
;95 M2 5.710E-03 hVRBaPGPT@@R@@RGYTGPT
;95    6.000E-03 HPRRcPVVT@@R@@RfTTVVT
;95    6.121E-03 x@RRhPfDT@@R@@RvBTfDT
;95 M1 6.121E-03 x@RRhPVPT@@R@@RVXTVPT
;95    8.000E-03 VaRcWPsFT@@R@@RCSTsFT
;95    1.000E-02 uXRtCPQbT@@R@@RQhTQbT
;95    1.500E-02 SdReVPFdS@@R@@RgDSFdS
;95    1.850E-02 SBRvGPShS@@R@@Rt@SSiS
;95 L3 1.850E-02 SBRvGPyHS@@R@@RyPSyIS
;95    2.000E-02 BeRfRPgTS@@R@@RWdSgUS
;95    2.294E-02 BQRGFPeES@@R@@RUPSeFS
;95 L2 2.294E-02 BQRGFPWPS@@R@@RwUSWQS
;95    2.335E-02 rERWBPWFS@@R@@RGPSWFS
;95    2.377E-02 r@RWGPFcS@@R@@RGGSFdS
;95 L1 2.377E-02 r@RWGPGhS@@R@@RXBSGiS
;95    3.000E-02 aXRGePtHS@@R@@RTUStHS
;95    4.000E-02 QCRhPPBHS@@R@@Rb@SBIS
;95    5.000E-02 XHQIGPQFS@@R@@RaESQGS
;95    6.000E-02 VGQyEPWER@@R@@RGfRgDR
;95    8.000E-02 CgQYWPsBR@@R@@RC`RCQR
;95    1.000E-01 bYQYWPAbR@@R@@RRIRQbR
;95    1.250E-01 AeQITPA@R@@R@@RaHRQ@R
;95 K  1.250E-01 AeQITPT@R@@R@@RtHRTIR
;95    1.500E-01 qEQiCPRYR@@R@@RBbRbXR
;95    2.000E-01 X@PxVPaER@@R@@RARRqDR
;95    3.000E-01 CiPGhPTPQ@@R@@ReXQeIQ
;95    4.000E-01 bIPWHPbDQ@@R@@RSHQReQ
;95    5.000E-01 QQPfRPqCQ@@R@@RRDQQiQ
;95    6.000E-01 AGPVFPH`P@@R@@RaPQQPQ
;95    8.000E-01 VHOEUPtTP@@R@@RAHQABQ
;95    1.000E+00 DAOTbPCAP@@R@@RxCPWcP
;95    1.022E+00 CeODgPBhP@@R@@RXDPwUP
;95    1.250E+00 bPODRPQePtRN@@RfWPFQP
;95    1.500E+00 AbODBPqIPrFO@@REbPeTP
;95    2.000E+00 ACOCTPxGOFeO@@REFPTfP
;95    2.044E+00 Y`NsIPHGOgCO@@REBPTbP
;95    3.000E+00 dSNrQPtFOAQPIVLdPPTUP
;95    4.000E+00 bQNbFPBdOQfPCeMTSPTPP
;95    5.000E+00 aXNQePBGOBQPgVMdPPTXP
;95    6.000E+00 QFNqSPaROB`PQGNtQPtPP
;95    7.000E+00 XVMQUPqCOSEPQXNDfPDeP
;95    8.000E+00 VVMAQPQBOCWPQgNEBPEAP
;95    9.000E+00 UHMq@PiUNsVPrDNUHPUGP
;95    1.000E+01 d@Ma@PHWNDBPrPNuDPuDP
;95    1.100E+01 CWMQBPWUNdGPCCNUPPUPP
;95    1.200E+01 RbMAEPFaNTPPsDNeVPeVP
;95    1.300E+01 BYMYbOVINtRPcTNEaPEaP
;95    1.400E+01 RDMyIOeXNTbPSbNUfPUfP
;95    1.500E+01 AgMXaOeDNUAPTHNV@PV@P
;95    1.600E+01 aTMHYODgNeIPDSNfCPfCP
;95    1.800E+01 q@MwUOdENeQPT`NFXPFXP
;95    2.000E+01 AEMWEOsXNU`PuBNvPPvPP
;95    2.200E+01 hXLfTOCPNVFPuPNVbPVaP
;95    2.400E+01 gILfAOCHNFPPFENWAPWAP
;95    2.600E+01 fBLEcOBbNfRPvGNgIPgIP
;95    2.800E+01 uFLUPObQNFbPfWNGVPGVP
;95    3.000E+01 dWLeAOBRNG@PVdNgRPgRP
;95    4.000E+01 bSLTDOqXNwVPHGNhGPhGP
;95    5.000E+01 aXLCUOAPNxBPXbNxWPxWP
;95    6.000E+01 QGLRhOQFNxUPYYNYFPYFP
;95    8.000E+01 VWKrEOXYMyIPAFOyTPyTP
;95    1.000E+02 d@KQeOFcMIdPQCOABQABQ
;95    1.500E+02 AgKqIOTQMAEQaEOAHQAHQ
;95    2.000E+02 AEKAIOsGMQ@QqBOQBQQBQ
;95    3.000E+02 dWJwQNbDMQEQAQOQGQQGQ
;95    4.000E+02 bSJFDNaWMQGQAVOa@Qa@Q
;95    5.000E+02 aXJE@NqDMQIQAYOaAQaAQ
;95    6.000E+02 QGJdHNQAMaAQQROaCQaCQ
;95    8.000E+02 VWIsDNxDLaBQQUOaDQaDQ
;95    1.000E+03 d@IrUNfWLaCQQXOaEQaEQ
;95    1.500E+03 AgIQbNDTLaEQaQOaGQaGQ
;95    2.000E+03 AEIAYNsCLaFQaSOaHQaHQ
;95    3.000E+03 dWHACNbBLaGQaVOaIQaIQ
;95    4.000E+03 bSHWhMaVLaHQaWOaIQaIQ
;95    5.000E+03 aXHVRMqCLaHQaXOq@Qq@Q
;95    6.000E+03 QGHUSMQALaHQaXOq@Qq@Q
;95    8.000E+03 VWGdFMxBKaHQaYOq@Qq@Q
;95    1.000E+04 d@GCXMfUKaIQaYOq@Qq@Q
;95    1.500E+04 AgGBPMDSKaIQqPOqAQqAQ
;95    2.000E+04 AEGAeMsBKaIQqPOqAQqAQ
;95    3.000E+04 dWFaGMbBKaIQqQOqAQqAQ
;95    4.000E+04 bSFyYLaVKaIQqQOqAQqAQ
;95    5.000E+04 aXFWgLqCKaIQqQOqAQqAQ
;95    6.000E+04 QGFvTLQAKaIQqQOqAQqAQ
;95    8.000E+04 VWEUGLxAJaIQqQOqAQqAQ
;95    1.000E+05 d@Ed@LfUJaIQqQOqAQqAQ
;==== ELEMENT  96
;96    1.000E-03 ASSdEOWRU@@R@@RWTUWRU
;96    1.074E-03 ARSdXOfTU@@R@@RfVUfTU
;96    1.154E-03 AQSUDOEgU@@R@@REhUEgU
;96 N3 1.154E-03 AQSUDOfCU@@R@@RfDUfCU
;96    1.289E-03 APSEiOEEU@@R@@REFUEEU
;96    1.440E-03 qHSvSODIU@@R@@RT@UDIU
;96 N2 1.440E-03 qHSvSOTGU@@R@@RTIUTGU
;96    1.500E-03 qGSGHOCbU@@R@@RCcUCbU
;96    1.643E-03 qESGhOSGU@@R@@RSHUSGU
;96 N1 1.643E-03 qESGhOcCU@@R@@RcDUcCU
;96    2.000E-03 q@SIhORDU@@R@@RREURDU
;96    3.000E-03 QGSQSPxWT@@R@@RHiTxWT
;96    3.971E-03 AESBBPdQT@@R@@RtQTdQT
;96 M5 3.971E-03 AESBBPADU@@R@@RAEUADU
;96    4.000E-03 AESBCPACU@@R@@RADUACU
;96    4.227E-03 ABSRDPiBT@@R@@RyBTiBT
;96 M4 4.227E-03 ABSRDPaIU@@R@@Rq@UaIU
;96    4.503E-03 YaRbGPQAU@@R@@RQBUQAU
;96    4.797E-03 iPRBPPYUT@@R@@RiUTYUT
;96 M3 4.797E-03 iPRBPPQAU@@R@@RQBUQAU
;96    5.000E-03 yIRBYPAAU@@R@@RABUAAU
;96    5.895E-03 XTRBfPfQT@@R@@RfYTfQT
;96 M2 5.895E-03 XTRBfPGAT@@R@@RGITGAT
;96    6.000E-03 HTRRaPvQT@@R@@RvYTvQT
;96    6.288E-03 h@RCBPUgT@@R@@RFETUgT
;96 M1 6.288E-03 h@RCBPfBT@@R@@Rv@TfBT
;96    8.000E-03 VdRcTPCTT@@R@@RSQTCTT
;96    1.000E-02 EaRdIPQgT@@R@@RBCTQgT
;96    1.500E-02 SgReRPGAS@@R@@RGQSGBS
;96    1.893E-02 CGRvIPCeS@@R@@RTFSCfS
;96 L3 1.893E-02 CGRvIPIDS@@R@@RyESIDS
;96    2.000E-02 BgRVWPGeS@@R@@RXDSGfS
;96    2.380E-02 rBRWBPDhS@@R@@RUBSDiS
;96 L2 2.380E-02 rBRWBPViS@@R@@RgBSViS
;96    2.413E-02 bHRWFPvTS@@R@@RVgSvTS
;96    2.446E-02 bDRg@PVPS@@R@@RvSSVPS
;96 L1 2.446E-02 bDRg@PGYS@@R@@RwRSGYS
;96    3.000E-02 qPRwYPDWS@@R@@RdUSDXS
;96    4.000E-02 QDRXSPRCS@@R@@RbFSRDS
;96    5.000E-02 hFQXiPQIS@@R@@RaHSa@S
;96    6.000E-02 fCQiGPwFR@@R@@RHGRGUR
;96    8.000E-02 SbQYPPCRR@@R@@RSaRSRR
;96    1.000E-01 rRQYPPAhR@@R@@RbERQhR
;96    1.282E-01 qYQyDPiWQ@@R@@RaDRAFR
;96 K  1.282E-01 qYQyDPSaR@@R@@RTHRD@R
;96    1.500E-01 qGQYFPbTR@@R@@RBgRrSR
;96    2.000E-01 h@PxPPaHR@@R@@RATRqFR
;96    3.000E-01 SdPGcPdQQ@@R@@RuYQuIQ
;96    4.000E-01 rCPWCPr@Q@@R@@RcDQCAQ
;96    5.000E-01 QSPVXPqGQ@@R@@RRHQBBQ
;96    6.000E-01 AIPVCPIGP@@R@@RaSQQRQ
;96    8.000E-01 fHOERPDiP@@R@@RAIQACQ
;96    1.000E+00 DHODiPSAP@@R@@RHQPH@P
;96    1.022E+00 SaODdPRhP@@R@@RhAPGbP
;96    1.250E+00 bTOtIPBAPtVN@@RvQPFUP
;96    1.500E+00 AeOD@PASPrIO@@REePeWP
;96    2.000E+00 AEOCRPhTOVcO@@REHPTgP
;96    2.044E+00 AAOsHPxCOw@O@@REDPTdP
;96    3.000E+00 tQNbYPDYOAQPIPLdQPTVP
;96    4.000E+00 bVNbEPRcOQfPCcMTSPTQP
;96    5.000E+00 qQNQdPRDOBRPgQMdPPTXP
;96    6.000E+00 QINqRPaWOBaPQGNtQPtPP
;96    7.000E+00 xQMQTPqGOSEPQWNDfPDeP
;96    8.000E+00 fWMAPPQEOCWPQfNEAPEAP
;96    9.000E+00 eGMaIPYdNsVPrCNUHPUGP
;96    1.000E+01 dGMa@PxSNDBPbXNuDPuCP
;96    1.100E+01 SSMQBPwXNdGPCANUPPUPP
;96    1.200E+01 RgMAEPGANTPPsBNeVPeUP
;96    1.300E+01 RSMIfOvHNtRPcRNEaPE`P
;96    1.400E+01 RHMyCOEeNTbPCiNUfPUeP
;96    1.500E+01 Q`MHfOEPNUAPTFNV@PV@P
;96    1.600E+01 aWMHTOEANeIPDQNfCPfCP
;96    1.800E+01 qBMwQOtHNeQPDgNFXPFXP
;96    2.000E+01 AGMWAOCiNU`PeHNvPPvPP
;96    2.200E+01 HdLfPOSPNVFPeVNVaPVaP
;96    2.400E+01 GRLVGOSHNFPPFANWAPWAP
;96    2.600E+01 vCLuYORaNfRPvCNgIPgIP
;96    2.800E+01 EULEWObXNFbPfRNGVPGVP
;96    3.000E+01 tULUHOBYNGAPV`NgRPgRP
;96    4.000E+01 bWLTAOAcNwVPHBNhGPhGP
;96    5.000E+01 qQLCSOATNxBPHfNxWPxWP
;96    6.000E+01 QILRfOQINxVPYSNYFPYFP
;96    8.000E+01 fXKrCOHeMIPPAEOyUPyUP
;96    1.000E+02 dHKQdOGCMIePQBOABQABQ
;96    1.500E+02 Q`KqHOdUMAFQaDOAHQAHQ
;96    2.000E+02 AGKAHOCWMQ@QqAOQBQQBQ
;96    3.000E+02 tUJgVNr@MQEQAPOQGQQGQ
;96    4.000E+02 bWJF@NqRMQHQAUOa@Qa@Q
;96    5.000E+02 qQJTgNqHMQIQAXOaAQaAQ
;96    6.000E+02 QIJdENQEMaAQQQOaCQaCQ
;96    8.000E+02 fXIsBNXYLaCQQTOaDQaDQ
;96    1.000E+03 dHIrSNFgLaDQQWOaFQaFQ
;96    1.500E+03 Q`IQaNTWLaEQaPOaGQaGQ
;96    2.000E+03 AGIAXNCSLaFQaROaHQaHQ
;96    3.000E+03 tUHACNbHLaGQaTOaIQaIQ
;96    4.000E+03 bWHWcMqQLaHQaVOaIQaIQ
;96    5.000E+03 qQHFXMqGLaHQaVOq@Qq@Q
;96    6.000E+03 QIHUPMQDLaHQaWOq@Qq@Q
;96    8.000E+03 fXGdDMXVKaIQaXOq@Qq@Q
;96    1.000E+04 dHGCVMFeKaIQaXOq@Qq@Q
;96    1.500E+04 Q`GrIMTWKaIQaYOqAQqAQ
;96    2.000E+04 AGGAdMCRKaIQaYOqAQqAQ
;96    3.000E+04 tUFaGMbHKaIQaYOqAQqAQ
;96    4.000E+04 bWFySLqQKaIQqPOqAQqAQ
;96    5.000E+04 qQFWbLqGKaIQqPOqAQqAQ
;96    6.000E+04 QIFvPLQDKaIQqPOqAQqAQ
;96    8.000E+04 fXEUDLXVJq@QqPOqAQqAQ
;96    1.000E+05 dHETHLFeJq@QqPOqAQqAQ
;==== ELEMENT  97
;97    1.000E-03 AVSdBOGcU@@R@@RGdUGcU
;97    1.111E-03 AUSDfOVPU@@R@@RVQUVPU
;97    1.235E-03 ASSUVOuIU@@R@@REQUuIU
;97 N3 1.235E-03 ASSUVOuTU@@R@@RuUUuTU
;97    1.500E-03 APSGDOSdU@@R@@RSeUSdU
;97    1.554E-03 qISwEOcWU@@R@@RcXUcWU
;97 N2 1.554E-03 qISwEOsQU@@R@@RsSUsQU
;97    1.651E-03 qHSW`OcHU@@R@@RcIUcHU
;97    1.755E-03 qGSHXOBiU@@R@@RR`UBiU
;97 N1 1.755E-03 qGSHXORdU@@R@@RRfURdU
;97    2.000E-03 qCSIeObDU@@R@@RbEUbDU
;97    3.000E-03 a@SQSPYGT@@R@@RiITYGT
;97    4.000E-03 AGSBDPtTT@@R@@RDdTtTT
;97    4.132E-03 AFSR@PtIT@@R@@RTPTtIT
;97 M5 4.132E-03 AFSR@PIbT@@R@@RYcTIbT
;97    4.247E-03 ADSRFPyCT@@R@@RITTyCT
;97    4.366E-03 ACSbAPHfT@@R@@RXgTHgT
;97 M4 4.366E-03 ACSbAPaCU@@R@@RaDUaCU
;97    4.661E-03 YhRrDPAFU@@R@@RAGUAFU
;97    4.977E-03 iTRBYPIFT@@R@@RYFTIFT
;97 M3 4.977E-03 iTRBYPAFU@@R@@RAGUAFU
;97    5.000E-03 iRRRPPAEU@@R@@RAFUAEU
;97    6.000E-03 hURRbPfPT@@R@@RfXTfPT
;97    6.147E-03 XRRRhPVIT@@R@@RfHTVIT
;97 M2 6.147E-03 XRRRhPVWT@@R@@RfUTVWT
;97    6.348E-03 xERCFPFFT@@R@@RVDTFFT
;97    6.556E-03 XGRSCPUYT@@R@@ReXTUYT
;97 M1 6.556E-03 XGRSCPEcT@@R@@RUaTEcT
;97    8.000E-03 WARcVPSWT@@R@@RcTTSWT
;97    1.000E-02 UeRtBPBDT@@R@@RR@TBDT
;97    1.500E-02 DFReVPw@S@@R@@RwQSwAS
;97    1.945E-02 CERVRPsTS@@R@@RDESsUS
;97 L3 1.945E-02 CERVRPxWS@@R@@RIISxXS
;97    2.000E-02 ReRfRPHIS@@R@@RxISX@S
;97    2.438E-02 r@RgEPtVS@@R@@RE@StWS
;97 L2 2.438E-02 r@RgEPFcS@@R@@RGGSFdS
;97    2.483E-02 bERw@PVQS@@R@@RvTSVRS
;97    2.527E-02 RIRwEPfAS@@R@@RFSSfBS
;97 L1 2.527E-02 RIRwEPWFS@@R@@RwHSWFS
;97    3.000E-02 qTRGePdTS@@R@@RDcSdUS
;97    4.000E-02 QGRhPPbBS@@R@@RrESbCS
;97    5.000E-02 HXQIGPaDS@@R@@RqDSaES
;97    6.000E-02 FQQyEPgYR@@R@@RHRRwXR
;97    8.000E-02 DCQYXPSXR@@R@@RDHRcXR
;97    1.000E-01 B`QYYPQgR@@R@@RrERBGR
;97    1.316E-01 qVQIPPIWQ@@R@@RaBRADR
;97 K  1.316E-01 qVQIPPsYR@@R@@RDFRChR
;97    1.500E-01 AQQiEPrTR@@R@@RRgRBcR
;97    2.000E-01 HUPxXPqBR@@R@@RQPRAQR
;97    3.000E-01 DFPWaPD`Q@@R@@RF@QUYQ
;97    4.000E-01 BPPgAPBPQ@@R@@RsFQSBQ
;97    5.000E-01 QXPfUPASQ@@R@@RbEQBIQ
;97    6.000E-01 QBPVIPIYP@@R@@RaXQQWQ
;97    8.000E-01 FXOEXPUCP@@R@@RQCQAFQ
;97    1.000E+00 dAOTdPcFP@@R@@RhRPh@P
;97    1.022E+00 DDODiPSBP@@R@@RHRPHAP
;97    1.250E+00 rSODSPRAPDhN@@RFgPVYP
;97    1.500E+00 QaODDPQPPBVO@@RUgPuXP
;97    2.000E+00 AIOCUPIFOWAO@@RUHPEGP
;97    2.044E+00 ADOCQPxSOWPO@@RUDPECP
;97    3.000E+00 DgNrRPtQOAUPYPLdYPdTP
;97    4.000E+00 rUNbGPCGOB@PCgMdQPTXP
;97    5.000E+00 qVNQfPbDOBVPgYMdXPdVP
;97    6.000E+00 aCNqSPqUOBePQHNtYPtXP
;97    7.000E+00 IAMQVPASOcAPQYNTcPTcP
;97    8.000E+00 VaMARPaAOSSPQhNEIPEIP
;97    9.000E+00 EVMq@PADOCbPrENeFPeEP
;97    1.000E+01 DRMaAPYENDIPrQNERPERP
;97    1.100E+01 cUMQCPXENtDPCDNUYPUXP
;97    1.200E+01 CGMAFPwDNTXPsFNuUPuTP
;97    1.300E+01 bRMYfOfXND`PcUNU`PU`P
;97    1.400E+01 bFMISOVBNE@PScNFEPFEP
;97    1.500E+01 QgMXeOeUNe@Pd@NVIPVIP
;97    1.600E+01 qSMXROeENuHPDUNvCPvCP
;97    1.800E+01 qGMwYOTYNuQPTaNVXPVXP
;97    2.000E+01 QAMWHODGNF@PuDNFaPFaP
;97    2.200E+01 YDLfWOcVNfGPuRNGCPGCP
;97    2.400E+01 gXLfCOsBNVQPFGNgCPgCP
;97    2.600E+01 VULEeOCENvSPvINGRPGQP
;97    2.800E+01 eULUROBaNVdPfYNWYPWYP
;97    3.000E+01 TbLeCObQNWCPVgNwUPwUP
;97    4.000E+01 rWLTFOQaNGiPX@NHQPHQP
;97    5.000E+01 qWLCWOQQNHWPXeNXbPXbP
;97    6.000E+01 aCLRiOaENXaPiRNyBPyBP
;97    8.000E+01 VaKrFOiFMYVPAFOYbPYbP
;97    1.000E+02 DSKQfOwFMA@QQCOACQACQ
;97    1.500E+02 QgKqIODfMAGQaEOQ@QQ@Q
;97    2.000E+02 QAKAIOcSMQBQqBOQDQQDQ
;97    3.000E+02 TbJwTNBQMQGQAQOQIQQIQ
;97    4.000E+02 rWJFFNA`Ma@QAVOaBQaBQ
;97    5.000E+02 qWJEBNATMaBQQPOaDQaDQ
;97    6.000E+02 aCJt@Na@MaCQQROaEQaEQ
;97    8.000E+02 VaIsFNXhLaEQQVOaGQaGQ
;97    1.000E+03 DSIrVNWHLaFQQXOaHQaHQ
;97    1.500E+03 QgIQcNtXLaGQaROaIQaIQ
;97    2.000E+03 QAIAYNSYLaHQaTOq@Qq@Q
;97    3.000E+03 TbHADNrILaIQaVOqAQqAQ
;97    4.000E+03 rWHHAMqYLq@QaWOqBQqBQ
;97    5.000E+03 qWHVUMASLq@QaXOqBQqBQ
;97    6.000E+03 aCHUUMQILqAQaXOqBQqBQ
;97    8.000E+03 VaGdHMXfKqAQaYOqCQqCQ
;97    1.000E+04 DSGSPMWGKqAQqPOqCQqCQ
;97    1.500E+04 QgGBQMtXKqAQqPOqCQqCQ
;97    2.000E+04 QAGAfMSXKqAQqQOqCQqCQ
;97    3.000E+04 TbFaHMrIKqBQqQOqCQqCQ
;97    4.000E+04 rWFIcLqYKqBQqQOqCQqCQ
;97    5.000E+04 qWFH@LASKqBQqQOqCQqCQ
;97    6.000E+04 aCFvWLQIKqBQqQOqCQqCQ
;97    8.000E+04 VaEUILXfJqBQqQOqCQqCQ
;97    1.000E+05 DSEdBLWFJqBQqROqDQqDQ
;==== ELEMENT  98
;98    1.000E-03 AWSDHOGhU@@R@@RGiUGhU
;98    1.131E-03 AUStXOvIU@@R@@RFQUvIU
;98    1.279E-03 ATSUYOUIU@@R@@Re@UUIU
;98 N3 1.279E-03 ATSUYOURU@@R@@RUTUURU
;98    1.500E-03 AQSF`ODEU@@R@@RDFUDEU
;98    1.616E-03 qISGTOCXU@@R@@RSPUCXU
;98 N2 1.616E-03 qISGTOSRU@@R@@RSTUSRU
;98    1.705E-03 qHSWcOSEU@@R@@RSGUSEU
;98    1.799E-03 qGSHUOBbU@@R@@RBdUBbU
;98 N1 1.799E-03 qGSHUOBgU@@R@@RBiUBgU
;98    2.000E-03 qDSYUOr@U@@R@@RrAUr@U
;98    3.000E-03 aASAYPIRT@@R@@RYTTIRT
;98    4.000E-03 AHSQiPDfT@@R@@RTgTDfT
;98    4.253E-03 AESRAPdAT@@R@@RtBTdAT
;98 M5 4.253E-03 AESRAPITT@@R@@RYUTITT
;98    4.373E-03 ADSRGPXcT@@R@@RICTXcT
;98    4.497E-03 ACSbBPHTT@@R@@RXUTHTT
;98 M4 4.497E-03 ACSbBPQGU@@R@@RQHUQGU
;98    5.000E-03 yPRBUPYHT@@R@@RiGTYHT
;98    5.109E-03 YYRBYPhYT@@R@@RxYThYT
;98 M3 5.109E-03 YYRBYPAAU@@R@@RABUAAU
;98    6.000E-03 xRRBfPvWT@@R@@RFeTvWT
;98    6.359E-03 HPRCAPEbT@@R@@RU`TEbT
;98 M2 6.359E-03 HPRCAPVGT@@R@@RfETVGT
;98    6.554E-03 hDRCHPuRT@@R@@RE`TuRT
;98    6.754E-03 HGRSFPuAT@@R@@RuITuAT
;98 M1 6.754E-03 HGRSFPUST@@R@@ReQTUST
;98    8.000E-03 WFRcPPcTT@@R@@RsRTcTT
;98    1.000E-02 UiRdFPBIT@@R@@RRETBIT
;98    1.500E-02 DIRUYPGWS@@R@@RGiSGXS
;98    1.993E-02 RiRVTPcPS@@R@@RSaScQS
;98 L3 1.993E-02 RiRVTPx@S@@R@@RhQSxAS
;98    2.000E-02 RgRVUPhGS@@R@@RXXShHS
;98    2.525E-02 bARgHPDSS@@R@@RdVSDTS
;98 L2 2.525E-02 bARgHPvGS@@R@@RfPSvHS
;98    2.568E-02 RFRwCPV@S@@R@@RvCSVAS
;98    2.611E-02 RBRwHPEdS@@R@@RFFSEeS
;98 L1 2.611E-02 RBRwHPvSS@@R@@RVeSvTS
;98    3.000E-02 qVRwXPtTS@@R@@RTbStUS
;98    4.000E-02 QHRXSPbHS@@R@@RBPSbIS
;98    5.000E-02 XWQXiPaHS@@R@@RqGSaHS
;98    6.000E-02 FXQiGPW`R@@R@@RhTRH@R
;98    8.000E-02 DGQYQPcYR@@R@@RTIRsXR
;98    1.000E-01 BcQYRPBDR@@R@@RBQRRCR
;98    1.360E-01 aYQiHPXgQ@@R@@RQFRY`Q
;98 K  1.360E-01 aYQiHPSUR@@R@@RCaRcTR
;98    1.500E-01 ASQYGPrYR@@R@@RCCRBhR
;98    2.000E-01 XVPxRPqER@@R@@RQRRATR
;98    3.000E-01 TBPGePTbQ@@R@@RVAQuPQ
;98    4.000E-01 BSPWFPBVQ@@R@@RCRQSGQ
;98    5.000E-01 aQPfPPAWQ@@R@@RbIQRCQ
;98    6.000E-01 QDPVEPyWP@@R@@RqQQQYQ
;98    8.000E-01 VXOETPeIP@@R@@RQDQAGQ
;98    1.000E+00 dHOTaPsFP@@R@@RxPPhGP
;98    1.022E+00 T@ODfPcBP@@R@@RHYPHHP
;98    1.250E+00 rXODQPRHPTbN@@RVaPfTP
;98    1.500E+00 QdODAPQUPBYO@@RF@PEaP
;98    2.000E+00 Q@OCSPyDOWIO@@Re@PEIP
;98    2.044E+00 AFOsIPIAOWWO@@RUEPEEP
;98    3.000E+00 TfNrQPDfOAVPIULtPPdUP
;98    4.000E+00 B`NbFPSGOBAPCeMdRPTYP
;98    5.000E+00 qYNQePrAOBWPgUMdXPdVP
;98    6.000E+00 aENqRPAaOBfPQGNtYPtWP
;98    7.000E+00 YGMQUPAWOcAPQXNTcPTbP
;98    8.000E+00 GCMAQPaDOSSPQgNEIPEHP
;98    9.000E+00 UUMq@PAGOCbPrDNeEPeEP
;98    1.000E+01 TPMa@PIRNDIPbYNERPEQP
;98    1.100E+01 sRMQBPxINtDPCBNUXPUXP
;98    1.200E+01 SCMAEPWVNTXPsDNuTPuTP
;98    1.300E+01 bVMYaOFhND`PcSNU`PU`P
;98    1.400E+01 r@MyGOvANEAPSaNFEPFDP
;98    1.500E+01 B@MX`OEbNe@PTGNVIPVIP
;98    1.600E+01 qVMHWOEPNuHPDRNvCPvCP
;98    1.800E+01 qIMwTOtRNuQPDhNVXPVXP
;98    2.000E+01 QCMWDOTINF@Pu@NFaPFaP
;98    2.200E+01 y@LfSOsWNfGPeXNGCPGCP
;98    2.400E+01 GbLf@OCSNVQPFCNgCPgCP
;98    2.600E+01 fVLEbOSCNvTPvENGQPGQP
;98    2.800E+01 uTLEYOBiNVdPfUNWYPWYP
;98    3.000E+01 E@Le@ObXNWCPVbNwUPwUP
;98    4.000E+01 BbLTCOQgNW`PHENHQPHQP
;98    5.000E+01 A`LCUOQVNHWPHiNXbPXbP
;98    6.000E+01 aELRgOaHNXbPYVNyCPyCP
;98    8.000E+01 GDKrDOYSMYWPAEOYbPYbP
;98    1.000E+02 TPKQeOWWMA@QQCOACQACQ
;98    1.500E+02 B@KqIOE@MAHQaDOQ@QQ@Q
;98    2.000E+02 QCKAIOsTMQBQqAOQDQQDQ
;98    3.000E+02 E@JwPNBXMQGQAPOQIQQIQ
;98    4.000E+02 BbJFCNAfMa@QAUOaBQaBQ
;98    5.000E+02 A`JTiNAXMaBQAYOaDQaDQ
;98    6.000E+02 aEJdGNaCMaCQQQOaEQaEQ
;98    8.000E+02 GDIsDNiELaEQQUOaGQaGQ
;98    1.000E+03 TPIrUNwILaFQQWOaHQaHQ
;98    1.500E+03 B@IQbNTbLaHQaPOaIQaIQ
;98    2.000E+03 QCIAXNcYLaIQaROq@Qq@Q
;98    3.000E+03 E@HACNBVLaIQaTOqAQqAQ
;98    4.000E+03 BbHWgMAdLq@QaVOqBQqBQ
;98    5.000E+03 A`HVQMAXLq@QaVOqBQqBQ
;98    6.000E+03 aEHURMaCLqAQaWOqBQqBQ
;98    8.000E+03 GDGdEMiBKqAQaXOqCQqCQ
;98    1.000E+04 TPGCWMwGKqAQaXOqCQqCQ
;98    1.500E+04 B@GBPMTaKqAQaYOqCQqCQ
;98    2.000E+04 QCGAeMcYKqBQaYOqCQqCQ
;98    3.000E+04 E@FaGMBVKqBQaYOqCQqCQ
;98    4.000E+04 BbFyWLAdKqBQqPOqDQqDQ
;98    5.000E+04 A`FWfLAWKqBQqPOqDQqDQ
;98    6.000E+04 aEFvSLaCKqBQqPOqDQqDQ
;98    8.000E+04 GDEUFLiBJqBQqPOqDQqDQ
;98    1.000E+05 TPEd@LwGJqBQqPOqDQqDQ
;==== ELEMENT  99
;99    1.000E-03 QPSDCOwWU@@R@@RwYUwWU
;99    1.016E-03 AYSTAOWXU@@R@@RgPUWXU
;99    1.032E-03 AYSd@OGPU@@R@@RGQUGPU
;99 N4 1.032E-03 AYSd@OgRU@@R@@RgTUgRU
;99    1.168E-03 AXSTcOfAU@@R@@RfCUfAU
;99    1.321E-03 AVSuUOEGU@@R@@REHUEGU
;99 N3 1.321E-03 AVSuUOuIU@@R@@REQUuIU
;99    1.500E-03 ASSvROdAU@@R@@RdBUdAU
;99    1.680E-03 AQSwQOsDU@@R@@RsFUsDU
;99 N2 1.680E-03 AQSwQOsHU@@R@@RCPUsHU
;99    1.772E-03 APShAOCCU@@R@@RCDUCCU
;99    1.868E-03 qISxTOrQU@@R@@RrSUrQU
;99 N1 1.868E-03 qISxTOrVU@@R@@RrWUrVU
;99    2.000E-03 qGSIVOrIU@@R@@RBPUrIU
;99    3.000E-03 aCSAXPI`T@@R@@RYbTI`T
;99    4.000E-03 QASQhPEFT@@R@@RUGTEFT
;99    4.374E-03 AFSRFPT@T@@R@@RdATT@T
;99 M5 4.374E-03 AFSRFPIET@@R@@RYFTIET
;99    4.500E-03 AESbAPXXT@@R@@RhYTXXT
;99    4.630E-03 ACSbGPXDT@@R@@RhDTXDT
;99 M4 4.630E-03 ACSbGPQCU@@R@@RQDUQCU
;99    5.000E-03 Y`RBTPIYT@@R@@RYYTIYT
;99    5.252E-03 iSRRUPHPT@@R@@RHYTHPT
;99 M3 5.252E-03 iSRRUPI`T@@R@@RY`TI`T
;99    6.000E-03 X`RBfPGBT@@R@@RWATGBT
;99    6.574E-03 xIRCHPUTT@@R@@ReSTUTT
;99 M2 6.574E-03 xIRCHPEhT@@R@@RUfTEhT
;99    6.773E-03 hBRSFPEUT@@R@@RUTTEUT
;99    6.977E-03 HFRcCPEFT@@R@@RUETEFT
;99 M1 6.977E-03 HFRcCPeGT@@R@@RuFTeHT
;99    8.000E-03 w@RcPPsVT@@R@@RCcTsVT
;99    1.000E-02 V@RdFPRFT@@R@@RbBTRFT
;99    1.500E-02 TGRePPwTS@@R@@RXFSwUS
;99    2.000E-02 CDRVWPsPS@@R@@RDASsPS
;99    2.041E-02 RfRfSPSQS@@R@@RCaSSRS
;99 L3 2.041E-02 RfRfSPXFS@@R@@RHVSXFS
;99    2.304E-02 RURGBPEhS@@R@@RVDSEiS
;99    2.602E-02 RGRwIPdDS@@R@@RDVSdES
;99 L2 2.602E-02 RGRwIPVAS@@R@@RvCSVAS
;99    2.646E-02 RCRGTPEeS@@R@@RFGSEeS
;99    2.690E-02 BHRGYPePS@@R@@REaSePS
;99 L1 2.690E-02 BHRGYPFUS@@R@@RfWSFVS
;99    3.000E-02 A`RGaPDiS@@R@@REHST`S
;99    4.000E-02 a@RXVPrFS@@R@@RBYSrGS
;99    5.000E-02 xVQICPqBS@@R@@RARSqCS
;99    6.000E-02 fSQyAPhBR@@R@@RXgRxAR
;99    8.000E-02 TGQYUPCdR@@R@@RtFRSdR
;99    1.000E-01 BiQYVPRBR@@R@@RRQRbBR
;99    1.395E-01 aVQyBPxVQ@@R@@RQCRiYQ
;99 K  1.395E-01 aVQyBPCRR@@R@@RcXRSQR
;99    1.500E-01 AVQiCPBhR@@R@@RSBRRgR
;99    2.000E-01 xWPxWPqIR@@R@@RQWRAXR
;99    3.000E-01 dBPW`PEIQ@@R@@RvAQEhQ
;99    4.000E-01 RPPg@PRUQ@@R@@RSRQcGQ
;99    5.000E-01 aUPfTPQSQ@@R@@RrFQRIQ
;99    6.000E-01 QGPVIPABQ@@R@@RqUQaTQ
;99    8.000E-01 vVOEXPUQP@@R@@RQGQQ@Q
;99    1.000E+00 DPOTePSPP@@R@@RHiPHUP
;99    1.022E+00 dBODiPsFP@@R@@RhWPhEP
;99    1.250E+00 BeODSPbGPEBN@@RGDPvVP
;99    1.500E+00 B@ODDPaRPRUO@@RVAPUaP
;99    2.000E+00 QDOCUPyUOwEO@@ReHPUFP
;99    2.044E+00 AIOCQPIPOwTO@@ReCPUCP
;99    3.000E+00 U@NrRPEGOAXPYPLtVPtQP
;99    4.000E+00 BhNbGPs@OBDPCgMdWPdUP
;99    5.000E+00 AeNQfPBQORQPgYMtTPtRP
;99    6.000E+00 aHNqTPAhOBiPQHNDdPDcP
;99    7.000E+00 IUMQVPQTOcEPQYNTiPThP
;99    8.000E+00 gCMARPq@OSWPQhNUEPUDP
;99    9.000E+00 uRMqAPQBOCgPrENuAPuAP
;99    1.000E+01 dSMaAPIbNTDPrQNEXPEXP
;99    1.100E+01 CcMQCPxUNDPPCDNeUPeTP
;99    1.200E+01 cBMAFPGhNdSPsFNEaPEaP
;99    1.300E+01 rTMYgOWGNDfPcUNUfPUfP
;99    1.400E+01 rGMISOVWNEFPScNVAPVAP
;99    1.500E+01 BFMXeOFFNeFPd@NfFPfFP
;99    1.600E+01 AaMXSOeSNETPDUNFPPFPP
;99    1.800E+01 ASMwYOTbNuXPTaNfVPfUP
;99    2.000E+01 QFMWIOtGNFGPuCNFiPFiP
;99    2.200E+01 YXLfWOScNvEPuQNWAPWAP
;99    2.400E+01 HELfDOSWNVYPFGNwAPwAP
;99    2.600E+01 FfLEfOcGNFbPvINWPPWPP
;99    2.800E+01 UbLUSOCANGCPfXNgXPgXP
;99    3.000E+01 UELeCOB`NgBPVfNGdPGdP
;99    4.000E+01 R`LTFOBENH@PHINXRPXRP
;99    5.000E+01 AfLCWOaRNXXPXdNIDPIDP
;99    6.000E+01 aILRiOqDNIDPiQNITPITP
;99    8.000E+01 gEKrFOYbMyPPAFOAAQAAQ
;99    1.000E+02 dTKQfOGiMABQQCOAEQAEQ
;99    1.500E+02 BFKqIOeAMAIQaEOQBQQBQ
;99    2.000E+02 QFKAIOCiMQCQqBOQFQQFQ
;99    3.000E+02 UEJwUNRXMQHQAQOaAQaAQ
;99    4.000E+02 R`JFGNQcMaAQAVOaCQaCQ
;99    5.000E+02 AfJEBNQTMaCQAYOaEQaEQ
;99    6.000E+02 aIJt@NaIMaEQQROaGQaGQ
;99    8.000E+02 gEIsFNiSLaFQQUOaHQaHQ
;99    1.000E+03 dTIrVNgYLaHQQXOaIQaIQ
;99    1.500E+03 BFIQcNUCLaIQaQOqAQqAQ
;99    2.000E+03 QFIAYNCdLq@QaSOqBQqBQ
;99    3.000E+03 UEHADNRVLqAQaUOqCQqCQ
;99    4.000E+03 R`HHAMQbLqBQaVOqDQqDQ
;99    5.000E+03 AfHVUMQTLqBQaWOqDQqDQ
;99    6.000E+03 aIHUVMaHLqBQaXOqDQqDQ
;99    8.000E+03 gEGdHMiPKqCQaXOqDQqDQ
;99    1.000E+04 dTGSPMgXKqCQaYOqEQqEQ
;99    1.500E+04 BFGBRMUBKqCQqPOqEQqEQ
;99    2.000E+04 QFGAfMCdKqCQqPOqEQqEQ
;99    3.000E+04 UEFaHMRVKqCQqPOqEQqEQ
;99    4.000E+04 R`FIcLQbKqDQqPOqEQqEQ
;99    5.000E+04 AfFHALQSKqDQqQOqEQqEQ
;99    6.000E+04 aIFvWLaHKqDQqQOqEQqEQ
;99    8.000E+04 gEEUILYYJqDQqQOqEQqEQ
;99    1.000E+05 dTEdBLgXJqDQqQOqEQqEQ
;====  END TABLE
