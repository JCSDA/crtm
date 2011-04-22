PRO read_vt_file, vt_file, nlev, alt_vt, nstate, $
                  states_arr_short, states_arr_long, $
                  mol_arr, iso_arr, band_center_arr, $
                  vt_prf_arr

; 02-OCT-09 Original. Vivienne Payne, AER Inc.
; 
; Description:
; ------------
; Routine to read in Manuel's vibrational temperature profiles for each 
; of the 48 UMBC atmospheres
; 
; Input:
; --------
; vt_file : string containing filename of vibrational temperature file
; 
; Output:
; --------
; nlev                     : number of vertical levels
; alt_vt(nlev)             ; altitude grid (km)
; nstate                   : number of vibrational states in the file
; states_arr_short(nstate) : array containing the state in HITRAN f100 notation
; states_arr_long(nstate)  : array containing the state in HITRAN f160 notation
; mol_arr(nstate)          : array containing molecule number (2 for CO2)
; iso_arr(nstate)          : array containing isotope number
; band_center_arr(nstate)  : array containing wavenumbers of the band centers
; vt_prf_arr(nstate, nlev) : array containing vibrational temperature profiles
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

openr, lunfil, vt_file, /get_lun
header =  ' '
readf, lunfil, header
nlev =  0
readf, lunfil, nlev
readf, lunfil, header
readf, lunfil, header
alt_vt =  fltarr(nlev)
readf, lunfil, alt_vt
FOR i= 0, 4 DO readf, lunfil, header
nstate =  0
readf, lunfil, nstate
states_arr_long =  strarr(nstate)
states_arr_short =  intarr(nstate)
band_center_arr =  fltarr(nstate)
mol_arr =  intarr(nstate)
iso_arr =  intarr(nstate)
vt_prf_arr =  fltarr(nstate, nlev)
; loop over vibrational states
FOR istate= 0, nstate - 1 DO BEGIN
    readf, lunfil, header
    readf, lunfil, header
    s_lon = strmid(header, 5, 5)
    b_c = float(strmid(header, 13, 8))
    states_arr_long(istate) =  s_lon
    band_center_arr(istate) =  b_c
    readf, lunfil, header
    mm =  fix(strmid(header, 1, 2))
    ii =  fix(strmid(header, 3, 1))
    s_s =  0
    readf, lunfil, s_s
    mol_arr(istate) =  mm
    iso_arr(istate) =  ii
    states_arr_short =  s_s
    vt_dum =  fltarr(nlev)
    readf, lunfil, vt_dum
    vt_prf_arr(istate,*) =  vt_dum(*)
ENDFOR ; istate

free_lun, lunfil

END
