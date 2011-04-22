PRO write_tape4_vt, n_mol_nlte, mol_name_nlte, mol_num_nlte, mol_nstate, $
                    nlev_vt, alt_vt, t_pt, vt_prf_arr_all, iso_arr_all, $ $
                    lbl_states_all, gs_all, e_states_all, deg_states_all, $
                    header_nlte, header_vib, $
                    tape4_file=tape4_file, tape4_path=tape4_path, header_arr=header_arr
; 
;    JAN-2010 VHP Update TAPE4 format
; 02-OCT-2009 Original. Vivienne Payne, AER, Inc.
; 
; Description:
;-------------
; Write out a TAPE4 file for a given set of vibrational temperature profiles.
; (Code could  in theory be used for all molecules for which LBLRTM can deal
; with non-LTE, but currently we are only supplying info for CO2.)
;  
; Input:
;--------
; n_mol_nlte    : number of molecules for which LBLRTM can deal with NLTE
; mol_name_nlte ; string array with names of molecules with NLTE in LBLRTM
; mol_num_nlte  : integer array with corresponding molecule numbers
; mol_nstate    : integer array with no. of LBLRTM allowed states for each mol
; nlev_vt       : number of altitude levels in the vibrational temp profiles
; alt_vt        : altitude grid for the vibrational temperature profiles
; t_pt          : kinetic temperature profile (should be on alt_vt grid)
; vt_prf_arr_all(n_mol_nlte, nstate, nlev_vt): vibrational temperature profiles
;               :(nstate is the max number of allowed vib.states of any mol)  
; 
; Output:
;--------
; 
; Keywords:
;-----------
; tape4_file    : filename for the output TAPE4 file (default is 'TAPE4')
; tape4_path    : path for the output TAPE4 file (default is './')
; header_arr    : string array with 3 elements containing header information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IF n_elements(tape4_file) EQ 0 THEN tape4_file =  'TAPE4'
IF n_elements(tape4_path) EQ 0 THEN tape4_path =  './'
IF n_elements(header_arr) EQ 0 THEN header_arr =  ['xxxxx', 'xxxxx', 'xxxxx']

IF n_elements(header_arr) NE 3 THEN BEGIN
    print, 'Must have three elements in the header array'
    stop
ENDIF

ff =  tape4_path + tape4_file
openw, lunfil, ff, /get_lun
FOR i= 0, 2 DO printf, lunfil, header_arr(i)
; specify that we are using vibrational temperature input
; (plh is just a placeholder for now)
vt_inp =  1 &  plh= 5
printf, lunfil, vt_inp, plh, format='(2I5)'

; loop over molecules for which non-LTE is allowed in LBLRTM
FOR im= 0, n_mol_nlte-1 DO BEGIN
    printf, lunfil, header_nlte(im)

    lbl_states =  [gs_all(im), transpose(lbl_states_all(im, *))]
    e_states =  [0., transpose(e_states_all(im, *))]
    deg_states =  [1, transpose(deg_states_all(im,*))]
    iso_states =  [1, transpose(iso_arr_all(im,*))]
    icnt =  1
    FOR is= 0,mol_nstate(im) DO BEGIN
        printf, lunfil, icnt,  iso_states(is), $
          '  '''+lbl_states(is)+'''', $
          e_states(is), deg_states(is)
        icnt =  icnt+1
    ENDFOR

    printf, lunfil, header_vib(im)

; loop over altitude levels
    FOR ilev= 0, nlev_vt-1 DO BEGIN
        IF mol_nstate(im) LE 6 THEN BEGIN
            printf, lunfil, alt_vt(ilev), t_pt(ilev), $
              vt_prf_arr_all(im, 0:mol_nstate(im)-1, ilev), $
              format='(f7.0, 7f11.3)'
        ENDIF ELSE BEGIN
            nline =  fix(mol_nstate(im) / 6.)
            printf, lunfil, alt_vt(ilev), t_pt(ilev), $
              vt_prf_arr_all(im, 0:5, ilev), $
              format='(f7.0, 7f11.3)'
            FOR iline=0, nline-1 DO BEGIN
                IF mol_nstate(im) LT 6*(iline+2) THEN nmax = mol_nstate(im) $
                ELSE nmax = 6*(iline+2)

                IF (6*(iline+1) LE nmax-1) THEN $
                    printf, lunfil, vt_prf_arr_all(im, 6*(iline+1):nmax-1, ilev), $
                    format='(18x, 6f11.3)'
            ENDFOR ; iline
        ENDELSE
    ENDFOR ; ilev

ENDFOR ; im

printf, lunfil, '------ END OF DATA'

free_lun, lunfil

end
