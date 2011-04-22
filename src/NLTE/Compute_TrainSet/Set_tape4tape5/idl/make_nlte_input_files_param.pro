pro make_nlte_input_files_param

;    JAN-10 VHP Updated to take account of selected input for TAPE5 creation 
; 17-DEC-09 VHP Updated to add extra info to the top of the TAPE4
; 02-OCT-09 VHP Original
; 
; Description:
;-------------
; Construct LBLRTM TAPE5s (for both LTE and non-LTE) and TAPE4s 
; (for the non-LTE runs) 
; 
; Known deficiencies:
;--------------------
; Have not yet worked out how best to put the XS profiles from the UMBC 
;  atmosphere profile into the TAPE5s. Am currently specifying the XS 
;  molecules listed in the UMBC atmosphere file, but using profiles for these
;  molecules from the UARS atmospheres.
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; set up all necessary inputs for the TAPE5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; cross section molecules listed in the UMBC atmospheres file
xsname =  ['F11', 'F14', 'F22'] 
; path for LBLRTM cross section files
;;xs_path= '../uars_atmospheres/'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EDIT THIS SECTION TO file names and file paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
AER_NLTE_DATA_DIR='/u/wx23yh/CRTM/EXP-NLTE/externals/AER/NLTE_data/'
; specify the location of Manuel's files
vt_loc = AER_NLTE_DATA_DIR + 'profiles_from_manuel/'
; umbc profie set location
atm_path = AER_NLTE_DATA_DIR + 'umbc_atmospheres/'
; path to lblrtm buildt in atmpheric profiles
input_path_built_in = AER_NLTE_DATA_DIR + 'afgl_atmospheres/'

; construct an LTE TAPE5 file path
t5_path =  '../tape5_files/'
; specify the path for the output TAPE4
tape4_path = '../tape4_files/'
; make directory
spawn, 'mkdir '+t5_path
spawn, 'mkdir '+tape4_path 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EDIT THIS SECTION TO SET UP THE TAPE5s AS YOU WANT THEM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; set up wavenumber range & res for some section of an AIRS-type calculation
; IASI band3 frequency range 2000 - 2760 cm-1
v1_IASI=['1900.0']
v2_IASI=['2860.0']

v1_arr = v1_IASI 
v2_arr = v2_IASI

;hwhm_arr = [0.25d]   ; original
hwhm_arr = [0.0005d]    

;v1_arr = ['1900.000'] 
;v2_arr = ['2900.00']

v1_scan_arr = v1_arr
v2_scan_arr = v2_arr

    
jfn = -4

; use the freq_one_index_calc to determine the freq
v1_intep_arr = V1_arr    
v2_intep_arr = V2_arr

dv_intep_arr = [0.25d]   
    
output_filename_beg = 'TAPE5_'

no_model_lev_adj = 0 ; 0 -> ensure that the bottom model level equals the bottom profile value
                         ; 1 -> if set, does not adjust bottom model level to equal bottom profile value.   

; define the number of molecules that we want to use in the tape5s
nmol_t5 =  7
; set up the angle, Y. Han, June 2010.
amass=[1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5, 2.75, 3.0, 3.25, 3.5, 3.75, 4.0]
; local zenith angle - at the earth surface
R2D=180.0D0/!PI
zangle =  ACOS(1.0D0/amass)*R2D 
; compute angle at the sensor height 97.5km - the height LBLRTM starts integration
H1 = 97.5D0
re = 6370.95D0 ; earth mean radius
angle = 180.0D0 - asin((Re/(Re + H1))*sin( zangle/R2D ))*R2D

; tell LBLRTM that this is a nadir view
itype = 2
; set flags to tell LBLRTM to look for external emissivity/reflectivity files
sremis_1 = -1.00
srrefl_1 = -1.00
; set surface type (sea)      
surf_refl = 's'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; END OF TAPE5 INPUT INFO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INFORMATION ABOUT NON-LTE MOLECULES/STATES TO GO INTO TAPE4    ;
; DO NOT EDIT THIS SECTION                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; number of degeneracies assoc with the ground state is always 1.
; energy of the ground state is always 0.

; define the allowed states for non-LTE in H2O in LBLRTM 
gs_mol1 =  '000' ; ground state
lbl_states_mol1 =  ['010', '020', '100', '001', '030', '110', '011']

; define the number of degeneracies of these states
deg_states_mol1 =  [1, 1, 1, 1, 1, 1, 1]

; define the energies of these states
e_states_mol1 =  [1594.750, 3151.630, 3657.053, 3755.930, 4666.793, $
                  5234.977, 5333.269]

; define the allowed states for non-LTE in CO2 in LBLRTM 
; lowermost vib state - 00001)
gs_mol2 =  '00001' ; ground state
lbl_states_mol2 =   ['01101', '10002', '02201', $
                    '10001', '11102', '03301', '11101', $
                    '00011', '20003', '12202', '20002', $
                    '04401', '12201', '20001', '01111', $
                    '10012', '02211', '10011', '11112', $
                    '03311', '11111', '20013', '04411', $
                    '20012', '20011']

; define the number of degeneracies of these states
deg_states_mol2 =  [2, 1, 2, $
                    1, 2, 2, 2, $
                    1, 1, 2, 1, $
                    2, 2, 1, 2, $
                    1, 2, 1, 2, $
                    2, 2, 1, 1, $
                    1, 1]

; define the energies of these states
e_states_mol2 =   [667.380, 1285.409, 1335.132,  1388.185, $
                  1932.470, 2003.246, 2076.856, 2349.143, $
                  2548.366, 2585.022, 2671.143, 2671.717, $
                  2760.725, 2797.135, 3004.012, 3612.842, $
                  3659.273, 3714.783, 4247.706, 4314.914, $
                  4390.629, 4853.623, 4970.931, 4977.834, $
                  5099.660]

; define the allowed states for non-LTE in o3 in LBLRTM 
gs_mol3 =  '000' ; ground state
lbl_states_mol3 =  ['010', '001', '100', '020', $
                    '011', '110', '002', '101', $
                    '200', '111', '003', '004', $
                    '005', '006', '007', '008', $
                    '009']

; define the number of degeneracies of these states
deg_states_mol3 =  [1, 1, 1, 1,1 ,1, 1,1, 1, 1, 1, 1,1 ,1, 1, 1, 1]

; define the energies of these states
e_states_mol3 =  [700.931, 1042.084, 1103.140, 1399.275, $
                  1726.528, 1796.261, 2057.892, 2110.785, $
                  2201.157, 2785.245, 3041.200, 3988., $
                  4910., 5803., 6665., 7497., $
                  8299.]

; define the allowed states for CO in LBLRTM
gs_mol5 =  '0'
lbl_states_mol5 =  ['1', '2']
deg_states_mol5 =  [1, 1]
e_states_mol5 =  [2143.272, 4260.063]

; define the allowed states for NO in LBLRTM 
gs_mol8 =  '0'
lbl_states_mol8 =  ['1', '2']
deg_states_mol8 =  [1, 1]
e_states_mol8 =  [1878.077, 3724.067]

; specify the molecules for which LBLRTM can currently deal with non-LTE
mol_name_nlte =  ['H2O', 'CO2', 'O3', 'CO', 'NO']
mol_num_nlte =  [1, 2, 3, 5, 8]
n_mol_nlte =  n_elements(mol_num_nlte)
; specify the number of currently allowed states for each molecule
mol_nstate =  [n_elements(lbl_states_mol1), n_elements(lbl_states_mol2), $
               n_elements(lbl_states_mol3), n_elements(lbl_states_mol5), $
               n_elements(lbl_states_mol8)]
header_nlte =  ['------ H2O VIBRATIONAL STATE DATA ' + $
                '(NUM(I),ISO(I),ID(I),EE(I),NDG(I),I=1,N)/',$ 
                '------ CO2 VIBRATIONAL STATE DATA ' + $
                '(NUM(I),ISO(I),ID(I),EE(I),NDG(I),I=1,N)/', $
                '------ O3 VIBRATIONAL STATE DATA, ' + $
                '(NUM(I),ISO(I),ID(I),EE(I),NDG(I),I=1,N)/', $
                '------ CO VIBRATIONAL STATE DATA ' + $
                '(NUM(I),ISO(I),ID(I),EE(I),NDG(I),I=1,N)/', $
                '------ NO VIBRATIONAL STATE DATA ' + $
                '(NUM(I),ISO(I),ID(I),EE(I),NDG(I),I=1,N)/']

header_vib =  ['------ H2O TEMPERATURE PROFILES  (ALT(KM), KINETIC TMP, VIBRATIONAL ' + $
               'TMP(2:N))', $
               '------ CO2 TEMPERATURE PROFILES  (ALT(KM), KINETIC TMP, VIBRATIONAL ' + $
               'TMP(2:N))', $
               '------ O3 TEMPERATURE PROFILES  (ALT(KM), KINETIC TMP, VIBRATIONAL ' + $
               'TMP(2:N))', $
               '------ CO TEMPERATURE PROFILES  (ALT(KM), KINETIC TMP, VIBRATIONAL ' + $
               'TMP(2:N))', $
               '------ NO TEMPERATURE PROFILES  (ALT(KM), KINETIC TMP, VIBRATIONAL ' + $
               'TMP(2:N))']
         
; get the names of Manuel's vibrational temperature files
spawn, 'ls ' + vt_loc + 'vt/vt*.prf', vt_lst
n_lst =  n_elements(vt_lst)

; determine the number of different UMBC atmospheres
spawn, 'ls ' +  vt_loc + 'pt/pt*.prf', pt_lst
n_atm =  n_elements(pt_lst)

; determine the number of different solar zenith angles
n_sza = 6

; read in all 48 UMBC atmospheres
read_umbc_atmospheres, natm, plev, molind, vmr_prof, tsfc, atm_path=atm_path, $
    input_path_built_in = input_path_built_in

; loop over UMBC atmospheres
FOR ia= 0, n_atm - 1 DO begin

; determine the profile (atmosphere) number
    prf_num =  string(ia+1, format='(i2.2)')
    IF strmid(prf_num, 0,1) EQ '0' THEN prf_num =  strmid(prf_num, 1, 1)

; read in the pressure, temperature profile for this atmosphere
    pt_file =  vt_loc + 'pt/pt' + prf_num + '.prf'
    read_pt_file, pt_file, nlev_pt, alt_pt, p_pt, t_pt
; restrict from 0 to 120 km, since this is the altitude range of the standard atmospheres
    g =  where(alt_pt LE 120., ng)
    alt_pt =  alt_pt(g)
    p_pt =  p_pt(g)
    t_pt =  t_pt(g)
    nlev_pt =  ng

;    molec_arr_name = ['H2O', 'CO2', 'O3', 'N2O', 'CO', 'CH4', 'O2', 'NO', 'SO2', $
;                      'NO2', 'NH3', 'HNO3', 'OH', 'HF', 'HCL', 'HBR', 'HI', $
;                      'CLO', 'OCS', 'H2CO', 'HOCL', 'N2', 'HCN', 'CH3CL', $
;                      'H2O2', $
;                      'C2H2', 'C2H6', 'PH3', 'COF2', 'SF6', 'H2S', 'HCOOH'] 
;
;    cfc_name = [['F11       F12       CF4       ' + $
;                 'CCL4      N2O5      HNO4                OTHER']]
;    cfc_arr_num = indgen(6) +51
;    ncfc = n_elements(cfc_arr_num)

; interpolate the molecular profiles onto Manuel's altitude grid

; use the US standard atmosphere to extend the umbc profile to 120 km, Y. Han, July 1, 2010
p_ext=  transpose(reverse([1.840e-3, 7.600e-4, 3.200e-4, 1.450e-4, 7.100e-5, 4.010e-5, 2.540e-5]))
h2o_ext=transpose(reverse([  1.0e-6,   1.0e-6,   1.0e-6,   1.0e-6,   1.0e-6,   1.0e-6,   1.0e-6]))
co2_ext=transpose(reverse([3.100e+2, 2.700e+2, 1.950e+2, 1.100e+2, 6.000e+1, 4.000e+1, 3.500e+1]))
o3_ext= transpose(reverse([7.000e-1, 7.000e-1, 4.000e-1, 2.000e-1, 5.000e-2, 5.000e-3, 5.000e-4]))
n2o_ext=transpose(reverse([4.708e-4, 3.932e-4, 3.323e-4, 2.837e-4, 2.443e-4, 2.120e-4, 1.851e-4]))
co_ext= transpose(reverse([5.843e+0, 1.013e+1, 1.692e+1, 2.467e+1, 3.356e+1, 4.148e+1, 5.000e+1]))
ch4_ext=transpose(reverse([1.400e-1, 1.300e-1, 1.200e-1, 1.100e-1, 9.500e-2, 6.000e-2, 3.000e-2]))
o2_ext= transpose(reverse([1.900e+5, 1.800e+5, 1.600e+5, 1.400e+5, 1.200e+5, 9.400e+4, 7.250e+4]))
molec_ext=[h2o_ext, co2_ext, o3_ext, n2o_ext, co_ext, ch4_ext, o2_ext]
nl_ext=n_elements(p_ext)
nl_umbc=n_elements(vmr_prof(0, 0, *))

    molec = fltarr(nmol_t5, ng)
    umbc_prf = fltarr(nl_umbc+nl_ext)
    umbc_p = fltarr(nl_umbc+nl_ext)
    umbc_p(0:nl_ext-1) = p_ext
    umbc_p(nl_ext:nl_ext+nl_umbc-1) = plev
    FOR im=0, nmol_t5-1 DO BEGIN
        umbc_prf(0:nl_ext-1) = molec_ext(im, *)
        umbc_prf(nl_ext:nl_ext+nl_umbc-1) = vmr_prof(im, ia, *)
 
        mdum = reverse(interpol(umbc_prf, alog(umbc_p), alog(reverse(p_pt))))
        molec(im, *) = mdum
    ENDFOR


; make sure that the model levels stop before the top of the atmosphere
    g2 =  where(alt_pt LT 100.)
    model_l = p_pt(g2)

 ; set up lblrtm format instruction based on number of molecules
    dd =  'A'
    bb =  ''
    FOR i=0, nmol_t5-1 DO bb = bb+dd

    tbound =  tsfc(ia)

; loop over sensor angles
    FOR iang=0, N_ELEMENTS(angle)-1 DO BEGIN
      angle_tag=STRING(amass[iang], format='(f4.2)')
      t5_file = output_filename_beg + 'prf' + prf_num + '_lte.'+angle_tag

      tape5_build, alt_pt, p_pt, t_pt, molec, tbound, v1_arr(0), v2_arr(0),$
        angle[iang], 'pres', model_levels=model_l, $
        jcharp= 'A', jchart= 'A', jlong= 'L', $
        jchar_arr= replicate(bb, n_elements(p_pt)), $
        output_filename=t5_file, output_path= t5_path, $
        iscan_intep=0, $
        v1_scan=v1_scan_arr(0), v2_scan=v2_scan_arr(0), hwhm=hwhm_arr(0), jfn=jfn, $
        v1_intep=v1_intep_arr(0), v2_intep=v2_intep_arr(0), dv_intep=dv_intep_arr(0), $
;        xsname=xsname, xs_path=xs_path, $
        itype=itype,$
        sremis_1=sr_emis_1, srrefl_1=srrefl_1, surf_refl=surf_refl, $
        no_model_lev_adj=no_model_lev_adj, $
        ihirac='1', ixsect='0'
      
; construct a non-LTE TAPE5 file
      t5_file = output_filename_beg + 'prf' + prf_num + '_nlte.'+angle_tag

      tape5_build, alt_pt, p_pt, t_pt, molec, tbound, v1_arr(0), v2_arr(0),$
        angle[iang], 'pres', model_levels=model_l, $
        jcharp= 'A', jchart= 'A', jlong= 'L', $
        jchar_arr= replicate(bb, n_elements(p_pt)), $
        output_filename=t5_file, output_path= t5_path, $
        iscan_intep=0, $
        v1_scan=v1_scan_arr(0), v2_scan=v2_scan_arr(0), hwhm=hwhm_arr(0), jfn=jfn, $
        v1_intep=v1_intep_arr(0), v2_intep=v2_intep_arr(0), dv_intep=dv_intep_arr(0), $
;        xsname=xsname, xs_path=xs_path, $
        itype=itype,$
        sremis_1=sr_emis_1, srrefl_1=srrefl_1, surf_refl=surf_refl, $
        no_model_lev_adj=no_model_lev_adj, $
        ihirac='4', ixsect='0'

    ENDFOR

    spawn, 'ls ' + vt_loc + '/vt/vt' + prf_num + '*.prf', lst_prf
    n_sza =  n_elements(lst_prf)

    FOR is= 0, n_sza-1 DO BEGIN

; read in the vibrational temperatures for all C02 states, isotopes that
; Manuel supplied
        vt_file =  lst_prf(is)
        read_vt_file, vt_file, nlev_vt, alt_vt, nstate, $
          states_arr_short, states_arr_long, $
          mol_arr, iso_arr, band_center_arr, $
          vt_prf_arr

; note that what are labeled here as "band centers" also correspond to the
; energies of the vibrational states (e_states_mol2)

; restrict the vibrational temperatures to the range 0 to 120 km
        g =  where(alt_vt LE 120, ng)
        nlev_vt =  ng
        alt_vt =  alt_vt(g)
        vt_prf_arr =  vt_prf_arr(*,0:ng-1)
  
; set up arrays to hold info and vibrational temperatures for all non-LTE molecules
        ngstate =  max(mol_nstate)
        band_center_arr_all =  fltarr(n_mol_nlte, ngstate)
        vt_prf_arr_all =  fltarr(n_mol_nlte, ngstate, nlev_vt)
        iso_arr_all = intarr(n_mol_nlte, ngstate)
; For now, LBLRTM only deals with vibrational temperatures for the main isotope (1)        
        iso_arr_all(*, *) = 1
        lbl_states_all =  strarr(n_mol_nlte, ngstate)
        lbl_states_all(0,0:mol_nstate(0)-1) =  lbl_states_mol1 ; h2o
        lbl_states_all(1,0:mol_nstate(1)-1) =  lbl_states_mol2 ; co2
        lbl_states_all(2,0:mol_nstate(2)-1) =  lbl_states_mol3 ; o3
        lbl_states_all(3,0:mol_nstate(3)-1) =  lbl_states_mol5 ; co
        lbl_states_all(4,0:mol_nstate(4)-1) =  lbl_states_mol8 ; no
        e_states_all =  fltarr(n_mol_nlte, ngstate)
        e_states_all(0,0:mol_nstate(0)-1) =  e_states_mol1 ; h2o
        e_states_all(1,0:mol_nstate(1)-1) =  e_states_mol2 ; co2
        e_states_all(2,0:mol_nstate(2)-1) =  e_states_mol3 ; o3
        e_states_all(3,0:mol_nstate(3)-1) =  e_states_mol5 ; co
        e_states_all(4,0:mol_nstate(4)-1) =  e_states_mol8 ; no
        gs_all =  strarr(n_mol_nlte)
        gs_all(0) =  gs_mol1 ; h2o
        gs_all(1) =  gs_mol2 ; co2
        gs_all(2) =  gs_mol3 ; o3
        gs_all(3) =  gs_mol5 ; co
        gs_all(4) =  gs_mol8 ; no
        deg_states_all =  intarr(n_mol_nlte, ngstate)
        deg_states_all(0,0:mol_nstate(0)-1) =  deg_states_mol1 ; h2o
        deg_states_all(1,0:mol_nstate(1)-1) =  deg_states_mol2 ; co2
        deg_states_all(2,0:mol_nstate(2)-1) =  deg_states_mol3 ; o3
        deg_states_all(3,0:mol_nstate(3)-1) =  deg_states_mol5 ; co
        deg_states_all(4,0:mol_nstate(4)-1) =  deg_states_mol8 ; no
        
 
; pick out the vibrational temperature profiles for the states currently
; allowed within LBLRTM for CO2
        m2 =  where(mol_num_nlte EQ 2)
        m2nstate =  mol_nstate(m2(0))
        FOR ist= 0, m2nstate-1 DO BEGIN
            gg =  where(mol_arr EQ 2 AND iso_arr EQ 1 AND $
                        states_arr_long EQ lbl_states_mol2(ist), ngg)
            IF ngg LE 0 THEN BEGIN
                print, 'no state matches'
                stop
            ENDIF
            IF ngg GT 1 THEN BEGIN
                print, 'more than one state matches'
                stop
            ENDIF
            vt_prf_arr_all(m2, ist, *) = vt_prf_arr(gg, *)
            band_center_arr_all(m2,ist) =  band_center_arr(gg)
            iso_arr_all(m2, ist) =  iso_arr(gg)
            
        ENDFOR

; specify filename for the output TAPE4
        sza = strmid(lst_prf(is), strpos(lst_prf(is), '_s'))
        nam =  strmid(lst_prf(is), strpos(lst_prf(is), 'vt'))
        tape4_file =  'TAPE4_vt' + prf_num +  sza 

; specify some useful information to go into the header array
        spawn, ['date','+%d %h %y  %H:%M%n'], datestr, /noshell
;        name_str =  'V. H. Payne (AER)'
        header_arr =  strarr(3)
        header_arr(0) =  '! File generated ' + datestr ;+ ' ' + name_str
        header_arr(1) =  '! Vibrational temperature profiles for CO2 supplied by ' + $
          'Manuel Lopez-Puertas'
        header_arr(2) =  '! Profile ' + string(ia, format='(i2)') + ' of 48 UMBC ' + $
          'atmospheres ' + nam

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

; construct a TAPE4 file
        write_tape4_vt, n_mol_nlte, mol_name_nlte, mol_num_nlte, mol_nstate, $
          nlev_vt, alt_vt, t_pt, vt_prf_arr_all, iso_arr_all, $
          lbl_states_all, gs_all, e_states_all, deg_states_all, $
          header_nlte, header_vib, $
          tape4_file=tape4_file, tape4_path=tape4_path, header_arr=header_arr

; end loop over solar zenith angles
    ENDFOR                      ; is

; end loop over UMBC atmospheres
ENDFOR ; ia

END

