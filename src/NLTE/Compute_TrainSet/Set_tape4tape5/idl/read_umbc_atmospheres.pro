PRO read_umbc_atmospheres, natm, plev, molind, vmr_prof, tsfc, $
                           atm_path=atm_path, atm_file=atm_file, $
                           input_path_built_in=input_path_built_in

; 01-NOV-09 VHP
; 
; Description: Reads in the 48 UMBC atmospheres from a file
; 
; Optional input  (Y. Han added, July 1, 2010)
;   input_path_built_in path to the built in atm files
;
; outputs: 
;----------
; natm ; number of atmospheres (should be 48!)
; plev ; array of pressure levels (nlev levels)
; molind ; indices of molecules in the file (nmol molecules)
; vmr_prof(nmol, natm, nlev) ; array holding VMR profiles for each atmopshere
; tsfc(natm) ; array of surface temperatures
;
; keywords:
;----------
; atm_path: string containing path for UMBC atmosphere file
; atm_file: string containing UMBC atmosphere filename
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IF n_elements(atm_file) LE 0 THEN atm_file =  'umbc48_4interComp_4ODtable.dat'
IF n_elements(atm_path) LE 0 THEN atm_path =  '../umbc_atmospheres/'

; specify the molecular weight of air
m_air =  28.97

; specify the molecular weight of molecules of interest
m_mass =  fltarr(56)
m_mass(0) =  18. ; h2o
m_mass(1) =  44. ; co2
m_mass(2) =  48. ; o3
m_mass(3) =  44. ; n2o
m_mass(4) =  28. ; co
m_mass(5) =  16. ; ch4
m_mass(6) =  32. ;  o2
m_mass(11) =  63. ; hno3
m_mass(18) =  60. ; ocs
m_mass(21) = 28. ; n2
m_mass(50) =  137. ; f11
m_mass(53) =  88. ; f14
m_mass(55) =  86.5 ; f22

openr, lunfil, atm_path + atm_file, /get_lun
; read in the number of levels
readf, lunfil, nlev
; read in the pressure values
plev =  fltarr(nlev)
readf, lunfil, plev
; read in the number of molecules
readf, lunfil, nmol
; read in molecular indices
molind =  intarr(nmol)
readf, lunfil, molind
nn =  max(molind)
; read in flags for variable/fixed molecules
varmol =  intarr(nmol)
readf, lunfil, varmol
; determine the number of variable molecules
var =  where(varmol EQ 0, nvar)
ffix =  where(varmol NE 0, nfix)
header =  ' '
readf, lunfil, header
readf, lunfil, natm
; set up arrays to hold variables for each atmosphere
pview =  fltarr(natm)
psfc =  fltarr(natm)
angle =  fltarr(natm)
sza =  fltarr(natm)
azi =  fltarr(natm)
tprof =  fltarr(natm, nlev)
tsfc =  fltarr(natm)
molprof =  fltarr(nvar, natm, nlev)
emflg =  intarr(natm)
emval =  fltarr(natm)
sctflg =  intarr(natm)
FOR iatm= 0, natm-1 DO BEGIN
    readf, lunfil, header
    readf, lunfil, dum
    dum1 = 0. &  dum2= 0. &  dum3= 0. &  dum4= 0. &  dum5= 0.
    readf, lunfil, dum1, dum2, dum3, dum4, dum5
    pview(iatm) =  dum1
    psfc(iatm) =  dum2
    angle(iatm) =  dum3
    sza(iatm) =  dum4
    azi(iatm) =  dum5
; read in the temperature profile
    tdum =  fltarr(nlev)
    readf, lunfil, tdum
    tprof(iatm,*) =  tdum
; read in the skin temperature
    tt =  0.
    readf, lunfil, tt
    tsfc(iatm) =  tt
; loop over variable gases
    FOR im= 0, nvar-1 DO BEGIN
        moldum =  fltarr(nlev)
; read in the molecular profiles
        readf, lunfil, moldum
        molprof(im,iatm,*) =  moldum
    ENDFOR
; read in the emissivity values
    dumflg =  0 &  dumval= 0.
    readf, lunfil, dumflg, dumval
    emflg(iatm) =  dumflg
    emval(iatm) =  dumval
; read in flag for scattering
    dumsct =  0
    readf, lunfil, dumsct
    sctflg(iatm) =  dumsct
ENDFOR

vmr_prof =  fltarr(nn, natm, nlev)
; convert mass mixing ratios to volume mixing ratios for the variable gases
FOR imol= 0, nvar-1 DO BEGIN
    mm =  molind(imol) - 1
    vmr_prof(mm,*,*) =  (molprof(imol,*,*) * m_air) / m_mass(mm)
ENDFOR

; read in the fixed gases from the US standard atmosphere.
; Interpolate the  profiles onto this pressure grid
atmos_profile_arr = ['tropical_xx', 'mid_lat_sum', 'mid_lat_win', $
                     'sub_arc_sum', 'sub_arc_win', 'usa_std_atm']
;;  Note: Minor molecules (after the first 7) are only available with US standard atmosphere
molec_arr_name = ['H2O', 'CO2', 'O3', 'N2O', 'CO', 'CH4', 'O2', 'NO', 'SO2', $
                  'NO2', 'NH3', 'HNO3', 'OH', 'HF', 'HCL', 'HBR', 'HI', $
                  'CLO', 'OCS', 'H2CO', 'HOCL', 'N2', 'HCN', 'CH3CL', $
                  'H2O2', $
                  'C2H2', 'C2H6', 'PH3', 'COF2', 'SF6', 'H2S', 'HCOOH'] 

cfc_name = [['F11       F12       CF4       ' + $
             'CCL4      N2O5      HNO4                OTHER']]
cfc_arr_num = indgen(6) +51
ncfc = n_elements(cfc_arr_num)

i_AFGL_atmos_prof = 5    
; 0 -> 'tropical_xx'; 1 -> 'mid_lat_sum'; 2 -> 'mid_lat_win'
; 3 -> 'sub_arc_sum'; 4 -> 'sub_arc_win'; 5 -> 'usa_std_atm'

atmos_profile = atmos_profile_arr(i_AFGL_atmos_prof)
molec_lblrtm = molec_arr_name(0)
        
; read in the afgl atmosphere
read_lblrtm_profiles, atmos_profile, molec_lblrtm, $
  input_path_built_in=input_path_built_in, $ $ $
  n_molec=n_molec, molec_arr_prof=molec_arr_prof, molec_i_prof=molec_i_prof, $
  density_air=AMOL8, molec_wgt_air=AIRMWT, $
  molec_wgts_i=molec_wgts_i, ALT=lblrtm_zm, PRES=lblrtm_pm, $
  TEMP=lblrtm_tm, HMOLC_arr=HMOLC_arr, header_str=prof_header

nmol_afgl =  n_elements(molec_arr_prof(*,0))

; interpolate fixed gases onto the pressure grid used here
; loop over fixed molecules
FOR iff= 0, nfix-1 DO BEGIN

    IF molind(ffix(iff)) LT nmol_afgl THEN BEGIN
; loop over atmospheres
        FOR iatm=0, natm-1 DO BEGIN
        
            afgl_prof = molec_arr_prof(molind(ffix(iff))-1, *)
            dumprf = interpol( afgl_prof, alog(lblrtm_pm), alog(plev))
; convert fom ppmv to ppv in order to
; make sure that the fixed gases end up in same units as variable gases
            vmr_prof(molind(ffix(iff))-1, iatm, *) = dumprf/1.e6
        ENDFOR                  ; iatm
    ENDIF
ENDFOR ; iff (fixed molecule)

; convert from ppv to ppmv
vmr_prof(*,*,*) =  vmr_prof(*,*,*) * 1.e6

END

