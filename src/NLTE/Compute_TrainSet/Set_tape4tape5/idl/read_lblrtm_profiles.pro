;+
; NAME: read_lblrtm_profiles.pro
;
;
;
; PURPOSE: To read in LBLRTM built in standard profiles
;
;
;
; CATEGORY: RC Group
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS: atmos_profile = 'tropical_xx'
;                         'mid_lat_sum' 
;                         'mid_lat_win' 
;                         'sub_arc_sum' 
;                         'sub_arc_win' 
;                         'usa_std_atm'
;
;         molec = 'H2O' 
;                 'CO2' 
;                 'O3' 
;                 'N2O'
;                 'CO' 
;                 'CH4'
;                 'O2'
;
;                or any of the following if usa_std_atm profile is choosen:
;
;                  NO SO2 NO2 NH3 HNO3 OH HF HCL HBR HI CLO OCS H2CO HOCL N2 HCN
;                  CH3C H2O2 C2H2 C2H6 PH3 
;
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:  molec_arr_prof                   ;array of molecule profiles (ppmv)
;           molec_i_prof=molec_i_prof        ;specified molecule profile (ppmv)
;           molec_arr=molec_arr              :profiles of all the molecules (ppmv) 
;           molec_wgts_i=molec_wgts_i        ;molecular weight of specified molecule
;           molec_wgt_air=molec_wgt_air      ;molecular weight of air
;           density_air=AMOL8                ;air density profile (molec cm-3)
;           ALT=ALT                          ;altitude (km)
;           PRES=PRES                        ;pressure profile (hPa)
;           TEMP=TEMP                        ;tempurature profile(K)
;           HMOLC_arr=HMOLC_arr              ;name of all molecules in LBLRTM
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS: Minor molecules are only available with US standard atmosphere
;
;
;
; PROCEDURE: read_lblrtm_profiles, atmos_profile, molec
;
;
;
; EXAMPLE: read_lblrtm_profiles, atmos_profile, molec, $
;                     molec_arr_prof=molec_arr_prof, molec_i_prof=molec_i_prof, molec_wgts_i=molec_wgts_i, $
;                     molec_wgt_air=molec_wgt_air, density_air=AMOL8, ALT=ALT, $
;                     PRES=PRES, TEMP=TEMP, HMOLC_arr=HMOLC_arr
;
;
;
; MODIFICATION HISTORY:
;
;-created by Mark W Shephard <mshep@aer.com>
; March, 2001

PRO read_lblrtm_profiles, atmos_profile, molec, input_path_built_in=input_path_built_in, $
                          n_molec=n_molec,  molec_arr_prof=molec_arr_prof, molec_i_prof=molec_i_prof, $
                          density_air=AMOL8, molec_wgt_air=AIRMWT, $
                          molec_wgts_i=molec_wgts_i, ALT=ALT, PRES=PRES, $
                          TEMP=TEMP, HMOLC_arr=HMOLC_arr, header_str=header_str

IF N_ELEMENTS(input_path_built_in) LE 0 then input_path_built_in = '../afgl_atmospheres/'

print, 'Processing LBLRTM Atmospheric Profile: ', atmos_profile
print, 'Processing LBLRTM Molecule : ', molec

atmos_i = ['tropical_xx', 'mid_lat_sum', 'mid_lat_win', $
           'sub_arc_sum', 'sub_arc_win' ,'usa_std_atm']

file_testing = where(atmos_profile EQ atmos_i)
IF file_testing(0) LT 0 THEN BEGIN 
 print,  'Input Error: atmos_profile does not match an LBLRTM built-in profiles:'
 print, atmos_i
 stop
ENDIF 

;;Read in file...................................................

input_name = atmos_profile+'.txt'
input_file = input_path_built_in+input_name

OPENR, unit, input_file, /get_lun

Print, 'Opening file: ', input_file

max_num = 10000
n_lev = 50
n_molec = 35

format_901 = '(f6.3)'
format_902 = '(35(f7.3))' 
format_903 = '(A21)'
format_904 = '(50(f5.1))'
format_905 = '(50(e10.3))'
format_906 = '(50(f6.2))'
format_907 = '(50(e10.3))'
format_908 = '(35(A5))'

header_str = ' '
HMOLC = ''                      ;molecule names
AMWT = FLTARR(n_molec)          ;molecular weights
AMOL8 = DBLARR(n_lev)           ;density of dry air
ALT =  DBLARR(n_lev)            ;altitude (km)
PRES =  DBLARR(n_lev)           ;pressure levels
TEMP =  DBLARR(n_lev)           ;temperature
molec_tmp = DBLARR(n_lev)
molec_arr_prof = DBLARR(n_molec, n_lev) ;molecules
;molec_arr_prof(0)                     ;H20
;molec_arr_prof(1)                     ;CO2
;molec_arr_prof(2)                     ;O3
;molec_arr_prof(3)                     ;NO2
;molec_arr_prof(4)                     ;CO
;molec_arr_prof(5)                     ;CH4
;molec_arr_prof(6)                     ;O2
;molec_arr_prof(7)                     ;NO 
;molec_arr_prof(8)                     ;SO2
;molec_arr_prof(9)                     ;NO2
;molec_arr_prof(10)                    ;NH3
;molec_arr_prof(11)                    ;HNO3
;molec_arr_prof(12)                    ;OH
;molec_arr_prof(13)                    ;HF
;molec_arr_prof(14)                    ;HCL
;molec_arr_prof(15)                    ;HBR
;molec_arr_prof(16)                    ;HI
;molec_arr_prof(17)                    ;CLO
;molec_arr_prof(18)                    ;OCS
;molec_arr_prof(19)                    ;H2CO
;molec_arr_prof(20)                    ;HOCL
;molec_arr_prof(21)                    ;N2
;molec_arr_prof(22)                    ;HCN
;molec_arr_prof(23)                    ;CH3CL
;molec_arr_prof(24)                    ;H2O2
;molec_arr_prof(25)                    ;C2H2
;molec_arr_prof(26)                    ;C2H6
;molec_arr_prof(27)                    ;PH3



readf, unit, header_str, FORMAT=format_903
print, header_str
readf, unit, AIRMWT, FORMAT=format_901 ;molecular weight of air
readf, unit, HMOLC              ;molecular names
readf, unit, AMWT, FORMAT=format_902 ;molecular weights
readf, unit, AMOL8, FORMAT=format_907 ;density of dry air
readf, unit, ALT, FORMAT=format_904 ;altitude (km)
readf, unit, PRES, FORMAT=format_905 ;pressure levels
readf, unit, TEMP, FORMAT=format_906 ;temperature

i = 0l
WHILE NOT eof(unit) DO BEGIN 
    readf, unit, molec_tmp, FORMAT=format_907 
    FOR j=0, n_lev-1 DO BEGIN 
        molec_arr_prof(i,j) = molec_tmp(j)
    ENDFOR 
i = i+1
ENDWHILE 

molec_arr_prof = molec_arr_prof(0:i-1,0:n_lev-1)

;;Obtain the array of LBLRTM molecular names out of the HMOLC string
n_HMOLC = STRLEN(HMOLC)/5
HMOLC_arr = STRARR(n_HMOLC)
FOR i = 0, n_HMOLC -1 DO BEGIN 
    HMOLC_arr(i) = STRTRIM(STRMID(HMOLC,i*5, 5),2)
ENDFOR 

;;Match the input molec to the LBLRTM string array
w_molec = where(molec EQ HMOLC_arr(*), count)

IF (count LE 0) THEN BEGIN 
    stop, 'Input Molecule ('+molec+ ') does not match one of LBLRTM molecules: ', HMOLC_arr
ENDIF 

;;Check to make sure that the input molecule is available for that atmosphere
molec_size = SIZE(molec_arr_prof)
IF (w_molec(0) GT molec_size(1)-1) THEN BEGIN 
    print, 'Molecule ('+molec+') is not avaiable for the ('+atmos_profile+') atmosphere'
    print, 'If you want a minor species then specify the US Standard Atmosphere'
    stop
ENDIF


;;Pick out desired molecular profile
molec_i_prof = REFORM(molec_arr_prof(w_molec(0), *)) ;reform to get rid of leading
                                                     ;dimension of 1  

molec_wgts_i = AMWT(w_molec(0))

free_lun, unit


END
