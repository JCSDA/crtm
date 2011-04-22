;+
; NAME: tape5_build.pro
;
;
;
; PURPOSE: Generate an LBLRTM input TAPE5 from user provided atmospheric profile 
;
;
;
; CATEGORY: General RC group
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS: 
;;---------------------------------------------------------------------------
;; input for user define profile format 
;;---------------------------------------------------------------------------
;
; INPUTS: 
;         ZM       : array of altitudes
;         PM       : array of pressures
;         TM       : array of temperatures
;         molec    : array of molecules molec(# molec, # levels)
; model_level_flag : tape5 sonde level input : 'pres' or 'alt'
;
; OPTIONAL INPUTS:
;        model_levels : array of model levels
;        co2_vmr      : co2_vmr (in ppmv i.e. 358.0)
;        jcharp       : lblrtm units code for pressure info
;        jchart       : lblrtm units code for temperature info
;        jlong        : flag for reading long info for molecular info
;        jchar_arr    : lblrtm units code for molecular info 
;        NOPRNT       : 0 -> full printout; 1 -> short printout
;        output_filename : string containing name of TAPE5 output file
;        output_path  : string containing name of output path
;        v1_scan      : start wavenumber
;        v2_scan      : end wavenumber
;        hwhm         : half width at half max for fftscan
;        jfn          : lblrtm flag for scanning function
;        param_fftscan:
;        v1_intep     : wavenumber info for FFTSCAN
;        v2_intep     : wavenumber info for FFTSCAN
;        dv_intep     : wavenumber info for FFTSCAN
;        xs_path      : string containing path for cross-section profile files
;        xsname       : string array of xsec molecules to include
;        h1, h2       ; altitude boundaries (as lblrtm instructions)
;        itype        ; as lblrtm instructions
;  sremis_1, srefl_1  : flags for external emissivity/reflectivity files
;        surf_refl    : flag for surface type
;  no_model_level_adj : flag to adjust bottom level of profile
;        ihirac       : as lblrtm instructions
;        icntnm       : as lblrtm instructions
;        xself        ; self h2o cntnm scaling, as lblrtm instructions
;        xfrgn        : foreign h2o cntnm scaling, as lblrtm instructions
;        xco2c        ; co2 cntnm scaling, as lblrtm instructions
;        xo3cn        : o3 cntnm scaling, as lblrtm instructions
;        xo2cn        : o2 contnm scaling, as lblrtm instructions
;        xn2cn        : n2 contnm scaling, as lblrtm instructions
;        xrayl        : as lblrtm instructions
;        anal_jac_flag: flag for analytic jacobians
;        nspcrt_arr   : analytic jacobian info
;        IAERSL       : as lblrtm instructions
;        ixsect       : as lblrtm instructions
;        iscan_intep  : flag for interpolation of FFTSCAN
;        iod          : as lblrtm_instructions
;        dvout        ; as lblrtm instructions
;
;
; OUTPUTS:
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
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
; Jan. 2010 : Comments updated by Vivienne Payne <vpayne@aer.com>
;-Nov. 2003 : Create by Mark Shephard <mshep@aer.com>
;
;

PRO tape5_build, ZM, PM, TM, molec, TBOUND, V1, V2, ANGLE,model_level_flag, $
                 model_levels=model_levels, $
                 co2_vmr=co2_vmr, jcharp=jcharp, jchart=jchart, jlong=jlong, $
                 JCHAR_arr=JCHAR_arr,  NOPRNT = NOPRNT, $
                 output_filename=output_filename, output_path=output_path, $
                 V1_scan=V1_scan, V2_scan=V2_scan, HWHM=HWHM, JFN=JFN, PARAM_fftscan=PARAM_fftscan, $
                 V1_intep=V1_intep, V2_intep=V2_intep, DV_intep = DV_intep, $
                 xs_path=xs_path, XSNAME=XSNAME, H1=H1, H2=H2, $
                 ITYPE=ITYPE, $
                 SREMIS_1=SREMIS_1, SRREFL_1=SRREFL_1, surf_refl=surf_refl, $
                 no_model_lev_adj=no_model_lev_adj, $
                 IHIRAC=IHIRAC, ICNTNM=ICNTNM, $
                 XSELF=XSELF, XFRGN=XFRGN, XCO2C=XCO2C, XO3CN=XO3CN, XO2CN= XO2CN, XN2CN=XN2CN, XRAYL= XRAYL,$
                 anal_jac_flag=anal_jac_flag, NSPCRT_arr=NSPCRT_arr, IAERSL=IAERSL, $
                 IXSECT=IXSECT, $
                 ISCAN_intep=ISCAN_intep, $
                 IOD=IOD,DVOUT=DVOUT

;;---------------------------------------------------------------------------
;..ORDER OF MOLECULES (***NOTE: Subtract -1 for IDL***) 
;                    H2O(1), CO2(2), O3(3), N2O(4), CO(5), CH4(6),   
;                    O2(7), NO(8), SO2(9), NO2(10), NH3(11), HNO3(12), OH(13),     
;                    HF(14 ), HCL(15), HBR(16), HI(17), CLO(18), OCS(19), H2CO(20) 
;                    HOCL(21), N2(22), HCN(23), CH3CL(24), H2O2(25), C2H2(26),     
;                    C2H6(27), PH3(28), COF2(29), SF6(30), H2S(31), HCOOH(32)      
;                    HO2(33),O(34),CLONO2(35),NO+(36),HOBr(37),C2H4(38), CH3OH(39) 
;;---------------------------------------------------------------------------
name_molec = ['H2O' , 'CO2' , 'O3' , 'N2O' , 'CO' , 'CH4' , 'O2' , 'NO' , $
              'SO2' , 'NO2' , 'NH3' , 'HNO3' , 'OH' , 'HF' , 'HCL' , 'HBR' , 'HI' , $
              'CLO' , 'OCS' , 'H2CO' , 'HOCL' , 'N2' , 'HCN' , 'CH3CL' , 'H2O2' , $
              'C2H2' , 'C2H6' , 'PH3' , 'COF2' , 'SF6' , 'H2S' , 'HCOOH' , $
              'HO2' , 'O' , 'CLONO2' , 'NO+' , 'HOBr' , 'C2H4', 'CH3OH']
;;---------------------------------------------------------------------------

i_plot = 0 ;  0 -> plotting turned off

dim = size(molec)
NMOL = dim(1)
IMMAX = dim(2)

V1 = STRTRIM(STRING(V1),2)
V2 = STRTRIM(STRING(V2),2)

IF N_ELEMENTS(V1_scan) LE 0 THEN V1_scan = V1   ; '600.057711'    
IF N_ELEMENTS(V2_scan) LE 0 THEN V2_scan = V2   ; '1074.76489' 
IF N_ELEMENTS(HWHM) LE 0 THEN HWHM = '0.5'
IF N_ELEMENTS(JFN) LE 0 THEN JFN = '2'
IF N_ELEMENTS(PARAM_fftscan) LE 0 THEN PARAM_fftscan = '0.0'
IF N_ELEMENTS(V1_intep) LE 0 THEN V1_intep = V1_scan  
IF N_ELEMENTS(V2_intep) LE 0 THEN V2_intep = V2_scan  
IF N_ELEMENTS(DV_intep) LE 0 THEN DV_intep = '0.10'           

IF N_ELEMENTS(co2_vmr) LE 0 THEN co2_vmr = 0.0d

IF N_ELEMENTS(jcharp) LE 0 THEN $
  jcharp='A'                    ;     jcharp: flag for units and input options for pressure
                                ;                       1-6 default values for specified model atmosphere
                                ;                       " " or A  pressure (mb)
                                ;                       B                  (atm)
                                ;                       C                  (torr)
IF N_ELEMENTS(jchart) LE 0 THEN $
  jchart='A'                    ;     jchart: flag for units and input options for temperature
                                ;                       1-6 default values for specified model atmosphere
                                ;                        " " or A  ambient temperature (K)
                                ;                        B                             (C)
IF N_ELEMENTS(jlong) LE 0 THEN $
  jlong=' '                     ;     jlong : flag for reading long record for molecular information
                                ;                       IF jlong equals "L" then the molecules are in 8e15.8 format
IF N_ELEMENTS(JCHAR_arr) LE 0 THEN $
  JCHAR_arr=STRJOIN(REPLICATE('A', NMOL))    ; flag for units of nth molecules JCHAR_arr(# molec)
                                ; 1-6 default values for specified model atmosphere
                                ;      -> 0 user:1 trop:2 midlat sum:3 midlat wint
                                ;      -> 4 sub artic sum:5 subartic wint:6 US std
                                ; " " or A:  VMR(ppmv)
                                ; B       :  number density (cm-3)                                
                                ; C       :  mass mixing ratio (gm/kg)   
                                ; D       :  mass density (gm m-3)   
                                ; E       :  partial pressure (mb)   
                                ; F       :  dew point T (K) *H2O only*
                                ; G       :  dew point T (C) *H2O only*   
                                ; H       :  relative humidity (percent) *H2O only*   
                                ; I       :  avail. for user definition  

;;If JCHAR_arr is a single string then replicate the string for each profile level
IF N_ELEMENTS(JCHAR_arr) EQ 1 THEN JCHAR_arr = REPLICATE(JCHAR_arr, IMMAX) 

IF N_ELEMENTS(NOPRNT) LE 0 then NOPRNT = 0  ; 0 -> full printout; 1 -> short printout
IF N_ELEMENTS(output_filename) LE 0 THEN output_filename = 'TAPE5_sonde'
IF N_ELEMENTS(output_path) LE 0 THEN output_path = ''
IF N_ELEMENTS(xs_path) LE 0 THEN xs_path = '../uars_atmospheres/'

IF N_ELEMENTS(ITYPE) LE 0 THEN ITYPE = 2   ; 2 -> nadir; 3 -> limb

IF ITYPE EQ 3 THEN BEGIN ;limb
    IF N_ELEMENTS(XSNAME) LE 0 THEN XSNAME = ['CCL4', 'F11', 'F12', 'CHCLF2', 'HNO3', 'CLONO2']
ENDIF ELSE BEGIN ;nadir, etc. 
;    XSNAME = ['CCL4', 'F11', 'F12', 'CHCLF2', 'HNO4']  ;HNO4 is a gray body
    IF N_ELEMENTS(XSNAME) LE 0 THEN  XSNAME = ['CCL4', 'F11', 'F12', 'CHCLF2']  
ENDELSE


IF N_ELEMENTS(SREMIS_1) LE 0 THEN SREMIS_1 = -1.000
IF N_ELEMENTS(SRREFL_1) LE 0 THEN SRREFL_1 = -1.000
IF N_ELEMENTS(surf_refl) LE 0 THEN surf_refl = 's'
IF N_ELEMENTS(anal_jac_flag) LE 0 THEN anal_jac_flag = 0
IF ((KEYWORD_SET(anal_jac_flag)) AND (N_ELEMENTS(NSPCRT_arr) LE 0)) THEN BEGIN 
    STOP, "Need to specify parameters for Jacobian TAPE5"
ENDIF


n_levels = N_ELEMENTS(ZM)

;;---------------------------------------------------------------------------
;;Change or insert new constituent profile values...............................
;;---------------------------------------------------------------------------
;insert_flag = 1
;IF insert_flag GT 0 THEN BEGIN
;    i_m =  2
;    path_insert = '/storm/rc2/mshep/TES/uars_vmr/'
;    filename_insert = name_molec(i_m)+'_vmr_UARS.txt', /get_lun
;    openr, unit_insert, path_insert+filename_insert , /get_lun
;    print, 'Opening vmr profile file to insert into TAPE5: ', filename_insert 
;    print, 'For Molecule i_m = ', i_m
;
;    header = ''
;    vmr_alt =  DOUBLE(0.0)
;    vmr_temp = DOUBLE(0.0)
;    vmr_pres = DOUBLE(0.0)
;    vmr = DOUBLE(0.0)
;    vmr_pres_arr = DBLARR(10000)
;    vmr_arr = DBLARR(10000)
;
;    readf, unit_insert, header
;    readf, unit_insert, header
;    readf, unit_insert, header
;    readf, unit_insert, header
;    readf, unit_insert, header
;
;    t = 0
;    WHILE NOT EOF(unit_insert) DO BEGIN
;        readf, unit_insert, vmr_alt, vmr_pres, vmr_temp, vmr 
;        print, vmr_pres, vmr
;        vmr_pres_arr(t) = vmr_pres
;       vmr_arr(t) = vmr
;        t = t+1
;    ENDWHILE
;    vmr_pres_arr = vmr_pres_arr(0:t-1)
;    vmr_arr = vmr_arr(0:t-1)
;
;    ;;linear interpolate onto the profile pressure level
;    vmr_arr_iter = DBLARR(N_ELEMENTS(PM))
;    vmr_arr_iter = INTERPOL(vmr_arr, vmr_pres_arr, PM)
;
;    molec(i_m,*) = vmr_arr_iter
;ENDIF

;;---------------------------------------------------------------------------

IF model_level_flag EQ 'pres' THEN BEGIN 

    IF N_ELEMENTS(model_levels) LE 0 THEN BEGIN 
     ;;TES
     model_levels = [ $
     1211.5300d, 1100.7000d, 1000.0000d,  908.5140d,  825.4020d,  749.8930d,   681.2910d,  618.9660d,   562.3420d, $
      510.8980d,  464.1600d,  421.6980d,  383.1170d,  348.0690d,  316.2270d,  287.2980d,   261.0160d, $
      237.1370d,  215.4440d,  195.7350d,  177.8290d,  161.5610d,  146.7790d,  133.3520d,   121.1520d, $
      110.0690d,  100.0000d,   90.8518d,   82.5406d,   74.9896d,   68.1295d,   61.8963d,    56.2339d, $
       51.0896d,   46.4158d,   42.1696d,   38.3119d,   34.8071d,   31.6229d,   28.7299d,    26.1017d, $
       23.7136d,   21.5443d,   19.5734d,   17.7828d,   16.1560d,   14.6780d,   13.3352d,    12.1153d, $
       11.0070d,   10.0000d,    9.0851d,    8.2540d,    7.4989d,    6.8129d,    6.1897d,     5.6234d, $
        5.1090d,    4.6416d,    4.2170d,    3.8312d,    3.4807d,    3.1623d,    2.8730d,     2.6102d, $
        2.3714d,    2.1544d,    1.9573d,    1.7783d,    1.6156d,    1.4678d,    1.3335d,     1.2115d, $
        1.1007d,    1.0000d,    0.8254d,    0.6813d,    0.5623d,    0.4642d,    0.3831d,     0.3162d, $
        0.2610d,    0.2154d,    0.1778d,    0.1468d,    0.1212d,    0.1000d ]
     ENDIF

     ;;ensure that the bottom model level equal the bottom profile value
     IF NOT keyword_set(no_model_lev_adj) THEN BEGIN  
         w_sub = where(model_levels LT (PM(0)-0.1d))
         model_levels = [PM(0), model_levels(w_sub)]
     ENDIF

     ;;make certain the model levels are within the input profile levels
     w_model_levels = where(((model_levels - PM(0)) LE 0.0001 ) AND  $
                            ((model_levels - PM(n_levels-1)) GE -0.0001), IBMAX_model_levels)

    model_levels = model_levels(w_model_levels)
    IBMAX = STRTRIM(STRING(-1*IBMAX_model_levels), 2) ;set IBMAX to neg number for pres

ENDIF ELSE IF model_level_flag EQ 'alt_to_pres' THEN BEGIN  

    IF N_ELEMENTS(model_levels) LE 0 THEN BEGIN 
      ;;TES
     model_levels = [ $
     1211.5300d, 1100.7000d, 1000.0000d,  908.5140d,  825.4020d,  749.8930d,   681.2910d,  618.9660d,   562.3420d, $
      510.8980d,  464.1600d,  421.6980d,  383.1170d,  348.0690d,  316.2270d,  287.2980d,   261.0160d, $
      237.1370d,  215.4440d,  195.7350d,  177.8290d,  161.5610d,  146.7790d,  133.3520d,   121.1520d, $
      110.0690d,  100.0000d,   90.8518d,   82.5406d,   74.9896d,   68.1295d,   61.8963d,    56.2339d, $
       51.0896d,   46.4158d,   42.1696d,   38.3119d,   34.8071d,   31.6229d,   28.7299d,    26.1017d, $
       23.7136d,   21.5443d,   19.5734d,   17.7828d,   16.1560d,   14.6780d,   13.3352d,    12.1153d, $
       11.0070d,   10.0000d,    9.0851d,    8.2540d,    7.4989d,    6.8129d,    6.1897d,     5.6234d, $
        5.1090d,    4.6416d,    4.2170d,    3.8312d,    3.4807d,    3.1623d,    2.8730d,     2.6102d, $
        2.3714d,    2.1544d,    1.9573d,    1.7783d,    1.6156d,    1.4678d,    1.3335d,     1.2115d, $
        1.1007d,    1.0000d,    0.8254d,    0.6813d,    0.5623d,    0.4642d,    0.3831d,     0.3162d, $
        0.2610d,    0.2154d,    0.1778d,    0.1468d,    0.1212d,    0.1000d ]
     ENDIF ELSE BEGIN

         ;;The user supplied model levels in altitude, but would like to convert to
         ;;pressure (this does not have to be that accurate as it is just specifying the initial
         ;;Quadratic interpolation onto the model pressure levels
         ln_PM = dblarr(N_ELEMENTS(PM))
         FOR j= 0, N_ELEMENTS(PM) -1 DO BEGIN 
             IF PM(j) NE 0 THEN BEGIN 
                 ln_PM(j) = ALOG(PM(j))
             ENDIF ELSE BEGIN 
                 ln_PM(j) =  -999
             ENDELSE
         ENDFOR
         model_levels_pres = INTERPOL(ln_PM, ZM, model_levels, /quadratic )
         model_levels = EXP(model_levels_pres)
         ;;set any negative values generated in the interpolation to zero 
         w_neg = where(model_levels LT 0.0)
         IF w_neg(0) GE 0 then model_levels(w_neg) = 0.0

     ENDELSE

    ;;make certain the model levels are within the input profile levels
    w_model_levels = where(((model_levels - PM(0)) LE 0.0001 ) AND  $
                           ((model_levels - PM(n_levels-1)) GE -0.0001), IBMAX_model_levels)

    model_levels = model_levels(w_model_levels)
    IBMAX = STRTRIM(STRING(-1*IBMAX_model_levels), 2) ;set IBMAX to neg number for pres


ENDIF ELSE IF model_level_flag EQ 'alt' THEN BEGIN  

    stop, "***This option is still under development****"
;
;    model_levels = [ $
;                   ]
;    ;;make certain the model levels are within the input profile levels
;    w_model_levels = where((model_levels GE ZM(0)) AND (model_levels LE ZM(n_levels-1)), IBMAX_model_levels)
;    model_levels = model_levels(w_model_levels)
;    IBMAX = STRTRIM(STRING(IBMAX_model_levels), 2) 

ENDIF ELSE BEGIN
    stop, 'model_level_flag is not valid:', model_level_flag
ENDELSE

;;---------------------------------------------------------------------------
;;Adjust the default H1 and H2 depending on if you are uplooking or downlooking
;; to the values at the end points of the models levels
;;---------------------------------------------------------------------------

IF N_ELEMENTS(H1) LE 0 THEN BEGIN 
    IF (ANGLE GE 90.0d) THEN BEGIN                       ;assume we are downlooking    
        H1 = STRTRIM(STRING(model_levels(IBMAX_model_levels-1),format='(f15.8)'),2)
    ENDIF ELSE BEGIN                                     ;assume that we are uplooking
        H1 = STRTRIM(STRING(model_levels(0),format='(f15.8)'),2)
    ENDELSE
ENDIF

IF N_ELEMENTS(H2) LE 0 THEN BEGIN 
    IF (ANGLE GE 90.0d) THEN BEGIN                        ;assume we are downlooking  
        H2 = STRTRIM(STRING(model_levels(0),format='(f15.8)'), 2)   
    ENDIF ELSE BEGIN                                     ;assume that we are uplooking
        H2 = STRTRIM(STRING(model_levels(IBMAX_model_levels-1),format='(f15.8)'), 2) 
    ENDELSE
ENDIF

;;---------------------------------------------------------------------------

n_mol = NMOL   ;total number of molecules


;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------

openw, unit_new, output_path+output_filename, /get_lun


;;write out profiles for plotting............................................
c_i = 0
;openw, unit_alt,'prof_alt.txt', /get_lun
;openw, unit_pres,'prof_pres.txt', /get_lun
;openw, unit_temp,'prof_temp.txt', /get_lun
;openw, unit_c_i, 'prof_'+name_molec(c_i)+'.txt', /get_lun


str_title = ['ZM','PM','TM',name_molec(0:n_mol-1)]

;;...........................................................................

;;---------------------------------------------------------------------------
;;Record 1.1 LBLRTM
;;---------------------------------------------------------------------------
CXID = '$' ;80A1
IF (KEYWORD_SET(anal_jac_flag)) THEN CXID = '$ Run A: *** Compute ODint_lll optical depth files for Jacobians ***'
;;---------------------------------------------------------------------------
;;Record 1.2 LBLRTM
;;---------------------------------------------------------------------------
IF N_ELEMENTS(IHIRAC) LE 0 THEN IHIRAC = '1' 
ILBLF4 = '1' 
IF N_ELEMENTS(ICNTNM) LE 0 THEN ICNTNM = '1' 
IF N_ELEMENTS(IAERSL) LE 0 THEN IAERSL = '0' 
IEMIT = '1' 
ISCAN = '1' 
IFILTR = '0' 
IPLOT = '0' 
ITEST = '0' 
IATM = '1' 
IMRG = '=0' 
ILAS = '0' 
IF N_ELEMENTS(IOD) LE 0 THEN IOD = '0' 
IF N_ELEMENTS(IXSECT) LE 0 THEN IXSECT = '1' 
MPTS =  '00'
NPTS =  '00'
;;--------------------------------------------------------------------------- 
;;Record 1.2a LBLRTM
;;---------------------------------------------------------------------------
IF N_ELEMENTS(XSELF) LE 0 THEN XSELF = '1.0'
IF N_ELEMENTS(XFRGN) LE 0 THEN XFRGN = '1.0'
IF N_ELEMENTS(XCO2C) LE 0 THEN XCO2C = '1.0'
IF N_ELEMENTS(XO3CN) LE 0 THEN XO3CN = '1.0'
IF N_ELEMENTS(XO2CN) LE 0 THEN XO2CN = '1.0'
IF N_ELEMENTS(XN2CN) LE 0 THEN XN2CN = '1.0'
IF N_ELEMENTS(XRAYL) LE 0 THEN XRAYL = '1.0'
;;--------------------------------------------------------------------------- 
;;Record 1.3 LBLRTM
;;---------------------------------------------------------------------------
V1 = V1
V2 = V2
SAMPLE = '0.0'
DVSET = '0.0'
ALFALO = '0.0'
AVMASS = '0.0'
DPTMIN = '0.0000'
DPTFAC = '0.000'
ILNFLG = '0'
IF N_ELEMENTS(DVOUT) LE 0 THEN DVOUT = '0.0' 
;;---------------------------------------------------------------------------
;;Record 1.4 LBLRTM
;;---------------------------------------------------------------------------
TBOUND =   STRTRIM(STRING(TBOUND),2)   
IF ITYPE EQ 3 THEN BEGIN ;limb
    SREMIS_1 = ''
    SREMIS_2 = ''
    SREMIS_3 = ''
    SRREFL_1 = ''
    SRREFL_2 = ''
    SRREFL_3 = ''          
    surf_refl = ''
ENDIF ELSE BEGIN ;nadir, etc. 
    SREMIS_1 = STRTRIM(STRING(SREMIS_1))
    SREMIS_2 = '0.0'
    SREMIS_3 = '0.0'
    SRREFL_1 =  STRTRIM(STRING(SRREFL_1))
    SRREFL_2 = '0.0'
    SRREFL_3 = '0.0'          
    surf_refl = STRTRIM(STRING(surf_refl))
ENDELSE


;;---------------------------------------------------------------------------
;;Record 3.1 LBLRTM
;;---------------------------------------------------------------------------
MODEL = '0'
ITYPE = STRTRIM(STRING(ITYPE), 2)
IBMAX =  IBMAX
NOZERO = '1'
NOPRNT = STRTRIM(STRING(NOPRNT),2) 
NMOL = STRTRIM(STRING(n_mol),2)
IF (KEYWORD_SET(anal_jac_flag)) THEN BEGIN
    IPUNCH = '2'
ENDIF ELSE IPUNCH = '1' 
IFXTYP = '0'
MUNITS =  '0'
RE = '0.000'
HSPACE = '120.000'
VBAR = '0.000'
CO2MX =  STRTRIM(STRING(co2_vmr),2) ;this will override the profile value if set
;;---------------------------------------------------------------------------
;;Record 3.2 LBLRTM
;;---------------------------------------------------------------------------
H1 = STRTRIM(STRING(H1, format='(f15.8)'),2)
H2 = STRTRIM(STRING(H2, format='(f15.8)'),2)
ANGLE = STRTRIM(STRING(ANGLE, format='(f15.8)'),2)   ;180.0 - satzen angle
RANGE = '0.0' 
BETA = '0.0'
LEN = '0'
HOBS = '0.0'
;;---------------------------------------------------------------------------
;;Record 3.3b LBLRTM
;;---------------------------------------------------------------------------
;; Need to interpolate onto fewer levels (make it the pressure levels)
ZBND = STRTRIM(STRING(model_levels(*),format='(f15.8)'),2)
;;---------------------------------------------------------------------------
;;Record 3.4 LBLRTM user defined profile
;;---------------------------------------------------------------------------
IMMAX = STRTRIM(STRING(IMMAX))
hmod = 'User defined Profile'
;;---------------------------------------------------------------------------
;;Record 3.5 LBLRTM user defined profile
;;---------------------------------------------------------------------------
ZM_str = STRTRIM(STRING(ZM,format='(f10.3)'),2)
PM_str = STRTRIM(STRING(PM,format='(f15.8)'),2)
TM_str = STRTRIM(STRING(TM,format='(f10.3)'),2)
JCHARP = JCHARP
JCHART = JCHART
JLONG = JLONG 
JCHAR_arr = JCHAR_arr
;;---------------------------------------------------------------------------
;;Record 3.6 LBLRTM
;;---------------------------------------------------------------------------
VMOL = molec

IF jlong EQ 'L' THEN BEGIN
    FOR j=0, n_mol-1 DO BEGIN 
        VMOL(j, *) = STRTRIM(STRING(molec(j,*), format='(e15.8)'), 2)
    ENDFOR
ENDIF ELSE BEGIN
    FOR j=0, n_mol-1 DO BEGIN     
        VMOL(j, *) = STRTRIM(STRING(molec(j,*), format='(e10.3)'), 2)
    ENDFOR
ENDELSE

;;---------------------------------------------------------------------------
;;Record 3.7 LBLRTM
;;---------------------------------------------------------------------------
IXMOLS = STRTRIM(STRING(N_ELEMENTS(XSNAME)),2)  ;;number of xs molecules
IPRFL = '0'     ;0 user input, 1 standard profile in LBLATM
IXSBIN = '0'    ;flag to deselect pressure convolution of xs 
                ;(0, default is to convolve with pressure) 
;;---------------------------------------------------------------------------
;;Record 3.7.1 LBLRTM
;;---------------------------------------------------------------------------
XSNAME =  XSNAME
;;---------------------------------------------------------------------------
;;Record 3.8 LBLRTM (X-sections)
;;---------------------------------------------------------------------------
LAYX = STRTRIM(STRING(N_ELEMENTS(PM)),2)
IZORP = '1'    ;determines if the ZORP is in 0->KM or 1->mb
XTITLE = 'Cross-section Values'
;;---------------------------------------------------------------------------
;;Record 3.8.1 LBLRTM (X-sections)
;;---------------------------------------------------------------------------
ZORP = STRTRIM(STRING(PM,format='(f15.8)'),2)
JCHAR_xs =  replicate("A", IXMOLS)
;;---------------------------------------------------------------------------
;;Record 3.8.2 LBLRTM (X-sections)
;;---------------------------------------------------------------------------
;;...........................................................................
;;Get the volume mixing ratio's for the cross-sections
;;...........................................................................
IF (IXSECT GT 0) AND (IPRFL EQ 0) THEN BEGIN 

    DENX = DBLARR(IXMOLS, N_ELEMENTS(PM))
    header =  ''
    xs_alt =  DOUBLE(0.0)
    xs_pres = DOUBLE(0.0)
    xs_temp = DOUBLE(0.0)
    xs_vmr = DOUBLE(0.0)

    FOR p=0, IXMOLS-1 DO BEGIN
        xs_pres_arr = DBLARR(10000)
        xs_vmr_arr = DBLARR(10000)

        IF XSNAME(p) NE 'HNO3' THEN BEGIN 
            xs_filename = XSNAME(p)+'_vmr_UARS.txt'
        ENDIF ELSE xs_filename = XSNAME(p)+'_vmr_JPL.txt' ;;pne file from JPL

;        xs_filename_tmp =  XSNAME(p)+'_vmr_UARS.txt'
        xs_filename = file_search(xs_path+xs_filename, count=n_xs_files)
        IF n_xs_files NE 1 THEN stop, $
          'Needs to be one and only one xs file for molecule : '+XSNAME(p)

        ;print, 'Opening XS file : ', xs_filename(0)

        openr, unit_xs_in, xs_filename(0), /get_lun
        
        readf, unit_xs_in, header
        readf, unit_xs_in, header
        readf, unit_xs_in, header
        readf, unit_xs_in, header
        readf, unit_xs_in, header

        i = 0l
        WHILE NOT EOF(unit_xs_in) DO BEGIN
            readf, unit_xs_in, xs_alt, xs_pres, xs_temp, xs_vmr, format='(4e15.5)'
            xs_pres_arr(i) = xs_pres
            xs_vmr_arr(i) = xs_vmr
            i = i+1
        ENDWHILE
        
        xs_pres_arr = xs_pres_arr(0:i-1)
        xs_vmr_arr = xs_vmr_arr(0:i-1)
        
        ;;Quadratic interpolation onto the profile pressure levels
        DENX(p, *) = INTERPOL(xs_vmr_arr, xs_pres_arr, PM, /quadratic )
;        DENX(p, *) = interp_4pt(xs_pres_arr, xs_vmr_arr, PM)
        ;;set any negative values generated in the interpolation to zero 
        w_neg = where(DENX(p,*) LT 0.0)
        IF w_neg(0) GE 0 then DENX(p,w_neg) = 0.0

        
        free_lun, unit_xs_in
        close, unit_xs_in

ENDFOR
ENDIF

;;---------------------------------------------------------------------------
;;Record 8.1 LBLRTM (Required if ISCAN = 1)   Y. Han, May 17, 2010
;;   Box scan 
;;---------------------------------------------------------------------------

HWHM_box = HWHM           
V1_box = STRTRIM(STRING(V1_intep, format='(f15.8)'),2)             
V2_box = STRTRIM(STRING(V2_intep, format='(f15.8)'),2)               
JEMIT_box = 1  ;              
JFN_box = 0  
JVAR_box = 0
SAMPLE_box = -HWHM_box*2.0            
IUNIT_box = 12       
IFILST_box = 1               
NIFILS_box = 1              
JUNIT_box = 13              
NPTS_box = 0                 

;;---------------------------------------------------------------------------
;;Record 10.1 LBLRTM (Required if ISCAN = 3) FFTSCN 
;;---------------------------------------------------------------------------
HWHM = STRTRIM(STRING(HWHM, format='(f15.8)'),2)  ; HWHM is Maximum OPD if JFN < 0      
V1_scan = STRTRIM(STRING(V1_scan, format='(f15.8)'),2)
V2_scan = STRTRIM(STRING(V2_scan, format='(f15.8)'),2)
JEMIT = '1'                  
JFN = STRTRIM(STRING(JFN),2)               ;;0->boxcar,1->triangle,2->gauss,3->sinc sqr,4->sinc,5->Beer
                                           ;;(JFN < 0  Maximum OPD)   
MRAT = '0'                   
DVOUT_rec_10_1 = '0.0'                       
IUNIT = '12'                   
FIL = '1'                      
NFIL = '1'                       
JUNIT = '13'                     
IVX = '0'                       
NOFIX = '0'                     
PARAM_fftscan = STRTRIM(STRING(PARAM_fftscan,format='(f10.6)'),2)

;;---------------------------------------------------------------------------
;;Record 9.1 LBLRTM (Required if ISCAN = 2 (intrpl))
;;---------------------------------------------------------------------------
IF ISCAN GT 0 THEN BEGIN 
    IUNIT_intep = JUNIT ;"13"       
ENDIF ELSE IUNIT_intep = IUNIT ;"12"         

DV_intep = STRTRIM(STRING(DV_intep, format='(f15.8)'),2)           
V1_intep = STRTRIM(STRING(V1_intep, format='(f15.8)'),2)             
V2_intep = STRTRIM(STRING(V2_intep, format='(f15.8)'),2)               
JEMIT_intep = JEMIT             
I4PT_intep = '1'                
IUNIT_intep =  IUNIT_intep       
IFILST_intep = FIL               
NIFILS_intep = NFIL              
JUNIT_intep = '11'              
NPTS_intep = '0'                 
;;---------------------------------------------------------------------------
;;Record 12.1 LBLRTM (Required if IPLOT=1)
;;---------------------------------------------------------------------------
CPRGID = '# Plot title not used'
CEX = '  '
;;---------------------------------------------------------------------------
;;Record 12.2a LBLRTM (Required if IPLOT=1)
;;---------------------------------------------------------------------------
V1_plot = V1_intep
V2_plot = V2_intep
XSIZE_plot = '10.2000'
DELV_plot = '100.0000'
NUMSBX_plot = '5'
NOENDX_plot = '0'
LFILE_plot = JUNIT_intep
LSKIPF_plot = '0'
SCALE_plot = '1.0000'
IOPT_plot = '0'
I4P_plot = '0'
IXDEC_plot = '0'
;;---------------------------------------------------------------------------
;;Record 12.3a LBLRTM (Required if IPLOT=1) (Radiance)
;;---------------------------------------------------------------------------
YMIN = '0.0000'
YMAX = '1.2000'
YSIZE = '7.0200'
DELY = '0.2000'
NUMSBY = '4'
NOENDY = '0'
IDEC = '1'
JEMIT = '1'
JPLOT = '0' ;  -> 0 : Plots Radiance if JEMIT=1 ; 1 : Plots Brightness Temperature if JEMIT=1
LOGPLT = '0'
JHDR = '0'
JOUT = '3'
JPLTFL = '27' 
;;---------------------------------------------------------------------------
;;Record 12.3a LBLRTM (Required if IPLOT=1) (Brightness Temperature)
;;---------------------------------------------------------------------------
YMIN_bt = '0.0000'
YMAX_bt = '1.2000'
YSIZE_bt = '7.0200'
DELY_bt = '0.2000'
NUMSBY_bt = '4'
NOENDY_bt = '0'
IDEC_bt = '1'
JEMIT_bt = '1'
JPLOT_bt = '1' ;  -> 0 : Plots Radiance if JEMIT=1 ; 1 : Plots Brightness Temperature if JEMIT=1
LOGPLT_bt = '0'
JHDR_bt = '0'
JOUT_bt = '3'
JPLTFL_bt = '28' 
;;---------------------------------------------------------------------------
;;Record 1.2_Interpolate of FFTSCN  (Required if ISCAN = 2 (intrpl))
;;                                               **Set SC_intep = '2'**
;;---------------------------------------------------------------------------
IHIRAC_intep = '0'
ILBLF4_intep = '0' 
ICNTNM_intep = '0' 
IAERSL_intep = '0' 
IEMIT_intep = '0' 
IF n_elements(ISCAN_intep) EQ 0 THEN ISCAN_intep = '2' 
IFILTR_intep = '0' 
IPLOT_intep = '0' 
ITEST_intep = '0' 
IATM_intep = '0' 
IMRG_intep = '=0' 
ILAS_intep = '0' 
IOD_intep = '0' 
IXSECT_intep = '0' 
MPTS_intep = '00'
NPTS_intep = '00'
;;---------------------------------------------------------------------------
;;Record 1.2_Plot Transfer to ASCII plotting data
;;                                               **Set PL_plor = '1'**
;;---------------------------------------------------------------------------
IHIRAC_plot = '0'
ILBLF4_plot = '0' 
ICNTNM_plot = '0' 
IAERSL_plot = '0' 
IEMIT_plot = '0' 
ISCAN_plot = '0' 
IFILTR_plot = '0' 
IPLOT_plot = '0' 
ITEST_plot = '0' 
IATM_plot = '0' 
IMRG_plot = '=0' 
ILAS_plot = '0' 
IOD_plot = '0' 
IXSECT_plot = '0' 
MPTS_plot = '00'
NPTS_plot = '00'
;;---------------------------------------------------------------------------
;;Record 12.2a
;;---------------------------------------------------------------------------
repeat_12_2a = '-1.'
;;
;;***************************************************************************
;;*********    Write out TAPE5 file  ****************************************
;;***************************************************************************
;;
;; Format Statements
;;
;; (Note: For some parameters i.e. v1, v2, dv, etc. we want to write out the 
;;  highest precision possible for the allocated space because fortran will
;;  adjust the precision based on the decimal point even with a formatted read.
;;      -we do this by writting out the inputted  string with correct spacing
;;
format_1_1 = '(A80)'
format_1_2 = '(%" HI=",i1,%" F4=",i1,%" CN=",i1,%" AE=",i1,%" EM=",i1,%" SC=",i1,%" ' + $
  'FI=",i1,%" PL=",i1,%" TS=",i1,%" AM=",i1,'+ $
  '%" MG",a2,%" LA=",i1,%" OD=",i1,%" XS=",i1,2(1x,i4))'
format_1_2a = '(7(A10,1x))'
format_1_3 = '(8(A10),4x,i1,5x,A10)'
format_1_4 = '(7(A10),4x,a1)'
format_3_1 = '(7i5,i2,1x,i2,4A10)'
format_3_2 = '(3(1X,A9),2(A10),i5,5x,A10)'
format_3_3b = '(8(1X,A9))'      
format_3_4 = '(i5,a24)'
format_3_5 = '(A10,1X,A9,A10,5x,a1,a1,1x,a1,1x,A38)'
format_3_6 = '(8e10.3)'
format_3_6_L = '(8e15.8)'
format_3_7 = '(3i5)'
format_3_7_1 = '(7a10,(/8a10))'
format_3_8 = '(i5,i5,a50)'
format_3_8_1 = '(1X,A9,5x,35a1)'
format_3_8_2 = '(8e10.3)'
format_9_1 = '(3A10,2i5,15x,5i5)'
format_10_1 = '(3A10,3i5,A10,4i5,i3,i2,A10)'
format_12_1 = '(A60, 18x, A2)'
format_12_2a = '(4A10,4i5,A10,i2,i3,i5)'
format_12_3a = '(2A10,2A10,6i5,i2,3x,i2,i3)'


;;for plotting 
format_plot_title ='(3(3X,A5,3X),(8(2x,A6,2x)))'
format_plot_title_L = '(3(3X,A5,3X),(8(4x,A7,4x)))'
format_plot = '(3(A10,1x),(8e10.3))'
format_plot_L = '(3(A10,1x),(8e15.8))'

;;for plotting 
;openw, unit_molec,'atmos_prof.txt',/get_lun
;IF jlong EQ 'L' THEN BEGIN 
;    printf, unit_molec, str_title,format=format_plot_title_L
;ENDIF ELSE BEGIN
;    printf, unit_molec, str_title, format=format_plot_title
;ENDELSE 

;;---------------------------------------------------------------------------
;;print record 1.1
;;---------------------------------------------------------------------------
CXID_fmt = ''
reads, CXID, CXID_fmt, format='(A80)' ;; format the string in order for the
                                      ;; formated string to start on the left
printf, unit_new, CXID_fmt, format=format_1_1
;;---------------------------------------------------------------------------
;;print record 1.2
;;---------------------------------------------------------------------------
IF (KEYWORD_SET(anal_jac_flag)) THEN BEGIN
    IEMIT_jac = '0'
    ISCAN_jac = '0'
    IMRG_jac = '=1'
    IOD_jac = '3'
    printf, unit_new, IHIRAC, ILBLF4, ICNTNM, IAERSL, IEMIT_jac, ISCAN_jac, IFILTR, IPLOT, $
      ITEST, IATM, IMRG_jac, ILAS, IOD_jac, IXSECT, MPTS, NPTS, format=format_1_2
ENDIF ELSE printf, unit_new, IHIRAC, ILBLF4, ICNTNM, IAERSL, IEMIT, ISCAN, IFILTR, IPLOT, ITEST, $
  IATM, IMRG, ILAS, IOD, IXSECT, MPTS, NPTS, format=format_1_2


;;---------------------------------------------------------------------------
;;print record 1.2 if ICNTNM = 6
;;---------------------------------------------------------------------------
IF ICNTNM EQ 6 THEN BEGIN 
    printf, unit_new, XSELF, XFRGN, XCO2C, XO3CN, XO2CN, XN2CN, XRAYL, $
      format=format_1_2a
ENDIF
;;---------------------------------------------------------------------------
;;print record 1.3
;;---------------------------------------------------------------------------
printf, unit_new, V1, V2, SAMPLE, DVSET, ALFALO, AVMASS, DPTMIN, $ 
  DPTFAC, ILNFLG, DVOUT, format=format_1_3
;;---------------------------------------------------------------------------
;;print record 1.4
;;---------------------------------------------------------------------------
IF NOT keyword_set(anal_jac_flag) THEN BEGIN
    printf, unit_new, TBOUND, SREMIS_1, SREMIS_2, SREMIS_3, SRREFL_1, SRREFL_2, $
      SRREFL_3, surf_refl, format=format_1_4
ENDIF
;;---------------------------------------------------------------------------
;;print record 3.1
;;---------------------------------------------------------------------------
printf, unit_new, MODEL, ITYPE, IBMAX, NOZERO, NOPRNT, NMOL, IPUNCH, $
  IFXTYP, MUNITS, RE, HSPACE, VBAR, CO2MX, format=format_3_1
;;---------------------------------------------------------------------------
;;print record 3.2
;;---------------------------------------------------------------------------
printf, unit_new, H1, H2, ANGLE, RANGE, BETA, LEN, HOBS, $ 
  format=format_3_2
;;---------------------------------------------------------------------------
;;print record 3.3b
;;---------------------------------------------------------------------------
printf, unit_new, ZBND, format=format_3_3b
;;---------------------------------------------------------------------------
;;print record 3.4
;;---------------------------------------------------------------------------
printf, unit_new, IMMAX, hmod, format=format_3_4
;;---------------------------------------------------------------------------
;;print record 3.5, 3.6
;;---------------------------------------------------------------------------

FOR j=0, IMMAX-1 DO BEGIN 
    JCHAR_fmt = ''
    reads, JCHAR_arr(j), JCHAR_fmt, format='(A38)' ;; format the string in order for the
                                                ;; it to start on the left
    printf, unit_new, ZM_str(j), PM_str(j), TM_str(j), $
      JCHARP, JCHART, JLONG, JCHAR_fmt, format=format_3_5
    IF jlong EQ 'L' THEN BEGIN 
        printf, unit_new, VMOL(*,j), format=format_3_6_L
;       for plotting
        ;printf, unit_molec, ZM_str(j), PM_str(j), TM_str(j), VMOL(*, j), format=format_plot_L
    ENDIF ELSE BEGIN
        printf, unit_new, VMOL(*,j), format=format_3_6
;       for plotting 
        ;printf, unit_molec, ZM_str(j), PM_str(j), TM_str(j), VMOL(*, j), format=format_plot
    ENDELSE
ENDFOR 
;;---------------------------------------------------------------------------
;;Cross-sections if IXSECT = 1
;;---------------------------------------------------------------------------
IF IXSECT EQ 1 THEN BEGIN 
    ;;print record 3.7
    printf, unit_new, IXMOLS, IPRFL, IXSBIN, format=format_3_7
    ;;print record 3.7.1
    printf, unit_new, XSNAME, format=format_3_7_1    
    IF (IPRFL EQ 0) THEN BEGIN
        ;;print record 3.8
        printf, unit_new, LAYX, IZORP, XTITLE, format=format_3_8    
        FOR kk=0, LAYX-1 DO BEGIN 
            ;;print record 3.8.1
            printf, unit_new, ZORP(kk), JCHAR_xs, format=format_3_8_1
            ;;print record 3.8.2
            printf, unit_new, DENX(*, kk), format=format_3_8_2
        ENDFOR
    ENDIF
ENDIF

;;---------------------------------------------------------------------------
;; IF Analytic Jacobian TAPE5 then write out Run B
;;---------------------------------------------------------------------------
IF keyword_set(anal_jac_flag) THEN BEGIN
    ;;---------------------------------------------------------------------------
    ;; FOR RUN B
    ;;---------------------------------------------------------------------------
    ;;print record 1.1 
    CXID = '$ Run_B:  *** Compute Downwelling Radiance Files, RDDNlayer_lll ***' 
    CXID_fmt = ''
    reads, CXID, CXID_fmt, format='(A80)' ;; format the string in order for the
    ;; formated string to start on the left
    printf, unit_new, CXID_fmt, format=format_1_1
    ;;print record 1.2
    printf, unit_new, IHIRAC_intep, ILBLF4_intep, ICNTNM_intep, IAERSL_intep, '1', $
      '0', IFILTR_intep, IPLOT_intep, ITEST_intep, IATM_intep, '40', ILAS_intep, $ 
      '3', IXSECT, MPTS_intep, NPTS_intep, format=format_1_2   
    ;;print record 1.3
    printf, unit_new, V1, V2, SAMPLE, DVSET, ALFALO, AVMASS, DPTMIN, $ 
      DPTFAC, ILNFLG, DVOUT, format=format_1_3
    ;;print record 1.4
    printf, unit_new, TBOUND, SREMIS_1, SREMIS_2, SREMIS_3, SRREFL_1, SRREFL_2, $
      SRREFL_3, surf_refl, format=format_1_4
    ;;print
    printf, unit_new, 'ODint_'

    FOR jj=0, N_ELEMENTS(NSPCRT_arr)-1 DO BEGIN 
        ;;---------------------------------------------------------------------------
        ;; FOR RUN C
        ;;---------------------------------------------------------------------------
        ;;print record 1.1 
        CXID = '$ Run_C:  *** Compute Analytic Jacobian Files in AJ Directory, LEV_RDderivUPW_00_lll and RDderivUPW_00_lll ***'
        CXID_fmt = ''
        reads, CXID, CXID_fmt, format='(A80)' ;; format the string in order for the
        ;; formated string to start on the left
        printf, unit_new, CXID_fmt, format=format_1_1
        ;;print record 1.2
        printf, unit_new, IHIRAC, ILBLF4, ICNTNM, IAERSL, '3', $
          '0', IFILTR_intep, IPLOT, ITEST, '0' , '41', ILAS, $ 
          '3', '0', MPTS, NPTS, format=format_1_2   
        ;;print record 1.3
        printf, unit_new, V1, V2, SAMPLE, DVSET, ALFALO, AVMASS, DPTMIN, $ 
          DPTFAC, ILNFLG, DVOUT, format=format_1_3
        ;;print record 1.4
        printf, unit_new, TBOUND, SREMIS_1, SREMIS_2, SREMIS_3, SRREFL_1, SRREFL_2, $
          SRREFL_3, surf_refl, format=format_1_4
        ;;print
        printf, unit_new, NSPCRT_arr(jj), format='(i5)'
        printf, unit_new, 'ODint_'
     ENDFOR

    ;; Close TAPE5 for Analytic Jacobians   
    printf, unit_new, repeat_12_2a
    printf, unit_new, '%%%%%%%'
    free_lun, unit_new
    close, unit_new

    ;; Open new TAPE5 that contains the Scan and Interpolation Function
    openw, unit_new, output_path+output_filename+'_scan', /get_lun
    ;;print record 1.1 
    CXID = '$ *** FFTSCAN'
    CXID_fmt = ''
    reads, CXID, CXID_fmt, format='(A80)' ;; format the string in order for the
    ;; formated string to start on the left
    printf, unit_new, CXID_fmt, format=format_1_1
    ;;print record 1.2
    printf, unit_new, IHIRAC_intep, ILBLF4_intep, ICNTNM_intep, IAERSL_intep, IEMIT_intep, ISCAN, IFILTR_intep, IPLOT_intep, ITEST_intep, $
       IATM_intep, IMRG_intep, ILAS_intep, IOD_intep, IXSECT_intep, MPTS_intep, NPTS_intep, format=format_1_2 
ENDIF

;;---------------------------------------------------------------------------
;;Record 8.1 LBLRTM (Required if ISCAN = 1)   Y. Han, May 17, 2010
;;   Box scan 
;;---------------------------------------------------------------------------
IF ISCAN EQ 1 THEN BEGIN
    printf, unit_new, HWHM_box, V1_box, V2_box, JEMIT_box, JFN_box, JVAR_box, SAMPLE_box, IUNIT, $
                      IFILST_box, NIFILS_box, JUNIT_box, NPTS_box,$
                      format='(F10.8, F10.4, F10.4, 3X,I2, 3X,I2, 3X,I2, F10.6, 3X,I2, 3X,I2, 3X,I2, 3X,I2, I5)'
ENDIF
;;---------------------------------------------------------------------------
;; FFTSCN (instrument line shape) if ISCAN = 3 
;;---------------------------------------------------------------------------
IF ISCAN EQ 3 THEN BEGIN 
    ;;print record 10.1 
    printf, unit_new, HWHM, V1_scan, V2_scan, JEMIT, JFN, MRAT, DVOUT_rec_10_1, IUNIT, FIL, $
      NFIL, JUNIT, IVX , NOFIX, PARAM_fftscan, format=format_10_1
   printf, unit_new, repeat_12_2a
ENDIF
;;---------------------------------------------------------------------------
;; Interpolation of FFTSCN if ISCAN = 2
;;---------------------------------------------------------------------------
IF ISCAN_intep EQ 2 THEN BEGIN 
    printf, unit_new, '$ Interpolation of FFTSCAN'
    ;;print record 1.2 (Interpolation)
    printf, unit_new, IHIRAC_intep, ILBLF4_intep, ICNTNM_intep, IAERSL_intep, IEMIT_intep, $
      ISCAN_intep, IFILTR_intep, IPLOT_intep, ITEST_intep, IATM_intep, IMRG_intep, ILAS_intep, $ 
      IOD_intep, IXSECT_intep, MPTS_intep, NPTS_intep, format=format_1_2
    ;;print record 9.1 if ISCAN_interp = 2 (ISCAN = 2)
    printf, unit_new,DV_intep , V1_intep, V2_intep, JEMIT_intep, I4PT_intep, $
      IUNIT_intep, IFILST_intep, NIFILS_intep, JUNIT_intep, NPTS_intep, format=format_9_1
    printf, unit_new, repeat_12_2a
ENDIF
;;---------------------------------------------------------------------------
;; ASCII Files (radiance and brightness temperature) if IPLOT=1
;;---------------------------------------------------------------------------
IF IPLOT_plot EQ 1 THEN BEGIN 
    printf, unit_new, '$ Transfer to ASCII plotting data'
    ;;print record 1.2 (ASCII Plotting)
    printf, unit_new, IHIRAC_plot, ILBLF4_plot, ICNTNM_plot, IAERSL_plot, IEMIT_plot, $
      ISCAN_plot, IFILTR_plot, IPLOT_plot, ITEST_plot, IATM_plot, IMRG_plot, ILAS_plot, $
      IOD_plot, IXSECT_plot, MPTS_plot, NPTS_plot, format=format_1_2
    ;;print record 12.1
    CPRGID_fmt = ''
    reads, CPRGID, CPRGID_fmt, format='(A60)' ;format strings
    CEX_fmt = ''
    reads, CEX, CEX_fmt, format='(A2)'        
    printf, unit_new, CPRGID_fmt, CEX_fmt, format=format_12_1
    ;;print record 12.2a (radiance)
    printf, unit_new, V1_plot, V2_plot, XSIZE_plot, DELV_plot, NUMSBX_plot, $
      NOENDX_plot, LFILE_plot, LSKIPF_plot, SCALE_plot, IOPT_plot, I4P_plot, $ 
      IXDEC_plot, format=format_12_2a
    ;;print record 12.3a (radiance)
    printf, unit_new, YMIN , YMAX, YSIZE, DELY, NUMSBY, NOENDY, IDEC, $ 
      JEMIT, JPLOT, LOGPLT, JHDR, JOUT, JPLTFL, format=format_12_3a
    ;;print record 12.2a (bright. temp.)
    printf, unit_new, V1_plot, V2_plot, XSIZE_plot, DELV_plot, NUMSBX_plot, $
      NOENDX_plot, LFILE_plot, LSKIPF_plot, SCALE_plot, IOPT_plot, I4P_plot, $ 
      IXDEC_plot, format=format_12_2a
    ;;print record 12.3a (bright. temp.)
    printf, unit_new, YMIN_bt , YMAX_bt, YSIZE_bt, DELY_bt, NUMSBY_bt, NOENDY_bt, $
      IDEC_bt, $ 
      JEMIT_bt, JPLOT_bt, LOGPLT_bt, JHDR_bt, JOUT_bt, JPLTFL_bt, format=format_12_3a
    printf, unit_new, repeat_12_2a
ENDIF

printf, unit_new, '-1.0'
printf, unit_new, '%%%%%%%'

free_lun, unit_new
;free_lun, unit_molec
close, unit_new
;close, unit_molec


IF i_plot GE 1 THEN BEGIN 

;;---------------------------------------------------------------
;;Plotting the profiles
;;---------------------------------------------------------------

;;plotting the data===============================================
!p.font = 0
!x.thick = 5
!y.thick = 5
!p.thick = 3.5
!p.charthick = 1.5
!p.region = [0.05, 0.05, 0.90, 0.95]
get_colors
plot_color = 3 
plot_color_a = 0
plot_color_b = 18
plot_color_c = 2
plot_color_d = 6
plot_color_e = 4
;;=================================================================

;;---------------------------------------------------------
;;Reading in LBLRTM Built in Profile Values for plotting
;;---------------------------------------------------------
;;
;;Inputs
;;
atmos_profile_arr = ['tropical_xx', 'mid_lat_sum', 'mid_lat_win', $
                     'sub_arc_sum', 'sub_arc_win', 'usa_std_atm']
;
;  Note: Minor molecules (after the first 7) are only available with US standard atmosphere
molec_arr_name = ['H2O', 'CO2', 'O3', 'N2O', 'CO', 'CH4', 'O2', 'NO', 'SO2', $
         'NO2', 'NH3', 'HNO3', 'OH', 'HF', 'HCL', 'HBR', 'HI', $
         'CLO', 'OCS', 'H2CO', 'HOCL', 'N2', 'HCN', 'CH3CL', 'H2O2', $
         'C2H2', 'C2H6', 'PH3', 'COF2', 'SF6', 'H2S', 'HCOOH'] 
;
; OUTPUTS:  molec_i_prof=molec_i_prof        ;specified molecule profile (ppmv) 
;           molec_wgts_i=molec_wgts_i        ;molecular weight of specified molecule
;           molec_wgt_air=molec_wgt_air      ;molecular weight of air
;           density_air=AMOL8                ;air density profile (molec cm-3)
;           ALT=ALT                          ;altitude (km)
;           PRES=PRES                        ;pressure profile (hPa)
;           TEMP=TEMP                        ;tempurature profile(K)
;           HMOLC_arr=HMOLC_arr              ;name of all molecules in LBLRTM

atmos_profile = atmos_profile_arr(0)
molec_lblrtm = molec_arr_name(0)

read_lblrtm_profiles, atmos_profile, molec_lblrtm, input_file=input_file, $
                          n_molec=n_molec,  molec_i_prof=molec_i_prof, $
                          density_air=AMOL8, molec_wgt_air=AIRMWT, $
                          molec_wgts_i=molec_wgts_i, ALT=ALT_lblrtm, PRES=PRES_lblrtm, $
                          TEMP=TEMP_lblrtm, HMOLC_arr=HMOLC_arr, header_str=prof_header


y_range = [MAX(DOUBLE(PM))+100.,MIN(DOUBLE(PM))]

;;;...............................................
;;;for plotting the minor tick marks on a log plot
mind = MIN(y_range)        ; MIN(plot_pres_orig)
maxd = MAX(y_range)        ; MAX(plot_pres_orig)
log_lev_y = loglevels([mind,maxd], fine=1)
;;;...............................................


    device, /color, /landscape, /bold, filename=output_path+'temp'+'_'+output_filename+'.ps' 

    plot, TM, PM, /nodata, /ylog, $
      xstyle=2, $
      ystyle=1, $
      yrange=y_range, $
      title='AIRS Validation Profile', $
      ytitle='Pressure (mb)', $
      xtitle='Temperature (K)' , $
      yticks=0, $
      ytickname=['         ', '         ', '         ', '         ', $
                 '         ', '         ' + $
                 '         ', '         ']
    
    legend_old, [ "Input Profile", "AFGL ("+STRTRIM(prof_header, 2)+") " ], $
      textcolors=[plot_color, plot_color_c], $
      box=0, spacing=1.0, colors=[plot_color, plot_color_c ], /right, /top, $
      linestyle=[0, 2]
    
    axlabel, log_lev_y, /yaxis  ;put on the minor tick label for log plot
    
    oplot, TM, PM, color=plot_color
    
    oplot, TEMP_lblrtm, PRES_lblrtm, linestyle=2, color=plot_color_c
    
    date_stamp
    device, /close

    ;;----------------
    
    c_i_arr = [0, 2]            ;molecule numbers to plot
    
    FOR i=0, N_ELEMENTS(c_i_arr)-1 DO BEGIN 
        
        c_i = c_i_arr(i)
        
        y_range = y_range

        device, /color, /landscape, /bold, filename=output_path+name_molec(c_i)+'_'+output_filename+'.ps' 
    
        plot, molec(c_i, *), PM, /nodata, /ylog, /xlog, $
          xstyle=2, $
          ystyle=1, $
          yrange=y_range, $
          title='Profile', $
          ytitle='Pressure (mb)', $
          xtitle=name_molec(c_i)+' (ppmv)' , $
          yticks=0, $
          ytickname=['         ', '         ', '         ', '         ', $
                     '         ', '         ' + $
                     '         ', '         ']
        
        axlabel, log_lev_y, yaxis=1, xaxis=0 ;put on the minor tick label for log plot
        
        oplot, molec(c_i, *), PM
        
        date_stamp
        device, /close
        
    ENDFOR

ENDIF

set_plot, 'X'

END
