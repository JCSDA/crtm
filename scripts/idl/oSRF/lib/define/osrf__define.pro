;+
; NAME:
;       OSRF__Define
;
; PURPOSE:
;       This is the OSRF object definition procedure.
;
; CALLING SEQUENCE:
;       This definition procedure is not called directly, but during the
;       creation of an object, e.g.
;
;       Obj = OBJ_NEW( 'OSRF' )
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 22-May-2009
;                       paul.vandelst@noaa.gov
;
;-

;===============================================================================
;
; SPECIAL NOTES:
;       The following components are set explicitly in methods/functions other
;       than the Set_Property method:
;       - Integral: Set in the oSRF_Subtract function.
;       - Flags   : Set in the oSRF_Subtract function.
;       - f0      : Set in the oSRF_Subtract function.
;       - T       : Set in the oSRF_Subtract function.
;       - Teff    : Set in the oSRF_Subtract function.
;       - Tfit    : Set in the oSRF_Subtract function.
;
;===============================================================================

PRO OSRF__Define

  COMPILE_OPT HIDDEN
  @osrf_parameters

  void = { OSRF, $
           Is_Allocated        : 0L,                             $  ; Allocation indicator
           Release             : 0L,                             $  ; Release: this identified structure and file format
           Version             : 0L,                             $  ; Version: this is just the data version
           n_Bands             : 0L,                             $  ; Number of frequency bands,  nB
           Sensor_Id           : ' ',                            $  ; Sensor identifier
           WMO_Satellite_ID    : 0L,                             $  ; Sensor ID defined by WMO
           WMO_Sensor_ID       : 0L,                             $  ; Sensor ID defined by WMO
           Sensor_Type         : 0L,                             $  ; Sensor type (MW, IR, etc)
           Channel             : 0L,                             $  ; Sensor channel number
           Integral            : 0.0d0,                          $  ; Integrated SRF
           Flags               : 0L,                             $  ; Bit-flags
           f0                  : 0.0d0,                          $  ; Central frequency
           Planck_Coeffs       : DBLARR(N_PLANCK_COEFFS),        $  ; Planck coefficients
           Polychromatic_Coeffs: DBLARR(N_POLYCHROMATIC_COEFFS), $  ; Polychromatic correction coefficients
           Convolved_R         : 0.0d0,                          $  ; Convolved radiance
           Convolved_T         : 0.0d0,                          $  ; "Convolved" brightness temperature radiance
           f1                  : HASH(),                         $  ; Band begin frequency (nB)
           f2                  : HASH(),                         $  ; Band end   frequency (nB)
           n_Points            : HASH(),                         $  ; Number of points per band, nP (nB)
           Frequency           : HASH(),                         $  ; SRF band frequencies (nP x nB)
           Response            : HASH(),                         $  ; SRF band response    (nP x nB)
           Bandwidth           : HASH(),                         $  ; Passband width (nB)
           ; ...Variables for radiometric calculations
           Radiance            : HASH(),                         $  ; Array of radiances (nP x nB)
           ; ...Variables for polychromatic correction calculations
           T                   : PTR_NEW(/ALLOCATE_HEAP),        $  ; Array of true temperatures
           Teff                : PTR_NEW(/ALLOCATE_HEAP),        $  ; Array of effective temperatures
           Tfit                : PTR_NEW(/ALLOCATE_HEAP),        $  ; Array of fitted temperatures
           ; ...Variables for plotting. Not copied if Assign() method is used.
           wRef                : OBJ_NEW(),                      $  ; SRF Graphics window
           pRef                : HASH(),                         $  ; SRF Passband plots
           twRef               : OBJ_NEW(),                      $  ; Tfit Graphics window
           tpRef               : OBJ_NEW()                       }  ; Tfit plot

END
