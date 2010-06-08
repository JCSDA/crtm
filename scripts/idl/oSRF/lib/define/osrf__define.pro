;+
; OSRF object definition procedure

PRO OSRF__Define

;-

  @osrf_parameters
  
  void = { OSRF, $
           n_Allocates         : 0L,                             $  ; Allocation counter
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
           f1                  : PTR_NEW(),                      $  ; Band begin frequency (nB)
           f2                  : PTR_NEW(),                      $  ; Band end   frequency (nB)
           n_Points            : PTR_NEW(),                      $  ; Number of points per band, nP (nB)
           Frequency           : PTR_NEW(),                      $  ; SRF band frequencies (nP x nB)
           Response            : PTR_NEW(),                      $  ; SRF band response    (nP x nB)
           ; The following components are PRIVATE to the class
           ; ...Variables for radiometric calculations
           Radiance            : PTR_NEW(),                      $  ; Array of radiances (nP x nB)
           ; ...Variables for plotting
           xsysvar             : PTR_NEW(),                      $  ; X-axis system variable
           ysysvar             : PTR_NEW(),                      $  ; Y-axis system variable
           psysvar             : PTR_NEW()                       }  ; Plotting system variable

; There is no expectation that each band will have the same number of points.
; How the Frequency, Response, B, and R components are structured for, e.g.,
; a channel with 4-bands is shown below.
;
;               ---------------
; Frequency -> | 1 | 2 | 3 | 4 |
;               -|---|---|---|-
;                /   |   |   \
;               /   /     \   \
;              /   |       |   \
;             /    |       |    \
;            /     |       |     \
;           |      |       |      |
;           V      V       V      V
;          ---    ---     ---    ---
;         |   |  |   |   |   |  |   |
;         |---|  |---|   |---|  |---|
;         |   |  |   |   |   |  |   |
;         |---|  |---|   |---|  |---|
;         |   |  |   |   |   |  |   |
;         |---|  |---|   |---|  |---|
;         |   |  |   |   |   |  |   |
;         |---|  |---|   |---|  |---|
;         |   |  |   |   |   |  |   |
;           .      .       .      .
;           .      .       .      .
;           .      .       .      .

END ; PRO OSRF__Define
