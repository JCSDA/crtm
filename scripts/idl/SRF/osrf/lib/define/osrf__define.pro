;+
; OSRF object definition procedure

PRO OSRF__Define

;-

  void = { OSRF, $
           n_Allocates      : 0L,        $  ; Allocation counter
           Release          : 0L,        $  ; Release: this identified structure and file format
           Version          : 0L,        $  ; Version: this is just the data version
           n_Bands          : 0L,        $  ; Number of frequency bands,  nB
           Sensor_Id        : ' ',       $  ; Sensor identifier
           WMO_Satellite_ID : 0L,        $  ; Sensor ID defined by WMO
           WMO_Sensor_ID    : 0L,        $  ; Sensor ID defined by WMO
           Sensor_Type      : 0L,        $  ; Sensor type (MW, IR, etc)
           Channel          : 0L,        $  ; Sensor channel number
           Integral         : 0.0d0,     $  ; Integrated SRF
           Flags            : 0L,        $  ; Bit-flags
           f0               : 0.0d0,     $  ; Central frequency
           Planck_C1        : 0.0d0,     $  ; First Planck coefficient, c1.v^3
           Planck_C2        : 0.0d0,     $  ; Second Planck coefficient, c2.v
           Band_C1          : 0.0d0,     $  ; Offset polychromatic correction coefficient
           Band_C2          : 0.0d0,     $  ; Slope polychromatic correction coefficient
           f1               : PTR_NEW(), $  ; Band begin frequency (nB)
           f2               : PTR_NEW(), $  ; Band end   frequency (nB)
           n_Points         : PTR_NEW(), $  ; Number of points per band, nP (nB)
           Frequency        : PTR_NEW(), $  ; SRF band frequencies (nP x nB)
           Response         : PTR_NEW(), $  ; SRF band response    (nP x nB)
           B                : PTR_NEW(), $  ; Planck radiance (nP x nB)
           R                : PTR_NEW()  }  ; LBL radiance (nP x nB)

; How the Frequency, Response, B, and R components are structured for, e.g.,
; a channel with 4-bands:
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
