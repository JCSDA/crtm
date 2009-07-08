;+
; AtmProfile object definition procedure

PRO AtmProfile__Define

;-

  void = { AtmProfile, $
           n_Allocates : 0L, $  ; Allocation counter
           Release     : 0L, $  ; Release: this identified structure and file format
           Version     : 0L, $  ; Version: this is just the data version
           ; Dimensions
           n_Levels        : 0L, $  ; Number of levels dimensions,   K+1
           n_Layers        : 0L, $  ; Number of layers dimension,    K
           n_Absorbers     : 0L, $  ; Number of absorbers dimension, J
           ; Profile information
           Profile           : 0L, $  ; Profile number
           Description       : '', $  ; Profile description
           Climatology_Model : 0L, $  ; Climatology model id
           ; Absorber information
           Absorber_ID         : PTR_NEW(), $  ; Absorber id, J
           Absorber_Units_ID   : PTR_NEW(), $  ; Absorber units id, J
           Absorber_Units_Name : PTR_NEW(), $  ; Absorber units name, J
           Absorber_Units_LBL  : PTR_NEW(), $  ; Absorber units id used in LBLRTM/MonoRTM, J
           ; Date and time associated with profile data
           Year : 0L, $
           Month: 0L, $
           Day  : 0L, $
           Hour : 0L, $
           ; Location associated with profile data
           Latitude         : 0.0d0, $
           Longitude        : 0.0d0, $
           Surface_Altitude : 0.0d0, $
           ; Level profile data
           Level_Pressure    : PTR_NEW(), $  ; Dimension K+1
           Level_Temperature : PTR_NEW(), $  ; Dimension K+1
           Level_Absorber    : PTR_NEW(), $  ; Dimension K+1 x J
           Level_Altitude    : PTR_NEW(), $  ; Dimension K+1
           ; Layer profile data
           Layer_Pressure    : PTR_NEW(), $  ; Dimension K
           Layer_Temperature : PTR_NEW(), $  ; Dimension K
           Layer_Absorber    : PTR_NEW(), $  ; Dimension K x J
           Layer_Delta_Z     : PTR_NEW(), $  ; Dimension K
           ;
           ; The following components are PRIVATE to the class
           ; ...Variables for plotting
           xsysvar             : PTR_NEW(), $  ; X-axis system variable
           ysysvar             : PTR_NEW(), $  ; Y-axis system variable
           psysvar             : PTR_NEW()  }  ; Plotting system variable

END ; PRO AtmProfile__Define
