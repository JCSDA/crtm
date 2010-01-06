;+
; OSRF file object definition procedure

PRO OSRF_File__Define

;-

  void = { OSRF_File, $
           ; File information
           filename: '', $
           Release : 0L, $  ; Release: this identified structure and file format
           Version : 0L, $  ; Version: this is just the data version
           ; Generic dimension data
           n_Channels : 0L, $
           ; Channel independent data
           Sensor_Id        : '', $  ; Sensor identifier
           WMO_Satellite_ID : 0L, $  ; Sensor ID defined by WMO
           WMO_Sensor_ID    : 0L, $  ; Sensor ID defined by WMO
           Sensor_Type      : 0L, $  ; Sensor type (MW, IR, etc)
           ; Global attributes
           Title            : '', $  ; File data title
           History          : '', $  ; File data history
           Comment          : '', $  ; File data comment
           ; Container for OSRF objects
           INHERITS IDL_Container }

END ; PRO OSRF_File__Define
