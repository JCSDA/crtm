;+
; SensorInfo structure definition procedure
;
PRO SensorInfo__Define
;
;-
  void = { SensorInfo, $
           n_Allocates     : 0L,        $  ; Allocation counter
           n_Channels      : 0L,        $  ; Number of sensor channels (L)
           Sensor_Name     : ' ',       $  ; The name of the sensor
           Satellite_Name  : ' ',       $  ; The name of the satellite platform
           Sensor_Id       : ' ',       $  ; Sensor identifier
           WMO_Satellite_ID: 0L,        $  ; Satellite ID defined by WMO
           WMO_Sensor_ID   : 0L,        $  ; Sensor ID defined by WMO
           Microwave_Flag  : 0L,        $  ; Flag for microwave sensor (0=IR, 1=MW) **DEPRECATED. Use Sensor_Type**
           Sensor_Type     : 0L,        $  ; Sensor type (MW, IR, etc)
           Sensor_Channel  : PTR_NEW(), $  ; List of sensor channel numbers
           Use_Flag        : PTR_NEW(), $  ; Channel use flag list (0=no use, 1=use)
           Noise           : PTR_NEW()  }  ; Noise level in Kelvin for sensor channels

END ; PRO SensorInfo__Define
