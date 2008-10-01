;+
; Procedure to inspect a SensorInfo structure

PRO SensorInfo::Inspect, Verbose=Verbose, Debug=Debug  ; Input keyword
;-

  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN
    HELP, *self.Sensor_Channel, $
          *self.Use_Flag      , $
          *self.Noise         
  ENDIF 
  
END ; FUNCTION SensorInfo::Inspect
