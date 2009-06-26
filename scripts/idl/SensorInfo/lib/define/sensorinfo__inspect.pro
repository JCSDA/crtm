;+
; NAME:
;       SensorInfo::Inspect
;
; PURPOSE:
;       The SensorInfo::Inspect procedure method outputs information
;       about the current SensorInfo object.
;
; CALLING SEQUENCE:
;       Obj->[SensorInfo::]Inspect, Verbose=Verbose, &  ; Input keyword
;                                   Debug  =Debug       ; Input keyword
;
; INPUT KEYWORD PARAMETERS:
;       Verbose:     Set this keyword for more verbose output.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Regular output. (DEFAULT)
;                       SET     => Information about all currently compiled
;                                  routines and their arguments are output.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; EXAMPLE:
;       Inspect the contents of a SensorInfo object, x:
;
;         IDL> x->Inspect, /Verbose
;         ** Object class SENSORINFO, 0 direct superclasses, 9 known methods
;            Known Function Methods:
;               SENSORINFO::ALLOCATE
;               SENSORINFO::ASSIGN
;               SENSORINFO::ASSOCIATED
;               SENSORINFO::DESTROY
;               SENSORINFO::GET
;               SENSORINFO::INIT
;               SENSORINFO::SET
;            Known Procedure Methods:
;               SENSORINFO::CLEANUP
;               SENSORINFO::INSPECT
;            Instance Data:
;               ** Structure SENSORINFO, 12 tags, length=72, data length=72:
;               N_ALLOCATES     LONG                 1
;               N_CHANNELS      LONG                19
;               SENSOR_NAME     STRING    'HIRS/4      '
;               SATELLITE_NAME  STRING    'NOAA-18     '
;               SENSOR_ID       STRING    'hirs4_n18           '
;               WMO_SATELLITE_ID
;                               LONG               209
;               WMO_SENSOR_ID   LONG               607
;               MICROWAVE_FLAG  LONG                 0
;               SENSOR_TYPE     LONG                 0
;               SENSOR_CHANNEL  POINTER   <PtrHeapVar2385>
;               USE_FLAG        POINTER   <PtrHeapVar2386>
;               NOISE           POINTER   <PtrHeapVar2387>
;         <PtrHeapVar2385>
;                         LONG      = Array[19]
;         <PtrHeapVar2386>
;                         LONG      = Array[19]
;         <PtrHeapVar2387>
;                         DOUBLE    = Array[19]
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

PRO SensorInfo::Inspect, Verbose=Verbose, Debug=Debug  ; Input keyword

  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN
    HELP, *self.Sensor_Channel, $
          *self.Use_Flag      , $
          *self.Noise         
  ENDIF 
  
END ; FUNCTION SensorInfo::Inspect
