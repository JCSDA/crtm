;+
; NAME:
;       OSRF::Apply_to_LBL
;
; PURPOSE:
;       The OSRF::Apply_to_LBL procedure method applies the OSRF data to
;       computed line-by-line (LBL) radiances. The LBL models used are
;       MonoRTM (for microwave sensors) and LBLRTM (for infrared sensors).
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Apply_to_LBL, $
;         gt5_File, $  ; Input
;         Debug=Debug  ; Input keyword
;
; INPUT ARGUMENTS:
;       gt5_File:    A generic MonoRTM/LBLRTM TAPE5 file that will be
;                    modified inline based on the OSRF frequencies.
;                    UNITS:      N/A
;                    TYPE:       CHARACTER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORDS:
;       Debug:       Set this keyword for debugging. If set then:
;                    - the error handler for this function is disabled
;                      so that execution halts where the error occurs,
;                    - more verbose output is produced.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 26-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Apply_to_LBL, $
  gt5_File, $  ; Input
  Debug=Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Check object
  IF ( NOT self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Some or all OSRF components are not allocated', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Check input
  IF ( NOT Valid_String(gt5_File) ) THEN $
    MESSAGE, 'Must specify a generic TAPE5 filename', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  fInfo = FILE_INFO(gt5_File)
  IF ( NOT fInfo.EXISTS ) THEN $
    MESSAGE, 'Generic TAPE5 filename '+STRTRIM(gt5_File,2)+' not found', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Loop over bands
  self->Get_Property, Debug=Debug, n_Bands=n_Bands, Sensor_Type=Sensor_Type
  FOR i = 0, n_Bands-1 DO BEGIN

    ; Get some band-dependent oSRF properties
    Current_Band = i+1
    self->Get_Property, Current_Band, $
      Debug     = debug, $
      f1        = f1, $
      f2        = f2, $
      Frequency = frequency, $
      Response  = response

    ; Branch for sensor type    
    CASE 1 OF

      ; MW sensor - set up for MonoRTM
      (Sensor_Type EQ MICROWAVE_SENSOR): BEGIN
         ; Calculate monortm radiances
         frequency = inverse_cm_to_GHz(frequency)
         self.Radiance[Current_Band] = MonoRTM_Radiances(frequency, gt5_File)
         ; Convolve LBL radiances with OSRF
         self->Convolve_Radiance, Debug=Debug
         END
      
      ; IR/Vis sensor - set up for LBLRTM
      (Sensor_Type EQ INFRARED_SENSOR OR Sensor_Type EQ VISIBLE_SENSOR): BEGIN
         ; Calculate LBLRTM convolved radiance
         self.Convolved_R = LBLRTM_Convolved_Radiance(f1,f2,response,gt5_File,Debug=debug)
        END
      
      ; Invalid sensor type
      ELSE: MESSAGE, 'Invalid sensor type', NONAME=MsgSwitch, NOPRINT=MsgSwitch

    ENDCASE
  ENDFOR

  ; Convert the radiance into brightness temperature
  self->Convolved_R2T, Debug=Debug

END
