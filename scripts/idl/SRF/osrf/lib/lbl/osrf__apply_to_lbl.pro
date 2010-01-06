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
    self->Get_Property, Current_Band, Debug=Debug, Frequency=Frequency

    ; Branch for sensor type    
    CASE 1 OF

      ; MW sensor - set up for MonoRTM
      (Sensor_Type EQ MICROWAVE_SENSOR): BEGIN
         ; Calculate monortm radiances
         *(*self.Radiance)[i] = MonoRTM_Radiances(Frequency, gt5_File)
         END

      
      ; IR/Vis sensor - set up for LBLRTM
      (Sensor_Type EQ INFRARED_SENSOR OR Sensor_Type EQ VISIBLE_SENSOR): BEGIN

        ; Create LBLRTM input file for current band
        
        ; Spawn run of LBLRTM for current band
        
        ; Read LBLRTM output into OSRF self.Radiance
    
        END
      
      ; Invalid sensor type
      ELSE: BEGIN
      
        END

    ENDCASE
  ENDFOR

  ; Convolve LBL radiances with OSRF
  R = self->Convolve(*self.Radiance)

  ; Convert the radiance into brightness temperature
  self->Get_Property, Debug=Debug, f0=f0
  IF ( self->Flag_Is_Set(FREQUENCY_UNITS_FLAG) ) THEN f0 = GHz_to_inverse_cm(f0)
  result = Planck_Temperature(f0, R, T)

  ; Assign convolved data to the osrf object
  self->Set_Property, Debug=Debug, Convolved_R = R, Convolved_T = T
 
  ; Done
  CATCH, /CANCEL
 
END ; PRO OSRF::Apply_to_LBL
