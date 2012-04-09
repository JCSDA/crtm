;+
FUNCTION RTS_Channel::Assign, $
  Debug=Debug  ; Input keyword
;-
  ; Set up
  @rts_func_err_handler


  ; Create a new object reference
  new = OBJ_NEW('RTS_Channel',Debug=Debug)


  ; Only perform the assignment if necessary
  IF ( self->Associated(Debug=Debug) ) THEN BEGIN

    ; Allocate the output object
    new->Create, self.n_Layers, Debug=Debug
    IF ( ~(new->Associated(Debug=Debug)) ) THEN $
      MESSAGE, 'Error allocating output RTS structure', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch

    ; Copy over data (this needs to be automated to allow for additions to the object)
    ; ...Scalars
    new.Sensor_Id               = self.Sensor_Id
    new.WMO_Satellite_Id        = self.WMO_Satellite_Id
    new.WMO_Sensor_Id           = self.WMO_Sensor_Id
    new.Sensor_Channel          = self.Sensor_Channel
    new.RT_Algorithm_Name       = self.RT_Algorithm_Name
    new.SOD                     = self.SOD
    new.Surface_Emissivity      = self.Surface_Emissivity
    new.Up_Radiance             = self.Up_Radiance
    new.Down_Radiance           = self.Down_Radiance
    new.Down_Solar_Radiance     = self.Down_Solar_Radiance
    new.Surface_Planck_Radiance = self.Surface_Planck_Radiance
    new.Radiance                = self.Radiance
    new.Brightness_Temperature  = self.Brightness_Temperature
    ; ...Pointers
    *new.Upwelling_Radiance  = *self.Upwelling_Radiance
    *new.Layer_Optical_Depth = *self.Layer_Optical_Depth

  ENDIF


  ; Return the new object reference
  RETURN, new

END
