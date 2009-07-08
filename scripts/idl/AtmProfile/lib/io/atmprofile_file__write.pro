;+
; NAME:
;       AtmProfile_File::Write
;
; PURPOSE:
;       The AtmProfile_File::Write procedure method writes all the contained AtmProfile
;       objects to an AtmProfile_File
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile_File::]Write, $
;         Debug = Debug  ;  Input keyword
;
; INPUT KEYWORDS:
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       atmprofile_file parameters: Include file containing AtmProfile_File specific
;                                   parameter value definitions.
;
;       atmprofile_pro_err_handler: Error handler code for AtmProfile procedures.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 06-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

; Utility function to ensure maximum
; string lengths are not exceeded
FUNCTION Match_String, in_strarr, max_length
  n = N_ELEMENTS(in_strarr)
  out_bytarr = BYTARR(max_length,n)
  FOR j = 0L, n-1L DO BEGIN
    i = STRLEN(in_strarr[j]) < max_length
    out_bytarr[0:i-1,j] = (BYTE(in_strarr[j]))[0:i-1]
  ENDFOR
  RETURN, out_bytarr
END



PRO AtmProfile_File::Write, $
  Debug = Debug     ; Input keyword


  ; Set up
  ; ...netCDF parameters
  @atmprofile_file_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler
  ; ...Create byte arrays for character string variables
  
  
  ; Get the AtmProfile references and check count
  atmprofile = self->Get(/ALL, ISA='AtmProfile', COUNT=n_Profiles)
  IF ( n_Profiles EQ 0 ) THEN RETURN


  ; Create the file
  self->Create, Debug=Debug


  ; Open the file for writing 
  fid = NCDF_OPEN( self.filename, /WRITE )
  NCDF_CONTROL, fid, /VERBOSE
  
  
  ; Write the profile independent data
  ; ...Get the data properties
  atmprofile[0]->AtmProfile::Get_Property, $
    Debug = Debug, $
    Absorber_Id         = Absorber_Id, $
    Absorber_Units_Id   = Absorber_Units_Id, $
    Absorber_Units_Name = Absorber_Units_Name
  ; ...The Absorber_Id
  VarId = NCDF_VARID( fid, ABSORBER_ID_VARNAME )
  NCDF_VARPUT, fid, VarId, Absorber_Id
  ; ...The Absorber_Units_Id
  VarId = NCDF_VARID( fid, ABSORBER_UNITS_ID_VARNAME )
  NCDF_VARPUT, fid, VarId, Absorber_Units_Id
  ; ...The Absorber_Units_Name
  VarId = NCDF_VARID( fid, ABSORBER_UNITS_NAME_VARNAME )
  NCDF_VARPUT, fid, VarId, Match_String(Absorber_Units_Name, AUNSL)
  
  
  ; Loop over contained AtmProfile objects
  FOR m = 0L, n_Profiles-1L DO BEGIN
  
  
    ; Write the profile dependent data
    ; ...The Description
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Description = Description
    VarId = NCDF_VARID( fid, DESCRIPTION_VARNAME )
    NCDF_VARPUT, fid, VarId, Match_String(Description, PDSL), OFFSET = [0,m]
  
    ; ...The Climatology_Model
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Climatology_Model = Climatology_Model
    VarId = NCDF_VARID( fid, CLIMATOLOGY_MODEL_VARNAME )
    NCDF_VARPUT, fid, VarId, Climatology_Model, OFFSET = m
  
    ; ...The Year
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Year = Year
    VarId = NCDF_VARID( fid, YEAR_VARNAME )
    NCDF_VARPUT, fid, VarId, Year, OFFSET = m
  
    ; ...The Month
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Month = Month
    VarId = NCDF_VARID( fid, MONTH_VARNAME )
    NCDF_VARPUT, fid, VarId, Month, OFFSET = m
  
    ; ...The Day
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Day = Day
    VarId = NCDF_VARID( fid, DAY_VARNAME )
    NCDF_VARPUT, fid, VarId, Day, OFFSET = m
  
    ; ...The Hour
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Hour = Hour
    VarId = NCDF_VARID( fid, HOUR_VARNAME )
    NCDF_VARPUT, fid, VarId, Hour, OFFSET = m
  
    ; ...The Latitude
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Latitude = Latitude
    VarId = NCDF_VARID( fid, LATITUDE_VARNAME )
    NCDF_VARPUT, fid, VarId, Latitude, OFFSET = m
  
    ; ...The Longitude
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Longitude = Longitude
    VarId = NCDF_VARID( fid, LONGITUDE_VARNAME )
    NCDF_VARPUT, fid, VarId, Longitude, OFFSET = m
  
    ; ...The Surface_Altitude
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Surface_Altitude = Surface_Altitude
    VarId = NCDF_VARID( fid, SURFACE_ALTITUDE_VARNAME )
    NCDF_VARPUT, fid, VarId, Surface_Altitude, OFFSET = m
  
    ; ...The Level_Pressure
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Level_Pressure = Level_Pressure
    VarId = NCDF_VARID( fid, LEVEL_PRESSURE_VARNAME )
    NCDF_VARPUT, fid, VarId, Level_Pressure, OFFSET = [0,m]
  
    ; ...The Level_Temperature
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Level_Temperature = Level_Temperature
    VarId = NCDF_VARID( fid, LEVEL_TEMPERATURE_VARNAME )
    NCDF_VARPUT, fid, VarId, Level_Temperature, OFFSET = [0,m]
  
    ; ...The Level_Absorber
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Level_Absorber = Level_Absorber
    VarId = NCDF_VARID( fid, LEVEL_ABSORBER_VARNAME )
    NCDF_VARPUT, fid, VarId, Level_Absorber, OFFSET = [0,0,m]
  
    ; ...The Level_Altitude
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Level_Altitude = Level_Altitude
    VarId = NCDF_VARID( fid, LEVEL_ALTITUDE_VARNAME )
    NCDF_VARPUT, fid, VarId, Level_Altitude, OFFSET = [0,m]
  
    ; ...The Layer_Pressure
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Layer_Pressure = Layer_Pressure
    VarId = NCDF_VARID( fid, LAYER_PRESSURE_VARNAME )
    NCDF_VARPUT, fid, VarId, Layer_Pressure, OFFSET = [0,m]
  
    ; ...The Layer_Temperature
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Layer_Temperature = Layer_Temperature
    VarId = NCDF_VARID( fid, LAYER_TEMPERATURE_VARNAME )
    NCDF_VARPUT, fid, VarId, Layer_Temperature, OFFSET = [0,m]
  
    ; ...The Layer_Absorber
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Layer_Absorber = Layer_Absorber
    VarId = NCDF_VARID( fid, LAYER_ABSORBER_VARNAME )
    NCDF_VARPUT, fid, VarId, Layer_Absorber, OFFSET = [0,0,m]
  
    ; ...The Layer_Delta_Z
    atmprofile[m]->AtmProfile::Get_Property, $
      Debug = Debug, $
      Layer_Delta_Z = Layer_Delta_Z
    VarId = NCDF_VARID( fid, LAYER_DELTA_Z_VARNAME )
    NCDF_VARPUT, fid, VarId, Layer_Delta_Z, OFFSET = [0,m]

  ENDFOR
  NCDF_CLOSE, fid


  ; Write the global attributes
  self->Write_GAtts, Debug=Debug
  

  ; Done
  CATCH, /CANCEL

END ; PRO AtmProfile_File::Write
