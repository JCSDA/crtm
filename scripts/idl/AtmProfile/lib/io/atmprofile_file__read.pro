;+
; NAME:
;       AtmProfile_File::Read
;
; PURPOSE:
;       The AtmProfile_File::Read procedure method reads an AtmProfile_File and fills the
;       container with all the AtmProfile objects in the file.
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile_File::]Read, $
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

PRO AtmProfile_File::Read, $
  Debug = Debug     ; Input keyword


  ; Set up
  ; ...netCDF parameters
  @atmprofile_file_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler
  
  
  ; Check the file exists
  fInfo = FILE_INFO(self.filename)
  IF ( NOT fInfo.EXISTS ) THEN $
    MESSAGE, self.filename+' not found', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Open the file for reading
  fid = NCDF_OPEN( self.filename, /NOWRITE )
  NCDF_CONTROL, fid, /VERBOSE
  
  
  ; Get the profile dimensions
  DimID = NCDF_DIMID( fid, LEVEL_DIMNAME )
  NCDF_DIMINQ, fid, DimID, DimName, n_Levels
  DimID = NCDF_DIMID( fid, LAYER_DIMNAME )
  NCDF_DIMINQ, fid, DimID, DimName, n_Layers
  DimID = NCDF_DIMID( fid, ABSORBER_DIMNAME )
  NCDF_DIMINQ, fid, DimID, DimName, n_Absorbers
  DimID = NCDF_DIMID( fid, PROFILE_DIMNAME )
  NCDF_DIMINQ, fid, DimID, DimName, n_Profiles

  
  ; Read the global attributes
  self->Read_GAtts, Debug = Debug
  
  
  ; Loop over the number of profiles
  FOR m = 0L, n_Profiles-1L DO BEGIN
  

    ; Allocate the current AtmProfile
    p = OBJ_NEW('AtmProfile', Debug = Debug)
    p->AtmProfile::Allocate, $
      n_Layers, $
      n_Absorbers, $
      Debug = Debug


    ; Set the profile number
    p->AtmProfile::Set_Property, $
      Profile = m+1
      
    
    ; Read the current profile's data
    ; ...The Description
    VarId = NCDF_VARID( fid, DESCRIPTION_VARNAME )
    NCDF_VARGET, fid, VarId, Description, OFFSET = [0,m], COUNT = [PDSL,1]
    p->AtmProfile::Set_Property, $
      Debug = Debug, $
      Description = STRING(Description)
  
    ; ...The Climatology_Model
    VarId = NCDF_VARID( fid, CLIMATOLOGY_MODEL_VARNAME )
    NCDF_VARGET, fid, VarId, Climatology_Model, OFFSET = m, COUNT = 1
    p->AtmProfile::Set_Property, $
      Debug = Debug, $
      Climatology_Model = Climatology_Model
  
;    ; ...The Year
;    VarId = NCDF_VARID( fid, YEAR_VARNAME )
;    NCDF_VARGET, fid, VarId, Year, OFFSET = m, COUNT = 1
;    p->AtmProfile::Set_Property, $
;      Debug = Debug, $
;      Year = Year
;  
;    ; ...The Month
;    VarId = NCDF_VARID( fid, MONTH_VARNAME )
;    NCDF_VARGET, fid, VarId, Month, OFFSET = m, COUNT = 1
;    p->AtmProfile::Set_Property, $
;      Debug = Debug, $
;      Month = Month
;  
;    ; ...The Day
;    VarId = NCDF_VARID( fid, DAY_VARNAME )
;    NCDF_VARGET, fid, VarId, Day, OFFSET = m, COUNT = 1
;    p->AtmProfile::Set_Property, $
;      Debug = Debug, $
;      Day = Day
;  
;    ; ...The Hour
;    VarId = NCDF_VARID( fid, HOUR_VARNAME )
;    NCDF_VARGET, fid, VarId, Hour, OFFSET = m, COUNT = 1
;    p->AtmProfile::Set_Property, $
;      Debug = Debug, $
;      Hour = Hour
  
    ; ...The Latitude
    VarId = NCDF_VARID( fid, LATITUDE_VARNAME )
    NCDF_VARGET, fid, VarId, Latitude, OFFSET = m, COUNT = 1
    p->AtmProfile::Set_Property, $
      Debug = Debug, $
      Latitude = Latitude
  
    ; ...The Longitude
    VarId = NCDF_VARID( fid, LONGITUDE_VARNAME )
    NCDF_VARGET, fid, VarId, Longitude, OFFSET = m, COUNT = 1
    p->AtmProfile::Set_Property, $
      Debug = Debug, $
      Longitude = Longitude
  
    ; ...The Surface_Altitude
    VarId = NCDF_VARID( fid, SURFACE_ALTITUDE_VARNAME )
    NCDF_VARGET, fid, VarId, Surface_Altitude, OFFSET = m, COUNT = 1
    p->AtmProfile::Set_Property, $
      Debug = Debug, $
      Surface_Altitude = Surface_Altitude
  
    ; ...The Absorber_Id
    VarId = NCDF_VARID( fid, ABSORBER_ID_VARNAME )
    NCDF_VARGET, fid, VarId, Absorber_Id
    p->AtmProfile::Set_Property, $
      Debug = Debug, $
      Absorber_Id = Absorber_Id
  
    ; ...The Absorber_Units_Id
    VarId = NCDF_VARID( fid, ABSORBER_UNITS_ID_VARNAME )
    NCDF_VARGET, fid, VarId, Absorber_Units_Id
    p->AtmProfile::Set_Property, $
      Debug = Debug, $
      Absorber_Units_Id = Absorber_Units_Id
  
    ; ...The Surface_Altitude
    VarId = NCDF_VARID( fid, SURFACE_ALTITUDE_VARNAME )
    NCDF_VARGET, fid, VarId, Surface_Altitude, OFFSET = m, COUNT = 1
    p->AtmProfile::Set_Property, $
      Debug = Debug, $
      Surface_Altitude = Surface_Altitude
  
    ; ...The Level_Pressure
    VarId = NCDF_VARID( fid, LEVEL_PRESSURE_VARNAME )
    NCDF_VARGET, fid, VarId, Level_Pressure, OFFSET = [0,m], COUNT = [n_Levels,1]
    p->AtmProfile::Set_Property, $
      Debug = Debug, $
      Level_Pressure = Level_Pressure
  
    ; ...The Level_Temperature
    VarId = NCDF_VARID( fid, LEVEL_TEMPERATURE_VARNAME )
    NCDF_VARGET, fid, VarId, Level_Temperature, OFFSET = [0,m], COUNT = [n_Levels,1]
    p->AtmProfile::Set_Property, $
      Debug = Debug, $
      Level_Temperature = Level_Temperature
  
    ; ...The Level_Absorber
    VarId = NCDF_VARID( fid, LEVEL_ABSORBER_VARNAME )
    NCDF_VARGET, fid, VarId, Level_Absorber, OFFSET = [0,0,m], COUNT = [n_Levels,n_Absorbers,1]
    p->AtmProfile::Set_Property, $
      Debug = Debug, $
      Level_Absorber = Level_Absorber
  
    ; ...The Level_Altitude
    VarId = NCDF_VARID( fid, LEVEL_ALTITUDE_VARNAME )
    NCDF_VARGET, fid, VarId, Level_Altitude, OFFSET = [0,m], COUNT = [n_Levels,1]
    p->AtmProfile::Set_Property, $
      Debug = Debug, $
      Level_Altitude = Level_Altitude
  
    ; ...The Layer_Pressure
    VarId = NCDF_VARID( fid, LAYER_PRESSURE_VARNAME )
    NCDF_VARGET, fid, VarId, Layer_Pressure, OFFSET = [0,m], COUNT = [n_Layers,1]
    p->AtmProfile::Set_Property, $
      Debug = Debug, $
      Layer_Pressure = Layer_Pressure
  
    ; ...The Layer_Temperature
    VarId = NCDF_VARID( fid, LAYER_TEMPERATURE_VARNAME )
    NCDF_VARGET, fid, VarId, Layer_Temperature, OFFSET = [0,m], COUNT = [n_Layers,1]
    p->AtmProfile::Set_Property, $
      Debug = Debug, $
      Layer_Temperature = Layer_Temperature
  
    ; ...The Layer_Absorber
    VarId = NCDF_VARID( fid, LAYER_ABSORBER_VARNAME )
    NCDF_VARGET, fid, VarId, Layer_Absorber, OFFSET = [0,0,m], COUNT = [n_Layers,n_Absorbers,1]
    p->AtmProfile::Set_Property, $
      Debug = Debug, $
      Layer_Absorber = Layer_Absorber
  
    ; ...The Layer_Delta_Z
    VarId = NCDF_VARID( fid, LAYER_DELTA_Z_VARNAME )
    NCDF_VARGET, fid, VarId, Layer_Delta_Z, OFFSET = [0,m], COUNT = [n_Layers,1]
    p->AtmProfile::Set_Property, $
      Debug = Debug, $
      Layer_Delta_Z = Layer_Delta_Z

  
    ; Add the current AtmProfile to the file object
    self->Add, p


  ENDFOR
  NCDF_CLOSE, fid


  ; Done
  CATCH, /CANCEL

END ; PRO AtmProfile_File::Read
