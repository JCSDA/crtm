;+
; NAME:
;       AtmProfile_File::Get_Property
;
; PURPOSE:
;       The AtmProfile_File::Get_Property procedure method gets the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile_File::]Get_Property, $
;         Debug          = Debug         , $  ; Input keyword
;         n_Levels       = n_Levels      , $  ; Output keyword
;         n_Layers       = n_Layers      , $  ; Output keyword
;         n_Absorbers    = n_Absorbers   , $  ; Output keyword
;         n_Profiles     = n_Profiles    , $  ; Output keyword
;         Version        = Version       , $  ; Output keyword
;         Profile_Set_Id = Profile_Set_Id, $  ; Output keyword
;         Title          = Title         , $  ; Output keyword
;         History        = History       , $  ; Output keyword
;         Comment        = Comment            ; Output keyword
;
; INPUT KEYWORDS:
;       Debug:              Set this keyword for debugging.
;                           If NOT SET => Regular output. (DEFAULT)
;                              SET     => Information about all currently compiled
;                                         routines and their arguments are output.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
; OUTPUT KEYWORDS:
;       n_Levels:           The number of levels dimension of the
;                           AtmProfile data.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Layers:           The number of layers dimension of the
;                           AtmProfile data.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Absorbers:        The number of absorbers dimension of the
;                           AtmProfile data.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Profiles:         The number of profiles dimension of the
;                           AtmProfile data.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Version:            The version number of the netCDF AtmProfile file.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Profile_Set_Id:     Character string identifying the profile set.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Title:              Character string written into the TITLE global
;                           attribute field of the netCDF AtmProfile file.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       History:            Character string written into the HISTORY global
;                           attribute field of the netCDF AtmProfile file.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Comment:            Character string written into the COMMENT global
;                           attribute field of the netCDF AtmProfile file.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
; INCLUDE FILES:
;       atmprofile_file parameters: Include file containing AtmProfile_File specific
;                                   parameter value definitions.
;
;       atmprofile_pro_err_handler: Error handler code for AtmProfile procedures.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 06-Jul-2009
;                       paul.vandelst@noaa.gov
;-

PRO AtmProfile_File::Get_Property, $
  Debug          = Debug         , $  ; Input keyword
  n_Levels       = n_Levels      , $  ; Output keyword
  n_Layers       = n_Layers      , $  ; Output keyword
  n_Absorbers    = n_Absorbers   , $  ; Output keyword
  n_Profiles     = n_Profiles    , $  ; Output keyword
  Version        = Version       , $  ; Output keyword
  Profile_Set_Id = Profile_Set_Id, $  ; Output keyword
  Title          = Title         , $  ; Output keyword
  History        = History       , $  ; Output keyword
  Comment        = Comment            ; Output keyword

  ; Set up
  ; ...netCDF parameters
  @atmprofile_file_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler


  ; Get data
  IF ( ARG_PRESENT(n_Levels      ) ) THEN n_Levels       = self.n_Levels   
  IF ( ARG_PRESENT(n_Layers      ) ) THEN n_Layers       = self.n_Layers   
  IF ( ARG_PRESENT(n_Absorbers   ) ) THEN n_Absorbers    = self.n_Absorbers
  IF ( ARG_PRESENT(n_Profiles    ) ) THEN n_Profiles     = self.n_Profiles
  IF ( ARG_PRESENT(Version       ) ) THEN Version        = self.Version         
  IF ( ARG_PRESENT(Profile_Set_Id) ) THEN Profile_Set_Id = self.Profile_Set_Id
  IF ( ARG_PRESENT(Title         ) ) THEN Title          = self.Title    
  IF ( ARG_PRESENT(History       ) ) THEN History        = self.History  
  IF ( ARG_PRESENT(Comment       ) ) THEN Comment        = self.Comment


  ; Done
  CATCH, /CANCEL

END ; PRO AtmProfile_File::Get_Property
