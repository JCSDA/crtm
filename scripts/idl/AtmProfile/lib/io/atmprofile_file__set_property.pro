;
; NAME:
;       AtmProfile_File::Set_Property
;
; PURPOSE:
;       The AtmProfile_File::Set_Property procedure method sets the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile_File::]Set_Property, $
;         Debug          = Debug         , $  ; Input keyword
;         Version        = Version       , $  ; Input keyword
;         Profile_Set_Id = Profile_Set_Id, $  ; Input keyword
;         Title          = Title         , $  ; Input keyword
;         History        = History       , $  ; Input keyword
;         Comment        = Comment         )  ; Input keyword
;
; INPUT KEYWORDS:
;       Debug:              Set this keyword for debugging.
;                           If NOT SET => Error handler is enabled. (DEFAULT)
;                              SET     => Error handler is disabled; Routine
;                                         traceback output is enabled.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Version:            The version number of the netCDF AtmProfile file.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Profile_Set_Id:     Character string identifying the profile set.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Title:              Character string written into the TITLE global
;                           attribute field of the netCDF AtmProfile file.
;                           Should contain a succinct description of what
;                           is in the netCDF datafile.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       History:            Character string written into the HISTORY global
;                           attribute field of the netCDF AtmProfile file.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Comment:            Character string written into the COMMENT global
;                           attribute field of the netCDF AtmProfile file.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
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

PRO AtmProfile_File::Set_Property, $
  Debug          = Debug         , $
  Version        = Version       , $
  Profile_Set_Id = Profile_Set_Id, $
  Title          = Title         , $
  History        = History       , $
  Comment        = Comment         


  ; Set up
  ; ...AtmProfile_File parameters
  @atmprofile_file_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler
 
  
  ; Set data
  IF ( N_ELEMENTS(Version) GT 0 ) THEN self.Version = Version

  IF ( Valid_String(Profile_Set_Id) ) THEN self.Profile_Set_Id = Profile_Set_Id       
  IF ( Valid_String(Title         ) ) THEN self.Title          = Title         
  IF ( Valid_String(History       ) ) THEN self.History        = History       
  IF ( Valid_String(Comment       ) ) THEN self.Comment        = Comment       
 
 
  ; Done
  CATCH, /CANCEL
 
END ; PRO AtmProfile_File::Set_Property
