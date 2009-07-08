;+
; NAME:
;       AtmProfile_File::Init
;
; PURPOSE:
;       The AtmProfile_File::Init function method initialises an AtmProfile_File object.
;
;       NOTE: Init methods are special *lifecycle methods* and, as
;             such, cannot be called outside the context of object
;             creation. This means that in most cases you cannot call
;             the Init method directly. There is one exception to this
;             rule: if you write your own subclass of this class, you
;             can call the Init method from within the Init method of
;             the subclass.
;
; CALLING SEQUENCE:
;       Obj = OBJ_NEW( 'AtmProfile_File', [Filename,] [PROPERTY=value,] Debug=Debug )
;
;         or, in a lifecycle method,
;
;       Result = Obj->[AtmProfile::]Init( Filename, $ 
;                                   [PROPERTY=value, $]
;                                   Debug=Debug )
;
; OPTIONAL INPUT ARGUMENTS:
;       Filename:    Specify this argument to set the filename
;                    associated with the AtmProfile_File object. If not
;                    supplied, the default value is "atmprofile.nc"
;                    UNITS:      N/A
;                    TYPE:       CHARACTER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
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
;       PROPERTY:    Any of the group of keywords accepted by the
;                    AtmProfile_File::Set_Property procedure method.
;
; FUNCTION RESULT:
;       Result:      The return value is an integer defining the object
;                    initialisation status. The return codes are defined
;                    in the error_codes include file.
;                    If == TRUE the object creation was sucessful
;                       == FALSE an unrecoverable error occurred
;                    
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;
; INCLUDE FILES:
;       atmprofile_file parameters: Include file containing AtmProfile_File specific
;                                   parameter value definitions.
;
;       error_codes: Include file containing error code definitions.
;
; EXAMPLE:
;       The Init method is invoked when an AtmProfile object is created,
;
;         IDL> x = OBJ_NEW('AtmProfile_File', 'test.nc')
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 06-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION AtmProfile_File::Init, $
  Filename, $
  Debug  = Debug, $    ; Input keyword
  _EXTRA = Properties  ; Keywords passed onto AtmProfile_File::Set_Property

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...netCDF parameters
  @atmprofile_file_parameters
  ; ...Set up error handler
  @error_codes
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    MESSAGE, '--> Entered.', /INFORMATIONAL
    MsgSwitch = 0
  ENDIF ELSE BEGIN
    CATCH, Error_Status
    IF ( Error_Status NE 0 ) THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, !ERROR_STATE.MSG, /CONTINUE
      RETURN, FALSE
    ENDIF
    MsgSwitch = 1
  ENDELSE
  ; ..Check input
  IF ( Valid_String(Filename) ) THEN $
    _Filename = Filename $
  ELSE $
    _Filename = 'atmprofile.nc'
 

  ; Set default values
  self.filename = _Filename
  self.Release  = ATMPROFILE_RELEASE
  self.Version  = ATMPROFILE_VERSION
  
  self.Profile_Set_Id = ''
  self.Title   = ''
  self.History = ''
  self.Comment = ''


  ; Set supplied properties.
  self->Set_Property, _EXTRA = Properties
  
  
  
  CATCH, /CANCEL
  RETURN, TRUE
 
END ; FUNCTION AtmProfile_File::Init
