;+
; NAME:
;       AtmProfile_File::Get
;
; PURPOSE:
;       The AtmProfile_File::Get function method returns object references to
;       objects in a AtmProfile_File container.
;
;       This method overrides the IDL_Container::Get function method.
;
; CALLING SEQUENCE:
;       Result = Obj->[AtmProfile_File::]Get( $
;         Debug   = Debug  , $  ; Input keyword
;         Profile = Profile, $  ; Input keyword
;         Count   = Count    )  ; Output keyword
;
; KEYWORDS:
;       Any keywords to the IDL_Container::Get method can be used.
;       In addition, the following keywords are available:
;
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Profile:     Set this keyword to a profile number or array of profile
;                    numbers for which the AtmProfile object reference(s) are to be
;                    retrieved.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar or Rank-1
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       
;       Count:       Set this keyword equal to a named variable that will
;                    contain the number of objects selected by the function.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(OUT), OPTIONAL
;
; FUNCTION RESULT:
;       Result:      The return value is a scalar or array of object references
;                    to AtmProfile objects in the AtmProfile_File container. If no objects
;                    are found in the container, the Get function returns -1.
;                    UNITS:      N/A
;                    TYPE:       Object
;                    DIMENSION:  Scalar or Rank-1
;
; INCLUDE FILES:
;       atmprofile_file parameters: Include file containing AtmProfile_File specific
;                                   parameter value definitions.
;
;       atmprofile_func_err_handler: Error handler code for AtmProfile procedures.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 06-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION AtmProfile_File::Get, $
  Debug      = Debug  , $  ; Input keyword
  Profile    = Profile, $  ; Input keyword
  Count      = Count  , $  ; Output keyword
  _REF_EXTRA = Extra       ; Keywords passed onto IDL_Container::Get
 
  ; Set up
  ; ...File parameters
  @atmprofile_file_parameters
  ; ...Set up error handler
  @atmprofile_func_err_handler


  ; Get object reference from container
  IF ( N_ELEMENTS(Profile) GT 0 ) THEN BEGIN
    ; Initialise return object reference
    objref = -1L
    ; Get references for all AtmProfile objects in list
    atmprofile = self->IDL_Container::Get(/ALL, ISA='AtmProfile', COUNT=n_Profiles)
    ; Loop over all AtmProfile objects to build the profile list
    Profile_List = LONARR(n_Profiles)
    FOR i = 0L, n_Profiles-1L DO BEGIN
      atmprofile[i]->Get_Property, Profile=pr
      Profile_List[i] = pr
    ENDFOR
    ; Pick out the matching profiles
    ; ...Initialise return count and array
    Count = 0L
    n_Profiles = N_ELEMENTS(Profile)
    objref = OBJARR(n_Profiles)
    ; ...Loop over requested profiles
    FOR i = 0L, n_Profiles-1 DO BEGIN
      loc = WHERE(Profile_List EQ Profile[i], n )
      IF ( n GT 0 ) THEN BEGIN
        objref[i] = atmprofile[loc]
        Count++
      ENDIF
    ENDFOR
    objref = objref[0:Count-1]
    
  ENDIF ELSE BEGIN
  
    ; Just get the requested object reference
    objref = self->IDL_Container::Get(COUNT = Count, _EXTRA = Extra)
  
  ENDELSE
  

  ; Done
  CATCH, /CANCEL
  RETURN, objref

END ; FUNCTION AtmProfile_File::Get
