;+
; NAME:
;       AtmProfile_File::Add
;
; PURPOSE:
;       The AtmProfile_File::Add procedure method adds an object references to
;       the AtmProfile_File container.
;
;       This method overrides the IDL_Container::Add function method.
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile_File::]Add, $
;         Obj_to_Add, $  ; Input
;         Debug = Debug  ; Input keyword
;
; INPUT ARGUMENTS:
;         Obj_to_Add: Object to add to the AtmProfile_File container, Obj.
;                     UNITS:      N/A
;                     TYPE:       Object
;                     DIMENSION:  Scalar
;                     ATTRIBUTES: INTENT(IN), OPTIONAL
;
; KEYWORDS:
;       Any keywords to the IDL_Container::Add method can be used.
;       In addition, the following keywords are available:
;
;       Debug:        Set this keyword for debugging.
;                     If NOT SET => Error handler is enabled. (DEFAULT)
;                        SET     => Error handler is disabled; Routine
;                                   traceback output is enabled.
;                     UNITS:      N/A
;                     TYPE:       INTEGER
;                     DIMENSION:  Scalar
;                     ATTRIBUTES: INTENT(IN), OPTIONAL
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

PRO AtmProfile_File::Add, $
  obj, $
  Debug      = Debug, $  ; Input keyword
  _REF_EXTRA = Extra     ; Keywords passed onto IDL_Container::Add
 
  ; Set up
  ; ...AtmProfile parameters
  @atmprofile_file_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler


  ; Check dimensions
  IF ( OBJ_ISA(obj, 'AtmProfile') ) THEN BEGIN

    ; Get the AtmProfile dimensions
    obj->Get_Property, $
      Debug       = Debug      , $
      n_Levels    = n_Levels   , $
      n_Layers    = n_Layers   , $
      n_Absorbers = n_Absorbers

    ; Check or set dimensions as necessary
    IF ( self.n_Profiles GT 0 ) THEN BEGIN
      ; ...Check dimensions
      IF ( self.n_Levels    NE n_Levels    OR  $
           self.n_Layers    NE n_Layers    OR  $
           self.n_Absorbers NE n_Absorbers     ) THEN $
        MESSAGE, 'AtmProfile and AtmProfile_File object dimensions are different!', $
                 NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ENDIF ELSE BEGIN
      ; ...Set dimensions
      self.n_Levels    = n_Levels   
      self.n_Layers    = n_Layers   
      self.n_Absorbers = n_Absorbers
    ENDELSE

    ; Dimensions agree, so add the AtmProfile object
    self->IDL_Container::Add, obj, _EXTRA = Extra
    self.n_Profiles++

  ENDIF ELSE BEGIN
  
    ; Add generic object object to the container
    self->IDL_Container::Add, obj, _EXTRA = Extra

  ENDELSE


  ; Done
  CATCH, /CANCEL

END ; PRO AtmProfile_File::Add
