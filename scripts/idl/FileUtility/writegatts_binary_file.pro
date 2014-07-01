PRO WriteSingleGAtt, fileid, gattname, gattvalue

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...Define error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, "Error writing "+STRTRIM(gattname,2)+": "+!ERROR_STATE.MSG, /CONTINUE
    RETURN
  ENDIF

  ; Set up
  _gattvalue = ''
  IF ( Valid_String(gattvalue) ) THEN _gattvalue = STRTRIM(gattname,2)+": "+STRTRIM(gattvalue)
  gattlen = STRLEN(STRTRIM(_gattvalue,2))

  ; Write the string length
  WRITEU, fileid, gattlen
  IF ( gattlen EQ 0 ) THEN RETURN

  ; Write the attribute
  WRITEU, fileid, STRTRIM(_gattvalue,2)

END

;+
PRO WriteGAtts_Binary_File, $
  fileid                     , $  ; Input
  Write_Module = Write_Module, $  ; Optional input
  Created_On   = Created_On  , $  ; Optional input
  Title        = Title       , $  ; Optional input
  History      = History     , $  ; Optional input
  Comment      = Comment          ; Optional input
;-
  ; Setup
  COMPILE_OPT HIDDEN
  ; ...Define error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN
  ENDIF
  ; ...Global attribute names
  WRITE_MODULE_GATTNAME = 'write_module'
  CREATED_ON_GATTNAME   = 'created_on'
  TITLE_GATTNAME        = 'title'
  HISTORY_GATTNAME      = 'history'
  COMMENT_GATTNAME      = 'comment'

  ; Check some input
  IF ( Valid_String(Created_On) ) THEN $
    _created_on = STRTRIM(Created_On,2) $
  ELSE $
    _created_on = SYSTIME()

  ; Write all the attributes
  WriteSingleGatt, fileid, WRITE_MODULE_GATTNAME, Write_Module
  WriteSingleGatt, fileid, CREATED_ON_GATTNAME  , _created_on  
  WriteSingleGatt, fileid, TITLE_GATTNAME       , Title       
  WriteSingleGatt, fileid, HISTORY_GATTNAME     , History     
  WriteSingleGatt, fileid, COMMENT_GATTNAME     , Comment     

END
