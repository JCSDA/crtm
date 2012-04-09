PRO ReadSingleGAtt, fileid, gattname, gattvalue

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...Define error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, "Error reading "+STRTRIM(gattname,2)+": "+!ERROR_STATE.MSG, /CONTINUE
    RETURN
  ENDIF

  ; Set up
  gattvalue = ''

  ; Read the string length
  gattlen = 0L
  READU, fileid, gattlen
  IF ( gattlen EQ 0 ) THEN RETURN

  ; Read the attribute
  gattvalue = BYTARR(gattlen)
  READU, fileid, gattvalue
  gattvalue = STRING(gattvalue)

  ; Strip out the attribute name
  gattvalue = STRMID(gattvalue, STRPOS(gattvalue,":"))

END

;+
PRO ReadGAtts_Binary_File, $
  fileid                     , $  ; Input
  Write_Module = Write_Module, $  ; Optional output
  Created_On   = Created_On  , $  ; Optional output
  Title        = Title       , $  ; Optional output
  History      = History     , $  ; Optional output
  Comment      = Comment          ; Optional output
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
  
  ; Read all the attributes
  ReadSingleGatt, fileid, WRITE_MODULE_GATTNAME, Write_Module
  ReadSingleGatt, fileid, CREATED_ON_GATTNAME  , Created_On  
  ReadSingleGatt, fileid, TITLE_GATTNAME       , Title       
  ReadSingleGatt, fileid, HISTORY_GATTNAME     , History     
  ReadSingleGatt, fileid, COMMENT_GATTNAME     , Comment     

END
