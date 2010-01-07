FUNCTION RTSfile::Read, $
  Filename   , $ ; Input
  Debug=Debug    ; Input keyword

  ; Set up
  ; ...Set up error handler
  @rts_func_err_handler


  ; Open the file
  self.FileId = Open_Binary_File( Filename )
  IF ( self.FileID < 0 ) THEN $
    MESSAGE, 'Error opening file '+Filename, $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  
END ; RTSfile::Read
