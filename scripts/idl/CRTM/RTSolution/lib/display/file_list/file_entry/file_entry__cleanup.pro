PRO File_Entry::Cleanup, $
  Debug=Debug
 
  ; Set up
  COMPILE_OPT HIDDEN
  ; ...Set up error handler
  @file_list_err_handler

  ; ...For future use...

  ; Done
  CATCH, /CANCEL

END
