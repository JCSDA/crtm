PRO File_List::Cleanup, $
  Debug=Debug
 
  ; Set up
  COMPILE_OPT HIDDEN
  ; ...Set up error handler
  @file_list_err_handler

  ; Empty container
  self->IDL_Container::Cleanup

  ; Done
  CATCH, /CANCEL

END
