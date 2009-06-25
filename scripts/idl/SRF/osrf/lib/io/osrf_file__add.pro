PRO OSRF_File::Add, $
  obj, $
  Debug=Debug
 
  ; Set up
  ; ...OSRF parameters
  @osrf_file_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler


  ; Add object to the container
  self->IDL_Container::Add, obj
  
  
  ; If the object added is an OSRF object, increment the channel counter
  IF ( OBJ_ISA(obj, 'OSRF') ) THEN self.n_Channels++


  ; Done
  CATCH, /CANCEL

END ; PRO OSRF_File::Add
