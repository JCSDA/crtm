PRO netCDF::Read, $
  File         , $  ; Input
  Variables = variables, $  ; Input
  Quiet     = quiet    , $  ; Input
  Debug     = debug         ; Input keyword

  ; Set up
  @netcdf_pro_err_handler
  ; ...process keywords
  noisy = TRUE
  IF ( KEYWORD_SET(quiet) ) THEN noisy = FALSE

  ; Open the file
  id = NCDF_OPEN( File, /NOWRITE )
  IF ( id EQ -1 ) THEN $
    MESSAGE, 'Error opening file ' + File, $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  self.file = File
  self.id   = id

  ; Get file info
  info = NCDF_INQUIRE( self.id )
  ; ...store it
  self.info = HASH(info, /EXTRACT)
  ; ...display it
  IF ( noisy ) THEN BEGIN
    PRINT, FORMAT='(/10x,"Number of dimensions        : ",i7 )', self.info["NDIMS"]
    PRINT, FORMAT='( 10x,"Number of variables         : ",i7 )', self.info["NVARS"]
    PRINT, FORMAT='( 10x,"Number of global attributes : ",i7 )', self.info["NGATTS"]
    PRINT, FORMAT='( 10x,"ID of unlimited dimension   : ",i7 )', self.info["RECDIM"]
  ENDIF

  ; Get dimensions
  FOR i=0, self.info["NDIMS"]-1 DO BEGIN
    ; ...Get dimension info
    NCDF_DIMINQ, self.id, i, dimname, dimsize
    ; ...Add to list
    self.dim.add, HASH("NAME", dimname, "SIZE", dimsize)
  ENDFOR

  ; Get global attributes
  FOR i=0, self.info["NGATTS"]-1 DO BEGIN
    ; ...Get the name
    gattname = NCDF_ATTNAME( self.id, i, /GLOBAL )
    ; ...Get the value
    NCDF_ATTGET, self.id, gattname, gattvalue, /GLOBAL
    ; ...Get the info
    gattinfo = NCDF_ATTINQ( self.id, gattname, /GLOBAL )
    ; ...Convert byte arrays to strings
    IF ( gattinfo.DATATYPE EQ 'CHAR' ) THEN gattvalue = STRING(gattvalue)
    ; ...Add to list
    self.gatt.add, HASH("NAME", gattname, "VALUE", gattvalue) + gattinfo
  ENDFOR


  ; Determine how many variables to get
  ; ...First get a list of ALL the variable names
  all_variables = STRARR(self.info["NVARS"])
  FOR i=0, self.info["NVARS"]-1 DO BEGIN
    all_variables[i] = STRUPCASE((NCDF_VARINQ(self.id,i)).NAME)
  ENDFOR
  ; ...Now get the USER variables
  IF ( N_ELEMENTS(variables) GT 0 ) THEN BEGIN
    ; ...specified via the VARIABLES keyword
    _variables = STRUPCASE(variables)
    _variables = _variables[UNIQ(_variables, SORT(_variables))]
    user_variables = []
    id_variables = []
    FOR i=0, N_ELEMENTS(_variables)-1 DO BEGIN
      id = WHERE(_variables[i] EQ all_variables, count)
      IF ( count EQ 0 ) THEN CONTINUE
      user_variables = [user_variables, _variables[i]]
      id_variables = [id_variables, id[0]]
    ENDFOR
  ENDIF ELSE BEGIN
    ; ...default is ALL variables
    user_variables = all_variables
    id_variables = LINDGEN(self.info["NVARS"])
  ENDELSE
  n_variables = N_ELEMENTS(user_variables)


  ; Get the variables
  FOR i=0, n_variables-1 DO BEGIN
    ; ...Get variable info
    varid = id_variables[i]
    varinfo = NCDF_VARINQ(self.id, varid)
    ; ...Get variable attributes
    varatt = LIST()
    FOR j=0, varinfo.NATTS-1 DO BEGIN
      ; ...Get the name
      attname = NCDF_ATTNAME( self.id, varid, j )
      ; ...Get the value
      NCDF_ATTGET, self.id, varid, attname, attvalue
      ; ...Get the info
      attinfo = NCDF_ATTINQ( self.id, varid, attname )
      ; ...Convert byte arrays to strings
      IF ( attinfo.DATATYPE EQ 'CHAR' ) THEN attvalue = STRING(attvalue)
      ; ...Add to list
      varatt.add, HASH("NAME", attname, "VALUE", attvalue) + attinfo   
    ENDFOR
    ; ...Get variable data
    NCDF_VARGET, self.id, varid, value
    ; ...Add to list
    self.data.add, HASH("VALUE", value, "ATT", varatt, "ID", varid) + varinfo
  ENDFOR

  ; Close the netCDF data file
  NCDF_CLOSE, self.id

END
