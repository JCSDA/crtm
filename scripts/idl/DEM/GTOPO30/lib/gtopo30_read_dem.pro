PRO GTOPO30_Dem__Define

  void = { GTOPO30_Dem, $
           HDR: {GTOPO30_Hdr}, $
           ELEVATION: PTR_NEW(), $
           LATITUDE : PTR_NEW(), $
           LONGITUDE: PTR_NEW() }

END


PRO GTOPO30_Read_Dem, $
  Id, $ ; Input
  Dem   ; Output

  ; Error handler
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /INFORMATIONAL
    RETURN
  ENDIF
  
  ; First read the header file
  GTOPO30_Read_Hdr, Id, Hdr
  HELP, Hdr, /STRUCT
  
  ; Set the DEM endian flags
  IF ( Hdr.ByteOrder EQ 'M' ) THEN BEGIN
    ; File is Big-endian
    SWAP_IF_LITTLE_ENDIAN = 1
    SWAP_IF_BIG_ENDIAN    = 0
  ENDIF ELSE BEGIN
    ; File is Little-endian
    SWAP_IF_LITTLE_ENDIAN = 0
    SWAP_IF_BIG_ENDIAN    = 1
  ENDELSE
  
  ; Open the DEM file
  Filename = STRUPCASE(Id+'.DEM')
  OPENR, Fileid, Filename, /GET_LUN, $
    SWAP_IF_LITTLE_ENDIAN = SWAP_IF_LITTLE_ENDIAN, $
    SWAP_IF_BIG_ENDIAN    = SWAP_IF_BIG_ENDIAN
    

  ; Create the output structure
  Dem = {GTOPO30_Dem}
  Dem.Hdr = TEMPORARY(Hdr)
  
  
  ; Create arrays for data
  Dem.Elevation = PTR_NEW(INTARR(Dem.Hdr.ncols,Dem.Hdr.nrows))
  READU, Fileid, *Dem.Elevation


  ; Close the file
  FREE_LUN, Fileid


  ; Flip the data along the row dimension
  idx = REVERSE(LINDGEN(Dem.Hdr.nrows))
  *Dem.Elevation = (*Dem.Elevation)[*,idx]
  
  
END
