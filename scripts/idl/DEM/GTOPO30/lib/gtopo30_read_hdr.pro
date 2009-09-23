PRO GTOPO30_Hdr__Define

  void = { GTOPO30_HDR, $
           BYTEORDER    :'', $
           LAYOUT       :'', $
           NROWS        : 0L, $
           NCOLS        : 0L, $
           NBANDS       : 0L, $
           NBITS        : 0L, $
           BANDROWBYTES : 0L, $
           TOTALROWBYTES: 0L, $
           BANDGAPBYTES : 0L, $
           NODATA       : 0L, $
           ULXMAP       : 0.0d0, $
           ULYMAP       : 0.0d0, $
           XDIM         : 0.0d0, $
           YDIM         : 0.0d0 }

END


PRO GTOPO30_Read_Hdr, $
  Id, $ ; Input
  Hdr   ; Output

  ; Error handler
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /INFORMATIONAL
    RETURN
  ENDIF
  
  ; Create a new header structure
  Hdr      = {GTOPO30_HDR}
  Hdr_Tags = TAG_NAMES(Hdr)
  
  ; Open the file
  Filename = STRUPCASE(Id+'.hdr')
  OPENR, Fileid, Filename, /GET_LUN
  
  ; Read the header data
  Buffer = ''
  WHILE NOT EOF(Fileid) DO BEGIN
    ; ...Read a line and split it by whitespace
    READF, Fileid, Buffer
    Data = STRSPLIT( Buffer, " ", /EXTRACT )
    ; ...Slot data into the structure
    idx = WHERE( Hdr_Tags EQ Data[0], count )
    IF ( Count EQ 0 ) THEN CONTINUE ELSE Idx=Idx[0]
    Hdr.(idx) = FIX(Data[1],TYPE=SIZE(Hdr.(idx),/TYPE))
  ENDWHILE
  
  ; Close the file
  FREE_LUN, Fileid

END
