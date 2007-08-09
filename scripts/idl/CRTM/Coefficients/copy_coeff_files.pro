;+
;
; NAME:
;       Copy_Coeff_Files
;
; PURPOSE:
;       Function to copy SpcCoeff and TauCoeff netCDF format files, replacing the
;       various satellite and sensor id values.
;
; CALLING SEQUENCE:
;       Error_Status = Copy_Coeff_Files( Old_Sensor_ID   , $  ; Input
;                                        Sensor_ID       , $  ; Input
;                                        WMO_Satellite_ID, $  ; Input 
;                                        NCEP_SENSOR_ID=NCEP_Sensor_ID, $  ; Optional input
;                                        FORCE         =Force           )  ; Optional input
;
; INPUTS:
;       Old_Sensor_ID:     Character string identifying the sensor FROM which
;                          the SpcCoeff and TauCoeff files are to be copied.
;                          UNITS:      N/A
;                          TYPE:       CHARACTER(*)
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT(IN)
;
;       Sensor_ID:         Character string identifying the sensor TO which
;                          the SpcCoeff and TauCoeff files are to be copied.
;                          UNITS:      N/A
;                          TYPE:       CHARACTER(*)
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT(IN)
;      
;       WMO_Satellite_ID:  Integer specifying the WMO satellite identifier
;                          for the new platform. Note that the WMO sensor
;                          identifier remains the same in the new file.
;                          UNITS:      N/A
;                          TYPE:       INTEGER
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT(IN)
;      
; INPUT KEYWORD PARAMETERS:
;       NCEP_Sensor_ID:    Integer specifying the NCEP sensor identifier
;                          for the new platform. This quantity is being
;                          phased out. Default value is -1.
;                          UNITS:      N/A
;                          TYPE:       INTEGER
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT(IN)
;      
;       Force:             Set this keyword to force creation of the new coefficient
;                          files if they already exist. Use with caution as this will
;                          overwrite any existing destination files.
;                          Default is to NOT overwrite existing files.
;                          UNITS:      N/A
;                          TYPE:       INTEGER
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT(IN)
;      
; FUNCTION RESULT:
;       Error_Status:      The return value is an integer defining the error status.
;                          The error codes are defined in the error_codes.pro file.
;                          If == SUCCESS the copy was sucessful
;                             == FAILURE an unrecoverable error occurred
;                          UNITS:      N/A
;                          TYPE:       INTEGER
;                          DIMENSION:  Scalar
;
; SIDE EFFECTS:
;       Existing destination files are overwritten if the FORCE keyword is set.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 09-Aug-2007
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION Empty_String
  RETURN, STRING(FORMAT='(a20)',' ')
END

FUNCTION File_Name, Sensor_Id, File_Type
  RETURN, STRTRIM(Sensor_Id,2)  + '.' + STRTRIM(File_Type,2) + '.nc'
END


FUNCTION Copy_File, In_Sensor_Id    , $  ; Input 
                    Out_Sensor_Id   , $  ; Input 
                    File_Type       , $  ; Input
                    WMO_Satellite_ID, $  ; Input 
                    NCEP_Sensor_ID       ; Optional input

  ; Set up
  ; ------ 
  ; Error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    


  ; Construct the filenames
  ; -----------------------
  Infile  = File_Name(In_Sensor_Id ,File_Type)
  Outfile = File_Name(Out_Sensor_Id,File_Type)
  
  
  ; Copy the input file
  ; -------------------
  FILE_COPY, Infile, Outfile, /OVERWRITE  

  
  ; Open the output file
  ; --------------------
  ncid = NCDF_OPEN( Outfile, /WRITE )
  
  
  ; Get the dimension data
  ; ----------------------
  IF ( File_Type EQ 'SpcCoeff' ) THEN $
    Channel_dimid = NCDF_DIMID(ncid,'n_channels') $
  ELSE $
    Channel_dimid = NCDF_DIMID(ncid,'n_Channels')
  NCDF_DIMINQ, ncid, Channel_dimid, name, n_Channels


  ; Get the various variables ids
  ; -----------------------------
  Sensor_Descriptor_varid = NCDF_VARID(ncid,'Sensor_Descriptor')
  WMO_Satellite_ID_varid  = NCDF_VARID(ncid,'WMO_Satellite_ID')
  NCEP_Sensor_ID_varid    = NCDF_VARID(ncid,'NCEP_Sensor_ID')


  ; Replace the sensor descriptor data
  ; ----------------------------------
  sd = Empty_String()
  STRPUT, sd, STRTRIM(Out_Sensor_Id,2)
  Sensor_Descriptor = REPLICATE( sd, n_Channels )
  NCDF_VARPUT, ncid, Sensor_Descriptor_varid, Sensor_Descriptor
  
  
  ; Replace the satellite id
  ; ------------------------
  id = REPLICATE( LONG(WMO_Satellite_ID), n_Channels )
  NCDF_VARPUT, ncid, WMO_Satellite_ID_varid, id
  
  
  ; Replace the NCEP id
  ; -------------------
  id = REPLICATE( LONG(NCEP_Sensor_ID), n_Channels )
  NCDF_VARPUT, ncid, NCEP_Sensor_ID_varid, id
  

  ; Alter the global attributes
  ; ---------------------------  
  NCDF_CONTROL, ncid, /REDEF

  ; Add to the history GAtt
  NCDF_ATTGET, ncid, 'history', Hisotry, /GLOBAL
  Hisotry = '$Id$; ' + $
            STRING(History)
  NCDF_ATTPUT, ncid, 'history', History, /GLOBAL

  ; Add to the comment GAtt
  NCDF_ATTGET, ncid, 'comment', Comment, /GLOBAL
  Comment = 'Data copied from '+In_Sensor_ID+' file. Satellite and sensor Ids updated; '+STRING(Comment)
  NCDF_ATTPUT, ncid, 'comment', Comment, /GLOBAL

  ; Replace the platform GAtt  
  NCDF_ATTPUT, ncid, 'platform_name', Out_Sensor_Id, /GLOBAL
  
  ; Done
  ; ----
  NCDF_CLOSE, ncid
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END


FUNCTION Copy_Coeff_Files, Old_Sensor_ID   , $  ; Input
                           Sensor_ID       , $  ; Input
                           WMO_Satellite_ID, $  ; Input 
                           NCEP_SENSOR_ID=NCEP_Sensor_ID, $  ; Optional input
                           FORCE         =Force              ; Optional input

  ; Set up
  ; ------ 
  ; Error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    

  ; Check mandatory input
  n_arguments = 3
  IF ( N_PARAMS() LT n_arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT

  IF ( NOT Valid_String( Old_Sensor_Id ) ) THEN $
    MESSAGE, 'Input Old_Sensor_Id  argument not defined!', $
             /NONAME, /NOPRINT
  IF ( NOT Valid_String( Sensor_Id ) ) THEN $
    MESSAGE, 'Input Sensor_Id  argument not defined!', $
             /NONAME, /NOPRINT
             
  IF ( N_ELEMENTS( WMO_Satellite_ID ) EQ 0 ) THEN $
    MESSAGE, 'Input WMO_Satellite_Id argument not defined!', $
             /NONAME, /NOPRINT

  ; Check optional input
  IF ( N_ELEMENTS( NCEP_Sensor_Id ) EQ 0 ) THEN NCEP_Sensor_Id = -1L

  ; Check that sensor ids are different
  IF ( STRTRIM(Old_Sensor_Id,2) EQ STRTRIM(Sensor_Id,2) ) THEN $
    MESSAGE, 'Input sensor ids are the same!', $
             /NONAME, /NOPRINT


  ; Loop over file types
  ; --------------------
  File_Type = [ 'SpcCoeff', 'TauCoeff' ]
  n_Types = N_ELEMENTS(File_Type)
  
  FOR i = 0, n_Types-1 DO BEGIN
  
    File = File_Name(Sensor_Id,File_Type[i])
    
    ; Check for existance of new file
    IF ( FILE_TEST( File ) AND ( NOT KEYWORD_SET(Force) ) ) THEN $
      MESSAGE, 'Output file '+File+' already exist! Use FORCE keyword to overwrite.', $
               /NONAME, /NOPRINT


    result = Copy_File( Old_Sensor_Id   , $  ; Input 
                        Sensor_Id       , $  ; Input 
                        File_Type[i]    , $  ; Input
                        WMO_Satellite_ID, $  ; Input 
                        NCEP_Sensor_ID    )  ; Optional input
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error copying file '+File, $
               /NONAME, /NOPRINT

  ENDFOR


  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS

END
