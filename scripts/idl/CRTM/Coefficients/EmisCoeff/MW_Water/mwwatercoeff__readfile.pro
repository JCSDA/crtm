;+
PRO MWwaterCoeff::ReadFile, $
  Filename           , $  ; Input
  Debug    = Debug   , $  ; Input keyword
  Title    = Title   , $  ; Output keyword
  History  = History , $  ; Output keyword
  Comment  = Comment      ; Output keyword
;-
  ; Set up
  @emiscoeff_pro_err_handler


  ; Check the file exists
  fileinfo = FILE_INFO(Filename)
  IF ( NOT fileinfo.EXISTS ) THEN $
    MESSAGE, Filename+' not found', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Open the file for reading
  fileid = Open_Binary_File( Filename )
  IF ( fileid < 0 ) THEN $
    MESSAGE, 'Error opening file '+Filename, $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
 
 
  ; Read and check the release and version
  release = self.Release
  version = self.Version
  READU, fileid, release, version
  IF ( release NE self.Release ) THEN $
    MESSAGE, 'Release check failed.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  self.Version = version
 
 
  ; Read the dimensions
  n_angles       = self.n_Angles      
  n_frequencies  = self.n_Frequencies 
  n_temperatures = self.n_Temperatures
  n_wind_speeds  = self.n_Wind_Speeds 
  READU, fileid, $
    n_angles      , $
    n_frequencies , $
    n_temperatures, $
    n_wind_speeds 


  ; Allocate the object
  self -> Create, $
    n_angles      , $
    n_frequencies , $
    n_temperatures, $
    n_wind_speeds , $
    Debug = Debug    
  IF ( NOT self -> Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Object allocation failed.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
   
 
  ; Read the global attributes
  ReadGAtts_Binary_File, $
    fileid, $
    Title   = Title  , $
    History = History, $
    Comment = Comment


  ; Read the coefficient data
  ; ...Read the dimensional vectors
  READU, fileid, $
    *self.Angle      , $
    *self.Frequency  , $
    *self.Temperature, $
    *self.Wind_Speed
  ; ...Read the emissivity data
  READU, fileid, $
    *self.ev, $
    *self.eh


  ; Close the file
  FREE_LUN, fileid
 
END
