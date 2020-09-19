!
! Solar_IO
!
! Module containing routines to read and write Solar netCDF
! format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 15-Jan-2002
!                       paul.vandelst@noaa.gov
!

MODULE Solar_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: Long, Double
  USE Message_Handler, ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility   , ONLY: File_Exists
  USE String_Utility , ONLY: StrClean
  USE Solar_Define   , ONLY: Solar_type          , &
                             Solar_Associated    , &
                             Solar_Destroy       , &
                             Solar_Create        , &
                             Solar_Inspect       , &
                             Solar_ValidRelease  , &
                             Solar_Info          , &
                             Solar_DefineVersion
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: Solar_InquireFile
  PUBLIC :: Solar_ReadFile
  PUBLIC :: Solar_WriteFile
  PUBLIC :: Solar_IOVersion



  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Default message string length
  INTEGER, PARAMETER :: ML = 1024
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME    = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME    = 'Version'
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME      = 'Title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME    = 'History' 
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME    = 'Comment' 
  CHARACTER(*), PARAMETER :: SOURCE_GATTNAME     = 'Source' 
  CHARACTER(*), PARAMETER :: REFERENCES_GATTNAME = 'References' 

  
  ! Dimension names. Case sensitive
  CHARACTER(*), PARAMETER :: FREQUENCY_DIMNAME  = 'n_Frequencies'


  ! Variable names. Case sensitive.
  CHARACTER(*), PARAMETER :: TB_VARNAME        = 'Blackbody_Temperature'
  CHARACTER(*), PARAMETER :: RADIUS_VARNAME    = 'Radius'
  CHARACTER(*), PARAMETER :: AU_VARNAME        = 'Earth_Sun_Distance'
  CHARACTER(*), PARAMETER :: FREQUENCY_VARNAME = 'Frequency'
  CHARACTER(*), PARAMETER :: HSOLAR_VARNAME    = 'Irradiance'
  CHARACTER(*), PARAMETER :: HTB_VARNAME       = 'Blackbody_Irradiance'


  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: TB_LONGNAME        = 'Solar Tb Source Temperature'
  CHARACTER(*), PARAMETER :: RADIUS_LONGNAME    = 'Solar Radius'
  CHARACTER(*), PARAMETER :: AU_LONGNAME        = 'Earth-Sun distance'
  CHARACTER(*), PARAMETER :: FREQUENCY_LONGNAME = 'Frequency'
  CHARACTER(*), PARAMETER :: HSOLAR_LONGNAME    = 'Solar Irradiance'
  CHARACTER(*), PARAMETER :: HTB_LONGNAME       = 'Solar Blackbody Irradiance'


  ! Description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: TB_DESCRIPTION        = 'Solar radiative temperature used to '//&
                                                     'generate the blackbody source function'
  CHARACTER(*), PARAMETER :: RADIUS_DESCRIPTION    = 'Radius of visible solar disk, or photosphere'
  CHARACTER(*), PARAMETER :: AU_DESCRIPTION        = 'Earth-Sun distance'
  CHARACTER(*), PARAMETER :: FREQUENCY_DESCRIPTION = 'Frequency of the solar and blackbody '//&
                                                     'irradiance spectra'
  CHARACTER(*), PARAMETER :: HSOLAR_DESCRIPTION    = 'Extraterrestrial TOA solar irradiance '//&
                                                     'using Kurucz spectrum'
  CHARACTER(*), PARAMETER :: HTB_DESCRIPTION       = 'Extraterrestrial TOA solar blackbody '//&
                                                     'irradiance using blackbody temperature'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER(*), PARAMETER :: TB_UNITS        = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: RADIUS_UNITS    = 'Metres (m)'
  CHARACTER(*), PARAMETER :: AU_UNITS        = 'Metres (m)'
  CHARACTER(*), PARAMETER :: FREQUENCY_UNITS = 'Inverse centimetres (cm^-1)'
  CHARACTER(*), PARAMETER :: HSOLAR_UNITS    = 'mW/(m^2.cm^-1)'
  CHARACTER(*), PARAMETER :: HTB_UNITS       = 'mW/(m^2.cm^-1)'


  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  REAL(Double), PARAMETER :: TB_FILLVALUE        = ZERO
  REAL(Double), PARAMETER :: RADIUS_FILLVALUE    = ZERO
  REAL(Double), PARAMETER :: AU_FILLVALUE        = ZERO
  REAL(Double), PARAMETER :: FREQUENCY_FILLVALUE = ZERO
  REAL(Double), PARAMETER :: HSOLAR_FILLVALUE    = ZERO
  REAL(Double), PARAMETER :: HTB_FILLVALUE       = ZERO


  ! Variable netCDF datatypes
  INTEGER, PARAMETER :: TB_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: RADIUS_TYPE    = NF90_DOUBLE
  INTEGER, PARAMETER :: AU_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: FREQUENCY_TYPE = NF90_DOUBLE
  INTEGER, PARAMETER :: HSOLAR_TYPE    = NF90_DOUBLE
  INTEGER, PARAMETER :: HTB_TYPE       = NF90_DOUBLE


CONTAINS




!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Solar_InquireFile
!
! PURPOSE:
!       Function to inquire Solar object data files.
!
! CALLING SEQUENCE:
!       Error_Status = Solar_InquireFile( &
!                        Filename                     , &
!                        n_Frequencies = n_Frequencies, &
!                        Release       = Release      , &
!                        Version       = Version      , &
!                        Title         = Title        , &
!                        History       = History      , &
!                        Comment       = Comment      , &
!                        Source        = Source       , &
!                        References    = References     )
!
! INPUTS:
!       Filename:           Character string specifying the name of the
!                           Solar data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Frequencies:      Number of spectral frquencies.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The release number of the Solar file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the Solar file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the Solar file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the Solar file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the Solar file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Source:             Character string written into the SOURCE global
!                           attribute field of the Solar file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       References:         Character string written into the REFERENCES global
!                           attribute field of the Solar file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the file inquiry was successful
!                              == FAILURE an error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Solar_InquireFile( &
    Filename     , &  ! Input
    n_Frequencies, &  ! Optional output  
    Release      , &  ! Optional output  
    Version      , &  ! Optional Output
    Title        , &  ! Optional Output
    History      , &  ! Optional Output
    Comment      , &  ! Optional Output
    Source       , &  ! Optional Output
    References   ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Frequencies
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release      
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version      
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title                 
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History               
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment               
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Source                
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: References            
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Solar_InquireFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    INTEGER :: nf90_status
    INTEGER :: fileid
    INTEGER :: dimid
    TYPE(Solar_type) :: Solar
    
    ! Set up
    err_stat = SUCCESS
    close_file = .FALSE.


    ! Open the file
    nf90_status = NF90_OPEN( Filename,NF90_NOWRITE,fileid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '// &
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Get the dimensions
    ! ...n_Frequencies dimension
    NF90_Status = NF90_INQ_DIMID( FileId,FREQUENCY_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//FREQUENCY_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=solar%n_Frequencies )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//FREQUENCY_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
  
  
    ! Get the global attributes
    err_stat = ReadGAtts( Filename, &
                          fileid  , &
                          Release    = Release   , &
                          Version    = Version   , &
                          Title      = Title     , &
                          History    = History   , &
                          Comment    = Comment   , &
                          Source     = Source    , &
                          References = References  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes from '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    nf90_status = NF90_CLOSE( fileid )
    close_file = .FALSE.
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return values
    IF ( PRESENT(n_Frequencies) ) n_Frequencies = Solar%n_Frequencies

  CONTAINS
 
    SUBROUTINE Inquire_CleanUp()
      IF ( close_file ) THEN
        nf90_status = NF90_CLOSE( fileid )
        IF ( nf90_status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION Solar_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Solar_WriteFile
!
! PURPOSE:
!       Function to write Solar object data files.
!
! CALLING SEQUENCE:
!       Error_Status = Solar_WriteFile( &
!                        Filename               , &
!                        Solar                  , &
!                        Quiet      = Quiet     , &
!                        Title      = Title     , &
!                        History    = History   , &
!                        Comment    = Comment   , &
!                        Source     = Source    , &
!                        References = References  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       Solar data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Solar:          Solar object containing the irradiance spectra.
!                       UNITS:      N/A
!                       TYPE:       Solar_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:          Character string written into the TITLE global
!                       attribute field of the Solar file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the Solar file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the Solar file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Source:         Character string written into the SOURCE global
!                       attribute field of the Solar file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       References:     Character string written into the REFERENCES global
!                       attribute field of the Solar file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the data write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Solar_WriteFile( &
    Filename  , &  ! Input
    Solar     , &  ! Input
    Quiet     , &  ! Optional input
    Title     , &  ! Optional input
    History   , &  ! Optional input
    Comment   , &  ! Optional input
    Source    , &  ! Optional input
    References) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    TYPE(Solar_type),       INTENT(IN) :: Solar
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Source    
    CHARACTER(*), OPTIONAL, INTENT(IN) :: References
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Solar_WriteFile(netCDF)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: nf90_status
    INTEGER :: fileid
    INTEGER :: varid

    ! Set up
    err_stat = SUCCESS
    close_file = .FALSE.
    ! ...Check structure pointer association status
    IF ( .NOT. Solar_Associated( Solar ) ) THEN
      msg = 'Solar structure is empty. Nothing to do!'
      CALL Write_CleanUp(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. Solar_ValidRelease( Solar ) ) THEN
      msg = 'Solar Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet


    ! Create the output file
    err_stat = CreateFile( &
                 Filename                  , &
                 Solar%n_Frequencies       , &
                 fileid                    , &
                 Version    = Solar%Version, &
                 Title      = Title        , &
                 History    = History      , &
                 Comment    = Comment      , &
                 Source     = Source       , &
                 References = References     )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error creating output file '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Write the data items
    ! ...Blackbody_Temperature variable
    NF90_Status = NF90_INQ_VARID( FileId,TB_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TB_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,Solar%Blackbody_Temperature )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TB_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Radius variable
    NF90_Status = NF90_INQ_VARID( FileId,RADIUS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//RADIUS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,Solar%Radius )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//RADIUS_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Earth_Sun_Distance variable
    NF90_Status = NF90_INQ_VARID( FileId,AU_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//AU_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,Solar%Earth_Sun_Distance )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//AU_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Frequency variable
    NF90_Status = NF90_INQ_VARID( FileId,FREQUENCY_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//FREQUENCY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,Solar%Frequency )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//FREQUENCY_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Irradiance variable
    NF90_Status = NF90_INQ_VARID( FileId,HSOLAR_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//HSOLAR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,Solar%Irradiance )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//HSOLAR_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Blackbody_Irradiance variable
    NF90_Status = NF90_INQ_VARID( FileId,HTB_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//HTB_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,Solar%Blackbody_Irradiance )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//HTB_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Close the file
    nf90_status = NF90_CLOSE( fileid )
    close_file = .FALSE.
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      CALL Solar_Info( Solar, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp()
      IF ( close_file ) THEN
        nf90_status = NF90_CLOSE( fileid )
        IF ( nf90_status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing output file during error cleanup - '//&
                TRIM(NF90_STRERROR( nf90_status ))
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION Solar_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Solar_ReadFile
!
! PURPOSE:
!       Function to read Solar object data files.
!
! CALLING SEQUENCE:
!       Error_Status = Solar_netCDF_ReadFile( &
!                        Filename               , &
!                        Solar                  , &
!                        Quiet      = Quiet     , &
!                        Title      = Title     , &
!                        History    = History   , &
!                        Comment    = Comment   , &
!                        Source     = Source    , &
!                        References = References  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       Solar data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Solar:          Solar object containing the irradiance spectra.
!                       UNITS:      N/A
!                       TYPE:       Solar_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       Title:          Character string written into the TITLE global
!                       attribute field of the Solar file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the Solar file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the Solar file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Source:         Character string written into the SOURCE global
!                       attribute field of the Solar file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       References:     Character string written into the REFERENCES global
!                       attribute field of the Solar file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the data write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Solar_ReadFile( &
    Filename  , &  ! Input
    Solar     , &  ! Output
    Quiet     , &  ! Optional input
    Title     , &  ! Optional output
    History   , &  ! Optional output
    Comment   , &  ! Optional output
    Source    , &  ! Optional output
    References) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(Solar_type),       INTENT(OUT) :: Solar
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Source    
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: References
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Solar_ReadFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: nf90_status
    INTEGER :: fileid
    INTEGER :: n_frequencies
    INTEGER :: varid


    ! Set up
    err_stat = SUCCESS
    close_file = .FALSE.
    ! ...Check that the file exists
    IF ( .NOT. File_Exists(Filename) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet


    ! Inquire the file to get the dimensions
    err_stat = Solar_InquireFile( &
                 Filename, &
                 n_Frequencies = n_frequencies )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error obtaining Solar dimensions from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the output structure
    CALL Solar_Create( &
           Solar, &
           n_frequencies )
    IF ( .NOT. Solar_Associated(Solar) ) THEN
      msg = 'Error allocating output Solar'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the file for reading
    nf90_status = NF90_OPEN( Filename,NF90_NOWRITE,fileid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '//&
            TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Read the global attributes
    err_stat = ReadGAtts( &
                 Filename, &
                 fileid  , &
                 Release    = Solar%Release, &
                 Version    = Solar%Version, &
                 Title      = Title        , &
                 History    = History      , &
                 Comment    = Comment      , &
                 Source     = Source       , &
                 References = References     )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. Solar_ValidRelease( Solar ) ) THEN
      msg = 'Solar Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF

    
    ! Read the Solar data
    ! ...Blackbody_Temperature variable
    nf90_status = NF90_INQ_VARID( fileid,TB_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TB_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,Solar%Blackbody_Temperature )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TB_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Radius variable
    nf90_status = NF90_INQ_VARID( fileid,RADIUS_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//RADIUS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,Solar%Radius )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//RADIUS_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Earth_Sun_Distance variable
    nf90_status = NF90_INQ_VARID( fileid,AU_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//AU_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,Solar%Earth_Sun_Distance )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//AU_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Frequency variable
    nf90_status = NF90_INQ_VARID( fileid,FREQUENCY_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//FREQUENCY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,Solar%Frequency )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//FREQUENCY_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Irradiance variable
    nf90_status = NF90_INQ_VARID( fileid,HSOLAR_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//HSOLAR_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,Solar%Irradiance )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//HSOLAR_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Blackbody_Irradiance variable
    nf90_status = NF90_INQ_VARID( fileid,HTB_VARNAME,varid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//HTB_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    nf90_status = NF90_GET_VAR( fileid,varid,Solar%Blackbody_Irradiance )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error reading '//HTB_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF
    
    
    ! Set the frequency limit values
    solar%f1 = solar%Frequency(1)
    solar%f2 = solar%Frequency(solar%n_Frequencies)


    ! Close the file
    nf90_status = NF90_CLOSE( fileid ); CLOSE_FILE = .FALSE.
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      CALL Solar_Info( Solar, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS
 
    SUBROUTINE Read_CleanUp()
      IF ( close_file ) THEN
        nf90_status = NF90_CLOSE( fileid )
        IF ( nf90_status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup- '//&
                TRIM(NF90_STRERROR( nf90_status ))
      END IF
      CALL Solar_Destroy( Solar )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp
    
  END FUNCTION Solar_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Solar_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL Solar_IOVersion( Id )
!
! OUTPUT ARGUMENTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Solar_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE Solar_IOVersion




!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! Function to write the global attributes to a Solar data file.

  FUNCTION WriteGAtts( &
    Filename  , &  ! Input
    FileId    , &  ! Input
    Version   , &  ! Optional input
    Title     , &  ! Optional input
    History   , &  ! Optional input
    Comment   , &  ! Optional input
    Source    , &  ! Optional input
    References) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    INTEGER     ,           INTENT(IN) :: FileId
    INTEGER     , OPTIONAL, INTENT(IN) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Source    
    CHARACTER(*), OPTIONAL, INTENT(IN) :: References
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Solar_WriteGAtts(netCDF)'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history'
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: gattname
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone
    INTEGER :: ver
    INTEGER :: nf90_status
    TYPE(Solar_type) :: Solar

    ! Set up
    err_stat = SUCCESS
    msg = ' '

    ! Mandatory global attributes
    ! ...Software ID
    gattname = WRITE_MODULE_HISTORY_GATTNAME
    nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),MODULE_VERSION_ID )
    IF ( nf90_status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    gattname = CREATION_DATE_AND_TIME_GATTNAME
    nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname), &
                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                czone//'UTC' )
    IF ( nf90_status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The Release
    gattname = RELEASE_GATTNAME
    nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),Solar%Release )
    IF ( nf90_status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! ...The Version
    IF ( PRESENT(Version) ) THEN
      ver = Version
    ELSE
      ver = Solar%Version
    END IF
    gattname = VERSION_GATTNAME
    nf90_status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(gattname),Ver )
    IF ( nf90_status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The Title
    IF ( PRESENT(Title) ) THEN
      gattname = TITLE_GATTNAME
      nf90_status = NF90_PUT_ATT( FileID,NF90_GLOBAL,TRIM(gattname),Title )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The History
    IF ( PRESENT(History) ) THEN
      gattname = HISTORY_GATTNAME
      nf90_status = NF90_PUT_ATT( FileID,NF90_GLOBAL,TRIM(gattname),History )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The Comment
    IF ( PRESENT(Comment) ) THEN
      gattname = COMMENT_GATTNAME
      nf90_status = NF90_PUT_ATT( FileID,NF90_GLOBAL,TRIM(gattname),Comment )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The Source
    IF ( PRESENT(Source) ) THEN
      gattname = SOURCE_GATTNAME
      nf90_status = NF90_PUT_ATT( FileID,NF90_GLOBAL,TRIM(gattname),Source )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The References
    IF ( PRESENT(References) ) THEN
      gattname = REFERENCES_GATTNAME
      nf90_status = NF90_PUT_ATT( FileID,NF90_GLOBAL,TRIM(gattname),References )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    
 CONTAINS
  
    SUBROUTINE WriteGAtts_CleanUp()
      nf90_status = NF90_CLOSE( FileId )
      IF ( nf90_status /= NF90_NOERR ) &
        msg = '; Error closing input file during error cleanup - '//&
              TRIM(NF90_STRERROR( nf90_status ) )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(gattname)//' attribute to '//&
                            TRIM(Filename)//' - '// &
                            TRIM(NF90_STRERROR( nf90_status ) )//TRIM(msg), &
                            err_stat )
    END SUBROUTINE WriteGAtts_CleanUp
    
  END FUNCTION WriteGAtts


  ! Function to read the global attributes from a Solar data file.

  FUNCTION ReadGAtts( &
    Filename  , &  ! Input
    FileId    , &  ! Input
    Release   , &  ! Optional output
    Version   , &  ! Optional output
    Title     , &  ! Optional output
    History   , &  ! Optional output
    Comment   , &  ! Optional output
    Source    , &  ! Optional output
    References) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     ,           INTENT(IN)  :: FileId
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release        
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Source    
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: References
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Solar_ReadGAtts(netCDF)'
    ! Local variables
    CHARACTER(ML)   :: msg
    CHARACTER(256)  :: gattname
    CHARACTER(5000) :: gattstring
    INTEGER :: nf90_status
    
    ! Set up
    err_stat = SUCCESS

    ! The global attributes
    ! ...The Release
    IF ( PRESENT(Release) ) THEN
      gattname = RELEASE_GATTNAME
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),Release )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The Version
    IF ( PRESENT(Version) ) THEN
      gattname = VERSION_GATTNAME
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),Version )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The Title
    IF ( PRESENT(Title) ) THEN
      gattname = TITLE_GATTNAME; gattstring = ''
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),gattstring )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF         
      CALL StrClean( gattstring )
      Title = gattstring(1:MIN(LEN(Title), LEN_TRIM(gattstring)))
    END IF
    ! ...The History
    IF ( PRESENT(History) ) THEN
      gattname = HISTORY_GATTNAME; gattstring = ''
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),gattstring )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF         
      CALL StrClean( gattstring )
      History = gattstring(1:MIN(LEN(History), LEN_TRIM(gattstring)))
    END IF
    ! ...The Comment
    IF ( PRESENT(Comment) ) THEN
      gattname = COMMENT_GATTNAME; gattstring = ''
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),gattstring )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF         
      CALL StrClean( gattstring )
      Comment = gattstring(1:MIN(LEN(Comment), LEN_TRIM(gattstring)))
    END IF
    ! ...The Source
    IF ( PRESENT(Source) ) THEN
      gattname = SOURCE_GATTNAME; gattstring = ''
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),gattstring )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF         
      CALL StrClean( gattstring )
      Source = gattstring(1:MIN(LEN(Source), LEN_TRIM(gattstring)))
    END IF
    ! ...The References
    IF ( PRESENT(References) ) THEN
      gattname = REFERENCES_GATTNAME; gattstring = ''
      nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),gattstring )
      IF ( nf90_status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF         
      CALL StrClean( gattstring )
      References = gattstring(1:MIN(LEN(References), LEN_TRIM(gattstring)))
    END IF

  CONTAINS

    SUBROUTINE ReadGAtts_CleanUp()
      err_stat = FAILURE
      msg = 'Error reading '//TRIM(gattname)//' attribute from '//TRIM(Filename)//' - '// &
            TRIM(NF90_STRERROR( nf90_status ) )
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE ReadGAtts_CleanUp

  END FUNCTION ReadGAtts



  ! Function to create a Solar file for writing

  FUNCTION CreateFile( &
    Filename     , &  ! Input
    n_Frequencies, &  ! Input
    FileId       , &  ! Output
    Version      , &  ! Optional input
    Title        , &  ! Optional input
    History      , &  ! Optional input
    Comment      , &  ! Optional input
    Source       , &  ! Optional input
    References   ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     ,           INTENT(IN)  :: n_Frequencies
    INTEGER     ,           INTENT(OUT) :: FileId
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Source    
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: References
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Solar_CreateFile(netCDF)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    INTEGER :: nf90_status
    INTEGER :: n_frequencies_dimid
    INTEGER :: varid
    INTEGER :: put_status(4)
    
    ! Setup
    err_stat = SUCCESS
    close_file = .FALSE.


    ! Create the data file
    nf90_status = NF90_CREATE( Filename,NF90_CLOBBER,FileId )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Define the dimensions
    ! ...Frequency of the solar and blackbody irradiance spectra
    nf90_status = NF90_DEF_DIM( FileID,FREQUENCY_DIMNAME,n_Frequencies,n_frequencies_dimid )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//FREQUENCY_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF


    ! Write the global attributes
    err_stat = WriteGAtts( &
                 Filename, &
                 FileId  , &
                 Version    = Version   , &
                 Title      = Title     , &
                 History    = History   , &
                 Comment    = Comment   , &
                 Source     = Source    , &
                 References = References  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing global attribute to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF


    ! Define the variables
    ! ...Blackbody_Temperature variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                TB_VARNAME, &
                                TB_TYPE, &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//TB_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,TB_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,TB_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,TB_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,TB_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//TB_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Radius variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                RADIUS_VARNAME, &
                                RADIUS_TYPE, &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//RADIUS_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,RADIUS_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,RADIUS_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,RADIUS_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,RADIUS_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//RADIUS_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Earth_Sun_Distance variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                AU_VARNAME, &
                                AU_TYPE, &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//AU_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,AU_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,AU_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,AU_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,AU_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//AU_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Frequency variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                FREQUENCY_VARNAME, &
                                FREQUENCY_TYPE, &
                                dimIDs=(/n_frequencies_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//FREQUENCY_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,FREQUENCY_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,FREQUENCY_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,FREQUENCY_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,FREQUENCY_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//FREQUENCY_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Irradiance variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                HSOLAR_VARNAME, &
                                HSOLAR_TYPE, &
                                dimIDs=(/n_frequencies_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//HSOLAR_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,HSOLAR_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,HSOLAR_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,HSOLAR_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,HSOLAR_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//HSOLAR_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Blackbody_Irradiance variable
    nf90_status = NF90_DEF_VAR( FileID, &
                                HTB_VARNAME, &
                                HTB_TYPE, &
                                dimIDs=(/n_frequencies_dimid/), &
                                varID=variD )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error defining '//HTB_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF
    put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,HTB_LONGNAME    )
    put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,HTB_DESCRIPTION )
    put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,HTB_UNITS       )
    put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,HTB_FILLVALUE   )
    IF ( ANY(put_status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//HTB_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF


    ! Take netCDF file out of define mode
    nf90_status = NF90_ENDDEF( FileId )
    IF ( nf90_status /= NF90_NOERR ) THEN
      msg = 'Error taking file '//TRIM(Filename)// &
            ' out of define mode - '//TRIM(NF90_STRERROR( nf90_status ))
      CALL Create_Cleanup(); RETURN
    END IF

  CONTAINS
 
    SUBROUTINE Create_CleanUp()
      IF ( close_file ) THEN
        nf90_status = NF90_CLOSE( FileID )
        IF ( nf90_status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                TRIM(NF90_STRERROR( nf90_status ))
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Create_CleanUp
    
  END FUNCTION CreateFile

END MODULE Solar_IO
