!
! CompTest_netCDF_IO
!
! Module containing routines to read and write netCDF format
! CompTest files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 04-Aug-2010
!                       paul.vandelst@noaa.gov
!

MODULE CompTest_netCDF_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility   , ONLY: File_Exists
  USE String_Utility , ONLY: StrClean
  USE CompTest_Define, ONLY: CompTest_type           , &
                             CompTest_Associated     , &
                             CompTest_Destroy        , &
                             CompTest_Create         , &
                             CompTest_Release_IsValid, &
                             CompTest_Inspect        , &
                             CompTest_Info           , &
                             CompTest_DefineVersion  , &
                             CompTest_Is_Valid       , &
                             CompTest_Is_FWDTL       , &
                             CompTest_Is_TLAD        , &
                             CompTest_Is_ADK         , &
                             CompTest_Is_Channel     , &
                             CompTest_Is_Frequency   , &
                             CompTest_HasPressure          , &
                             CompTest_HasSpectral          , &
                             CompTest_HasPerturbation      , &
                             CompTest_SetPressureFlag      , &
                             CompTest_SetSpectralFlag      , &
                             CompTest_SetPerturbationFlag  , &
                             CompTest_ClearPressureFlag    , &
                             CompTest_ClearSpectralFlag    , &
                             CompTest_ClearPerturbationFlag

  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: CompTest_netCDF_InquireFile
  PUBLIC :: CompTest_netCDF_WriteFile
  PUBLIC :: CompTest_netCDF_ReadFile
  PUBLIC :: CompTest_netCDF_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module version
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Default message string length
  INTEGER, PARAMETER :: ML = 1024
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME  = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME  = 'Version'
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME    = 'Title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME  = 'History' 
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME  = 'Comment' 
  CHARACTER(*), PARAMETER :: TESTTYPE_GATTNAME = 'TestType'
  CHARACTER(*), PARAMETER :: DATATYPE_GATTNAME = 'DataType'
  CHARACTER(*), PARAMETER :: FLAGS_GATTNAME    = 'Flags'

  ! Dimension names. Case sensitive
  CHARACTER(*), PARAMETER :: LAYER_DIMNAME           = 'n_Layers'
  CHARACTER(*), PARAMETER :: CHANNEL_DIMNAME         = 'n_Channels'
  CHARACTER(*), PARAMETER :: FREQUENCY_DIMNAME       = 'n_Frequencies'
  CHARACTER(*), PARAMETER :: PERTURBATION_DIMNAME    = 'n_Perturbations'
  CHARACTER(*), PARAMETER :: INPUT_VARIABLE_DIMNAME  = 'n_Input_Variables'
  CHARACTER(*), PARAMETER :: OUTPUT_VARIABLE_DIMNAME = 'n_Output_Variables'
  CHARACTER(*), PARAMETER :: VSL_DIMNAME             = 'vsl'
  CHARACTER(*), PARAMETER :: DATASET_DIMNAME         = 'n_DataSets'

  ! Variable names. Case sensitive.
  CHARACTER(*), PARAMETER :: PRESSURE_VARNAME        = 'Pressure'
  CHARACTER(*), PARAMETER :: CHANNEL_VARNAME         = 'Channel'
  CHARACTER(*), PARAMETER :: FREQUENCY_VARNAME       = 'Frequency'
  CHARACTER(*), PARAMETER :: PERTURBATION_VARNAME    = 'Perturbation'
  CHARACTER(*), PARAMETER :: INPUT_VARIABLE_VARNAME  = 'Input_Variable_Name'
  CHARACTER(*), PARAMETER :: INPUT_UNITS_VARNAME     = 'Input_Variable_Units'
  CHARACTER(*), PARAMETER :: OUTPUT_VARIABLE_VARNAME = 'Output_Variable_Name'
  CHARACTER(*), PARAMETER :: OUTPUT_UNITS_VARNAME    = 'Output_Variable_Units'
  CHARACTER(*), PARAMETER :: D1_DEFAULT_VARNAME      = 'd1'
  CHARACTER(*), PARAMETER :: D2_DEFAULT_VARNAME      = 'd2'
  CHARACTER(*), PARAMETER :: D1_FWDTL_VARNAME        = 'd_NL'
  CHARACTER(*), PARAMETER :: D2_FWDTL_VARNAME        = 'd_TL'
  CHARACTER(*), PARAMETER :: D1_TLAD_VARNAME         = 'd_TL'
  CHARACTER(*), PARAMETER :: D2_TLAD_VARNAME         = 'd_AD'
  CHARACTER(*), PARAMETER :: D1_ADK_VARNAME          = 'd_AD'
  CHARACTER(*), PARAMETER :: D2_ADK_VARNAME          = 'd_K'
  CHARACTER(*), PARAMETER :: DATASET_NAME_VARNAME    = 'DataSet_Name'
  
  
  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: PRESSURE_LONGNAME        = 'Pressure'
  CHARACTER(*), PARAMETER :: CHANNEL_LONGNAME         = 'Sensor Channel'
  CHARACTER(*), PARAMETER :: FREQUENCY_LONGNAME       = 'Frequency'
  CHARACTER(*), PARAMETER :: PERTURBATION_LONGNAME    = 'Perturbation fraction'
  CHARACTER(*), PARAMETER :: INPUT_VARIABLE_LONGNAME  = 'Input Variable Name'
  CHARACTER(*), PARAMETER :: INPUT_UNITS_LONGNAME     = 'Output Variable Name'
  CHARACTER(*), PARAMETER :: OUTPUT_VARIABLE_LONGNAME = 'Input Variable Units'
  CHARACTER(*), PARAMETER :: OUTPUT_UNITS_LONGNAME    = 'Output Variable Units'
  CHARACTER(*), PARAMETER :: D1_DEFAULT_LONGNAME      = 'd1'
  CHARACTER(*), PARAMETER :: D2_DEFAULT_LONGNAME      = 'd2'
  CHARACTER(*), PARAMETER :: D1_FWDTL_LONGNAME        = 'd(NL)'
  CHARACTER(*), PARAMETER :: D2_FWDTL_LONGNAME        = 'd(TL)'
  CHARACTER(*), PARAMETER :: D1_TLAD_LONGNAME         = 'd(TL)'
  CHARACTER(*), PARAMETER :: D2_TLAD_LONGNAME         = 'd(AD)'
  CHARACTER(*), PARAMETER :: D1_ADK_LONGNAME          = 'd(AD)'
  CHARACTER(*), PARAMETER :: D2_ADK_LONGNAME          = 'd(K)'
  CHARACTER(*), PARAMETER :: DATASET_NAME_LONGNAME    = 'DataSet Name'


  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: PRESSURE_DESCRIPTION = &
'Layer pressures used in the component test.'
  CHARACTER(*), PARAMETER :: CHANNEL_DESCRIPTION = &
'Sensor channels used in the component test.'
  CHARACTER(*), PARAMETER :: FREQUENCY_DESCRIPTION = &
'Frequencies used in the component test.'
  CHARACTER(*), PARAMETER :: PERTURBATION_DESCRIPTION = &
'List of the perturbation fractions applied to the variables in the component test.'
  CHARACTER(*), PARAMETER :: INPUT_VARIABLE_DESCRIPTION = &
'List of the input variable names in the component test'
  CHARACTER(*), PARAMETER :: INPUT_UNITS_DESCRIPTION = &
'List of the input variable units in the component test'
  CHARACTER(*), PARAMETER :: OUTPUT_VARIABLE_DESCRIPTION = &
'List of the output variable names in the component test'
  CHARACTER(*), PARAMETER :: OUTPUT_UNITS_DESCRIPTION = &
'List of the output variable units in the component test'
  CHARACTER(*), PARAMETER :: D1_DEFAULT_DESCRIPTION = &
'The d1 data.'
  CHARACTER(*), PARAMETER :: D2_DEFAULT_DESCRIPTION = &
'The d2 data.'
  CHARACTER(*), PARAMETER :: D1_FWDTL_DESCRIPTION = &
'The FWD model non-linear difference for the given input variable perturbation.'
  CHARACTER(*), PARAMETER :: D2_FWDTL_DESCRIPTION = &
'The TL model values for the given input variable perturbation.'
  CHARACTER(*), PARAMETER :: D1_TLAD_DESCRIPTION = &
'The TL model values for unity perturbation of the TL variables'
  CHARACTER(*), PARAMETER :: D2_TLAD_DESCRIPTION = &
'The AD model values for unity perturbation of the AD variables'
  CHARACTER(*), PARAMETER :: D1_ADK_DESCRIPTION = &
'The AD model values for unity perturbation of the AD variables'
  CHARACTER(*), PARAMETER :: D2_ADK_DESCRIPTION = &
'The K-matrix model values for unity perturbation of the AD variables'
  CHARACTER(*), PARAMETER :: DATASET_NAME_DESCRIPTION = &
'List of descriptions for each dataset in the file'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER(*), PARAMETER :: PRESSURE_UNITS        = 'hPa'
  CHARACTER(*), PARAMETER :: CHANNEL_UNITS         = 'Channel number'
  CHARACTER(*), PARAMETER :: FREQUENCY_UNITS       = 'cm^-1 or GHz'
  CHARACTER(*), PARAMETER :: PERTURBATION_UNITS    = 'Variable. Input dependent.'
  CHARACTER(*), PARAMETER :: INPUT_VARIABLE_UNITS  = 'N/A'
  CHARACTER(*), PARAMETER :: OUTPUT_VARIABLE_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: INPUT_UNITS_UNITS     = 'N/A'
  CHARACTER(*), PARAMETER :: OUTPUT_UNITS_UNITS    = 'N/A'
  CHARACTER(*), PARAMETER :: D1_UNITS              = 'Variable'
  CHARACTER(*), PARAMETER :: D2_UNITS              = 'Variable'
  CHARACTER(*), PARAMETER :: DATASET_NAME_UNITS    = 'N/A'

  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'

  REAL(fp)    , PARAMETER :: PRESSURE_FILLVALUE        = ZERO
  REAL(fp)    , PARAMETER :: SPECTRAL_FILLVALUE        = ZERO
  REAL(fp)    , PARAMETER :: PERTURBATION_FILLVALUE    = ZERO
  CHARACTER(*), PARAMETER :: INPUT_VARIABLE_FILLVALUE  = NF90_FILL_CHAR
  CHARACTER(*), PARAMETER :: OUTPUT_VARIABLE_FILLVALUE = NF90_FILL_CHAR
  CHARACTER(*), PARAMETER :: INPUT_UNITS_FILLVALUE     = NF90_FILL_CHAR
  CHARACTER(*), PARAMETER :: OUTPUT_UNITS_FILLVALUE    = NF90_FILL_CHAR
  REAL(fp)    , PARAMETER :: D1_FILLVALUE              = ZERO
  REAL(fp)    , PARAMETER :: D2_FILLVALUE              = ZERO
  CHARACTER(*), PARAMETER :: DATASET_NAME_FILLVALUE    = NF90_FILL_CHAR

  ! Variable netCDF datatypes
  INTEGER, PARAMETER :: PRESSURE_TYPE        = NF90_DOUBLE 
  INTEGER, PARAMETER :: SPECTRAL_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: PERTURBATION_TYPE    = NF90_DOUBLE
  INTEGER, PARAMETER :: INPUT_VARIABLE_TYPE  = NF90_CHAR
  INTEGER, PARAMETER :: OUTPUT_VARIABLE_TYPE = NF90_CHAR
  INTEGER, PARAMETER :: INPUT_UNITS_TYPE     = NF90_CHAR
  INTEGER, PARAMETER :: OUTPUT_UNITS_TYPE    = NF90_CHAR
  INTEGER, PARAMETER :: D1_TYPE              = NF90_DOUBLE
  INTEGER, PARAMETER :: D2_TYPE              = NF90_DOUBLE
  INTEGER, PARAMETER :: DATASET_NAME_TYPE    = NF90_CHAR


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
!       CompTest_netCDF_InquireFile
!
! PURPOSE:
!       Function to inquire CompTest object netCDF format files.
!
! CALLING SEQUENCE:
!       Error_Status = CompTest_netCDF_InquireFile( &
!                        Filename, &
!                        n_InputVars      = n_InputVars     , &
!                        n_OutputVars     = n_OutputVars    , &
!                        n_Layers         = n_Layers        , &
!                        n_Spectral       = n_Spectral      , &
!                        n_Perturbations  = n_Perturbations , &
!                        n_Datasets       = n_Datasets      , &
!                        Flags            = Flags           , &
!                        Release          = Release         , &
!                        Version          = Version         , &
!                        Title            = Title           , &
!                        History          = History         , &
!                        Comment          = Comment           )
!
! INPUTS:
!       Filename:           Character string specifying the name of the
!                           CompTest data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_InputVars:        The number of input variables dimension of the
!                           CompTest data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_OutputVars:       The number of output variables dimension of the
!                           CompTest data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Layers:           The size of the layers dimension of the
!                           CompTest data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Spectral:         The size of the spectral dimension of the
!                           CompTest data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Perturbations:    The number of perturbations dimension of the
!                           CompTest data for FWD/TL component tests.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Datasets:         The number of datasets in the CompTest datafile.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Flags:              The CompTest bit flags. These are used to determine
!                           dimensionality, the test type, and the spectral
!                           dimension type 
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The release number of the CompTest file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the CompTest file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the CompTest file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the CompTest file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the CompTest file.
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

  FUNCTION CompTest_netCDF_InquireFile( &
    Filename       , &  ! Input
    n_InputVars    , &  ! Optional output
    n_OutputVars   , &  ! Optional output
    n_Layers       , &  ! Optional output
    n_Spectral     , &  ! Optional output
    n_Perturbations, &  ! Optional output
    n_Datasets     , &  ! Optional output
    Flags          , &  ! Optional output
    Release        , &  ! Optional output
    Version        , &  ! Optional output
    Title          , &  ! Optional output
    History        , &  ! Optional output
    Comment        ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_InputVars        
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_OutputVars       
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Layers           
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Spectral      
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Perturbations    
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Datasets
    INTEGER,      OPTIONAL, INTENT(OUT) :: Flags        
    INTEGER,      OPTIONAL, INTENT(OUT) :: Release         
    INTEGER,      OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title           
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment         
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CompTest_InquireFile(netCDF)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    INTEGER :: NF90_Status
    INTEGER :: FileId
    INTEGER :: DimId
    CHARACTER(256) :: Spectral_DimName
    TYPE(CompTest_type) :: c
    
    ! Set up
    err_stat = SUCCESS
    Close_File = .FALSE.


    ! Open the file
    NF90_Status = NF90_OPEN( Filename,NF90_NOWRITE,FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.


    ! Get the global attributes
    err_stat = ReadGAtts( c, &
                          Filename, &
                          FileId  , &
                          Title   = Title  , &
                          History = History, &
                          Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes from '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Get the dimensions
    ! ...n_Layers dimension
    NF90_Status = NF90_INQ_DIMID( FileId,LAYER_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//LAYER_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=c%nK )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//LAYER_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...Spectral dimension
    CALL Spectral_Names(c, DimName = Spectral_DimName )
    NF90_Status = NF90_INQ_DIMID( FileId,TRIM(Spectral_DimName),DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//TRIM(Spectral_DimName)//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=c%nL )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//TRIM(Spectral_DimName)//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Perturbations dimension
    NF90_Status = NF90_INQ_DIMID( FileId,PERTURBATION_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//PERTURBATION_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=c%nP )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//PERTURBATION_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Input_Variables dimension
    NF90_Status = NF90_INQ_DIMID( FileId,INPUT_VARIABLE_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//INPUT_VARIABLE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=c%nIV )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//INPUT_VARIABLE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Output_Variables dimension
    NF90_Status = NF90_INQ_DIMID( FileId,OUTPUT_VARIABLE_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//OUTPUT_VARIABLE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=c%nOV )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//OUTPUT_VARIABLE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_DataSets dimension
    NF90_Status = NF90_INQ_DIMID( FileId,DATASET_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//DATASET_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=c%nM )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//DATASET_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    NF90_Status = NF90_CLOSE( FileId )
    Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return values
    IF ( PRESENT(n_InputVars    ) ) n_InputVars     = c%nIV   
    IF ( PRESENT(n_OutputVars   ) ) n_OutputVars    = c%nOV
    IF ( PRESENT(n_Layers       ) ) n_Layers        = c%nK
    IF ( PRESENT(n_Spectral     ) ) n_Spectral      = c%nL
    IF ( PRESENT(n_Perturbations) ) n_Perturbations = c%nP
    IF ( PRESENT(n_Datasets     ) ) n_Datasets      = c%nM
    IF ( PRESENT(Flags          ) ) Flags           = c%Flags
    IF ( PRESENT(Release        ) ) Release         = c%Release   
    IF ( PRESENT(Version        ) ) Version         = c%Version

  CONTAINS
 
    SUBROUTINE Inquire_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileId )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION CompTest_netCDF_InquireFile
  
  
!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_netCDF_WriteFile
!
! PURPOSE:
!       Function to write CompTest object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = CompTest_netCDF_WriteFile( &
!                        CompTest, &
!                        Filename, &
!                        Quiet   = Quiet  , &
!                        New     = New    , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! OBJECTS:
!       CompTest:       CompTest object to write to file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CompTest_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       CompTest data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
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
!       New:            Set this argument to write the CompTest object
!                       data to a new file. Default action is to write to
!                       an existing file.
!                       If NEW = .TRUE. , data is written to an existing file. [DEFAULT]
!                          NEW = .FALSE., a new file is created for output.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:          Character string written into the TITLE global
!                       attribute field of the CompTest file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the CompTest file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the CompTest file.
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

  FUNCTION CompTest_netCDF_WriteFile( &
    CompTest, &  ! Input
    Filename, &  ! Input
    Quiet   , &  ! Optional input
    New     , &  ! Optional input
    Title   , &  ! Optional input
    History , &  ! Optional input
    Comment ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    TYPE(CompTest_type),    INTENT(IN) :: CompTest
    CHARACTER(*),           INTENT(IN) :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,      OPTIONAL, INTENT(IN) :: New
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CompTest_WriteFile(netCDF)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    LOGICAL :: Noisy
    LOGICAL :: Old
    INTEGER :: NF90_Status
    INTEGER :: FileId
    TYPE(CompTest_type) :: c

    ! Set up
    err_stat = SUCCESS
    Close_File = .FALSE.
    ! ...Check structure status
    IF ( .NOT. CompTest_Associated( CompTest ) ) THEN
      msg = 'CompTest structure is empty. Nothing to do!'
      CALL Write_CleanUp(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. CompTest_Release_IsValid( CompTest ) ) THEN
      msg = 'CompTest Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check if dataset is valid
    IF ( CompTest%nM < 1 ) THEN
      msg = 'Dataset number component must be > 0.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet
    ! ...Check New argument
    Old = .TRUE.
    IF ( PRESENT(New) ) Old = .NOT. New


    ! Open the output file
    IF ( Old .AND. File_Exists(Filename) ) THEN
      ! ...Open existing file
      NF90_Status = NF90_OPEN( Filename,NF90_WRITE,FileId )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error opening '//TRIM(Filename)//' for write access - '// &
              TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Read the global attributes
      err_stat = ReadGAtts( &
                   c, &
                   Filename, &
                   FileID    )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading global attribute from '//TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Make sure the flags match
      IF ( CompTest%Flags /= c%Flags ) THEN
        msg = 'Attempting to write an incompatible structure to '//TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    ELSE
      ! ...Create a new file
      err_stat = CreateFile( &
                   CompTest, &  ! Input
                   Filename, &  ! Input
                   FileId  , &  ! Output
                   Title   = Title  , &  ! Optional input
                   History = History, &  ! Optional input
                   Comment = Comment  )  ! Optional input
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error creating output file '//TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Write the data items
    err_stat = WriteVar( &
                 CompTest, &
                 Filename, &
                 FileId    )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing variables to output file '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Close the file
    NF90_Status = NF90_CLOSE( FileId )
    Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( Noisy ) THEN
      CALL CompTest_Info( CompTest, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileId )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing output file during error cleanup - '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION CompTest_netCDF_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_netCDF_ReadFile
!
! PURPOSE:
!       Function to read CompTest object files in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = CompTest_netCDF_ReadFile( &
!                        CompTest, &
!                        Filename, &
!                        DataSet , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! OBJECTS:
!       CompTest:       CompTest object to hold data read from file.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CompTest_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       CompTest data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       DataSet:        The dataset number (unlimited dimension) of the
!                       CompTest structure to read from file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
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
!                       attribute field of the CompTest file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the CompTest file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the CompTest file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the data read was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CompTest_netCDF_ReadFile( &
    CompTest, &  ! Output
    Filename, &  ! Input
    DataSet , &  ! Input
    Quiet   , &  ! Optional input
    Title   , &  ! Optional input
    History , &  ! Optional input
    Comment ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    TYPE(CompTest_type),    INTENT(OUT) :: CompTest
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,                INTENT(IN)  :: DataSet
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CompTest_ReadFile(netCDF)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: FileId
    INTEGER :: nIV     
    INTEGER :: nOV     
    INTEGER :: nK      
    INTEGER :: nL      
    INTEGER :: nP      
    INTEGER :: nM
    TYPE(CompTest_type) :: c      

    ! Set up
    err_stat = SUCCESS
    Close_File = .FALSE.
    ! ...Check that the file exists
    IF ( .NOT. File_Exists(Filename) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet


    ! Inquire the file to get the the dimensions and data type
    err_stat = CompTest_netCDF_InquireFile( &
                 Filename, &
                 n_InputVars     = nIV, &
                 n_OutputVars    = nOV, &
                 n_Layers        = nK , &
                 n_Spectral      = nL , &
                 n_Perturbations = nP , &
                 n_Datasets      = nM , &
                 Flags           = c%Flags )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error obtaining CompTest dimensions from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if requested dataset is valid
    IF ( DataSet < 1 ) THEN
      msg = 'Requested dataset number is < 1.'
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( DataSet > nM ) THEN
      WRITE( msg, '("Requested dataset number (",i0,") is > file maximum (",i0,")")' ) DataSet, nM
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the output structure
    CALL CompTest_Create( &
           CompTest, &
           nIV, &
           nOV, &
           n_Layers        = nK, &
           n_Spectral      = nL, &
           n_Perturbations = nP  )
    IF ( .NOT. CompTest_Associated(CompTest) ) THEN
      msg = 'Error allocating output CompTest'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Set the dataset value
    CompTest%nM = DataSet


    ! Open the file for reading
    NF90_Status = NF90_OPEN( Filename,NF90_NOWRITE,FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '//&
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.


    ! Read the global attributes
    err_stat = ReadGAtts( &
                 CompTest, &
                 Filename, &
                 FileID  , &
                 Title   = Title  , &
                 History = History, &
                 Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. CompTest_Release_IsValid( CompTest ) ) THEN
      msg = 'CompTest Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the data items
    err_stat = ReadVar( &
                 CompTest, &
                 Filename, &
                 FileId    )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading variables from input file '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Close the file
    NF90_Status = NF90_CLOSE( FileId )
    Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing inputput file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( Noisy ) THEN
      CALL CompTest_Info( CompTest, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Read_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileId )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION CompTest_netCDF_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_netCDF_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CompTest_netCDF_IOVersion( Id )
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

  SUBROUTINE CompTest_netCDF_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CompTest_netCDF_IOVersion


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       Test_Names
!
! PURPOSE:
!       Subroutine to generate the various names for the test quantities 
!       in the output file based on the CompTest object test type.
!
! CALLING SEQUENCE:
!       CALL Test_Names ( CompTest, &
!                         d1_VarName     = d1_VarName    , &
!                         d2_VarName     = d2_VarName    , &
!                         d1_Description = d1_Description, &
!                         d2_Description = d2_Description, &
!                         d1_LongName    = d1_LongName   , &
!                         d2_LongName    = d2_LongName     )
!
! OBJECTS:
!       CompTest:         CompTest object about which test type info is required.
!                         UNITS:      N/A
!                         TYPE:       TYPE(CompTest_type)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       d1_VarName:       The variable name for the first test quantity.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       d2_VarName:       The variable name for the second test quantity.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       d1_Description:   The variable description for the first test quantity.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       d2_Description:   The variable description for the second test quantity.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       d1_LongName:      The netCDF longname for the first test quantity.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       d2_LongName:      The netCDF longname for the second test quantity.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!------------------------------------------------------------------------------

  SUBROUTINE Test_Names( &
    CompTest                      , &  ! Input
    d1_VarName    , d2_VarName    , &  ! Optional output
    d1_Description, d2_Description, &  ! Optional output
    d1_LongName   , d2_LongName     )  ! Optional output
    ! Arguments
    TYPE(CompTest_type),    INTENT(IN)  :: CompTest
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: d1_VarName    , d2_VarName    
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: d1_Description, d2_Description
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: d1_LongName   , d2_LongName   
    ! Local parameters
    INTEGER, PARAMETER :: LL = 256
    ! Local variables
    CHARACTER(LL) :: l_d1_VarName    , l_d2_VarName
    CHARACTER(LL) :: l_d1_Description, l_d2_Description
    CHARACTER(LL) :: l_d1_LongName   , l_d2_LongName
    ! Assign values to the local variables based on the test type
    IF ( CompTest_Is_FWDTL(CompTest) ) THEN
      l_d1_VarName     = D1_FWDTL_VARNAME
      l_d1_Description = D1_FWDTL_DESCRIPTION
      l_d1_LongName    = D1_FWDTL_LONGNAME
      l_d2_VarName     = D2_FWDTL_VARNAME
      l_d2_Description = D2_FWDTL_DESCRIPTION
      l_d2_LongName    = D2_FWDTL_LONGNAME
    ELSE IF ( CompTest_Is_TLAD(CompTest) ) THEN
      l_d1_VarName     = D1_TLAD_VARNAME
      l_d1_Description = D1_TLAD_DESCRIPTION
      l_d1_LongName    = D1_TLAD_LONGNAME
      l_d2_VarName     = D2_TLAD_VARNAME
      l_d2_Description = D2_TLAD_DESCRIPTION
      l_d2_LongName    = D2_TLAD_LONGNAME
    ELSE IF ( CompTest_Is_ADK(CompTest) ) THEN
      l_d1_VarName     = D1_ADK_VARNAME
      l_d1_Description = D1_ADK_DESCRIPTION
      l_d1_LongName    = D1_ADK_LONGNAME
      l_d2_VarName     = D2_ADK_VARNAME
      l_d2_Description = D2_ADK_DESCRIPTION
      l_d2_LongName    = D2_ADK_LONGNAME
    ELSE
      l_d1_VarName     = D1_DEFAULT_VARNAME
      l_d1_Description = D1_DEFAULT_DESCRIPTION
      l_d1_LongName    = D1_DEFAULT_LONGNAME
      l_d2_VarName     = D2_DEFAULT_VARNAME
      l_d2_Description = D2_DEFAULT_DESCRIPTION
      l_d2_LongName    = D2_DEFAULT_LONGNAME
    END IF
    IF ( PRESENT(d1_VarName    ) ) d1_VarName     = l_d1_VarName   
    IF ( PRESENT(d1_Description) ) d1_Description = l_d1_Description
    IF ( PRESENT(d1_LongName   ) ) d1_LongName    = l_d1_LongName  
    IF ( PRESENT(d2_VarName    ) ) d2_VarName     = l_d2_VarName   
    IF ( PRESENT(d2_Description) ) d2_Description = l_d2_Description
    IF ( PRESENT(d2_LongName   ) ) d2_LongName    = l_d2_LongName  
  END SUBROUTINE Test_Names


!------------------------------------------------------------------------------
!
! NAME:
!       Spectral_Names
!
! PURPOSE:
!       Subroutine to generate the various names for the spectral variable
!       in the output file based on the CompTest object data type.
!
! CALLING SEQUENCE:
!       CALL Spectral_Names( CompTest, &
!                            DimName     = DimName    , &
!                            VarName     = VarName    , &
!                            Description = Description, &
!                            LongName    = LongName   , &
!                            Units       = Units        )
!
! OBJECTS:
!       CompTest:         CompTest object about which spectral data type
!                         info is required.
!                         UNITS:      N/A
!                         TYPE:       TYPE(CompTest_type)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       DimName:          The spectral dimension name.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       VarName:          The spectral variable name.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Description:      The spectral variable description.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       LongName:         The spectral variable netCDF longname.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Units:            The spectral variable units.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!------------------------------------------------------------------------------

  SUBROUTINE Spectral_Names( &
    CompTest   , &  ! Input
    DimName    , &  ! Optional output
    VarName    , &  ! Optional output
    Description, &  ! Optional output
    LongName   , &  ! Optional output
    Units        )  ! Optional output
    ! Arguments
    TYPE(CompTest_type),    INTENT(IN)  :: CompTest
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: DimName    
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: VarName    
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Description
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: LongName   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Units      
    ! Local parameters
    INTEGER, PARAMETER :: LL = 256
    ! Local variables
    CHARACTER(LL) :: l_DimName
    CHARACTER(LL) :: l_VarName
    CHARACTER(LL) :: l_Description
    CHARACTER(LL) :: l_LongName
    CHARACTER(LL) :: l_Units
    ! Assign values to the local variables based on data type
    IF ( CompTest_Is_Frequency(CompTest) ) THEN
      l_DimName     = FREQUENCY_DIMNAME
      l_VarName     = FREQUENCY_VARNAME
      l_Description = FREQUENCY_DESCRIPTION
      l_LongName    = FREQUENCY_LONGNAME
      l_Units       = FREQUENCY_UNITS
    ELSE
      l_DimName     = CHANNEL_DIMNAME
      l_VarName     = CHANNEL_VARNAME
      l_Description = CHANNEL_DESCRIPTION
      l_LongName    = CHANNEL_LONGNAME
      l_Units       = CHANNEL_UNITS
    END IF
    ! Assign return values
    IF ( PRESENT(DimName    ) ) DimName     = l_DimName    
    IF ( PRESENT(VarName    ) ) VarName     = l_VarName    
    IF ( PRESENT(Description) ) Description = l_Description
    IF ( PRESENT(LongName   ) ) LongName    = l_LongName   
    IF ( PRESENT(Units      ) ) Units       = l_Units      
  END SUBROUTINE Spectral_Names
  
      
!------------------------------------------------------------------------------
!
! NAME:
!       WriteGAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF CompTest
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = WriteGAtts( CompTest, &
!                                  Filename, &
!                                  FileId  , &
!                                  Title   = Title   , &
!                                  History = History , &
!                                  Comment = Comment   )
!
! OBJECTS:
!       CompTest:         CompTest object containing data to be written as
!                         global attributes.
!                         UNITS:      N/A
!                         TYPE:       TYPE(CompTest_type)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:         Character string specifying the name of the
!                         netCDF CompTest file in which the global
!                         attributes are to be written.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       FileID:           NetCDF file ID number returned from netCDF
!                         open or create function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF CompTest file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF CompTest file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF CompTest file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the global attribute write was successful.
!                            == FAILURE an error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION WriteGAtts( &
    CompTest, &  ! Input
    Filename, &  ! Input
    FileID  , &  ! Input
    Title   , &  ! Optional input
    History , &  ! Optional input
    Comment ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    TYPE(CompTest_type),    INTENT(IN) :: CompTest
    CHARACTER(*),           INTENT(IN) :: Filename
    INTEGER     ,           INTENT(IN) :: FileID
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CompTest_WriteGAtts(netCDF)'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: GAttName
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone
    INTEGER :: NF90_Status

    ! Set up
    err_stat = SUCCESS
    msg = ' '


    ! Mandatory global attributes
    ! ...Software ID
    GAttName = WRITE_MODULE_HISTORY_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),MODULE_VERSION_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    GAttName = CREATION_DATE_AND_TIME_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName), &
                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                czone//'UTC' )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),CompTest%Release )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The Version
    GAttName = VERSION_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),CompTest%Version )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The Flags
    GAttName = FLAGS_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),CompTest%Flags )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! Optional global attributes
    ! ...The Title
    IF ( PRESENT(Title) ) THEN
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),Title )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The History
    IF ( PRESENT(History) ) THEN
      GAttName = HISTORY_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),History )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The Comment
    IF ( PRESENT(Comment) ) THEN
      GAttName = COMMENT_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),Comment )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    
 CONTAINS
  
    SUBROUTINE WriteGAtts_CleanUp()
      NF90_Status = NF90_CLOSE( FileId )
      IF ( NF90_Status /= NF90_NOERR ) &
        msg = '; Error closing input file during error cleanup - '//&
              TRIM(NF90_STRERROR( NF90_Status ) )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(GAttName)//' attribute to '//&
                            TRIM(Filename)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status ) )//TRIM(msg), &
                            err_stat )
    END SUBROUTINE WriteGAtts_CleanUp
    
  END FUNCTION WriteGAtts


!------------------------------------------------------------------------------
!
! NAME:
!       ReadGAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF CompTest
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = ReadGAtts( CompTest, &
!                                 Filename, &
!                                 FileId  , &
!                                 Title   = Title   , &
!                                 History = History , &
!                                 Comment = Comment   )
!
! OBJECTS:
!       CompTest:         CompTest object to contain data read as
!                         global attributes.
!                         UNITS:      N/A
!                         TYPE:       TYPE(CompTest_type)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       Filename:         Character string specifying the name of the
!                         netCDF CompTest format data file to read from.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       FileId:           NetCDF file ID number.
!                         function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF CompTest file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF CompTest file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF CompTest file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the global attribute read was successful.
!                            == FAILURE an error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION ReadGAtts( &
    CompTest, &  ! In/output
    Filename, &  ! Input
    FileID  , &  ! Input
    Title   , &  ! Optional output
    History , &  ! Optional output
    Comment ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    TYPE(CompTest_type)   , INTENT(IN OUT) :: CompTest
    CHARACTER(*),           INTENT(IN)     :: Filename
    INTEGER     ,           INTENT(IN)     :: FileId
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CompTest_ReadGAtts(netCDF)'
    ! Local variables
    CHARACTER(ML)   :: msg
    CHARACTER(256)  :: GAttName
    CHARACTER(5000) :: GAttString
    INTEGER :: NF90_Status

    ! Set up
    err_stat = SUCCESS

    ! The object global attributes
    ! ...The Version
    GAttName = VERSION_GATTNAME
    NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),CompTest%Version )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL ReadGAtts_Cleanup(); RETURN
    END IF
    ! ...The Flags
    GAttName = FLAGS_GATTNAME
    NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),CompTest%Flags )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL ReadGAtts_Cleanup(); RETURN
    END IF

    ! The optional argument global attributes
    ! ...The title
    IF ( PRESENT(title) ) THEN
      GAttName = TITLE_GATTNAME; GAttString = ''
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( GAttString )
      title = GAttString(1:MIN(LEN(title), LEN_TRIM(GAttString)))
    END IF
    ! ...The history
    IF ( PRESENT(history) ) THEN
      GAttName = HISTORY_GATTNAME; GAttString = ''
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( GAttString )
      history = GAttString(1:MIN(LEN(history), LEN_TRIM(GAttString)))
    END IF
    ! ...The comment
    IF ( PRESENT(comment) ) THEN
      GAttName = COMMENT_GATTNAME; GAttString = ''
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF        
      CALL StrClean( GAttString )
      comment = GAttString(1:MIN(LEN(comment), LEN_TRIM(GAttString)))
    END IF
    
  CONTAINS

    SUBROUTINE ReadGAtts_CleanUp()
      err_stat = FAILURE
      msg = 'Error reading '//TRIM(GAttName)//' attribute from '//TRIM(Filename)//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ) )
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE ReadGAtts_CleanUp

  END FUNCTION ReadGAtts


!------------------------------------------------------------------------------
!
! NAME:
!       WriteVar
!
! PURPOSE:
!       Function to write the CompTest variables in an output
!       netCDF file in which they have been defined.
!
! CALLING SEQUENCE:
!       Error_Status = WriteVar( CompTest, &
!                                Filename, &
!                                FileId    )
!
! OBJECTS:
!       CompTest:         CompTest object containing data to write.
!                         UNITS:      N/A
!                         TYPE:       TYPE(CompTest_type)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! INPUTS
!       Filename:         Character string specifying the name of the
!                         already created netCDF CompTest format file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       FileID:           NetCDF file ID number of the file in which
!                         the variables are to be written.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! SIDE EFFECTS:
!       If an error occurs, the netCDF file is closed.
!
!------------------------------------------------------------------------------

  FUNCTION WriteVar( &
    CompTest, &  ! Input
    Filename, &  ! Input
    FileId  ) &  ! Input
  RESULT( err_stat )
    ! Arguments
    TYPE(CompTest_type), INTENT(IN)  :: CompTest
    CHARACTER(*)       , INTENT(IN)  :: Filename
    INTEGER            , INTENT(IN)  :: FileId
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'WriteVar'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: NF90_Status
    INTEGER :: VarId
    CHARACTER(256) :: Spectral_VarName, d1_VarName, d2_VarName
                               
    ! Set up
    err_stat = SUCCESS


    ! Write the variable data
    ! ...Pressure variable
    NF90_Status = NF90_INQ_VARID( FileId,PRESSURE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//PRESSURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( &
      FileId, &
      VarID,  &
      CompTest%Pressure )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//PRESSURE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! ...Spectral variable
    CALL Spectral_Names( CompTest, VarName = Spectral_VarName )
    NF90_Status = NF90_INQ_VARID( FileId,TRIM(Spectral_VarName),VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(Spectral_VarName)//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( &
      FileId, &
      VarID,  &
      CompTest%Spectral )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TRIM(Spectral_VarName)//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! ...Perturbation variable
    NF90_Status = NF90_INQ_VARID( FileId,PERTURBATION_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//PERTURBATION_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( &
      FileId, &
      VarID,  &
      CompTest%Perturbation )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//PERTURBATION_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! ...Input_Variable_Name variable
    NF90_Status = NF90_INQ_VARID( FileId,INPUT_VARIABLE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//INPUT_VARIABLE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( &
      FileId, &
      VarID,  &
      CompTest%Input_Variable_Name )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//INPUT_VARIABLE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! ...Input_Variable_Units variable
    NF90_Status = NF90_INQ_VARID( FileId,INPUT_UNITS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//INPUT_UNITS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( &
      FileId, &
      VarID,  &
      CompTest%Input_Variable_Units )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//INPUT_UNITS_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! ...Output_Variable_Name variable
    NF90_Status = NF90_INQ_VARID( FileId,OUTPUT_VARIABLE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//OUTPUT_VARIABLE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( &
      FileId, &
      VarID,  &
      CompTest%Output_Variable_Name )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//OUTPUT_VARIABLE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! ...Output_Variable_Units variable
    NF90_Status = NF90_INQ_VARID( FileId,OUTPUT_UNITS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//OUTPUT_UNITS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( &
      FileId, &
      VarID,  &
      CompTest%Output_Variable_Units )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//OUTPUT_UNITS_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! ...DataSet_Name variable
    NF90_Status = NF90_INQ_VARID( FileId,DATASET_NAME_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//DATASET_NAME_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( &
      FileId, &
      VarID,  &
      CompTest%nM_Name, &
      START=(/1,CompTest%nM/) )    
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//DATASET_NAME_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! ...d1 variable
    CALL Test_Names( CompTest, d1_VarName = d1_VarName )
    NF90_Status = NF90_INQ_VARID( FileId,TRIM(d1_VarName),VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(d1_VarName)//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( &
      FileId, &
      VarID,  &
      CompTest%d1, &
      START=(/1,1,1,1,1,CompTest%nM/) )    
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TRIM(d1_VarName)//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! ...d2 variable
    CALL Test_Names( CompTest, d2_VarName = d2_VarName )
    NF90_Status = NF90_INQ_VARID( FileId,TRIM(d2_VarName),VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(d2_VarName)//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( &
      FileId, &
      VarID,  &
      CompTest%d2, &
      START=(/1,1,1,1,1,CompTest%nM/) )    
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TRIM(d2_VarName)//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE WriteVar_CleanUp()
      NF90_Status = NF90_CLOSE( FileId )
      IF ( NF90_Status /= NF90_NOERR ) &
        msg = TRIM(msg)//'; Error closing output file during error cleanup - '//&
              TRIM(NF90_STRERROR( NF90_Status ) )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE WriteVar_CleanUp

  END FUNCTION WriteVar


!------------------------------------------------------------------------------
!
! NAME:
!       ReadVar
!
! PURPOSE:
!       Function to read the CompTest variables from an input
!       netCDF file in which they have been defined.
!
! CALLING SEQUENCE:
!       Error_Status = ReadVar( CompTest, &
!                               Filename, &
!                               FileId    )
!
! OBJECTS:
!       CompTest:         CompTest object in which the data is placed.
!                         UNITS:      N/A
!                         TYPE:       TYPE(CompTest_type)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS
!       Filename:         Character string specifying the name of the
!                         already netCDF CompTest format file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       FileID:           NetCDF file ID number of the file from which
!                         the variables are to be read.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! SIDE EFFECTS:
!       If an error occurs, the netCDF file is closed.
!
!------------------------------------------------------------------------------

  FUNCTION ReadVar( &
    CompTest, &  ! Input
    Filename, &  ! Input
    FileId  ) &  ! Input
  RESULT( err_stat )
    ! Arguments
    TYPE(CompTest_type), INTENT(IN OUT) :: CompTest
    CHARACTER(*)       , INTENT(IN)     :: Filename
    INTEGER            , INTENT(IN)     :: FileId
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadVar'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: NF90_Status
    INTEGER :: VarId
    CHARACTER(256) :: Spectral_VarName, d1_VarName, d2_VarName
                               
    ! Set up
    err_stat = SUCCESS


    ! Read the variable data
    ! ...Pressure variable
    NF90_Status = NF90_INQ_VARID( FileId,PRESSURE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//PRESSURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( &
      FileId, &
      VarID,  &
      CompTest%Pressure )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//PRESSURE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! ...Spectral variable
    CALL Spectral_Names( CompTest, VarName = Spectral_VarName )
    NF90_Status = NF90_INQ_VARID( FileId,TRIM(Spectral_VarName),VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(Spectral_VarName)//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( &
      FileId, &
      VarID,  &
      CompTest%Spectral )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TRIM(Spectral_VarName)//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! ...Perturbation variable
    NF90_Status = NF90_INQ_VARID( FileId,PERTURBATION_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//PERTURBATION_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( &
      FileId, &
      VarID,  &
      CompTest%Perturbation )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//PERTURBATION_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! ...Input_Variable_Name variable
    NF90_Status = NF90_INQ_VARID( FileId,INPUT_VARIABLE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//INPUT_VARIABLE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( &
      FileId, &
      VarID,  &
      CompTest%Input_Variable_Name )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//INPUT_VARIABLE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! ...Input_Variable_Units variable
    NF90_Status = NF90_INQ_VARID( FileId,INPUT_UNITS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//INPUT_UNITS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( &
      FileId, &
      VarID,  &
      CompTest%Input_Variable_Units )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//INPUT_UNITS_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! ...Output_Variable_Name variable
    NF90_Status = NF90_INQ_VARID( FileId,OUTPUT_VARIABLE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//OUTPUT_VARIABLE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( &
      FileId, &
      VarID,  &
      CompTest%Output_Variable_Name )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//OUTPUT_VARIABLE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! ...Output_Variable_Units variable
    NF90_Status = NF90_INQ_VARID( FileId,OUTPUT_UNITS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//OUTPUT_UNITS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( &
      FileId, &
      VarID,  &
      CompTest%Output_Variable_Units )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//OUTPUT_UNITS_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! ...DataSet_Name variable
    NF90_Status = NF90_INQ_VARID( FileId,DATASET_NAME_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//DATASET_NAME_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( &
      FileId, &
      VarID,  &
      CompTest%nM_Name, &
      START=(/1,CompTest%nM/) )    
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//DATASET_NAME_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    CALL StrClean( CompTest%nM_Name )
    ! ...d1 variable
    CALL Test_Names( CompTest, d1_VarName = d1_VarName )
    NF90_Status = NF90_INQ_VARID( FileId,TRIM(d1_VarName),VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(d1_VarName)//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( &
      FileId, &
      VarID,  &
      CompTest%d1, &
      START=(/1,1,1,1,1,CompTest%nM/) )    
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TRIM(d1_VarName)//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! ...d2 variable
    CALL Test_Names( CompTest, d2_VarName = d2_VarName )
    NF90_Status = NF90_INQ_VARID( FileId,TRIM(d2_VarName),VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(d2_VarName)//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( &
      FileId, &
      VarID,  &
      CompTest%d2, &
      START=(/1,1,1,1,1,CompTest%nM/) )    
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TRIM(d2_VarName)//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE ReadVar_CleanUp()
      NF90_Status = NF90_CLOSE( FileId )
      IF ( NF90_Status /= NF90_NOERR ) &
        msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
              TRIM(NF90_STRERROR( NF90_Status ) )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE ReadVar_CleanUp

  END FUNCTION ReadVar


!------------------------------------------------------------------------------
!
! NAME:
!       CreateFile
!
! PURPOSE:
!       Function to create a netCDF CompTest data file for writing.
!
! CALLING SEQUENCE:
!       Error_Status = CreateFile( CompTest, &
!                                  Filename, &
!                                  FileId  , &
!                                  Title   = Title  , &
!                                  History = History, &
!                                  Comment = Comment  )
!
! OBJECTS:
!       CompTest:       CompTest object template used to create an output file
!                       UNITS:      N/A
!                       TYPE:       TYPE(CompTest_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:           Character string specifying the name of the
!                           netCDF CompTest format data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       FileID:             NetCDF file ID number to be used for subsequent
!                           writing to the output file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF CompTest file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF CompTest file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF CompTest file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.  
!                           The error codes are defined in the Message_Handler module. 
!                           If == SUCCESS the netCDF file creation was successful.     
!                              == FAILURE an error occurred.
!                           UNITS:      N/A                                            
!                           TYPE:       INTEGER                                        
!                           DIMENSION:  Scalar                                         
!
!------------------------------------------------------------------------------

  FUNCTION CreateFile( &
    CompTest, &  ! Input
    Filename, &  ! Input
    FileId  , &  ! Output
    Title   , &  ! Optional input
    History , &  ! Optional input
    Comment ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    TYPE(CompTest_type)   , INTENT(IN)  :: CompTest
    CHARACTER(*)          , INTENT(IN)  :: Filename
    INTEGER               , INTENT(OUT) :: FileID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CompTest_CreateFile(netCDF)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    INTEGER :: NF90_Status
    INTEGER :: nK_DimID
    INTEGER :: nL_DimID
    INTEGER :: nP_DimID
    INTEGER :: nIV_DimID
    INTEGER :: nOV_DimID
    INTEGER :: vsl_DimID
    INTEGER :: nM_DimID
    INTEGER :: varID
    INTEGER :: Put_Status(4)
    CHARACTER(256) :: Spectral_DimName
    CHARACTER(256) :: Spectral_VarName
    CHARACTER(256) :: Spectral_Description
    CHARACTER(256) :: Spectral_LongName
    CHARACTER(256) :: Spectral_Units
    CHARACTER(256) :: d1_VarName
    CHARACTER(256) :: d1_Description
    CHARACTER(256) :: d1_LongName
    CHARACTER(256) :: d2_VarName
    CHARACTER(256) :: d2_Description
    CHARACTER(256) :: d2_LongName
    

    ! Setup
    err_stat = SUCCESS
    Close_File = .FALSE.
    ! ...Check input object
    IF ( .NOT. CompTest_Associated(CompTest) ) THEN
      msg = 'CompTest object is empty!'
      CALL Create_Cleanup(); RETURN
    END IF
    IF ( .NOT. CompTest_Is_Valid(CompTest) ) THEN
      msg = 'CompTest object has invalid test type'
      CALL Create_Cleanup(); RETURN
    END IF


    ! Create the data file
    NF90_Status = NF90_CREATE( Filename,NF90_CLOBBER,FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(Filename)//' - '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.


    ! Define the dimensions
    ! ...The n_Layers dimension
    NF90_Status = NF90_DEF_DIM( FileID,LAYER_DIMNAME,CompTest%nK,nK_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//LAYER_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...The n_Spectral dimension
    CALL Spectral_Names( CompTest, DimName = Spectral_DimName )
    NF90_Status = NF90_DEF_DIM( FileID,TRIM(Spectral_DimName),CompTest%nL,nL_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//TRIM(Spectral_DimName)//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! The n_Perturbations dimension
    NF90_Status = NF90_DEF_DIM( FileId,PERTURBATION_DIMNAME,CompTest%nP,nP_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//PERTURBATION_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! The n_Input_Variables dimension
    NF90_Status = NF90_DEF_DIM( FileId,INPUT_VARIABLE_DIMNAME,CompTest%nIV,nIV_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//INPUT_VARIABLE_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! The n_Output_Variables dimension
    NF90_Status = NF90_DEF_DIM( FileId,OUTPUT_VARIABLE_DIMNAME,CompTest%nOV,nOV_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//OUTPUT_VARIABLE_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! The variable name string length dimension
    NF90_Status = NF90_DEF_DIM( FileId,VSL_DIMNAME,LEN(CompTest%Input_Variable_Name),vsl_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//VSL_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! The n_DataSets dimension
    NF90_Status = NF90_DEF_DIM( FileId,DATASET_DIMNAME,NF90_UNLIMITED,nM_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//DATASET_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF


    ! Write the global attributes
    err_stat = WriteGAtts( CompTest, &
                           Filename, &
                           FileId  , &
                           Title   = Title  , &
                           History = History, &
                           Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing global attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF


    ! Define the variables
    ! ...Pressure variable
    NF90_Status = NF90_DEF_VAR( FileID, &
                                PRESSURE_VARNAME, &
                                PRESSURE_TYPE, &
                                dimIDs=(/nK_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//PRESSURE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,PRESSURE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,PRESSURE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,PRESSURE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,PRESSURE_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//PRESSURE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Spectral variable
    CALL Spectral_Names( &
           CompTest, &
           VarName     = Spectral_VarName    , &
           Description = Spectral_Description, &
           LongName    = Spectral_LongName   , &
           Units       = Spectral_Units        )
    NF90_Status = NF90_DEF_VAR( FileID, &
                                TRIM(Spectral_VarName), &
                                SPECTRAL_TYPE, &
                                dimIDs=(/nL_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//TRIM(Spectral_VarName)//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,TRIM(Spectral_Description) )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,TRIM(Spectral_LongName) )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,TRIM(Spectral_Units) )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,SPECTRAL_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//TRIM(Spectral_VarName)//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Perturbation variable
    NF90_Status = NF90_DEF_VAR( FileID, &
                                PERTURBATION_VARNAME, &
                                PERTURBATION_TYPE, &
                                dimIDs=(/nP_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//PERTURBATION_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,PERTURBATION_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,PERTURBATION_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,PERTURBATION_UNITS )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,PERTURBATION_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//PERTURBATION_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Input_Variable_Name variable
    NF90_Status = NF90_DEF_VAR( FileID, &
                                INPUT_VARIABLE_VARNAME, &
                                INPUT_VARIABLE_TYPE, &
                                dimIDs=(/vsl_DimId,nIV_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//INPUT_VARIABLE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,INPUT_VARIABLE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,INPUT_VARIABLE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,INPUT_VARIABLE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,INPUT_VARIABLE_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//INPUT_VARIABLE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Input_Variable_Units variable
    NF90_Status = NF90_DEF_VAR( FileID, &
                                INPUT_UNITS_VARNAME, &
                                INPUT_UNITS_TYPE, &
                                dimIDs=(/vsl_DimId,nIV_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//INPUT_UNITS_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,INPUT_UNITS_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,INPUT_UNITS_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,INPUT_UNITS_UNITS )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,INPUT_UNITS_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//INPUT_UNITS_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Output_Variable_Name variable
    NF90_Status = NF90_DEF_VAR( FileID, &
                                OUTPUT_VARIABLE_VARNAME, &
                                OUTPUT_VARIABLE_TYPE, &
                                dimIDs=(/vsl_DimId,nOV_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//OUTPUT_VARIABLE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,OUTPUT_VARIABLE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,OUTPUT_VARIABLE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,OUTPUT_VARIABLE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,OUTPUT_VARIABLE_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//OUTPUT_VARIABLE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Output_Variable_Units variable
    NF90_Status = NF90_DEF_VAR( FileID, &
                                OUTPUT_UNITS_VARNAME, &
                                OUTPUT_UNITS_TYPE, &
                                dimIDs=(/vsl_DimId,nOV_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//OUTPUT_UNITS_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,OUTPUT_UNITS_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,OUTPUT_UNITS_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,OUTPUT_UNITS_UNITS )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,OUTPUT_UNITS_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//OUTPUT_UNITS_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...d1 variable
    CALL Test_Names( &
           CompTest, &
           d1_VarName     = d1_VarName    , &
           d1_Description = d1_Description, &
           d1_LongName    = d1_LongName     )
    NF90_Status = NF90_DEF_VAR( FileID, &
                                TRIM(d1_VarName), &
                                D1_TYPE, &
                                dimIDs=(/nK_DimID,nL_DimID,nP_DimID,nIV_DimID,nOV_DimID,nM_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//TRIM(d1_VarName)//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,TRIM(d1_Description) )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,TRIM(d1_LongName) )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,D1_UNITS )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,D1_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//TRIM(d1_VarName)//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...d2 variable
    CALL Test_Names( &
           CompTest, &
           d2_VarName     = d2_VarName    , &
           d2_Description = d2_Description, &
           d2_LongName    = d2_LongName     )
    NF90_Status = NF90_DEF_VAR( FileID, &
                                TRIM(d2_VarName), &
                                D2_TYPE, &
                                dimIDs=(/nK_DimID,nL_DimID,nP_DimID,nIV_DimID,nOV_DimID,nM_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//TRIM(d2_VarName)//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,TRIM(d2_Description) )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,TRIM(d2_LongName) )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,D2_UNITS )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,D2_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//TRIM(d2_VarName)//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...DataSet_Name variable
    NF90_Status = NF90_DEF_VAR( FileID, &
                                DATASET_NAME_VARNAME, &
                                DATASET_NAME_TYPE, &
                                dimIDs=(/vsl_DimId,nM_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//DATASET_NAME_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,DATASET_NAME_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,DATASET_NAME_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,DATASET_NAME_UNITS )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,DATASET_NAME_FILLVALUE )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//DATASET_NAME_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    

    ! Take netCDF file out of define mode
    NF90_Status = NF90_ENDDEF( FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error taking file '//TRIM(Filename)// &
            ' out of define mode - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Create_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileID )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Create_CleanUp

  END FUNCTION CreateFile

END MODULE CompTest_netCDF_IO
