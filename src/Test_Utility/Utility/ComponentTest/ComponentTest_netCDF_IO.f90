!
! ComponentTest_netCDF_IO
!
! Module containing routines to read and write netCDF format
! ComponentTest files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 02-Mar-2006
!                       paul.vandelst@noaa.gov
!

MODULE ComponentTest_netCDF_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds          , ONLY: fp
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                  Display_Message
  USE File_Utility        , ONLY: File_Exists
  USE String_Utility      , ONLY: StrClean
  USE ComponentTest_Define, ONLY: COMPONENTTEST_TESTTYPE      , &
                                  COMPONENTTEST_FWDTL_TESTTYPE, & 
                                  COMPONENTTEST_TLAD_TESTTYPE , & 
                                  COMPONENTTEST_ADK_TESTTYPE  , & 
                                  COMPONENTTEST_DATATYPE      , & 
                                  COMPONENTTEST_POLY_DATATYPE , & 
                                  COMPONENTTEST_MONO_DATATYPE , & 
                                  ComponentTest_type, &
                                  Associated_ComponentTest, &
                                  Destroy_ComponentTest, &
                                  Allocate_ComponentTest, &
                                  CheckRelease_ComponentTest, &
                                  Info_ComponentTest
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Inquire_ComponentTest_netCDF
  PUBLIC :: Write_ComponentTest_netCDF
  PUBLIC :: Read_ComponentTest_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------

  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Message string length
  INTEGER, PARAMETER :: ML = 512
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME            = 'title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME          = 'history' 
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME          = 'comment' 
  CHARACTER(*), PARAMETER :: ID_TAG_GATTNAME           = 'id_tag' 
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME          = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME          = 'Version'
  CHARACTER(*), PARAMETER :: SENSOR_ID_GATTNAME        = 'Sensor_Id'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_GATTNAME = 'WMO_Satellite_Id'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_GATTNAME    = 'WMO_Sensor_Id'
  CHARACTER(*), PARAMETER :: TESTTYPE_GATTNAME         = 'TestType'
  CHARACTER(*), PARAMETER :: DATATYPE_GATTNAME         = 'DataType'

  ! Dimension names. Case sensitive
  CHARACTER(*), PARAMETER :: LAYER_DIMNAME           = 'n_Layers'
  CHARACTER(*), PARAMETER :: POLY_DATATYPE_DIMNAME   = 'n_Channels'
  CHARACTER(*), PARAMETER :: MONO_DATATYPE_DIMNAME   = 'n_Frequencies'
  CHARACTER(*), PARAMETER :: PERTURBATION_DIMNAME    = 'n_Perturbations'
  CHARACTER(*), PARAMETER :: INPUT_VARIABLE_DIMNAME  = 'n_Input_Variables'
  CHARACTER(*), PARAMETER :: OUTPUT_VARIABLE_DIMNAME = 'n_Output_Variables'
  CHARACTER(*), PARAMETER :: VSL_DIMNAME             = 'vsl'
  CHARACTER(*), PARAMETER :: DATASET_DIMNAME         = 'n_DataSets'

  ! Variable names. Case sensitive.
  CHARACTER(*), PARAMETER :: PRESSURE_VARNAME        = 'Pressure'
  CHARACTER(*), PARAMETER :: POLY_DATATYPE_VARNAME   = 'Channel'
  CHARACTER(*), PARAMETER :: MONO_DATATYPE_VARNAME   = 'Frequency'
  CHARACTER(*), PARAMETER :: PERTURBATION_VARNAME    = 'Perturbation'
  CHARACTER(*), PARAMETER :: INPUT_VARIABLE_VARNAME  = 'Input_Variable_Name'
  CHARACTER(*), PARAMETER :: INPUT_UNITS_VARNAME     = 'Input_Variable_Units'
  CHARACTER(*), PARAMETER :: OUTPUT_VARIABLE_VARNAME = 'Output_Variable_Name'
  CHARACTER(*), PARAMETER :: OUTPUT_UNITS_VARNAME    = 'Output_Variable_Units'
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
  CHARACTER(*), PARAMETER :: POLY_DATATYPE_LONGNAME   = 'Sensor Channel'
  CHARACTER(*), PARAMETER :: MONO_DATATYPE_LONGNAME   = 'Frequency'
  CHARACTER(*), PARAMETER :: PERTURBATION_LONGNAME    = 'Perturbation fraction'
  CHARACTER(*), PARAMETER :: INPUT_VARIABLE_LONGNAME  = 'Input Variable Name'
  CHARACTER(*), PARAMETER :: INPUT_UNITS_LONGNAME     = 'Output Variable Name'
  CHARACTER(*), PARAMETER :: OUTPUT_VARIABLE_LONGNAME = 'Input Variable Units'
  CHARACTER(*), PARAMETER :: OUTPUT_UNITS_LONGNAME    = 'Output Variable Units'
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
'Layer pressures used in the Component Test.'
  CHARACTER(*), PARAMETER :: POLY_DATATYPE_DESCRIPTION = &
'Sensor channels used in the Component Test.'
  CHARACTER(*), PARAMETER :: MONO_DATATYPE_DESCRIPTION = &
'Frequencies used in the Component Test.'
  CHARACTER(*), PARAMETER :: PERTURBATION_DESCRIPTION = &
'List of the perturbation fractions applied to the variables in the Component Test.'
  CHARACTER(*), PARAMETER :: INPUT_VARIABLE_DESCRIPTION = &
'List of the input variable names in the Component Test'
  CHARACTER(*), PARAMETER :: INPUT_UNITS_DESCRIPTION = &
'List of the input variable units in the Component Test'
  CHARACTER(*), PARAMETER :: OUTPUT_VARIABLE_DESCRIPTION = &
'List of the output variable names in the Component Test'
  CHARACTER(*), PARAMETER :: OUTPUT_UNITS_DESCRIPTION = &
'List of the output variable units in the Component Test'
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

  CHARACTER(*), PARAMETER :: PRESSURE_UNITS      = 'hPa'
  CHARACTER(*), PARAMETER :: POLY_DATATYPE_UNITS = 'Channel number'
  CHARACTER(*), PARAMETER :: MONO_DATATYPE_UNITS = 'cm^-1 or GHz'
  CHARACTER(*), PARAMETER :: PERTURBATION_UNITS  = 'Variable. Input dependent.'
  CHARACTER(*), PARAMETER :: D1_UNITS            = 'Variable'
  CHARACTER(*), PARAMETER :: D2_UNITS            = 'Variable'

  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'

  REAL(fp), PARAMETER :: PRESSURE_FILLVALUE     = ZERO
  REAL(fp), PARAMETER :: SPECTRAL_FILLVALUE     = ZERO
  REAL(fp), PARAMETER :: PERTURBATION_FILLVALUE = ZERO
  REAL(fp), PARAMETER :: D1_FILLVALUE           = ZERO
  REAL(fp), PARAMETER :: D2_FILLVALUE           = ZERO

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
!       Inquire_ComponentTest_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF ComponentTest format file to obtain the 
!       dimensions and global attributes.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_ComponentTest_netCDF( NC_Filename                      , &
!                                                    nK              =nK              , &
!                                                    nL              =nL              , &
!                                                    nP              =nP              , &
!                                                    nIV             =nIV             , &
!                                                    nOV             =nOV             , &
!                                                    nM              =nM              , &
!                                                    TestType        =TestType        , &
!                                                    DataType        =DataType        , &
!                                                    Release         =Release         , &
!                                                    Version         =Version         , &
!                                                    Sensor_Id       =Sensor_Id       , &
!                                                    WMO_Satellite_Id=WMO_Satellite_Id, &
!                                                    WMO_Sensor_Id   =WMO_Sensor_Id   , &
!                                                    Title           =Title           , &
!                                                    History         =History         , &
!                                                    Comment         =Comment         , &
!                                                    ID_Tag          =ID_Tag          , &
!                                                    RCS_Id          =RCS_Id          , &
!                                                    Message_Log     =Message_Log       )
!
! INPUT ARGUMENTS:
!       NC_Filename:       Character string specifying the name of the netCDF
!                          format ComponentTest data file to inquire.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       nK:                The number of layers dimension of the
!                          ComponentTest data.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       nL:                The spectral dimension (channels/frequencies) of
!                          the ComponentTest data.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       nP:                The number of perturbations dimension of the
!                          ComponentTest data.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       nIV:               The number of input variables dimension of the
!                          ComponentTest data.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       nOV:               The number of output variables dimension of the
!                          ComponentTest data.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       nM:                The number of datasets in the ComponentTest datafile.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       TestType:          Integer flag indicating whether the test type is
!                          FWD/TL, TL/AD, or AD/K.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       DataType:          Integer flag indicating whether the spectral dimension
!                          is polychromatic or monochromatic.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:           The release number of the netCDF ComponentTest file.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:           The version number of the netCDF ComponentTest file.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Id:         Character string sensor/platform identifier.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_Id:  The WMO code used to identify satellite platforms.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_Id:     The WMO code used to identify sensors.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:             Character string written into the TITLE global
!                          attribute field of the netCDF ComponentTest file.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:           Character string written into the HISTORY global
!                          attribute field of the netCDF ComponentTest file.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:           Character string written into the COMMENT global
!                          attribute field of the netCDF ComponentTest file.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       ID_Tag:            Character string written into the ID_TAG global
!                          attribute field of the netCDF ComponentTest file.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the Message_Handler module.
!                          If == SUCCESS the netCDF file inquiry was successful
!                             == FAILURE an error occurred reading any of the requested
!                                        dimension or release/version data.
!                             == WARNING - an error occurred reading any of the requested
!                                          global file attributes, or
!                                        - an error occurred closing the netCDF file.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Inquire_ComponentTest_netCDF( NC_Filename     , &  ! Input
                                         nK              , &  ! Optional output
                                         nL              , &  ! Optional output
                                         nP              , &  ! Optional output
                                         nIV             , &  ! Optional output
                                         nOV             , &  ! Optional output
                                         nM              , &  ! Optional output
                                         TestType        , &  ! Optional output
                                         DataType        , &  ! Optional output
                                         Release         , &  ! Optional output
                                         Version         , &  ! Optional output
                                         Sensor_Id       , &  ! Optional output
                                         WMO_Satellite_Id, &  ! Optional output
                                         WMO_Sensor_Id   , &  ! Optional output
                                         Title           , &  ! Optional output
                                         History         , &  ! Optional output
                                         Comment         , &  ! Optional output
                                         ID_Tag          , &  ! Optional output
                                         RCS_Id          , &  ! Version control
                                         Message_Log     ) &  ! Error messaging
                                       RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: nK
    INTEGER     , OPTIONAL, INTENT(OUT) :: nL
    INTEGER     , OPTIONAL, INTENT(OUT) :: nP
    INTEGER     , OPTIONAL, INTENT(OUT) :: nIV
    INTEGER     , OPTIONAL, INTENT(OUT) :: nOV
    INTEGER     , OPTIONAL, INTENT(OUT) :: nM
    INTEGER     , OPTIONAL, INTENT(OUT) :: TestType
    INTEGER     , OPTIONAL, INTENT(OUT) :: DataType
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_ComponentTest_netCDF'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: NC_FileID
    INTEGER :: NF90_STATUS
    INTEGER :: File_DataType
    INTEGER :: DimId
    INTEGER :: n_Layers
    INTEGER :: n_f
    INTEGER :: n_Perturbations
    INTEGER :: n_Input_Variables
    INTEGER :: n_Output_Variables
    INTEGER :: n_DataSets
    CHARACTER(256) :: f_DimName

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID


    ! Open the file
    ! -------------
    NF90_Status = NF90_OPEN( NC_Filename,NF90_NOWRITE,NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(NC_Filename)//' for read access - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Get the data type for the spectral dimension name
    ! -------------------------------------------------
    ! Get the data type attribute
    Error_Status = ReadGAtts( NC_Filename              , &
                              NC_FileID                , &
                              DataType   =File_DataType, &
                              Message_Log=Message_Log    )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error reading DataType attribute from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! Assign the appropriate spectral dimension name
    CALL fNames( File_DataType,f_DimName=f_DimName )


    ! Get the dimensions
    ! ------------------
    ! Get the n_Layers dimension
    NF90_Status = NF90_INQ_DIMID( NC_FileId,LAYER_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//LAYER_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileId,DimId,Len=n_Layers )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//LAYER_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Get the Spectral dimension
    NF90_Status = NF90_INQ_DIMID( NC_FileId,TRIM(f_DimName),DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//TRIM(f_DimName)//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileId,DimId,Len=n_f )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//TRIM(f_DimName)//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Get the n_Perturbations dimension
    NF90_Status = NF90_INQ_DIMID( NC_FileId,PERTURBATION_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//PERTURBATION_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileId,DimId,Len=n_Perturbations )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//PERTURBATION_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Get the n_Input_Variables dimension
    NF90_Status = NF90_INQ_DIMID( NC_FileId,INPUT_VARIABLE_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//INPUT_VARIABLE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileId,DimId,Len=n_Input_Variables )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//INPUT_VARIABLE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Get the n_Output_Variables dimension
    NF90_Status = NF90_INQ_DIMID( NC_FileId,OUTPUT_VARIABLE_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//OUTPUT_VARIABLE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileId,DimId,Len=n_Output_Variables )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//OUTPUT_VARIABLE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Get the n_DataSets dimension
    NF90_Status = NF90_INQ_DIMID( NC_FileId,DATASET_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//DATASET_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileId,DimId,Len=n_DataSets )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//DATASET_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Get the global attributes
    ! -------------------------
    Error_Status = ReadGAtts( NC_Filename                       , &
                              NC_FileID                         , &
                              TestType         =TestType        , &
                              DataType         =DataType        , &
                              Release          =Release         , &
                              Version          =Version         , &
                              Sensor_Id        =Sensor_Id       , &
                              WMO_Satellite_Id =WMO_Satellite_Id, &
                              WMO_Sensor_Id    =WMO_Sensor_Id   , &
                              ID_Tag           =ID_Tag          , &
                              Title            =Title           , &
                              History          =History         , &
                              Comment          =Comment         , &
                              Message_Log      =Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error reading global attributes from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    ! --------------
    NF90_Status = NF90_CLOSE( NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the dimension return values
    ! -------------------------------
    IF ( PRESENT(nK)  ) nK  = n_Layers
    IF ( PRESENT(nL)  ) nL  = n_f
    IF ( PRESENT(nP)  ) nP  = n_Perturbations
    IF ( PRESENT(nIV) ) nIV = n_Input_Variables
    IF ( PRESENT(nOV) ) nOV = n_Output_Variables
    IF ( PRESENT(nM)  ) nM  = n_DataSets
    
  CONTAINS
  
    SUBROUTINE Inquire_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          NF90_Status = NF90_CLOSE( NC_FileId )
          IF ( NF90_Status /= NF90_NOERR ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ))
        END IF
      END IF
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION Inquire_ComponentTest_netCDF


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Write_ComponentTest_netCDF
!
! PURPOSE:
!       Function to write ComponentTest data to a netCDF format ComponentTest file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_ComponentTest_netCDF( NC_Filename            , &
!                                                  ComponentTest          , &
!                                                  Quiet      =Quiet      , &
!                                                  New        =New        , &
!                                                  Title      =Title      , &
!                                                  History    =History    , &
!                                                  Comment    =Comment    , &
!                                                  ID_Tag     =ID_Tag     , &
!                                                  RCS_Id     =RCS_Id     , &
!                                                  Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the
!                        netCDF format ComponentTest data file to write to.
!                        If the file does not exist, it is created.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ComponentTest:   Structure containing the ComponentTest data to
!                        write to file.
!                        UNITS:      N/A
!                        TYPE:       ComponentTest_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information msgs being
!                        printed to standard output (or the msg log file if
!                        the Message_Log optional argument is used.) By default,
!                        information msgs are printed.
!                        If QUIET = .TRUE. , information msgs are OUTPUT. [DEFAULT]
!                           QUIET = .FALSE., information msgs are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       LOGICAL
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       New:             Set this argument to write the ComponentTest structure
!                        data to a new file. Default action is to write to
!                        an existing file.
!                        If NEW = .TRUE. , data is written to an existing file. [DEFAULT]
!                           NEW = .FALSE., a new file is created for output.
!                        UNITS:      N/A
!                        TYPE:       LOGICAL
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF ComponentTest file.
!                        Should contain a succinct description of what
!                        is in the netCDF datafile.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF ComponentTest file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF ComponentTest file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       ID_Tag:          Character string written into the ID_TAG global
!                        attribute field of the netCDF ComponentTest file.
!                        Should contain a short tag used to identify the
!                        profile set.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the netCDF file write was successful
!                           == FAILURE an unrecoverable error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Write_ComponentTest_netCDF( NC_Filename, &  ! Input
                                       cTest      , &  ! Input
                                       Quiet      , &  ! Optional input
                                       New        , &  ! Optional input
                                       Title      , &  ! Optional input
                                       History    , &  ! Optional input
                                       Comment    , &  ! Optional input
                                       ID_Tag     , &  ! Optional input
                                       RCS_Id     , &  ! Version control
                                       Message_Log) &  ! Error messaging
                                     RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),             INTENT(IN)  :: NC_Filename
    TYPE(ComponentTest_type), INTENT(IN)  :: cTest
    LOGICAL     ,   OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL     ,   OPTIONAL, INTENT(IN)  :: New
    CHARACTER(*),   OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*),   OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*),   OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*),   OPTIONAL, INTENT(IN)  :: ID_Tag
    CHARACTER(*),   OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),   OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_ComponentTest_netCDF'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    LOGICAL :: Old
    INTEGER :: NF90_Status
    INTEGER :: NC_FileID

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Output informational msgs....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet ) Noisy = .FALSE.
    END IF

    ! Write to existing file....
    Old = File_Exists(NC_Filename)
    ! ....unless the NEW keyword is set.
    IF ( PRESENT(New) ) THEN
      IF ( New ) Old = .FALSE.
    END IF

    ! Check structure association
    IF ( .NOT. Associated_ComponentTest( cTest ) ) THEN
      msg = 'Some or all INPUT ComponentTest pointer members are NOT associated.'
      CALL Write_Cleanup(); RETURN
    END IF

    ! Check the dataset number
    IF ( cTest%nM < 1 ) THEN
      msg = 'Dataset number component, nM, must be > 0.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Open the output file
    ! --------------------
    IF ( Old ) THEN
      ! Open existing file
      NF90_Status = NF90_OPEN( NC_Filename,NF90_WRITE,NC_FileId )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error opening '//TRIM(NC_Filename)//' for write access - '// &
              TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
    ELSE
      ! Create a new file
      Error_Status = CreateFile( NC_Filename                             , &  ! Input
                                 cTest%nK                                , &  ! Input
                                 cTest%nL                                , &  ! Input
                                 cTest%nP                                , &  ! Input
                                 cTest%nIV                               , &  ! Input
                                 cTest%nOV                               , &  ! Input
                                 cTest%TestType                          , &  ! Input
                                 cTest%DataType                          , &  ! Input
                                 NC_FileID                               , &  ! Output
                                 Version          =cTest%Version         , &  ! Optional input
                                 Sensor_Id        =cTest%Sensor_Id       , &  ! Optional input
                                 WMO_Satellite_Id =cTest%WMO_Satellite_Id, &  ! Optional input
                                 WMO_Sensor_Id    =cTest%WMO_Sensor_Id   , &  ! Optional input
                                 ID_Tag           =ID_Tag                , &  ! Optional input
                                 Title            =Title                 , &  ! Optional input
                                 History          =History               , &  ! Optional input
                                 Comment          =Comment               , &  ! Optional input
                                 Message_Log      =Message_Log             )  ! Error messaging
      IF ( Error_Status /= SUCCESS ) THEN
        msg = 'Error creating output file '//TRIM(NC_Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Write the ComponentTest data
    ! ----------------------------
    Error_Status = WriteVar( NC_Filename            , &
                             NC_FileID              , &
                             cTest                  , &
                             Message_Log=Message_Log  )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error writing ComponentTest variables to output file '//TRIM(NC_Filename)
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    

    ! Close the file
    ! --------------
    NF90_Status = NF90_CLOSE( NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info msg
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_ComponentTest( cTest, msg )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(msg), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          NF90_Status = NF90_CLOSE( NC_FileId )
          IF ( NF90_Status /= NF90_NOERR ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ))
        END IF
      END IF
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Write_CleanUp

  END FUNCTION Write_ComponentTest_netCDF



!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Read_ComponentTest_netCDF
!
! PURPOSE:
!       Function to read ComponentTest data for a specified dataset from a
!       netCDF format ComponentTest file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_ComponentTest_netCDF( NC_Filename                , &
!                                                 nM                         , &
!                                                 ComponentTest              , &
!                                                 Quiet         = Quiet      , &
!                                                 Title         = Title      , &
!                                                 History       = History    , &
!                                                 Comment       = Comment    , &
!                                                 ID_Tag        = ID_Tag     , &
!                                                 RCS_Id        = RCS_Id     , &
!                                                 Message_Log   = Message_Log  )
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the netCDF
!                        format ComponentTest data file to read.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       nM:              The dataset number of the ComponentTest structure to
!                        read from file.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ComponentTest:   Structure to contain the spectral coefficient data read
!                        from the file.
!                        UNITS:      N/A
!                        TYPE:       ComponentTest_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information msgs being
!                        printed to standard output (or the msg log file if
!                        the Message_Log optional argument is used.) By default,
!                        information msgs are printed.
!                        If QUIET = .TRUE. , information msgs are OUTPUT.
!                           QUIET = .FALSE., information msgs are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       LOGICAL
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF ComponentTest file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF ComponentTest file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF ComponentTest file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       ID_Tag:          Character string written into the ID_TAG global
!                        attribute field of the netCDF ComponentTest file.
!                        Should contain a short tag used to identify the
!                        profile set.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the netCDF file read was successful
!                           == FAILURE an unrecoverable read error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output ComponentTest argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Read_ComponentTest_netCDF( NC_Filename , &  ! Input
                                      nM          , &  ! Input
                                      cTest       , &  ! Output
                                      Quiet       , &  ! Optional input
                                      Title       , &  ! Optional output
                                      History     , &  ! Optional output
                                      Comment     , &  ! Optional output
                                      ID_Tag      , &  ! Optional output
                                      RCS_Id      , &  ! Revision control
                                      Message_Log ) &  ! Error messaging
                                    RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)            , INTENT(IN)     :: NC_Filename
    INTEGER                 , INTENT(IN)     :: nM
    TYPE(ComponentTest_type), INTENT(IN OUT) :: cTest
    LOGICAL     ,   OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*),   OPTIONAL, INTENT(OUT)    :: Title
    CHARACTER(*),   OPTIONAL, INTENT(OUT)    :: History
    CHARACTER(*),   OPTIONAL, INTENT(OUT)    :: Comment
    CHARACTER(*),   OPTIONAL, INTENT(OUT)    :: ID_Tag
    CHARACTER(*),   OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),   OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_ComponentTest_netCDF'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: NF90_Status
    INTEGER :: nK, nL, nP, nIV, nOV
    INTEGER :: n_DataSets


    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Output informational msgs....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet ) Noisy = .FALSE.
    END IF

    
    ! Allocate the structure for the netCDF read
    ! ------------------------------------------
    ! Read the dimension values
    Error_Status = Inquire_ComponentTest_netCDF( NC_Filename   , &
                                                 nK =nK        , &
                                                 nL =nL        , &
                                                 nP =nP        , &
                                                 nIV=nIV       , &
                                                 nOV=nOV       , &
                                                 nM =n_DataSets, &
                                                 Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error obtaining dimensions from '//TRIM(NC_Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! Check the dataset value
    IF ( nM < 1 .OR. nM > n_DataSets ) THEN
      msg = 'Invalid dataset number, nM'
      CALL Read_Cleanup(); RETURN
    END IF
    ! Allocate 
    Error_Status = Allocate_ComponentTest( nK,nL,nP,nIV,nOV,cTest,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error occurred allocating ComponentTest structure.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! Save the dataset indicator
    cTest%nM = nM
    

    ! Open the netCDF file for reading
    ! --------------------------------
    NF90_Status = NF90_OPEN( NC_Filename,NF90_NOWRITE,NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(NC_Filename)//' for read access - '//&
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    

    ! Read the global attributes
    ! --------------------------
    Error_Status = ReadGAtts( NC_Filename                            , &
                              NC_FileID                              , &
                              TestType        =cTest%TestType        , &
                              DataType        =cTest%DataType        , &
                              Release         =cTest%Release         , &
                              Version         =cTest%Version         , &
                              Sensor_Id       =cTest%Sensor_Id       , &
                              WMO_Satellite_Id=cTest%WMO_Satellite_Id, &
                              WMO_Sensor_Id   =cTest%WMO_Sensor_Id   , &
                              ID_Tag          =ID_Tag                , &
                              Title           =Title                 , &
                              History         =History               , &
                              Comment         =Comment               , &
                              Message_Log     =Message_Log             )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(NC_Filename)
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    
    ! Check the release
    Error_Status = CheckRelease_ComponentTest( cTest,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'ComponentTest Release check failed for '//TRIM(NC_Filename)
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Read the ComponentTest data
    ! ---------------------------
    Error_Status = ReadVar( NC_Filename            , &
                            NC_FileID              , &
                            cTest                  , &
                            Message_Log=Message_Log  )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error reading ComponentTest variables from '//TRIM(NC_Filename)
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Close the file
    ! --------------
    NF90_Status = NF90_CLOSE( NC_FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Output an info msg
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_ComponentTest( cTest, msg )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(msg), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp( Close_File )
       LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          NF90_Status = NF90_CLOSE( NC_FileId )
          IF ( NF90_Status /= NF90_NOERR ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ))
        END IF
      END IF
      ! Destroy the structure if necessary
      IF ( Associated_ComponentTest( cTest ) ) THEN
        Error_Status = Destroy_ComponentTest(cTest, Message_Log=Message_Log)
        IF ( Error_Status /= SUCCESS ) &
          msg = TRIM(msg)//'; Error destroying ComponentTest during error cleanup.'
      END IF
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_ComponentTest_netCDF


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! Assign the names for the test type
  ! ----------------------------------
  SUBROUTINE dNames( TestType, &
                     d1_VarName    , d2_VarName    , &
                     d1_Description, d2_Description, &
                     d1_LongName   , d2_LongName     )
    INTEGER               , INTENT(IN)  :: TestType
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: d1_VarName    , d2_VarName    
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: d1_Description, d2_Description
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: d1_LongName   , d2_LongName   
    CHARACTER(256) :: VarName_d1    , VarName_d2    
    CHARACTER(256) :: Description_d1, Description_d2
    CHARACTER(256) :: LongName_d1   , LongName_d2   
    SELECT CASE ( TestType )
      CASE ( COMPONENTTEST_FWDTL_TESTTYPE )
        VarName_d1     = D1_FWDTL_VARNAME
        Description_d1 = D1_FWDTL_DESCRIPTION
        LongName_d1    = D1_FWDTL_LONGNAME
        VarName_d2     = D2_FWDTL_VARNAME
        Description_d2 = D2_FWDTL_DESCRIPTION
        LongName_d2    = D2_FWDTL_LONGNAME
      CASE ( COMPONENTTEST_TLAD_TESTTYPE )
        VarName_d1     = D1_TLAD_VARNAME
        Description_d1 = D1_TLAD_DESCRIPTION
        LongName_d1    = D1_TLAD_LONGNAME
        VarName_d2     = D2_TLAD_VARNAME
        Description_d2 = D2_TLAD_DESCRIPTION
        LongName_d2    = D2_TLAD_LONGNAME
      CASE ( COMPONENTTEST_ADK_TESTTYPE )
        VarName_d1     = D1_ADK_VARNAME
        Description_d1 = D1_ADK_DESCRIPTION
        LongName_d1    = D1_ADK_LONGNAME
        VarName_d2     = D2_ADK_VARNAME
        Description_d2 = D2_ADK_DESCRIPTION
        LongName_d2    = D2_ADK_LONGNAME
    END SELECT
    IF ( PRESENT(d1_VarName    ) ) d1_VarName     = VarName_d1    
    IF ( PRESENT(d1_Description) ) d1_Description = Description_d1
    IF ( PRESENT(d1_LongName   ) ) d1_LongName    = LongName_d1   
    IF ( PRESENT(d2_VarName    ) ) d2_VarName     = VarName_d2    
    IF ( PRESENT(d2_Description) ) d2_Description = Description_d2
    IF ( PRESENT(d2_LongName   ) ) d2_LongName    = LongName_d2   
  END SUBROUTINE dNames


  ! Assign the names for the spectral dimension
  ! -------------------------------------------
  SUBROUTINE fNames( DataType     , &
                     f_DimName    , &
                     f_VarName    , &
                     f_Description, &
                     f_LongName   , &
                     f_Units        )
    INTEGER               , INTENT(IN)  :: DataType
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: f_DimName    
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: f_VarName    
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: f_Description
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: f_LongName   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: f_Units      
    CHARACTER(256) :: DimName_f    
    CHARACTER(256) :: VarName_f     
    CHARACTER(256) :: Description_f 
    CHARACTER(256) :: LongName_f    
    CHARACTER(256) :: Units_f       
    SELECT CASE ( DataType )
      CASE ( COMPONENTTEST_POLY_DATATYPE )
        DimName_f     = POLY_DATATYPE_DIMNAME
        VarName_f     = POLY_DATATYPE_VARNAME
        Description_f = POLY_DATATYPE_DESCRIPTION
        LongName_f    = POLY_DATATYPE_LONGNAME
        Units_f       = POLY_DATATYPE_UNITS
      CASE ( COMPONENTTEST_MONO_DATATYPE )
        DimName_f     = MONO_DATATYPE_DIMNAME
        VarName_f     = MONO_DATATYPE_VARNAME
        Description_f = MONO_DATATYPE_DESCRIPTION
        LongName_f    = MONO_DATATYPE_LONGNAME
        Units_f       = MONO_DATATYPE_UNITS
    END SELECT
    IF ( PRESENT(f_DimName    ) ) f_DimName     = DimName_f    
    IF ( PRESENT(f_VarName    ) ) f_VarName     = VarName_f    
    IF ( PRESENT(f_Description) ) f_Description = Description_f
    IF ( PRESENT(f_LongName   ) ) f_LongName    = LongName_f   
    IF ( PRESENT(f_Units      ) ) f_Units       = Units_f      
  END SUBROUTINE fNames
  
      
!------------------------------------------------------------------------------
!
! NAME:
!       WriteGAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF ComponentTest
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = WriteGAtts( NC_Filename                      , &
!                                  NC_FileID                        , &
!                                  TestType                         , &
!                                  DataType                         , &
!                                  Version         =Version         , &
!                                  Sensor_Id       =Sensor_Id       , &
!                                  WMO_Satellite_Id=WMO_Satellite_Id, &
!                                  WMO_Sensor_Id   =WMO_Sensor_Id   , &
!                                  ID_Tag          =ID_Tag          , &
!                                  Title           =Title           , &
!                                  History         =History         , &
!                                  Comment         =Comment         , &
!                                  Message_Log     =Message_Log       )
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF ComponentTest format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_ComponentTest_netCDF() function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       TestType:         Integer flag indicating whether the test type is
!                         FWD/TL, TL/AD, or AD/K.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       DataType:         Integer flag indicating whether the spectral dimension
!                         is polychromatic or monochromatic.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Version:          The version number of the netCDF ComponentTest file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Sensor_Id:        Character string sensor/platform identifier.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Satellite_Id: The WMO code used to identify satellite platforms.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Sensor_Id:    The WMO code used to identify sensors.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       ID_Tag:           Character string written into the ID_TAG global
!                         attribute field of the netCDF ComponentTest file.
!                         Should contain a short tag used to identify the
!                         dependent profile set.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF ComponentTest file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF ComponentTest file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF ComponentTest file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which
!                         any msgs will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output msgs to standard
!                         output.
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

  FUNCTION WriteGAtts( NC_Filename     , &  ! Input
                       NC_FileID       , &  ! Input
                       TestType        , &  ! Input
                       DataType        , &  ! Input
                       Version         , &  ! Optional input
                       Sensor_Id       , &  ! Optional input
                       WMO_Satellite_Id, &  ! Optional input
                       WMO_Sensor_Id   , &  ! Optional input
                       ID_Tag          , &  ! Optional input
                       Title           , &  ! Optional input
                       History         , &  ! Optional input
                       Comment         , &  ! Optional input
                       Message_Log     ) &  ! Error messaging
                     RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: NC_Filename
    INTEGER     ,           INTENT(IN) :: NC_FileID
    INTEGER     ,           INTENT(IN) :: TestType
    INTEGER     ,           INTENT(IN) :: DataType
    INTEGER     , OPTIONAL, INTENT(IN) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(IN) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(IN) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(IN) :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'WriteGAtts'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: GAttName
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone
    INTEGER :: Ver
    INTEGER :: NF90_Status
    TYPE(ComponentTest_type) :: ComponentTest_Default

    ! Set up
    ! ------
    Error_Status = SUCCESS
    msg = ' '


    ! Mandatory global attributes
    ! ---------------------------
    ! Software ID
    GAttName = WRITE_MODULE_HISTORY_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                MODULE_RCS_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    
    ! Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    GAttName = CREATION_DATE_AND_TIME_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                czone//'UTC' )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! The Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                ComponentTest_Default%Release )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! The test type
    GAttName = TESTTYPE_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                TestType )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! The data type
    GAttName = DATATYPE_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                DataType )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! --------------------------
    ! The Version
    IF ( PRESENT(Version) ) THEN
      Ver = Version
    ELSE
      Ver = ComponentTest_Default%Version
    END IF
    GAttName = VERSION_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                Ver )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      GAttName = SENSOR_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      GAttName = WMO_SATELLITE_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Satellite_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The WMO_Sensor_Id
    IF ( PRESENT(WMO_Sensor_Id) ) THEN
      GAttName = WMO_SENSOR_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The ID_Tag
    IF ( PRESENT(ID_Tag) ) THEN
      GAttName = ID_TAG_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  ID_Tag )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Title
    IF ( PRESENT(Title) ) THEN
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Title )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The History
    IF ( PRESENT(History) ) THEN
      GAttName = HISTORY_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  History )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Comment
    IF ( PRESENT(Comment) ) THEN
      GAttName = COMMENT_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Comment )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

  CONTAINS
  
    SUBROUTINE WriteGAtts_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        msg = '; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(GAttName)//' attribute to '//&
                            TRIM(NC_Filename)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status ) )//TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE WriteGAtts_CleanUp
    
  END FUNCTION WriteGAtts


!------------------------------------------------------------------------------
!
! NAME:
!       ReadGAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF ComponentTest
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = ReadGAtts( NC_Filename                      , &
!                                 NC_FileID                        , &
!                                 Release         =Release         , &
!                                 Version         =Version         , &
!                                 TestType        =TestType        , &
!                                 DataType        =DataType        , &
!                                 Sensor_Id       =Sensor_Id       , &
!                                 WMO_Satellite_Id=WMO_Satellite_Id, &
!                                 WMO_Sensor_Id   =WMO_Sensor_Id   , &
!                                 ID_Tag          =ID_Tag          , &
!                                 Title           =Title           , &
!                                 History         =History         , &
!                                 Comment         =Comment         , &
!                                 Message_Log     =Message_Log       )
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF ComponentTest format data file to read from.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number.
!                         function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which
!                         any msgs will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output msgs to standard
!                         output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Release:          The release number of the netCDF ComponentTest file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:          The version number of the netCDF ComponentTest file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       TestType:         Integer flag indicating whether the test type is
!                         FWD/TL, TL/AD, or AD/K.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       DataType:         Integer flag indicating whether the spectral dimension
!                         is polychromatic or monochromatic.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Id:        Character string sensor/platform identifier.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_Id: The WMO code used to identify satellite platforms.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_Id:    The WMO code used to identify sensors.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       ID_Tag:           Character string written into the ID_TAG global
!                         attribute field of the netCDF ComponentTest file.
!                         Should contain a short tag used to identify the
!                         dependent profile set.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF ComponentTest file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF ComponentTest file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF ComponentTest file.
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

  FUNCTION ReadGAtts( NC_Filename     , &  ! Input
                      NC_FileID       , &  ! Input
                      Release         , &  ! Optional output
                      Version         , &  ! Optional output
                      TestType        , &  ! Optional output
                      DataType        , &  ! Optional output
                      Sensor_Id       , &  ! Optional output
                      WMO_Satellite_Id, &  ! Optional output
                      WMO_Sensor_Id   , &  ! Optional output
                      ID_Tag          , &  ! Optional output
                      Title           , &  ! Optional output
                      History         , &  ! Optional output
                      Comment         , &  ! Optional output
                      Message_Log     ) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    INTEGER     , OPTIONAL, INTENT(OUT) :: TestType
    INTEGER     , OPTIONAL, INTENT(OUT) :: DataType
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadGAtts'
    ! Local variables
    CHARACTER(256)  :: GAttName
    CHARACTER(5000) :: GAttString
    INTEGER :: Rel
    INTEGER :: NF90_Status
    TYPE(ComponentTest_type) :: ComponentTest_Default

    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! The mandatory GAtts for checking
    ! --------------------------------
    ! The Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_GET_ATT( NC_FileId, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                Rel )
    IF ( NF90_Status /= NF90_NOERR .OR. Rel /= ComponentTest_Default%Release) THEN
      CALL ReadGAtts_Cleanup(); RETURN
    END IF
    IF ( PRESENT(Release) ) Release = ComponentTest_Default%Release


    ! The optional GAtts
    ! ------------------
    ! The Version
    IF ( PRESENT(Version) ) THEN
      GAttName = VERSION_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Version )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The TestType
    IF ( PRESENT(TestType) ) THEN
      GAttName = TESTTYPE_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  TestType )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The DataType
    IF ( PRESENT(DataType) ) THEN
      GAttName = DATATYPE_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  DataType )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      GAttString = ' '; Sensor_Id = ' '
      GAttName = SENSOR_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      Sensor_Id = GAttString(1:MIN( LEN(Sensor_Id), LEN_TRIM(GAttString) ))
    END IF

    ! The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      GAttName = WMO_SATELLITE_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Satellite_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The WMO_Sensor_Id
    IF ( PRESENT(WMO_Sensor_Id) ) THEN
      GAttName = WMO_SENSOR_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The ID_Tag
    IF ( PRESENT(ID_Tag) ) THEN
      GAttString = ' '; ID_Tag = ' '
      GAttName = ID_TAG_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      ID_Tag = GAttString(1:MIN( LEN(ID_Tag), LEN_TRIM(GAttString) ))
    END IF

    ! The Title
    IF ( PRESENT(Title) ) THEN
      GAttString = ' '; Title = ' '
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      Title = GAttString(1:MIN( LEN(Title), LEN_TRIM(GAttString) ))
    END IF

    ! The History
    IF ( PRESENT(History) ) THEN
      GAttString = ' '; History = ' '
      GAttName = HISTORY_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      History = GAttString(1:MIN( LEN(History), LEN_TRIM(GAttString) ))
    END IF

    ! The Comment
    IF ( PRESENT(Comment) ) THEN
      GAttString = ' '; Comment = ' '
      GAttName = COMMENT_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      Comment = GAttString(1:MIN( LEN(Comment), LEN_TRIM(GAttString) ))
    END IF

  CONTAINS
  
    SUBROUTINE ReadGAtts_CleanUp()
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM(GAttName)//&
                            ' attribute from '//TRIM(NC_Filename)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE ReadGAtts_CleanUp

  END FUNCTION ReadGAtts


!------------------------------------------------------------------------------
!
! NAME:
!       DefineVar
!
! PURPOSE:
!       Function to define the ComponentTest variables in an output
!       netCDF file.
!
! CALLING SEQUENCE:
!       Error_Status = DefineVar( NC_Filename            , &
!                                 NC_FileID              , &
!                                 nK_DimID               , &
!                                 nL_DimID               , &
!                                 nP_DimID               , &
!                                 nIV_DimID              , &
!                                 nOV_DimID              , &
!                                 VSL_DimID              , &
!                                 nM_DimID               , &
!                                 TestType               , &
!                                 DataType               , &
!                                 Message_Log=Message_Log  )
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF ComponentTest format file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:          NetCDF file ID number of the file in which
!                           the variables are to be defned.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       nK_DimID:           NetCDF dimension ID of the number of layers.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       nL_DimID:           NetCDF dimension ID of the spectral dimension.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       nP_DimID:           NetCDF dimension ID of the number of perturbations.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       nIV_DimID:          NetCDF dimension ID of the number of input variables.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       nOV_DimID:          NetCDF dimension ID of the number of output variables.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!
!       nL_DimID:           NetCDF dimension ID of the spectral dimension.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       TestType:           Integer flag indicating whether the test type is
!                           FWD/TL, TL/AD, or AD/K.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       DataType:           Integer flag indicating whether the spectral dimension
!                           is polychromatic or monochromatic.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:        Character string specifying a filename in which any
!                           msgs will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output msgs to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!
!------------------------------------------------------------------------------

  FUNCTION DefineVar( NC_Filename       , &  ! Input
                      NC_FileID         , &  ! Input
                      nK_DimID          , &  ! Input
                      nL_DimID          , &  ! Input
                      nP_DimID          , &  ! Input
                      nIV_DimID         , &  ! Input
                      nOV_DimID         , &  ! Input
                      VSL_DimID         , &  ! Input
                      nM_DimID          , &  ! Input
                      TestType          , &  ! Input
                      DataType          , &  ! Input
                      Message_Log       ) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: NC_Filename
    INTEGER     ,           INTENT(IN) :: NC_FileID
    INTEGER     ,           INTENT(IN) :: nK_DimID 
    INTEGER     ,           INTENT(IN) :: nL_DimID 
    INTEGER     ,           INTENT(IN) :: nP_DimID 
    INTEGER     ,           INTENT(IN) :: nIV_DimID
    INTEGER     ,           INTENT(IN) :: nOV_DimID
    INTEGER     ,           INTENT(IN) :: VSL_DimID
    INTEGER     ,           INTENT(IN) :: nM_DimID 
    INTEGER     ,           INTENT(IN) :: TestType 
    INTEGER     ,           INTENT(IN) :: DataType 
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'DefineVar'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: NF90_Status(4)
    INTEGER :: varID
    CHARACTER(256) :: d1_VarName    , d2_VarName    
    CHARACTER(256) :: d1_Description, d2_Description
    CHARACTER(256) :: d1_LongName   , d2_LongName   
    CHARACTER(256) :: f_DimName    
    CHARACTER(256) :: f_VarName    
    CHARACTER(256) :: f_Description
    CHARACTER(256) :: f_LongName   
    CHARACTER(256) :: f_Units      

                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      


    ! Assign the attributes for the output
    ! variables of the different test types
    ! -------------------------------------
    CALL dNames( TestType, &
                 d1_VarName     = d1_VarName    , &
                 d1_Description = d1_Description, &
                 d1_LongName    = d1_LongName   , &
                 d2_VarName     = d2_VarName    , &
                 d2_Description = d2_Description, &
                 d2_LongName    = d2_LongName     )


    ! Assign the attributes for the spectral dimension
    ! ------------------------------------------------
    CALL fNames( DataType, &
                 f_DimName     = f_DimName    , &
                 f_VarName     = f_VarName    , &
                 f_Description = f_Description, &
                 f_LongName    = f_LongName   , &
                 f_Units       = f_Units        )


    ! Begin all the variable definitions
    ! ----------------------------------
    ! The Pressure variable
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,PRESSURE_VARNAME,PRESSURE_TYPE, &
                                   dimIDs=nK_DimId, &
                                   varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//PRESSURE_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,PRESSURE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,PRESSURE_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,PRESSURE_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,PRESSURE_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//PRESSURE_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    ! The Spectral variable
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,TRIM(f_VarName),SPECTRAL_TYPE, &
                                   dimIDs=nL_DimId, &
                                   varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TRIM(f_VarName)//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,TRIM(f_LongName) )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,TRIM(f_Description) )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,TRIM(f_Units) )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,SPECTRAL_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//TRIM(f_VarName)//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    ! The Perturbation variable
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,PERTURBATION_VARNAME,PERTURBATION_TYPE, &
                                   dimIDs=nP_DimId, &
                                   varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//PERTURBATION_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,PERTURBATION_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,PERTURBATION_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,PERTURBATION_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,PERTURBATION_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//PERTURBATION_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    ! The Input_Variable variable
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,INPUT_VARIABLE_VARNAME,INPUT_VARIABLE_TYPE, &
                                   dimIDs=(/ vsl_DimId,nIV_DimId /), &
                                   varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//INPUT_VARIABLE_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,INPUT_VARIABLE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,INPUT_VARIABLE_DESCRIPTION )
    IF ( ANY(NF90_Status(1:2) /= SUCCESS) ) THEN
      msg = 'Error writing '//INPUT_VARIABLE_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    ! The Input_Units variable
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,INPUT_UNITS_VARNAME,INPUT_UNITS_TYPE, &
                                   dimIDs=(/ vsl_DimId,nIV_DimId /), &
                                   varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//INPUT_UNITS_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,INPUT_UNITS_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,INPUT_UNITS_DESCRIPTION )
    IF ( ANY(NF90_Status(1:2) /= SUCCESS) ) THEN
      msg = 'Error writing '//INPUT_UNITS_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    ! The Output_Variable variable
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,OUTPUT_VARIABLE_VARNAME,OUTPUT_VARIABLE_TYPE, &
                                   dimIDs=(/ vsl_DimId,nOV_DimId /), &
                                   varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//OUTPUT_VARIABLE_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,OUTPUT_VARIABLE_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,OUTPUT_VARIABLE_DESCRIPTION )
    IF ( ANY(NF90_Status(1:2) /= SUCCESS) ) THEN
      msg = 'Error writing '//OUTPUT_VARIABLE_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    ! The Output_Units variable
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,OUTPUT_UNITS_VARNAME,OUTPUT_UNITS_TYPE, &
                                   dimIDs=(/ vsl_DimId,nOV_DimId /), &
                                   varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//OUTPUT_UNITS_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,OUTPUT_UNITS_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,OUTPUT_UNITS_DESCRIPTION )
    IF ( ANY(NF90_Status(1:2) /= SUCCESS) ) THEN
      msg = 'Error writing '//OUTPUT_UNITS_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    ! The DataSet_Name variable
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,DATASET_NAME_VARNAME,DATASET_NAME_TYPE, &
                                   dimIDs=(/ vsl_DimId,nM_DimId /), &
                                   varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//DATASET_NAME_VARNAME//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,DATASET_NAME_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,DATASET_NAME_DESCRIPTION )
    IF ( ANY(NF90_Status(1:2) /= SUCCESS) ) THEN
      msg = 'Error writing '//DATASET_NAME_VARNAME//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    ! The d1 variable
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,TRIM(d1_VarName),D1_TYPE, &
                                   dimIDs=(/ nK_DimId,nL_DimId,nP_DimId,nIV_DimId,nOV_DimId,nM_DimId /), &
                                   varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TRIM(d1_VarName)//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,TRIM(d1_LongName) )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,TRIM(d1_Description) )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,D1_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,D1_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//TRIM(d1_VarName)//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    ! The d2 variable
    NF90_Status(1) = NF90_DEF_VAR( NC_FileID,TRIM(d2_VarName),D2_TYPE, &
                                   dimIDs=(/ nK_DimId,nL_DimId,nP_DimId,nIV_DimId,nOV_DimId,nM_DimId /), &
                                   varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//TRIM(d2_VarName)//' variable in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL DefineVar_Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( NC_FileID,VarID,LONGNAME_ATTNAME,TRIM(d2_LongName) )
    NF90_Status(2) = NF90_PUT_ATT( NC_FileID,VarID,DESCRIPTION_ATTNAME,TRIM(d2_Description) )
    NF90_Status(3) = NF90_PUT_ATT( NC_FileID,VarID,UNITS_ATTNAME,D2_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( NC_FileID,VarID,FILLVALUE_ATTNAME,D2_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//TRIM(d2_VarName)//' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE DefineVar_CleanUp()
      ! Close file
      NF90_Status(1) = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status(1) /= NF90_NOERR ) &
        msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
              TRIM(NF90_STRERROR( NF90_Status(1) ) )
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE DefineVar_CleanUp

  END FUNCTION DefineVar


!------------------------------------------------------------------------------
!
! NAME:
!       WriteVar
!
! PURPOSE:
!       Function to write the ComponentTest variables in an output
!       netCDF file in which they have been defined.
!
! CALLING SEQUENCE:
!       Error_Status = WriteVar( NC_Filename            , &
!                                NC_FileID              , &
!                                ComponentTest          , &
!                                Message_Log=Message_Log  )
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF ComponentTest format file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:          NetCDF file ID number of the file in which
!                           the variables are to be written.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       ComponentTest:      Structure containing the data to write to file.
!                           UNITS:      N/A
!                           TYPE:       TYPE(ComponentTest_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:        Character string specifying a filename in which any
!                           msgs will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output msgs to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! SIDE EFFECTS:
!       If an error occurs, the netCDF file is closed.
!
!------------------------------------------------------------------------------

  FUNCTION WriteVar( NC_Filename, &  ! Input
                     NC_FileID  , &  ! Input
                     cTest      , &  ! Input
                     Message_Log) &  ! Error messaging
                   RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)            , INTENT(IN)  :: NC_Filename
    INTEGER                 , INTENT(IN)  :: NC_FileID
    TYPE(ComponentTest_type), INTENT(IN)  :: cTest
    CHARACTER(*), OPTIONAL  , INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'WriteVar'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: NF90_Status
    INTEGER :: VarId
    CHARACTER(256) :: d1_VarName, d2_VarName, f_VarName
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! Assign names based on test and data types
    ! -----------------------------------------
    CALL dNames( cTest%TestType,d1_VarName=d1_VarName,d2_VarName=d2_VarName )
    CALL fNames( cTest%DataType,f_VarName=f_VarName )
    
    
    ! Write the variable data
    ! -----------------------
    ! The Pressure variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,PRESSURE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//PRESSURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,cTest%Pressure )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//PRESSURE_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Spectral variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,TRIM(f_VarName),VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(f_VarName)//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,cTest%Spectral )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TRIM(f_VarName)//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Perturbation variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,PERTURBATION_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//PERTURBATION_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,cTest%Perturbation )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//PERTURBATION_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Input_Variable variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,INPUT_VARIABLE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//INPUT_VARIABLE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,cTest%Input_Variable_Name )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//INPUT_VARIABLE_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Input_Units variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,INPUT_UNITS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//INPUT_UNITS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,cTest%Input_Variable_Units )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//INPUT_UNITS_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Output_Variable variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,OUTPUT_VARIABLE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//OUTPUT_VARIABLE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,cTest%Output_Variable_Name )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//OUTPUT_VARIABLE_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The Output_Units variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,OUTPUT_UNITS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//OUTPUT_UNITS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,cTest%Output_Variable_Units )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//OUTPUT_UNITS_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The DataSet_Name variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,DATASET_NAME_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//DATASET_NAME_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,cTest%nM_Name, &
                                START=(/1,cTest%nM/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//DATASET_NAME_VARNAME//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The d1 variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,TRIM(d1_VarName),VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(d1_VarName)//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,cTest%d1, &
                                START=(/1,1,1,1,1,cTest%nM/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TRIM(d1_VarName)//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    ! The d2 variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,TRIM(d2_VarName),VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(d2_VarName)//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( NC_FileId,VarID,cTest%d2, &
                                START=(/1,1,1,1,1,cTest%nM/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TRIM(d2_VarName)//' to '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL WriteVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE WriteVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
              TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE WriteVar_CleanUp

  END FUNCTION WriteVar


!------------------------------------------------------------------------------
!
! NAME:
!       ReadVar
!
! PURPOSE:
!       Function to read the ComponentTest variables from any input
!       netCDF file in which they have been defined.
!
! CALLING SEQUENCE:
!       Error_Status = ReadVar( NC_Filename            , &
!                               NC_FileID              , &
!                               ComponentTest          , &
!                               Message_Log=Message_Log  )
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF ComponentTest format file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:          NetCDF file ID number of the file in which
!                           the variables are to be written.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ComponentTest:      Structure containing the data that was read
!                           from file.
!                           UNITS:      N/A
!                           TYPE:       TYPE(ComponentTest_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:        Character string specifying a filename in which any
!                           msgs will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output msgs to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!
! SIDE EFFECTS:
!       If an error occurs, the netCDF file is closed.
!
! COMMENTS:
!       The INTENT on the output ComponentTest argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION ReadVar( NC_Filename, &  ! Input
                    NC_FileID  , &  ! Input
                    cTest      , &  ! Output
                    Message_Log) &  ! Error messaging
                  RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)            , INTENT(IN)     :: NC_Filename
    INTEGER                 , INTENT(IN)     :: NC_FileID
    TYPE(ComponentTest_type), INTENT(IN OUT) :: cTest
    CHARACTER(*), OPTIONAL  , INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadVar'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: NF90_Status
    INTEGER :: VarId
    CHARACTER(256) :: d1_VarName,d2_VarName,f_VarName

                               
    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! Assign names based on test and data types
    ! -----------------------------------------
    CALL dNames( cTest%TestType,d1_VarName=d1_VarName,d2_VarName=d2_VarName )
    CALL fNames( cTest%DataType,f_VarName=f_VarName )
    
    
    ! Write the variable data
    ! -----------------------
    ! The Pressure variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,PRESSURE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//PRESSURE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,cTest%Pressure )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//PRESSURE_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Spectral variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,TRIM(f_VarName),VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(f_VarName)//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,cTest%Spectral )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TRIM(f_VarName)//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Perturbation variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,PERTURBATION_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//PERTURBATION_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,cTest%Perturbation )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//PERTURBATION_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Input_Variable variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,INPUT_VARIABLE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//INPUT_VARIABLE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,cTest%Input_Variable_Name )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//INPUT_VARIABLE_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Input_Units variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,INPUT_UNITS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//INPUT_UNITS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,cTest%Input_Variable_Units )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//INPUT_UNITS_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Output_Variable variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,OUTPUT_VARIABLE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//OUTPUT_VARIABLE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,cTest%Output_Variable_Name )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//OUTPUT_VARIABLE_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The Output_Units variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,OUTPUT_UNITS_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//OUTPUT_UNITS_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,cTest%Output_Variable_Units )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//OUTPUT_UNITS_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The DataSet_Name variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,DATASET_NAME_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//DATASET_NAME_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,cTest%nM_Name, &
                                START=(/1,cTest%nM/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//DATASET_NAME_VARNAME//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    CALL StrClean( cTest%nM_Name )
    ! The d1 variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,TRIM(d1_VarName),VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(d1_VarName)//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,cTest%d1, &
                                START=(/1,1,1,1,1,cTest%nM/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TRIM(d1_VarName)//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    ! The d2 variable
    NF90_Status = NF90_INQ_VARID( NC_FileId,TRIM(d2_VarName),VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(d2_VarName)//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( NC_FileId,VarID,cTest%d2, &
                                START=(/1,1,1,1,1,cTest%nM/) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TRIM(d2_VarName)//' from '//TRIM(NC_Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL ReadVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE ReadVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
              TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE ReadVar_CleanUp

  END FUNCTION ReadVar


!------------------------------------------------------------------------------
!
! NAME:
!       CreateFile
!
! PURPOSE:
!       Function to create a netCDF ComponentTest data file for writing.
!
! CALLING SEQUENCE:
!       Error_Status = CreateFile( NC_Filename                      , &
!                                  nK                               , &
!                                  nL                               , &
!                                  nP                               , &
!                                  nIV                              , &
!                                  nOV                              , &
!                                  TestType                         , &
!                                  DataType                         , &
!                                  NC_FileID                        , &
!                                  Version         =Version         , &
!                                  Sensor_Id       =Sensor_Id       , &
!                                  WMO_Satellite_Id=WMO_Satellite_Id, &
!                                  WMO_Sensor_Id   =WMO_Sensor_Id   , &
!                                  ID_Tag          =ID_Tag          , &
!                                  Title           =Title           , &
!                                  History         =History         , &
!                                  Comment         =Comment         , &
!                                  Message_Log     =Message_Log       )
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF ComponentTest format data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       nK:                 The number of layers dimension of the
!                           ComponentTest data. For surface component tests,
!                           should be set to 1.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       nL:                 The spectral dimension (channels/frequencies) of
!                           the ComponentTest data.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       nP:                 The number of perturbations dimension of the
!                           ComponentTest data. For non-FWD/TL component tests,
!                           should be set to 1.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       nIV:                The number of input variables dimension of the
!                           ComponentTest data.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       nOV:                The number of output variables dimension of the
!                           ComponentTest data.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       TestType:           Integer flag indicating whether the test type is
!                           FWD/TL, TL/AD, or AD/K.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       DataType:           Integer flag indicating whether the spectral dimension
!                           is polychromatic or monochromatic.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       NC_FileID:          NetCDF file ID number to be used for subsequent
!                           writing to the output file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Version:            The version number of the netCDF ComponentTest file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Sensor_Id:          Character string sensor/platform identifier.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Satellite_Id:   The WMO code used to identify satellite platforms.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Sensor_Id:      The WMO code used to identify sensors.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       ID_Tag:             Character string written into the ID_TAG global
!                           attribute field of the netCDF ComponentTest file.
!                           Should contain a short tag used to identify the
!                           dependent profile set.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF ComponentTest file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF ComponentTest file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF ComponentTest file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which
!                           any msgs will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output msgs to standard
!                           output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.  
!                           The error codes are defined in the Message_Handler module. 
!                           If == SUCCESS the netCDF file creation was successful.     
!                              == FAILURE an unrecoverable error occurred.             
!                              == WARNING - an error occurred writing any of the       
!                                           supplied global attributes.                
!                                         - an error occurred closing the netCDF file. 
!                           UNITS:      N/A                                            
!                           TYPE:       INTEGER                                        
!                           DIMENSION:  Scalar                                         
!
!------------------------------------------------------------------------------

  FUNCTION CreateFile( NC_Filename     , &  ! Input
                       nK              , &  ! Input
                       nL              , &  ! Input
                       nP              , &  ! Input
                       nIV             , &  ! Input
                       nOV             , &  ! Input
                       TestType        , &  ! Input
                       DataType        , &  ! Input
                       NC_FileID       , &  ! Output
                       Version         , &  ! Optional input
                       Sensor_Id       , &  ! Optional input
                       WMO_Satellite_Id, &  ! Optional input
                       WMO_Sensor_Id   , &  ! Optional input
                       ID_Tag          , &  ! Optional input
                       Title           , &  ! Optional input
                       History         , &  ! Optional input
                       Comment         , &  ! Optional input
                       Message_Log     ) &  ! Error messaging
                     RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    INTEGER               , INTENT(IN)  :: nK
    INTEGER               , INTENT(IN)  :: nL
    INTEGER               , INTENT(IN)  :: nP
    INTEGER               , INTENT(IN)  :: nIV
    INTEGER               , INTENT(IN)  :: nOV
    INTEGER               , INTENT(IN)  :: TestType
    INTEGER               , INTENT(IN)  :: DataType
    INTEGER               , INTENT(OUT) :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: ID_Tag
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CreateFile'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: NF90_Status
    INTEGER :: nK_DimID
    INTEGER :: nL_DimID
    INTEGER :: nP_DimID
    INTEGER :: nIV_DimID
    INTEGER :: nOV_DimID
    INTEGER :: VSL_DimID
    INTEGER :: nM_DimID
    CHARACTER(256) :: f_DimName
    TYPE(ComponentTest_type) :: Dummy
    

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Check dimension input
    IF ( nK  < 1 .OR. & 
         nL  < 1 .OR. &  
         nP  < 1 .OR. & 
         nIV < 1 .OR. &
         nOV < 1      ) THEN
      msg = 'Invalid dimension input detected.'
      CALL Create_Cleanup(); RETURN
    END IF

    ! Check test type
    IF ( ALL( COMPONENTTEST_TESTTYPE(1:) /= TestType ) ) THEN
      msg = 'Invalid TestType'
      CALL Create_Cleanup(); RETURN
    END IF

    ! Check data type
    IF ( ALL( COMPONENTTEST_DATATYPE(1:) /= DataType ) ) THEN
      msg = 'Invalid DataType'
      CALL Create_Cleanup(); RETURN
    END IF


    ! Assign name based on data type
    ! ------------------------------
    CALL fNames( DataType,f_DimName=f_DimName )


    ! Create the data file
    ! --------------------
    NF90_Status = NF90_CREATE( NC_Filename,NF90_CLOBBER,NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(NC_Filename)//' - '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF


    ! Define the dimensions
    ! ---------------------
    ! The n_Layers dimension
    NF90_Status = NF90_DEF_DIM( NC_FileID,LAYER_DIMNAME,nK,nK_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//LAYER_DIMNAME//' dimension in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! The Spectral dimension
    NF90_Status = NF90_DEF_DIM( NC_FileID,TRIM(f_DimName),nL,nL_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//TRIM(f_DimName)//' dimension in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! The n_Perturbations dimension
    NF90_Status = NF90_DEF_DIM( NC_FileID,PERTURBATION_DIMNAME,nP,nP_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//PERTURBATION_DIMNAME//' dimension in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! The n_Input_Variables dimension
    NF90_Status = NF90_DEF_DIM( NC_FileID,INPUT_VARIABLE_DIMNAME,nIV,nIV_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//INPUT_VARIABLE_DIMNAME//' dimension in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! The n_Output_Variables dimension
    NF90_Status = NF90_DEF_DIM( NC_FileID,OUTPUT_VARIABLE_DIMNAME,nOV,nOV_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//OUTPUT_VARIABLE_DIMNAME//' dimension in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! The VarStrLen dimension
    NF90_Status = NF90_DEF_DIM( NC_FileID,VSL_DIMNAME,LEN(Dummy%Input_Variable_Name),vsl_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//VSL_DIMNAME//' dimension in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! The n_DataSets dimension
    NF90_Status = NF90_DEF_DIM( NC_FileID,DATASET_DIMNAME,NF90_UNLIMITED,nM_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//DATASET_DIMNAME//' dimension in '//&
            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Write the global attributes
    ! ---------------------------
    Error_Status = WriteGAtts( NC_Filename                      , &
                               NC_FileID                        , &
                               TestType                         , &
                               DataType                         , &
                               Version         =Version         , &
                               Sensor_Id       =Sensor_Id       , &
                               WMO_Satellite_Id=WMO_Satellite_Id, &
                               WMO_Sensor_Id   =WMO_Sensor_Id   , &
                               ID_Tag          =ID_Tag          , &
                               Title           =Title           , &
                               History         =History         , &
                               Comment         =Comment         , &
                               Message_Log     =Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error writing global attributes to '//TRIM(NC_Filename)
      CALL Create_Cleanup(); RETURN
    END IF


    ! Define the ComponentTest variables
    ! ------------------------------
    Error_Status = DefineVar( NC_Filename, &
                              NC_FileID  , &
                              nK_DimID   , &
                              nL_DimID   , &
                              nP_DimID   , &
                              nIV_DimID  , &
                              nOV_DimID  , &
                              VSL_DimID  , &
                              nM_DimID   , &
                              TestType   , &
                              DataType   , &
                              Message_Log=Message_Log  )
    IF ( Error_Status /= SUCCESS ) THEN
      msg = 'Error defining variables in '//TRIM(NC_Filename)
      CALL Create_Cleanup(); RETURN
    END IF
                                             

    ! Take netCDF file out of define mode
    ! -----------------------------------
    NF90_Status = NF90_ENDDEF( NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error taking '//TRIM(NC_Filename)//' out of define mode.'
      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Create_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          NF90_Status = NF90_CLOSE( NC_FileID )
          IF ( NF90_Status /= NF90_NOERR ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
        END IF
      END IF
      ! Set error status and print error msg
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END SUBROUTINE Create_CleanUp

  END FUNCTION CreateFile

END MODULE ComponentTest_netCDF_IO
