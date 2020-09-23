!
! NLTE_Define
!
! Module defining the NLTE structure and containing routines to 
! manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, 08-05-2010
!

MODULE NLTE_Define
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler

  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Public structure type
  PUBLIC :: NLTE_type
  ! Public parameters
  PUBLIC :: N_PLay, Pt
  PUBLIC :: N_NLTE_PREDICTORS
  PUBLIC :: N_ENSEMBLE_TEMPERATURES
  PUBLIC :: MIN_TM_INDEX  
  PUBLIC :: MAX_TM_INDEX  
  PUBLIC :: MEAN_TM_INDEX 
  PUBLIC :: NLTE_WMO_SENSOR_ID
  PUBLIC :: N_NLTE_SENSOR_ID
  ! Public fuctions and subroutines
  PUBLIC :: NLTE_Associated
  PUBLIC :: NLTE_Create
  PUBLIC :: NLTE_Destroy
  PUBLIC :: CheckRelease_NLTE
  PUBLIC :: Info_NLTE
  PUBLIC :: NOT_NLTE

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &

  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Sensor id string length
  INTEGER,  PARAMETER :: SL = 20
  ! Invalid sensor ids
  INTEGER, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047
  ! Invalid index
  INTEGER, PARAMETER :: INVALID_INDEX = -1
  ! flag for non NLTE channel
  INTEGER, PARAMETER :: NOT_NLTE = -1

  ! Current valid release and version numbers
  INTEGER, PARAMETER :: NLTE_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: NLTE_VERSION = 1  ! This is just the data version.
  
  REAL(fp), PARAMETER :: ZERO = 0.0_fp, ONE = 1.0_fp, TWO = 2.0_fp
  ! Pressure levels for computing mean temperatures in the two layers
  INTEGER,  PARAMETER :: N_PLay = 2
  REAL(fp), PARAMETER :: Pt(2, N_PLay) = &
                                 RESHAPE( (/ 0.005_fp, 0.2_fp, &
                                             0.2_fp,  52.0_fp /), &
                                           (/2, N_PLay/)) 
  ! number of predictors including the constant term
  INTEGER, PARAMETER  :: N_NLTE_PREDICTORS = 3
  ! number of mean temperatures (Min Tm, Max Tm and Mean Tm)
  INTEGER, PARAMETER  :: N_ENSEMBLE_TEMPERATURES = 3
  INTEGER, PARAMETER  :: MIN_TM_INDEX  = 1
  INTEGER, PARAMETER  :: MAX_TM_INDEX  = 2
  INTEGER, PARAMETER  :: MEAN_TM_INDEX = 3
  
  INTEGER, PARAMETER  :: N_NLTE_SENSOR_ID   = 3
  INTEGER, PARAMETER  :: NLTE_WMO_SENSOR_ID(N_NLTE_SENSOR_ID) = (/&
                                             420, &  ! AIRS
                                             221, &  ! IASI
                                             620 /)  ! CrIS
  
  ! -------------------------
  ! NLTE data type definition
  ! -------------------------
  TYPE :: NLTE_type

    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.

    ! Release and version information
    INTEGER :: Release = NLTE_RELEASE
    INTEGER :: Version = NLTE_VERSION

    ! Dimension values
    INTEGER :: n_Predictors      = 0  ! n1 dimension
    INTEGER :: n_Sensor_Zangles  = 0  ! n2 dimension
    INTEGER :: n_Solar_Zangles   = 0  ! n3 dimension
    INTEGER :: n_NLTE_Channels   = 0  ! n4 dimension
    INTEGER :: n_Channels        = 0  ! n5 dimension

    ! sensor id
    CHARACTER(SL) :: Sensor_Id        = ' '
    INTEGER       :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER       :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID

    ! Min., Max. and Mean layer temperatures used as the temperautre predictor limits 
    REAL(fp) :: Tm(N_ENSEMBLE_TEMPERATURES, N_PLAY) 
    
    ! coefficients for NLTE corrections and associated node variables
    REAL(fp), ALLOCATABLE :: C(:,:,:,:)                 ! n1 x n2 x n3 x n4 
    ! secant of the Sensor zenith angles at the surface   
    REAL(fp), ALLOCATABLE :: Sensor_Zangle(:)            ! n2  
    ! Solar zenith angle at the surface in degree
    REAL(fp), ALLOCATABLE :: Solar_Zangle(:)             ! n3  
    ! Sensor channel indexes of the NLTE channels
    INTEGER, ALLOCATABLE  :: NLTE_Channel(:)            ! n4  
    ! The element of the array at position i is eigher a positive integer as an index to the 
    ! the C array along the channel dimension, or a negtive integer indicating channel i
    ! is not a NLTE channel.
    INTEGER, ALLOCATABLE  :: NLTE_Channel_Index(:)      ! n5  
    
  END TYPE NLTE_type

CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTE_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the NLTE structure.
!
! CALLING SEQUENCE:
!       Status = NLTE_Associated( nlte )
!
! OBJECTS:
!       nlte:      NLTE structure which is to have its member's
!                  status tested.
!                  UNITS:      N/A
!                  TYPE:       NLTE_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:    The return value is a logical value indicating the
!                  status of the NLTE members.
!                    .TRUE.  - if the array components are allocated.
!                    .FALSE. - if the array components are not allocated.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION NLTE_Associated( nlte ) RESULT( Status )
    ! Arguments
    TYPE(NLTE_type), INTENT(IN) :: nlte
    ! Function result
    LOGICAL :: Status

    Status = nlte%Is_Allocated
    
  END FUNCTION NLTE_Associated

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTE_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize NLTE objects.
!
! CALLING SEQUENCE:
!       CALL CRTM_Atmosphere_Destroy( Atm )
!
! OBJECTS:
!       Atm:          Re-initialized Atmosphere structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE NLTE_Destroy( nlte )
    TYPE(NLTE_type), INTENT(OUT) :: nlte
    nlte%Is_Allocated = .FALSE.
  END SUBROUTINE NLTE_Destroy

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTE_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of the CRTM Atmosphere object.
!
! CALLING SEQUENCE:
!       CALL NLTE_Create( nlte              , &
!                         n_NLTE_Channels   , &           
!                         n_Channels   )                  
!
! OBJECTS:
!       nlte:         NLTE structure.
!                     UNITS:      N/A
!                     TYPE:       NLTE_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Predictors: Number of predictors.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as the NLTE object
!                     ATTRIBUTES: INTENT(IN)
!
!   n_Sensor_Zangles: Number of sensor zenith angles.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as the NLTE object
!                     ATTRIBUTES: INTENT(IN)
!
!    n_Solar_Zangles: Number of solar zenith angles.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as the NLTE object
!                     ATTRIBUTES: INTENT(IN)
!
!   n_NLTE_Channels:  Number of NLTE channels.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as the NLTE object
!                     ATTRIBUTES: INTENT(IN)
!
!        n_Channels:  Number of channels of the sensor.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as NLTE object
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE NLTE_Create( &
    nlte              , &  ! Output
    n_Predictors      , &  ! Input
    n_Sensor_Zangles  , &  ! Input
    n_Solar_Zangles   , &  ! Input
    n_NLTE_Channels   , &  ! Input
    n_Channels   )         ! Input
    ! Arguments
    TYPE(NLTE_type)   , INTENT(OUT) :: nlte
    INTEGER           , INTENT(IN)  :: n_Predictors            
    INTEGER           , INTENT(IN)  :: n_Sensor_Zangles              
    INTEGER           , INTENT(IN)  :: n_Solar_Zangles            
    INTEGER           , INTENT(IN)  :: n_NLTE_Channels            
    INTEGER           , INTENT(IN)  :: n_Channels              
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Predictors < 1 .OR. n_Sensor_Zangles < 1.OR. n_Solar_Zangles < 1.OR. &
         n_NLTE_Channels < 1 .OR. n_Channels < 1 ) RETURN
    
    ! Perform the allocation
    ALLOCATE( nlte%C( n_Predictors, n_Sensor_Zangles, n_Solar_Zangles, n_NLTE_Channels ), &
              nlte%Sensor_Zangle( n_Sensor_Zangles ), &
              nlte%Solar_Zangle( n_Solar_Zangles ), &
              nlte%NLTE_Channel( n_NLTE_Channels ), &
              nlte%NLTE_Channel_Index( n_Channels ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    nlte%n_Predictors     = n_Predictors
    nlte%n_Sensor_Zangles = n_Sensor_Zangles
    nlte%n_Solar_Zangles  = n_Solar_Zangles
    nlte%n_NLTE_Channels  = n_NLTE_Channels
    nlte%n_Channels       = n_Channels
    ! ...Arrays
    nlte%C                   = ZERO
    nlte%Sensor_Zangle       = ZERO
    nlte%Solar_Zangle        = ZERO
    nlte%NLTE_Channel        = INVALID_INDEX
    nlte%NLTE_Channel_Index  = NOT_NLTE

    ! Set allocation indicator
    nlte%Is_Allocated = .TRUE.

  END SUBROUTINE NLTE_Create

!----------------------------------------------------------------------------------
!
! NAME:
!       CheckRelease_NLTE
!
! PURPOSE:
!       Function to check the NLTE Release value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckRelease_NLTE(  NLTE                   , &  ! Input
!                                          RCS_Id     = RCS_Id    , &  ! Revision control
!                                          Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NLTE:          NLTE structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       NLTE_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structure Release value is valid.
!                         == FAILURE the structure Release value is NOT valid
!                                    and either a data file file or software
!                                    update is required.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION CheckRelease_NLTE( nlte       , &  ! Input
                              RCS_Id     , &  ! Revision control
                              Message_Log) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    TYPE(nlte_type)       , INTENT(IN)  :: nlte
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckRelease_nlte'
    ! Local variables
    CHARACTER(256) :: Message

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Check the release
    ! -----------------
    ! Check that release is not too old
    IF ( nlte%Release < NLTE_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An NLTE data update is needed. ", &
                        &"NLTE release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      NLTE%Release, NLTE_RELEASE
      CALL Display_Message( ROUTINE_NAME,  &
                            TRIM(Message), &
                            Error_Status,  &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check that release is not too new
    IF ( NLTE%Release > NLTE_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An NLTE software update is needed. ", &
                        &"NLTE release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      NLTE%Release, NLTE_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckRelease_NLTE

!------------------------------------------------------------------------------
!
! NAME:
!       Info_ODAS
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the ODAS data structure.
!
! CALLING SEQUENCE:
!       CALL Info_ODAS( ODAS         , &  ! Input
!                       Info         , &  ! Output
!                       RCS_Id=RCS_Id  )  ! Revision control
!
! INPUT ARGUMENTS:
!       ODAS:          Filled ODAS structure.
!                      UNITS:      N/A
!                      TYPE:       ODAS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed ODAS data structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Info_NLTE( NLTE  , &  ! Input
                        Info  , &  ! Output
                        RCS_Id  )  ! Revision control
    ! Arguments
    TYPE(NLTE_type)       , INTENT(IN)  :: NLTE
    CHARACTER(*),           INTENT(OUT) :: Info
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    ! Local variables
    CHARACTER(2000) :: LongString

    ! Set up
    ! ------
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Write the required data to the local string
    ! -------------------------------------------
    WRITE( LongString,'(3x,"NLTE RELEASE.VERSION: ",i2,".",i2.2,2x,&
                      &"N_PREDICTORS=",i0,2x,&
                      &"N_SENSOR_ZANLGE=",i0,2x,&
                      &"N_SOLAR_ZANGLE=",i0,2x, &
                      &"N_NLTE_CHANNELS=",i0,2x, &
                      &"N_CHANNELS=",i0)' ) &
                      NLTE%Release, NLTE%Version, &
                      NLTE%n_Predictors, &
                      NLTE%n_Sensor_Zangles, &
                      NLTE%n_Solar_Zangles, &
                      NLTE%n_NLTE_Channels, &
                      NLTE%n_Channels

    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------
    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

  END SUBROUTINE Info_NLTE

END MODULE NLTE_Define
