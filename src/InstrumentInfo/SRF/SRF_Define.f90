!
! SRF_Define
!
! Module defining the SRF data structure and containing routines to 
! manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!
!       Modified by:    Yong Chen, CIRA/CSU/JCSDA 20-Aug-2008
!                       Yong.Chen@noaa.gov
 
MODULE SRF_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,            ONLY: fp
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  USE Integrate_Utility,     ONLY: Integral
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE

  ! Public procedures
  ! -----------------
  PUBLIC :: Associated_SRF
  PUBLIC :: Destroy_SRF
  PUBLIC :: Allocate_SRF
  PUBLIC :: Assign_SRF
  PUBLIC :: Equal_SRF
  PUBLIC :: CheckRelease_SRF
  PUBLIC :: Info_SRF
  PUBLIC :: Frequency_SRF
  PUBLIC :: Integrate_SRF
  
  ! Public parameters
  ! -----------------
  ! Sensor Id defaults
  PUBLIC :: INVALID_WMO_SATELLITE_ID
  PUBLIC :: INVALID_WMO_SENSOR_ID
  ! Allowable channel type values and names
  PUBLIC :: N_SENSOR_TYPES
  PUBLIC :: INVALID_SENSOR  
  PUBLIC :: MICROWAVE_SENSOR
  PUBLIC :: INFRARED_SENSOR 
  PUBLIC :: VISIBLE_SENSOR  
  PUBLIC :: ULTRAVIOLET_SENSOR  
  PUBLIC :: SENSOR_TYPE_NAME


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE Destroy_SRF
    MODULE PROCEDURE Destroy_SRF_scalar
    MODULE PROCEDURE Destroy_SRF_rank1
  END INTERFACE Destroy_SRF


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! String lengths
  INTEGER,  PARAMETER :: ML = 256 ! msg length
  INTEGER,  PARAMETER :: SL = 20  ! Sensor Id length
  ! Keyword set value
  INTEGER,  PARAMETER :: SET = 1
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: SRF_RELEASE = 2  ! This determines structure and file formats.
  INTEGER, PARAMETER :: SRF_VERSION = 1  ! This is just the data version.
  ! ASCII codes for Version routine
  INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
  INTEGER, PARAMETER :: LINEFEED = 10
  ! Invalid SRF values
  INTEGER,  PARAMETER :: INVALID = -1
  ! Invalid WMO sensor ids
  INTEGER,  PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER,  PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047
  ! The instrument types
  INTEGER, PARAMETER :: N_SENSOR_TYPES     = 4
  INTEGER, PARAMETER :: INVALID_SENSOR     = 0
  INTEGER, PARAMETER :: MICROWAVE_SENSOR   = 1
  INTEGER, PARAMETER :: INFRARED_SENSOR    = 2
  INTEGER, PARAMETER :: VISIBLE_SENSOR     = 3
  INTEGER, PARAMETER :: ULTRAVIOLET_SENSOR = 4
  CHARACTER(*), PARAMETER :: SENSOR_TYPE_NAME(0:N_SENSOR_TYPES) = (/ 'Invalid    ', &
                                                                     'Microwave  ', &
                                                                     'Infrared   ', &
                                                                     'Visible    ', &
                                                                     'Ultraviolet' /)


  ! ------------------------
  ! SRF data type definition
  ! ------------------------
  TYPE, PUBLIC :: SRF_type
    INTEGER :: n_Allocates = 0
    ! Release and version information
    INTEGER :: Release = SRF_RELEASE
    INTEGER :: Version = SRF_VERSION
    ! Dimension values
    INTEGER :: n_Points = 0  ! L
    INTEGER :: n_Bands  = 0  ! N
    ! The sensor and satellite Ids
    CHARACTER(SL) :: Sensor_ID  = ' '
    INTEGER :: WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    INTEGER :: WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    INTEGER :: Sensor_Type      = INVALID_SENSOR
    ! The channel number
    INTEGER :: Channel          = INVALID
    ! The SRF normalisation values
    REAL(fp) :: Integrated_SRF  = ZERO
    REAL(fp) :: Summation_SRF   = ZERO
    ! The band parameters
    REAL(fp), POINTER :: f1_Band(:)   => NULL() ! N
    REAL(fp), POINTER :: f2_Band(:)   => NULL() ! N 
    INTEGER,  POINTER :: npts_Band(:) => NULL() ! N
    ! The SRF data
    REAL(fp), POINTER :: Frequency(:) => NULL() ! L
    REAL(fp), POINTER :: Response(:)  => NULL() ! L 
  END TYPE SRF_type


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
!       Associated_SRF
!
! PURPOSE:
!       Function to test if ALL the pointer members of a SRF structure
!       are associated.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_SRF( SRF              , &  ! Input
!                                            ANY_Test=Any_Test  )  ! Optional input
!
! INPUT ARGUMENTS:
!       SRF:                 SRF structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       TYPE(SRF_type)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            SRF structure pointer members are associated.
!                            The default is to test if ALL the pointer members
!                            are associated.
!                            If ANY_Test = 0, test if ALL the pointer members
!                                             are associated.  (DEFAULT)
!                               ANY_Test = 1, test if ANY of the pointer members
!                                             are associated.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the SRF pointer members.
!                            .TRUE.  - if ALL the SRF pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the SRF pointer
!                                      members are associated.
!                            .FALSE. - some or all of the SRF pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Associated_SRF( SRF     , & ! Input
                           ANY_Test) & ! Optional input
                         RESULT( Association_Status )
    ! Arguments
    TYPE(SRF_type),    INTENT(IN) :: SRF
    INTEGER, OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test

    ! Set up
    ! ------
    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF

    ! Test the members that MUST be associated
    ! ----------------------------------------
    Association_Status = .FALSE.
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED( SRF%f1_Band   ) .AND. &
           ASSOCIATED( SRF%f2_Band   ) .AND. &
           ASSOCIATED( SRF%npts_Band ) .AND. &
           ASSOCIATED( SRF%Frequency ) .AND. &
           ASSOCIATED( SRF%Response  )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( SRF%f1_Band   ) .OR. &
           ASSOCIATED( SRF%f2_Band   ) .OR. &
           ASSOCIATED( SRF%npts_Band ) .AND. &
           ASSOCIATED( SRF%Frequency ) .OR. &
           ASSOCIATED( SRF%Response  )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION Associated_SRF


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Destroy_SRF
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of SRF
!       data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_SRF( SRF                    , &  ! Output
!                                   RCS_Id     =RCS_Id     , &  ! Revision control
!                                   Message_Log=Message_Log  )  ! Error messaging
!
! OUTPUT ARGUMENTS:
!       SRF:          Re-initialised SRF structure.
!                     UNITS:      N/A
!                     TYPE:       TYPE(SRF_type)
!                     DIMENSION:  Scalar or Rank-1 array
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure re-initialisation was successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to zero (0) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SRF argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Destroy_SRF_scalar( SRF        , &  ! Output
                               No_Clear   , &  ! Optional input
                               RCS_Id     , &  ! Revision control
                               Message_Log) &  ! Error messaging
                             RESULT( Error_Status )
    ! Arguments
    TYPE(SRF_type),         INTENT(IN OUT) :: SRF
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_SRF(scalar)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Clear
    INTEGER :: Destroy_Status
 
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Reinitialise the dimensions
    SRF%n_Points = 0
    SRF%n_Bands  = 0
    
    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT(No_Clear) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF
    IF ( Clear ) CALL Clear_SRF( SRF )

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_SRF( SRF ) ) RETURN


    ! Deallocate the regular array components
    ! ---------------------------------------
    DEALLOCATE( SRF%Frequency, &
                SRF%Response , &
                SRF%npts_Band, &
                SRF%f1_Band  , &
                SRF%f2_Band  , &
                STAT=Destroy_Status )
    IF ( Destroy_Status /= 0 ) THEN                                               
      Error_Status = FAILURE                                                      
      WRITE( msg,'("Error deallocating SRF components. STAT = ",i0)' ) &  
                     Destroy_Status                                              
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      ! No return, just continue
    END IF                                                                        
 
 
    ! Decrement and test allocation counter
    ! -------------------------------------
    SRF%n_Allocates = SRF%n_Allocates - 1
    IF ( SRF%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Allocation counter /= 0, Value = ",i0)' ) SRF%n_Allocates
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END IF

  END FUNCTION Destroy_SRF_scalar

  FUNCTION Destroy_SRF_rank1( SRF        , &  ! Output
                              No_Clear   , &  ! Optional input
                              RCS_Id     , &  ! Revision control
                              Message_Log) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    TYPE(SRF_type),         INTENT(IN OUT) :: SRF(:)
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_SRF(rank1)'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: Scalar_Status
    INTEGER :: l

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Reinitialise array
    ! ------------------
    DO l = 1, SIZE(SRF)
      Scalar_Status = Destroy_SRF_scalar( SRF(l), &
                                          No_Clear   =No_Clear, &
                                          Message_Log=Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( msg, '( "Error destroying element (",i0,")", &
                          &" of SRF structure array." )' ) l
        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      END IF
    END DO

  END FUNCTION Destroy_SRF_rank1


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Allocate_SRF
! 
! PURPOSE:
!       Function to allocate the pointer members of the SRF data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_SRF( n_Points               , &  ! Input
!                                    SRF                    , &  ! Output
!                                    n_Bands    =n_Bands    , &  ! Optional input
!                                    RCS_Id     =RCS_Id     , &  ! Revision control
!                                    Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Points:     The number of total spectral points used to represent
!                     the channel SRF.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SRF:          SRF structure with allocated pointer members
!                     UNITS:      N/A
!                     TYPE:       TYPE(SRF_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       n_Bands:      The number of frequency bands used to represent the
!                     channel SRF. The argument should only be used for
!                     microwave channel SRFs.
!                     If not specified, the default value is 1.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure allocation was successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to one (1) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SRF argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Allocate_SRF( n_Points   , &  ! Input
                         SRF        , &  ! Output
                         n_Bands    , &  ! Optional input
                         RCS_Id     , &  ! Revision control
                         Message_Log) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    INTEGER,                INTENT(IN)     :: n_Points
    TYPE(SRF_type),         INTENT(IN OUT) :: SRF
    INTEGER,      OPTIONAL, INTENT(IN)     :: n_Bands
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_SRF'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: Allocate_Status
    INTEGER :: n_Local_Bands

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Check dimension inputs
    IF ( n_Points < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input N_POINTS must be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    n_Local_Bands = 1
    IF ( PRESENT(n_Bands) ) THEN
      IF ( n_Bands < 1 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Input N_BANDS must be > 0.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
      n_Local_Bands = n_Bands
    END IF

    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_SRF( SRF, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_SRF( SRF,No_Clear=SET,Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating SRF pointer members.', &
                              Error_Status,    &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF


    ! Allocate the intrinsic type arrays
    ! ----------------------------------
    ALLOCATE(  SRF%f1_Band( n_Local_Bands ),   &
               SRF%f2_Band( n_Local_Bands ),   &
               SRF%npts_Band( n_Local_Bands ), &
               SRF%Frequency( n_Points ),      &
               SRF%Response( n_Points ),       &
               STAT = Allocate_Status          )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Error allocating SRF data arrays. STAT = ",i0)' ) Allocate_Status
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign the dimensions and initialise arrays
    SRF%n_Points = n_Points
    SRF%n_Bands  = n_Local_Bands

    SRF%f1_Band   = ZERO
    SRF%f2_Band   = ZERO
    SRF%npts_Band = 0
    SRF%Frequency = ZERO
    SRF%Response  = ZERO
 
 
    ! Increment and test allocation counter
    ! -------------------------------------
    SRF%n_Allocates = SRF%n_Allocates + 1
    IF ( SRF%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( msg,'("Allocation counter /= 1, Value = ",i0)' ) SRF%n_Allocates
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
    END IF

  END FUNCTION Allocate_SRF


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Assign_SRF
!
! PURPOSE:
!       Function to copy valid SRF structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_SRF( SRF_in                 , &  ! Input
!                                  SRF_out                , &  ! Output
!                                  RCS_Id     =RCS_Id     , &  ! Revision control
!                                  Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SRF_in:       SRF structure which is to be copied.
!                     UNITS:      N/A
!                     TYPE:       TYPE(SRF_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SRF_out:      Copy of the input structure, SRF_in.
!                     UNITS:      N/A
!                     TYPE:       TYPE(SRF_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure assignment was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SRF argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Assign_SRF( SRF_in     , &  ! Input
                       SRF_out    , &  ! Output
                       RCS_Id     , &  ! Revision control
                       Message_Log) &  ! Error messaging
                     RESULT( Error_Status )
    ! Arguments
    TYPE(SRF_type),         INTENT(IN)     :: SRF_in
    TYPE(SRF_type),         INTENT(IN OUT) :: SRF_out
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_SRF'
    ! Local variables

    ! Setup
    ! -----
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. Associated_SRF( SRF_In ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT SRF pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Allocate the structure
    ! ----------------------
    Error_Status = Allocate_SRF( SRF_In%n_Points, &
                                 SRF_Out, &
                                 n_Bands=SRF_In%n_Bands, &
                                 Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output SRF arrays.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign intrinsic data types
    ! ---------------------------
    SRF_out%Release = SRF_in%Release
    SRF_out%Version = SRF_in%Version
    
    SRF_out%Sensor_ID        = SRF_in%Sensor_ID  
    SRF_out%WMO_Satellite_Id = SRF_in%WMO_Satellite_Id
    SRF_out%WMO_Sensor_Id    = SRF_in%WMO_Sensor_Id
    SRF_out%Sensor_Type      = SRF_in%Sensor_Type   
    SRF_out%Channel          = SRF_in%Channel
    SRF_out%Integrated_SRF   = SRF_in%Integrated_SRF
    SRF_out%Summation_SRF    = SRF_in%Summation_SRF
    SRF_out%f1_Band          = SRF_in%f1_Band 
    SRF_out%f2_Band          = SRF_in%f2_Band 
    SRF_out%npts_Band        = SRF_in%npts_Band 
    SRF_out%Frequency        = SRF_in%Frequency
    SRF_out%Response         = SRF_in%Response

  END FUNCTION Assign_SRF


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Equal_SRF
!
! PURPOSE:
!       Function to test if two SRF structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = Equal_SRF( SRF_LHS                , &  ! Input
!                                 SRF_RHS                , &  ! Input
!                                 ULP_Scale  =ULP_Scale  , &  ! Optional input
!                                 Check_All  =Check_All  , &  ! Optional input
!                                 RCS_Id     =RCS_Id     , &  ! Revision control
!                                 Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SRF_LHS:       SRF structure to be compared; equivalent to the
!                      left-hand side of a lexical comparison, e.g.
!                        IF ( SRF_LHS == SRF_RHS ).
!                      UNITS:      N/A
!                      TYPE:       TYPE(SRF_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       SRF_RHS:       SRF structure to be compared to; equivalent to
!                      right-hand side of a lexical comparison, e.g.
!                        IF ( SRF_LHS == SRF_RHS ).
!                      UNITS:      N/A
!                      TYPE:       Same as SRF_LHS
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP_Scale:     Unit of data precision used to scale the floating
!                      point comparison. ULP stands for "Unit in the Last Place,"
!                      the smallest possible increment or decrement that can be
!                      made using a machine's floating point arithmetic.
!                      Value must be positive - if a negative value is supplied,
!                      the absolute value is used. If not specified, the default
!                      value is 1.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Check_All:     Set this argument to check ALL the floating point
!                      channel data of the SRF structures. The default
!                      action is return with a FAILURE status as soon as
!                      any difference is found. This optional argument can
!                      be used to get a listing of ALL the differences
!                      between data in SRF structures.
!                      If == 0, Return with FAILURE status as soon as
!                               ANY difference is found  *DEFAULT*
!                         == 1, Set FAILURE status if ANY difference is
!                               found, but continue to check ALL data.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structures were equal
!                         == FAILURE - an error occurred, or
!                                    - the structures were different.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Equal_SRF( SRF_LHS, &  ! Input
                      SRF_RHS, &  ! Input
                      ULP_Scale   , &  ! Optional input
                      Check_All   , &  ! Optional input
                      RCS_Id      , &  ! Revision control
                      Message_Log ) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    TYPE(SRF_type)        , INTENT(IN)  :: SRF_LHS
    TYPE(SRF_type)        , INTENT(IN)  :: SRF_RHS
    INTEGER     , OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER     , OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_SRF'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: n

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Default precision is a single unit in last place
    ULP = 1
    ! ... unless the ULP_Scale argument is set and positive
    IF ( PRESENT( ULP_Scale ) ) THEN
      IF ( ULP_Scale > 0 ) ULP = ULP_Scale
    END IF

    ! Default action is to return on ANY difference...
    Check_Once = .TRUE.
    ! ...unless the Check_All argument is set
    IF ( PRESENT( Check_All ) ) THEN
      IF ( Check_All == SET ) Check_Once = .FALSE.
    END IF

    ! Check the structure association status
    IF ( .NOT. Associated_SRF( SRF_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SRF_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( .NOT. Associated_SRF( SRF_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT SRF_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check Release/Version info
    ! --------------------------
    IF ( ( SRF_LHS%Release /= SRF_RHS%Release ) .OR. &
         ( SRF_LHS%Version /= SRF_RHS%Version )      ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Release/Version numbers are different : ",&
                 &i2,".",i2.2," vs. ",i2,".",i2.2)' ) &
                      SRF_LHS%Release, SRF_LHS%Version, &
                      SRF_RHS%Release, SRF_RHS%Version
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! Check dimensions
    ! ----------------
    IF ( SRF_LHS%n_Points /= SRF_RHS%n_Points ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("n_Points dimensions are different : ",i0," vs. ",i0)' ) &
                 SRF_LHS%n_Points, SRF_RHS%n_Points
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF
    IF ( SRF_LHS%n_Bands /= SRF_RHS%n_Bands ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("n_Bands dimensions are different : ",i0," vs. ",i0)' ) &
                 SRF_LHS%n_Bands, SRF_RHS%n_Bands
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF


    ! Compare the values
    ! ------------------
    ! The Sensor_ID
    IF ( SRF_LHS%Sensor_Id /= SRF_RHS%Sensor_Id ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Sensor_ID values are different, ",a," vs. ",a)' ) &
                 TRIM( SRF_LHS%Sensor_Id), TRIM( SRF_RHS%Sensor_Id)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The WMO Satellite ID
    IF ( SRF_LHS%WMO_Satellite_ID /= SRF_RHS%WMO_Satellite_ID ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("WMO_Satellite_ID values are different, ",i0," vs. ",i0 )' ) &
                 SRF_LHS%WMO_Satellite_ID, SRF_RHS%WMO_Satellite_ID
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The WMO Sensor ID
    IF ( SRF_LHS%WMO_Sensor_ID /= SRF_RHS%WMO_Sensor_ID ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("WMO_Sensor_ID values are different, ",i0," vs. ",i0 )' ) &
                 SRF_LHS%WMO_Sensor_ID, SRF_RHS%WMO_Sensor_ID
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The Sensor_Type
    IF ( SRF_LHS%Sensor_Type /= SRF_RHS%Sensor_Type ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Sensor types are different, ",i0,"(",a,") vs. ",i0,"(",a,")")' ) &
                 SRF_LHS%Sensor_Type, TRIM(SENSOR_TYPE_NAME(SRF_LHS%Sensor_Type)), &
                 SRF_RHS%Sensor_Type, TRIM(SENSOR_TYPE_NAME(SRF_RHS%Sensor_Type))
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The Channel
    IF ( SRF_LHS%Channel /= SRF_RHS%Channel ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Channel values are different, ",i0," vs. ",i0 )' ) &
                 SRF_LHS%Channel, SRF_RHS%Channel
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The Integrated_SRF
    IF ( .NOT. Compare_Float( SRF_LHS%Integrated_SRF,SRF_RHS%Integrated_SRF,ULP=ULP ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Integrated_SRF values are different, ",es13.6," vs. ",es13.6)' ) &
                 SRF_LHS%Integrated_SRF,SRF_RHS%Integrated_SRF
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The Summation_SRF
    IF ( .NOT. Compare_Float( SRF_LHS%Summation_SRF,SRF_RHS%Summation_SRF,ULP=ULP ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Summation_SRF values are different, ",es13.6," vs. ",es13.6)' ) &
                 SRF_LHS%Summation_SRF,SRF_RHS%Summation_SRF
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! The f1_Band values
    DO n = 1, SRF_RHS%n_Bands
      IF ( .NOT. Compare_Float( SRF_LHS%f1_Band(n),SRF_RHS%f1_Band(n),ULP=ULP ) ) THEN
        Error_Status = FAILURE
        WRITE( msg,'("f1_Band ",i0," values are different, ",es13.6," vs. ",es13.6)' ) &
                   n, SRF_LHS%f1_Band(n),SRF_RHS%f1_Band(n)
        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The f2_Band values
    DO n = 1, SRF_RHS%n_Bands
      IF ( .NOT. Compare_Float( SRF_LHS%f2_Band(n),SRF_RHS%f2_Band(n),ULP=ULP ) ) THEN
        Error_Status = FAILURE
        WRITE( msg,'("f2_Band ",i0," values are different, ",es13.6," vs. ",es13.6)' ) &
                   n, SRF_LHS%f2_Band(n),SRF_RHS%f2_Band(n)
        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The npts_Band values
    DO n = 1, SRF_RHS%n_Bands
      IF ( SRF_LHS%npts_Band(n) /= SRF_RHS%npts_Band(n) ) THEN
        Error_Status = FAILURE
        WRITE( msg,'("npts_Band ",i0," values are different, ",i0," vs. ",i0)' ) &
                   n, SRF_LHS%npts_Band(n),SRF_RHS%npts_Band(n)
        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO

    ! The Frequency values
    DO n = 1, SRF_RHS%n_Points
      IF ( .NOT. Compare_Float( SRF_LHS%Frequency(n),SRF_RHS%Frequency(n),ULP=ULP ) ) THEN
        Error_Status = FAILURE
        WRITE( msg,'("Frequency values are different, ",es13.6," vs. ",es13.6," at point ",i0)' ) &
                   SRF_LHS%Frequency(n),SRF_RHS%Frequency(n), n
        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
  
  END FUNCTION Equal_SRF


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CheckRelease_SRF
!
! PURPOSE:
!       Function to check the SRF Release value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckRelease_SRF( SRF                    , &  ! Input
!                                        RCS_Id     =RCS_Id     , &  ! Revision control
!                                        Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SRF:           SRF structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TYPE(SRF_type)
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
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
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
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION CheckRelease_SRF( SRF        , &  ! Input
                             RCS_Id     , &  ! Revision control
                             Message_Log) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    TYPE(SRF_type)        , INTENT(IN)  :: SRF
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckRelease_SRF'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID


    ! Check the release
    ! -----------------
    ! Check that release is not too old
    IF ( SRF%Release < SRF_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("An SRF data update is needed. SRF release is ",i2,". Valid release is ",i2)' ) &
                 SRF%Release, SRF_RELEASE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF

    ! Check that release is not too new
    IF ( SRF%Release > SRF_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("An SRF software update is needed. SRF release is ",i2,". Valid release is ",i2)' ) &
                 SRF%Release, SRF_RELEASE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckRelease_SRF


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Info_SRF
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the SRF data structure.
!
! CALLING SEQUENCE:
!       CALL Info_SRF( SRF          , &  ! Input
!                      Info         , &  ! Output
!                      RCS_Id=RCS_Id  )  ! Revision control
!
! INPUT ARGUMENTS:
!       SRF:           SRF structure.
!                      UNITS:      N/A
!                      TYPE:       TYPE(SRF_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed SRF data structure.
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
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Info_SRF( SRF   , &  ! Input
                       Info  , &  ! Output
                       RCS_Id  )  ! Revision control
    ! Arguments
    TYPE(SRF_type)        , INTENT(IN)  :: SRF
    CHARACTER(*)          , INTENT(OUT) :: Info
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    ! Local variables
    CHARACTER(2000) :: LongString

    ! Set up
    ! ------
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Write the required data to the local string
    ! -------------------------------------------
    WRITE( LongString,'(a,1x,"SRF RELEASE.VERSION: ",i0,".",i2.2,2x,&
                       &a," CHANNEL:",i0,2x,&
                       &"N_BANDS=",i0,2x,&
                       &"N_POINTS=",i0)' ) &
                       ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                       SRF%Release, SRF%Version, &
                       TRIM(SRF%Sensor_ID), SRF%Channel, &
                       SRF%n_Bands, SRF%n_Points

    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------
    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

  END SUBROUTINE Info_SRF


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Frequency_SRF
!
! PURPOSE:
!       Function to compute the frequency grid for a supplied SRF data
!       structure.
!
! CALLING SEQUENCE:
!       Error_Status = Frequency_SRF( SRF                    , &  ! In/Output
!                                     Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SRF:          SRF structure with fields containing the begin and
!                     end frequencies of the frequency grid to compute.
!                     UNITS:      N/A
!                     TYPE:       TYPE(SRF_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       SRF:          SRF structure with the frequency component filled.
!                     UNITS:      N/A
!                     TYPE:       TYPE(SRF_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the frequency grid calculation was successful
!                        == FAILURE an error occurred processing the input
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The FREQUENCY field of the input SRF structure is filled.
!
! RESTRICTIONS:
!       SRF structure must contain at least 2 points of frequency and response
!       data.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Frequency_SRF( SRF        , &  ! In/Output
                          Message_Log) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(SRF_type),         INTENT(IN OUT) :: SRF
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Frequency_SRF'
    ! Local variables
    INTEGER :: i1, i2, i, m, n

    ! Setup
    ! -----
    Error_Status = SUCCESS

    ! ALL pointers must be associated
    IF ( .NOT. Associated_SRF( SRF ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SRF pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the number of bands
    IF ( SRF%n_Bands < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF structure must contain at least 1 band', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the number of points
    IF ( SRF%n_Points < 2 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF structure must contain at least 2 points.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the number of points in each band
    IF ( ANY(SRF%npts_Band < 2) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF must contain at least 2 points for each band.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the total points                                                   
    IF ( SUM(SRF%npts_Band) /= SRF%n_Points ) THEN                            
      Error_Status = FAILURE                                             
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF must have consistent data points.', &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF                                                               


    ! Compute the SRF frequency grid
    ! ------------------------------
    ! Initialise the offset counter
    n = 0
    ! Loop over the number of bands
    DO m = 1, SRF%n_Bands
      ! The point limits for this band
      i1 = n + 1
      i2 = SRF%npts_Band(m) + n
      ! Construct a frequency grid of 0->1 for this band
      SRF%Frequency(i1:i2) = (/(REAL(i-1,fp),i=1,SRF%npts_Band(m))/) / REAL(SRF%npts_Band(m)-1,fp)
      SRF%Frequency(i1:i2) = SRF%f1_Band(m) + &
                             ( SRF%Frequency(i1:i2) * (SRF%f2_Band(m)-SRF%f1_Band(m)) )
      ! Update the offset counter
      n = n + SRF%npts_Band(m)
    END DO

  END FUNCTION Frequency_SRF


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Integrate_SRF
!
! PURPOSE:
!       Function to integrate the response supplied in an SRF data
!       structure.
!
! CALLING SEQUENCE:
!       Error_Status = Integrate_SRF( SRF                    , &  ! In/Output
!                                     Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SRF:          SRF structure with fields containing the frequency
!                     and response arrays.
!                     UNITS:      N/A
!                     TYPE:       TYPE(SRF_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       SRF:          SRF structure with the integration components filled.
!                     UNITS:      N/A
!                     TYPE:       TYPE(SRF_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the integration was successful
!                        == FAILURE an error occurred processing the input
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The INTEGRATED_SRF and SUMMATION_SRF fields of the input SRF structure
!       are filled.
!
! RESTRICTIONS:
!       SRF structure must contain at least 2 points of frequency and response
!       data.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Integrate_SRF( SRF        , &  ! In/Output
                          Message_Log) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    TYPE(SRF_type),         INTENT(IN OUT) :: SRF
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Integrate_SRF'
    ! Local variables
    CHARACTER(ML) :: msg
    REAL(fp) :: dF
    INTEGER :: i1, i2, m, n
    REAL(fp):: Int_SRF, Sum_SRF

    ! Setup
    ! -----
    Error_Status = SUCCESS
    
    ! ALL pointers must be associated
    IF ( .NOT. Associated_SRF( SRF ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SRF pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the number of bands
    IF ( SRF%n_Bands < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF structure must contain at least 1 band', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the number of points
    IF ( SRF%n_Points < 2 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF structure must contain at least 2 points.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the number of points in each band
    IF ( ANY(SRF%npts_Band < 2) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF must contain at least 2 points for each band.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the total points                                                   
    IF ( SUM(SRF%npts_Band) /= SRF%n_Points ) THEN                            
      Error_Status = FAILURE                                             
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF must have consistent data points.', &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF


    ! Check the number of points
    n = SRF%n_Points
    m = SRF%n_Bands


    ! Compute the SRF integrals
    ! -------------------------
    ! Initialisation of sums
    SRF%Integrated_SRF = ZERO
    SRF%Summation_SRF  = ZERO
    ! Initialise the offset counter
    n = 0

    ! Loop over the bands
    DO m = 1, SRF%n_Bands
    
      ! The point limits for this band
      i1 = n + 1
      i2 = SRF%npts_Band(m) + n
      
      ! Integration using Simpson's rule
      ! --------------------------------                                            
      Error_Status = Integral( SRF%Frequency(i1:i2), &
                               SRF%Response(i1:i2),  &
                               Int_SRF               )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( msg,'("Error occurred integrating channel ",i0," SRF")' ) SRF%Channel
        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF
      
      ! Integration by simple summation
      ! -------------------------------
      ! Compute the average frequency grid interval
      dF = SUM(SRF%Frequency(i1+1:i2 ) - SRF%Frequency(i1:i2-1)) / REAL(SRF%npts_Band(m)-1,fp)
      ! Do the summation
      Sum_SRF = SUM(SRF%Response(i1:i2)) * dF
      
      ! Accumulate the band sums                                                                        
      SRF%Integrated_SRF = SRF%Integrated_SRF + Int_SRF
      SRF%Summation_SRF  = SRF%Summation_SRF  + Sum_SRF

      ! Update the offset counter
      n = n + SRF%npts_Band(m)
    END DO

  END FUNCTION Integrate_SRF


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------------
!
! NAME:
!       Clear_SRF
!
! PURPOSE:
!       Subroutine to clear the scalar members of an SRF structure.
!
! CALLING SEQUENCE:
!       CALL Clear_SRF( SRF )
!
! OUTPUT ARGUMENTS:
!       SRF:         SRF structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       TYPE(SRF_type)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output SRF argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------------

  SUBROUTINE Clear_SRF( SRF )
    TYPE(SRF_type), INTENT(IN OUT) :: SRF
    
    SRF%Release = SRF_RELEASE
    SRF%Version = SRF_VERSION

    SRF%Sensor_ID        = ' '
    SRF%WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    SRF%WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    SRF%Sensor_Type      = INVALID_SENSOR
    
    SRF%Channel      = INVALID

    SRF%Integrated_SRF = ZERO
    SRF%Summation_SRF  = ZERO
 
  END SUBROUTINE Clear_SRF

END MODULE SRF_Define

 
