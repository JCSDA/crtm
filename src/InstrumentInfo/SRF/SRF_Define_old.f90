
MODULE SRF_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler
  USE Integrate_Utility


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Associated_SRF
  PUBLIC :: Destroy_SRF
  PUBLIC :: Allocate_SRF
  PUBLIC :: Assign_SRF
  PUBLIC :: Frequency_SRF
  PUBLIC :: Integrate_SRF


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE Destroy_SRF
    MODULE PROCEDURE Destroy_SRF_scalar
    MODULE PROCEDURE Destroy_SRF_rank1
  END INTERFACE ! Destroy_SRF


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id field
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: SRF_Define.f90 774 2007-07-24 18:24:06Z paul.vandelst@noaa.gov $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Literal constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind

  ! -- SRF invalid values
  INTEGER,         PRIVATE, PARAMETER ::    INVALID = -1
  REAL( fp_kind ), PRIVATE, PARAMETER :: FP_INVALID = -1.0_fp_kind

  ! -- SRF character strings length
  INTEGER, PRIVATE, PARAMETER :: SDSL = 64

  ! ------------------------
  ! SRF data type definition
  ! ------------------------

  TYPE, PUBLIC :: SRF_type
    INTEGER :: n_Allocates = 0

    INTEGER :: StrLen = SDSL

    INTEGER :: n_Points = 0

    CHARACTER( SDSL ) :: Sensor_Name   = ' '
    CHARACTER( SDSL ) :: Platform_Name = ' '
    INTEGER :: NCEP_Sensor_Id   = INVALID
    INTEGER :: WMO_Satellite_Id = INVALID
    INTEGER :: WMO_Sensor_Id    = INVALID
    INTEGER :: Channel          = INVALID

    REAL( fp_kind ) :: Begin_Frequency = FP_INVALID
    REAL( fp_kind ) :: End_Frequency   = FP_INVALID
    REAL( fp_kind ) :: Integrated_SRF  = FP_INVALID
    REAL( fp_kind ) :: Summation_SRF   = FP_INVALID

    REAL( fp_kind ), POINTER, DIMENSION( : ) :: Frequency => NULL()
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: Response  => NULL()
  END TYPE SRF_type


CONTAINS







  SUBROUTINE Clear_SRF( SRF )

    TYPE( SRF_type ), INTENT( IN OUT ) :: SRF

    SRF%StrLen = SDSL

    SRF%n_Points = 0

    SRF%Sensor_Name   = ' '
    SRF%Platform_Name = ' '

    SRF%NCEP_Sensor_Id   = INVALID
    SRF%WMO_Satellite_Id = INVALID
    SRF%WMO_Sensor_Id    = INVALID
    SRF%Channel          = INVALID

    SRF%Begin_Frequency = FP_INVALID
    SRF%End_Frequency   = FP_INVALID
    SRF%Integrated_SRF  = FP_INVALID
    SRF%Summation_SRF   = FP_INVALID

  END SUBROUTINE Clear_SRF







  FUNCTION Associated_SRF( SRF, &
                           ANY_Test ) &
                         RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SRF_type ),  INTENT( IN ) :: SRF

    ! -- Optional input
    INTEGER, OPTIONAL, INTENT( IN ) :: ANY_Test


    ! ---------------
    ! Function result
    ! ---------------

    LOGICAL :: Association_Status


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: ALL_Test



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! -- Default is to test ALL the pointer members
    ! -- for a true association status....
    ALL_Test = .TRUE.

    ! -- ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( SRF%Frequency ) .AND. &
           ASSOCIATED( SRF%Response  )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( SRF%Frequency ) .OR. &
           ASSOCIATED( SRF%Response  )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_SRF






  FUNCTION Destroy_SRF_scalar( SRF,          &  ! Output
                               No_Clear,     &  ! Optional input
                               RCS_Id,       &  ! Revision control
                               Message_Log ) &  ! Error messaging
                             RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( SRF_type ),         INTENT( IN OUT ) :: SRF

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_SRF(scalar)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! -- Default is to clear scalar members...
    Clear = .TRUE.
    ! -- ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF


    
    !#--------------------------------------------------------------------------#
    !#                      -- PERFORM REINITIALISATION --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_SRF( SRF )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_SRF( SRF ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the SRF Frequency
    IF ( ASSOCIATED( SRF%Frequency ) ) THEN

      DEALLOCATE( SRF%Frequency, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SRF frequency ", &
                          &"array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the SRF Response
    IF ( ASSOCIATED( SRF%Response ) ) THEN

      DEALLOCATE( SRF%Response, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SRF response ", &
                          &"array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- DECREMENT AND TEST ALLOCATION COUNTER --                #
    !#--------------------------------------------------------------------------#

    SRF%n_Allocates = SRF%n_Allocates - 1

    IF ( SRF%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      SRF%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_SRF_scalar

  FUNCTION Destroy_SRF_rank1( SRF,          &  ! Output
                              No_Clear,     &  ! Optional input
                              RCS_Id,       &  ! Revision control
                              Message_Log ) &  ! Error messaging

                            RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( SRF_type ), DIMENSION( : ), INTENT( IN OUT ) :: SRF

    ! -- Optional input
    INTEGER,        OPTIONAL,         INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ), OPTIONAL,         INTENT( OUT )    :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,         INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_SRF(rank1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Scalar_Status
    INTEGER :: l



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- PERFORM REINITIALISATION --                      #
    !#--------------------------------------------------------------------------#

    DO l = 1, SIZE( SRF )

      ! -- Clear the current structure array element
      Scalar_Status = Destroy_SRF_scalar( SRF( l ), &
                                          No_Clear = No_Clear, &
                                          Message_Log = Message_Log )

      ! -- If it failed, set the return error status, but
      ! -- continue to attempt to destroy structure array
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( i10 )' ) l
        CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying SRF structure array element '//&
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Destroy_SRF_rank1





  FUNCTION Allocate_SRF( n_Points,     &  ! Input
                         SRF,          &  ! Output
                         RCS_Id,       &  ! Revision control
                         Message_Log ) &  ! Error messaging
                       RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )     :: n_Points

    ! -- Output
    TYPE( SRF_type ),         INTENT( IN OUT ) :: SRF

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_SRF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ------------------
    ! Spectral dimension
    ! ------------------

    IF ( n_Points < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input N_POINTS must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_SRF( SRF, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_SRF( SRF, &
                                  No_Clear = SET, &
                                  Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating SRF pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE(  SRF%Frequency( n_Points ), &
               SRF%Response(  n_Points ), &
               STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating SRF data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- ASSIGN THE N_POINTS MEMBER --                      #
    !#--------------------------------------------------------------------------#

    SRF%n_Points = n_Points



    !#--------------------------------------------------------------------------#
    !#                  -- INITIALISE THE POINTER MEMBERS --                    #
    !#--------------------------------------------------------------------------#

    SRF%Frequency = ZERO
    SRF%Response  = ZERO



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    SRF%n_Allocates = SRF%n_Allocates + 1

    IF ( SRF%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      SRF%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_SRF







  FUNCTION Assign_SRF( SRF_in,       &  ! Input
                       SRF_out,      &  ! Output
                       Scalar_Only,  &  ! Optional input
                       RCS_Id,       &  ! Revision control
                       Message_Log ) &  ! Error messaging
                     RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SRF_type ),         INTENT( IN )     :: SRF_in

    ! -- Output
    TYPE( SRF_type ),         INTENT( IN OUT ) :: SRF_out

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: Scalar_Only

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_SRF'


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Copy_Arrays



    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE ARGUMENT POINTER ASSOCIATION --          #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------
    ! ALL *input* pointers must be associated
    ! ---------------------------------------

    IF ( .NOT. Associated_SRF( SRF_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT SRF pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CHECK THE OPTIONAL ARGUMENTS --                    #
    !#--------------------------------------------------------------------------#

    ! -- Default is to copy the array components...
    Copy_Arrays = .TRUE.
    ! -- ...unless the Scalar_Only argument is set
    IF ( PRESENT( Scalar_Only ) ) THEN
      IF ( Scalar_Only == SET ) Copy_Arrays = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! Assign scalar members
    ! ---------------------

    SRF_out%Sensor_Name   = SRF_in%Sensor_Name
    SRF_out%Platform_Name = SRF_in%Platform_Name

    SRF_out%NCEP_Sensor_Id   = SRF_in%NCEP_Sensor_Id
    SRF_out%WMO_Satellite_Id = SRF_in%WMO_Satellite_Id
    SRF_out%WMO_Sensor_Id    = SRF_in%WMO_Sensor_Id
    SRF_out%Channel          = SRF_in%Channel

    SRF_out%Begin_Frequency = SRF_in%Begin_Frequency
    SRF_out%End_Frequency   = SRF_in%End_Frequency

    SRF_out%Integrated_SRF = SRF_in%Integrated_SRF
    SRF_out%Summation_SRF  = SRF_in%Summation_SRF


    ! -----------------
    ! Assign array data
    ! -----------------

    IF ( Copy_Arrays ) THEN

      ! -- Allocate data arrays
      Error_Status = Allocate_SRF( SRF_in%n_Points, &
                                   SRF_out,         &
                                   Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error allocating output SRF arrays.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Copy array data
      SRF_out%Frequency = SRF_in%Frequency
      SRF_out%Response  = SRF_in%Response

    END IF

  END FUNCTION Assign_SRF






  FUNCTION Frequency_SRF( SRF,          &  ! In/Output
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SRF_type ),         INTENT( IN OUT ) :: SRF

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Frequency_SRF'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE ARGUMENT POINTER ASSOCIATION --          #
    !#--------------------------------------------------------------------------#

    ! -------------------------------
    ! ALL pointers must be associated
    ! -------------------------------

    IF ( .NOT. Associated_SRF( SRF ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SRF pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CALCULATE THE FREQUENCY GRID --                    #
    !#--------------------------------------------------------------------------#

    ! --------------------------
    ! Check the number of points
    ! --------------------------

    n = SRF%n_Points

    IF ( n < 2 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Allocated SRF structure arrays must contain at least 2 points.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------------
    ! Construct a frequency grid of 0->1
    ! ----------------------------------

    SRF%Frequency(1:n)  = (/ ( REAL( i - 1, fp_kind ), i = 1, n ) /) / &
    !                    ------------------------------------------
                                     REAL( n - 1, fp_kind )


    ! -----------------------------
    ! Scale it to the actual values
    ! -----------------------------

    SRF%Frequency(1:n) = SRF%Begin_Frequency + &
                         ( SRF%Frequency(1:n) * ( SRF%End_Frequency - SRF%Begin_Frequency ) )

  END FUNCTION Frequency_SRF






  FUNCTION Integrate_SRF( SRF,          &  ! In/Output
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SRF_type ),         INTENT( IN OUT ) :: SRF

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Integrate_SRF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: n
    REAL( fp_kind ) :: dF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE ARGUMENT POINTER ASSOCIATION --          #
    !#--------------------------------------------------------------------------#

    ! -------------------------------
    ! ALL pointers must be associated
    ! -------------------------------

    IF ( .NOT. Associated_SRF( SRF ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SRF pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CALCULATE THE INTEGRALS --                       #
    !#--------------------------------------------------------------------------#

    ! --------------------------
    ! Check the number of points
    ! --------------------------

    n = SRF%n_Points

    IF ( n < 2 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Allocated SRF structure arrays must contain at least 2 points.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------
    ! Integration using Simpson's rule
    ! --------------------------------

    Error_Status = Integral( SRF%Frequency, &
                                      SRF%Response,  &
                                      SRF%Integrated_SRF )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error occurred integrating channel ", i5, " SRF" )' ) &
                      SRF%Channel
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------------------------
    ! Integration by simple summation
    ! -------------------------------

    ! -- Compute the frequency grid interval
    dF = SUM( SRF%Frequency( 2:n ) - SRF%Frequency( 1:n-1 ) ) / &
    !    ----------------------------------------------------
                        REAL( n - 1, fp_kind )

    ! -- Do the summation
    SRF%Summation_SRF = SUM( SRF%Response ) * dF

  END FUNCTION Integrate_SRF






  SUBROUTINE Information_SRF( SRF,         &  ! Input
                              Information, &  ! Output
                              RCS_Id       )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SRF_type ),         INTENT( IN )  :: SRF

    ! -- Output
    CHARACTER( * ),           INTENT( OUT ) :: Information

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id


    ! ----------
    ! Parameters
    ! ----------

    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 5000 ) :: Long_String



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- FILL THE VERSION INFO STRING --                   #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------------
    ! Write the required data to the local string
    ! -------------------------------------------

    WRITE( Long_String, FMT = '( a," SRF: N_POINTS=",i6, &
                                &a,"      SENSOR NAME  :", a, &
                                &a,"      PLATFORM NAME:", a, &
                                &a,"      CHANNEL      :", i4  )' ) &
                              ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                              SRF%n_Points, &
                              ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                              TRIM( SRF%Sensor_Name ), &
                              ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                              TRIM( SRF%Platform_Name ), &
                              ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                              SRF%Channel

    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Information = Long_String(1:MIN( LEN( Information ), LEN_TRIM( Long_String ) ))

  END SUBROUTINE Information_SRF

END MODULE SRF_Define


