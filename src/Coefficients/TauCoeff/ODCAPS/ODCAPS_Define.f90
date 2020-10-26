
MODULE ODCAPS_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds,      ONLY: Long, Double, Single
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, Display_Message
   
  USE ODCAPS_ODAS_Define
  USE ODCAPS_TraceGas_Define
  USE ODCAPS_Subset_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Public procedures to manipulate the TauCoeff_SARTA structure
  PUBLIC :: Associated_ODCAPS
  PUBLIC :: Destroy_ODCAPS
  PUBLIC :: Allocate_ODCAPS
  PUBLIC :: Assign_ODCAPS
  PUBLIC :: CheckRelease_ODCAPS
  PUBLIC :: CheckAlgorithm_ODCAPS
  PUBLIC :: Info_ODCAPS

  ! Public parameters
  ! -----------------
  ! Sensor Id defaults
  PUBLIC :: INVALID_WMO_SATELLITE_ID
  PUBLIC :: INVALID_WMO_SENSOR_ID
  ! Allowable sensor type values and names
  PUBLIC :: N_SENSOR_TYPES
  PUBLIC :: INVALID_SENSOR  
  PUBLIC :: MICROWAVE_SENSOR
  PUBLIC :: INFRARED_SENSOR 
  PUBLIC :: VISIBLE_SENSOR  
  PUBLIC :: SENSOR_TYPE_NAME
  ! The Global unique algorithm ID
  PUBLIC :: ODCAPS_ALGORITHM


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: ODCAPS_Define.f90,v 1.10 2006/05/03 19:42:09 ychen Exp $'

  ! -- ODCAPS valid values
  INTEGER,      PARAMETER :: IP_INVALID = -1
  REAL(Double), PARAMETER :: FP_INVALID = -1.0_Double
 
  ! -- Keyword set value
  INTEGER, PARAMETER :: SET = 1

  ! String lengths
  INTEGER, PARAMETER :: SL = 20   ! Sensor Id
  INTEGER, PARAMETER :: ML = 256  ! Messages

  ! -- Current valid release and version numbers
  INTEGER, PARAMETER :: ODCAPS_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: ODCAPS_VERSION = 1  ! This is just the data version.

  ! The optical depth algorithm Id
  INTEGER     , PARAMETER :: ODCAPS_ALGORITHM = 3
  CHARACTER(*), PARAMETER :: ODCAPS_ALGORITHM_NAME = 'ODCAPS'

  ! ASCII codes for Version routine
  INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
  INTEGER, PARAMETER :: LINEFEED = 10
  ! Invalid sensor ids
  INTEGER, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047
  ! The instrument types
  INTEGER, PARAMETER :: N_SENSOR_TYPES     = 4
  INTEGER, PARAMETER :: INVALID_SENSOR     = 0
  INTEGER, PARAMETER :: MICROWAVE_SENSOR   = 1
  INTEGER, PARAMETER :: INFRARED_SENSOR    = 2
  INTEGER, PARAMETER :: VISIBLE_SENSOR     = 3
  INTEGER, PARAMETER :: ULTRAVIOLET_SENSOR = 4
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_SENSOR_TYPES ) :: &
    SENSOR_TYPE_NAME = (/ 'Invalid    ', &
                          'Microwave  ', &
                          'Infrared   ', &
                          'Visible    ', &
                          'Ultraviolet' /)
 
  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! ------------------------------
  ! ODCAPS data type definition
  ! ------------------------------

  TYPE, PUBLIC :: ODCAPS_type
    INTEGER :: n_Allocates = 0

    ! -- Release and version information
    INTEGER( Long ) :: Release = ODCAPS_RELEASE
    INTEGER( Long ) :: Version = ODCAPS_VERSION
    ! Algorithm identifer
    INTEGER( Long ) :: Algorithm = ODCAPS_ALGORITHM

    ! -- Array dimensions
    INTEGER( Long ) :: n_Layers            = 0    ! Ilayer 100 layers
    INTEGER( Long ) :: n_Channels          = 0    ! L AIRS 2378 channels

    INTEGER( Long ) :: n_Tunings           = 0    ! J  7 tuning multipliers
    INTEGER( Long ) :: n_F_Predictors      = 0    ! MF 6 thermal "F" factor coefs
    INTEGER (Long ) :: n_RefProfile_Items  = 0    ! N  12 item 
    INTEGER (Long ) :: n_NONLTE_Predictors = 0    ! MNON 6 coefs
    INTEGER (Long ) :: n_NONLTE_Channels   = 0    ! MChannels 201 channels

    INTEGER( Long ) :: n_TraceGases        = 0    ! KTraces 4 trace gases perturbation: CO2, SO2, HNO3 N2O 
    INTEGER( Long ) :: n_Subsets           = 0    ! K  7 Tau coefficients subsets
     
    ! Scalar components
    CHARACTER(SL) :: Sensor_Id        = ' '
    INTEGER(Long) :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER(Long) :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    INTEGER(Long) :: Sensor_Type      = INVALID_SENSOR

    ! Water vapor OPTRAN coefficients data
    TYPE (ODCAPS_ODAS_type), POINTER :: ODCAPS_ODAS => NULL()  ! scalar
   
    ! -- The actual sensor channel numbers
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Sensor_Channel             => NULL()  ! L  value from 1-2378

    ! -- The set number for each channel
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_Subset_Index      => NULL()  ! L  value 1-7

    ! -- The position in subset for each channel
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_Subset_Position   => NULL()  ! L

    ! -- The channels for H2O using OPTRAN
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_H2O_OPTRAN        => NULL()  ! L

    ! -- The channels for CO2 perturbation
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_CO2_Perturbation  => NULL()  ! L

    ! -- The channels for SO2 perturbation
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_SO2_Perturbation  => NULL()  ! L

    ! -- The channels for HNO3 perturbation
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_HNO3_Perturbation => NULL()  ! L

    ! -- The channels for N2O perturbation
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_N2O_Perturbation  => NULL()  ! L

    ! -- The channels for non-LTE effects for strong CO2 absorption at the upper atmosphere (~4um)
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_NON_LTE           => NULL()  ! L
 
    ! -- The standard 101 levels pressure 
    REAL( Single ),  POINTER, DIMENSION( : ) :: Standard_Level_Pressure   => NULL()  ! 0:Ilayer

    ! -- The fix gases adjustment for each layer 
    REAL( Single ),  POINTER, DIMENSION( : ) :: Fix_Gases_Adjustment      => NULL()  ! Ilayer

    ! -- The downward thermal F factor coefficients.
    REAL( Single ),  POINTER, DIMENSION( :, : ) :: Down_F_Factor          => NULL()  ! MF x L
 
    ! -- Tuning_Multiple array containing the absorber coefficients tuning multiplies
    REAL( Single ),  POINTER, DIMENSION( :, : ) :: Tuning_Multiple        => NULL()  ! J x L
    
    ! -- Reference Profile data array containing the reference profile for SARTA
    ! -- have 12 column, 1) Layer average altitude (m), 2) layer thickness (m), 
    ! -- 3) layer slab averge pressure (atm), 4) layer slab average temperature (K)
    ! -- 5) fixed gases amount (k.mol/cm2), 6) water amount (k.mol/cm)2
    ! -- 7) ozone amount, 8) carbon monoxide amount, 9) methane amount
    ! --10) SO2, 11) HNO3, 12) N2O
    REAL( Single ),  POINTER, DIMENSION( :, : ) :: Ref_Profile_Data       => NULL()  ! n x ILayer

    ! NON-LTE Coefficients
    REAL( Single ),  POINTER, DIMENSION( :, : ) :: Non_LTE_Coeff          => NULL()  ! MNON x MChannels


    ! Trace Gases Tau Coefficients Subset
    TYPE (ODCAPS_TraceGas_type), POINTER, DIMENSION( : ) :: ODCAPS_TraceGas  => NULL() ! KTraces
    
    ! Tau Coefficients Subset
    TYPE (ODCAPS_Subset_type), POINTER, DIMENSION( : ) :: ODCAPS_Subset  => NULL()  ! K

  END TYPE ODCAPS_type


CONTAINS





  SUBROUTINE Clear_ODCAPS( ODCAPS )

    TYPE( ODCAPS_type ), INTENT( IN OUT ) :: ODCAPS

    ODCAPS%n_Layers     = 0
    ODCAPS%n_Channels   = 0

    ODCAPS%n_Tunings     = 0
    ODCAPS%n_F_Predictors     = 0
    ODCAPS%n_RefProfile_Items = 0
    ODCAPS%n_NONLTE_Predictors     = 0
    ODCAPS%n_NONLTE_Channels = 0


    ODCAPS%n_TraceGases   = 0
    ODCAPS%n_Subsets    = 0

    ODCAPS%Release   = ODCAPS_RELEASE
    ODCAPS%Version   = ODCAPS_VERSION
    ODCAPS%Algorithm = ODCAPS_ALGORITHM
    ODCAPS%Sensor_Id        = ' '
    ODCAPS%Sensor_Type      = INVALID_SENSOR
    ODCAPS%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    ODCAPS%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID

  END SUBROUTINE Clear_ODCAPS







  FUNCTION Associated_ODCAPS(   ODCAPS,                & ! Input
                                ANY_Test,                & ! Optional input
				Skip_ODCAPS_ODAS,   & ! Optional input
				Skip_ODCAPS_TraceGas,  & ! Optional input
 				Skip_ODCAPS_Subset)    & ! Optional input
                              RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( ODCAPS_type ), INTENT( IN ) :: ODCAPS

    ! -- Optional input
    INTEGER,     OPTIONAL, INTENT( IN ) :: ANY_Test
    INTEGER,     OPTIONAL, INTENT( IN ) :: Skip_ODCAPS_ODAS 
    INTEGER,     OPTIONAL, INTENT( IN ) :: Skip_ODCAPS_TraceGas 
    INTEGER,     OPTIONAL, INTENT( IN ) :: Skip_ODCAPS_Subset

    ! ---------------
    ! Function result
    ! ---------------

    LOGICAL :: Association_Status


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: ALL_Test
    LOGICAL :: Include_ODCAPS_ODAS 
    LOGICAL :: Include_ODCAPS_TraceGas 
    LOGICAL :: Include_ODCAPS_Subset



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! -- Default is to test ALL the pointer members
    ! -- for a true association status....
    ALL_Test = .TRUE.

    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF

    ! -----------------------------------------
    ! The Skip_ODCAPS_ODAS optional argument
    ! -----------------------------------------

    ! -- Default is to include the ODCAPS_ODAS member
    ! -- in the association test....
    Include_ODCAPS_ODAS = .TRUE.

    ! ...unless the Skip_ODCAPS_ODAS argument is set.
    IF ( PRESENT( Skip_ODCAPS_ODAS ) ) THEN
      IF ( Skip_ODCAPS_ODAS == SET ) Include_ODCAPS_ODAS = .FALSE.
    END IF

    ! -----------------------------------------
    ! The Skip_ODCAPS_TraceGas optional argument
    ! -----------------------------------------

    ! -- Default is to include the ODCAPS_TraceGas member
    ! -- in the association test....
    Include_ODCAPS_TraceGas = .TRUE.

    ! ...unless the Skip_ODCAPS_TraceGas argument is set.
    IF ( PRESENT( Skip_ODCAPS_TraceGas ) ) THEN
      IF ( Skip_ODCAPS_TraceGas == SET ) Include_ODCAPS_TraceGas = .FALSE.
    END IF


    ! -----------------------------------------
    ! The Skip_ODCAPS_Subset optional argument
    ! -----------------------------------------

    ! -- Default is to include the ODCAPS_Subset member
    ! -- in the association test....
    Include_ODCAPS_Subset = .TRUE.

    ! ...unless the Skip_ODCAPS_Subset argument is set.
    IF ( PRESENT( Skip_ODCAPS_Subset ) ) THEN
      IF ( Skip_ODCAPS_Subset == SET ) Include_ODCAPS_Subset = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( ODCAPS%Sensor_Channel             ) .AND. &
           ASSOCIATED( ODCAPS%Channel_Subset_Index      ) .AND. &
           ASSOCIATED( ODCAPS%Channel_Subset_Position   ) .AND. &
           ASSOCIATED( ODCAPS%Channel_H2O_OPTRAN        ) .AND. &
           ASSOCIATED( ODCAPS%Channel_CO2_Perturbation  ) .AND. &
           ASSOCIATED( ODCAPS%Channel_SO2_Perturbation  ) .AND. &
           ASSOCIATED( ODCAPS%Channel_HNO3_Perturbation ) .AND. &
           ASSOCIATED( ODCAPS%Channel_N2O_Perturbation  ) .AND. &
           ASSOCIATED( ODCAPS%Channel_NON_LTE           ) .AND. &
           ASSOCIATED( ODCAPS%Standard_Level_Pressure   ) .AND. &
           ASSOCIATED( ODCAPS%Fix_Gases_Adjustment      ) .AND. &
           ASSOCIATED( ODCAPS%Down_F_Factor             ) .AND. &
           ASSOCIATED( ODCAPS%Tuning_Multiple           ) .AND. &
           ASSOCIATED( ODCAPS%Ref_Profile_Data          ) .AND. &
           ASSOCIATED( ODCAPS%Non_LTE_Coeff             )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( ODCAPS%Sensor_Channel             ) .OR. &
           ASSOCIATED( ODCAPS%Channel_Subset_Index      ) .OR. &
           ASSOCIATED( ODCAPS%Channel_Subset_Position   ) .OR. &
           ASSOCIATED( ODCAPS%Channel_H2O_OPTRAN        ) .OR. &
           ASSOCIATED( ODCAPS%Channel_CO2_Perturbation  ) .OR. &
           ASSOCIATED( ODCAPS%Channel_SO2_Perturbation  ) .OR. &
           ASSOCIATED( ODCAPS%Channel_HNO3_Perturbation ) .OR. &
           ASSOCIATED( ODCAPS%Channel_N2O_Perturbation  ) .OR. &
           ASSOCIATED( ODCAPS%Channel_NON_LTE           ) .OR. &
           ASSOCIATED( ODCAPS%Standard_Level_Pressure   ) .OR. &
           ASSOCIATED( ODCAPS%Fix_Gases_Adjustment      ) .OR. &
           ASSOCIATED( ODCAPS%Down_F_Factor             ) .OR. &
           ASSOCIATED( ODCAPS%Tuning_Multiple           ) .OR. &
           ASSOCIATED( ODCAPS%Ref_Profile_Data          ) .OR. &
           ASSOCIATED( ODCAPS%Non_LTE_Coeff             )       ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

    ! ---------------------------------------
    ! Test the members that MAY be associated
    ! ---------------------------------------

    ! -- ODCAPS_ODAS  
    IF ( Include_ODCAPS_ODAS ) THEN

      IF ( ALL_Test ) THEN

        IF ( Association_Status             .AND. &
             ASSOCIATED( ODCAPS%ODCAPS_ODAS )       ) THEN
          Association_Status = .TRUE.
        END IF

      ELSE

        IF ( Association_Status             .OR. &
             ASSOCIATED( ODCAPS%ODCAPS_ODAS )      ) THEN
          Association_Status = .TRUE.
        END IF

      END IF

    END IF

    ! -- ODCAPS_TraceGas 
    IF ( Include_ODCAPS_TraceGas ) THEN

      IF ( ALL_Test ) THEN

        IF ( Association_Status             .AND. &
             ASSOCIATED( ODCAPS%ODCAPS_TraceGas )       ) THEN
          Association_Status = .TRUE.
        END IF

      ELSE

        IF ( Association_Status             .OR. &
             ASSOCIATED( ODCAPS%ODCAPS_TraceGas )      ) THEN
          Association_Status = .TRUE.
        END IF

      END IF

    END IF

    ! -- ODCAPS_Subset
    IF ( Include_ODCAPS_Subset ) THEN

      IF ( ALL_Test ) THEN

        IF ( Association_Status             .AND. &
             ASSOCIATED( ODCAPS%ODCAPS_Subset )       ) THEN
          Association_Status = .TRUE.
        END IF

      ELSE

        IF ( Association_Status             .OR. &
             ASSOCIATED( ODCAPS%ODCAPS_Subset )      ) THEN
          Association_Status = .TRUE.
        END IF

      END IF

    END IF

  END FUNCTION Associated_ODCAPS






  FUNCTION Destroy_ODCAPS(   ODCAPS,       &  ! Output
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
    TYPE( ODCAPS_type ),    INTENT( IN OUT )   :: ODCAPS

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_Clear

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_ODCAPS'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message
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
    !#                     -- PERFORM RE-INITIALISATION --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_ODCAPS( ODCAPS )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_ODCAPS( ODCAPS ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the channel index array
    IF ( ASSOCIATED( ODCAPS%Sensor_Channel ) ) THEN

      DEALLOCATE( ODCAPS%Sensor_Channel, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Sensor_Channel ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channel subset index array
    IF ( ASSOCIATED( ODCAPS%Channel_Subset_Index ) ) THEN

      DEALLOCATE( ODCAPS%Channel_Subset_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Channel_Subset_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channel subset position array
    IF ( ASSOCIATED( ODCAPS%Channel_Subset_Position ) ) THEN

      DEALLOCATE( ODCAPS%Channel_Subset_Position, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Channel_Subset_Position ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for H2O using OPTRAN array
    IF ( ASSOCIATED( ODCAPS%Channel_H2O_OPTRAN ) ) THEN

      DEALLOCATE( ODCAPS%Channel_H2O_OPTRAN, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Channel_H2O_OPTRAN ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for CO2 Perturbation array
    IF ( ASSOCIATED( ODCAPS%Channel_CO2_Perturbation ) ) THEN

      DEALLOCATE( ODCAPS%Channel_CO2_Perturbation, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Channel_CO2_Perturbation ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for SO2 Perturbation array
    IF ( ASSOCIATED( ODCAPS%Channel_SO2_Perturbation ) ) THEN

      DEALLOCATE( ODCAPS%Channel_SO2_Perturbation, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Channel_SO2_Perturbation ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for HNO3 Perturbation array
    IF ( ASSOCIATED( ODCAPS%Channel_HNO3_Perturbation ) ) THEN

      DEALLOCATE( ODCAPS%Channel_HNO3_Perturbation, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Channel_HNO3_Perturbation ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for N2O Perturbation array
    IF ( ASSOCIATED( ODCAPS%Channel_N2O_Perturbation ) ) THEN

      DEALLOCATE( ODCAPS%Channel_N2O_Perturbation, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Channel_N2O_Perturbation ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for non-LTE effects array
    IF ( ASSOCIATED( ODCAPS%Channel_NON_LTE ) ) THEN

      DEALLOCATE( ODCAPS%Channel_NON_LTE, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Channel_NON_LTE ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate standard levels pressure array
    IF ( ASSOCIATED( ODCAPS%Standard_Level_Pressure ) ) THEN

      DEALLOCATE( ODCAPS%Standard_Level_Pressure, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Standard_Level_Pressure ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate fix gases adjustment array
    IF ( ASSOCIATED( ODCAPS%Fix_Gases_Adjustment ) ) THEN

      DEALLOCATE( ODCAPS%Fix_Gases_Adjustment, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Fix_Gases_Adjustment ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Down_F_Factor
    IF ( ASSOCIATED( ODCAPS%Down_F_Factor ) ) THEN

      DEALLOCATE( ODCAPS%Down_F_Factor, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Down_F_Factor ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Tuning_Multiple
    IF ( ASSOCIATED( ODCAPS%Tuning_Multiple ) ) THEN

      DEALLOCATE( ODCAPS%Tuning_Multiple, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Tuning_Multiple ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate reference profile data array
    IF ( ASSOCIATED( ODCAPS%Ref_Profile_Data ) ) THEN

      DEALLOCATE( ODCAPS%Ref_Profile_Data, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Ref_Profile_Data ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate non-LTE coefficients data array
    IF ( ASSOCIATED( ODCAPS%Non_LTE_Coeff ) ) THEN

      DEALLOCATE( ODCAPS%Non_LTE_Coeff, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Non_LTE_Coeff ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -------------------------------------------------------------
    ! Deallocate the ODCAPS_ODAS structure pointer member
    ! -------------------------------------------------------------

    IF ( ASSOCIATED( ODCAPS%ODCAPS_ODAS ) ) THEN


      ! -- Destroy the ODCAPS_ODAS structure(s)
      Error_Status = Destroy_ODCAPS_ODAS( ODCAPS%ODCAPS_ODAS, &
                                              No_Clear = No_Clear, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error destroying ODCAPS_ODAS structure(s).', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF

    END IF

    ! -------------------------------------------------------------
    ! Deallocate the ODCAPS_TraceGas structure array pointer member
    ! -------------------------------------------------------------

    IF ( ASSOCIATED( ODCAPS%ODCAPS_TraceGas ) ) THEN


      ! -- Destroy the ODCAPS_TraceGas structure(s)
      Error_Status = Destroy_ODCAPS_TraceGas( ODCAPS%ODCAPS_TraceGas, &
                                              No_Clear = No_Clear, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error destroying ODCAPS_TraceGas structure(s).', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF


      ! -- Deallocate the array
      DEALLOCATE( ODCAPS%ODCAPS_TraceGas, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS_TraceGas", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -------------------------------------------------------------
    ! Deallocate the ODCAPS_Subset structure array pointer member
    ! -------------------------------------------------------------

    IF ( ASSOCIATED( ODCAPS%ODCAPS_Subset ) ) THEN


      ! -- Destroy the ODCAPS_Subset structure(s)
      Error_Status = Destroy_ODCAPS_Subset( ODCAPS%ODCAPS_Subset, &
                                              No_Clear = No_Clear, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error destroying ODCAPS_Subset structure(s).', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF


      ! -- Deallocate the array
      DEALLOCATE( ODCAPS%ODCAPS_Subset, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS_Subset", &
                          &"member. STAT = ", i5 )' ) &
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

    ODCAPS%n_Allocates = ODCAPS%n_Allocates - 1

    IF ( ODCAPS%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      ODCAPS%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_ODCAPS






  FUNCTION Allocate_ODCAPS(   n_Layers,           &  ! Input
                              n_Channels,         &  ! Input
                              n_Tunings,          &  ! Input
			      n_F_Predictors,     &  ! Input
			      n_RefProfile_Items, &  ! Input
			      n_NONLTE_Predictors,&  ! Input
			      n_NONLTE_Channels,  &  ! Input
			      n_TraceGases,       &  ! Input
			      n_Subsets,          &  ! Input
                              ODCAPS,             &  ! Output
                              RCS_Id,             &  ! Revision control
                              Message_Log )       &  ! Error messaging
                            RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )     :: n_Layers
    INTEGER,                  INTENT( IN )     :: n_Channels
    INTEGER,                  INTENT( IN )     :: n_Tunings 
    INTEGER,                  INTENT( IN )     :: n_F_Predictors
    INTEGER,                  INTENT( IN )     :: n_RefProfile_Items
    INTEGER,                  INTENT( IN )     :: n_NONLTE_Predictors 
    INTEGER,                  INTENT( IN )     :: n_NONLTE_Channels
    INTEGER,                  INTENT( IN )     :: n_TraceGases
    INTEGER,                  INTENT( IN )     :: n_Subsets

    ! -- Output
    TYPE( ODCAPS_type ),    INTENT( IN OUT )   :: ODCAPS

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_ODCAPS'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

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
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Dimensions
    ! ----------

    IF ( n_Layers             < 1 .OR. &
         n_Channels           < 1 .OR. &
         n_Tunings            < 1 .OR. &
         n_F_Predictors       < 1 .OR. &
         n_RefProfile_Items   < 1 .OR. &
         n_NONLTE_Predictors  < 1 .OR. &
         n_NONLTE_Channels    < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ODCAPS dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Number of TraceGases. Can be == 0.
    IF ( n_TraceGases < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_TraceGases must be > or = 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Number of Subsets. Can be == 0.
    IF ( n_Subsets < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Subsets must be > or = 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -----------------------------------------------
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_ODCAPS( ODCAPS, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_ODCAPS(   ODCAPS, &
                                       No_Clear = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating ODCAPS pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#
   
    ! -------------------------
    ! The intrinsic type arrays
    ! -------------------------

    ALLOCATE( ODCAPS%Sensor_Channel( n_Channels ), &
              ODCAPS%Channel_Subset_Index( n_Channels ), &
              ODCAPS%Channel_Subset_Position( n_Channels ), &
              ODCAPS%Channel_H2O_OPTRAN( n_Channels ), &
              ODCAPS%Channel_CO2_Perturbation( n_Channels ), &
              ODCAPS%Channel_SO2_Perturbation( n_Channels ), &
              ODCAPS%Channel_HNO3_Perturbation( n_Channels ), &
              ODCAPS%Channel_N2O_Perturbation( n_Channels ), &
              ODCAPS%Channel_NON_LTE( n_Channels ), &
              ODCAPS%Standard_Level_Pressure( 0:n_Layers ), &
              ODCAPS%Fix_Gases_Adjustment( n_Layers ), &
              ODCAPS%Down_F_Factor( n_F_Predictors, n_Channels ), &
              ODCAPS%Tuning_Multiple( n_Tunings, n_Channels ), &
              ODCAPS%Ref_Profile_Data(n_RefProfile_Items, n_Layers ),&
              ODCAPS%Non_LTE_Coeff(n_NONLTE_Predictors, n_NONLTE_Channels ),&
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating ODCAPS data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! ------------------------------
    ! The ODCAPS_ODAS structure 
    ! ------------------------------

    ! -- Allocate the individual structures
    ALLOCATE( ODCAPS%ODCAPS_ODAS, &
                STAT = Allocate_Status )
		
    IF ( Allocate_Status /= 0 ) THEN								   
      Error_Status = FAILURE									   
      WRITE( Message, '( "Error allocating ODCAPS_ODAS structure. STAT = ", i5 )' ) &  
    		      Allocate_Status								   
      CALL Display_Message( ROUTINE_NAME,    &  						   
    			    TRIM( Message ), &  						   
    			    Error_Status,    &  						   
    			    Message_Log = Message_Log ) 					   
      RETURN											   
    END IF											   
		
    ! ---------------------------------------
    ! The ODCAPS_TraceGas structure array
    ! ---------------------------------------

    IF ( n_TraceGases > 0 ) THEN

      ! -- Allocate the structure array
      ALLOCATE( ODCAPS%ODCAPS_TraceGas( n_TraceGases ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error allocating ODCAPS_TraceGas structure array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF
    
    ! -----------------------------------
    ! The ODCAPS_Subset structure array
    ! -----------------------------------

    IF ( n_Subsets > 0 ) THEN

      ! -- Allocate the structure array
      ALLOCATE( ODCAPS%ODCAPS_Subset( n_Subsets ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error allocating ODCAPS Subset structure array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
 
    END IF

    !#--------------------------------------------------------------------------#
    !#                        -- ASSIGN THE DIMENSIONS --                       #
    !#--------------------------------------------------------------------------#

    ODCAPS%n_Layers           = n_Layers
    ODCAPS%n_Channels         = n_Channels
    ODCAPS%n_Tunings          = n_Tunings 
    ODCAPS%n_F_Predictors     = n_F_Predictors
    ODCAPS%n_RefProfile_Items = n_RefProfile_Items
    ODCAPS%n_NONLTE_Predictors= n_NONLTE_Predictors
    ODCAPS%n_NONLTE_Channels  = n_NONLTE_Channels
    ODCAPS%n_TraceGases       = n_TraceGases 
    ODCAPS%n_Subsets          = n_Subsets

  
    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    ODCAPS%n_Allocates = ODCAPS%n_Allocates + 1

    IF ( ODCAPS%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      ODCAPS%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_ODCAPS






  FUNCTION Assign_ODCAPS(   ODCAPS_in,   &  ! Input
                            ODCAPS_out,  &  ! Output
                            RCS_Id,        &  ! Revision control
                            Message_Log )  &  ! Error messaging
                          RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( ODCAPS_type ),    INTENT( IN )     :: ODCAPS_in

    ! -- Output
    TYPE( ODCAPS_type ),    INTENT( IN OUT ) :: ODCAPS_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_ODCAPS'



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
    ! BUT the ODCAPS_TraceGas and/or 
    ! ODCAPS_Subset pointer members may not be.
    ! ---------------------------------------

    IF ( .NOT. Associated_ODCAPS( ODCAPS_in, &
                                    Skip_ODCAPS_TraceGas = SET ) ) THEN
      Error_Status = Destroy_ODCAPS( ODCAPS_out, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN 
        CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT ODCAPS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      END IF			    
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------

    ODCAPS_out%Release           = ODCAPS_in%Release
    ODCAPS_out%Version           = ODCAPS_in%Version
    ODCAPS_out%Algorithm         = ODCAPS_in%Algorithm  
    ODCAPS_out%Sensor_Id         = ODCAPS_in%Sensor_Id
    ODCAPS_out%Sensor_Type       = ODCAPS_in%Sensor_Type
    ODCAPS_out%WMO_Satellite_ID  = ODCAPS_in%WMO_Satellite_ID
    ODCAPS_out%WMO_Sensor_ID     = ODCAPS_in%WMO_Sensor_ID


    ! -----------------
    ! Assign array data
    ! -----------------

    ! -- Allocate data arrays
    Error_Status = Allocate_ODCAPS(   ODCAPS_in%n_Layers,           &
                                      ODCAPS_in%n_Channels,         &	     
                                      ODCAPS_in%n_Tunings,	      &	     
                                      ODCAPS_in%n_F_Predictors,     &	     
                                      ODCAPS_in%n_RefProfile_Items, &	     
                                      ODCAPS_in%n_NONLTE_Predictors,&      
                                      ODCAPS_in%n_NONLTE_Channels,  &   
                                      ODCAPS_in%n_TraceGases,       &     
                                      ODCAPS_in%n_Subsets,          &
                                      ODCAPS_out,                   &
                                      Message_Log = Message_Log )   
                                     		  
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output ODCAPS arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Copy array data

    ODCAPS_out%Sensor_Channel            = ODCAPS_in%Sensor_Channel 
    ODCAPS_out%Channel_Subset_Index     = ODCAPS_in%Channel_Subset_Index 
    ODCAPS_out%Channel_Subset_Position  = ODCAPS_in%Channel_Subset_Position  
    ODCAPS_out%Channel_H2O_OPTRAN       = ODCAPS_in%Channel_H2O_OPTRAN  
    ODCAPS_out%Channel_CO2_Perturbation = ODCAPS_in%Channel_CO2_Perturbation 
    ODCAPS_out%Channel_SO2_Perturbation = ODCAPS_in%Channel_SO2_Perturbation 
    ODCAPS_out%Channel_HNO3_Perturbation= ODCAPS_in%Channel_HNO3_Perturbation 
    ODCAPS_out%Channel_N2O_Perturbation = ODCAPS_in%Channel_N2O_Perturbation 
    ODCAPS_out%Channel_NON_LTE          = ODCAPS_in%Channel_NON_LTE
    ODCAPS_out%Standard_Level_Pressure  = ODCAPS_in%Standard_Level_Pressure
    ODCAPS_out%Fix_Gases_Adjustment     = ODCAPS_in%Fix_Gases_Adjustment 
    ODCAPS_out%Down_F_Factor            = ODCAPS_in%Down_F_Factor
    ODCAPS_out%Tuning_Multiple          = ODCAPS_in%Tuning_Multiple
    ODCAPS_out%Ref_Profile_Data         = ODCAPS_in%Ref_Profile_Data 
    ODCAPS_out%Non_LTE_Coeff            = ODCAPS_in%Non_LTE_Coeff

    ! ---------------------
    ! Assign structure data
    ! ---------------------

    ! -- Copy ODCAPS_ODAS structure
    Error_Status = Assign_ODCAPS_ODAS( ODCAPS_in%ODCAPS_ODAS ,  &  
    					    ODCAPS_out%ODCAPS_ODAS , &  
    					    Message_Log = Message_Log)         

    IF ( Error_Status /= SUCCESS ) THEN 				       
      CALL Display_Message( ROUTINE_NAME, &				       
    			    'Error copying ODCAPS_ODAS structure.', &     
    			    Error_Status, &				       
    			    Message_Log = Message_Log ) 		       
      RETURN								       
    END IF								       
 
    ! -- Copy ODCAPS_TraceGas structure
    IF ( ODCAPS_in%n_TraceGases > 0 ) THEN

      Error_Status = Assign_ODCAPS_TraceGas( ODCAPS_in%ODCAPS_TraceGas,  &
                                               ODCAPS_out%ODCAPS_TraceGas, &
                                               Message_Log = Message_Log)

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error copying ODCAPS_TraceGas structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF

    ! -- Copy ODCAPS_Subset structure
    IF ( ODCAPS_in%n_Subsets > 0 ) THEN

      Error_Status = Assign_ODCAPS_Subset( ODCAPS_in%ODCAPS_Subset, &
                                             ODCAPS_out%ODCAPS_Subset, &
                                             Message_Log = Message_Log)

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error copying ODCAPS_Subset structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF


  END FUNCTION Assign_ODCAPS



  FUNCTION CheckRelease_ODCAPS(    ODCAPS,       &  ! Input
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
    TYPE( ODCAPS_type ),    INTENT( IN )  :: ODCAPS

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CheckRelease_ODCAPS'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
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
    !#               -- CHECK THAT THE RELEASE IS NOT TOO OLD --                #
    !#--------------------------------------------------------------------------#

    IF ( ODCAPS%Release < ODCAPS_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An ODCAPS data update is needed. ", &
                        &"ODCAPS release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      ODCAPS%Release, ODCAPS_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#               -- CHECK THAT THE RELEASE IS NOT TOO NEW --                #
    !#--------------------------------------------------------------------------#

    IF ( ODCAPS%Release > ODCAPS_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An ODCAPS software update is needed. ", &
                        &"ODCAPS release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      ODCAPS%Release, ODCAPS_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION CheckRelease_ODCAPS



  FUNCTION CheckAlgorithm_ODCAPS( ODCAPS       , &  ! Input
                                  RCS_Id     , &  ! Revision control
                                  Message_Log) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    TYPE(ODCAPS_type)     , INTENT(IN)  :: ODCAPS
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckAlgorithm_ODCAPS'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID


    ! Check the algorithm ID
    ! ----------------------
    IF ( ODCAPS%Algorithm /= ODCAPS_ALGORITHM ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'The ODCAPS Algorithm ID check failed. '//&
                            'The data structure is not an ODCAPS structure', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckAlgorithm_ODCAPS



 !------------------------------------------------------------------------------

  SUBROUTINE Info_ODCAPS( ODCAPS,     &  ! Input
                          Version_Info, &  ! Output
                          RCS_Id        )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( ODCAPS_type ),    INTENT( IN )  :: ODCAPS

    ! -- Output
    CHARACTER( * ),           INTENT( OUT ) :: Version_Info

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER(2000) :: Long_String



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

    WRITE( Long_String,'( a,1x,"ODCAPS RELEASE.VERSION: ", i2, ".", i2.2, 2x,&
                           &"Algorithm=", i2, 2x, &
			   &"N_LAYERS=",i3,2x,&
                           &"N_CHANNELS=",i4,2x,&
			   &"N_SUBSETS=",i2,2x,&
			   &"N_F_PREDICTORS=",i2,2x,&
			   &"N_REFPROFILE_ITEMS=",i2)' ) &
                         ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                         ODCAPS%Release, ODCAPS%Version, &
                         ODCAPS%Algorithm, &
			 ODCAPS%n_Layers, &
                         ODCAPS%n_Channels, &
			 ODCAPS%n_Subsets, &
                         ODCAPS%n_F_Predictors, &
			 ODCAPS%n_RefProfile_Items
 

    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Version_Info = Long_String(1:MIN( LEN( Version_Info ), LEN_TRIM( Long_String ) ))

  END SUBROUTINE Info_ODCAPS

END MODULE ODCAPS_Define
