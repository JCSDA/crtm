
MODULE EmisTestData_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Public procedures to manipulate the EmisTestData structure
  PUBLIC :: Associated_EmisTestData
  PUBLIC :: Destroy_EmisTestData
  PUBLIC :: Allocate_EmisTestData
  PUBLIC :: Assign_EmisTestData


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: EmisTestData_Define.f90,v 1.2 2006/05/02 14:58:35 dgroff Exp $'

  ! -- EmisTestData scalar member invalid value
  INTEGER,        PRIVATE, PARAMETER :: INVALID = -1
  REAL( Double ), PRIVATE, PARAMETER :: FP_INVALID = -1.0_Double

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Observation type string length
  INTEGER, PRIVATE, PARAMETER :: DL = 10


  ! ---------------------------------
  ! EmisTestData data type definition
  ! ---------------------------------

  TYPE, PUBLIC :: EmisTestData_type
    INTEGER :: n_Allocates = 0

    INTEGER( Long ) :: ObsType_StrLen = DL
    INTEGER( Long ) :: n_Channels = 0  ! L dimension

    CHARACTER( DL ) :: ObsType                = ' '
    INTEGER( Long ) :: Channel                =    INVALID
    REAL( Double )  :: Frequency              = FP_INVALID
    REAL( Double )  :: Channel_Polarization   = FP_INVALID
    REAL( Double )  :: Latitude               = FP_INVALID
    REAL( Double )  :: Longitude              = FP_INVALID
    REAL( Double )  :: Satellite_Zenith_Angle = FP_INVALID
    REAL( Double )  :: Satellite_View_Angle   = FP_INVALID
    INTEGER( Long ) :: LandSea_Flag           =    INVALID
    INTEGER( Long ) :: IceSnow_Flag           =    INVALID
    INTEGER( Long ) :: Surface_Type           =    INVALID
    REAL( Double )  :: Wind_Speed_10m         = FP_INVALID
    REAL( Double )  :: Skin_Temperature       = FP_INVALID
    REAL( Double )  :: Snow_Depth             = FP_INVALID
    REAL( Double )  :: Vegetation_Fraction    = FP_INVALID
    REAL( Double )  :: Vegetation_Type        = FP_INVALID
    REAL( Double )  :: Soil_Type              = FP_INVALID
    REAL( Double )  :: Soil_Moisture          = FP_INVALID
    REAL( Double )  :: Soil_Temperature       = FP_INVALID
    REAL( Double )  :: SimulatedTb            = FP_INVALID
    REAL( Double )  :: Emissivity             = FP_INVALID
    REAL( Double )  :: Emissivity_Vertical    = FP_INVALID
    REAL( Double )  :: Emissivity_Horizontal  = FP_INVALID
    REAL( Double ), POINTER, DIMENSION(:) :: ObsTb => NULL()  ! L
  END TYPE EmisTestData_type


CONTAINS






  SUBROUTINE Clear_EmisTestData( EmisTestData )

    TYPE( EmisTestData_type ), INTENT( IN OUT ) :: EmisTestData

    EmisTestData%ObsType_StrLen = DL
    EmisTestData%n_Channels     = 0

    EmisTestData%ObsType                = ' '
    EmisTestData%Channel                =    INVALID
    EmisTestData%Frequency              = FP_INVALID
    EmisTestData%Channel_Polarization   = FP_INVALID
    EmisTestData%Latitude               = FP_INVALID
    EmisTestData%Longitude              = FP_INVALID
    EmisTestData%Satellite_Zenith_Angle = FP_INVALID
    EmisTestData%Satellite_View_Angle   = FP_INVALID
    EmisTestData%LandSea_Flag           =    INVALID
    EmisTestData%IceSnow_Flag           =    INVALID
    EmisTestData%Surface_Type           =    INVALID
    EmisTestData%Wind_Speed_10m         = FP_INVALID
    EmisTestData%Skin_Temperature       = FP_INVALID
    EmisTestData%Snow_Depth             = FP_INVALID
    EmisTestData%Vegetation_Fraction    = FP_INVALID
    EmisTestData%Vegetation_Type        = FP_INVALID
    EmisTestData%Soil_Type              = FP_INVALID
    EmisTestData%Soil_Moisture          = FP_INVALID
    EmisTestData%Soil_Temperature       = FP_INVALID
    EmisTestData%SimulatedTb            = FP_INVALID
    EmisTestData%Emissivity             = FP_INVALID
    EmisTestData%Emissivity_Vertical    = FP_INVALID
    EmisTestData%Emissivity_Horizontal  = FP_INVALID

  END SUBROUTINE Clear_EmisTestData







  FUNCTION Associated_EmisTestData( EmisTestData, & ! Input
                                    ANY_Test )    & ! Optional input
                                  RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( EmisTestData_type ), INTENT( IN ) :: EmisTestData

    ! -- Optional input
    INTEGER,         OPTIONAL, INTENT( IN ) :: ANY_Test


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

    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    ! ONLY ONE ITEM SO THE ANY TEST IS SUPERFLUOUS
    ! BUT I LEFT IT IN

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( EmisTestData%ObsTb ) ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( EmisTestData%ObsTb ) ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_EmisTestData






  FUNCTION Destroy_EmisTestData( EmisTestData, &  ! Output
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
    TYPE( EmisTestData_type ), INTENT( IN OUT ) :: EmisTestData

    ! -- Optional input
    INTEGER,         OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_EmisTestData'


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
    !#                       -- PERFORM REINITIALISATION --                     #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_EmisTestData( EmisTestData )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_EmisTestData( EmisTestData ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the observation brightness temperatures
    IF ( ASSOCIATED( EmisTestData%ObsTb ) ) THEN

      DEALLOCATE( EmisTestData%ObsTb, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating EmisTestData ObsTb ", &
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

    EmisTestData%n_Allocates = EmisTestData%n_Allocates - 1

    IF ( EmisTestData%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      EmisTestData%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_EmisTestData






  FUNCTION Allocate_EmisTestData( n_Channels,   &  ! Input
                                  EmisTestData, &  ! Output       
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
    INTEGER,                   INTENT( IN )     :: n_Channels

    ! -- Output
    TYPE( EmisTestData_type ), INTENT( IN OUT ) :: EmisTestData

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_EmisTestData'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

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

    ! ------------------
    ! Spectral dimension
    ! ------------------

    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input N_CHANNELS must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_EmisTestData( EmisTestData, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_EmisTestData( EmisTestData, &
                                           No_Clear = SET, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating EmisTestData pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( EmisTestData%ObsTb( n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating EmisTestData data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                      -- ASSIGN SOME COMPONENTS --                        #
    !#--------------------------------------------------------------------------#

    ! --------------
    ! The dimensions
    ! --------------

    EmisTestData%n_Channels = n_Channels


    ! --------
    ! The data
    ! --------

    EmisTestData%ObsTb = FP_INVALID



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    EmisTestData%n_Allocates = EmisTestData%n_Allocates + 1

    IF ( EmisTestData%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      EmisTestData%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_EmisTestData






  FUNCTION Assign_EmisTestData( EmisTestData_in,  &  ! Input
                                EmisTestData_out, &  ! Output
                                RCS_Id,           &  ! Revision control
                                Message_Log )     &  ! Error messaging
                              RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( EmisTestData_type ), INTENT( IN )     :: EmisTestData_in

    ! -- Output
    TYPE( EmisTestData_type ), INTENT( IN OUT ) :: EmisTestData_out

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_EmisTestData'



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

    IF ( .NOT. Associated_EmisTestData( EmisTestData_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT EmisTestData pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Allocate the structure
    ! ----------------------

    Error_Status = Allocate_EmisTestData( EmisTestData_in%n_Channels,  &
                                          EmisTestData_out, &
                                          Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output EmisTestData arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------

    EmisTestData_Out%ObsType                = EmisTestData_In%ObsType               
    EmisTestData_Out%Channel                = EmisTestData_In%Channel               
    EmisTestData_Out%Frequency              = EmisTestData_In%Frequency             
    EmisTestData_Out%Channel_Polarization   = EmisTestData_In%Channel_Polarization  
    EmisTestData_Out%Latitude               = EmisTestData_In%Latitude              
    EmisTestData_Out%Longitude              = EmisTestData_In%Longitude             
    EmisTestData_Out%Satellite_Zenith_Angle = EmisTestData_In%Satellite_Zenith_Angle
    EmisTestData_Out%Satellite_View_Angle   = EmisTestData_In%Satellite_View_Angle  
    EmisTestData_Out%LandSea_Flag           = EmisTestData_In%LandSea_Flag          
    EmisTestData_Out%IceSnow_Flag           = EmisTestData_In%IceSnow_Flag          
    EmisTestData_Out%Surface_Type           = EmisTestData_In%Surface_Type          
    EmisTestData_Out%Wind_Speed_10m         = EmisTestData_In%Wind_Speed_10m        
    EmisTestData_Out%Skin_Temperature       = EmisTestData_In%Skin_Temperature      
    EmisTestData_Out%Snow_Depth             = EmisTestData_In%Snow_Depth            
    EmisTestData_Out%Vegetation_Fraction    = EmisTestData_In%Vegetation_Fraction   
    EmisTestData_Out%Vegetation_Type        = EmisTestData_In%Vegetation_Type       
    EmisTestData_Out%Soil_Type              = EmisTestData_In%Soil_Type             
    EmisTestData_Out%Soil_Moisture          = EmisTestData_In%Soil_Moisture         
    EmisTestData_Out%Soil_Temperature       = EmisTestData_In%Soil_Temperature      
    EmisTestData_Out%SimulatedTb            = EmisTestData_In%SimulatedTb           
    EmisTestData_Out%Emissivity             = EmisTestData_In%Emissivity            
    EmisTestData_Out%Emissivity_Vertical    = EmisTestData_In%Emissivity_Vertical   
    EmisTestData_Out%Emissivity_Horizontal  = EmisTestData_In%Emissivity_Horizontal 


    ! -----------------
    ! Assign array data
    ! -----------------

    EmisTestData_Out%ObsTb = EmisTestData_In%ObsTb

  END FUNCTION Assign_EmisTestData

END MODULE EmisTestData_Define


