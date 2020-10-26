
MODULE CRTM_ChannelInfo


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler

  USE CRTM_ChannelInfo_Define
  USE SpcCoeff_Define
  USE CRTM_SpcCoeff


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- CRTM_ChannelInfo structure data type
  ! -- in the CRTM_ChannelInfo_Define module
  PUBLIC :: CRTM_ChannelInfo_type

  ! -- CRTM_ChannelInfo structure routines inherited
  ! -- from the CRTM_ChannelInfo_Define module
  ! -- Definition functions
  PUBLIC :: CRTM_Associated_ChannelInfo
  PUBLIC :: CRTM_Destroy_ChannelInfo
  PUBLIC :: CRTM_Allocate_ChannelInfo
  PUBLIC :: CRTM_Assign_ChannelInfo

  ! -- This modules public routines
  PUBLIC :: CRTM_Index_ChannelInfo
  PUBLIC :: CRTM_Set_ChannelInfo
  PUBLIC :: CRTM_Fill_ChannelInfo

  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE CRTM_Index_ChannelInfo
    MODULE PROCEDURE IndexChannelInfo_DESC
    MODULE PROCEDURE IndexChannelInfo_NCEPID
  END INTERFACE CRTM_Index_ChannelInfo

  INTERFACE CRTM_Set_ChannelInfo
    MODULE PROCEDURE Set_ChannelInfo_F1
    MODULE PROCEDURE Set_ChannelInfo_F2
  END INTERFACE CRTM_Set_ChannelInfo

  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_ChannelInfo.f90,v 1.11 2006/05/02 14:58:34 dgroff Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


CONTAINS











  FUNCTION IndexChannelInfo_DESC( Master_Sensor_Descriptor, &  ! Input
                                  Master_Sensor_Channel,    &  ! Input
                                  User_Sensor_Descriptor,   &  ! Input
                                  User_Sensor_Channel,      &  ! Input
                                  ChannelInfo,              &  ! Output
                                  RCS_Id,                   &  ! Revision control
                                  Message_Log )             &  ! Error messaging
                                RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ), DIMENSION( : ), INTENT( IN )     :: Master_Sensor_Descriptor
    INTEGER,        DIMENSION( : ), INTENT( IN )     :: Master_Sensor_Channel
    CHARACTER( * ), DIMENSION( : ), INTENT( IN )     :: User_Sensor_Descriptor
    INTEGER,        DIMENSION( : ), INTENT( IN )     :: User_Sensor_Channel

    ! -- Output
    TYPE( CRTM_ChannelInfo_type ),  INTENT( IN OUT ) :: ChannelInfo

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_IndexChannelInfo(DESC)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: l, n
    INTEGER :: n_Master_Channels
    INTEGER :: n_User_Channels
    INTEGER :: n_Matched_Channels
    INTEGER, DIMENSION( SIZE( Master_Sensor_Channel ) ) :: Channel_Index
    INTEGER, DIMENSION( 1 ) :: Idx



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                              -- CHECK INPUT --                           #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Check the dimensions
    ! --------------------

    ! -- Check that Master inputs are the same size
    n_Master_Channels = SIZE( Master_Sensor_Descriptor )
    IF ( SIZE( Master_Sensor_Channel ) /= n_Master_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Master_Sensor_Descriptor and Master_Sensor_Channel '//&
                            'arrays have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Check that User inputs are the same size
    n_User_Channels = SIZE( User_Sensor_Descriptor )
    IF ( SIZE( User_Sensor_Channel ) /= n_User_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input User_Sensor_Descriptor and User_Sensor_Channel '//&
                            'arrays have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Check that User inputs aren't zero-sized
    IF ( n_User_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input User arrays must have sizes > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE MATCHING INDICES --                      #
    !#--------------------------------------------------------------------------#

    n_Matched_Channels = 0

    DO l = 1, n_User_Channels

      ! -- Count the matchups
      n = COUNT( Master_Sensor_Descriptor == User_Sensor_Descriptor(l) .AND. &
                 Master_Sensor_Channel    == User_Sensor_Channel(l)          )
 
      ! -- If none, go to next channel
      IF ( n == 0 ) CYCLE
      ! -- If more than one, Master arrays are screwy
      IF ( n > 1 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Multiple entries in Master arrays for the same '//&
                              'User specified channel.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Increment channel match counter
      n_Matched_Channels = n_Matched_Channels + 1
 
      ! -- Get the matching index
      Idx = PACK( (/ ( n, n = 1, n_Master_Channels ) /), &
                  Master_Sensor_Descriptor == User_Sensor_Descriptor(l) .AND. &
                  Master_Sensor_Channel    == User_Sensor_Channel(l)          )

      ! -- Save it
      Channel_Index( n_Matched_Channels ) = Idx(1)

    END DO

    ! -- Were ANY matches found?
    IF ( n_Matched_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'No Master/User array data matches were found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#       -- POPULATE THE ChannelInfo Channel_Index STRUCTURE MEMBER --      #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------
    ! Allocate the ChannelInfo structure
    ! ----------------------------------

    Error_Status = CRTM_Allocate_ChannelInfo( n_Matched_Channels, &
                                              ChannelInfo, &
                                              Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Allocation of ChannelInfo structure failed.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------
    ! Save the channel indices
    ! ------------------------

    ChannelInfo%Channel_Index = Channel_Index( 1:n_Matched_Channels )


  END FUNCTION IndexChannelInfo_DESC

  FUNCTION IndexChannelInfo_NCEPID( Master_NCEP_Sensor_ID, &  ! Input
                                    Master_Sensor_Channel, &  ! Input
                                    User_NCEP_Sensor_ID,   &  ! Input
                                    User_Sensor_Channel,   &  ! Input
                                    ChannelInfo,           &  ! Output
                                    RCS_Id,                &  ! Revision control
                                    Message_Log )          &  ! Error messaging
                                  RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,        DIMENSION( : ), INTENT( IN )     :: Master_NCEP_Sensor_ID
    INTEGER,        DIMENSION( : ), INTENT( IN )     :: Master_Sensor_Channel
    INTEGER,        DIMENSION( : ), INTENT( IN )     :: User_NCEP_Sensor_ID
    INTEGER,        DIMENSION( : ), INTENT( IN )     :: User_Sensor_Channel

    ! -- Output
    TYPE( CRTM_ChannelInfo_type ),  INTENT( IN OUT ) :: ChannelInfo

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_IndexChannelInfo(NCEPID)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: l, n
    INTEGER :: n_Master_Channels
    INTEGER :: n_User_Channels
    INTEGER :: n_Matched_Channels
    INTEGER, DIMENSION( SIZE( Master_Sensor_Channel ) ) :: Channel_Index
    INTEGER, DIMENSION( 1 ) :: Idx



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                              -- CHECK INPUT --                           #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Check the dimensions
    ! --------------------

    ! -- Check that Master inputs are the same size
    n_Master_Channels = SIZE( Master_NCEP_Sensor_ID )
    IF ( SIZE( Master_Sensor_Channel ) /= n_Master_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Master_NCEP_Sensor_ID and Master_Sensor_Channel '//&
                            'arrays have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Check that User inputs are the same size
    n_User_Channels = SIZE( User_NCEP_Sensor_ID )
    IF ( SIZE( User_Sensor_Channel ) /= n_User_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input User_NCEP_Sensor_ID and User_Sensor_Channel '//&
                            'arrays have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Check that User inputs aren't zero-sized
    IF ( n_User_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input User arrays must have sizes > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE MATCHING INDICES --                      #
    !#--------------------------------------------------------------------------#

    n_Matched_Channels = 0

    DO l = 1, n_User_Channels

      ! -- Count the matchups
      n = COUNT( Master_NCEP_Sensor_ID == User_NCEP_Sensor_ID(l) .AND. &
                 Master_Sensor_Channel == User_Sensor_Channel(l)       )
 
      ! -- If none, go to next channel
      IF ( n == 0 ) CYCLE

      ! -- If more than one, Master arrays are screwy
      IF ( n > 1 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Multiple entries in Master arrays for the same '//&
                              'User specified channel.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Increment channel match counter
      n_Matched_Channels = n_Matched_Channels + 1
 
      ! -- Get the matching index
      Idx = PACK( (/ ( n, n = 1, n_Master_Channels ) /), &
                  Master_NCEP_Sensor_ID == User_NCEP_Sensor_ID(l) .AND. &
                  Master_Sensor_Channel == User_Sensor_Channel(l)       )

      ! -- Save it
      Channel_Index( n_Matched_Channels ) = Idx(1)

    END DO

    ! -- Were ANY matches found?
    IF ( n_Matched_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'No Master/User array data matches were found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#       -- POPULATE THE ChannelInfo Channel_Index STRUCTURE MEMBER --      #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------
    ! Allocate the ChannelInfo structure
    ! ----------------------------------

    Error_Status = CRTM_Allocate_ChannelInfo( n_Matched_Channels, &
                                              ChannelInfo, &
                                              Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Allocation of ChannelInfo structure failed.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------
    ! Save the channel indices
    ! ------------------------

    ChannelInfo%Channel_Index = Channel_Index( 1:n_Matched_Channels )


  END FUNCTION IndexChannelInfo_NCEPID


  FUNCTION Set_ChannelInfo_F1( Sensor_Descriptor,       &  ! Input
                               ChannelInfo,             &  ! Output              
                               RCS_Id,                  &  ! Revision control    
                               Message_Log )            &  ! Error messaging     
                             RESULT( Error_Status )                              

    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ), INTENT( IN )     :: Sensor_Descriptor

    ! -- Output
    TYPE( CRTM_ChannelInfo_type ),  INTENT( IN OUT ) :: ChannelInfo

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Set_ChannelInfo (F1)'


    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER, ALLOCATABLE :: Channel_Index(:)
    CHARACTER( 256 )     :: Message
    INTEGER              :: i, n_Matched_Channels, Allocate_Status

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

    ! -- Check that the shared data structure SpcCoeff is loaded. 
    IF( .NOT. Associated_SpcCoeff( SC ) )THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'CRTM has not be correctly initialized. ', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
      
    ENDIF

    ! --- Get the number of required channels 
    n_Matched_Channels = COUNT( SC%Sensor_Descriptor == Sensor_Descriptor )
    IF ( n_Matched_Channels == 0 ) THEN                                                             
      Error_Status = FAILURE                                                      
      CALL Display_Message( ROUTINE_NAME, &                                       
                            'The Sensor '//TRIM(Sensor_Descriptor)//&
                            ' is not included in the SpcCoeff data file.', &  
                            Error_Status, &                                       
                            Message_Log = Message_Log )                           
      RETURN                                                                      
    END IF

    ! -- Allocate Channel_Index
    ALLOCATE( Channel_Index( n_Matched_Channels ), STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating Channel_Index ", i5 )' ) Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &           
                            TRIM( Message ), &           
                            Error_Status,    &           
                            Message_Log = Message_Log )  
    END IF

    Channel_Index = PACK( (/ ( i, i = 1, SC%n_Channels ) /), &
                    MASK = SC%Sensor_Descriptor == Sensor_Descriptor ) 

    ! --- POPULATE THE ChannelInfo Channel_Index STRUCTURE MEMBER
    Error_Status = CRTM_Index_ChannelInfo( SC%Sensor_Descriptor,                &
                                           SC%Sensor_Channel,                   & 
                                           SC%Sensor_Descriptor(Channel_Index), & 
                                           SC%Sensor_Channel(Channel_Index),    &
                                           ChannelInfo,                         &
                                           Message_Log = Message_Log )

    IF ( Error_Status  /= SUCCESS ) THEN                     
      CALL Display_Message( ROUTINE_NAME, &                  
                            'Error indexing ChannelInfo', &  
                            Error_Status, &                  
                            Message_Log = Message_Log )      
      RETURN                                                 
    END IF 

    ! -- Deallocate Channel_Index
    DEALLOCATE( Channel_Index, STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating Channel_Index ", i5 )' ) Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &           
                            TRIM( Message ), &           
                            Error_Status,    &           
                            Message_Log = Message_Log )  
    END IF


    ! ------------------------------------------
    ! Fill the rest of the ChannelInfo structure
    ! ------------------------------------------

    CALL CRTM_Fill_ChannelInfo( ChannelInfo )

  END FUNCTION Set_ChannelInfo_F1

  FUNCTION Set_ChannelInfo_F2( Sensor_Descriptor,       &  ! Input
                               Sensor_Channel,          &   ! Input              
                               ChannelInfo,             &  ! Output              
                               RCS_Id,                  &  ! Revision control    
                               Message_Log )            &  ! Error messaging     
                             RESULT( Error_Status )                              

    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ), DIMENSION( : ), INTENT( IN )     :: Sensor_Descriptor
    INTEGER,        DIMENSION( : ), INTENT( IN )     :: Sensor_Channel

    ! -- Output
    TYPE( CRTM_ChannelInfo_type ),  INTENT( IN OUT ) :: ChannelInfo

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Set_ChannelInfo (F2)'


    ! ---------------
    ! Local variables
    ! ---------------


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

    ! -- Check that the shared data structure SpcCoeff is loaded. 
    IF( .NOT. Associated_SpcCoeff( SC ) )THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'CRTM has not be correctly initialized. ', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
      
    ENDIF

    ! --- Fill ChannelInfo structure
    Error_Status = CRTM_Index_ChannelInfo( SC%Sensor_Descriptor, &     
                                           SC%Sensor_Channel,    &      
                                           Sensor_Descriptor,    &
                                           Sensor_Channel,       &
                                           ChannelInfo,          &
                                           Message_Log = Message_Log )

    IF ( Error_Status  /= SUCCESS ) THEN                     
      CALL Display_Message( ROUTINE_NAME, &                  
                            'Error indexing ChannelInfo', &  
                            Error_Status, &                  
                            Message_Log = Message_Log )      
      RETURN                                                 
    END IF 

    ! ------------------------------------------
    ! Fill the rest of the ChannelInfo structure
    ! ------------------------------------------

    CALL CRTM_Fill_ChannelInfo( ChannelInfo )

  END FUNCTION Set_ChannelInfo_F2



  SUBROUTINE CRTM_Fill_ChannelInfo( ChannelInfo )

    TYPE( CRTM_ChannelInfo_type ),  INTENT( IN OUT ) :: ChannelInfo

    ChannelInfo%Sensor_Descriptor = SC%Sensor_Descriptor( ChannelInfo%Channel_Index )
    ChannelInfo%NCEP_Sensor_ID    = SC%NCEP_Sensor_ID( ChannelInfo%Channel_Index )
    ChannelInfo%WMO_Satellite_ID  = SC%WMO_Satellite_ID( ChannelInfo%Channel_Index )
    ChannelInfo%WMO_Sensor_ID     = SC%WMO_Sensor_ID( ChannelInfo%Channel_Index )
    ChannelInfo%Sensor_Channel    = SC%Sensor_Channel( ChannelInfo%Channel_Index )

  END SUBROUTINE CRTM_Fill_ChannelInfo

END MODULE CRTM_ChannelInfo


