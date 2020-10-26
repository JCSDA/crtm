
MODULE ODCAPS_ODAS_Define


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

  ! -- Public procedures to manipulate the ODCAPS_ODAS structure
  PUBLIC :: Associated_ODCAPS_ODAS
  PUBLIC :: Destroy_ODCAPS_ODAS
  PUBLIC :: Allocate_ODCAPS_ODAS
  PUBLIC :: Assign_ODCAPS_ODAS

  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: ODCAPS_ODAS_Define.f90,v 1.10 2006/05/02 19:42:09 ychen Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! ------------------------------
  ! ODCAPS_ODAS data type definition
  ! ------------------------------

  TYPE, PUBLIC :: ODCAPS_ODAS_type
    INTEGER :: n_Allocates = 0

    ! -- Array dimensions
    INTEGER( Long ) :: n_Layers      = 0    ! Ilayer   
    INTEGER( Long ) :: n_Predictors  = 0    ! Iuse     
    INTEGER( Long ) :: n_Channels    = 0    ! L        
    INTEGER( Long ) :: n_ProfAves    = 0    ! J        

    ! -- The actual sensor channel numbers
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_Index => NULL()    ! L

    ! -- The OPTRAN water amount ( Kilomoles/cm^2)
    REAL( Single ), POINTER, DIMENSION( : ) ::  Water_Amount  => NULL()  ! Ilayer

    ! -- The OPTRAN water average profile values
    REAL( Single ), POINTER, DIMENSION( :, : ) :: Water_ProfAve  => NULL()  ! J x Ilayer
    
    ! -- The array of coefficients
    REAL( Single ), POINTER, DIMENSION( :, :, : ) :: Water_Coeff => NULL() ! Iuse x Ilayer x L

  END TYPE ODCAPS_ODAS_type


CONTAINS







  SUBROUTINE Clear_ODCAPS_ODAS( ODCAPS_ODAS )

    TYPE( ODCAPS_ODAS_type ), INTENT( IN OUT ) :: ODCAPS_ODAS

    ODCAPS_ODAS%n_Layers     = 0
    ODCAPS_ODAS%n_Predictors = 0
    ODCAPS_ODAS%n_Channels   = 0
    ODCAPS_ODAS%n_ProfAves    = 0
    
  END SUBROUTINE Clear_ODCAPS_ODAS




  FUNCTION Associated_ODCAPS_ODAS( ODCAPS_ODAS,  & ! Input
                                        ANY_Test ) & ! Optional input
                                        RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( ODCAPS_ODAS_type ), INTENT( IN ) :: ODCAPS_ODAS

    ! -- Optional input
    INTEGER,     OPTIONAL, INTENT( IN ) :: ANY_Test


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

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( ODCAPS_ODAS%Channel_Index    ) .AND. &
           ASSOCIATED( ODCAPS_ODAS%Water_Amount     ) .AND. &
           ASSOCIATED( ODCAPS_ODAS%Water_ProfAve    ) .AND. &
           ASSOCIATED( ODCAPS_ODAS%Water_Coeff      )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( ODCAPS_ODAS%Channel_Index    ) .OR. &
           ASSOCIATED( ODCAPS_ODAS%Water_Amount     ) .OR. &
           ASSOCIATED( ODCAPS_ODAS%Water_ProfAve    ) .OR. &
           ASSOCIATED( ODCAPS_ODAS%Water_Coeff      )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_ODCAPS_ODAS




  FUNCTION Destroy_ODCAPS_ODAS( ODCAPS_ODAS,     &  ! Output
                                     No_Clear,     &  ! Optional input
                                     RCS_Id,	   &  ! Revision control
                                     Message_Log ) &  ! Error messaging

                                    RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( ODCAPS_ODAS_type ),    INTENT( IN OUT ) :: ODCAPS_ODAS

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_ODCAPS_ODAS'


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

    IF ( Clear ) CALL Clear_ODCAPS_ODAS( ODCAPS_ODAS )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_ODCAPS_ODAS( ODCAPS_ODAS ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the sensor channel number array
    IF ( ASSOCIATED( ODCAPS_ODAS%Channel_Index ) ) THEN

      DEALLOCATE( ODCAPS_ODAS%Channel_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS_ODAS Channel_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Water amount
    IF ( ASSOCIATED( ODCAPS_ODAS%Water_Amount ) ) THEN

      DEALLOCATE( ODCAPS_ODAS%Water_Amount, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS_ODAS Water_Amount ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate water average profile values
    IF ( ASSOCIATED( ODCAPS_ODAS%Water_ProfAve ) ) THEN

      DEALLOCATE( ODCAPS_ODAS%Water_ProfAve, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS_ODAS Water_ProfAve ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate coefficients
    IF ( ASSOCIATED( ODCAPS_ODAS%Water_Coeff ) ) THEN

      DEALLOCATE( ODCAPS_ODAS%Water_Coeff, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS_ODAS Water_Coeff ", &
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

    ODCAPS_ODAS%n_Allocates = ODCAPS_ODAS%n_Allocates - 1

    IF ( ODCAPS_ODAS%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      ODCAPS_ODAS%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_ODCAPS_ODAS




  FUNCTION Allocate_ODCAPS_ODAS( n_Layers,     &  ! Input
                                      n_Predictors, &  ! Input
                                      n_Channels,   &  ! Input
                                      n_ProfAves ,   &  ! Input
                                      ODCAPS_ODAS,     &  ! Output
                                      RCS_Id,	    &  ! Revision control
                                      Message_Log ) &  ! Error messaging
                                    RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )     :: n_Layers
    INTEGER,                  INTENT( IN )     :: n_Predictors
    INTEGER,                  INTENT( IN )     :: n_Channels
    INTEGER,                  INTENT( IN )     :: n_ProfAves

    ! -- Output
    TYPE( ODCAPS_ODAS_type ),    INTENT( IN OUT ) :: ODCAPS_ODAS

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_ODCAPS_ODAS'


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

    IF ( n_Layers     < 1 .OR. &
         n_Predictors < 1 .OR. &
         n_Channels   < 1 .OR. &
	 n_ProfAves   < 1  ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ODCAPS_ODAS dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_ODCAPS_ODAS( ODCAPS_ODAS, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_ODCAPS_ODAS( ODCAPS_ODAS, &
                                       No_Clear = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating ODCAPS_ODAS pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( ODCAPS_ODAS%Channel_Index( n_Channels ), &
              ODCAPS_ODAS%Water_Amount( n_Layers ), &
              ODCAPS_ODAS%Water_ProfAve( n_ProfAves, n_Layers ), &
              ODCAPS_ODAS%Water_Coeff( n_Predictors, n_Layers, n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating ODCAPS_ODAS data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                        -- ASSIGN THE DIMENSIONS --                       #
    !#--------------------------------------------------------------------------#

    ODCAPS_ODAS%n_Layers     = n_Layers
    ODCAPS_ODAS%n_Predictors = n_Predictors
    ODCAPS_ODAS%n_Channels   = n_Channels
    ODCAPS_ODAS%n_ProfAves   = n_ProfAves


    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    ODCAPS_ODAS%n_Allocates = ODCAPS_ODAS%n_Allocates + 1

    IF ( ODCAPS_ODAS%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      ODCAPS_ODAS%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_ODCAPS_ODAS




  FUNCTION Assign_ODCAPS_ODAS( ODCAPS_ODAS_in,   &  ! Input
                                    ODCAPS_ODAS_out,  &  ! Output
                                    RCS_Id,	   &  ! Revision control
                                    Message_Log )  &  ! Error messaging
                                  RESULT( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( ODCAPS_ODAS_type ),    INTENT( IN )     :: ODCAPS_ODAS_in

    ! -- Output
    TYPE( ODCAPS_ODAS_type ),    INTENT( IN OUT ) :: ODCAPS_ODAS_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_ODCAPS_ODAS'



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

    IF ( .NOT. Associated_ODCAPS_ODAS( ODCAPS_ODAS_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT ODCAPS_ODAS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! Assign array data
    ! -----------------

    ! -- Allocate data arrays
    Error_Status = Allocate_ODCAPS_ODAS( ODCAPS_ODAS_in%n_Layers, &
                                              ODCAPS_ODAS_in%n_Predictors, &
                                              ODCAPS_ODAS_in%n_Channels, &
                                              ODCAPS_ODAS_in%n_ProfAves, &
                                              ODCAPS_ODAS_out, &
                                              Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output ODCAPS_ODAS arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Copy array data
    ODCAPS_ODAS_out%Channel_Index    = ODCAPS_ODAS_in%Channel_Index
    ODCAPS_ODAS_out%Water_Amount     = ODCAPS_ODAS_in%Water_Amount 
    ODCAPS_ODAS_out%Water_ProfAve    = ODCAPS_ODAS_in%Water_ProfAve
    ODCAPS_ODAS_out%Water_Coeff      = ODCAPS_ODAS_in%Water_Coeff

  END FUNCTION Assign_ODCAPS_ODAS

END MODULE ODCAPS_ODAS_Define 
