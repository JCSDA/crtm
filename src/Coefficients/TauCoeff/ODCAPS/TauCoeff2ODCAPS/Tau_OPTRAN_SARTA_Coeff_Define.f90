
MODULE Tau_OPTRAN_SARTA_Coeff_Define


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

  ! -- Public procedures to manipulate the Tau_OPTRAN_SARTA_Coeff structure
  PUBLIC :: Associated_Tau_OPTRAN_Coeff
  PUBLIC :: Destroy_Tau_OPTRAN_Coeff
  PUBLIC :: Allocate_Tau_OPTRAN_Coeff
  PUBLIC :: Assign_Tau_OPTRAN_Coeff

  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: Tau_OPTRAN_SARTA_Coeff_Define.f90,v 1.10 2006/05/02 19:42:09 ychen Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! ------------------------------
  ! Tau_OPTRAN_SARTA_Coeff data type definition
  ! ------------------------------

  TYPE, PUBLIC :: Tau_OPTRAN_SARTA_Coeff_type
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

  END TYPE Tau_OPTRAN_SARTA_Coeff_type


CONTAINS







  SUBROUTINE Clear_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff )

    TYPE( Tau_OPTRAN_SARTA_Coeff_type ), INTENT( IN OUT ) :: Tau_OPTRAN_Coeff

    Tau_OPTRAN_Coeff%n_Layers     = 0
    Tau_OPTRAN_Coeff%n_Predictors = 0
    Tau_OPTRAN_Coeff%n_Channels   = 0
    Tau_OPTRAN_Coeff%n_ProfAves    = 0
    
  END SUBROUTINE Clear_Tau_OPTRAN_Coeff




  FUNCTION Associated_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff,  & ! Input
                                        ANY_Test ) & ! Optional input
                                        RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( Tau_OPTRAN_SARTA_Coeff_type ), INTENT( IN ) :: Tau_OPTRAN_Coeff

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

      IF ( ASSOCIATED( Tau_OPTRAN_Coeff%Channel_Index    ) .AND. &
           ASSOCIATED( Tau_OPTRAN_Coeff%Water_Amount     ) .AND. &
           ASSOCIATED( Tau_OPTRAN_Coeff%Water_ProfAve    ) .AND. &
           ASSOCIATED( Tau_OPTRAN_Coeff%Water_Coeff      )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( Tau_OPTRAN_Coeff%Channel_Index    ) .OR. &
           ASSOCIATED( Tau_OPTRAN_Coeff%Water_Amount     ) .OR. &
           ASSOCIATED( Tau_OPTRAN_Coeff%Water_ProfAve    ) .OR. &
           ASSOCIATED( Tau_OPTRAN_Coeff%Water_Coeff      )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_Tau_OPTRAN_Coeff




  FUNCTION Destroy_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff,     &  ! Output
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
    TYPE( Tau_OPTRAN_SARTA_Coeff_type ),    INTENT( IN OUT ) :: Tau_OPTRAN_Coeff

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_Tau_OPTRAN_Coeff'


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

    IF ( Clear ) CALL Clear_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the sensor channel number array
    IF ( ASSOCIATED( Tau_OPTRAN_Coeff%Channel_Index ) ) THEN

      DEALLOCATE( Tau_OPTRAN_Coeff%Channel_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Tau_OPTRAN_SARTA_Coeff Channel_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Water amount
    IF ( ASSOCIATED( Tau_OPTRAN_Coeff%Water_Amount ) ) THEN

      DEALLOCATE( Tau_OPTRAN_Coeff%Water_Amount, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Tau_OPTRAN_SARTA_Coeff Water_Amount ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate water average profile values
    IF ( ASSOCIATED( Tau_OPTRAN_Coeff%Water_ProfAve ) ) THEN

      DEALLOCATE( Tau_OPTRAN_Coeff%Water_ProfAve, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Tau_OPTRAN_SARTA_Coeff Water_ProfAve ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate coefficients
    IF ( ASSOCIATED( Tau_OPTRAN_Coeff%Water_Coeff ) ) THEN

      DEALLOCATE( Tau_OPTRAN_Coeff%Water_Coeff, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Tau_OPTRAN_SARTA_Coeff Water_Coeff ", &
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

    Tau_OPTRAN_Coeff%n_Allocates = Tau_OPTRAN_Coeff%n_Allocates - 1

    IF ( Tau_OPTRAN_Coeff%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      Tau_OPTRAN_Coeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Tau_OPTRAN_Coeff




  FUNCTION Allocate_Tau_OPTRAN_Coeff( n_Layers,     &  ! Input
                                      n_Predictors, &  ! Input
                                      n_Channels,   &  ! Input
                                      n_ProfAves ,   &  ! Input
                                      Tau_OPTRAN_Coeff,     &  ! Output
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
    TYPE( Tau_OPTRAN_SARTA_Coeff_type ),    INTENT( IN OUT ) :: Tau_OPTRAN_Coeff

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_Tau_OPTRAN_Coeff'


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
                            'Input Tau_OPTRAN_SARTA_Coeff dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff, &
                                       No_Clear = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating Tau_OPTRAN_SARTA_Coeff pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( Tau_OPTRAN_Coeff%Channel_Index( n_Channels ), &
              Tau_OPTRAN_Coeff%Water_Amount( n_Layers ), &
              Tau_OPTRAN_Coeff%Water_ProfAve( n_ProfAves, n_Layers ), &
              Tau_OPTRAN_Coeff%Water_Coeff( n_Predictors, n_Layers, n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating Tau_OPTRAN_SARTA_Coeff data arrays. STAT = ", i5 )' ) &
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

    Tau_OPTRAN_Coeff%n_Layers     = n_Layers
    Tau_OPTRAN_Coeff%n_Predictors = n_Predictors
    Tau_OPTRAN_Coeff%n_Channels   = n_Channels
    Tau_OPTRAN_Coeff%n_ProfAves   = n_ProfAves


    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    Tau_OPTRAN_Coeff%n_Allocates = Tau_OPTRAN_Coeff%n_Allocates + 1

    IF ( Tau_OPTRAN_Coeff%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      Tau_OPTRAN_Coeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Tau_OPTRAN_Coeff




  FUNCTION Assign_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff_in,   &  ! Input
                                    Tau_OPTRAN_Coeff_out,  &  ! Output
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
    TYPE( Tau_OPTRAN_SARTA_Coeff_type ),    INTENT( IN )     :: Tau_OPTRAN_Coeff_in

    ! -- Output
    TYPE( Tau_OPTRAN_SARTA_Coeff_type ),    INTENT( IN OUT ) :: Tau_OPTRAN_Coeff_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_Tau_OPTRAN_Coeff'



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

    IF ( .NOT. Associated_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT Tau_OPTRAN_SARTA_Coeff pointer '//&
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
    Error_Status = Allocate_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff_in%n_Layers, &
                                              Tau_OPTRAN_Coeff_in%n_Predictors, &
                                              Tau_OPTRAN_Coeff_in%n_Channels, &
                                              Tau_OPTRAN_Coeff_in%n_ProfAves, &
                                              Tau_OPTRAN_Coeff_out, &
                                              Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output Tau_OPTRAN_SARTA_Coeff arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Copy array data
    Tau_OPTRAN_Coeff_out%Channel_Index    = Tau_OPTRAN_Coeff_in%Channel_Index
    Tau_OPTRAN_Coeff_out%Water_Amount     = Tau_OPTRAN_Coeff_in%Water_Amount 
    Tau_OPTRAN_Coeff_out%Water_ProfAve    = Tau_OPTRAN_Coeff_in%Water_ProfAve
    Tau_OPTRAN_Coeff_out%Water_Coeff      = Tau_OPTRAN_Coeff_in%Water_Coeff

  END FUNCTION Assign_Tau_OPTRAN_Coeff

END MODULE Tau_OPTRAN_SARTA_Coeff_Define 
