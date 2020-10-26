
MODULE TauCoeff_SARTA_Subset_Define


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

  ! -- Public procedures to manipulate the TauCoeff_SARTA_Subset structure
  PUBLIC :: Associated_TauCoeff_Subset
  PUBLIC :: Destroy_TauCoeff_Subset
  PUBLIC :: Allocate_TauCoeff_Subset
  PUBLIC :: Assign_TauCoeff_Subset
  PUBLIC :: Zero_TauCoeff_Subset
  PUBLIC :: Check_TauCoeff_Subset_Release
  PUBLIC :: Version_TauCoeff_Subset

  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE Destroy_TauCoeff_Subset 
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
  END INTERFACE Destroy_TauCoeff_Subset 

  INTERFACE Allocate_TauCoeff_Subset 
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank1
  END INTERFACE Allocate_TauCoeff_Subset 

  INTERFACE Assign_TauCoeff_Subset 
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE Assign_TauCoeff_Subset 

  INTERFACE Zero_TauCoeff_Subset 
    MODULE PROCEDURE Zero_Scalar
    MODULE PROCEDURE Zero_Rank1
  END INTERFACE Zero_TauCoeff_Subset 


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: TauCoeff_SARTA_Subset_Define.f90,v 1.10 2006/05/03 19:42:09 ychen Exp $'

  ! -- TauCoeff_SARTA_Subset valid values
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1
  INTEGER, PRIVATE, PARAMETER ::   VALID =  1

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Current valid release and version numbers
  INTEGER, PRIVATE, PARAMETER :: TauCoeff_Subset_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PRIVATE, PARAMETER :: TauCoeff_Subset_VERSION = 1  ! This is just the data version.


  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! ------------------------------
  ! TauCoeff_SARTA_Subset data type definition
  ! ------------------------------
 
  TYPE, PUBLIC :: TauCoeff_SARTA_Subset_type
    INTEGER :: n_Allocates = 0

    ! -- Release and version information
    INTEGER( Long ) :: Release = TAUCOEFF_SUBSET_RELEASE
    INTEGER( Long ) :: Version = TAUCOEFF_SUBSET_VERSION

    ! -- Array dimensions
    INTEGER( Long ) :: n_Layers           = 0    ! Ilayer
    INTEGER( Long ) :: n_Total_Predictors = 0    ! Iuse
    INTEGER( Long ) :: n_Absorbers        = 0    ! J
    INTEGER( Long ) :: n_Channels         = 0    ! L

    ! -- The actual sensor channel numbers
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_Index => NULL()    ! L  value from 1-2378

    ! -- The absorber ID
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Absorber_ID => NULL()     ! J

    ! -- The predictor numbers for each absorber
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Absorber_Predictor => NULL()  ! J

    ! -- The array of coefficients
    REAL( Single ),  POINTER, DIMENSION( :, :, : ) :: C => NULL() ! Iuse x Ilayer x L
  END TYPE TauCoeff_SARTA_Subset_type

CONTAINS







  SUBROUTINE Clear_TauCoeff_Subset( TauCoeff_Subset )

    TYPE( TauCoeff_SARTA_Subset_type ), INTENT( IN OUT ) :: TauCoeff_Subset

    TauCoeff_Subset%n_Layers           = 0
    TauCoeff_Subset%n_Total_Predictors = 0
    TauCoeff_Subset%n_Absorbers        = 0
    TauCoeff_Subset%n_Channels         = 0

  END SUBROUTINE Clear_TauCoeff_Subset







  FUNCTION Associated_TauCoeff_Subset( TauCoeff_Subset,  & ! Input
                                       ANY_Test )        & ! Optional input
                                     RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_SARTA_Subset_type ), INTENT( IN ) :: TauCoeff_Subset

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

      IF ( ASSOCIATED( TauCoeff_Subset%Channel_Index     ) .AND. &
           ASSOCIATED( TauCoeff_Subset%Absorber_ID       ) .AND. &
           ASSOCIATED( TauCoeff_Subset%Absorber_Predictor) .AND. &
           ASSOCIATED( TauCoeff_Subset%C                 )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( TauCoeff_Subset%Channel_Index     ) .OR. &
           ASSOCIATED( TauCoeff_Subset%Absorber_ID       ) .OR. &
           ASSOCIATED( TauCoeff_Subset%Absorber_Predictor) .OR. &
           ASSOCIATED( TauCoeff_Subset%C                 )       ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_TauCoeff_Subset






  FUNCTION Destroy_Scalar( TauCoeff_Subset,  &  ! Output
                           No_Clear,	     &  ! Optional input  
                           RCS_Id,	     &  ! Revision control
                           Message_Log )     &  ! Error messaging 
                          RESULT( Error_Status )	        



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( TauCoeff_SARTA_Subset_type ),    INTENT( IN OUT ) :: TauCoeff_Subset

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_TauCoeff_SARTA_Subset(Scalar)'


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

    IF ( Clear ) CALL Clear_TauCoeff_Subset( TauCoeff_Subset )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_TauCoeff_Subset( TauCoeff_Subset ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the sensor channel number array
    IF ( ASSOCIATED( TauCoeff_Subset%Channel_Index ) ) THEN

      DEALLOCATE( TauCoeff_Subset%Channel_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA_Subset Channel_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Absorber_ID
    IF ( ASSOCIATED( TauCoeff_Subset%Absorber_ID ) ) THEN

      DEALLOCATE( TauCoeff_Subset%Absorber_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA_Subset Absorber_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Absorber predictor numbers for each absorber
    IF ( ASSOCIATED( TauCoeff_Subset%Absorber_Predictor ) ) THEN

      DEALLOCATE( TauCoeff_Subset%Absorber_Predictor, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA_Subset Absorber_Predictor ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate coefficients
    IF ( ASSOCIATED( TauCoeff_Subset%C ) ) THEN

      DEALLOCATE( TauCoeff_Subset%C, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA_Subset coefficients ", &
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

    TauCoeff_Subset%n_Allocates = TauCoeff_Subset%n_Allocates - 1

    IF ( TauCoeff_Subset%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      TauCoeff_Subset%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Scalar 


  FUNCTION Destroy_Rank1( TauCoeff_Subset,  &  ! Output
                           No_Clear,	     &  ! Optional input  
                           RCS_Id,	     &  ! Revision control
                           Message_Log )     &  ! Error messaging 
                          RESULT( Error_Status )	        


    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( TauCoeff_SARTA_Subset_type ), DIMENSION( : ), INTENT( IN OUT ) :: TauCoeff_Subset

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_TauCoeff_SARTA_Subset(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message
    INTEGER :: Scalar_Status
    INTEGER :: n



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
    !#                       -- PERFORM REINITIALISATION --                     #
    !#--------------------------------------------------------------------------#

    DO n = 1, SIZE( TauCoeff_Subset )

      Scalar_Status = Destroy_Scalar( TauCoeff_Subset(n), &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i5, &
                          &" of TauCoeff_SARTA_Subset structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Destroy_Rank1




  FUNCTION Allocate_Scalar(n_Layers,		    &  ! Input	       
                  	   n_Total_Predictors,      &  ! Input  	          
                  	   n_Absorbers, 	    &  ! Input  	          
                  	   n_Channels,  	    &  ! Input  	          
                  	   TauCoeff_Subset,	    &  ! Output 	          
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
    INTEGER,                  INTENT( IN )     :: n_Layers 
    INTEGER,                  INTENT( IN )     :: n_Total_Predictors
    INTEGER,                  INTENT( IN )     :: n_Absorbers
    INTEGER,                  INTENT( IN )     :: n_Channels

    ! -- Output
    TYPE( TauCoeff_SARTA_Subset_type ),    INTENT( IN OUT ) :: TauCoeff_Subset

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_TauCoeff_SARTA_Subset(Scalar)'


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

    IF ( n_Layers           < 1 .OR. &
         n_Total_Predictors < 1 .OR. &
         n_Absorbers        < 1 .OR. &
         n_Channels         < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input TauCoeff_SARTA_Subset dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_TauCoeff_Subset( TauCoeff_Subset, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_TauCoeff_Subset( TauCoeff_Subset, &
                                       No_Clear = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating TauCoeff_SARTA_Subset pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( TauCoeff_Subset%Channel_Index( n_Channels ), &
              TauCoeff_Subset%Absorber_ID( n_Absorbers ), &
              TauCoeff_Subset%Absorber_Predictor( n_Absorbers ), &
              TauCoeff_Subset%C( n_Total_Predictors, n_Layers, n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating TauCoeff_SARTA_Subset data arrays. STAT = ", i5 )' ) &
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

    TauCoeff_Subset%n_Layers     = n_Layers
    TauCoeff_Subset%n_Total_Predictors = n_Total_Predictors
    TauCoeff_Subset%n_Absorbers  = n_Absorbers
    TauCoeff_Subset%n_Channels   = n_Channels



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    TauCoeff_Subset%n_Allocates = TauCoeff_Subset%n_Allocates + 1

    IF ( TauCoeff_Subset%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      TauCoeff_Subset%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Scalar 


  FUNCTION Allocate_Rank1(n_Layers,		    &  ! Input	       
                  	   n_Total_Predictors,      &  ! Input  	          
                  	   n_Absorbers, 	    &  ! Input  	          
                  	   n_Channels,  	    &  ! Input  	          
                  	   TauCoeff_Subset,	    &  ! Output 	          
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
    INTEGER,                  INTENT( IN )     :: n_Layers 
    INTEGER,                  INTENT( IN )     :: n_Total_Predictors
    INTEGER,                  INTENT( IN )     :: n_Absorbers
    INTEGER,                  INTENT( IN )     :: n_Channels

    ! -- Output
    TYPE( TauCoeff_SARTA_Subset_type ), DIMENSION( : ), INTENT( IN OUT ) :: TauCoeff_Subset

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_TauCoeff_SARTA_Subset(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: Scalar_Status
    INTEGER :: i



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
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, SIZE( TauCoeff_Subset )

      Scalar_Status = Allocate_Scalar( n_Layers, &
                                       n_Total_Predictors, &
                                       n_Absorbers, &
                                       n_Channels, &
				       TauCoeff_Subset(i), &
                                       Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of TauCoeff_SARTA_Subset structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank1




  FUNCTION Assign_Scalar( TauCoeff_Subset_in,   &  ! Input     
                          TauCoeff_Subset_out,  &  ! Output      
                          RCS_Id,	 &  ! Revision control   
                          Message_Log )  &  ! Error messaging    
                        RESULT( Error_Status )		        



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_SARTA_Subset_type ),    INTENT( IN )     :: TauCoeff_Subset_in

    ! -- Output
    TYPE( TauCoeff_SARTA_Subset_type ),    INTENT( IN OUT ) :: TauCoeff_Subset_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_TauCoeff_SARTA_Subset(Scalar)'



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
    !
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    ! ---------------------------------------

    IF ( .NOT. Associated_TauCoeff_Subset( TauCoeff_Subset_In ) ) THEN

      Error_Status = Destroy_TauCoeff_Subset( TauCoeff_Subset_Out, &
                                         Message_Log = Message_Log )
       CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauCoeff_SARTA_Subset pointer '//&
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

    Error_Status = Allocate_TauCoeff_Subset( TauCoeff_Subset_In%n_Layers, &
                                             TauCoeff_Subset_In%n_Total_Predictors, &
                                             TauCoeff_Subset_In%n_Absorbers, &
                                             TauCoeff_Subset_In%n_Channels, &
                                             TauCoeff_Subset_Out, &
                                             Message_Log = Message_Log ) 
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output TauCoeff_SARTA_Subset arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------

    TauCoeff_Subset_out%Release = TauCoeff_Subset_in%Release
    TauCoeff_Subset_out%Version = TauCoeff_Subset_in%Version

 
    ! -----------------
    ! Assign array data
    ! -----------------
    TauCoeff_Subset_out%Channel_Index      = TauCoeff_Subset_in%Channel_Index
    TauCoeff_Subset_out%Absorber_ID        = TauCoeff_Subset_in%Absorber_ID
    TauCoeff_Subset_out%Absorber_Predictor = TauCoeff_Subset_in%Absorber_Predictor
    TauCoeff_Subset_out%C                  = TauCoeff_Subset_in%C

  END FUNCTION Assign_Scalar 



  FUNCTION Assign_Rank1(  TauCoeff_Subset_in,   &  ! Input     
                          TauCoeff_Subset_out,  &  ! Output      
                          RCS_Id,	 &  ! Revision control   
                          Message_Log )  &  ! Error messaging    
                        RESULT( Error_Status )		        



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_SARTA_Subset_type ),  DIMENSION( : ), INTENT( IN )     :: TauCoeff_Subset_in

    ! -- Output
    TYPE( TauCoeff_SARTA_Subset_type ),  DIMENSION( : ), INTENT( IN OUT ) :: TauCoeff_Subset_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_TauCoeff_SARTA_Subset(Rank-1)'

    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: Scalar_Status
    INTEGER :: i, n

    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


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
    !#                               -- TEST THE INPUT --                       #
    !#--------------------------------------------------------------------------#

    n = SIZE( TauCoeff_Subset_in )

    IF ( SIZE( TauCoeff_Subset_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input TauCoeff_SARTA_Subset_in and TauCoeff_SARTA_Subset_out '//&
			    'arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n 

      Scalar_Status = Assign_Scalar(   TauCoeff_Subset_in(i), & 
				       TauCoeff_Subset_out(i), &
                                       Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i5, &
                          &" of TauCoeff_SARTA_Subset structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO
  END FUNCTION Assign_Rank1


  SUBROUTINE Zero_Scalar( TauCoeff_Subset )  ! Output
    TYPE( TauCoeff_SARTA_Subset_type ),  INTENT( IN OUT ) :: TauCoeff_Subset

    ! -- Reset the array components
    TauCoeff_Subset%Channel_Index      = 0  
    TauCoeff_Subset%Absorber_ID        = 0
    TauCoeff_Subset%Absorber_Predictor = 0
    TauCoeff_Subset%C		       = 0.0
 
  END SUBROUTINE Zero_Scalar


  SUBROUTINE Zero_Rank1( TauCoeff_Subset )  ! Output

    TYPE( TauCoeff_SARTA_Subset_type ), DIMENSION( : ), INTENT( IN OUT ) :: TauCoeff_Subset
    INTEGER :: n

    DO n = 1, SIZE( TauCoeff_Subset )
      CALL Zero_Scalar( TauCoeff_Subset(n) )
    END DO

  END SUBROUTINE Zero_Rank1




  FUNCTION Check_TauCoeff_Subset_Release( TauCoeff_Subset,     &  ! Input
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
    TYPE( TauCoeff_SARTA_Subset_type ),    INTENT( IN )  :: TauCoeff_Subset

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Check_TauCoeff_SARTA_Subset_Release'


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

    IF ( TauCoeff_Subset%Release < TAUCOEFF_SUBSET_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A TauCoeff_SARTA_Subset data update is needed. ", &
                        &"TauCoeff_SARTA_Subset release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      TauCoeff_Subset%Release, TAUCOEFF_SUBSET_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#               -- CHECK THAT THE RELEASE IS NOT TOO NEW --                #
    !#--------------------------------------------------------------------------#

    IF ( TauCoeff_Subset%Release > TAUCOEFF_SUBSET_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A TauCoeff_SARTA_Subset software update is needed. ", &
                        &"TauCoeff_Subset release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      TauCoeff_Subset%Release, TAUCOEFF_SUBSET_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Check_TauCoeff_Subset_Release


 

  SUBROUTINE Version_TauCoeff_Subset( TauCoeff_Subset,     &  ! Input
                               Version_Info, &  ! Output
                               RCS_Id        )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_SARTA_Subset_type ),    INTENT( IN )  :: TauCoeff_Subset

    ! -- Output
    CHARACTER( * ),           INTENT( OUT ) :: Version_Info

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id


    ! ----------
    ! Parameters
    ! ----------

    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 512 ) :: Long_String



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

    WRITE( Long_String, '( a,1x,"TauCoeff_SARTA_Subset RELEASE.VERSION: ", i2, ".", i2.2, 2x, &
                           &"N_LAYERS=",i2,2x,&
                           &"N_TOTAL_PREDICTORS=",i2,2x,&
                           &"N_ABSORBERS=",i2,2x,&
                           &"N_CHANNELS=",i4)' ) & 
                         ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                         TauCoeff_Subset%Release, TauCoeff_Subset%Version, &
                         TauCoeff_Subset%n_Layers, &
                         TauCoeff_Subset%n_Total_Predictors, &
                         TauCoeff_Subset%n_Absorbers, &
                         TauCoeff_Subset%n_Channels
 

    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Version_Info = Long_String(1:MIN( LEN( Version_Info ), LEN_TRIM( Long_String ) ))

  END SUBROUTINE Version_TauCoeff_Subset

END MODULE TauCoeff_SARTA_Subset_Define



