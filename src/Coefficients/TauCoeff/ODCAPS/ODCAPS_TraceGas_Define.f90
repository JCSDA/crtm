
MODULE ODCAPS_TraceGas_Define


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

  ! -- Public procedures to manipulate the ODCAPS_TraceGas structure
  PUBLIC :: Associated_ODCAPS_TraceGas
  PUBLIC :: Destroy_ODCAPS_TraceGas
  PUBLIC :: Allocate_ODCAPS_TraceGas
  PUBLIC :: Assign_ODCAPS_TraceGas
  PUBLIC :: Zero_ODCAPS_TraceGas
 
  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE Destroy_ODCAPS_TraceGas 
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
  END INTERFACE Destroy_ODCAPS_TraceGas 

  INTERFACE Allocate_ODCAPS_TraceGas 
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank1
  END INTERFACE Allocate_ODCAPS_TraceGas 

  INTERFACE Assign_ODCAPS_TraceGas 
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE Assign_ODCAPS_TraceGas 

  INTERFACE Zero_ODCAPS_TraceGas 
    MODULE PROCEDURE Zero_Scalar
    MODULE PROCEDURE Zero_Rank1
  END INTERFACE Zero_ODCAPS_TraceGas 


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: ODCAPS_TraceGas_Define.f90,v 1.10 2006/05/03 19:42:09 ychen Exp $'

  ! -- ODCAPS_TraceGas valid values
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1
  INTEGER, PRIVATE, PARAMETER ::   VALID =  1

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! ------------------------------
  ! ODCAPS_TraceGas data type definition
  ! ------------------------------
 
  TYPE, PUBLIC :: ODCAPS_TraceGas_type
    INTEGER :: n_Allocates = 0

    ! -- Array dimensions
    INTEGER( Long ) :: n_Layers       = 0    ! Ilayer	 
    INTEGER( Long ) :: n_Predictors   = 0    ! Iuse	 
    INTEGER( Long ) :: n_Channels     = 0    ! L	 

    ! -- The trace gas ID 
    INTEGER( Long ) :: Absorber_ID    = 0    
   
    ! -- The actual sensor channel numbers
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_Index => NULL()    ! L  value from 1-2378

    ! -- The array of coefficients
    REAL( Single ),  POINTER, DIMENSION( :, :, : ) :: Trace_Coeff => NULL() ! Iuse x Ilayer x L
  END TYPE ODCAPS_TraceGas_type

CONTAINS







  SUBROUTINE Clear_ODCAPS_TraceGas( ODCAPS_TraceGas )

    TYPE( ODCAPS_TraceGas_type ), INTENT( IN OUT ) :: ODCAPS_TraceGas

    ODCAPS_TraceGas%n_Layers        = 0   
    ODCAPS_TraceGas%n_Predictors    = 0   
    ODCAPS_TraceGas%n_Channels      = 0   
    
    ODCAPS_TraceGas%Absorber_ID     = 0 
  END SUBROUTINE Clear_ODCAPS_TraceGas







  FUNCTION Associated_ODCAPS_TraceGas( ODCAPS_TraceGas,  & ! Input
                                         ANY_Test )          & ! Optional input
                                     RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( ODCAPS_TraceGas_type ), INTENT( IN ) :: ODCAPS_TraceGas

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

      IF ( ASSOCIATED( ODCAPS_TraceGas%Channel_Index     ) .AND. &
           ASSOCIATED( ODCAPS_TraceGas%Trace_Coeff       )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( ODCAPS_TraceGas%Channel_Index     ) .OR. &
           ASSOCIATED( ODCAPS_TraceGas%Trace_Coeff       )       ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_ODCAPS_TraceGas






  FUNCTION Destroy_Scalar( ODCAPS_TraceGas,  &  ! Output
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
    TYPE( ODCAPS_TraceGas_type ),    INTENT( IN OUT ) :: ODCAPS_TraceGas

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_ODCAPS_TraceGas(Scalar)'


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

    IF ( Clear ) CALL Clear_ODCAPS_TraceGas( ODCAPS_TraceGas )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_ODCAPS_TraceGas( ODCAPS_TraceGas ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the sensor channel number array
    IF ( ASSOCIATED( ODCAPS_TraceGas%Channel_Index ) ) THEN

      DEALLOCATE( ODCAPS_TraceGas%Channel_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS_TraceGas Channel_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate coefficients
    IF ( ASSOCIATED( ODCAPS_TraceGas%Trace_Coeff ) ) THEN

      DEALLOCATE( ODCAPS_TraceGas%Trace_Coeff, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS_TraceGas coefficients ", &
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

    ODCAPS_TraceGas%n_Allocates = ODCAPS_TraceGas%n_Allocates - 1

    IF ( ODCAPS_TraceGas%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      ODCAPS_TraceGas%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Scalar 


  FUNCTION Destroy_Rank1( ODCAPS_TraceGas, &  ! Output
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
    TYPE( ODCAPS_TraceGas_type ), DIMENSION( : ), INTENT( IN OUT ) :: ODCAPS_TraceGas

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_ODCAPS_TraceGas(Rank-1)'


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

    DO n = 1, SIZE( ODCAPS_TraceGas )

      Scalar_Status = Destroy_Scalar( ODCAPS_TraceGas(n), &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i5, &
                          &" of ODCAPS_TraceGas structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Destroy_Rank1




  FUNCTION Allocate_Scalar(n_Layers,		    &  ! Input	       
                  	   n_Predictors,            &  ! Input  	          
                  	   n_Channels,  	    &  ! Input  	          
                  	   ODCAPS_TraceGas,	    &  ! Output 	          
                  	   RCS_Id ,                 &  ! Revision control         
                  	   Message_Log  )           &  ! Error messaging          
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

    ! -- Output
    TYPE( ODCAPS_TraceGas_type ),    INTENT( IN OUT ) :: ODCAPS_TraceGas

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_ODCAPS_TraceGas(Scalar)'


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
         n_Predictors       < 1 .OR. &
         n_Channels         < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ODCAPS_TraceGas dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_ODCAPS_TraceGas( ODCAPS_TraceGas, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_ODCAPS_TraceGas( ODCAPS_TraceGas, &
                                       No_Clear = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating ODCAPS_TraceGas pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( ODCAPS_TraceGas%Channel_Index( n_Channels ), &
              ODCAPS_TraceGas%Trace_Coeff( n_Predictors, n_Layers, n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating ODCAPS_TraceGas data arrays. STAT = ", i5 )' ) &
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

    ODCAPS_TraceGas%n_Layers     = n_Layers
    ODCAPS_TraceGas%n_Predictors = n_Predictors
    ODCAPS_TraceGas%n_Channels   = n_Channels



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    ODCAPS_TraceGas%n_Allocates = ODCAPS_TraceGas%n_Allocates + 1

    IF ( ODCAPS_TraceGas%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      ODCAPS_TraceGas%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Scalar 


  FUNCTION Allocate_Rank1(n_Layers,		    &  ! Input	       
                  	   n_Predictors,            &  ! Input  	          
                   	   n_Channels,  	    &  ! Input  	          
                  	   ODCAPS_TraceGas,	    &  ! Output 	          
                  	   RCS_Id,                  &  ! Revision control         
                  	   Message_Log)             &  ! Error messaging          
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

    ! -- Output
    TYPE( ODCAPS_TraceGas_type ), DIMENSION( : ), INTENT( IN OUT ) :: ODCAPS_TraceGas

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_ODCAPS_TraceGas(Rank-1)'


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

    DO i = 1, SIZE( ODCAPS_TraceGas )

      Scalar_Status = Allocate_Scalar( n_Layers, &
                                       n_Predictors, &
                                       n_Channels, &
				       ODCAPS_TraceGas(i), &
                                       Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of ODCAPS_TraceGas structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank1




  FUNCTION Assign_Scalar( ODCAPS_TraceGas_in,   &  ! Input     
                          ODCAPS_TraceGas_out,  &  ! Output      
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
    TYPE( ODCAPS_TraceGas_type ),    INTENT( IN )     :: ODCAPS_TraceGas_in

    ! -- Output
    TYPE( ODCAPS_TraceGas_type ),    INTENT( IN OUT ) :: ODCAPS_TraceGas_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_ODCAPS_TraceGas(Scalar)'



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

    IF ( .NOT. Associated_ODCAPS_TraceGas( ODCAPS_TraceGas_In ) ) THEN

      Error_Status = Destroy_ODCAPS_TraceGas( ODCAPS_TraceGas_Out, &
                                         Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN 
       CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT ODCAPS_TraceGas pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      END IF
      
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Allocate the structure
    ! ----------------------

    Error_Status = Allocate_ODCAPS_TraceGas( ODCAPS_TraceGas_In%n_Layers, &
                                             ODCAPS_TraceGas_In%n_Predictors, &
                                             ODCAPS_TraceGas_In%n_Channels, &
                                             ODCAPS_TraceGas_Out, &
                                             Message_Log = Message_Log ) 
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output ODCAPS_TraceGas arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------

    ODCAPS_TraceGas_out%Absorber_ID = ODCAPS_TraceGas_in%Absorber_ID


    ! -----------------
    ! Assign array data
    ! -----------------
    ODCAPS_TraceGas_out%Channel_Index  = ODCAPS_TraceGas_in%Channel_Index	 
    ODCAPS_TraceGas_out%Trace_Coeff    = ODCAPS_TraceGas_in%Trace_Coeff	 

  END FUNCTION Assign_Scalar 



  FUNCTION Assign_Rank1(  ODCAPS_TraceGas_in,   &  ! Input     
                          ODCAPS_TraceGas_out,  &  ! Output      
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
    TYPE( ODCAPS_TraceGas_type ),  DIMENSION( : ), INTENT( IN )     :: ODCAPS_TraceGas_in

    ! -- Output
    TYPE( ODCAPS_TraceGas_type ),  DIMENSION( : ), INTENT( IN OUT ) :: ODCAPS_TraceGas_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_ODCAPS_TraceGas(Rank-1)'

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

    n = SIZE( ODCAPS_TraceGas_in )

    IF ( SIZE( ODCAPS_TraceGas_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ODCAPS_TraceGas_in and ODCAPS_TraceGas_out '//&
			    'arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n 

      Scalar_Status = Assign_Scalar(   ODCAPS_TraceGas_in(i), & 
				       ODCAPS_TraceGas_out(i), &
                                       Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i5, &
                          &" of ODCAPS_TraceGas structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO
  END FUNCTION Assign_Rank1


  SUBROUTINE Zero_Scalar( ODCAPS_TraceGas )  ! Output
    TYPE( ODCAPS_TraceGas_type ),  INTENT( IN OUT ) :: ODCAPS_TraceGas

    ! -- Reset the array components
    ODCAPS_TraceGas%Channel_Index      = 0  
    ODCAPS_TraceGas%Trace_Coeff        = 0.0
 
  END SUBROUTINE Zero_Scalar


  SUBROUTINE Zero_Rank1( ODCAPS_TraceGas )  ! Output

    TYPE( ODCAPS_TraceGas_type ), DIMENSION( : ), INTENT( IN OUT ) :: ODCAPS_TraceGas
    INTEGER :: n

    DO n = 1, SIZE( ODCAPS_TraceGas )
      CALL Zero_Scalar( ODCAPS_TraceGas(n) )
    END DO

  END SUBROUTINE Zero_Rank1


END MODULE ODCAPS_TraceGas_Define
