!------------------------------------------------------------------------------
!M+
! NAME:
!       TauCoeff_SARTA_TraceGas_Define
!
! PURPOSE:
!       Module defining the TauCoeff_SARTA trace gas data structure and containing routines to 
!       manipulate it.
!       
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE TauCoeff_SARTA_TraceGas_Define
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Message_Handler:          Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
! CONTAINS:
!       Associated_TauCoeff_TraceGas:    Function to test the association status
!                                      of the pointer members of a TauCoeff_SARTA_TraceGas
!                                      structure.
!
!       Destroy_TauCoeff_TraceGas:       Function to re-initialize a TauCoeff_SARTA_TraceGas
!                                      structure.
!
!       Allocate_TauCoeff_TraceGas:      Function to allocate the pointer members
!                                      of a TauCoeff_SARTA_TraceGas structure.
!
!       Assign_TauCoeff_TraceGas:        Function to copy a valid TauCoeff_SARTA_TraceGas structure.
!
!
!
! DERIVED TYPES:
!       TauCoeff_SARTA_TraceGas_type:   Definition of the public TauCoeff_SARTA_TraceGas data structure. Fields
!                        are...
!
!         n_Layers:            Maximum layers for the aborber coefficients.
!                              "Ilayers" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Predictors:        Number of predictors used in the
!                              gas absorption regression.
!                              "Iuse" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Channels:          Total number of spectral channels.
!                              "L" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         Absorber_ID:         A flag value used to identify the individual
!                              or collective molecular species for which
!                              the gas absorption coefficients were
!                              generated.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar 
!
!         Channel_Index:       This is the sensor channel number associated
!                              with the data in the coefficient file. Helps
!                              in identifying channels where the numbers are
!                              not contiguous (e.g. AIRS).
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         Trace_Coeff:         Array containing the gas absorption
!                              model coefficients.
!                              UNITS:      Variable
!                              TYPE:       REAL( Double )
!                              DIMENSION:  Iuse x Ilayer x L
!                              ATTRIBUTES: POINTER
!
!       *!IMPORTANT!*
!       -------------
!       Note that the TauCoeff_SARTA_TraceGas_type is PUBLIC and its members are not
!       encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user initialize,
!       destroy, allocate, assign, and concatenate the structure
!       using only the routines in this module where possible to
!       eliminate -- or at least minimise -- the possibility of 
!       memory leakage since most of the structure members are
!       pointers.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 03-May-2006
!                       Yong.Chen@noaa.gov
!
!  Copyright (C) 2006 Yong Chen
!
!M-
!------------------------------------------------------------------------------

MODULE TauCoeff_SARTA_TraceGas_Define


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

  ! -- Public procedures to manipulate the TauCoeff_SARTA_TraceGas structure
  PUBLIC :: Associated_TauCoeff_TraceGas
  PUBLIC :: Destroy_TauCoeff_TraceGas
  PUBLIC :: Allocate_TauCoeff_TraceGas
  PUBLIC :: Assign_TauCoeff_TraceGas
  PUBLIC :: Zero_TauCoeff_TraceGas
 
  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE Destroy_TauCoeff_TraceGas 
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
  END INTERFACE Destroy_TauCoeff_TraceGas 

  INTERFACE Allocate_TauCoeff_TraceGas 
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank1
  END INTERFACE Allocate_TauCoeff_TraceGas 

  INTERFACE Assign_TauCoeff_TraceGas 
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE Assign_TauCoeff_TraceGas 

  INTERFACE Zero_TauCoeff_TraceGas 
    MODULE PROCEDURE Zero_Scalar
    MODULE PROCEDURE Zero_Rank1
  END INTERFACE Zero_TauCoeff_TraceGas 


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- TauCoeff_SARTA_TraceGas valid values
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1
  INTEGER, PRIVATE, PARAMETER ::   VALID =  1

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! ------------------------------
  ! TauCoeff_SARTA_TraceGas data type definition
  ! ------------------------------
 
  TYPE, PUBLIC :: TauCoeff_SARTA_TraceGas_type
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
  END TYPE TauCoeff_SARTA_TraceGas_type

CONTAINS







  SUBROUTINE Clear_TauCoeff_TraceGas( TauCoeff_TraceGas )

    TYPE( TauCoeff_SARTA_TraceGas_type ), INTENT( IN OUT ) :: TauCoeff_TraceGas

    TauCoeff_TraceGas%n_Layers        = 0   
    TauCoeff_TraceGas%n_Predictors    = 0   
    TauCoeff_TraceGas%n_Channels      = 0   
    
    TauCoeff_TraceGas%Absorber_ID     = 0 
  END SUBROUTINE Clear_TauCoeff_TraceGas







  FUNCTION Associated_TauCoeff_TraceGas( TauCoeff_TraceGas,  & ! Input
                                         ANY_Test )          & ! Optional input
                                     RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_SARTA_TraceGas_type ), INTENT( IN ) :: TauCoeff_TraceGas

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

      IF ( ASSOCIATED( TauCoeff_TraceGas%Channel_Index     ) .AND. &
           ASSOCIATED( TauCoeff_TraceGas%Trace_Coeff       )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( TauCoeff_TraceGas%Channel_Index     ) .OR. &
           ASSOCIATED( TauCoeff_TraceGas%Trace_Coeff       )       ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_TauCoeff_TraceGas






  FUNCTION Destroy_Scalar( TauCoeff_TraceGas,  &  ! Output
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
    TYPE( TauCoeff_SARTA_TraceGas_type ),    INTENT( IN OUT ) :: TauCoeff_TraceGas

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_TauCoeff_TraceGas(Scalar)'


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

    IF ( Clear ) CALL Clear_TauCoeff_TraceGas( TauCoeff_TraceGas )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_TauCoeff_TraceGas( TauCoeff_TraceGas ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the sensor channel number array
    IF ( ASSOCIATED( TauCoeff_TraceGas%Channel_Index ) ) THEN

      DEALLOCATE( TauCoeff_TraceGas%Channel_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA_TraceGas Channel_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate coefficients
    IF ( ASSOCIATED( TauCoeff_TraceGas%Trace_Coeff ) ) THEN

      DEALLOCATE( TauCoeff_TraceGas%Trace_Coeff, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA_TraceGas coefficients ", &
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

    TauCoeff_TraceGas%n_Allocates = TauCoeff_TraceGas%n_Allocates - 1

    IF ( TauCoeff_TraceGas%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      TauCoeff_TraceGas%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Scalar 


  FUNCTION Destroy_Rank1( TauCoeff_TraceGas, &  ! Output
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
    TYPE( TauCoeff_SARTA_TraceGas_type ), DIMENSION( : ), INTENT( IN OUT ) :: TauCoeff_TraceGas

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_TauCoeff_TraceGas(Rank-1)'


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

    DO n = 1, SIZE( TauCoeff_TraceGas )

      Scalar_Status = Destroy_Scalar( TauCoeff_TraceGas(n), &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i5, &
                          &" of TauCoeff_SARTA_TraceGas structure array." )' ) n
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
                  	   TauCoeff_TraceGas,	    &  ! Output 	          
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
    TYPE( TauCoeff_SARTA_TraceGas_type ),    INTENT( IN OUT ) :: TauCoeff_TraceGas

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_TauCoeff_TraceGas(Scalar)'


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
                            'Input TauCoeff_SARTA_TraceGas dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_TauCoeff_TraceGas( TauCoeff_TraceGas, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_TauCoeff_TraceGas( TauCoeff_TraceGas, &
                                       No_Clear = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating TauCoeff_SARTA_TraceGas pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( TauCoeff_TraceGas%Channel_Index( n_Channels ), &
              TauCoeff_TraceGas%Trace_Coeff( n_Predictors, n_Layers, n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating TauCoeff_SARTA_TraceGas data arrays. STAT = ", i5 )' ) &
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

    TauCoeff_TraceGas%n_Layers     = n_Layers
    TauCoeff_TraceGas%n_Predictors = n_Predictors
    TauCoeff_TraceGas%n_Channels   = n_Channels



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    TauCoeff_TraceGas%n_Allocates = TauCoeff_TraceGas%n_Allocates + 1

    IF ( TauCoeff_TraceGas%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      TauCoeff_TraceGas%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Scalar 


  FUNCTION Allocate_Rank1(n_Layers,		    &  ! Input	       
                  	   n_Predictors,            &  ! Input  	          
                   	   n_Channels,  	    &  ! Input  	          
                  	   TauCoeff_TraceGas,	    &  ! Output 	          
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
    TYPE( TauCoeff_SARTA_TraceGas_type ), DIMENSION( : ), INTENT( IN OUT ) :: TauCoeff_TraceGas

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_TauCoeff_TraceGas(Rank-1)'


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

    DO i = 1, SIZE( TauCoeff_TraceGas )

      Scalar_Status = Allocate_Scalar( n_Layers, &
                                       n_Predictors, &
                                       n_Channels, &
				       TauCoeff_TraceGas(i), &
                                       Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of TauCoeff_SARTA_TraceGas structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank1




  FUNCTION Assign_Scalar( TauCoeff_TraceGas_in,   &  ! Input     
                          TauCoeff_TraceGas_out,  &  ! Output      
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
    TYPE( TauCoeff_SARTA_TraceGas_type ),    INTENT( IN )     :: TauCoeff_TraceGas_in

    ! -- Output
    TYPE( TauCoeff_SARTA_TraceGas_type ),    INTENT( IN OUT ) :: TauCoeff_TraceGas_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_TauCoeff_TraceGas(Scalar)'



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

    IF ( .NOT. Associated_TauCoeff_TraceGas( TauCoeff_TraceGas_In ) ) THEN

      Error_Status = Destroy_TauCoeff_TraceGas( TauCoeff_TraceGas_Out, &
                                         Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN 
       CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauCoeff_SARTA_TraceGas pointer '//&
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

    Error_Status = Allocate_TauCoeff_TraceGas( TauCoeff_TraceGas_In%n_Layers, &
                                             TauCoeff_TraceGas_In%n_Predictors, &
                                             TauCoeff_TraceGas_In%n_Channels, &
                                             TauCoeff_TraceGas_Out, &
                                             Message_Log = Message_Log ) 
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output TauCoeff_SARTA_TraceGas arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------

    TauCoeff_TraceGas_out%Absorber_ID = TauCoeff_TraceGas_in%Absorber_ID


    ! -----------------
    ! Assign array data
    ! -----------------
    TauCoeff_TraceGas_out%Channel_Index  = TauCoeff_TraceGas_in%Channel_Index	 
    TauCoeff_TraceGas_out%Trace_Coeff    = TauCoeff_TraceGas_in%Trace_Coeff	 

  END FUNCTION Assign_Scalar 



  FUNCTION Assign_Rank1(  TauCoeff_TraceGas_in,   &  ! Input     
                          TauCoeff_TraceGas_out,  &  ! Output      
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
    TYPE( TauCoeff_SARTA_TraceGas_type ),  DIMENSION( : ), INTENT( IN )     :: TauCoeff_TraceGas_in

    ! -- Output
    TYPE( TauCoeff_SARTA_TraceGas_type ),  DIMENSION( : ), INTENT( IN OUT ) :: TauCoeff_TraceGas_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_TauCoeff_TraceGas(Rank-1)'

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

    n = SIZE( TauCoeff_TraceGas_in )

    IF ( SIZE( TauCoeff_TraceGas_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input TauCoeff_SARTA_TraceGas_in and TauCoeff_SARTA_TraceGas_out '//&
			    'arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n 

      Scalar_Status = Assign_Scalar(   TauCoeff_TraceGas_in(i), & 
				       TauCoeff_TraceGas_out(i), &
                                       Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i5, &
                          &" of TauCoeff_SARTA_TraceGas structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO
  END FUNCTION Assign_Rank1


  SUBROUTINE Zero_Scalar( TauCoeff_TraceGas )  ! Output
    TYPE( TauCoeff_SARTA_TraceGas_type ),  INTENT( IN OUT ) :: TauCoeff_TraceGas

    ! -- Reset the array components
    TauCoeff_TraceGas%Channel_Index      = 0  
    TauCoeff_TraceGas%Trace_Coeff        = 0.0
 
  END SUBROUTINE Zero_Scalar


  SUBROUTINE Zero_Rank1( TauCoeff_TraceGas )  ! Output

    TYPE( TauCoeff_SARTA_TraceGas_type ), DIMENSION( : ), INTENT( IN OUT ) :: TauCoeff_TraceGas
    INTEGER :: n

    DO n = 1, SIZE( TauCoeff_TraceGas )
      CALL Zero_Scalar( TauCoeff_TraceGas(n) )
    END DO

  END SUBROUTINE Zero_Rank1


END MODULE TauCoeff_SARTA_TraceGas_Define



