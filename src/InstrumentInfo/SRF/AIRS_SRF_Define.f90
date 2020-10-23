!------------------------------------------------------------------------------
!M+
! NAME:
!       AIRS_SRF_Define
!
! PURPOSE:
!       Module defining the AIRS SRF data structure and containing routines
!       to manipulate it.
!
! CATEGORY:
!       Instrument Information : SRF
!
! CALLING SEQUENCE:
!       USE AIRS_SRF_Define
!
! MODULES:
!
! CONTAINS:
!
! EXTERNALS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known
!
! RESTRICTIONS:
!       None known
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 19-Nov-2001
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2001 Paul van Delst
!
!M-
!------------------------------------------------------------------------------

MODULE AIRS_SRF_Define

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

  PRIVATE
  PUBLIC :: Initialize_AIRS_SRF
  PUBLIC :: Destroy_AIRS_SRF
  PUBLIC :: Allocate_AIRS_SRF
  PUBLIC :: Assign_AIRS_SRF


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- RCS Id field
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Invalid specifier
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1


  ! -- AIRS SRF data types

  !....THESE ARE *REQUIRED* FOR HDF ACCESS....
!  INTEGER, PUBLIC, PARAMETER :: AIRS_SRF_chanid_type = Short
!  INTEGER, PUBLIC, PARAMETER :: AIRS_SRF_freq_type   = Double
!  INTEGER, PUBLIC, PARAMETER :: AIRS_SRF_fwgrid_type = Single
!  INTEGER, PUBLIC, PARAMETER :: AIRS_SRF_srfval_type = Single
!  INTEGER, PUBLIC, PARAMETER :: AIRS_SRF_width_type  = Single

  !....THESE WILL WORK FOR NETCDF ACCESS....
  !....PREFER THESE DEFns SINCE THEY ARE GENERAL....
  INTEGER, PUBLIC, PARAMETER :: AIRS_SRF_chanid_type = Long    !Short
  INTEGER, PUBLIC, PARAMETER :: AIRS_SRF_freq_type   = fp_kind !Double
  INTEGER, PUBLIC, PARAMETER :: AIRS_SRF_fwgrid_type = fp_kind !Single
  INTEGER, PUBLIC, PARAMETER :: AIRS_SRF_srfval_type = fp_kind !Single
  INTEGER, PUBLIC, PARAMETER :: AIRS_SRF_width_type  = fp_kind !Single


  ! -----------------------------------------------------
  ! AIRS SRF type definition
  ! Values with Double or Single floating point types are
  ! declared the same as in the HDF data file.
  ! -----------------------------------------------------

  TYPE, PUBLIC :: AIRS_SRF_type
    INTEGER :: n_Allocates

    INTEGER :: n_Points
    INTEGER :: Channel

    REAL( AIRS_SRF_freq_type )  :: Central_Frequency
    REAL( AIRS_SRF_width_type ) :: FWHM

    REAL( fp_kind )             :: Begin_Frequency
    REAL( fp_kind )             :: End_Frequency

    REAL( AIRS_SRF_fwgrid_type ), POINTER, DIMENSION( : ) :: Frequency
    REAL( AIRS_SRF_srfval_type ), POINTER, DIMENSION( : ) :: Response
  END TYPE AIRS_SRF_type




  ! ----------
  ! Intrinsics
  ! ----------

  INTRINSIC ASSOCIATED, &
            PRESENT, &
            REAL, &
            TRIM



CONTAINS



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
!       Clear_AIRS_SRF
!
! PURPOSE:
!       Subroutine to clear the scalar members of an AIRS SRF structure.
!
! CATEGORY:
!       SRF
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       CALL Clear_AIRS_SRF( AIRS_SRF )
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       AIRS_SRF:    AIRS_SRF structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       AIRS_SRF_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-May-2002
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------------

  SUBROUTINE Clear_AIRS_SRF( AIRS_SRF )

    TYPE( AIRS_SRF_type ), INTENT( OUT ) :: AIRS_SRF

    AIRS_SRF%n_Points          = 0
    AIRS_SRF%Channel           = INVALID
    AIRS_SRF%Central_Frequency = REAL( INVALID, AIRS_SRF_freq_type )
    AIRS_SRF%FWHM              = REAL( INVALID, AIRS_SRF_width_type )
    AIRS_SRF%Begin_Frequency   = REAL( INVALID, fp_kind )
    AIRS_SRF%End_Frequency     = REAL( INVALID, fp_kind )

  END SUBROUTINE Clear_AIRS_SRF





!--------------------------------------------------------------------------------
!
! NAME:
!       Associated_AIRS_SRF
!
! PURPOSE:
!       Function to test if ALL the pointer members of a AIRS SRF structure
!       are associated.
!
! CATEGORY:
!       AIRS_SRF
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = Associated_AIRS_SRF( AIRS_SRF )  ! Input
!
! INPUT ARGUMENTS:
!       AIRS_SRF:        AIRS_SRF structure which is to have its pointer
!                        member's association status tested.
!                        UNITS:      N/A
!                        TYPE:       AIRS_SRF_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The return value is a logical value.
!
!       If result = .TRUE.  if ALL the AIRS_SRF pointer members are
!                           associated, or if the ANY_Test argument is set 
!                           and ANY of the AIRS_SRF pointer members
!                           associated.
!                 = .FALSE. some or all of the AIRS_SRF pointer
!                           members are *not* associated.
!
! CALLS:
!       display_message:      Subroutine to output Messages
!                             SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function tests the association status of the AIRS_SRF structure
!       pointer members, Therefore this function must only be called after
!       the input AIRS_SRF structure has had its pointer members initialized.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Sep-2003
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_AIRS_SRF( AIRS_SRF, &
                                ANY_Test ) &
                              RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( AIRS_SRF_type ), INTENT( IN ) :: AIRS_SRF

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

    ! -- ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( AIRS_SRF%Frequency ) .AND. &
           ASSOCIATED( AIRS_SRF%Response  )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( AIRS_SRF%Frequency ) .OR. &
           ASSOCIATED( AIRS_SRF%Response  )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_AIRS_SRF





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!S+
! NAME:
!       Initialize_AIRS_SRF
! 
! PURPOSE:
!       Subroutine to initialize the scalar and pointer members of AIRS_SRF
!       data structures.
!
! CATEGORY:
!       SRF
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       CALL Initialize_AIRS_SRF( AIRS_SRF,       &  ! Output
!                                 RCS_Id = RCS_Id )  ! Revision control
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       AIRS_SRF:     AIRS_SRF structure which is to be initialized
!                     UNITS:      N/A
!                     TYPE:       AIRS_SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This subroutine nullifies the AIRS_SRF structure pointer members.
!       Therefore, this function should *only* be called to initialise AIRS_SRF
!       structures before their *first* use. Subsequent re-initialisations
!       should be done using the destroy_AIRS_SRF() function.
!       
! PROCEDURE:
!       The scalar structure members are set to an "invalid" value and the 
!       pointer members are nullified.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 19-Nov-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  SUBROUTINE Initialize_AIRS_SRF( AIRS_SRF, &  ! Output
                                  RCS_Id    )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    TYPE( AIRS_SRF_type ),    INTENT( OUT ) :: AIRS_SRF

    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM INITIALISATION --                       #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------
    ! Initialise the allocation counter. This
    ! is only done for initialisation, not in
    ! the Clear() function
    ! ---------------------------------------

    AIRS_SRF%n_Allocates = 0


    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    CALL Clear_AIRS_SRF( AIRS_SRF )


    ! ---------------------------
    ! Nullify the pointer members
    ! ---------------------------

    NULLIFY( AIRS_SRF%Frequency, &
             AIRS_SRF%Response )
             
  END SUBROUTINE Initialize_AIRS_SRF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_AIRS_SRF
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of AIRS_SRF
!       data structures.
!
! CATEGORY:
!       SRF
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = Destroy_AIRS_SRF( AIRS_SRF,                 &  ! Output
!                                  RCS_Id = RCS_Id,          &  ! Revision control
!                                  Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       Character
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AIRS_SRF:     Re-initialised AIRS_SRF structure.
!                     UNITS:      N/A
!                     TYPE:       AIRS_SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       The return value is an integer defining the error status. The
!       error status codes are defined in the Message_Handler module.
!
!       If result = SUCCESS the structure re-initialisation was successful
!                 = FAILURE an error occurred
!
! CALLS:
!       display_message:    Subroutine to output messages
!                           SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function checks the association status of the AIRS_SRF structure
!       pointer members. Therefore, this function should *only* be called
!       *after* the AIRS_SRF structure has been initialised via the
!       initialize_AIRS_SRF() subroutine.
!
!       This restriction is due to Fortran-90 not providing any mechanism
!       for initialising pointer association status in derived type definitions.
!       This means the association status of the AIRS_SRF structure pointer members
!       will be undefined until they are initialised (via the initialize_AIRS_SRF()
!       subroutine).
!
! PROCEDURE:
!       The scalar structure members are set to an "invalid" value and the 
!       pointer members are deallocated.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_AIRS_SRF( AIRS_SRF,     &  ! Output
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
    TYPE( AIRS_SRF_type ),    INTENT( IN OUT ) :: AIRS_SRF

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_AIRS_SRF'


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
    !#            -- TEST THE INPUT STRUCTURE POINTER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_AIRS_SRF( AIRS_SRF ) ) THEN
      CALL display_message( ROUTINE_NAME, &
                            'Not all input AIRS_SRF pointer members are associated.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- PERFORM REINITIALISATION --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    CALL Clear_AIRS_SRF( AIRS_SRF )


    ! ---------------------------------
    ! Deallocate the AIRS_SRF frequency
    ! ---------------------------------

    IF ( ASSOCIATED( AIRS_SRF%Frequency ) ) THEN

      DEALLOCATE( AIRS_SRF%Frequency, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AIRS_SRF frequency ", &
                          &"array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL display_message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! --------------------------------
    ! Deallocate the AIRS_SRF response
    ! --------------------------------

    IF ( ASSOCIATED( AIRS_SRF%Response ) ) THEN

      DEALLOCATE( AIRS_SRF%Response, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AIRS_SRF response ", &
                          &"array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL display_message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- DECREMENT AND TEST ALLOCATION COUNTER --                #
    !#--------------------------------------------------------------------------#

    AIRS_SRF%n_Allocates = AIRS_SRF%n_Allocates - 1

    IF ( AIRS_SRF%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      AIRS_SRF%n_Allocates
      CALL display_message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_AIRS_SRF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_AIRS_SRF
! 
! PURPOSE:
!       Function to allocate the pointer members of the AIRS_SRF data structure.
!
! CATEGORY:
!       SRF
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = Allocate_AIRS_SRF( n_Points, &  ! Input
!
!                                   AIRS_SRF, &  ! Output
!
!                                   RCS_Id = RCS_Id,          &  ! Revision control
!                                   Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Points:     Required dimension of AIRS_SRF structure pointer members.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       Character
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AIRS_SRF:     AIRS_SRF structure with allocated pointer members
!                     UNITS:      N/A
!                     TYPE:       AIRS_SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       The return value is an integer defining the error status. The
!       error status codes are defined in the Message_Handler module.
!
!       If result = SUCCESS the structure pointer allocations were successful
!                 = FAILURE an error occurred
!
! CALLS:
!       display_message:    Subroutine to output messages
!                           SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function checks the association status of the AIRS_SRF structure
!       pointer members. Therefore, this function should *only* be called
!       *after* the AIRS_SRF structure has been initialised via the
!       Initialize_AIRS_SRF() subroutine.
!
!       This restriction is due to Fortran-90 not providing any mechanism
!       for initialising pointer association status in derived type definitions.
!       This means the association status of the AIRS_SRF structure pointer members
!       will be undefined until they are initialised (via the Initialize_AIRS_SRF()
!       subroutine).
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jan-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Allocate_AIRS_SRF( n_Points,     &  ! Input
                              AIRS_SRF,     &  ! Output
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
    INTEGER,                  INTENT( IN )  :: n_Points

    ! -- Output
    TYPE( AIRS_SRF_type ),    INTENT( OUT ) :: AIRS_SRF

    ! -- Revision control
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_AIRS_SRF'


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
      CALL display_message( ROUTINE_NAME, &
                            'Input n_Points must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------------------
    ! Check if ANY pointers are already associated
    ! --------------------------------------------

    IF ( Associated_AIRS_SRF( AIRS_SRF, ANY_Test = SET ) ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME,    &
                            'One or more AIRS_SRF pointer members are already associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE(  AIRS_SRF%Frequency( n_Points ), &
               AIRS_SRF%Response( n_Points ), &
               STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating AIRS_SRF data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL display_message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- ASSIGN THE N_POINTS MEMBER --                      #
    !#--------------------------------------------------------------------------#

    AIRS_SRF%n_Points = n_Points



    !#--------------------------------------------------------------------------#
    !#             -- FILL THE POINTER MEMBERS WITH INVALID VALUES --           #
    !#--------------------------------------------------------------------------#

    AIRS_SRF%Frequency = REAL( INVALID, fp_kind )
    AIRS_SRF%Response  = REAL( INVALID, fp_kind )



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    AIRS_SRF%n_Allocates = AIRS_SRF%n_Allocates + 1

    IF ( AIRS_SRF%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      AIRS_SRF%n_Allocates
      CALL display_message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_AIRS_SRF







!------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_AIRS_SRF
!
! PURPOSE:
!       Function to copy valid AIRS_SRF structures.
!
! CATEGORY:
!       SRF
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = Assign_AIRS_SRF( AIRS_SRF_in,  &  ! Input
!                                 AIRS_SRF_out, &  ! Output
!
!                                 RCS_Id = RCS_Id,          &  ! Revision control
!                                 Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AIRS_SRF_in:  AIRS_SRF structure which is to be copied.
!                     UNITS:      N/A
!                     TYPE:       AIRS_SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       Character
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AIRS_SRF_out: Copy of the input structure, AIRS_SRF_in.
!                     UNITS:      N/A
!                     TYPE:       AIRS_SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       The return value is an integer defining the error status. The
!       error status codes are defined in the Message_Handler module.
!
!       If result = SUCCESS the AIRS_SRF structure assignment was successful
!                 = FAILURE an error occurred
!
! CALLS:
!       Allocate_AIRS_SRF:    Function to allocate the pointer members of
!                             the AIRS_SRF data structure.
!
!       display_message:      Subroutine to output messages
!                             SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function allocates the output AIRS_SRF structure pointer members.
!       Therefore this function should *only* be called *after* the output
!       AIRS_SRF structure has been initialised via the initialize_AIRS_SRF()
!       subroutine or re-initialised via the Destroy_AIRS_SRF() function.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Assign_AIRS_SRF( AIRS_SRF_in,  &  ! Input
                            AIRS_SRF_out, &  ! Output
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
    TYPE( AIRS_SRF_type ),    INTENT( IN )  :: AIRS_SRF_in

    ! -- Output
    TYPE( AIRS_SRF_type ),    INTENT( OUT ) :: AIRS_SRF_out

    ! -- Revision control
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_AIRS_SRF'



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

    IF ( .NOT. Associated_AIRS_SRF( AIRS_SRF_In ) ) THEN

      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME,    &
                            'Some or all INPUT AIRS_SRF pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------------------
    ! ANY *output* pointers must NOT be associated
    ! --------------------------------------------

    IF ( Associated_AIRS_SRF( AIRS_SRF_Out, ANY_Test = SET ) ) THEN

      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME,    &
                            'Some or all OUTPUT AIRS_SRF pointer '//&
                            'members are associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Allocate the pointer members
    ! ----------------------------

    Error_Status = Allocate_AIRS_SRF( AIRS_SRF_in%n_Points, &
                                      AIRS_SRF_out,         &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME,    &
                            'Error allocating output AIRS_SRF arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------
    ! Assign scalar members
    ! ---------------------

    AIRS_SRF_out%Channel = AIRS_SRF_in%Channel

    AIRS_SRF_out%Central_Frequency = AIRS_SRF_in%Central_Frequency
    AIRS_SRF_out%FWHM              = AIRS_SRF_in%FWHM
    AIRS_SRF_out%Begin_Frequency   = AIRS_SRF_in%Begin_Frequency
    AIRS_SRF_out%End_Frequency     = AIRS_SRF_in%End_Frequency


    ! -----------------
    ! Assign array data
    ! -----------------

    AIRS_SRF_out%Response  = AIRS_SRF_in%Response
    AIRS_SRF_out%Frequency = AIRS_SRF_in%Frequency

  END FUNCTION Assign_AIRS_SRF

END MODULE AIRS_SRF_Define


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/08/15 20:51:04 $
!
! $Revision$
!
! $State: Exp $
!
! $Log: AIRS_SRF_Define.f90,v $
! Revision 1.6  2006/08/15 20:51:04  wd20pd
! Additional replacement of Error_Handler with Message_Handler.
!
! Revision 1.5  2006/05/02 16:58:02  dgroff
! *** empty log message ***
!
! Revision 1.4  2003/11/19 15:26:26  paulv
! - Updated header documentation.
!
! Revision 1.3  2003/09/04 15:16:42  paulv
! - Added Associated() function.
! - Other changes made to update code to current module standards.
!
! Revision 1.2  2002/05/20 19:47:52  paulv
! - Changed the AIRS SRF data types from the HDF required ones to the netCDF ones.
!
! Revision 1.1  2002/05/08 19:16:24  paulv
! Initial checkin.
!
!
!
!
