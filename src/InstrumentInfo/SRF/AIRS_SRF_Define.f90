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




  SUBROUTINE Clear_AIRS_SRF( AIRS_SRF )

    TYPE( AIRS_SRF_type ), INTENT( OUT ) :: AIRS_SRF

    AIRS_SRF%n_Points          = 0
    AIRS_SRF%Channel           = INVALID
    AIRS_SRF%Central_Frequency = REAL( INVALID, AIRS_SRF_freq_type )
    AIRS_SRF%FWHM              = REAL( INVALID, AIRS_SRF_width_type )
    AIRS_SRF%Begin_Frequency   = REAL( INVALID, fp_kind )
    AIRS_SRF%End_Frequency     = REAL( INVALID, fp_kind )

  END SUBROUTINE Clear_AIRS_SRF






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


