!------------------------------------------------------------------------------
!M+
! NAME:
!       SRF_Define
!
! PURPOSE:
!       Module defining the SRF data structure and containing routines to 
!       manipulate it.
!       
! CATEGORY:
!       Instrument Information : SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE SRF_Define
!
! MODULES:
!       Type_Kinds:        Module containing definitions for kinds
!                          of variable types.
!
!       Message_Handler:   Module to define simple error codes and
!                          handle error conditions
!                          USEs: FILE_UTILITY module
!
!       Integrate_Utility: Module containing integration routines.
!                          USEs: TYPE_KINDS module
!                                Message_Handler module
!                                INTERPOLATE module
!
! CONTAINS:
!       Associated_SRF:  Function to test the association status
!                        of the pointer members of a SRF
!                        structure.
!
!       Destroy_SRF:     Function to re-initialize an SRF structure.
!
!       Allocate_SRF:    Function to allocate the pointer members
!                        of an SRF structure.
!
!       Assign_SRF:      Function to copy an SRF structure.
!
!       Frequency_SRF:   Function to compute the frequency grid for
!                        a supplied SRF data structure.
!
!       Integrate_SRF:   Function to integrate the SRF for 
!                        a supplied SRF data structure.
!
!       Information_SRF: Subroutine to return a string containing information
!                        about the SRF data structure.
!
!
! DERIVED TYPES:
!       SRF_type:  Definition of the public SRF data structure.
!                  Fields are,
!
!         n_Points:          Number of points defining the current
!                            SRF spectral dimension.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!         Sensor_Name:       Character string containing the name of the
!                            sensor for the current SRF.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!
!         Platform_Name:     Character string containing the name of the
!                            platform containing the sensor for the current
!                            SRF.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!
!         NCEP_Sensor_ID:    An "in-house" value used at NOAA/NCEP/EMC 
!                            to identify a satellite/sensor combination.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!         WMO_Satellite_ID:  The WMO code for identifying satellite
!                            platforms. Taken from the WMO common
!                            code tables at:
!                              http://www.wmo.ch/web/ddbs/Code-tables.html
!                            The Satellite ID is from Common Code
!                            table C-5, or code table 0 01 007 in BUFR
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!         WMO_Sensor_ID:     The WMO code for identifying a satelite
!                            sensor. Taken from the WMO common
!                            code tables at:
!                              http://www.wmo.ch/web/ddbs/Code-tables.html
!                            The Sensor ID is from Common Code
!                            table C-8, or code table 0 02 019 in BUFR
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!         Channel:           Sensor channel number of the currently
!                            defined SRF
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!         Begin_Frequency:   The frequency of the first SRF point.
!                            UNITS:      cm^-1
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Scalar
!
!         End_Frequency:     The frequency of the last SRF point.
!                            UNITS:      cm^-1
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Scalar
!
!         Frequency:         The frequency grid of the SRF data.
!                            UNITS:      inverse centimetres (cm^-1)
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Rank-1
!                            ATTRIBUTES: POINTER
!
!         Response:          Array containing the channel
!                            spectral response data.
!                            UNITS:      None
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Rank-1
!                            ATTRIBUTES: POINTER
!
!         Integrated_SRF:    The integrated area of the SRF determined using
!                            an integration formula.
!                            UNITS:      None.
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Scalar
!
!         Summation_SRF:     The integrated area of the SRF determined by
!                            simple summation.
!                            UNITS:      None.
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Scalar
!
!       *!IMPORTANT!*
!       -------------
!       Note that the SRF_type is PUBLIC and its members are not
!       encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user destroy,
!       allocate, assign, and concatenate the structure using only
!       the routines in this module where possible to eliminate --
!       or at least minimise -- the possibility of memory leakage
!       since some of the structure members are pointers.
!
! INCLUDE FILES:
!      None.
!
! EXTERNALS:
!      None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!      None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2001 Paul van Delst
!
!M-
!------------------------------------------------------------------------------

MODULE SRF_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler
  USE Integrate_Utility


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Associated_SRF
  PUBLIC :: Destroy_SRF
  PUBLIC :: Allocate_SRF
  PUBLIC :: Assign_SRF
  PUBLIC :: Frequency_SRF
  PUBLIC :: Integrate_SRF


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE Destroy_SRF
    MODULE PROCEDURE Destroy_SRF_scalar
    MODULE PROCEDURE Destroy_SRF_rank1
  END INTERFACE ! Destroy_SRF


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id field
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Literal constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind

  ! -- SRF invalid values
  INTEGER,         PRIVATE, PARAMETER ::    INVALID = -1
  REAL( fp_kind ), PRIVATE, PARAMETER :: FP_INVALID = -1.0_fp_kind

  ! -- SRF character strings length
  INTEGER, PRIVATE, PARAMETER :: SDSL = 64

  ! ------------------------
  ! SRF data type definition
  ! ------------------------

  TYPE, PUBLIC :: SRF_type
    INTEGER :: n_Allocates = 0

    INTEGER :: StrLen = SDSL

    INTEGER :: n_Points = 0

    CHARACTER( SDSL ) :: Sensor_Name   = ' '
    CHARACTER( SDSL ) :: Platform_Name = ' '
    INTEGER :: NCEP_Sensor_Id   = INVALID
    INTEGER :: WMO_Satellite_Id = INVALID
    INTEGER :: WMO_Sensor_Id    = INVALID
    INTEGER :: Channel          = INVALID

    REAL( fp_kind ) :: Begin_Frequency = FP_INVALID
    REAL( fp_kind ) :: End_Frequency   = FP_INVALID
    REAL( fp_kind ) :: Integrated_SRF  = FP_INVALID
    REAL( fp_kind ) :: Summation_SRF   = FP_INVALID

    REAL( fp_kind ), POINTER, DIMENSION( : ) :: Frequency => NULL()
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: Response  => NULL()
  END TYPE SRF_type


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
!       Clear_SRF
!
! PURPOSE:
!       Subroutine to clear the scalar members of a SRF structure.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_SRF( SRF )
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       SRF:         SRF structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       SRF_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN OUT )
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
! COMMENTS:
!       Note the INTENT on the output SRF argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-May-2002
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------------

  SUBROUTINE Clear_SRF( SRF )

    TYPE( SRF_type ), INTENT( IN OUT ) :: SRF

    SRF%StrLen = SDSL

    SRF%n_Points = 0

    SRF%Sensor_Name   = ' '
    SRF%Platform_Name = ' '

    SRF%NCEP_Sensor_Id   = INVALID
    SRF%WMO_Satellite_Id = INVALID
    SRF%WMO_Sensor_Id    = INVALID
    SRF%Channel          = INVALID

    SRF%Begin_Frequency = FP_INVALID
    SRF%End_Frequency   = FP_INVALID
    SRF%Integrated_SRF  = FP_INVALID
    SRF%Summation_SRF   = FP_INVALID

  END SUBROUTINE Clear_SRF





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!S+
! NAME:
!       Associated_SRF
!
! PURPOSE:
!       Function to test if ALL the pointer members of a SRF structure
!       are associated.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_SRF( SRF,                &  ! Input
!                                            ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       SRF:                 SRF structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       SRF_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            SRF structure pointer members are associated.
!                            The default is to test if ALL the pointer members
!                            are associated.
!                            If ANY_Test = 0, test if ALL the pointer members
!                                             are associated.  (DEFAULT)
!                               ANY_Test = 1, test if ANY of the pointer members
!                                             are associated.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the SRF pointer members.
!                            .TRUE.  - if ALL the SRF pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the SRF pointer
!                                      members are associated.
!                            .FALSE. - some or all of the SRF pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:      Subroutine to output Messages
!                             SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function tests the association status of the SRF structure
!       pointer members, Therefore this function must only be called after
!       the input SRF structure has had its pointer members initialized.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_SRF( SRF, &
                           ANY_Test ) &
                         RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SRF_type ),  INTENT( IN ) :: SRF

    ! -- Optional input
    INTEGER, OPTIONAL, INTENT( IN ) :: ANY_Test


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

      IF ( ASSOCIATED( SRF%Frequency ) .AND. &
           ASSOCIATED( SRF%Response  )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( SRF%Frequency ) .OR. &
           ASSOCIATED( SRF%Response  )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_SRF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_SRF
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of SRF
!       data structures.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_SRF( SRF,                      &  ! Output
!                                   RCS_Id = RCS_Id,          &  ! Revision control
!                                   Message_Log = Message_Log )  ! Error messaging
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
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SRF:          Re-initialised SRF structure.
!                     UNITS:      N/A
!                     TYPE:       SRF_type
!                     DIMENSION:  Scalar or Rank-1 array
!                     ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure re-initialisation was successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to zero (0) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:    Subroutine to output messages
!                           SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SRF argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_SRF_scalar( SRF,          &  ! Output
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
    TYPE( SRF_type ),         INTENT( IN OUT ) :: SRF

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_Clear

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_SRF(scalar)'


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
    !#                      -- PERFORM REINITIALISATION --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_SRF( SRF )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_SRF( SRF ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the SRF Frequency
    IF ( ASSOCIATED( SRF%Frequency ) ) THEN

      DEALLOCATE( SRF%Frequency, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SRF frequency ", &
                          &"array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the SRF Response
    IF ( ASSOCIATED( SRF%Response ) ) THEN

      DEALLOCATE( SRF%Response, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SRF response ", &
                          &"array. STAT = ", i5 )' ) &
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

    SRF%n_Allocates = SRF%n_Allocates - 1

    IF ( SRF%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      SRF%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_SRF_scalar

  FUNCTION Destroy_SRF_rank1( SRF,          &  ! Output
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
    TYPE( SRF_type ), DIMENSION( : ), INTENT( IN OUT ) :: SRF

    ! -- Optional input
    INTEGER,        OPTIONAL,         INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ), OPTIONAL,         INTENT( OUT )    :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,         INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_SRF(rank1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Scalar_Status
    INTEGER :: l



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
    !#                      -- PERFORM REINITIALISATION --                      #
    !#--------------------------------------------------------------------------#

    DO l = 1, SIZE( SRF )

      ! -- Clear the current structure array element
      Scalar_Status = Destroy_SRF_scalar( SRF( l ), &
                                          No_Clear = No_Clear, &
                                          Message_Log = Message_Log )

      ! -- If it failed, set the return error status, but
      ! -- continue to attempt to destroy structure array
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( i10 )' ) l
        CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying SRF structure array element '//&
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Destroy_SRF_rank1




!------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_SRF
! 
! PURPOSE:
!       Function to allocate the pointer members of the SRF data structure.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_SRF( n_Points,                 &  ! Input
!                                    SRF,                      &  ! Output
!                                    RCS_Id      = RCS_Id,     &  ! Revision control
!                                    Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Points:     Required dimension of SRF structure pointer members.
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
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SRF:          SRF structure with allocated pointer members
!                     UNITS:      N/A
!                     TYPE:       SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure re-initialisation was successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to one (1) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_SRF:     Function to test the association status of the
!                           pointer members of a SRF structure.
!
!       Destroy_SRF:        Function to re-initialize the scalar and pointer
!                           members of SRF data structures.
!
!       Display_Message:    Subroutine to output messages
!                           SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SRF argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jan-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Allocate_SRF( n_Points,     &  ! Input
                         SRF,          &  ! Output
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
    INTEGER,                  INTENT( IN )     :: n_Points

    ! -- Output
    TYPE( SRF_type ),         INTENT( IN OUT ) :: SRF

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_SRF'


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
      CALL Display_Message( ROUTINE_NAME, &
                            'Input N_POINTS must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_SRF( SRF, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_SRF( SRF, &
                                  No_Clear = SET, &
                                  Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating SRF pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE(  SRF%Frequency( n_Points ), &
               SRF%Response(  n_Points ), &
               STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating SRF data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- ASSIGN THE N_POINTS MEMBER --                      #
    !#--------------------------------------------------------------------------#

    SRF%n_Points = n_Points



    !#--------------------------------------------------------------------------#
    !#                  -- INITIALISE THE POINTER MEMBERS --                    #
    !#--------------------------------------------------------------------------#

    SRF%Frequency = ZERO
    SRF%Response  = ZERO



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    SRF%n_Allocates = SRF%n_Allocates + 1

    IF ( SRF%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      SRF%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_SRF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_SRF
!
! PURPOSE:
!       Function to copy valid SRF structures.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_SRF( SRF_in,                    &  ! Input
!                                  SRF_out,                   &  ! Output
!                                  Scalar_Only = Scalar_Only, &  ! Opotional input
!                                  RCS_Id      = RCS_Id,      &  ! Revision control
!                                  Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SRF_in:       SRF structure which is to be copied.
!                     UNITS:      N/A
!                     TYPE:       SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Scalar_Only:  Set this argument to copy *only* the scalar components.
!                     The default action is to copy all components, allocating
!                     the arrays as required.
!                     If == 0, Copy ALL components, allocating arrays as required.
!                        == 1, Copy scalar components ONLY. No array allocation
!                              is performed.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL

!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SRF_out:      Copy of the input structure, SRF_in.
!                     UNITS:      N/A
!                     TYPE:       SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure assignment was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_SRF:       Function to test the association status of the
!                             pointer members of a SRF structure.
!
!       Allocate_SRF:         Function to allocate the pointer members of
!                             the SRF data structure.
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SRF argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Assign_SRF( SRF_in,       &  ! Input
                       SRF_out,      &  ! Output
                       Scalar_Only,  &  ! Optional input
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
    TYPE( SRF_type ),         INTENT( IN )     :: SRF_in

    ! -- Output
    TYPE( SRF_type ),         INTENT( IN OUT ) :: SRF_out

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: Scalar_Only

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_SRF'


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Copy_Arrays



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

    IF ( .NOT. Associated_SRF( SRF_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT SRF pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CHECK THE OPTIONAL ARGUMENTS --                    #
    !#--------------------------------------------------------------------------#

    ! -- Default is to copy the array components...
    Copy_Arrays = .TRUE.
    ! -- ...unless the Scalar_Only argument is set
    IF ( PRESENT( Scalar_Only ) ) THEN
      IF ( Scalar_Only == SET ) Copy_Arrays = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! Assign scalar members
    ! ---------------------

    SRF_out%Sensor_Name   = SRF_in%Sensor_Name
    SRF_out%Platform_Name = SRF_in%Platform_Name

    SRF_out%NCEP_Sensor_Id   = SRF_in%NCEP_Sensor_Id
    SRF_out%WMO_Satellite_Id = SRF_in%WMO_Satellite_Id
    SRF_out%WMO_Sensor_Id    = SRF_in%WMO_Sensor_Id
    SRF_out%Channel          = SRF_in%Channel

    SRF_out%Begin_Frequency = SRF_in%Begin_Frequency
    SRF_out%End_Frequency   = SRF_in%End_Frequency

    SRF_out%Integrated_SRF = SRF_in%Integrated_SRF
    SRF_out%Summation_SRF  = SRF_in%Summation_SRF


    ! -----------------
    ! Assign array data
    ! -----------------

    IF ( Copy_Arrays ) THEN

      ! -- Allocate data arrays
      Error_Status = Allocate_SRF( SRF_in%n_Points, &
                                   SRF_out,         &
                                   Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error allocating output SRF arrays.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Copy array data
      SRF_out%Frequency = SRF_in%Frequency
      SRF_out%Response  = SRF_in%Response

    END IF

  END FUNCTION Assign_SRF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Frequency_SRF
!
! PURPOSE:
!       Function to compute the frequency grid for a supplied SRF data
!       structure.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Frequency_SRF( SRF,                      &  ! In/Output
!                                     Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SRF:          SRF structure with fields containing the begin and
!                     end frequencies of the frequency grid to compute.
!                     UNITS:      N/A
!                     TYPE:       SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SRF:          SRF structure with the frequency component filled.
!                     UNITS:      N/A
!                     TYPE:       SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the frequency grid calculation was successful
!                        == FAILURE an error occurred processing the input
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:      Subroutine to output messages
!                             SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       The FREQUENCY field of the input SRF structure is filled.
!
! RESTRICTIONS:
!       SRF structure must contain at least 2 points of frequency and response
!       data.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-May-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Frequency_SRF( SRF,          &  ! In/Output
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SRF_type ),         INTENT( IN OUT ) :: SRF

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Frequency_SRF'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE ARGUMENT POINTER ASSOCIATION --          #
    !#--------------------------------------------------------------------------#

    ! -------------------------------
    ! ALL pointers must be associated
    ! -------------------------------

    IF ( .NOT. Associated_SRF( SRF ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SRF pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CALCULATE THE FREQUENCY GRID --                    #
    !#--------------------------------------------------------------------------#

    ! --------------------------
    ! Check the number of points
    ! --------------------------

    n = SRF%n_Points

    IF ( n < 2 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Allocated SRF structure arrays must contain at least 2 points.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------------
    ! Construct a frequency grid of 0->1
    ! ----------------------------------

    SRF%Frequency(1:n)  = (/ ( REAL( i - 1, fp_kind ), i = 1, n ) /) / &
    !                    ------------------------------------------
                                     REAL( n - 1, fp_kind )


    ! -----------------------------
    ! Scale it to the actual values
    ! -----------------------------

    SRF%Frequency(1:n) = SRF%Begin_Frequency + &
                         ( SRF%Frequency(1:n) * ( SRF%End_Frequency - SRF%Begin_Frequency ) )

  END FUNCTION Frequency_SRF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Integrate_SRF
!
! PURPOSE:
!       Function to integrate the response supplied in an SRF data
!       structure.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Integrate_SRF( SRF,                      &  ! In/Output
!                                     Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SRF:          SRF structure with fields containing the frequency
!                     and response arrays.
!                     UNITS:      N/A
!                     TYPE:       SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SRF:          SRF structure with the integration components filled.
!                     UNITS:      N/A
!                     TYPE:       SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the integration was successful
!                        == FAILURE an error occurred processing the input
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:      Subroutine to output messages
!                             SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       The INTEGRATED_SRF and SUMMATION_SRF fields of the input SRF structure
!       are filled.
!
! RESTRICTIONS:
!       SRF structure must contain at least 2 points of frequency and response
!       data.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Integrate_SRF( SRF,          &  ! In/Output
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SRF_type ),         INTENT( IN OUT ) :: SRF

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Integrate_SRF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: n
    REAL( fp_kind ) :: dF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE ARGUMENT POINTER ASSOCIATION --          #
    !#--------------------------------------------------------------------------#

    ! -------------------------------
    ! ALL pointers must be associated
    ! -------------------------------

    IF ( .NOT. Associated_SRF( SRF ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SRF pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CALCULATE THE INTEGRALS --                       #
    !#--------------------------------------------------------------------------#

    ! --------------------------
    ! Check the number of points
    ! --------------------------

    n = SRF%n_Points

    IF ( n < 2 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Allocated SRF structure arrays must contain at least 2 points.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------
    ! Integration using Simpson's rule
    ! --------------------------------

    Error_Status = Integral( SRF%Frequency, &
                                      SRF%Response,  &
                                      SRF%Integrated_SRF )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error occurred integrating channel ", i5, " SRF" )' ) &
                      SRF%Channel
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------------------------
    ! Integration by simple summation
    ! -------------------------------

    ! -- Compute the frequency grid interval
    dF = SUM( SRF%Frequency( 2:n ) - SRF%Frequency( 1:n-1 ) ) / &
    !    ----------------------------------------------------
                        REAL( n - 1, fp_kind )

    ! -- Do the summation
    SRF%Summation_SRF = SUM( SRF%Response ) * dF

  END FUNCTION Integrate_SRF





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Information_SRF
!
! PURPOSE:
!       Subroutine to return a string containing information about the
!       SRF data structure.
!
! CATEGORY:
!       Instrument Information : SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Information_SRF( SRF,            &  ! Input
!                             Information,    &  ! Output
!                             RCS_Id = RCS_Id )  ! Revision control
! 
! INPUT ARGUMENTS:
!       SRF:           Filled SRF structure.
!                      UNITS:      N/A
!                      TYPE:       SRF_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Information:   String containing information about the passed
!                      SRF data structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      None
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
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
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Information_SRF( SRF,         &  ! Input
                              Information, &  ! Output
                              RCS_Id       )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SRF_type ),         INTENT( IN )  :: SRF

    ! -- Output
    CHARACTER( * ),           INTENT( OUT ) :: Information

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id


    ! ----------
    ! Parameters
    ! ----------

    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 5000 ) :: Long_String



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

    WRITE( Long_String, FMT = '( a," SRF: N_POINTS=",i6, &
                                &a,"      SENSOR NAME  :", a, &
                                &a,"      PLATFORM NAME:", a, &
                                &a,"      CHANNEL      :", i4  )' ) &
                              ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                              SRF%n_Points, &
                              ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                              TRIM( SRF%Sensor_Name ), &
                              ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                              TRIM( SRF%Platform_Name ), &
                              ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                              SRF%Channel

    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Information = Long_String(1:MIN( LEN( Information ), LEN_TRIM( Long_String ) ))

  END SUBROUTINE Information_SRF

END MODULE SRF_Define


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/08/15 20:32:27 $
!
! $Revision: 774 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SRF_Define.f90,v $
! Revision 2.10  2006/08/15 20:32:27  wd20pd
! Altered USE Integrate to USE Integrate_Utility to reflect changes in
! CRTM repository heirarchy.
!
! Revision 2.9  2006/05/02 16:58:02  dgroff
! *** empty log message ***
!
! Revision 2.8  2005/08/11 17:34:09  paulv
! - Added structure pointer association test in Destroy() function.
!
! Revision 2.7  2004/08/31 20:47:53  paulv
! - Upgraded to Fortran95.
! - Derived type component initialisation is now done in the defintion block.
! - Initialize_SRF() subroutine has been removed.
! - Intent of SRF dummy argument in Clear_SRF() routine changed from
!   OUT to IN OUT to prevent memory leaks.
! - Added optional No_Clear argument to Destroy_SRF() function.
! - Intent of SRF dummy argument in Allocate_SRF() routine changed from
!   OUT to IN OUT to prevent memory leaks.
! - Call to Destroy_SRF() added to Allocate_SRF() function for the case
!   where the input SRF argument is already allocated.
! - Intent of SRF_out dummy argument in Assign_SRF() routine changed from
!   OUT to IN OUT to prevent memory leaks.
! - Added Scalar_Only optional argument to Assign_SRF() function to copy
!   only the scalar components and do no allocation or array member copy.
! - Updated header documentation.
!
! Revision 2.6  2004/06/25 17:15:57  paulv
! - Removed unused variables from type declarations.
! - Cosmetic changes.
!
! Revision 2.5  2003/12/01 19:34:55  paulv
! - Corrected some documentation errors.
!
! Revision 2.4  2003/11/19 15:26:27  paulv
! - Updated header documentation.
!
! Revision 2.3  2003/11/18 15:31:31  paulv
! - Decreased the number of required points in the Integrate_SRF() routine
!   from 3 to 2.
! - Tidied up output format string in Information_SRF() routine.
!
! Revision 2.2  2003/09/05 16:06:06  paulv
! - Cosmetic changes to documentaiton.
!
! Revision 2.1  2003/09/04 15:20:50  paulv
! - Removed the Clear_SRF() call from the Allocate_SRF() function. Now the
!   scalar members of the unallocated SRF structure can be filled prior to
!   allocation without losing their contents.
!
! Revision 2.0  2003/08/29 18:04:45  paulv
! - New version.
! - Added sensor ID components to structure.
! - Added frequency grid component to structure that is filled in the
!   netCDF and ASCII read functions.
! - Added frequency grid calc and SRF integration functions.
!
! Revision 1.14  2002/11/22 17:30:23  paulv
! - Cosmetic changes only.
!
! Revision 1.13  2002/06/05 19:10:13  paulv
! - Removed MESSAGE as a module variable and placed definitions in each
!   module subprogram.
!
! Revision 1.12  2002/05/31 21:46:42  paulv
! - Initialize_SRF() and Destroy_SRF() routines are now overloaded for scalar
!   and rank-1 array input.
! - Added summation_SRF field to the SRF data type.
!
! Revision 1.11  2002/05/20 19:57:26  paulv
! - Added private Clear_SRF() function to initialise scalar members of the
!   SRF structure and inserted the call to this function in Initialize_SRF()
!   and Destroy_SRF().
!
! Revision 1.10  2002/05/07 14:48:26  paulv
! - Added FREQUENCY_SRF() function to calculate the SRF frequency grid.
!
! Revision 1.9  2002/05/07 14:16:46  paulv
! - Removed FREQUENCY member of the SRF data structure. The frequency
!   grid is now defined solely by the begin and end frequencies and the
!   total number of points for the channel.
!
! Revision 1.8  2002/05/03 19:20:29  paulv
! - Removed CHANNEL_TYPE component from SRF structure.
! - Renamed CHANNEL_NUMBER component of SRF structure to CHANNEL.
! - Added INTEGRATED_SRF component to SRF structure.
! - Altered module routines to reflect changes in SRF structure.
!
! Revision 1.7  2002/03/23 13:43:55  paulv
! - Removed unneeded type declarations from INITIALIZE_SRF().
! - Changed the INTENT( IN OUT ) attribute for SRF arguments to INTENT( OUT ).
!
! Revision 1.6  2002/03/22 23:30:03  paulv
! - Removed VALID_SRF() function. I found that the VALID member of the SRF
!   data structure could have an initial value of anything, including my
!   value that indicated a valid, i.e. initialised, structure. Stupid of me
!   to rely on an initialised value that could be literally anything. So, now
!   there is no VALID_SRF() function and no VALID structure member.
! - Changed INITIALIZE_SRF() from a function to a subroutine. Now all this
!   routine does is nullify the pointer members.
! - Added a DESTROY_SRF() function to deallocate the SRF pointer members,
!   i.e. to RE-initialise an SRF structure.
! - Removed all old VALID_SRF() and INITIALIZE_SRF() function calls from
!   ALLOCATE_SRF() and ASSIGN_SRF() functions.
! - Updated routine header documentation.
!
! Revision 1.5  2002/01/24 22:40:40  paulv
! - Modified for netCDF input
! - Generic SRF_type defined. No ASCII header.
!
!
!
!
