!------------------------------------------------------------------------------
!M+
! NAME:
!       TauProfile_Define
!
! PURPOSE:
!       Module defining the TauProfile data structure and containing
!       routines to manipulate it.
!       
! CATEGORY:
!       TauProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE TauProfile_Define
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Message_Handler:          Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       Compare_Float_Numbers:  Module containing routines to perform equality
!                               check comparisons on input floating point
!                               numbers.
!                               USEs: TYPE_KINDS module
!
! CONTAINS:
!       Associated_TauProfile:  Function to test the association status of the
!                               pointer members of a TauProfile structure.
!
!       Destroy_TauProfile:     Function to re-initialize an TauProfile
!                               structure.
!
!       Allocate_TauProfile:    Function to allocate the pointer members
!                               of an TauProfile structure.
!
!       Assign_TauProfile:      Function to copy an TauProfile structure.
!
!       Concatenate_TauProfile: Function to concatenate two valid TauProfile
!                               structures.
!
!       Information_TauProfile: Subroutine to return a string containing
!                               information about the TauProfile data structure.
!
! DERIVED TYPES:
!       TauProfile_type:  Definition of the public TauProfile data structure.
!                         Fields are,
!
!         n_Layers:          Number of atmospheric layers.
!                            "K" dimension.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!         n_Channels:        Number of spectral channels.
!                            "L" dimension.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!         n_Angles:          Number of view angles.
!                            "I" dimension.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!         n_Profiles:        Number of atmospheric profiles.
!                            "M" dimension.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!         n_Molecule_Sets:   Number of individual or mixed
!                            gaseous absorbers.
!                            "J" dimension.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
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
!         Level_Pressure:    Array containing the level pressure values
!                            associated with the transmittance profiles.
!                            UNITS:      hectoPascals (hPa)
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Rank-1 (n_Layers+1, K+1)
!                            ATTRIBUTES: POINTER
!
!         Channel:           This is the sensor channel number associated
!                            with the transmittance profiles. Helps
!                            in identifying channels where the numbers are
!                            not contiguous (e.g. AIRS).
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Rank-1 (n_Channels, L)
!                            ATTRIBUTES: POINTER
!
!         Angle:             Array containing the view angles associated
!                            with the transmittance profiles.
!                            UNITS:      Degrees from vertical.
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Rank-1 (n_Angles, I)
!                            ATTRIBUTES: POINTER
!
!         Profile:           Array containing the atmospheric profile
!                            index number used in the calculation of
!                            the transmittance profiles..
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Rank-1 (n_Profiles, M)
!                            ATTRIBUTES: POINTER
!
!         Molecule_Set:      Array containing the molecule set ID s
!                            associated with the transmittance
!                            profiles.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Rank-1 (n_Molecule_Sets, J)
!                            ATTRIBUTES: POINTER
!
!         Tau:               The transmittance profile data.
!                            UNITS:      None.
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Rank-5 (K x L x I x M x J)
!                            ATTRIBUTES: POINTER
!
!       *!IMPORTANT!*
!       -------------
!       Note that the TauProfile_type is PUBLIC and its members are not
!       encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user destroy,
!       allocate, assign, and concatenate the structure
!       using only the routines in this module where possible to
!       eliminate -- or at least minimise -- the possibility of 
!       memory leakage since most of the structure members are
!       pointers.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 28-May-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2001 Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!------------------------------------------------------------------------------

MODULE TauProfile_Define_old


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler
  USE Compare_Float_Numbers


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Associated_TauProfile
  PUBLIC :: Destroy_TauProfile
  PUBLIC :: Allocate_TauProfile
  PUBLIC :: Assign_TauProfile
  PUBLIC :: Concatenate_TauProfile
  PUBLIC :: Information_TauProfile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE Destroy_TauProfile
    MODULE PROCEDURE Destroy_TauProfile_scalar
    MODULE PROCEDURE Destroy_TauProfile_rank1
  END INTERFACE ! Destroy_TauProfile


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- TauProfile invalid values
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1
  INTEGER, PRIVATE, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PRIVATE, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


  ! -------------------------------
  ! TauProfile data type definition
  ! -------------------------------

  TYPE, PUBLIC :: TauProfile_type
    INTEGER :: n_Allocates = 0

    INTEGER( Long ) :: n_Layers        = 0 ! == K
    INTEGER( Long ) :: n_Channels      = 0 ! == L
    INTEGER( Long ) :: n_Angles        = 0 ! == I
    INTEGER( Long ) :: n_Profiles      = 0 ! == M
    INTEGER( Long ) :: n_Molecule_Sets = 0 ! == J

    INTEGER( Long ) :: NCEP_Sensor_ID   = INVALID
    INTEGER( Long ) :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER( Long ) :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID   

    REAL( Double ),  DIMENSION( : ), POINTER :: Level_Pressure => NULL() ! K+1
    INTEGER( Long ), DIMENSION( : ), POINTER :: Channel        => NULL() ! L
    REAL( Double ),  DIMENSION( : ), POINTER :: Angle          => NULL() ! I
    INTEGER( Long ), DIMENSION( : ), POINTER :: Profile        => NULL() ! M
    INTEGER( Long ), DIMENSION( : ), POINTER :: Molecule_Set   => NULL() ! J

    REAL( Double ), DIMENSION( :, :, :, :, : ), POINTER :: Tau => NULL() ! K x L x I x M x J

  END TYPE TauProfile_type


CONTAINS





!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       Clear_TauProfile
!
! PURPOSE:
!       Subroutine to clear the scalar members of a TauProfile structure.
!
! CATEGORY:
!       TauProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_TauProfile( TauProfile ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       TauProfile:  TauProfile structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       TauProfile_type
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
!       Note the INTENT on the output TauProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Jun-2002
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_TauProfile( TauProfile )

    TYPE( TauProfile_type ), INTENT( IN OUT ) :: TauProfile

    TauProfile%n_Layers        = 0
    TauProfile%n_Channels      = 0
    TauProfile%n_Angles        = 0
    TauProfile%n_Profiles      = 0
    TauProfile%n_Molecule_Sets = 0

    TauProfile%NCEP_Sensor_ID   = INVALID
    TauProfile%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    TauProfile%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID   

  END SUBROUTINE Clear_TauProfile





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
!       Associated_TauProfile
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       TauProfile structure.
!
! CATEGORY:
!       TauProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_TauProfile( TauProfile,         &  ! Input
!                                                   ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       TauProfile:  TauProfile structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       TauProfile_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                    TauProfile structure pointer members are associated.
!                    The default is to test if ALL the pointer members
!                    are associated.
!                    If ANY_Test = 0, test if ALL the pointer members
!                                     are associated.  (DEFAULT)
!                       ANY_Test = 1, test if ANY of the pointer members
!                                     are associated.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the TauProfile pointer
!                            members.
!                            .TRUE.  - if ALL the TauProfile pointer members
!                                      are associated, or if the ANY_Test argument
!                                      is set and ANY of the TauProfile pointer
!                                      members are associated.
!                            .FALSE. - some or all of the TauProfile pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function tests the association status of the TauProfile
!       structure pointer members. Therefore this function must only
!       be called after the input TauProfile structure has, at least,
!       had its pointer members initialized.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Jun-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_TauProfile( TauProfile, & ! Input
                                  ANY_Test )  & ! Optional input
                                RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauProfile_type ), INTENT( IN ) :: TauProfile

    ! -- Optional input
    INTEGER,       OPTIONAL, INTENT( IN ) :: ANY_Test


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

      IF ( ASSOCIATED( TauProfile%Level_Pressure ) .AND. &
           ASSOCIATED( TauProfile%Channel        ) .AND. &
           ASSOCIATED( TauProfile%Angle          ) .AND. &
           ASSOCIATED( TauProfile%Profile        ) .AND. &
           ASSOCIATED( TauProfile%Molecule_Set   ) .AND. &
           ASSOCIATED( TauProfile%Tau            )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( TauProfile%Level_Pressure ) .OR. &
           ASSOCIATED( TauProfile%Channel        ) .OR. &
           ASSOCIATED( TauProfile%Angle          ) .OR. &
           ASSOCIATED( TauProfile%Profile        ) .OR. &
           ASSOCIATED( TauProfile%Molecule_Set   ) .OR. &
           ASSOCIATED( TauProfile%Tau            )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_TauProfile





!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_TauProfile
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of TauProfile
!       data structures.
!
! CATEGORY:
!       TauProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_TauProfile( TauProfile,               &  ! Output
!                                          RCS_Id = RCS_Id,          &  ! Revision control
!                                          Message_Log = Message_Log )  ! Error messaging
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
!       TauProfile:   Re-initialised TauProfile structure.
!                     UNITS:      N/A
!                     TYPE:       TauProfile_type
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
!                     The error codes are defined in the ERROR_HANDLER module.
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
!       Associated_TauProfile:  Function to check the association status of
!                               the TauProfile pointer components.
!
!       Clear_TauProfile:       Function to clear the scalar members of a
!                               TauProfile structure.
!
!       Display_Message:        Subroutine to output messages
!                               SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output TauProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-May-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_TauProfile_scalar( TauProfile,   &  ! Output
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
    TYPE( TauProfile_type ),  INTENT( IN OUT ) :: TauProfile

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_TauProfile(scalar)'


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
    !#                     -- PERFORM RE-INITIALISATION --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_TauProfile( TauProfile )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_TauProfile( TauProfile ) ) RETURN


    ! -----------------------------------------
    ! Deallocate the TauProfile pointer members
    ! -----------------------------------------

    ! -- Level pressure
    IF ( ASSOCIATED( TauProfile%Level_Pressure ) ) THEN

      DEALLOCATE( TauProfile%Level_Pressure, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauProfile level pressure member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Channel list
    IF ( ASSOCIATED( TauProfile%Channel ) ) THEN

      DEALLOCATE( TauProfile%Channel, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauProfile Channel member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Angle list
    IF ( ASSOCIATED( TauProfile%Angle ) ) THEN

      DEALLOCATE( TauProfile%Angle, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauProfile Angle member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Profile list
    IF ( ASSOCIATED( TauProfile%Profile ) ) THEN

      DEALLOCATE( TauProfile%Profile, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauProfile Profile member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Molecule set list
    IF ( ASSOCIATED( TauProfile%Molecule_Set ) ) THEN

      DEALLOCATE( TauProfile%Molecule_Set, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauProfile Molecule_Set member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Transmitance
    IF ( ASSOCIATED( TauProfile%Tau ) ) THEN

      DEALLOCATE( TauProfile%Tau, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauProfile TAU member. ", &
                          &"STAT = ", i5 )' ) &
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

    TauProfile%n_Allocates = TauProfile%n_Allocates - 1

    IF ( TauProfile%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      TauProfile%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_TauProfile_scalar





  FUNCTION Destroy_TauProfile_rank1( TauProfile,   &  ! Output
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
    TYPE( TauProfile_type ), DIMENSION( : ), INTENT( IN OUT ) :: TauProfile

    ! -- Optional input
    INTEGER,                 OPTIONAL,       INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),          OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),          OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_TauProfile(rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

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
    !#                     -- PERFORM RE-INITIALISATION --                      #
    !#--------------------------------------------------------------------------#

    DO i = 1, SIZE( TauProfile )

      ! -- Clear the current structure array element
      Scalar_Status = Destroy_TauProfile_scalar( TauProfile(i), &
                                                 No_Clear = No_Clear, &
                                                 Message_Log = Message_Log )

      ! -- If it failed, set the return error status, but
      ! -- continue to attempt to destroy structure array
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( i10 )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying TauProfile structure array element '//&
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Destroy_TauProfile_rank1




!------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_TauProfile
! 
! PURPOSE:
!       Function to allocate the pointer members of the TauProfile
!       data structure.
!
! CATEGORY:
!       TauProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_TauProfile( n_Layers,                  &  ! Input
!                                           n_Channels,                &  ! Input
!                                           n_Angles,                  &  ! Input
!                                           n_Profiles,                &  ! Input
!                                           n_Molecule_Sets,           &  ! Input
!                                           TauProfile,                &  ! Output
!                                           RCS_Id      = RCS_Id,      &  ! Revision control
!                                           Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Layers:         Number of atmospheric layers dimension.
!                         Must be > 0.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       n_Channels:       Number of spectral channels dimension.
!                         Must be > 0.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       n_Angles:         Number of view angles dimension.
!                         Must be > 0.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       n_Profiles:       Number of atmospheric profiles dimension.
!                         Must be > 0.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       n_Molecule_Sets:  Number of molecular species/sets dimension.
!                         Must be > 0.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      None
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       TauProfile:       TauProfile structure with allocated
!                         pointer members
!                         UNITS:      N/A
!                         TYPE:       TauProfile_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:           Character string containing the Revision Control
!                         System Id field for the module.
!                         UNITS:      None
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the structure pointer allocations were successful
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
!       Associated_TauProfile:  Function to test the association status of the
!                               pointer members of a TauProfile structure.
!
!       Destroy_TauProfile:     Function to re-initialize the scalar and pointer
!                               members of TauProfile data structures.
!
!       Display_Message:        Subroutine to output messages
!                               SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output TauProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-May-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Allocate_TauProfile( n_Layers,        &  ! Input
                                n_Channels,      &  ! Input
                                n_Angles,        &  ! Input
                                n_Profiles,      &  ! Input
                                n_Molecule_Sets, &  ! Input

                                TauProfile,      &  ! Output

                                RCS_Id,          &  ! Revision control

                                Message_Log )    &  ! Error messaging

                              RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )     :: n_Layers
    INTEGER,                  INTENT( IN )     :: n_Channels
    INTEGER,                  INTENT( IN )     :: n_Angles
    INTEGER,                  INTENT( IN )     :: n_Profiles
    INTEGER,                  INTENT( IN )     :: n_Molecule_Sets

    ! -- Output
    TYPE( TauProfile_type ),  INTENT( IN OUT ) :: TauProfile

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_TauProfile'


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

    ! ----------
    ! Dimensions
    ! ----------

    IF ( n_Layers        < 1 .OR. &
         n_Channels      < 1 .OR. &
         n_Angles        < 1 .OR. &
         n_Profiles      < 1 .OR. &
         n_Molecule_Sets < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input TauProfile dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_TauProfile( TauProfile, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_TauProfile( TauProfile, &
                                         No_Clear = SET, &
                                         Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating TauProfile pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( TauProfile%Level_Pressure( n_Layers+1 ), &     
              TauProfile%Channel( n_Channels ), &     
              TauProfile%Angle( n_Angles ), &
              TauProfile%Profile( n_Profiles ), &
              TauProfile%Molecule_Set( n_Molecule_Sets ), &
              TauProfile%Tau( n_Layers, &
                              n_Channels, &
                              n_Angles, &
                              n_Profiles, &
                              n_Molecule_Sets ), & 

              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating TauProfile data arrays. STAT = ", i5 )' ) &
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

    TauProfile%n_Layers        = n_Layers
    TauProfile%n_Channels      = n_Channels
    TauProfile%n_Angles        = n_Angles
    TauProfile%n_Profiles      = n_Profiles
    TauProfile%n_Molecule_Sets = n_Molecule_Sets



    !#--------------------------------------------------------------------------#
    !#          -- FILL THE POINTER MEMBERS WITH INVALID VALUES --              #
    !#--------------------------------------------------------------------------#

    TauProfile%Level_Pressure = REAL( INVALID, fp_kind )
    TauProfile%Channel        = INVALID
    TauProfile%Angle          = REAL( INVALID, fp_kind )
    TauProfile%Profile        = INVALID
    TauProfile%Molecule_Set   = INVALID
    TauProfile%Tau            = REAL( INVALID, fp_kind )



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    TauProfile%n_Allocates = TauProfile%n_Allocates + 1

    IF ( TauProfile%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      TauProfile%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_TauProfile





!------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_TauProfile
!
! PURPOSE:
!       Function to copy valid TauProfile structures.
!
! CATEGORY:
!       TauProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_TauProfile( TauProfile_in,            &  ! Input
!                                         TauProfile_out,           &  ! Output
!                                         RCS_Id      = RCS_Id,     &  ! Revision control
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauProfile_in:  TauProfile structure which is to be copied.
!                       UNITS:      N/A
!                       TYPE:       TauProfile_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      None
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       TauProfile_out: Copy of the input structure, TauProfile_in.
!                       UNITS:      N/A
!                       TYPE:       TauProfile_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      None
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the structure assignment was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_TauProfile:  Function to test the association status of the
!                               pointer members of a TauProfile structure.
!
!       Allocate_TauProfile:    Function to allocate the pointer members of
!                               the TauProfile data structure.
!
!       Display_Message:        Subroutine to output messages
!                               SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output TauProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-May-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Assign_TauProfile( TauProfile_in,  &  ! Input
                              TauProfile_out, &  ! Output
                              RCS_Id,         &  ! Revision control
                              Message_Log )   &  ! Error messaging
                            RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauProfile_type ),  INTENT( IN )     :: TauProfile_in

    ! -- Output
    TYPE( TauProfile_type ),  INTENT( IN OUT ) :: TauProfile_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_TauProfile'



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

    IF ( .NOT. Associated_TauProfile( TauProfile_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauProfile pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! Assign scalar members
    ! ---------------------

    TauProfile_out%NCEP_Sensor_ID   = TauProfile_in%NCEP_Sensor_ID
    TauProfile_out%WMO_Satellite_ID = TauProfile_in%WMO_Satellite_ID
    TauProfile_out%WMO_Sensor_ID    = TauProfile_in%WMO_Sensor_ID


    ! -----------------
    ! Assign array data
    ! -----------------

    ! -- Allocate data arrays
    Error_Status = Allocate_TauProfile( TauProfile_in%n_Layers, &
                                        TauProfile_in%n_Channels, &
                                        TauProfile_in%n_Angles, &
                                        TauProfile_in%n_Profiles, &
                                        TauProfile_in%n_Molecule_Sets, &
                                        TauProfile_out, &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output TauProfile arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Copy array data
    TauProfile_out%Level_Pressure = TauProfile_in%Level_Pressure
    TauProfile_out%Channel        = TauProfile_in%Channel
    TauProfile_out%Angle          = TauProfile_in%Angle
    TauProfile_out%Profile        = TauProfile_in%Profile
    TauProfile_out%Molecule_Set   = TauProfile_in%Molecule_Set
    TauProfile_out%Tau            = TauProfile_in%Tau

  END FUNCTION Assign_TauProfile





!------------------------------------------------------------------------------
!S+
! NAME:
!       Concatenate_TauProfile
!
! PURPOSE:
!       Function to concatenate two valid TauProfile structures.
!
! CATEGORY:
!       TauProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Concatenate_TauProfile( TauProfile1,              &  ! Input/Output
!                                              TauProfile2,              &  ! Input
!                                              By_Profile  = By_Profile, &  ! Optional input
!                                              RCS_Id      = RCS_Id,     &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauProfile1:   First TauProfile structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       TauProfile_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
!       TauProfile2:   Second TauProfile structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       TauProfile_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       By_Profile:    Set this optional argument to indicate that the 
!                      structure concatenation is to be performed along
!                      the PROFILE dimension. If not specified, the default
!                      action is to concatenate the structures along the 
!                      MOLECULE_SET dimension.
!                      If == 0, or unspecified, concatenation is along
!                               the molecule set dimension **DEFAULT**
!                         == 1, concatenation along profile dimension.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      None
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       TauProfile1:   The concatenated TauProfile structure. The order of
!                      concatenation is TauProfile1,TauProfile2 along the 
!                      absorber dimension.
!                      UNITS:      N/A
!                      TYPE:       TauProfile_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      None
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the structure concatenation was successful
!                        == FAILURE an error occurred
!                        == WARNING the destruction of a temporary, local
!                                   TauProfile structure failed.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_TauProfile:   Function to test the association status of the
!                                pointer members of a TauProfile structure.
!
!       Assign_TauProfile:       Function to copy valid TauProfile data structures.
!
!       Destroy_TauProfile:      Function to re-initialize the scalar and pointer
!                                members of TauProfile data structures.
!
!       Allocate_TauProfile:     Function to allocate the pointer members of
!                                the TauProfile data structure.
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       The input TauProfile1 argument contains the concatenated structure
!       data (in character-speak: TauProfile1//TauProfile2) on output. It is
!       reallocated within this routine so if an error occurs during the
!       reallocation, the contents of the input TauProfile1 structure will
!       be lost.
!
!       Because of the structure reallocation there is a potential that 
!       available memory will become fragmented. Use this routine in a
!       manner that will minimise this effect (e.g. destroying structures or
!       allocatable arrays in the opposite order in which they were created). 
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Concatenate_TauProfile( TauProfile1,  &  ! Input/Output
                                   TauProfile2,  &  ! Input
                                   By_Profile,   &  ! Optional Input
                                   RCS_Id,       &  ! Revision control
                                   Message_Log ) &  ! Error messaging
                                 RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input/Output
    TYPE( TauProfile_type ),  INTENT( IN OUT )  :: TauProfile1
    TYPE( TauProfile_type ),  INTENT( IN )      :: TauProfile2

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )      :: By_Profile

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )     :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )      :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Concatenate_TauProfile'


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: By_Molecule_Set

    INTEGER :: n_Profiles,      m1, m2
    INTEGER :: n_Molecule_Sets, j1, j2

    TYPE( TauProfile_type ) :: TauProfile_Tmp



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#          -- CHECK CONCATENATION DIMENSION OPTIONAL ARGUMENT --           #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Concatentation is along the molecule
    ! set dimension by default...
    ! ------------------------------------

    By_Molecule_Set = .TRUE.


    ! ---------------------------------
    ! ...unless the BY_PROFILE optional
    ! argument is set
    ! ---------------------------------

    IF ( PRESENT( By_Profile ) ) THEN
      IF ( By_Profile == SET ) By_Molecule_Set = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! The first structure
    ! -------------------

    IF ( .NOT. Associated_TauProfile( TauProfile1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauProfile1 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------
    ! The second structure
    ! --------------------

    IF ( .NOT. Associated_TauProfile( TauProfile2 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauProfile2 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- CHECK THE INPUT STRUCTURE CONTENTS --                  #
    !#--------------------------------------------------------------------------#

    ! --------------------------------
    ! The non-concatenation dimensions
    ! --------------------------------

    IF ( TauProfile1%n_Layers   /= TauProfile2%n_Layers   .OR. &
         TauProfile1%n_Channels /= TauProfile2%n_Channels .OR. &
         TauProfile1%n_Angles   /= TauProfile2%n_Angles        ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'n_Layers, n_Channels, or n_Angles TauProfile dimensions are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------
    ! The concatenation dimensions
    ! ----------------------------

    IF ( By_Molecule_Set ) THEN

      IF ( TauProfile1%n_Profiles /= TauProfile2%n_Profiles ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'n_Profiles TauProfile dimensions are different.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    ELSE

      IF ( TauProfile1%n_Molecule_Sets /= TauProfile2%n_Molecule_Sets ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'n_Molecule_Sets TauProfile dimensions are different.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF


    ! -------
    ! The IDs
    ! -------

    IF ( TauProfile1%NCEP_Sensor_ID   /= TauProfile2%NCEP_Sensor_ID   .OR. &
         TauProfile1%WMO_Satellite_ID /= TauProfile2%WMO_Satellite_ID .OR. &
         TauProfile1%WMO_Sensor_ID    /= TauProfile2%WMO_Sensor_ID         ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TauProfile sensor ID values are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------------------
    ! The level pressure, channel, or angle values
    ! --------------------------------------------

    ! -- All the pressures must be the same
    IF ( ANY( .NOT. Compare_Float( TauProfile1%Level_Pressure, &
                                   TauProfile2%Level_Pressure  ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TauProfile level pressure values are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- All the channels numbers must be the same
    IF ( ANY( ( TauProfile1%Channel - TauProfile2%Channel ) /= 0 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TauProfile channel values are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- All the angle values must be the same
    IF ( ANY( .NOT. Compare_Float( TauProfile1%Angle, &
                                   TauProfile2%Angle  ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'TauProfile angle values are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------------
    ! The profile or molecule set values
    ! ----------------------------------

    IF ( By_Molecule_Set ) THEN

      ! -- All the molecule set IDs must be the same
      IF ( ANY( ( TauProfile1%Molecule_Set - TauProfile2%Molecule_Set ) /= 0 ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'TauProfile molecule set IDs are different.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    ELSE

      ! -- All the profile number values must be the same
      IF ( ANY( ( TauProfile1%Profile - TauProfile2%Profile ) /= 0 ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'TauProfile profile numbers are different.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                -- COPY FIRST INPUT TauProfile STRUCTURE --               #
    !#--------------------------------------------------------------------------#

    Error_Status = Assign_TauProfile( TauProfile1, TauProfile_Tmp, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error copying TauProfile1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
   


    !#--------------------------------------------------------------------------#
    !#             -- REALLOCATE FIRST INPUT TauProfile STRUCTURE --            #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Destroy it
    ! ----------

    Error_Status = Destroy_TauProfile( TauProfile1, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying TauProfile1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------
    ! Re-Allocate it
    ! --------------

    Reallocate_TauProfile1: IF ( By_Molecule_Set ) THEN

      ! -- Set the total number of molecule sets
      n_Molecule_Sets = TauProfile_Tmp%n_Molecule_Sets + TauProfile2%n_Molecule_Sets

      ! -- Perform the allocation
      Error_Status = Allocate_TauProfile( TauProfile_Tmp%n_Layers, &
                                          TauProfile_Tmp%n_Channels, &
                                          TauProfile_Tmp%n_Angles, &
                                          TauProfile_Tmp%n_Profiles, &
                                          n_Molecule_Sets, &
                                          TauProfile1, &
                                          Message_Log = Message_Log )

    ELSE ! Reallocate_TauProfile1

      ! -- Set the total number of profiles
      n_Profiles = TauProfile_Tmp%n_Profiles + TauProfile2%n_Profiles

      ! -- Perform the allocation
      Error_Status = Allocate_TauProfile( TauProfile_Tmp%n_Layers, &
                                          TauProfile_Tmp%n_Channels, &
                                          TauProfile_Tmp%n_Angles, &
                                          n_Profiles, &
                                          TauProfile_Tmp%n_Molecule_Sets, &
                                          TauProfile1, &
                                          Message_Log = Message_Log )

    END IF Reallocate_TauProfile1

    ! -- Check for errors
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reallocating TauProfile1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE CONCATENATION --                    #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------
    ! Assign the non-concatenation data
    ! ---------------------------------

    TauProfile1%NCEP_Sensor_ID   = TauProfile_Tmp%NCEP_Sensor_ID
    TauProfile1%WMO_Satellite_ID = TauProfile_Tmp%WMO_Satellite_ID
    TauProfile1%WMO_Sensor_ID    = TauProfile_Tmp%WMO_Sensor_ID

    TauProfile1%Level_Pressure = TauProfile_Tmp%Level_Pressure
    TauProfile1%Channel        = TauProfile_Tmp%Channel
    TauProfile1%Angle          = TauProfile_Tmp%Angle


    ! -----------------------------
    ! Concatenate the required bits
    ! -----------------------------

    Concatenate_TauProfile1: IF ( By_Molecule_Set ) THEN


      ! -----------------------------
      ! Concatenate the molecule sets
      ! -----------------------------

      ! -- Assign the profile numbers
      TauProfile1%Profile = TauProfile_Tmp%Profile

      ! -- The first part
      j1 = 1
      j2 = TauProfile_Tmp%n_Molecule_Sets

      TauProfile1%Molecule_Set(j1:j2) = TauProfile_Tmp%Molecule_Set
      TauProfile1%Tau(:,:,:,:,j1:j2)  = TauProfile_Tmp%Tau

      ! -- The second part
      j1 = j2 + 1
      j2 = n_Molecule_Sets

      TauProfile1%Molecule_Set(j1:j2) = TauProfile2%Molecule_Set
      TauProfile1%Tau(:,:,:,:,j1:j2)  = TauProfile2%Tau

    ELSE ! Concatenate_TauProfile1


      ! ------------------------
      ! Concatenate the profiles
      ! ------------------------

      ! -- Assign the molecule set ID values
      TauProfile1%Molecule_Set = TauProfile_Tmp%Molecule_Set

      ! -- The first part
      m1 = 1
      m2 = TauProfile_Tmp%n_Profiles

      TauProfile1%Profile(m1:m2)     = TauProfile_Tmp%Molecule_Set
      TauProfile1%Tau(:,:,:,m1:m2,:) = TauProfile_Tmp%Tau

      ! -- The second part
      m1 = m2 + 1
      m2 = n_Molecule_Sets

      TauProfile1%Profile(m1:m2)     = TauProfile2%Molecule_Set
      TauProfile1%Tau(:,:,:,m1:m2,:) = TauProfile2%Tau

    END IF Concatenate_TauProfile1



    !#--------------------------------------------------------------------------#
    !#            -- DEALLOCATE THE TEMPORARY TauProfile STRUCTURE --           #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_TauProfile( TauProfile_Tmp, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying TauProfile_Tmp structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Concatenate_TauProfile





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Information_TauProfile
!
! PURPOSE:
!       Subroutine to return a string containing information about the
!       TauProfile data structure.
!
! CATEGORY:
!       TauProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Information_TauProfile( TauProfile,     &  ! Input
!                                    Information,    &  ! Output
!                                    RCS_Id = RCS_Id )  ! Revision control
! 
! INPUT ARGUMENTS:
!       TauProfile:    Filled TauProfile structure.
!                      UNITS:      N/A
!                      TYPE:       TauProfile_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Information:   String containing information about the passed
!                      TauProfile data structure.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Information_TauProfile( TauProfile,  &  ! Input
                                     Information, &  ! Output
                                     RCS_Id       )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauProfile_type ),  INTENT( IN )  :: TauProfile

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

    WRITE( Long_String, '( a,1x,"TauProfile: ", &
                           &"N_LAYERS=",i3,2x,&
                           &"N_CHANNELS=",i4,2x,&
                           &"N_ANGLES=",i1,2x,&
                           &"N_PROFILES=",i3,2x,&
                           &"N_MOLECULE_SETS=",i2 )' ) &
                         ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                         TauProfile%n_Layers, &
                         TauProfile%n_Channels, &
                         TauProfile%n_Angles, &
                         TauProfile%n_Profiles, &
                         TauProfile%n_Molecule_Sets


    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Information = Long_String(1:MIN( LEN( Information ), LEN_TRIM( Long_String ) ))

  END SUBROUTINE Information_TauProfile

END MODULE TauProfile_Define_old


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/06/30 16:47:16 $
!
! $Revision$
!
! $Name:  $
!
! $State: Exp $
!
! $Log: TauProfile_Define.f90,v $
! Revision 1.11  2006/06/30 16:47:16  dgroff
! Changed "Error_Handler" references to "Message_Handler"
!
! Revision 1.10  2004/09/14 17:19:58  paulv
! - Upgraded to Fortran95.
! - Derived type component initialisation is now done in the defintion block.
! - Init_TauProfile() subroutine has been removed.
! - Intent of TauProfile dummy argument in Clear_TauProfile() routine changed from
!   OUT to IN OUT to prevent memory leaks.
! - Added optional No_Clear argument to Destroy_TauProfile() function.
! - Intent of TauProfile dummy argument in Allocate_TauProfile() routine changed from
!   OUT to IN OUT to prevent memory leaks.
! - Call to Destroy_TauProfile() added to Allocate_TauProfile() function for the case
!   where the input TauProfile argument is already allocated.
! - Intent of TauProfile_out dummy argument in Assign_TauProfile() routine changed from
!   OUT to IN OUT to prevent memory leaks.
! - Added association test in destroy function. If no pointer components
!   are associated, the function returns without modifying the n_Allocates
!   counter. Thus, calling the destroy function on an already destroyed
!   structure want flag an error.
! - Updated header documentation.
!
! Revision 1.9  2003/12/01 15:06:13  paulv
! - Correct variable usage bug in rank1 destruction function.
!
! Revision 1.8  2003/11/25 22:29:24  paulv
! - Altered Clear() subroutine to reset dimension members to 0.
! - Added Association() function call to Destroy() function.
! - Updated header documentation.
!
! Revision 1.7  2003/11/24 19:48:29  paulv
! - Updated header documentation.
!
! Revision 1.6  2003/07/16 20:06:49  paulv
! - Corrected a number of spelling errors.
!
! Revision 1.5  2003/06/30 00:30:05  paulv
! - New versions for updated structure definition and netCDF I/O. Untested.
!
! Revision 1.4  2003/06/20 22:01:21  paulv
! - Updating software for use with RTM statistics code. Incomplete.
!
! Revision 1.3  2002/06/25 21:37:58  paulv
! - Modified the Destroy() function to deallocate if a pointer member
!   is associated. Previously if any pointer members *weren't* allocated
!   an error was generated. What was the go with that? Very silly.
!
! Revision 1.2  2002/06/05 19:14:17  paulv
! - Removed MESSAGE as a module variable and placed definitions in each
!   module subprogram.
! - Removed INQUIRE/GET/SET functions. Made TauProfile members public.
!
! Revision 1.1  2002/05/29 17:43:46  paulv
! Initial checkin. Untested.
!
!
!
!
