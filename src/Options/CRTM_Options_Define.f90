!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Options_Define
!
! PURPOSE:
!       Module defining the CRTM Options optional argument data structure
!       and containing routines to manipulate it.
!       
! CATEGORY:
!       CRTM : Options
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_Options_Define
!
! MODULES:
!       Type_Kinds:          Module containing definitions for kinds
!                            of variable types.
!
!       Message_Handler:     Module to define simple error codes and
!                            handle error conditions
!                            USEs: FILE_UTILITY module
!
!       CRTM_Parameters:     Module of parameter definitions for the CRTM.
!                            USEs: TYPE_KINDS module
!
! CONTAINS:
!       CRTM_Associated_Options:  Function to test the association status
!                                 of the pointer members of a Options
!                                 structure.
!
!       CRTM_Destroy_Options:     Function to re-initialize a Options
!                                 structure.
!
!       CRTM_Allocate_Options:    Function to allocate the pointer members
!                                 of an Options structure.
!
!       CRTM_Assign_Options:      Function to copy a valid Options
!                                 structure.
!
! DERIVED TYPES:
!       CRTM_Options_type
!       ---------------------
!         Definition of the public CRTM_Options data structure.
!         Fields are,
!
!         n_Channels:                  The number of channels used to specify the
!                                      input spectral data. This value must agree
!                                      with the number of channels used to define
!                                      other mandatory spectral CRTM inputs.
!                                      UNITS:      N/A
!                                      TYPE:       INTEGER
!                                      DIMENSION:  Scalar
!
!         Emissivity_Switch:           An integer switch value to indicate emissivity
!                                      spectrum input.
!                                      If == 0, surface emissivity spectrum is
!                                               computed internally. [**DEFAULT**]
!                                         == 1, surface emissivity spectrum is
!                                               supplied by the user in the CRTM_Options
!                                               structure Emissivity component (see
!                                               below). 
!                                      UNITS:      N/A
!                                      TYPE:       INTEGER
!                                      DIMENSION:  Scalar
!
!         Emissivity:                  Arry to hold the user-supplied INPUT emissivity
!                                      spectrum for the forward model.
!                                      UNITS:      N/A
!                                      TYPE:       REAL( fp_kind )
!                                      DIMENSION:  Rank-1 (n_Channels)
!                                      ATTRIBUTES: POINTER
!
!         Direct_Reflectivity_Switch:  An integer switch value to indicate direct
!                                      refelctivity spectrum input. 
!                                      NOTE: Direct reflectivities are accepted
!                                            ONLY when the emissivity switch (see
!                                            above) is also set.
!                                      If == 0, direct reflectivity spectrum used
!                                               is (1-emissivity). [**DEFAULT**]
!                                         == 1, direct reflectivity spectrum is
!                                               supplied by the user in the CRTM_Options
!                                               structure Direct_Reflectivity component
!                                               see below). 
!                                      UNITS:      N/A
!                                      TYPE:       INTEGER
!                                      DIMENSION:  Scalar
!
!         Direct_Reflectivity:         Array to hold the user-supplied INPUT direct
!                                      reflectivity spectrum for the forward model.
!                                      UNITS:      N/A
!                                      TYPE:       REAL( fp_kind )
!                                      DIMENSION:  Rank-1 (n_Channels)
!                                      ATTRIBUTES: POINTER
!
!       *!IMPORTANT!*
!       -------------
!       Note that the CRTM_Options_type is PUBLIC and its members are
!       not encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user destroy,
!       allocate, and assign the structure using only the routines
!       in this module where possible to eliminate -- or at least
!       minimise -- the possibility of memory leakage since most
!       of the structure members are pointers.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Sep-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2005 Paul van Delst
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

MODULE CRTM_Options_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler
  USE CRTM_Parameters, ONLY: SET, NOT_SET, ZERO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Definition functions
  PUBLIC :: CRTM_Associated_Options
  PUBLIC :: CRTM_Destroy_Options
  PUBLIC :: CRTM_Allocate_Options
  PUBLIC :: CRTM_Assign_Options


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE CRTM_Destroy_Options
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Scalar_Multi
    MODULE PROCEDURE Destroy_Rank1
    MODULE PROCEDURE Destroy_Rank1_Multi
  END INTERFACE CRTM_Destroy_Options

  INTERFACE CRTM_Allocate_Options
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank1
  END INTERFACE CRTM_Allocate_Options

  INTERFACE CRTM_Assign_Options
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE CRTM_Assign_Options


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Options_Define.f90,v 1.5 2006/05/25 19:35:52 wd20pd Exp $'



  ! ----------------------------
  ! Options data type definition
  ! ----------------------------

  TYPE, PUBLIC :: CRTM_Options_type
    INTEGER :: n_Allocates = 0

    ! -- Dimensions
    INTEGER :: n_Channels = 0  ! L dimension

    ! -- Index into channel-specific components
    INTEGER :: Channel = 0

    ! -- Emissivity optional arguments
    INTEGER                                  :: Emissivity_Switch =  NOT_SET
    REAL( fp_kind ), DIMENSION( : ), POINTER :: Emissivity        => NULL() ! L

    ! -- Direct reflectivity optional arguments
    INTEGER                                  :: Direct_Reflectivity_Switch =  NOT_SET
    REAL( fp_kind ), DIMENSION( : ), POINTER :: Direct_Reflectivity        => NULL() ! L

  END TYPE CRTM_Options_type


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
!       CRTM_Clear_Options
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM Options structure.
!
! CATEGORY:
!       CRTM : Options
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_Options( Options ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Options:      Options structure for which the scalar members have
!                     been cleared.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Options_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
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
!       Note the INTENT on the output Options argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Sep-2005
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_Options( Options )

    TYPE( CRTM_Options_type ), INTENT( IN OUT ) :: Options

    Options%n_Channels = 0
    Options%Emissivity_Switch = NOT_SET
    Options%Direct_Reflectivity_Switch = NOT_SET

  END SUBROUTINE CRTM_Clear_Options





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
!       CRTM_Associated_Options
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       CRTM_Options structure.
!
! CATEGORY:
!       CRTM : Options
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_Options( Options,            &  ! Input
!                                                     ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       Options:     Options structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       CRTM_Options_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                    Options structure pointer members are associated.
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
!                            association status of the Options pointer
!                            members.
!                            .TRUE.  - if ALL the Options pointer members
!                                      are associated, or if the ANY_Test argument
!                                      is set and ANY of the Options
!                                      pointer members are associated.
!                            .FALSE. - some or all of the Options pointer
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
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Sep-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_Options( Options,   & ! Input
                                    ANY_Test ) & ! Optional input
                                  RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Options_type ), INTENT( IN ) :: Options

    ! -- Optional input
    INTEGER,         OPTIONAL, INTENT( IN ) :: ANY_Test


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

      IF ( ASSOCIATED( Options%Emissivity          ) .AND. &
           ASSOCIATED( Options%Direct_Reflectivity )       ) THEN 
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( Options%Emissivity          ) .OR. &
           ASSOCIATED( Options%Direct_Reflectivity )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION CRTM_Associated_Options





!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy_Options
! 
! PURPOSE:
!       Function to re-initialize CRTM_Options data structures.
!
! CATEGORY:
!       CRTM : Options
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_Options( Options1, [ Options2, ..., Options10, ] &  ! Output
!                                            RCS_Id      = RCS_Id,                   &  ! Revision control
!                                            Message_Log = Message_Log               )  ! Error messaging
! 
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     Messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output Messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Options1, [ Options2, ..., Options10 ]:
!                     Re-initialized Options structure(s). At least one
!                     structure or structure array must be specified, and
!                     no more than 10 structures or structure arrays must
!                     be specified.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Options_type
!                     DIMENSION:  Scalar
!                                   OR
!                                 Rank-1 array
!                     ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
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
!       CRTM_Clear_Options:           Subroutine to clear the scalar members
!                                     of a CRTM Options structure.
!
!       CRTM_Associated_Options:      Function to test the association status
!                                     of the pointer members of a CRTM_Options
!                                     structure.
!
!       Display_Message:              Subroutine to output messages
!                                     SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!       
! COMMENTS:
!       Note the INTENT on the output Options argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Sep-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_Scalar( Options,      &  ! Output
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
    TYPE( CRTM_Options_type ), INTENT( IN OUT ) :: Options

    ! -- Optional input
    INTEGER,         OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Options(Scalar)'


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

    IF ( Clear ) CALL CRTM_Clear_Options( Options )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. CRTM_Associated_Options( Options ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the Emissivity member
    IF ( ASSOCIATED( Options%Emissivity ) ) THEN

      DEALLOCATE( Options%Emissivity, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Options Emissivity ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Direct_Reflectivity member
    IF ( ASSOCIATED( Options%Direct_Reflectivity ) ) THEN

      DEALLOCATE( Options%Direct_Reflectivity, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Options Direct_Reflectivity ", &
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

    Options%n_Allocates = Options%n_Allocates - 1

    IF ( Options%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      Options%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Scalar



  FUNCTION Destroy_Scalar_Multi( Options1,     &  ! Output
                                 Options2,     &  ! Output
                                 Options3,     &  ! Optional Output
                                 Options4,     &  ! Optional Output
                                 Options5,     &  ! Optional Output
                                 Options6,     &  ! Optional Output
                                 Options7,     &  ! Optional Output
                                 Options8,     &  ! Optional Output
                                 Options9,     &  ! Optional Output
                                 Options10,    &  ! Optional Output
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
    TYPE( CRTM_Options_type ),           INTENT( IN OUT ) :: Options1
    TYPE( CRTM_Options_type ),           INTENT( IN OUT ) :: Options2

    ! -- Optional output
    TYPE( CRTM_Options_type ), OPTIONAL, INTENT( IN OUT ) :: Options3
    TYPE( CRTM_Options_type ), OPTIONAL, INTENT( IN OUT ) :: Options4
    TYPE( CRTM_Options_type ), OPTIONAL, INTENT( IN OUT ) :: Options5
    TYPE( CRTM_Options_type ), OPTIONAL, INTENT( IN OUT ) :: Options6
    TYPE( CRTM_Options_type ), OPTIONAL, INTENT( IN OUT ) :: Options7
    TYPE( CRTM_Options_type ), OPTIONAL, INTENT( IN OUT ) :: Options8
    TYPE( CRTM_Options_type ), OPTIONAL, INTENT( IN OUT ) :: Options9
    TYPE( CRTM_Options_type ), OPTIONAL, INTENT( IN OUT ) :: Options10

    ! -- Optional input
    INTEGER,                   OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Options(Scalar,Multi)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: Destroy_Status



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

    ! -----------------------
    ! The mandatory arguments
    ! -----------------------

    Destroy_Status = Destroy_Scalar( Options1, &
                                     No_Clear = No_Clear, &
                                     Message_Log = Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying first Options structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    Destroy_Status = Destroy_Scalar( Options2, &
                                     No_Clear = No_Clear, &
                                     Message_Log = Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying second Options structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ----------------------
    ! The optional arguments
    ! ----------------------

    IF ( PRESENT( Options3 ) ) THEN
      Destroy_Status = Destroy_Scalar( Options3, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying third Options structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Options4 ) ) THEN
      Destroy_Status = Destroy_Scalar( Options4, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying fourth Options structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Options5 ) ) THEN
      Destroy_Status = Destroy_Scalar( Options5, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying fifth Options structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Options6 ) ) THEN
      Destroy_Status = Destroy_Scalar( Options6, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying sixth Options structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Options7 ) ) THEN
      Destroy_Status = Destroy_Scalar( Options7, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying seventh Options structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Options8 ) ) THEN
      Destroy_Status = Destroy_Scalar( Options8, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying eighth Options structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Options9 ) ) THEN
      Destroy_Status = Destroy_Scalar( Options9, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying ninth Options structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Options10 ) ) THEN
      Destroy_Status = Destroy_Scalar( Options10, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying tenth Options structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF

  END FUNCTION Destroy_Scalar_Multi


  FUNCTION Destroy_Rank1( Options,      &  ! Output
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
    TYPE( CRTM_Options_type ), DIMENSION( : ), INTENT( IN OUT ) :: Options

    ! -- Optional input
    INTEGER,                   OPTIONAL,       INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Options(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

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

    DO n = 1, SIZE( Options )

      Scalar_Status = Destroy_Scalar( Options(n), &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i5, &
                          &" of CRTM_Options structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Destroy_Rank1

  FUNCTION Destroy_Rank1_Multi( Options1,     &  ! Output
                                Options2,     &  ! Output
                                Options3,     &  ! Optional Output
                                Options4,     &  ! Optional Output
                                Options5,     &  ! Optional Output
                                Options6,     &  ! Optional Output
                                Options7,     &  ! Optional Output
                                Options8,     &  ! Optional Output
                                Options9,     &  ! Optional Output
                                Options10,    &  ! Optional Output
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
    TYPE( CRTM_Options_type ),           DIMENSION( : ), INTENT( IN OUT ) :: Options1
    TYPE( CRTM_Options_type ),           DIMENSION( : ), INTENT( IN OUT ) :: Options2

    ! -- Optional output
    TYPE( CRTM_Options_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Options3
    TYPE( CRTM_Options_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Options4
    TYPE( CRTM_Options_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Options5
    TYPE( CRTM_Options_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Options6
    TYPE( CRTM_Options_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Options7
    TYPE( CRTM_Options_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Options8
    TYPE( CRTM_Options_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Options9
    TYPE( CRTM_Options_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Options10

    ! -- Optional input
    INTEGER,                   OPTIONAL,                 INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,                 INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL,                 INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Options(Rank-1,Multi)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: Destroy_Status



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

    ! -----------------------
    ! The mandatory arguments
    ! -----------------------

    Destroy_Status = Destroy_Rank1( Options1, &
                                    No_Clear = No_Clear, &
                                    Message_Log = Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying first Options structure array.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    Destroy_Status = Destroy_Rank1( Options2, &
                                    No_Clear = No_Clear, &
                                    Message_Log = Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying second Options structure array.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ----------------------
    ! The optional arguments
    ! ----------------------

    IF ( PRESENT( Options3 ) ) THEN
      Destroy_Status = Destroy_Rank1( Options3, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying third Options structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Options4 ) ) THEN
      Destroy_Status = Destroy_Rank1( Options4, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying fourth Options structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Options5 ) ) THEN
      Destroy_Status = Destroy_Rank1( Options5, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying fifth Options structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Options6 ) ) THEN
      Destroy_Status = Destroy_Rank1( Options6, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying sixth Options structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Options7 ) ) THEN
      Destroy_Status = Destroy_Rank1( Options7, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying seventh Options structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Options8 ) ) THEN
      Destroy_Status = Destroy_Rank1( Options8, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying eighth Options structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Options9 ) ) THEN
      Destroy_Status = Destroy_Rank1( Options9, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying ninth Options structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Options10 ) ) THEN
      Destroy_Status = Destroy_Rank1( Options10, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying tenth Options structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF

  END FUNCTION Destroy_Rank1_Multi




!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Allocate_Options
! 
! PURPOSE:
!       Function to allocate the pointer members of a CRTM Options
!       data structure.
!
! CATEGORY:
!       CRTM : Options
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_Options( n_Channels,               &  ! Input
!                                             Options,                  &  ! Output
!                                             RCS_Id = RCS_Id,          &  ! Revision control
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Channels:          The number of channels used to specify the
!                            input spectral data. This value must agree
!                            with the number of channels used to define
!                            other mandatory spectral CRTM inputs.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:         Character string specifying a filename in which any
!                            Messages will be logged. If not specified, or if an
!                            error occurs opening the log file, the default action
!                            is to output Messages to standard output.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER( * )
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Options:             Options structure with allocated pointer members. The
!                            rank-1 case is for handling separate Options structures
!                            for different profiles. Note that each element of the 
!                            rank-1 case is allocated to the same number of channels.
!                            UNITS:      N/A
!                            TYPE:       CRTM_Options_type
!                            DIMENSION:  Scalar
!                                          OR
!                                        Rank-1
!                            ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:              Character string containing the Revision Control
!                            System Id field for the module.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:        The return value is an integer defining the error status.
!                            The error codes are defined in the ERROR_HANDLER module.
!                            If == SUCCESS the structure pointer allocations were
!                                          successful
!                               == FAILURE - an error occurred, or
!                                          - the structure internal allocation counter
!                                            is not equal to one (1) upon exiting this
!                                            function. This value is incremented and
!                                            decremented for every structure allocation
!                                            and deallocation respectively.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Associated_Options:  Function to test the association status
!                                 of the pointer members of a CRTM
!                                 Options structure.
!
!       CRTM_Destroy_Options:     Function to re-initialize the scalar and
!                                 pointer members of a CRTM Options
!                                 data structure.
!
!       Display_Message:          Subroutine to output messages
!                                 SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!       
! COMMENTS:
!       Note the INTENT on the output Options argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Sep-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Allocate_Scalar( n_Channels,   &  ! Input
                            Options,      &  ! Output
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
    INTEGER,                   INTENT( IN )     :: n_Channels

    ! -- Output
    TYPE( CRTM_Options_type ), INTENT( IN OUT ) :: Options

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Options(Scalar)'


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

    ! --------------
    ! The dimensions
    ! --------------

    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Channels must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( CRTM_Associated_Options( Options, ANY_Test = SET ) ) THEN

      Error_Status = CRTM_Destroy_Options( Options, &
                                           No_Clear = SET, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTM_Options pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( Options%Emissivity( n_Channels ), &
              Options%Direct_Reflectivity( n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating CRTM_Options data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- ASSIGN THE DIMENSIONS AND INITALISE ARRAYS --             #
    !#--------------------------------------------------------------------------#

    Options%n_Channels = n_Channels

    Options%Emissivity = ZERO
    Options%Direct_Reflectivity = ZERO



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    Options%n_Allocates = Options%n_Allocates + 1

    IF ( Options%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      Options%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Scalar


  FUNCTION Allocate_Rank1( n_Channels,   &  ! Input
                           Options,   &  ! Output, M
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
    INTEGER,                                   INTENT( IN )     :: n_Channels

    ! -- Output
    TYPE( CRTM_Options_type ), DIMENSION( : ), INTENT( IN OUT ) :: Options

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Options(Rank-1)'


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
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, SIZE( Options )

      Scalar_Status = Allocate_Scalar( n_Channels,    & ! Input
                                       Options(i), & ! Output
                                       Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Options structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank1





!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Assign_Options
!
! PURPOSE:
!       Function to copy valid CRTM Options structures.
!
! CATEGORY:
!       CRTM : Options
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_Options( Options_in,               &  ! Input
!                                           Options_out,              &  ! Output
!                                           RCS_Id = RCS_Id,          &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Options_in:      Options structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Options_type
!                        DIMENSION:  Scalar
!                                      OR
!                                    Rank-1
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        Messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output Messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Options_out:     Copy of the input structure, Options_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Options_type
!                        DIMENSION:  Same as Options_in
!                        ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the structure assignment was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Associated_Options:  Function to test the association status
!                                 of the pointer members of a CRTM
!                                 Options structure.
!
!
!       CRTM_Destroy_Options:     Function to re-initialize the scalar and
!                                 pointer members of a CRTM Options
!                                 data structure.
!
!       CRTM_Allocate_Options:    Function to allocate the pointer members
!                                 of a CRTM Options structure.
!
!       Display_Message:          Subroutine to output Messages
!                                 SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output Options argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Sep-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Assign_Scalar( Options_in,   &  ! Input
                          Options_out,  &  ! Output
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
    TYPE( CRTM_Options_type ), INTENT( IN )     :: Options_in

    ! -- Output
    TYPE( CRTM_Options_type ), INTENT( IN OUT ) :: Options_out

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Options(Scalar)'



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

    ! ----------------------------------------------
    ! ALL *input* pointers must be associated.
    !
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    ! ----------------------------------------------

    IF ( .NOT. CRTM_Associated_Options( Options_In ) ) THEN

      Error_Status = CRTM_Destroy_Options( Options_Out, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output CRTM_Options pointer members.', &
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

    Error_Status = CRTM_Allocate_Options( Options_in%n_Channels, &
                                          Options_out, &
                                          Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output CRTM_Options arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------
    ! Assign scalar data
    ! ------------------

    Options_out%Emissivity_Switch          = Options_in%Emissivity_Switch
    Options_out%Direct_Reflectivity_Switch = Options_in%Direct_Reflectivity_Switch


    ! -----------------
    ! Assign array data
    ! -----------------

    Options_out%Emissivity          = Options_in%Emissivity
    Options_out%Direct_Reflectivity = Options_in%Direct_Reflectivity

  END FUNCTION Assign_Scalar


  FUNCTION Assign_Rank1( Options_in,   &  ! Input
                         Options_out,  &  ! Output
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
    TYPE( CRTM_Options_type ), DIMENSION( : ), INTENT( IN )     :: Options_in

    ! -- Output
    TYPE( CRTM_Options_type ), DIMENSION( : ), INTENT( IN OUT ) :: Options_out

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Options(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Scalar_Status
    INTEGER :: i, n



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

    n = SIZE( Options_in )

    IF ( SIZE( Options_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Options_in and Options_out arrays'//&
                            ' have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Assign_Scalar( Options_in(i), &
                                     Options_out(i), &
                                     Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i5, &
                          &" of CRTM_Options structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Assign_Rank1

END MODULE CRTM_Options_Define


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_Options_Define.f90,v 1.5 2006/05/25 19:35:52 wd20pd Exp $
!
! $Date: 2006/05/25 19:35:52 $
!
! $Revision: 1.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_Options_Define.f90,v $
! Revision 1.5  2006/05/25 19:35:52  wd20pd
! - ONLY clauses now used with USE statements to minimise errors for a common
!   compiler bug.
! - Removed redundant parameter definitions.
!
! Revision 1.4  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.3  2005/10/19 21:46:22  paulv
! - Updated header documentation to indicate Options use is for the forward
!   model only.
!
! Revision 1.2  2005/10/14 21:59:22  paulv
! - Overloaded the Destroy() function to handle scalar, rank-1, multi-scalar
!   (up to 10) and multi-rank-1 (up to 10) Options arguments.
! - Overloaded the Allocate() function to handle scalar and rank-1 arguments.
! - Overloaded the Assign() function to handle scalar and rank-1 arguments.
!
! Revision 1.1  2005/10/04 20:25:05  paulv
! Initial checkin.
!
!
!
!
