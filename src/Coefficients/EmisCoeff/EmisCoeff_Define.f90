!------------------------------------------------------------------------------
!M+
! NAME:
!       EmisCoeff_Define
!
! PURPOSE:
!       Module defining the Spectral EmisCoeff data structure and containing
!       routines to manipulate it.
!       
! CATEGORY:
!       CRTM : Coefficients : EmisCoeff : Spectral Emissivity Model
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE EmisCoeff_Define
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Message_Handler:        Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       Compare_Float_Numbers:  Module containing routines to perform equality
!                               check comparisons on input floating point
!                               numbers.
!                               USEs: TYPE_KINDS module
!
! CONTAINS:
!       Associated_EmisCoeff:     Function to test the association status
!                                 of the pointer members of a EmisCoeff
!                                 structure.
!
!       Destroy_EmisCoeff:        Function to re-initialize a EmisCoeff
!                                 structure.
!
!       Allocate_EmisCoeff:       Function to allocate the pointer members
!                                 of a EmisCoeff structure.
!
!       Assign_EmisCoeff:         Function to copy a valid EmisCoeff structure.
!
!       Equal_EmisCoeff:          Function to test if two EmisCoeff
!                                 structures are equal.
!
!       Check_EmisCoeff_Release:  Function to check the EmisCoeff Release value.
!
!       Version_EmisCoeff:        Subroutine to return a string containing
!                                 version and dimension information about
!                                 the EmisCoeff data structure.
!
! DERIVED TYPES:
!       EmisCoeff_type:   Definition of the public EmisCoeff data structure. Fields
!                         are...
!
!         Release:        Data file release number.
!                         UNITS:      N/A
!                         TYPE:       INTEGER( Long )
!                         DIMENSION:  Scalar
!
!         Version:        Data file version number.
!                         UNITS:      N/A
!                         TYPE:       INTEGER( Long )
!                         DIMENSION:  Scalar
!
!         Data_Type:      Flag to indicate if this EmisCoeff data
!                         is for the spectral or sensor emissivity
!                         model.
!                         UNITS:      N/A
!                         TYPE:       INTEGER( Long )
!                         DIMENSION:  Scalar
!
!         n_Angles:       Size of the angle dimension of the 
!                         emissivity data.
!                         UNITS:      N/A
!                         TYPE:       INTEGER( Long )
!                         DIMENSION:  Scalar
!
!         n_Frequencies:  Size of the frequency dimension of the 
!                         emissivity data.
!                         UNITS:      N/A
!                         TYPE:       INTEGER( Long )
!                         DIMENSION:  Scalar
!
!         n_Wind_Speeds:  Size of the wind speed dimension of the 
!                         emissivity data.
!                         UNITS:      N/A
!                         TYPE:       INTEGER( Long )
!                         DIMENSION:  Scalar
!
!         Angle:          Angles ordinate data.
!                         UNITS:      Degrees (from vertical)
!                         TYPE:       REAL( Double )
!                         DIMENSION:  Rank-1 (n_Angles)
!                         ATTRIBUTES: POINTER
!
!         Frequency:      Frequency ordinate data.
!                         UNITS:      Inverse centimetres (cm^-1)
!                         TYPE:       REAL( Double )
!                         DIMENSION:  Rank-1 (n_Frequencies)
!                         ATTRIBUTES: POINTER
!
!         Wind_Speed:     Surface wind speed ordinate data.
!                         UNITS:      metres/second (m.s^-1)
!                         TYPE:       REAL( Double )
!                         DIMENSION:  Rank-1 (n_Wind_Speeds)
!                         ATTRIBUTES: POINTER
!
!         Emissivity:     Sea surface emissivity.
!                         UNITS:      N/A
!                         TYPE:       REAL( Double )
!                         DIMENSION:  Rank-3 (n_Angles x n_Frequencies x n_Wind_Speeds)
!                         ATTRIBUTES: POINTER
!
!       *!IMPORTANT!*
!       -------------
!       Note that the EmisCoeff_type is PUBLIC and its members are not
!       encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user destroy,
!       allocate, assign, and concatenate the structure using only
!       the routines in this module where possible to eliminate --
!       or at least minimise -- the possibility of memory leakage
!       since most of the structure members are pointers.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 13-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2003 Paul van Delst
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

MODULE EmisCoeff_Define


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

  ! -- Everything private by default
  PRIVATE

  ! -- Public procedures to manipulate the EmisCoeff structure
  PUBLIC :: Associated_EmisCoeff
  PUBLIC :: Destroy_EmisCoeff
  PUBLIC :: Allocate_EmisCoeff
  PUBLIC :: Assign_EmisCoeff
  PUBLIC :: Equal_EmisCoeff
  PUBLIC :: Check_EmisCoeff_Release
  PUBLIC :: Version_EmisCoeff




  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: EmisCoeff_Define.f90,v 2.1 2006/05/02 14:58:34 dgroff Exp $'

  ! -- EmisCoeff invalid values
  INTEGER,        PRIVATE, PARAMETER ::    INVALID = -1
  REAL( Double ), PRIVATE, PARAMETER :: FP_INVALID = -1.0_Double

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Current valid release and version numbers
  INTEGER, PRIVATE, PARAMETER :: EMISCOEFF_RELEASE = 2  ! This determines structure and file formats.
  INTEGER, PRIVATE, PARAMETER :: EMISCOEFF_VERSION = 1  ! This is just the data version.


  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! -- The EmisCoeff types
  INTEGER( Long ), PUBLIC, PARAMETER :: SPECTRAL_EMISCOEFF_TYPE = 1
  INTEGER( Long ), PUBLIC, PARAMETER ::   SENSOR_EMISCOEFF_TYPE = 2

  ! -- Number of EmisCoeff data items
  INTEGER( Long ), PUBLIC, PARAMETER :: N_EMISCOEFF_ITEMS = 4_Long

  ! -- Internal data type descriptors for the EmisCoeff data
  !    5 = Double (i.e. 8-byte float)
  !    4 = Single (i.e. 4-byte float)
  !    3 = Long   (i.e. 4-byte integer)
  INTEGER( Long ), PRIVATE, PARAMETER ::      LONG_TYPE = 3_Long
  INTEGER( Long ), PRIVATE, PARAMETER ::    SINGLE_TYPE = 4_Long
  INTEGER( Long ), PRIVATE, PARAMETER ::    DOUBLE_TYPE = 5_Long
  INTEGER( Long ), PRIVATE, PARAMETER :: CHARACTER_TYPE = 7_Long

  INTEGER( Long ), PUBLIC, PARAMETER, &
                   DIMENSION( N_EMISCOEFF_ITEMS ) :: EMISCOEFF_DATA_TYPE = &
                                                       (/ DOUBLE_TYPE, &  ! Angle
                                                          DOUBLE_TYPE, &  ! Frequency
                                                          DOUBLE_TYPE, &  ! Wind speed
                                                          DOUBLE_TYPE /)  ! Emissivity

  ! -- Names of the data items (for error processing)
  CHARACTER( * ), PUBLIC, PARAMETER, &
                  DIMENSION( N_EMISCOEFF_ITEMS ) :: EMISCOEFF_DATA_NAME = &
                                                      (/ 'Angle     ', &
                                                         'Frequency ', &
                                                         'Wind speed', &
                                                         'Emissivity' /)


  ! ------------------------------
  ! EmisCoeff data type definition
  ! ------------------------------

  TYPE, PUBLIC :: EmisCoeff_type
    INTEGER :: n_Allocates = 0

    ! -- Version info
    INTEGER( Long ) :: Release = EMISCOEFF_RELEASE 
    INTEGER( Long ) :: Version = EMISCOEFF_VERSION 

    ! -- Data type
    INTEGER( Long ) :: Data_Type = SPECTRAL_EMISCOEFF_TYPE

    ! -- The dimensions
    INTEGER( Long ) :: n_Angles      = 0   ! I dimension
    INTEGER( Long ) :: n_Frequencies = 0   ! L dimension
    INTEGER( Long ) :: n_Wind_Speeds = 0   ! N dimension

    ! -- The dimension ordinate data
    REAL( Double ), POINTER, DIMENSION( : )       :: Angle      => NULL()  ! I
    REAL( Double ), POINTER, DIMENSION( : )       :: Frequency  => NULL()  ! L
    REAL( Double ), POINTER, DIMENSION( : )       :: Wind_Speed => NULL()  ! N

    ! -- The spectral emissivity
    REAL( Double ), POINTER, DIMENSION( :, :, : ) :: Emissivity => NULL()  ! I x L x N

  END TYPE EmisCoeff_type


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
!       Clear_EmisCoeff
!
! PURPOSE:
!       Subroutine to clear the scalar members of a EmisCoeff structure.
!
! CATEGORY:
!       CRTM : Coefficients : EmisCoeff : Spectral Emissivity Model
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_EmisCoeff( EmisCoeff ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       EmisCoeff:   EmisCoeff structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       EmisCoeff_type
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
!       Note the INTENT on the output EmisCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_EmisCoeff( EmisCoeff )

    TYPE( EmisCoeff_type ), INTENT( IN OUT ) :: EmisCoeff

    EmisCoeff%n_Angles      = 0
    EmisCoeff%n_Frequencies = 0
    EmisCoeff%n_Wind_Speeds = 0

  END SUBROUTINE Clear_EmisCoeff







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
!       Associated_EmisCoeff
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       EmisCoeff structure.
!
! CATEGORY:
!       CRTM : Coefficients : EmisCoeff : Spectral Emissivity Model
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_EmisCoeff( EmisCoeff,          &  ! Input
!                                                  ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       EmisCoeff:   EmisCoeff structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       EmisCoeff_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                    EmisCoeff structure pointer members are associated.
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
!                            association status of the EmisCoeff pointer members.
!                            .TRUE.  - if ALL the EmisCoeff pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the EmisCoeff pointer
!                                      members are associated.
!                            .FALSE. - some or all of the EmisCoeff pointer
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
!       Written by:     Paul van Delst, CIMSS/SSEC 13-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_EmisCoeff( EmisCoeff, & ! Input
                                 ANY_Test ) & ! Optional input
                               RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( EmisCoeff_type ), INTENT( IN ) :: EmisCoeff

    ! -- Optional input
    INTEGER,      OPTIONAL, INTENT( IN ) :: ANY_Test


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

      IF ( ASSOCIATED( EmisCoeff%Angle      ) .AND. &
           ASSOCIATED( EmisCoeff%Frequency  ) .AND. &
           ASSOCIATED( EmisCoeff%Wind_Speed ) .AND. &
           ASSOCIATED( EmisCoeff%Emissivity )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( EmisCoeff%Angle      ) .OR. &
           ASSOCIATED( EmisCoeff%Frequency  ) .OR. &
           ASSOCIATED( EmisCoeff%Wind_Speed ) .OR. &
           ASSOCIATED( EmisCoeff%Emissivity )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_EmisCoeff





!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_EmisCoeff
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of EmisCoeff
!       data structures.
!
! CATEGORY:
!       CRTM : Coefficients : EmisCoeff : Spectral Emissivity Model
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_EmisCoeff( EmisCoeff,                &  ! Output
!                                         RCS_Id = RCS_Id,          &  ! Revision control
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
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
!       EmisCoeff:    Re-initialized EmisCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       EmisCoeff_type
!                     DIMENSION:  Scalar
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
!       Display_Message:    Subroutine to output messages
!                           SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output EmisCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_EmisCoeff( EmisCoeff,    &  ! Output
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
    TYPE( EmisCoeff_type ),   INTENT( IN OUT ) :: EmisCoeff

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_EmisCoeff'


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

    IF ( Clear ) CALL Clear_EmisCoeff( EmisCoeff )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_EmisCoeff( EmisCoeff ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the Angle
    IF ( ASSOCIATED( EmisCoeff%Angle ) ) THEN

      DEALLOCATE( EmisCoeff%Angle, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Spectral EmisCoeff Angle ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Frequency
    IF ( ASSOCIATED( EmisCoeff%Frequency ) ) THEN

      DEALLOCATE( EmisCoeff%Frequency, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Spectral EmisCoeff Frequency ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Wind_Speed
    IF ( ASSOCIATED( EmisCoeff%Wind_Speed ) ) THEN

      DEALLOCATE( EmisCoeff%Wind_Speed, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Spectral EmisCoeff Wind_Speed ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Emissivity
    IF ( ASSOCIATED( EmisCoeff%Emissivity ) ) THEN

      DEALLOCATE( EmisCoeff%Emissivity, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Spectral EmisCoeff Emissivity ", &
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

    EmisCoeff%n_Allocates = EmisCoeff%n_Allocates - 1

    IF ( EmisCoeff%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      EmisCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_EmisCoeff





!------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_EmisCoeff
! 
! PURPOSE:
!       Function to allocate the pointer members of the EmisCoeff
!       data structure.
!
! CATEGORY:
!       CRTM : Coefficients : EmisCoeff : Spectral Emissivity Model
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_EmisCoeff( n_Angles,                  &  ! Input
!                                          n_Frequencies,             &  ! Input
!                                          n_Wind_Speeds,             &  ! Input
!                                          EmisCoeff,                 &  ! Output
!                                          RCS_Id = RCS_Id,           &  ! Revision control
!                                          Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Angles:      Number of angles dimension.
!                      Must be > 0.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
!       n_Frequencies: Number of frequencies dimension.
!                      Must be > 0.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
!       n_Wind_Speeds: Number of wind speeds dimension.
!                      Must be > 0.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in
!                      which any messages will be logged. If not
!                      specified, or if an error occurs opening
!                      the log file, the default action is to
!                      output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       EmisCoeff:     EmisCoeff structure with allocated
!                      pointer members
!                      UNITS:      N/A
!                      TYPE:       EmisCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structure allocation was successful
!                         == FAILURE - an error occurred, or
!                                    - the structure internal allocation counter
!                                      is not equal to one (1) upon exiting this
!                                      function. This value is incremented and
!                                      decremented for every structure allocation
!                                      and deallocation respectively.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       Associated_EmisCoeff:  Function to test the association status of the
!                              pointer members of a EmisCoeff structure.
!
!       Destroy_EmisCoeff:     Function to re-initialize the scalar and pointer
!                              members of EmisCoeff data structures.
!
!       Display_Message:       Subroutine to output messages
!                              SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output EmisCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Allocate_EmisCoeff( n_Angles,      &  ! Input
                               n_Frequencies, &  ! Input
                               n_Wind_Speeds, &  ! Input
                               EmisCoeff,     &  ! Output
                               RCS_Id,        &  ! Revision control
                               Message_Log )  &  ! Error messaging
                             RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )     :: n_Angles
    INTEGER,                  INTENT( IN )     :: n_Frequencies
    INTEGER,                  INTENT( IN )     :: n_Wind_Speeds

    ! -- Output
    TYPE( EmisCoeff_type ),   INTENT( IN OUT ) :: EmisCoeff

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_EmisCoeff'


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

    IF ( n_Angles      < 1 .OR. &
         n_Frequencies < 1 .OR. &
         n_Wind_Speeds < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Spectral EmisCoeff dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_EmisCoeff( EmisCoeff, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_EmisCoeff( EmisCoeff, &
                                        No_Clear = SET, &
                                        Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating Spectral EmisCoeff pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( EmisCoeff%Angle( n_Angles ), &
              EmisCoeff%Frequency( n_Frequencies ), &
              EmisCoeff%Wind_Speed( n_Wind_Speeds ), &
              EmisCoeff%Emissivity( n_Angles, n_Frequencies, n_Wind_Speeds ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating Spectral EmisCoeff data arrays. ", &
                        &"STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- ASSIGN THE DIMENSIONS --                        #
    !#--------------------------------------------------------------------------#

    EmisCoeff%n_Angles      = n_Angles     
    EmisCoeff%n_Frequencies = n_Frequencies
    EmisCoeff%n_Wind_Speeds = n_Wind_Speeds



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    EmisCoeff%n_Allocates = EmisCoeff%n_Allocates + 1

    IF ( EmisCoeff%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      EmisCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_EmisCoeff





!------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_EmisCoeff
!
! PURPOSE:
!       Function to copy valid EmisCoeff structures.
!
! CATEGORY:
!       CRTM : Coefficients : EmisCoeff : Spectral Emissivity Model
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_EmisCoeff( EmisCoeff_in,             &  ! Input
!                                        EmisCoeff_out,            &  ! Output
!                                        RCS_Id = RCS_Id,          &  ! Revision control
!                                        Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       EmisCoeff_in:  EmisCoeff structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       EmisCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       EmisCoeff_out: Copy of the input structure, EmisCoeff_in.
!                      UNITS:      N/A
!                      TYPE:       EmisCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structure assignment was successful
!                         == FAILURE an error occurred
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       Associated_EmisCoeff: Function to test the association status of the
!                             pointer members of a EmisCoeff structure.
!
!       Allocate_EmisCoeff:   Function to allocate the pointer members of
!                             the EmisCoeff data structure.
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output EmisCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Assign_EmisCoeff( EmisCoeff_in,  &  ! Input
                             EmisCoeff_out, &  ! Output
                             RCS_Id,        &  ! Revision control
                             Message_Log )  &  ! Error messaging
                           RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( EmisCoeff_type ),   INTENT( IN )     :: EmisCoeff_in

    ! -- Output
    TYPE( EmisCoeff_type ),   INTENT( IN OUT ) :: EmisCoeff_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_EmisCoeff'



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

    IF ( .NOT. Associated_EmisCoeff( EmisCoeff_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT Spectral EmisCoeff pointer '//&
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

    Error_Status = Allocate_EmisCoeff( EmisCoeff_in%n_Angles     , &
                                       EmisCoeff_in%n_Frequencies, &
                                       EmisCoeff_in%n_Wind_Speeds, &
                                       EmisCoeff_out, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output Spectral EmisCoeff arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------

    EmisCoeff_out%Release = EmisCoeff_in%Release
    EmisCoeff_out%Version = EmisCoeff_in%Version


    ! -----------------
    ! Assign array data
    ! -----------------

    EmisCoeff_out%Angle      = EmisCoeff_in%Angle
    EmisCoeff_out%Frequency  = EmisCoeff_in%Frequency 
    EmisCoeff_out%Wind_Speed = EmisCoeff_in%Wind_Speed
    EmisCoeff_out%Emissivity = EmisCoeff_in%Emissivity

  END FUNCTION Assign_EmisCoeff





!------------------------------------------------------------------------------
!S+
! NAME:
!       Equal_EmisCoeff
!
! PURPOSE:
!       Function to test if two EmisCoeff structures are equal.
!
! CATEGORY:
!       CRTM : Coefficients : EmisCoeff : Spectral Emissivity Model
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Equal_EmisCoeff( EmisCoeff_LHS,            &  ! Input
!                                       EmisCoeff_RHS,            &  ! Input
!                                       ULP_Scale   = ULP_Scale,  &  ! Optional input
!                                       Check_All   = Check_All,  &  ! Optional input
!                                       RCS_Id      = RCS_Id,     &  ! Revision control
!                                       Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       EmisCoeff_LHS: EmisCoeff structure to be compared; equivalent to the
!                      left-hand side of a lexical comparison, e.g.
!                        IF ( EmisCoeff_LHS == EmisCoeff_RHS ).
!                      UNITS:      N/A
!                      TYPE:       EmisCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
!       EmisCoeff_RHS: EmisCoeff structure to be compared to; equivalent to
!                      right-hand side of a lexical comparison, e.g.
!                        IF ( EmisCoeff_LHS == EmisCoeff_RHS ).
!                      UNITS:      N/A
!                      TYPE:       EmisCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP_Scale:     Unit of data precision used to scale the floating
!                      point comparison. ULP stands for "Unit in the Last Place,"
!                      the smallest possible increment or decrement that can be
!                      made using a machine's floating point arithmetic.
!                      Value must be positive - if a negative value is supplied,
!                      the absolute value is used. If not specified, the default
!                      value is 1.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Check_All:     Set this argument to check ALL the data in the
!                      EmisCoeff structures. The default action is return
!                      with a FAILURE status as soon as any difference is
!                      found. This optional argument can be used to get a
!                      listing of ALL the differences between data in
!                      the EmisCoeff structures. This does not apply if
!                      the structure dimensions are different, or the
!                      instrument/channel IDs are different.
!                      If == 0, Return with FAILURE status as soon as
!                               ANY difference is found  *DEFAULT*
!                         == 1, Set FAILURE status if ANY difference is
!                               found, but continue to check ALL data if
!                               structure dimensions are conformable.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structures were equal
!                         == FAILURE - an error occurred, or
!                                    - the structures were different.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       Compare_Float:        Function to compare floating point numbers
!                             for equality.
!                             SOURCE: COMPARE_FLOAT_NUMBERS module.
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Congruency of the structure data is a prerequisite of equality.
!       That is, the *order* of the data is also important.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Equal_EmisCoeff( EmisCoeff_LHS, &  ! Input
                            EmisCoeff_RHS, &  ! Input
                            ULP_Scale,     &  ! Optional input
                            Check_All,     &  ! Optional input
                            RCS_Id,        &  ! Revision control
                            Message_Log )  &  ! Error messaging
                          RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( EmisCoeff_type ),   INTENT( IN )  :: EmisCoeff_LHS
    TYPE( EmisCoeff_type ),   INTENT( IN )  :: EmisCoeff_RHS

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: ULP_Scale
    INTEGER,        OPTIONAL, INTENT( IN )  :: Check_All

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Equal_EmisCoeff'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: n



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
    !#                   -- CHECK THE OPTIONAL ARGUMENTS --                     #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Test the ULP_Scale argument
    ! ---------------------------

    ! -- Default precision is a single unit in last place
    ULP = 1
    ! -- ... unless the ULP_Scale argument is set and positive
    IF ( PRESENT( ULP_Scale ) ) THEN
      IF ( ULP_Scale > 0 ) ULP = ULP_Scale
    END IF


    ! ---------------------------
    ! Test the Check_All argument
    ! ---------------------------

    ! -- Default action is to return on ANY difference...
    Check_Once = .TRUE.
    ! -- ...unless the Check_All argument is set
    IF ( PRESENT( Check_All ) ) THEN
      IF ( Check_All == SET ) Check_Once = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! The LHS structure
    ! -----------------

    IF ( .NOT. Associated_EmisCoeff( EmisCoeff_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT Spectral EmisCoeff_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------
    ! The RHS structure
    ! -----------------

    IF ( .NOT. Associated_EmisCoeff( EmisCoeff_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT Spectral EmisCoeff_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- CHECK SCALAR MEMBERS --                        #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Release/Version info
    ! --------------------

    IF ( ( EmisCoeff_LHS%Release /= EmisCoeff_RHS%Release ) .OR. &
         ( EmisCoeff_LHS%Version /= EmisCoeff_RHS%Version )      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Release/Version numbers are different : ", &
                        &i2, ".", i2.2, " vs. ", i2, ".", i2.2 )' ) &
                      EmisCoeff_LHS%Release, EmisCoeff_LHS%Version, &
                      EmisCoeff_RHS%Release, EmisCoeff_RHS%Version
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! ----------
    ! Dimensions
    ! ----------

    ! -- Number of angles
    IF ( EmisCoeff_LHS%n_Angles /= EmisCoeff_RHS%n_Angles ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Angles dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      EmisCoeff_LHS%n_Angles, EmisCoeff_RHS%n_Angles
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Number of frequencies
    IF ( EmisCoeff_LHS%n_Frequencies /= EmisCoeff_RHS%n_Frequencies ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Frequencies dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      EmisCoeff_LHS%n_Frequencies, EmisCoeff_RHS%n_Frequencies
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Number of wind speeds
    IF ( EmisCoeff_LHS%n_Wind_Speeds /= EmisCoeff_RHS%n_Wind_Speeds ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Wind_Speeds dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      EmisCoeff_LHS%n_Wind_Speeds, EmisCoeff_RHS%n_Wind_Speeds
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CHECK ARRAY MEMBERS --                        #
    !#--------------------------------------------------------------------------#

    ! ------------
    ! Angle values
    ! ------------

    n = COUNT( .NOT. Compare_Float( EmisCoeff_LHS%Angle, &
                                    EmisCoeff_RHS%Angle, &
                                    ULP = ULP                 ) )
    IF ( n > 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( i5, " out of ", i5, " angle values are different." )' ) &
                      n, EmisCoeff_LHS%n_Angles
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( ADJUSTL( Message ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! ----------------
    ! Frequency values
    ! ----------------

    n = COUNT( .NOT. Compare_Float( EmisCoeff_LHS%Frequency, &
                                    EmisCoeff_RHS%Frequency, &
                                    ULP = ULP                ) )
    IF ( n > 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( i5, " out of ", i5, " frequency values are different." )' ) &
                      n, EmisCoeff_LHS%n_Frequencies
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( ADJUSTL( Message ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! -----------------
    ! Wind_Speed values
    ! -----------------

    n = COUNT( .NOT. Compare_Float( EmisCoeff_LHS%Wind_Speed, &
                                    EmisCoeff_RHS%Wind_Speed, &
                                    ULP = ULP                 ) )
    IF ( n > 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( i5, " out of ", i5, " wind speed values are different." )' ) &
                      n, EmisCoeff_LHS%n_Wind_Speeds
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( ADJUSTL( Message ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! -----------------
    ! Emissivity values
    ! -----------------

    n = COUNT( .NOT. Compare_Float( EmisCoeff_LHS%Emissivity, &
                                    EmisCoeff_RHS%Emissivity, &
                                    ULP = ULP                 ) )
    IF ( n > 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( i8, " out of ", i8, " emissivity values are different." )' ) &
                      n, SIZE( EmisCoeff_LHS%Emissivity )
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( ADJUSTL( Message ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF

  END FUNCTION Equal_EmisCoeff





!----------------------------------------------------------------------------------
!S+
! NAME:
!       Check_EmisCoeff_Release
!
! PURPOSE:
!       Function to check the EmisCoeff Release value.
!
! CATEGORY:
!       CRTM : Coefficients : EmisCoeff : Spectral Emissivity Model
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Check_EmisCoeff_Release( EmisCoeff,                &  ! Input
!                                               RCS_Id      = RCS_Id,     &  ! Revision control
!                                               Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       EmisCoeff:      EmisCoeff structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       EmisCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structure Release value is valid.
!                         == FAILURE the structure Release value is NOT valid
!                                    and either a data file file or software
!                                    update is required.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:      Subroutine to output messages
!                             SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!----------------------------------------------------------------------------------

  FUNCTION Check_EmisCoeff_Release( EmisCoeff,    &  ! Input
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
    TYPE( EmisCoeff_type ),   INTENT( IN )  :: EmisCoeff

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Check_EmisCoeff_Release'


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

    IF ( EmisCoeff%Release < EMISCOEFF_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A Spectral EmisCoeff data update is needed. ", &
                        &"Spectral EmisCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      EmisCoeff%Release, EMISCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#               -- CHECK THAT THE RELEASE IS NOT TOO NEW --                #
    !#--------------------------------------------------------------------------#

    IF ( EmisCoeff%Release > EMISCOEFF_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A Spectral EmisCoeff software update is needed. ", &
                        &"Spectral EmisCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      EmisCoeff%Release, EMISCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Check_EmisCoeff_Release





!------------------------------------------------------------------------------
!S+
! NAME:
!       Version_EmisCoeff
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the EmisCoeff data structure.
!
! CATEGORY:
!       CRTM : Coefficients : EmisCoeff : Spectral Emissivity Model
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Version_EmisCoeff( EmisCoeff,      &  ! Input
!                               Version_Info,   &  ! Output
!                               RCS_Id = RCS_Id )  ! Revision control
!
! INPUT ARGUMENTS:
!       EmisCoeff:     Filled EmisCoeff structure.
!                      UNITS:      N/A
!                      TYPE:       EmisCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Version_Info:  String containing version and dimension information
!                      about the passed EmisCoeff data structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
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
!       Written by:     Paul van Delst, CIMSS/SSEC 13-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  SUBROUTINE Version_EmisCoeff( EmisCoeff,    &  ! Input
                                Version_Info, &  ! Output
                                RCS_Id        )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( EmisCoeff_type ),   INTENT( IN )  :: EmisCoeff

    ! -- Output
    CHARACTER( * ),           INTENT( OUT ) :: Version_Info

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

    WRITE( Long_String, '( a,1x,"Spectral EmisCoeff RELEASE.VERSION: ", i2, ".", i2.2, 2x, &
                           &"N_ANGLES=",i3,2x,&
                           &"N_FREQUENCIES=",i5,2x,&
                           &"N_WIND_SPEEDS=",i3 )' ) &
                         ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                         EmisCoeff%Release, EmisCoeff%Version, &
                         EmisCoeff%n_Angles, &
                         EmisCoeff%n_Frequencies, &
                         EmisCoeff%n_Wind_Speeds


    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Version_Info = Long_String(1:MIN( LEN( Version_Info ), LEN_TRIM( Long_String ) ))

  END SUBROUTINE Version_EmisCoeff

END MODULE EmisCoeff_Define


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: EmisCoeff_Define.f90,v 2.1 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 2.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: EmisCoeff_Define.f90,v $
! Revision 2.1  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 2.0  2005/07/19 15:12:55  paulv
! - Update to Release 2.0. Emissivity derivative is no longer in the structure
!   and the dimension order of the emissivity data is altered to relfect the
!   order of calculation in the CRTM.
!
! Revision 1.3  2005/06/20 21:26:42  paulv
! - Changed category to CRTM Coefficients base.
!
! Revision 1.2  2005/06/20 21:25:22  paulv
! - Renamed structure member d2E_FV to d2E_dV2.
! - Updated output messages.
! - Added back Clear optional argument to Destroy() function and its related
!   calls.
!
! Revision 1.1  2005/06/20 15:40:33  paulv
! Initial checkin.
!
!
!
