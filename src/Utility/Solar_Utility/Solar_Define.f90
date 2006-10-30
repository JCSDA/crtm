!------------------------------------------------------------------------------
!M+
! NAME:
!       Solar_Define
!
! PURPOSE:
!       Module defining the Solar data structure and containing routines to 
!       manipulate it.
!       
! CATEGORY:
!       Solar_Spectrum
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Solar_Define
!
! MODULES:
!       Type_Kinds:       Module containing definitions for kinds
!                         of variable types.
!
!       Error_Handler:    Module to define simple error codes and
!                         handle error conditions
!                         USEs: FILE_UTILITY module
!
! CONTAINS:
!       Associated_Solar:  Function to test the association status
!                          of the pointer members of a Solar structure.
!
!       Destroy_Solar:     Function to re-initialize a Solar data structure.
!
!       Allocate_Solar:    Function to allocate the data arrays in
!                          a Solar structures.
!
!       Assign_Solar:      Function to copy valid Solar structures.
!
!       Frequency_Solar:   Function to compute the frequency grid for
!                          a supplied Solar data structure.
!
! DERIVED TYPES:
!       Solar_type:   Definition of the public Solar data structure. Fields
!                     are,
!
!         Begin_Frequency:         Begin frequency of the irradiance
!                                  data.
!                                  UNITS:      Inverse centimetres (cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         End_Frequency:           End frequency of the irradiance
!                                  data.
!                                  UNITS:      Inverse centimetres (cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Frequency_Interval:      Frequency interval of the irradiance
!                                  data.
!                                  UNITS:      Inverse centimetres (cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Blackbody_Temperature:   Solar radiative temperature used
!                                  in blackbody source function.
!                                  UNITS:      Kelvin (K)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Radius:                  Mean radius of visible Solar disk,
!                                  or photosphere.
!                                  UNITS:      Metres (m)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Earth_Sun_Distance:      Annual mean of earth-sun distance
!                                  UNITS:      Metres (m)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Frequency:               The frequency grid of the solar data.
!                                  UNITS:      inverse centimetres (cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1
!                                  ATTRIBUTES: POINTER
!
!         Irradiance:              Synthetic extraterrestrial monochromatic
!                                  Solar irradiance of Kurucz.
!                                  UNITS:      mW/(m^2.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1
!                                  ATTRIBUTES: POINTER
!
!         Blackbody_Irradiance:    Blackbody extraterrestrial monochromatic
!                                  Solar irradiance
!                                  UNITS:      mW/(m^2.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1
!                                  ATTRIBUTES: POINTER
!
!       *!IMPORTANT!*
!       -------------
!       Note that the Solar_type is PUBLIC and its members are not
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
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jan-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002 Paul van Delst
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

MODULE Solar_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Error_Handler


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Public procedures to manipulate the Solar structure
  PUBLIC :: Associated_Solar
  PUBLIC :: Destroy_Solar
  PUBLIC :: Allocate_Solar
  PUBLIC :: Assign_Solar
  PUBLIC :: Frequency_Solar


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- RCS Id for the module
  CHARACTER( * ),  PARAMETER, PRIVATE :: MODULE_RCS_ID = &
  '$Id: Solar_Define.f90,v 2.3 2005/08/11 17:32:29 paulv Exp $'

  ! -- Default Solar blackbody temperature in KELVIN
  REAL( fp_kind ), PARAMETER, PRIVATE :: DEFAULT_BLACKBODY_TEMPERATURE = 5783.0_fp_kind

  ! -- Default mean Solar radius in METRES
  REAL( fp_kind ), PARAMETER, PRIVATE :: DEFAULT_RADIUS = 6.599e+08_fp_kind

  ! -- Default mean Earth-Solar distance in METRES
  REAL( fp_kind ), PARAMETER, PRIVATE :: DEFAULT_EARTH_SUN_DISTANCE = 1.495979e+11_fp_kind

  ! -- Literal constants
  REAL( fp_kind ), PARAMETER, PRIVATE :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER, PRIVATE :: ONE  = 1.0_fp_kind

  ! -- Scalar member invalid value
  REAL( fp_kind ), PARAMETER, PRIVATE :: INVALID = -ONE

  ! -- Keyword set value
  INTEGER,         PARAMETER, PRIVATE :: SET = 1



  ! --------------------------
  ! Solar data type definition
  ! --------------------------

  TYPE, PUBLIC :: Solar_type
    INTEGER :: n_Allocates = 0

    INTEGER :: n_Frequencies = 0

    REAL( fp_kind ) :: Begin_Frequency       = INVALID
    REAL( fp_kind ) :: End_Frequency         = INVALID
    REAL( fp_kind ) :: Frequency_Interval    = INVALID
    REAL( fp_kind ) :: Blackbody_Temperature = DEFAULT_BLACKBODY_TEMPERATURE
    REAL( fp_kind ) :: Radius                = DEFAULT_RADIUS
    REAL( fp_kind ) :: Earth_Sun_Distance    = DEFAULT_EARTH_SUN_DISTANCE

    REAL( fp_kind ), POINTER, DIMENSION( : ) :: Frequency            => NULL()
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: Irradiance           => NULL()
    REAL( fp_kind ), POINTER, DIMENSION( : ) :: Blackbody_Irradiance => NULL()
  END TYPE Solar_type


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
!       Clear_Solar
!
! PURPOSE:
!       Subroutine to clear the scalar members of a Solar structure.
!
! CATEGORY:
!       Solar Spectrum
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_Solar( Solar ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Solar:       Solar structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       Solar_type
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
!       Note the INTENT on the output Solar argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jan-2002
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_Solar( Solar )

    TYPE( Solar_type ), INTENT( IN OUT ) :: Solar

    Solar%n_Frequencies = 0

    Solar%Begin_Frequency       = INVALID
    Solar%End_Frequency         = INVALID
    Solar%Frequency_Interval    = INVALID
    Solar%Blackbody_Temperature = DEFAULT_BLACKBODY_TEMPERATURE
    Solar%Radius                = DEFAULT_RADIUS
    Solar%Earth_Sun_Distance    = DEFAULT_EARTH_SUN_DISTANCE

  END SUBROUTINE Clear_Solar





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
!       Associated_Solar
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       Solar structure.
!
! CATEGORY:
!       Solar Spectrum
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_Solar( Solar,              &  ! Input
!                                              ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       Solar:               Solar structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       Solar_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            Solar structure pointer members are associated.
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
!                            association status of the Solar pointer members.
!                            .TRUE.  - if ALL the Solar pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the Solar pointer
!                                      members are associated.
!                            .FALSE. - some or all of the Solar pointer
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
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_Solar( Solar,     & ! Input
                             ANY_Test ) & ! Optional input
                           RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( Solar_type ), INTENT( IN ) :: Solar

    ! -- Optional input
    INTEGER,  OPTIONAL, INTENT( IN ) :: ANY_Test


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

      IF ( ASSOCIATED( Solar%Frequency            ) .AND. &
           ASSOCIATED( Solar%Irradiance           ) .AND. &
           ASSOCIATED( Solar%Blackbody_Irradiance )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( Solar%Frequency            ) .OR. &
           ASSOCIATED( Solar%Irradiance           ) .OR. &
           ASSOCIATED( Solar%Blackbody_Irradiance )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_Solar





!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_Solar
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of a 
!       Solar data structure.
!
! CATEGORY:
!       Solar_Spectrum
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_Solar( Solar,                    &  ! Output
!                                     RCS_Id      = RCS_Id,     &  ! Revision control
!                                     Message_Log = Message_Log )  ! Error messaging
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
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Solar:        Re-nitialized Solar structure
!                     UNITS:      N/A
!                     TYPE:       Solar_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
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
!       Note the INTENT on the output Solar argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jan-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_Solar( Solar,        &  ! Output
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
    TYPE( Solar_type ),       INTENT( IN OUT ) :: Solar

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_Solar'


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
    !#                       -- PERFORM INITIALISATION --                       #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_Solar( Solar )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_Solar( Solar ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the Frequency array
    IF ( ASSOCIATED( Solar%Frequency ) ) THEN

      DEALLOCATE( Solar%Frequency, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Solar Frequency ", &
                          &"array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Irradiance array
    IF ( ASSOCIATED( Solar%Irradiance ) ) THEN

      DEALLOCATE( Solar%Irradiance, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Solar Irradiance ", &
                          &"array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Blackbody Irradiance array
    IF ( ASSOCIATED( Solar%Blackbody_Irradiance ) ) THEN

      DEALLOCATE( Solar%Blackbody_Irradiance, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Solar Blackbody Irradiance ", &
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

    Solar%n_Allocates = Solar%n_Allocates - 1

    IF ( Solar%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      Solar%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Solar





!------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_Solar
! 
! PURPOSE:
!       Function to allocate the pointer members of the Solar data structure.
!
! CATEGORY:
!       Solar_Spectrum
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_Solar( n_Frequencies,            &  ! Input 
!                                      Solar,                    &  ! Output
!                                      RCS_Id = RCS_Id,          &  ! Revision control
!                                      Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Frquencies: Spectral dimension of the Solar structure pointer
!                     members. Must be > 0.
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
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Solar:        Solar structure with allocated pointer members
!                     UNITS:      N/A
!                     TYPE:       Solar_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the structure pointer allocations were
!                                   successful
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
!       Associated_Solar:  Function to test the association status of the
!                          pointer members of a Solar structure.
!
!       Destroy_Solar:     Function to re-initialize the scalar and pointer
!                          members of Solar data structures.
!
!       Display_Message:   Subroutine to output messages
!                          SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output Solar argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Jan-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Allocate_Solar( n_Frequencies, &  ! Input
                           Solar,         &  ! Output
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
    INTEGER,                  INTENT( IN )     :: n_Frequencies

    ! -- Output
    TYPE( Solar_type ),       INTENT( IN OUT ) :: Solar

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_Solar'


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

    ! ---------
    ! Dimension
    ! ---------

    IF ( n_Frequencies < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input N_FREQUENCIES must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_Solar( Solar, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_Solar( Solar, &
                                    No_Clear = SET, &
                                    Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating Solar pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE(  Solar%Frequency( n_Frequencies ),            &
               Solar%Irradiance( n_Frequencies ),           &
               Solar%Blackbody_Irradiance( n_Frequencies ), &
               STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating Solar data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN THE DIMENSIONS AND ARRAYS --                  #
    !#--------------------------------------------------------------------------#

    Solar%n_Frequencies = n_Frequencies

    Solar%Frequency            = ZERO
    Solar%Irradiance           = ZERO
    Solar%Blackbody_Irradiance = ZERO



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    Solar%n_Allocates = Solar%n_Allocates + 1

    IF ( Solar%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      Solar%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Solar





!------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_Solar
!
! PURPOSE:
!       Function to copy valid Solar structures.
!
! CATEGORY:
!       Solar_Spectrum
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_Solar( Solar_in,                 &  ! Input
!                                    Solar_out,                &  ! Output
!                                    RCS_Id = RCS_Id,          &  ! Revision control
!                                    Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Solar_in:     Solar structure which is to be copied.
!                     UNITS:      N/A
!                     TYPE:       Solar_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Solar_out:    Copy of the input structure, Solar_in.
!                     UNITS:      N/A
!                     TYPE:       Solar_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
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
!       Associated_Solar:   Function to test the association status of the
!                           pointer members of a Solar structure.
!
!       Allocate_Solar:     Function to allocate the pointer members of
!                           the Solar data structure.
!
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
!       Note the INTENT on the output Solar argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Jan-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Assign_Solar( Solar_in,     &  ! Input
                         Solar_out,    &  ! Output
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
    TYPE( Solar_type ),       INTENT( IN )     :: Solar_in

    ! -- Output
    TYPE( Solar_type ),       INTENT( IN OUT ) :: Solar_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_Solar'


    ! ---------------
    ! Local variables
    ! ---------------



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

    IF ( .NOT. Associated_Solar( Solar_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT Solar pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Allocate the output structure
    ! -----------------------------

    Error_Status = Allocate_Solar( Solar_in%n_Frequencies, &
                                   Solar_out, &
                                   Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output Solar structure.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------
    ! Assign scalar members
    ! ---------------------

    Solar_out%Begin_Frequency       = Solar_in%Begin_Frequency
    Solar_out%End_Frequency         = Solar_in%End_Frequency
    Solar_out%Frequency_Interval    = Solar_in%Frequency_Interval
    Solar_out%Blackbody_Temperature = Solar_in%Blackbody_Temperature
    Solar_out%Radius                = Solar_in%Radius
    Solar_out%Earth_Sun_Distance    = Solar_in%Earth_Sun_Distance


    ! -----------------
    ! Assign array data
    ! -----------------

    Solar_out%Frequency            = Solar_in%Frequency
    Solar_out%Irradiance           = Solar_in%Irradiance
    Solar_out%Blackbody_Irradiance = Solar_in%Blackbody_Irradiance

  END FUNCTION Assign_Solar





!------------------------------------------------------------------------------
!S+
! NAME:
!       Frequency_Solar
!
! PURPOSE:
!       Function to compute the frequency grid for a supplied Solar data
!       structure.
!
! CATEGORY:
!       Solar_Spectrum
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Frequency_Solar( Solar,                    &  ! In/Output
!                                       Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Solar:        Solar structure with fields containing the begin and
!                     end frequencies of the frequency grid to compute.
!                     UNITS:      N/A
!                     TYPE:       Solar_type
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
!       Solar:        Solar structure with the frequency component filled.
!                     UNITS:      N/A
!                     TYPE:       Solar_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the frequency grid calculation was successful
!                        == FAILURE an error occurred processing the input
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:      Subroutine to output messages
!                             SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       The FREQUENCY field of the input Solar structure is filled.
!
! RESTRICTIONS:
!       Solar structure must contain at least 2 points of frequency and response
!       data.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-May-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Frequency_Solar( Solar,        &  ! In/Output
                            Message_Log ) &  ! Error messaging
                          RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( Solar_type ),       INTENT( IN OUT ) :: Solar

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Frequency_Solar'


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

    IF ( .NOT. Associated_Solar( Solar ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT Solar pointer members are NOT associated.', &
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

    n = Solar%n_Frequencies

    IF ( n < 2 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Allocated Solar structure arrays must contain at least 2 points.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------------
    ! Construct a frequency grid of 0->1
    ! ----------------------------------

    Solar%Frequency(1:n)  = (/ ( REAL( i - 1, fp_kind ), i = 1, n ) /) / &
    !                       ------------------------------------------
                                     REAL( n - 1, fp_kind )


    ! -----------------------------
    ! Scale it to the actual values
    ! -----------------------------

    Solar%Frequency(1:n) = Solar%Begin_Frequency + &
                           ( Solar%Frequency(1:n) * ( Solar%End_Frequency - Solar%Begin_Frequency ) )

  END FUNCTION Frequency_Solar

END MODULE Solar_Define


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Solar_Define.f90,v 2.3 2005/08/11 17:32:29 paulv Exp $
!
! $Date: 2005/08/11 17:32:29 $
!
! $Revision: 2.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Solar_Define.f90,v $
! Revision 2.3  2005/08/11 17:32:29  paulv
! - Added Frequency component to Solar structure.
! - Added Frequency_Solar function to compute the frequency grid for the
!   solar spectrum.
!
! Revision 2.2  2004/08/31 18:19:24  paulv
! - Upgraded to Fortran95.
! - Derived type component initialisation is now done in the definition block.
! - Init_Solar() subroutine has been removed.
! - Intent of Solar dummy argument in Clear_Solar() routine changed from
!   OUT to IN OUT to prevent memory leaks.
! - Added optional No_Clear argument to Destroy_Solar() function.
! - Intent of Solar dummy argument in Allocate_Solar() routine changed from
!   OUT to IN OUT to prevent memory leaks.
! - Call to Destroy_Solar() added to Allocate_Solar() function for the case
!   where the input Solar argument is already allocated.
! - Intent of Solar_out dummy argument in Assign_Solar() routine changed from
!   OUT to IN OUT to prevent memory leaks.
! - Updated header documentation.
!
! Revision 2.1  2004/06/25 17:17:38  paulv
! - Removed unused variables from type declarations.
! - Added Associated() function.
! - Cosmetic changes.
!
! Revision 2.0  2003/02/11 22:12:57  paulv
! - New version. Updated documentation. Solar_type structure components
!   renamed.
!
! Revision 1.2  2002/08/22 20:22:48  paulv
! - Corrected dummy argument name declaration bug
!
! Revision 1.1  2002/08/22 17:24:02  paulv
! Initial checkin.
!
!
!
!
!
