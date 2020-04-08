!--------------------------------------------------------------------------------
!M+
! NAME:
!       EmisTestData_Define
!
! PURPOSE:
!       Module defining the EmisTestData data structure and containing routines to 
!       manipulate it.
!       
! CATEGORY:
!       CRTM : User Code : NESDIS Emissivity
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE EmisTestData_Define
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Message_Handler:        Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
! CONTAINS:
!       Associated_EmisTestData:      Function to test the association status
!                                     of the pointer members of a EmisTestData
!                                     structure.
!
!       Destroy_EmisTestData:         Function to re-initialize an EmisTestData
!                                     structure.
!
!       Allocate_EmisTestData:        Function to allocate the pointer members
!                                     of an EmisTestData structure.
!
!       Assign_EmisTestData:          Function to copy an EmisTestData structure.
!
!
! DERIVED TYPES:
!       EmisTestData_type:   Definition of the EmisTestData data structure. Fields
!                            are...
!
!
!       *!IMPORTANT!*
!       -------------
!       Note that the EmisTestData_type is PUBLIC and its members are not
!       encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user
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
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Dec-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
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
!--------------------------------------------------------------------------------

MODULE EmisTestData_Define


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

  ! -- Public procedures to manipulate the EmisTestData structure
  PUBLIC :: Associated_EmisTestData
  PUBLIC :: Destroy_EmisTestData
  PUBLIC :: Allocate_EmisTestData
  PUBLIC :: Assign_EmisTestData


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- EmisTestData scalar member invalid value
  INTEGER,        PRIVATE, PARAMETER :: INVALID = -1
  REAL( Double ), PRIVATE, PARAMETER :: FP_INVALID = -1.0_Double

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Observation type string length
  INTEGER, PRIVATE, PARAMETER :: DL = 10


  ! ---------------------------------
  ! EmisTestData data type definition
  ! ---------------------------------

  TYPE, PUBLIC :: EmisTestData_type
    INTEGER :: n_Allocates = 0

    INTEGER( Long ) :: ObsType_StrLen = DL
    INTEGER( Long ) :: n_Channels = 0  ! L dimension

    CHARACTER( DL ) :: ObsType                = ' '
    INTEGER( Long ) :: Channel                =    INVALID
    REAL( Double )  :: Frequency              = FP_INVALID
    REAL( Double )  :: Channel_Polarization   = FP_INVALID
    REAL( Double )  :: Latitude               = FP_INVALID
    REAL( Double )  :: Longitude              = FP_INVALID
    REAL( Double )  :: Satellite_Zenith_Angle = FP_INVALID
    REAL( Double )  :: Satellite_View_Angle   = FP_INVALID
    INTEGER( Long ) :: LandSea_Flag           =    INVALID
    INTEGER( Long ) :: IceSnow_Flag           =    INVALID
    INTEGER( Long ) :: Surface_Type           =    INVALID
    REAL( Double )  :: Wind_Speed_10m         = FP_INVALID
    REAL( Double )  :: Skin_Temperature       = FP_INVALID
    REAL( Double )  :: Snow_Depth             = FP_INVALID
    REAL( Double )  :: Vegetation_Fraction    = FP_INVALID
    REAL( Double )  :: Vegetation_Type        = FP_INVALID
    REAL( Double )  :: Soil_Type              = FP_INVALID
    REAL( Double )  :: Soil_Moisture          = FP_INVALID
    REAL( Double )  :: Soil_Temperature       = FP_INVALID
    REAL( Double )  :: SimulatedTb            = FP_INVALID
    REAL( Double )  :: Emissivity             = FP_INVALID
    REAL( Double )  :: Emissivity_Vertical    = FP_INVALID
    REAL( Double )  :: Emissivity_Horizontal  = FP_INVALID
    REAL( Double ), POINTER, DIMENSION(:) :: ObsTb => NULL()  ! L
  END TYPE EmisTestData_type


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
!       Clear_EmisTestData
!
! PURPOSE:
!       Subroutine to clear the scalar members of a EmisTestData structure.
!
! CATEGORY:
!       CRTM : User Code : NESDIS Emissivity
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_EmisTestData( EmisTestData) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       EmisTestData:  EmisTestData structure for which the scalar members have
!                      been cleared.
!                      UNITS:      N/A
!                      TYPE:       EmisTestData_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
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
!       Note the INTENT on the output EmisTestData argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Dec-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_EmisTestData( EmisTestData )

    TYPE( EmisTestData_type ), INTENT( IN OUT ) :: EmisTestData

    EmisTestData%ObsType_StrLen = DL
    EmisTestData%n_Channels     = 0

    EmisTestData%ObsType                = ' '
    EmisTestData%Channel                =    INVALID
    EmisTestData%Frequency              = FP_INVALID
    EmisTestData%Channel_Polarization   = FP_INVALID
    EmisTestData%Latitude               = FP_INVALID
    EmisTestData%Longitude              = FP_INVALID
    EmisTestData%Satellite_Zenith_Angle = FP_INVALID
    EmisTestData%Satellite_View_Angle   = FP_INVALID
    EmisTestData%LandSea_Flag           =    INVALID
    EmisTestData%IceSnow_Flag           =    INVALID
    EmisTestData%Surface_Type           =    INVALID
    EmisTestData%Wind_Speed_10m         = FP_INVALID
    EmisTestData%Skin_Temperature       = FP_INVALID
    EmisTestData%Snow_Depth             = FP_INVALID
    EmisTestData%Vegetation_Fraction    = FP_INVALID
    EmisTestData%Vegetation_Type        = FP_INVALID
    EmisTestData%Soil_Type              = FP_INVALID
    EmisTestData%Soil_Moisture          = FP_INVALID
    EmisTestData%Soil_Temperature       = FP_INVALID
    EmisTestData%SimulatedTb            = FP_INVALID
    EmisTestData%Emissivity             = FP_INVALID
    EmisTestData%Emissivity_Vertical    = FP_INVALID
    EmisTestData%Emissivity_Horizontal  = FP_INVALID

  END SUBROUTINE Clear_EmisTestData





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
!       Associated_EmisTestData
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       EmisTestData structure.
!
! CATEGORY:
!       CRTM : User Code : NESDIS Emissivity
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_EmisTestData( EmisTestData,      &  ! Input
!                                                    ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       EmisTestData:        EmisTestData structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       EmisTestData_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            EmisTestData structure pointer members are associated.
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
!                            association status of the EmisTestData pointer members.
!                            .TRUE.  - if ALL the EmisTestData pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the EmisTestData pointer
!                                      members are associated.
!                            .FALSE. - some or all of the EmisTestData pointer
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
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Dec-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_EmisTestData( EmisTestData, & ! Input
                                    ANY_Test )    & ! Optional input
                                  RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( EmisTestData_type ), INTENT( IN ) :: EmisTestData

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

    ! ONLY ONE ITEM SO THE ANY TEST IS SUPERFLUOUS
    ! BUT I LEFT IT IN

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( EmisTestData%ObsTb ) ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( EmisTestData%ObsTb ) ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_EmisTestData





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_EmisTestData
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of EmisTestData
!       data structures.
!
! CATEGORY:
!       CRTM : User Code : NESDIS Emissivity
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_EmisTestData( EmisTestData,             &  ! Output
!                                            RCS_Id = RCS_Id,          &  ! Revision control
!                                            Message_Log = Message_Log )  ! Error messaging
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
!       EmisTestData: Re-initialized EmisTestData structure.
!                     UNITS:      N/A
!                     TYPE:       EmisTestData_type
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
!       Note the INTENT on the output EmisTestData argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Dec-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Destroy_EmisTestData( EmisTestData, &  ! Output
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
    TYPE( EmisTestData_type ), INTENT( IN OUT ) :: EmisTestData

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_EmisTestData'


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
    !#                       -- PERFORM REINITIALISATION --                     #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_EmisTestData( EmisTestData )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_EmisTestData( EmisTestData ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the observation brightness temperatures
    IF ( ASSOCIATED( EmisTestData%ObsTb ) ) THEN

      DEALLOCATE( EmisTestData%ObsTb, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating EmisTestData ObsTb ", &
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

    EmisTestData%n_Allocates = EmisTestData%n_Allocates - 1

    IF ( EmisTestData%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      EmisTestData%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_EmisTestData





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_EmisTestData
! 
! PURPOSE:
!       Function to allocate the pointer members of the EmisTestData
!       data structure.
!
! CATEGORY:
!       CRTM : User Code : NESDIS Emissivity
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_EmisTestData( n_Channels,               &  ! Input
!                                             EmisTestData,             &  ! Output
!                                             RCS_Id = RCS_Id,          &  ! Revision control
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Channels:   Required dimension of EmisTestData structure pointer
!                     members.
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
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       EmisTestData: EmisTestData structure with allocated pointer members
!                     UNITS:      N/A
!                     TYPE:       EmisTestData_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
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
!       Associated_EmisTestData:  Function to test the association status of the
!                                 pointer members of a EmisTestData structure.
!
!       Destroy_EmisTestData:     Function to re-initialize the scalar and pointer
!                                 members of EmisTestData data structures.
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
!       Note the INTENT on the output EmisTestData argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Dec-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Allocate_EmisTestData( n_Channels,   &  ! Input
                                  EmisTestData, &  ! Output       
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
    TYPE( EmisTestData_type ), INTENT( IN OUT ) :: EmisTestData

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_EmisTestData'


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

    ! ------------------
    ! Spectral dimension
    ! ------------------

    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input N_CHANNELS must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_EmisTestData( EmisTestData, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_EmisTestData( EmisTestData, &
                                           No_Clear = SET, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating EmisTestData pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( EmisTestData%ObsTb( n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating EmisTestData data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                      -- ASSIGN SOME COMPONENTS --                        #
    !#--------------------------------------------------------------------------#

    ! --------------
    ! The dimensions
    ! --------------

    EmisTestData%n_Channels = n_Channels


    ! --------
    ! The data
    ! --------

    EmisTestData%ObsTb = FP_INVALID



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    EmisTestData%n_Allocates = EmisTestData%n_Allocates + 1

    IF ( EmisTestData%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      EmisTestData%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_EmisTestData





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_EmisTestData
!
! PURPOSE:
!       Function to copy valid EmisTestData structures.
!
! CATEGORY:
!       CRTM : User Code : NESDIS Emissivity
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_EmisTestData( EmisTestData_in,          &  ! Input
!                                           EmisTestData_out,         &  ! Output
!                                           RCS_Id = RCS_Id,          &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       EmisTestData_in:   EmisTestData structure which is to be copied.
!                          UNITS:      N/A
!                          TYPE:       EmisTestData_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       EmisTestData_out:  Copy of the input structure, EmisTestData_in.
!                          UNITS:      N/A
!                          TYPE:       EmisTestData_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the ERROR_HANDLER module.
!                          If == SUCCESS the structure assignment was successful
!                             == FAILURE an error occurred
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! CALLS:
!       Associated_EmisTestData:  Function to test the association status of the
!                                 pointer members of a EmisTestData structure.
!
!       Allocate_EmisTestData:    Function to allocate the pointer members of
!                                 the EmisTestData data structure.
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
!       Note the INTENT on the output EmisTestData argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Dec-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Assign_EmisTestData( EmisTestData_in,  &  ! Input
                                EmisTestData_out, &  ! Output
                                RCS_Id,           &  ! Revision control
                                Message_Log )     &  ! Error messaging
                              RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( EmisTestData_type ), INTENT( IN )     :: EmisTestData_in

    ! -- Output
    TYPE( EmisTestData_type ), INTENT( IN OUT ) :: EmisTestData_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_EmisTestData'



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

    IF ( .NOT. Associated_EmisTestData( EmisTestData_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT EmisTestData pointer '//&
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

    Error_Status = Allocate_EmisTestData( EmisTestData_in%n_Channels,  &
                                          EmisTestData_out, &
                                          Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output EmisTestData arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------

    EmisTestData_Out%ObsType                = EmisTestData_In%ObsType               
    EmisTestData_Out%Channel                = EmisTestData_In%Channel               
    EmisTestData_Out%Frequency              = EmisTestData_In%Frequency             
    EmisTestData_Out%Channel_Polarization   = EmisTestData_In%Channel_Polarization  
    EmisTestData_Out%Latitude               = EmisTestData_In%Latitude              
    EmisTestData_Out%Longitude              = EmisTestData_In%Longitude             
    EmisTestData_Out%Satellite_Zenith_Angle = EmisTestData_In%Satellite_Zenith_Angle
    EmisTestData_Out%Satellite_View_Angle   = EmisTestData_In%Satellite_View_Angle  
    EmisTestData_Out%LandSea_Flag           = EmisTestData_In%LandSea_Flag          
    EmisTestData_Out%IceSnow_Flag           = EmisTestData_In%IceSnow_Flag          
    EmisTestData_Out%Surface_Type           = EmisTestData_In%Surface_Type          
    EmisTestData_Out%Wind_Speed_10m         = EmisTestData_In%Wind_Speed_10m        
    EmisTestData_Out%Skin_Temperature       = EmisTestData_In%Skin_Temperature      
    EmisTestData_Out%Snow_Depth             = EmisTestData_In%Snow_Depth            
    EmisTestData_Out%Vegetation_Fraction    = EmisTestData_In%Vegetation_Fraction   
    EmisTestData_Out%Vegetation_Type        = EmisTestData_In%Vegetation_Type       
    EmisTestData_Out%Soil_Type              = EmisTestData_In%Soil_Type             
    EmisTestData_Out%Soil_Moisture          = EmisTestData_In%Soil_Moisture         
    EmisTestData_Out%Soil_Temperature       = EmisTestData_In%Soil_Temperature      
    EmisTestData_Out%SimulatedTb            = EmisTestData_In%SimulatedTb           
    EmisTestData_Out%Emissivity             = EmisTestData_In%Emissivity            
    EmisTestData_Out%Emissivity_Vertical    = EmisTestData_In%Emissivity_Vertical   
    EmisTestData_Out%Emissivity_Horizontal  = EmisTestData_In%Emissivity_Horizontal 


    ! -----------------
    ! Assign array data
    ! -----------------

    EmisTestData_Out%ObsTb = EmisTestData_In%ObsTb

  END FUNCTION Assign_EmisTestData

END MODULE EmisTestData_Define


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: EmisTestData_Define.f90,v $
! Revision 1.2  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2004/12/08 16:47:35  paulv
! Initial checkin.
!
!
!
!
!
