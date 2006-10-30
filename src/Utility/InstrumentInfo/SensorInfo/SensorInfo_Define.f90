!------------------------------------------------------------------------------
!M+
! NAME:
!       SensorInfo_Define
!
! PURPOSE:
!       Module defining the SensorInfo data structure and containing routines to 
!       manipulate it.
!       
! CATEGORY:
!       Instrument_Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE SensorInfo_Define
!
! MODULES:
!       Type_Kinds:          Module containing definitions for kinds
!                            of variable types.
!
!       Message_Handler:     Module to define simple error codes and
!                            handle error conditions
!                            USEs: FILE_UTILITY module
!
! CONTAINS:
!       Associated_SensorInfo:  Function to test the association status of the
!                               pointer members of a SensorInfo structure.
!
!       Destroy_SensorInfo:     Function to re-initialize an SensorInfo structure.
!
!       Allocate_SensorInfo:    Function to allocate the pointer members
!                               of an SensorInfo structure.
!
!       Assign_SensorInfo:      Function to copy a valid SensorInfo structure.
!
! DERIVED TYPES:
!       SensorInfo_type:  Definition of the public SensorInfo data structure.
!                         Fields are,
!
!         n_Channels:        Total number of channels.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!         Sensor_Name:       A character string containing the sensor name.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER
!                            DIMENSION:  Scalar
!
!         Satellite_Name:    A character string containing the satellite name.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER( * )
!                            DIMENSION:  Scalar
!
!         File_Prefix:       A character string used to contruct filenames.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER( * )
!                            DIMENSION:  Scalar
!
!         Microwave_Flag:    A flag indicating if the sensor is a microwave
!                            instrument.
!                            If == 0, instrument is an infrared (IR) instrument
!                               == 1, instrument is a microwave (MW) instrument
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!         NCEP_Sensor_ID:    The NCEP/EMC "in-house" value used to distinguish
!                            between different sensor/platform combinations.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!         WMO_Sensor_ID:     The WMO Sensor ID number taken from Common
!                            Code Table C-8 in documentation at
!                              http://www.wmo.ch/web/ddbs/Code-tables.html
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!         WMO_Satellite_ID:  The WMO Satellite ID number taken from Common
!                            Code Table C-5 in documentation at
!                              http://www.wmo.ch/web/ddbs/Code-tables.html
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!         Sensor_Channel:    The channel numbers for the sensor channels
!                            in the data structure.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Rank-1
!                            ATTRIBUTES: POINTER
!
!         Use_Flag:          A flag value for indicating if the channel is to
!                            be used for anything. The interpretation of the
!                            values of this field is left to the user. In this
!                            code, however, the following is assumed:
!                            If = 0; channel NOT used.
!                               = 1; channel IS used.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Rank-1
!                            ATTRIBUTES: POINTER
!
!         Noise:             A noise estimate for the channels.
!                            UNITS:      Kelvin (K)
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Rank-1
!                            ATTRIBUTES: POINTER
!
!       *!IMPORTANT!*
!       -------------
!       Note that the SensorInfo_type is PUBLIC and its members are not
!       encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user destroy,
!       allocate, and assign the structure using only the routines
!       in this module where possible to eliminate -- or at least
!       minimise -- the possibility of memory leakage since a number
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
!       Written by:     Paul van Delst, CIMSS/SSEC 09-Aug-2002
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

MODULE SensorInfo_Define


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
  PUBLIC :: Associated_SensorInfo
  PUBLIC :: Destroy_SensorInfo
  PUBLIC :: Allocate_SensorInfo
  PUBLIC :: Assign_SensorInfo


  ! -------------------
  ! Procedure overloads
  ! -------------------

  INTERFACE Destroy_SensorInfo
    MODULE PROCEDURE Destroy_SensorInfo_scalar
    MODULE PROCEDURE Destroy_SensorInfo_rank1
  END INTERFACE ! Destroy_SensorInfo


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: SensorInfo_Define.f90,v 4.10 2006/05/02 16:58:02 dgroff Exp $'

  ! -- SensorInfo scalar invalid value
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1

  ! -- String length
  INTEGER, PRIVATE, PARAMETER :: STRING_LENGTH = 12

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


  ! ------------------------
  ! PUBLIC module parameters
  ! ------------------------

  ! -- The allowed sensor types
  INTEGER, PUBLIC, PARAMETER ::  INFRARED_SENSOR_TYPE = 0
  INTEGER, PUBLIC, PARAMETER :: MICROWAVE_SENSOR_TYPE = 1


  ! -------------------------------
  ! SensorInfo data type definition
  ! -------------------------------

  TYPE, PUBLIC :: SensorInfo_type
    INTEGER :: n_Allocates = 0

    INTEGER :: n_Channels = 0

    CHARACTER( 12 ) :: Sensor_Name    = ' '
    CHARACTER( 12 ) :: Satellite_Name = ' '
    CHARACTER( 20 ) :: File_Prefix    = ' '

    INTEGER :: Microwave_Flag = INVALID

    INTEGER :: NCEP_Sensor_ID   = INVALID
    INTEGER :: WMO_Sensor_ID    = INVALID
    INTEGER :: WMO_Satellite_ID = INVALID

    INTEGER,         DIMENSION( : ), POINTER :: Sensor_Channel => NULL()
    INTEGER,         DIMENSION( : ), POINTER :: Use_Flag       => NULL()
    REAL( fp_kind ), DIMENSION( : ), POINTER :: Noise          => NULL()
  END TYPE SensorInfo_type


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
!       Clear_SensorInfo
!
! PURPOSE:
!       Subroutine to clear the scalar members of a SensorInfo structure.
!
! CATEGORY:
!       Instrument Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_SensorInfo( SensorInfo) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       SensorInfo:  SensorInfo structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       SensorInfo_type
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
!       Note the INTENT on the output SensorInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined (at least
!       its components may be) upon input. To prevent memory leaks, the IN OUT
!       INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Aug-2002
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_SensorInfo( SensorInfo )

    TYPE( SensorInfo_type ), INTENT( IN OUT ) :: SensorInfo

    SensorInfo%n_Channels = 0

    SensorInfo%Sensor_Name    = ' '
    SensorInfo%Satellite_Name = ' '
    SensorInfo%File_Prefix    = ' '

    SensorInfo%Microwave_Flag   = INVALID 
    SensorInfo%NCEP_Sensor_ID   = INVALID 
    SensorInfo%WMO_Sensor_ID    = INVALID
    SensorInfo%WMO_Satellite_ID = INVALID

  END SUBROUTINE Clear_SensorInfo





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
!       Associated_SensorInfo
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       SensorInfo structure.
!
! CATEGORY:
!       Instrument Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_SensorInfo( SensorInfo,         &  ! Input
!                                                   ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       SensorInfo:  SensorInfo structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       SensorInfo_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                    SensorInfo structure pointer members are associated.
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
!                            association status of the SensorInfo pointer members.
!                            .TRUE.  - if ALL the SensorInfo pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the SensorInfo pointer
!                                      members are associated.
!                            .FALSE. - some or all of the SensorInfo pointer
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
!       This function tests the association status of the SensorInfo
!       structure pointer members. Therefore this function must only
!       be called after the input SensorInfo structure has, at least,
!       had its pointer members initialized.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Mar-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_SensorInfo( SensorInfo, & ! Input
                                  ANY_Test )  & ! Optional input
                                RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SensorInfo_type ), INTENT( IN ) :: SensorInfo

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

      IF ( ASSOCIATED( SensorInfo%Sensor_Channel ) .AND. &
           ASSOCIATED( SensorInfo%Use_Flag       ) .AND. &
           ASSOCIATED( SensorInfo%Noise          )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( SensorInfo%Sensor_Channel ) .OR. &
           ASSOCIATED( SensorInfo%Use_Flag       ) .OR. &
           ASSOCIATED( SensorInfo%Noise          )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_SensorInfo





!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_SensorInfo
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of SensorInfo
!       data structures.
!
! CATEGORY:
!       Instrument_Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_SensorInfo( SensorInfo,               &  ! Output
!                                          RCS_Id = RCS_Id,          &  ! Revision control
!                                          Message_Log = Message_Log )  ! Error messaging
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
!       SensorInfo:   Re-initialized SensorInfo structure.
!                     UNITS:      N/A
!                     TYPE:       SensorInfo_type
!                     DIMENSION:  Scalar or Rank-1
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
!       Display_Message:    Subroutine to output Messages
!                           SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SensorInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 09-Aug-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  !#============================================================================#
  !#                        -- SCALAR INPUT SUBROUTINE --                       #
  !#============================================================================#

  FUNCTION Destroy_SensorInfo_scalar( SensorInfo,   &  ! Output
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
    TYPE( SensorInfo_type ),  INTENT( IN OUT ) :: SensorInfo

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_SensorInfo'


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

    ! --------------------------------
    ! Re-initialise the scalar members
    ! --------------------------------

    IF ( Clear ) CALL Clear_SensorInfo( SensorInfo )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_SensorInfo( SensorInfo ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the sensor channel number array
    IF ( ASSOCIATED( SensorInfo%Sensor_Channel ) ) THEN

      DEALLOCATE( SensorInfo%Sensor_Channel, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SensorInfo Sensor_Channel ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the channel use flag array
    IF ( ASSOCIATED( SensorInfo%Use_Flag ) ) THEN
      DEALLOCATE( SensorInfo%Use_Flag, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SensorInfo Use_Flag ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the channel noise array
    IF ( ASSOCIATED( SensorInfo%Noise ) ) THEN
      DEALLOCATE( SensorInfo%Noise, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SensorInfo Noise ", &
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

    SensorInfo%n_Allocates = SensorInfo%n_Allocates - 1

    IF ( SensorInfo%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      SensorInfo%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_SensorInfo_scalar



  !#============================================================================#
  !#                        -- RANK-1 INPUT SUBROUTINE --                       #
  !#============================================================================#

  FUNCTION Destroy_SensorInfo_rank1( SensorInfo,   &  ! Output
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
    TYPE( SensorInfo_type ), DIMENSION( : ), INTENT( IN OUT ) :: SensorInfo

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_SensorInfo(rank1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 10 ) :: Message

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
    !#                      -- PERFORM RE-INITIALISATION --                     #
    !#--------------------------------------------------------------------------#

    DO n = 1, SIZE( SensorInfo )


      ! ------------------------
      ! Call the scalar function
      ! ------------------------

      Scalar_Status = Destroy_SensorInfo_scalar( SensorInfo(n), &
                                                 No_Clear = No_Clear, &
                                                 Message_Log = Message_Log )


      ! -------------------------------------------------
      ! Check the result, but do not halt so deallocation
      ! continues even if an error is encountered.
      ! -------------------------------------------------

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( i10 )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying SensorInfo structure array element '//&
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Destroy_SensorInfo_rank1





!------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_SensorInfo
! 
! PURPOSE:
!       Function to allocate the pointer members of the SensorInfo
!       data structure.
!
! CATEGORY:
!       Instrument Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_SensorInfo( n_Channels,               &  ! Input
!                                           SensorInfo,               &  ! Output
!                                           RCS_Id = RCS_Id,          &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Channels:   The number of channels in the SensorInfo structure.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
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
!       SensorInfo:   SensorInfo structure with allocated pointer members
!                     UNITS:      N/A
!                     TYPE:       SensorInfo_type
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
!       Associated_SensorInfo:  Function to test the association status of the
!                               pointer members of a SensorInfo structure.
!
!       Destroy_SensorInfo:     Function to re-initialize the scalar and pointer
!                               members of SensorInfo data structures.
!
!       Display_Message:        Subroutine to output Messages
!                               SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SensorInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Allocate_SensorInfo( n_Channels,   &  ! Input
                                SensorInfo,   &  ! Output
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
    INTEGER,                  INTENT( IN )     :: n_Channels

    ! -- Output
    TYPE( SensorInfo_type ),  INTENT( IN OUT ) :: SensorInfo

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_SensorInfo'


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

    ! ----------------------
    ! The number of channels
    ! ----------------------

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

    IF ( Associated_SensorInfo( SensorInfo, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_SensorInfo( SensorInfo, &
                                         No_Clear = SET, &
                                         Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating SensorInfo pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( SensorInfo%Sensor_Channel( n_Channels ), &
              SensorInfo%Use_Flag( n_Channels ), &
              SensorInfo%Noise( n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating SensorInfo data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- ASSIGN THE n_Channels DIMENSION --                 #
    !#--------------------------------------------------------------------------#

    SensorInfo%n_Channels = n_Channels



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    SensorInfo%n_Allocates = SensorInfo%n_Allocates + 1

    IF ( SensorInfo%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      SensorInfo%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_SensorInfo





!------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_SensorInfo
!
! PURPOSE:
!       Function to copy valid SensorInfo structures.
!
! CATEGORY:
!       Instrument_Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_SensorInfo( SensorInfo_in,            &  ! Input
!                                         SensorInfo_out,           &  ! Output
!                                         RCS_Id      = RCS_Id,     &  ! Revision control
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SensorInfo_in:   SensorInfo structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       SensorInfo_type
!                        DIMENSION:  Scalar
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
!       SensorInfo_out:  Copy of the input structure, SensorInfo_in.
!                        UNITS:      N/A
!                        TYPE:       SensorInfo_type
!                        DIMENSION:  Scalar
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
!       Associated_SensorInfo:  Function to test the association status of the
!                               pointer members of a SensorInfo structure.
!
!       Allocate_SensorInfo:    Function to allocate the pointer members of
!                               the SensorInfo data structure.
!
!       Display_Message:        Subroutine to output Messages
!                               SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SensorInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 09-Aug-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Assign_SensorInfo( SensorInfo_in,  &  ! Input
                              SensorInfo_out, &  ! Output
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
    TYPE( SensorInfo_type ),  INTENT( IN )     :: SensorInfo_in

    ! -- Output
    TYPE( SensorInfo_type ),  INTENT( IN OUT ) :: SensorInfo_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_SensorInfo'



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

    IF ( .NOT. Associated_SensorInfo( SensorInfo_In ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT SensorInfo pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! -----------------------
    ! Allocate data structure
    ! -----------------------

    Error_Status = Allocate_SensorInfo( SensorInfo_in%n_Channels, &
                                        SensorInfo_out, &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output SensorInfo arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------
    ! Assign scalar members
    ! ---------------------

    SensorInfo_out%Sensor_Name    = SensorInfo_in%Sensor_Name
    SensorInfo_out%Satellite_Name = SensorInfo_in%Satellite_Name
    SensorInfo_out%File_Prefix    = SensorInfo_in%File_Prefix

    SensorInfo_out%Microwave_Flag = SensorInfo_in%Microwave_Flag

    SensorInfo_out%NCEP_Sensor_ID   = SensorInfo_in%NCEP_Sensor_ID
    SensorInfo_out%WMO_Sensor_ID    = SensorInfo_in%WMO_Sensor_ID
    SensorInfo_out%WMO_Satellite_ID = SensorInfo_in%WMO_Satellite_ID


    ! -----------------
    ! Assign array data
    ! -----------------

    SensorInfo_out%Sensor_Channel = SensorInfo_in%Sensor_Channel
    SensorInfo_out%Use_Flag       = SensorInfo_in%Use_Flag
    SensorInfo_out%Noise          = SensorInfo_in%Noise

  END FUNCTION Assign_SensorInfo

END MODULE SensorInfo_Define


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: SensorInfo_Define.f90,v 4.10 2006/05/02 16:58:02 dgroff Exp $
!
! $Date: 2006/05/02 16:58:02 $
!
! $Revision: 4.10 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SensorInfo_Define.f90,v $
! Revision 4.10  2006/05/02 16:58:02  dgroff
! *** empty log message ***
!
! Revision 4.9  2006/01/26 23:45:37  paulv
! - Tidied up the Assign() function a bit.
!
! Revision 4.8  2005/04/01 15:28:53  paulv
! - Added association test to Destroy() function.
! - Removed output of warning message if association test passes in Allocate()
!   function. Now, the SensorInfo structure is simply deallocated for reuse.
!
! Revision 4.7  2004/09/03 15:49:09  paulv
! - Made Associated_SensorInfo() function public.
!
! Revision 4.6  2004/08/18 15:36:25  paulv
! - Updated header documentation.
! - Added optional No_Clear  argument to the rank-1 Destroy_SensorInfo()
!   function. Left it out in error when it was added to the scalar form.
!
! Revision 4.5  2004/08/17 19:17:22  paulv
! - Fortran-95 upgrade. Structure component initialisation is now done
!   in the derived type definition.
! - Removed structure initialisation function.
! - Made the SensorInfo argument in the Clear_SensorInfo() routine have INTENT( IN OUT)
!   to prevent memory leaks.
! - Added a No_Clear argument to the Destroy_SensorInfo() function so the pointer members
!   can be deallocated without clearing the scalar members.
! - Made the SensorInfo argument in the Allocate_SensorInfo() routine have INTENT( IN OUT)
!   to prevent memory leaks.
! - The Allocate_SensorInfo() function now deallocates the pointer members for
!   the *output* SensorInfo argument if they are already allocated.
! - Made the SensorInfo argument in the Assign_SensorInfo() routine have INTENT( IN OUT)
!   to prevent memory leaks.
!
! Revision 4.4  2004/07/19 19:32:11  paulv
! - Updated the document headers.
!
! Revision 4.3  2003/10/21 13:10:28  paulv
! - SensorInfo arguments to the Destroy() functions are now INTENT( IN OUT ).
!
! Revision 4.2  2003/07/24 18:59:12  paulv
! - Updated some header documentation.
!
! Revision 4.1  2003/06/16 19:16:30  paulv
! - Added Microwave_Flag structure member.
!
! Revision 4.0  2003/06/04 15:14:57  paulv
! - Removed use of ChannelInfo structure and routines. All channel information
!   is now kept in pointer components of the SensorInfo structure itself. Makes
!   the use of the SensorInfo files a bit less flexible (i.e. channel information
!   is now required rather than optional), but now there is one and only one
!   SensorInfo file format.
! - Added public Allocate_SensorInfo() function.
! - Added private Associated_SensorInfo() function.
! - Added optional RCS_Id argument to all public routines.
! - Added internal allocation counter and checks in Allocate_SensorInfo() and
!   Destroy_SensorInfo() functions.
!
! Revision 3.0  2003/04/16 21:11:12  paulv
! - New version for use with the SensorInfo_LinkedList module.
!
! Revision 2.3  2003/04/16 14:47:34  paulv
! - Added Concatenate_SensorInfo() function.
! - Updated header documentation.
!
! Revision 2.2  2003/04/11 16:32:03  paulv
! - Updated header documentation.
!
! Revision 2.1  2003/02/28 19:34:40  paulv
! - Updated documentation.
! - Added No_ChannelInfo optional argument to the Associated_SensorInfo()
!   Allocate_SensorInfo(), and Assign_SensorInfo() function. Specifying
!   this argument prevents action being taken on the ChannelInfo member
!   of the SensorInfo structure.
!
! Revision 2.0  2003/02/07 21:32:12  paulv
! - New version with ChannelInfo component.
!
! Revision 1.1  2002/08/09 18:30:17  paulv
! Initial checkin.
!
!
!
!
!
