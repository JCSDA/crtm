!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_ChannelInfo
!
! PURPOSE:
!       Module containing routines to populate the CRTM ChannelInfo structure.
!       
! CATEGORY:
!       CRTM : ChannelInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_ChannelInfo
!
! MODULES:
!       Type_Kinds:           Module containing definitions for kinds      
!                             of variable types.                           
!
!       Message_Handler:      Module to define simple error codes and      
!                             handle error conditions                      
!                             USEs: FILE_UTILITY module                    
!
!       SpcCoeff_Define:      Module defining the SpcCoeff data structure and
!                             containing routines to manipulate it.
!                             USEs: TYPE_KINDS module
!                                   ERROR_HANDLER module
!                                   COMPUTE_FLOAT_NUMBERS module
!
!       CRTM_ChannelInfo_Define:  Module defining the CRTM ChannelInfo    
!                             data structure and containing routines      
!                             to manipulate it.                           
!                             USEs: TYPE_KINDS module                     
!                                   ERROR_HANDLER module                  
!
!       CRTM_SpcCoeff:        Module containing the shared CRTM spectral 
!                             coefficients and their load/destruction    
!                             routines.                                  
!                             USEs: TYPE_KINDS module                    
!                                   ERROR_HANDLER module                 
!                                   SPCCOEFF_DEFINE module               
!                                   SPCCOEFF_BINARY_IO module            
!                                   CRTM_PARAMETERS module               
! CONTAINS:
!       PUBLIC subprograms
!       ------------------
!         CRTM_Index_ChannelInfo:  Function to populate the Channel_Index     
!                                  component of a ChannelInfo structure.      
!
!         CRTM_Set_ChannelInfo:    Function to populate the CRTM ChannelInfo 
!                                  structure according to user specified 
!                                  sensor information
!
!         CRTM_Fill_ChannelInfo:   Function to populate rest of the CRTM 
!                                  ChannelInfo structure members after
!
!                                  the CRTM_Index_ChannelInfo() is called.
!       PRIVATE subprograms
!       -------------------
!         None.
!
!
! USE ASSOCIATED PUBLIC SUBPROGRAMS:
!       CRTM_Associated_ChannelInfo:  Function to test the association status
!                                     of the pointer members of a ChannelInfo
!                                     structure.
!                                     SOURCE: CRTM_CHANNELINFO_DEFINE module
!
!       CRTM_Destroy_ChannelInfo:     Function to re-initialize a ChannelInfo
!                                     structure.
!                                     SOURCE: CRTM_CHANNELINFO_DEFINE module
!
!       CRTM_Allocate_ChannelInfo:    Function to allocate the pointer members
!                                     of an ChannelInfo structure.
!                                     SOURCE: CRTM_CHANNELINFO_DEFINE module
!
!       CRTM_Assign_ChannelInfo:      Function to copy a valid ChannelInfo
!                                     structure.
!                                     SOURCE: CRTM_CHANNELINFO_DEFINE module
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
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
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
!------------------------------------------------------------------------------

MODULE CRTM_ChannelInfo


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler

  USE CRTM_ChannelInfo_Define
  USE SpcCoeff_Define
  USE CRTM_SpcCoeff


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- CRTM_ChannelInfo structure data type
  ! -- in the CRTM_ChannelInfo_Define module
  PUBLIC :: CRTM_ChannelInfo_type

  ! -- CRTM_ChannelInfo structure routines inherited
  ! -- from the CRTM_ChannelInfo_Define module
  ! -- Definition functions
  PUBLIC :: CRTM_Associated_ChannelInfo
  PUBLIC :: CRTM_Destroy_ChannelInfo
  PUBLIC :: CRTM_Allocate_ChannelInfo
  PUBLIC :: CRTM_Assign_ChannelInfo

  ! -- This modules public routines
  PUBLIC :: CRTM_Index_ChannelInfo
  PUBLIC :: CRTM_Set_ChannelInfo
  PUBLIC :: CRTM_Fill_ChannelInfo

  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE CRTM_Index_ChannelInfo
    MODULE PROCEDURE IndexChannelInfo_DESC
    MODULE PROCEDURE IndexChannelInfo_NCEPID
  END INTERFACE CRTM_Index_ChannelInfo

  INTERFACE CRTM_Set_ChannelInfo
    MODULE PROCEDURE Set_ChannelInfo_F1
    MODULE PROCEDURE Set_ChannelInfo_F2
  END INTERFACE CRTM_Set_ChannelInfo

  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


CONTAINS





!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################




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
!       CRTM_Index_ChannelInfo
!
! PURPOSE:
!       Function to populate the CRTM ChannelInfo structure Channel_Index
!       component
!
! CATEGORY:
!       CRTM : ChannelInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Index_ChannelInfo( Master_Descriptor,        &  ! Input
!                                              Master_Sensor_Channel,    &  ! Input
!                                              User_Descriptor,          &  ! Input
!                                              User_Sensor_Channel,      &  ! Input
!                                              ChannelInfo,              &  ! Output
!                                              RCS_Id      = RCS_Id,     &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Master_Descriptor:      Master satellite/sensor descriptor list that
!                               contains an entry for all the available
!                               satellites/sensors for every sensor channel.
!                               This argument can either be a string
!                               (Sensor_Descriptor) or an integer (NCEP_Sensor_ID)
!                               UNITS:      N/A
!                               TYPE:       CHARACTER(*)
!                                               or
!                                           INTEGER
!                               DIMENSION:  Rank-1 (Max_n_Channels)
!                               ATTRIBUTES: INTENT( IN )
!
!       Master_Sensor_Channel:  Master satellite/sensor channel list that
!                               contains an entry for all the available
!                               satellite/sensor channels
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Rank-1 (Max_n_Channels)
!                               ATTRIBUTES: INTENT( IN )
!
!       User_Descriptor:        User satellite/sensor descriptor list that
!                               contains an entry for all the required
!                               satellites/sensors for every required sensor
!                               channel.
!                               UNITS:      N/A
!                               TYPE:       Same as Master_Descriptor argument
!                               DIMENSION:  Rank-1 (n_Channels)
!                               ATTRIBUTES: INTENT( IN )
!
!       User_Sensor_Channel:    USer satellite/sensor channel list that
!                               contains an entry for all the required
!                               satellite/sensor channels
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Rank-1 (n_Channels)
!                               ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:            Character string specifying a filename in which any
!                               Messages will be logged. If not specified, or if an
!                               error occurs opening the log file, the default action
!                               is to output Messages to standard output.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER( * )
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       ChannelInfo:            ChannelInfo structure with the CHANNEL_INDEX field
!                               filled according to the matchups between the Master
!                               and User descriptor and sensor channel lists.
!                               UNITS:      N/A
!                               TYPE:       CRTM_ChannelInfo_type
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:                 Character string containing the Revision Control
!                               System Id field for the module.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER(*)
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:           The return value is an integer defining the error status.
!                               The error codes are defined in the ERROR_HANDLER module.
!                               If == SUCCESS the indexing was successful
!                                  == FAILURE an unrecoverable error occurred
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Allocate_ChannelInfo:    Function to allocate the pointer members
!                                     of a CRTM ChannelInfo structure.
!                                     SOURCE: CRTM_CHANNELINFO_DEFINE module
!
!       Display_Message:              Subroutine to output Messages
!                                     SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!       
! COMMENTS:
!       Note the INTENT on the output ChannelInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION IndexChannelInfo_DESC( Master_Sensor_Descriptor, &  ! Input
                                  Master_Sensor_Channel,    &  ! Input
                                  User_Sensor_Descriptor,   &  ! Input
                                  User_Sensor_Channel,      &  ! Input
                                  ChannelInfo,              &  ! Output
                                  RCS_Id,                   &  ! Revision control
                                  Message_Log )             &  ! Error messaging
                                RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ), DIMENSION( : ), INTENT( IN )     :: Master_Sensor_Descriptor
    INTEGER,        DIMENSION( : ), INTENT( IN )     :: Master_Sensor_Channel
    CHARACTER( * ), DIMENSION( : ), INTENT( IN )     :: User_Sensor_Descriptor
    INTEGER,        DIMENSION( : ), INTENT( IN )     :: User_Sensor_Channel

    ! -- Output
    TYPE( CRTM_ChannelInfo_type ),  INTENT( IN OUT ) :: ChannelInfo

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_IndexChannelInfo(DESC)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: l, n
    INTEGER :: n_Master_Channels
    INTEGER :: n_User_Channels
    INTEGER :: n_Matched_Channels
    INTEGER, DIMENSION( SIZE( Master_Sensor_Channel ) ) :: Channel_Index
    INTEGER, DIMENSION( 1 ) :: Idx



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                              -- CHECK INPUT --                           #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Check the dimensions
    ! --------------------

    ! -- Check that Master inputs are the same size
    n_Master_Channels = SIZE( Master_Sensor_Descriptor )
    IF ( SIZE( Master_Sensor_Channel ) /= n_Master_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Master_Sensor_Descriptor and Master_Sensor_Channel '//&
                            'arrays have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Check that User inputs are the same size
    n_User_Channels = SIZE( User_Sensor_Descriptor )
    IF ( SIZE( User_Sensor_Channel ) /= n_User_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input User_Sensor_Descriptor and User_Sensor_Channel '//&
                            'arrays have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Check that User inputs aren't zero-sized
    IF ( n_User_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input User arrays must have sizes > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE MATCHING INDICES --                      #
    !#--------------------------------------------------------------------------#

    n_Matched_Channels = 0

    DO l = 1, n_User_Channels

      ! -- Count the matchups
      n = COUNT( Master_Sensor_Descriptor == User_Sensor_Descriptor(l) .AND. &
                 Master_Sensor_Channel    == User_Sensor_Channel(l)          )
 
      ! -- If none, go to next channel
      IF ( n == 0 ) CYCLE
      ! -- If more than one, Master arrays are screwy
      IF ( n > 1 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Multiple entries in Master arrays for the same '//&
                              'User specified channel.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Increment channel match counter
      n_Matched_Channels = n_Matched_Channels + 1
 
      ! -- Get the matching index
      Idx = PACK( (/ ( n, n = 1, n_Master_Channels ) /), &
                  Master_Sensor_Descriptor == User_Sensor_Descriptor(l) .AND. &
                  Master_Sensor_Channel    == User_Sensor_Channel(l)          )

      ! -- Save it
      Channel_Index( n_Matched_Channels ) = Idx(1)

    END DO

    ! -- Were ANY matches found?
    IF ( n_Matched_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'No Master/User array data matches were found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#       -- POPULATE THE ChannelInfo Channel_Index STRUCTURE MEMBER --      #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------
    ! Allocate the ChannelInfo structure
    ! ----------------------------------

    Error_Status = CRTM_Allocate_ChannelInfo( n_Matched_Channels, &
                                              ChannelInfo, &
                                              Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Allocation of ChannelInfo structure failed.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------
    ! Save the channel indices
    ! ------------------------

    ChannelInfo%Channel_Index = Channel_Index( 1:n_Matched_Channels )


  END FUNCTION IndexChannelInfo_DESC

  FUNCTION IndexChannelInfo_NCEPID( Master_NCEP_Sensor_ID, &  ! Input
                                    Master_Sensor_Channel, &  ! Input
                                    User_NCEP_Sensor_ID,   &  ! Input
                                    User_Sensor_Channel,   &  ! Input
                                    ChannelInfo,           &  ! Output
                                    RCS_Id,                &  ! Revision control
                                    Message_Log )          &  ! Error messaging
                                  RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,        DIMENSION( : ), INTENT( IN )     :: Master_NCEP_Sensor_ID
    INTEGER,        DIMENSION( : ), INTENT( IN )     :: Master_Sensor_Channel
    INTEGER,        DIMENSION( : ), INTENT( IN )     :: User_NCEP_Sensor_ID
    INTEGER,        DIMENSION( : ), INTENT( IN )     :: User_Sensor_Channel

    ! -- Output
    TYPE( CRTM_ChannelInfo_type ),  INTENT( IN OUT ) :: ChannelInfo

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_IndexChannelInfo(NCEPID)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: l, n
    INTEGER :: n_Master_Channels
    INTEGER :: n_User_Channels
    INTEGER :: n_Matched_Channels
    INTEGER, DIMENSION( SIZE( Master_Sensor_Channel ) ) :: Channel_Index
    INTEGER, DIMENSION( 1 ) :: Idx



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                              -- CHECK INPUT --                           #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Check the dimensions
    ! --------------------

    ! -- Check that Master inputs are the same size
    n_Master_Channels = SIZE( Master_NCEP_Sensor_ID )
    IF ( SIZE( Master_Sensor_Channel ) /= n_Master_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Master_NCEP_Sensor_ID and Master_Sensor_Channel '//&
                            'arrays have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Check that User inputs are the same size
    n_User_Channels = SIZE( User_NCEP_Sensor_ID )
    IF ( SIZE( User_Sensor_Channel ) /= n_User_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input User_NCEP_Sensor_ID and User_Sensor_Channel '//&
                            'arrays have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Check that User inputs aren't zero-sized
    IF ( n_User_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input User arrays must have sizes > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE MATCHING INDICES --                      #
    !#--------------------------------------------------------------------------#

    n_Matched_Channels = 0

    DO l = 1, n_User_Channels

      ! -- Count the matchups
      n = COUNT( Master_NCEP_Sensor_ID == User_NCEP_Sensor_ID(l) .AND. &
                 Master_Sensor_Channel == User_Sensor_Channel(l)       )
 
      ! -- If none, go to next channel
      IF ( n == 0 ) CYCLE

      ! -- If more than one, Master arrays are screwy
      IF ( n > 1 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Multiple entries in Master arrays for the same '//&
                              'User specified channel.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Increment channel match counter
      n_Matched_Channels = n_Matched_Channels + 1
 
      ! -- Get the matching index
      Idx = PACK( (/ ( n, n = 1, n_Master_Channels ) /), &
                  Master_NCEP_Sensor_ID == User_NCEP_Sensor_ID(l) .AND. &
                  Master_Sensor_Channel == User_Sensor_Channel(l)       )

      ! -- Save it
      Channel_Index( n_Matched_Channels ) = Idx(1)

    END DO

    ! -- Were ANY matches found?
    IF ( n_Matched_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'No Master/User array data matches were found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#       -- POPULATE THE ChannelInfo Channel_Index STRUCTURE MEMBER --      #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------
    ! Allocate the ChannelInfo structure
    ! ----------------------------------

    Error_Status = CRTM_Allocate_ChannelInfo( n_Matched_Channels, &
                                              ChannelInfo, &
                                              Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Allocation of ChannelInfo structure failed.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------
    ! Save the channel indices
    ! ------------------------

    ChannelInfo%Channel_Index = Channel_Index( 1:n_Matched_Channels )


  END FUNCTION IndexChannelInfo_NCEPID

!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Set_ChannelInfo
!
! PURPOSE:
!       Function to populate the CRTM ChannelInfo structure according to user
!       specified sensor information
!
! CATEGORY:
!       CRTM : ChannelInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Set_ChannelInfo( Sensor_Descriptor,        &  ! Input
!                                            ChannelInfo,              &  ! Output
!                                            RCS_Id      = RCS_Id,     &  ! Revision control
!                                            Message_Log = Message_Log )  ! Error messaging
!                      or
!
!        Error_Status = CRTM_Set_ChannelInfo( Sensor_Descriptors,      &  ! Input
!                                             Sensor_Channels,         &  ! Input            
!                                             ChannelInfo,             &  ! Output            
!                                             RCS_Id = RCS_Id,         &  ! Revision control  
!                                             Message_Log = Message_Log ) ! Error messaging   
!
! INPUT ARGUMENTS:
!       Sensor_Descriptor:      satellite/sensor descriptor that
!                               contains an entry for the required
!                               satellite/sensor.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER(*)
!                               DIMENSION:  Scalsr
!                               ATTRIBUTES: INTENT( IN )
!
!       Sensor_Descriptors:     satellite/sensor descriptor list that
!                               contains an entry for all the available
!                               satellites/sensors for every sensor channel.
!                               This argument can either be a string
!                               (Sensor_Descriptor) or an integer (NCEP_Sensor_ID)
!                               UNITS:      N/A
!                               TYPE:       CHARACTER(*)
!                               DIMENSION:  Rank-1 (Max_n_Channels)
!                               ATTRIBUTES: INTENT( IN )
!
!       Sensor_Channels:        satellite/sensor channel list that
!                               contains an entry for all the available
!                               satellite/sensor channels
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Rank-1 (Max_n_Channels)
!                               ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:            Character string specifying a filename in which any
!                               Messages will be logged. If not specified, or if an
!                               error occurs opening the log file, the default action
!                               is to output Messages to standard output.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER( * )
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       ChannelInfo:            ChannelInfo structure with the CHANNEL_INDEX field
!                               filled according to the matchups between the Master
!                               and User descriptor and sensor channel lists.
!                               UNITS:      N/A
!                               TYPE:       CRTM_ChannelInfo_type
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:                 Character string containing the Revision Control
!                               System Id field for the module.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER(*)
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:           The return value is an integer defining the error status.
!                               The error codes are defined in the ERROR_HANDLER module.
!                               If == SUCCESS the indexing was successful
!                                  == FAILURE an unrecoverable error occurred
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!
! CALLS:
!       Associated_SpcCoeff:    Function to test the association status
!                               of the pointer members of a SpcCoeff
!                               structure.
!                               SOURCE: SpcCoeff_Define 
!
!       CRTM_Index_ChannelInfo: Function to populate the CRTM ChannelInfo structure Channel_Index
!                               component
!                               SOURCE: this module
!
!       Display_Message:              Subroutine to output Messages
!                                     SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!       
! COMMENTS:
!       Note the INTENT on the output ChannelInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Y. Han, August 20, 2005
!S-
!--------------------------------------------------------------------------------

  FUNCTION Set_ChannelInfo_F1( Sensor_Descriptor,       &  ! Input
                               ChannelInfo,             &  ! Output              
                               RCS_Id,                  &  ! Revision control    
                               Message_Log )            &  ! Error messaging     
                             RESULT( Error_Status )                              

    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ), INTENT( IN )     :: Sensor_Descriptor

    ! -- Output
    TYPE( CRTM_ChannelInfo_type ),  INTENT( IN OUT ) :: ChannelInfo

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Set_ChannelInfo (F1)'


    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER, ALLOCATABLE :: Channel_Index(:)
    CHARACTER( 256 )     :: Message
    INTEGER              :: i, n_Matched_Channels, Allocate_Status

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

    ! -- Check that the shared data structure SpcCoeff is loaded. 
    IF( .NOT. Associated_SpcCoeff( SC ) )THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'CRTM has not be correctly initialized. ', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
      
    ENDIF

    ! --- Get the number of required channels 
    n_Matched_Channels = COUNT( SC%Sensor_Descriptor == Sensor_Descriptor )
    IF ( n_Matched_Channels == 0 ) THEN                                                             
      Error_Status = FAILURE                                                      
      CALL Display_Message( ROUTINE_NAME, &                                       
                            'The Sensor '//TRIM(Sensor_Descriptor)//&
                            ' is not included in the SpcCoeff data file.', &  
                            Error_Status, &                                       
                            Message_Log = Message_Log )                           
      RETURN                                                                      
    END IF

    ! -- Allocate Channel_Index
    ALLOCATE( Channel_Index( n_Matched_Channels ), STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating Channel_Index ", i5 )' ) Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &           
                            TRIM( Message ), &           
                            Error_Status,    &           
                            Message_Log = Message_Log )  
    END IF

    Channel_Index = PACK( (/ ( i, i = 1, SC%n_Channels ) /), &
                    MASK = SC%Sensor_Descriptor == Sensor_Descriptor ) 

    ! --- POPULATE THE ChannelInfo Channel_Index STRUCTURE MEMBER
    Error_Status = CRTM_Index_ChannelInfo( SC%Sensor_Descriptor,                &
                                           SC%Sensor_Channel,                   & 
                                           SC%Sensor_Descriptor(Channel_Index), & 
                                           SC%Sensor_Channel(Channel_Index),    &
                                           ChannelInfo,                         &
                                           Message_Log = Message_Log )

    IF ( Error_Status  /= SUCCESS ) THEN                     
      CALL Display_Message( ROUTINE_NAME, &                  
                            'Error indexing ChannelInfo', &  
                            Error_Status, &                  
                            Message_Log = Message_Log )      
      RETURN                                                 
    END IF 

    ! -- Deallocate Channel_Index
    DEALLOCATE( Channel_Index, STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating Channel_Index ", i5 )' ) Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &           
                            TRIM( Message ), &           
                            Error_Status,    &           
                            Message_Log = Message_Log )  
    END IF


    ! ------------------------------------------
    ! Fill the rest of the ChannelInfo structure
    ! ------------------------------------------

    CALL CRTM_Fill_ChannelInfo( ChannelInfo )

  END FUNCTION Set_ChannelInfo_F1

  FUNCTION Set_ChannelInfo_F2( Sensor_Descriptor,       &  ! Input
                               Sensor_Channel,          &   ! Input              
                               ChannelInfo,             &  ! Output              
                               RCS_Id,                  &  ! Revision control    
                               Message_Log )            &  ! Error messaging     
                             RESULT( Error_Status )                              

    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ), DIMENSION( : ), INTENT( IN )     :: Sensor_Descriptor
    INTEGER,        DIMENSION( : ), INTENT( IN )     :: Sensor_Channel

    ! -- Output
    TYPE( CRTM_ChannelInfo_type ),  INTENT( IN OUT ) :: ChannelInfo

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Set_ChannelInfo (F2)'


    ! ---------------
    ! Local variables
    ! ---------------


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

    ! -- Check that the shared data structure SpcCoeff is loaded. 
    IF( .NOT. Associated_SpcCoeff( SC ) )THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'CRTM has not be correctly initialized. ', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
      
    ENDIF

    ! --- Fill ChannelInfo structure
    Error_Status = CRTM_Index_ChannelInfo( SC%Sensor_Descriptor, &     
                                           SC%Sensor_Channel,    &      
                                           Sensor_Descriptor,    &
                                           Sensor_Channel,       &
                                           ChannelInfo,          &
                                           Message_Log = Message_Log )

    IF ( Error_Status  /= SUCCESS ) THEN                     
      CALL Display_Message( ROUTINE_NAME, &                  
                            'Error indexing ChannelInfo', &  
                            Error_Status, &                  
                            Message_Log = Message_Log )      
      RETURN                                                 
    END IF 

    ! ------------------------------------------
    ! Fill the rest of the ChannelInfo structure
    ! ------------------------------------------

    CALL CRTM_Fill_ChannelInfo( ChannelInfo )

  END FUNCTION Set_ChannelInfo_F2


!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Fill_ChannelInfo
!
! PURPOSE:
!       Function to populate the rest of the CRTM ChannelInfo structure members after
!       the CRTM_Index_ChannelInfo() is called.
!
! CATEGORY:
!       CRTM : ChannelInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Fill_ChannelInfo( ChannelInfo ) ! In/Output
!
! INPUT ARGUMENTS:
!
! In/OUTPUT ARGUMENTS:
!       ChannelInfo:            ChannelInfo structure with the CHANNEL_INDEX field
!                               filled.
!                               UNITS:      N/A
!                               TYPE:       CRTM_ChannelInfo_type
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN OUT )
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!       
! COMMENTS:
!       Note the INTENT on the output ChannelInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Y. Han, August 20, 2005
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Fill_ChannelInfo( ChannelInfo )

    TYPE( CRTM_ChannelInfo_type ),  INTENT( IN OUT ) :: ChannelInfo

    ChannelInfo%Sensor_Descriptor = SC%Sensor_Descriptor( ChannelInfo%Channel_Index )
    ChannelInfo%NCEP_Sensor_ID    = SC%NCEP_Sensor_ID( ChannelInfo%Channel_Index )
    ChannelInfo%WMO_Satellite_ID  = SC%WMO_Satellite_ID( ChannelInfo%Channel_Index )
    ChannelInfo%WMO_Sensor_ID     = SC%WMO_Sensor_ID( ChannelInfo%Channel_Index )
    ChannelInfo%Sensor_Channel    = SC%Sensor_Channel( ChannelInfo%Channel_Index )

  END SUBROUTINE CRTM_Fill_ChannelInfo

END MODULE CRTM_ChannelInfo


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 1.11 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_ChannelInfo.f90,v $
! Revision 1.11  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.10  2006/04/24 17:51:57  wd20pd
! - Merged CRTM_Sensor branch onto the main trunk.
!
! Revision 1.9.2.5  2005/09/01 21:44:46  yhan
! -- modified ROUTINE_NAME for Set_ChannelInfo_F1 and Set_ChannelInfo_F2
!
! Revision 1.9.2.4  2005/09/01 20:43:59  yhan
! --- Corrected the content of ROUTINE_NAME for Set_ChannelInfo_F1 and
!     Set_ChannelInfo_F2
!
! Revision 1.9.2.3  2005/09/01 20:34:57  yhan
! --- Modified document for function CRTM_Set_ChannelInfo
! --- Moved function IndexChannelInfo_NCEPID up close to IndexChannelInfo_DESC
!
! Revision 1.9.2.2  2005/08/23 12:51:35  yhan
! -- Renamed IndexChannelInfo_USER1() and IndexChannelInfo_USER2() to CRTM_Set_ChannelInfo_F1()
!    and CRTM_Set_ChannelInfo_F2() and moved them to a new generic interface block
!    CRTM_Set_ChannelInfo because in addition to indexing ChannelInfo these functions also
!    need to fill rest of the ChnnanelInfo structure.
! -- Added public subroutine CRTM_Fill_ChannelInfo() to fill the remaining unfilled structure
!    members for ChannelInfo, which may be called after the function CRTM_Index_ChannelInfo()
!    is called.
!
! Revision 1.9.2.1  2005/08/22 18:34:14  yhan
! -- Added the two functions IndexChannelInfo_USER1() and IndexChannelInfo_USER2() and
!    listed them in the generic interface block (Procedure overloading) CRTM_Index_ChannelInfo.
!    The two user-callable routines are to specify a subset of channels with a sensor descriptor
!    (IndexChannelInfo_USER1) or with a set of sensor descriptors and sensor_channel indexes.
! -- In the Module use, added "USE SpcCoeff_Define" and "USE CRTM_SpcCoeff" for the added two
!    functions to access the shared data SC and use the function Associated_SpcCoeff() defined
!    in SpcCoeff_Define.
!
! Revision 1.9  2005/02/16 15:45:21  paulv
! - Updated header documentation.
!
! Revision 1.8  2004/11/03 22:36:46  paulv
! - Intent of output structures in the Index() routines changed from (OUT)
!   to (IN OUT) to prevent memory leaks.
! - Added generic specification to the Index() routine END INTERFACE statement.
!
! Revision 1.7  2004/08/06 17:31:31  paulv
! - Updated header documentation.
!
! Revision 1.6  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.5  2004/06/29 20:09:04  paulv
! - Separated the definition and application code into separate modules.
!
! Revision 1.4  2004/06/24 18:59:39  paulv
! - Removed code that triggered an error in the indexing function if the
!   user inputs more channels than are available.
!
! Revision 1.3  2004/06/15 21:58:39  paulv
! - Corrected some minor indexing and declaration bugs.
!
! Revision 1.2  2004/06/15 21:43:41  paulv
! - Added indexing functions.
! - Renamed module from CRTM_ChannelInfo_Define.
!
! Revision 1.1  2004/05/19 19:55:18  paulv
! Initial checkin.
!
!
!
!
