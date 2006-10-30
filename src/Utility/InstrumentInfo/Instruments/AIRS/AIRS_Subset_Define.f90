!------------------------------------------------------------------------------
!M+
! NAME:
!       AIRS_Subset_Define
!
! PURPOSE:
!       Module containing the AIRS channel subset type definition and routins
!       to manipulate it.
!
! CATEGORY:
!       AIRS
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:              Module containing definitions for kinds
!                                of variable types.
!
!       Message_Handler:           Module to define simple error codes and
!                                handle error conditions
!                                USEs: FILE_UTILITY module
!
! CONTAINS:
!       Associated_AIRS_Subset:  Function to test the association status
!                                of the pointer members of an AIRS Subset
!                                structure.
!
!       Destroy_AIRS_Subset:     Function to destroy the AIRS_Subset
!                                data structure. 
!
!       Allocate_AIRS_Subset:    Function to allocate the AIRS_Subset
!                                data structure. 
!
!       Assign_AIRS_Subset:      Function to copy the AIRS_Subset
!                                data structure. 
!
! DERIVED TYPES:
!       AIRS_Subset_type:  Definition of the public structure used
!                          for channel subsetting of AIRS data.
!                          Fields are,
!
!         n_Channels:        Number of channels dimension
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!         Channel_Number:    The actual channel numbers for the subset
!                            channels.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Rank-1 (n_Channels)
!                            ATTRIBUTES: POINTER
!
!         Channel_Index:     The channel index values for the selected
!                            channels.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Rank-1 (n_Channels)
!                            ATTRIBUTES: POINTER
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Nov-2002
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

MODULE AIRS_Subset_Define

  ! ------------
  ! Module usage
  ! ------------

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
  PUBLIC :: Associated_AIRS_Subset
  PUBLIC :: Destroy_AIRS_Subset
  PUBLIC :: Allocate_AIRS_Subset
  PUBLIC :: Assign_AIRS_Subset


  ! -------------------
  ! Procedure overloads
  ! -------------------

  INTERFACE Destroy_AIRS_Subset
    MODULE PROCEDURE Destroy_scalar
    MODULE PROCEDURE Destroy_rank1
  END INTERFACE ! Destroy_AIRS_Subset


  ! -----------------
  ! Module Parameters
  ! -----------------

  ! -- The module version control ID string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: AIRS_Subset_Define.f90,v 1.6 2006/06/15 17:14:58 wd20pd Exp $'

  INTEGER, PRIVATE, PARAMETER :: SET = 1
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1


  ! --------------------------------
  ! AIRS_Subset data type definition
  ! --------------------------------

  TYPE, PUBLIC :: AIRS_Subset_type
    INTEGER :: n_Allocates = 0

    ! -- Dimensions
    INTEGER :: n_Channels = 0

    ! -- Channel subset inforamtion
    INTEGER, DIMENSION( : ), POINTER :: Channel_Number => NULL()
    INTEGER, DIMENSION( : ), POINTER :: Channel_Index  => NULL()
  END TYPE AIRS_Subset_type

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
!       Clear_AIRS_Subset
!
! PURPOSE:
!       Subroutine to clear the scalar members of a AIRS_Subset structure.
!
! CATEGORY:
!       AIRS
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_AIRS_Subset( Subset ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Subset:         AIRS Subset structure for which the scalar members have
!                       been cleared.
!                       UNITS:      N/A
!                       TYPE:       TYPE( AIRS_Subset_type )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
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
!       Note the INTENT on the output Subset argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_AIRS_Subset( Subset )

    TYPE( AIRS_Subset_type ), INTENT( IN OUT ) :: Subset

    Subset%n_Channels = 0

  END SUBROUTINE Clear_AIRS_Subset





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
!       Associated_AIRS_Subset
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       AIRS Subset structure.
!
! CATEGORY:
!       AIRS
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_AIRS_Subset( Subset,             &  ! Input
!                                                    ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       Subset:              AIRS Subset structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       TYPE( AIRS_Subset_type )
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            AIRS_Subset structure pointer members are associated.
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
!                            association status of the AIRS_Subset pointer members.
!                            .TRUE.  - if ALL the AIRS_Subset pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the AIRS_Subset pointer
!                                      members are associated.
!                            .FALSE. - some or all of the AIRS_Subset pointer
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
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_AIRS_Subset( Subset,    & ! Input
                                   ANY_Test ) & ! Optional input
                                 RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( AIRS_Subset_type ), INTENT( IN ) :: Subset

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN ) :: ANY_Test


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

      IF ( ASSOCIATED( Subset%Channel_Number ) .AND. &
           ASSOCIATED( Subset%Channel_Index  )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( Subset%Channel_Number ) .OR. &
           ASSOCIATED( Subset%Channel_Index  )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_AIRS_Subset





!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_AIRS_Subset
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of
!       AIRS_Subset data structures.
!
! CATEGORY:
!       AIRS
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_AIRS_Subset( Subset,                   &  ! Output
!                                           RCS_Id      = RCS_Id,     &  ! Optional output
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER( * )
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Subset:        Re-initialized AIRS_Subset structure.
!                      UNITS:      N/A
!                      TYPE:       TYPE( AIRS_Subset_type )
!                      DIMENSION:  Scalar
!                                    OR
!                                  Rank-1
!                      ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER( * )
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structure re-initialisation was successful
!                         == FAILURE - an error occurred, or
!                                    - the structure internal allocation counter
!                                      is not equal to zero (0) upon exiting this
!                                      function. This value is incremented and
!                                      decremented for every structure allocation
!                                      and deallocation respectively.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
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
!       Note the INTENT on the output Subset argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_scalar( Subset,       &  ! Output
                           No_Clear,     &  ! Optional input
                           RCS_Id,       &  ! Optional output
                           Message_Log ) &  ! Error messaging
                         RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( AIRS_Subset_type ), INTENT( IN OUT ) :: Subset

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Optional output
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_AIRS_Subset(scalar)'


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

    IF ( Clear ) CALL Clear_AIRS_Subset( Subset )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_AIRS_Subset( Subset ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the Channel_Number
    IF ( ASSOCIATED( Subset%Channel_Number ) ) THEN

      DEALLOCATE( Subset%Channel_Number, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( message, '( "Error deallocating AIRS_Subset Channel_Number ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Channel_Index
    IF ( ASSOCIATED( Subset%Channel_Index ) ) THEN

      DEALLOCATE( Subset%Channel_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( message, '( "Error deallocating AIRS_Subset Channel_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- DECREMENT AND TEST ALLOCATION COUNTER --                #
    !#--------------------------------------------------------------------------#

    Subset%n_Allocates = Subset%n_Allocates - 1

    IF ( Subset%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      Subset%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_scalar

  FUNCTION Destroy_rank1( Subset,       &  ! Output
                          No_Clear,     &  ! Optional input
                          RCS_Id,       &  ! Optional output
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( AIRS_Subset_type ), DIMENSION(:), INTENT( IN OUT ) :: Subset

    ! -- Optional input
    INTEGER,                      OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Optional output
    CHARACTER( * ),               OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_AIRS_Subset(rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: n
    INTEGER :: Scalar_Status



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
    !#                      -- LOOP OVER ARRAY ELEMENTS --                      #
    !#--------------------------------------------------------------------------#

    DO n = 1, SIZE( Subset )


      ! --------------------
      ! Call scalar function
      ! --------------------

      Scalar_Status = Destroy_scalar( Subset( n ), &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )

      ! -----------------------------
      ! Process error, but keep going 
      ! -----------------------------

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error deallocating AIRS_Subset element # ", i5 )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Destroy_rank1





!------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_AIRS_Subset
! 
! PURPOSE:
!       Function to allocate the pointer members of the AIRS_Subset
!       data structure.
!
! CATEGORY:
!       AIRS
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_AIRS_Subset( n_Channels,                &  ! Input
!                                            Subset,                    &  ! Output
!                                            RCS_Id      = RCS_Id,      &  ! Optional output
!                                            Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Channels:         Number of channels dimension.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in
!                           which any messages will be logged. If not
!                           specified, or if an error occurs opening
!                           the log file, the default action is to
!                           output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Subset:             AIRS_Subset structure with allocated
!                           pointer members
!                           UNITS:      N/A
!                           TYPE:       TYPE( AIRS_Subset_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:        The return value is an integer defining the error status.
!                            The error codes are defined in the Message_Handler module.
!                            If == SUCCESS the structure re-initialisation was successful
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
!       Note the INTENT on the output Subset argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Allocate_AIRS_Subset( n_Channels,   &  ! Input
                                 Subset,       &  ! Output
                                 RCS_Id,       &  ! Optional output
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
    TYPE( AIRS_Subset_type ), INTENT( IN OUT ) :: Subset

    ! -- Optional output
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_AIRS_Subset'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

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

    ! ----------
    ! Dimensions
    ! ----------

    IF ( n_Channels  < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input AIRS_Subset channel dimension must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_AIRS_Subset( Subset, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_AIRS_Subset( Subset, &
                                          No_Clear = SET, &
                                          Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating Subset pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( Subset%Channel_Number( n_Channels ), &
              Subset%Channel_Index( n_Channels ), &

              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( message, '( "Error allocating AIRS_Subset data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#             -- ASSIGN THE DIMENSION AND INITALISE ARRAYS --              #
    !#--------------------------------------------------------------------------#

    Subset%n_Channels = n_Channels

    Subset%Channel_Number = INVALID
    Subset%Channel_Index  = INVALID



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    Subset%n_Allocates = Subset%n_Allocates + 1

    IF ( Subset%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      Subset%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_AIRS_Subset





!------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_AIRS_Subset
!
! PURPOSE:
!       Function to copy valid AIRS_Subset structures.
!
! CATEGORY:
!       AIRS
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_AIRS_Subset( Subset_in,                &  ! Input
!                                          Subset_out,               &  ! Output
!                                          RCS_Id      = RCS_Id,     &  ! Revision control
!                                          Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Subset_in:         AIRS_Subset structure which is to be copied.
!                          UNITS:      N/A
!                          TYPE:       TYPE( AIRS_Subset_type )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER( * )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Subset_out:        Copy of the input structure, Subset_in.
!                          UNITS:      N/A
!                          TYPE:       TYPE( AIRS_Subset_type )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER( * )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the Message_Handler module.
!                          If == SUCCESS the structure assignment was successful
!                             == FAILURE an error occurred
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! CALLS:
!       Allocate_AIRS_Subset:    Function to allocate the pointer members of
!                                the AIRS_Subset data structure.
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output Subset argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Assign_AIRS_Subset( Subset_in,    &  ! Input
                               Subset_out,   &  ! Output
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
    TYPE( AIRS_Subset_type ), INTENT( IN )     :: Subset_in

    ! -- Output
    TYPE( AIRS_Subset_type ), INTENT( IN OUT ) :: Subset_out

    ! -- Revision contorl
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_AIRS_Subset'



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

    IF ( .NOT. Associated_AIRS_Subset( Subset_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT Subset pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! Assign array data
    ! -----------------

    ! -- Allocate data arrays
    Error_Status = Allocate_AIRS_Subset( Subset_in%n_Channels, &
                                         Subset_out, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output AIRS_Subset arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Copy array data
    Subset_out%Channel_Number = Subset_in%Channel_Number
    Subset_out%Channel_Index  = Subset_in%Channel_Index

  END FUNCTION Assign_AIRS_Subset

END MODULE AIRS_Subset_Define


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: AIRS_Subset_Define.f90,v 1.6 2006/06/15 17:14:58 wd20pd Exp $
!
! $Date: 2006/06/15 17:14:58 $
!
! $Revision: 1.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: AIRS_Subset_Define.f90,v $
! Revision 1.6  2006/06/15 17:14:58  wd20pd
! Replaced all references to Error_Handler with Message_Handler.
!
! Revision 1.5  2004/09/09 19:28:59  paulv
! - Intent of Subset dummy arguments changed from OUT to IN OUT to prevent memory leaks.
! - Added optional No_Clear argument to Destroy_Subset() function.
! - Call to Destroy_Subset() added to Allocate_Subset() function for the case
!   where the input Subset argument is already allocated.
! - Made Associated_AIRS_Subset() function PUBLIC.
!
! Revision 1.4  2004/08/11 12:05:45  paulv
! - Reorganising and f90->f95 conversion. Incomplete.
!
! Revision 1.3  2004/08/10 16:53:55  paulv
! - Modified for f90->f95 conversion.
!
! Revision 1.2  2003/11/21 14:59:48  paulv
! - Replaced old, crappy AIRS_Channel_Index routines and derived types with
!   the AIRS_Subset derived type and it's associated initialisation and
!   destruction functions.
! - Added Index_AIRS_Subset() function to return the required AIRS channel
!   indices for the subset channels iun a given module.
!
! Revision 1.1  2002/11/27 15:01:24  paulv
! Initial checkin.
!
!
!
!
