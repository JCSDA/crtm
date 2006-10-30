!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Cloud_Define
!
! PURPOSE:
!       Module defining the CRTM_Cloud structure and containing routines to 
!       manipulate it.
!       
! CATEGORY:
!       CRTM : Cloud
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       USE CRTM_Cloud_Define
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
!       CRTM_Init_Cloud:        Subroutine to initialize an Cloud
!                               structure.
!
!       CRTM_Destroy_Cloud:     Function to re-initialize an Cloud
!                               structure.
!
!       CRTM_Allocate_Cloud:    Function to allocate the pointer members
!                               of an Cloud structure.
!
!       CRTM_Assign_Cloud:      Function to copy an Cloud structure.
!
!
! DERIVED TYPES:
!
!       *!IMPORTANT!*
!       -------------
!       Note that the CRTM_Cloud_type is PUBLIC and its members are not
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
!       Written by:     Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       20-Feb-2004
!
!  Copyright (C) 2004 Yong Han, Quanhua Liu, Paul van Delst
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

MODULE CRTM_Cloud_Define_old


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
  PUBLIC :: CRTM_Init_Cloud
  PUBLIC :: CRTM_Destroy_Cloud
  PUBLIC :: CRTM_Allocate_Cloud
  PUBLIC :: CRTM_Assign_Cloud


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE CRTM_Init_Cloud
    MODULE PROCEDURE Initialize_Cloud_Scalar
    MODULE PROCEDURE Initialize_Cloud_Rank1
  END INTERFACE ! CRTM_Init_Cloud

  INTERFACE CRTM_Destroy_Cloud
    MODULE PROCEDURE Destroy_Cloud_Scalar
    MODULE PROCEDURE Destroy_Cloud_Rank1
  END INTERFACE ! CRTM_Destroy_Cloud

  INTERFACE CRTM_Allocate_Cloud
    MODULE PROCEDURE Allocate_Cloud_Scalar
    MODULE PROCEDURE Allocate_Cloud_Rank1
  END INTERFACE ! CRTM_Allocate_Cloud

  INTERFACE CRTM_Assign_Cloud
    MODULE PROCEDURE Assign_Cloud_Scalar
    MODULE PROCEDURE Assign_Cloud_Rank1
  END INTERFACE ! CRTM_Assign_Cloud


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Cloud_Define.f90,v 1.4 2004/07/01 20:52:51 paulv Exp $'

  ! -- Cloud scalar member invalid value
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  INTEGER, PUBLIC, PARAMETER :: N_VALID_CLOUD_TYPES = 6

  INTEGER, PUBLIC, PARAMETER ::      NO_CLOUD = 0
  INTEGER, PUBLIC, PARAMETER ::   WATER_CLOUD = 1
  INTEGER, PUBLIC, PARAMETER ::     ICE_CLOUD = 2
  INTEGER, PUBLIC, PARAMETER ::    RAIN_CLOUD = 3
  INTEGER, PUBLIC, PARAMETER ::    SNOW_CLOUD = 4
  INTEGER, PUBLIC, PARAMETER :: GRAUPEL_CLOUD = 5
  INTEGER, PUBLIC, PARAMETER ::    HAIL_CLOUD = 6

  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_CLOUD_TYPES ) :: &
    CLOUD_TYPE_NAME = (/ 'None   ', &
                         'Water  ', &
                         'Ice    ', &
                         'Rain   ', &
                         'Snow   ', &
                         'Graupel', &
                         'Hail   ' /)


  ! --------------------------
  ! Cloud data type definition
  ! --------------------------

  TYPE, PUBLIC :: CRTM_Cloud_type
    INTEGER :: n_Allocates

    ! -- Dimension values
    INTEGER :: n_Layers    ! K dimension.

    ! -- Cloud type
    INTEGER :: Type

    ! -- Particle size distribution parameters
    REAL( fp_kind ), DIMENSION( : ), POINTER :: Effective_Radius    ! K. Units are microns
    REAL( fp_kind ), DIMENSION( : ), POINTER :: Effective_Variance  ! K. Units are Dimensionless

    ! -- Cloud state variables
    REAL( fp_kind ), DIMENSION( : ), POINTER :: Water_Content       ! K. Units are g/m^2

  END TYPE CRTM_Cloud_type


  ! ----------
  ! Intrinsics
  ! ----------

  INTRINSIC ASSOCIATED, &
            PRESENT, &
            TRIM


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
!       CRTM_Clear_Cloud
!
! PURPOSE:
!       Subroutine to clear the scalar members of a Cloud structure.
!
! CATEGORY:
!       CRTM : Cloud
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_Cloud( Cloud ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Cloud:       Cloud structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       CRTM_Cloud_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( OUT )
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_Cloud( Cloud )

    TYPE( CRTM_Cloud_type ), INTENT( OUT ) :: Cloud

    Cloud%n_Layers      = 0
    Cloud%Type          = NO_CLOUD

  END SUBROUTINE CRTM_Clear_Cloud





!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Associated_Cloud
!
! PURPOSE:
!       Function to test the association status of a CRTM_Cloud structure.
!
! CATEGORY:
!       CRTM : Cloud
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_Cloud( Cloud,              &  ! Input
!                                                   ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       Cloud:               Cloud structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_Cloud_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            Cloud structure pointer members are associated.
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
!                            association status of the Cloud pointer members.
!                            .TRUE.  - if ALL the Cloud pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the Cloud pointer
!                                      members are associated.
!                            .FALSE. - some or all of the Cloud pointer
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
!       This function tests the association status of the Cloud
!       structure pointer members. Therefore this function must only
!       be called after the input Cloud structure has, at least,
!       had its pointer members initialized.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_Cloud( Cloud,     & ! Input
                                  ANY_Test ) & ! Optional input
                                RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Cloud_type ), INTENT( IN ) :: Cloud

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

      IF ( ASSOCIATED( Cloud%Effective_Radius   ) .AND. &
           ASSOCIATED( Cloud%Effective_Variance ) .AND. &
           ASSOCIATED( Cloud%Water_Content      )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( Cloud%Effective_Radius   ) .OR. &
           ASSOCIATED( Cloud%Effective_Variance ) .OR. &
           ASSOCIATED( Cloud%Water_Content      )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION CRTM_Associated_Cloud





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
!       CRTM_Init_Cloud
! 
! PURPOSE:
!       Function to initialize CRTM_Cloud structures.
!
! CATEGORY:
!       CRTM : Cloud
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       CALL CRTM_Init_Cloud( Cloud,          &  ! Output
!                             RCS_Id = RCS_Id )  ! Revision control
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Cloud:        Initialized Cloud structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar
!                                   OR
!                                 Rank1 array
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This subroutine nullifies the Cloud structure pointer members.
!       Therefore, this function should *only* be called to initialise
!       Cloud structures before their *first* use. Subsequent
!       re-initialisations should be done using the cRTM_Destroy_Cloud()
!       function.
!       
! PROCEDURE:
!       The scalar structure members are reset to a null value and the 
!       pointer members are nullified.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Initialize_Cloud_Scalar( Cloud, &  ! Output
                                      RCS_Id )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( CRTM_Cloud_type ),  INTENT( OUT ) :: Cloud

    ! -- Revision control
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

    Cloud%n_Allocates = 0


    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    CALL CRTM_Clear_Cloud( Cloud )


    ! ---------------------------
    ! Nullify the pointer members
    ! ---------------------------

    NULLIFY( Cloud%Effective_Radius, &
             Cloud%Effective_Variance, &
             Cloud%Water_Content )

  END SUBROUTINE Initialize_Cloud_Scalar


  SUBROUTINE Initialize_Cloud_Rank1( Cloud, &  ! Output
                                     RCS_Id )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( CRTM_Cloud_type ), DIMENSION( : ), INTENT( OUT ) :: Cloud

    ! -- Revision control
    CHARACTER( * ),          OPTIONAL,       INTENT( OUT ) :: RCS_Id


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: n



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

    DO n = 1, SIZE( Cloud )

      CALL Initialize_Cloud_Scalar( Cloud(n) )

    END DO

  END SUBROUTINE Initialize_Cloud_Rank1





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy_Cloud
! 
! PURPOSE:
!       Function to re-initialize CRTM_Cloud structures.
!
! CATEGORY:
!       CRTM : Cloud
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_Cloud( Cloud,                    &  ! Output
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
!       Cloud:        Re-initialized Cloud structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar
!                                   OR
!                                 Rank1 array
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
!       Display_Message:    Subroutine to output messages
!                           SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function checks the association status of the Cloud structure
!       pointer members. Therefore, this function should *only* be called
!       *after* the Cloud structure has been initialised via the
!       CRTM_Init_Cloud() subroutine.
!
!       This restriction is due to Fortran-90 not providing any mechanism
!       for initialising pointer association status in derived type definitions.
!       This means the association status of the Cloud structure pointer
!       members will be undefined until they are initialised (via the
!       CRTM_Init_Cloud() subroutine).
!
! PROCEDURE:
!       The scalar structure members are set to a null value and the 
!       pointer members are deallocated.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Destroy_Cloud_Scalar( Cloud,        &  ! Output
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
    TYPE( CRTM_Cloud_type ),  INTENT( IN OUT ) :: Cloud

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Cloud(Scalar)'


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
    !#                       -- PERFORM REINITIALISATION --                     #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    CALL CRTM_Clear_Cloud( Cloud )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. CRTM_Associated_Cloud( Cloud ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the Effective_Radius profile
    IF ( ASSOCIATED( Cloud%Effective_Radius ) ) THEN

      DEALLOCATE( Cloud%Effective_Radius, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Cloud Effective_Radius ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Effective_Variance profile
    IF ( ASSOCIATED( Cloud%Effective_Variance ) ) THEN

      DEALLOCATE( Cloud%Effective_Variance, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Cloud Effective_Variance ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the water content profile
    IF ( ASSOCIATED( Cloud%Water_Content ) ) THEN

      DEALLOCATE( Cloud%Water_Content, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Cloud Water_Content ", &
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

    Cloud%n_Allocates = Cloud%n_Allocates - 1

    IF ( Cloud%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      Cloud%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Cloud_Scalar


  FUNCTION Destroy_Cloud_Rank1( Cloud,        &  ! Output
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
    TYPE( CRTM_Cloud_type ), DIMENSION( : ), INTENT( IN OUT ) :: Cloud

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Cloud(Rank-1)'


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

    DO n = 1, SIZE( Cloud )

      Scalar_Status = Destroy_Cloud_Scalar( Cloud(n), &
                                            Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i5, &
                          &" of Cloud structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Destroy_Cloud_Rank1





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Allocate_Cloud
! 
! PURPOSE:
!       Function to allocate CRTM_Cloud structures.
!
! CATEGORY:
!       CRTM : Cloud
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_Cloud( n_Layers,  &  ! Input
!                                           Cloud,     &  ! Output
!                                           RCS_Id = RCS_Id,          &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Layers:     Number of layers for which there is cloud data.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                                   OR
!                                 Rank1 array
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
!       Cloud:        Cloud structure with allocated pointer members
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Same as input n_Layers
!                     ATTRIBUTES: INTENT( OUT )
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
!                     The error codes are defined in the ERROR_HANDLER module.
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
!       CRTM_Associated_Cloud:   Function to test the association status of
!                                a CRTM_Cloud structure.
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function checks the association status of the Cloud structure
!       pointer members. Therefore, this function should *only* be called
!       *after* the Cloud structure has been initialised via the
!       CRTM_Init_Cloud() subroutine.
!
!       This restriction is due to Fortran-90 not providing any mechanism
!       for initialising pointer association status in derived type definitions.
!       This means the association status of the Cloud structure pointer
!       members will be undefined until they are initialised (via the 
!       CRTM_Init_Cloud() subroutine).
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Allocate_Cloud_Scalar( n_Layers,     &  ! Input

                                  Cloud,        &  ! Output

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
    INTEGER,                  INTENT( IN )  :: n_Layers

    ! -- Output
    TYPE( CRTM_Cloud_type ),  INTENT( OUT ) :: Cloud

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Cloud(Scalar)'


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

    ! ---------------
    ! Layer dimension
    ! ---------------

    IF ( n_Layers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------------------
    ! Check if ANY pointers are already associated
    ! --------------------------------------------

    IF ( CRTM_Associated_Cloud( Cloud, ANY_Test = SET ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'One or more CRTM_Cloud pointer members are associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( Cloud%Effective_Radius( n_Layers ), &
              Cloud%Effective_Variance( n_Layers ), &
              Cloud%Water_Content( n_Layers ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating CRTM_Cloud data arrays. STAT = ", i5 )' ) &
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

    Cloud%n_Layers = n_Layers

    Cloud%Effective_Radius   = REAL( INVALID, fp_kind )
    Cloud%Effective_Variance = REAL( INVALID, fp_kind )
    Cloud%Water_Content      = REAL( INVALID, fp_kind )



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    Cloud%n_Allocates = Cloud%n_Allocates + 1

    IF ( Cloud%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      Cloud%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Cloud_Scalar


  FUNCTION Allocate_Cloud_Rank1( n_Layers,     &  ! Input

                                 Cloud,        &  ! Output

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
    INTEGER,                 DIMENSION( : ), INTENT( IN )  :: n_Layers

    ! -- Output
    TYPE( CRTM_Cloud_type ), DIMENSION( : ), INTENT( OUT ) :: Cloud

    ! -- Revision control
    CHARACTER( * ),          OPTIONAL,       INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),          OPTIONAL,       INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Cloud(Rank-1)'


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
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    n = SIZE( n_Layers )

    IF ( SIZE( Cloud ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers and CRTM_Cloud arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Cloud_Scalar( n_Layers(i), &
                                             Cloud(i), &
                                             Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Cloud structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Cloud_Rank1





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Assign_Cloud
!
! PURPOSE:
!       Function to copy valid CRTM_Cloud structures.
!
! CATEGORY:
!       CRTM : Cloud
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_Cloud( Cloud_in,  &  ! Input
!                                         Cloud_out, &  ! Output
!                                         RCS_Id = RCS_Id,          &  ! Revision control
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Cloud_in:      Cloud structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
!                      DIMENSION:  Scalar
!                                    OR
!                                  Rank1 array
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
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
!       Cloud_out:     Copy of the input structure, Cloud_in.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
!                      DIMENSION:  Same size as Cloud_in
!                      ATTRIBUTES: INTENT( OUT )
!
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
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structure assignment was successful
!                         == FAILURE an error occurred
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Associated_Cloud:    Function to test the association status of
!                                 a CRTM_Cloud structure.
!
!       CRTM_Allocate_Cloud:      Function to allocate a CRTM_Cloud structure.
!
!       Display_Message:          Subroutine to output messages
!                                 SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function allocates the output Cloud structure pointer members.
!       Therefore this function should *only* be called *after* the output
!       Cloud structure has been initialised via the CRTM_Init_Cloud()
!       subroutine or re-initialised via the CRTM_Destroy_Cloud() function.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Assign_Cloud_Scalar( Cloud_in,     &  ! Input
                                Cloud_out,    &  ! Output
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
    TYPE( CRTM_Cloud_type ),  INTENT( IN )  :: Cloud_in

    ! -- Output
    TYPE( CRTM_Cloud_type ),  INTENT( OUT ) :: Cloud_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Cloud(Scalar)'



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
    !#           -- TEST THE STRUCTURE ARGUMENT POINTER ASSOCIATION --          #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------------------------
    ! ALL *input* pointers must be associated.
    ! If this test fails, the assumption is that
    ! ALL of the pointers are NOT associated,
    ! not just some of them. Thus, do nothing.
    !
    ! This is done to accomodate use of the CRTM_Cloud
    ! structure withing the CRTM_Atmosphere structure,
    ! specifically relating to the CRTM_Assign_Atmosphere()
    ! function. There had to be a way to handle the case
    ! where the Cloud structure components of the
    ! CRTM_Atmosphere  structure were not (yet) associated.
    ! -----------------------------------------------------

    IF ( .NOT. CRTM_Associated_Cloud( Cloud_In ) ) RETURN


    ! --------------------------------------------
    ! ANY *output* pointers must NOT be associated
    ! --------------------------------------------

    IF ( CRTM_Associated_Cloud( Cloud_Out, ANY_Test = SET ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all OUTPUT CRTM_Cloud pointer '//&
                            'members are associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------

    Cloud_out%Type = Cloud_in%Type


    ! -----------------
    ! Assign array data
    ! -----------------

    ! -- Allocate data arrays
    Error_Status = CRTM_Allocate_Cloud( Cloud_in%n_Layers, &
                                        Cloud_out, &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output CRTM_Cloud arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Copy array data
    Cloud_out%Effective_Radius   = Cloud_in%Effective_Radius  
    Cloud_out%Effective_Variance = Cloud_in%Effective_Variance
    Cloud_out%Water_Content      = Cloud_in%Water_Content

  END FUNCTION Assign_Cloud_Scalar


  FUNCTION Assign_Cloud_Rank1( Cloud_in,     &  ! Input
                               Cloud_out,    &  ! Output
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
    TYPE( CRTM_Cloud_type ), DIMENSION( : ), INTENT( IN )  :: Cloud_in

    ! -- Output
    TYPE( CRTM_Cloud_type ), DIMENSION( : ), INTENT( OUT ) :: Cloud_out

    ! -- Revision control
    CHARACTER( * ),          OPTIONAL,       INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),          OPTIONAL,       INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Cloud(Rank-1)'


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

    n = SIZE( Cloud_in )

    IF ( SIZE( Cloud_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Cloud_in and Cloud_out arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Assign_Cloud_Scalar( Cloud_in(i), &
                                           Cloud_out(i), &
                                           Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i5, &
                          &" of CRTM_Cloud structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Assign_Cloud_Rank1

END MODULE CRTM_Cloud_Define_old


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: CRTM_Cloud_Define.f90,v 1.4 2004/07/01 20:52:51 paulv Exp $
!
! $Date: 2004/07/01 20:52:51 $
!
! $Revision: 1.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_Cloud_Define.f90,v $
! Revision 1.4  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.3  2004/06/14 14:50:25  paulv
! - Removed pressure and temperature arrays from Cloud definition. It is now
!   assumed that the values in the parent Atmosphere structure correspond to
!   the same layering as that in the Cloud structure.
! - Replaced the particle size component with the effective radius and
!   effective variance to allow not only some measure of the cloud particle
!   size, but also the distribution.
!
! Revision 1.2  2004/06/04 20:11:23  paulv
! - Altered components for specification of the cloud particle size
!   distribution. Removed Particle_Size and replaced it with
!   Effective_Radius and Effective_Variance.
!
! Revision 1.1  2004/05/19 19:55:18  paulv
! Initial checkin.
!
!
!
!
