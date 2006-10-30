!------------------------------------------------------------------------------
!M+
! NAME:
!       pCRTMstats_Define
!
! PURPOSE:
!       Module defining the pCRTMstats data structure and containing
!       routines to manipulate it.
!       
! CATEGORY:
!       pCRTM : pCRTMstats
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE pCRTMstats_Define
!
! MODULES:
!       Type_Kinds:               Module containing definitions for kinds
!                                 of variable types.
!
!       Message_Handler:          Module to define simple error codes and
!                                 handle error conditions
!                                 USEs: FILE_UTILITY module
!
! CONTAINS:
!       Associated_pCRTMstats:    Function to test the association status
!                                 of the pointer members of a pCRTMstats
!                                 structure.
!
!       Destroy_pCRTMstats:       Function to re-initialize an pCRTMstats
!                                 structure.
!
!       Allocate_pCRTMstats:      Function to allocate the pointer members
!                                 of an pCRTMstats structure.
!
!       Assign_pCRTMstats:        Function to copy an pCRTMstats structure.
!
!       Concatenate_pCRTMstats:   Function to concatenate two pCRTMstats
!                                 structures along the channel dimension.
!
!       Compute_pCRTMstats:       Function to compute the statistics for
!                                 a pCRTMstats data structure.
!
!       Count_pCRTMstats_Sensors: Subroutine to count the number of
!                                 different satellites/sensors in the
!                                 pCRTMstats data structure.
!
!       Information_pCRTMstats:   Subroutine to return a string containing
!                                 information about the pCRTMstats data structure.
!
! DERIVED TYPES:
!       pCRTMstats_type:  Definition of the public pCRTMstats data structure.
!                         Fields are,
!
!         LBL_Profile_ID_Tag:  Character string containing a short tag
!                              to identify the profile set used in the
!                              line-by-line (LBL) transmittance
!                              calculations.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( 16 )
!                              DIMENSION:  Scalar
!
!         REG_Profile_ID_Tag:  Character string containing a short tag
!                              to identify the profile set used to generate
!                              the regression transmittance coefficients.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( 16 )
!                              DIMENSION:  Scalar
!
!         n_Channels:          Number of spectral channels.
!                              "L" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
!         n_Angles:            Number of view angles.
!                              "I" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
!         n_Profiles:          Number of atmospheric profiles.
!                              "M" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
!         n_Molecule_Sets:     Number of individual or mixed
!                              gaseous absorbers.
!                              "J" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
!         NCEP_Sensor_ID:      An "in-house" value used at NOAA/NCEP/EMC 
!                              to identify a satellite/sensor combination.
!                              UNITS:      N/A
!                              TYPE:       Rank-1 (n_Channels, L)
!                              DIMENSION:  Scalar
!
!         WMO_Satellite_ID:    The WMO code for identifying satellite
!                              platforms. Taken from the WMO common
!                              code tables at:
!                                http://www.wmo.ch/web/ddbs/Code-tables.html
!                              The Satellite ID is from Common Code
!                              table C-5, or code table 0 01 007 in BUFR
!                              UNITS:      N/A
!                              TYPE:       Rank-1 (n_Channels, L)
!                              DIMENSION:  Scalar
!
!         WMO_Sensor_ID:       The WMO code for identifying a satelite
!                              sensor. Taken from the WMO common
!                              code tables at:
!                                http://www.wmo.ch/web/ddbs/Code-tables.html
!                              The Sensor ID is from Common Code
!                              table C-8, or code table 0 02 019 in BUFR
!                              UNITS:      N/A
!                              TYPE:       Rank-1 (n_Channels, L)
!                              DIMENSION:  Scalar
!
!         Sensor_Channel:      This is the sensor channel number associated
!                              with the transmittance profiles. Helps
!                              in identifying channels where the numbers are
!                              not contiguous (e.g. AIRS).
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels, L)
!                              ATTRIBUTES: POINTER
!
!         Frequency:           The central frequency for the sensor channels.
!                              UNITS:      Inverse centimetres, cm^-1
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels, L)
!                              ATTRIBUTES: POINTER
!
!         Angle:               Array containing the view angles associated
!                              with the transmittance profiles.
!                              UNITS:      Degrees from vertical.
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-1 (n_Angles, I)
!                              ATTRIBUTES: POINTER
!
!         Profile:             Array containing the atmospheric profile
!                              index number used in the calculation of
!                              the transmittance profiles..
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Profiles, M)
!                              ATTRIBUTES: POINTER
!
!         Molecule_Set:        Array containing the molecule set ID s
!                              associated with the transmittance
!                              profiles.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Molecule_Sets, J)
!                              ATTRIBUTES: POINTER
!
!         LBL_Tau:             Array containing the surface (SFC) to
!                              top-of-atmosphere (TOA) transmittance
!                              from the line-by-line (LBL) calculations.
!                              UNITS:      None.
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-4 (L x I x M x J)
!                              ATTRIBUTES: POINTER
!                                                     
!         REG_Tau:             Array containing the surface (SFC) to
!                              top-of-atmosphere (TOA) transmittance
!                              from the regression (REG) transmittance
!                              model calculations.
!                              UNITS:      None.
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-4 (L x I x M x J)
!                              ATTRIBUTES: POINTER
!
!         Mean_dTau:           Array containing the mean LBL-REG transmittance
!                              difference over all angles and profiles.
!                              UNITS:      None.
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-2 (L x J)
!                              ATTRIBUTES: POINTER
!
!         RMS_dTau:            Array containing the RMS LBL-REG transmittance
!                              difference over all angles and profiles.
!                              UNITS:      None.
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-2 (L x J)
!                              ATTRIBUTES: POINTER
!
!         Mean_dTau_by_Angle:  Array containing the mean LBL-REG transmittance
!                              difference for each individual angle over
!                              all profiles.
!                              UNITS:      None.
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-3 (L x I x J)
!                              ATTRIBUTES: POINTER
!
!         RMS_dTau_by_Angle:   Array containing the RMS LBL-REG transmittance
!                              difference for each individual angle over
!                              all profiles.
!                              UNITS:      None.
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-3 (L x I x J)
!                              ATTRIBUTES: POINTER
!
!         LBL_BT:              Array containing the TOA brightness
!                              temperatures resulting from the radiative
!                              transfer calculations using the LBL
!                              transmittances.
!                              UNITS:      Kelvin, K
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-4 (L x I x M x J)
!                              ATTRIBUTES: POINTER
!                                                     
!         REG_BT:              Array containing the TOA brightness
!                              temperatures resulting from the radiative
!                              transfer calculations using the regression
!                              transmittance model.
!                              UNITS:      Kelvin, K
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-4 (L x I x M x J)
!                              ATTRIBUTES: POINTER
!
!         Mean_dBT:            Array containing the mean LBL-REG brightness
!                              temperature difference over all angles and
!                              profiles.
!                              UNITS:      Kelvin, K
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-2 (L x J)
!                              ATTRIBUTES: POINTER
!
!         RMS_dBT:             Array containing the RMS LBL-REG brightness
!                              temperature difference over all angles and
!                              profiles.
!                              UNITS:      Kelvin, K
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-2 (L x J)
!                              ATTRIBUTES: POINTER
!
!         Mean_dBT_by_Angle:   Array containing the mean LBL-REG brightness
!                              temperature difference for each individual
!                              angle over all profiles.
!                              UNITS:      Kelvin, K
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-3 (L x I x J)
!                              ATTRIBUTES: POINTER
!
!         RMS_dBT_by_Angle:    Array containing the RMS LBL-REG brightness
!                              temperature difference for each individual
!                              angle over all profiles.
!                              UNITS:      Kelvin, K
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-3 (L x I x J)
!                              ATTRIBUTES: POINTER
!
!       *!IMPORTANT!*
!       -------------
!       Note that the pCRTMstats_type is PUBLIC and its members are not
!       encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user destroy,
!       allocate, assign, and concatenate the structure using only the
!       routines in this module where possible to eliminate -- or at
!       least minimise -- the possibility of memory leakage since most
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
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jan-2004
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

MODULE pCRTMstats_Define


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
  PUBLIC :: Associated_pCRTMstats
  PUBLIC :: Destroy_pCRTMstats
  PUBLIC :: Allocate_pCRTMstats
  PUBLIC :: Assign_pCRTMstats
  PUBLIC :: Concatenate_pCRTMstats
  PUBLIC :: Compute_pCRTMstats
  PUBLIC :: Count_pCRTMstats_Sensors
  PUBLIC :: Information_pCRTMstats



  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: pCRTMstats_Define.f90,v 1.6 2006/05/02 14:58:35 dgroff Exp $'

  ! -- pCRTMstats invalid value
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


  ! -------------------------------
  ! pCRTMstats data type definition
  ! -------------------------------

  TYPE, PUBLIC :: pCRTMstats_type
    INTEGER :: n_Allocates = 0

    ! -- Profile ID descriptors
    CHARACTER( 16 ) :: LBL_Profile_ID_Tag = ' '
    CHARACTER( 16 ) :: REG_Profile_ID_Tag = ' '

    ! -- Dimensions
    INTEGER :: n_Channels      = 0 ! == L
    INTEGER :: n_Angles        = 0 ! == I
    INTEGER :: n_Profiles      = 0 ! == M
    INTEGER :: n_Molecule_Sets = 0 ! == J

    ! -- The number of satellite/sensors
    INTEGER :: n_Sensors = 0

    ! -- Sensor/satellite IDs
    INTEGER,         DIMENSION( : ), POINTER :: NCEP_Sensor_ID   => NULL()  ! L
    INTEGER,         DIMENSION( : ), POINTER :: WMO_Satellite_ID => NULL()  ! L
    INTEGER,         DIMENSION( : ), POINTER :: WMO_Sensor_ID    => NULL()  ! L

    ! -- Dimension descriptors
    INTEGER,         DIMENSION( : ), POINTER :: Sensor_Channel => NULL()  ! L
    REAL( fp_kind ), DIMENSION( : ), POINTER :: Frequency      => NULL()  ! L
    REAL( fp_kind ), DIMENSION( : ), POINTER :: Angle          => NULL()  ! I
    INTEGER,         DIMENSION( : ), POINTER :: Profile        => NULL()  ! M
    INTEGER,         DIMENSION( : ), POINTER :: Molecule_Set   => NULL()  ! J

    ! -- Transmittance data
    REAL( fp_kind ), DIMENSION( :, :, :, : ), POINTER :: LBL_Tau            => NULL()  ! L x I x M x J
    REAL( fp_kind ), DIMENSION( :, :, :, : ), POINTER :: REG_Tau            => NULL()  ! L x I x M x J
    REAL( fp_kind ), DIMENSION( :, : ),       POINTER :: Mean_dTau          => NULL()  ! L x J
    REAL( fp_kind ), DIMENSION( :, : ),       POINTER ::  RMS_dTau          => NULL()  ! L x J
    REAL( fp_kind ), DIMENSION( :, :, : ),    POINTER :: Mean_dTau_by_Angle => NULL()  ! L x I x J
    REAL( fp_kind ), DIMENSION( :, :, : ),    POINTER ::  RMS_dTau_by_Angle => NULL()  ! L x I x J

    ! -- Brightness temperature data
    REAL( fp_kind ), DIMENSION( :, :, :, : ), POINTER :: LBL_BT            => NULL()  ! L x I x M x J
    REAL( fp_kind ), DIMENSION( :, :, :, : ), POINTER :: REG_BT            => NULL()  ! L x I x M x J
    REAL( fp_kind ), DIMENSION( :, : ),       POINTER :: Mean_dBT          => NULL()  ! L x J
    REAL( fp_kind ), DIMENSION( :, : ),       POINTER ::  RMS_dBT          => NULL()  ! L x J
    REAL( fp_kind ), DIMENSION( :, :, : ),    POINTER :: Mean_dBT_by_Angle => NULL()  ! L x I x J
    REAL( fp_kind ), DIMENSION( :, :, : ),    POINTER ::  RMS_dBT_by_Angle => NULL()  ! L x I x J
  END TYPE pCRTMstats_type


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
!       Clear_pCRTMstats
!
! PURPOSE:
!       Subroutine to clear the scalar members of a pCRTMstats structure.
!
! CATEGORY:
!       pCRTM : pCRTMstats
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_pCRTMstats( pCRTMstats ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       pCRTMstats:  pCRTMstats structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       pCRTMstats_type
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
!       Note the INTENT on the output pCRTMstats argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jan-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_pCRTMstats( pCRTMstats )

    TYPE( pCRTMstats_type ), INTENT( IN OUT ) :: pCRTMstats

    pCRTMstats%LBL_Profile_ID_Tag = ' '
    pCRTMstats%REG_Profile_ID_Tag = ' '
    
    pCRTMstats%n_Channels      = 0
    pCRTMstats%n_Angles        = 0
    pCRTMstats%n_Profiles      = 0
    pCRTMstats%n_Molecule_Sets = 0

    pCRTMstats%n_Sensors = 0

  END SUBROUTINE Clear_pCRTMstats





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
!       Associated_pCRTMstats
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       pCRTMstats structure.
!
! CATEGORY:
!       pCRTM : pCRTMstats
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_pCRTMstats( pCRTMstats,         &  ! Input
!                                                   ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       pCRTMstats:  pCRTMstats structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       pCRTMstats_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                    pCRTMstats structure pointer members are associated.
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
!                            association status of the pCRTMstats pointer
!                            members.
!                            .TRUE.  - if ALL the pCRTMstats pointer members
!                                      are associated, or if the ANY_Test argument
!                                      is set and ANY of the pCRTMstats pointer
!                                      members are associated.
!                            .FALSE. - some or all of the pCRTMstats pointer
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
!       This function tests the association status of the pCRTMstats
!       structure pointer members. Therefore this function must only
!       be called after the input pCRTMstats structure has, at least,
!       had its pointer members initialized.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jan-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_pCRTMstats( pCRTMstats, & ! Input
                                  ANY_Test )  & ! Optional input
                                RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( pCRTMstats_type ), INTENT( IN ) :: pCRTMstats

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

      IF ( ASSOCIATED( pCRTMstats%NCEP_Sensor_ID     ) .AND. &
           ASSOCIATED( pCRTMstats%WMO_Satellite_ID   ) .AND. &
           ASSOCIATED( pCRTMstats%WMO_Sensor_ID      ) .AND. &
           ASSOCIATED( pCRTMstats%Sensor_Channel     ) .AND. &
           ASSOCIATED( pCRTMstats%Frequency          ) .AND. &
           ASSOCIATED( pCRTMstats%Angle              ) .AND. &
           ASSOCIATED( pCRTMstats%Profile            ) .AND. &
           ASSOCIATED( pCRTMstats%Molecule_Set       ) .AND. &
           ASSOCIATED( pCRTMstats%LBL_Tau            ) .AND. &
           ASSOCIATED( pCRTMstats%REG_Tau            ) .AND. &
           ASSOCIATED( pCRTMstats%Mean_dTau          ) .AND. &
           ASSOCIATED( pCRTMstats%RMS_dTau           ) .AND. &
           ASSOCIATED( pCRTMstats%Mean_dTau_by_Angle ) .AND. &
           ASSOCIATED( pCRTMstats%RMS_dTau_by_Angle  ) .AND. &
           ASSOCIATED( pCRTMstats%LBL_BT             ) .AND. &
           ASSOCIATED( pCRTMstats%REG_BT             ) .AND. &
           ASSOCIATED( pCRTMstats%Mean_dBT           ) .AND. &
           ASSOCIATED( pCRTMstats%RMS_dBT            ) .AND. &
           ASSOCIATED( pCRTMstats%Mean_dBT_by_Angle  ) .AND. &
           ASSOCIATED( pCRTMstats%RMS_dBT_by_Angle   )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( pCRTMstats%NCEP_Sensor_ID     ) .OR. &
           ASSOCIATED( pCRTMstats%WMO_Satellite_ID   ) .OR. &
           ASSOCIATED( pCRTMstats%WMO_Sensor_ID      ) .OR. &
           ASSOCIATED( pCRTMstats%Sensor_Channel     ) .OR. &
           ASSOCIATED( pCRTMstats%Frequency          ) .OR. &
           ASSOCIATED( pCRTMstats%Angle              ) .OR. &
           ASSOCIATED( pCRTMstats%Profile            ) .OR. &
           ASSOCIATED( pCRTMstats%Molecule_Set       ) .OR. &
           ASSOCIATED( pCRTMstats%LBL_Tau            ) .OR. &
           ASSOCIATED( pCRTMstats%REG_Tau            ) .OR. &
           ASSOCIATED( pCRTMstats%Mean_dTau          ) .OR. &
           ASSOCIATED( pCRTMstats%RMS_dTau           ) .OR. &
           ASSOCIATED( pCRTMstats%Mean_dTau_by_Angle ) .OR. &
           ASSOCIATED( pCRTMstats%RMS_dTau_by_Angle  ) .OR. &
           ASSOCIATED( pCRTMstats%LBL_BT             ) .OR. &
           ASSOCIATED( pCRTMstats%REG_BT             ) .OR. &
           ASSOCIATED( pCRTMstats%Mean_dBT           ) .OR. &
           ASSOCIATED( pCRTMstats%RMS_dBT            ) .OR. &
           ASSOCIATED( pCRTMstats%Mean_dBT_by_Angle  ) .OR. &
           ASSOCIATED( pCRTMstats%RMS_dBT_by_Angle   )       ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_pCRTMstats





!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_pCRTMstats
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of pCRTMstats
!       data structures.
!
! CATEGORY:
!       pCRTM : pCRTMstats
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_pCRTMstats( pCRTMstats,               &  ! Output
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
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       pCRTMstats:   Re-initialised pCRTMstats structure.
!                     UNITS:      N/A
!                     TYPE:       pCRTMstats_type
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
!       Associated_pCRTMstats:  Function to check the association status of
!                               the pCRTMstats pointer components.
!
!       Clear_pCRTMstats:       Function to clear the scalar members of a
!                               pCRTMstats structure.
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
!       Note the INTENT on the output pCRTMstats argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jan-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_pCRTMstats( pCRTMstats,   &  ! Output
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
    TYPE( pCRTMstats_type ),  INTENT( IN OUT ) :: pCRTMstats

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_pCRTMstats'


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

    IF ( Clear ) CALL Clear_pCRTMstats( pCRTMstats )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_pCRTMstats( pCRTMstats ) ) RETURN


    ! ------------------------------------------
    ! Deallocate the sensor/satellite ID members
    ! ------------------------------------------

    ! -- Deallocate the NCEP Sensor ID
    IF ( ASSOCIATED( pCRTMstats%NCEP_Sensor_ID ) ) THEN

      DEALLOCATE( pCRTMstats%NCEP_Sensor_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats NCEP_Sensor_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the WMO Satellite ID
    IF ( ASSOCIATED( pCRTMstats%WMO_Satellite_ID ) ) THEN

      DEALLOCATE( pCRTMstats%WMO_Satellite_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats WMO_Satellite_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the WMO Sensor ID
    IF ( ASSOCIATED( pCRTMstats%WMO_Sensor_ID ) ) THEN

      DEALLOCATE( pCRTMstats%WMO_Sensor_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats WMO_Sensor_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -------------------------------------------
    ! Deallocate the dimension descriptor members
    ! -------------------------------------------

    ! -- Sensor channel list
    IF ( ASSOCIATED( pCRTMstats%Sensor_Channel ) ) THEN

      DEALLOCATE( pCRTMstats%Sensor_Channel, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats Sensor_Channel member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Channel central frequency
    IF ( ASSOCIATED( pCRTMstats%Frequency ) ) THEN

      DEALLOCATE( pCRTMstats%Frequency, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats Frequency member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Angle list
    IF ( ASSOCIATED( pCRTMstats%Angle ) ) THEN

      DEALLOCATE( pCRTMstats%Angle, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats Angle member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Profile list
    IF ( ASSOCIATED( pCRTMstats%Profile ) ) THEN

      DEALLOCATE( pCRTMstats%Profile, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats Profile member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Molecule set list
    IF ( ASSOCIATED( pCRTMstats%Molecule_Set ) ) THEN

      DEALLOCATE( pCRTMstats%Molecule_Set, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats Molecule_Set member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -----------------------------------------
    ! Deallocate the transmittance data members
    ! -----------------------------------------

    ! -- LBL TOA transmitance
    IF ( ASSOCIATED( pCRTMstats%LBL_Tau ) ) THEN

      DEALLOCATE( pCRTMstats%LBL_Tau, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats LBL_Tau member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Regression TOA transmitance
    IF ( ASSOCIATED( pCRTMstats%REG_Tau ) ) THEN

      DEALLOCATE( pCRTMstats%REG_Tau, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats REG_Tau member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Mean LBL-REG transmittance difference
    IF ( ASSOCIATED( pCRTMstats%Mean_dTau ) ) THEN

      DEALLOCATE( pCRTMstats%Mean_dTau, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats Mean_dTau member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- RMS LBL-REG transmittance difference
    IF ( ASSOCIATED( pCRTMstats%RMS_dTau ) ) THEN

      DEALLOCATE( pCRTMstats%RMS_dTau, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats RMS_dTau member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Mean LBL-REG transmittance difference BY ANGLE
    IF ( ASSOCIATED( pCRTMstats%Mean_dTau_by_Angle ) ) THEN

      DEALLOCATE( pCRTMstats%Mean_dTau_by_Angle, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats Mean_dTau_by_Angle member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- RMS LBL-REG transmittance difference BY ANGLE
    IF ( ASSOCIATED( pCRTMstats%RMS_dTau_by_Angle ) ) THEN

      DEALLOCATE( pCRTMstats%RMS_dTau_by_Angle, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats RMS_dTau_by_Angle member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! --------------------------------------------------
    ! Deallocate the brightness temperature data members
    ! --------------------------------------------------

    ! -- LBL TOA brightness temperature
    IF ( ASSOCIATED( pCRTMstats%LBL_BT ) ) THEN

      DEALLOCATE( pCRTMstats%LBL_BT, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats LBL_BT member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Regression TOA brightness temperature
    IF ( ASSOCIATED( pCRTMstats%REG_BT ) ) THEN

      DEALLOCATE( pCRTMstats%REG_BT, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats REG_BT member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Mean LBL-REG brightness temperature difference
    IF ( ASSOCIATED( pCRTMstats%Mean_dBT ) ) THEN

      DEALLOCATE( pCRTMstats%Mean_dBT, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats Mean_dBT member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- RMS LBL-REG brightness temperature difference
    IF ( ASSOCIATED( pCRTMstats%RMS_dBT ) ) THEN

      DEALLOCATE( pCRTMstats%RMS_dBT, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats RMS_dBT member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Mean LBL-REG brightness temperature difference BY ANGLE
    IF ( ASSOCIATED( pCRTMstats%Mean_dBT_by_Angle ) ) THEN

      DEALLOCATE( pCRTMstats%Mean_dBT_by_Angle, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats Mean_dBT_by_Angle member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- RMS LBL-REG brightness temperature difference BY ANGLE
    IF ( ASSOCIATED( pCRTMstats%RMS_dBT_by_Angle ) ) THEN

      DEALLOCATE( pCRTMstats%RMS_dBT_by_Angle, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating pCRTMstats RMS_dBT_by_Angle member. ", &
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

    pCRTMstats%n_Allocates = pCRTMstats%n_Allocates - 1

    IF ( pCRTMstats%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      pCRTMstats%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_pCRTMstats





!------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_pCRTMstats
! 
! PURPOSE:
!       Function to allocate the pointer members of the pCRTMstats
!       data structure.
!
! CATEGORY:
!       pCRTM : pCRTMstats
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_pCRTMstats( n_Channels,               &  ! Input
!                                           n_Angles,                 &  ! Input
!                                           n_Profiles,               &  ! Input
!                                           n_Molecule_Sets,          &  ! Input
!                                           pCRTMstats,               &  ! Output
!                                           RCS_Id      = RCS_Id,     &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
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
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       pCRTMstats:       pCRTMstats structure with allocated
!                         pointer members
!                         UNITS:      N/A
!                         TYPE:       pCRTMstats_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:           Character string containing the Revision Control
!                         System Id field for the module.
!                         UNITS:      N/A
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
!       Associated_pCRTMstats:  Function to test the association status of the
!                               pointer members of a pCRTMstats structure.
!
!       Destroy_pCRTMstats:     Function to re-initialize the scalar and pointer
!                               members of pCRTMstats data structures.
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
!       Note the INTENT on the output pCRTMstats argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jan-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Allocate_pCRTMstats( n_Channels,      &  ! Input
                                n_Angles,        &  ! Input
                                n_Profiles,      &  ! Input
                                n_Molecule_Sets, &  ! Input
                                pCRTMstats,      &  ! Output
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
    INTEGER,                  INTENT( IN )     :: n_Channels
    INTEGER,                  INTENT( IN )     :: n_Angles
    INTEGER,                  INTENT( IN )     :: n_Profiles
    INTEGER,                  INTENT( IN )     :: n_Molecule_Sets

    ! -- Output
    TYPE( pCRTMstats_type ),  INTENT( IN OUT ) :: pCRTMstats

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_pCRTMstats'


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

    IF ( n_Channels      < 1 .OR. &
         n_Angles        < 1 .OR. &
         n_Profiles      < 1 .OR. &
         n_Molecule_Sets < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input pCRTMstats dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_pCRTMstats( pCRTMstats, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_pCRTMstats( pCRTMstats, &
                                         No_Clear = SET, &
                                         Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating pCRTMstats pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( pCRTMstats%NCEP_Sensor_ID( n_Channels ), &
              pCRTMstats%WMO_Satellite_ID ( n_Channels ), &
              pCRTMstats%WMO_Sensor_ID( n_Channels ), &

              pCRTMstats%Sensor_Channel( n_Channels ), &     
              pCRTMstats%Frequency( n_Channels ), &
              pCRTMstats%Angle( n_Angles ), &
              pCRTMstats%Profile( n_Profiles ), &
              pCRTMstats%Molecule_Set( n_Molecule_Sets ), &

              pCRTMstats%LBL_Tau( n_Channels, n_Angles, n_Profiles, n_Molecule_Sets ), & 
              pCRTMstats%REG_Tau( n_Channels, n_Angles, n_Profiles, n_Molecule_Sets ), & 
              pCRTMstats%Mean_dTau( n_Channels, n_Molecule_Sets ), & 
              pCRTMstats%RMS_dTau( n_Channels, n_Molecule_Sets ), & 
              pCRTMstats%Mean_dTau_by_Angle( n_Channels, n_Angles, n_Molecule_Sets ), & 
              pCRTMstats%RMS_dTau_by_Angle(  n_Channels, n_Angles, n_Molecule_Sets ), & 

              pCRTMstats%LBL_BT( n_Channels, n_Angles, n_Profiles, n_Molecule_Sets ), & 
              pCRTMstats%REG_BT( n_Channels, n_Angles, n_Profiles, n_Molecule_Sets ), & 
              pCRTMstats%Mean_dBT( n_Channels, n_Molecule_Sets ), & 
              pCRTMstats%RMS_dBT( n_Channels, n_Molecule_Sets ), & 
              pCRTMstats%Mean_dBT_by_Angle( n_Channels, n_Angles, n_Molecule_Sets ), & 
              pCRTMstats%RMS_dBT_by_Angle(  n_Channels, n_Angles, n_Molecule_Sets ), & 

              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating pCRTMstats data arrays. STAT = ", i5 )' ) &
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

    pCRTMstats%n_Channels      = n_Channels
    pCRTMstats%n_Angles        = n_Angles
    pCRTMstats%n_Profiles      = n_Profiles
    pCRTMstats%n_Molecule_Sets = n_Molecule_Sets



    !#--------------------------------------------------------------------------#
    !#          -- FILL THE POINTER MEMBERS WITH INVALID VALUES --              #
    !#--------------------------------------------------------------------------#

    pCRTMstats%NCEP_Sensor_ID   = INVALID
    pCRTMstats%WMO_Satellite_ID = INVALID
    pCRTMstats%WMO_Sensor_ID    = INVALID

    pCRTMstats%Sensor_Channel = INVALID
    pCRTMstats%Frequency      = REAL( INVALID, fp_kind )
    pCRTMstats%Angle          = REAL( INVALID, fp_kind )
    pCRTMstats%Profile        = INVALID
    pCRTMstats%Molecule_Set   = INVALID

    pCRTMstats%LBL_Tau            = REAL( INVALID, fp_kind )
    pCRTMstats%REG_Tau            = REAL( INVALID, fp_kind )
    pCRTMstats%Mean_dTau          = REAL( INVALID, fp_kind )
    pCRTMstats%RMS_dTau           = REAL( INVALID, fp_kind )
    pCRTMstats%Mean_dTau_by_Angle = REAL( INVALID, fp_kind )
    pCRTMstats%RMS_dTau_by_Angle  = REAL( INVALID, fp_kind )

    pCRTMstats%LBL_BT             = REAL( INVALID, fp_kind )
    pCRTMstats%REG_BT             = REAL( INVALID, fp_kind )
    pCRTMstats%Mean_dBT           = REAL( INVALID, fp_kind )
    pCRTMstats%RMS_dBT            = REAL( INVALID, fp_kind )
    pCRTMstats%Mean_dBT_by_Angle  = REAL( INVALID, fp_kind )
    pCRTMstats%RMS_dBT_by_Angle   = REAL( INVALID, fp_kind )



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    pCRTMstats%n_Allocates = pCRTMstats%n_Allocates + 1

    IF ( pCRTMstats%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      pCRTMstats%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_pCRTMstats





!------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_pCRTMstats
!
! PURPOSE:
!       Function to copy valid pCRTMstats structures.
!
! CATEGORY:
!       pCRTM : pCRTMstats
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_pCRTMstats( pCRTMstats_in,            &  ! Input
!                                         pCRTMstats_out,           &  ! Output
!                                         RCS_Id = RCS_Id,          &  ! Revision control
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       pCRTMstats_in:  pCRTMstats structure which is to be copied.
!                       UNITS:      N/A
!                       TYPE:       pCRTMstats_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       pCRTMstats_out: Copy of the input structure, pCRTMstats_in.
!                       UNITS:      N/A
!                       TYPE:       pCRTMstats_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error
!                       status. The error codes are defined in the
!                       ERROR_HANDLER module.
!                       If == SUCCESS the structure assignment was successful
!                          == FAILURE an error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! CALLS:
!       Associated_pCRTMstats:  Function to test the association status of the
!                               pointer members of a pCRTMstats structure.
!
!       Allocate_pCRTMstats:    Function to allocate the pointer members of
!                               the pCRTMstats data structure.
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
!       Note the INTENT on the output pCRTMstats argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jan-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Assign_pCRTMstats( pCRTMstats_in,  &  ! Input
                              pCRTMstats_out, &  ! Output
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
    TYPE( pCRTMstats_type ),  INTENT( IN )     :: pCRTMstats_in

    ! -- Output
    TYPE( pCRTMstats_type ),  INTENT( IN OUT ) :: pCRTMstats_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_pCRTMstats'



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

    IF ( .NOT. Associated_pCRTMstats( pCRTMstats_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT pCRTMstats pointer '//&
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

    pCRTMstats_out%LBL_Profile_ID_Tag = pCRTMstats_in%LBL_Profile_ID_Tag
    pCRTMstats_out%REG_Profile_ID_Tag = pCRTMstats_in%REG_Profile_ID_Tag

    pCRTMstats_out%n_Sensors = pCRTMstats_in%n_Sensors


    ! -----------------
    ! Assign array data
    ! -----------------

    ! -- Allocate data arrays
    Error_Status = Allocate_pCRTMstats( pCRTMstats_in%n_Channels, &
                                        pCRTMstats_in%n_Angles, &
                                        pCRTMstats_in%n_Profiles, &
                                        pCRTMstats_in%n_Molecule_Sets, &
                                        pCRTMstats_out, &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output pCRTMstats arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Copy array data
    pCRTMstats_out%NCEP_Sensor_ID   = pCRTMstats_in%NCEP_Sensor_ID
    pCRTMstats_out%WMO_Satellite_ID = pCRTMstats_in%WMO_Satellite_ID
    pCRTMstats_out%WMO_Sensor_ID    = pCRTMstats_in%WMO_Sensor_ID

    pCRTMstats_out%Sensor_Channel = pCRTMstats_in%Sensor_Channel
    pCRTMstats_out%Frequency      = pCRTMstats_in%Frequency
    pCRTMstats_out%Angle          = pCRTMstats_in%Angle
    pCRTMstats_out%Profile        = pCRTMstats_in%Profile
    pCRTMstats_out%Molecule_Set   = pCRTMstats_in%Molecule_Set

    pCRTMstats_out%LBL_Tau            = pCRTMstats_in%LBL_Tau           
    pCRTMstats_out%REG_Tau            = pCRTMstats_in%REG_Tau           
    pCRTMstats_out%Mean_dTau          = pCRTMstats_in%Mean_dTau         
    pCRTMstats_out%RMS_dTau           = pCRTMstats_in%RMS_dTau          
    pCRTMstats_out%Mean_dTau_by_Angle = pCRTMstats_in%Mean_dTau_by_Angle
    pCRTMstats_out%RMS_dTau_by_Angle  = pCRTMstats_in%RMS_dTau_by_Angle 

    pCRTMstats_out%LBL_BT            = pCRTMstats_in%LBL_BT            
    pCRTMstats_out%REG_BT            = pCRTMstats_in%REG_BT            
    pCRTMstats_out%Mean_dBT          = pCRTMstats_in%Mean_dBT          
    pCRTMstats_out%RMS_dBT           = pCRTMstats_in%RMS_dBT           
    pCRTMstats_out%Mean_dBT_by_Angle = pCRTMstats_in%Mean_dBT_by_Angle 
    pCRTMstats_out%RMS_dBT_by_Angle  = pCRTMstats_in%RMS_dBT_by_Angle  

  END FUNCTION Assign_pCRTMstats





!------------------------------------------------------------------------------
!S+
! NAME:
!       Concatenate_pCRTMstats
!
! PURPOSE:
!       Function to concatenate two valid pCRTMstats structures along
!       the channel dimension.
!
! CATEGORY:
!       pCRTM : pCRTMstats
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Concatenate_pCRTMstats( pCRTMstats1,              &  ! Input/Output
!                                              pCRTMstats2,              &  ! Input
!                                              RCS_Id = RCS_Id,          &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       pCRTMstats1:   First pCRTMstats structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       pCRTMstats_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
!       pCRTMstats2:   Second pCRTMstats structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       pCRTMstats_type
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
!       pCRTMstats1:   The concatenated pCRTMstats structure. The order of
!                      concatenation is pCRTMstats1,pCRTMstats2 along the 
!                      channel dimension.
!                      UNITS:      N/A
!                      TYPE:       pCRTMstats_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
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
!                      If == SUCCESS the structure concatenation was successful
!                         == FAILURE an error occurred, or
!                         == WARNING - the version numbers of the pCRTMstats structure
!                                      data are different.
!                                    - the destruction of a temporary, local pCRTMstats
!                                      structure failed.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       Initialize_pCRTMstats:    Function to initialize the scalar and pointer
!                                 members of pCRTMstats data structures.
!
!       Associated_pCRTMstats:    Function to test the association status of the
!                                 pointer members of a pCRTMstats structure.
!
!       Assign_pCRTMstats:        Function to copy valid pCRTMstats data structures.
!
!       Destroy_pCRTMstats:       Function to re-initialize the scalar and pointer
!                                 members of pCRTMstats data structures.
!
!       Allocate_pCRTMstats:      Function to allocate the pointer members of
!                                 the pCRTMstats data structure.
!
!       Count_pCRTMstats_Sensors: Subroutine to count the number of different
!                                 satellite/sensors in the pCRTMstats structure
!                                 and set the n_Sensors field.
!
!       Display_Message:          Subroutine to output messages
!                                 SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       The input pCRTMstats1 argument contains the concatenated structure
!       data (in character-speak: pCRTMstats1//pCRTMstats2) on output. It is
!       reallocated within this routine so if an error occurs during the
!       reallocation, the contents of the input pCRTMstats1 structure will
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
! COMMENTS:
!       Note the INTENT on the output pCRTMstats argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Concatenate_pCRTMstats( pCRTMstats1,  &  ! Input/Output
                                   pCRTMstats2,  &  ! Input
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
    TYPE( pCRTMstats_type ),  INTENT( IN OUT )  :: pCRTMstats1
    TYPE( pCRTMstats_type ),  INTENT( IN )      :: pCRTMstats2

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Concatenate_pCRTMstats'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: n_Channels, l1, l2

    TYPE( pCRTMstats_type ) :: pCRTMstats_Tmp



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! The first structure
    ! -------------------

    IF ( .NOT. Associated_pCRTMstats( pCRTMstats1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT pCRTMstats1 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------
    ! The second structure
    ! --------------------

    IF ( .NOT. Associated_pCRTMstats( pCRTMstats2 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT pCRTMstats2 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- CHECK THE NON-CHANNEL DIMENSIONS --                  #
    !#--------------------------------------------------------------------------#

    IF ( pCRTMstats1%n_Angles         /= pCRTMstats2%n_Angles        .OR. &
         pCRTMstats1%n_Profiles       /= pCRTMstats2%n_Profiles      .OR. &
         pCRTMstats1%n_Molecule_Sets  /= pCRTMstats2%n_Molecule_Sets      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Non-channel pCRTMstats dimensions are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- COPY FIRST INPUT pCRTMstats STRUCTURE --               #
    !#--------------------------------------------------------------------------#

    Error_Status = Assign_pCRTMstats( pCRTMstats1, pCRTMstats_Tmp, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error copying pCRTMstats1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
   


    !#--------------------------------------------------------------------------#
    !#             -- REALLOCATE FIRST INPUT pCRTMstats STRUCTURE --            #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Destroy it
    ! ----------

    Error_Status = Destroy_pCRTMstats( pCRTMstats1, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying pCRTMstats1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------
    ! Re-Allocate it
    ! --------------

    ! -- Set the total number of channels
    n_Channels = pCRTMstats_Tmp%n_Channels + pCRTMstats2%n_Channels

    ! -- Perform the allocation
    Error_Status = Allocate_pCRTMstats( n_Channels, &
                                        pCRTMstats_Tmp%n_Angles, &
                                        pCRTMstats_Tmp%n_Profiles, &
                                        pCRTMstats_Tmp%n_Molecule_Sets, &
                                        pCRTMstats1, &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reallocating pCRTMstats1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE CONCATENATION --                    #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! Assign scalar members
    ! ---------------------

    pCRTMstats1%LBL_Profile_ID_Tag = pCRTMstats_Tmp%LBL_Profile_ID_Tag
    pCRTMstats1%REG_Profile_ID_Tag = pCRTMstats_Tmp%REG_Profile_ID_Tag


    ! ---------------------------------
    ! Assign the non-channel array data
    ! ---------------------------------

    pCRTMstats1%Angle        = pCRTMstats_Tmp%Angle
    pCRTMstats1%Profile      = pCRTMstats_Tmp%Profile
    pCRTMstats1%Molecule_Set = pCRTMstats_Tmp%Molecule_Set


    ! ------------------------------
    ! Concatenate channel array data
    ! ------------------------------

    ! -- The first part
    l1 = 1
    l2 = pCRTMstats_Tmp%n_Channels

    pCRTMstats1%NCEP_Sensor_ID(l1:l2)   = pCRTMstats_Tmp%NCEP_Sensor_ID
    pCRTMstats1%WMO_Satellite_ID(l1:l2) = pCRTMstats_Tmp%WMO_Satellite_ID
    pCRTMstats1%WMO_Sensor_ID(l1:l2)    = pCRTMstats_Tmp%WMO_Sensor_ID

    pCRTMstats1%Sensor_Channel(l1:l2) = pCRTMstats_Tmp%Sensor_Channel
    pCRTMstats1%Frequency(l1:l2)      = pCRTMstats_Tmp%Frequency

    pCRTMstats1%LBL_Tau(l1:l2,:,:,:)          = pCRTMstats_Tmp%LBL_Tau
    pCRTMstats1%REG_Tau(l1:l2,:,:,:)          = pCRTMstats_Tmp%REG_Tau
    pCRTMstats1%Mean_dTau(l1:l2,:)            = pCRTMstats_Tmp%Mean_dTau
    pCRTMstats1% RMS_dTau(l1:l2,:)            = pCRTMstats_Tmp% RMS_dTau
    pCRTMstats1%Mean_dTau_by_Angle(l1:l2,:,:) = pCRTMstats_Tmp%Mean_dTau_by_Angle
    pCRTMstats1% RMS_dTau_by_Angle(l1:l2,:,:) = pCRTMstats_Tmp% RMS_dTau_by_Angle

    pCRTMstats1%LBL_BT(l1:l2,:,:,:)          = pCRTMstats_Tmp%LBL_BT
    pCRTMstats1%REG_BT(l1:l2,:,:,:)          = pCRTMstats_Tmp%REG_BT
    pCRTMstats1%Mean_dBT(l1:l2,:)            = pCRTMstats_Tmp%Mean_dBT
    pCRTMstats1% RMS_dBT(l1:l2,:)            = pCRTMstats_Tmp% RMS_dBT
    pCRTMstats1%Mean_dBT_by_Angle(l1:l2,:,:) = pCRTMstats_Tmp%Mean_dBT_by_Angle
    pCRTMstats1% RMS_dBT_by_Angle(l1:l2,:,:) = pCRTMstats_Tmp% RMS_dBT_by_Angle


    ! -- The second part
    l1 = l2 + 1
    l2 = n_Channels

    pCRTMstats1%NCEP_Sensor_ID(l1:l2)   = pCRTMstats2%NCEP_Sensor_ID
    pCRTMstats1%WMO_Satellite_ID(l1:l2) = pCRTMstats2%WMO_Satellite_ID
    pCRTMstats1%WMO_Sensor_ID(l1:l2)    = pCRTMstats2%WMO_Sensor_ID

    pCRTMstats1%Sensor_Channel(l1:l2) = pCRTMstats2%Sensor_Channel
    pCRTMstats1%Frequency(l1:l2)      = pCRTMstats2%Frequency

    pCRTMstats1%LBL_Tau(l1:l2,:,:,:)          = pCRTMstats2%LBL_Tau
    pCRTMstats1%REG_Tau(l1:l2,:,:,:)          = pCRTMstats2%REG_Tau
    pCRTMstats1%Mean_dTau(l1:l2,:)            = pCRTMstats2%Mean_dTau
    pCRTMstats1%RMS_dTau(l1:l2,:)             = pCRTMstats2%RMS_dTau
    pCRTMstats1%Mean_dTau_by_Angle(l1:l2,:,:) = pCRTMstats2%Mean_dTau_by_Angle
    pCRTMstats1%RMS_dTau_by_Angle(l1:l2,:,:)  = pCRTMstats2%RMS_dTau_by_Angle

    pCRTMstats1%LBL_BT(l1:l2,:,:,:)          = pCRTMstats2%LBL_BT
    pCRTMstats1%REG_BT(l1:l2,:,:,:)          = pCRTMstats2%REG_BT
    pCRTMstats1%Mean_dBT(l1:l2,:)            = pCRTMstats2%Mean_dBT
    pCRTMstats1%RMS_dBT(l1:l2,:)             = pCRTMstats2%RMS_dBT
    pCRTMstats1%Mean_dBT_by_Angle(l1:l2,:,:) = pCRTMstats2%Mean_dBT_by_Angle
    pCRTMstats1%RMS_dBT_by_Angle(l1:l2,:,:)  = pCRTMstats2%RMS_dBT_by_Angle



    !#--------------------------------------------------------------------------#
    !#                    -- COUNT THE NUMBER OF SENSORS --                     #
    !#--------------------------------------------------------------------------#

    CALL Count_pCRTMstats_Sensors( pCRTMstats1, Use_WMO_Id = SET )



    !#--------------------------------------------------------------------------#
    !#           -- DEALLOCATE THE TEMPORARY pCRTMstats STRUCTURE --            #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_pCRTMstats( pCRTMstats_Tmp, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying pCRTMstats_Tmp structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Concatenate_pCRTMstats





!------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_pCRTMstats
! 
! PURPOSE:
!       Function to compute the statistics for an pCRTMstats data structure.
!
! CATEGORY:
!       pCRTM : pCRTMstats
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Compute_pCRTMstats( pCRTMstats,                &  ! In/Output
!                                          RCS_Id = RCS_Id,           &  ! Revision control
!                                          Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       pCRTMstats:       pCRTMstats structure with the TOA transmittance
!                         and brightness temperature components fillde
!                         with data.
!                         UNITS:      N/A
!                         TYPE:       pCRTMstats_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       pCRTMstats:       pCRTMstats structure with the mean and RMS
!                         transmittance and brightness temperature
!                         components filled based on the input
!                         structure data.
!                         UNITS:      N/A
!                         TYPE:       pCRTMstats_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:           Character string containing the Revision Control
!                         System Id field for the module.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error
!                         status. The error codes are defined in the
!                         ERROR_HANDLER module.
!                         If == SUCCESS the statistics computatation was
!                                       successful.
!                            == FAILURE an unrecoverable error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       Associated_pCRTMstats:  Function to test the association status of the
!                               pointer members of a pCRTMstats structure.
!
!       Display_Message:        Subroutine to output messages
!                               SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       The statistics components of the input pCRTMstats argument are filled
!       in this routine.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jan-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Compute_pCRTMstats( pCRTMstats,   &  ! In/Output
                               RCS_Id,       &  ! Revision control
                               Message_Log ) &  ! Error messaging
                             RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- In/Output
    TYPE( pCRTMstats_type ),  INTENT( IN OUT ) :: pCRTMstats

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_pCRTMstats'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Allocate_Status

    INTEGER :: i, j, l

    REAL( fp_kind ) :: rn_Profiles
    REAL( fp_kind ) :: rn_Angles_Profiles

   


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

    ! ---------------------------------------
    ! ALL *input* pointers must be associated
    ! ---------------------------------------

    IF ( .NOT. Associated_pCRTMstats( pCRTMstats ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT pCRTMstats pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- CALCULATE THE STATS --                         #
    !#--------------------------------------------------------------------------#

    DO j = 1, pCRTMstats%n_Molecule_Sets


      ! ----------------------------------
      ! Sum the angle dependent quantities
      ! ----------------------------------

      DO i = 1, pCRTMstats%n_Angles

        DO l = 1, pCRTMstats%n_Channels

          ! -- The transmittances by angle
          pCRTMstats%Mean_dTau_by_Angle(l,i,j) = SUM( pCRTMstats%LBL_Tau(l,i,:,j) - &
                                                      pCRTMstats%REG_Tau(l,i,:,j)   )
          pCRTMstats%RMS_dTau_by_Angle(l,i,j)  = SUM( (pCRTMstats%LBL_Tau(l,i,:,j) - &
                                                       pCRTMstats%REG_Tau(l,i,:,j)   )**2 )

          ! -- The brightness temperatures by angle
          pCRTMstats%Mean_dBT_by_Angle(l,i,j) = SUM( pCRTMstats%LBL_BT(l,i,:,j) - &
                                                     pCRTMstats%REG_BT(l,i,:,j)   )
          pCRTMstats%RMS_dBT_by_Angle(l,i,j)  = SUM( (pCRTMstats%LBL_BT(l,i,:,j) - &
                                                      pCRTMstats%REG_BT(l,i,:,j)   )**2 )
        END DO

      END DO


      ! ------------------------------------
      ! Sum the angle independent quantities
      ! ------------------------------------

      DO l = 1, pCRTMstats%n_Channels

        ! -- The transmittances
        pCRTMstats%Mean_dTau(l,j) = SUM( pCRTMstats%Mean_dTau_by_Angle(l,:,j) )
        pCRTMstats%RMS_dTau(l,j)  = SUM( pCRTMstats%RMS_dTau_by_Angle(l,:,j)  )

        ! -- The brightness temperatures
        pCRTMstats%Mean_dBT(l,j) = SUM( pCRTMstats%Mean_dBT_by_Angle(l,:,j) )
        pCRTMstats%RMS_dBT(l,j)  = SUM( pCRTMstats%RMS_dBT_by_Angle(l,:,j)  )

      END DO

    END DO


    ! ------------------------
    ! Compute the final values
    ! ------------------------

    ! -- The divisors
    rn_Profiles        = REAL( pCRTMstats%n_Profiles, fp_kind )
    rn_Angles_Profiles = REAL( pCRTMstats%n_Angles*pCRTMstats%n_Profiles, fp_kind )

    ! -- The transmittance statistics
    pCRTMstats%Mean_dTau_by_Angle = pCRTMstats%Mean_dTau_by_Angle / rn_Profiles
    pCRTMstats%RMS_dTau_by_Angle  = SQRT( pCRTMstats%RMS_dTau_by_Angle / rn_Profiles )

    pCRTMstats%Mean_dTau = pCRTMstats%Mean_dTau / rn_Angles_Profiles
    pCRTMstats%RMS_dTau  = SQRT( pCRTMstats%RMS_dTau / rn_Angles_Profiles )

    ! -- The brightness temperature statistics
    pCRTMstats%Mean_dBT_by_Angle = pCRTMstats%Mean_dBT_by_Angle / rn_Profiles
    pCRTMstats%RMS_dBT_by_Angle  = SQRT( pCRTMstats%RMS_dBT_by_Angle / rn_Profiles )

    pCRTMstats%Mean_dBT = pCRTMstats%Mean_dBT / rn_Angles_Profiles
    pCRTMstats%RMS_dBT  = SQRT( pCRTMstats%RMS_dBT / rn_Angles_Profiles )

  END FUNCTION Compute_pCRTMstats





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Count_pCRTMstats_Sensors
!
! PURPOSE:
!       Subroutine to count the number of different satellite/sensors in the
!       pCRTMstats structure and set the n_Sensors field.
!
! CATEGORY:
!       pCRTM : pCRTMstats
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Count_pCRTMstats_Sensors( pCRTMstats,              &  ! In/Output
!                                      Use_WMO_ID = Use_WMO_ID, &  ! Optional input
!                                      RCS_Id = RCS_Id          )  ! Revision control
!
! INPUT ARGUMENTS:
!       pCRTMstats_in: Filled pCRTMstats structure.
!                      UNITS:      N/A
!                      TYPE:       pCRTMstats_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Use_WMO_ID:    Set this argument to use the WMO satellite and sensor
!                      IDs in the pCRTMstats structure to count the number of
!                      different sensors. By default, the NCEP sensor ID is
!                      used.
!                      If = 0, use NCEP sensor ID (default)
!                         = 1, use WMO satellite/sensor ID
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
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
!       The N_SENSORS component of the pCRTMstats structure is modified.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Count_pCRTMstats_Sensors( pCRTMstats, &  ! In/Output
                                       Use_WMO_ID, &  ! Optional input
                                       RCS_Id      )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( pCRTMstats_type ),  INTENT( IN OUT ) :: pCRTMstats

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: Use_WMO_ID

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Use_NCEP_ID
    INTEGER :: l, j, n

    INTEGER, DIMENSION( pCRTMstats%n_Channels ) :: idx



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENT --                 #
    !#--------------------------------------------------------------------------#

    Use_NCEP_ID = .TRUE.
    IF ( PRESENT( Use_WMO_ID ) ) THEN
      IF ( Use_WMO_ID == SET ) Use_NCEP_ID = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- INITIALISE INVALID RESULT VALUE --                  #
    !#--------------------------------------------------------------------------#

    pCRTMstats%n_Sensors = INVALID



    !#--------------------------------------------------------------------------#
    !#                     -- COUNT THE DIFFERENT SENSORS --                    #
    !#--------------------------------------------------------------------------#

    ID_Type: IF ( Use_NCEP_ID ) THEN


      !#------------------------------------------------------------------------#
      !#                     -- USING THE NCEP SENSOR ID --                     #
      !#------------------------------------------------------------------------#

      ! --------------------
      ! Check the data array
      ! --------------------

      ! -- Check that the pointer member is associated
      IF ( .NOT. ASSOCIATED( pCRTMstats%NCEP_Sensor_ID ) ) RETURN

      ! -- Check that all the values are valid 
      IF ( ANY( pCRTMstats%NCEP_Sensor_ID == INVALID ) ) RETURN


      ! ---------------------------
      ! Initialise the sensor count
      ! ---------------------------

      pCRTMstats%n_Sensors = 1


      ! ------------------
      ! Loop over channels
      ! ------------------

      DO l = 2, pCRTMstats%n_Channels

        ! -- Only increment sensor count if the current channel's
        ! -- value has not been previously encountered
        IF ( ALL( pCRTMstats%NCEP_Sensor_ID(1:l-1) /= pCRTMstats%NCEP_Sensor_ID(l) ) ) THEN
          pCRTMstats%n_Sensors = pCRTMstats%n_Sensors + 1
        END IF

      END DO

    ELSE ! Use WMO ID


      !#------------------------------------------------------------------------#
      !#                       -- USING THE WMO IDs --                          #
      !#------------------------------------------------------------------------#

      ! ---------------------
      ! Check the data arrays
      ! ---------------------

      ! -- Check that the pointer members are associated
      IF ( .NOT. ASSOCIATED( pCRTMstats%WMO_Satellite_ID ) .OR. &
           .NOT. ASSOCIATED( pCRTMstats%WMO_Sensor_ID    )      ) RETURN

      ! -- Check that all the values are valid 
      IF ( ANY( pCRTMstats%WMO_Satellite_ID == INVALID ) .OR. &
           ANY( pCRTMstats%WMO_Sensor_ID    == INVALID )      ) RETURN


      ! ---------------------------
      ! Initialise the sensor count
      ! ---------------------------

      pCRTMstats%n_Sensors = 1


      ! ------------------
      ! Loop over channels
      ! ------------------

      l_Channel_loop: DO l = 2, pCRTMstats%n_Channels


        ! ------------------------------------------
        ! Count the number of channels with the SAME
        ! WMO SENSOR ID as the current channel
        ! ------------------------------------------

        n = COUNT( pCRTMstats%WMO_Sensor_ID(1:l-1) == pCRTMstats%WMO_Sensor_ID(l) )


        ! ----------------------------------------------
        ! How many channels have the same WMO SENSOR ID?
        ! ----------------------------------------------

        IF ( n == 0 ) THEN

          ! -- None. Increment the sensor count
          pCRTMstats%n_Sensors = pCRTMstats%n_Sensors + 1

        ELSE

          ! -- Some channels have the same SENSOR ID.
          ! -- Now get those corresponding array indices
          idx(1:n) = PACK( (/ ( j, j=1,l-1 ) /), &
                           pCRTMstats%WMO_Sensor_ID(1:l-1) == pCRTMstats%WMO_Sensor_ID(l) )

          ! -- If ALL of the previous channels' SATELLITE ID
          ! -- values are different from the current channel,
          ! -- then we have a different sensor so increment
          ! -- the sensor count.
          IF ( ALL( pCRTMstats%WMO_Satellite_ID(idx(1:n)) /= pCRTMstats%WMO_Satellite_ID(l) ) ) THEN
            pCRTMstats%n_Sensors = pCRTMstats%n_Sensors + 1
          END IF

        END IF

      END DO l_Channel_loop

    END IF ID_Type

  END SUBROUTINE Count_pCRTMstats_Sensors





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Information_pCRTMstats
!
! PURPOSE:
!       Subroutine to return a string containing information about the
!       pCRTMstats data structure.
!
! CATEGORY:
!       pCRTM : pCRTMstats
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Information_pCRTMstats( pCRTMstats,     &  ! Input
!                                    Information,    &  ! Output
!                                    RCS_Id = RCS_Id )  ! Revision control
! 
! INPUT ARGUMENTS:
!       pCRTMstats:    Filled pCRTMstats structure.
!                      UNITS:      N/A
!                      TYPE:       pCRTMstats_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Information:   String containing information about the passed
!                      pCRTMstats data structure.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jan-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Information_pCRTMstats( pCRTMstats,  &  ! Input
                                     Information, &  ! Output
                                     RCS_Id       )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( pCRTMstats_type ),    INTENT( IN )  :: pCRTMstats

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

    WRITE( Long_String, '( a,1x,"pCRTMstats: ", &
                           &"LBL Profile set- ", a, &
                           &"; REG Profile set- ", a, a, &
                           &"N_CHANNELS=",i4,2x,&
                           &"N_ANGLES=",i1,2x,&
                           &"N_PROFILES=",i3,2x,&
                           &"N_MOLECULE_SETS=",i2 )' ) &
                         ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                         TRIM( pCRTMstats%LBL_Profile_ID_Tag ), &
                         TRIM( pCRTMstats%REG_Profile_ID_Tag ), &
                         ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                         pCRTMstats%n_Channels, &
                         pCRTMstats%n_Angles, &
                         pCRTMstats%n_Profiles, &
                         pCRTMstats%n_Molecule_Sets


    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Information = Long_String(1:MIN( LEN( Information ), LEN_TRIM( Long_String ) ))

  END SUBROUTINE Information_pCRTMstats

END MODULE pCRTMstats_Define


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: pCRTMstats_Define.f90,v 1.6 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 1.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: pCRTMstats_Define.f90,v $
! Revision 1.6  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.5  2005/01/07 18:44:20  paulv
! - Category change to pCRTM.
!
! Revision 1.4  2005/01/06 19:01:32  paulv
! - Upgraded to Fortran-95
!
! Revision 1.3  2004/02/13 17:18:37  paulv
! - Altered the sensor/satellite IDs from scalars to arrays to allow for
!   more than one type of sensor/satellite in the data (e.g. if a combined
!   RTM run os performed.) All the routines affected were changed to reflect
!   the change.
! - Added the n_Sensors scalar to the pCRTMstats data structure.
! - Added the Compute_pCRTMstats_Sensors to determine the number of sensors.
!
! Revision 1.2  2004/02/12 20:23:09  paulv
! - Added concatenation function.
!
! Revision 1.1  2004/01/28 02:50:31  paulv
! Initial checkin. Complete but untested.
!
!
!
!
!
