!----------------------------------------------------------------------------------
!M+
! NAME:
!       AtmProfile_Define
!
! PURPOSE:
!       Module defining the AtmProfile data structure and containing routines
!       to manipulate it.
!       
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE AtmProfile_Define
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Error_Handler:          Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       Compare_Float_Numbers:  Module containing routines to perform equality
!                               check comparisons on input floating point
!                               numbers.
!                               USEs: TYPE_KINDS module
!
! CONTAINS:
!       Associated_AtmProfile:  Function to test the association status
!                               of the pointer members of a AtmProfile
!                               structure.
!
!       Destroy_AtmProfile:     Function to re-initialize an AtmProfile
!                               structure.
!
!       Allocate_AtmProfile:    Function to allocate the pointer members
!                               of an AtmProfile structure.
!
!       Assign_AtmProfile:      Function to copy an AtmProfile structure.
!
!       Equal_AtmProfile:       Function to test if two AtmProfile structures
!                               are equal.
!
!       Information_AtmProfile: Subroutine to return a string containing
!                               information about the AtmProfile data structure.
!
!
! DERIVED TYPES:
!       AtmProfile_type:  Definition of the public AtmProfile data structure.
!       ---------------   Fields are,
!
!         
!         n_Levels:              The number of levels dimension, K+1.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Scalar
!
!         n_Layers:              The number of layers dimension, K.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Scalar
!
!         n_Absorbers:           The number of absorbers dimension, J.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Scalar
!
!         n_Profiles:            The number of profiles dimension, M.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Scalar
!
!         Absorber_ID:           An array containing the HITRAN absorber ID
!                                values for the absorbers included in the profile.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Rank-1, J
!                                ATTRIBUTES: POINTER
!
!         Absorber_Units_ID:     An array containing the absorber units ID
!                                values for the absorbers included in the profile.
!                                These values correspond to those used by LBLRTM.
!                                1 = ppmv
!                                2 = cm^-3
!                                3 = g/kg
!                                4 = g.m^-3
!                                5 = hPa
!                                6 = Dew point, K      [H2O only]
!                                7 = Dew point, deg.C  [H2O only]
!                                8 = Relative humidity [H2O only]
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Rank-1, J
!                                ATTRIBUTES: POINTER
!
!         Absorber_Units_Name:   An array containing the name of the absorber
!                                units corresponding to the absorber units ID.
!                                These values correspond to those used by LBLRTM.
!                                'ppmv'
!                                'cm^-3'
!                                'g/kg'
!                                'g.m^-3'
!                                'hPa'
!                                'Dew point, K'      [H2O only]
!                                'Dew point, deg.C'  [H2O only]
!                                'Relative humidity' [H2O only]
!                                UNITS:      N/A
!                                TYPE:       CHARACTER
!                                DIMENSION:  Rank-1, J
!                                ATTRIBUTES: POINTER
!
!         Absorber_Units_LBLRTM: An array containing the character identifier
!                                for absorber units used in LBLRTM.
!                                'A' = Volume mixing ratio (ppmv)
!                                'B' = Number density (cm^-3)
!                                'C' = Mass mixing ratio (g/kg)
!                                'D' = Mass density (g.m^-3)
!                                'E' = Partial pressure (hPa)
!                                'F' = Dew point (Kelvin)    [H2O only]
!                                'G' = Dew point (Celsius)   [H2O only]
!                                'H' = Relative humidity (%) [H2O only]
!                                UNITS:      N/A
!                                TYPE:       CHARACTER(1)
!                                DIMENSION:  Rank-1, J
!                                ATTRIBUTES: POINTER
!
!         Description:           A character string array containing a description
!                                of each profile.
!                                UNITS:      N/A
!                                TYPE:       CHARACTER
!                                DIMENSION:  Rank-1, M
!                                ATTRIBUTES: POINTER
!
!         Climatology_Model:     The climatology model associated with each
!                                profile.
!                                1 = Tropical
!                                2 = Midlatitude summer
!                                3 = Midlatitude winter
!                                4 = Subarctic summer
!                                5 = Subarctic winter
!                                6 = U.S. Standard Atmosphere
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Rank-1, M
!                                ATTRIBUTES: POINTER
!
!         DateTime:              A structure containing date and time information
!                                for each profile.
!                                UNITS:      N/A
!                                TYPE:       AtmProfileDateTime_type
!                                DIMENSION:  Rank-1, M
!                                ATTRIBUTES: POINTER
!
!         Location:              A structure containing location information
!                                for each profile.
!                                UNITS:      N/A
!                                TYPE:       AtmProfileLocation_type
!                                DIMENSION:  Rank-1, M
!                                ATTRIBUTES: POINTER
!
!         Level_Pressure:        Array of LEVEL pressure values.
!                                UNITS:      hectoPascals (hPa)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-2, K+1 x M
!                                ATTRIBUTES: POINTER
!
!         Level_Temperature:     Array of LEVEL temperature values.
!                                UNITS:      Kelvin (K)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-2, K+1 x M
!                                ATTRIBUTES: POINTER
!
!         Level_Absorber:        Array of LEVEL absorber amounts.
!                                UNITS:      Variable (see Absorber_Units_ID)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-3, K+1 x J x M
!                                ATTRIBUTES: POINTER
!
!         Level_Altitude:        Array of LEVEL altitudes.
!                                UNITS:      metres (m)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-2, K+1 x M
!                                ATTRIBUTES: POINTER
!
!         Layer_Pressure:        Array of LAYER average pressure values.
!                                UNITS:      hectoPascals (hPa)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-2, K x M
!                                ATTRIBUTES: POINTER
!
!         Layer_Temperature:     Array of LAYER average emperature values.
!                                UNITS:      Kelvin (K)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-2, K x M
!                                ATTRIBUTES: POINTER
!
!         Layer_Absorber:        Array of LAYER average absorber amounts.
!                                UNITS:      Variable (see Absorber_Units_ID)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-3, K x J x M
!                                ATTRIBUTES: POINTER
!
!         Layer_Delta_Z:         Array of LAYER thicknesses.
!                                UNITS:      metres (m)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-2, K x M
!                                ATTRIBUTES: POINTER
!
!       *!IMPORTANT!*
!       -------------
!       Note that the AtmProfile_type is PUBLIC and its members are not
!       encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user destroy, 
!       allocate, and assign the structure using only the routines in
!       this module where possible to eliminate -- or at least minimise
!       -- the possibility of memory leakage since most of the structure
!       members are pointers.
!
!
!       AtmProfileDateTime_type:  Definition of the public AtmProfileDateTime
!       -----------------------   data structure. This structure is a component
!                                 of the AtmProfile data structure. Fields are,
!
!         Year:   Year corresponding to the measured/calculated
!                 profile data. (e.g. sonde launch)
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!
!         Month:  Month corresponding to the measured/calculated
!                 profile data. (e.g. sonde launch)
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!
!         Day:    Day corresponding to the measured/calculated
!                 profile data. (e.g. sonde launch)
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!
!         Hour:   Hour corresponding to the measured/calculated
!                 profile data. (e.g. sonde launch)
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!
!
!       AtmProfileLocation_type:  Definition of the public AtmProfileLocation
!       -----------------------   data structure. This structure is a component
!                                 of the AtmProfile data structure. Fields are,
!
!         Longitude:         Longitude corresponding to the measured/calculated
!                            profile data. (e.g. sonde launch)
!                            UNITS:      degress East (0->360)
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Scalar
!
!         Latitude:          Latitude corresponding to the measured/calculated
!                            profile data. (e.g. sonde launch)
!                            UNITS:      degress North (-90->+90)
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Scalar
!
!         Surface_Altitude:  Surface_Altitude corresponding to the measured/
!                            calculated profile data. (e.g. sonde launch)
!                            UNITS:      metres (m)
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Scalar
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
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jul-2002
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
!--------------------------------------------------------------------------------

MODULE AtmProfile_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Error_Handler
  USE Compare_Float_Numbers


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Associated_AtmProfile
  PUBLIC :: Destroy_AtmProfile
  PUBLIC :: Allocate_AtmProfile
  PUBLIC :: Assign_AtmProfile
  PUBLIC :: Equal_AtmProfile
  PUBLIC :: Information_AtmProfile


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: AtmProfile_Define.f90,v 4.1 2004/12/29 20:54:32 paulv Exp $'

  ! -- Maximum number of absorbers
  INTEGER, PUBLIC, PARAMETER :: ATMPROFILE_N_ABSORBERS = 32

  ! -- Absorber units parameters
  INTEGER, PUBLIC, PARAMETER :: ATMPROFILE_N_ABSORBER_UNITS = 8

  INTEGER, PRIVATE :: i
  INTEGER, PUBLIC, PARAMETER, &
           DIMENSION( ATMPROFILE_N_ABSORBER_UNITS ) :: ATMPROFILE_ABSORBER_UNITS_ID = &
                    (/ ( i, i = 1, ATMPROFILE_N_ABSORBER_UNITS ) /)

  CHARACTER( * ), PUBLIC, PARAMETER, &
                  DIMENSION( ATMPROFILE_N_ABSORBER_UNITS ) :: ATMPROFILE_ABSORBER_UNITS_NAME = &
                    (/ 'ppmv                ', &
                       'cm^-3               ', &
                       'g/kg                ', &
                       'g.m^-3              ', &
                       'hPa                 ', &
                       'Dew point, K        ', &  ! [H2O only]
                       'Dew point, deg.C    ', &  ! [H2O only]
                       'Relative humidity, %' /)  ! [H2O only]

  CHARACTER( * ), PUBLIC, PARAMETER, &
                  DIMENSION( ATMPROFILE_N_ABSORBER_UNITS ) :: ATMPROFILE_ABSORBER_UNITS_CHAR = &
                    (/ 'A', &  ! Volume mixing ratio (ppmv)
                       'B', &  ! Number density (cm^-3)
                       'C', &  ! Mass mixing ratio (g/kg)
                       'D', &  ! Mass density (g.m^-3)
                       'E', &  ! Partial pressure (hPa)
                       'F', &  ! Dew point (Kelvin) [H2O only]
                       'G', &  ! Dew point (Celsius) [H2O only]
                       'H' /)  ! Relative humidity (%) [H2O only]

  ! -- Invalid values
  INTEGER,         PUBLIC, PARAMETER :: ATMPROFILE_IP_INVALID = -1
  REAL( fp_kind ), PUBLIC, PARAMETER :: ATMPROFILE_FP_INVALID = -999.0_fp_kind

  ! -- Profile description string length
  INTEGER, PRIVATE, PARAMETER :: PDSL = 512

  ! -- Absorber units name string length
  INTEGER, PRIVATE, PARAMETER :: AUNSL = 32

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


  ! --------------------------------
  ! AtmProfile data type definitions
  ! --------------------------------

  ! -- Date, time, and location
  TYPE, PUBLIC :: AtmProfileDateTime_type
    INTEGER( Long ) :: Year  = ATMPROFILE_IP_INVALID
    INTEGER( Long ) :: Month = ATMPROFILE_IP_INVALID
    INTEGER( Long ) :: Day   = ATMPROFILE_IP_INVALID
    INTEGER( Long ) :: Hour  = ATMPROFILE_IP_INVALID
  END TYPE AtmProfileDateTime_type

  TYPE, PUBLIC :: AtmProfileLocation_type
    REAL( Double ) :: Latitude         = ATMPROFILE_FP_INVALID
    REAL( Double ) :: Longitude        = ATMPROFILE_FP_INVALID
    REAL( Double ) :: Surface_Altitude = ATMPROFILE_FP_INVALID
  END TYPE AtmProfileLocation_type


  ! -- Profile
  TYPE, PUBLIC :: AtmProfile_type
    INTEGER :: n_Allocates = 0

    INTEGER :: PD_StrLen  = PDSL
    INTEGER :: AUN_StrLen = AUNSL

    ! -- Dimensions
    INTEGER( Long ) :: n_Levels    = 0 ! K+1
    INTEGER( Long ) :: n_Layers    = 0 ! K
    INTEGER( Long ) :: n_Absorbers = 0 ! J
    INTEGER( Long ) :: n_Profiles  = 0 ! M

    ! -- Absorber information
    INTEGER( Long ),    POINTER, DIMENSION( : ) :: Absorber_ID => NULL()           ! Dimension J
    INTEGER( Long ),    POINTER, DIMENSION( : ) :: Absorber_Units_ID => NULL()     ! Dimension J
    CHARACTER( AUNSL ), POINTER, DIMENSION( : ) :: Absorber_Units_Name => NULL()   ! Dimension J
    CHARACTER(   1   ), POINTER, DIMENSION( : ) :: Absorber_Units_LBLRTM => NULL() ! Dimension J

    ! -- Profile data    
    CHARACTER( PDSL ),               POINTER, DIMENSION( : )       :: Description       => NULL()  ! Dimension M
    INTEGER( Long ),                 POINTER, DIMENSION( : )       :: Climatology_Model => NULL()  ! Dimension M
    TYPE( AtmProfileDateTime_type ), POINTER, DIMENSION( : )       :: DateTime          => NULL()  ! Dimension M
    TYPE( AtmProfileLocation_type ), POINTER, DIMENSION( : )       :: Location          => NULL()  ! Dimension M

    REAL( Double ),                  POINTER, DIMENSION( :, : )    :: Level_Pressure     => NULL() ! Dimension K+1 x M
    REAL( Double ),                  POINTER, DIMENSION( :, : )    :: Level_Temperature  => NULL() ! Dimension K+1 x M
    REAL( Double ),                  POINTER, DIMENSION( :, :, : ) :: Level_Absorber     => NULL() ! Dimension K+1 x J x M
    REAL( Double ),                  POINTER, DIMENSION( :, : )    :: Level_Altitude     => NULL() ! Dimension K+1 x M

    REAL( Double ),                  POINTER, DIMENSION( :, : )    :: Layer_Pressure    => NULL()  ! Dimension K x M
    REAL( Double ),                  POINTER, DIMENSION( :, : )    :: Layer_Temperature => NULL()  ! Dimension K x M
    REAL( Double ),                  POINTER, DIMENSION( :, :, : ) :: Layer_Absorber    => NULL()  ! Dimension K x J x M
    REAL( Double ),                  POINTER, DIMENSION( :, : )    :: Layer_Delta_Z     => NULL()  ! Dimension K x M
  END TYPE AtmProfile_type


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------------
!
! NAME:
!       Clear_AtmProfile
!
! PURPOSE:
!       Subroutine to clear the scalar members of a AtmProfile structure.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_AtmProfile( AtmProfile) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       AtmProfile:  AtmProfile structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       AtmProfile_type
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
!       Note the INTENT on the output AtmProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-May-2002
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------------

  SUBROUTINE Clear_AtmProfile( AtmProfile )

    TYPE( AtmProfile_type ), INTENT( IN OUT ) :: AtmProfile

    AtmProfile%n_Levels    = 0
    AtmProfile%n_Layers    = 0
    AtmProfile%n_Absorbers = 0
    AtmProfile%n_Profiles  = 0

  END SUBROUTINE Clear_AtmProfile





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
!       Associated_AtmProfile
!
! PURPOSE:
!       Function to test if ALL the pointer members of a AtmProfile
!       structure are associated.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_AtmProfile( AtmProfile,         &  ! Input
!                                                   ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       AtmProfile:   AtmProfile structure which is to have its pointer
!                     member's association status tested.
!                     UNITS:      N/A
!                     TYPE:       AtmProfile_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:     Set this argument to test if ANY of the
!                     AtmProfile structure pointer members are associated.
!                     The default is to test if ALL the pointer members
!                     are associated.
!                     If ANY_Test = 0, test if ALL the pointer members
!                                      are associated.  (DEFAULT)
!                        ANY_Test = 1, test if ANY of the pointer members
!                                      are associated.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the AtmProfile pointer
!                            members.
!                            .TRUE.  - if ALL the AtmProfile pointer members
!                                      are associated, or if the ANY_Test argument
!                                      is set and ANY of the AtmProfile pointer
!                                      members are associated.
!                            .FALSE. - some or all of the AtmProfile pointer
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
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_AtmProfile( AtmProfile, &
                                  ANY_Test ) &
                                RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( AtmProfile_type ), INTENT( IN ) :: AtmProfile

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

    ! -- ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( AtmProfile%Absorber_ID           ) .AND. &
           ASSOCIATED( AtmProfile%Absorber_Units_ID     ) .AND. &
           ASSOCIATED( AtmProfile%Absorber_Units_Name   ) .AND. &
           ASSOCIATED( AtmProfile%Absorber_Units_LBLRTM ) .AND. &

           ASSOCIATED( AtmProfile%Level_Pressure        ) .AND. &
           ASSOCIATED( AtmProfile%Layer_Pressure        ) .AND. &

           ASSOCIATED( AtmProfile%Description           ) .AND. &
           ASSOCIATED( AtmProfile%Climatology_Model     ) .AND. &
           ASSOCIATED( AtmProfile%DateTime              ) .AND. &
           ASSOCIATED( AtmProfile%Location              ) .AND. &
           ASSOCIATED( AtmProfile%Level_Temperature     ) .AND. &
           ASSOCIATED( AtmProfile%Level_Absorber        ) .AND. &
           ASSOCIATED( AtmProfile%Level_Altitude        ) .AND. &
           ASSOCIATED( AtmProfile%Layer_Temperature     ) .AND. &
           ASSOCIATED( AtmProfile%Layer_Absorber        ) .AND. &
           ASSOCIATED( AtmProfile%Layer_Delta_Z         )      ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( AtmProfile%Absorber_ID           ) .OR. &
           ASSOCIATED( AtmProfile%Absorber_Units_ID     ) .OR. &
           ASSOCIATED( AtmProfile%Absorber_Units_Name   ) .OR. &
           ASSOCIATED( AtmProfile%Absorber_Units_LBLRTM ) .OR. &

           ASSOCIATED( AtmProfile%Level_Pressure        ) .OR. &
           ASSOCIATED( AtmProfile%Layer_Pressure        ) .OR. &

           ASSOCIATED( AtmProfile%Description           ) .OR. &
           ASSOCIATED( AtmProfile%Climatology_Model     ) .OR. &
           ASSOCIATED( AtmProfile%DateTime              ) .OR. &
           ASSOCIATED( AtmProfile%Location              ) .OR. &
           ASSOCIATED( AtmProfile%Level_Temperature     ) .OR. &
           ASSOCIATED( AtmProfile%Level_Absorber        ) .OR. &
           ASSOCIATED( AtmProfile%Level_Altitude        ) .OR. &
           ASSOCIATED( AtmProfile%Layer_Pressure        ) .OR. &
           ASSOCIATED( AtmProfile%Layer_Temperature     ) .OR. &
           ASSOCIATED( AtmProfile%Layer_Absorber        ) .OR. &
           ASSOCIATED( AtmProfile%Layer_Delta_Z         )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_AtmProfile





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_AtmProfile
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of AtmProfile
!       data structures.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_AtmProfile( AtmProfile,               &  ! Output
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
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AtmProfile:   Re-initialised AtmProfile structure.
!                     UNITS:      N/A
!                     TYPE:       AtmProfile_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
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
!       Associated_AtmProfile:  Function to check the association status of
!                               the AtmProfile pointer components.
!
!       Clear_AtmProfile:       Function to clear the scalar members of a
!                               AtmProfile structure.
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
!       Note the INTENT on the output AtmProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-May-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Destroy_AtmProfile( AtmProfile,   &  ! Output
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
    TYPE( AtmProfile_type ),  INTENT( IN OUT ) :: AtmProfile

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_AtmProfile'


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

    IF ( Clear ) CALL Clear_AtmProfile( AtmProfile )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_AtmProfile( AtmProfile ) ) RETURN


    ! -------------------------------------------------------
    ! Deallocate the AtmProfile Absorber info pointer members
    ! -------------------------------------------------------

    ! -- Absorber ID
    IF ( ASSOCIATED( AtmProfile%Absorber_ID ) ) THEN

      DEALLOCATE( AtmProfile%Absorber_ID, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AtmProfile Absorber_ID member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Absorber Units ID
    IF ( ASSOCIATED( AtmProfile%Absorber_Units_ID ) ) THEN

      DEALLOCATE( AtmProfile%Absorber_Units_ID, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AtmProfile Absorber_Units_ID member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Absorber Units Name
    IF ( ASSOCIATED( AtmProfile%Absorber_Units_Name ) ) THEN

      DEALLOCATE( AtmProfile%Absorber_Units_Name, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AtmProfile Absorber_Units_Name member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Absorber Units LBLRTM
    IF ( ASSOCIATED( AtmProfile%Absorber_Units_LBLRTM ) ) THEN

      DEALLOCATE( AtmProfile%Absorber_Units_LBLRTM, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AtmProfile Absorber_Units_LBLRTM member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -----------------------------------
    ! Deallocate the pressure data arrays
    ! -----------------------------------

    ! -- Level pressure array
    IF ( ASSOCIATED( AtmProfile%Level_Pressure ) ) THEN

      DEALLOCATE( AtmProfile%Level_Pressure, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AtmProfile Level_Pressure member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Layer pressure array
    IF ( ASSOCIATED( AtmProfile%Layer_Pressure ) ) THEN

      DEALLOCATE( AtmProfile%Layer_Pressure, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AtmProfile Layer_Pressure member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! --------------------------------------------------------
    ! Deallocate the profile dependent information data arrays
    ! --------------------------------------------------------

    ! -- Profile description array
    IF ( ASSOCIATED( AtmProfile%Description ) ) THEN

      DEALLOCATE( AtmProfile%Description, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AtmProfile Description member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Profile climatology model array
    IF ( ASSOCIATED( AtmProfile%Climatology_Model ) ) THEN

      DEALLOCATE( AtmProfile%Climatology_Model, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AtmProfile Climatology_Model member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Profile date/time array
    IF ( ASSOCIATED( AtmProfile%DateTime ) ) THEN

      DEALLOCATE( AtmProfile%DateTime, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AtmProfile DateTime member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Profile location array
    IF ( ASSOCIATED( AtmProfile%Location ) ) THEN

      DEALLOCATE( AtmProfile%Location, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AtmProfile Location member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! --------------------------------------------------
    ! Deallocate the profile dependent LEVEL data arrays
    ! --------------------------------------------------

    ! -- Level temperature array
    IF ( ASSOCIATED( AtmProfile%Level_Temperature ) ) THEN

      DEALLOCATE( AtmProfile%Level_Temperature, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AtmProfile Level_Temperature member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Level absorber array
    IF ( ASSOCIATED( AtmProfile%Level_Absorber ) ) THEN

      DEALLOCATE( AtmProfile%Level_Absorber, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AtmProfile Level_Absorber member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Level altitude array
    IF ( ASSOCIATED( AtmProfile%Level_Altitude ) ) THEN

      DEALLOCATE( AtmProfile%Level_Altitude, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AtmProfile Level_Altitude member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! --------------------------------------------------
    ! Deallocate the profile dependent LAYER data arrays
    ! --------------------------------------------------

    ! -- Layer temperature array
    IF ( ASSOCIATED( AtmProfile%Layer_Temperature ) ) THEN

      DEALLOCATE( AtmProfile%Layer_Temperature, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AtmProfile Layer_Temperature member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Layer absorber array
    IF ( ASSOCIATED( AtmProfile%Layer_Absorber ) ) THEN

      DEALLOCATE( AtmProfile%Layer_Absorber, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AtmProfile Layer_Absorber member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Layer thickness array
    IF ( ASSOCIATED( AtmProfile%Layer_Delta_Z ) ) THEN

      DEALLOCATE( AtmProfile%Layer_Delta_Z, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AtmProfile Layer_Delta_Z member. ", &
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

    AtmProfile%n_Allocates = AtmProfile%n_Allocates - 1

    IF ( AtmProfile%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      AtmProfile%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_AtmProfile





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_AtmProfile
! 
! PURPOSE:
!       Function to allocate the pointer members of the AtmProfile
!       data structure.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_AtmProfile( n_Layers,                 &  ! Input
!                                           n_Absorbers,              &  ! Input
!                                           n_Profiles,               &  ! Input
!                                           AtmProfile,               &  ! Output
!                                           RCS_Id      = RCS_Id,     &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Layers:     Number of layers to be allocated for the AtmProfile
!                     structure pointer members.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       n_Absorbers:  Number of absorbers to be allocated for the AtmProfile
!                     structure pointer members.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       n_Profiles:   Number of profiles to be allocated for the AtmProfile
!                     structure pointer members.
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
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AtmProfile:   AtmProfile structure with allocated pointer members
!                     UNITS:      N/A
!                     TYPE:       AtmProfile_type
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
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
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
!       Associated_AtmProfile:  Function to test the association status of the
!                               pointer members of a AtmProfile structure.
!
!       Destroy_AtmProfile:     Function to re-initialize the scalar and pointer
!                               members of AtmProfile data structures.
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
!       Note the INTENT on the output AtmProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-May-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Allocate_AtmProfile( n_Layers,     &  ! Input
                                n_Absorbers,  &  ! Input
                                n_Profiles,   &  ! Input
                                AtmProfile,   &  ! Output
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
    INTEGER,                  INTENT( IN )     :: n_Layers
    INTEGER,                  INTENT( IN )     :: n_Absorbers
    INTEGER,                  INTENT( IN )     :: n_Profiles

    ! -- Output
    TYPE( AtmProfile_type ),  INTENT( IN OUT ) :: AtmProfile

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_AtmProfile'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Allocate_Status
    INTEGER :: n_Levels



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

    IF ( n_Layers    < 1 .OR. &
         n_Absorbers < 1 .OR. &
         n_Profiles  < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input dimensions must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    n_Levels = n_Layers + 1


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_AtmProfile( AtmProfile, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_AtmProfile( AtmProfile, &
                                         No_Clear = SET, &
                                         Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating AtmProfile pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( AtmProfile%Absorber_ID( n_Absorbers ), &      
              AtmProfile%Absorber_Units_ID( n_Absorbers ), &      
              AtmProfile%Absorber_Units_Name( n_Absorbers ), &      
              AtmProfile%Absorber_Units_LBLRTM( n_Absorbers ), &      
              AtmProfile%Description( n_Profiles ), &
              AtmProfile%Climatology_Model( n_Profiles ), &
              AtmProfile%DateTime( n_Profiles ), &
              AtmProfile%Location( n_Profiles ), &
              AtmProfile%Level_Pressure( n_Levels, n_Profiles ), &
              AtmProfile%Level_Temperature( n_Levels, n_Profiles ), &
              AtmProfile%Level_Absorber( n_Levels, n_Absorbers, n_Profiles ), &
              AtmProfile%Level_Altitude( n_Levels, n_Profiles ), &
              AtmProfile%Layer_Pressure( n_Layers, n_Profiles ), &
              AtmProfile%Layer_Temperature( n_Layers, n_Profiles ), &
              AtmProfile%Layer_Absorber( n_Layers, n_Absorbers, n_Profiles ), &
              AtmProfile%Layer_Delta_Z( n_Layers, n_Profiles ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating AtmProfile data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- ASSIGN THE DIMENSIONS --                         #
    !#--------------------------------------------------------------------------#

    AtmProfile%n_Levels    = n_Levels
    AtmProfile%n_Layers    = n_Layers
    AtmProfile%n_Absorbers = n_Absorbers
    AtmProfile%n_Profiles  = n_Profiles



    !#--------------------------------------------------------------------------#
    !#                  -- INITIALIZE THE POINTER MEMBERS --                    #
    !#--------------------------------------------------------------------------#

    AtmProfile%Absorber_ID           = ATMPROFILE_IP_INVALID
    AtmProfile%Absorber_Units_ID     = ATMPROFILE_IP_INVALID
    AtmProfile%Absorber_Units_Name   = ' '
    AtmProfile%Absorber_Units_LBLRTM = ' '

    AtmProfile%Level_Pressure = ATMPROFILE_FP_INVALID
    AtmProfile%Layer_Pressure = ATMPROFILE_FP_INVALID

    AtmProfile%Description       = ' '
    AtmProfile%Climatology_Model = ATMPROFILE_IP_INVALID
    AtmProfile%DateTime = AtmProfileDateTime_type( ATMPROFILE_IP_INVALID, &
                                                   ATMPROFILE_IP_INVALID, &
                                                   ATMPROFILE_IP_INVALID, &
                                                   ATMPROFILE_IP_INVALID  )
    AtmProfile%Location = AtmProfileLocation_type( ATMPROFILE_FP_INVALID, &
                                                   ATMPROFILE_FP_INVALID, &
                                                   ATMPROFILE_FP_INVALID  )

    AtmProfile%Level_Temperature = ATMPROFILE_FP_INVALID
    AtmProfile%Level_Absorber    = ATMPROFILE_FP_INVALID
    AtmProfile%Level_Altitude    = ATMPROFILE_FP_INVALID

    AtmProfile%Layer_Temperature = ATMPROFILE_FP_INVALID
    AtmProfile%Layer_Absorber    = ATMPROFILE_FP_INVALID
    AtmProfile%Layer_Delta_Z     = ATMPROFILE_FP_INVALID



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    AtmProfile%n_Allocates = AtmProfile%n_Allocates + 1

    IF ( AtmProfile%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      AtmProfile%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_AtmProfile





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_AtmProfile
!
! PURPOSE:
!       Function to copy valid AtmProfile structures.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_AtmProfile( AtmProfile_in,            &  ! Input
!                                         AtmProfile_out,           &  ! Output
!                                         RCS_Id      = RCS_Id,     &  ! Revision control
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AtmProfile_in:  AtmProfile structure which is to be copied.
!                       UNITS:      N/A
!                       TYPE:       AtmProfile_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       Messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output Messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AtmProfile_out: Copy of the input structure, AtmProfile_in.
!                       UNITS:      N/A
!                       TYPE:       AtmProfile_type
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
!                       ATTRIBUTES: OPTIONAL, INTENT( OUT )
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
!       Associated_AtmProfile:  Function to test the association status of the
!                               pointer members of a AtmProfile structure.
!
!       Allocate_AtmProfile:    Function to allocate the pointer members of
!                               the AtmProfile data structure.
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
!       Note the INTENT on the output AtmProfile argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-May-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Assign_AtmProfile( AtmProfile_in,  &  ! Input
                              AtmProfile_out, &  ! Output
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
    TYPE( AtmProfile_type ),  INTENT( IN )     :: AtmProfile_in

    ! -- Output
    TYPE( AtmProfile_type ),  INTENT( IN OUT ) :: AtmProfile_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_AtmProfile'



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

    IF ( .NOT. Associated_AtmProfile( AtmProfile_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT AtmProfile pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Allocate the pointer members
    ! ----------------------------

    Error_Status = Allocate_AtmProfile( AtmProfile_in%n_Layers, &
                                        AtmProfile_in%n_Absorbers, &
                                        AtmProfile_in%n_Profiles, &
                                        AtmProfile_out, &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output AtmProfile arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------
    ! Assign array data
    ! -----------------

    ! -- Absorber info
    AtmProfile_out%Absorber_ID           = AtmProfile_in%Absorber_ID
    AtmProfile_out%Absorber_Units_ID     = AtmProfile_in%Absorber_Units_ID
    AtmProfile_out%Absorber_Units_Name   = AtmProfile_in%Absorber_Units_Name
    AtmProfile_out%Absorber_Units_LBLRTM = AtmProfile_in%Absorber_Units_LBLRTM

    ! -- Profile independent pressures
    AtmProfile_out%Level_Pressure    = AtmProfile_in%Level_Pressure
    AtmProfile_out%Layer_Pressure    = AtmProfile_in%Layer_Pressure

    ! -- Profile dependent information
    AtmProfile_out%Description       = AtmProfile_in%Description
    AtmProfile_out%Climatology_Model = AtmProfile_in%Climatology_Model
    AtmProfile_out%DateTime          = AtmProfile_in%DateTime
    AtmProfile_out%Location          = AtmProfile_in%Location

    ! -- Profile dependent LEVEL data
    AtmProfile_out%Level_Temperature = AtmProfile_in%Level_Temperature
    AtmProfile_out%Level_Absorber    = AtmProfile_in%Level_Absorber
    AtmProfile_out%Level_Altitude    = AtmProfile_in%Level_Altitude

    ! -- Profile dependent LAYER data
    AtmProfile_out%Layer_Temperature = AtmProfile_in%Layer_Temperature
    AtmProfile_out%Layer_Absorber    = AtmProfile_in%Layer_Absorber
    AtmProfile_out%Layer_Delta_Z     = AtmProfile_in%Layer_Delta_Z

  END FUNCTION Assign_AtmProfile





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Equal_AtmProfile
!
! PURPOSE:
!       Function to test if two AtmProfile structures are equal.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Equal_AtmProfile( AtmProfile_LHS,           &  ! Input
!                                        AtmProfile_RHS,           &  ! Input
!                                        Check_All   = Check_All,  &  ! Optional input
!                                        RCS_Id      = RCS_Id,     &  ! Revision control
!                                        Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AtmProfile_LHS:  AtmProfile structure to be compared; equivalent to the
!                        left-hand side of a lexical comparison, e.g.
!                          IF ( AtmProfile_LHS == AtmProfile_RHS ).
!                        UNITS:      N/A
!                        TYPE:       AtmProfile_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       AtmProfile_RHS:  AtmProfile structure to be compared to; equivalent to
!                        right-hand side of a lexical comparison, e.g.
!                          IF ( AtmProfile_LHS == AtmProfile_RHS ).
!                        UNITS:      N/A
!                        TYPE:       AtmProfile_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Check_All:       Set this argument to check ALL the floating point
!                        channel data of the AtmProfile structures. The default
!                        action is return with a FAILURE status as soon as
!                        any difference is found. This optional argument can
!                        be used to get a listing of ALL the differences
!                        between data in AtmProfile structures.
!                        If == 0, Return with FAILURE status as soon as
!                                 ANY difference is found  *DEFAULT*
!                           == 1, Set FAILURE status if ANY difference is
!                                 found, but continue to check ALL data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      None
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the structures were equal
!                           == FAILURE - an error occurred, or
!                                      - the structures were different.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Equal_AtmProfile( AtmProfile_LHS, &  ! Input
                             AtmProfile_RHS, &  ! Input
                             Check_All,      &  ! Optional input
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
    TYPE( AtmProfile_type ),  INTENT( IN )  :: AtmProfile_LHS
    TYPE( AtmProfile_type ),  INTENT( IN )  :: AtmProfile_RHS

    ! -- Optional input
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Equal_AtmProfile'


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Check_Once



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

    IF ( .NOT. Associated_AtmProfile( AtmProfile_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT AtmProfile_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------
    ! The RHS structure
    ! -----------------

    IF ( .NOT. Associated_AtmProfile( AtmProfile_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT AtmProfile_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CHECK DIMENSIONS --                           #
    !#--------------------------------------------------------------------------#

    IF ( AtmProfile_LHS%n_Levels    /= AtmProfile_RHS%n_Levels    .OR. &
         AtmProfile_LHS%n_Layers    /= AtmProfile_RHS%n_Layers    .OR. &
         AtmProfile_LHS%n_Absorbers /= AtmProfile_RHS%n_Absorbers .OR. &
         AtmProfile_LHS%n_Profiles  /= AtmProfile_RHS%n_Profiles       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Structure dimensions are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                    -- CHECK ABSORBER INFORMATION --                      #
    !#--------------------------------------------------------------------------#

    ! ----------------
    ! The absorber Ids
    ! ----------------

    IF ( ANY( AtmProfile_LHS%Absorber_ID /= AtmProfile_RHS%Absorber_ID ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Absorber IDs are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! ---------------------
    ! The absorber unit Ids
    ! ---------------------

    IF ( ANY( AtmProfile_LHS%Absorber_Units_ID /= AtmProfile_RHS%Absorber_Units_ID ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Absorber unit IDs are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! -----------------------
    ! The absorber unit names
    ! -----------------------

    IF ( ANY( AtmProfile_LHS%Absorber_Units_Name /= AtmProfile_RHS%Absorber_Units_Name ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Absorber unit names are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! ------------------------------------
    ! The LBLRTM absorber unit identifiers
    ! ------------------------------------

    IF ( ANY( AtmProfile_LHS%Absorber_Units_LBLRTM /= AtmProfile_RHS%Absorber_Units_LBLRTM ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Absorber units LBLRTM identifiers are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CHECK PRESSURES --                            #
    !#--------------------------------------------------------------------------#

    ! ---------------
    ! Level pressures
    ! ---------------

    IF ( ANY( .NOT. Compare_Float( AtmProfile_LHS%Level_Pressure, &
                                   AtmProfile_RHS%Level_Pressure  ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Level pressure values are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! ---------------
    ! Layer pressures
    ! ---------------

    IF ( ANY( .NOT. Compare_Float( AtmProfile_LHS%Layer_Pressure, &
                                   AtmProfile_RHS%Layer_Pressure ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Layer pressure values are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- PROFILE DESCRIPTOR DATA --                       #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! Profile descriptor string
    ! -------------------------

    IF ( ANY( AtmProfile_LHS%Description /= AtmProfile_RHS%Description ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Profile description strings are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! ----------------------------
    ! Profile climatology model ID
    ! ----------------------------

    IF ( ANY( AtmProfile_LHS%Climatology_Model /= AtmProfile_RHS%Climatology_Model ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Profile climatology model IDs are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! ---------------------
    ! Profile date and time
    ! ---------------------

    IF ( ANY( AtmProfile_LHS%DateTime%Year  /= AtmProfile_RHS%DateTime%Year  ) .OR. &
         ANY( AtmProfile_LHS%DateTime%Month /= AtmProfile_RHS%DateTime%Month ) .OR. &
         ANY( AtmProfile_LHS%DateTime%Day   /= AtmProfile_RHS%DateTime%Day   ) .OR. &
         ANY( AtmProfile_LHS%DateTime%Hour  /= AtmProfile_RHS%DateTime%Hour  )      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Profile date/time data are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! ----------------
    ! Profile location
    ! ----------------

    IF ( ANY( .NOT. Compare_Float( AtmProfile_LHS%Location%Latitude, &
                                   AtmProfile_RHS%Location%Latitude  ) ) .OR. &
         ANY( .NOT. Compare_Float( AtmProfile_LHS%Location%Longitude, &
                                   AtmProfile_RHS%Location%Longitude  ) ) .OR. &
         ANY( .NOT. Compare_Float( AtmProfile_LHS%Location%Surface_Altitude, &
                                   AtmProfile_RHS%Location%Surface_Altitude  ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Profile location data are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- LEVEL PROFILE DATA --                         #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! Level temperature
    ! -----------------

    IF ( ANY( .NOT. Compare_Float( AtmProfile_LHS%Level_Temperature, &
                                   AtmProfile_RHS%Level_Temperature ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Level temperature values are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! --------------
    ! Level Absorber
    ! --------------

    IF ( ANY( .NOT. Compare_Float( AtmProfile_LHS%Level_Absorber, &
                                   AtmProfile_RHS%Level_Absorber ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Level absorber values are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! -----------------
    ! Level Altitude
    ! -----------------

    IF ( ANY( .NOT. Compare_Float( AtmProfile_LHS%Level_Altitude, &
                                   AtmProfile_RHS%Level_Altitude ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Level altitude values are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- LAYER PROFILE DATA --                         #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! Layer temperature
    ! -----------------

    IF ( ANY( .NOT. Compare_Float( AtmProfile_LHS%Layer_Temperature, &
                                   AtmProfile_RHS%Layer_Temperature ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Layer aemperature values are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! --------------
    ! Layer Absorber
    ! --------------

    IF ( ANY( .NOT. Compare_Float( AtmProfile_LHS%Layer_Absorber, &
                                   AtmProfile_RHS%Layer_Absorber ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Layer absorber values are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF


    ! ---------------
    ! Layer thickness
    ! ---------------

    IF ( ANY( .NOT. Compare_Float( AtmProfile_LHS%Layer_Delta_Z, &
                                   AtmProfile_RHS%Layer_Delta_Z ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Layer thickness values are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      IF ( Check_Once ) RETURN
    END IF

  END FUNCTION Equal_AtmProfile





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Information_AtmProfile
!
! PURPOSE:
!       Subroutine to return a string containing information about the
!       AtmProfile data structure.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Information_AtmProfile( AtmProfile,     &  ! Input
!                                    Information,    &  ! Output
!                                    RCS_Id = RCS_Id )  ! Revision control
! 
! INPUT ARGUMENTS:
!       AtmProfile:    Filled AtmProfile structure.
!                      UNITS:      N/A
!                      TYPE:       AtmProfile_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Information:   String containing information about the passed
!                      AtmProfile data structure.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Information_AtmProfile( AtmProfile,  &  ! Input
                                     Information, &  ! Output
                                     RCS_Id       )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( AtmProfile_type ),  INTENT( IN )  :: AtmProfile

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

    CHARACTER(  256 ) :: Fmt_String
    CHARACTER( 5000 ) :: Long_String



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

    ! --------------------------------------------------
    ! Create the (rather complicated) format string with
    ! variable format for the number of absorbers
    ! --------------------------------------------------

    WRITE( Fmt_String, '( "(a,",''" AtmProfile: "'',",", &
                         &''"N_LAYERS="'',",i3,2x,", &
                         &''"N_ABSORBERS="'',",i3,2x,",&
                         &''"N_PROFILES="'',",i3, ",&
                         &"a,",''"             ABSORBER_IDs:   "'', ", ", i2, "i3,", &
                         &"a,",''"             ABSORBER_UNITS: "'', ", ", i2, "a5)" )' ) &
                       AtmProfile%n_Absorbers, AtmProfile%n_Absorbers


    ! -------------------------------------------
    ! Write the required data to the local string
    ! -------------------------------------------

    WRITE( Long_String, FMT = Fmt_String )ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                                          AtmProfile%n_Layers, &
                                          AtmProfile%n_Absorbers, &
                                          AtmProfile%n_Profiles, &
                                          ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                                          AtmProfile%Absorber_ID, &
                                          ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                                          AtmProfile%Absorber_Units_Name


    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Information = Long_String(1:MIN( LEN( Information ), LEN_TRIM( Long_String ) ))

  END SUBROUTINE Information_AtmProfile

 END MODULE AtmProfile_Define


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: AtmProfile_Define.f90,v 4.1 2004/12/29 20:54:32 paulv Exp $
!
! $Date: 2004/12/29 20:54:32 $
!
! $Revision: 4.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: AtmProfile_Define.f90,v $
! Revision 4.1  2004/12/29 20:54:32  paulv
! - Updated header documentation.
!
! Revision 4.0  2004/11/02 20:13:02  paulv
! - New versions for modified AtmProfile structure.
!
! Revision 3.1  2004/08/27 17:14:58  paulv
! - Cosmetic changes only.
!
! Revision 3.0  2004/08/27 14:32:41  paulv
! - Updated to Fortran95
! - New versions to handle derived type initialisation.
!
! Revision 2.5  2003/12/01 19:17:29  paulv
! - Updated header documentation.
!
! Revision 2.4  2003/11/25 22:27:51  paulv
! - Updated header documentation.
!
! Revision 2.3  2003/11/17 20:34:56  paulv
! - Corrected yet another syntax bug in format string in Information() routine.
!
! Revision 2.2  2003/11/17 19:59:13  paulv
! - Corrected bug in constructed format string in Information() routine.
!
! Revision 2.1  2003/11/17 19:46:18  paulv
! - Added Information() routine.
!
! Revision 2.0  2003/05/23 20:57:33  paulv
! - New version. Entire database is read into the structure rather than a
!   profile at a time.
!
! Revision 1.5  2003/02/25 17:53:17  paulv
! - Added association test function.
!
! Revision 1.4  2002/07/29 15:28:27  paulv
! - Added parameters for:
!   o Maximum number of absorbers
!   o Maximum number of absorber units
!   o The list of absorber unit IDs
!   o The list of absorber unit names
!   o The list of corresponding absorber units LBLRTM specification
!
! Revision 1.3  2002/07/12 18:52:22  paulv
! - Added RCS_Id optional output arguments to public functions.
! - Increased the profile description field of the AtmProfile structure from
!   256 to 512 characters.
!
! Revision 1.2  2002/07/11 15:04:09  paulv
! - Now using integer and floating point invalid parameter values for clearing
!   the scalar structure members and filling the pointer members when allocated.
! - Removed AtmProfileTime_type and AtmProfileDate_type as components of the
!   AtmProfileLocation_type. A new structure definition, AtmProfileDateTime_type,
!   is now included in the main structure.
!
! Revision 1.1  2002/07/09 21:08:16  paulv
! Initial checkin. Incomplete.
!
!
!
