!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Atmosphere_Define
!
! PURPOSE:
!       Module defining the CRTM Atmosphere structure and containing routines to 
!       manipulate it.
!       
! CATEGORY:
!       CRTM : Atmosphere
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       USE CRTM_Atmosphere_Define
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Message_Handler:        Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       CRTM_Cloud_Define:      Module defining the CRTM Cloud structure and
!                               containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
! CONTAINS:
!       CRTM_Init_Atmosphere:      Subroutine to initialize a CRTM_Atmosphere
!                                  structure.
!
!       CRTM_Destroy_Atmosphere:   Function to re-initialize a CRTM_Atmosphere
!                                  structure.
!
!       CRTM_Allocate_Atmosphere:  Function to allocate the pointer members
!                                  of a CRTM_Atmosphere structure.
!
!       CRTM_Assign_Atmosphere:    Function to copy a CRTM_Atmosphere structure.
!
!
! DERIVED TYPES:
!
!       *!IMPORTANT!*
!       -------------
!       Note that the CRTM_Atmosphere_type is PUBLIC and its members are
!       not encapsulated; that is, they can be fully accessed outside the
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
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Feb-2004
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

MODULE CRTM_Atmosphere_Define_old


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler

  USE CRTM_Cloud_Define_old


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- CRTM_Cloud structure data type
  ! -- in the CRTM_Cloud_Define module
  PUBLIC :: CRTM_Cloud_type

  ! -- CRTM_Cloud structure routines inherited
  ! -- from the CRTM_Cloud_Define module
  PUBLIC :: CRTM_Init_Cloud
  PUBLIC :: CRTM_Destroy_Cloud
  PUBLIC :: CRTM_Allocate_Cloud
  PUBLIC :: CRTM_Assign_Cloud

  ! -- CRTM_Atmosphere routines in this module
  PUBLIC :: CRTM_Init_Atmosphere
  PUBLIC :: CRTM_Destroy_Atmosphere
  PUBLIC :: CRTM_Allocate_Atmosphere
  PUBLIC :: CRTM_Assign_Atmosphere


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE CRTM_Init_Atmosphere
    MODULE PROCEDURE Init_Atmosphere_Scalar
    MODULE PROCEDURE Init_Atmosphere_Rank1
  END INTERFACE ! CRTM_Init_Atmosphere

  INTERFACE CRTM_Destroy_Atmosphere
    MODULE PROCEDURE Destroy_Atmosphere_Scalar
    MODULE PROCEDURE Destroy_Atmosphere_Rank1
  END INTERFACE ! CRTM_Destroy_Atmosphere

  INTERFACE CRTM_Allocate_Atmosphere
    MODULE PROCEDURE Allocate_Atmosphere_Scalar
    MODULE PROCEDURE Allocate_Atmosphere_Rank0001
    MODULE PROCEDURE Allocate_Atmosphere_Rank1011
    MODULE PROCEDURE Allocate_Atmosphere_Rank0011
    MODULE PROCEDURE Allocate_Atmosphere_Rank1001
  END INTERFACE ! CRTM_Allocate_Atmosphere

  INTERFACE CRTM_Assign_Atmosphere
    MODULE PROCEDURE Assign_Atmosphere_Scalar
    MODULE PROCEDURE Assign_Atmosphere_Rank1
  END INTERFACE ! CRTM_Assign_Atmosphere


  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! -- The absorber IDs. Use HITRAN definitions
  INTEGER, PUBLIC, PARAMETER :: N_VALID_ABSORBER_IDS = 32

  INTEGER, PUBLIC, PARAMETER ::   H2O_ID =  1
  INTEGER, PUBLIC, PARAMETER ::   CO2_ID =  2
  INTEGER, PUBLIC, PARAMETER ::    O3_ID =  3
  INTEGER, PUBLIC, PARAMETER ::   N2O_ID =  4
  INTEGER, PUBLIC, PARAMETER ::    CO_ID =  5
  INTEGER, PUBLIC, PARAMETER ::   CH4_ID =  6
  INTEGER, PUBLIC, PARAMETER ::    O2_ID =  7
  INTEGER, PUBLIC, PARAMETER ::    NO_ID =  8
  INTEGER, PUBLIC, PARAMETER ::   SO2_ID =  9
  INTEGER, PUBLIC, PARAMETER ::   NO2_ID = 10
  INTEGER, PUBLIC, PARAMETER ::   NH3_ID = 11
  INTEGER, PUBLIC, PARAMETER ::  HNO3_ID = 12
  INTEGER, PUBLIC, PARAMETER ::    OH_ID = 13
  INTEGER, PUBLIC, PARAMETER ::    HF_ID = 14
  INTEGER, PUBLIC, PARAMETER ::   HCl_ID = 15
  INTEGER, PUBLIC, PARAMETER ::   HBr_ID = 16
  INTEGER, PUBLIC, PARAMETER ::    HI_ID = 17
  INTEGER, PUBLIC, PARAMETER ::   ClO_ID = 18
  INTEGER, PUBLIC, PARAMETER ::   OCS_ID = 19
  INTEGER, PUBLIC, PARAMETER ::  H2CO_ID = 20
  INTEGER, PUBLIC, PARAMETER ::  HOCl_ID = 21
  INTEGER, PUBLIC, PARAMETER ::    N2_ID = 22
  INTEGER, PUBLIC, PARAMETER ::   HCN_ID = 23
  INTEGER, PUBLIC, PARAMETER ::  CH3l_ID = 24
  INTEGER, PUBLIC, PARAMETER ::  H2O2_ID = 25
  INTEGER, PUBLIC, PARAMETER ::  C2H2_ID = 26
  INTEGER, PUBLIC, PARAMETER ::  C2H6_ID = 27
  INTEGER, PUBLIC, PARAMETER ::   PH3_ID = 28
  INTEGER, PUBLIC, PARAMETER ::  COF2_ID = 29
  INTEGER, PUBLIC, PARAMETER ::   SF6_ID = 30
  INTEGER, PUBLIC, PARAMETER ::   H2S_ID = 31
  INTEGER, PUBLIC, PARAMETER :: HCOOH_ID = 32

  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_ABSORBER_IDS ) :: &
    ABSORBER_ID_NAME = (/ 'None ', &
                          'H2O  ', 'CO2  ', 'O3   ', 'N2O  ', 'CO   ', 'CH4  ', 'O2   ', 'NO   ', &
                          'SO2  ', 'NO2  ', 'NH3  ', 'HNO3 ', 'OH   ', 'HF   ', 'HCl  ', 'HBr  ', &
                          'HI   ', 'ClO  ', 'OCS  ', 'H2CO ', 'HOCl ', 'N2   ', 'HCN  ', 'CH3Cl', &
                          'H2O2 ', 'C2H2 ', 'C2H6 ', 'PH3  ', 'COF2 ', 'SF6  ', 'H2S  ', 'HCOOH' /)


  ! -- The absorber units. Use LBLRTM definitions and then some.
  INTEGER, PUBLIC, PARAMETER :: N_VALID_ABSORBER_UNITS = 10

  INTEGER, PUBLIC, PARAMETER ::                     NO_UNITS =  0
  INTEGER, PUBLIC, PARAMETER ::    VOLUME_MIXING_RATIO_UNITS =  1
  INTEGER, PUBLIC, PARAMETER ::         NUMBER_DENSITY_UNITS =  2
  INTEGER, PUBLIC, PARAMETER ::      MASS_MIXING_RATIO_UNITS =  3
  INTEGER, PUBLIC, PARAMETER ::           MASS_DENSITY_UNITS =  4
  INTEGER, PUBLIC, PARAMETER ::       PARTIAL_PRESSURE_UNITS =  5
  INTEGER, PUBLIC, PARAMETER :: DEWPOINT_TEMPERATURE_K_UNITS =  6 ! H2O only
  INTEGER, PUBLIC, PARAMETER :: DEWPOINT_TEMPERATURE_C_UNITS =  7 ! H2O only
  INTEGER, PUBLIC, PARAMETER ::      RELATIVE_HUMIDITY_UNITS =  8 ! H2O only
  INTEGER, PUBLIC, PARAMETER ::        SPECIFIC_AMOUNT_UNITS =  9
  INTEGER, PUBLIC, PARAMETER ::        INTEGRATED_PATH_UNITS = 10

  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_ABSORBER_UNITS ) :: &
    ABSORBER_UNITS_NAME = (/ 'None                               ', &
                             'Volume mixing ratio, ppmv          ', &
                             'Number density, cm^-3              ', &
                             'Mass mixing ratio, g/kg            ', &
                             'Mass density, g.m^-3               ', &
                             'Partial pressure, hPa              ', &
                             'Dewpoint temperature, K  (H2O ONLY)', &
                             'Dewpoint temperature, C  (H2O ONLY)', &
                             'Relative humidity, %     (H2O ONLY)', &
                             'Specific amount, g/g               ', &
                             'Integrated path, mm                ' /)

  INTEGER, PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_ABSORBER_UNITS ) :: &
    H2O_ONLY_UNITS_FLAG = (/ 0, &  ! None
                             0, &  ! Volume mixing ratio, ppmv
                             0, &  ! Number density, cm^-3
                             0, &  ! Mass mixing ratio, g/kg
                             0, &  ! Mass density, g.m^-3
                             0, &  ! Partial pressure, hPa
                             1, &  ! Dewpoint temperature, K  (H2O ONLY)
                             1, &  ! Dewpoint temperature, C  (H2O ONLY)
                             1, &  ! Relative humidity, %     (H2O ONLY)
                             0, &  ! Specific amount, g/g
                             0 /)  ! Integrated path, mm



  ! -- The climatology model.
  INTEGER, PUBLIC, PARAMETER :: N_VALID_CLIMATOLOGY_MODELS = 6

  INTEGER, PUBLIC, PARAMETER :: NO_MODEL               = 0
  INTEGER, PUBLIC, PARAMETER :: TROPICAL               = 1
  INTEGER, PUBLIC, PARAMETER :: MIDLATITUDE_SUMMER     = 2
  INTEGER, PUBLIC, PARAMETER :: MIDLATITUDE_WINTER     = 3
  INTEGER, PUBLIC, PARAMETER :: SUBARCTIC_SUMMER       = 4
  INTEGER, PUBLIC, PARAMETER :: SUBARCTIC_WINTER       = 5
  INTEGER, PUBLIC, PARAMETER :: US_STANDARD_ATMOSPHERE = 6 

  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_CLIMATOLOGY_MODELS ) :: &
    CLIMATOLOGY_MODEL_NAME = (/ 'None                    ', &
                                'Tropical                ', &
                                'Midlatitude summer      ', &
                                'Midlatitude winter      ', &
                                'Subarctic summer        ', &
                                'Subarctic winter        ', &
                                'U.S. Standard Atmosphere' /)


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Atmosphere_Define.f90,v 1.2 2004/07/01 20:52:51 paulv Exp $'

  ! -- Atmosphere scalar member invalid value
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


  ! -------------------------------
  ! Atmosphere data type definition
  ! -------------------------------

  TYPE, PUBLIC :: CRTM_Atmosphere_type
    INTEGER :: n_Allocates

    ! -- Dimension values
    INTEGER :: n_Layers     ! K dimension
    INTEGER :: n_Absorbers  ! J dimension
    INTEGER :: n_Clouds     ! N dimension

    ! -- Climatology model associated with the profile
    !   1: Tropical
    !   2: Midlatitude summer
    !   3: Midlatitude winter
    !   4: Subarctic summer
    !   5: Subarctic winter
    !   6: U.S. Standard Atmosphere
    INTEGER :: Climatology

    ! -- Absorber ID. Use HITRAN definitions.
    !  1: H2O    2: CO2    3: O3     4: N2O    5: CO     6: CH4   7: O2    8: NO 
    !  9: SO2   10: NO2   11: NH3   12: HNO3  13: OH    14: HF   15: HCl  16: HBr  
    ! 17: HI    18: ClO   19: OCS   20: H2CO  21: HOCl  22: N2   23: HCN  24: CH3Cl 
    ! 25: H2O2  26: C2H2  27: C2H6  28: PH3   29: COF2  30: SF6  31: H2S  32: HCOOH
    ! Add more as needed.
    INTEGER, DIMENSION( : ), POINTER :: Absorber_ID  ! J

    ! -- Absorber Units. Use LBLRTM definitions
    !  1: Volume mixing ratio, ppmv
    !  2: Number density, cm^-3
    !  3: Mass mixing ratio, g/kg
    !  4: Mass density, g.m^-3
    !  5: Partial pressure, hPa
    !  6: Dewpoint temperature, K  (H2O ONLY)
    !  7: Dewpoint temperature, C  (H2O ONLY)
    !  8: Relative humidity, %     (H2O ONLY)
    !  9: Specific amount, g/g
    ! Add more as needed. E.g. kmol.cm^-2
    INTEGER, DIMENSION( : ), POINTER :: Absorber_Units  ! J

    ! -- Profile LEVEL pressure and LAYER quantities
    REAL( fp_kind ), DIMENSION( : ),    POINTER :: Level_Pressure   ! K
    REAL( fp_kind ), DIMENSION( : ),    POINTER :: Pressure         ! K
    REAL( fp_kind ), DIMENSION( : ),    POINTER :: Temperature      ! K
    REAL( fp_kind ), DIMENSION( :, : ), POINTER :: Absorber         ! K x J

    ! -- Clouds associated with each profile
    TYPE( CRTM_Cloud_type ), DIMENSION( : ), POINTER :: Cloud     ! N

  END TYPE CRTM_Atmosphere_type


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
!       CRTM_Clear_Atmosphere
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM_Atmosphere structure.
!
! CATEGORY:
!       CRTM : Atmosphere
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_Atmosphere( Atmosphere) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Atmosphere:  Atmosphere structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       CRTM_Atmosphere_type
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
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_Atmosphere( Atmosphere )

    TYPE( CRTM_Atmosphere_type ), INTENT( OUT ) :: Atmosphere

    ! -- Dimensions
    Atmosphere%n_Layers    = 0
    Atmosphere%n_Absorbers = 0
    Atmosphere%n_Clouds    = 0

    ! -- Climatology model associated with the profile
    Atmosphere%Climatology = NO_MODEL

  END SUBROUTINE CRTM_Clear_Atmosphere





!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Associated_Atmosphere
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       CRTM_Atmosphere structure.
!
! CATEGORY:
!       CRTM : Atmosphere
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_Atmosphere( Atmosphere,             &  ! Input
!                                                        ANY_Test   = Any_Test,  &  ! Optional input
!                                                        Skip_Cloud = Skip_Cloud )  ! Optional input
!
! INPUT ARGUMENTS:
!       Atmosphere:          Structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_Atmosphere_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            Atmosphere structure pointer members are associated.
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
!       Skip_Cloud:          Set this argument to not include the Cloud
!                            member in the association test. This is required
!                            because a valid Atmosphere structure can be
!                            cloud-free.
!                            If Skip_Cloud = 0, the Cloud member association
!                                               status is tested.  (DEFAULT)
!                               Skip_Cloud = 1, the Cloud member association
!                                               status is NOT tested.
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
!                            association status of the Atmosphere pointer members.
!                            .TRUE.  - if ALL the Atmosphere pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the Atmosphere pointer
!                                      members are associated.
!                            .FALSE. - some or all of the Atmosphere pointer
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
!       This function tests the association status of the Atmosphere
!       structure pointer members. Therefore this function must only
!       be called after the input Atmosphere structure has, at least,
!       had its pointer members initialized.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_Atmosphere( Atmosphere,  & ! Input
                                       ANY_Test,    & ! Optional input
                                       Skip_Cloud ) & ! Optional input
                                     RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ), INTENT( IN ) :: Atmosphere

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN ) :: ANY_Test
    INTEGER,            OPTIONAL, INTENT( IN ) :: Skip_Cloud


    ! ---------------
    ! Function result
    ! ---------------

    LOGICAL :: Association_Status


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: ALL_Test
    LOGICAL :: Include_Cloud



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! ------------------------------
    ! The ANY_Test optional argument
    ! ------------------------------

    ! -- Default is to test ALL the pointer members
    ! -- for a true association status....
    ALL_Test = .TRUE.

    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF


    ! --------------------------------
    ! The Skip_Cloud optional argument
    ! --------------------------------

    ! -- Default is to include the Cloud member
    ! -- in the association test....
    Include_Cloud = .TRUE.

    ! ...unless the Skip_Cloud argument is set.
    IF ( PRESENT( Skip_Cloud ) ) THEN
      IF ( Skip_Cloud == SET ) Include_Cloud = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Initialise a result
    ! -------------------

    Association_Status = .FALSE.


    ! ----------------------------------------
    ! Test the members that MUST be associated
    ! ----------------------------------------

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( Atmosphere%Absorber_ID      ) .AND. &
           ASSOCIATED( Atmosphere%Absorber_Units   ) .AND. &
           ASSOCIATED( Atmosphere%Level_Pressure   ) .AND. &
           ASSOCIATED( Atmosphere%Pressure         ) .AND. &
           ASSOCIATED( Atmosphere%Temperature      ) .AND. &
           ASSOCIATED( Atmosphere%Absorber         )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( Atmosphere%Absorber_ID      ) .OR. &
           ASSOCIATED( Atmosphere%Absorber_Units   ) .OR. &
           ASSOCIATED( Atmosphere%Level_Pressure   ) .OR. &
           ASSOCIATED( Atmosphere%Pressure         ) .OR. &
           ASSOCIATED( Atmosphere%Temperature      ) .OR. &
           ASSOCIATED( Atmosphere%Absorber         )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF


    ! ---------------------------------------
    ! Test the members that MAY be associated
    ! ---------------------------------------

    IF ( Include_Cloud ) THEN

      IF ( ALL_Test ) THEN

        IF ( Association_Status             .AND. &
             ASSOCIATED( Atmosphere%Cloud )       ) THEN
          Association_Status = .TRUE.
        END IF

      ELSE

        IF ( Association_Status             .OR. &
             ASSOCIATED( Atmosphere%Cloud )      ) THEN
          Association_Status = .TRUE.
        END IF

      END IF

    END IF

  END FUNCTION CRTM_Associated_Atmosphere





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
!       CRTM_Init_Atmosphere
! 
! PURPOSE:
!       Function to initialize a CRTM_Atmosphere structure.
!
! CATEGORY:
!       CRTM : Atmosphere
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       CALL CRTM_Init_Atmosphere( Atmosphere,     &  ! Output
!                                  RCS_Id = RCS_Id )  ! Revision control
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Atmosphere:   Initialized Atmosphere structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
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
!       This subroutine nullifies the Atmosphere structure pointer members.
!       Therefore, this function should *only* be called to initialise
!       Atmosphere structures before their *first* use. Subsequent
!       re-initialisations should be done using the CRTM_Destroy_Atmosphere()
!       function.
!       
! PROCEDURE:
!       The scalar structure members are reset to a null value and the 
!       pointer members are nullified.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Init_Atmosphere_Scalar( Atmosphere, &  ! Output
                                     RCS_Id      )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( CRTM_Atmosphere_type ),  INTENT( OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),      OPTIONAL, INTENT( OUT ) :: RCS_Id



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

    Atmosphere%n_Allocates = 0


    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    CALL CRTM_Clear_Atmosphere( Atmosphere )


    ! ---------------------------
    ! Nullify the pointer members
    ! ---------------------------

    NULLIFY( Atmosphere%Absorber_ID, &
             Atmosphere%Absorber_Units, &
             Atmosphere%Level_Pressure, &
             Atmosphere%Pressure, &
             Atmosphere%Temperature, &
             Atmosphere%Absorber, &
             Atmosphere%Cloud )

  END SUBROUTINE Init_Atmosphere_Scalar


  SUBROUTINE Init_Atmosphere_Rank1( Atmosphere, &  ! Output
                                    RCS_Id      )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT ) :: RCS_Id


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

    DO n = 1, SIZE( Atmosphere )

      CALL Init_Atmosphere_Scalar( Atmosphere(n) )

    END DO

  END SUBROUTINE Init_Atmosphere_Rank1





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy_Atmosphere
! 
! PURPOSE:
!       Function to re-initialize CRTM_Atmosphere data structures.
!
! CATEGORY:
!       CRTM : Atmosphere
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_Atmosphere( Atmosphere,               &  ! Output
!                                               RCS_Id = RCS_Id,          &  ! Revision control
!                                               Message_Log = Message_Log )  ! Error messaging
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
!       Atmosphere:   Re-initialized Atmosphere structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar
!                                   OR
!                                 Rank-1 array
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
!       This function checks the association status of the Atmosphere structure
!       pointer members. Therefore, this function should *only* be called
!       *after* the Atmosphere structure has been initialised via the
!       CRTM_Init_Atmosphere() subroutine.
!
!       This restriction is due to Fortran-90 not providing any mechanism
!       for initialising pointer association status in derived type definitions.
!       This means the association status of the Atmosphere structure pointer
!       members will be undefined until they are initialised (via the
!       CRTM_Init_Atmosphere() subroutine).
!
! PROCEDURE:
!       The scalar structure members are set to a null value and the 
!       pointer members are deallocated.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Destroy_Atmosphere_Scalar( Atmosphere,   &  ! Output
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
    TYPE( CRTM_Atmosphere_type ), INTENT( IN OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Atmosphere(Scalar)'


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

    CALL CRTM_Clear_Atmosphere( Atmosphere )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. CRTM_Associated_Atmosphere( Atmosphere ) ) RETURN


    ! ------------------------------------
    ! Deallocate the array pointer members
    ! ------------------------------------

    ! -- Deallocate the Atmosphere Absorber_ID member
    IF ( ASSOCIATED( Atmosphere%Absorber_ID ) ) THEN

      DEALLOCATE( Atmosphere%Absorber_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Atmosphere Absorber_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Atmosphere Absorber_Units member
    IF ( ASSOCIATED( Atmosphere%Absorber_Units ) ) THEN

      DEALLOCATE( Atmosphere%Absorber_Units, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Atmosphere Absorber_Units ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Atmosphere Level_Pressure member
    IF ( ASSOCIATED( Atmosphere%Level_Pressure ) ) THEN

      DEALLOCATE( Atmosphere%Level_Pressure, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Atmosphere Level_Pressure ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Atmosphere Pressure member
    IF ( ASSOCIATED( Atmosphere%Pressure ) ) THEN

      DEALLOCATE( Atmosphere%Pressure, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Atmosphere Pressure ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Atmosphere Temperature member
    IF ( ASSOCIATED( Atmosphere%Temperature ) ) THEN

      DEALLOCATE( Atmosphere%Temperature, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Atmosphere Temperature ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Atmosphere Absorber member
    IF ( ASSOCIATED( Atmosphere%Absorber ) ) THEN

      DEALLOCATE( Atmosphere%Absorber, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Atmosphere Absorber ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! ---------------------------------------------------
    ! Deallocate the Cloud structure array pointer member
    ! ---------------------------------------------------

    IF ( ASSOCIATED( Atmosphere%Cloud ) ) THEN


      ! -- Destroy the cloud structure(s)
      Error_Status = CRTM_Destroy_Cloud( Atmosphere%Cloud, &
                                         Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error destroying CRTM_Atmosphere Cloud structure(s).', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF


      ! -- Deallocate the array
      DEALLOCATE( Atmosphere%Cloud, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Atmosphere cloud ", &
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

    Atmosphere%n_Allocates = Atmosphere%n_Allocates - 1

    IF ( Atmosphere%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      Atmosphere%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Atmosphere_Scalar


  FUNCTION Destroy_Atmosphere_Rank1( Atmosphere,   &  ! Output
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
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Atmosphere(Rank-1)'


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

    DO n = 1, SIZE( Atmosphere )

      Scalar_Status = Destroy_Atmosphere_Scalar( Atmosphere(n), &
                                                 Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i5, &
                          &" of CRTM_Atmosphere structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Destroy_Atmosphere_Rank1





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Allocate_Atmosphere
! 
! PURPOSE:
!       Function to allocate CRTM_Atmosphere data structures.
!
! CATEGORY:
!       CRTM : Atmosphere
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_Atmosphere( n_Layers,    &  ! Input
!                                                n_Absorbers, &  ! Input
!                                                n_Clouds,    &  ! Input
!
!                                                Atmosphere,  &  ! Output
!
!                                                RCS_Id = RCS_Id,          &  ! Revision control
!                                                Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Layers:     Number of layers for which there is Atmosphere data.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar OR Rank-1
!                                 See output Atmosphere dimensionality chart
!                     ATTRIBUTES: INTENT( IN )
!
!       n_Absorbers:  Number of absorbers dimension. This will be the same for
!                     all elements if the Atmosphere argument is an array.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       n_Clouds:     Number of clouds dimension of Atmosphere data.
!                     ** Note: Can be = 0 (i.e. clear sky). **
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar OR Rank-1
!                                 See output Atmosphere dimensionality chart
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
!       Atmosphere:   Atmosphere structure with allocated pointer members. The
!                     following table shows the allowable dimension combinations
!                     for the calling routine, where M == number of profiles:
!
!                        Input       Input       Input       Output
!                       n_Layers   n_Absorbers  n_Clouds    Atmosphere
!                       dimension   dimension   dimension   dimension
!                     --------------------------------------------------
!                        scalar      scalar      scalar      scalar
!                        scalar      scalar      scalar        M
!                          M         scalar        M           M
!                        scalar      scalar        M           M
!                          M         scalar      scalar        M
!
!                     Note the number of absorbers cannot vary with the profile.
!
!                     These multiple interfaces are supplied purely for ease of
!                     use depending on what data is available.
!                     
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar or Rank-1
!                                 See chart above.
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
!       CRTM_Associated_Atmosphere: Function to test the association status of the
!                                   pointer members of a Atmosphere structure.
!
!       CRTM_Init_Cloud:            Subroutine to initialize the scalar and pointer
!                                   members of Cloud data structures.
!                                   SOURCE: CRTM_CLOUD_DEFINE module
!
!       Display_Message:            Subroutine to output messages
!                                   SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function checks the association status of the Atmosphere structure
!       pointer members. Therefore, this function should *only* be called
!       *after* the Atmosphere structure has been initialised via the
!       CRTM_Init_Atmosphere() subroutine.
!
!       This restriction is due to Fortran-90 not providing any mechanism
!       for initialising pointer association status in derived type definitions.
!       This means the association status of the Atmosphere structure pointer
!       members will be undefined until they are initialised (via the 
!       CRTM_Init_Atmosphere() subroutine).
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Mar-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Allocate_Atmosphere_Scalar( n_Layers,     &  ! Input
                                       n_Absorbers,  &  ! Input
                                       n_Clouds,     &  ! Input
                                       
                                       Atmosphere,   &  ! Output

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
    INTEGER,                      INTENT( IN )  :: n_Layers    
    INTEGER,                      INTENT( IN )  :: n_Absorbers 
    INTEGER,                      INTENT( IN )  :: n_Clouds    

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), INTENT( OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Atmosphere(Scalar)'


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

    ! -- Number of layers
    IF ( n_Layers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Number of absorbers
    IF ( n_Absorbers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Absorbers must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Number of clouds. Can be == 0.
    IF ( n_Clouds < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Clouds must be > or = 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------------------
    ! Check if ANY pointers are already associated
    ! --------------------------------------------

    IF ( CRTM_Associated_Atmosphere( Atmosphere, ANY_Test = SET ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'One or more CRTM_Atmosphere pointer members are associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ! ---------------
    ! The data arrays
    ! ---------------

    ALLOCATE( Atmosphere%Absorber_ID( n_Absorbers ), &
              Atmosphere%Absorber_Units( n_Absorbers ), &
              Atmosphere%Level_Pressure( n_Layers ), &
              Atmosphere%Pressure( n_Layers ), &
              Atmosphere%Temperature( n_Layers ), &
              Atmosphere%Absorber( n_Layers, n_Absorbers ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating CRTM_Atmosphere data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------------------
    ! The cloud structure array
    ! -------------------------

    IF ( n_Clouds > 0 ) THEN

      ! -- Allocate the structure array
      ALLOCATE( Atmosphere%Cloud( n_Clouds ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error allocating CRTM_Atmosphere Cloud structure array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Initialise the structure array
      CALL CRTM_Init_Cloud( Atmosphere%Cloud )

    END IF



    !#--------------------------------------------------------------------------#
    !#             -- ASSIGN THE DIMENSIONS AND INITALISE ARRAYS --             #
    !#--------------------------------------------------------------------------#

    Atmosphere%n_Layers    = n_Layers
    Atmosphere%n_Absorbers = n_Absorbers
    Atmosphere%n_Clouds    = n_Clouds

    Atmosphere%Level_Pressure = REAL( INVALID, fp_kind )
    Atmosphere%Pressure       = REAL( INVALID, fp_kind )
    Atmosphere%Temperature    = REAL( INVALID, fp_kind )
    Atmosphere%Absorber       = REAL( INVALID, fp_kind )



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    Atmosphere%n_Allocates = Atmosphere%n_Allocates + 1

    IF ( Atmosphere%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      Atmosphere%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Atmosphere_Scalar


  FUNCTION Allocate_Atmosphere_Rank0001( n_Layers,     &  ! Input, scalar
                                         n_Absorbers,  &  ! Input, scalar
                                         n_Clouds,     &  ! Input, scalar
                                       
                                         Atmosphere,   &  ! Output, M

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
    INTEGER,                                      INTENT( IN )  :: n_Layers
    INTEGER,                                      INTENT( IN )  :: n_Absorbers
    INTEGER,                                      INTENT( IN )  :: n_Clouds

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Atmosphere(Rank-0001)'


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

    DO i = 1, SIZE( Atmosphere )

      Scalar_Status = Allocate_Atmosphere_Scalar( n_Layers,      & ! Input
                                                  n_Absorbers,   & ! Input
                                                  n_Clouds,      & ! Input
                                                  Atmosphere(i), & ! Output
                                                  Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Atmosphere structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Atmosphere_Rank0001


  FUNCTION Allocate_Atmosphere_Rank1011( n_Layers,     &  ! Input, M
                                         n_Absorbers,  &  ! Input, scalar
                                         n_Clouds,     &  ! Input, M

                                         Atmosphere,   &  ! Output, M

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
    INTEGER,                      DIMENSION( : ), INTENT( IN )  :: n_Layers
    INTEGER,                                      INTENT( IN )  :: n_Absorbers
    INTEGER,                      DIMENSION( : ), INTENT( IN )  :: n_Clouds

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Atmosphere(Rank-1011)'


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

    ! --------------------------------
    ! Dimensions of n_Layers, n_Clouds
    ! and Atmosphere must be the same
    ! --------------------------------

    n = SIZE( n_Layers )

    IF ( SIZE( n_Clouds )   /= n .OR. &
         SIZE( Atmosphere ) /= n      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers, n_Clouds and Atmosphere arrays'//&
                            ' have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Atmosphere_Scalar( n_Layers(i),   & ! Input
                                                  n_Absorbers,   & ! Input
                                                  n_Clouds(i),   & ! Input
                                                  Atmosphere(i), & ! Output
                                                  Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Atmosphere structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Atmosphere_Rank1011


  FUNCTION Allocate_Atmosphere_Rank0011( n_Layers,     &  ! Input, scalar
                                         n_Absorbers,  &  ! Input, scalar
                                         n_Clouds,     &  ! Input, M
                                       
                                         Atmosphere,   &  ! Output, M

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
    INTEGER,                                      INTENT( IN )  :: n_Layers
    INTEGER,                                      INTENT( IN )  :: n_Absorbers
    INTEGER,                      DIMENSION( : ), INTENT( IN )  :: n_Clouds

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Atmosphere(Rank-0011)'


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

    ! ---------------------------
    ! Dimensions of n_Clouds and
    ! Atmosphere must be the same
    ! ---------------------------

    n = SIZE( n_Clouds )

    IF ( SIZE( Atmosphere ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Clouds and CRTM_Atmosphere arrays'//&
                            ' have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Atmosphere_Scalar( n_Layers,      & ! Input
                                                  n_Absorbers,   & ! Input
                                                  n_Clouds(i),   & ! Input
                                                  Atmosphere(i), & ! Output
                                                  Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Atmosphere structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Atmosphere_Rank0011


  FUNCTION Allocate_Atmosphere_Rank1001( n_Layers,     &  ! Input, M
                                         n_Absorbers,  &  ! Input, scalar
                                         n_Clouds,     &  ! Input, scalar
                                       
                                         Atmosphere,   &  ! Output, M

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
    INTEGER,                      DIMENSION( : ), INTENT( IN )  :: n_Layers
    INTEGER,                                      INTENT( IN )  :: n_Absorbers
    INTEGER,                                      INTENT( IN )  :: n_Clouds

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Atmosphere(Rank-1001)'


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

    ! ---------------------------
    ! Dimensions of n_Layers and
    ! Atmosphere must be the same
    ! ---------------------------

    n = SIZE( n_Layers )

    IF ( SIZE( Atmosphere ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers and CRTM_Atmosphere arrays'//&
                            ' have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Atmosphere_Scalar( n_Layers(i),   & ! Input
                                                  n_Absorbers,   & ! Input
                                                  n_Clouds,      & ! Input
                                                  Atmosphere(i), & ! Output
                                                  Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Atmosphere structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Atmosphere_Rank1001





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_Atmosphere
!
! PURPOSE:
!       Function to copy valid Atmosphere structures.
!
! CATEGORY:
!       CRTM : Atmosphere
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Error_Status = Assign_Atmosphere( Atmosphere_in,  &  ! Input
!                                         Atmosphere_out, &  ! Output
!
!                                         RCS_Id = RCS_Id,          &  ! Revision control
! 
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere_in:   Atmosphere structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar OR Rank-1
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
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
!       Atmosphere_out:  Copy of the input structure, Atmosphere_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Same as Atmosphere_in
!                        ATTRIBUTES: INTENT( OUT )
!
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
!                        If == SUCCESS the structure assignment was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Associated_Atmosphere:  Function to test the association status of the
!                                    pointer members of a CRTM Atmosphere structure.
!
!       CRTM_Allocate_Atmosphere:    Function to allocate the pointer members of
!                                    the CRTM Atmosphere data structure.
!
!       CRTM_Assign_Cloud:           Function to copy valid CRTM Cloud structures.
!                                    SOURCE: CRTM_CLOUD_DEFINE module
!
!       Display_Message:             Subroutine to output messages
!                                    SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function allocates the output Atmosphere structure pointer members.
!       Therefore this function should *only* be called *after* the output
!       Atmosphere structure has been initialised via the Initialize_Atmosphere()
!       subroutine or re-initialised via the Destroy_Atmosphere() function.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Mar-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Assign_Atmosphere_Scalar( Atmosphere_in,  &  ! Input
                                     Atmosphere_out, &  ! Output
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
    TYPE( CRTM_Atmosphere_type ), INTENT( IN )  :: Atmosphere_in

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), INTENT( OUT ) :: Atmosphere_out

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Atmosphere(Scalar)'



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
    ! BUT the Cloud pointer member may not be
    ! ---------------------------------------

    IF ( .NOT. CRTM_Associated_Atmosphere( Atmosphere_In, Skip_Cloud = SET ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT CRTM_Atmosphere pointer '//&
                            'members are NOT associated.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------------------
    ! ANY *output* pointers must NOT be associated
    ! INCLUDING the Cloud pointer member
    ! --------------------------------------------

    IF ( CRTM_Associated_Atmosphere( Atmosphere_Out, ANY_Test = SET ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all OUTPUT CRTM_Atmosphere pointer '//&
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

    Atmosphere_out%Climatology  = Atmosphere_in%Climatology


    ! -----------------
    ! Assign array data
    ! -----------------

    ! -- Allocate data arrays
    Error_Status = CRTM_Allocate_Atmosphere( Atmosphere_in%n_Layers, &
                                             Atmosphere_in%n_Absorbers, &
                                             Atmosphere_in%n_Clouds, &

                                             Atmosphere_out, &
                                             Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output CRTM_Atmosphere arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Copy array data
    Atmosphere_out%Absorber_ID    = Atmosphere_in%Absorber_ID
    Atmosphere_out%Absorber_Units = Atmosphere_in%Absorber_Units
    Atmosphere_out%Level_Pressure = Atmosphere_in%Level_Pressure
    Atmosphere_out%Pressure       = Atmosphere_in%Pressure
    Atmosphere_out%Temperature    = Atmosphere_in%Temperature
    Atmosphere_out%Absorber       = Atmosphere_in%Absorber


    ! ---------------------
    ! Assign structure data
    ! ---------------------

    ! -- Copy Cloud structure
    IF ( Atmosphere_in%n_Clouds > 0 ) THEN

      Error_Status = CRTM_Assign_Cloud( Atmosphere_in%Cloud, &
                                        Atmosphere_out%Cloud, &
                                        Message_Log = Message_Log)

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error copying Cloud structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF

  END FUNCTION Assign_Atmosphere_Scalar


  FUNCTION Assign_Atmosphere_Rank1( Atmosphere_in,  &  ! Input
                                    Atmosphere_out, &  ! Output
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
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN )  :: Atmosphere_in

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( OUT ) :: Atmosphere_out

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Atmosphere(Rank-1)'


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

    n = SIZE( Atmosphere_in )

    IF ( SIZE( Atmosphere_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Atmosphere_in and Atmosphere_out arrays'//&
                            ' have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Assign_Atmosphere_Scalar( Atmosphere_in(i), &
                                                Atmosphere_out(i), &
                                                Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i5, &
                          &" of CRTM_Atmosphere structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Assign_Atmosphere_Rank1

END MODULE CRTM_Atmosphere_Define_old


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: CRTM_Atmosphere_Define.f90,v 1.2 2004/07/01 20:52:51 paulv Exp $
!
! $Date: 2004/07/01 20:52:51 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_Atmosphere_Define.f90,v $
! Revision 1.2  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.1  2004/05/19 19:55:11  paulv
! Initial checkin.
!
!
!
!
