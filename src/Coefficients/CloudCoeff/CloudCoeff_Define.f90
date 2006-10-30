!--------------------------------------------------------------------------------
!M+
! NAME:
!       CloudCoeff_Define
!
! PURPOSE:
!       Module defining the CloudCoeff data structure and containing routines to 
!       manipulate it.
!       
! CATEGORY:
!       Scattering : CloudCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CloudCoeff_Define
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
!       Associated_CloudCoeff:      Function to test the association status
!                                     of the pointer members of a CloudCoeff
!                                     structure.
!
!       Destroy_CloudCoeff:         Function to re-initialize an CloudCoeff
!                                     structure.
!
!       Allocate_CloudCoeff:        Function to allocate the pointer members
!                                     of an CloudCoeff structure.
!
!       Assign_CloudCoeff:          Function to copy an CloudCoeff structure.
!
!       Equal_CloudCoeff:           Function to test if two CloudCoeff
!                                     structures are equal.
!
!       Check_CloudCoeff_Release:   Function to check the CloudCoeff Release value.
!
!       Version_CloudCoeff:         Subroutine to return a string containing
!                                     version and dimension information about
!                                     the CloudCoeff data structure.
!
! DERIVED TYPES:
!       CloudCoeff_type:   Definition of the CloudCoeff data structure. Fields
!                            are...
!
!         Release:                       Coefficient data release.
!                                        UNITS:      N/A
!                                        TYPE:       INTEGER
!                                        DIMENSION:  Scalar
!
!         Version:                       Coefficient data version.
!                                        UNITS:      N/A
!                                        TYPE:       INTEGER
!                                        DIMENSION:  Scalar
!
!
!         *** CloudCoeff COMPONENTS FOLLOW ***
!
!         n_Frequency:             The number of monocromatic frequencies 
!                                  within look-up table (LUT) 
!                                  The "I1" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         n_Size_MW:               The number of discrete effective radii 
!                                  of scatters in MW range 
!                                  The "I2" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         n_wavenumber:            The number of monocromatic wavenumbers within LUT 
!                                  The "I3" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         n_Size_IR:               The number of discrete effective radii 
!                                  of scatters in IR range 
!                                  The "I4" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         n_Temperature:           The number of discrete layer temperatures within LUT 
!                                  The "I5" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         n_Density:               The number of fixed densities for snow, graupel, hail/ice 
!                                  The "I6" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         Max_Legendre_Terms:      The maximum number of Legendre polynomial
!                                  terms.
!                                  The "I7" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         n_Legendre_Terms:        The number of Legendre polynomial terms used to
!                                  describe the scattering phase matrix. The
!         Max_Phase_Elements:      The maximum number of phase elements within LUT.
!                                  The "I8" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         n_Phase_Elements:        The number of unique, non-zero elements in the
!                                  scattering matrix, *P*. The scattering matrix is a
!                                  4x4 matrix that connects the Stokes vectors of the
!                                  incident to the scattered radiation. For spherical
!                                  scatters, it has only 4 independent elements. 
!
!         frequency:               Frequencies within LUT 
!                                  UNITS:     GHz 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-1 (I1)
!                                        ATTRIBUTES: POINTER
!
!         wavenumber:              wavenumbers: within LUT 
!                                  UNITS:     1/cm 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-1 (I3)
!                                        ATTRIBUTES: POINTER
!
!         Reff_MW:                 Effective radii for microwave range 
!                                  UNITS:     millimeter 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-1 (I2)
!                                        ATTRIBUTES: POINTER
!
!         Reff_IR:                 Effective radii for infrared range 
!                                  UNITS:     millimeter 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-1 (I4)
!                                        ATTRIBUTES: POINTER
!
!         Temperature:             Layer temperatures within LUT 
!                                  UNITS:     Kelvin 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-1 (I5)
!                                        ATTRIBUTES: POINTER
!
!         Density:                 Cloud water content density within LUT 
!                                  UNITS:     g / cm^3 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-1 (I6)
!                                        ATTRIBUTES: POINTER
!
!         ext_L_MW:                Mass extinction coefficient for liquid phase (L) 
!                                  (cloud liquid, rain) in microwave (MW) range
!                                  Optica_Depth = ext_L_MW * Cloud_Water_Content_in_mm 
!                                  UNITS:     1/mm 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-3 (I1 x I2 x I5)
!                                        ATTRIBUTES: POINTER
!
!           w_L_MW:                Single scattering albedo for liquid phase (L) in microwave range
!                                  UNITS:     N/A 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-3 (I1 x I2 x I5) 
!                                        ATTRIBUTES: POINTER
!
!           g_L_MW:                Asymmetry Factor for liquid phase (L) in microwave range
!                                  UNITS:     N/A 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-3 (I1 x I2 x I5) 
!                                        ATTRIBUTES: POINTER
!
!         ext_S_MW:                Mass extinction coefficient for solid phase (S) 
!                                  (snow, graupel, hail/ice) in microwave (MW) range
!                                  Optica_Depth = ext_S_MW * Cloud_Water_Content_in_mm 
!                                  UNITS:     1/mm 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-3 (I1 x I2 x I6) 
!                                        ATTRIBUTES: POINTER
!
!           w_S_MW:                Single scattering albedo for solid phase (S) in microwave range
!                                  UNITS:     N/A 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-3 (I1 x I2 x I6) 
!                                        ATTRIBUTES: POINTER
!
!           g_S_MW:                Asymmetry Factor for solid phase (S) in microwave range
!                                  UNITS:     N/A 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-3 (I1 x I2 x I6) 
!                                        ATTRIBUTES: POINTER
!
!           phase_coeff_L_MW:      Legendre expansion coefficient for liquid phase (L) in microwave range
!                                  UNITS:     N/A 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-5 (I1 x I2 x I5 x I7 x I8) 
!                                        ATTRIBUTES: POINTER
!
!           phase_coeff_S_MW:      Legendre expansion coefficient for solid phase (S) in microwave range
!                                  UNITS:     N/A 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-5 (I1 x I2 x I6 x I7 x I8) 
!                                        ATTRIBUTES: POINTER
!
!         ext_L_IR:                Mass extinction coefficient for liquid phase (L) 
!                                  (cloud liquid, rain) in infrared (IR) range
!                                  Optica_Depth = ext_L_IR * Cloud_Water_Content_in_mm 
!                                  UNITS:     1/mm 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-2 (I3 x I4)
!                                        ATTRIBUTES: POINTER
!
!           w_L_IR:                Single scattering albedo for liquid phase (L) in infrared range
!                                  UNITS:     N/A 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-2 (I3 x I4) 
!                                        ATTRIBUTES: POINTER
!
!           g_L_IR:                Asymmetry Factor for liquid phase (L) in infrared range
!                                  UNITS:     N/A 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-2 (I3 x I4) 
!                                        ATTRIBUTES: POINTER
!
!         ext_S_IR:                Mass extinction coefficient for solid phase (S) 
!                                  (snow, graupel, hail/ice) in infrared (IR) range
!                                  Optica_Depth = ext_S_IR * Cloud_Water_Content_in_mm 
!                                  UNITS:     1/mm 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-3 (I3 x I4 x I6) 
!                                        ATTRIBUTES: POINTER
!
!           w_S_MW:                Single scattering albedo for solid phase (S) in infrared range
!                                  UNITS:     N/A 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-3 (I3 x I4 x I6) 
!                                        ATTRIBUTES: POINTER
!
!           g_S_MW:                Asymmetry Factor for solid phase (S) in infrared range
!                                  UNITS:     N/A 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-3 (I3 x I4 x I6) 
!                                        ATTRIBUTES: POINTER
!
!           phase_coeff_L_IR:      Legendre expansion coefficient for liquid phase (L) in infrared range
!                                  UNITS:     N/A 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-4 (I3 x I4 x I7 x I8) 
!                                        ATTRIBUTES: POINTER
!
!           phase_coeff_S_IR:      Legendre expansion coefficient for solid phase (S) in infrared range
!                                  UNITS:     N/A 
!                                  TYPE:      REAL( Double ) 
!                                  DIMENSION:  Rank-5 (I3 x I4 x I6 x I7 x I8) 
!                                        ATTRIBUTES: POINTER
!
!
!        ** THE ACTUAL CloudCoeff COMPONENTS LISTED HERE **
!
!
!
!
!       *!IMPORTANT!*
!       -------------
!       Note that the CloudCoeff_type is PUBLIC and its members are not
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
!       Written by:     Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2005 Yong Han, Quanhua Liu, Paul van Delst
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

MODULE CloudCoeff_Define


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

  ! -- Public procedures to manipulate the CloudCoeff structure
  PUBLIC :: Associated_CloudCoeff
  PUBLIC :: Destroy_CloudCoeff
  PUBLIC :: Allocate_CloudCoeff
  PUBLIC :: Assign_CloudCoeff
  PUBLIC :: Equal_CloudCoeff
  PUBLIC :: Check_CloudCoeff_Release
  PUBLIC :: Version_CloudCoeff


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CloudCoeff_Define.f90,v 1.2 2006/05/02 14:58:34 dgroff Exp $'

  ! -- CloudCoeff scalar member invalid value
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Sensor descriptor component string length
  INTEGER, PRIVATE, PARAMETER :: DL = 20

  ! -- Current valid release and version numbers
  INTEGER, PRIVATE, PARAMETER :: CloudCoeff_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PRIVATE, PARAMETER :: CloudCoeff_VERSION = 1  ! This is just the data version.


  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! -- Number of CloudCoeff pointer data items
  INTEGER( Long ), PUBLIC, PARAMETER :: N_CloudCoeff_ITEMS = 22_Long

  ! -- Data types of the CloudCoeff pointer data
  !    7 = Character string
  !    5 = Double (i.e. 8-byte float)
  !    4 = Single (i.e. 4-byte float)
  !    3 = Long   (i.e. 4-byte integer)
  INTEGER( Long ), PUBLIC, PARAMETER, &
                   DIMENSION( N_CloudCoeff_ITEMS ) :: CloudCoeff_DATA_TYPE = &
                                                       (/ 5_Long, &  ! frequency
                                                          5_Long, &  ! wavenumber
                                                          5_Long, &  ! Reff_MW 
                                                          5_Long, &  ! Reff_IR
                                                          5_Long, &  ! Temperature
                                                          5_Long, &  ! Density
                                                          5_Long, &  ! ext_L_MW
                                                          5_Long, &  ! w_L_MW
                                                          5_Long, &  ! g_L_MW
                                                          5_Long, &  ! ext_S_MW
                                                          5_Long, &  ! w_S_MW
                                                          5_Long, &  ! g_S_MW
                                                          5_Long, &  ! phase_coeff_L_MW
                                                          5_Long, &  ! phase_coeff_S_MW
                                                          5_Long, &  ! ext_L_IR
                                                          5_Long, &  ! w_L_IR
                                                          5_Long, &  ! g_L_IR
                                                          5_Long, &  ! ext_S_IR
                                                          5_Long, &  ! w_S_IR
                                                          5_Long, &  ! g_S_IR
                                                          5_Long, &  ! phase_coeff_L_IR
                                                          5_Long /)  ! phase_coeff_S_IR

  ! -- Names of the pointer data items (for error processing)
  CHARACTER( * ), PUBLIC, PARAMETER, &
                  DIMENSION( N_CloudCoeff_ITEMS ) :: CloudCoeff_DATA_NAME = &
                                                    (/ 'frequency       ', &
                                                       'wavenumber      ', &
                                                       'Reff_MW         ', &
                                                       'Reff_IR         ', &
                                                       'Temperature     ', &
                                                       'Density         ', &
                                                       'ext_L_MW        ', &
                                                       'w_L_MW          ', &
                                                       'g_L_MW          ', &
                                                       'ext_S_MW        ', &
                                                       'w_S_MW          ', &
                                                       'g_S_MW          ', &
                                                       'phase_coeff_L_MW', &
                                                       'phase_coeff_S_MW', &
                                                       'ext_L_IR        ', &
                                                       'w_L_IR          ', &
                                                       'g_L_IR          ', &
                                                       'ext_S_IR        ', &
                                                       'w_S_IR          ', &
                                                       'g_S_IR          ', &
                                                       'phase_coeff_L_IR', &
                                                       'phase_coeff_S_MW' /)


  ! ------------------------------
  ! CloudCoeff data type definition, 
  !    MW : microwave, IR : infrared, L : liquid phase, S : solid phase
  ! ------------------------------
  TYPE, PUBLIC :: CloudCoeff_type
    INTEGER :: n_Allocates
                                  
    INTEGER( Long ) :: Release
    INTEGER( Long ) :: Version
                                                                 
    ! *** DUMMY COMPONENTS ***
    INTEGER( Long ) :: n_Frequency         = 0   ! I1 dimension 
    INTEGER( Long ) :: n_Size_MW           = 0   ! I2 dimension
    INTEGER( Long ) :: n_wavenumber        = 0   ! I3 dimension
    INTEGER( Long ) :: n_Size_IR           = 0   ! I4 dimension
    INTEGER( Long ) :: n_Temperature       = 0   ! I5 dimension
    INTEGER( Long ) :: n_Density           = 0   ! I6 dimension
    INTEGER( Long ) :: Max_Legendre_Terms  = 0   ! I7 dimension
    INTEGER( Long ) :: n_Legendre_Terms    = 0   
    INTEGER( Long ) :: Max_Phase_Elements  = 0   ! I8 dimension
    INTEGER( Long ) :: n_Phase_Elements    = 0   

    REAL( Double ), POINTER, DIMENSION( :) ::  frequency => NULL()     ! I1
    REAL( Double ), POINTER, DIMENSION( :) ::  wavenumber => NULL()    ! I3
    REAL( Double ), POINTER, DIMENSION( :) ::  Reff_MW => NULL()       ! I2 
    REAL( Double ), POINTER, DIMENSION( :) ::  Reff_IR => NULL()       ! I4
    REAL( Double ), POINTER, DIMENSION( :) ::  Temperature => NULL()   ! I5
    REAL( Double ), POINTER, DIMENSION( :) ::  Density => NULL()       ! I6
    REAL( Double ), POINTER, DIMENSION( :, :, : ) :: ext_L_MW => NULL()  ! I1 x I2 x I5
    REAL( Double ), POINTER, DIMENSION( :, :, : ) :: w_L_MW => NULL()    ! I1 x I2 x I5
    REAL( Double ), POINTER, DIMENSION( :, :, : ) :: g_L_MW => NULL()    ! I1 x I2 x I5
    REAL( Double ), POINTER, DIMENSION( :, :, : ) :: ext_S_MW => NULL()  ! I1 x I2 x I6
    REAL( Double ), POINTER, DIMENSION( :, :, : ) :: w_S_MW => NULL()    ! I1 x I2 x I6
    REAL( Double ), POINTER, DIMENSION( :, :, : ) :: g_S_MW => NULL()    ! I1 x I2 x I6
    REAL( Double ), POINTER, DIMENSION( :, :, :, :, : ) :: phase_coeff_L_MW => NULL()  ! I1 x I2 x I5 x I7 x I8
    REAL( Double ), POINTER, DIMENSION( :, :, :, :, : ) :: phase_coeff_S_MW => NULL()  ! I1 x I2 x I6 x I7 x I8
    REAL( Double ), POINTER, DIMENSION( :, : ) :: ext_L_IR => NULL()  ! I3 x I4
    REAL( Double ), POINTER, DIMENSION( :, : ) :: w_L_IR => NULL()    ! I3 x I4
    REAL( Double ), POINTER, DIMENSION( :, : ) :: g_L_IR => NULL()    ! I3 x I4
    REAL( Double ), POINTER, DIMENSION( :, :, : ) :: ext_S_IR => NULL()  ! I3 x I4 x I6
    REAL( Double ), POINTER, DIMENSION( :, :, : ) :: w_S_IR => NULL()    ! I3 x I4 x I6
    REAL( Double ), POINTER, DIMENSION( :, :, : ) :: g_S_IR => NULL()    ! I3 x I4 x I6
    REAL( Double ), POINTER, DIMENSION( :, :, : ) :: phase_coeff_L_IR => NULL() ! I3 x I4 x I7
    REAL( Double ), POINTER, DIMENSION( :, :, :, : ) :: phase_coeff_S_IR => NULL() ! I3 x I4 x I6 x I7
  END TYPE CloudCoeff_type



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
!       Clear_CloudCoeff
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CloudCoeff structure.
!
! CATEGORY:
!       Scattering : CloudCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_CloudCoeff( CloudCoeff ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       CloudCoeff:  CloudCoeff structure for which the scalar members have
!                      been cleared.
!                      UNITS:      N/A
!                      TYPE:       CloudCoeff_type
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
!       Note the INTENT on the output CloudCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_CloudCoeff( CloudCoeff )

    TYPE( CloudCoeff_type ), INTENT( IN OUT ) :: CloudCoeff

    CloudCoeff%n_Frequency =  0
    CloudCoeff%n_Size_MW = 0
    CloudCoeff%n_wavenumber =  0
    CloudCoeff%n_Size_IR =  0
    CloudCoeff%n_Temperature = 0
    CloudCoeff%n_Density = 0
    CloudCoeff%Max_Legendre_Terms = 0
    CloudCoeff%n_Legendre_Terms =  0
    CloudCoeff%Max_Phase_Elements = 0
    CloudCoeff%n_Phase_Elements = 0

  END SUBROUTINE Clear_CloudCoeff





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
!       Associated_CloudCoeff
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       CloudCoeff structure.
!
! CATEGORY:
!       Scattering : CloudCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_CloudCoeff( CloudCoeff,       &  ! Input
!                                                     ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       CloudCoeff:        CloudCoeff structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CloudCoeff_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            CloudCoeff structure pointer members are associated.
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
!                            association status of the CloudCoeff pointer members.
!                            .TRUE.  - if ALL the CloudCoeff pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the CloudCoeff pointer
!                                      members are associated.
!                            .FALSE. - some or all of the CloudCoeff pointer
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
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_CloudCoeff( CloudCoeff, & ! Input
                                    ANY_Test )    & ! Optional input
                                  RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CloudCoeff_type ), INTENT( IN ) :: CloudCoeff

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

    Association_Status = .FALSE.
                                                  
    IF ( ALL_Test ) THEN
                                                
      IF ( ASSOCIATED( CloudCoeff%frequency ) .AND. &
           ASSOCIATED( CloudCoeff%wavenumber ) .AND. &
           ASSOCIATED( CloudCoeff%Reff_MW ) .AND. &
           ASSOCIATED( CloudCoeff%Reff_IR ) .AND. &
           ASSOCIATED( CloudCoeff%Temperature ) .AND. &
           ASSOCIATED( CloudCoeff%Density ) .AND. &
           ASSOCIATED( CloudCoeff%ext_L_MW ) .AND. &
           ASSOCIATED( CloudCoeff%w_L_MW ) .AND. &
           ASSOCIATED( CloudCoeff%g_L_MW ) .AND. &
           ASSOCIATED( CloudCoeff%ext_S_MW ) .AND. &
           ASSOCIATED( CloudCoeff%w_S_MW ) .AND. &
           ASSOCIATED( CloudCoeff%g_S_MW ) .AND. &
           ASSOCIATED( CloudCoeff%phase_coeff_L_MW ) .AND. &
           ASSOCIATED( CloudCoeff%phase_coeff_S_MW ) .AND. &
           ASSOCIATED( CloudCoeff%ext_L_IR ) .AND. &
           ASSOCIATED( CloudCoeff%w_L_IR ) .AND. &
           ASSOCIATED( CloudCoeff%g_L_IR ) .AND. &
           ASSOCIATED( CloudCoeff%ext_S_IR ) .AND. &
           ASSOCIATED( CloudCoeff%w_S_IR ) .AND. &
           ASSOCIATED( CloudCoeff%g_S_IR ) .AND. &
           ASSOCIATED( CloudCoeff%phase_coeff_L_IR ) .AND. &
           ASSOCIATED( CloudCoeff%phase_coeff_S_IR ) ) THEN
        Association_Status = .TRUE.
      END IF
                                   
    ELSE
                                                    
      IF ( ASSOCIATED( CloudCoeff%frequency ) .OR. &
           ASSOCIATED( CloudCoeff%wavenumber ) .OR. &
           ASSOCIATED( CloudCoeff%Reff_MW ) .OR. &
           ASSOCIATED( CloudCoeff%Reff_IR ) .OR. &
           ASSOCIATED( CloudCoeff%Temperature ) .OR. &
           ASSOCIATED( CloudCoeff%Density ) .OR. &
           ASSOCIATED( CloudCoeff%ext_L_MW ) .OR. &
           ASSOCIATED( CloudCoeff%w_L_MW ) .OR. &
           ASSOCIATED( CloudCoeff%g_L_MW ) .OR. &
           ASSOCIATED( CloudCoeff%ext_S_MW ) .OR. &
           ASSOCIATED( CloudCoeff%w_S_MW ) .OR. &
           ASSOCIATED( CloudCoeff%g_S_MW ) .OR. &
           ASSOCIATED( CloudCoeff%phase_coeff_L_MW ) .OR. &
           ASSOCIATED( CloudCoeff%phase_coeff_S_MW ) .OR. &
           ASSOCIATED( CloudCoeff%ext_L_IR ) .OR. &
           ASSOCIATED( CloudCoeff%w_L_IR ) .OR. &
           ASSOCIATED( CloudCoeff%g_L_IR ) .OR. &
           ASSOCIATED( CloudCoeff%ext_S_IR ) .OR. &
           ASSOCIATED( CloudCoeff%w_S_IR ) .OR. &
           ASSOCIATED( CloudCoeff%g_S_IR ) .OR. &
           ASSOCIATED( CloudCoeff%phase_coeff_L_IR ) .OR. &
           ASSOCIATED( CloudCoeff%phase_coeff_S_IR ) ) THEN
        Association_Status = .TRUE.
      END IF
                                         
    END IF


  END FUNCTION Associated_CloudCoeff





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_CloudCoeff
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of CloudCoeff
!       data structures.
!
! CATEGORY:
!       Scattering : CloudCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_CloudCoeff( CloudCoeff,             &  ! Output
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
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       CloudCoeff: Re-initialized CloudCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       CloudCoeff_type
!                     DIMENSION:  Scalar
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
!       None.
!
! COMMENTS:
!       Note the INTENT on the output CloudCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Destroy_CloudCoeff( CloudCoeff, &  ! Output
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
    TYPE( CloudCoeff_type ), INTENT( IN OUT ) :: CloudCoeff

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_CloudCoeff'


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

    IF ( Clear ) CALL Clear_CloudCoeff( CloudCoeff )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_CloudCoeff( CloudCoeff ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate frequency
    IF ( ASSOCIATED( CloudCoeff%frequency ) ) THEN
                                        
      DEALLOCATE( CloudCoeff%frequency, STAT = Allocate_Status )
                                          
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff frequency ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                                                 
    ! -- Deallocate wavenumber
    IF ( ASSOCIATED( CloudCoeff%wavenumber ) ) THEN
                                                     
      DEALLOCATE( CloudCoeff%wavenumber, STAT = Allocate_Status )
                                           
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff wavenumber ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                                           
    ! -- Deallocate Reff_MW 
    IF ( ASSOCIATED( CloudCoeff%Reff_MW ) ) THEN
                                                       
      DEALLOCATE( CloudCoeff%Reff_MW, STAT = Allocate_Status )
                                                    
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff Reff_MW ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                                           
    ! -- Deallocate 
    IF ( ASSOCIATED( CloudCoeff%Reff_IR ) ) THEN
                                                           
      DEALLOCATE( CloudCoeff%Reff_IR, STAT = Allocate_Status )
                                                  
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff Reff_IR ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                                              
    ! -- Deallocate Temperature
    IF ( ASSOCIATED( CloudCoeff%Temperature ) ) THEN
                                                
      DEALLOCATE( CloudCoeff%Temperature, STAT = Allocate_Status )
                                                  
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff Temperature ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                                       
    ! -- Deallocate Density
    IF ( ASSOCIATED( CloudCoeff%Density ) ) THEN
                                           
      DEALLOCATE( CloudCoeff%Density, STAT = Allocate_Status )
                                          
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff Density ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                                              
    ! -- Deallocate ext_L_MW 
    IF ( ASSOCIATED( CloudCoeff%ext_L_MW ) ) THEN
                                               
      DEALLOCATE( CloudCoeff%ext_L_MW, STAT = Allocate_Status )
                                             
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff ext_L_MW ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                                         
    ! -- Deallocate w_L_MW 
    IF ( ASSOCIATED( CloudCoeff%w_L_MW ) ) THEN
                                                
      DEALLOCATE( CloudCoeff%w_L_MW, STAT = Allocate_Status )
                                      
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff w_L_MW ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
    
    ! -- Deallocate g_L_MW 
    IF ( ASSOCIATED( CloudCoeff%g_L_MW ) ) THEN
                                   
      DEALLOCATE( CloudCoeff%g_L_MW, STAT = Allocate_Status )
                                                 
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff g_L_MW ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                                                 
    ! -- Deallocate ext_S_MW 
    IF ( ASSOCIATED( CloudCoeff%ext_S_MW ) ) THEN
                                                
      DEALLOCATE( CloudCoeff%ext_S_MW, STAT = Allocate_Status )
                                             
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff ext_S_MW ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                                                
    ! -- Deallocate w_S_MW 
    IF ( ASSOCIATED( CloudCoeff%w_S_MW ) ) THEN
                                                         
      DEALLOCATE( CloudCoeff%w_S_MW, STAT = Allocate_Status )
                                                      
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff w_S_MW ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
    
    ! -- Deallocate g_S_MW 
    IF ( ASSOCIATED( CloudCoeff%g_S_MW ) ) THEN
                                                   
      DEALLOCATE( CloudCoeff%g_S_MW, STAT = Allocate_Status )
                                                  
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff g_S_MW ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                                                        
    ! -- Deallocate phase_coeff_L_MW
    IF ( ASSOCIATED( CloudCoeff%phase_coeff_L_MW ) ) THEN
                                                      
      DEALLOCATE( CloudCoeff%phase_coeff_L_MW, STAT = Allocate_Status )
                                                       
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff phase_coeff_L_MW ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                                              
    ! -- Deallocate phase_coeff_S_MW
    IF ( ASSOCIATED( CloudCoeff%phase_coeff_S_MW ) ) THEN
                                            
      DEALLOCATE( CloudCoeff%phase_coeff_S_MW, STAT = Allocate_Status )
                                                   
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff phase_coeff_S_MW ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                             
    ! -- Deallocate ext_L_IR 
    IF ( ASSOCIATED( CloudCoeff%ext_L_IR ) ) THEN
                                                       
      DEALLOCATE( CloudCoeff%ext_L_IR, STAT = Allocate_Status )
                                                          
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff ext_L_IR ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                                                                
    ! -- Deallocate w_L_IR 
    IF ( ASSOCIATED( CloudCoeff%w_L_IR ) ) THEN
                                                    
      DEALLOCATE( CloudCoeff%w_L_IR, STAT = Allocate_Status )
                                                                
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff w_L_IR ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
    
    ! -- Deallocate g_L_IR 
    IF ( ASSOCIATED( CloudCoeff%g_L_IR ) ) THEN
                                   
      DEALLOCATE( CloudCoeff%g_L_IR, STAT = Allocate_Status )
                                    
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff g_L_IR ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                                                 
    ! -- Deallocate ext_S_IR 
    IF ( ASSOCIATED( CloudCoeff%ext_S_IR ) ) THEN
                                           
      DEALLOCATE( CloudCoeff%ext_S_IR, STAT = Allocate_Status )
                                                 
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff ext_S_IR ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                                    
    ! -- Deallocate w_S_IR 
    IF ( ASSOCIATED( CloudCoeff%w_S_IR ) ) THEN
                                                
      DEALLOCATE( CloudCoeff%w_S_IR, STAT = Allocate_Status )
                                            
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff w_S_IR ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
    
    ! -- Deallocate g_S_IR 
    IF ( ASSOCIATED( CloudCoeff%g_S_IR ) ) THEN
                                                   
      DEALLOCATE( CloudCoeff%g_S_IR, STAT = Allocate_Status )
                                            
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff g_S_IR ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                                                        
    ! -- Deallocate phase_coeff_L_IR
    IF ( ASSOCIATED( CloudCoeff%phase_coeff_L_IR ) ) THEN
                                                    
      DEALLOCATE( CloudCoeff%phase_coeff_L_IR, STAT = Allocate_Status )
                                                   
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff phase_coeff_L_IR ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
                                             
    ! -- Deallocate phase_coeff_S_IR
    IF ( ASSOCIATED( CloudCoeff%phase_coeff_S_IR ) ) THEN
                                       
      DEALLOCATE( CloudCoeff%phase_coeff_S_IR, STAT = Allocate_Status )
                                           
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CloudCoeff phase_coeff_S_IR ", &
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
                                                  
    CloudCoeff%n_Allocates = CloudCoeff%n_Allocates - 1
                                                     
    IF ( CloudCoeff%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      CloudCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_CloudCoeff





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_CloudCoeff
! 
! PURPOSE:
!       Function to allocate the pointer members of the CloudCoeff
!       data structure.
!
! CATEGORY:
!       Scattering : CloudCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_CloudCoeff( n_Frequency, n_Size_MW, n_wavenumber, n_Size_IR, ! Input
!                                             n_Temperature, n_Density, Max_Legendre_Terms,    ! Input
!                                             Max_Phase_Elements, &  ! Input
!                                             CloudCoeff,             &  ! Output
!                                             RCS_Id = RCS_Id,          &  ! Revision control
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!         n_Frequency:             The number of monocromatic frequencies 
!                                  within look-up table (LUT) 
!                                  The "I1" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!         n_Size_MW:               The number of discrete effective radii 
!                                  of scatters in MW range 
!                                  The "I2" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!         n_wavenumber:            The number of monocromatic wavenumbers within LUT 
!                                  The "I3" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!         n_Size_IR:               The number of discrete effective radii 
!                                  of scatters in IR range 
!                                  The "I4" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!         n_Temperature:           The number of discrete layer temperatures within LUT 
!                                  The "I5" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!         n_Density:               The number of fixed densities for snow, graupel, hail/ice 
!                                  The "I6" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!         Max_Legendre_Terms:      The maximum number of Legendre polynomial
!                                  terms.
!                                  The "I7" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!         Max_Phase_Elements:      The maximum number of phase elements 
!                                  The "I8" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      None
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       CloudCoeff:       CloudCoeff structure with allocated pointer members
!                           UNITS:      N/A
!                           TYPE:       CloudCoeff_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      None
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the ERROR_HANDLER module.
!                           If == SUCCESS the structure pointer allocations were
!                                         successful
!                              == FAILURE - an error occurred, or
!                                         - the structure internal allocation counter
!                                           is not equal to one (1) upon exiting this
!                                           function. This value is incremented and
!                                           decremented for every structure allocation
!                                           and deallocation respectively.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!       Associated_CloudCoeff:  Function to test the association status of the
!                                 pointer members of a CloudCoeff structure.
!
!       Destroy_CloudCoeff:     Function to re-initialize the scalar and pointer
!                                 members of CloudCoeff data structures.
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
!       Note the INTENT on the output CloudCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Allocate_CloudCoeff( n_Frequency, n_Size_MW, n_wavenumber, n_Size_IR, & ! Input
                                  n_Temperature, n_Density, Max_Legendre_Terms,    & ! Input
                                  Max_Phase_Elements, &  ! Input
                                  CloudCoeff,     &  ! Output
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
    INTEGER, INTENT( IN )  :: n_Frequency, n_Size_MW, n_wavenumber, n_Size_IR

    INTEGER, INTENT( IN )  :: n_Temperature, n_Density, Max_Legendre_Terms, Max_Phase_Elements 
    ! -- Output
    TYPE( CloudCoeff_type ), INTENT( IN OUT ) :: CloudCoeff

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_CloudCoeff'


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

    ! --------------
    ! The dimensions
    ! --------------

    IF ( n_Frequency < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Frequency must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_Size_MW < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Size_MW must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_wavenumber < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_wavenumber must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_Size_IR < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Size_IR must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_Temperature < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Temperature must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_Density < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Density must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( Max_Legendre_Terms < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Max_Legendre_Terms must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( Max_Phase_Elements < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Max_Phase_Elements must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_CloudCoeff( CloudCoeff, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_CloudCoeff( CloudCoeff, &
                                           No_Clear = SET, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CloudCoeff pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( CloudCoeff%frequency(n_Frequency), &
              CloudCoeff%wavenumber(n_wavenumber), &
              CloudCoeff%Reff_MW(n_Size_MW), &
              CloudCoeff%Reff_IR(n_Size_IR), &
              CloudCoeff%Temperature(n_Temperature), &
              CloudCoeff%Density(n_Density), &
              CloudCoeff%ext_L_MW(n_Frequency,n_Size_MW,n_Temperature), &
              CloudCoeff%w_L_MW(n_Frequency,n_Size_MW,n_Temperature), &
              CloudCoeff%g_L_MW(n_Frequency,n_Size_MW,n_Temperature), &
              CloudCoeff%ext_S_MW(n_Frequency,n_Size_MW,n_Density), &
              CloudCoeff%w_S_MW(n_Frequency,n_Size_MW,n_Density), &
              CloudCoeff%g_S_MW(n_Frequency,n_Size_MW,n_Density), &
   CloudCoeff%phase_coeff_L_MW(n_Frequency,n_Size_MW,n_Temperature,0:Max_Legendre_Terms,Max_Phase_Elements), &
   CloudCoeff%phase_coeff_S_MW(n_Frequency,n_Size_MW,n_Density,0:Max_Legendre_Terms,Max_Phase_Elements), &
              CloudCoeff%ext_L_IR(n_wavenumber,n_Size_IR), &
              CloudCoeff%w_L_IR(n_wavenumber,n_Size_IR), &
              CloudCoeff%g_L_IR(n_wavenumber,n_Size_IR), &
              CloudCoeff%ext_S_IR(n_wavenumber,n_Size_IR,n_Density), &
              CloudCoeff%w_S_IR(n_wavenumber,n_Size_IR,n_Density), &
              CloudCoeff%g_S_IR(n_wavenumber,n_Size_IR,n_Density), &
   CloudCoeff%phase_coeff_L_IR(n_wavenumber,n_Size_IR,0:Max_Legendre_Terms), &
   CloudCoeff%phase_coeff_S_IR(n_wavenumber,n_Size_IR,n_Density,0:Max_Legendre_Terms), &
              STAT = Allocate_Status )


    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating CloudCoeff data arrays. STAT = ", i5 )' ) &
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

    CloudCoeff%n_Frequency =  n_Frequency
    CloudCoeff%n_Size_MW = n_Size_MW
    CloudCoeff%n_wavenumber =  n_wavenumber
    CloudCoeff%n_Size_IR =  n_Size_IR
    CloudCoeff%n_Temperature = n_Temperature
    CloudCoeff%n_Density = n_Density
    CloudCoeff%Max_Legendre_Terms = Max_Legendre_Terms
    CloudCoeff%n_Legendre_Terms =  Max_Legendre_Terms 
    CloudCoeff%Max_Phase_Elements = Max_Phase_Elements
    CloudCoeff%n_Phase_Elements = Max_Phase_Elements 


    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    CloudCoeff%n_Allocates = CloudCoeff%n_Allocates + 1

    IF ( CloudCoeff%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      CloudCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_CloudCoeff





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_CloudCoeff
!
! PURPOSE:
!       Function to copy valid CloudCoeff structures.
!
! CATEGORY:
!       Scattering : CloudCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_CloudCoeff( CloudCoeff_in,          &  ! Input
!                                           CloudCoeff_out,         &  ! Output
!                                           RCS_Id = RCS_Id,          &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       CloudCoeff_in:   CloudCoeff structure which is to be copied.
!                          UNITS:      N/A
!                          TYPE:       CloudCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       CloudCoeff_out:  Copy of the input structure, CloudCoeff_in.
!                          UNITS:      N/A
!                          TYPE:       CloudCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      None
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
!       Associated_CloudCoeff:  Function to test the association status of the
!                                 pointer members of a CloudCoeff structure.
!
!       Allocate_CloudCoeff:    Function to allocate the pointer members of
!                                 the CloudCoeff data structure.
!
!       Display_Message:          Subroutine to output messages
!                                 SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       The CloudCoeff structure copy is performed only if *all* of the
!       pointer members of the input structure are associated. If this is
!       not the case, then the input structure is treated as "empty" and
!       the output structure is thus simply destroyed upon exit to reflect
!       this "empty" status.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output CloudCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Assign_CloudCoeff( CloudCoeff_in,  &  ! Input
                                CloudCoeff_out, &  ! Output
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
    TYPE( CloudCoeff_type ), INTENT( IN )     :: CloudCoeff_in

    ! -- Output
    TYPE( CloudCoeff_type ), INTENT( IN OUT ) :: CloudCoeff_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_CloudCoeff'



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

    ! ----------------------------------------------
    ! ALL *input* pointers must be associated.
    !
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    ! ----------------------------------------------

    IF ( .NOT. Associated_CloudCoeff( CloudCoeff_In ) ) THEN

      Error_Status = Destroy_CloudCoeff( CloudCoeff_Out, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating output CRTM_CloudCoeff pointer members.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      RETURN

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Allocate the structure
    ! ----------------------

    Error_Status = Allocate_CloudCoeff( CloudCoeff_in%n_Frequency,     &
                                          CloudCoeff_in%n_Size_MW,          &
                                          CloudCoeff_in%n_wavenumber,       &
                                          CloudCoeff_in%n_Size_IR,          &
                                          CloudCoeff_in%n_Temperature,      &
                                          CloudCoeff_in%n_Density,          &
                                          CloudCoeff_in%Max_Legendre_Terms, &
                                          CloudCoeff_in%Max_Phase_Elements, &
                                          CloudCoeff_out, &
                                          Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output CloudCoeff arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------
    ! Assign scalar members
    ! ---------------------

    CloudCoeff_out%Release = CloudCoeff_in%Release
    CloudCoeff_out%Version = CloudCoeff_in%Version


    ! -----------------
    ! Assign array data
    ! -----------------

    CloudCoeff_out%frequency = CloudCoeff_in%frequency
    CloudCoeff_out%wavenumber = CloudCoeff_in%wavenumber
    CloudCoeff_out%Reff_MW = CloudCoeff_in%Reff_MW
    CloudCoeff_out%Reff_IR = CloudCoeff_in%Reff_IR
    CloudCoeff_out%Temperature = CloudCoeff_in%Temperature
    CloudCoeff_out%Density = CloudCoeff_in%Density
    CloudCoeff_out%ext_L_MW = CloudCoeff_in%ext_L_MW
    CloudCoeff_out%w_L_MW = CloudCoeff_in%w_L_MW
    CloudCoeff_out%g_L_MW = CloudCoeff_in%g_L_MW
    CloudCoeff_out%ext_S_MW = CloudCoeff_in%ext_S_MW
    CloudCoeff_out%w_S_MW = CloudCoeff_in%w_S_MW
    CloudCoeff_out%g_S_MW = CloudCoeff_in%g_S_MW
    CloudCoeff_out%phase_coeff_L_MW = CloudCoeff_in%phase_coeff_L_MW
    CloudCoeff_out%phase_coeff_S_MW = CloudCoeff_in%phase_coeff_S_MW
    CloudCoeff_out%ext_L_IR = CloudCoeff_in%ext_L_IR
    CloudCoeff_out%w_L_IR = CloudCoeff_in%w_L_IR
    CloudCoeff_out%g_L_IR = CloudCoeff_in%g_L_IR
    CloudCoeff_out%ext_S_IR = CloudCoeff_in%ext_S_IR
    CloudCoeff_out%w_S_IR = CloudCoeff_in%w_S_IR
    CloudCoeff_out%g_S_IR = CloudCoeff_in%g_S_IR
    CloudCoeff_out%phase_coeff_L_IR = CloudCoeff_in%phase_coeff_L_IR
    CloudCoeff_out%phase_coeff_S_IR = CloudCoeff_in%phase_coeff_S_IR

  END FUNCTION Assign_CloudCoeff





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Equal_CloudCoeff
!
! PURPOSE:
!       Function to test if two CloudCoeff structures are equal.
!
! CATEGORY:
!       Scattering : CloudCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Equal_CloudCoeff( CloudCoeff_LHS,         &  ! Input
!                                          CloudCoeff_RHS,         &  ! Input
!                                          ULP_Scale   = ULP_Scale,  &  ! Optional input
!                                          Check_All = Check_All,    &  ! Optional input
!                                          RCS_Id = RCS_Id,          &  ! Revision control
!                                          Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       CloudCoeff_LHS:  CloudCoeff structure to be compared; equivalent to the
!                          left-hand side of a lexical comparison, e.g.
!                            IF ( CloudCoeff_LHS == CloudCoeff_RHS ).
!                          UNITS:      N/A
!                          TYPE:       CloudCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       CloudCoeff_RHS:  CloudCoeff structure to be compared to; equivalent to
!                          right-hand side of a lexical comparison, e.g.
!                            IF ( CloudCoeff_LHS == CloudCoeff_RHS ).
!                          UNITS:      N/A
!                          TYPE:       CloudCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP_Scale:         Unit of data precision used to scale the floating
!                          point comparison. ULP stands for "Unit in the Last Place,"
!                          the smallest possible increment or decrement that can be
!                          made using a machine's floating point arithmetic.
!                          Value must be positive - if a negative value is supplied,
!                          the absolute value is used. If not specified, the default
!                          value is 1.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Check_All:         Set this argument to check ALL the floating point
!                          channel data of the CloudCoeff structures. The default
!                          action is return with a FAILURE status as soon as
!                          any difference is found. This optional argument can
!                          be used to get a listing of ALL the differences
!                          between data in CloudCoeff structures.
!                          If == 0, Return with FAILURE status as soon as
!                                   ANY difference is found  *DEFAULT*
!                             == 1, Set FAILURE status if ANY difference is
!                                   found, but continue to check ALL data.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the ERROR_HANDLER module.
!                          If == SUCCESS the structures were equal
!                             == FAILURE - an error occurred, or
!                                        - the structures were different.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! CALLS:
!       Associated_CloudCoeff:  Function to test the association status of the
!                                 pointer members of a CloudCoeff structure.
!
!       Compare_Float:            Function to compare floating point numbers
!                                 for equality.
!                                 SOURCE: COMPARE_FLOAT_NUMBERS module.
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Equal_CloudCoeff( CloudCoeff_LHS, &  ! Input
                               CloudCoeff_RHS, &  ! Input
                               ULP_Scale,        &  ! Optional input
                               Check_All,        &  ! Optional input
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
    TYPE( CloudCoeff_type ), INTENT( IN )  :: CloudCoeff_LHS
    TYPE( CloudCoeff_type ), INTENT( IN )  :: CloudCoeff_RHS

    ! -- Optional input
    INTEGER,         OPTIONAL, INTENT( IN )  :: ULP_Scale
    INTEGER,         OPTIONAL, INTENT( IN )  :: Check_All

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Equal_CloudCoeff'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



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

    IF ( .NOT. Associated_CloudCoeff( CloudCoeff_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT CloudCoeff_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------
    ! The RHS structure
    ! -----------------

    IF ( .NOT. Associated_CloudCoeff( CloudCoeff_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT CloudCoeff_RHS pointer '//&
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

    IF ( ( CloudCoeff_LHS%Release /= CloudCoeff_RHS%Release ) .OR. &
         ( CloudCoeff_LHS%Version /= CloudCoeff_RHS%Version )      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Release/Version numbers are different : ", &
                        &i2, ".", i2.2, " vs. ", i2, ".", i2.2 )' ) &
                      CloudCoeff_LHS%Release, CloudCoeff_LHS%Version, &
                      CloudCoeff_RHS%Release, CloudCoeff_RHS%Version
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------
    ! dimension
    ! ---------------

    IF ( CloudCoeff_LHS%n_Frequency /= CloudCoeff_RHS%n_Frequency ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Frequency dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      CloudCoeff_LHS%n_Frequency, CloudCoeff_RHS%n_Frequency
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( CloudCoeff_LHS%n_Size_MW /= CloudCoeff_RHS%n_Size_MW ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Size_MW dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      CloudCoeff_LHS%n_Size_MW, CloudCoeff_RHS%n_Size_MW
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    IF ( CloudCoeff_LHS%n_wavenumber /= CloudCoeff_RHS%n_wavenumber ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_wavenumber dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      CloudCoeff_LHS%n_wavenumber, CloudCoeff_RHS%n_wavenumber
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    IF ( CloudCoeff_LHS%n_Size_IR /= CloudCoeff_RHS%n_Size_IR ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Size_IR dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      CloudCoeff_LHS%n_Size_IR, CloudCoeff_RHS%n_Size_IR
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    IF ( CloudCoeff_LHS%n_Temperature /= CloudCoeff_RHS%n_Temperature ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Temperature dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      CloudCoeff_LHS%n_Temperature, CloudCoeff_RHS%n_Temperature
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    IF ( CloudCoeff_LHS%n_Density /= CloudCoeff_RHS%n_Density ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Density dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      CloudCoeff_LHS%n_Density, CloudCoeff_RHS%n_Density
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    IF ( CloudCoeff_LHS%Max_Legendre_Terms /= CloudCoeff_RHS%Max_Legendre_Terms ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Max_Legendre_Terms dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      CloudCoeff_LHS%Max_Legendre_Terms, CloudCoeff_RHS%Max_Legendre_Terms
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    IF ( CloudCoeff_LHS%Max_Phase_Elements /= CloudCoeff_RHS%Max_Phase_Elements ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Max_Phase_Elements dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      CloudCoeff_LHS%Max_Phase_Elements, CloudCoeff_RHS%Max_Phase_Elements
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


  END FUNCTION Equal_CloudCoeff





!----------------------------------------------------------------------------------
!S+
! NAME:
!       Check_CloudCoeff_Release
!
! PURPOSE:
!       Function to check the CloudCoeff Release value.
!
! CATEGORY:
!       Scattering : CloudCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Check_CloudCoeff_Release( CloudCoeff,             &  ! Input
!                                                  RCS_Id      = RCS_Id,     &  ! Revision control
!                                                  Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       CloudCoeff:  CloudCoeff structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       CloudCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT )
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
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!----------------------------------------------------------------------------------

  FUNCTION Check_CloudCoeff_Release( CloudCoeff, &  ! Input
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
    TYPE( CloudCoeff_type ), INTENT( IN )  :: CloudCoeff

    ! -- Optional output
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Check_CloudCoeff_Release'


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

    IF ( CloudCoeff%Release < CloudCoeff_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A CloudCoeff data update is needed. ", &
                        &"CloudCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      CloudCoeff%Release, CloudCoeff_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#               -- CHECK THAT THE RELEASE IS NOT TOO NEW --                #
    !#--------------------------------------------------------------------------#

    IF ( CloudCoeff%Release > CloudCoeff_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A CloudCoeff software update is needed. ", &
                        &"CloudCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      CloudCoeff%Release, CloudCoeff_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Check_CloudCoeff_Release





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Version_CloudCoeff
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the CloudCoeff data structure.
!
! CATEGORY:
!       Scattering : CloudCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Version_CloudCoeff( CloudCoeff,       &  ! Input
!                                  Version_Info,   &  ! Output
!                                  RCS_Id = RCS_Id )  ! Revision control
!
! INPUT ARGUMENTS:
!       CloudCoeff:  Filled CloudCoeff structure.
!                      UNITS:      N/A
!                      TYPE:       CloudCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Version_Info:  String containing version and dimension information
!                      about the passed CloudCoeff data structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT )
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
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Version_CloudCoeff( CloudCoeff, &  ! Input
                                   Version_Info, &  ! Output
                                   RCS_Id        )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CloudCoeff_type ), INTENT( IN )  :: CloudCoeff

    ! -- Output
    CHARACTER( * ),            INTENT( OUT ) :: Version_Info

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: RCS_Id


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

    WRITE( Long_String, '( a,1x,"CloudCoeff RELEASE.VERSION: ", i2, ".", i2.2, 2x, &
                           &"n_Frequency=",i4 )' ) &
                        ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                        CloudCoeff%Release, CloudCoeff%Version, &
                        CloudCoeff%n_Frequency


    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Version_Info = Long_String(1:MIN( LEN( Version_Info ), LEN_TRIM( Long_String ) ))

  END SUBROUTINE Version_CloudCoeff

END MODULE CloudCoeff_Define


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: CloudCoeff_Define.f90,v 1.2 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CloudCoeff_Define.f90,v $
! Revision 1.2  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2005/08/19 20:08:21  qliu
! -- rename ScatterCoeff_Define.f90 to CloudCoeff_Define.f90
!    first working version for cloud optical coefficient structure.
!
! Revision 1.1  2004/11/04 21:09:29  paulv
! Initial checkin.
!
!
!
