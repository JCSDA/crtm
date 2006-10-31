!--------------------------------------------------------------------------------
!M+
! NAME:
!       LBLRTM_Fhdr_IO
!
! PURPOSE:
!       Module containing routines to read, write, and manipulate an LBLRTM
!       format file header
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE LBLRTM_Fhdr_IO
!
! MODULES:
!       Type_Kinds:         Module with data type kind definitions.
!
!       File_Utility:       Module containing generic file utility routines
!
!       Message_Handler:    Module containing error handling definitions and
!                           routines.
!                           USEs: FILE_UTILITY module
!
!       LBLRTM_Parameters:  Module containing shared parameters required
!                           for LBLRTM format file IO
!                           USEs: TYPE_KINDS module
!
! CONTAINS:
!       Clear_LBLRTM_Fhdr:  Subroutine to clear the members of an LBLRTM_Fhdr
!                           structure.
!
!       Print_LBLRTM_Fhdr:  Function to print the contents of the file header
!                           to standard output or to a log file.
!
!       Read_LBLRTM_Fhdr:   Function to read a file header from an LBLRTM
!                           format file.
!
!       Write_LBLRTM_Fhdr:  Function to write a file header to an LBLRTM
!                           format file.
!       
! DERIVED TYPES:
!       LBLRTM_Fhdr_type:   Definition of the public LBLRTM_Fhdr data structure.
!       ================    Fields are:
!
!         User_ID:                        User specified character string for
!                                         LBLRTM calculation identification.
!                                         UNITS:      N/A
!                                         TYPE:       CHARACTER( 80 )
!                                         DIMENSION:  Scalar
!
!         Column_Scale_Factor:            The factor used to scale the column amounts
!                                         based on the view angle (secant).
!                                         UNITS:      N/A
!                                         TYPE:       REAL( Double )
!                                         DIMENSION:  Scalar
!
!         Average_Layer_Pressure:         The average layer pressure for the
!                                         LBLRTM calculation.
!                                         The data type kind is determined by the
!                                         values set in the LBLRTM_Parameters module.
!                                         UNITS:      hPa
!                                         TYPE:       REAL( LBLRTM_FP_KIND )
!                                         DIMENSION:  Scalar
!
!         Average_Layer_Temperature:      The average layer temperature for the
!                                         LBLRTM calculation.
!                                         The data type kind is determined by the
!                                         values set in the LBLRTM_Parameters module.
!                                         UNITS:      Kelvin
!                                         TYPE:       REAL( LBLRTM_FP_KIND )
!                                         DIMENSION:  Scalar
!
!         Molecule_ID:                    Idenitifer for the molecules selected
!                                         for inclusion in the calculation.
!                                         UNITS:      N/A
!                                         TYPE:       CHARACTER( 8 )
!                                         DIMENSION:  Rank-1, LBLRTM_MAX_N_MOLECULES
!
!         Molecule_Column_Density:        The column densities of the molecules
!                                         selected for inclusion in the calculation.
!                                         The data type kind is determined by the
!                                         values set in the LBLRTM_Parameters module.
!                                         UNITS:      molecules.cm^-2
!                                         TYPE:       REAL( LBLRTM_FP_KIND )
!                                         DIMENSION:  Rank-1, LBLRTM_MAX_N_MOLECULES
!
!         Broadening_Gas_Column_Density:  The column density of the broadening
!                                         gases, i.e. everything besides those
!                                         molecules selected for use in the 
!                                         calculation. The data type kind is
!                                         determined by the values set in the
!                                         LBLRTM_Parameters module.
!                                         UNITS:      molecules.cm^-2
!                                         TYPE:       REAL( LBLRTM_FP_KIND )
!                                         DIMENSION:  Scalar
!
!         Frequency_Interval:             Frequency interval for the spectral
!                                         data. The data type kind is determined
!                                         by the values set in the LBLRTM_Parameters
!                                         module.
!                                         UNITS:      cm^-1
!                                         TYPE:       REAL( LBLRTM_FP_KIND )
!                                         DIMENSION:  Scalar
!
!         Begin_Frequency:                Beginning frequency for the spectral data.
!                                         UNITS:      cm^-1
!                                         TYPE:       REAL( Double )
!                                         DIMENSION:  Scalar
!
!         End_Frequency:                  Ending frequency for the spectral data.
!                                         UNITS:      cm^-1
!                                         TYPE:       REAL( Double )
!                                         DIMENSION:  Scalar
!
!         Boundary_Temperature:           The temperature of the boundary at the
!                                         end of the path in the LBLRTM calculation.
!                                         The data type kind is determined by the
!                                         values set in the LBLRTM_Parameters module.
!                                         UNITS:      Kelvin
!                                         TYPE:       REAL( LBLRTM_FP_KIND )
!                                         DIMENSION:  Scalar
!
!         Boundary_Emissivity:            The emissivity of the boundary at the
!                                         end of the path in the LBLRTM calculation.
!                                         The data type kind is determined by the
!                                         values set in the LBLRTM_Parameters module.
!                                         UNITS:      N/A
!                                         TYPE:       REAL( LBLRTM_FP_KIND )
!                                         DIMENSION:  Scalar
!
!         Run_Flags:                      The run flags specified in the LBLRTM
!                                         calculation.
!                                         UNITS:      N/A
!                                         TYPE:       TYPE( LBLRTM_Run_Flags_type )
!                                         DIMENSION:  Scalar
!                                         ATTRIBUTES: PUBLIC
!
!         n_Molecules:                    The number of molecules selected for the 
!                                         LBLRTM calculation.
!                                         The data type kind is determined by the
!                                         values set in the LBLRTM_Parameters module.
!                                         UNITS:      N/A
!                                         TYPE:       INTEGER( LBLRTM_IP_KIND )
!                                         DIMENSION:  Scalar
!
!         n_Layers:                       The current layer if more than one layer
!                                         was output, otherwise the number of layers.
!                                         The data type kind is determined by the
!                                         values set in the LBLRTM_Parameters module.
!                                         UNITS:      N/A
!                                         TYPE:       INTEGER( LBLRTM_IP_KIND )
!                                         DIMENSION:  Scalar
!
!         OD_Layering_Control_Flag:       This is a flag used in the LBLRTM calculation
!                                         to for layering control in the optical depth
!                                         calculation. Corresponds to IMULT, which is
!                                         is equivalenced to YI1, in the LBLRTM code.
!                                         The data type kind is determined by the
!                                         values set in the LBLRTM_Parameters module.
!                                         UNITS:      N/A
!                                         TYPE:       INTEGER( LBLRTM_IP_KIND )
!                                         DIMENSION:  Scalar
!
!         Calculation_Date:               A string containing the date at which
!                                         the LBLRTM calculation was performed.
!                                         Corresponds to YID(1) in LBLRTM code.
!                                         UNITS:      N/A
!                                         TYPE:       CHARACTER( 8 )
!                                         DIMENSION:  Scalar
!
!         Calculation_Time:               A string containing the time at which
!                                         the LBLRTM calculation was begun.
!                                         Corresponds to YID(2) in LBLRTM code.
!                                         UNITS:      N/A
!                                         TYPE:       CHARACTER( 8 )
!                                         DIMENSION:  Scalar
!
!         ancillary:                      A block of ancillary data used in the
!                                         LBLRTM calculation. Corresponds to
!                                         YID(3-10) in the LBLRTM code. This
!                                         character block is written to and
!                                         equivalenced in weird ways so rather
!                                         than try to decode it, it's all lumped
!                                         in here. See subroutines YDIH1 and PATH
!                                         for how the variables H1, H2, ANGLE, LTGNT,
!                                         LH1, and LH2 are stored in the YID array.
!                                         UNITS:      N/A
!                                         TYPE:       CHARACTER( 8 )
!                                         DIMENSION:  Rank-1; 8 elements
!
!
!       LBLRTM_Run_Flags_type:   Definition of the public LBLRTM_Run_Flags data
!       =====================    structure. Fields are:
!
!         hirac:   Indicates the version of HIRAC used in the LBLRTM calculation.
!                    = 0 HIRAC not activated; line-by-line calculation is bypassed
!                    = 1 Voigt profile
!                    = 2 Lorentz profile, not available in LBLRTM
!                    = 3 Doppler profile, not available in LBLRTM
!                    = 4 NLTE Option (Non Local Thermodynamic Equilibrium)
!                    = 9 central line contribution omitted (functions 1-3)
!                  UNITS:      N/A
!                  TYPE:       INTEGER( LBLRTM_IP_KIND )
!                  DIMENSION:  Scalar
!
!         lblf4:   Indicates the line-by-line function 4 usage. (LBLF4 extends
!                  bound of line by line calculation beyond 64 halfwidths from
!                  line center) 
!                    = 0  LBLF4 not activated (line by line bound is 64 halfwidths)
!                    = 1  line by line bound is 25 cm-1 for all layer pressures
!                    = 2  line by line bound is 25 cm-1 for layers with pressures >  0.5 mb
!                                            and 5 cm-1 for layers with pressures <= 0.5 mb
!                  UNITS:      N/A
!                  TYPE:       INTEGER( LBLRTM_IP_KIND )
!                  DIMENSION:  Scalar
!
!         xscnt:   Indicates the continuum and X-seciton inclusion selected.
!                  From the LBLRTM code,
!                    IXSCNT = IXSECT*10+ICNTNM
!                  where IXSECT = 0 or 1 to indicate if X-section data is used
!                        ICNTNM = 0 or 1 to indicate if continua are included.
!                                 (any other ICNTNM value is set = 1 after scaling
!                                  factors are set.)
!                  UNITS:      N/A
!                  TYPE:       INTEGER( LBLRTM_IP_KIND )
!                  DIMENSION:  Scalar
!
!         aersl:   Indicates aerosol usage.
!                    = 0  no aerosols used
!                    = 1  internal LOWTRAN aerosol models
!                    = 7  user defined aerosol models
!                    = 9  precalculated aerosols
!                  UNITS:      N/A
!                  TYPE:       INTEGER( LBLRTM_IP_KIND )
!                  DIMENSION:  Scalar
!
!         emit:    Indicates if/how emission is computed.
!                     = 0  optical depth.
!                     = 1  radiance and transmittance.
!                     = 2  solar radiance
!                  UNITS:      N/A
!                  TYPE:       INTEGER( LBLRTM_IP_KIND )
!                  DIMENSION:  Scalar
!
!         scan:    Indicates the type of scanning function applied to the data.
!                    = 1  scanning function
!                    = 2  interpolating procedure
!                    = 3  Fast Fourier Transform scan
!                  UNITS:      N/A
!                  TYPE:       INTEGER( LBLRTM_IP_KIND )
!                  DIMENSION:  Scalar
!
!         plot:    Indicates if a plot file was created
!                     = 0  No plot file instructions
!                     = 1  Plot option used.
!                  UNITS:      N/A
!                  TYPE:       INTEGER( LBLRTM_IP_KIND )
!                  DIMENSION:  Scalar
!
!         path:    Indicates the direction of data merging performed.
!                    = 0  determined by sign of secant SECNTO
!                    = 1  looking down merge
!                    = 2  tangent layer merge
!                    = 3  looking up merge
!                  UNITS:      N/A
!                  TYPE:       INTEGER( LBLRTM_IP_KIND )
!                  DIMENSION:  Scalar
!
!         jrad:    Indicates how the radiation term is included.
!                    = -1  no radiation term in absorption coefficients
!                    =  0  radiation term put in by panel
!                    =  1  radiation term included in line strengths
!                  UNITS:      N/A
!                  TYPE:       INTEGER( LBLRTM_IP_KIND )
!                  DIMENSION:  Scalar
!
!         test:    Indicates that LBLRTM was run in test mode.
!                    = 0  regular run mode. TAPE3 must exist.
!                    = 1  test mode. TAPE3 is created. 
!                  UNITS:      N/A
!                  TYPE:       INTEGER( LBLRTM_IP_KIND )
!                  DIMENSION:  Scalar
!
!         merge:   Indicates the type of data merging performed. This
!                  option has numerous values. See the LBLRTM instructions
!                  for the explanation.
!                  UNITS:      N/A
!                  TYPE:       INTEGER( LBLRTM_IP_KIND )
!                  DIMENSION:  Scalar
!
!         scnid:   Indicates the type of scanning applied to the data. This
!                  option has numerous values and is a combination of various
!                  values needed in determining the scanning mode.
!                  UNITS:      N/A
!                  TYPE:       REAL( LBLRTM_FP_KIND )
!                  DIMENSION:  Scalar
!
!         hwhm:    Half-Width-at-Half-Maximum for the scanning function or,
!                  if FFT scanning is used, can be the maximum optical delay.
!                  UNITS:      N/A
!                  TYPE:       REAL( LBLRTM_FP_KIND )
!                  DIMENSION:  Scalar
!
!         idabs:   Used to select between transmission and absorption output
!                  descriptor.
!                    = -1  absorption
!                    =  0  transmission
!                  UNITS:      N/A
!                  TYPE:       INTEGER( LBLRTM_IP_KIND )
!                  DIMENSION:  Scalar
!
!         atm:     Indicates if the atmospheric ray trace routine, LBLATM,
!                  was called.
!                    = 0  LBLATM not called and implies LBLRTM inputs were
!                         density weighted temperatures, pressures and
!                         integrated absorber amounts, i.e. LAYER values.
!                    = 1  LBLATM was called and implies LBLRTM inputs were
!                         LEVEL values; i.e. level boundary temperature,
!                         pressure and absorber concentration such as a
!                         radiosonde profile.
!                  UNITS:      N/A
!                  TYPE:       INTEGER( LBLRTM_IP_KIND )
!                  DIMENSION:  Scalar
!
!         layr1:   Indicates the initial layer used in merging the data
!                  up/down through the atmosphere.
!                  UNITS:      N/A
!                  TYPE:       INTEGER( LBLRTM_IP_KIND )
!                  DIMENSION:  Scalar
!
!         nlayr:   Indicates the number of LBLRTM calculation layers.
!                  UNITS:      N/A
!                  TYPE:       INTEGER( LBLRTM_IP_KIND )
!                  DIMENSION:  Scalar
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 23-Jan-2000
!                     paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000 Paul van Delst
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

MODULE LBLRTM_Fhdr_IO

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE LBLRTM_Parameters, N_MOL => LBLRTM_MAX_N_MOLECULES


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Clear_LBLRTM_Fhdr
  PUBLIC :: Print_LBLRTM_Fhdr
  PUBLIC :: Read_LBLRTM_Fhdr
  PUBLIC :: Write_LBLRTM_Fhdr


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: LBLRTM_Fhdr_IO.f90,v 1.11 2006/07/26 21:43:58 wd20pd Exp $'

  ! -- Scalar invalid values
  INTEGER,                PRIVATE, PARAMETER :: IP_INVALID = -1
  REAL( LBLRTM_FP_KIND ), PRIVATE, PARAMETER :: FP_INVALID = -1.0_LBLRTM_FP_KIND
  REAL( Double ),         PRIVATE, PARAMETER :: DP_INVALID = -1.0_Double


  ! ------------------------
  ! Derived type definitions
  ! ------------------------

  ! -- LBLRTM file header run flags
  TYPE, PUBLIC :: LBLRTM_Run_Flags_type
    INTEGER( LBLRTM_IP_KIND ) :: hirac = IP_INVALID
    INTEGER( LBLRTM_IP_KIND ) :: lblf4 = IP_INVALID
    INTEGER( LBLRTM_IP_KIND ) :: xscnt = IP_INVALID
    INTEGER( LBLRTM_IP_KIND ) :: aersl = IP_INVALID
    INTEGER( LBLRTM_IP_KIND ) :: emit  = IP_INVALID
    INTEGER( LBLRTM_IP_KIND ) :: scan  = IP_INVALID
    INTEGER( LBLRTM_IP_KIND ) :: plot  = IP_INVALID
    INTEGER( LBLRTM_IP_KIND ) :: path  = IP_INVALID
    INTEGER( LBLRTM_IP_KIND ) :: jrad  = IP_INVALID
    INTEGER( LBLRTM_IP_KIND ) :: test  = IP_INVALID
    INTEGER( LBLRTM_IP_KIND ) :: merge = IP_INVALID
    REAL( LBLRTM_FP_KIND )    :: scnid = FP_INVALID
    REAL( LBLRTM_FP_KIND )    :: hwhm  = FP_INVALID
    INTEGER( LBLRTM_IP_KIND ) :: idabs = IP_INVALID
    INTEGER( LBLRTM_IP_KIND ) :: atm   = IP_INVALID
    INTEGER( LBLRTM_IP_KIND ) :: layr1 = IP_INVALID
    INTEGER( LBLRTM_IP_KIND ) :: nlayr = IP_INVALID
  END TYPE LBLRTM_Run_Flags_type

  ! -- LBLRTM file header
  TYPE, PUBLIC :: LBLRTM_Fhdr_type
    CHARACTER( 80 )                            :: User_ID                       = ' '
    REAL( Double )                             :: Column_Scale_Factor           = DP_INVALID
    REAL( LBLRTM_FP_KIND )                     :: Average_Layer_Pressure        = FP_INVALID
    REAL( LBLRTM_FP_KIND )                     :: Average_Layer_Temperature     = FP_INVALID
    CHARACTER( 8 ),         DIMENSION( N_MOL ) :: Molecule_ID                   = ' '
    REAL( LBLRTM_FP_KIND ), DIMENSION( N_MOL ) :: Molecule_Column_Density       = FP_INVALID
    REAL( LBLRTM_FP_KIND )                     :: Broadening_Gas_Column_Density = FP_INVALID
    REAL( LBLRTM_FP_KIND )                     :: Frequency_Interval            = FP_INVALID
    REAL( Double )                             :: Begin_Frequency               = DP_INVALID
    REAL( Double )                             :: End_Frequency                 = DP_INVALID
    REAL( LBLRTM_FP_KIND )                     :: Boundary_Temperature          = FP_INVALID
    REAL( LBLRTM_FP_KIND )                     :: Boundary_Emissivity           = FP_INVALID
    TYPE ( LBLRTM_Run_Flags_type )             :: Run_Flags
    INTEGER( LBLRTM_IP_KIND )                  :: n_Molecules                   = IP_INVALID
    INTEGER( LBLRTM_IP_KIND )                  :: n_Layer                       = IP_INVALID
    INTEGER( LBLRTM_IP_KIND )                  :: OD_Layering_Control_Flag      = IP_INVALID
    CHARACTER( 8 )                             :: Calculation_Date              = ' '
    CHARACTER( 8 )                             :: Calculation_Time              = ' '
    CHARACTER( 8 ), DIMENSION( 8 )             :: ancillary                     = ' '
  END TYPE LBLRTM_Fhdr_type


CONTAINS


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
!       Clear_LBLRTM_Fhdr
!
! PURPOSE:
!       Subroutine to clear the members of an LBLRTM_Fhdr structure.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_LBLRTM_Fhdr( LBLRTM_Fhdr )
!
! INPUTS:
!       None
!
! OPTIONAL INPUTS:
!       None
!
! OUTPUTS:
!       LBLRTM_Fhdr:  LBLRTM file header structure with all member
!                     values reset to default "INVALID" value.
!                     UNITS:      N/A
!                     TYPE:       TYPE( LBLRTM_Fhdr_type )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTUTS:
!       None
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 29-Mar-2003
!                     paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Clear_LBLRTM_Fhdr ( LBLRTM_Fhdr )

    TYPE ( LBLRTM_Fhdr_type ), INTENT( IN OUT ) :: LBLRTM_Fhdr
  
    LBLRTM_Fhdr%User_ID                       = ' '
    LBLRTM_Fhdr%Column_Scale_Factor           = DP_INVALID
    LBLRTM_Fhdr%Average_Layer_Pressure        = FP_INVALID
    LBLRTM_Fhdr%Average_Layer_Temperature     = FP_INVALID
    LBLRTM_Fhdr%Molecule_ID                   = ' '
    LBLRTM_Fhdr%Molecule_Column_Density       = FP_INVALID
    LBLRTM_Fhdr%Broadening_Gas_Column_Density = FP_INVALID
    LBLRTM_Fhdr%Frequency_Interval            = FP_INVALID
    LBLRTM_Fhdr%Begin_Frequency               = DP_INVALID
    LBLRTM_Fhdr%End_Frequency                 = DP_INVALID
    LBLRTM_Fhdr%Boundary_Temperature          = FP_INVALID
    LBLRTM_Fhdr%Boundary_Emissivity           = FP_INVALID

    LBLRTM_Fhdr%n_Molecules                   = IP_INVALID
    LBLRTM_Fhdr%n_Layer                       = IP_INVALID
    LBLRTM_Fhdr%OD_Layering_Control_Flag      = IP_INVALID
    LBLRTM_Fhdr%Calculation_Date              = ' '
    LBLRTM_Fhdr%Calculation_Time              = ' '
    LBLRTM_Fhdr%ancillary                     = ' '

    LBLRTM_Fhdr%Run_Flags = LBLRTM_Run_Flags_type( IP_INVALID, &  ! hirac
                                                   IP_INVALID, &  ! lblf4
                                                   IP_INVALID, &  ! xscnt
                                                   IP_INVALID, &  ! aersl
                                                   IP_INVALID, &  ! emit
                                                   IP_INVALID, &  ! scan
                                                   IP_INVALID, &  ! plot
                                                   IP_INVALID, &  ! path
                                                   IP_INVALID, &  ! jrad
                                                   IP_INVALID, &  ! test
                                                   IP_INVALID, &  ! merge
                                                   FP_INVALID, &  ! scnid
                                                   FP_INVALID, &  ! hwhm
                                                   IP_INVALID, &  ! idabs
                                                   IP_INVALID, &  ! atm
                                                   IP_INVALID, &  ! layr1
                                                   IP_INVALID  )  ! nlayr

  END SUBROUTINE Clear_LBLRTM_Fhdr





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Print_LBLRTM_Fhdr
!
! PURPOSE:
!       Subroutine to print the file header structure member values.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Print_LBLRTM_Fhdr( LBLRTM_Fhdr,              &  ! Input
!                               RCS_Id = RCS_Id,          &  ! Revision control
!                               Message_Log = Message_Log )  ! Message logging
!
! INPUTS:
!       LBLRTM_Fhdr:  LBLRTM file header structure to print.
!                     UNITS:      N/A
!                     TYPE:       TYPE( LBLRTM_Fhdr_type )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL INPUTS:
!       Message_Log:  Character string specifying a filename to which the
!                     output will be sent. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to write to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUTS:
!       None
!
! OPTIONAL OUTPUTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! SIDE EFFECTS:
!       Data is writen to a log file if the optional MESSAGE_LOG argument
!       is used.
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 29-Mar-2003
!                     paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Print_LBLRTM_Fhdr( LBLRTM_Fhdr, &  ! Input
                                RCS_Id,      &  ! Revision control
                                Message_Log  )  ! Error messaging



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE ( LBLRTM_Fhdr_type ), INTENT( IN )  :: LBLRTM_Fhdr

    ! -- Revision control
    CHARACTER( * ), OPTIONAL,  INTENT( OUT ) :: RCS_Id

    ! -- Message logging
    CHARACTER( * ), OPTIONAL,  INTENT( IN )  :: Message_Log


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Print_LBLRTM_Fhdr'

    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 5000 ) :: String1, String2, String3, String4
    CHARACTER( 2 ) :: CRLF



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = ROUTINE_NAME//' from '//MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CREATE THE OUTPUT STRINGS --                     #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------
    ! The carriage return - line feed string
    ! --------------------------------------

    CRLF = ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)


    ! -------------------------------------------
    ! The various LBLRTM file header info strings
    ! -------------------------------------------

    WRITE( String1, '( a, 1x, a, &
                      &a, 5x, "Column scale factor       = ", es13.6,           &
                      &a, 5x, "Average layer pressure    = ", es13.6, " hPa",   &
                      &a, 5x, "Average layer temperature = ", es13.6, " K",     &
                      &a, 5x, "Frequency interval        = ", es13.6, " cm^-1", &
                      &a, 5x, "Begin frequency           = ", es13.6, " cm^-1", &
                      &a, 5x, "End frequency             = ", es13.6, " cm^-1", &
                      &a, 5x, "Boundary temperature      = ", es13.6, " K",     &
                      &a, 5x, "Boundary emissivity       = ", es13.6,           &
                      &a, 5x, "# Molecules               = ", i5, &
                      &a, 5x, "# Layers                  = ", i5, &
                      &a, 5x, "Od layering control flag  = ", i5, &
                      &a, 5x, "Calculation date          = ", a,  &
                      &a, 5x, "Calculation time          = ", a   )' ) &
                    CRLF, LBLRTM_Fhdr%User_ID, &
                    CRLF, LBLRTM_Fhdr%Column_Scale_Factor, &
                    CRLF, LBLRTM_Fhdr%Average_Layer_Pressure, &
                    CRLF, LBLRTM_Fhdr%Average_Layer_Temperature, &
                    CRLF, LBLRTM_Fhdr%Frequency_Interval, &
                    CRLF, LBLRTM_Fhdr%Begin_Frequency, &
                    CRLF, LBLRTM_Fhdr%End_Frequency, &
                    CRLF, LBLRTM_Fhdr%Boundary_Temperature, &
                    CRLF, LBLRTM_Fhdr%Boundary_Emissivity, &
                    CRLF, LBLRTM_Fhdr%n_Molecules, &
                    CRLF, LBLRTM_Fhdr%n_Layer, &
                    CRLF, LBLRTM_Fhdr%OD_Layering_Control_Flag, &
                    CRLF, LBLRTM_Fhdr%Calculation_Date, &
                    CRLF, LBLRTM_Fhdr%Calculation_Time
             
    WRITE( String2, '( a, 5x, "Molecule ID : ", a, 8( a13, : ) )' ) &
                    CRLF, CRLF, LBLRTM_Fhdr%Molecule_ID( 1:LBLRTM_Fhdr%n_Molecules )

    WRITE( String3, '( a, 5x, "Broadening gas column density = ", es13.6, " mol/cm^2" )' ) &
                    CRLF, LBLRTM_Fhdr%Molecule_Column_Density( 1:LBLRTM_Fhdr%n_Molecules )

    WRITE( String4, '( a,  5x, "Run flags:", &
                      &a, 10x, "hirac = ", i5,     &
                      &a, 10x, "lblf4 = ", i5,     &
                      &a, 10x, "xscnt = ", i5,     &
                      &a, 10x, "aersl = ", i5,     &
                      &a, 10x, "emit  = ", i5,     &
                      &a, 10x, "scan  = ", i5,     &
                      &a, 10x, "plot  = ", i5,     &
                      &a, 10x, "path  = ", i5,     &
                      &a, 10x, "jrad  = ", i5,     &
                      &a, 10x, "test  = ", i5,     &
                      &a, 10x, "merge = ", i5,     &
                      &a, 10x, "scnid = ", es13.6, &
                      &a, 10x, "hwhm  = ", es13.6, &
                      &a, 10x, "idabs = ", i5,     &
                      &a, 10x, "atm   = ", i5,     &
                      &a, 10x, "layr1 = ", i5,     &
                      &a, 10x, "nlayr = ", i5      )' ) &
                    CRLF, LBLRTM_Fhdr%Run_Flags%hirac, &  
                    CRLF, LBLRTM_Fhdr%Run_Flags%lblf4, &  
                    CRLF, LBLRTM_Fhdr%Run_Flags%xscnt, &  
                    CRLF, LBLRTM_Fhdr%Run_Flags%aersl, &  
                    CRLF, LBLRTM_Fhdr%Run_Flags%emit,  &  
                    CRLF, LBLRTM_Fhdr%Run_Flags%scan,  &  
                    CRLF, LBLRTM_Fhdr%Run_Flags%plot,  &  
                    CRLF, LBLRTM_Fhdr%Run_Flags%path,  &  
                    CRLF, LBLRTM_Fhdr%Run_Flags%jrad,  &  
                    CRLF, LBLRTM_Fhdr%Run_Flags%test,  &  
                    CRLF, LBLRTM_Fhdr%Run_Flags%merge, &  
                    CRLF, LBLRTM_Fhdr%Run_Flags%scnid, &  
                    CRLF, LBLRTM_Fhdr%Run_Flags%hwhm,  &  
                    CRLF, LBLRTM_Fhdr%Run_Flags%idabs, &  
                    CRLF, LBLRTM_Fhdr%Run_Flags%atm,   &  
                    CRLF, LBLRTM_Fhdr%Run_Flags%layr1, &  
                    CRLF, LBLRTM_Fhdr%Run_Flags%nlayr



    !#--------------------------------------------------------------------------#
    !#                  -- OUTPUT THE FILE HEADER INFORMATION --                #
    !#--------------------------------------------------------------------------#

    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( String1 )//&
                          TRIM( String2 )//&
                          TRIM( String3 )//&
                          TRIM( String4 ), &
                          INFORMATION, &
                          Message_Log = Message_Log )
     
  END SUBROUTINE Print_LBLRTM_Fhdr





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Read_LBLRTM_Fhdr
!
! PURPOSE:
!       Function to read an LBLRTM file header structure from an LBLRTM format
!       file.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_LBLRTM_Fhdr ( FileID,                   &  ! Input
!                                         LBLRTM_Fhdr,              &  ! Output
!                                         EOF,                      &  ! Output
!                                         RCS_Id = RCS_Id,          &  ! Revision control
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUTS:
!       FileID:       Logical unit number associated with LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUTS:
!       LBLRTM_Fhdr:  LBLRTM file header structure.
!                     UNITS:      N/A
!                     TYPE:       TYPE( LBLRTM_Fhdr_type )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
!       EOF:          Flag indicating end-of-file status for the LBLRTM
!                     format file after the read. Valid return values are
!                     defined in the LBLRTM_Parameters module.
!                       = LBLRTM_FILE_PTR_EOF:   End-of-file has been reached.
!                                                The file is then closed.
!                       = LBLRTM_FILE_PTR_OK:    No EOF or EOL condition. File
!                                                is positioned for further 
!                                                reading.
!                       = LBLRTM_FILE_PTR_UNDEF: An error occurred. The file is
!                                                closed.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTUTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the LBLRTM file header read was successful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If an error occurs or the end-of-file is encountered, the input file is 
!       closed.
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 29-Mar-2003
!                     paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Read_LBLRTM_Fhdr( FileID,       &  ! Input
                             LBLRTM_Fhdr,  &  ! Output
                             EOF,          &  ! Output
                             RCS_Id,       &  ! Revision control
                             Message_Log ) &  ! Error messaging
                           RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                   INTENT( IN )  :: FileID

    ! -- Output
    TYPE ( LBLRTM_Fhdr_type ), INTENT( OUT ) :: LBLRTM_Fhdr
    INTEGER,                   INTENT( OUT ) :: EOF

    ! -- Revision control
    CHARACTER( * ), OPTIONAL,  INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,  INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_LBLRTM_Fhdr'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = ROUTINE_NAME//' from '//MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CHECK IF FILE IS OPEN --                           #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. File_Open( FileID ) ) THEN
      Error_Status = FAILURE
      EOF          = LBLRTM_FILE_PTR_UNDEF
      CALL Display_Message( ROUTINE_NAME, &
                            'LBLRTM file is not open.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- READ THE DATA --                             #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) LBLRTM_Fhdr


    ! -----------------
    ! Check read status
    ! -----------------

    IO_Status_check: SELECT CASE ( IO_Status )

      ! -- End of file has been reached
      CASE ( :-1 )
        Error_Status = SUCCESS
        EOF          = LBLRTM_FILE_PTR_EOF
        CLOSE( FileID )
        RETURN

      ! -- Read was successful, no errors
      CASE ( 0 )
        Error_Status = SUCCESS
        EOF          = LBLRTM_FILE_PTR_OK

      ! -- Error occurred in read
      CASE ( 1: )
        Error_Status = FAILURE
        EOF          = LBLRTM_FILE_PTR_UNDEF
        WRITE( Message, '( "Error reading LBLRTM file header. ", &
                          &"IOSTAT = ", i5 )' ) IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN

    END SELECT IO_Status_check

  END FUNCTION Read_LBLRTM_Fhdr





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Write_LBLRTM_Fhdr
!
! PURPOSE:
!       Function to write an LBLRTM file header structure to an LBLRTM format
!       file.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_LBLRTM_Fhdr( FileID,                   &  ! Input
!                                         LBLRTM_Fhdr,              &  ! Input
!                                         RCS_Id = RCS_Id,          &  ! Revision control
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUTS:
!       FileID:       Logical unit number associated with LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       LBLRTM_Fhdr:  LBLRTM file header structure to write.
!                     UNITS:      N/A
!                     TYPE:       TYPE( LBLRTM_Fhdr_type )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL INPUTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUTS:
!       None
!
! OPTIONAL OUTUTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the LBLRTM file header write was successful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If an error occurs the output file is closed.
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 29-Mar-2003
!                     paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Write_LBLRTM_Fhdr( FileID,       &  ! Input
                              LBLRTM_Fhdr,  &  ! Input
                              RCS_Id,       &  ! Revision control
                              Message_Log ) &  ! Error messaging
                            RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                   INTENT( IN )  :: FileID
    TYPE ( LBLRTM_Fhdr_type ), INTENT( IN )  :: LBLRTM_Fhdr

    ! -- Revision control
    CHARACTER( * ), OPTIONAL,  INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,  INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_LBLRTM_Fhdr'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = ROUTINE_NAME//' from '//MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK IF FILE IS OPEN --                         #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. File_Open( FileID ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'LBLRTM file is not open.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- WRITE THE DATA --                            #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) LBLRTM_Fhdr

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing LBLRTM file file header. ", &
                        &"IOSTAT = ", i5 )' ) IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

  END FUNCTION Write_LBLRTM_Fhdr

END MODULE LBLRTM_Fhdr_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: LBLRTM_Fhdr_IO.f90,v 1.11 2006/07/26 21:43:58 wd20pd Exp $
!
! $Date: 2006/07/26 21:43:58 $
!
! $Revision: 1.11 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: LBLRTM_Fhdr_IO.f90,v $
! Revision 1.11  2006/07/26 21:43:58  wd20pd
! Replacement of "Error_Handler" with "Message_Handler" in USE statements and
! in documentaiton blocks.
!
! Revision 1.10  2005/09/15 16:53:04  paulv
! - Corrected bug in maximum molecule parameter rename.
! - Corrected bug in file header print formats.
!
! Revision 1.9  2005/05/08 14:02:59  paulv
! - Upgraded to Fortran-95
! - Removed Initialization() function. Structure initialisation is now
!   done in the type definition.
! - Added Clear() function.
! - Altered Print() function to not require log file open functionality.
! - Added optional RCS_Id argument to public functions.
!
! Revision 1.8  2002/06/05 18:53:25  paulv
! - Removed MESSAGE as a module variable and placed definitions in each
!   module subprogram.
!
! Revision 1.7  2002/04/16 18:36:43  paulv
! - Updated documentation
!
! Revision 1.6  2002/04/12 20:33:15  paulv
! - Added print_LBLRTM_Fhdr() subroutine.
! - Updated documentation.
!
! Revision 1.5  2002/04/10 02:36:59  paulv
! - Added write function.
! - Added documentation. Incomplete.
!
! Revision 1.4  2002/04/09 15:35:51  paulv
! - In the process of updating documentation. Incomplete.
!
! Revision 1.3  2002/04/09 03:20:54  paulv
! - Bringing repository up to date. Minor changes but still incomplete.
!
! Revision 1.2  2002/03/30 04:51:50  paulv
! - Corrected some naming errors.
! - Moved return status definitions to begin of function rather than after
!   the file open check.
! - Code untested.
!
! Revision 1.1  2002/03/29 19:53:56  paulv
! Initial checkin of new LBLRTM IO routines.
!
!
!
