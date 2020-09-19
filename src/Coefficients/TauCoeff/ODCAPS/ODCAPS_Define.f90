!------------------------------------------------------------------------------
!M+
! NAME:
!       ODCAPS_Define
!
! PURPOSE:
!       Module defining the ODCAPS (Optical Depth, Combined Absorber & Pressure Space)
!       data structure and containing routines to manipulate it.
!       
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE ODCAPS_Define
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Message_Handler:          Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
! CONTAINS:
!       Associated_ODCAPS:    Function to test the association status             
!                               of the pointer members of a ODCAPS	            
!                               structure.				            
!
!       Destroy_ODCAPS:       Function to re-initialize a ODCAPS	            
!                               structure.				            
!
!       Allocate_ODCAPS:      Function to allocate the pointer members            
!                               of a ODCAPS structure.		            
!
!       Assign_ODCAPS:        Function to copy a valid ODCAPS structure.        
!
!       CheckRelease_ODCAPS: Function to check the ODCAPS Release value.       
!
!
!       Info_ODCAPS:       Subroutine to return a string containing            
!                               version and dimension information about             
!                               the ODCAPS data structure.		            
!
! DERIVED TYPES:
!       ODCAPS_type:   Definition of the public ODCAPS data structure. Fields
!                        are...
!
!         Release:             Coefficient data file release number.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         Version:             Coefficient data file version number.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         Algorithm:           The ODCAPS data Algorithm_ID. Used to determine
!                              which algorithm to be used.
!                              UNITS:	   N/A
!                              TYPE:	   INTEGER
!                              DIMENSION:  Scalar
!
!         n_Layers:            Maximum layers for the coefficients.
!                              "Ilayers" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Channels:          Total number of spectral channels.
!                              "L" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Tunings:           Number of tuning multipliers for the gases 
!                              "J" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_F_Predictors:      Number of predictors used in the
!                              thermal "F" factor coefs.
!                              "MF" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_RefProfile_Items:  Number of item in the reference profile.
!                              "N" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_NONLTE_Predictors: The number of predictors for the non-LTE.
!                              "MNON" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_NONLTE_Channels:   The number of channels for the non-LTE.
!                              "MChannels" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_TraceGases:        The number of trace gases.
!                              "KTraces" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Subsets:           The number of subset for the gas absorption coeffs.
!                              "K" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         Sensor_Descriptor:   String variable containing a short text
!                              description of the sensor and satellite.
!                              Descriptors are taken from the SensorInfo
!                              file prefix member. Examples are:
!                                - hirs3_n17
!                                - airs_aqua
!                                - ssmis_f16... etc
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( 20 )
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         NCEP_Sensor_ID:      An "in-house" value used at NOAA/NCEP/EMC 
!                              to identify a satellite/sensor combination.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         WMO_Satellite_ID:    The WMO code for identifying satellite
!                              platforms. Taken from the WMO common
!                              code tables at:
!                                http://www.wmo.ch/web/ddbs/Code-tables.html
!                              The Satellite ID is from Common Code
!                              table C-5, or code table 0 01 007 in BUFR
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         WMO_Sensor_ID:       The WMO code for identifying a satelite
!                              sensor. Taken from the WMO common
!                              code tables at:
!                                http://www.wmo.ch/web/ddbs/Code-tables.html
!                              The Sensor ID is from Common Code
!                              table C-8, or code table 0 02 019 in BUFR
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         Sensor_Channel:       This is the sensor channel number associated
!                              with the data in the coefficient file. Helps
!                              in identifying channels where the numbers are
!                              not contiguous (e.g. AIRS).
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         Channel_Subset_Index:This is the set number associated
!                              with the subset in which the coefficients are
!                              located. Values are range from 1 to 7
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         Channel_Subset_Position: This is the channel position associated
!                              with the subset in which the coefficients are
!                              located.  
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         Channel_H2O_OPTRAN:  This is OPTRAN H2O channel indices associated
!                              with the channel positions in which 
!                              the coefficients are located.  
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         Channel_CO2_Perturbation: This is CO2 perturbation channel indices 
!                              associated with the channel positions in which 
!                              the coefficients are located.  
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         Channel_SO2_Perturbation: This is SO2 perturbation channel indices 
!                              associated with the channel positions in which 
!                              the coefficients are located.  
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         Channel_HNO3_Perturbation: This is HNO3 perturbation channel indices 
!                              associated with the channel positions in which 
!                              the coefficients are located.  
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         Channel_N2O_Perturbation: This is N2O perturbation channel indices 
!                              associated with the channel positions in which 
!                              the coefficients are located.  
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         Channel_NON_LTE:     This is non-LTE channel indices 
!                              associated with the channel positions in which 
!                              the coefficients are located.  
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         Standard_Level_Pressure:  The standard level pressure for the
!                              atmosphere. 
!                              UNITS:      N/A
!                              TYPE:       REAL (Single)
!                              DIMENSION:  Rank-1(0:Ilayer) 
!                              ATTRIBUTES: POINTER			       
!
!         Fix_Gases_Adjustment:The fix gases adjustment for each layer, 
!                              associated with combined effects of water vapor
!                              displacement, gravity (as a function of latitude),
!                              and layer pathlength, to modify the fix gas
!                              optical depth.  
!                              UNITS:      N/A
!                              TYPE:       REAL (Single)
!                              DIMENSION:  Rank-1(Ilayer)  
!                              ATTRIBUTES: POINTER			       
!!
!         Down_F_Factor:       Array containing the downward thermal F factor
!                              coefficients.
!                              UNITS:      N/A
!                              TYPE:       REAL (Single)
!                              DIMENSION:  MF (n_F_Predictors) x L (n_Channels)
!                              ATTRIBUTES: POINTER			       
!
!         Tuning_Multiple:     Array containing the absorber coefficients
!                              tuning multiplies
!                              UNITS:      N/A
!                              TYPE:       REAL (Single)
!                              DIMENSION:  J (n_Tunings) x L (n_Channels)
!                              ATTRIBUTES: POINTER			       
!                               
!         Ref_Profile_Data :   Array containing the reference profile data.
!                              UNITS:      N/A
!                              TYPE:       REAL (Single) 
!                              DIMENSION:  n (n_RefProfile_Items) x ILayer 
!                              ATTRIBUTES: POINTER
!
!         Non_LTE_Coeff  :     Array containing the Non-LTE effect coefficients.
!                              UNITS:      N/A
!                              TYPE:       REAL (Single) 
!                              DIMENSION:  MNON (n_NONLTE_Predictors) x MChannels  
!                              ATTRIBUTES: POINTER
!
!         ODCAPS_ODAS :        Stucture containing the water vapor absorption
!                              OPTRAN model coefficients.
!                              See the  ODCAPS_ODAS_Define module.
!                              UNITS:      N/A
!                              TYPE:       TYPE (ODCAPS_ODAS_type) 
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: POINTER
!
!         ODCAPS_TraceGas:    Stucture containing the trace gas absorption
!                              model coefficients.
!                              See the ODCAPS_TraceGas_Define module.
!                              UNITS:      N/A
!                              TYPE:       TYPE (ODCAPS_TraceGas_type) 
!                              DIMENSION:  Rank-1 (n_TraceGases) 
!                              ATTRIBUTES: POINTER
!
!         ODCAPS_Subset:      Stucture containing the gas absorption
!                              model coefficients.
!                              See the ODCAPS_Subset_Define module.
!                              UNITS:      N/A 
!                              TYPE:       TYPE (ODCAPS_Subset_type) 
!                              DIMENSION:  Rank-1 (n_Subsets) 
!                              ATTRIBUTES: POINTER
!
!       *!IMPORTANT!*
!       -------------
!       Note that the ODCAPS_type is PUBLIC and its members are not
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
!       Written by:     Yong Chen, CSU/CIRA 04-May-2006
!                       Yong.Chen@noaa.gov
!
!  Copyright (C) 2006 Yong Chen
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

MODULE ODCAPS_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds,      ONLY: Long, Double, Single
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, Display_Message
   
  USE ODCAPS_ODAS_Define
  USE ODCAPS_TraceGas_Define
  USE ODCAPS_Subset_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Public procedures to manipulate the TauCoeff_SARTA structure
  PUBLIC :: Associated_ODCAPS
  PUBLIC :: Destroy_ODCAPS
  PUBLIC :: Allocate_ODCAPS
  PUBLIC :: Assign_ODCAPS
  PUBLIC :: CheckRelease_ODCAPS
  PUBLIC :: CheckAlgorithm_ODCAPS
  PUBLIC :: Info_ODCAPS

  ! Public parameters
  ! -----------------
  ! Sensor Id defaults
  PUBLIC :: INVALID_WMO_SATELLITE_ID
  PUBLIC :: INVALID_WMO_SENSOR_ID
  ! Allowable sensor type values and names
  PUBLIC :: N_SENSOR_TYPES
  PUBLIC :: INVALID_SENSOR  
  PUBLIC :: MICROWAVE_SENSOR
  PUBLIC :: INFRARED_SENSOR 
  PUBLIC :: VISIBLE_SENSOR  
  PUBLIC :: SENSOR_TYPE_NAME
  ! The Global unique algorithm ID
  PUBLIC :: ODCAPS_ALGORITHM


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- ODCAPS valid values
  INTEGER,      PARAMETER :: IP_INVALID = -1
  REAL(Double), PARAMETER :: FP_INVALID = -1.0_Double
 
  ! -- Keyword set value
  INTEGER, PARAMETER :: SET = 1

  ! String lengths
  INTEGER, PARAMETER :: SL = 20   ! Sensor Id
  INTEGER, PARAMETER :: ML = 256  ! Messages

  ! -- Current valid release and version numbers
  INTEGER, PARAMETER :: ODCAPS_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: ODCAPS_VERSION = 1  ! This is just the data version.

  ! The optical depth algorithm Id
  INTEGER     , PARAMETER :: ODCAPS_ALGORITHM = 3
  CHARACTER(*), PARAMETER :: ODCAPS_ALGORITHM_NAME = 'ODCAPS'

  ! ASCII codes for Version routine
  INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
  INTEGER, PARAMETER :: LINEFEED = 10
  ! Invalid sensor ids
  INTEGER, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047
  ! The instrument types
  INTEGER, PARAMETER :: N_SENSOR_TYPES     = 4
  INTEGER, PARAMETER :: INVALID_SENSOR     = 0
  INTEGER, PARAMETER :: MICROWAVE_SENSOR   = 1
  INTEGER, PARAMETER :: INFRARED_SENSOR    = 2
  INTEGER, PARAMETER :: VISIBLE_SENSOR     = 3
  INTEGER, PARAMETER :: ULTRAVIOLET_SENSOR = 4
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_SENSOR_TYPES ) :: &
    SENSOR_TYPE_NAME = (/ 'Invalid    ', &
                          'Microwave  ', &
                          'Infrared   ', &
                          'Visible    ', &
                          'Ultraviolet' /)
 
  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! ------------------------------
  ! ODCAPS data type definition
  ! ------------------------------

  TYPE, PUBLIC :: ODCAPS_type
    INTEGER :: n_Allocates = 0

    ! -- Release and version information
    INTEGER( Long ) :: Release = ODCAPS_RELEASE
    INTEGER( Long ) :: Version = ODCAPS_VERSION
    ! Algorithm identifer
    INTEGER( Long ) :: Algorithm = ODCAPS_ALGORITHM

    ! -- Array dimensions
    INTEGER( Long ) :: n_Layers            = 0    ! Ilayer 100 layers
    INTEGER( Long ) :: n_Channels          = 0    ! L AIRS 2378 channels

    INTEGER( Long ) :: n_Tunings           = 0    ! J  7 tuning multipliers
    INTEGER( Long ) :: n_F_Predictors      = 0    ! MF 6 thermal "F" factor coefs
    INTEGER (Long ) :: n_RefProfile_Items  = 0    ! N  12 item 
    INTEGER (Long ) :: n_NONLTE_Predictors = 0    ! MNON 6 coefs
    INTEGER (Long ) :: n_NONLTE_Channels   = 0    ! MChannels 201 channels

    INTEGER( Long ) :: n_TraceGases        = 0    ! KTraces 4 trace gases perturbation: CO2, SO2, HNO3 N2O 
    INTEGER( Long ) :: n_Subsets           = 0    ! K  7 Tau coefficients subsets
     
    ! Scalar components
    CHARACTER(SL) :: Sensor_Id        = ' '
    INTEGER(Long) :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER(Long) :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    INTEGER(Long) :: Sensor_Type      = INVALID_SENSOR

    ! Water vapor OPTRAN coefficients data
    TYPE (ODCAPS_ODAS_type), POINTER :: ODCAPS_ODAS => NULL()  ! scalar
   
    ! -- The actual sensor channel numbers
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Sensor_Channel             => NULL()  ! L  value from 1-2378

    ! -- The set number for each channel
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_Subset_Index      => NULL()  ! L  value 1-7

    ! -- The position in subset for each channel
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_Subset_Position   => NULL()  ! L

    ! -- The channels for H2O using OPTRAN
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_H2O_OPTRAN        => NULL()  ! L

    ! -- The channels for CO2 perturbation
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_CO2_Perturbation  => NULL()  ! L

    ! -- The channels for SO2 perturbation
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_SO2_Perturbation  => NULL()  ! L

    ! -- The channels for HNO3 perturbation
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_HNO3_Perturbation => NULL()  ! L

    ! -- The channels for N2O perturbation
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_N2O_Perturbation  => NULL()  ! L

    ! -- The channels for non-LTE effects for strong CO2 absorption at the upper atmosphere (~4um)
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_NON_LTE           => NULL()  ! L
 
    ! -- The standard 101 levels pressure 
    REAL( Single ),  POINTER, DIMENSION( : ) :: Standard_Level_Pressure   => NULL()  ! 0:Ilayer

    ! -- The fix gases adjustment for each layer 
    REAL( Single ),  POINTER, DIMENSION( : ) :: Fix_Gases_Adjustment      => NULL()  ! Ilayer

    ! -- The downward thermal F factor coefficients.
    REAL( Single ),  POINTER, DIMENSION( :, : ) :: Down_F_Factor          => NULL()  ! MF x L
 
    ! -- Tuning_Multiple array containing the absorber coefficients tuning multiplies
    REAL( Single ),  POINTER, DIMENSION( :, : ) :: Tuning_Multiple        => NULL()  ! J x L
    
    ! -- Reference Profile data array containing the reference profile for SARTA
    ! -- have 12 column, 1) Layer average altitude (m), 2) layer thickness (m), 
    ! -- 3) layer slab averge pressure (atm), 4) layer slab average temperature (K)
    ! -- 5) fixed gases amount (k.mol/cm2), 6) water amount (k.mol/cm)2
    ! -- 7) ozone amount, 8) carbon monoxide amount, 9) methane amount
    ! --10) SO2, 11) HNO3, 12) N2O
    REAL( Single ),  POINTER, DIMENSION( :, : ) :: Ref_Profile_Data       => NULL()  ! n x ILayer

    ! NON-LTE Coefficients
    REAL( Single ),  POINTER, DIMENSION( :, : ) :: Non_LTE_Coeff          => NULL()  ! MNON x MChannels


    ! Trace Gases Tau Coefficients Subset
    TYPE (ODCAPS_TraceGas_type), POINTER, DIMENSION( : ) :: ODCAPS_TraceGas  => NULL() ! KTraces
    
    ! Tau Coefficients Subset
    TYPE (ODCAPS_Subset_type), POINTER, DIMENSION( : ) :: ODCAPS_Subset  => NULL()  ! K

  END TYPE ODCAPS_type


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
!       Clear_ODCAPS
!
! PURPOSE:
!       Subroutine to clear the scalar members of a ODCAPS structure.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_ODCAPS( TauCoeff ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!      ODCAPS:       ODCAPS structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       TauCoeff_SARTA_type
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
!       Note the INTENT on the output ODCAPS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 04-May-2006
!                       Yong.Chen@noaa.gov
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_ODCAPS( ODCAPS )

    TYPE( ODCAPS_type ), INTENT( IN OUT ) :: ODCAPS

    ODCAPS%n_Layers     = 0
    ODCAPS%n_Channels   = 0

    ODCAPS%n_Tunings     = 0
    ODCAPS%n_F_Predictors     = 0
    ODCAPS%n_RefProfile_Items = 0
    ODCAPS%n_NONLTE_Predictors     = 0
    ODCAPS%n_NONLTE_Channels = 0


    ODCAPS%n_TraceGases   = 0
    ODCAPS%n_Subsets    = 0

    ODCAPS%Release   = ODCAPS_RELEASE
    ODCAPS%Version   = ODCAPS_VERSION
    ODCAPS%Algorithm = ODCAPS_ALGORITHM
    ODCAPS%Sensor_Id        = ' '
    ODCAPS%Sensor_Type      = INVALID_SENSOR
    ODCAPS%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    ODCAPS%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID

  END SUBROUTINE Clear_ODCAPS





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
!       Associated_ODCAPS
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       ODCAPS structure.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_ODCAPS(ODCAPS,           &  ! Input
!                                ANY_Test = Any_Test,&  ! Optional input					      
!				 Skip_ODCAPS_ODAS  = Skip_ODCAPS_ODAS,   & ! Optional input		      
!				 Skip_ODCAPS_TraceGas = Skip_ODCAPS_TraceGas,  & ! Optional input		      
! 				 Skip_ODCAPS_Subset	= Skip_ODCAPS_Subset)    & ! Optional input		      
!
! INPUT ARGUMENTS:
!       ODCAPS:      ODCAPS structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       ODCAPS_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                    ODCAPS structure pointer members are associated.
!                    The default is to test if ALL the pointer members
!                    are associated.
!                    If ANY_Test = 0, test if ALL the pointer members
!                                     are associated.  (DEFAULT)
!                       ANY_Test = 1, test if ANY of the pointer members
!                                     are associated.
!
!       Skip_ODCAPS_ODAS :  Set this argument to not include the ODCAPS_ODAS 
!                                member in the association test.  
!                                If Skip_ODCAPS_ODAS = 0, the ODCAPS_ODAS member association
!                                		               status is tested.  (DEFAULT)
!                                   Skip_ODCAPS_ODAS = 1, the ODCAPS_ODAS member association
!                                		               status is NOT tested.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Skip_ODCAPS_TraceGas:  Set this argument to not include the ODCAPS_TraceGas
!                                member in the association test. This is required
!                                because a valid ODCAPS structure can be
!                                tracegas-free.
!                                If Skip_ODCAPS_TraceGas = 0, the ODCAPS_TraceGas member association
!                                		                status is tested.  (DEFAULT)
!                                   Skip_ODCAPS_TraceGas = 1, the ODCAPS_TraceGas member association
!                                		                status is NOT tested.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Skip_ODCAPS_Subset:   Set this argument to not include the ODCAPS_Subset 
!                               member in the association test. 
!                               If Skip_ODCAPS_Subset = 0, the ODCAPS_Subset member association
!                               		   status is tested.  (DEFAULT)
!                                  Skip_ODCAPS_Subset = 1, the ODCAPS_Subset member association
!                               		   status is NOT tested.
!                               UNITS:      N/A
!                               TYPE:	    INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the ODCAPS pointer members.
!                            .TRUE.  - if ALL the ODCAPS pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the ODCAPS pointer
!                                      members are associated.
!                            .FALSE. - some or all of the ODCAPS pointer
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
!       This function tests the association status of the ODCAPS
!       structure pointer members. Therefore this function must only
!       be called after the input ODCAPS structure has, at least,
!       had its pointer members initialized.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 04-May-2006
!                       Yong.Chen@noaa.gov
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_ODCAPS(   ODCAPS,                & ! Input
                                ANY_Test,                & ! Optional input
				Skip_ODCAPS_ODAS,   & ! Optional input
				Skip_ODCAPS_TraceGas,  & ! Optional input
 				Skip_ODCAPS_Subset)    & ! Optional input
                              RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( ODCAPS_type ), INTENT( IN ) :: ODCAPS

    ! -- Optional input
    INTEGER,     OPTIONAL, INTENT( IN ) :: ANY_Test
    INTEGER,     OPTIONAL, INTENT( IN ) :: Skip_ODCAPS_ODAS 
    INTEGER,     OPTIONAL, INTENT( IN ) :: Skip_ODCAPS_TraceGas 
    INTEGER,     OPTIONAL, INTENT( IN ) :: Skip_ODCAPS_Subset

    ! ---------------
    ! Function result
    ! ---------------

    LOGICAL :: Association_Status


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: ALL_Test
    LOGICAL :: Include_ODCAPS_ODAS 
    LOGICAL :: Include_ODCAPS_TraceGas 
    LOGICAL :: Include_ODCAPS_Subset



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

    ! -----------------------------------------
    ! The Skip_ODCAPS_ODAS optional argument
    ! -----------------------------------------

    ! -- Default is to include the ODCAPS_ODAS member
    ! -- in the association test....
    Include_ODCAPS_ODAS = .TRUE.

    ! ...unless the Skip_ODCAPS_ODAS argument is set.
    IF ( PRESENT( Skip_ODCAPS_ODAS ) ) THEN
      IF ( Skip_ODCAPS_ODAS == SET ) Include_ODCAPS_ODAS = .FALSE.
    END IF

    ! -----------------------------------------
    ! The Skip_ODCAPS_TraceGas optional argument
    ! -----------------------------------------

    ! -- Default is to include the ODCAPS_TraceGas member
    ! -- in the association test....
    Include_ODCAPS_TraceGas = .TRUE.

    ! ...unless the Skip_ODCAPS_TraceGas argument is set.
    IF ( PRESENT( Skip_ODCAPS_TraceGas ) ) THEN
      IF ( Skip_ODCAPS_TraceGas == SET ) Include_ODCAPS_TraceGas = .FALSE.
    END IF


    ! -----------------------------------------
    ! The Skip_ODCAPS_Subset optional argument
    ! -----------------------------------------

    ! -- Default is to include the ODCAPS_Subset member
    ! -- in the association test....
    Include_ODCAPS_Subset = .TRUE.

    ! ...unless the Skip_ODCAPS_Subset argument is set.
    IF ( PRESENT( Skip_ODCAPS_Subset ) ) THEN
      IF ( Skip_ODCAPS_Subset == SET ) Include_ODCAPS_Subset = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( ODCAPS%Sensor_Channel             ) .AND. &
           ASSOCIATED( ODCAPS%Channel_Subset_Index      ) .AND. &
           ASSOCIATED( ODCAPS%Channel_Subset_Position   ) .AND. &
           ASSOCIATED( ODCAPS%Channel_H2O_OPTRAN        ) .AND. &
           ASSOCIATED( ODCAPS%Channel_CO2_Perturbation  ) .AND. &
           ASSOCIATED( ODCAPS%Channel_SO2_Perturbation  ) .AND. &
           ASSOCIATED( ODCAPS%Channel_HNO3_Perturbation ) .AND. &
           ASSOCIATED( ODCAPS%Channel_N2O_Perturbation  ) .AND. &
           ASSOCIATED( ODCAPS%Channel_NON_LTE           ) .AND. &
           ASSOCIATED( ODCAPS%Standard_Level_Pressure   ) .AND. &
           ASSOCIATED( ODCAPS%Fix_Gases_Adjustment      ) .AND. &
           ASSOCIATED( ODCAPS%Down_F_Factor             ) .AND. &
           ASSOCIATED( ODCAPS%Tuning_Multiple           ) .AND. &
           ASSOCIATED( ODCAPS%Ref_Profile_Data          ) .AND. &
           ASSOCIATED( ODCAPS%Non_LTE_Coeff             )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( ODCAPS%Sensor_Channel             ) .OR. &
           ASSOCIATED( ODCAPS%Channel_Subset_Index      ) .OR. &
           ASSOCIATED( ODCAPS%Channel_Subset_Position   ) .OR. &
           ASSOCIATED( ODCAPS%Channel_H2O_OPTRAN        ) .OR. &
           ASSOCIATED( ODCAPS%Channel_CO2_Perturbation  ) .OR. &
           ASSOCIATED( ODCAPS%Channel_SO2_Perturbation  ) .OR. &
           ASSOCIATED( ODCAPS%Channel_HNO3_Perturbation ) .OR. &
           ASSOCIATED( ODCAPS%Channel_N2O_Perturbation  ) .OR. &
           ASSOCIATED( ODCAPS%Channel_NON_LTE           ) .OR. &
           ASSOCIATED( ODCAPS%Standard_Level_Pressure   ) .OR. &
           ASSOCIATED( ODCAPS%Fix_Gases_Adjustment      ) .OR. &
           ASSOCIATED( ODCAPS%Down_F_Factor             ) .OR. &
           ASSOCIATED( ODCAPS%Tuning_Multiple           ) .OR. &
           ASSOCIATED( ODCAPS%Ref_Profile_Data          ) .OR. &
           ASSOCIATED( ODCAPS%Non_LTE_Coeff             )       ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

    ! ---------------------------------------
    ! Test the members that MAY be associated
    ! ---------------------------------------

    ! -- ODCAPS_ODAS  
    IF ( Include_ODCAPS_ODAS ) THEN

      IF ( ALL_Test ) THEN

        IF ( Association_Status             .AND. &
             ASSOCIATED( ODCAPS%ODCAPS_ODAS )       ) THEN
          Association_Status = .TRUE.
        END IF

      ELSE

        IF ( Association_Status             .OR. &
             ASSOCIATED( ODCAPS%ODCAPS_ODAS )      ) THEN
          Association_Status = .TRUE.
        END IF

      END IF

    END IF

    ! -- ODCAPS_TraceGas 
    IF ( Include_ODCAPS_TraceGas ) THEN

      IF ( ALL_Test ) THEN

        IF ( Association_Status             .AND. &
             ASSOCIATED( ODCAPS%ODCAPS_TraceGas )       ) THEN
          Association_Status = .TRUE.
        END IF

      ELSE

        IF ( Association_Status             .OR. &
             ASSOCIATED( ODCAPS%ODCAPS_TraceGas )      ) THEN
          Association_Status = .TRUE.
        END IF

      END IF

    END IF

    ! -- ODCAPS_Subset
    IF ( Include_ODCAPS_Subset ) THEN

      IF ( ALL_Test ) THEN

        IF ( Association_Status             .AND. &
             ASSOCIATED( ODCAPS%ODCAPS_Subset )       ) THEN
          Association_Status = .TRUE.
        END IF

      ELSE

        IF ( Association_Status             .OR. &
             ASSOCIATED( ODCAPS%ODCAPS_Subset )      ) THEN
          Association_Status = .TRUE.
        END IF

      END IF

    END IF

  END FUNCTION Associated_ODCAPS





!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_ODCAPS
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of ODCAPS
!       data structures.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_ODCAPS(   ODCAPS,                 &  ! Output
!                                        RCS_Id = RCS_Id,          &  ! Revision control
!                                        Message_Log = Message_Log )  ! Error messaging
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
!       ODCAPS:       Re-initialized ODCAPS structure.
!                     UNITS:      N/A
!                     TYPE:       ODCAPS_type
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
!                     The error codes are defined in the Message_Handler module.
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
!       Associated_ODCAPS:  Function to test the association status of the
!                             pointer members of a ODCAPS structure.
!
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
!       Note the INTENT on the output ODCAPS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 04-May-2006
!                       Yong.Chen@noaa.gov
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_ODCAPS(   ODCAPS,       &  ! Output
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
    TYPE( ODCAPS_type ),    INTENT( IN OUT )   :: ODCAPS

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_ODCAPS'


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
    !#                     -- PERFORM RE-INITIALISATION --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_ODCAPS( ODCAPS )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_ODCAPS( ODCAPS ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the channel index array
    IF ( ASSOCIATED( ODCAPS%Sensor_Channel ) ) THEN

      DEALLOCATE( ODCAPS%Sensor_Channel, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Sensor_Channel ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channel subset index array
    IF ( ASSOCIATED( ODCAPS%Channel_Subset_Index ) ) THEN

      DEALLOCATE( ODCAPS%Channel_Subset_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Channel_Subset_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channel subset position array
    IF ( ASSOCIATED( ODCAPS%Channel_Subset_Position ) ) THEN

      DEALLOCATE( ODCAPS%Channel_Subset_Position, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Channel_Subset_Position ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for H2O using OPTRAN array
    IF ( ASSOCIATED( ODCAPS%Channel_H2O_OPTRAN ) ) THEN

      DEALLOCATE( ODCAPS%Channel_H2O_OPTRAN, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Channel_H2O_OPTRAN ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for CO2 Perturbation array
    IF ( ASSOCIATED( ODCAPS%Channel_CO2_Perturbation ) ) THEN

      DEALLOCATE( ODCAPS%Channel_CO2_Perturbation, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Channel_CO2_Perturbation ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for SO2 Perturbation array
    IF ( ASSOCIATED( ODCAPS%Channel_SO2_Perturbation ) ) THEN

      DEALLOCATE( ODCAPS%Channel_SO2_Perturbation, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Channel_SO2_Perturbation ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for HNO3 Perturbation array
    IF ( ASSOCIATED( ODCAPS%Channel_HNO3_Perturbation ) ) THEN

      DEALLOCATE( ODCAPS%Channel_HNO3_Perturbation, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Channel_HNO3_Perturbation ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for N2O Perturbation array
    IF ( ASSOCIATED( ODCAPS%Channel_N2O_Perturbation ) ) THEN

      DEALLOCATE( ODCAPS%Channel_N2O_Perturbation, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Channel_N2O_Perturbation ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for non-LTE effects array
    IF ( ASSOCIATED( ODCAPS%Channel_NON_LTE ) ) THEN

      DEALLOCATE( ODCAPS%Channel_NON_LTE, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Channel_NON_LTE ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate standard levels pressure array
    IF ( ASSOCIATED( ODCAPS%Standard_Level_Pressure ) ) THEN

      DEALLOCATE( ODCAPS%Standard_Level_Pressure, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Standard_Level_Pressure ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate fix gases adjustment array
    IF ( ASSOCIATED( ODCAPS%Fix_Gases_Adjustment ) ) THEN

      DEALLOCATE( ODCAPS%Fix_Gases_Adjustment, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Fix_Gases_Adjustment ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Down_F_Factor
    IF ( ASSOCIATED( ODCAPS%Down_F_Factor ) ) THEN

      DEALLOCATE( ODCAPS%Down_F_Factor, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Down_F_Factor ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Tuning_Multiple
    IF ( ASSOCIATED( ODCAPS%Tuning_Multiple ) ) THEN

      DEALLOCATE( ODCAPS%Tuning_Multiple, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Tuning_Multiple ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate reference profile data array
    IF ( ASSOCIATED( ODCAPS%Ref_Profile_Data ) ) THEN

      DEALLOCATE( ODCAPS%Ref_Profile_Data, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Ref_Profile_Data ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate non-LTE coefficients data array
    IF ( ASSOCIATED( ODCAPS%Non_LTE_Coeff ) ) THEN

      DEALLOCATE( ODCAPS%Non_LTE_Coeff, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS Non_LTE_Coeff ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -------------------------------------------------------------
    ! Deallocate the ODCAPS_ODAS structure pointer member
    ! -------------------------------------------------------------

    IF ( ASSOCIATED( ODCAPS%ODCAPS_ODAS ) ) THEN


      ! -- Destroy the ODCAPS_ODAS structure(s)
      Error_Status = Destroy_ODCAPS_ODAS( ODCAPS%ODCAPS_ODAS, &
                                              No_Clear = No_Clear, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error destroying ODCAPS_ODAS structure(s).', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF

    END IF

    ! -------------------------------------------------------------
    ! Deallocate the ODCAPS_TraceGas structure array pointer member
    ! -------------------------------------------------------------

    IF ( ASSOCIATED( ODCAPS%ODCAPS_TraceGas ) ) THEN


      ! -- Destroy the ODCAPS_TraceGas structure(s)
      Error_Status = Destroy_ODCAPS_TraceGas( ODCAPS%ODCAPS_TraceGas, &
                                              No_Clear = No_Clear, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error destroying ODCAPS_TraceGas structure(s).', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF


      ! -- Deallocate the array
      DEALLOCATE( ODCAPS%ODCAPS_TraceGas, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS_TraceGas", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -------------------------------------------------------------
    ! Deallocate the ODCAPS_Subset structure array pointer member
    ! -------------------------------------------------------------

    IF ( ASSOCIATED( ODCAPS%ODCAPS_Subset ) ) THEN


      ! -- Destroy the ODCAPS_Subset structure(s)
      Error_Status = Destroy_ODCAPS_Subset( ODCAPS%ODCAPS_Subset, &
                                              No_Clear = No_Clear, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error destroying ODCAPS_Subset structure(s).', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF


      ! -- Deallocate the array
      DEALLOCATE( ODCAPS%ODCAPS_Subset, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ODCAPS_Subset", &
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

    ODCAPS%n_Allocates = ODCAPS%n_Allocates - 1

    IF ( ODCAPS%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      ODCAPS%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_ODCAPS





!------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_ODCAPS
! 
! PURPOSE:
!       Function to allocate the pointer members of the ODCAPS
!       data structure.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_ODCAPS(   n_Layers,           &  ! Input
!                                  	  n_Channels,	      &  ! Input	     
!                                   	  n_Tunings,	      &  ! Input	    
!                                   	  n_F_Predictors,     &  ! Input	    
!			           	  n_RefProfile_Items, &  ! Input	    
!			           	  n_NONLTE_Predictors,&  ! Input	    
!                                   	  n_NONLTE_Channels,  &  ! Input	    
!                                   	  n_TraceGases,       &  ! Input	    
!                                   	  n_Subsets,	      &  ! Input	    
!					  ODCAPS,	      &  ! Output
! 					  RCS_Id,	      &  ! Revision control
!					  Message_Log )       &  ! Error messaging
!
!
! INPUT ARGUMENTS:
!         n_Layers:            Maximum layers for the coefficients.
!                              "Ilayers" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Channels:          Total number of spectral channels.
!                              "L" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Tunings:           Number of tuning multipliers for the gases 
!                              "J" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_F_Predictors:      Number of predictors used in the
!                              thermal "F" factor coefs.
!                              "MF" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_RefProfile_Items:  Number of item in the reference profile.
!                              "N" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_NONLTE_Predictors: The number of predictors for the non-LTE.
!                              "MNON" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_NONLTE_Channels:   The number of channels for the non-LTE.
!                              "MChannels" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_TraceGases:        The number of trace gases.
!                              "KTraces" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Subsets:           The number of subset for the gas absorption coeffs.
!                              "K" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in
!                     which any messages will be logged. If not
!                     specified, or if an error occurs opening
!                     the log file, the default action is to
!                     output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       ODCAPS:       ODCAPS structure with allocated
!                     pointer members
!                     UNITS:      N/A
!                     TYPE:       ODCAPS_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
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
!                     The error codes are defined in the Message_Handler module.
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
!       Associated_ODCAPS:  Function to test the association status of the
!                             pointer members of a ODCAPS structure.
!
!       Destroy_ODCAPS:     Function to re-initialize the scalar and pointer
!                             members of ODCAPS data structures.
!
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
!       Note the INTENT on the output ODCAPS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 04-May-2006
!                       Yong.Chen@noaa.gov
!S-
!------------------------------------------------------------------------------

  FUNCTION Allocate_ODCAPS(   n_Layers,           &  ! Input
                              n_Channels,         &  ! Input
                              n_Tunings,          &  ! Input
			      n_F_Predictors,     &  ! Input
			      n_RefProfile_Items, &  ! Input
			      n_NONLTE_Predictors,&  ! Input
			      n_NONLTE_Channels,  &  ! Input
			      n_TraceGases,       &  ! Input
			      n_Subsets,          &  ! Input
                              ODCAPS,             &  ! Output
                              RCS_Id,             &  ! Revision control
                              Message_Log )       &  ! Error messaging
                            RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )     :: n_Layers
    INTEGER,                  INTENT( IN )     :: n_Channels
    INTEGER,                  INTENT( IN )     :: n_Tunings 
    INTEGER,                  INTENT( IN )     :: n_F_Predictors
    INTEGER,                  INTENT( IN )     :: n_RefProfile_Items
    INTEGER,                  INTENT( IN )     :: n_NONLTE_Predictors 
    INTEGER,                  INTENT( IN )     :: n_NONLTE_Channels
    INTEGER,                  INTENT( IN )     :: n_TraceGases
    INTEGER,                  INTENT( IN )     :: n_Subsets

    ! -- Output
    TYPE( ODCAPS_type ),    INTENT( IN OUT )   :: ODCAPS

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_ODCAPS'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

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

    IF ( n_Layers             < 1 .OR. &
         n_Channels           < 1 .OR. &
         n_Tunings            < 1 .OR. &
         n_F_Predictors       < 1 .OR. &
         n_RefProfile_Items   < 1 .OR. &
         n_NONLTE_Predictors  < 1 .OR. &
         n_NONLTE_Channels    < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input ODCAPS dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Number of TraceGases. Can be == 0.
    IF ( n_TraceGases < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_TraceGases must be > or = 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Number of Subsets. Can be == 0.
    IF ( n_Subsets < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Subsets must be > or = 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -----------------------------------------------
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_ODCAPS( ODCAPS, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_ODCAPS(   ODCAPS, &
                                       No_Clear = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating ODCAPS pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#
   
    ! -------------------------
    ! The intrinsic type arrays
    ! -------------------------

    ALLOCATE( ODCAPS%Sensor_Channel( n_Channels ), &
              ODCAPS%Channel_Subset_Index( n_Channels ), &
              ODCAPS%Channel_Subset_Position( n_Channels ), &
              ODCAPS%Channel_H2O_OPTRAN( n_Channels ), &
              ODCAPS%Channel_CO2_Perturbation( n_Channels ), &
              ODCAPS%Channel_SO2_Perturbation( n_Channels ), &
              ODCAPS%Channel_HNO3_Perturbation( n_Channels ), &
              ODCAPS%Channel_N2O_Perturbation( n_Channels ), &
              ODCAPS%Channel_NON_LTE( n_Channels ), &
              ODCAPS%Standard_Level_Pressure( 0:n_Layers ), &
              ODCAPS%Fix_Gases_Adjustment( n_Layers ), &
              ODCAPS%Down_F_Factor( n_F_Predictors, n_Channels ), &
              ODCAPS%Tuning_Multiple( n_Tunings, n_Channels ), &
              ODCAPS%Ref_Profile_Data(n_RefProfile_Items, n_Layers ),&
              ODCAPS%Non_LTE_Coeff(n_NONLTE_Predictors, n_NONLTE_Channels ),&
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating ODCAPS data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! ------------------------------
    ! The ODCAPS_ODAS structure 
    ! ------------------------------

    ! -- Allocate the individual structures
    ALLOCATE( ODCAPS%ODCAPS_ODAS, &
                STAT = Allocate_Status )
		
    IF ( Allocate_Status /= 0 ) THEN								   
      Error_Status = FAILURE									   
      WRITE( Message, '( "Error allocating ODCAPS_ODAS structure. STAT = ", i5 )' ) &  
    		      Allocate_Status								   
      CALL Display_Message( ROUTINE_NAME,    &  						   
    			    TRIM( Message ), &  						   
    			    Error_Status,    &  						   
    			    Message_Log = Message_Log ) 					   
      RETURN											   
    END IF											   
		
    ! ---------------------------------------
    ! The ODCAPS_TraceGas structure array
    ! ---------------------------------------

    IF ( n_TraceGases > 0 ) THEN

      ! -- Allocate the structure array
      ALLOCATE( ODCAPS%ODCAPS_TraceGas( n_TraceGases ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error allocating ODCAPS_TraceGas structure array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF
    
    ! -----------------------------------
    ! The ODCAPS_Subset structure array
    ! -----------------------------------

    IF ( n_Subsets > 0 ) THEN

      ! -- Allocate the structure array
      ALLOCATE( ODCAPS%ODCAPS_Subset( n_Subsets ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error allocating ODCAPS Subset structure array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
 
    END IF

    !#--------------------------------------------------------------------------#
    !#                        -- ASSIGN THE DIMENSIONS --                       #
    !#--------------------------------------------------------------------------#

    ODCAPS%n_Layers           = n_Layers
    ODCAPS%n_Channels         = n_Channels
    ODCAPS%n_Tunings          = n_Tunings 
    ODCAPS%n_F_Predictors     = n_F_Predictors
    ODCAPS%n_RefProfile_Items = n_RefProfile_Items
    ODCAPS%n_NONLTE_Predictors= n_NONLTE_Predictors
    ODCAPS%n_NONLTE_Channels  = n_NONLTE_Channels
    ODCAPS%n_TraceGases       = n_TraceGases 
    ODCAPS%n_Subsets          = n_Subsets

  
    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    ODCAPS%n_Allocates = ODCAPS%n_Allocates + 1

    IF ( ODCAPS%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      ODCAPS%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_ODCAPS





!------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_ODCAPS
!
! PURPOSE:
!       Function to copy valid ODCAPS structures.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_ODCAPS(   ODCAPS_in,              &  ! Input
!                                       ODCAPS_out,             &  ! Output
!                                       RCS_Id      = RCS_Id,     &  ! Revision control
!                                       Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODCAPS_in:     ODCAPS structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       ODCAPS_type
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
!       ODCAPS_out:    Copy of the input structure, ODCAPS_in.
!                      UNITS:      N/A
!                      TYPE:       ODCAPS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
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
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure assignment was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_ODCAPS:  Function to test the association status of the
!                             pointer members of a ODCAPS structure.
!
!       Allocate_ODCAPS:    Function to allocate the pointer members of
!                             the ODCAPS data structure.
!
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
!       Note the INTENT on the output ODCAPS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 04-May-2006
!                       Yong.Chen@noaa.gov
!S-
!------------------------------------------------------------------------------

  FUNCTION Assign_ODCAPS(   ODCAPS_in,   &  ! Input
                            ODCAPS_out,  &  ! Output
                            RCS_Id,        &  ! Revision control
                            Message_Log )  &  ! Error messaging
                          RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( ODCAPS_type ),    INTENT( IN )     :: ODCAPS_in

    ! -- Output
    TYPE( ODCAPS_type ),    INTENT( IN OUT ) :: ODCAPS_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_ODCAPS'



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
    ! BUT the ODCAPS_TraceGas and/or 
    ! ODCAPS_Subset pointer members may not be.
    ! ---------------------------------------

    IF ( .NOT. Associated_ODCAPS( ODCAPS_in, &
                                    Skip_ODCAPS_TraceGas = SET ) ) THEN
      Error_Status = Destroy_ODCAPS( ODCAPS_out, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN 
        CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT ODCAPS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      END IF			    
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------

    ODCAPS_out%Release           = ODCAPS_in%Release
    ODCAPS_out%Version           = ODCAPS_in%Version
    ODCAPS_out%Algorithm         = ODCAPS_in%Algorithm  
    ODCAPS_out%Sensor_Id         = ODCAPS_in%Sensor_Id
    ODCAPS_out%Sensor_Type       = ODCAPS_in%Sensor_Type
    ODCAPS_out%WMO_Satellite_ID  = ODCAPS_in%WMO_Satellite_ID
    ODCAPS_out%WMO_Sensor_ID     = ODCAPS_in%WMO_Sensor_ID


    ! -----------------
    ! Assign array data
    ! -----------------

    ! -- Allocate data arrays
    Error_Status = Allocate_ODCAPS(   ODCAPS_in%n_Layers,           &
                                      ODCAPS_in%n_Channels,         &	     
                                      ODCAPS_in%n_Tunings,	      &	     
                                      ODCAPS_in%n_F_Predictors,     &	     
                                      ODCAPS_in%n_RefProfile_Items, &	     
                                      ODCAPS_in%n_NONLTE_Predictors,&      
                                      ODCAPS_in%n_NONLTE_Channels,  &   
                                      ODCAPS_in%n_TraceGases,       &     
                                      ODCAPS_in%n_Subsets,          &
                                      ODCAPS_out,                   &
                                      Message_Log = Message_Log )   
                                     		  
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output ODCAPS arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Copy array data

    ODCAPS_out%Sensor_Channel            = ODCAPS_in%Sensor_Channel 
    ODCAPS_out%Channel_Subset_Index     = ODCAPS_in%Channel_Subset_Index 
    ODCAPS_out%Channel_Subset_Position  = ODCAPS_in%Channel_Subset_Position  
    ODCAPS_out%Channel_H2O_OPTRAN       = ODCAPS_in%Channel_H2O_OPTRAN  
    ODCAPS_out%Channel_CO2_Perturbation = ODCAPS_in%Channel_CO2_Perturbation 
    ODCAPS_out%Channel_SO2_Perturbation = ODCAPS_in%Channel_SO2_Perturbation 
    ODCAPS_out%Channel_HNO3_Perturbation= ODCAPS_in%Channel_HNO3_Perturbation 
    ODCAPS_out%Channel_N2O_Perturbation = ODCAPS_in%Channel_N2O_Perturbation 
    ODCAPS_out%Channel_NON_LTE          = ODCAPS_in%Channel_NON_LTE
    ODCAPS_out%Standard_Level_Pressure  = ODCAPS_in%Standard_Level_Pressure
    ODCAPS_out%Fix_Gases_Adjustment     = ODCAPS_in%Fix_Gases_Adjustment 
    ODCAPS_out%Down_F_Factor            = ODCAPS_in%Down_F_Factor
    ODCAPS_out%Tuning_Multiple          = ODCAPS_in%Tuning_Multiple
    ODCAPS_out%Ref_Profile_Data         = ODCAPS_in%Ref_Profile_Data 
    ODCAPS_out%Non_LTE_Coeff            = ODCAPS_in%Non_LTE_Coeff

    ! ---------------------
    ! Assign structure data
    ! ---------------------

    ! -- Copy ODCAPS_ODAS structure
    Error_Status = Assign_ODCAPS_ODAS( ODCAPS_in%ODCAPS_ODAS ,  &  
    					    ODCAPS_out%ODCAPS_ODAS , &  
    					    Message_Log = Message_Log)         

    IF ( Error_Status /= SUCCESS ) THEN 				       
      CALL Display_Message( ROUTINE_NAME, &				       
    			    'Error copying ODCAPS_ODAS structure.', &     
    			    Error_Status, &				       
    			    Message_Log = Message_Log ) 		       
      RETURN								       
    END IF								       
 
    ! -- Copy ODCAPS_TraceGas structure
    IF ( ODCAPS_in%n_TraceGases > 0 ) THEN

      Error_Status = Assign_ODCAPS_TraceGas( ODCAPS_in%ODCAPS_TraceGas,  &
                                               ODCAPS_out%ODCAPS_TraceGas, &
                                               Message_Log = Message_Log)

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error copying ODCAPS_TraceGas structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF

    ! -- Copy ODCAPS_Subset structure
    IF ( ODCAPS_in%n_Subsets > 0 ) THEN

      Error_Status = Assign_ODCAPS_Subset( ODCAPS_in%ODCAPS_Subset, &
                                             ODCAPS_out%ODCAPS_Subset, &
                                             Message_Log = Message_Log)

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error copying ODCAPS_Subset structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF


  END FUNCTION Assign_ODCAPS


!----------------------------------------------------------------------------------
!S+
! NAME:
!       CheckRelease_ODCAPS
!
! PURPOSE:
!       Function to check the ODCAPS Release value.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CheckRelease_ODCAPS (   ODCAPS,                 &  ! Input
!                                              RCS_Id      = RCS_Id,     &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODCAPS:        ODCAPS structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       ODCAPS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT )
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
! OUTPUT ARGUMENTS:
!       None.
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
!                      The error codes are defined in the Message_Handler module.
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
!                             SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 04-May-2006
!                       Yong.Chen@noaa.gov
!S-
!----------------------------------------------------------------------------------

  FUNCTION CheckRelease_ODCAPS(    ODCAPS,       &  ! Input
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
    TYPE( ODCAPS_type ),    INTENT( IN )  :: ODCAPS

    ! -- Optional output
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CheckRelease_ODCAPS'


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

    IF ( ODCAPS%Release < ODCAPS_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An ODCAPS data update is needed. ", &
                        &"ODCAPS release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      ODCAPS%Release, ODCAPS_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#               -- CHECK THAT THE RELEASE IS NOT TOO NEW --                #
    !#--------------------------------------------------------------------------#

    IF ( ODCAPS%Release > ODCAPS_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An ODCAPS software update is needed. ", &
                        &"ODCAPS release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      ODCAPS%Release, ODCAPS_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION CheckRelease_ODCAPS


!----------------------------------------------------------------------------------
!
! NAME:
!       CheckAlgorithm_ODCAPS
!
! PURPOSE:
!       Function to check the ODCAPS Algorithm value.
!
! CALLING SEQUENCE:
!       Error_Status = CheckAlgorithm_ODCAPS( ODCAPS                   , &  ! Input
!                                           RCS_Id     = RCS_Id    , &  ! Revision control
!                                           Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       ODCAPS:        ODCAPS structure for which the Algorithm member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       ODCAPS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(IN)
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structure Algorithm value is valid.
!                         == FAILURE the structure Algorithm value is NOT valid.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION CheckAlgorithm_ODCAPS( ODCAPS       , &  ! Input
                                  RCS_Id     , &  ! Revision control
                                  Message_Log) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    TYPE(ODCAPS_type)     , INTENT(IN)  :: ODCAPS
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckAlgorithm_ODCAPS'

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID


    ! Check the algorithm ID
    ! ----------------------
    IF ( ODCAPS%Algorithm /= ODCAPS_ALGORITHM ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'The ODCAPS Algorithm ID check failed. '//&
                            'The data structure is not an ODCAPS structure', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CheckAlgorithm_ODCAPS



 !------------------------------------------------------------------------------
!S+
! NAME:
!       Info_ODCAPS
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the TauCoeff_SARTA data structure.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Info_ODCAPS( ODCAPS,       &  ! Input
!                         Version_Info,   &  ! Output
!                         RCS_Id = RCS_Id )  ! Revision control
!
! INPUT ARGUMENTS:
!       ODCAPS:        Filled ODCAPS structure.
!                      UNITS:      N/A
!                      TYPE:       ODCAPS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Version_Info:  String containing version and dimension information
!                      about the passed ODCAPS data structure.
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
!       Written by:     Yong Chen, CSU/CIRA 04-May-2006
!                       Yong.Chen@noaa.gov
!S-
!------------------------------------------------------------------------------

  SUBROUTINE Info_ODCAPS( ODCAPS,     &  ! Input
                          Version_Info, &  ! Output
                          RCS_Id        )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( ODCAPS_type ),    INTENT( IN )  :: ODCAPS

    ! -- Output
    CHARACTER( * ),           INTENT( OUT ) :: Version_Info

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER(2000) :: Long_String



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

    WRITE( Long_String,'( a,1x,"ODCAPS RELEASE.VERSION: ", i2, ".", i2.2, 2x,&
                           &"Algorithm=", i2, 2x, &
			   &"N_LAYERS=",i3,2x,&
                           &"N_CHANNELS=",i4,2x,&
			   &"N_SUBSETS=",i2,2x,&
			   &"N_F_PREDICTORS=",i2,2x,&
			   &"N_REFPROFILE_ITEMS=",i2)' ) &
                         ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                         ODCAPS%Release, ODCAPS%Version, &
                         ODCAPS%Algorithm, &
			 ODCAPS%n_Layers, &
                         ODCAPS%n_Channels, &
			 ODCAPS%n_Subsets, &
                         ODCAPS%n_F_Predictors, &
			 ODCAPS%n_RefProfile_Items
 

    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Version_Info = Long_String(1:MIN( LEN( Version_Info ), LEN_TRIM( Long_String ) ))

  END SUBROUTINE Info_ODCAPS

END MODULE ODCAPS_Define
