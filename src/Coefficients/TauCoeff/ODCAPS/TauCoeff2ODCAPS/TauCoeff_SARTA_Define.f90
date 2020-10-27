!------------------------------------------------------------------------------
!M+
! NAME:
!       TauCoeff_SARTA_Define
!
! PURPOSE:
!       Module defining the TauCoeff_SARTA data structure and containing routines to 
!       manipulate it.
!       
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE TauCoeff_SARTA_Define
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
!       Associated_TauCoeff_SARTA:    Function to test the association status             
!                               of the pointer members of a TauCoeff_SARTA	            
!                               structure.				            
!
!       Destroy_TauCoeff_SARTA:       Function to re-initialize a TauCoeff_SARTA	            
!                               structure.				            
!
!       Allocate_TauCoeff_SARTA:      Function to allocate the pointer members            
!                               of a TauCoeff_SARTA structure.		            
!
!       Assign_TauCoeff_SARTA:        Function to copy a valid TauCoeff_SARTA structure.        
!
!       Check_TauCoeff_SARTA_Release: Function to check the TauCoeff_SARTA Release value.       
!
!       Count_TauCoeff_SARTA_Sensors: Subroutine to count the number of	            
!                               different satellites/sensors in the	            
!                               TauCoeff_SARTA data structure.		            
!
!       Version_TauCoeff_SARTA:       Subroutine to return a string containing            
!                               version and dimension information about             
!                               the TauCoeff_SARTA data structure.		            
!
! DERIVED TYPES:
!       TauCoeff_SARTA_type:   Definition of the public TauCoeff_SARTA data structure. Fields
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
!         Algorithm_ID:        The TauCoeff_SARTA data Algorithm_ID. Used to determine
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
!         n_Sensors:           Number of different satellite/sensors in the
!                              data structure.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
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
!         Tau_OPTRAN_SARTA_Coeff:    Stucture containing the water vapor absorption
!                              OPTRAN model coefficients.
!                              See the Tau_OPTRAN_SARTA_Coeff_Define module.
!                              UNITS:      N/A
!                              TYPE:       TYPE (Tau_OPTRAN_Coeff_type) 
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: POINTER
!
!         TauCoeff_SARTA_TraceGas:   Stucture containing the trace gas absorption
!                              model coefficients.
!                              See the TauCoeff_SARTA_TraceGas_Define module.
!                              UNITS:      N/A
!                              TYPE:       TYPE (TauCoeff_SARTA_TraceGas_type) 
!                              DIMENSION:  Rank-1 (n_TraceGases) 
!                              ATTRIBUTES: POINTER
!
!         TauCoeff_SARTA_Subset:     Stucture containing the gas absorption
!                              model coefficients.
!                              See the TauCoeff_SARTA_Subset_Define module.
!                              UNITS:      N/A 
!                              TYPE:       TYPE (TauCoeff_SARTA_Subset_type) 
!                              DIMENSION:  Rank-1 (n_Subsets) 
!                              ATTRIBUTES: POINTER
!
!       *!IMPORTANT!*
!       -------------
!       Note that the TauCoeff_SARTA_type is PUBLIC and its members are not
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
!M-
!------------------------------------------------------------------------------

MODULE TauCoeff_SARTA_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler
   
  USE Tau_OPTRAN_SARTA_Coeff_Define
  USE TauCoeff_SARTA_TraceGas_Define
  USE TauCoeff_SARTA_Subset_Define


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
  PUBLIC :: Associated_TauCoeff_SARTA
  PUBLIC :: Destroy_TauCoeff_SARTA
  PUBLIC :: Allocate_TauCoeff_SARTA
  PUBLIC :: Assign_TauCoeff_SARTA
  PUBLIC :: Check_TauCoeff_SARTA_Release
  PUBLIC :: Count_TauCoeff_SARTA_Sensors
  PUBLIC :: Version_TauCoeff_SARTA


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- TauCoeff_SARTA valid values
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1
  INTEGER, PRIVATE, PARAMETER ::   VALID =  1

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Sensor descriptor component string length
  INTEGER, PRIVATE, PARAMETER :: DL = 20

  ! -- Current valid release and version numbers
  INTEGER, PRIVATE, PARAMETER :: TAUCOEFF_SARTA_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PRIVATE, PARAMETER :: TAUCOEFF_SARTA_VERSION = 1  ! This is just the data version.

  ! -- The maximum number of Channels for Tau_OPTRAN_Coeff 
  INTEGER, PRIVATE, PARAMETER :: MAX_N_WATER_OPTRAN_CHANNELS  = 637  

  ! -- The maximum number of layers for Tau_OPTRAN_Coeff
  INTEGER, PRIVATE, PARAMETER :: MAX_N_WATER_OPTRAN_LAYERS = 300   

  ! -- The maximum number of OPTRAN water average profile values for Tau_OPTRAN_Coeff 
  INTEGER, PRIVATE, PARAMETER :: MAX_N_WATER_OPTRAN_PROFAVES  = 4  

  ! -- The maximum number of total predictors for Tau_OPTRAN_Coeff
  INTEGER, PRIVATE, PARAMETER :: MAX_N_WATER_OPTRAN_PREDICTORS = 9   
  
  ! -- The maximum number of trace gas perturbation predictors for TauCoeff_SARTA_TraceGas 
  INTEGER, PRIVATE, PARAMETER :: MAX_N_TRACEGASES_PREDICTORS   = 7  
  
  ! -- The maximum number of Channels for TauCoeff_SARTA_TraceGas
  INTEGER, PRIVATE, PARAMETER :: MAX_N_TRACEGASES_CHANNELS  = 868  ! CO2 perturbation
  
  ! -- The maximum number of total predictors for TauCoeff_SARTA_Subset
  INTEGER, PRIVATE, PARAMETER :: MAX_N_SUBSET_TOTAL_PREDICTORS = 45   ! Subset 4
  
  ! -- The maximum number of Channels for TauCoeff_SARTA_Subset
  INTEGER, PRIVATE, PARAMETER :: MAX_N_SUBSET_CHANNELS  = 1090 ! Subset 1
  
  ! -- The maximum number of Absorbers for TauCoeff_SARTA_Subset
  INTEGER, PRIVATE, PARAMETER :: MAX_N_SUBSET_ABSORBERS = 5    ! Subset 4
  

  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! ------------------------------
  ! TauCoeff_SARTA data type definition
  ! ------------------------------

  TYPE, PUBLIC :: TauCoeff_SARTA_type
    INTEGER :: n_Allocates = 0

    ! -- Release and version information
    INTEGER( Long ) :: Release = TauCoeff_SARTA_RELEASE
    INTEGER( Long ) :: Version = TauCoeff_SARTA_VERSION
    INTEGER( Long ) :: Algorithm_ID = 0

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
     
    INTEGER( Long ) :: Sensor_Descriptor_StrLen = DL

    ! -- Number of different satellite/sensor combinations
    INTEGER( Long ) :: n_Sensors = 0

    ! Water vapor OPTRAN coefficients data
    TYPE (Tau_OPTRAN_SARTA_Coeff_type), POINTER :: Tau_OPTRAN_Coeff => NULL()  ! scalar
   
    ! -- The satellite and sensor IDs
    CHARACTER( DL ), POINTER, DIMENSION( : ) :: Sensor_Descriptor => NULL() ! L
    INTEGER( Long ), POINTER, DIMENSION( : ) :: NCEP_Sensor_ID    => NULL() ! L
    INTEGER( Long ), POINTER, DIMENSION( : ) :: WMO_Satellite_ID  => NULL() ! L
    INTEGER( Long ), POINTER, DIMENSION( : ) :: WMO_Sensor_ID     => NULL() ! L

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
    TYPE (TauCoeff_SARTA_TraceGas_type), POINTER, DIMENSION( : ) :: TauCoeff_TraceGas  => NULL() ! KTraces
    
    ! Tau Coefficients Subset
    TYPE (TauCoeff_SARTA_Subset_type), POINTER, DIMENSION( : ) :: TauCoeff_Subset  => NULL()  ! K

  END TYPE TauCoeff_SARTA_type


CONTAINS





  SUBROUTINE Clear_TauCoeff_SARTA( TauCoeff )

    TYPE( TauCoeff_SARTA_type ), INTENT( IN OUT ) :: TauCoeff

    TauCoeff%n_Layers     = 0
    TauCoeff%n_Channels   = 0

    TauCoeff%n_Tunings     = 0
    TauCoeff%n_F_Predictors     = 0
    TauCoeff%n_RefProfile_Items = 0
    TauCoeff%n_NONLTE_Predictors     = 0
    TauCoeff%n_NONLTE_Channels = 0


    TauCoeff%n_TraceGases   = 0
    TauCoeff%n_Subsets    = 0

    TauCoeff%Sensor_Descriptor_StrLen = DL

    TauCoeff%n_Sensors = 0

  END SUBROUTINE Clear_TauCoeff_SARTA







  FUNCTION Associated_TauCoeff_SARTA( TauCoeff,                & ! Input
                                ANY_Test,                & ! Optional input
				Skip_Tau_OPTRAN_Coeff,   & ! Optional input
				Skip_TauCoeff_TraceGas,  & ! Optional input
 				Skip_TauCoeff_Subset)    & ! Optional input
                              RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_SARTA_type ), INTENT( IN ) :: TauCoeff

    ! -- Optional input
    INTEGER,     OPTIONAL, INTENT( IN ) :: ANY_Test
    INTEGER,     OPTIONAL, INTENT( IN ) :: Skip_Tau_OPTRAN_Coeff 
    INTEGER,     OPTIONAL, INTENT( IN ) :: Skip_TauCoeff_TraceGas 
    INTEGER,     OPTIONAL, INTENT( IN ) :: Skip_TauCoeff_Subset

    ! ---------------
    ! Function result
    ! ---------------

    LOGICAL :: Association_Status


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: ALL_Test
    LOGICAL :: Include_Tau_OPTRAN_Coeff 
    LOGICAL :: Include_TauCoeff_TraceGas 
    LOGICAL :: Include_TauCoeff_Subset



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
    ! The Skip_Tau_OPTRAN_Coeff optional argument
    ! -----------------------------------------

    ! -- Default is to include the Tau_OPTRAN_Coeff member
    ! -- in the association test....
    Include_Tau_OPTRAN_Coeff = .TRUE.

    ! ...unless the Skip_Tau_OPTRAN_Coeff argument is set.
    IF ( PRESENT( Skip_Tau_OPTRAN_Coeff ) ) THEN
      IF ( Skip_Tau_OPTRAN_Coeff == SET ) Include_Tau_OPTRAN_Coeff = .FALSE.
    END IF

    ! -----------------------------------------
    ! The Skip_TauCoeff_TraceGas optional argument
    ! -----------------------------------------

    ! -- Default is to include the TauCoeff_SARTA_TraceGas member
    ! -- in the association test....
    Include_TauCoeff_TraceGas = .TRUE.

    ! ...unless the Skip_TauCoeff_SARTA_TraceGas argument is set.
    IF ( PRESENT( Skip_TauCoeff_TraceGas ) ) THEN
      IF ( Skip_TauCoeff_TraceGas == SET ) Include_TauCoeff_TraceGas = .FALSE.
    END IF


    ! -----------------------------------------
    ! The Skip_TauCoeff_Subset optional argument
    ! -----------------------------------------

    ! -- Default is to include the TauCoeff_SARTA_Subset member
    ! -- in the association test....
    Include_TauCoeff_Subset = .TRUE.

    ! ...unless the Skip_TauCoeff_SARTA_Subset argument is set.
    IF ( PRESENT( Skip_TauCoeff_Subset ) ) THEN
      IF ( Skip_TauCoeff_Subset == SET ) Include_TauCoeff_Subset = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( TauCoeff%Sensor_Descriptor         ) .AND. &
           ASSOCIATED( TauCoeff%NCEP_Sensor_ID            ) .AND. &
           ASSOCIATED( TauCoeff%WMO_Satellite_ID          ) .AND. &
           ASSOCIATED( TauCoeff%WMO_Sensor_ID             ) .AND. &
           ASSOCIATED( TauCoeff%Sensor_Channel             ) .AND. &
           ASSOCIATED( TauCoeff%Channel_Subset_Index      ) .AND. &
           ASSOCIATED( TauCoeff%Channel_Subset_Position   ) .AND. &
           ASSOCIATED( TauCoeff%Channel_H2O_OPTRAN        ) .AND. &
           ASSOCIATED( TauCoeff%Channel_CO2_Perturbation  ) .AND. &
           ASSOCIATED( TauCoeff%Channel_SO2_Perturbation  ) .AND. &
           ASSOCIATED( TauCoeff%Channel_HNO3_Perturbation ) .AND. &
           ASSOCIATED( TauCoeff%Channel_N2O_Perturbation  ) .AND. &
           ASSOCIATED( TauCoeff%Channel_NON_LTE           ) .AND. &
           ASSOCIATED( TauCoeff%Standard_Level_Pressure   ) .AND. &
           ASSOCIATED( TauCoeff%Fix_Gases_Adjustment      ) .AND. &
           ASSOCIATED( TauCoeff%Down_F_Factor             ) .AND. &
           ASSOCIATED( TauCoeff%Tuning_Multiple           ) .AND. &
           ASSOCIATED( TauCoeff%Ref_Profile_Data          ) .AND. &
           ASSOCIATED( TauCoeff%Non_LTE_Coeff             )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( TauCoeff%Sensor_Descriptor         ) .OR. &
           ASSOCIATED( TauCoeff%NCEP_Sensor_ID            ) .OR. &
           ASSOCIATED( TauCoeff%WMO_Satellite_ID          ) .OR. &
           ASSOCIATED( TauCoeff%WMO_Sensor_ID             ) .OR. &
           ASSOCIATED( TauCoeff%Sensor_Channel             ) .OR. &
           ASSOCIATED( TauCoeff%Channel_Subset_Index      ) .OR. &
           ASSOCIATED( TauCoeff%Channel_Subset_Position   ) .OR. &
           ASSOCIATED( TauCoeff%Channel_H2O_OPTRAN        ) .OR. &
           ASSOCIATED( TauCoeff%Channel_CO2_Perturbation  ) .OR. &
           ASSOCIATED( TauCoeff%Channel_SO2_Perturbation  ) .OR. &
           ASSOCIATED( TauCoeff%Channel_HNO3_Perturbation ) .OR. &
           ASSOCIATED( TauCoeff%Channel_N2O_Perturbation  ) .OR. &
           ASSOCIATED( TauCoeff%Channel_NON_LTE           ) .OR. &
           ASSOCIATED( TauCoeff%Standard_Level_Pressure   ) .OR. &
           ASSOCIATED( TauCoeff%Fix_Gases_Adjustment      ) .OR. &
           ASSOCIATED( TauCoeff%Down_F_Factor             ) .OR. &
           ASSOCIATED( TauCoeff%Tuning_Multiple           ) .OR. &
           ASSOCIATED( TauCoeff%Ref_Profile_Data          ) .OR. &
           ASSOCIATED( TauCoeff%Non_LTE_Coeff             )       ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

    ! ---------------------------------------
    ! Test the members that MAY be associated
    ! ---------------------------------------

    ! -- Tau_OPTRAN_Coeff  
    IF ( Include_Tau_OPTRAN_Coeff ) THEN

      IF ( ALL_Test ) THEN

        IF ( Association_Status             .AND. &
             ASSOCIATED( TauCoeff%Tau_OPTRAN_Coeff )       ) THEN
          Association_Status = .TRUE.
        END IF

      ELSE

        IF ( Association_Status             .OR. &
             ASSOCIATED( TauCoeff%Tau_OPTRAN_Coeff )      ) THEN
          Association_Status = .TRUE.
        END IF

      END IF

    END IF

    ! -- TauCoeff_SARTA_TraceGas 
    IF ( Include_TauCoeff_TraceGas ) THEN

      IF ( ALL_Test ) THEN

        IF ( Association_Status             .AND. &
             ASSOCIATED( TauCoeff%TauCoeff_TraceGas )       ) THEN
          Association_Status = .TRUE.
        END IF

      ELSE

        IF ( Association_Status             .OR. &
             ASSOCIATED( TauCoeff%TauCoeff_TraceGas )      ) THEN
          Association_Status = .TRUE.
        END IF

      END IF

    END IF

    ! -- TauCoeff_SARTA_Subset
    IF ( Include_TauCoeff_Subset ) THEN

      IF ( ALL_Test ) THEN

        IF ( Association_Status             .AND. &
             ASSOCIATED( TauCoeff%TauCoeff_Subset )       ) THEN
          Association_Status = .TRUE.
        END IF

      ELSE

        IF ( Association_Status             .OR. &
             ASSOCIATED( TauCoeff%TauCoeff_Subset )      ) THEN
          Association_Status = .TRUE.
        END IF

      END IF

    END IF

  END FUNCTION Associated_TauCoeff_SARTA






  FUNCTION Destroy_TauCoeff_SARTA( TauCoeff,     &  ! Output
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
    TYPE( TauCoeff_SARTA_type ),    INTENT( IN OUT ) :: TauCoeff

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_TauCoeff_SARTA'


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

    IF ( Clear ) CALL Clear_TauCoeff_SARTA( TauCoeff )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_TauCoeff_SARTA( TauCoeff ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the Sensor Descriptor
    IF ( ASSOCIATED( TauCoeff%Sensor_Descriptor ) ) THEN

      DEALLOCATE( TauCoeff%Sensor_Descriptor, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA Sensor_Descriptor ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the NCEP Sensor ID
    IF ( ASSOCIATED( TauCoeff%NCEP_Sensor_ID ) ) THEN

      DEALLOCATE( TauCoeff%NCEP_Sensor_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA NCEP_Sensor_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the WMO Satellite ID
    IF ( ASSOCIATED( TauCoeff%WMO_Satellite_ID ) ) THEN

      DEALLOCATE( TauCoeff%WMO_Satellite_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA WMO_Satellite_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the WMO Sensor ID
    IF ( ASSOCIATED( TauCoeff%WMO_Sensor_ID ) ) THEN

      DEALLOCATE( TauCoeff%WMO_Sensor_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA WMO_Sensor_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channel index array
    IF ( ASSOCIATED( TauCoeff%Sensor_Channel ) ) THEN

      DEALLOCATE( TauCoeff%Sensor_Channel, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA Sensor_Channel ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channel subset index array
    IF ( ASSOCIATED( TauCoeff%Channel_Subset_Index ) ) THEN

      DEALLOCATE( TauCoeff%Channel_Subset_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA Channel_Subset_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channel subset position array
    IF ( ASSOCIATED( TauCoeff%Channel_Subset_Position ) ) THEN

      DEALLOCATE( TauCoeff%Channel_Subset_Position, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA Channel_Subset_Position ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for H2O using OPTRAN array
    IF ( ASSOCIATED( TauCoeff%Channel_H2O_OPTRAN ) ) THEN

      DEALLOCATE( TauCoeff%Channel_H2O_OPTRAN, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA Channel_H2O_OPTRAN ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for CO2 Perturbation array
    IF ( ASSOCIATED( TauCoeff%Channel_CO2_Perturbation ) ) THEN

      DEALLOCATE( TauCoeff%Channel_CO2_Perturbation, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA Channel_CO2_Perturbation ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for SO2 Perturbation array
    IF ( ASSOCIATED( TauCoeff%Channel_SO2_Perturbation ) ) THEN

      DEALLOCATE( TauCoeff%Channel_SO2_Perturbation, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA Channel_SO2_Perturbation ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for HNO3 Perturbation array
    IF ( ASSOCIATED( TauCoeff%Channel_HNO3_Perturbation ) ) THEN

      DEALLOCATE( TauCoeff%Channel_HNO3_Perturbation, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA Channel_HNO3_Perturbation ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for N2O Perturbation array
    IF ( ASSOCIATED( TauCoeff%Channel_N2O_Perturbation ) ) THEN

      DEALLOCATE( TauCoeff%Channel_N2O_Perturbation, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA Channel_N2O_Perturbation ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the channels for non-LTE effects array
    IF ( ASSOCIATED( TauCoeff%Channel_NON_LTE ) ) THEN

      DEALLOCATE( TauCoeff%Channel_NON_LTE, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA Channel_NON_LTE ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate standard levels pressure array
    IF ( ASSOCIATED( TauCoeff%Standard_Level_Pressure ) ) THEN

      DEALLOCATE( TauCoeff%Standard_Level_Pressure, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA Standard_Level_Pressure ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate fix gases adjustment array
    IF ( ASSOCIATED( TauCoeff%Fix_Gases_Adjustment ) ) THEN

      DEALLOCATE( TauCoeff%Fix_Gases_Adjustment, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA Fix_Gases_Adjustment ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Down_F_Factor
    IF ( ASSOCIATED( TauCoeff%Down_F_Factor ) ) THEN

      DEALLOCATE( TauCoeff%Down_F_Factor, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA Down_F_Factor ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Tuning_Multiple
    IF ( ASSOCIATED( TauCoeff%Tuning_Multiple ) ) THEN

      DEALLOCATE( TauCoeff%Tuning_Multiple, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA Tuning_Multiple ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate reference profile data array
    IF ( ASSOCIATED( TauCoeff%Ref_Profile_Data ) ) THEN

      DEALLOCATE( TauCoeff%Ref_Profile_Data, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA Ref_Profile_Data ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate non-LTE coefficients data array
    IF ( ASSOCIATED( TauCoeff%Non_LTE_Coeff ) ) THEN

      DEALLOCATE( TauCoeff%Non_LTE_Coeff, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA Non_LTE_Coeff ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -------------------------------------------------------------
    ! Deallocate the Tau_OPTRAN_Coeff structure pointer member
    ! -------------------------------------------------------------

    IF ( ASSOCIATED( TauCoeff%Tau_OPTRAN_Coeff ) ) THEN


      ! -- Destroy the Tau_OPTRAN_Coeff structure(s)
      Error_Status = Destroy_Tau_OPTRAN_Coeff( TauCoeff%Tau_OPTRAN_Coeff, &
                                              No_Clear = No_Clear, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error destroying Tau_OPTRAN_Coeff structure(s).', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF

    END IF

    ! -------------------------------------------------------------
    ! Deallocate the TauCoeff_SARTA_TraceGas structure array pointer member
    ! -------------------------------------------------------------

    IF ( ASSOCIATED( TauCoeff%TauCoeff_TraceGas ) ) THEN


      ! -- Destroy the TauCoeff_TraceGas structure(s)
      Error_Status = Destroy_TauCoeff_TraceGas( TauCoeff%TauCoeff_TraceGas, &
                                              No_Clear = No_Clear, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error destroying TauCoeff_SARTA_TraceGas structure(s).', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF


      ! -- Deallocate the array
      DEALLOCATE( TauCoeff%TauCoeff_TraceGas, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA_TraceGas", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -------------------------------------------------------------
    ! Deallocate the TauCoeff_SARTA_Subset structure array pointer member
    ! -------------------------------------------------------------

    IF ( ASSOCIATED( TauCoeff%TauCoeff_Subset ) ) THEN


      ! -- Destroy the TauCoeff_Subset structure(s)
      Error_Status = Destroy_TauCoeff_Subset( TauCoeff%TauCoeff_Subset, &
                                              No_Clear = No_Clear, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error destroying TauCoeff_Subset structure(s).', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF


      ! -- Deallocate the array
      DEALLOCATE( TauCoeff%TauCoeff_Subset, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA_Subset", &
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

    TauCoeff%n_Allocates = TauCoeff%n_Allocates - 1

    IF ( TauCoeff%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      TauCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_TauCoeff_SARTA






  FUNCTION Allocate_TauCoeff_SARTA( n_Layers,           &  ! Input
                              n_Channels,         &  ! Input
                              n_Tunings,          &  ! Input
			      n_F_Predictors,     &  ! Input
			      n_RefProfile_Items, &  ! Input
			      n_NONLTE_Predictors,&  ! Input
			      n_NONLTE_Channels,  &  ! Input
			      n_TraceGases,       &  ! Input
			      n_Subsets,          &  ! Input
                              TauCoeff,           &  ! Output
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
    TYPE( TauCoeff_SARTA_type ),    INTENT( IN OUT ) :: TauCoeff

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_TauCoeff_SARTA'


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
                            'Input TauCoeff_SARTA dimensions must all be > 0.', &
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

    IF ( Associated_TauCoeff_SARTA( TauCoeff, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_TauCoeff_SARTA( TauCoeff, &
                                       No_Clear = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating TauCoeff_SARTA pointer members.', &
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

    ALLOCATE( TauCoeff%Sensor_Descriptor( n_Channels ), &
              TauCoeff%NCEP_Sensor_ID( n_Channels ), &
              TauCoeff%WMO_Satellite_ID( n_Channels ), &
              TauCoeff%WMO_Sensor_ID( n_Channels ), &
              TauCoeff%Sensor_Channel( n_Channels ), &
              TauCoeff%Channel_Subset_Index( n_Channels ), &
              TauCoeff%Channel_Subset_Position( n_Channels ), &
              TauCoeff%Channel_H2O_OPTRAN( n_Channels ), &
              TauCoeff%Channel_CO2_Perturbation( n_Channels ), &
              TauCoeff%Channel_SO2_Perturbation( n_Channels ), &
              TauCoeff%Channel_HNO3_Perturbation( n_Channels ), &
              TauCoeff%Channel_N2O_Perturbation( n_Channels ), &
              TauCoeff%Channel_NON_LTE( n_Channels ), &
              TauCoeff%Standard_Level_Pressure( 0:n_Layers ), &
              TauCoeff%Fix_Gases_Adjustment( n_Layers ), &
              TauCoeff%Down_F_Factor( n_F_Predictors, n_Channels ), &
              TauCoeff%Tuning_Multiple( n_Tunings, n_Channels ), &
              TauCoeff%Ref_Profile_Data(n_RefProfile_Items, n_Layers ),&
              TauCoeff%Non_LTE_Coeff(n_NONLTE_Predictors, n_NONLTE_Channels ),&
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating TauCoeff_SARTA data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! ------------------------------
    ! The Tau_OPTRAN_Coeff structure 
    ! ------------------------------

    ! -- Allocate the individual structures
    ALLOCATE( TauCoeff%Tau_OPTRAN_Coeff, &
                STAT = Allocate_Status )
		
    IF ( Allocate_Status /= 0 ) THEN								   
      Error_Status = FAILURE									   
      WRITE( Message, '( "Error allocating Tau_OPTRAN_Coeff structure. STAT = ", i5 )' ) &  
    		      Allocate_Status								   
      CALL Display_Message( ROUTINE_NAME,    &  						   
    			    TRIM( Message ), &  						   
    			    Error_Status,    &  						   
    			    Message_Log = Message_Log ) 					   
      RETURN											   
    END IF											   
		
    


    ! ---------------------------------------
    ! The TauCoeff_SARTA_TraceGas structure array
    ! ---------------------------------------

    IF ( n_TraceGases > 0 ) THEN

      ! -- Allocate the structure array
      ALLOCATE( TauCoeff%TauCoeff_TraceGas( n_TraceGases ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error allocating TauCoeff_SARTA_TraceGas structure array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Allocate the individual structures


    END IF
    
    ! -----------------------------------
    ! The TauCoeff_SARTA_Subset structure array
    ! -----------------------------------

    IF ( n_Subsets > 0 ) THEN

      ! -- Allocate the structure array
      ALLOCATE( TauCoeff%TauCoeff_Subset( n_Subsets ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error allocating TauCoeff_SARTA Subset structure array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Allocate the individual structures


    END IF

    !#--------------------------------------------------------------------------#
    !#                        -- ASSIGN THE DIMENSIONS --                       #
    !#--------------------------------------------------------------------------#

    TauCoeff%n_Layers           = n_Layers
    TauCoeff%n_Channels         = n_Channels
    TauCoeff%n_Tunings          = n_Tunings 
    TauCoeff%n_F_Predictors     = n_F_Predictors
    TauCoeff%n_RefProfile_Items = n_RefProfile_Items
    TauCoeff%n_NONLTE_Predictors= n_NONLTE_Predictors
    TauCoeff%n_NONLTE_Channels  = n_NONLTE_Channels
    TauCoeff%n_TraceGases       = n_TraceGases 
    TauCoeff%n_Subsets          = n_Subsets

  
    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    TauCoeff%n_Allocates = TauCoeff%n_Allocates + 1

    IF ( TauCoeff%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      TauCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_TauCoeff_SARTA






  FUNCTION Assign_TauCoeff_SARTA( TauCoeff_in,   &  ! Input
                            TauCoeff_out,  &  ! Output
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
    TYPE( TauCoeff_SARTA_type ),    INTENT( IN )     :: TauCoeff_in

    ! -- Output
    TYPE( TauCoeff_SARTA_type ),    INTENT( IN OUT ) :: TauCoeff_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_TauCoeff_SARTA'



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
    ! BUT the TauCoeff_SARTA_TraceGas and/or 
    ! TauCoeff_SARTA_Subset pointer members may not be.
    ! ---------------------------------------

    IF ( .NOT. Associated_TauCoeff_SARTA( TauCoeff_in, &
                                    Skip_TauCoeff_TraceGas = SET ) ) THEN
      Error_Status = Destroy_TauCoeff_SARTA( TauCoeff_out, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN 
        CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauCoeff_SARTA pointer '//&
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

    TauCoeff_out%Release = TauCoeff_in%Release
    TauCoeff_out%Version = TauCoeff_in%Version
    TauCoeff_out%Algorithm_ID   = TauCoeff_in%Algorithm_ID  

    TauCoeff_out%n_Sensors = TauCoeff_in%n_Sensors


    ! -----------------
    ! Assign array data
    ! -----------------

    ! -- Allocate data arrays
    Error_Status = Allocate_TauCoeff_SARTA( TauCoeff_in%n_Layers,           &
                                      TauCoeff_in%n_Channels,         &	     
                                      TauCoeff_in%n_Tunings,	      &	     
                                      TauCoeff_in%n_F_Predictors,     &	     
                                      TauCoeff_in%n_RefProfile_Items, &	     
                                      TauCoeff_in%n_NONLTE_Predictors,&      
                                      TauCoeff_in%n_NONLTE_Channels,  &   
                                      TauCoeff_in%n_TraceGases,       &     
                                      TauCoeff_in%n_Subsets,          &
                                      TauCoeff_out,                   &
                                      Message_Log = Message_Log )   
                                     		  
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output TauCoeff_SARTA arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Copy array data
    TauCoeff_out%Sensor_Descriptor        = TauCoeff_in%Sensor_Descriptor
    TauCoeff_out%NCEP_Sensor_ID           = TauCoeff_in%NCEP_Sensor_ID
    TauCoeff_out%WMO_Satellite_ID         = TauCoeff_in%WMO_Satellite_ID
    TauCoeff_out%WMO_Sensor_ID            = TauCoeff_in%WMO_Sensor_ID
    TauCoeff_out%Sensor_Channel            = TauCoeff_in%Sensor_Channel 
    TauCoeff_out%Channel_Subset_Index     = TauCoeff_in%Channel_Subset_Index 
    TauCoeff_out%Channel_Subset_Position  = TauCoeff_in%Channel_Subset_Position  
    TauCoeff_out%Channel_H2O_OPTRAN       = TauCoeff_in%Channel_H2O_OPTRAN  
    TauCoeff_out%Channel_CO2_Perturbation = TauCoeff_in%Channel_CO2_Perturbation 
    TauCoeff_out%Channel_SO2_Perturbation = TauCoeff_in%Channel_SO2_Perturbation 
    TauCoeff_out%Channel_HNO3_Perturbation= TauCoeff_in%Channel_HNO3_Perturbation 
    TauCoeff_out%Channel_N2O_Perturbation = TauCoeff_in%Channel_N2O_Perturbation 
    TauCoeff_out%Channel_NON_LTE          = TauCoeff_in%Channel_NON_LTE
    TauCoeff_out%Standard_Level_Pressure  = TauCoeff_in%Standard_Level_Pressure
    TauCoeff_out%Fix_Gases_Adjustment     = TauCoeff_in%Fix_Gases_Adjustment 
    TauCoeff_out%Down_F_Factor            = TauCoeff_in%Down_F_Factor
    TauCoeff_out%Tuning_Multiple          = TauCoeff_in%Tuning_Multiple
    TauCoeff_out%Ref_Profile_Data         = TauCoeff_in%Ref_Profile_Data 
    TauCoeff_out%Non_LTE_Coeff            = TauCoeff_in%Non_LTE_Coeff

    ! ---------------------
    ! Assign structure data
    ! ---------------------

    ! -- Copy Tau_OPTRAN_Coeff structure
    Error_Status = Assign_Tau_OPTRAN_Coeff( TauCoeff_in%Tau_OPTRAN_Coeff ,  &  
    					    TauCoeff_out%Tau_OPTRAN_Coeff , &  
    					    Message_Log = Message_Log)         

    IF ( Error_Status /= SUCCESS ) THEN 				       
      CALL Display_Message( ROUTINE_NAME, &				       
    			    'Error copying Tau_OPTRAN_Coeff structure.', &     
    			    Error_Status, &				       
    			    Message_Log = Message_Log ) 		       
      RETURN								       
    END IF								       
 
    ! -- Copy TauCoeff_SARTA_TraceGas structure
    IF ( TauCoeff_in%n_TraceGases > 0 ) THEN

      Error_Status = Assign_TauCoeff_TraceGas( TauCoeff_in%TauCoeff_TraceGas,  &
                                               TauCoeff_out%TauCoeff_TraceGas, &
                                               Message_Log = Message_Log)

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error copying TauCoeff_SARTA_TraceGas structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF

    ! -- Copy TauCoeff_SARTA_Subset structure
    IF ( TauCoeff_in%n_Subsets > 0 ) THEN

      Error_Status = Assign_TauCoeff_Subset( TauCoeff_in%TauCoeff_Subset, &
                                             TauCoeff_out%TauCoeff_Subset, &
                                             Message_Log = Message_Log)

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error copying TauCoeff_SARTA_Subset structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF


  END FUNCTION Assign_TauCoeff_SARTA



  FUNCTION Check_TauCoeff_SARTA_Release( TauCoeff,     &  ! Input
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
    TYPE( TauCoeff_SARTA_type ),    INTENT( IN )  :: TauCoeff

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Check_TauCoeff_SARTA_Release'


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

    IF ( TauCoeff%Release < TAUCOEFF_SARTA_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A TauCoeff_SARTA data update is needed. ", &
                        &"TauCoeff_SARTA release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      TauCoeff%Release, TAUCOEFF_SARTA_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#               -- CHECK THAT THE RELEASE IS NOT TOO NEW --                #
    !#--------------------------------------------------------------------------#

    IF ( TauCoeff%Release > TAUCOEFF_SARTA_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A TauCoeff_SARTA software update is needed. ", &
                        &"TauCoeff_SARTA release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      TauCoeff%Release, TAUCOEFF_SARTA_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Check_TauCoeff_SARTA_Release





  SUBROUTINE Count_TauCoeff_SARTA_Sensors( TauCoeff,   &  ! In/Output
                                     Use_WMO_ID, &  ! Optional input
                                     RCS_Id      )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_SARTA_type ),    INTENT( INOUT ) :: TauCoeff

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )    :: Use_WMO_ID

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )   :: RCS_Id


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Use_NCEP_ID
    INTEGER :: l, j, n

    INTEGER, DIMENSION( TauCoeff%n_Channels ) :: idx



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENT --                 #
    !#--------------------------------------------------------------------------#

    Use_NCEP_ID = .TRUE.
    IF ( PRESENT( Use_WMO_ID ) ) THEN
      IF ( Use_WMO_ID == SET ) Use_NCEP_ID = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- INITIALISE INVALID RESULT VALUE --                  #
    !#--------------------------------------------------------------------------#

    TauCoeff%n_Sensors = INVALID



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
      IF ( .NOT. ASSOCIATED( TauCoeff%NCEP_Sensor_ID ) ) RETURN

      ! -- Check that all the values are valid 
      IF ( ANY( TauCoeff%NCEP_Sensor_ID == INVALID ) ) RETURN


      ! ---------------------------
      ! Initialise the sensor count
      ! ---------------------------

      TauCoeff%n_Sensors = 1


      ! ------------------
      ! Loop over channels
      ! ------------------

      DO l = 2, TauCoeff%n_Channels

        ! -- Only increment sensor count if the current channel's
        ! -- value has not been previously encountered
        IF ( ALL( TauCoeff%NCEP_Sensor_ID(1:l-1) /= TauCoeff%NCEP_Sensor_ID(l) ) ) THEN
          TauCoeff%n_Sensors = TauCoeff%n_Sensors + 1
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
      IF ( .NOT. ASSOCIATED( TauCoeff%WMO_Satellite_ID ) .OR. &
           .NOT. ASSOCIATED( TauCoeff%WMO_Sensor_ID    )      ) RETURN

      ! -- Check that all the values are valid 
      IF ( ANY( TauCoeff%WMO_Satellite_ID == INVALID ) .OR. &
           ANY( TauCoeff%WMO_Sensor_ID    == INVALID )      ) RETURN


      ! ---------------------------
      ! Initialise the sensor count
      ! ---------------------------

      TauCoeff%n_Sensors = 1


      ! ------------------
      ! Loop over channels
      ! ------------------

      l_Channel_Loop: DO l = 2, TauCoeff%n_Channels


        ! ------------------------------------------
        ! Count the number of channels with the SAME
        ! WMO SENSOR ID as the current channel
        ! ------------------------------------------

        n = COUNT( TauCoeff%WMO_Sensor_ID(1:l-1) == TauCoeff%WMO_Sensor_ID(l) )


        ! ----------------------------------------------
        ! How many channels have the same WMO SENSOR ID?
        ! ----------------------------------------------

        IF ( n == 0 ) THEN

          ! -- None. Increment the sensor count
          TauCoeff%n_Sensors = TauCoeff%n_Sensors + 1

        ELSE

          ! -- Some channels have the same SENSOR ID.
          ! -- Now get those corresponding array indices
          idx(1:n) = PACK( (/ ( j, j=1,l-1 ) /), &
                           TauCoeff%WMO_Sensor_ID(1:l-1) == TauCoeff%WMO_Sensor_ID(l) )

          ! -- If ALL of the previous channels' SATELLITE ID
          ! -- values are different from the current channel,
          ! -- then we have a different sensor so increment
          ! -- the sensor count.
          IF ( ALL( TauCoeff%WMO_Satellite_ID(idx(1:n)) /= TauCoeff%WMO_Satellite_ID(l) ) ) THEN
            TauCoeff%n_Sensors = TauCoeff%n_Sensors + 1
          END IF

        END IF

      END DO l_Channel_Loop

    END IF ID_Type

  END SUBROUTINE Count_TauCoeff_SARTA_Sensors






  SUBROUTINE Version_TauCoeff_SARTA( TauCoeff,     &  ! Input
                               Version_Info, &  ! Output
                               RCS_Id        )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_SARTA_type ),    INTENT( IN )  :: TauCoeff

    ! -- Output
    CHARACTER( * ),           INTENT( OUT ) :: Version_Info

    ! -- Optional output
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

    WRITE( Long_String,'( a,1x,"TauCoeff_SARTA RELEASE.VERSION: ", i2, ".", i2.2, 2x,&
                           &"Algorithm_ID=", i2, 2x, &
			   &"N_LAYERS=",i3,2x,&
                           &"N_CHANNELS=",i4,2x,&
			   &"N_SUBSETS=",i2,2x,&
			   &"N_F_PREDICTORS=",i2,2x,&
			   &"N_REFPROFILE_ITEMS=",i2,2x,&
                           &"N_SENSORS=",i2 )' ) &
                         ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                         TauCoeff%Release, TauCoeff%Version, &
                         TauCoeff%Algorithm_ID, &
			 TauCoeff%n_Layers, &
                         TauCoeff%n_Channels, &
			 TauCoeff%n_Subsets, &
                         TauCoeff%n_F_Predictors, &
			 TauCoeff%n_RefProfile_Items, &
			 TauCoeff%n_Sensors


    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Version_Info = Long_String(1:MIN( LEN( Version_Info ), LEN_TRIM( Long_String ) ))

  END SUBROUTINE Version_TauCoeff_SARTA

END MODULE TauCoeff_SARTA_Define



 
