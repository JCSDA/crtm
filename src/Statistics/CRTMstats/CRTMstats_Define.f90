!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTMstats_Define
!
! PURPOSE:
!       Module defining the CRTMstats data structure and containing
!       routines to manipulate it.
!       
! CATEGORY:
!       CRTM : CRTMstats
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTMstats_Define
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
!       Associated_CRTMstats:    Function to test the association status
!                                 of the pointer members of a CRTMstats
!                                 structure.
!
!       Destroy_CRTMstats:       Function to re-initialize an CRTMstats
!                                 structure.
!
!       Allocate_CRTMstats:      Function to allocate the pointer members
!                                 of an CRTMstats structure.
!
!       Assign_CRTMstats:        Function to copy an CRTMstats structure.
!
!       Concatenate_CRTMstats:   Function to concatenate two CRTMstats
!                                 structures along the channel dimension.
!
!       Compute_CRTMstats:       Function to compute the statistics for
!                                 a CRTMstats data structure.
!
!       Count_CRTMstats_Sensors: Subroutine to count the number of
!                                 different satellites/sensors in the
!                                 CRTMstats data structure.
!
!       Information_CRTMstats:   Subroutine to return a string containing
!                                 information about the CRTMstats data structure.
!
! DERIVED TYPES:
!       CRTMstats_type:  Definition of the public CRTMstats data structure.
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
!         n_Layers:            Number of atmospheric layers.
!                              "K" dimension.
!                              UNITS:     N/A
!                              TYPE:      INTEGER 
!                              DIMENSION: Scalar
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
!         Int_Water_Vapor:     Integrated Water Vapor
!                              UNITS:     g/cm^2
!                              TYPE:      REAL
!                              DIMENSION: Rank-1 (n_Profiles, M)
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
!         LBL_OD:              Array containing line-by-line Optical 
!                              depth calculations 
!                              UNITS:      None.
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-4 (K x L x I x M x J)
!                              ATTRIBUTES: POINTER
!
!         REG_OD:              Array containing regression Optical 
!                              depth calculations 
!                              UNITS:      None.
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-4 (K x L x I x M x J)
!                              ATTRIBUTES: POINTER
!
!         dOD:                 Array containing (REG-LBL) Optical 
!                              depth differences
!                              UNITS:      None.
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-4 (K x L x I x M x J)
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
!       Note that the CRTMstats_type is PUBLIC and its members are not
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
!M-
!------------------------------------------------------------------------------

MODULE CRTMstats_Define


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
  PUBLIC :: Associated_CRTMstats
  PUBLIC :: Destroy_CRTMstats
  PUBLIC :: Allocate_CRTMstats
  PUBLIC :: Assign_CRTMstats
  PUBLIC :: Concatenate_CRTMstats
  PUBLIC :: Compute_CRTMstats
  PUBLIC :: Count_CRTMstats_Sensors
  PUBLIC :: Information_CRTMstats



  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- CRTMstats invalid value
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


  ! -------------------------------
  ! CRTMstats data type definition
  ! -------------------------------

  TYPE, PUBLIC :: CRTMstats_type
    INTEGER :: n_Allocates = 0

    ! -- Profile ID descriptors
    CHARACTER( 16 ) :: LBL_Profile_ID_Tag = ' '
    CHARACTER( 16 ) :: REG_Profile_ID_Tag = ' '

    ! -- Dimensions
    INTEGER :: n_Layers        = 0 ! == K
    INTEGER :: n_Channels      = 0 ! == L
    INTEGER :: n_Angles        = 0 ! == I
    INTEGER :: n_Profiles      = 0 ! == M
    INTEGER :: n_Molecule_Sets = 0 ! == J

    ! -- The number of satellite/sensors
    INTEGER :: n_Sensors = 0

    ! -- The Integrated Water vapor
    REAL( fp_kind ), DIMENSION( : ), POINTER :: Int_Water_Vapor => NULL()  ! M  

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
    
    ! -- Optical Depth data
    REAL( fp_kind ), DIMENSION( :, :, :, :, : ), POINTER :: LBL_OD             => NULL()  ! K x L x I x M x J
    REAL( fp_kind ), DIMENSION( :, :, :, :, : ), POINTER :: REG_OD             => NULL()  ! K x L x I x M x J
    REAL( fp_kind ), DIMENSION( :, :, :, :, : ), POINTER :: dOD                => NULL()  ! K x L x I x M x J

    ! -- Transmittance data
    REAL( fp_kind ), DIMENSION( :, :, :, : ), POINTER :: LBL_Tau             => NULL()  ! L x I x M x J
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
  
  END TYPE CRTMstats_type


CONTAINS








  SUBROUTINE Clear_CRTMstats( CRTMstats )

    TYPE( CRTMstats_type ), INTENT( IN OUT ) :: CRTMstats

    CRTMstats%LBL_Profile_ID_Tag = ' '
    CRTMstats%REG_Profile_ID_Tag = ' '
    
    CRTMstats%n_Layers        = 0
    CRTMstats%n_Channels      = 0
    CRTMstats%n_Angles        = 0
    CRTMstats%n_Profiles      = 0
    CRTMstats%n_Molecule_Sets = 0

    CRTMstats%n_Sensors = 0

  END SUBROUTINE Clear_CRTMstats







  FUNCTION Associated_CRTMstats( CRTMstats, & ! Input
                                  ANY_Test )  & ! Optional input
                                RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTMstats_type ), INTENT( IN ) :: CRTMstats

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

      IF ( ASSOCIATED( CRTMstats%NCEP_Sensor_ID     ) .AND. &
           ASSOCIATED( CRTMstats%Int_Water_Vapor    ) .AND. &
           ASSOCIATED( CRTMstats%WMO_Satellite_ID   ) .AND. &
           ASSOCIATED( CRTMstats%WMO_Sensor_ID      ) .AND. &
           ASSOCIATED( CRTMstats%Sensor_Channel     ) .AND. &
           ASSOCIATED( CRTMstats%Frequency          ) .AND. &
           ASSOCIATED( CRTMstats%Angle              ) .AND. &
           ASSOCIATED( CRTMstats%Profile            ) .AND. &
           ASSOCIATED( CRTMstats%Molecule_Set       ) .AND. &
           ASSOCIATED( CRTMstats%LBL_OD             ) .AND. &
           ASSOCIATED( CRTMstats%REG_OD             ) .AND. &
           ASSOCIATED( CRTMstats%dOD                ) .AND. &
           ASSOCIATED( CRTMstats%LBL_Tau            ) .AND. &
           ASSOCIATED( CRTMstats%REG_Tau            ) .AND. &
           ASSOCIATED( CRTMstats%Mean_dTau          ) .AND. &
           ASSOCIATED( CRTMstats%RMS_dTau           ) .AND. &
           ASSOCIATED( CRTMstats%Mean_dTau_by_Angle ) .AND. &
           ASSOCIATED( CRTMstats%RMS_dTau_by_Angle  ) .AND. &
           ASSOCIATED( CRTMstats%LBL_BT             ) .AND. &
           ASSOCIATED( CRTMstats%REG_BT             ) .AND. &
           ASSOCIATED( CRTMstats%Mean_dBT           ) .AND. &
           ASSOCIATED( CRTMstats%RMS_dBT            ) .AND. &
           ASSOCIATED( CRTMstats%Mean_dBT_by_Angle  ) .AND. &
           ASSOCIATED( CRTMstats%RMS_dBT_by_Angle   )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( CRTMstats%NCEP_Sensor_ID     ) .OR. &
           ASSOCIATED( CRTMstats%Int_Water_Vapor    ) .OR. &
           ASSOCIATED( CRTMstats%WMO_Satellite_ID   ) .OR. &
           ASSOCIATED( CRTMstats%WMO_Sensor_ID      ) .OR. &
           ASSOCIATED( CRTMstats%Sensor_Channel     ) .OR. &
           ASSOCIATED( CRTMstats%Frequency          ) .OR. &
           ASSOCIATED( CRTMstats%Angle              ) .OR. &
           ASSOCIATED( CRTMstats%Profile            ) .OR. &
           ASSOCIATED( CRTMstats%Molecule_Set       ) .OR. &
           ASSOCIATED( CRTMstats%LBL_OD             ) .AND. &
           ASSOCIATED( CRTMstats%REG_OD             ) .AND. &
           ASSOCIATED( CRTMstats%dOD                ) .AND. &
           ASSOCIATED( CRTMstats%LBL_Tau            ) .OR. &
           ASSOCIATED( CRTMstats%REG_Tau            ) .OR. &
           ASSOCIATED( CRTMstats%Mean_dTau          ) .OR. &
           ASSOCIATED( CRTMstats%RMS_dTau           ) .OR. &
           ASSOCIATED( CRTMstats%Mean_dTau_by_Angle ) .OR. &
           ASSOCIATED( CRTMstats%RMS_dTau_by_Angle  ) .OR. &
           ASSOCIATED( CRTMstats%LBL_BT             ) .OR. &
           ASSOCIATED( CRTMstats%REG_BT             ) .OR. &
           ASSOCIATED( CRTMstats%Mean_dBT           ) .OR. &
           ASSOCIATED( CRTMstats%RMS_dBT            ) .OR. &
           ASSOCIATED( CRTMstats%Mean_dBT_by_Angle  ) .OR. &
           ASSOCIATED( CRTMstats%RMS_dBT_by_Angle   )       ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_CRTMstats






  FUNCTION Destroy_CRTMstats( CRTMstats,   &  ! Output
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
    TYPE( CRTMstats_type ),  INTENT( IN OUT ) :: CRTMstats

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_CRTMstats'


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

    IF ( Clear ) CALL Clear_CRTMstats( CRTMstats )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_CRTMstats( CRTMstats ) ) RETURN


    ! ------------------------------------------
    ! Deallocate the sensor/satellite ID members
    ! ------------------------------------------

    ! -- Deallocate the NCEP Sensor ID
    IF ( ASSOCIATED( CRTMstats%NCEP_Sensor_ID ) ) THEN

      DEALLOCATE( CRTMstats%NCEP_Sensor_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats NCEP_Sensor_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the WMO Satellite ID
    IF ( ASSOCIATED( CRTMstats%WMO_Satellite_ID ) ) THEN

      DEALLOCATE( CRTMstats%WMO_Satellite_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats WMO_Satellite_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the WMO Sensor ID
    IF ( ASSOCIATED( CRTMstats%WMO_Sensor_ID ) ) THEN

      DEALLOCATE( CRTMstats%WMO_Sensor_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats WMO_Sensor_ID ", &
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
    IF ( ASSOCIATED( CRTMstats%Sensor_Channel ) ) THEN

      DEALLOCATE( CRTMstats%Sensor_Channel, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats Sensor_Channel member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Channel central frequency
    IF ( ASSOCIATED( CRTMstats%Frequency ) ) THEN

      DEALLOCATE( CRTMstats%Frequency, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats Frequency member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Angle list
    IF ( ASSOCIATED( CRTMstats%Angle ) ) THEN

      DEALLOCATE( CRTMstats%Angle, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats Angle member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Profile list
    IF ( ASSOCIATED( CRTMstats%Profile ) ) THEN

      DEALLOCATE( CRTMstats%Profile, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats Profile member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Molecule set list
    IF ( ASSOCIATED( CRTMstats%Molecule_Set ) ) THEN

      DEALLOCATE( CRTMstats%Molecule_Set, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats Molecule_Set member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
    
    ! -- Integrated Water Vapor
    IF ( ASSOCIATED( CRTMstats%Int_Water_Vapor ) ) THEN

      DEALLOCATE( CRTMstats%Int_Water_Vapor, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats Int_Water_Vapor member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -----------------------------------------
    ! Deallocate the optical depth data members
    ! -----------------------------------------

    ! -- LBL Optical Depths
    IF ( ASSOCIATED( CRTMstats%LBL_OD ) ) THEN

      DEALLOCATE( CRTMstats%LBL_OD, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats LBL_OD member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Regression Optical Depths
    IF ( ASSOCIATED( CRTMstats%REG_OD ) ) THEN

      DEALLOCATE( CRTMstats%REG_OD, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats REG_OD member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF
    
    ! -- Regression - LBL Optical Depth difference
    IF ( ASSOCIATED( CRTMstats%dOD ) ) THEN

      DEALLOCATE( CRTMstats%dOD, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats dOD member. ", &
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
    IF ( ASSOCIATED( CRTMstats%LBL_Tau ) ) THEN

      DEALLOCATE( CRTMstats%LBL_Tau, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats LBL_Tau member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Regression TOA transmitance
    IF ( ASSOCIATED( CRTMstats%REG_Tau ) ) THEN

      DEALLOCATE( CRTMstats%REG_Tau, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats REG_Tau member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Mean LBL-REG transmittance difference
    IF ( ASSOCIATED( CRTMstats%Mean_dTau ) ) THEN

      DEALLOCATE( CRTMstats%Mean_dTau, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats Mean_dTau member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- RMS LBL-REG transmittance difference
    IF ( ASSOCIATED( CRTMstats%RMS_dTau ) ) THEN

      DEALLOCATE( CRTMstats%RMS_dTau, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats RMS_dTau member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Mean LBL-REG transmittance difference BY ANGLE
    IF ( ASSOCIATED( CRTMstats%Mean_dTau_by_Angle ) ) THEN

      DEALLOCATE( CRTMstats%Mean_dTau_by_Angle, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats Mean_dTau_by_Angle member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- RMS LBL-REG transmittance difference BY ANGLE
    IF ( ASSOCIATED( CRTMstats%RMS_dTau_by_Angle ) ) THEN

      DEALLOCATE( CRTMstats%RMS_dTau_by_Angle, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats RMS_dTau_by_Angle member. ", &
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
    IF ( ASSOCIATED( CRTMstats%LBL_BT ) ) THEN

      DEALLOCATE( CRTMstats%LBL_BT, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats LBL_BT member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Regression TOA brightness temperature
    IF ( ASSOCIATED( CRTMstats%REG_BT ) ) THEN

      DEALLOCATE( CRTMstats%REG_BT, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats REG_BT member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Mean LBL-REG brightness temperature difference
    IF ( ASSOCIATED( CRTMstats%Mean_dBT ) ) THEN

      DEALLOCATE( CRTMstats%Mean_dBT, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats Mean_dBT member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- RMS LBL-REG brightness temperature difference
    IF ( ASSOCIATED( CRTMstats%RMS_dBT ) ) THEN

      DEALLOCATE( CRTMstats%RMS_dBT, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats RMS_dBT member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Mean LBL-REG brightness temperature difference BY ANGLE
    IF ( ASSOCIATED( CRTMstats%Mean_dBT_by_Angle ) ) THEN

      DEALLOCATE( CRTMstats%Mean_dBT_by_Angle, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats Mean_dBT_by_Angle member. ", &
                          &"STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- RMS LBL-REG brightness temperature difference BY ANGLE
    IF ( ASSOCIATED( CRTMstats%RMS_dBT_by_Angle ) ) THEN

      DEALLOCATE( CRTMstats%RMS_dBT_by_Angle, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTMstats RMS_dBT_by_Angle member. ", &
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

    CRTMstats%n_Allocates = CRTMstats%n_Allocates - 1

    IF ( CRTMstats%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      CRTMstats%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_CRTMstats






  FUNCTION Allocate_CRTMstats(  n_Layers,        &  ! Input
                                n_Channels,      &  ! Input
                                n_Angles,        &  ! Input
                                n_Profiles,      &  ! Input
                                n_Molecule_Sets, &  ! Input
                                CRTMstats,      &  ! Output
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
    INTEGER,                  INTENT( IN )     :: n_Layers
    INTEGER,                  INTENT( IN )     :: n_Channels
    INTEGER,                  INTENT( IN )     :: n_Angles
    INTEGER,                  INTENT( IN )     :: n_Profiles
    INTEGER,                  INTENT( IN )     :: n_Molecule_Sets

    ! -- Output
    TYPE( CRTMstats_type ),  INTENT( IN OUT ) :: CRTMstats

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_CRTMstats'


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

    IF ( n_Layers        < 1 .OR. &
         n_Channels      < 1 .OR. &
         n_Angles        < 1 .OR. &
         n_Profiles      < 1 .OR. &
         n_Molecule_Sets < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input CRTMstats dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_CRTMstats( CRTMstats, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_CRTMstats( CRTMstats, &
                                         No_Clear = SET, &
                                         Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTMstats pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( CRTMstats%Int_Water_Vapor( n_Profiles ), &
    
              CRTMstats%NCEP_Sensor_ID( n_Channels ), &
              CRTMstats%WMO_Satellite_ID ( n_Channels ), &
              CRTMstats%WMO_Sensor_ID( n_Channels ), &

              CRTMstats%Sensor_Channel( n_Channels ), &     
              CRTMstats%Frequency( n_Channels ), &
              CRTMstats%Angle( n_Angles ), &
              CRTMstats%Profile( n_Profiles ), &
              CRTMstats%Molecule_Set( n_Molecule_Sets ), &
              
              CRTMstats%LBL_OD( n_Layers, n_Channels, n_Angles, n_Profiles, n_Molecule_Sets ), &
              CRTMstats%REG_OD( n_Layers, n_Channels, n_Angles, n_Profiles, n_Molecule_Sets ), &
              CRTMstats%dOD( n_Layers, n_Channels, n_Angles, n_Profiles, n_Molecule_Sets ),  &

              CRTMstats%LBL_Tau( n_Channels, n_Angles, n_Profiles, n_Molecule_Sets ), & 
              CRTMstats%REG_Tau( n_Channels, n_Angles, n_Profiles, n_Molecule_Sets ), & 
              CRTMstats%Mean_dTau( n_Channels, n_Molecule_Sets ), & 
              CRTMstats%RMS_dTau( n_Channels, n_Molecule_Sets ), & 
              CRTMstats%Mean_dTau_by_Angle( n_Channels, n_Angles, n_Molecule_Sets ), & 
              CRTMstats%RMS_dTau_by_Angle(  n_Channels, n_Angles, n_Molecule_Sets ), & 

              CRTMstats%LBL_BT( n_Channels, n_Angles, n_Profiles, n_Molecule_Sets ), & 
              CRTMstats%REG_BT( n_Channels, n_Angles, n_Profiles, n_Molecule_Sets ), & 
              CRTMstats%Mean_dBT( n_Channels, n_Molecule_Sets ), & 
              CRTMstats%RMS_dBT( n_Channels, n_Molecule_Sets ), & 
              CRTMstats%Mean_dBT_by_Angle( n_Channels, n_Angles, n_Molecule_Sets ), & 
              CRTMstats%RMS_dBT_by_Angle(  n_Channels, n_Angles, n_Molecule_Sets ), & 

              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating CRTMstats data arrays. STAT = ", i5 )' ) &
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

    CRTMstats%n_Layers        = n_Layers
    CRTMstats%n_Channels      = n_Channels
    CRTMstats%n_Angles        = n_Angles
    CRTMstats%n_Profiles      = n_Profiles
    CRTMstats%n_Molecule_Sets = n_Molecule_Sets



    !#--------------------------------------------------------------------------#
    !#          -- FILL THE POINTER MEMBERS WITH INVALID VALUES --              #
    !#--------------------------------------------------------------------------#
    CRTMstats%Int_Water_Vapor  = INVALID
   
    CRTMstats%NCEP_Sensor_ID   = INVALID
    CRTMstats%WMO_Satellite_ID = INVALID
    CRTMstats%WMO_Sensor_ID    = INVALID

    CRTMstats%Sensor_Channel = INVALID
    CRTMstats%Frequency      = REAL( INVALID, fp_kind )
    CRTMstats%Angle          = REAL( INVALID, fp_kind )
    CRTMstats%Profile        = INVALID
    CRTMstats%Molecule_Set   = INVALID
    
    CRTMstats%LBL_OD             = REAL( INVALID, fp_kind )
    CRTMstats%REG_OD             = REAL( INVALID, fp_kind )
    CRTMstats%dOD                = REAL( INVALID, fp_kind )

    CRTMstats%LBL_Tau            = REAL( INVALID, fp_kind )
    CRTMstats%REG_Tau            = REAL( INVALID, fp_kind )
    CRTMstats%Mean_dTau          = REAL( INVALID, fp_kind )
    CRTMstats%RMS_dTau           = REAL( INVALID, fp_kind )
    CRTMstats%Mean_dTau_by_Angle = REAL( INVALID, fp_kind )
    CRTMstats%RMS_dTau_by_Angle  = REAL( INVALID, fp_kind )

    CRTMstats%LBL_BT             = REAL( INVALID, fp_kind )
    CRTMstats%REG_BT             = REAL( INVALID, fp_kind )
    CRTMstats%Mean_dBT           = REAL( INVALID, fp_kind )
    CRTMstats%RMS_dBT            = REAL( INVALID, fp_kind )
    CRTMstats%Mean_dBT_by_Angle  = REAL( INVALID, fp_kind )
    CRTMstats%RMS_dBT_by_Angle   = REAL( INVALID, fp_kind )



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    CRTMstats%n_Allocates = CRTMstats%n_Allocates + 1

    IF ( CRTMstats%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      CRTMstats%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_CRTMstats






  FUNCTION Assign_CRTMstats( CRTMstats_in,  &  ! Input
                              CRTMstats_out, &  ! Output
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
    TYPE( CRTMstats_type ),  INTENT( IN )     :: CRTMstats_in

    ! -- Output
    TYPE( CRTMstats_type ),  INTENT( IN OUT ) :: CRTMstats_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_CRTMstats'



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

    IF ( .NOT. Associated_CRTMstats( CRTMstats_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT CRTMstats pointer '//&
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

    CRTMstats_out%LBL_Profile_ID_Tag = CRTMstats_in%LBL_Profile_ID_Tag
    CRTMstats_out%REG_Profile_ID_Tag = CRTMstats_in%REG_Profile_ID_Tag

    CRTMstats_out%n_Sensors = CRTMstats_in%n_Sensors


    ! -----------------
    ! Assign array data
    ! -----------------

    ! -- Allocate data arrays
    Error_Status = Allocate_CRTMstats( CRTMstats_in%n_Layers,        & 
                                       CRTMstats_in%n_Channels,      &
                                       CRTMstats_in%n_Angles,        &
                                       CRTMstats_in%n_Profiles,      &
                                       CRTMstats_in%n_Molecule_Sets, &
                                       CRTMstats_out,                &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output CRTMstats arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Copy array data
    CRTMstats_out%Int_Water_Vapor  = CRTMstats_in%Int_Water_Vapor
    
    CRTMstats_out%NCEP_Sensor_ID   = CRTMstats_in%NCEP_Sensor_ID
    CRTMstats_out%WMO_Satellite_ID = CRTMstats_in%WMO_Satellite_ID
    CRTMstats_out%WMO_Sensor_ID    = CRTMstats_in%WMO_Sensor_ID

    CRTMstats_out%Sensor_Channel = CRTMstats_in%Sensor_Channel
    CRTMstats_out%Frequency      = CRTMstats_in%Frequency
    CRTMstats_out%Angle          = CRTMstats_in%Angle
    CRTMstats_out%Profile        = CRTMstats_in%Profile
    CRTMstats_out%Molecule_Set   = CRTMstats_in%Molecule_Set
    
    CRTMstats_out%LBL_OD             = CRTMstats_in%LBL_OD           
    CRTMstats_out%REG_OD             = CRTMstats_in%REG_OD           
    CRTMstats_out%dOD                = CRTMstats_in%dOD

    CRTMstats_out%LBL_Tau            = CRTMstats_in%LBL_Tau           
    CRTMstats_out%REG_Tau            = CRTMstats_in%REG_Tau           
    CRTMstats_out%Mean_dTau          = CRTMstats_in%Mean_dTau         
    CRTMstats_out%RMS_dTau           = CRTMstats_in%RMS_dTau          
    CRTMstats_out%Mean_dTau_by_Angle = CRTMstats_in%Mean_dTau_by_Angle
    CRTMstats_out%RMS_dTau_by_Angle  = CRTMstats_in%RMS_dTau_by_Angle 

    CRTMstats_out%LBL_BT            = CRTMstats_in%LBL_BT            
    CRTMstats_out%REG_BT            = CRTMstats_in%REG_BT            
    CRTMstats_out%Mean_dBT          = CRTMstats_in%Mean_dBT          
    CRTMstats_out%RMS_dBT           = CRTMstats_in%RMS_dBT           
    CRTMstats_out%Mean_dBT_by_Angle = CRTMstats_in%Mean_dBT_by_Angle 
    CRTMstats_out%RMS_dBT_by_Angle  = CRTMstats_in%RMS_dBT_by_Angle  

  END FUNCTION Assign_CRTMstats






  FUNCTION Concatenate_CRTMstats( CRTMstats1,  &  ! Input/Output
                                   CRTMstats2,  &  ! Input
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
    TYPE( CRTMstats_type ),  INTENT( IN OUT )  :: CRTMstats1
    TYPE( CRTMstats_type ),  INTENT( IN )      :: CRTMstats2

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Concatenate_CRTMstats'
    INTEGER :: n_Layers = 100


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: n_Channels, l1, l2, n_Profiles

    TYPE( CRTMstats_type ) :: CRTMstats_Tmp



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

    IF ( .NOT. Associated_CRTMstats( CRTMstats1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT CRTMstats1 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------
    ! The second structure
    ! --------------------

    IF ( .NOT. Associated_CRTMstats( CRTMstats2 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT CRTMstats2 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- CHECK THE NON-CHANNEL DIMENSIONS --                  #
    !#--------------------------------------------------------------------------#

    IF ( CRTMstats1%n_Angles         /= CRTMstats2%n_Angles        .OR. &
         CRTMstats1%n_Profiles       /= CRTMstats2%n_Profiles      .OR. &
         CRTMstats1%n_Molecule_Sets  /= CRTMstats2%n_Molecule_Sets      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Non-channel CRTMstats dimensions are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Set the number of profiles
    n_Profiles = CRTMstats2%n_Profiles
    !#--------------------------------------------------------------------------#
    !#                -- COPY FIRST INPUT CRTMstats STRUCTURE --               #
    !#--------------------------------------------------------------------------#

    Error_Status = Assign_CRTMstats( CRTMstats1, CRTMstats_Tmp, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error copying CRTMstats1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
   


    !#--------------------------------------------------------------------------#
    !#             -- REALLOCATE FIRST INPUT CRTMstats STRUCTURE --            #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Destroy it
    ! ----------

    Error_Status = Destroy_CRTMstats( CRTMstats1, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying CRTMstats1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------
    ! Re-Allocate it
    ! --------------

    ! -- Set the total number of channels
    n_Channels = CRTMstats_Tmp%n_Channels + CRTMstats2%n_Channels

    ! -- Perform the allocation
    Error_Status = Allocate_CRTMstats(  n_Layers, &
                                        n_Channels, &
                                        CRTMstats_Tmp%n_Angles, &
                                        CRTMstats_Tmp%n_Profiles, &
                                        CRTMstats_Tmp%n_Molecule_Sets, &
                                        CRTMstats1, &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reallocating CRTMstats1 structure.', &
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

    CRTMstats1%LBL_Profile_ID_Tag = CRTMstats_Tmp%LBL_Profile_ID_Tag
    CRTMstats1%REG_Profile_ID_Tag = CRTMstats_Tmp%REG_Profile_ID_Tag


    ! ---------------------------------
    ! Assign the non-channel array data
    ! ---------------------------------

    CRTMstats1%Angle        = CRTMstats_Tmp%Angle
    CRTMstats1%Profile      = CRTMstats_Tmp%Profile
    CRTMstats1%Molecule_Set = CRTMstats_Tmp%Molecule_Set


    ! ------------------------------
    ! Concatenate channel array data
    ! ------------------------------

    ! -- The first part
    l1 = 1
    l2 = CRTMstats_Tmp%n_Channels

    CRTMstats1%NCEP_Sensor_ID(l1:l2)   = CRTMstats_Tmp%NCEP_Sensor_ID
    CRTMstats1%WMO_Satellite_ID(l1:l2) = CRTMstats_Tmp%WMO_Satellite_ID
    CRTMstats1%WMO_Sensor_ID(l1:l2)    = CRTMstats_Tmp%WMO_Sensor_ID

    CRTMstats1%Sensor_Channel(l1:l2) = CRTMstats_Tmp%Sensor_Channel
    CRTMstats1%Frequency(l1:l2)      = CRTMstats_Tmp%Frequency

    CRTMstats1%LBL_Tau(l1:l2,:,:,:)          = CRTMstats_Tmp%LBL_Tau
    CRTMstats1%REG_Tau(l1:l2,:,:,:)          = CRTMstats_Tmp%REG_Tau
    CRTMstats1%Mean_dTau(l1:l2,:)            = CRTMstats_Tmp%Mean_dTau
    CRTMstats1% RMS_dTau(l1:l2,:)            = CRTMstats_Tmp% RMS_dTau
    CRTMstats1%Mean_dTau_by_Angle(l1:l2,:,:) = CRTMstats_Tmp%Mean_dTau_by_Angle
    CRTMstats1% RMS_dTau_by_Angle(l1:l2,:,:) = CRTMstats_Tmp% RMS_dTau_by_Angle

    CRTMstats1%LBL_BT(l1:l2,:,:,:)          = CRTMstats_Tmp%LBL_BT
    CRTMstats1%REG_BT(l1:l2,:,:,:)          = CRTMstats_Tmp%REG_BT
    CRTMstats1%Mean_dBT(l1:l2,:)            = CRTMstats_Tmp%Mean_dBT
    CRTMstats1% RMS_dBT(l1:l2,:)            = CRTMstats_Tmp% RMS_dBT
    CRTMstats1%Mean_dBT_by_Angle(l1:l2,:,:) = CRTMstats_Tmp%Mean_dBT_by_Angle
    CRTMstats1% RMS_dBT_by_Angle(l1:l2,:,:) = CRTMstats_Tmp% RMS_dBT_by_Angle


    ! -- The second part
    l1 = l2 + 1
    l2 = n_Channels

    CRTMstats1%NCEP_Sensor_ID(l1:l2)   = CRTMstats2%NCEP_Sensor_ID
    CRTMstats1%WMO_Satellite_ID(l1:l2) = CRTMstats2%WMO_Satellite_ID
    CRTMstats1%WMO_Sensor_ID(l1:l2)    = CRTMstats2%WMO_Sensor_ID

    CRTMstats1%Sensor_Channel(l1:l2) = CRTMstats2%Sensor_Channel
    CRTMstats1%Frequency(l1:l2)      = CRTMstats2%Frequency

    CRTMstats1%LBL_Tau(l1:l2,:,:,:)          = CRTMstats2%LBL_Tau
    CRTMstats1%REG_Tau(l1:l2,:,:,:)          = CRTMstats2%REG_Tau
    CRTMstats1%Mean_dTau(l1:l2,:)            = CRTMstats2%Mean_dTau
    CRTMstats1%RMS_dTau(l1:l2,:)             = CRTMstats2%RMS_dTau
    CRTMstats1%Mean_dTau_by_Angle(l1:l2,:,:) = CRTMstats2%Mean_dTau_by_Angle
    CRTMstats1%RMS_dTau_by_Angle(l1:l2,:,:)  = CRTMstats2%RMS_dTau_by_Angle

    CRTMstats1%LBL_BT(l1:l2,:,:,:)          = CRTMstats2%LBL_BT
    CRTMstats1%REG_BT(l1:l2,:,:,:)          = CRTMstats2%REG_BT
    CRTMstats1%Mean_dBT(l1:l2,:)            = CRTMstats2%Mean_dBT
    CRTMstats1%RMS_dBT(l1:l2,:)             = CRTMstats2%RMS_dBT
    CRTMstats1%Mean_dBT_by_Angle(l1:l2,:,:) = CRTMstats2%Mean_dBT_by_Angle
    CRTMstats1%RMS_dBT_by_Angle(l1:l2,:,:)  = CRTMstats2%RMS_dBT_by_Angle



    !#--------------------------------------------------------------------------#
    !#                    -- COUNT THE NUMBER OF SENSORS --                     #
    !#--------------------------------------------------------------------------#

    CALL Count_CRTMstats_Sensors( CRTMstats1, Use_WMO_Id = SET )



    !#--------------------------------------------------------------------------#
    !#           -- DEALLOCATE THE TEMPORARY CRTMstats STRUCTURE --            #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_CRTMstats( CRTMstats_Tmp, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying CRTMstats_Tmp structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Concatenate_CRTMstats






  FUNCTION Compute_CRTMstats( CRTMstats,   &  ! In/Output
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
    TYPE( CRTMstats_type ),  INTENT( IN OUT ) :: CRTMstats

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_CRTMstats'


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

    IF ( .NOT. Associated_CRTMstats( CRTMstats ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT CRTMstats pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- CALCULATE THE STATS --                         #
    !#--------------------------------------------------------------------------#

    DO j = 1, CRTMstats%n_Molecule_Sets


      ! ----------------------------------
      ! Sum the angle dependent quantities
      ! ----------------------------------

      DO i = 1, CRTMstats%n_Angles

        DO l = 1, CRTMstats%n_Channels

          ! -- The transmittances by angle
          CRTMstats%Mean_dTau_by_Angle(l,i,j) = SUM( CRTMstats%LBL_Tau(l,i,:,j) - &
                                                      CRTMstats%REG_Tau(l,i,:,j)   )
          CRTMstats%RMS_dTau_by_Angle(l,i,j)  = SUM( (CRTMstats%LBL_Tau(l,i,:,j) - &
                                                       CRTMstats%REG_Tau(l,i,:,j)   )**2 )

          ! -- The brightness temperatures by angle
          CRTMstats%Mean_dBT_by_Angle(l,i,j) = SUM( CRTMstats%LBL_BT(l,i,:,j) - &
                                                     CRTMstats%REG_BT(l,i,:,j)   )
          CRTMstats%RMS_dBT_by_Angle(l,i,j)  = SUM( (CRTMstats%LBL_BT(l,i,:,j) - &
                                                      CRTMstats%REG_BT(l,i,:,j)   )**2 )
        END DO

      END DO


      ! ------------------------------------
      ! Sum the angle independent quantities
      ! ------------------------------------

      DO l = 1, CRTMstats%n_Channels

        ! -- The transmittances
        CRTMstats%Mean_dTau(l,j) = SUM( CRTMstats%Mean_dTau_by_Angle(l,:,j) )
        CRTMstats%RMS_dTau(l,j)  = SUM( CRTMstats%RMS_dTau_by_Angle(l,:,j)  )

        ! -- The brightness temperatures
        CRTMstats%Mean_dBT(l,j) = SUM( CRTMstats%Mean_dBT_by_Angle(l,:,j) )
        CRTMstats%RMS_dBT(l,j)  = SUM( CRTMstats%RMS_dBT_by_Angle(l,:,j)  )

      END DO

    END DO


    ! ------------------------
    ! Compute the final values
    ! ------------------------

    ! -- The divisors
    rn_Profiles        = REAL( CRTMstats%n_Profiles, fp_kind )
    rn_Angles_Profiles = REAL( CRTMstats%n_Angles*CRTMstats%n_Profiles, fp_kind )

    ! -- The transmittance statistics
    CRTMstats%Mean_dTau_by_Angle = CRTMstats%Mean_dTau_by_Angle / rn_Profiles
    CRTMstats%RMS_dTau_by_Angle  = SQRT( CRTMstats%RMS_dTau_by_Angle / rn_Profiles )

    CRTMstats%Mean_dTau = CRTMstats%Mean_dTau / rn_Angles_Profiles
    CRTMstats%RMS_dTau  = SQRT( CRTMstats%RMS_dTau / rn_Angles_Profiles )

    ! -- The brightness temperature statistics
    CRTMstats%Mean_dBT_by_Angle = CRTMstats%Mean_dBT_by_Angle / rn_Profiles
    CRTMstats%RMS_dBT_by_Angle  = SQRT( CRTMstats%RMS_dBT_by_Angle / rn_Profiles )

    CRTMstats%Mean_dBT = CRTMstats%Mean_dBT / rn_Angles_Profiles
    CRTMstats%RMS_dBT  = SQRT( CRTMstats%RMS_dBT / rn_Angles_Profiles )

  END FUNCTION Compute_CRTMstats






  SUBROUTINE Count_CRTMstats_Sensors( CRTMstats, &  ! In/Output
                                       Use_WMO_ID, &  ! Optional input
                                       RCS_Id      )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTMstats_type ),  INTENT( IN OUT ) :: CRTMstats

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: Use_WMO_ID

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Use_NCEP_ID
    INTEGER :: l, j, n

    INTEGER, DIMENSION( CRTMstats%n_Channels ) :: idx



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

    CRTMstats%n_Sensors = INVALID



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
      IF ( .NOT. ASSOCIATED( CRTMstats%NCEP_Sensor_ID ) ) RETURN

      ! -- Check that all the values are valid 
      IF ( ANY( CRTMstats%NCEP_Sensor_ID == INVALID ) ) RETURN


      ! ---------------------------
      ! Initialise the sensor count
      ! ---------------------------

      CRTMstats%n_Sensors = 1


      ! ------------------
      ! Loop over channels
      ! ------------------

      DO l = 2, CRTMstats%n_Channels

        ! -- Only increment sensor count if the current channel's
        ! -- value has not been previously encountered
        IF ( ALL( CRTMstats%NCEP_Sensor_ID(1:l-1) /= CRTMstats%NCEP_Sensor_ID(l) ) ) THEN
          CRTMstats%n_Sensors = CRTMstats%n_Sensors + 1
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
      IF ( .NOT. ASSOCIATED( CRTMstats%WMO_Satellite_ID ) .OR. &
           .NOT. ASSOCIATED( CRTMstats%WMO_Sensor_ID    )      ) RETURN

      ! -- Check that all the values are valid 
      IF ( ANY( CRTMstats%WMO_Satellite_ID == INVALID ) .OR. &
           ANY( CRTMstats%WMO_Sensor_ID    == INVALID )      ) RETURN


      ! ---------------------------
      ! Initialise the sensor count
      ! ---------------------------

      CRTMstats%n_Sensors = 1


      ! ------------------
      ! Loop over channels
      ! ------------------

      l_Channel_loop: DO l = 2, CRTMstats%n_Channels


        ! ------------------------------------------
        ! Count the number of channels with the SAME
        ! WMO SENSOR ID as the current channel
        ! ------------------------------------------

        n = COUNT( CRTMstats%WMO_Sensor_ID(1:l-1) == CRTMstats%WMO_Sensor_ID(l) )


        ! ----------------------------------------------
        ! How many channels have the same WMO SENSOR ID?
        ! ----------------------------------------------

        IF ( n == 0 ) THEN

          ! -- None. Increment the sensor count
          CRTMstats%n_Sensors = CRTMstats%n_Sensors + 1

        ELSE

          ! -- Some channels have the same SENSOR ID.
          ! -- Now get those corresponding array indices
          idx(1:n) = PACK( (/ ( j, j=1,l-1 ) /), &
                           CRTMstats%WMO_Sensor_ID(1:l-1) == CRTMstats%WMO_Sensor_ID(l) )

          ! -- If ALL of the previous channels' SATELLITE ID
          ! -- values are different from the current channel,
          ! -- then we have a different sensor so increment
          ! -- the sensor count.
          IF ( ALL( CRTMstats%WMO_Satellite_ID(idx(1:n)) /= CRTMstats%WMO_Satellite_ID(l) ) ) THEN
            CRTMstats%n_Sensors = CRTMstats%n_Sensors + 1
          END IF

        END IF

      END DO l_Channel_loop

    END IF ID_Type

  END SUBROUTINE Count_CRTMstats_Sensors






  SUBROUTINE Information_CRTMstats( CRTMstats,  &  ! Input
                                     Information, &  ! Output
                                     RCS_Id       )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTMstats_type ),    INTENT( IN )  :: CRTMstats

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

    WRITE( Long_String, '( a,1x,"CRTMstats: ", &
                           &"LBL Profile set- ", a, &
                           &"; REG Profile set- ", a, a, &
                           &"N_LAYERS=",i4,2x,&
                           &"N_CHANNELS=",i4,2x,&
                           &"N_ANGLES=",i1,2x,&
                           &"N_PROFILES=",i3,2x,&
                           &"N_MOLECULE_SETS=",i2 )' ) &
                         ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                         TRIM( CRTMstats%LBL_Profile_ID_Tag ), &
                         TRIM( CRTMstats%REG_Profile_ID_Tag ), &
                         ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                         CRTMstats%n_Layers,  &
                         CRTMstats%n_Channels, &
                         CRTMstats%n_Angles, &
                         CRTMstats%n_Profiles, &
                         CRTMstats%n_Molecule_Sets


    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Information = Long_String(1:MIN( LEN( Information ), LEN_TRIM( Long_String ) ))

  END SUBROUTINE Information_CRTMstats

END MODULE CRTMstats_Define


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/11/22 16:09:30 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTMstats_Define.f90,v $
! Revision 1.2  2006/11/22 16:09:30  dgroff
! This file is needed to generate the CRTM stats netcdf files.
! (This defines the inputs for generating the netcdf stats files)
!
! Revision 1.6  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.5  2005/01/07 18:44:20  paulv
! - Category change to CRTM.
!
! Revision 1.4  2005/01/06 19:01:32  paulv
! - Upgraded to Fortran-95
!
! Revision 1.3  2004/02/13 17:18:37  paulv
! - Altered the sensor/satellite IDs from scalars to arrays to allow for
!   more than one type of sensor/satellite in the data (e.g. if a combined
!   RTM run os performed.) All the routines affected were changed to reflect
!   the change.
! - Added the n_Sensors scalar to the CRTMstats data structure.
! - Added the Compute_CRTMstats_Sensors to determine the number of sensors.
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
