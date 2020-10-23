!------------------------------------------------------------------------------
!M+
! NAME:
!       ODCAPS_Predictor_Define
!
! PURPOSE:
!       Module defining the Optical Depth Combining Absorber and Pressure Space 
!       (ODCAPS) Predictor structure and containing routines to manipulate it.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE ODCAPS_Predictor_Define
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
!       Associated_Predictor:  Function to test the association status
!                                       of the pointer members of an Predictor
!                                       structure.
!
!       Destroy_Predictor:     Function to re-initialize a
!                                       Predictor structure.
!
!       Allocate_Predictor:    Function to allocate the pointer
!                                       members of a Predictor
!                                       structure.
!
!       Assign_Predictor:      Function to copy a valid 
!                                       Predictor structure.
!
! DERIVED TYPES:
!       Predictor_type
!       -----------------------
!         Definition of the CRTM gaseous absorption data structure.
!         Fields are:
!
!         n_Absorbers:      Number of absorbing species.
!                           "J" dimension.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!         Other fields:     To hold ODCAPS predictor intermediate results, which
!                           are not by non-ODCAPS modules.
!
!       *!IMPORTANT!*
!       -------------
!       Note that the Predictor_type is PUBLIC and its members are
!       not encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user destroy,
!       allocate, and assign the structure using only the routines
!       in this module where possible to eliminate -- or at least
!       minimise -- the possibility of memory leakage since most
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
!       Written by:     Yong Chen, CSU/CIRA 24-May-2006
!                       Yong.Chen@noaa.gov
!
!  Copyright (C) 2006 Yong Chen
!
!M-
!------------------------------------------------------------------------------

MODULE ODCAPS_Predictor_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds,      ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message, Warning
!  USE CRTM_Parameters  

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE
  ! Predictor data structure definition
  PUBLIC :: Predictor_type

  ! -- Definition functions
  PUBLIC :: Associated_Predictor
  PUBLIC :: Destroy_Predictor
  PUBLIC :: Allocate_Predictor
  PUBLIC :: Assign_Predictor
  PUBLIC :: Zero_Predictor 


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- Literal constants
  REAL( fp ), PRIVATE, PARAMETER :: ZERO = 0.0_fp
  INTEGER, PRIVATE,PARAMETER :: IP_INIT = -1

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- The maximum number of layers for ODCAPS 
  INTEGER, PUBLIC, PARAMETER :: MAX_N_ODCAPS_LAYERS = 100   
  
  ! -- The maximum number of layers for water optran
  INTEGER, PUBLIC, PARAMETER :: MAX_N_WATER_OPTRAN_LAYERS = 300   
   
  ! -- The maximum number of total predictors for Tau_OPTRAN_Coeff
  INTEGER, PUBLIC, PARAMETER :: MAX_N_WATER_OPTRAN_PREDICTORS = 9   

  ! -- The maximum number of absorbers for ODCAPS  
  INTEGER, PUBLIC, PARAMETER :: MAX_N_ABSORBERS_ODCAPS = 8

  ! -- The maximum number of subset  
  INTEGER, PUBLIC, PARAMETER :: MAX_N_SUBSETS = 7

  ! -- The maximum number of subset for SUN  
  INTEGER, PUBLIC, PARAMETER :: MAX_N_SUBSETS_SUN = 4
  
  ! -- The maximum number of total predictors for TauCoeff_Subset
  INTEGER, PUBLIC, PARAMETER :: MAX_N_SUBSET_TOTAL_PREDICTORS = 45   ! Subset 4
    
  ! -- The maximum number of trace gas perturbation predictors for TauCoeff_TraceGas 
  INTEGER, PUBLIC, PARAMETER :: MAX_N_TRACEGASES_PREDICTORS   = 7  

  ! -- The maximum number of non-LTE predictors
  INTEGER, PUBLIC, PARAMETER :: MAX_N_NON_LTE_PREDICTORS = 6
  ! ----------------------------------
  ! Predictor data type definition
  ! ----------------------------------

  TYPE :: Predictor_type
    INTEGER :: n_Allocates = 0

    ! -- Dimensions
    INTEGER :: n_Absorbers  = 0  ! J dimension

    ! -- Algorithm specific members
    ! -- Dimensions
    INTEGER :: n_Layers    =  0  ! L dimension

    LOGICAL :: Calc_Sun_Angle_Secant = .FALSE.   
    
    REAL( fp ) :: Sun_Fudge         =  ZERO   
    REAL( fp ) :: Source_COS_Layer1 =  ZERO   
    REAL( fp ) :: BL_Frac_Multi     =  ZERO   

    ! --OPTRAN min and max level to interpolation
    INTEGER ::  Min_OPTRAN_Level    =  0  
    INTEGER ::  Max_OPTRAN_Level    =  0
    
    INTEGER, DIMENSION( : ), POINTER :: Absorber_ID    => NULL() ! J

    ! -- ODCAPS Atmospheric profiles
    REAL( fp ), DIMENSION( : ),    POINTER :: Level_Pressure  => NULL()   ! 0: L 
    REAL( fp ), DIMENSION( : ),    POINTER :: Layer_Pressure  => NULL()   ! L
    REAL( fp ), DIMENSION( : ),    POINTER :: Temperature     => NULL()   ! L
    REAL( fp ), DIMENSION( :, : ), POINTER :: Absorber        => NULL()   ! L x J
    
    REAL( fp ), DIMENSION( : ),    POINTER :: Altitude        => NULL()   ! L 
    REAL( fp ), DIMENSION( : ),    POINTER :: Secant_Sensor_Zenith  => NULL()   ! L 
    REAL( fp ), DIMENSION( : ),    POINTER :: Secant_Source_Zenith  => NULL()   ! L 

    REAL( fp ), DIMENSION( : ),    POINTER :: Fix_Amount_Multiplier  => NULL()   ! L 

    ! -- Optional gases amount
    REAL( fp ), DIMENSION( : ),    POINTER :: CO2_Amount      => NULL()   ! L 
    REAL( fp ), DIMENSION( : ),    POINTER :: SO2_Amount      => NULL()   ! L 
    REAL( fp ), DIMENSION( : ),    POINTER :: HNO3_Amount     => NULL()   ! L 
    REAL( fp ), DIMENSION( : ),    POINTER :: N2O_Amount      => NULL()   ! L 
        
    ! -- Optional gases multiplier
    REAL( fp ), DIMENSION( : ),    POINTER :: CO2_Multiplier     => NULL()   ! L 
    REAL( fp ), DIMENSION( : ),    POINTER :: SO2_Multiplier     => NULL()   ! L 
    REAL( fp ), DIMENSION( : ),    POINTER :: HNO3_Multiplier    => NULL()   ! L 
    REAL( fp ), DIMENSION( : ),    POINTER :: N2O_Multiplier     => NULL()   ! L 

    ! -- OPTRAN for water vapor
    LOGICAL, DIMENSION( : ), POINTER :: OPTRAN_Level_Use => NULL()  ! MAX_N_WATER_OPTRAN_LAYERS = 300
    REAL( fp ), DIMENSION( : ),    POINTER ::  Layer_Water_Amount      => NULL() ! L
    REAL( fp ), DIMENSION( : ),    POINTER ::  Water_OPTRAN_Scaling    => NULL() ! 300
    INTEGER,         DIMENSION( : ),    POINTER ::  Lower_OPTRAN_Level      => NULL() ! L
    REAL( fp ), DIMENSION( : ),    POINTER ::  OPTRAN_Interp_Frac      => NULL() ! L 
    REAL( fp ), DIMENSION( :, : ), POINTER ::  OPTRAN_Water_Predictors => NULL() ! 9 x 300 
    
    
    ! Subset Predictors and trace gas predictors
    REAL( fp ), DIMENSION( :, :, : ), POINTER :: Predictor_Subset      => NULL() ! 7 x 45 x L  
    REAL( fp ), DIMENSION( :, :, : ), POINTER :: Predictor_Subset_Sun  => NULL() ! 4 x 45 x L  
    REAL( fp ), DIMENSION( :, : ),    POINTER :: TraceGas_Predictors   => NULL() ! 7 x L  
    REAL( fp ), DIMENSION( :, : ),    POINTER :: TraceGas_Predictors_Sun   => NULL() ! 7 x L  
    
    ! -- non-LTE predictors   
    REAL( fp ), DIMENSION( : ), POINTER ::  Non_LTE_Predictors => NULL() ! 6
    REAL( fp ) :: R_non_LTE   =  ZERO       ! -- For solar radiation non_LTE effect

    ! -- Optical Depth
    REAL( fp ) :: Surf_To_Space_Optical_Depth  =  ZERO       ! -- For solar radiation
    REAL( fp ), DIMENSION( : ), POINTER :: Optical_Depth       => NULL() !   L  
    REAL( fp ), DIMENSION( : ), POINTER :: LTS_Optical_Depth   => NULL() !   L  
    
    ! -- Lower and upper bound index for profile interpolation
    INTEGER, DIMENSION( :, : ), POINTER :: Index_Interpolation   => NULL()   ! L x 2
    
    ! For use of optical path profile interpolation
    INTEGER :: n_User_Layers = 0 ! Ku - number of layers of user profile
    REAL(fp), DIMENSION(:), POINTER :: User_Level_Pressure => NULL() ! Ku 
    
  END TYPE Predictor_type


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
!       Clear_Predictor
!
! PURPOSE:
!       Subroutine to clear the scalar members of a Predictor structure.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_Predictor( Predictor ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Predictor:  Predictor structure for which the scalar
!                       members have been cleared.
!                       UNITS:      N/A
!                       TYPE:       Predictor_type
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
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 24-May-2006
!                       Yong.Chen@noaa.gov
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_Predictor( Predictor )

    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor

    Predictor%n_Absorbers  = 0
    Predictor%n_Layers     = 0
    
    Predictor%Calc_Sun_Angle_Secant = .FALSE.

    Predictor%Sun_Fudge                    = ZERO
    Predictor%Source_COS_Layer1            =  ZERO  
    Predictor%BL_Frac_Multi                =  ZERO   
    Predictor%Min_OPTRAN_Level             =  0 
    Predictor%Max_OPTRAN_Level             =  0 
    Predictor%Surf_To_Space_Optical_Depth  =  ZERO
    Predictor%R_non_LTE                    =  ZERO
        
  END SUBROUTINE Clear_Predictor


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
!       Associated_Predictor
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       Predictor structure.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_Predictor( Predictor,      &  ! Input
!                                                  ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       Predictor:       Predictor structure which is to have its
!                            pointer member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       Predictor_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            Predictor structure pointer members are
!                            associated.
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
!       Association_Status:  The return value is a logical value indicating
!                            the association status of the Predictor
!                            pointer members.
!                            .TRUE.  - if ALL the Predictor pointer
!                                      members are associated, or if the
!                                      ANY_Test argument is set and ANY of the
!                                      Predictor pointer members are
!                                      associated.
!                            .FALSE. - some or all of the Predictor
!                                      pointer members are NOT associated.
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
!       Written by:     Yong Chen, CSU/CIRA 24-May-2006
!                       Yong.Chen@noaa.gov
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_Predictor( Predictor,     & ! Input
                                 ANY_Test )     & ! Optional input
                         RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( Predictor_type ), INTENT( IN ) :: Predictor

    ! -- Optional input
    INTEGER,      OPTIONAL, INTENT( IN ) :: ANY_Test


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
    !#                                                                          #
    !#  Note that there is no difference betweent he ALL_Test and ANY_Test, but #
    !#  but the hooks are kept here for future expansion of the Predictor   #
    !#  structure.                                                              #
    !#--------------------------------------------------------------------------#

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( Predictor%Absorber_ID             ) .AND. &  
           ASSOCIATED( Predictor%Level_Pressure          ) .AND. &  
           ASSOCIATED( Predictor%Layer_Pressure          ) .AND. &  
           ASSOCIATED( Predictor%Temperature             ) .AND. &  
           ASSOCIATED( Predictor%Absorber                ) .AND. &  
           ASSOCIATED( Predictor%Altitude                ) .AND. &  
           ASSOCIATED( Predictor%Secant_Sensor_Zenith    ) .AND. &  
           ASSOCIATED( Predictor%Fix_Amount_Multiplier   ) .AND. &  
           ASSOCIATED( Predictor%CO2_Amount              ) .AND. &  
           ASSOCIATED( Predictor%SO2_Amount              ) .AND. &  
           ASSOCIATED( Predictor%HNO3_Amount             ) .AND. &  
           ASSOCIATED( Predictor%N2O_Amount              ) .AND. &  
           ASSOCIATED( Predictor%CO2_Multiplier          ) .AND. &  
           ASSOCIATED( Predictor%SO2_Multiplier          ) .AND. &  
           ASSOCIATED( Predictor%HNO3_Multiplier         ) .AND. &  
           ASSOCIATED( Predictor%N2O_Multiplier          ) .AND. &  
           ASSOCIATED( Predictor%OPTRAN_Level_Use        ) .AND. &  
           ASSOCIATED( Predictor%Layer_Water_Amount      ) .AND. &  
           ASSOCIATED( Predictor%Water_OPTRAN_Scaling    ) .AND. &  
           ASSOCIATED( Predictor%Lower_OPTRAN_Level      ) .AND. &  
           ASSOCIATED( Predictor%OPTRAN_Interp_Frac      ) .AND. &  
           ASSOCIATED( Predictor%OPTRAN_Water_Predictors ) .AND. &  
           ASSOCIATED( Predictor%Predictor_Subset        ) .AND. &  
           ASSOCIATED( Predictor%TraceGas_Predictors     ) .AND. &  
           ASSOCIATED( Predictor%Optical_Depth           ) .AND. &  
           ASSOCIATED( Predictor%LTS_Optical_Depth       ) .AND. &  
           ASSOCIATED( Predictor%Index_Interpolation  )  ) THEN     
        Association_Status = .TRUE.
      END IF
      
      IF ( Predictor%Calc_Sun_Angle_Secant) THEN
      
        Association_Status = Association_Status .AND. &
           ASSOCIATED( Predictor%Secant_Source_Zenith    ) .AND. &     
           ASSOCIATED( Predictor%Predictor_Subset_Sun    ) .AND. &     
           ASSOCIATED( Predictor%TraceGas_Predictors_Sun ) .AND. &  
           ASSOCIATED( Predictor%Non_LTE_Predictors      )  		   
      
      END IF               
      IF( Predictor%n_User_Layers > 0 )THEN
        Association_Status = Association_Status .AND. &
                             ASSOCIATED( Predictor%User_Level_Pressure )
      END IF

    ELSE

      IF ( ASSOCIATED( Predictor%Absorber_ID             ) .OR. &  
           ASSOCIATED( Predictor%Level_Pressure          ) .OR. &  
           ASSOCIATED( Predictor%Layer_Pressure          ) .OR. &  
           ASSOCIATED( Predictor%Temperature             ) .OR. &  
           ASSOCIATED( Predictor%Absorber                ) .OR. &  
           ASSOCIATED( Predictor%Altitude                ) .OR. &  
           ASSOCIATED( Predictor%Secant_Sensor_Zenith    ) .OR. &  
           ASSOCIATED( Predictor%Fix_Amount_Multiplier   ) .OR. &  
           ASSOCIATED( Predictor%CO2_Amount              ) .OR. &  
           ASSOCIATED( Predictor%SO2_Amount              ) .OR. &  
           ASSOCIATED( Predictor%HNO3_Amount             ) .OR. &  
           ASSOCIATED( Predictor%N2O_Amount              ) .OR. &  
           ASSOCIATED( Predictor%CO2_Multiplier          ) .OR. &  
           ASSOCIATED( Predictor%SO2_Multiplier          ) .OR. &  
           ASSOCIATED( Predictor%HNO3_Multiplier         ) .OR. &  
           ASSOCIATED( Predictor%N2O_Multiplier          ) .OR. &  
           ASSOCIATED( Predictor%OPTRAN_Level_Use        ) .OR. &  
           ASSOCIATED( Predictor%Layer_Water_Amount      ) .OR. &  
           ASSOCIATED( Predictor%Water_OPTRAN_Scaling    ) .OR. &  
           ASSOCIATED( Predictor%Lower_OPTRAN_Level      ) .OR. &  
           ASSOCIATED( Predictor%OPTRAN_Interp_Frac      ) .OR. &  
           ASSOCIATED( Predictor%OPTRAN_Water_Predictors ) .OR. &  
           ASSOCIATED( Predictor%Predictor_Subset        ) .OR. &  
           ASSOCIATED( Predictor%TraceGas_Predictors     ) .OR. &  
           ASSOCIATED( Predictor%Optical_Depth           ) .OR. &  
           ASSOCIATED( Predictor%LTS_Optical_Depth       ) .OR. &  
           ASSOCIATED( Predictor%Index_Interpolation  )  ) THEN     
        Association_Status = .TRUE.
      END IF
      
      IF ( Predictor%Calc_Sun_Angle_Secant) THEN
      
        Association_Status = Association_Status .OR. &
           ASSOCIATED( Predictor%Secant_Source_Zenith    ) .OR. &     
           ASSOCIATED( Predictor%Predictor_Subset_Sun    ) .OR. &     
           ASSOCIATED( Predictor%TraceGas_Predictors_Sun ) .OR. &  
           ASSOCIATED( Predictor%Non_LTE_Predictors      )  		   
      
      END IF               
      IF( Predictor%n_User_Layers > 0 )THEN
        Association_Status = Association_Status .OR. &
                             ASSOCIATED( Predictor%User_Level_Pressure )
      END IF
 
    END IF

  END FUNCTION Associated_Predictor



!--------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_Predictor
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of
!       a Predictor data structure.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_Predictor( Predictor,            &  ! Output
!                                         RCS_Id = RCS_Id,      &  ! Revision control
!                                     Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
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
!       Predictor:  Re-initialized Predictor structure.
!                       UNITS:      N/A
!                       TYPE:       Predictor_type
!                       DIMENSION:  Scalar OR Rank-1 array
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the structure re-initialisation was successful
!                          == FAILURE - an error occurred, or
!                                     - the structure internal allocation counter
!                                       is not equal to zero (0) upon exiting this
!                                       function. This value is incremented and
!                                       decremented for every structure allocation
!                                       and deallocation respectively.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! CALLS:
!       Clear_Predictor:       Subroutine to clear the scalar members
!                                       of a Predictor structure.
!
!       Associated_Predictor:  Function to test the association status
!                                       of the pointer members of a
!                                       Predictor structure.
!
!       Display_Message:                Subroutine to output messages
!                                       SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!       
! COMMENTS:
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 24-May-2006
!                       Yong.Chen@noaa.gov
!S-
!--------------------------------------------------------------------------------

  FUNCTION Destroy_Predictor( Predictor,     &  ! Output
                              No_Clear,      &  ! Optional input           
                              RCS_Id,        &  ! Revision control         
                              Message_Log )  &  ! Error messaging          
                            RESULT( Error_Status )                         



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor

    ! -- Optional input
    INTEGER,               OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),        OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),        OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_Predictor'


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

    IF ( Clear ) CALL Clear_Predictor( Predictor )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_Predictor( Predictor ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the Predictor Absorber_ID 
    IF ( ASSOCIATED( Predictor%Absorber_ID  ) ) THEN

      DEALLOCATE( Predictor%Absorber_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Absorber_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! -- Deallocate the Predictor Level_Pressure 
    IF ( ASSOCIATED( Predictor%Level_Pressure ) ) THEN

      DEALLOCATE( Predictor%Level_Pressure, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Level_Pressure ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Deallocate the Predictor Layer_Pressure 
    IF ( ASSOCIATED( Predictor%Layer_Pressure ) ) THEN

      DEALLOCATE( Predictor%Layer_Pressure, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Layer_Pressure ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Deallocate the Predictor  Temperature 
    IF ( ASSOCIATED( Predictor%Temperature ) ) THEN

      DEALLOCATE( Predictor%Temperature, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Temperature ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Deallocate the Predictor Absorber  
    IF ( ASSOCIATED( Predictor%Absorber ) ) THEN

      DEALLOCATE( Predictor%Absorber, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Absorber ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Deallocate the Predictor Altitude   
    IF ( ASSOCIATED( Predictor%Altitude ) ) THEN

      DEALLOCATE( Predictor%Altitude, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Altitude ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Deallocate the Predictor Secant_Sensor_Zenith    
    IF ( ASSOCIATED( Predictor%Secant_Sensor_Zenith ) ) THEN

      DEALLOCATE( Predictor%Secant_Sensor_Zenith, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Secant_Sensor_Zenith ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Deallocate the Predictor Fix_Amount_Multiplier    
    IF ( ASSOCIATED( Predictor%Fix_Amount_Multiplier ) ) THEN

      DEALLOCATE( Predictor%Fix_Amount_Multiplier, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Fix_Amount_Multiplier ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Deallocate the Predictor CO2_Amount     
    IF ( ASSOCIATED( Predictor%CO2_Amount ) ) THEN

      DEALLOCATE( Predictor%CO2_Amount, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor CO2_Amount ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Deallocate the Predictor SO2_Amount     
    IF ( ASSOCIATED( Predictor%SO2_Amount ) ) THEN

      DEALLOCATE( Predictor%SO2_Amount, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor SO2_Amount ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Deallocate the Predictor HNO3_Amount     
    IF ( ASSOCIATED( Predictor%HNO3_Amount ) ) THEN

      DEALLOCATE( Predictor%HNO3_Amount, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor HNO3_Amount ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! -- Deallocate the Predictor N2O_Amount     
    IF ( ASSOCIATED( Predictor%N2O_Amount ) ) THEN

      DEALLOCATE( Predictor%N2O_Amount, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor N2O_Amount ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Deallocate the Predictor CO2_Multiplier     
    IF ( ASSOCIATED( Predictor%CO2_Multiplier ) ) THEN

      DEALLOCATE( Predictor%CO2_Multiplier, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor CO2_Multiplier ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Predictor SO2_Multiplier     
    IF ( ASSOCIATED( Predictor%SO2_Multiplier ) ) THEN

      DEALLOCATE( Predictor%SO2_Multiplier, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor SO2_Multiplier ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Predictor HNO3_Multiplier     
    IF ( ASSOCIATED( Predictor%HNO3_Multiplier ) ) THEN

      DEALLOCATE( Predictor%HNO3_Multiplier, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor HNO3_Multiplier ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Predictor N2O_Multiplier     
    IF ( ASSOCIATED( Predictor%N2O_Multiplier ) ) THEN

      DEALLOCATE( Predictor%N2O_Multiplier, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor N2O_Multiplier ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Predictor OPTRAN_Level_Use    
    IF ( ASSOCIATED( Predictor%OPTRAN_Level_Use  ) ) THEN

      DEALLOCATE( Predictor%OPTRAN_Level_Use, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor OPTRAN_Level_Use ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Predictor Layer_Water_Amount    
    IF ( ASSOCIATED( Predictor%Layer_Water_Amount  ) ) THEN

      DEALLOCATE( Predictor%Layer_Water_Amount, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Layer_Water_Amount ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Predictor Water_OPTRAN_Scaling    
    IF ( ASSOCIATED( Predictor%Water_OPTRAN_Scaling  ) ) THEN

      DEALLOCATE( Predictor%Water_OPTRAN_Scaling, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Water_OPTRAN_Scaling ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Predictor Lower_OPTRAN_Level     
    IF ( ASSOCIATED( Predictor%Lower_OPTRAN_Level  ) ) THEN

      DEALLOCATE( Predictor%Lower_OPTRAN_Level, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Lower_OPTRAN_Level ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Predictor OPTRAN_Interp_Frac      
    IF ( ASSOCIATED( Predictor%OPTRAN_Interp_Frac ) ) THEN

      DEALLOCATE( Predictor%OPTRAN_Interp_Frac, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor OPTRAN_Interp_Frac ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Predictor OPTRAN_Water_Predictors      
    IF ( ASSOCIATED( Predictor%OPTRAN_Water_Predictors ) ) THEN

      DEALLOCATE( Predictor%OPTRAN_Water_Predictors, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor OPTRAN_Water_Predictors ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Predictor Predictor_Subset       
    IF ( ASSOCIATED( Predictor%Predictor_Subset ) ) THEN

      DEALLOCATE( Predictor%Predictor_Subset, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Predictor_Subset ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Predictor TraceGas_Predictors        
    IF ( ASSOCIATED( Predictor%TraceGas_Predictors ) ) THEN

      DEALLOCATE( Predictor%TraceGas_Predictors, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor TraceGas_Predictors ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Predictor Optical_Depth         
    IF ( ASSOCIATED( Predictor%Optical_Depth ) ) THEN

      DEALLOCATE( Predictor%Optical_Depth, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Optical_Depth ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Predictor LTS_Optical_Depth         
    IF ( ASSOCIATED( Predictor%LTS_Optical_Depth ) ) THEN

      DEALLOCATE( Predictor%LTS_Optical_Depth, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor LTS_Optical_Depth ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Predictor Index_Interpolation         
    IF ( ASSOCIATED( Predictor%Index_Interpolation ) ) THEN

      DEALLOCATE( Predictor%Index_Interpolation, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Index_Interpolation ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Deallocate the Predictor Secant_Source_Zenith         
    IF ( ASSOCIATED( Predictor%Secant_Source_Zenith ) ) THEN

      DEALLOCATE( Predictor%Secant_Source_Zenith, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Secant_Source_Zenith ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Deallocate the Predictor Predictor_Subset_Sun       
    IF ( ASSOCIATED( Predictor%Predictor_Subset_Sun ) ) THEN

      DEALLOCATE( Predictor%Predictor_Subset_Sun, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Predictor_Subset_Sun ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Deallocate the Predictor TraceGas_Predictors_Sun        
    IF ( ASSOCIATED( Predictor%TraceGas_Predictors_Sun ) ) THEN

      DEALLOCATE( Predictor%TraceGas_Predictors_Sun, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor TraceGas_Predictors_Sun ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Deallocate the Predictor Non_LTE_Predictors        
    IF ( ASSOCIATED( Predictor%Non_LTE_Predictors ) ) THEN

      DEALLOCATE( Predictor%Non_LTE_Predictors, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Predictor Non_LTE_Predictors ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF
    ! Deallocate the User_Level_Pressure array
    IF( Predictor%n_User_Layers > 0 )THEN
      DEALLOCATE( Predictor%User_Level_Pressure, &                                  
                  STAT = Allocate_Status )                                    
      IF ( Allocate_Status /= 0 ) THEN                                        
        Error_Status = FAILURE                                                
        WRITE( Message, '( "Error deallocating the Predictor "//&
            &"User_Level_Pressure pointer member. STAT = ", i5 )' ) & 
                        Allocate_Status                                       
        CALL Display_Message( &                                               
                             ROUTINE_NAME, &                                  
                             TRIM(Message), &                                 
                             Error_Status, &                                  
                             Message_Log=Message_Log )                        
        RETURN                                                                
      END IF 

       Predictor%n_User_Layers = 0
    END IF                                                                 




    !#--------------------------------------------------------------------------#
    !#               -- DECREMENT AND TEST ALLOCATION COUNTER --                #
    !#--------------------------------------------------------------------------#

    Predictor%n_Allocates = Predictor%n_Allocates - 1

    IF ( Predictor%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      Predictor%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Destroy_Predictor


!--------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_Predictor
! 
! PURPOSE:
!       Function to allocate the pointer members of the Predictor
!       data structure.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_Predictor(  n_Absorbers,              &  ! Input                   
!                                           n_Layers,                 &  ! Input                         
!                                           Predictor,                &  ! Output                      
!                                           n_User_Layers,            &  ! Optional Input                
!                                           Calc_Sun_Angle_Secant,    &  ! Optional Input          
!                                           RCS_Id = RCS_Id,          &  ! Revision control        
!                                           Message_Log = Message_Log )  ! Error messaging         
!
! INPUT ARGUMENTS:
!         n_Absorbers:       Number of atmospheric absorbers.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
!         n_Layers:          Number of atmospheric layers.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!         n_User_Layers:     Number of user input atmospheric layers.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
!     Calc_Sun_Angle_Secant: If true, need calculate the solar effect and non-LTE; 
!                            If false, don't need calculate the solar effect and non-LTE
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
!       Message_Log:         Character string specifying a filename in which any
!                            messages will be logged. If not specified, or if an
!                            error occurs opening the log file, the default action
!                            is to output messages to standard output.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Predictor:       Predictor structure with allocated pointer members
!                            UNITS:      N/A
!                            TYPE:       Predictor_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:              Character string containing the Revision Control
!                            System Id field for the module.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( OUT ), OPTIONAL
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
!       Associated_Predictor: Function to test the association status
!                                      of the pointer members of a Predictor
!                                      structure.
!
!       Display_Message:               Subroutine to output messages
!                                      SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!       
! COMMENTS:
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 24-May-2006
!                       Yong.Chen@noaa.gov
!S-
!--------------------------------------------------------------------------------

  FUNCTION Allocate_Predictor(    n_Absorbers,           &  ! Input                       
                                  n_Layers,              &  ! Input                             
                                  Predictor,             &  ! Output                          
                                  n_User_Layers,         &  ! Optional Input                      
                                  Calc_Sun_Angle_Secant, &  ! Optional input         
                                  RCS_Id,                &  ! Revision control            
                                  Message_Log )          &  ! Error messaging             
                                RESULT( Error_Status )                               

    ! ---------
    ! Arguments
    ! ---------
    ! -- Input
    INTEGER,                         INTENT( IN )     :: n_Absorbers
    INTEGER,                         INTENT( IN )     :: n_Layers 
    ! -- Output
    TYPE( Predictor_type ),          INTENT( IN OUT ) :: Predictor
     
    ! -- Optional Input
    INTEGER,               OPTIONAL, INTENT( IN )     :: n_User_Layers
    Logical,               OPTIONAL, INTENT( IN )     :: Calc_Sun_Angle_Secant 

    ! -- Revision control
    CHARACTER( * ),        OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),        OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_Predictor'


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

    IF ( n_Absorbers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Absorbers must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_Layers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_Predictor( Predictor, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_Predictor( Predictor, &
                                                 No_Clear = SET, &
                                                 Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating Predictor pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( Predictor%Absorber_ID( n_Absorbers ),                        &  
              Predictor%Level_Pressure( 0:n_Layers ),                      & 
              Predictor%Layer_Pressure( n_Layers ),                        & 
              Predictor%Temperature( n_Layers ),                           & 
              Predictor%Absorber( n_Layers, n_Absorbers ),                 & 
              Predictor%Altitude( n_Layers ),                              & 
              Predictor%Secant_Sensor_Zenith( n_Layers ),                  & 
              Predictor%Fix_Amount_Multiplier( n_Layers ),                 &  
              Predictor%CO2_Amount( n_Layers ),	                           &  
              Predictor%SO2_Amount ( n_Layers ),	                   &  
              Predictor%HNO3_Amount( n_Layers ),	                   &  
              Predictor%N2O_Amount( n_Layers ),	                           &  
              Predictor%CO2_Multiplier( n_Layers ),	                   &  
              Predictor%SO2_Multiplier( n_Layers ),	                   &  
              Predictor%HNO3_Multiplier( n_Layers ),                       &  
              Predictor%N2O_Multiplier( n_Layers ),	                   &  
              Predictor%OPTRAN_Level_Use( MAX_N_WATER_OPTRAN_LAYERS ),     &  
              Predictor%Layer_Water_Amount( n_Layers ),                    &  
              Predictor%Water_OPTRAN_Scaling( MAX_N_WATER_OPTRAN_LAYERS ), &  
              Predictor%Lower_OPTRAN_Level( n_Layers ),                    &  
              Predictor%OPTRAN_Interp_Frac( n_Layers ),                    &  
              Predictor%OPTRAN_Water_Predictors(MAX_N_WATER_OPTRAN_PREDICTORS, MAX_N_WATER_OPTRAN_LAYERS), &  
              Predictor%Predictor_Subset(MAX_N_SUBSETS,MAX_N_SUBSET_TOTAL_PREDICTORS, n_Layers ),&  
              Predictor%TraceGas_Predictors(MAX_N_TRACEGASES_PREDICTORS,n_Layers ), &  
              Predictor%Optical_Depth( n_Layers ),                         &  
              Predictor%LTS_Optical_Depth( n_Layers ),                     &       
              Predictor%Index_Interpolation( n_Layers, 2 ),                &
	      STAT = Allocate_Status )     
 
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating Predictor data arrays here. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    Predictor%Absorber_ID 		= 0				      
    Predictor%Level_Pressure 		= ZERO					      
    Predictor%Layer_Pressure 		= ZERO					      
    Predictor%Temperature 		= ZERO					      
    Predictor%Absorber 			= ZERO				      
    Predictor%Altitude 			= ZERO					      
    Predictor%Secant_Sensor_Zenith 	= ZERO					      
    Predictor%Fix_Amount_Multiplier 	= ZERO						      
    Predictor%CO2_Amount 		= ZERO				      
    Predictor%SO2_Amount 		= ZERO				      
    Predictor%HNO3_Amount 		= ZERO		      
    Predictor%N2O_Amount 		= ZERO	 			      
    Predictor%CO2_Multiplier 		= ZERO				      
    Predictor%SO2_Multiplier 		= ZERO			      
    Predictor%HNO3_Multiplier 		= ZERO				      
    Predictor%N2O_Multiplier 		= ZERO			      
    Predictor%OPTRAN_Level_Use 		=.FALSE.			      
    Predictor%Layer_Water_Amount 	= ZERO			      
    Predictor%Water_OPTRAN_Scaling 	= ZERO			      
    Predictor%Lower_OPTRAN_Level 	= 0				      
    Predictor%OPTRAN_Interp_Frac 	= ZERO			      
    Predictor%OPTRAN_Water_Predictors 	= ZERO      
    Predictor%Predictor_Subset 	        = ZERO
    Predictor%TraceGas_Predictors 	= ZERO	      
    Predictor%Optical_Depth 	        = ZERO				      
    Predictor%LTS_Optical_Depth         = ZERO				      
    Predictor%Index_Interpolation 	= IP_INIT				      
 
        
    IF(PRESENT( Calc_Sun_Angle_Secant ) .AND. Calc_Sun_Angle_Secant) THEN
      ALLOCATE( Predictor%Secant_Source_Zenith( n_Layers ),  &     
                Predictor%Predictor_Subset_Sun(MAX_N_SUBSETS_SUN,MAX_N_SUBSET_TOTAL_PREDICTORS, n_Layers ),  &     
                Predictor%TraceGas_Predictors_Sun(MAX_N_TRACEGASES_PREDICTORS,n_Layers ), &  
                Predictor%Non_LTE_Predictors(MAX_N_NON_LTE_PREDICTORS), STAT= Allocate_Status ) 		   
    
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error allocating Predictor data arrays. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

      Predictor%Calc_Sun_Angle_Secant   = .TRUE.
      
      Predictor%Secant_Source_Zenith    = ZERO
      Predictor%Predictor_Subset_Sun    = ZERO
      Predictor%TraceGas_Predictors_Sun = ZERO	      
      Predictor%Non_LTE_Predictors      = ZERO
    ENDIF
    
    IF( PRESENT(n_User_Layers) )THEN
      IF ( n_User_Layers < 1 )THEN
        Error_Status = FAILURE
        CALL Display_Message( &
               ROUTINE_NAME, &
               'Input n_User_Layers must be > 0.', &
               Error_Status, &
               Message_Log=Message_Log )
        RETURN
      END IF
      
      ALLOCATE( Predictor%User_Level_Pressure(0:n_User_Layers), &
                STAT = Allocate_Status )   
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating User_Level_Pressure array. STAT = ", i5 )' ) &
                        Allocate_Status
        Error_Status = FAILURE
        CALL Display_Message( &
               ROUTINE_NAME, &
               TRIM(Message), &
               Error_Status, &
               Message_Log=Message_Log )
        RETURN
      END IF
      Predictor%n_User_Layers = n_User_Layers
      Predictor%User_Level_Pressure(0:n_User_Layers) = ZERO
    END IF
    
    !#--------------------------------------------------------------------------#
    !#             -- ASSIGN THE DIMENSIONS   --             #
    !#--------------------------------------------------------------------------#

    Predictor%n_Absorbers    = n_Absorbers
    Predictor%n_Layers       = n_Layers


    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    Predictor%n_Allocates = Predictor%n_Allocates + 1

    IF ( Predictor%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      Predictor%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Predictor





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_Predictor
!
! PURPOSE:
!       Function to copy valid Predictor structures.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_Predictor( Predictor_in,             &  ! Input
!                                        Predictor_out,            &  ! Output                       
!                                        RCS_Id      = RCS_Id,     &  ! Revision control         
!                                        Message_Log = Message_Log )  ! Error messaging          
!
! INPUT ARGUMENTS:
!       Predictor_in:  Predictor structure which is to be copied.
!                          UNITS:      N/A
!                          TYPE:       Predictor_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Predictor_out: Copy of the input structure, Predictor_in.
!                          UNITS:      N/A
!                          TYPE:       Predictor_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
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
!       Associated_Predictor: Function to test the association status
!                                      of the pointer members of a
!                                      Predictor structure.
!
!       Destroy_Predictor:    Function to re-initialize Predictor
!                                      structures.
!
!       Allocate_Predictor:   Function to allocate the pointer members
!                                      of the Predictor data structure.
!
!       Display_Message:               Subroutine to output messages
!                                      SOURCE: Message_Handler module
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output AtmScatter argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 24-May-2006
!                       Yong.Chen@noaa.gov
!S-
!--------------------------------------------------------------------------------

  FUNCTION Assign_Predictor( Predictor_in,      &  ! Input
                             Predictor_out,     &  ! Output                       
                             RCS_Id,            &  ! Revision control         
                             Message_Log )      &  ! Error messaging          
                           RESULT( Error_Status )                             



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( Predictor_type ), INTENT( IN )     :: Predictor_in

    ! -- Output
    TYPE( Predictor_type ), INTENT( IN OUT ) :: Predictor_out

    ! -- Revision control
    CHARACTER( * ),        OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),        OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_Predictor'



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

    ! ----------------------------------------------
    ! ALL *input* pointers must be associated.
    !
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    ! ----------------------------------------------

    IF ( .NOT. Associated_Predictor( Predictor_In ) ) THEN

      Error_Status = Destroy_Predictor( Predictor_Out, &
                                    Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output Predictor pointer members.', &
                              Error_Status,    &
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

    Error_Status = Allocate_Predictor(Predictor_in%n_Absorbers, &                                            
                                      Predictor_in%n_Layers , &                                              
                                      Predictor_out, &                                                       
                                      n_User_Layers = Predictor_in%n_User_Layers, &                          
				      Calc_Sun_Angle_Secant = Predictor_in%Calc_Sun_Angle_Secant, &          
                                      Message_Log = Message_Log )                                            

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output Predictor arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------
    ! Assign array data
    ! -----------------
    Predictor_out%Absorber_ID	           = Predictor_in%Absorber_ID          
    Predictor_out%Level_Pressure           = Predictor_in%Level_Pressure       
    Predictor_out%Layer_Pressure           = Predictor_in%Layer_Pressure       
    Predictor_out%Temperature	           = Predictor_in%Temperature          
    Predictor_out%Absorber	           = Predictor_in%Absorber         
    Predictor_out%Altitude	           = Predictor_in%Altitude         
    Predictor_out%Secant_Sensor_Zenith     = Predictor_in%Secant_Sensor_Zenith 
    Predictor_out%Fix_Amount_Multiplier    = Predictor_in%Fix_Amount_Multiplier
    Predictor_out%CO2_Amount	           = Predictor_in%CO2_Amount               
    Predictor_out%SO2_Amount	           = Predictor_in%SO2_Amount               
    Predictor_out%HNO3_Amount	           = Predictor_in%HNO3_Amount          
    Predictor_out%N2O_Amount	           = Predictor_in%N2O_Amount               
    Predictor_out%CO2_Multiplier           = Predictor_in%CO2_Multiplier       
    Predictor_out%SO2_Multiplier           = Predictor_in%SO2_Multiplier       
    Predictor_out%HNO3_Multiplier          = Predictor_in%HNO3_Multiplier      
    Predictor_out%N2O_Multiplier           = Predictor_in%N2O_Multiplier       
    Predictor_out%OPTRAN_Level_Use         = Predictor_in%OPTRAN_Level_Use     
    Predictor_out%Layer_Water_Amount       = Predictor_in%Layer_Water_Amount	   
    Predictor_out%Water_OPTRAN_Scaling     = Predictor_in%Water_OPTRAN_Scaling   
    Predictor_out%Lower_OPTRAN_Level       = Predictor_in%Lower_OPTRAN_Level	   
    Predictor_out%OPTRAN_Interp_Frac       = Predictor_in%OPTRAN_Interp_Frac	   
    Predictor_out%OPTRAN_Water_Predictors  = Predictor_in%OPTRAN_Water_Predictors 
    Predictor_out%Predictor_Subset	   = Predictor_in%Predictor_Subset	   
    Predictor_out%TraceGas_Predictors      = Predictor_in%TraceGas_Predictors     
    Predictor_out%Optical_Depth            = Predictor_in%Optical_Depth     
    Predictor_out%LTS_Optical_Depth        = Predictor_in%LTS_Optical_Depth 
    Predictor_out%Index_Interpolation      = Predictor_in%Index_Interpolation     

    IF ( Predictor_in%Calc_Sun_Angle_Secant ) THEN
      Predictor_out%Secant_Source_Zenith   = Predictor_in%Secant_Source_Zenith    
      Predictor_out%Predictor_Subset_Sun   = Predictor_in%Predictor_Subset_Sun    
      Predictor_out%TraceGas_Predictors_Sun= Predictor_in%TraceGas_Predictors_Sun     
      Predictor_out%Non_LTE_Predictors     = Predictor_in%Non_LTE_Predictors    
    ENDIF     

    IF ( Predictor_in%n_User_Layers > 0 ) THEN
      Predictor_out%User_Level_Pressure    = Predictor_in%User_Level_Pressure    
    ENDIF     
    
  END FUNCTION Assign_Predictor

!--------------------------------------------------------------------------------
!S+
! NAME:
!       Zero_Predictor
! 
! PURPOSE:
!       Subroutine to zero-out all members of a Predictor structure - both
!       scalar and pointer.
!
! CATEGORY:
!       CRTM : Predictor
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Zero_Predictor ( Predictor )
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Predictor:   Zeroed out Predictor structure.
!                        UNITS:      N/A
!                        TYPE:       Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
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
!       - No checking of the input structure is performed, so there are no
!         tests for pointer member association status. This means the Predictor 
!         structure must have allocated pointer members upon entry to this
!         routine.
!
! COMMENTS:
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument must be defined upon
!       input.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 28-June-2006
!                       Yong.Chen@noaa.gov
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Zero_Predictor( Predictor )  ! Output
    TYPE( Predictor_type ),  INTENT( IN OUT ) :: Predictor

!    Predictor%Absorber_ID 		= 0				      
    Predictor%Level_Pressure 		= ZERO					      
    Predictor%Layer_Pressure 		= ZERO					      
    Predictor%Temperature 		= ZERO					      
    Predictor%Absorber 			= ZERO				      
    Predictor%Altitude 			= ZERO					      
    Predictor%Secant_Sensor_Zenith 	= ZERO					      
    Predictor%Fix_Amount_Multiplier 	= ZERO						      
    Predictor%CO2_Amount 		= ZERO				      
    Predictor%SO2_Amount 		= ZERO				      
    Predictor%HNO3_Amount 		= ZERO		      
    Predictor%N2O_Amount 		= ZERO	 			      
    Predictor%CO2_Multiplier 		= ZERO				      
    Predictor%SO2_Multiplier 		= ZERO			      
    Predictor%HNO3_Multiplier 		= ZERO				      
    Predictor%N2O_Multiplier 		= ZERO			      
    Predictor%OPTRAN_Level_Use 		=.FALSE.			      
    Predictor%Layer_Water_Amount 	= ZERO			      
    Predictor%Water_OPTRAN_Scaling 	= ZERO			      
    Predictor%Lower_OPTRAN_Level 	= 0				      
    Predictor%OPTRAN_Interp_Frac 	= ZERO			      
    Predictor%OPTRAN_Water_Predictors 	= ZERO      
    Predictor%Predictor_Subset 	        = ZERO
    Predictor%TraceGas_Predictors 	= ZERO	      
    Predictor%Optical_Depth 	        = ZERO				      
    Predictor%LTS_Optical_Depth         = ZERO				      
    Predictor%Index_Interpolation 	= IP_INIT				      
    
    IF( Predictor%Calc_Sun_Angle_Secant ) THEN
      Predictor%Secant_Source_Zenith    = ZERO
      Predictor%Predictor_Subset_Sun    = ZERO
      Predictor%TraceGas_Predictors_Sun = ZERO	      
      Predictor%Non_LTE_Predictors      = ZERO
    ENDIF
    IF(Predictor%n_User_Layers > 0)THEN
      Predictor%User_Level_Pressure = ZERO
    END IF
 
  END SUBROUTINE Zero_Predictor 

END MODULE ODCAPS_Predictor_Define
