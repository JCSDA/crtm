!
! CRTM_AerosolScatter
!
! Module to compute the aerosol particle absorption and scattering properties
! required for radiative transfer in a aerosol-y atmosphere.
!
!
! CREATION HISTORY  
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Feb-2005
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_AerosolScatter


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds, ONLY: fp=>fp_kind
  USE Message_Handler

  ! CRTM modules
  USE CRTM_Parameters
  USE CRTM_SpcCoeff
  USE CRTM_AerosolCoeff
  USE CRTM_Atmosphere_Define,   ONLY: CRTM_Atmosphere_type
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type

  ! The AtmScatter structure definition module
  ! The PUBLIC entities in CRTM_AtmScatter_Define
  ! are also explicitly defined as PUBLIC here
  ! (down below) so a user need only USE this
  ! module (CRTM_AerosolScatter).
  USE CRTM_AtmScatter_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! Everything private by default
  PRIVATE

  ! CRTM_AtmScatter structure data type
  ! in the CRTM_AtmScatter_Define module
  PUBLIC :: CRTM_AtmScatter_type

  ! CRTM_AtmScatter structure routines inherited
  ! from the CRTM_AtmScatter_Define module
  PUBLIC :: CRTM_Associated_AtmScatter
  PUBLIC :: CRTM_Destroy_AtmScatter
  PUBLIC :: CRTM_Allocate_AtmScatter
  PUBLIC :: CRTM_Assign_AtmScatter

  ! Science routines in this modules
  PUBLIC :: CRTM_Compute_AerosolScatter
  PUBLIC :: CRTM_Compute_AerosolScatter_TL
  PUBLIC :: CRTM_Compute_AerosolScatter_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_AerosolScatter.f90,v 1.4.2.1 2006/09/07 09:07:36 frpv Exp $'


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------

  TYPE, PUBLIC :: CRTM_ASVariables_type
    PRIVATE
    INTEGER :: dummy=0
  END TYPE CRTM_ASVariables_type




CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_AerosolScatter
!
! PURPOSE:
!       Function to compute the aerosol absorption and scattering properties
!       and populate the output AerosolScatter structure for a single channel.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_AerosolScatter( Atmosphere             , &  ! Input
!                                                   SensorIndex            , &  ! Input
!                                                   ChannelIndex           , &  ! Input
!                                                   AerosolScatter         , &  ! Output
!                                                   ASVariables            , &  ! Internal variable output
!                                                   Message_Log=Message_Log  )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Atmosphere:      CRTM_Atmosphere structure containing the atmospheric
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Atmosphere_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!        AerosolScatter: CRTM_AtmScatter structure containing the aerosol
!                        absorption and scattering properties required by
!                        the radiative transfer.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AtmScatter_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!        ASVariables:    Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_AerosolScatter module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_ASVariables_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output AerosolScatter argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_AerosolScatter( Atmosphere    , &  ! Input
                                        SensorIndex   , &  ! Input
                                        ChannelIndex  , &  ! Input
                                        AerosolScatter, &  ! Output
                                        ASV           , &  ! Internal variable output
                                        Message_Log   ) &  ! Error messaging
                                      RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type) , INTENT(IN)     :: Atmosphere
    INTEGER                    , INTENT(IN)     :: SensorIndex
    INTEGER                    , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_AtmScatter_type) , INTENT(IN OUT) :: AerosolScatter
    TYPE(CRTM_ASVariables_type), INTENT(OUT)    :: ASV
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AerosolScatter'
    ! Local variables

    ! Set up
    Error_Status = SUCCESS

    ! Dummy placeholder
    ASV%Dummy=1

  END FUNCTION CRTM_Compute_AerosolScatter


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_AerosolScatter_TL
!
! PURPOSE:
!       Function to compute the tangent-linear aerosol absorption and 
!       scattering properties and populate the output AerosolScatter_TL
!       structure for a single channel.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_AerosolScatter_TL( Atmosphere             , &  ! Input
!                                                      AerosolScatter         , &  ! Input
!                                                      Atmosphere_TL          , &  ! Input
!                                                      SensorIndex            , &  ! Input
!                                                      ChannelIndex           , &  ! Input
!                                                      AerosolScatter_TL      , &  ! Output  
!                                                      ASVariables            , &  ! Internal variable input
!                                                      Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:         CRTM_Atmosphere structure containing the atmospheric
!                           profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Atmosphere_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AerosolScatter:     CRTM_AtmScatter structure containing the forward model
!                           aerosol absorption and scattering properties required
!                           for radiative transfer.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AtmScatter_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:      CRTM Atmosphere structure containing the tangent-linear
!                           atmospheric state data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Atmosphere_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:        Sensor index id. This is a unique index associated
!                           with a (supported) sensor used to access the
!                           shared coefficient data for a particular sensor.
!                           See the ChannelIndex argument.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:       Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access the
!                           shared coefficient data for a particular sensor's
!                           channel.
!                           See the SensorIndex argument.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       ASVariables:        Structure containing internal variables required for
!                           subsequent tangent-linear or adjoint model calls.
!                           The contents of this structure are NOT accessible
!                           outside of the CRTM_AerosolScatter module.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_ASVariables_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!        AerosolScatter_TL: CRTM_AtmScatter structure containing the tangent-linear
!                           aerosol absorption and scattering properties required
!                           for radiative transfer.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AtmScatter_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the computation was sucessful
!                              == FAILURE an unrecoverable error occurred
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output AerosolScatter_TL argument is IN OUT
!       rather than just OUT. This is necessary because the argument may be
!       defined upon input. To prevent memory leaks, the IN OUT INTENT is
!       a must.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_AerosolScatter_TL( Atmosphere       , &  ! Input
                                           AerosolScatter   , &  ! Input
                                           Atmosphere_TL    , &  ! Input
                                           SensorIndex      , &  ! Input
                                           ChannelIndex     , &  ! Input
                                           AerosolScatter_TL, &  ! Output
                                           ASV              , &  ! Internal variable input
                                           Message_Log      ) &  ! Error messaging
                                         RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type) , INTENT(IN)     :: Atmosphere
    TYPE(CRTM_AtmScatter_type) , INTENT(IN)     :: AerosolScatter
    TYPE(CRTM_Atmosphere_type) , INTENT(IN)     :: Atmosphere_TL
    INTEGER                    , INTENT(IN)     :: SensorIndex
    INTEGER                    , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_AtmScatter_type) , INTENT(IN OUT) :: AerosolScatter_TL
    TYPE(CRTM_ASVariables_type), INTENT(IN)     :: ASV
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AerosolScatter_TL'
    ! Local variables

    ! Set up
    Error_Status = SUCCESS


  END FUNCTION CRTM_Compute_AerosolScatter_TL


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_AerosolScatter_AD
!
! PURPOSE:
!       Function to compute the adjoint aerosol absorption and scattering
!       properties for a single channel.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_AerosolScatter_AD( Atmosphere             , &  ! Input
!                                                      AerosolScatter         , &  ! Input
!                                                      AerosolScatter_AD      , &  ! Input
!                                                      SensorIndex            , &  ! Input
!                                                      ChannelIndex           , &  ! Input
!                                                      Atmosphere_AD          , &  ! Output
!                                                      ASVariables            , &  ! Internal variable input
!                                                      Message_Log=Message_Log  )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Atmosphere:         CRTM_Atmosphere structure containing the atmospheric
!                           profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Atmosphere_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AerosolScatter:     CRTM_AtmScatter structure containing the forward model
!                           aerosol absorption and scattering properties required
!                           for radiative transfer.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AtmScatter_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AerosolScatter_AD:  CRTM_AtmScatter structure containing the adjoint
!                           aerosol absorption and scattering properties.
!                           **NOTE: On EXIT from this function, the contents of
!                                   this structure may be modified (e.g. set to
!                                   zero.)
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AtmScatter_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
!       SensorIndex:        Sensor index id. This is a unique index associated
!                           with a (supported) sensor used to access the
!                           shared coefficient data for a particular sensor.
!                           See the ChannelIndex argument.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:       Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access the
!                           shared coefficient data for a particular sensor's
!                           channel.
!                           See the SensorIndex argument.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
! 
!       ASVariables:        Structure containing internal variables required for
!                           subsequent tangent-linear or adjoint model calls.
!                           The contents of this structure are NOT accessible
!                           outside of the CRTM_AerosolScatter module.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_ASVariables_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:      CRTM Atmosphere structure containing the adjoint
!                           atmospheric state data.
!                           **NOTE: On ENTRY to this function, the contents of
!                                   this structure should be defined (e.g.
!                                   initialized to some value based on the
!                                   position of this function in the call chain.)
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Atmosphere_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the computation was sucessful
!                              == FAILURE an unrecoverable error occurred
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on all of the adjoint arguments (whether input or output)
!       is IN OUT rather than just OUT. This is necessary because the INPUT
!       adjoint arguments are modified, and the OUTPUT adjoint arguments must
!       be defined prior to entry to this routine. So, anytime a structure is
!       to be output, to prevent memory leaks the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_AerosolScatter_AD( Atmosphere       , &  ! Input
                                           AerosolScatter   , &  ! Input
                                           AerosolScatter_AD, &  ! Input
                                           SensorIndex      , &  ! Input
                                           ChannelIndex     , &  ! Input
                                           Atmosphere_AD    , &  ! Output
                                           ASV              , &  ! Internal variable input
                                           Message_Log      ) &  ! Error messaging
                                         RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type) , INTENT(IN)     :: Atmosphere
    TYPE(CRTM_AtmScatter_type) , INTENT(IN)     :: AerosolScatter
    TYPE(CRTM_AtmScatter_type) , INTENT(IN OUT) :: AerosolScatter_AD
    INTEGER                    , INTENT(IN)     :: SensorIndex
    INTEGER                    , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Atmosphere_type) , INTENT(IN OUT) :: Atmosphere_AD
    TYPE(CRTM_ASVariables_type), INTENT(IN)     :: ASV
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AerosolScatter_AD'
    ! Local variables

    ! Set up
    Error_Status = SUCCESS


  END FUNCTION CRTM_Compute_AerosolScatter_AD

END MODULE CRTM_AerosolScatter
