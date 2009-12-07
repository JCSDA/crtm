!
! CRTM_GeometryInfo
!
! Application module for the GeometryInfo structure.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 19-May-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_GeometryInfo

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds, ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, WARNING, FAILURE, Display_Message
  USE CRTM_Parameters, ONLY: ZERO, ONE, TWO, PI      , &
                             EARTH_RADIUS            , &
                             SATELLITE_HEIGHT        , &
                             DEGREES_TO_RADIANS      , &
                             MAX_SENSOR_ZENITH_ANGLE , &
                             MAX_SENSOR_AZIMUTH_ANGLE, &
                             MAX_SOURCE_AZIMUTH_ANGLE, &
                             MAX_FLUX_ZENITH_ANGLE   , &
                             DIFFUSIVITY_ANGLE       , &
                             DIFFUSIVITY_RADIAN      , &
                             SECANT_DIFFUSIVITY
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type  , &
                                      CRTM_Assign_GeometryInfo, &
                                      CRTM_Equal_GeometryInfo
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! CRTM_GeometryInfo structure data type
  ! in the CRTM_GeometryInfo_Define module
  PUBLIC :: CRTM_GeometryInfo_type
  ! CRTM_GeometryInfo structure routines inherited
  ! from the CRTM_GeometryInfo_Define module
  PUBLIC :: CRTM_Assign_GeometryInfo
  PUBLIC :: CRTM_Equal_GeometryInfo
  ! Public procedures
  PUBLIC :: CRTM_Compute_GeometryInfo

  PUBLIC :: SACONV 
  PUBLIC :: VACONV
  PUBLIC :: SACONV_TL 
  PUBLIC :: VACONV_TL
  PUBLIC :: SACONV_AD 
  PUBLIC :: VACONV_AD
 
  REAL(fp), PRIVATE,  PARAMETER :: THOUSAND = 1000.0_fp 

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*),  PARAMETER :: MODULE_RCS_ID = &
  '$Id$'


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_GeometryInfo
! 
! PURPOSE:
!       Function to compute the derived geometry from the user specified
!       components of the CRTM GeometryInfo structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_GeometryInfo( GeometryInfo           , &
!                                                 Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       GeometryInfo:  The GeometryInfo structure containing the user
!                      defined inputs, in particular the angles.
!                      UNITS:      N/A
!                      TYPE:       CRTM_GeometryInfo_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       GeometryInfo:  The GeometryInfo structure with the derived
!                      angle components filled..
!                      UNITS:      N/A
!                      TYPE:       CRTM_GeometryInfo_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to the screen.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER( * )
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the computation was sucessful
!                          == WARNING invalid data was found, but altered to default.
!                          == FAILURE invalid data was found
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function changes the values of the derived components of the
!       GeometryInfo structure argument.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_GeometryInfo( gInfo      , &  ! In/Output
                                      Message_Log) &  ! Optional input
                                    RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_GeometryInfo_type), INTENT(IN OUT) :: gInfo
    CHARACTER(*),       OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_GeometryInfo'

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Check sensor angles
    IF ( ABS(gInfo%Sensor_Zenith_Angle) > MAX_SENSOR_ZENITH_ANGLE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid sensor zenith angle', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( gInfo%Sensor_Azimuth_Angle < ZERO                     .OR. &
         gInfo%Sensor_Azimuth_Angle > MAX_SENSOR_AZIMUTH_ANGLE      ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid sensor azimuth angle. Setting to 0.0', &
                            Error_Status, &
                            Message_Log=Message_Log )
      gInfo%Sensor_Azimuth_Angle = ZERO
    END IF

    ! Check source angles
    IF ( gInfo%Source_Azimuth_Angle < ZERO                     .OR. &
         gInfo%Source_Azimuth_Angle > MAX_SOURCE_AZIMUTH_ANGLE      ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid source azimuth angle. Setting to 0.0', &
                            Error_Status, &
                            Message_Log=Message_Log )
      gInfo%Source_Azimuth_Angle = ZERO
    END IF

    ! Check flux angles
    IF ( ABS(gInfo%Flux_Zenith_Angle) > MAX_FLUX_ZENITH_ANGLE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid flux zenith angle', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Compute the derived components
    ! ------------------------------    
    ! Sensor angles
    gInfo%Sensor_Scan_Radian    = DEGREES_TO_RADIANS * gInfo%Sensor_Scan_Angle
    gInfo%Sensor_Zenith_Radian  = DEGREES_TO_RADIANS * gInfo%Sensor_Zenith_Angle
    gInfo%Sensor_Azimuth_Radian = DEGREES_TO_RADIANS * gInfo%Sensor_Azimuth_Angle
    gInfo%Secant_Sensor_Zenith  = ONE / COS(gInfo%Sensor_Zenith_Radian)
    
    ! Distance ratio. Only modify if zenith angle large enough.
    IF ( ABS(gInfo%Sensor_Zenith_Angle) > ONE ) THEN
      gInfo%Distance_Ratio = ABS(SIN(gInfo%Sensor_Scan_Radian)/SIN(gInfo%Sensor_Zenith_Radian))
    END IF

    ! Source angles
    gInfo%Source_Zenith_Radian  = DEGREES_TO_RADIANS * gInfo%Source_Zenith_Angle
    gInfo%Source_Azimuth_Radian = DEGREES_TO_RADIANS * gInfo%Source_Azimuth_Angle
    gInfo%Secant_Source_Zenith  = ONE / COS(gInfo%Source_Zenith_Radian)

    ! Flux angles
    gInfo%Flux_Zenith_Radian = DEGREES_TO_RADIANS * gInfo%Flux_Zenith_Angle
    gInfo%Secant_Flux_Zenith = ONE / COS(gInfo%Flux_Zenith_Radian)

    ! Square of ratio between mean and actual Sun-Earth distances
    ! see Liou, 2002: An Introduction to Atmospheric Radiation
    gInfo%AU_ratio2 = TWO * PI * (gInfo%Day_of_Year-ONE)/365.0_fp
    gInfo%AU_ratio2 = 1.00011_fp + 0.034221_fp * cos(gInfo%AU_ratio2)  &
                    + 0.00128_fp * sin(gInfo%AU_ratio2)  &
                    + 0.000719_fp * cos(TWO*gInfo%AU_ratio2)  &
                    + 0.000077_fp * sin(TWO*gInfo%AU_ratio2)
                    
  END FUNCTION CRTM_Compute_GeometryInfo

!----------------------------------------------------------------------------------
!S+
! NAME:
!      SACONV  
!
! PURPOSE:
!      Convert the surface Sensor zenith angle SZA into the
!      local Sensor angle at altitude ALT.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!        CALL    SACONV( Sensor_Zeith_Angle, & ! Input 
!                        Altitude,          &  ! Input
!                        Local_Sensor_Angle)   ! Output
! INPUT ARGUMENTS:
!       Sensor_Zeith_Angle: Sensor Zenith Angle SZA at the Earth's
!       		   surface  
!       		   UNITS:      degrees
!       		   TYPE:       REAL
!       		   DIMENSION:  Scalar
!       		   ATTRIBUTES: INTENT( IN )
!
!       Altitude:	   The Altitude in which the Sensor zenith 
!       		   angle convert to 
!       		   UNITS:      meters
!       		   TYPE:       REAL
!       		   DIMENSION:  Scalar
!       		   ATTRIBUTES: INTENT( IN )
!  
! OUTPUT ARGUMENTS:
!       Local_Sensor_Angle: The return value is the local zenith angle
!       		   UNITS:      RADIANS 
!       		   TYPE:       REAL
!       		   DIMENSION:  Scalar
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
!       		    
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 10-May-2006
!                       Yong.Chen@noaa.gov
!S-
!--------------------------------------------------------------------------------
  SUBROUTINE SACONV( Sensor_Zenith_Angle, & ! Input
                     Altitude,           & ! Input
                     Local_Zenith_Angle )  ! Output

    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp ), INTENT( IN ) :: Sensor_Zenith_Angle 
    REAL( fp ), INTENT( IN ) :: Altitude 
    
    ! -- Output
    REAL( fp ), INTENT( OUT )  :: Local_Zenith_Angle
 
    ! ---------------
    ! Local variables
    ! ---------------
 
    REAL( fp )  ::     RA   


    !  ------------------
    !  Assign some values
    !  ------------------
    !  RA = radius of the point to calc the angle at (in km)
       RA = EARTH_RADIUS + (Altitude/THOUSAND)

    !   -----------------
    !   Do the conversion
    !   -----------------
       Local_Zenith_Angle = ASIN( (EARTH_RADIUS/RA) * &
                      SIN(DEGREES_TO_RADIANS*Sensor_Zenith_Angle) )
 
  
  END SUBROUTINE SACONV


  SUBROUTINE SACONV_TL( Sensor_Zenith_Angle,     & ! Input
                        Altitude,               & ! Input
			Altitude_TL,            & ! Input
			Local_Zenith_Angle,     & ! Output
                        Local_Zenith_Angle_TL )   ! Output

    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp ), INTENT( IN ) :: Sensor_Zenith_Angle 
    REAL( fp ), INTENT( IN ) :: Altitude 
    REAL( fp ), INTENT( IN ) :: Altitude_TL 
    
    ! -- Output
    REAL( fp ), INTENT( OUT )  :: Local_Zenith_Angle
    REAL( fp ), INTENT( OUT )  :: Local_Zenith_Angle_TL
 
    ! ---------------
    ! Local variables
    ! ---------------
    REAL( fp )  ::     RA, RA_TL   


    !  ------------------
    !  Assign some values
    !  ------------------

    !  RA = radius of the point to calc the angle at (in km)
       RA = EARTH_RADIUS + (Altitude / THOUSAND)
       
       RA_TL = Altitude_TL/ THOUSAND 

    !   -----------------
    !   Do the conversion
    !   -----------------
       Local_Zenith_Angle = ASIN( (EARTH_RADIUS/RA) * &
                      SIN(DEGREES_TO_RADIANS*Sensor_Zenith_Angle) )
 
       Local_Zenith_Angle_TL = - RA_TL * EARTH_RADIUS * SIN(DEGREES_TO_RADIANS*Sensor_Zenith_Angle) &
                                / ( RA**TWO * COS( Local_Zenith_Angle ) ) 
  
  END SUBROUTINE SACONV_TL


  SUBROUTINE SACONV_AD( Sensor_Zenith_Angle,     & ! Input
                        Altitude,               & ! Input
			Local_Zenith_Angle_AD,  & ! Input
			Altitude_AD)              ! In/output
			 
    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp ), INTENT( IN ) :: Sensor_Zenith_Angle 
    REAL( fp ), INTENT( IN ) :: Altitude 
    REAL( fp ), INTENT( IN ) :: Local_Zenith_Angle_AD  
    
    ! -- Output
    REAL( fp ), INTENT( IN OUT )  :: Altitude_AD
 
    ! ---------------
    ! Local variables
    ! ---------------
 
    REAL( fp )  ::     Local_Zenith_Angle  
    REAL( fp )  ::     RA, RA_AD   


    !  ------------------
    !  Assign some values
    !  ------------------
    !  RA = radius of the point to calc the angle at (in km)
       RA = EARTH_RADIUS + (Altitude / THOUSAND)
       
    !   -----------------
    !   Do the conversion
    !   -----------------
       Local_Zenith_Angle = ASIN( (EARTH_RADIUS/RA) * &
                      SIN(DEGREES_TO_RADIANS*Sensor_Zenith_Angle) )

    ! Adjoint Model
       RA_AD = - Local_Zenith_Angle_AD * EARTH_RADIUS * SIN(DEGREES_TO_RADIANS*Sensor_Zenith_Angle) &
                                / ( RA**TWO * COS( Local_Zenith_Angle ) ) 
    
       Altitude_AD = Altitude_AD + RA_AD / THOUSAND
   
  END SUBROUTINE SACONV_AD

  
!----------------------------------------------------------------------------------
!S+
! NAME:
!      VACONV  
!
! PURPOSE:
!      Convert the AIRS satellite viewing angle into the
!      local path angle. 
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!        CALL       VACONV( Sensor_View_Angle,  & ! Input
!                           Satellite_Altitude, & ! Input
!                           Altitude,           & ! Input
!                           Local_Path_Angle)     ! Output
!
! INPUT ARGUMENTS:
!       Sensor_View_Angle: Sensor viewing Angle SVA at the Earth's
!       		   surface  
!       		   UNITS:      degrees
!       		   TYPE:       REAL
!       		   DIMENSION:  Scalar
!
!       Satellite_Altitude:The Satellite altitude  
!       		   UNITS:      kilometers
!       		   TYPE:       REAL
!       		   DIMENSION:  Scalar
!       		   ATTRIBUTES: INTENT( IN )
!
!       Altitude:	   The Altitude in which the local path  
!       		   angle convert to 
!       		   UNITS:      meters
!       		   TYPE:       REAL
!       		   DIMENSION:  Scalar
!       		   ATTRIBUTES: INTENT( IN )
!  
! OUTPUT ARGUMENTS:
!       Local_Path_Angle:  The return value is the local path angle
!       		   UNITS:      RADIANS 
!       		   TYPE:       REAL
!       		   DIMENSION:  Scalar
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
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 10-May-2006
!                       Yong.Chen@noaa.gov
!S-
!--------------------------------------------------------------------------------
  SUBROUTINE VACONV(  Sensor_View_Angle,  & ! Input
                      Satellite_Altitude, & ! Input
                      Altitude,           & ! Input
                      Local_Path_Angle )    ! Output

    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp ), INTENT( IN ) :: Sensor_View_Angle 
    REAL( fp ), INTENT( IN ) :: Satellite_Altitude 
    REAL( fp ), INTENT( IN ) :: Altitude 
    
    ! -- Output
    REAL( fp ), INTENT( OUT )  :: Local_Path_Angle
 
    ! ---------------
    ! Local variables
    ! ---------------
 
    REAL( fp )  ::     RA   
    REAL( fp )  ::     RS   


    !  ------------------
    !  Assign some values
    !  ------------------
    !  RA = radius of the point to calc the angle at (in km)
       RA = EARTH_RADIUS + (Altitude / THOUSAND)
       
    !  RS = radius of the satellite orbit (in km)
       RS = EARTH_RADIUS + Satellite_Altitude

    !   -----------------
    !   Do the conversion
    !   -----------------
       Local_Path_Angle = ASIN( (RS/RA) * &
                      SIN(DEGREES_TO_RADIANS*Sensor_View_Angle) )
 
  
  END SUBROUTINE VACONV

  SUBROUTINE VACONV_TL( Sensor_View_Angle,  & ! Input
                        Satellite_Altitude, & ! Input
                        Altitude,           & ! Input
			Altitude_TL,        & ! Input
			Local_Path_Angle,   & ! Output
                        Local_Path_Angle_TL )   ! Output
 
    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp ), INTENT( IN ) :: Sensor_View_Angle 
    REAL( fp ), INTENT( IN ) :: Satellite_Altitude 
    REAL( fp ), INTENT( IN ) :: Altitude 
    REAL( fp ), INTENT( IN ) :: Altitude_TL 
    
    ! -- Output
    REAL( fp ), INTENT( OUT )  :: Local_Path_Angle
    REAL( fp ), INTENT( OUT )  :: Local_Path_Angle_TL
 
    ! ---------------
    ! Local variables
    ! ---------------
 
    REAL( fp )  ::     RA, RA_TL   
    REAL( fp )  ::     RS   


    !  ------------------
    !  Assign some values
    !  ------------------
    !  RA = radius of the point to calc the angle at (in km)
       RA = EARTH_RADIUS + (Altitude / THOUSAND)
       
       RA_TL =  Altitude_TL / THOUSAND      
    !  RS = radius of the satellite orbit (in km)
       RS = EARTH_RADIUS + Satellite_Altitude

    !   -----------------
    !   Do the conversion
    !   -----------------
       Local_Path_Angle = ASIN( (RS/RA) * &
                      SIN(DEGREES_TO_RADIANS*Sensor_View_Angle) )
 
       Local_Path_Angle_TL = - RA_TL * RS * SIN(DEGREES_TO_RADIANS*Sensor_View_Angle) &
                                / ( RA**TWO * COS( Local_Path_Angle ) ) 
  
  END SUBROUTINE VACONV_TL


  SUBROUTINE VACONV_AD( Sensor_View_Angle,        & ! Input
                        Satellite_Altitude,       & ! Input
                        Altitude,                 & ! Input
			Local_PATH_Angle_AD,      & ! Input
                        Altitude_AD )               ! In/Output
 
    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp ), INTENT( IN ) :: Sensor_View_Angle 
    REAL( fp ), INTENT( IN ) :: Satellite_Altitude 
    REAL( fp ), INTENT( IN ) :: Altitude 
    REAL( fp ), INTENT( IN ) :: Local_Path_Angle_AD 
    
    ! -- Output
    REAL( fp ), INTENT( IN OUT )  :: Altitude_AD
 
    ! ---------------
    ! Local variables
    ! ---------------
 
    REAL( fp )  ::     RA, RA_AD   
    REAL( fp )  ::     RS   
    REAL( fp )  :: Local_Path_Angle


    !  ------------------
    !  Assign some values
    !  ------------------
    !  RA = radius of the point to calc the angle at (in km)
       RA = EARTH_RADIUS + (Altitude / THOUSAND)
       
    !  RS = radius of the satellite orbit (in km)
       RS = EARTH_RADIUS + Satellite_Altitude

    !   -----------------
    !   Do the conversion
    !   -----------------
       Local_Path_Angle = ASIN( (RS/RA) * &
                      SIN(DEGREES_TO_RADIANS*Sensor_View_Angle) )
 
    ! Adjoint Model
       RA_AD = - Local_Path_Angle_AD * RS * SIN(DEGREES_TO_RADIANS*Sensor_View_Angle) &
                                / ( RA**TWO * COS( Local_Path_Angle ) ) 
    
       Altitude_AD = Altitude_AD + RA_AD / THOUSAND
  
  END SUBROUTINE VACONV_AD
END MODULE CRTM_GeometryInfo
