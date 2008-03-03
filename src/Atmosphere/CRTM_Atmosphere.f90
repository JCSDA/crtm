!
! CRTM_Atmosphere
!
! Module for adding layers to the CRTM atmosphere structure as required.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 29-Oct-2007
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_Atmosphere

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds            , ONLY: fp
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters       , ONLY: ZERO, POINT_5, SET, TOA_PRESSURE
  USE CRTM_Atmosphere_Define, ONLY: CRTM_Atmosphere_type    , &
                                    CRTM_Allocate_Atmosphere, &
                                    CRTM_Assign_Atmosphere  , &  
                                    CRTM_Sum_Atmosphere     , &
                                    CRTM_Zero_Atmosphere
  USE CRTM_Model_Profiles   , ONLY: MODEL_LEVEL_PRESSURE, & 
                                    CRTM_Get_Model_Profile  
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Module procedures
  PUBLIC :: CRTM_AddLayers_Atmosphere
  PUBLIC :: CRTM_AddLayers_Atmosphere_TL
  PUBLIC :: CRTM_AddLayers_Atmosphere_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_AddLayers_Atmosphere
!
! PURPOSE:
!       Function to copy an atmosphere structure and adding extra layers from
!       climatology as required to supplement the upper atmosphere profile data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_AddLayers_Atmosphere( Atm_In                 , &  ! Input
!                                                 Atm_Out                , &  ! Output
!                                                 Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atm_In:          Atmosphere structure that is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Atm_Out:         Copy of the input atmosphere structure with extra upper
!                        atmosphere layers added as required.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
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
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the operation was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Atm_Out argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_AddLayers_Atmosphere( Atm_In     , &  ! Input
                                      Atm_Out    , &  ! Output
                                      Message_Log) &  ! Error messaging
                                    RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm_In
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_Out
    CHARACTER(*), OPTIONAL    , INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AddLayers_Atmosphere'
    REAL(fp)    , PARAMETER :: MINIMUM_ABSORBER_AMOUNT = 1.0e-06_fp
    ! Local variables
    INTEGER :: Allocate_Status
    INTEGER :: i, j, k, n
    REAL(fp), ALLOCATABLE :: pl(:), tl(:), al(:,:) ! Level arrays
    REAL(fp), ALLOCATABLE :: p(:) , t(:) , a(:,:)  ! Layer arrays
    REAL(fp) :: lpoly
    REAL(fp) :: t_int, dt
    REAL(fp) :: a_int, da


    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! If extra layers are NOT needed,
    ! then simply copy the structure
    ! -------------------------------
    IF ( Atm_In%Level_Pressure(0) <= TOA_PRESSURE) THEN
      Error_Status = CRTM_Assign_Atmosphere( Atm_In, Atm_Out, &
                                             Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error assigning Atmosphere structure with NO extra layers', &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
      RETURN
    END IF
    
    
    ! Determine how many extra layers are needed
    ! ------------------------------------------
    n = Extra_Layers( Atm_In )


    ! Get the extra layer profiles
    ! ----------------------------
    ! Allocate temporary arrays
    ALLOCATE( pl(0:n), tl(0:n), al(0:n,Atm_In%n_Absorbers), &  ! Level arrays
              p(n)   , t(n)   , a(n,Atm_In%n_Absorbers)   , &  ! Layer arrays
              STAT=Allocate_Status )
    IF ( Allocate_Status/= 0 ) THEN 
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating temporary profile arrays', & 
                            Error_Status, &
                            Message_Log=Message_Log )  
      RETURN
    END IF

    ! Fill them
    CALL CRTM_Get_Model_Profile( pl, tl, al, Model=Atm_In%Climatology )

    ! First interpolate the extra levels to the user top pressure
    lpoly = Interp_LPoly( Atm_In%Level_Pressure(0), pl(n-1:n) )
    pl(n) = Atm_In%Level_Pressure(0)                ! Make pl(n) == Atm_In%Level_Pressure(0)
    t_int = Interp_Linear( lpoly, tl(n-1:n) )
    tl(n) = t_int                                   ! Make tl(n) == "Atm_In%Level_Temperature(0)"
    DO j = 1, Atm_In%n_Absorbers
      a_int = Interp_Linear( lpoly, al(n-1:n,j) )
      al(n,j) = a_int                               ! Make al(n) == "Atm_In%Level_Absorber(0)"
    END DO
    
    ! Now compute the model profile layer averages
    DO k = 1, n
      p(k) = Layer_P(pl(k-1:k))
      t(k) = Layer_X(tl(k-1:k))
    END DO
    DO j = 1, Atm_In%n_Absorbers
      DO k = 1, n
        a(k,j) = Layer_X(al(k-1:k,j))
      END DO
    END DO
    
    ! Now, extrapolate user layer profile to get the "layer 0" value and
    ! use it to shift the model profile to avoid a discontinuity at p(n)
    lpoly = Interp_LPoly( p(n), Atm_In%Pressure(1:2) )
    t_int = Interp_Linear( lpoly, Atm_In%Temperature(1:2) )
    dt = t_int - t(n)
    t  = t + dt
    DO j = 1, Atm_In%n_Absorbers
      a_int = Interp_Linear( lpoly, Atm_In%Absorber(1:2,j) )
      da = a_int - a(n,j)
      a(:,j) = a(:,j) + da
    END DO
    
    ! Make sure the absorber amounts are not negative.
    ! (Is a further, more physical, check needed here?)
    WHERE (a < ZERO) a = MINIMUM_ABSORBER_AMOUNT
    

    ! Copy over the atmosphere structure with extra layers
    ! ----------------------------------------------------
    Error_Status = CRTM_Assign_Atmosphere( Atm_In, Atm_Out, &
                                           n_Added_Layers = n, &
                                           Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error assigning Atmosphere structure with extra layers', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Slot the added layers into the output atmosphere structure
    ! Note: Cloud and Aerosol assignments not really needed (the
    !       zeroing is handled by the structure allocation) since
    !       at TOA, typically, there are not any clouds and/or
    !       aerosols.
    ! -----------------------------------------------------------
    ! Profile
    Atm_Out%Level_Pressure(0:n) = pl
    Atm_Out%Pressure(1:n)       = p
    Atm_Out%Temperature(1:n)    = t
    DO j = 1, Atm_Out%n_Absorbers
      Atm_Out%Absorber(1:n,j)   = a(:,j)
    END DO
    
    ! Clouds
    IF ( Atm_In%n_Clouds > 0 ) THEN
      DO i = 1, Atm_In%n_Clouds
        Atm_Out%Cloud(i)%Effective_Radius(1:n)   = ZERO
        Atm_Out%Cloud(i)%Effective_Variance(1:n) = ZERO
        Atm_Out%Cloud(i)%Water_Content(1:n)      = ZERO
      END DO
    END IF
  
    ! Aerosols
    IF ( Atm_In%n_Aerosols > 0 ) THEN
      DO i = 1, Atm_In%n_Aerosols
        Atm_Out%Aerosol(i)%Effective_Radius(1:n) = ZERO
        Atm_Out%Aerosol(i)%Concentration(1:n)    = ZERO
      END DO
    END IF


    ! Clean up
    ! --------
    DEALLOCATE( pl, tl, al, &  ! Level arrays
                p , t , a , &  ! Layer arrays
                STAT=Allocate_Status )
    IF ( Allocate_Status/= 0 ) THEN 
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating temporary profile arrays', & 
                            Error_Status, &
                            Message_Log=Message_Log )  
      RETURN
    END IF

  CONTAINS

    !#--------------------------------------------------------------------------#
    !#                         -- INTERNAL SUBPROGAMS --                        #
    !#--------------------------------------------------------------------------#
    
    ! ------------------------------
    ! Internal subprogram to compute
    ! the average layer pressure
    ! ------------------------------
    FUNCTION Layer_P( p ) RESULT( p_layer )
      REAL(fp), INTENT(IN) :: p(2)
      REAL(fp) :: p_layer
      p_layer = (p(2)-p(1))/LOG(p(2)/p(1))
    END FUNCTION Layer_P

    ! ------------------------------
    ! Internal subprogram to compute
    ! the average layer amount of X
    ! ------------------------------
    FUNCTION Layer_X( x ) RESULT( x_layer )
      REAL(fp), INTENT(IN) :: x(2)
      REAL(fp) :: x_layer
      x_layer = POINT_5*(x(1)+x(2))
    END FUNCTION Layer_X
    
    ! ------------------------------------------------
    ! Internal subprogram to compute the interpolating
    ! polynomial linear in log(p)
    ! ------------------------------------------------
    FUNCTION Interp_LPoly( p_int, p ) RESULT( lpoly )
      REAL(fp), INTENT(IN) :: p_int
      REAL(fp), INTENT(IN) :: p(2)
      REAL(fp) :: lpoly
      lpoly = (LOG(p_int)-LOG(p(1))) / (LOG(p(2))-LOG(p(1)))
    END FUNCTION Interp_LPoly

    ! ------------------------------    
    ! Internal subprogram to perform
    ! linear interpolation
    ! ------------------------------    
    FUNCTION Interp_Linear( lpoly, x ) RESULT( x_int )
      REAL(fp), INTENT(IN) :: lpoly
      REAL(fp), INTENT(IN) :: x(2)
      REAL(fp) :: x_int
      x_int = (x(2)-x(1))*lpoly + x(1)
    END FUNCTION Interp_Linear
                                                                   
  END FUNCTION CRTM_AddLayers_Atmosphere


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_AddLayers_Atmosphere_TL
!
! PURPOSE:
!       Function to copy a tangent-linear atmosphere structure and add extra
!       layers as required to supplement the upper atmosphere profile data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_AddLayers_Atmosphere_TL( Atm_In                 , &  ! FWD Input
!                                                    Atm_In_TL              , &  ! TL  Input
!                                                    Atm_Out_TL             , &  ! TL  Output
!                                                    Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atm_In:          Forward model atmosphere structure.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atm_In_TL:       Tangent-linear model atmosphere structure that is
!                        to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Atm_Out_TL:      Copy of the input tangent-linear atmosphere structure
!                        with extra upper atmosphere layers added as required.
!                        Note that the tangent-linear values of the added layers
!                        is *always* zero.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
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
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the operation was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Atm_Out_TL argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_AddLayers_Atmosphere_TL( Atm_In     , &  ! FWD Input
                                         Atm_In_TL  , &  ! TL  Input
                                         Atm_Out_TL , &  ! TL  Output
                                         Message_Log) &  ! Error messaging
                                       RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm_In
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm_In_TL
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_Out_TL
    CHARACTER(*), OPTIONAL    , INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AddLayers_Atmosphere_TL'
    ! Local variables
    INTEGER :: n


    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! If extra layers are NOT needed,
    ! then simply copy the structure
    ! -------------------------------
    IF ( Atm_In%Level_Pressure(0) <= TOA_PRESSURE) THEN
      Error_Status = CRTM_Assign_Atmosphere( Atm_In_TL, Atm_Out_TL, &
                                             Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error assigning Atmosphere structure with NO extra layers', &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
      RETURN
    END IF
    
    
    ! Determine how many extra layers are needed
    ! ------------------------------------------
    n = Extra_Layers( Atm_In )


    ! Copy over the atmosphere structure with extra layers
    ! (which will be zero by definition)
    ! ----------------------------------------------------
    Error_Status = CRTM_Assign_Atmosphere( Atm_In_TL, Atm_Out_TL, &
                                           n_Added_Layers = n, &
                                           Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error assigning Atmosphere structure with extra layers', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
  END FUNCTION CRTM_AddLayers_Atmosphere_TL
  
  
!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_AddLayers_Atmosphere_AD
!
! PURPOSE:
!       Function to copy back an adjoint atmosphere structure removing added
!       extra layers as were required to supplement the upper atmosphere
!       profile data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_AddLayers_Atmosphere_AD( Atm_In                 , &  ! FWD Input
!                                                    Atm_Out_AD             , &  ! AD  Input
!                                                    Atm_In_AD              , &  ! AD  Output
!                                                    Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atm_In:          Forward model atmosphere structure.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atm_Out_AD:      Adjoint atmosphere structure that contains the added
!                        extra layers.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       Atm_In_AD:       Adjoint atmosphere structure at the original, user
!                        specified layering.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
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
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the operation was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The input adjoint atmosphere structure, Atm_Out_AD, is zeroed out
!       prior to returning to the calling procedure.
!
! COMMENTS:
!       Note the INTENT on the output Atm_In_AD argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_AddLayers_Atmosphere_AD( Atm_In     , &  ! FWD Input
                                         Atm_Out_AD , &  ! AD  Input
                                         Atm_In_AD  , &  ! AD  Output
                                         Message_Log) &  ! Error messaging
                                       RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm_In
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_Out_AD
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_In_AD
    CHARACTER(*), OPTIONAL    , INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AddLayers_Atmosphere_AD'
    ! Local variables
    INTEGER :: i, j, n, no, nt


    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! If extra layers are NOT needed, then simply perform
    ! the adjoint sum. Remember the TL form is
    !   Atm_Out_TL = Atm_In_TL
    ! so the adjoint form is
    !   Atm_In_AD  = Atm_In_AD + Atm_Out_AD
    !   Atm_Out_AD = ZERO
    ! -------------------------------
    IF ( Atm_In%Level_Pressure(0) <= TOA_PRESSURE) THEN
      Error_Status = CRTM_Sum_Atmosphere( Atm_In_AD, Atm_Out_AD, &
                                          Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error summing Atm_In_AD atmosphere structure with NO extra layers', &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
      CALL CRTM_Zero_Atmosphere( Atm_Out_AD )
      RETURN
    END IF
    
    
    ! Determine how many extra layers have been used
    ! ----------------------------------------------
    n = Extra_Layers( Atm_In )


    ! Perform the adjoint summations
    ! ------------------------------
    no = Atm_In_AD%n_Layers
    nt = n + no

    ! Aerosols
    IF ( Atm_In_AD%n_Aerosols > 0 ) THEN
      DO i = 1, Atm_In_AD%n_Aerosols
        Atm_In_AD%Aerosol(i)%Concentration(1:no) = Atm_In_AD%Aerosol(i)%Concentration(1:no) + &
                                                   Atm_Out_AD%Aerosol(i)%Concentration(n+1:nt)
        
        Atm_In_AD%Aerosol(i)%Effective_Radius(1:no) = Atm_In_AD%Aerosol(i)%Effective_Radius(1:no) + &
                                                      Atm_Out_AD%Aerosol(i)%Effective_Radius(n+1:nt)

        Atm_In_AD%Aerosol(i)%Type = Atm_Out_AD%Aerosol(i)%Type
      END DO
    END IF

    ! Clouds    
    IF ( Atm_In_AD%n_Clouds > 0 ) THEN
      DO i = 1, Atm_In_AD%n_Clouds
        Atm_In_AD%Cloud(i)%Water_Content(1:no) = Atm_In_AD%Cloud(i)%Water_Content(1:no) + &
                                                 Atm_Out_AD%Cloud(i)%Water_Content(n+1:nt)
        
        Atm_In_AD%Cloud(i)%Effective_Variance(1:no) = Atm_In_AD%Cloud(i)%Effective_Variance(1:no) + &
                                                      Atm_Out_AD%Cloud(i)%Effective_Variance(n+1:nt)

        Atm_In_AD%Cloud(i)%Effective_Radius(1:no) = Atm_In_AD%Cloud(i)%Effective_Radius(1:no) + &
                                                    Atm_Out_AD%Cloud(i)%Effective_Radius(n+1:nt)

        Atm_In_AD%Cloud(i)%Type = Atm_Out_AD%Cloud(i)%Type
      END DO
    END IF

    ! Absorber data
    DO j = 1, Atm_In_AD%n_Absorbers
      Atm_In_AD%Absorber(1:no,j) = Atm_In_AD%Absorber(1:no,j) + Atm_Out_AD%Absorber(n+1:nt,j)
    END DO

    ! Temperature data
    Atm_In_AD%Temperature(1:no) = Atm_In_AD%Temperature(1:no) + Atm_Out_AD%Temperature(n+1:nt)

    ! Pressure data
    Atm_In_AD%Pressure(1:no)       = Atm_In_AD%Pressure(1:no) + Atm_Out_AD%Pressure(n+1:nt)
    Atm_In_AD%Level_Pressure(0:no) = Atm_In_AD%Level_Pressure(0:no) + Atm_Out_AD%Level_Pressure(n:nt)


    ! Zero the output atmosphere structure
    ! ------------------------------------
    CALL CRTM_Zero_Atmosphere( Atm_Out_AD )

  END FUNCTION CRTM_AddLayers_Atmosphere_AD


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! ---------------------------------------------
  ! Determine the number of extra layers required
  ! ---------------------------------------------
  FUNCTION Extra_Layers( Atm ) RESULT( n )
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atm
    INTEGER :: n
    ! NOTE: Assumption here is that the lowest input pressure
    !       is *always* less than the highest model pressure.
    n = MINLOC(Atm%Level_Pressure(0)-MODEL_LEVEL_PRESSURE, &
               DIM=1, &
               MASK=(Atm%Level_Pressure(0)-MODEL_LEVEL_PRESSURE) > ZERO)
  END FUNCTION Extra_Layers
  
END MODULE CRTM_Atmosphere
