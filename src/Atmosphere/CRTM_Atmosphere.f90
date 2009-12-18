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
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE CRTM_Parameters       , ONLY: ZERO, ONE, POINT_5, SET, &
                                    TOA_PRESSURE           , &
                                    MINIMUM_ABSORBER_AMOUNT
  USE CRTM_Atmosphere_Define, ONLY: CRTM_Atmosphere_type    , &
                                    OPERATOR(==), &
                                    OPERATOR(+), &
                                    CRTM_Atmosphere_Associated, &
                                    CRTM_Atmosphere_Create, &
                                    CRTM_Atmosphere_AddLayerCopy, &
                                    CRTM_Atmosphere_Zero
  USE CRTM_Model_Profiles   , ONLY: MODEL_LEVEL_PRESSURE, & 
                                    CRTM_Get_Model_Profile
  ! ...Internal variable definition module
  USE iAtm_Define,            ONLY: iAtm_type      , &
                                    iAtm_Associated, &
                                    iAtm_Create    , &
                                    iAtm_Destroy
                                    
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Module procedures
  PUBLIC :: CRTM_Atmosphere_AddLayers
  PUBLIC :: CRTM_Atmosphere_AddLayers_TL
  PUBLIC :: CRTM_Atmosphere_AddLayers_AD
  ! iAtm entities
  ! ...Structure
  PUBLIC :: iAtm_type      
  ! ...Procedures
  PUBLIC :: iAtm_Associated
  PUBLIC :: iAtm_Create
  PUBLIC :: iAtm_Destroy


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
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_AddLayers
!
! PURPOSE:
!       Function to copy an atmosphere structure and adding extra layers from
!       climatology as required to supplement the upper atmosphere profile data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Atmosphere_AddLayers( Atm_In , &  ! Input
!                                                 Atm_Out, &  ! Output
!                                                 iAtm     )  ! Internal variable output
!
! INPUTS:
!       Atm_In:          Atmosphere structure that is to be supplemented
!                        or copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Atm_Out:         Copy of the input atmosphere structure with extra upper
!                        atmosphere layers added as required.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       iAtm:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure should not be used
!                        outside of this module, i.e. CRTM_Atmosphere.
!                        UNITS:      N/A
!                        TYPE:       TYPE(iAtm_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
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
!       Note the INTENT on the output structure argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Atmosphere_AddLayers( &
    Atm_In , &  ! Input
    Atm_Out, &  ! Output
    iAtm   ) &  ! Internal variable output
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm_In
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_Out
    TYPE(iAtm_type)           , INTENT(OUT)    :: iAtm
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_AddLayers'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: i, j, k, n


    ! Set up
    err_stat = SUCCESS


    ! If extra layers are NOT needed,
    ! then simply copy the structure
    IF ( Atm_In%Level_Pressure(0) <= TOA_PRESSURE) THEN
      Atm_Out = Atm_In
      IF ( .NOT. CRTM_Atmosphere_Associated( Atm_Out ) ) THEN
        err_stat = FAILURE
        msg = 'Error assigning Atmosphere structure with NO extra layers'
        CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
      END IF
      RETURN
    END IF
    
    
    ! Allocate the internal variable structure
    n = Extra_Layers( Atm_In )
    CALL iAtm_Create( iAtm, n, Atm_In%n_Absorbers )
    IF ( .NOT. iAtm_Associated( iAtm ) ) THEN
      err_stat = FAILURE
      msg = 'Error allocating iAtm internal structure'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
      RETURN
    END IF


    ! Get the extra layer profiles
    CALL CRTM_Get_Model_Profile( iAtm%pl, iAtm%tl, iAtm%al, Model=Atm_In%Climatology )


    ! First interpolate the extra levels to the user top pressure
    ! replacing the model data at that array index
    CALL Interp_LPoly( Atm_In%Level_Pressure(0), iAtm%pl(n-1:n), iAtm%ilpoly )
    iAtm%plint_save = Atm_In%Level_Pressure(0)
    iAtm%pln_save   = iAtm%pl(n)
    iAtm%pl(n)      = iAtm%plint_save
    CALL Interp_Linear( iAtm%ilpoly, iAtm%tl(n-1:n), iAtm%tlint_save )
    iAtm%tln_save = iAtm%tl(n)
    iAtm%tl(n)    = iAtm%tlint_save
    DO j = 1, Atm_In%n_Absorbers
      CALL Interp_Linear( iAtm%ilpoly, iAtm%al(n-1:n,j), iAtm%alint_save(j) )
      iAtm%aln_save(j) = iAtm%al(n,j)
      iAtm%al(n,j)     = iAtm%alint_save(j)
    END DO
    
    ! Now compute the model profile layer averages
    DO k = 1, n
      CALL Layer_P(iAtm%pl(k-1:k), iAtm%p(k))
      CALL Layer_X(iAtm%tl(k-1:k), iAtm%t(k))
    END DO
    DO j = 1, Atm_In%n_Absorbers
      DO k = 1, n
        CALL Layer_X(iAtm%al(k-1:k,j), iAtm%a(k,j))
      END DO
    END DO
    
    
    ! Now, extrapolate user layer profile to get the "layer 0" value and
    ! use it to shift the model profile to avoid a discontinuity at p(n)
    CALL Interp_LPoly( iAtm%p(n), Atm_In%Pressure(1:2), iAtm%elpoly )
    CALL Shift_Profile( iAtm%elpoly, Atm_In%Temperature(1:2), iAtm%t )
    DO j = 1, Atm_In%n_Absorbers
      CALL Shift_Profile( iAtm%elpoly, Atm_In%Absorber(1:2,j), iAtm%a(:,j) )
    END DO


    ! Make sure the absorber amounts are not negative.
    ! (Is a further, more physical, check needed here?)
    iAtm%a_save = iAtm%a
    WHERE (iAtm%a_save < ZERO) iAtm%a = MINIMUM_ABSORBER_AMOUNT
    

    ! Copy over the atmosphere structure with extra layers
    atm_out = CRTM_Atmosphere_AddLayerCopy( Atm_In, n )
    IF ( .NOT. CRTM_Atmosphere_Associated( atm_out ) ) THEN
      err_stat = FAILURE
      msg = 'Error copying Atmosphere structure with extra layers'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
      RETURN
    END IF


    ! Slot the added layers into the output atmosphere structure
    ! Note: Cloud and Aerosol assignments not really needed (the
    !       zeroing is handled by the structure allocation) since
    !       at TOA, typically, there are not any clouds and/or
    !       aerosols.
    ! ...Profile
    Atm_Out%Level_Pressure(0:n) = iAtm%pl
    Atm_Out%Pressure(1:n)       = iAtm%p
    Atm_Out%Temperature(1:n)    = iAtm%t
    DO j = 1, Atm_Out%n_Absorbers
      Atm_Out%Absorber(1:n,j)   = iAtm%a(:,j)
    END DO
    ! ...Clouds
    IF ( Atm_In%n_Clouds > 0 ) THEN
      DO i = 1, Atm_In%n_Clouds
        Atm_Out%Cloud(i)%Effective_Radius(1:n)   = ZERO
        Atm_Out%Cloud(i)%Effective_Variance(1:n) = ZERO
        Atm_Out%Cloud(i)%Water_Content(1:n)      = ZERO
      END DO
    END IF
    ! ...Aerosols
    IF ( Atm_In%n_Aerosols > 0 ) THEN
      DO i = 1, Atm_In%n_Aerosols
        Atm_Out%Aerosol(i)%Effective_Radius(1:n) = ZERO
        Atm_Out%Aerosol(i)%Concentration(1:n)    = ZERO
      END DO
    END IF

  END FUNCTION CRTM_Atmosphere_AddLayers


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_AddLayers_TL
!
! PURPOSE:
!       Function to copy a tangent-linear atmosphere structure and add extra
!       layers as required to supplement the upper atmosphere profile data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Atmosphere_AddLayers_TL( Atm_In    , &  ! FWD Input
!                                                    Atm_In_TL , &  ! TL  Input
!                                                    Atm_Out_TL, &  ! TL  Output
!                                                    iAtm        )  ! Internal variable input
!
! INPUTS:
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
!       iAtm:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure should not be used
!                        outside of this module, i.e. CRTM_Atmosphere.
!                        UNITS:      N/A
!                        TYPE:       TYPE(iAtm_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Atm_Out_TL:      Copy of the input tangent-linear atmosphere structure
!                        with extra upper atmosphere layers added as required.
!                        Note that the tangent-linear values of the added layers
!                        is *always* zero.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
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
!       Note the INTENT on the output structure argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Atmosphere_AddLayers_TL( &
    Atm_In    , &  ! FWD Input
    Atm_In_TL , &  ! TL  Input
    Atm_Out_TL, &  ! TL  Output
    iAtm      ) &  ! Internal variable input
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm_In
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm_In_TL
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_Out_TL
    TYPE(iAtm_type)           , INTENT(IN)     :: iAtm
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_AddLayers_TL'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: alloc_stat
    INTEGER :: i, j, k, n
    REAL(fp), ALLOCATABLE :: pl_TL(:), tl_TL(:), al_TL(:,:) ! Level arrays
    REAL(fp), ALLOCATABLE :: p_TL(:) , t_TL(:) , a_TL(:,:)  ! Layer arrays
    REAL(fp) :: lpoly_TL
    REAL(fp) :: t_int_TL
    REAL(fp) :: a_int_TL
    TYPE(iAtm_type) :: l_iAtm


    ! Set up
    err_stat = SUCCESS


    ! If extra layers are NOT needed,
    ! then simply copy the structure
    IF ( Atm_In%Level_Pressure(0) <= TOA_PRESSURE) THEN
      Atm_Out_TL = Atm_In_TL
      IF ( .NOT. CRTM_Atmosphere_Associated( Atm_Out_TL ) ) THEN
        err_stat = FAILURE
        msg = 'Error assigning Atmosphere structure with NO extra layers'
        CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
      END IF
      RETURN
    END IF
    
    ! Determine how many extra layers are needed
    n = Extra_Layers( Atm_In )

    ! Allocate temporary arrays
    ALLOCATE( pl_TL(0:n), tl_TL(0:n), al_TL(0:n,Atm_In%n_Absorbers), &  ! Level arrays
              p_TL(n)   , t_TL(n)   , a_TL(n,Atm_In%n_Absorbers)   , &  ! Layer arrays
              STAT=alloc_stat )
    IF ( alloc_stat/= 0 ) THEN 
      err_stat = FAILURE
      WRITE(msg,'("Error allocating temporary profile arrays. STAT=",i0)' ) alloc_stat
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
      RETURN
    END IF
    
    ! Initialise them
    pl_TL = ZERO
    tl_TL = ZERO
    al_TL = ZERO
    
    ! Copy the input internal variable structure for modification
    l_iAtm = iAtm
    IF ( .NOT. iAtm_Associated( l_iAtm ) ) THEN
      err_stat = FAILURE
      msg = 'Error copying iAtm internal variable structure'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
      RETURN
    END IF
    
    ! First interpolate the extra levels to the user top pressure
    ! replacing the model data at that array index
    l_iAtm%pl(n) = l_iAtm%pln_save
    CALL Interp_LPoly_TL( Atm_In%Level_Pressure(0), l_iAtm%pl(n-1:n), Atm_In_TL%Level_Pressure(0), pl_TL(n-1:n), lpoly_TL )
    l_iAtm%pl(n) = l_iAtm%plint_save
    l_iAtm%tl(n) = l_iAtm%tln_save
    CALL Interp_Linear_TL( l_iAtm%ilpoly, l_iAtm%tl(n-1:n), lpoly_TL, tl_TL(n-1:n), t_int_tl )
    l_iAtm%tl(n) = l_iAtm%tlint_save
    tl_TL(n) = t_int_TL
    DO j = 1, Atm_In%n_Absorbers
      l_iAtm%al(n,j) = l_iAtm%aln_save(j)
      CALL Interp_Linear_TL( l_iAtm%ilpoly, l_iAtm%al(n-1:n,j), lpoly_TL, al_TL(n-1:n,j), a_int_TL )
      l_iAtm%al(n,j) = l_iAtm%alint_save(j)
      al_TL(n,j) = a_int_TL
    END DO
    
    ! Now compute the model profile layer averages
    DO k = 1, n
      CALL Layer_P_TL( l_iAtm%pl(k-1:k), pl_TL(k-1:k), p_TL(k))
      CALL Layer_X_TL( tl_TL(k-1:k), t_TL(k) )
    END DO
    DO j = 1, Atm_In%n_Absorbers
      DO k = 1, n
        CALL Layer_X_TL( al_TL(k-1:k,j), a_TL(k,j) )
      END DO
    END DO
    
    ! Now, extrapolate user layer profile to get the "layer 0" value and
    ! use it to shift the model profile to avoid a discontinuity at p(n)
    CALL Interp_LPoly_TL( l_iAtm%p(n), Atm_In%Pressure(1:2), p_TL(n), Atm_In_TL%Pressure(1:2), lpoly_TL )
    CALL Shift_Profile_TL( l_iAtm%elpoly, Atm_In%Temperature(1:2), lpoly_TL, Atm_In_TL%Temperature(1:2), t_TL )
    DO j = 1, Atm_In%n_Absorbers
      CALL Shift_Profile_TL( l_iAtm%elpoly, Atm_In%Absorber(1:2,j), lpoly_TL, Atm_In_TL%Absorber(1:2,j), a_TL(:,j) )
    END DO
    
    ! Make sure the absorber amounts are not negative.
    ! (Is a further, more physical, check needed here?)
    WHERE (l_iAtm%a_save < ZERO) a_TL = ZERO
    

    ! Copy over the atmosphere structure with extra layers
    ! (which will be zero by definition)
    atm_out_TL = CRTM_Atmosphere_AddLayerCopy( Atm_In_TL, n )
    IF ( .NOT. CRTM_Atmosphere_Associated( atm_out_TL ) ) THEN
      err_stat = FAILURE
      msg = 'Error copying Atmosphere structure with extra layers'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
      RETURN
    END IF
    

    ! Slot the added layers into the output atmosphere structure
    ! Note: Cloud and Aerosol assignments not really needed (the
    !       zeroing is handled by the structure allocation) since
    !       at TOA, typically, there are not any clouds and/or
    !       aerosols.
    ! ...Profile
    Atm_Out_TL%Level_Pressure(0:n) = pl_TL
    Atm_Out_TL%Pressure(1:n)       = p_TL
    Atm_Out_TL%Temperature(1:n)    = t_TL
    DO j = 1, Atm_In%n_Absorbers
      Atm_Out_TL%Absorber(1:n,j)   = a_TL(:,j)
    END DO
    ! ...Clouds
    IF ( Atm_In%n_Clouds > 0 ) THEN
      DO i = 1, Atm_In%n_Clouds
        Atm_Out_TL%Cloud(i)%Effective_Radius(1:n)   = ZERO
        Atm_Out_TL%Cloud(i)%Effective_Variance(1:n) = ZERO
        Atm_Out_TL%Cloud(i)%Water_Content(1:n)      = ZERO
      END DO
    END IF
    ! ...Aerosols
    IF ( Atm_In%n_Aerosols > 0 ) THEN
      DO i = 1, Atm_In%n_Aerosols
        Atm_Out_TL%Aerosol(i)%Effective_Radius(1:n) = ZERO
        Atm_Out_TL%Aerosol(i)%Concentration(1:n)    = ZERO
      END DO
    END IF


    ! Clean up
    CALL iAtm_Destroy( l_iAtm )
    ! Local allocated arrays
    DEALLOCATE( pl_TL, tl_TL, al_TL, &  ! Level arrays
                p_TL , t_TL , a_TL , &  ! Layer arrays
                STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      WRITE(msg,'("Error deallocating temporary profile arrays. STAT=",i0)' ) alloc_stat
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING )
    END IF

  END FUNCTION CRTM_Atmosphere_AddLayers_TL
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_AddLayers_AD
!
! PURPOSE:
!       Function to copy back an adjoint atmosphere structure removing added
!       extra layers as were required to supplement the upper atmosphere
!       profile data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Atmosphere_AddLayers_AD( Atm_In    , &  ! FWD Input
!                                                    Atm_Out_AD, &  ! AD  Input
!                                                    Atm_In_AD , &  ! AD  Output
!                                                    iAtm        )  ! Internal variable input
!
! INPUTS:
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
!       iAtm:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure should not be used
!                        outside of this module, i.e. CRTM_Atmosphere.
!                        UNITS:      N/A
!                        TYPE:       TYPE(iAtm_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Atm_In_AD:       Adjoint atmosphere structure at the original, user
!                        specified layering.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
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
!       Note the INTENT on the output structure argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Atmosphere_AddLayers_AD( &
    Atm_In    , &  ! FWD Input
    Atm_Out_AD, &  ! AD  Input
    Atm_In_AD , &  ! AD  Output
    iAtm      ) &  ! Internal variable input
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm_In
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_Out_AD
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_In_AD
    TYPE(iAtm_type)           , INTENT(IN)     :: iAtm
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_AddLayers_AD'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: alloc_stat
    INTEGER :: i, j, k, n, no, nt
    REAL(fp), ALLOCATABLE :: pl_AD(:), tl_AD(:), al_AD(:,:) ! Level arrays
    REAL(fp), ALLOCATABLE :: p_AD(:) , t_AD(:) , a_AD(:,:)  ! Layer arrays
    REAL(fp) :: lpoly_AD
    REAL(fp) :: t_int_AD
    REAL(fp) :: a_int_AD


    ! Set up
    err_stat = SUCCESS


    ! If extra layers are NOT needed, then simply perform
    ! the adjoint sum. Remember the TL form is
    !   Atm_Out_TL = Atm_In_TL
    ! so the adjoint form is
    !   Atm_In_AD  = Atm_In_AD + Atm_Out_AD
    !   Atm_Out_AD = ZERO
    IF ( Atm_In%Level_Pressure(0) <= TOA_PRESSURE) THEN
      Atm_In_AD = Atm_In_AD + Atm_Out_AD
      CALL CRTM_Atmosphere_Zero( Atm_Out_AD )
      RETURN
    END IF
    
    
    ! Determine how many extra layers have been used
    n = Extra_Layers( Atm_In )


    ! Allocate temporary arrays
    ALLOCATE( pl_AD(0:n), tl_AD(0:n), al_AD(0:n,Atm_In%n_Absorbers), &  ! Level arrays
              p_AD(n)   , t_AD(n)   , a_AD(n,Atm_In%n_Absorbers)   , &  ! Layer arrays
              STAT=alloc_stat )
    IF ( alloc_stat/= 0 ) THEN 
      err_stat = FAILURE
      msg = 'Error allocating temporary profile arrays'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )  
      RETURN
    END IF
    
    
    ! Initialise local adjoint variables
    pl_AD = ZERO
    tl_AD = ZERO
    al_AD = ZERO
    p_AD = ZERO
    t_AD = ZERO
    a_AD = ZERO
    lpoly_AD = ZERO


    ! Slot the added layers from the output adjoint
    ! atmosphere into the local arrays.
    DO j = 1, Atm_In%n_Absorbers
      a_AD(:,j) = a_AD(:,j) + Atm_Out_AD%Absorber(1:n,j)
      Atm_Out_AD%Absorber(1:n,j) = ZERO
    END DO
    t_AD = t_AD + Atm_Out_AD%Temperature(1:n)
    Atm_Out_AD%Temperature(1:n) = ZERO

    p_AD = p_AD + Atm_Out_AD%Pressure(1:n)
    Atm_Out_AD%Pressure(1:n) = ZERO

    pl_AD = pl_AD + Atm_Out_AD%Level_Pressure(0:n)
    Atm_Out_AD%Level_Pressure(0:n) = ZERO

    
    ! Perform the adjoint summations
    ! This is the adjoint equivalent of the TL Assign_Atmosphere
    no = Atm_In_AD%n_Layers
    nt = n + no
    ! ...Aerosols
    IF ( Atm_In_AD%n_Aerosols > 0 ) THEN
      DO i = 1, Atm_In_AD%n_Aerosols
        Atm_In_AD%Aerosol(i)%Concentration(1:no) = Atm_In_AD%Aerosol(i)%Concentration(1:no) + &
                                                   Atm_Out_AD%Aerosol(i)%Concentration(n+1:nt)
        Atm_In_AD%Aerosol(i)%Effective_Radius(1:no) = Atm_In_AD%Aerosol(i)%Effective_Radius(1:no) + &
                                                      Atm_Out_AD%Aerosol(i)%Effective_Radius(n+1:nt)
        Atm_In_AD%Aerosol(i)%Type = Atm_Out_AD%Aerosol(i)%Type
      END DO
    END IF
    ! ...Clouds    
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
    ! ...Absorber data
    DO j = 1, Atm_In_AD%n_Absorbers
      Atm_In_AD%Absorber(1:no,j) = Atm_In_AD%Absorber(1:no,j) + Atm_Out_AD%Absorber(n+1:nt,j)
    END DO
    ! ...Temperature data
    Atm_In_AD%Temperature(1:no) = Atm_In_AD%Temperature(1:no) + Atm_Out_AD%Temperature(n+1:nt)
    ! ...Pressure data
    Atm_In_AD%Pressure(1:no)       = Atm_In_AD%Pressure(1:no) + Atm_Out_AD%Pressure(n+1:nt)
    Atm_In_AD%Level_Pressure(0:no) = Atm_In_AD%Level_Pressure(0:no) + Atm_Out_AD%Level_Pressure(n:nt)


    ! Zero the output atmosphere structure
    CALL CRTM_Atmosphere_Zero( Atm_Out_AD )


    ! Check for negative absorber amounts
    ! (Is a further, more physical, check needed here?)
    WHERE (iAtm%a_save <= ZERO) a_AD = ZERO


    ! The adjoint of the user layer profile extrapolation to get the "layer 0"
    ! value used to shift the model profile to avoid a discontinuity at p(n)
    DO j = 1, Atm_In%n_Absorbers
      CALL Shift_Profile_AD( iAtm%elpoly, Atm_In%Absorber(1:2,j), a_AD(:,j), lpoly_AD, Atm_In_AD%Absorber(1:2,j) )
    END DO
    CALL Shift_Profile_AD( iAtm%elpoly, Atm_In%Temperature(1:2), t_AD, lpoly_AD, Atm_In_AD%Temperature(1:2) )
    CALL Interp_LPoly_AD( iAtm%p(n), Atm_In%Pressure(1:2), lpoly_AD, p_AD(n), Atm_In_AD%Pressure(1:2) )


    ! Compute the adjoint of the model profile layer averages
    DO j = 1, Atm_In%n_Absorbers
      DO k = 1, n
        CALL Layer_X_AD( a_AD(k,j), al_AD(k-1:k,j) )
      END DO
    END DO
    DO k = 1, n
      CALL Layer_X_AD( t_AD(k), tl_AD(k-1:k) )
      CALL Layer_P_AD( iAtm%pl(k-1:k), p_AD(k), pl_AD(k-1:k))
    END DO


    ! Adjoint of the interpolation of the extra levels to the user
    ! top pressure replacing the model data at that array index
    DO j = 1, Atm_In%n_Absorbers
      a_int_AD   = a_int_AD + al_AD(n,j)
      al_AD(n,j) = ZERO
      CALL Interp_Linear_AD( iAtm%ilpoly, iAtm%al(n-1:n,j), a_int_AD, lpoly_AD, al_AD(n-1:n,j) )
    END DO
    t_int_AD = t_int_AD + tl_AD(n)
    tl_AD(n) = ZERO
    CALL Interp_Linear_AD( iAtm%ilpoly, iAtm%tl(n-1:n), t_int_AD, lpoly_AD, tl_AD(n-1:n) )
    Atm_In_AD%Level_Pressure(0) = Atm_In_AD%Level_Pressure(0) + pl_AD(n)
    pl_AD(n)                    = ZERO
    CALL Interp_LPoly_AD( Atm_In%Level_Pressure(0), iAtm%pl(n-1:n), lpoly_AD, Atm_In_AD%Level_Pressure(0), pl_AD(n-1:n) )
    

    ! Clean up
    DEALLOCATE( pl_AD, tl_AD, al_AD, &  ! Level arrays
                p_AD , t_AD , a_AD , &  ! Layer arrays
                STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      msg = 'Error deallocating temporary profile arrays'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING )
    END IF

  END FUNCTION CRTM_Atmosphere_AddLayers_AD


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! Subprogram to determine the number of extra layers required
  FUNCTION Extra_Layers( Atm ) RESULT( n )
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atm
    INTEGER :: n
    ! NOTE: Assumption here is that the lowest input pressure
    !       is *always* less than the highest model pressure.
    n = MINLOC(Atm%Level_Pressure(0)-MODEL_LEVEL_PRESSURE, &
               DIM=1, &
               MASK=(Atm%Level_Pressure(0)-MODEL_LEVEL_PRESSURE) > ZERO)
  END FUNCTION Extra_Layers


  ! Subprogram to compute the average layer pressure
  SUBROUTINE Layer_P( p, p_layer )
    REAL(fp), INTENT(IN)  :: p(2)     ! Input
    REAL(fp), INTENT(OUT) :: p_layer  ! Output
    p_layer = (p(2)-p(1))/LOG(p(2)/p(1))
  END SUBROUTINE Layer_P

  SUBROUTINE Layer_P_TL( p, p_TL, p_layer_TL )
    REAL(fp), INTENT(IN)  :: p(2), p_TL(2)    ! Input
    REAL(fp), INTENT(OUT) :: p_layer_TL       ! Output
    REAL(fp) :: v, dpl_dp1,dpl_dp2
    v = LOG(p(2)/p(1))
    dpl_dp1 = (p(2)/p(1) - v - ONE )/v**2
    dpl_dp2 = (p(1)/p(2) + v - ONE )/v**2
    p_layer_TL = dpl_dp1*p_TL(1) + dpl_dp2*p_TL((2))
  END SUBROUTINE Layer_P_TL

  SUBROUTINE Layer_P_AD( p, p_layer_AD, p_AD )
    REAL(fp), INTENT(IN)     :: p(2)        ! Input
    REAL(fp), INTENT(IN OUT) :: p_layer_AD  ! Input
    REAL(fp), INTENT(IN OUT) :: p_AD(2)     ! Output
    REAL(fp) :: v, dpl_dp1,dpl_dp2
    v = LOG(p(2)/p(1))
    dpl_dp1 = (p(2)/p(1) - v - ONE )/v**2
    dpl_dp2 = (p(1)/p(2) + v - ONE )/v**2
    p_AD(2) = p_AD(2) + dpl_dp2*p_layer_AD
    p_AD(1) = p_AD(1) + dpl_dp1*p_layer_AD
    p_layer_AD = ZERO
  END SUBROUTINE Layer_P_AD


  ! Subprogram to compute the average layer amount of X
  SUBROUTINE Layer_X( x, x_layer )
    REAL(fp), INTENT(IN)  :: x(2)
    REAL(fp), INTENT(OUT) :: x_layer
    x_layer = POINT_5*(x(1)+x(2))
  END SUBROUTINE Layer_X
  
  SUBROUTINE Layer_X_TL( x_TL, x_layer_TL )
    REAL(fp), INTENT(IN)  :: x_TL(2)
    REAL(fp), INTENT(OUT) :: x_layer_TL
    x_layer_TL = POINT_5*(x_TL(1)+x_TL(2))
  END SUBROUTINE Layer_X_TL
  
  SUBROUTINE Layer_X_AD( x_layer_AD, x_AD )
    REAL(fp), INTENT(IN OUT) :: x_layer_AD  ! Input
    REAL(fp), INTENT(IN OUT) :: x_AD(2)     ! Output
    x_AD(2) = x_AD(2) + POINT_5*x_layer_AD
    x_AD(1) = x_AD(1) + POINT_5*x_layer_AD
    x_layer_AD = ZERO
  END SUBROUTINE Layer_X_AD
  

  ! Subprogram to compute the interpolating polynomial linear in log(p)
  SUBROUTINE Interp_LPoly( p_int, p, lpoly )
    REAL(fp), INTENT(IN)  :: p_int
    REAL(fp), INTENT(IN)  :: p(2)
    REAL(fp), INTENT(OUT) :: lpoly
    lpoly = (LOG(p_int)-LOG(p(1))) / (LOG(p(2))-LOG(p(1)))
  END SUBROUTINE Interp_LPoly

  SUBROUTINE Interp_LPoly_TL( p_int, p, p_int_TL, p_TL, lpoly_TL )
    REAL(fp), INTENT(IN)  :: p_int
    REAL(fp), INTENT(IN)  :: p(2)
    REAL(fp), INTENT(IN)  :: p_int_TL
    REAL(fp), INTENT(IN)  :: p_TL(2)
    REAL(fp), INTENT(OUT) :: lpoly_TL
    REAL(fp) :: v, dlp_dpi, dlp_dp1, dlp_dp2
    v = LOG(p(2))-LOG(p(1))
    dlp_dpi = ONE / (p_int*v)
    dlp_dp1 =  (LOG(p_int)-LOG(p(2))) / (p(1) * v**2)
    dlp_dp2 = -(LOG(p_int)-LOG(p(1))) / (p(2) * v**2)
    lpoly_TL = dlp_dpi*p_int_TL + dlp_dp1*p_TL(1) + dlp_dp2*p_TL(2)
  END SUBROUTINE Interp_LPoly_TL

  SUBROUTINE Interp_LPoly_AD( p_int, p, lpoly_AD, p_int_AD, p_AD )
    REAL(fp), INTENT(IN)     :: p_int
    REAL(fp), INTENT(IN)     :: p(2)
    REAL(fp), INTENT(IN OUT) :: lpoly_AD  ! Input
    REAL(fp), INTENT(IN OUT) :: p_int_AD  ! Output
    REAL(fp), INTENT(IN OUT) :: p_AD(2)   ! Output
    REAL(fp) :: v, dlp_dpi, dlp_dp1, dlp_dp2
    v = LOG(p(2))-LOG(p(1))
    dlp_dpi = ONE / (p_int*v)
    dlp_dp1 =  (LOG(p_int)-LOG(p(2))) / (p(1) * v**2)
    dlp_dp2 = -(LOG(p_int)-LOG(p(1))) / (p(2) * v**2)
    p_AD(2)  = p_AD(2)  + dlp_dp2*lpoly_AD
    p_AD(1)  = p_AD(1)  + dlp_dp1*lpoly_AD
    p_int_AD = p_int_AD + dlp_dpi*lpoly_AD
    lpoly_AD = ZERO
  END SUBROUTINE Interp_LPoly_AD


  ! Subprogram to perform linear interpolation
  SUBROUTINE Interp_Linear( lpoly, x, x_int )
    REAL(fp), INTENT(IN)  :: lpoly
    REAL(fp), INTENT(IN)  :: x(2)
    REAL(fp), INTENT(OUT) :: x_int
    x_int = (x(2)-x(1))*lpoly + x(1)
  END SUBROUTINE Interp_Linear

  SUBROUTINE Interp_Linear_TL( lpoly, x, lpoly_TL, x_TL, x_int_TL )
    REAL(fp), INTENT(IN)  :: lpoly
    REAL(fp), INTENT(IN)  :: x(2)
    REAL(fp), INTENT(IN)  :: lpoly_TL
    REAL(fp), INTENT(IN)  :: x_TL(2)
    REAL(fp), INTENT(OUT) :: x_int_TL
    REAL(fp) :: dxi_dx2, dxi_dx1, dxi_dlp
    dxi_dx2 = lpoly
    dxi_dx1 = ONE - lpoly
    dxi_dlp = x(2) - x(1)
    x_int_TL = dxi_dx2*x_TL(2) + dxi_dx1*x_TL(1) + dxi_dlp*lpoly_TL
  END SUBROUTINE Interp_Linear_TL
                                                                   
  SUBROUTINE Interp_Linear_AD( lpoly, x, x_int_AD, lpoly_AD, x_AD )
    REAL(fp), INTENT(IN)     :: lpoly
    REAL(fp), INTENT(IN)     :: x(2)
    REAL(fp), INTENT(IN OUT) :: x_int_AD  ! Input
    REAL(fp), INTENT(IN OUT) :: lpoly_AD  ! Output
    REAL(fp), INTENT(IN OUT) :: x_AD(2)   ! Output
    REAL(fp) :: dxi_dx2, dxi_dx1, dxi_dlp
    dxi_dx2 = lpoly
    dxi_dx1 = ONE - lpoly
    dxi_dlp = x(2) - x(1)
    lpoly_AD = lpoly_AD + dxi_dlp*x_int_AD
    x_AD(1)  = x_AD(1)  + dxi_dx1*x_int_AD
    x_AD(2)  = x_AD(2)  + dxi_dx2*x_int_AD
    x_int_AD = ZERO
  END SUBROUTINE Interp_Linear_AD


  ! Subprogram to shifted the added profile layers
  SUBROUTINE Shift_Profile( lpoly, x_toa, x_shifted )
    REAL(fp), INTENT(IN)     :: lpoly
    REAL(fp), INTENT(IN)     :: x_toa(2)
    REAL(fp), INTENT(IN OUT) :: x_shifted(:)
    INTEGER :: n
    REAL(fp) :: x_int, dx
    n = SIZE(x_shifted)
    CALL Interp_Linear( lpoly, x_toa, x_int )
    dx = x_int - x_shifted(n)
    x_shifted = x_shifted + dx
  END SUBROUTINE Shift_Profile
    
  SUBROUTINE Shift_Profile_TL( lpoly, x_toa, lpoly_TL, x_toa_TL, x_shifted_TL )
    REAL(fp), INTENT(IN)     :: lpoly
    REAL(fp), INTENT(IN)     :: x_toa(2)
    REAL(fp), INTENT(IN)     :: lpoly_TL
    REAL(fp), INTENT(IN)     :: x_toa_TL(2)
    REAL(fp), INTENT(IN OUT) :: x_shifted_TL(:)
    INTEGER :: n
    REAL(fp) :: x_int_TL, dx_TL
    n = SIZE(x_shifted_TL)
    CALL Interp_Linear_TL( lpoly, x_toa, lpoly_TL, x_toa_TL, x_int_TL )
    dx_TL = x_int_TL - x_shifted_TL(n)
    x_shifted_TL = x_shifted_TL + dx_TL
  END SUBROUTINE Shift_Profile_TL
    
  SUBROUTINE Shift_Profile_AD( lpoly, x_toa, x_shifted_AD, lpoly_AD, x_toa_AD )
    REAL(fp), INTENT(IN)     :: lpoly
    REAL(fp), INTENT(IN)     :: x_toa(2)
    REAL(fp), INTENT(IN OUT) :: x_shifted_AD(:)
    REAL(fp), INTENT(IN OUT) :: lpoly_AD
    REAL(fp), INTENT(IN OUT) :: x_toa_AD(2)
    INTEGER :: i, n
    REAL(fp) :: dx_AD, x_int_AD
    n = SIZE(x_shifted_AD)
    dx_AD    = ZERO
    x_int_AD = ZERO
    DO i = n, 1, -1
      dx_AD = dx_AD + x_shifted_AD(i)
    END DO
    x_shifted_AD(n) = x_shifted_AD(n) - dx_AD
    x_int_AD        = x_int_AD + dx_AD
    dx_AD           = ZERO
    CALL Interp_Linear_AD( lpoly, x_toa, x_int_AD, lpoly_AD, x_toa_AD )
  END SUBROUTINE Shift_Profile_AD
    
END MODULE CRTM_Atmosphere
