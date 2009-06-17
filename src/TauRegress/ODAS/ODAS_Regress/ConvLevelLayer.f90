!====================================================================
!
! MODULE: ConvLevelLayer
!
!  SUBROUTINE: Conv_LevelLayer()
!
!
! Created by Y.Tahara in Aug,02
!
!====================================================================



module ConvLevelLayer

  ! -- include modules

  use type_kinds, only : fp_kind
  use ParametersGenCoef

  
  ! -- disable implicit typing

  IMPLICIT NONE


  !--- public & private

  Private

  Public  Conv_LevelLayer


 CONTAINS

  !===========================================================================
  ! Subroutine to convert atmospheric elements on levels to layers.
  ! For the conversion, Paul's utilities are used.
  !===========================================================================

  subroutine Conv_LevelLayer( level_p, level_t, level_aa, &
                              layer_p, layer_t, layer_aa, &
                              N_LEVELS, N_ABSORBERS )

    ! -- Inputs

    INTEGER        , INTENT(IN)  :: N_LEVELS                    ! # of levels
    INTEGER        , INTENT(IN)  :: N_ABSORBERS                 ! # of gases
    REAL( fp_kind ), INTENT(IN)  :: level_p ( N_LEVELS )        ! pressure on levels (hPa)
    REAL( fp_kind ), INTENT(IN)  :: level_t ( N_LEVELS )        ! temperature on levels (K)
    REAL( fp_kind ), INTENT(IN)  :: level_aa( N_LEVELS, N_ABSORBERS )
                                                                ! atmospheric gases
                                                                ! level_aa(:,1) : contains water vapor mixing ratio (g/kg)

    ! -- Outputs

    REAL( fp_kind ), INTENT(OUT) :: layer_p ( N_LEVELS-1 )      ! pressure on layers
    REAL( fp_kind ), INTENT(OUT) :: layer_t ( N_LEVELS-1 )      ! temperature on layers
    REAL( fp_kind ), INTENT(OUT) :: layer_aa( N_LEVELS-1, N_ABSORBERS )
                                                                ! atmospheric gases
                                                                ! level_aa(:,1) : water vapor mixing ratio (g/kg)


    ! -- layer P
    
    layer_p(1:(N_LEVELS-1)) =    ( level_p(1:(N_LEVELS-1)) - level_p(2:N_LEVELS) ) / &
                              log( level_p(1:(N_LEVELS-1)) / level_p(2:N_LEVELS) )


    ! -- layer T

    layer_t(1:(N_LEVELS-1)) = ( level_t(1:(N_LEVELS-1)) + level_t(2:N_LEVELS) ) / TWO


    ! -- layer absorber
    
    layer_aa(1:(N_LEVELS-1),:) = ( level_aa(1:(N_LEVELS-1),:) + level_aa(2:N_LEVELS,:) ) / TWO


  end subroutine Conv_LevelLayer

end module ConvLevelLayer
