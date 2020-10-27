!------------------------------------------------------------------------------
!M+
! NAME:
!       Level_Temperatures
!
! PURPOSE:
!       Compute Level Temps from Layer Temps
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       USE Level_Temperatures
!
! MODULES:
!       Type_Kinds:              Module containing data type kind definitions.
!
! CONTAINS:
!       Level_Temperatures:      Subroutine to calculate the level temperatures from the layer temperatures.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       All of the array documentation lists the dimensions by a single letter.
!       K: Array dimension is of K atmospheric layers.
!
!
! CREATION HISTORY:
!       Written by:     Chris O'Dell (UW-AOS) 2004
!                       odell@aos.wisc.edu
!

!------------------------------------------------------------------------------

MODULE Level_Temperatures


  ! ---------------------
  ! Module use statements
  ! ---------------------

!  USE Type_Kinds, ONLY : fp_kind

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE

	INTEGER, PARAMETER	::	fp_kind = SELECTED_REAL_KIND(6)  ! Single precision (4 bytes)
!	INTEGER, PARAMETER	::	fp_kind = SELECTED_REAL_KIND(15)  ! Double precision (8 bytes)


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC  :: Compute_Level_Temperature

CONTAINS


!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Level_Temperature
!
! PURPOSE:
!       Subroutine to calculate the level temperatures from the layer temperatures.
!
!
! CALLING SEQUENCE:
!       CALL Compute_Level_Temperature( Layer_T,            &  ! Input, K
!                                       Level_T,            &  ! Output, K+1
!                                       Surface_Air_Temperature, &  ! Input, scalar, optional
!                                       TOA_Temperature     )  ! Input, scalar, optional
!
! INPUT ARGUMENTS:
!       Layer_T:                 Profile LAYER average temperature array.
!                                UNITS:      Kelvin
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  K
!                                ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Surface_Air_Temperature: Surface Air temperature (2 meter temperature)
!                                UNITS:      Kelvin
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       TOA_Temperature:         Temperature of the TOA boundary
!                                UNITS:      Kelvin
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Level_T:                 Profile LEVEL temperature array.
!                                UNITS:      Kelvin
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  K+1
!                                ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!    Uses simple linear extrapolation to deduce the level temperatures from the
!    layer temperatures.  If Surface_Temperature is not included, a linear fit is performed
!    through the first and second layer temperatures and extrapolated down to the surface.
!    If TOA_Temperature is not included, a linear fit is performed through the top and
!    second-to-top layer and extrapolated upwards.
!
!--------------------------------------------------------------------------------


  SUBROUTINE Compute_Level_Temperature( Layer_T,            &  ! Input, K
                                        Level_T,            &  ! Output, K+1
                                        Surface_Air_Temperature, &  ! Input, scalar, optional
                                        TOA_Temperature     )  ! Input, scalar, optional


    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Layer_T

    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: Level_T

    REAL( fp_kind ), OPTIONAL,       INTENT( IN )  :: Surface_Air_Temperature
    REAL( fp_kind ), OPTIONAL,       INTENT( IN )  :: TOA_Temperature

    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Level_Temperature'

    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k, n_Layers

    ! ----------
    ! Intrinsics
    ! ----------

    INTRINSIC SIZE, PRESENT

    !#--------------------------------------------------------------------------#
    !#                   -- Determine array dimensions --                       #
    !#--------------------------------------------------------------------------#

    n_Layers = SIZE( Layer_T )

    ! SURFACE LEVEL

    IF (PRESENT(Surface_Air_Temperature)) THEN
        Level_T(n_Layers+1) = Surface_Air_Temperature
    ELSE
        Level_T(n_Layers+1) = 1.5 * Layer_T(n_Layers) - 0.5 * Layer_T(n_Layers-1)
    ENDIF

    ! TOA LEVEL

    IF (PRESENT(TOA_Temperature)) THEN
        Level_T(1) = TOA_Temperature
    ELSE
        Level_T(1) = 1.5 * Layer_T(1) - 0.5 * Layer_T(2)
    ENDIF

    ! ALL OTHER LEVELS
    layers_loop: do k = 2, n_layers
        Level_T(k) = 0.5 * (Layer_T(k-1) + Layer_T(k))
    end do layers_loop

  END SUBROUTINE Compute_Level_Temperature

END MODULE Level_Temperatures

!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2005/01/05 21:48:26 $
!
! $Revision: 1.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: level_temperatures.f90,v $
! Revision 1.1  2005/01/05 21:48:26  paulv
! Initial checkin.
!
!
!
