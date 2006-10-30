!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Planck_Functions
!
! PURPOSE:
!       Module containing the sensor Planck function routines.
!
! CATEGORY:
!       CRTM : Source Functions
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_Planck_Functions
!
! OUTPUTS:
!       None.
!
! MODULES:
!       Type_Kinds:             Module containing data type kind definitions.
!
!       CRTM_Parameters:        Module containing parameter definitions for the
!                               CRTM.
!                               USEs: TYPE_KINDS module
!
!       CRTM_SpcCoeff:          Module containing the CRTM spectral coefficient
!                               data
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!                                     PARAMETERS module
!                                     SPCCOEFF_DEFINE module
!                                     SPCCOEFF_BINARY_IO module
!
! CONTAINS:
!       CRTM_Planck_Radiance:        Subroutine to calculate the instrument
!                                    channel radiance.
!
!       CRTM_Planck_Radiance_TL:     Subroutine to calculate the tangent-linear
!                                    instrument channel radiance.
!
!       CRTM_Planck_Radiance_AD:     Subroutine to calculate the adjoint
!                                    instrument channel radiance.
!
!       CRTM_Planck_Temperature:     Subroutine to calculate the instrument
!                                    channel brightness temperature.
!
!       CRTM_Planck_Temperature_TL:  Subroutine to calculate the tangent-linear
!                                    instrument channel brightness temperature.
!
!       CRTM_Planck_Temperature_AD:  Subroutine to calculate the adjoint
!                                    instrument channel brightness temperature.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! RESTRICTIONS:
!       These functions are called frequently so no input checking is
!       performed.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Aug-2001
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2001, 2003, 2004 Paul van Delst
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

MODULE CRTM_Planck_Functions


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds, ONLY : fp_kind

  USE CRTM_Parameters
  USE CRTM_SpcCoeff


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE

  PUBLIC  :: CRTM_Planck_Radiance
  PUBLIC  :: CRTM_Planck_Radiance_TL
  PUBLIC  :: CRTM_Planck_Radiance_AD

  PUBLIC  :: CRTM_Planck_Temperature
  PUBLIC  :: CRTM_Planck_Temperature_TL
  PUBLIC  :: CRTM_Planck_Temperature_AD


CONTAINS


!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Planck_Radiance
!
! PURPOSE:
!       Subroutine to calculate the instrument channel radiance.
!
! CATEGORY:
!       CRTM : Source Functions
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Planck_Radiance( Channel,     &  ! Input
!                                  Temperature, &  ! Input
!                                  Radiance     )  ! Output
!
! INPUT ARGUMENTS:
!       Channel:     Channel index id. This is a unique index
!                    to a (supported) sensor channel. Used to
!                    reference the spectral coefficient data
!                    in the CRTM_SpcCoeff module.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
!       Temperature: Temperature for which the Planck radiance is
!                    to be calculated.
!                    UNITS:      Kelvin, K
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Radiance:    Channel Planck radiance.
!                    UNITS:      mW/(m^2.sr.cm^-1)
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( OUT )
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
!       Spectral coefficients are obtained from the CRTM_SpcCoeff module
!       so only radiances for those sensors which are included in the spectral
!       coefficient data file can be calculated.
!
! PROCEDURE:
!       First a polychromatic correction is applied to give an effective
!       Temperature,
!
!         T_eff = bc1 + ( bc2 * T )
!
!       The sensor radiance is then calculated using the effective temperature:
!
!                       pc1
!         R = ------------------------
!              EXP( pc2 / T_eff ) - 1
!
!       The bc1, bc2, pc1, and pc2 values are obtained from the 
!       CRTM_SpcCoeff module which is filled during the initialisation
!       phase.
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Planck_Radiance( Channel,     &  ! Input
                                   Temperature, &  ! Input
                                   Radiance     )  ! Output

    ! -- Arguments
    INTEGER,         INTENT( IN )  :: Channel
    REAL( fp_kind ), INTENT( IN )  :: Temperature
    REAL( fp_kind ), INTENT( OUT ) :: Radiance

    ! -- Local
    REAL( fp_kind ) :: Effective_Temperature


    ! -------------------------------------
    ! Apply the polychromaticity correction
    ! to obtain an effective temperature
    ! -------------------------------------

    Effective_Temperature = SC%Band_C1( Channel ) + ( SC%Band_C2( Channel ) * Temperature )


    ! -----------------------------
    ! Calculate the Planck radiance
    ! -----------------------------

    Radiance =                  SC%Planck_C1( Channel )  / &
    !          ----------------------------------------------------------------
               ( EXP( SC%Planck_C2( Channel ) / Effective_Temperature ) - ONE )

  END SUBROUTINE CRTM_Planck_Radiance




!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Planck_Radiance_TL
!
! PURPOSE:
!       Subroutine to calculate the tangent-linear instrument channel radiance.
!
! CATEGORY:
!       CRTM : Source Functions
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Planck_Radiance_TL( Channel,        &  ! Input
!                                     Temperature,    &  ! Input
!                                     Temperature_TL, &  ! Input
!                                     Radiance_TL     )  ! Output
!
! INPUT ARGUMENTS:
!       Channel:        Channel index id. This is a unique index
!                       to a (supported) sensor channel.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Temperature:    Temperature for which the tangent-linear Planck radiance
!                       is to be calculated.
!                       UNITS:      Kelvin, K
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Temperature_TL: Tangent-linear temperature for which the tangent-linear
!                       Planck radiance is required.
!                       UNITS:      Kelvin, K
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Radiance_TL:    Tangent-linear Planck radiance.
!                       UNITS:      mW/(m^2.sr.cm^-1)
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT )
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
!       Spectral coefficients are obtained from the CRTM_SpcCoeff module
!       so only radiances for those sensors which are included in the spectral
!       coefficient data file can be calculated.
!
! PROCEDURE:
!       First a polychromatic correction is applied to give an effective
!       temperature,
!
!         T_eff = bc1 + ( bc2 . T )
!
!       The sensor tangent-linear radiance is then calculated by first computing
!       the exponential term,
!
!          exponential = EXP( pc2 / T_eff )
!
!       and then the actual operator,
!
!                 pc1 . pc2 . bc1 . exponential
!         F = ------------------------------------
!              ( T_eff . ( exponential - 1 ) )^2
!
!       which is the derivate of the Planck equation wrt temperature. The
!       tangent-linear radiance is then determined by,
!
!         dR = F . dT
!
!       where dT is the input tangent-linear temperature.
!
!       The bc1, bc2, pc1, and pc2 values are obtained from the 
!       CRTM_SpcCoeff module which is filled during the initialisation
!       phase.
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Planck_Radiance_TL( Channel,        &  ! Input
                                      Temperature,    &  ! Input
                                      Temperature_TL, &  ! Input
                                      Radiance_TL     )  ! Output

    ! -- Arguments
    INTEGER,         INTENT( IN )  :: Channel
    REAL( fp_kind ), INTENT( IN )  :: Temperature
    REAL( fp_kind ), INTENT( IN )  :: Temperature_TL
    REAL( fp_kind ), INTENT( OUT ) :: Radiance_TL

    ! -- Local
    REAL( fp_kind ) :: Effective_Temperature
    REAL( fp_kind ) :: Exponential
    REAL( fp_kind ) :: F


    ! -------------------------------------
    ! Apply the polychromaticity correction
    ! -------------------------------------

    Effective_Temperature = SC%Band_C1( Channel ) + ( SC%Band_C2( Channel ) * Temperature )


    ! --------------------------------------
    ! Calculate the Planck function operator
    ! --------------------------------------

    ! -- The exponential term
    Exponential = EXP( SC%Planck_C2( Channel ) / Effective_Temperature )

    ! -- The operator, call it F
    F =  SC%Planck_C1( Channel ) * SC%Planck_C2( Channel ) * Exponential * SC%Band_C2( Channel ) / &
    !   ----------------------------------------------------------------------------------------
                      ( Effective_Temperature * ( Exponential - ONE ) )**2


    ! -------------------------------------
    ! Calculate the tangent-linear radiance
    ! -------------------------------------

    Radiance_TL = F * Temperature_TL

  END SUBROUTINE CRTM_Planck_Radiance_TL




!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Planck_Radiance_AD
!
! PURPOSE:
!       Subroutine to calculate the adjoint instrument channel radiance.
!
! CATEGORY:
!       CRTM : Source Functions
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Planck_Radiance_AD( Channel,       &  ! Input
!                                     Temperature,   &  ! Input
!                                     Radiance_AD,   &  ! Input
!                                     Temperature_AD )  ! In/Output
!
! INPUT ARGUMENTS:
!       Channel:        Channel index id. This is a unique index
!                       to a (supported) sensor channel.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Temperature:    Temperature for which the tangent-linear Planck radiance
!                       is to be calculated.
!                       UNITS:      Kelvin
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Radiance_AD:    Adjoint Planck radiance.
!                       UNITS:      mW/(m2.sr.cm-1)
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Temperature_AD: Adjoint Planck temperature
!                       UNITS:      Kelvin
!                       TYPE:       REAL( fp_kind )
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
!       The input adjoint radiance argument, Radiance_AD, is NOT set to zero
!       before returning to the calling routine.
!
! RESTRICTIONS:
!       Spectral coefficients are obtained from the CRTM_SpcCoeff module
!       so only radiances for those sensors which are included in the spectral
!       coefficient data file can be calculated.
!
! PROCEDURE:
!       First a polychromatic correction is applied to give an effective
!       temperature,
!
!         T_eff = bc1 + ( bc2 . T )
!
!       The sensor tangent-linear radiance is then calculated by first computing
!       the exponential term,
!
!          exponential = EXP( pc2 / T_eff )
!
!       and then the actual operator,
!
!                 pc1 . pc2 . bc1 . exponential
!         F = ------------------------------------
!              ( T_eff . ( exponential - 1 ) )^2
!
!       which is the derivate of the Planck equation wrt temperature. The
!       adjoint temperature is then determined from,
!
!         T_AD = T_AD + ( F . R_AD )
!
!       where T_AD and R_AD on the LHS are the input adjoint temperature and
!       radiance respectively.
!
!       The bc1, bc2, pc1, and pc2 values are obtained from the 
!       CRTM_SpcCoeff module which is filled during the initialisation
!       phase.
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Planck_Radiance_AD( Channel,       &  ! Input
                                      Temperature,   &  ! Input
                                      Radiance_AD,   &  ! Input
                                      Temperature_AD )  ! In/Output

    ! -- Arguments
    INTEGER,         INTENT( IN )     :: Channel
    REAL( fp_kind ), INTENT( IN )     :: Temperature
    REAL( fp_kind ), INTENT( IN )     :: Radiance_AD
    REAL( fp_kind ), INTENT( IN OUT ) :: Temperature_AD

    ! -- Local
    REAL( fp_kind ) :: Effective_Temperature
    REAL( fp_kind ) :: Exponential
    REAL( fp_kind ) :: F


    ! -------------------------------------
    ! Apply the polychromaticity correction
    ! -------------------------------------

    Effective_Temperature = SC%Band_C1( Channel ) + ( SC%Band_C2( Channel ) * Temperature )


    ! --------------------------------------
    ! Calculate the Planck function operator
    ! --------------------------------------

    ! -- The exponential term
    Exponential = EXP( SC%Planck_C2( Channel ) / Effective_Temperature )

    ! -- The operator, call it F
    F =  SC%Planck_C1( Channel ) * SC%Planck_C2( Channel ) * Exponential * SC%Band_C2( Channel ) / &
    !   ----------------------------------------------------------------------------------------
                      ( Effective_Temperature * ( Exponential - ONE ) )**2


    ! ---------------------------------
    ! Calculate the adjoint temperature
    ! ---------------------------------

    Temperature_AD = Temperature_AD + ( F * Radiance_AD )

  END SUBROUTINE CRTM_Planck_Radiance_AD




!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Planck_Temperature
!
! PURPOSE:
!       Subroutine to calculate the instrument channel brightness temperature.
!
! CATEGORY:
!       CRTM : Source Functions
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Planck_Temperature( Channel,    &  ! Input
!                                     Radiance,   &  ! Input
!                                     Temperature )  ! Output
!
! INPUT ARGUMENTS:
!       Channel:     Channel index id. This is a unique index
!                    to a (supported) sensor channel.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
!       Radiance:    Radiance for which the Planck temperature is desired.
!                    UNITS:      mW/(m^2.sr.cm^-1)
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Temperature: Planck temperature.
!                    UNITS:      Kelvin, K
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
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
!       Spectral coefficients are obtained from the CRTM_SpcCoeff module
!       so only temperatures for those sensors which are included in the spectral
!       coefficient data file can be calculated.
!
! PROCEDURE:
!       First the effective temperature is calculated from the inverse Planck function,
!
!                        pc2
!         T_eff = ------------------
!                  LOG( pc1/R + 1 )
!
!       The polychromatic correction is then removed to provide the brightness
!       temperature,
!
!              T_eff - bc1
!         T = -------------
!                  bc2
!
!       The bc1, bc2, pc1, and pc2 values are obtained from the 
!       CRTM_SpcCoeff module which is filled during the initialisation
!       phase.
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Planck_Temperature( Channel,    &  ! Input
                                      Radiance,   &  ! Input
                                      Temperature )  ! Output

    ! -- Arguments
    INTEGER,         INTENT( IN )  :: Channel
    REAL( fp_kind ), INTENT( IN )  :: Radiance
    REAL( fp_kind ), INTENT( OUT ) :: Temperature

    ! -- Local
    REAL( fp_kind ) :: Effective_Temperature


    ! -----------------------------------
    ! Calculate the effective temperature
    ! -----------------------------------

    Effective_Temperature =              SC%Planck_C2( Channel )  / &
    !                       ---------------------------------------------------
                            LOG( ( SC%Planck_C1( Channel ) / Radiance ) + ONE )

    ! -------------------------------------
    ! Apply the polychromatic correction to 
    ! obtain the true temperature
    ! -------------------------------------

    Temperature = ( Effective_Temperature - SC%Band_C1( Channel ) ) / &
    !             ---------------------------------------------------
                                SC%Band_C2( Channel )

  END SUBROUTINE CRTM_Planck_Temperature




!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Planck_Temperature_TL
!
! PURPOSE:
!       Subroutine to calculate the tangent-linear instrument channel
!       brightness temperature.
!
! CATEGORY:
!       CRTM : Source Functions
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Planck_Temperature_TL( Channel,       &  ! Input
!                                        Radiance,      &  ! Input
!                                        Radiance_TL,   &  ! Input
!                                        Temperature_TL )  ! Output
!
! INPUT ARGUMENTS:
!       Channel:        Channel index id. This is a unique index
!                       to a (supported) sensor channel.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Radiance:       Radiance at which the tangent-linear Planck temperature
!                       is desired.
!                       UNITS:      mW/(m^2.sr.cm^-1)
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Radiance_TL:    Tangent-linear radiance for which the tangent-linear
!                       Planck temperature is desired.
!                       UNITS:      mW/(m^2.sr.cm^-1)
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Temperature_TL: Tangent-linear Planck temperature.
!                       UNITS:      Kelvin, K
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
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
!       Spectral coefficients are obtained from the CRTM_SpcCoeff module
!       so only temperatures for those sensors which are included in the spectral
!       coefficient data file can be calculated.
!
! PROCEDURE:
!       First the logarithm argument is calculated,
!
!         a = pc1/R + 1
!
!       The inverse Planck function operator is then calculated,
!
!                      pc1 . pc2
!         F = ------------------------------
!              bc2 . a . ( R . LOG( a ) )^2
!
!       and the tangent-linear temperature is then given by,
!
!         dT = F . dR
!
!       The bc1, bc2, pc1, and pc2 values are obtained from the 
!       CRTM_SpcCoeff module which is filled during the initialisation
!       phase.
!S-
!--------------------------------------------------------------------------------


  SUBROUTINE CRTM_Planck_Temperature_TL( Channel,       &  ! Input
                                         Radiance,      &  ! Input
                                         Radiance_TL,   &  ! Input
                                         Temperature_TL )  ! Output

    ! -- Arguments
    INTEGER,         INTENT( IN )  :: Channel
    REAL( fp_kind ), INTENT( IN )  :: Radiance
    REAL( fp_kind ), INTENT( IN )  :: Radiance_TL
    REAL( fp_kind ), INTENT( OUT ) :: Temperature_TL

    ! -- Local
    REAL( fp_kind ) :: Argument
    REAL( fp_kind ) :: F


    ! --------------------------------------
    ! Calculate the Planck function operator
    ! --------------------------------------

    ! -- The logarithm Argument
    Argument = ( SC%Planck_C1( Channel ) / Radiance ) + ONE

    ! -- The operator, call it F
    F =             SC%Planck_C1( Channel ) * SC%Planck_C2( Channel ) / &
    !   -------------------------------------------------------------------------
         ( SC%Band_C2( Channel ) * Argument * ( Radiance * LOG( Argument ) )**2 )


    ! ----------------------------------------
    ! Calculate the tangent-linear temperature
    ! ----------------------------------------

    Temperature_TL = F * Radiance_TL

  END SUBROUTINE CRTM_Planck_Temperature_TL



!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Planck_Temperature_AD
!
! PURPOSE:
!       Subroutine to calculate the adjoint instrument channel
!       brightness temperature.
!
! CATEGORY:
!       CRTM : Source Functions
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Planck_Temperature_AD( Channel,        &  ! Input
!                                        Radiance,       &  ! Input
!                                        Temperature_AD, &  ! Input
!                                        Radiance_AD     )  ! In/Output
!
! INPUT ARGUMENTS:
!       Channel:        Channel index id. This is a unique index
!                       to a (supported) sensor channel.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Radiance:       Radiance at which the adjoint radiance is desired.
!                       UNITS:      mW/(m^2.sr.cm^-1)
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Temperature_AD: Adjoint Planck temperature.
!                       UNITS:      Kelvin, K
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Radiance_AD:    Adjoint radiance.
!                       UNITS:      K.m^2.sr.cm^-1/mW
!                       TYPE:       REAL( fp_kind )
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
!       The input adjoint temperature argument, Temperature_AD, is NOT set to zero
!       before returning to the calling routine.
!
! RESTRICTIONS:
!       Spectral coefficients are obtained from the CRTM_SpcCoeff module
!       so only temperatures for those sensors which are included in the spectral
!       coefficient data file can be calculated.
!
! PROCEDURE:
!       First the logarithm argument is calculated,
!
!         a = pc1/R + 1
!
!       The inverse Planck function operator is then calculated,
!
!                      pc1 . pc2
!         F = ------------------------------
!              bc2 . a . ( R . LOG( a ) )^2
!
!       which is the derivate of the Planck temperature wrt radiance. The
!       adjoint radiance is then determined from,
!
!         R_AD = R_AD + ( F . T_AD )
!
!       where R_AD and T_AD on the LHS are the input adjoint radiance and
!       temperature respectively.
!
!       The bc1, bc2, pc1, and pc2 values are obtained from the 
!       CRTM_SpcCoeff module which is filled during the initialisation
!       phase.
!S-
!--------------------------------------------------------------------------------


  SUBROUTINE CRTM_Planck_Temperature_AD( Channel,        &  ! Input
                                         Radiance,       &  ! Input
                                         Temperature_AD, &  ! Input
                                         Radiance_AD     )  ! In/Output

    ! -- Arguments
    INTEGER,         INTENT( IN )     :: Channel
    REAL( fp_kind ), INTENT( IN )     :: Radiance
    REAL( fp_kind ), INTENT( IN )     :: Temperature_AD
    REAL( fp_kind ), INTENT( IN OUT ) :: Radiance_AD

    ! -- Local
    REAL( fp_kind ) :: Argument
    REAL( fp_kind ) :: F


    ! --------------------------------------
    ! Calculate the Planck function operator
    ! --------------------------------------

    ! -- The logarithm Argument
    Argument = ( SC%Planck_C1( Channel ) / Radiance ) + ONE

    ! -- The operator, call it F
    F =             SC%Planck_C1( Channel ) * SC%Planck_C2( Channel ) / &
    !   -------------------------------------------------------------------------
         ( SC%Band_C2( Channel ) * Argument * ( Radiance * LOG( Argument ) )**2 )


    ! ------------------------------
    ! Calculate the adjoint radiance
    ! ------------------------------

    Radiance_AD = Radiance_AD + ( F * Temperature_AD )

  END SUBROUTINE CRTM_Planck_Temperature_AD

END MODULE CRTM_Planck_Functions


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_Planck_Functions.f90,v 2.2 2004/11/03 16:04:03 paulv Exp $
!
! $Date: 2004/11/03 16:04:03 $
!
! $Revision: 2.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_Planck_Functions.f90,v $
! Revision 2.2  2004/11/03 16:04:03  paulv
! - Tidied up documentation.
! - Replaced "Exponent" variable with "Exponential" variable in radiance
!   routines.
! - Removed INTRINSIC statements.
!
! Revision 2.1  2004/08/17 20:42:42  paulv
! - Updated to use CRTM modules.
!
! Revision 2.0  2003/05/16 17:29:18  paulv
! - New version that uses the spectral coefficient structure.
!
! Revision 1.2  2001/10/01 20:28:47  paulv
! - Added "Name" to RCS keyword list.
!
! Revision 1.1  2001/08/08 20:04:03  paulv
! Initial checkin.
! - Routines were extracted from the radiance module and placed in their
!   own module to facilitate code-sharing.
!
!
!
