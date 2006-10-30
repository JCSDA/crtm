!------------------------------------------------------------------------------
!P+
! NAME:
!       Check_TauProfile
!
! PURPOSE:
!       Program to check TauProfile datafiles for invalid data.
!
! CATEGORY:
!       Transmittance Production
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       File_Utility:               Module containing generic file utility routines
!
!       Error_Handler:              Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       Compare_Float_Numbers:      Module containing routines to perform equality
!                                   and relational comparisons on floating point
!                                   numbers.
!
!       TauProfile_Define:          Module defining the TauProfile data
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       TauProfile_netCDF_IO:       Module containing routines to read and
!                                   write TauProfile netCDF format files.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         ATMPROFILE_DEFINE module
!                                         NETCDF module
!                                         NETCDF_UTILITY module
!
! CONTAINS:
!       None.
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
!       Input:  netCDF TauProfile data file
!
!       Output: None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 09-Feb-2006
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2006 Paul van Delst
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
!P-
!------------------------------------------------------------------------------

PROGRAM Check_TauProfile


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Error_Handler
  USE Compare_Float_Numbers

  USE TauProfile_Define
  USE TauProfile_netCDF_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Check_TauProfile'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Check_TauProfile.f90,v 1.1 2006/02/09 18:35:44 paulv Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ONE  = 1.0_fp_kind

  REAL( fp_kind ), PARAMETER :: TOLERANCE     = EPSILON( ONE )
  REAL( fp_kind ), PARAMETER :: TAU_TOLERANCE = 100.0_fp_kind * TOLERANCE

  INTEGER, PARAMETER :: N_LAYERS = 100


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status
  INTEGER :: Allocate_Status

  CHARACTER( 256 ) :: InFile

  INTEGER ::  l,  i,  m,  j
  INTEGER :: nL, nI, nM, nJ
  INTEGER,         DIMENSION(:), ALLOCATABLE :: L_List
  REAL( fp_kind ), DIMENSION(:), ALLOCATABLE :: I_List
  INTEGER,         DIMENSION(:), ALLOCATABLE :: M_List
  INTEGER,         DIMENSION(:), ALLOCATABLE :: J_List
  CHARACTER(20) :: L_Tag, I_Tag, M_Tag, J_Tag

  REAL( fp_kind ), DIMENSION(N_LAYERS) :: Tau



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to check TauProfile datafiles for invalid data.")' )
  WRITE( *, '(/5x, " $Revision: 1.1 $")' )
  WRITE( *, '( 5x, a, /)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                    -- ENTER THE FILENAMES TO COMPARE --                    #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter TauProfile filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) InFile
  InFile = ADJUSTL( InFile )

  ! -- Check that both files exist
  IF ( .NOT. File_Exists( InFile ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Input file '//&
                          TRIM(InFile)//&
                          ' not found.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                       -- INQUIRE THE TauProfile FILE --                    #
  !#----------------------------------------------------------------------------#

  ! ------------------
  ! Get the dimensions
  ! ------------------

  Error_Status = Inquire_TauProfile_netCDF( InFile, &
                                            n_Channels      = nL, &     
                                            n_Angles        = nI, &       
                                            n_Profiles      = nM, &     
                                            n_Molecule_Sets = nJ  ) 

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring netCDF TauProfile file '//&
                          TRIM(InFile)//&
                          ' for dimensions', &
                          Error_Status )
    STOP
  END IF


  ! ----------------------------------
  ! Allocate the dimension list arrays
  ! ----------------------------------

  ALLOCATE( L_List( nL ), &
            I_List( nI ), &
            M_List( nM ), &
            J_List( nJ ), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating dimension list arrays for input file ", a, &
                      &". STAT = ", i5 )' ) &
                    TRIM( InFile ), Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          Error_Status )
    STOP
  END IF


  ! -----------------------
  ! Get the dimension lists
  ! -----------------------

  Error_Status = Inquire_TauProfile_netCDF( InFile, &
                                            Channel_List      = L_List, &
                                            Angle_List        = I_List, &
                                            Profile_List      = M_List, &
                                            Molecule_Set_List = J_List  )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring netCDF TauProfile file '//&
                          TRIM(InFile)//&
                          ' for dimension lists', &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                         -- CHECK TRANSMITTANCES --                         #
  !#----------------------------------------------------------------------------#

  J_Loop: DO j = 1, nJ
    WRITE( J_Tag, '( "; Molecule Set:",i3 )' ) J_List(j)
    M_Loop: DO m = 1, nM
      WRITE( M_Tag, '( "; Profile:",i3 )' ) M_List(m)
      I_Loop: DO i = 1, nI
        WRITE( I_Tag, '( "; Angle secant:",f4.2 )' ) I_List(i)
        L_Loop: DO l = 1, nL
          WRITE( L_Tag, '( "; Channel:",i4 )' ) L_List(l)


          ! --------------------------------------
          ! Read the current transmittance profile
          ! --------------------------------------

          Error_Status = Read_TauProfile_netCDF( TRIM( InFile ), &
                                                 L_List(l), &
                                                 I_List(i), &
                                                 M_List(m), &
                                                 J_List(j), &
                                                 Tau, &
                                                 Quiet = 1 )

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading netCDF TauProfile file '//&
                                  TRIM( InFile )//&
                                  TRIM(L_Tag)//TRIM(I_Tag)//TRIM(M_Tag)//TRIM(J_Tag), &
                                  Error_Status )
            STOP
          END IF


          ! --------------------------------------
          ! Correct transmittances close to 0 or 1
          ! but non-physical due to numerics
          ! --------------------------------------

          ! -- Check for 1 < tau 1+e
          WHERE( Tau > ONE .AND. Tau < (ONE+TAU_TOLERANCE) )
            Tau = ONE
          END WHERE

          ! -- Check for 0-e < tau < 0
          WHERE( Tau > (ZERO-TAU_TOLERANCE) .AND. Tau < ZERO )
            Tau = ZERO
          END WHERE


          ! -----------------------------------------------
          ! Now check for grossly crappy transmittances > 1
          ! -----------------------------------------------

          IF ( ANY( Tau > ONE ) ) THEN
            WRITE( Message, '( "Transmittances > 1.0 found in ", a, a )' ) &
                            TRIM( InFile ), &
                            TRIM(L_Tag)//TRIM(I_Tag)//TRIM(M_Tag)//TRIM(J_Tag)
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM( Message ), &
                                  WARNING )
          END IF

          ! -----------------------------------------------
          ! Now check for grossly crappy transmittances < 0
          ! -----------------------------------------------

          IF ( ANY( Tau < ZERO ) ) THEN
            WRITE( Message, '( "Transmittances < 0.0 found in ", a, a )' ) &
                            TRIM( InFile ), &
                            TRIM(L_Tag)//TRIM(I_Tag)//TRIM(M_Tag)//TRIM(J_Tag)
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM( Message ), &
                                  WARNING )
          END IF

        END DO L_Loop
      END DO I_Loop
    END DO M_Loop
  END DO J_Loop

END PROGRAM Check_TauProfile


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Check_TauProfile.f90,v 1.1 2006/02/09 18:35:44 paulv Exp $
!
! $Date: 2006/02/09 18:35:44 $
!
! $Revision: 1.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Check_TauProfile.f90,v $
! Revision 1.1  2006/02/09 18:35:44  paulv
! Initial checkin.
!
!
!
