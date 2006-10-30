!------------------------------------------------------------------------------
!M+
! NAME:
!       Compute_Effective_TauProfile
!
! PURPOSE:
!       Program to compute the effective molecular transmittances profiles
!       from the various available molecular combinations.
!
! CATEGORY:
!       Transmittance Production
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                  Module containing definitions for kinds
!                                    of variable types.
!
!       File_Utility:                Module containing generic file utility
!                                    routines
!
!       Error_Handler:               Module to define simple error codes and
!                                    handle error conditions
!                                    USEs: FILE_UTILITY module
!
!       ProcessControl_Define:       Module containing the Process Control
!                                    data type definition and routines to
!                                    manipulate the structure.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!
!       ProcessControl_IO:           Module containing routines to read and write
!                                    ASCII Process Control files.
!                                    USEs: TYPE_KINDS module
!                                          FILE_UTILITY module
!                                          ERROR_HANDLER module
!                                          PROCESSCONTROL_DEFINE module
!
!       TauProfile_Define:           Module defining the TauProfile data structure
!                                    and containing routines to manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!
!       TauProfile_netCDF_IO:        Module containing routines to read and write
!                                    netCDF format TauProfile files.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          TAUPROFILE_DEFINE module
!                                          NETCDF module
!                                          NETCDF_UTILITY module
!
!       Tau_Production_Parameters:   Module defining parameters used in the LBL
!                                    transmittance production runs
!                                    USEs: TYPE_KINDS module
!
!       Tau_Production_Utility:      Module continaing utility routines for the
!                                    LBL transmittance production runs.
!                                    USEs: TYPE_KINDS module
!                                          FILE_UTILITY module
!                                          ERROR_HANDLER module
!                                          TAU_PRODUCTION_PARAMETERS module
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
!       Input:  TauProfile netCDF file containing transmittance profiles for
!               molecular species combinations.
!
!       Output: TauProfile netCDF file containing the total transmittance and
!               the effective transmittance profiles for single molecular species.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-June-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002 Paul van Delst
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

PROGRAM Compute_Effective_TauProfile


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Error_Handler

  USE ProcessControl_Define
  USE ProcessControl_IO

  USE TauProfile_Define
  USE TauProfile_netCDF_IO

  USE Tau_Production_Parameters
  USE Tau_Production_Utility


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Compute_Effective_TauProfile'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Compute_Effective_TauProfile.f90,v 2.7 2006/02/14 17:12:08 paulv Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ONE  = 1.0_fp_kind

  INTEGER, PARAMETER :: N_DIRECTIONS = 2
  INTEGER, PARAMETER :: UP_DIRECTION   = 1
  INTEGER, PARAMETER :: DOWN_DIRECTION = 2
  INTEGER, PARAMETER, DIMENSION( N_DIRECTIONS ) :: &
    DIRECTION_ID = (/ UP_DIRECTION,   &
                      DOWN_DIRECTION /)
  CHARACTER( * ),  PARAMETER, DIMENSION( N_DIRECTIONS ) :: &
    DIRECTION_NAME = (/ 'upwelling  ', &
                        'downwelling' /)


  INTEGER, PARAMETER :: WLO_IDX =  1  ! H2O lines only, no continua
  INTEGER, PARAMETER :: ALL_IDX = 10  ! First 7 molecules with continua
  INTEGER, PARAMETER :: WVO_IDX = 11  ! H2O and O3 only with continua
  INTEGER, PARAMETER :: WET_IDX = 12  ! H2O lines and continua
  INTEGER, PARAMETER :: DRY_IDX = 13  ! Dry gases (no H2O and O3) and continua
  INTEGER, PARAMETER :: OZO_IDX = 14  ! O3 lines and continua
  INTEGER, PARAMETER :: WCO_IDX = 15  ! H2O continua only, no line absorption
  INTEGER, PARAMETER :: EFFECTIVE_WLO_IDX = WLO_IDX + 100  ! WET/WCO
  INTEGER, PARAMETER :: EFFECTIVE_DRY_IDX = DRY_IDX + 100  ! ALL/WVO
  INTEGER, PARAMETER :: EFFECTIVE_OZO_IDX = OZO_IDX + 100  ! WVO/WET


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: IO_Status
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status

  CHARACTER( 256 ) :: Control_Filename
  CHARACTER( 256 ) :: TauProfile_Filename
  CHARACTER( 256 ) :: Signal_Filename

  INTEGER :: Direction

  INTEGER :: l, n_l ! n_Channels
  INTEGER :: i, n_i ! n_Angles
  INTEGER :: m, n_m ! n_Profiles
  INTEGER :: j, n_j ! n_Molecule_Sets
  INTEGER :: n
  INTEGER :: nGT1, k
  INTEGER, DIMENSION(N_LAYERS) :: IdxGT1

  CHARACTER( 5000 ) :: History

  TYPE( ProcessControl_type ) :: ProcessControl

  INTEGER, DIMENSION( : ), ALLOCATABLE :: Profile_List

  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_ALL
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_WVO
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_WET
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_WCO



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to compute the effective molecular transmittance ")' )
  WRITE( *, '( 5x, "   profiles from the various available molecular          ")' )
  WRITE( *, '( 5x, "   combinations.                                          ")' )
  WRITE( *, '(/5x, " $Revision: 2.7 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                        -- DETERMINE THE DIRECTION --                       #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Select atmospheric path" )' )
  DO n = 1, N_DIRECTIONS
    WRITE( *, '( 10x, i1, ") ", a )' ) n, DIRECTION_NAME(n)
  END DO
  WRITE( *, FMT = '( /5x, "Enter choice: " )', ADVANCE = 'NO' )
  READ( *, FMT = '( i5 )', IOSTAT = IO_Status ) Direction

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPHERIC PATH identifier input.', &
                          FAILURE )
    STOP
  END IF

  IF ( Direction < DIRECTION_ID(1) .OR. Direction > DIRECTION_ID(N_DIRECTIONS) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPERIC PATH identifier value.', &
                          FAILURE )
    STOP
  ENDIF



  !#----------------------------------------------------------------------------#
  !#                      -- READ A PROCESS CONTROL FILE --                     #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter a Process Control filename : " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) Control_Filename
  Control_Filename = ADJUSTL( Control_Filename )


  ! -----------------------------
  ! Read the Process Control data
  ! -----------------------------

  Error_Status = Read_ProcessControl( TRIM( Control_Filename ), &
                                      ProcessControl )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Process Control file '//&
                          TRIM( Control_Filename )//'.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#            -- BEGIN MAIN LOOP OVER SENSOR/PLATFORM FILES --                #
  !#----------------------------------------------------------------------------#

  Sensor_Loop: DO n = 1, ProcessControl%n_Files


    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE THE TauProfile FILENAME --                    #
    !#--------------------------------------------------------------------------#

    TauProfile_Filename = './TauProfile_data/'//&
                          TRIM( DIRECTION_NAME( Direction ) )//'.'//&
                          TRIM( ProcessControl%File_Prefix( n ) )//'.TauProfile.nc'




    !#--------------------------------------------------------------------------#
    !#                 -- INQUIRE THE INPUT TauProfile FILE --                  #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------
    ! Inquire the file to get dimensions
    ! and global attributes
    ! ----------------------------------

    Error_Status = Inquire_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                              n_Channels      = n_l, &
                                              n_Angles        = n_i, &
                                              n_Profiles      = n_m, &
                                              n_Molecule_Sets = n_j, &
                                              History = History )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error inquiring netCDF TauProfile file '//&
                            TRIM( TauProfile_Filename )//' dimensions.', &
                            Error_Status )
      STOP
    END IF


    ! ---------------------------
    ! Inquire the file to get the
    ! dimension list data
    ! ---------------------------

    ! -- Allocate the array
    ALLOCATE( Profile_List( n_m ), STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating dimension list arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    
    ! -- Read the profile list
    Error_Status = Inquire_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                              Profile_List = Profile_List, &
                                              History = History )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error inquiring netCDF TauProfile file '//&
                            TRIM( TauProfile_Filename )//' profile list.', &
                            Error_Status )
      STOP
    END IF


    ! -----------------------------------
    ! Modify the HISTORY global attribute
    ! -----------------------------------

    Error_Status = Modify_TauProfile_GAtts( TRIM( TauProfile_Filename ), &
                                            History = PROGRAM_RCS_ID//':'//&
                                                      TRIM( History ) )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( PROGRAM_NAME, &
                            'Error modifying HISTORY global attribute in '//&
                            TRIM( TauProfile_Filename ), &
                            Error_Status )
      STOP
    END IF



    !#--------------------------------------------------------------------------#
    !#      -- ALLOCATE ARRAYS FOR EFFECTIVE TRANSMITTANCE COMPUTATION --       #
    !#--------------------------------------------------------------------------#
 
    ALLOCATE( Tau_ALL( N_LAYERS, n_l, n_i ), &
              Tau_WVO( N_LAYERS, n_l, n_i ), &
              Tau_WET( N_LAYERS, n_l, n_i ), &
              Tau_WCO( N_LAYERS, n_l, n_i ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating transmittance arrays for ", a, ". STAT = ", i5 )' ) &
                      TRIM( TauProfile_Filename ), Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- BEGIN LOOP OVER PROFILES --                      #
    !#--------------------------------------------------------------------------#
 
    WRITE( *, '( 5x, "Computing the effective TauProfile data for the ", a, " sensor..." )' ) &
              TRIM( ProcessControl%File_Prefix( n ) )


    Profile_Loop: DO m = 1, n_m


      ! ----------------------------
      !  Read the transmittance data
      ! ----------------------------

      ! -- Total transmittance, Tau_ALL
      Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                             Profile_List(m), &
                                             ALL_IDX, &
                                             Tau_ALL )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading profile ", i3, " Tau_ALL from ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF


      ! -- Water vapor + ozone transmittance, Tau_WVO
      Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                             Profile_List(m), &
                                             WVO_IDX, &
                                             Tau_WVO )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading profile ", i3, " Tau_WVO from ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF


      ! -- Water vapor only transmittance, Tau_WET
      Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                             Profile_List(m), &
                                             WET_IDX, &
                                             Tau_WET )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading profile ", i3, " Tau_WET from ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF


      ! -- Water vapor continua only transmittance, Tau_WCO
      Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                             Profile_List(m), &
                                             WCO_IDX, &
                                             Tau_WCO )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading profile ", i3, " Tau_WCO from ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF



      ! ----------------------------------------------------------------
      ! Compute and correct, if necessary, the effective transmittances.
      ! The multi-nested loops are used to minimise memory usage.
      ! ----------------------------------------------------------------

      DO i = 1, n_i
        DO l = 1, n_l

          ! ------------------------------------
          ! Compute the effective transmittances
          ! ------------------------------------

          ! --Effective DRY transmittance
          CALL Compute_EffTau( Tau_ALL(:,l,i), Tau_WVO(:,l,i) )

          ! -- Effective OZO transmittance
          CALL Compute_EffTau( Tau_WVO(:,l,i), Tau_WET(:,l,i) )

          ! -- Effective WLO transmittance
          CALL Compute_EffTau( Tau_WET(:,l,i), Tau_WCO(:,l,i) )


          ! ------------------------------------------------------------------
          ! Correct the effective transmittances
          ! ** Note: Only the upwelling transmittances are corrected. Not sure
          !          what to do about the downwelling ones yet
          ! ------------------------------------------------------------------

          IF ( Direction == UP_DIRECTION ) THEN

            ! -- Effective DRY transmittance
            CALL Correct_EffTau( Tau_ALL(:,l,i) )

            ! -- Effective OZO transmittance
            CALL Correct_EffTau( Tau_WVO(:,l,i) )

            ! -- Effective WLO transmittance
            CALL Correct_EffTau( Tau_WET(:,l,i) )

          END IF

        END DO
      END DO


      ! ----------------------------------
      ! Output the current profile to file
      ! ----------------------------------

      ! -- Effective DRY transmittance
      Error_Status = Write_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                              Tau_ALL, &
                                              Profile_List(m), &
                                              EFFECTIVE_DRY_IDX )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile ", i3, " Effective Tau_DRY to ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      ! -- Effective OZO transmittance
      Error_Status = Write_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                              Tau_WVO, &
                                              Profile_List(m), &
                                              EFFECTIVE_OZO_IDX )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile ", i3, " Effective Tau_OZO to ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      ! -- Effective WLO transmittance
      Error_Status = Write_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                              Tau_WET, &
                                              Profile_List(m), &
                                              EFFECTIVE_WLO_IDX )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile ", i3, " Effective Tau_OZO to ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      WRITE( *, '( 15x, "Profile # ", i3, " effective transmittances written..." )' ) &
                Profile_List(m)

    END DO Profile_Loop



    !#--------------------------------------------------------------------------#
    !#     -- DEALLOCATE ARRAYS FOR EFFECTIVE TRANSMITTANCE COMPUTATION --      #
    !#--------------------------------------------------------------------------#
 
    DEALLOCATE( Tau_ALL, Tau_WVO, Tau_WET, Tau_WCO, Profile_List, &
                STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating arrays for ", a, ". STAT = ", i5 )' ) &
                      TRIM( TauProfile_Filename ), Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

  END DO Sensor_Loop



  !#----------------------------------------------------------------------------#
  !#                -- CREATE A DIRECTION DEPENDENT SIGNAL FILE --              #
  !#----------------------------------------------------------------------------#

  WRITE( Signal_Filename, '( a, ".",  a )' ) &
                          PROGRAM_NAME, &
                         TRIM( DIRECTION_NAME( Direction ) )

  Error_Status = Create_Signal_File( TRIM( Signal_Filename ) )



  !#----------------------------------------------------------------------------#
  !#                                -- CLEAN UP --                              #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_ProcessControl( ProcessControl )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ProcessControl data structure.', &
                          Error_Status )
  END IF


CONTAINS


  ! Subroutine to compute the effective transmittances
  ! The effective transmittance is returned in the first argument,
  ! the numerator transmittance
  ! Only compute effective transmittance if the denominator
  ! is greater than numerical precision.

  SUBROUTINE Compute_EffTau( TauNUM, TauDENOM )
    REAL( fp_kind ), DIMENSION(:), INTENT( IN OUT ) :: TauNUM
    REAL( fp_kind ), DIMENSION(:), INTENT( IN )     :: TauDENOM
    REAL( fp_kind ), PARAMETER :: TOLERANCE = EPSILON( ONE )
    WHERE( TauDENOM > TOLERANCE )
      TauNUM = TauNUM/TauDENOM
    ELSEWHERE
      TauNUM = ZERO
    END WHERE
  END SUBROUTINE Compute_EffTau


  ! Subroutine to correct the effective transmittances
  ! For UPWELLING transmittances:
  ! -- Check for transmittances > 1.0.
  ! -- If found, all transmittances after the first are set to 1.0.

  SUBROUTINE Correct_EffTau( EffTau )
    REAL( fp_kind ), DIMENSION(:), INTENT( IN OUT ) :: EffTau
    INTEGER :: nGT1, k
    INTEGER, DIMENSION(N_LAYERS) :: IdxGT1
    nGT1 = COUNT( EffTau > ONE )
    IF ( nGT1 > 0 ) THEN
      IdxGT1(1:nGT1) = PACK( (/ (k,k=1,N_LAYERS) /), EffTau > ONE )
      k = IdxGT1(1)
      EffTau(k:) = ONE
    END IF
  END SUBROUTINE Correct_EffTau

END PROGRAM Compute_Effective_TauProfile


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Compute_Effective_TauProfile.f90,v 2.7 2006/02/14 17:12:08 paulv Exp $
!
! $Date: 2006/02/14 17:12:08 $
!
! $Revision: 2.7 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Compute_Effective_TauProfile.f90,v $
! Revision 2.7  2006/02/14 17:12:08  paulv
! - Added internal subprogram to correct effective transmittances (for upwelling
!   transmittances only) when they become > 1.0. Downwelling effective transmittances
!   are not changed.
! - Added internal subprgram to compute the effective transmittances also.
! - Added parameter definitions for direction ids.
!
! Revision 2.6  2006/01/26 18:40:22  paulv
! - Changed so that the profile numbers used are those from the profile list
!   in the input files, rather than simply 1->N for the profile set being
!   used. This allows subsets of profiles to be processed.
!
! Revision 2.5  2005/09/16 20:27:43  paulv
! - Updates made to reflect changes in ProcessControl structure name and
!   components.
!
! Revision 2.4  2003/09/09 20:18:33  paulv
! - Updated code to use new Process Control modules.
!
! Revision 2.3  2003/07/18 16:30:18  paulv
! - Changes made to use new TauProfile I/O module.
! - Added the calculation of the effective water vapour, LINES ONLY transmittance
!   component.
!
! Revision 2.2  2003/01/16 15:02:49  paulv
! - Added some informational output.
!
! Revision 2.1  2002/10/29 22:33:13  paulv
! - No longer reading all of the transmittance data in at once...the memory
!   requirements were too unwieldy. Method used now is to only read the data
!   as it is required BY PROFILE. Thus only 3 sets of transmittances are
!   read in: Tau(ALL),TAU(WVO), and Tau(WET). The effective transmittances
!   are computed and appended to the TauProfile file. This is slightly more
!   expensive in terms of I/O but much much less expensive in terms of
!   memory usage (form 244MB to about 950KB).
! - Only works with TauProfile data files that have the molecule set dimension
!   as the unlimited dimension in the netCDF dataset.
!
! Revision 1.7  2002/09/09 21:40:00  paulv
! - Added more molecules to output.
! - Generalised the straight transmittance data copying. Effective output is
!   still done separately for each molecule set.
!
! Revision 1.6  2002/08/02 19:14:57  paulv
! - Changed the set value for those cases where the denominator in the
!   effective transmittance calculation is < numberical precision. Previously
!   the set value was ZERO. Now it is -ONE.
!
! Revision 1.5  2002/07/25 22:02:44  paulv
! - Corrected error in program name. Was screwing up the signal file name. Argh.
!
! Revision 1.4  2002/07/25 21:11:51  paulv
! - Output a signal file at the program conclusion.
! - Moved the TauProfile structure destruction call to the end of the file loop.
! - Cleared the strings used to obtain the TauProfile gloabal attributes before
!   using them.
!
! Revision 1.3  2002/06/27 20:22:32  paulv
! - Replaced the WHERE over the entire transmittance arrays to a WHERE within
!   multi-nested loops. This was done to eliminate memory faults due to
!   (I think) the need to copy the entire array in memory and it just begin
!   too big for some reason (only 40MB which doesn't sound like a lot.)
!
! Revision 1.2  2002/06/27 19:29:13  paulv
! - New version. Memory fault occurs in WHERE construct divisions I think becuase
!   the array section is copied and the required amount of memory is about
!   44MB. Doesn't seem to big but when the statement is removed, execution
!   proceeds.
!
! Revision 1.1  2002/06/27 17:34:32  paulv
! Initial checkin. Not complete.
!
!
!
!
!
