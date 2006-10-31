!------------------------------------------------------------------------------
!M+
! NAME:
!       Create_LBLRTM_Input_Files
!
! PURPOSE:
!       Program to create LBLRTM TAPE5 input data files given an input 
!       netCDF AtmProfile dataset
!
! CATEGORY:
!       Transmittance Production
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       Message_Handler:         Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
!       AtmProfile_Define:     Module defining the AtmProfile data
!                              structure and containing routines to
!                              manipulate it.
!                              USEs: TYPE_KINDS module
!                                    ERROR_HANDLER module
!
!       AtmProfile_netCDF_IO:  Module containing routines to read and
!                              write AtmProfile netCDF format files.
!                              USEs: TYPE_KINDS module
!                                    ERROR_HANDLER module
!                                    ATMPROFILE_DEFINE module
!                                    NETCDF module
!                                    NETCDF_UTILITY module
!
!       LBLRTM_Input:          Module containing routines for creating LBLRTM
!                              input files.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    ERROR_HANDLER module
!                                    STRING_PROCESSING module
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
!       Input:  netCDF AtmProfile data sets.
!
!       Output: Individual TAPE5 output files for each profile in the 
!               netCDF AtmProfile dataset.
!
! SIDE EFFECTS:
!       All output files are overwritten if they already exist.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
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

PROGRAM Create_LBLRTM_Input_Files

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE AtmProfile_Define
  USE AtmProfile_netCDF_IO

  USE LBLRTM_Input


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Create_LBLRTM_Input_Files'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Create_LBLRTM_Input_Files.f90,v 1.7 2006/06/30 16:47:16 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Numbers
  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ONE  = 1.0_fp_kind

  ! -- Required absorber IDs
  INTEGER, PARAMETER :: H2O_ID = 1
  INTEGER, PARAMETER ::  O3_ID = 3

  ! -- Default climatology model
  INTEGER, PARAMETER :: US_STD_ATM = 6


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status

  CHARACTER( 256 )        :: AtmProfile_fileNAME
  INTEGER                 :: AtmProfile_fileID
  TYPE( AtmProfile_type ) :: AtmProfile

  CHARACTER( 256 ) :: Profile_Set_ID_Tag

  CHARACTER( 256 ) :: TAPE5_File
  CHARACTER(  78 ) :: TAPE5_Header

  INTEGER :: j, m, n
  INTEGER, DIMENSION( 2 ) :: j_idx



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to create the LBLRTM TAPE5 input data files. ")' )
  WRITE( *, '(/5x, " $Revision: 1.7 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                      -- READ THE AtmProfile DATAFILE --                    #
  !#----------------------------------------------------------------------------#

  ! ----------------
  ! Get the filename
  ! ----------------

  WRITE( *, FMT = '( /5x, "Enter the netCDF AtmProfile filename: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a ) ' ) AtmProfile_fileNAME
  AtmProfile_fileNAME = ADJUSTL( AtmProfile_fileNAME )


  ! -------------
  ! Read the file
  ! -------------

  Error_Status = Read_AtmProfile_netCDF( TRIM( AtmProfile_fileNAME ), &
                                         AtmProfile, &
                                         ID_Tag = Profile_Set_ID_Tag )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error reading AtmProfile file '//&
                          TRIM( AtmProfile_fileNAME ), &
                          FAILURE )
    STOP
  END IF


  !#----------------------------------------------------------------------------#
  !#            -- FIND THE ABSORBER INDICES FOR H2O and O3 ONLY --             #
  !#----------------------------------------------------------------------------#

  n = COUNT( AtmProfile%Absorber_ID == H2O_ID .OR. &
             AtmProfile%Absorber_ID ==  O3_ID      )

  IF ( n /= 2 ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'No H2O and O3 in absorber set.', &
                          FAILURE )
    STOP
  END IF

  j_idx = PACK( (/ ( j, j = 1, AtmProfile%n_Absorbers ) /), &
                ( AtmProfile%Absorber_ID == H2O_ID .OR. &
                  AtmProfile%Absorber_ID ==  O3_ID      ) )



  !#----------------------------------------------------------------------------#
  !#                             -- PROFILE LOOP --                             #
  !#----------------------------------------------------------------------------#

  WRITE( *, * )

  m_profile_loop: DO m = 1, AtmProfile%n_Profiles

     WRITE( *, '( 5x, "Processing profile #", i3, "...." )' ) m



    !#--------------------------------------------------------------------------#
    !#                -- CREATE THE LBLRTM TAPE5 INPUT FILES --                 #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Construct TAPE5 filename and header
    ! -----------------------------------

    ! -- Header
    TAPE5_Header = ' '
    WRITE( TAPE5_Header, '( a, " profile #", i3.3, "; ", a )' ) &
                         TRIM( Profile_Set_Id_Tag ), m

    ! -- Filename
    TAPE5_File = ' '
    WRITE( TAPE5_File, '( "./TAPE5_files/TAPE5.", a, "_profile", i2.2 )' ) &
                       TRIM( Profile_Set_Id_Tag ), m


    ! ---------------------
    ! Create the TAPE5 file
    ! ---------------------

    Error_Status = Create_LBLRTM_TAPE5( AtmProfile%Level_Pressure( :, m ), &
                                        AtmProfile%Level_Temperature( :, m ), &
                                        AtmProfile%Level_Absorber( :, j_idx, m ), &
                                        AtmProfile%Absorber_Units_LBLRTM( j_idx ), &
                                        AtmProfile%Absorber_ID( j_idx ), &
                                        ZERO,         &    ! Surface altitude
                                        ONE, ONE+ONE, &    ! Dummy frequencies
                                        Climatology_model = US_STD_ATM, &
                                        Header   = TRIM( TAPE5_Header ), &
                                        Filename = TRIM( TAPE5_File ),   &
                                        Placeholder   = 1, &  ! Frequency/angle placeholder
                                        No_Terminator = 1  )  ! Do not output input terminator
                             

    IF ( Error_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing TAPE5 file for ", a, " profile #", i2, "." )' ) &
                       TRIM( Profile_Set_Id_Tag ), m
      CALL display_message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF


  END DO m_profile_loop



  !#----------------------------------------------------------------------------#
  !#                  -- DESTROY THE AtmProfile STRUCTURE --                    #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_AtmProfile( AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error destroying AtmProfile structure.', &
                          WARNING )
  END IF

END PROGRAM Create_LBLRTM_Input_Files


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Create_LBLRTM_Input_Files.f90,v 1.7 2006/06/30 16:47:16 dgroff Exp $
!
! $Date: 2006/06/30 16:47:16 $
!
! $Revision: 1.7 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Create_LBLRTM_Input_Files.f90,v $
! Revision 1.7  2006/06/30 16:47:16  dgroff
! Changed "Error_Handler" references to "Message_Handler"
!
! Revision 1.6  2005/07/29 22:01:27  paulv
! - Upgraded to Fortran95
!
! Revision 1.5  2003/12/01 18:07:57  paulv
! - Cosmetic change. Altered inline comment for optional argument "Placeholder"
!   in the Create_LBLRTM_TAPE5() call.
!
! Revision 1.4  2003/07/18 18:30:24  paulv
! - Changed code to use new AtmProfile_netCDF_IO module. The entire database
!   is now read/written rather than a profile at a time.
!
! Revision 1.3  2002/09/10 21:33:46  paulv
! - Only writing H2O and O3 profile data to TAPE5 files. Default climatology
!   is the US Std Atm.
!
! Revision 1.2  2002/07/17 17:26:51  paulv
! - Added some info output.
!
! Revision 1.1  2002/07/17 17:16:46  paulv
! Initial checkin. Code replaces Create_Profile_Input_Files.f90.
!
!
!
!
!
