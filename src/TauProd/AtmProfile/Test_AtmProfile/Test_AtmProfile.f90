!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_AtmProfile
!
! PURPOSE:
!       Program to test the AtmProfile definition and I/O routines.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-90
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       File_Utility:           Module containing generic file utility
!                               routines.
!
!       Message_Handler:          Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       AtmProfile_Define:      Module defining the AtmProfile data structure and
!                               containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!
!       AtmProfile_Binary_IO:   Module containing routines to read and
!                               write Binary format AtmProfile files.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!                                     BINARY_FILE_UTILITY module
!                                     ATMPROFILE_DEFINE module
!
!       AtmProfile_netCDF_IO:   Module containing routines to read and
!                               write netCDF format AtmProfile files.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     NETCDF module
!                                     NETCDF_UTILITY module
!                                     ATMPROFILE_DEFINE module
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       User specified input files and a test output file.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Aug-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
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

PROGRAM Test_AtmProfile


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE AtmProfile_Define
  USE AtmProfile_Binary_IO
  USE AtmProfile_netCDF_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
 
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Test_AtmProfile'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Test_AtmProfile.f90,v 1.3 2006/06/30 16:47:16 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  CHARACTER( * ), PARAMETER ::  NC_FILENAME = 'Test.AtmProfile.nc'
  CHARACTER( * ), PARAMETER :: BIN_FILENAME = 'Test.AtmProfile.bin'

  INTEGER, PARAMETER :: MAX_N_LOOPS  = 10000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 500


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message
  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: FileID
  CHARACTER( 256 ) AtmProfile_Filename
  INTEGER :: Error_Status
  INTEGER :: n_Layers   
  INTEGER :: n_Absorbers
  INTEGER :: n_Profiles 
  INTEGER :: n
  TYPE( AtmProfile_type ) :: AtmProfile1
  TYPE( AtmProfile_type ) :: AtmProfile2
  TYPE( AtmProfile_type ) :: AtmProfile3



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test AtmProfile definition and I/O routines. ")' )
  WRITE( *, '(/5x, " $Revision: 1.3 $")' )
  WRITE( *, '(/5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                         -- DELETE THE TEST FILES --                        #
  !#----------------------------------------------------------------------------#

  IF ( File_Exists( NC_FILENAME ) ) THEN
    FileID = Get_Lun()
    OPEN( FileID, FILE   = NC_FILENAME, &
                  FORM   = 'UNFORMATTED', &
                  ACCESS = 'DIRECT', &
                  RECL   = 1, &
                  IOSTAT = Error_Status )
    CLOSE( FileID, STATUS = 'DELETE' )
  END IF

  IF ( File_Exists( BIN_FILENAME ) ) THEN
    FileID = Get_Lun()
    OPEN( FileID, FILE   = BIN_FILENAME, &
                  FORM   = 'UNFORMATTED', &
                  ACCESS = 'DIRECT', &
                  RECL   = 1, &
                  IOSTAT = Error_Status )
    CLOSE( FileID, STATUS = 'DELETE' )
  END IF



  !#----------------------------------------------------------------------------#
  !#                       -- ENTER THE INPUT FILENAME --                       #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter an netCDF AtmProfile filename: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) AtmProfile_Filename
  AtmProfile_Filename = ADJUSTL( AtmProfile_Filename )

  IF ( .NOT. File_Exists( TRIM( AtmProfile_Filename ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( AtmProfile_Filename )//' not found.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                -- READ AND WRITE NETCDF AtmProfile FILES --                #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Testing AtmProfile netCDF I/O functions ..." )' )


  ! ---------------------------
  ! Inquire the netCDF datafile
  ! ---------------------------

  Error_Status = Inquire_AtmProfile_netCDF( TRIM( AtmProfile_Filename ), &
                                            n_Layers    = n_Layers, &
                                            n_Absorbers = n_Absorbers, &
                                            n_Profiles  = n_Profiles )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error inquiring the Binary AtmProfile file '//TRIM( AtmProfile_Filename )
    n_LAyers    = -1
    n_Absorbers = -1
    n_Profiles  = -1
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( AtmProfile_Filename )//' netCDF file inquiry was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )

  WRITE( *, '( /5x, a, " file dimensions:", &
              &/10x, "n_Layers    = ", i5, &
              &/10x, "n_Absorbers = ", i5, &
              &/10x, "n_Profiles  = ", i5, / )' ) &
            TRIM( AtmProfile_Filename ), n_Layers, n_Absorbers, n_Profiles


  ! -------------------------
  ! Read the netCDF data file
  ! -------------------------

  Error_Status = Read_AtmProfile_netCDF( TRIM( AtmProfile_Filename ), &
                                         AtmProfile1 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error reading the netCDF AtmProfile file '//TRIM( AtmProfile_Filename )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( AtmProfile_Filename )//' netCDF file read was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )


  ! -----------------------------
  ! Write a test netCDF data file
  ! -----------------------------

  Error_Status = Write_AtmProfile_netCDF( NC_FILENAME, &
                                          AtmProfile1, &
                                          Title = 'This is the title attribute', &
                                          History = 'This is the history attribute', &
                                          Comment = 'This is the comment attribute', &
                                          ID_Tag = 'This is the id_tag attribute' )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error writing the netCDF AtmProfile file '//NC_FILENAME
  ELSE
    Error_Status = INFORMATION
    Message = NC_FILENAME//' netCDF file write was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )



  !#----------------------------------------------------------------------------#
  !#               -- WRITE AND READ THE BINARY AtmProfile FILE --              #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Testing AtmProfile Binary I/O functions ..." )' )


  ! --------------------------
  ! Write the Binary data file
  ! --------------------------

  Error_Status = Write_AtmProfile_Binary( BIN_FILENAME, &
                                          AtmProfile1 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error writing the Binary AtmProfile file '//BIN_FILENAME
  ELSE
    Error_Status = INFORMATION
    Message = BIN_FILENAME//' Binary file write was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )


  ! ---------------------------
  ! Inquire the Binary datafile
  ! ---------------------------

  Error_Status = Inquire_AtmProfile_Binary( BIN_FILENAME, &
                                            n_Layers    = n_Layers, &
                                            n_Absorbers = n_Absorbers, &
                                            n_Profiles  = n_Profiles )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error inquiring the Binary AtmProfile file '//BIN_FILENAME
    n_LAyers    = -1
    n_Absorbers = -1
    n_Profiles  = -1
  ELSE
    Error_Status = INFORMATION
    Message = BIN_FILENAME//' Binary file inquiry was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )

  WRITE( *, '( /5x, a, " file dimensions:", &
              &/10x, "n_Layers    = ", i5, &
              &/10x, "n_Absorbers = ", i5, &
              &/10x, "n_Profiles  = ", i5, / )' ) &
            BIN_FILENAME, n_Layers, n_Absorbers, n_Profiles


  ! -------------------------
  ! Read the Binary data file
  ! -------------------------

  Error_Status = Read_AtmProfile_Binary( BIN_FILENAME, &
                                         AtmProfile3 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error reading the Binary AtmProfile file '//BIN_FILENAME
  ELSE
    Error_Status = INFORMATION
    Message = BIN_FILENAME//' Binary file read was successful'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )


  ! --------------------------------------------
  ! Compare the netCDF and Binary read structure
  ! --------------------------------------------

  Error_Status = Equal_AtmProfile( AtmProfile3, AtmProfile1 )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'NetCDF I/O structures are different'
  ELSE
    Error_Status = INFORMATION
    Message = 'NetCDF I/O structures are equal'
  END IF

  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )

  WRITE( *, '( //5x, "Press <ENTER> to test for netCDF reader memory leaks..." )' )
  READ( *, * )



  !#----------------------------------------------------------------------------#
  !#                       -- LOOP FOR MEMORY LEAK TEST --                      #
  !#----------------------------------------------------------------------------#

  ! ---------------------------------------
  ! Test the netCDF reader for memory leaks
  ! ---------------------------------------

  WRITE( *, '( /5x, "Looping for netCDF read memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = Read_AtmProfile_netCDF( NC_FILENAME, &
                                           AtmProfile1, &
                                           Quiet = 1 )

    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO

  WRITE( *, '( //5x, "Press <ENTER> to test for Binary reader memory leaks..." )' )
  READ( *, * )


  ! ---------------------------------------
  ! Test the Binary reader for memory leaks
  ! ---------------------------------------

  WRITE( *, '( /5x, "Looping for Binary read memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = Read_AtmProfile_Binary( BIN_FILENAME, &
                                           AtmProfile2, &
                                           Quiet = 1 )

    IF ( MOD( n, INFO_N_LOOPS ) == 0 .AND. n /= MAX_N_LOOPS ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO

  WRITE( *, '( //5x, "Press <ENTER> to test for structure assign function memory leaks..." )' )
  READ( *, * )


  ! -----------------------------------------
  ! Test the Assign function for memory leaks
  ! -----------------------------------------

  WRITE( *, '( /5x, "Looping for structure copy memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = Assign_AtmProfile( AtmProfile3, AtmProfile2 )

    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO



  !#----------------------------------------------------------------------------#
  !#                          -- DESTROY THE STRUCTURES --                      #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_AtmProfile( AtmProfile1 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AtmProfile1 structure.', &
                          WARNING )
  END IF


  Error_Status = Destroy_AtmProfile( AtmProfile2 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AtmProfile2 structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_AtmProfile( AtmProfile3 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AtmProfile3 structure.', &
                          WARNING )
  END IF

END PROGRAM Test_AtmProfile


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_AtmProfile.f90,v 1.3 2006/06/30 16:47:16 dgroff Exp $
!
! $Date: 2006/06/30 16:47:16 $
!
! $Revision: 1.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_AtmProfile.f90,v $
! Revision 1.3  2006/06/30 16:47:16  dgroff
! Changed "Error_Handler" references to "Message_Handler"
!
! Revision 1.2  2004/08/27 14:15:14  paulv
! - Added call to Equal_AtmProfile() function to determine if the I/O functions
!   somehow modify the numbers.
!
! Revision 1.1  2004/08/26 20:24:58  paulv
! Initial checkin.
!
!
!
