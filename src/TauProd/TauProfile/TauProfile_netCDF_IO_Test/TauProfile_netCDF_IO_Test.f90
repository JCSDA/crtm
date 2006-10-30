!------------------------------------------------------------------------------
!P+
! NAME:
!       TauProfile_netCDF_IO_Test
!
! PURPOSE:
!       Program to test the TauProfile netCDF I/O functions.
!
! CATEGORY:
!       TauProfile
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       Error_Handler:              Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
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
!       Input: Existing netCDF TauProfile data file
!
!       Output: Test netCDF TauProfile data file.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 30-Sep-2002
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
!P-
!------------------------------------------------------------------------------

PROGRAM TauProfile_netCDF_IO_Test


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Error_Handler

  USE TauProfile_Define
  USE TauProfile_netCDF_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'TauProfile_netCDF_IO_Test'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: TauProfile_netCDF_IO_Test.f90,v 1.3 2004/09/14 17:27:07 paulv Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  CHARACTER( * ), PARAMETER :: INPUT_FILENAME = 'Input.TauProfile.nc'

  INTEGER, PARAMETER :: MAX_N_LOOPS  = 1000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 50

  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status
  INTEGER :: Allocate_Status

  CHARACTER( 256 ) :: Output_Filename

  INTEGER :: n

  ! -- TauProfile dimensions
  INTEGER :: n_Layers
  INTEGER :: n_Channels, l
  INTEGER :: n_Angles, i
  INTEGER :: n_Profiles, m
  INTEGER :: n_Molecule_Sets, j

  ! -- TauProfile global attributes
  CHARACTER(  256 ) :: ID_Tag
  CHARACTER(  256 ) :: Title
  CHARACTER( 5000 ) :: History
  CHARACTER(  256 ) :: Sensor_Name
  CHARACTER(  256 ) :: Platform_Name
  CHARACTER( 5000 ) :: Comment

  ! -- Sensor IDs
  INTEGER :: NCEP_Sensor_ID
  INTEGER :: WMO_Satellite_ID
  INTEGER :: WMO_Sensor_ID

  ! -- TauProfile list data
  INTEGER,         DIMENSION( : ), ALLOCATABLE :: Channel_List
  REAL( fp_kind ), DIMENSION( : ), ALLOCATABLE :: Angle_List
  INTEGER,         DIMENSION( : ), ALLOCATABLE :: Profile_List
  INTEGER,         DIMENSION( : ), ALLOCATABLE :: Molecule_Set_List

  ! -- TauProfile data arrays
  REAL( fp_kind ), DIMENSION(:),         ALLOCATABLE :: Tau1
  REAL( fp_kind ), DIMENSION(:,:),       ALLOCATABLE :: Tau2
  REAL( fp_kind ), DIMENSION(:,:,:),     ALLOCATABLE :: Tau3
  REAL( fp_kind ), DIMENSION(:,:,:,:),   ALLOCATABLE :: Tau4
  REAL( fp_kind ), DIMENSION(:,:,:,:,:), ALLOCATABLE :: Tau5

  ! -- TauProfile data structure
  TYPE( TauProfile_type ) :: TauProfile, TauProfile1



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test read a netCDF format TauProfile file.    ")' )
  WRITE( *, '(/5x, " $Revision: 1.3 $")' )
  WRITE( *, '( 5x, a, /)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                   -- INQUIRE THE TauProfile FILE --                        #
  !#----------------------------------------------------------------------------#

  ! ---------------------------------------------
  ! Get the file dimensions and global attributes
  ! ---------------------------------------------

  ! -- Read the data
  Error_Status = Inquire_TauProfile_netCDF( INPUT_FILENAME,                    &  ! Input
                                            ! -- Dimensions
                                            n_Layers        = n_Layers,        &  ! Optional output
                                            n_Channels      = n_Channels,      &  ! Optional output
                                            n_Angles        = n_Angles,        &  ! Optional output
                                            n_Profiles      = n_Profiles,      &  ! Optional output
                                            n_Molecule_Sets = n_Molecule_Sets, &  ! Optional output
                                            ! -- Sensor Ids
                                            NCEP_Sensor_ID   = NCEP_Sensor_ID,   &  ! Optional output
                                            WMO_Satellite_ID = WMO_Satellite_ID, &  ! Optional output
                                            WMO_Sensor_ID    = WMO_Sensor_ID,    &  ! Optional output
                                            ! -- Global attributes
                                            ID_Tag          = ID_Tag,          &  ! Optional output
                                            Title           = Title,           &  ! Optional output
                                            History         = History,         &  ! Optional output
                                            Sensor_Name     = Sensor_Name,     &  ! Optional output
                                            Platform_Name   = Platform_Name,   &  ! Optional output
                                            Comment         = Comment          )  ! Optional output

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring netCDF TauProfile file '//&
                          INPUT_FILENAME//&
                          ' for dimensions and global attributes', &
                          Error_Status )
    STOP
  END IF


  ! -- Output some info
  WRITE( *, '( /5x, "Dimensions of ", a, " TauProfile file:", &
             &/10x, "n_Layers        = ", i5, &
             &/10x, "n_Channels      = ", i5, &
             &/10x, "n_Angles        = ", i5, &
             &/10x, "n_Profiles      = ", i5, &
             &/10x, "n_Molecule_Sets = ", i5 )' ) &
            INPUT_FILENAME, &
            n_Layers, &
            n_Channels, &
            n_Angles, &
            n_Profiles, &
            n_Molecule_Sets

  WRITE( *, '( /5x, "Global attributes of ", a, " TauProfile file:", &
             & /, "ID_Tag : ", &
             & /, "--------", /, a, &
             &//, "Title : ", &
             & /, "-------", /, a, &
             &//, "History : ", &
             & /, "---------", /, a, &
             &//, "Sensor_name : ", &
             & /, "-------------", /, a, &
             &//, "Platform_name : ", &
             & /, "---------------", /, a, &
             &//, "Comment : ", &
             & /, "---------", /, a )' ) &
            INPUT_FILENAME, &
            TRIM( ID_Tag ), &
            TRIM( Title ), &
            TRIM( History ), &
            TRIM( Sensor_Name ), &
            TRIM( Platform_Name ), &
            TRIM( Comment )


  ! -----------------
  ! Get the list data
  ! -----------------

  ! -- Allocate the list arrays
  ALLOCATE( Channel_List( n_Channels ), &
            Angle_List( n_Angles ), &
            Profile_List( n_Profiles ), &
            Molecule_Set_List( n_Molecule_Sets ), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message, '( "Error allocating list arrays array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( MEssage ), &
                          Error_Status )
    STOP
  END IF


  ! -- Read the list data
  Error_Status = Inquire_TauProfile_netCDF( INPUT_FILENAME, &
                                            Channel_List      = Channel_List,     &
                                            Angle_List        = Angle_List,       &
                                            Profile_List      = Profile_List,     &
                                            Molecule_Set_List = Molecule_Set_List )
                                           
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring netCDF TauProfile file '//&
                          INPUT_FILENAME//&
                          ' for list data.', &
                          Error_Status )
    STOP
  END IF

  ! -- Output some info
  WRITE( *, '(//5x, "Channel list of ", a, " TauProfile file: ", /, &
               &10( 1x, i4 ) )' ) INPUT_FILENAME, Channel_List
  WRITE( *, '(  5x, "Angle list of ", a, " TauProfile file: ", /, &
               &7( 1x, f6.3 ) )' ) INPUT_FILENAME, Angle_List
  WRITE( *, '(  5x, "Profile list of ", a, " TauProfile file: ", /, &
               &10( 1x, i4 ) )' ) INPUT_FILENAME, Profile_List
  WRITE( *, '(  5x, "Molecule set list of ", a, " TauProfile file: ", /, &
               &10( 1x, i4 ) )' ) INPUT_FILENAME, Molecule_Set_List

  WRITE( *, '( //5x, "Press <ENTER> to test the netCDF TauProfile Read interfaces..." )' )
  READ( *, * )



  !#----------------------------------------------------------------------------#
  !#                   -- READ THE NETCDF TauProfile FILE --                    #
  !#                                                                            #
  !# Note that the Read_TauProfile_netCDF() function is overloaded so that      #
  !# various groupings of transmittance profiles can be read. See the header    #
  !# documentation for more information.                                        #
  !#----------------------------------------------------------------------------#

  ! -----------------
  ! Rank-1 array read
  ! -----------------

  WRITE( *, '( //5x, "Test reading the netCDF TauProfile file: Rank-1 interface ..." )' )

  ! -- Allocate the array
  ALLOCATE( Tau1( n_Layers ), STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message, '( "Error allocating rank-1 transmittance data array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( MEssage ), &
                          Error_Status )
    STOP
  END IF


  ! -- Read loop
  DO j = 1, n_Molecule_Sets
    DO m = 1, n_Profiles
      DO i = 1, n_Angles
        DO l = 1, n_Channels

          Error_Status = Read_TauProfile_netCDF( INPUT_FILENAME, &
                                                 Channel_List(l), &
                                                 Angle_List(i), &
                                                 Profile_List(m), &
                                                 Molecule_Set_List(j), &
                                                 Tau1 )

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error reading TauProfile file ", a, &
                              &" using rank-1 interface at channel index, ", i4, &
                              &", angle index ", i4, &
                              &", profile index ", i4, &
                              &", and molecule set index ", i4 )' ) &
                            INPUT_FILENAME, l, i, m, j
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM( Message ), &
                                  Error_Status )
            STOP
          END IF

        END DO
      END DO
    END DO
  END DO


  ! -----------------
  ! Rank-2 array read
  ! -----------------

  WRITE( *, '( /5x, "Test reading the netCDF TauProfile file: Rank-2 interface ..." )' )

  ! -- Allocate the array
  ALLOCATE( Tau2( n_Layers, n_Channels ), STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message, '( "Error allocating rank-2 transmittance data array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( MEssage ), &
                          Error_Status )
    STOP
  END IF


  ! -- Read loop
  DO j = 1, n_Molecule_Sets
    DO m = 1, n_Profiles
      DO i = 1, n_Angles

        Error_Status = Read_TauProfile_netCDF( INPUT_FILENAME, &
                                               Angle_List(i), &
                                               Profile_List(m), &
                                               Molecule_Set_List(j), &
                                               Tau2 )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error reading TauProfile file ", a, &
                            &" using rank-2 interface at angle index ", i4, &
                            &", profile index ", i4, &
                            &", and molecule set index ", i4 )' ) &
                          INPUT_FILENAME, i, m, j
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF

      END DO
    END DO
  END DO


  ! -----------------
  ! Rank-3 array read
  ! -----------------

  WRITE( *, '( /5x, "Test reading the netCDF TauProfile file: Rank-3 interface ..." )' )

  ! -- Allocate the array
  ALLOCATE( Tau3( n_Layers, n_Channels, n_Angles ), STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message, '( "Error allocating rank-3 transmittance data array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( MEssage ), &
                          Error_Status )
    STOP
  END IF


  ! -- Read loop
  DO j = 1, n_Molecule_Sets
    DO m = 1, n_Profiles

      Error_Status = Read_TauProfile_netCDF( INPUT_FILENAME, &
                                             Profile_List(m), &
                                             Molecule_Set_List(j), &
                                             Tau3 )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading TauProfile file ", a, &
                          &" using rank-3 interface at profile index ", i4, &
                          &", and molecule set index ", i4 )' ) &
                        INPUT_FILENAME, m, j
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

    END DO
  END DO


  ! -----------------
  ! Rank-4 array read
  ! -----------------

  WRITE( *, '( /5x, "Test reading the netCDF TauProfile file: Rank-4 interface ..." )' )

  ! -- Allocate the array
  ALLOCATE( Tau4( n_Layers, n_Channels, n_Angles, n_Profiles ), STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message, '( "Error allocating rank-4 transmittance data array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( MEssage ), &
                          Error_Status )
    STOP
  END IF


  ! -- Read loop
  DO j = 1, n_Molecule_Sets

    Error_Status = Read_TauProfile_netCDF( INPUT_FILENAME, &
                                           Molecule_Set_List(j), &
                                           Tau4 )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading TauProfile file ", a, &
                        &" using rank-4 interface at molecule set index ", i4 )' ) &
                      INPUT_FILENAME, j
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

  END DO


  ! -----------------
  ! Rank-5 array read
  ! -----------------

  WRITE( *, '( /5x, "Test reading the netCDF TauProfile file: Rank-5 interface ..." )' )

  ! -- Allocate the array
  ALLOCATE( Tau5( n_Layers, n_Channels, n_Angles, n_Profiles, n_Molecule_Sets ), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message, '( "Error allocating rank-5 transmittance data array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( MEssage ), &
                          Error_Status )
    STOP
  END IF


  ! -- Read the data
  Error_Status = Read_TauProfile_netCDF( INPUT_FILENAME, &
                                         Tau5 )

  IF ( Error_Status /= SUCCESS ) THEN
    WRITE( Message, '( "Error reading TauProfile file ", a, &
                      &" using rank-5 interface" )' ) &
                    INPUT_FILENAME
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          Error_Status )
    STOP
  END IF


  ! --------------
  ! Structure read
  ! --------------

  WRITE( *, '( /5x, "Test reading the netCDF TauProfile file: Structure interface ..." )' )

  Error_Status = Read_TauProfile_netCDF( INPUT_FILENAME, &
                                         TauProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF TauProfile file '//&
                          INPUT_FILENAME, &
                          Error_Status )
    STOP
  END IF

  ! -- Output some structure info
  WRITE( *, '( 5x, "TauProfile channels:", /, &
               &10( 1x, i4 ) )' ) TauProfile%Channel
  WRITE( *, '( 5x, "TauProfile angle secants:", /, &
               &7( 1x, f6.3 ) )' ) TauProfile%Angle
  WRITE( *, '( 5x, "TauProfile profiles:", /, &
               &10( 1x, i4 ) )' ) TauProfile%Profile
  WRITE( *, '( 5x, "TauProfile molecule sets:", /, &
               &10( 1x, i4 ) )' ) TauProfile%Molecule_Set



  WRITE( *, '( //5x, "Press <ENTER> to test the netCDF TauProfile Write interfaces..." )' )
  READ( *, * )



  !#----------------------------------------------------------------------------#
  !#                    -- WRITE A NETCDF TauProfile FILE --                    #
  !#                                                                            #
  !# Note that the Write_TauProfile_netCDF() function is overloaded so that     #
  !# various groupings of transmittance profiles can be written. See the header #
  !# documentation for more information.                                        #
  !#----------------------------------------------------------------------------#

  ! ------------------
  ! Rank-1 array write
  ! ------------------

  WRITE( *, '( //5x, "Test writing a netCDF TauProfile file: Rank-1 interface ..." )' )


  ! -- Create an output file
  Output_Filename = 'Output.Rank-1.TauProfile.nc'
  Error_Status = Create_TauProfile_netCDF( TRIM( Output_Filename ), &
                                           TauProfile%Level_Pressure, &
                                           Channel_List, &
                                           Angle_List, &
                                           Profile_List, &
                                           Molecule_Set_List, &
                                           NCEP_Sensor_ID   = NCEP_Sensor_ID,   &
                                           WMO_Satellite_ID = WMO_Satellite_ID, &
                                           WMO_Sensor_ID    = WMO_Sensor_ID,    &
                                           ID_Tag           = TRIM( ID_Tag ), &
                                           Title            = TRIM( Title ), &
                                           History          = PROGRAM_RCS_ID//'; '//&
                                                              TRIM( History ), &
                                           Sensor_Name      = TRIM( Sensor_Name ), &
                                           Platform_Name    = TRIM( Platform_Name ), &
                                           Comment          = 'Rank-1 test output file; '//&
                                                              TRIM( Comment ) )

  ! -- Write loop
  DO j = 1, n_Molecule_Sets
    DO m = 1, n_Profiles
      DO i = 1, n_Angles
        DO l = 1, n_Channels

          Error_Status = Write_TauProfile_netCDF( TRIM( Output_Filename ), &
                                                  TauProfile%Tau(:,l,i,m,j), &
                                                  Channel_List(l), &
                                                  Angle_List(i), &
                                                  Profile_List(m), &
                                                  Molecule_Set_List(j) )

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error writing TauProfile file ", a, &
                              &" using rank-1 interface at channel index, ", i4, &
                              &", angle index ", i4, &
                              &", profile index ", i4, &
                              &", and molecule set index ", i4 )' ) &
                            TRIM( Output_Filename ), l, i, m, j
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM( Message ), &
                                  Error_Status )
            STOP
          END IF

        END DO
      END DO
    END DO
  END DO


  ! ------------------
  ! Rank-2 array write
  ! ------------------

  WRITE( *, '( /5x, "Test writing a netCDF TauProfile file: Rank-2 interface ..." )' )


  ! -- Create an output file
  Output_Filename = 'Output.Rank-2.TauProfile.nc'
  Error_Status = Create_TauProfile_netCDF( TRIM( Output_Filename ), &
                                           TauProfile%Level_Pressure, &
                                           Channel_List, &
                                           Angle_List, &
                                           Profile_List, &
                                           Molecule_Set_List, &
                                           NCEP_Sensor_ID   = NCEP_Sensor_ID,   &
                                           WMO_Satellite_ID = WMO_Satellite_ID, &
                                           WMO_Sensor_ID    = WMO_Sensor_ID,    &
                                           ID_Tag           = TRIM( ID_Tag ), &
                                           Title            = TRIM( Title ), &
                                           History          = PROGRAM_RCS_ID//'; '//&
                                                              TRIM( History ), &
                                           Sensor_Name      = TRIM( Sensor_Name ), &
                                           Platform_Name    = TRIM( Platform_Name ), &
                                           Comment          = 'Rank-2 test output file; '//&
                                                              TRIM( Comment ) )

  ! -- Write loop
  DO j = 1, n_Molecule_Sets
    DO m = 1, n_Profiles
      DO i = 1, n_Angles

        Error_Status = Write_TauProfile_netCDF( TRIM( Output_Filename ), &
                                                TauProfile%Tau(:,:,i,m,j), &
                                                Angle_List(i), &
                                                Profile_List(m), &
                                                Molecule_Set_List(j) )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error writing TauProfile file ", a, &
                            &" using rank-2 interface at angle index ", i4, &
                            &", profile index ", i4, &
                            &", and molecule set index ", i4 )' ) &
                          TRIM( Output_Filename ), i, m, j
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF

      END DO
    END DO
  END DO


  ! ------------------
  ! Rank-3 array write
  ! ------------------

  WRITE( *, '( /5x, "Test writing a netCDF TauProfile file: Rank-3 interface ..." )' )


  ! -- Create an output file
  Output_Filename = 'Output.Rank-3.TauProfile.nc'
  Error_Status = Create_TauProfile_netCDF( TRIM( Output_Filename ), &
                                           TauProfile%Level_Pressure, &
                                           Channel_List, &
                                           Angle_List, &
                                           Profile_List, &
                                           Molecule_Set_List, &
                                           NCEP_Sensor_ID   = TauProfile%NCEP_Sensor_ID,   &
                                           WMO_Satellite_ID = TauProfile%WMO_Satellite_ID, &
                                           WMO_Sensor_ID    = TauProfile%WMO_Sensor_ID,    &
                                           ID_Tag           = TRIM( ID_Tag ), &
                                           Title            = TRIM( Title ), &
                                           History          = PROGRAM_RCS_ID//'; '//&
                                                              TRIM( History ), &
                                           Sensor_Name      = TRIM( Sensor_Name ), &
                                           Platform_Name    = TRIM( Platform_Name ), &
                                           Comment          = 'Rank-3 test output file; '//&
                                                              TRIM( Comment ) )

  ! -- Write loop
  DO j = 1, n_Molecule_Sets
    DO m = 1, n_Profiles

      Error_Status = Write_TauProfile_netCDF( TRIM( Output_Filename ), &
                                              TauProfile%Tau(:,:,:,m,j), &
                                              Profile_List(m), &
                                              Molecule_Set_List(j) )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing TauProfile file ", a, &
                          &" using rank-3 interface at profile index ", i4, &
                          &", and molecule set index ", i4 )' ) &
                        TRIM( Output_Filename ), m, j
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

    END DO
  END DO


  ! ------------------
  ! Rank-4 array write
  ! ------------------

  WRITE( *, '( /5x, "Test writing a netCDF TauProfile file: Rank-4 interface ..." )' )


  ! -- Create an output file
  Output_Filename = 'Output.Rank-4.TauProfile.nc'
  Error_Status = Create_TauProfile_netCDF( TRIM( Output_Filename ), &
                                           TauProfile%Level_Pressure, &
                                           Channel_List, &
                                           Angle_List, &
                                           Profile_List, &
                                           Molecule_Set_List, &
                                           NCEP_Sensor_ID   = NCEP_Sensor_ID,   &
                                           WMO_Satellite_ID = WMO_Satellite_ID, &
                                           WMO_Sensor_ID    = WMO_Sensor_ID,    &
                                           ID_Tag           = TRIM( ID_Tag ), &
                                           Title            = TRIM( Title ), &
                                           History          = PROGRAM_RCS_ID//'; '//&
                                                              TRIM( History ), &
                                           Sensor_Name      = TRIM( Sensor_Name ), &
                                           Platform_Name    = TRIM( Platform_Name ), &
                                           Comment          = 'Rank-4 test output file; '//&
                                                              TRIM( Comment ) )

  ! -- Write loop
  DO j = 1, n_Molecule_Sets

    Error_Status = Write_TauProfile_netCDF( TRIM( Output_Filename ), &
                                            TauProfile%Tau(:,:,:,:,j), &
                                            Molecule_Set_List(j) )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing TauProfile file ", a, &
                        &" using rank-4 interface at molecule set index ", i4 )' ) &
                      TRIM( Output_Filename ), j
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

  END DO


  ! ------------------
  ! Rank-5 array write
  ! ------------------

  WRITE( *, '( /5x, "Test writing a netCDF TauProfile file: Rank-5 interface ..." )' )


  ! -- Create an output file
  Output_Filename = 'Output.Rank-5.TauProfile.nc'
  Error_Status = Create_TauProfile_netCDF( TRIM( Output_Filename ), &
                                           TauProfile%Level_Pressure, &
                                           Channel_List, &
                                           Angle_List, &
                                           Profile_List, &
                                           Molecule_Set_List, &
                                           NCEP_Sensor_ID   = NCEP_Sensor_ID,   &
                                           WMO_Satellite_ID = WMO_Satellite_ID, &
                                           WMO_Sensor_ID    = WMO_Sensor_ID,    &
                                           ID_Tag           = TRIM( ID_Tag ), &
                                           Title            = TRIM( Title ), &
                                           History          = PROGRAM_RCS_ID//'; '//&
                                                              TRIM( History ), &
                                           Sensor_Name      = TRIM( Sensor_Name ), &
                                           Platform_Name    = TRIM( Platform_Name ), &
                                           Comment          = 'Rank-5 test output file; '//&
                                                              TRIM( Comment ) )

  ! -- Write array
  Error_Status = Write_TauProfile_netCDF( TRIM( Output_Filename ), &
                                          TauProfile%Tau )

  IF ( Error_Status /= SUCCESS ) THEN
    WRITE( Message, '( "Error writing TauProfile file ", a, &
                      &" using rank-5 interface" )' ) &
                    TRIM( Output_Filename ), j
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          Error_Status )
    STOP
  END IF


  ! ---------------
  ! Structure write
  ! ---------------

  WRITE( *, '( /5x, "Test writing a netCDF TauProfile file: Structure interface ..." )' )


  ! -- Create an output file
  Output_Filename = 'Output.Structure.TauProfile.nc'
  Error_Status = Create_TauProfile_netCDF( TRIM( Output_Filename ), &
                                           TauProfile%Level_Pressure, &
                                           Channel_List, &
                                           Angle_List, &
                                           Profile_List, &
                                           Molecule_Set_List, &
                                           NCEP_Sensor_ID   = TauProfile%NCEP_Sensor_ID,   &
                                           WMO_Satellite_ID = TauProfile%WMO_Satellite_ID, &
                                           WMO_Sensor_ID    = TauProfile%WMO_Sensor_ID,    &
                                           ID_Tag           = TRIM( ID_Tag ), &
                                           Title            = TRIM( Title ), &
                                           History          = PROGRAM_RCS_ID//'; '//&
                                                              TRIM( History ), &
                                           Sensor_Name      = TRIM( Sensor_Name ), &
                                           Platform_Name    = TRIM( Platform_Name ), &
                                           Comment          = 'Structure test output file; '//&
                                                              TRIM( Comment ) )

  ! -- Write array
  Error_Status = Write_TauProfile_netCDF( TRIM( Output_Filename ), &
                                          TauProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    WRITE( Message, '( "Error writing TauProfile file ", a, &
                      &" using structure interface" )' ) &
                    TRIM( Output_Filename ), j
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                      -- DEALLOCATE ALL THE ARRAYS --                       #
  !#----------------------------------------------------------------------------#

  DEALLOCATE( Channel_List, &
              Angle_List, &
              Profile_List, &
              Molecule_Set_List, &
              STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error deallocating list arrays array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( MEssage ), &
                          WARNING )
  END IF


  DEALLOCATE( Tau1, Tau2, Tau3, Tau4, Tau5, &
              STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error deallocating transmittance data arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          WARNING )
    STOP
  END IF


  WRITE( *, '( //5x, "Press <ENTER> to test for memory leaks..." )' )
  READ( *, * )



  !#----------------------------------------------------------------------------#
  !#                       -- LOOP FOR MEMORY LEAK TEST --                      #
  !#----------------------------------------------------------------------------#

  ! ---------------------------------------
  ! Test the netCDF reader for memory leaks
  ! ---------------------------------------

  WRITE( *, '( /5x, "Looping for netCDF read memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = Read_TauProfile_netCDF( INPUT_FILENAME, &
                                           TauProfile, &
                                           Quiet = 1 )

    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO


  ! -----------------------------------------
  ! Test the Assign function for memory leaks
  ! -----------------------------------------

  WRITE( *, '( /5x, "Looping for structure copy memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = Assign_TauProfile( TauProfile, TauProfile1 )

    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO



  !#----------------------------------------------------------------------------#
  !#                      -- DEALLOCATE ALL THE STRUCTURES --                   #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_TauProfile( TauProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauProfile structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_TauProfile( TauProfile1 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauProfile1 structure.', &
                          WARNING )
  END IF

END PROGRAM TauProfile_netCDF_IO_Test


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: TauProfile_netCDF_IO_Test.f90,v 1.3 2004/09/14 17:27:07 paulv Exp $
!
! $Date: 2004/09/14 17:27:07 $
!
! $Revision: 1.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: TauProfile_netCDF_IO_Test.f90,v $
! Revision 1.3  2004/09/14 17:27:07  paulv
! - Upgraded to Fortran-95
! - Updated to use new TauProfile modules.
! - Added memory leak tests.
!
! Revision 1.2  2003/11/24 19:47:31  paulv
! - Updated to use new TauProfile modules.
!
! Revision 1.1  2002/09/30 17:47:01  paulv
! Initial checkin.
!
!
!
!
