!------------------------------------------------------------------------------
!P+
! NAME:
!       Insert_TauProfile
!
! PURPOSE:
!       Program to insert TauProfile data from one file into another, e.g. from
!       a TauProfile file for a single profile, angle, molecule set into a 
!       "master" TauProfile file containing all profiles, angles, and molecule
!       sets.
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
!       String_Utility:             Module containing string utility routines
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
!       Input: - ASCII SensorInfo file
!              - netCDF TauProfile data file
!
!       Output: Existing netCDF TauProfile data file.
!
! SIDE EFFECTS:
!       The TauProfile data from the input file is inserted into the 
!       appropriate position in the output file. If the output file
!       already contains data in the required "slot", it is overwritten.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Jan-2006
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

PROGRAM Insert_TauProfile


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Error_Handler
  USE String_Utility

  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO

  USE TauProfile_Define
  USE TauProfile_netCDF_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Insert_TauProfile'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Insert_TauProfile.f90,v 1.5 2006/02/06 22:34:25 paulv Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: N_DIRECTIONS = 2
  CHARACTER( * ),  PARAMETER, DIMENSION( N_DIRECTIONS ) :: &
    DIRECTION_NAME = (/ 'upwelling  ', &
                        'downwelling' /)

  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status
  INTEGER :: Allocate_Status

  CHARACTER( 256 )             :: SensorInfo_Filename
  TYPE( SensorInfo_type )      :: SensorInfo
  TYPE( SensorInfo_List_type ) :: SensorInfo_List

  INTEGER :: n_Sensors, iDir, i, j, m, n

  CHARACTER( 256 ) :: Path
  CHARACTER( 256 ) :: InFile
  CHARACTER( 256 ) :: OutFile

  CHARACTER( 5000 ) :: History
  CHARACTER( 5000 ) :: Comment, New_Comment, Tmp_Comment

  TYPE( TauProfile_type ) :: TauProfile

  INTEGER :: i_nK, i_nL, i_nI, i_nM, i_nJ
  INTEGER :: i_NCEP_Sensor_ID
  CHARACTER( 80 ) :: i_ID_Tag, i_Sensor_Name, i_Platform_Name
  INTEGER,         DIMENSION(:), ALLOCATABLE :: i_Channel_List
  REAL( fp_kind ), DIMENSION(:), ALLOCATABLE :: i_Angle_List
  INTEGER,         DIMENSION(:), ALLOCATABLE :: i_Profile_List
  INTEGER,         DIMENSION(:), ALLOCATABLE :: i_Molecule_Set_List

  INTEGER :: o_nK, o_nL, o_nI, o_nM, o_nJ
  INTEGER :: o_NCEP_Sensor_ID
  CHARACTER( 80 ) :: o_ID_Tag, o_Sensor_Name, o_Platform_Name
  INTEGER,         DIMENSION(:), ALLOCATABLE :: o_Channel_List
  REAL( fp_kind ), DIMENSION(:), ALLOCATABLE :: o_Angle_List
  INTEGER,         DIMENSION(:), ALLOCATABLE :: o_Profile_List
  INTEGER,         DIMENSION(:), ALLOCATABLE :: o_Molecule_Set_List



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to insert data from one (smaller) netCDF format", &
             &/5x, "   TauProfile file into another (larger) netCDF format", &
             &/5x, "   TauProfile file")' )
  WRITE( *, '(/5x, " $Revision: 1.5 $")' )
  WRITE( *, '( 5x, a, /)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                      -- READ THE SensorInfo FILE --                        #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Get the SensorInfo filename
  ! ---------------------------

  WRITE( *, FMT     = '( /5x, "Enter the SensorInfo filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) SensorInfo_Filename

  SensorInfo_Filename = ADJUSTL( SensorInfo_Filename )


  ! ------------------------
  ! Read the SensorInfo data
  ! ------------------------

  ! -- First create a new list
  SensorInfo_List = New_SensorInfo_List()

  ! -- Now fill it
  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List, &
                                  Quiet = 1 )
                               
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//TRIM( SensorInfo_Filename ), &
                          FAILURE )
    STOP
  END IF

  ! -- Count the number of sensors
  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )

  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                   -- ENTER THE PATH OF THE DATA TO INSERT --               #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT    ='( /5x, "Enter the path of the datafiles to insert [e.g. ./TauProfile_data] :" )', &
            ADVANCE='NO' )
  READ( *, '(a)' ) Path

  WRITE( *, '( // )' )



  !#----------------------------------------------------------------------------#
  !#            -- BEGIN MAIN LOOP OVER SENSOR/PLATFORM FILES --                #
  !#----------------------------------------------------------------------------#

  Sensor_Loop: DO n = 1, n_Sensors



    !#--------------------------------------------------------------------------#
    !#          -- GET THE CURRENT SensorInfo DATA FROM THE LIST --             #
    !#--------------------------------------------------------------------------#

    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                            n, &
                                            SensorInfo )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error retrieving SensorInfo data for sensor # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER DIRECTIONS --                         #
    !#--------------------------------------------------------------------------#

    Direction_Loop: DO iDir = 1, N_DIRECTIONS


      ! ------------------------------------
      ! Construct input and output filenames
      ! ------------------------------------

      OutFile = TRIM(DIRECTION_NAME(iDir))//'.'//TRIM(SensorInfo%File_Prefix)//'.TauProfile.nc'
      InFile  = TRIM(Path)//'/'//TRIM(OutFile)


      ! ---------------------------
      ! Check that both files exist
      ! ---------------------------

      IF ( .NOT. File_Exists( InFile  ) .OR. &
           .NOT. File_Exists( OutFile )      ) CYCLE Direction_Loop


      ! ---------------------------------
      ! Inquire the input TauProfile file
      ! ---------------------------------

      ! -- Get the dimensions
      Error_Status = Inquire_TauProfile_netCDF( Infile, &
                                                n_Layers        = i_nK, &
                                                n_Channels      = i_nL, &
                                                n_Angles        = i_nI, &
                                                n_Profiles      = i_nM, &
                                                n_Molecule_Sets = i_nJ, &
                                                NCEP_Sensor_ID  = i_NCEP_Sensor_ID, &
                                                ID_Tag          = i_ID_Tag,         &
                                                Sensor_Name     = i_Sensor_Name,    &
                                                Platform_Name   = i_Platform_Name   )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring netCDF TauProfile file '//&
                              TRIM(InFile)//&
                              ' for dimensions and global attributes', &
                              Error_Status )
        STOP
      END IF

      ! -- Allocate the dimension list arrays
      ALLOCATE( i_Channel_List( i_nL ), &
                i_Angle_List( i_nI ), &
                i_Profile_List( i_nM ), &
                i_Molecule_Set_List( i_nJ ), &
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

      ! -- Get the dimension lists
      Error_Status = Inquire_TauProfile_netCDF( InFile, &
                                                Channel_List      = i_Channel_List,     &
                                                Angle_List        = i_Angle_List,       &
                                                Profile_List      = i_Profile_List,     &
                                                Molecule_Set_List = i_Molecule_Set_List )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring netCDF TauProfile file '//&
                              TRIM(InFile)//&
                              ' for dimensions lists', &
                              Error_Status )
        STOP
      END IF


      ! ----------------------------------
      ! Inquire the output TauProfile file
      ! ----------------------------------

      ! -- Get the dimensions
      Error_Status = Inquire_TauProfile_netCDF( Outfile, &
                                                n_Layers        = o_nK, &
                                                n_Channels      = o_nL, &
                                                n_Angles        = o_nI, &
                                                n_Profiles      = o_nM, &
                                                n_Molecule_Sets = o_nJ, &
                                                NCEP_Sensor_ID  = o_NCEP_Sensor_ID, &
                                                History         = History, &
                                                Comment         = Comment, &
                                                ID_Tag          = o_ID_Tag,         &
                                                Sensor_Name     = o_Sensor_Name,    &
                                                Platform_Name   = o_Platform_Name   )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring netCDF TauProfile file '//&
                              TRIM(OutFile)//&
                              ' for dimensions and global attributes', &
                              Error_Status )
        STOP
      END IF

      ! -- Allocate the dimension list arrays
      ALLOCATE( o_Channel_List( o_nL ), &
                o_Angle_List( o_nI ), &
                o_Profile_List( o_nM ), &
                o_Molecule_Set_List( o_nJ ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating dimension list arrays for output file ", a, &
                          &". STAT = ", i5 )' ) &
                        TRIM( OutFile ), Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

      ! -- Get the dimension lists
      Error_Status = Inquire_TauProfile_netCDF( OutFile, &
                                                Channel_List      = o_Channel_List,     &
                                                Angle_List        = o_Angle_List,       &
                                                Profile_List      = o_Profile_List,     &
                                                Molecule_Set_List = o_Molecule_Set_List )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring netCDF TauProfile file '//&
                              TRIM(OutFile)//&
                              ' for dimensions lists', &
                              Error_Status )
        STOP
      END IF


      ! ----------------------------
      ! Compare the TauProfile files
      ! ----------------------------

      ! -- Layer and channel dimensions must agree
      IF ( i_nK /= o_nK .OR. &
           i_nL /= o_nL      ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Incompatible layer/channel dimensions for '//&
                              TRIM(SensorInfo%File_Prefix)//&
                              ' TauProfile files', &
                              FAILURE )
        STOP
      END IF

      ! -- The channel number lists must be the same
      IF ( ANY( (i_Channel_List - o_Channel_List ) /= 0 ) ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Different channel number lists for '//&
                              TRIM(SensorInfo%File_Prefix)//&
                              ' TauProfile files', &
                              FAILURE )
        STOP
      END IF

      ! -- Angle value
      DO i = 1, i_nI
        IF ( COUNT( o_Angle_List == i_Angle_List(i) ) /= 1 ) THEN
          WRITE( Message, '( "Input file angle #, ", i3, "(", f4.2, ") not present in output file ", a )' ) &
                          i, i_Angle_List(i), TRIM( OutFile )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                FAILURE )
          STOP
        END IF
      END DO

      ! -- Profile number
      DO m = 1, i_nM
        IF ( COUNT( o_Profile_List == i_Profile_List(m) ) /= 1 ) THEN
          WRITE( Message, '( "Input file profile #, ", i3, " not present in output file ", a )' ) &
                          i_Profile_List(m), TRIM( OutFile )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                FAILURE )
          STOP
        END IF
      END DO

      ! -- Molecule set number
      DO j = 1, i_nJ
        IF ( COUNT( o_Molecule_Set_List == i_Molecule_Set_List(j) ) /= 1 ) THEN
          WRITE( Message, '( "Input file molecule set #, ", i3, " not present in output file ", a )' ) &
                          i_Molecule_Set_List(j), TRIM( OutFile )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                FAILURE )
          STOP
        END IF
      END DO

      ! -- Other IDs/names/etc
      IF ( i_NCEP_Sensor_ID /= o_NCEP_Sensor_ID ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Different NCEP Sensor IDs for '//&
                              TRIM(SensorInfo%File_Prefix)//&
                              ' TauProfile files', &
                              FAILURE )
        STOP
      END IF

      IF ( TRIM(i_ID_Tag) /= TRIM(o_ID_Tag) ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Different profile set ID tag for '//&
                              TRIM(SensorInfo%File_Prefix)//&
                              ' TauProfile files', &
                              FAILURE )
        STOP
      END IF

      IF ( StrUpCase(TRIM(i_Sensor_Name)) /= StrUpCase(TRIM(o_Sensor_Name)) ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Different sensor identifiers for '//&
                              TRIM(SensorInfo%File_Prefix)//&
                              ' TauProfile files', &
                              FAILURE )
        STOP
      END IF

      IF ( StrUpCase(TRIM(i_Platform_Name)) /= StrUpCase(TRIM(o_Platform_Name)) ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Different platform identifiers for '//&
                              TRIM(SensorInfo%File_Prefix)//&
                              ' TauProfile files: '//TRIM(i_Platform_Name)//&
                              ' and '//TRIM(o_Platform_Name), &
                              FAILURE )
        STOP
      END IF



      ! -------------------------
      ! Read the input TauProfile
      ! -------------------------

      Error_Status = Read_TauProfile_netCDF( InFile, &
                                             TauProfile )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading netCDF TauProfile file '//&
                              TRIM( InFile ), &
                              Error_Status )
        STOP
      END IF


      ! -----------------------------------------------------------
      ! Write the transmittance data. The data is written as rank-2
      ! arrays looping over molecule set, profile, and angle.
      ! -----------------------------------------------------------

      j_Loop: DO j = 1, i_nJ
        m_Loop: DO m = 1, i_nM
          i_Loop: DO i = 1, i_nI

            Error_Status = Write_TauProfile_netCDF( OutFile, &
                                                    TauProfile%Tau(:,:,i,m,j), &
                                                    i_Angle_List(i), &
                                                    i_Profile_List(m), &
                                                    i_Molecule_Set_List(j) )

            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message, '( "Error inserting angle ", i3, "(", f4.2, "), profile ", i3, &
                                &" and molecule set ", i3, " into ", a )' ) &
                              i, i_Angle_List(i), i_Profile_List(m), i_Molecule_Set_List(j), &
                              TRIM( OutFile )
              CALL Display_Message( PROGRAM_NAME, &
                                    TRIM( Message ), &
                                    Error_Status )
              STOP
            END IF

          END DO i_Loop
        END DO m_Loop
      END DO j_Loop


      ! ---------------------------------------
      ! Modify the output file TauProfile GAtts
      ! ---------------------------------------

      WRITE( New_Comment, '( "Updated data for angle(s): ", 7(f5.3,:) )' ) i_Angle_List
      WRITE( Tmp_Comment, '( ", profile(s): ", 99(i3,:) )' ) i_Profile_List
      New_Comment = TRIM( New_Comment )//TRIM( Tmp_Comment )
      WRITE( Tmp_Comment, '( ", molecule set(s): ", 99(i4,:) )' ) i_Molecule_Set_List
      New_Comment = TRIM( New_Comment )//TRIM( Tmp_Comment )


      Error_Status = Modify_TauProfile_GAtts( OutFile, &
                                              Sensor_Name   = TRIM( SensorInfo%Sensor_Name ), &
                                              Platform_Name = TRIM( SensorInfo%Satellite_Name ), &
                                              Title = TRIM( SensorInfo%Sensor_Name )//' '//&
                                                      TRIM(DIRECTION_NAME(iDir))//&
                                                      ' transmittances for '//&
                                                      TRIM( SensorInfo%Satellite_Name ), &
                                              History = PROGRAM_RCS_ID//'; '//&
                                                        TRIM( History ), &
                                              Comment = TRIM( New_Comment )//'; '//&
                                                        TRIM( Comment ) )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error modifying GAtts for netCDF TauProfile file '//&
                              TRIM( OutFile ), &
                              Error_Status )
        STOP
      END IF

      WRITE( *, '( 5x, a, ". ", a )' ) TRIM( OutFile ), TRIM( New_Comment )


      ! ------------------------------------
      ! Deallocate the dimension list arrays
      ! ------------------------------------

      DEALLOCATE( i_Channel_List, i_Angle_List, i_Profile_List, i_Molecule_Set_List, &
                  o_Channel_List, o_Angle_List, o_Profile_List, o_Molecule_Set_List, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error deallocating dimension list arrays for input file ", a, &
                          &". STAT = ", i5 )' ) &
                        TRIM( InFile ), Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

    END DO Direction_Loop

  END DO Sensor_Loop


  !#----------------------------------------------------------------------------#
  !#            -- DESTROY THE SensorInfo STRUCTURE AND LINKED LIST --          #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_SensorInfo( SensorInfo )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo data structure.', &
                          WARNING )
  END IF


  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          '( "Error destroying SensorInfo_List.', &
                          WARNING )
  END IF

END PROGRAM Insert_TauProfile


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Insert_TauProfile.f90,v 1.5 2006/02/06 22:34:25 paulv Exp $
!
! $Date: 2006/02/06 22:34:25 $
!
! $Revision: 1.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Insert_TauProfile.f90,v $
! Revision 1.5  2006/02/06 22:34:25  paulv
! - Cosmetic changes to output.
!
! Revision 1.4  2006/02/06 20:42:59  paulv
! - Now using String_Utility functions in sensor and platform name comparisons.
!
! Revision 1.3  2006/02/06 20:34:17  paulv
! - Altered code so that the TauProfile data to insert drives how much data
!   get inserted. Now only the number of layers and channels needs to be the
!   same.
!
! Revision 1.2  2006/01/26 21:26:08  paulv
! - Also updated Title in TauProfile write.
! - Added some info output.
!
! Revision 1.1  2006/01/26 21:16:01  paulv
! Initial checkin.
!
!
!
!
