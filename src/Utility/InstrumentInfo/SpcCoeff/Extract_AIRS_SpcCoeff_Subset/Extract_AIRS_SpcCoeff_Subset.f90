!------------------------------------------------------------------------------
!M+
! NAME:
!       Extract_AIRS_SpcCoeff_Subset
!
! PURPOSE:
!       Program to extract AIRS channel subsets from the individual
!       AIRS module netCDF format SpcCoeff data files.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:           Module containing definitions for kinds
!                             of variable types.
!
!       Message_Handler:      Module to define simple error codes and
!                             handle error conditions
!                             USEs: FILE_UTILITY module
!
!       List_File_Utility:    Module containing routines for reading list
!                             files, i.e. ASCII files that contain lists of
!                             character or integer data, one item per line.
!                             USEs: TYPE_KINDS module
!                                   FILE_UTILITY module
!                                   ERROR_HANDLER module
!
!       SpcCoeff_Define:      Module defining the SpcCoeff data structure
!                             and containing routines to manipulate it.
!                             USEs: TYPE_KINDS module
!                                   FILE_UTILITY module
!                                   ERROR_HANDLER module
!
!       SpcCoeff_netCDF_IO:   Module containing routines to read and
!                             write netCDF format SpcCoeff files.
!                             USEs: TYPE_KINDS module
!                                   ERROR_HANDLER module
!                                   SPCCOEFF_DEFINE module
!                                   NETCDF module
!                                   NETCDF_UTILITY module
!
!       AIRS_Define:          Module containing AIRS channel and module
!                             definitions.
!                             USEs: TYPE_KINDS module
!                                   ERROR_HANDLER module
!
!       AIRS_Subset:          Module containing AIRS channel subsetting
!                             definitions and routines.
!                             USEs: TYPE_KINDS module
!                                   ERROR_HANDLER module
!                                   AIRS_SUBSET_DEFINE module
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
!       Input: - netCDF format individual AIRS module SpcCoeff datafiles.
!              - For user specified channel subsetting, a list file containing
!                the required AIRS channels to subset.
!
!       Output: netCDF format AIRS channel SUBSET SpcCoeff datafile.
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten.
!
! RESTRICTIONS:
!       *ALL* of the required input data must be present for the output
!       file to be successfully written.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Nov-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002, 2004 Paul van Delst
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

PROGRAM Extract_AIRS_SpcCoeff_Subset


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler
  USE List_File_Utility

  USE SpcCoeff_Define
  USE SpcCoeff_netCDF_IO

  USE AIRS_Define
  USE AIRS_Subset


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Extract_AIRS_SpcCoeff_Subset'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Extract_AIRS_SpcCoeff_Subset.f90,v 6.2 2006/05/02 16:58:03 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER,        PARAMETER :: N_VALID_SETS = 3
  CHARACTER( * ), PARAMETER, DIMENSION( N_VALID_SETS ) :: VALID_SET_NAME = (/ '281 channel set', &
                                                                              '324 channel set', &
                                                                              'User specified ' /)
  INTEGER,        PARAMETER :: MAX_N_INVALID = 5


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: i, Set, Invalid

  CHARACTER( 256 ) :: List_Filename
  TYPE( Integer_List_File_type ) :: User_Subset_List

  CHARACTER( 256 ) :: Sensor_Descriptor

  CHARACTER( 256 ) :: In_Filename
  CHARACTER( 256 ) :: Out_Filename

  INTEGER :: Release
  INTEGER :: Version

  CHARACTER( 5000 ) :: History
  CHARACTER(  256 ) :: Sensor_Name
  CHARACTER(  256 ) :: Platform_Name
  CHARACTER( 5000 ) :: Comment

  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: Allocate_Status

  INTEGER :: l, l1, l2
  LOGICAL :: First_Module
  
  INTEGER                            :: n_Subset_Channels
  CHARACTER( 256 )                   :: Subset_Comment
  INTEGER, DIMENSION(:), ALLOCATABLE :: Subset_List
  
  TYPE( SpcCoeff_Sensor_type ) :: In_SpcCoeff
  TYPE( SpcCoeff_Sensor_type ) :: Out_SpcCoeff

  TYPE( AIRS_Subset_type ) :: Subset



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to extract the AIRS channel SUBSET spectral")' )
  WRITE( *, '( 5x, "   coefficient data from the individual module netCDF")' )
  WRITE( *, '( 5x, "   SpcCoeff files and write them to a separate netCDF")' )
  WRITE( *, '( 5x, "   datafile.")' )
  WRITE( *, '(/5x, " $Revision: 6.2 $")' )
  WRITE( *, '( 5x, a, /)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                       -- SELECT A SUBSET SET --                            #
  !#----------------------------------------------------------------------------#

  Invalid = 0

  Set_Loop: DO


    ! ----------------------------------
    ! Prompt user to select a subset set 
    ! ----------------------------------

    WRITE( *, '( /5x, "Select an AIRS channel subset" )' )
    DO i = 1, N_VALID_SETS
      WRITE( *, '( 10x, i1, ") ", a )' ) i, VALID_SET_NAME(i)
    END DO
    WRITE( *, FMT     = '( 5x, "Enter choice: " )', &
              ADVANCE = 'NO' )

    READ( *, FMT    = '( i2 )', &
             IOSTAT = IO_Status ) Set


    ! ---------------
    ! Check the input
    ! ---------------

    IF ( IO_Status /= 0 ) THEN
      Invalid = Invalid + 1
    ELSE
      IF ( Set < 1 .OR. Set > N_VALID_SETS ) THEN
        Invalid = Invalid + 1
      ELSE
        EXIT Set_Loop
      END IF
    END IF

    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid subset selection', &
                          INFORMATION )


    ! -----------------------------------------
    ! Only loop so many times for correct input
    ! -----------------------------------------

    IF ( Invalid == MAX_N_INVALID ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Too many invalid entries.', &
                            FAILURE )
      STOP
    END IF

  END DO Set_Loop


  
  !#----------------------------------------------------------------------------#
  !#                   -- GET THE REQUIRED CHANNELS LIST --                     #
  !#----------------------------------------------------------------------------#

  SELECT CASE ( Set )


    ! ----------------------
    ! The 281 channel subset
    ! ----------------------

    CASE ( 1 )
      n_Subset_Channels = N_AIRS_SUBSET_281
      Subset_Comment = AIRS_SUBSET_281_COMMENT

      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating Subset_List array. STAT = ", i5 )' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

      Subset_List = AIRS_SUBSET_281

      Sensor_Descriptor = 'airs281SUBSET_aqua'


    ! ----------------------
    ! The 324 channel subset
    ! ----------------------

    CASE ( 2 )
      n_Subset_Channels = N_AIRS_SUBSET_324
      Subset_Comment = AIRS_SUBSET_324_COMMENT

      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating Subset_List array. STAT = ", i5 )' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

      Subset_List = AIRS_SUBSET_324

      Sensor_Descriptor = 'airs324SUBSET_aqua'


    ! -------------------------------
    ! A user specified channel subset
    ! -------------------------------

    CASE ( 3 )

      ! -- Get a channel subset list filename
      WRITE( *, FMT     = '( /5x, "Enter an AIRS channel subset list filename : " )', &
                ADVANCE = 'NO' )
      READ( *, FMT = '( a )' ) List_Filename
      List_Filename = ADJUSTL( List_Filename )

      ! -- Read the channel subset list file
      Error_Status = Read_List_File( List_Filename, &
                                     User_Subset_List )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading list file '//TRIM( List_Filename ), &
                              Error_Status )
        STOP
      END IF

      ! -- Retrieve the number of subset channels
      n_Subset_Channels = Get_List_Size( User_Subset_List )

      IF ( n_Subset_Channels < 1 ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'No channels listed in '//TRIM( List_Filename ), &
                              Error_Status )
        STOP
      END IF

      ! -- Check the number of channels
      IF ( n_Subset_Channels < 1 .OR. n_Subset_Channels > N_AIRS_CHANNELS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Number of channels listed in '//TRIM( List_Filename )//' outside of valid range.', &
                              Error_Status )
        STOP
      END IF

      ! -- Allocate the subset list to use
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating Subset_List array. STAT = ", i5 )' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

      ! -- Fill the subset list
      DO l = 1, n_Subset_Channels

        Error_Status = Get_List_Entry( User_Subset_List, l, Subset_List(l) )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error retrieving user subset channel list entry ", i4 )' ) l
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF

      END DO

      ! -- Create the sensor descriptor
      WRITE( Sensor_Descriptor, '( i4 )' ) n_Subset_Channels
      Sensor_Descriptor = 'airs'//TRIM( ADJUSTL( Sensor_Descriptor ) )//'SUBSET_aqua'

  END SELECT




  !#----------------------------------------------------------------------------#
  !#                  -- ALLOCATE OUTPUT SpcCoeff STRUCTURE --                  #
  !#----------------------------------------------------------------------------#

  Error_Status = Allocate_SpcCoeff( n_Subset_Channels, &
                                    Out_SpcCoeff )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating output SpcCoeff data structure.', &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#        -- LOOP OVER MODULE DATA FILES TO EXTRACT SUBSET CHANNELS --        #
  !#----------------------------------------------------------------------------#

  ! -------------------------------------
  ! Initialise the start index for output
  ! and the "initial module" flag
  ! -------------------------------------

  l1 = 1
  First_Module = .TRUE.


  ! -----------------
  ! Begin module loop
  ! -----------------

  Module_Loop: DO l = 1, N_AIRS_MODULES



    !#--------------------------------------------------------------------------#
    !#                   -- CURRENT MODULE CHANNEL SUBSET --                    #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Determine the subset channel indices
    ! for the current module
    ! ------------------------------------
      
    Error_Status = Index_AIRS_Subset( l, Subset_List, Subset )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error extracting subset channel indices for module '//&
                            TRIM( AIRS_MODULE( l ) ), &
                            Error_Status )
      STOP
    END IF


    ! ----------------------------------------
    ! Output the number of channels to extract
    ! ----------------------------------------

    WRITE( *, '(/10x, "There are ", i3, " channels to be extracted from module ", a, ":" )' ) &
              Subset%n_Channels, TRIM( AIRS_MODULE( l ) )



    !#--------------------------------------------------------------------------#
    !#               -- READ THE INPUT SpcCoeff FILE IF REQUIRED --             #
    !#--------------------------------------------------------------------------#

    Non_Zero_n_Channels: IF ( Subset%n_Channels > 0 ) THEN


      ! ---------------------------------------------
      ! Output the list of channel numbers to extract
      ! ---------------------------------------------

      WRITE( *, FMT = '( 10x, 10i5 )' ) Subset%Channel_Number


      ! -------------------
      ! Define the filename
      ! -------------------

      In_Filename = 'airs'//&
                       TRIM( AIRS_MODULE( l ) )//&
                       '_aqua.SpcCoeff.nc'
!                       '_aqua.Sensor.SpcCoeff.nc'


      ! ----------------------------------------
      ! Get the file release/version info and
      ! global attributes for the initial module
      ! ----------------------------------------

      IF ( First_Module ) THEN

        ! --- Update the test flag
        First_Module = .FALSE.

        ! -- Inquire the SpcCoeff data file
        Error_Status = Inquire_SpcCoeff_netCDF( TRIM( In_Filename ), &
                                                Release       = Out_SpcCoeff%Release, &
                                                Version       = Out_SpcCoeff%Version, &
                                                History       = History, &
                                                Sensor_Name   = Sensor_Name, &
                                                Platform_Name = Platform_Name, &
                                                Comment       = Comment )

        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error inquiring the input netCDF SpcCoeff file '//&
                                TRIM( In_Filename ), &
                                Error_Status )
          STOP
        END IF

        ! -- Append onto the comment attribute.
        Comment = TRIM( Subset_Comment )//'; '//TRIM( Comment )

      END IF


      ! --------------------------------------------------------
      ! Get the Release/Version info for all modules and compare
      ! --------------------------------------------------------

      Error_Status = Inquire_SpcCoeff_netCDF( TRIM( In_Filename ), &
                                              Release = Release, &
                                              Version = Version )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring the input netCDF SpcCoeff file '//&
                              TRIM( In_Filename )//' for Release/Version info.', &
                              Error_Status )
        STOP
      END IF


      ! -------------------------------------
      ! Check the Release and Version numbers
      ! -------------------------------------

      ! -- Check the Release value. If different - issue error and stop
      IF ( Out_SpcCoeff%Release /= Release ) THEN
        WRITE( Message, '( "Input file ", a, " Release, ", i2, &
                          &", is different from previous file value, ", i2, "." )' ) &
                        TRIM( In_Filename ), Release, Out_SpcCoeff%Release
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

      ! -- Check the Version value. If different - issue warning and continue, 
      ! -- but modify the Comment global attribute field for output
      IF ( Out_SpcCoeff%Version /= Version ) THEN
        WRITE( Message, '( "Input file ", a, " Version, ", i2, &
                          &", is different from previous file value, ", i2, "." )' ) &
                        TRIM( In_Filename ), Version, Out_SpcCoeff%Version
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              WARNING )
        Comment = TRIM( Message )//'; '//TRIM( Comment )
      END IF


      ! -------------
      ! Read the data
      ! -------------

      Error_Status = Read_SpcCoeff_netCDF( In_Filename, &
                                           In_SpcCoeff )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading netCDF AIRS SpcCoeff file '//&
                              TRIM( In_Filename ), &
                              Error_Status )
        STOP
      END IF
    

      ! --------------------------------
      ! Copy the required channel's data
      ! --------------------------------

      l2 = l1 + Subset%n_Channels - 1

      Out_SpcCoeff%Sensor_Descriptor( l1:l2 )          = TRIM( Sensor_Descriptor )
      Out_SpcCoeff%Sensor_Type( l1:l2 )                = In_SpcCoeff%Sensor_Type( Subset%Channel_Index )
      Out_SpcCoeff%NCEP_Sensor_ID( l1:l2 )             = In_SpcCoeff%NCEP_Sensor_ID( Subset%Channel_Index )
      Out_SpcCoeff%WMO_Satellite_ID( l1:l2 )           = In_SpcCoeff%WMO_Satellite_ID( Subset%Channel_Index )
      Out_SpcCoeff%WMO_Sensor_ID( l1:l2 )              = In_SpcCoeff%WMO_Sensor_ID( Subset%Channel_Index )
      Out_SpcCoeff%Sensor_Channel( l1:l2 )             = In_SpcCoeff%Sensor_Channel( Subset%Channel_Index )

      Out_SpcCoeff%Frequency( l1:l2 )                  = In_SpcCoeff%Frequency( Subset%Channel_Index )
      Out_SpcCoeff%Wavenumber( l1:l2 )                 = In_SpcCoeff%Wavenumber( Subset%Channel_Index )
      Out_SpcCoeff%Planck_C1( l1:l2 )                  = In_SpcCoeff%Planck_C1( Subset%Channel_Index )
      Out_SpcCoeff%Planck_C2( l1:l2 )                  = In_SpcCoeff%Planck_C2( Subset%Channel_Index )
      Out_SpcCoeff%Band_C1( l1:l2 )                    = In_SpcCoeff%Band_C1( Subset%Channel_Index )
      Out_SpcCoeff%Band_C2( l1:l2 )                    = In_SpcCoeff%Band_C2( Subset%Channel_Index )
      Out_SpcCoeff%Is_Microwave_Channel( l1:l2 )       = In_SpcCoeff%Is_Microwave_Channel( Subset%Channel_Index )
      Out_SpcCoeff%Polarization( l1:l2 )               = In_SpcCoeff%Polarization( Subset%Channel_Index )
      Out_SpcCoeff%Cosmic_Background_Radiance( l1:l2 ) = In_SpcCoeff%Cosmic_Background_Radiance( Subset%Channel_Index )
      Out_SpcCoeff%Is_Solar_Channel( l1:l2 )           = In_SpcCoeff%Is_Solar_Channel( Subset%Channel_Index )
      Out_SpcCoeff%Solar_Irradiance( l1:l2 )           = In_SpcCoeff%Solar_Irradiance( Subset%Channel_Index )

      l1 = l2 + 1


      ! -------------------------------------
      ! Destropy the input SpcCoeff structure
      ! for the next module read
      ! -------------------------------------

      Error_Status = Destroy_SpcCoeff( In_SpcCoeff )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff structure for input from '//&
                              TRIM( In_Filename ), &
                              Error_Status )
        STOP
      END IF

    END IF Non_Zero_n_Channels



    !#--------------------------------------------------------------------------#
    !#                 -- DESTROY THE AIRS_Subset STRUCTURE --                  #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_AIRS_Subset( Subset )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying AIRS Subset structure for input from '//&
                            TRIM( In_Filename ), &
                            Error_Status )
      STOP
    END IF

  END DO Module_Loop



  !#----------------------------------------------------------------------------#
  !#                     -- WRITE THE OUTPUT SpcCoeff FILE --                   #
  !#----------------------------------------------------------------------------#

  ! -----------------------
  ! Set the output filename
  ! -----------------------

  Out_Filename = TRIM( Sensor_Descriptor )//'.Sensor.SpcCoeff.nc'


  ! -------------------------
  ! Set the number of sensors
  ! -------------------------

  Out_SpcCoeff%n_Sensors = 1


  ! --------------
  ! Write the data
  ! --------------

  WRITE( *, '(/10x, "Creating the output file..." )' )

  Error_Status = Write_SpcCoeff_netCDF( TRIM( Out_Filename ), &
                                        Out_SpcCoeff, &
                                        Title         = 'Spectral coefficients for '//&
                                                        TRIM( Platform_Name )//' '//&
                                                        TRIM( Sensor_Name )//&
                                                        ' SUBSET channel set.', &
                                        History       = PROGRAM_RCS_ID//'; '//&
                                                        TRIM( History ), &
                                        Sensor_Name   = TRIM( Sensor_Name ), &
                                        Platform_Name = TRIM( Platform_Name ), &
                                        Comment       = 'Data extracted from the individual '//&
                                                        'AIRS module SpcCoeff datafiles.; '//&
                                                        TRIM( Comment ) )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing the AIRS subset SpcCoeff file '//&
                          TRIM( Out_Filename ), &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                 -- DESTROY THE OUTPUT SpcCoeff STRUCTURE --                #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_SpcCoeff( Out_SpcCoeff )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SpcCoeff structure for output to '//&
                          TRIM( Out_Filename ), &
                          Error_Status )
  END IF

END PROGRAM Extract_AIRS_SpcCoeff_Subset


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Extract_AIRS_SpcCoeff_Subset.f90,v 6.2 2006/05/02 16:58:03 dgroff Exp $
!
! $Date: 2006/05/02 16:58:03 $
!
! $Revision: 6.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Extract_AIRS_SpcCoeff_Subset.f90,v $
! Revision 6.2  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 6.1  2005/12/26 18:34:42  paulv
! - Updated SpcCoeff filename for Sensor format.
!
! Revision 6.0  2005/08/03 17:42:14  paulv
! - Changed SpcCoeff type and file definitions to be SENSOR specific.
!
! Revision 5.0  2005/04/01 19:06:28  paulv
! - Update for SpcCoeff Release5
!
! Revision 2.9  2004/09/09 19:32:23  paulv
! - New version allowing user to control the channel subsetting a bit more.
! - Using new Fortran95 support modules. No structure initialisation functions.
!
! Revision 2.8  2004/08/02 16:41:05  paulv
! - Changed SpcCoeff initialization call from Initialize_SpcCoeff() to
!   Init_SpcCoeff().
!
! Revision 2.7  2004/05/17 19:05:54  paulv
! - Changed the assignment of the Sensor_Descriptor SpcCoeff structure component
!   from the individual module names to airsSUBSET_aqua.
!
! Revision 2.6  2004/05/17 19:00:38  paulv
! - Added assignment of Sensor_Descriptor component of the SpcCoeff structure.
!
! Revision 2.5  2003/12/01 20:44:12  paulv
! - Altered method of determing the subset channel indices in each module.
!   Now calling the Index_AIRS_Subset() function in the AIRS_Define module
!   using the AIRS_Subset structure.
! - All references to previous subset method changed to use the AIRS_Subset
!   structure components.
!
! Revision 2.4  2003/10/24 18:18:14  paulv
! - Code category changed from
!     NCEP RTM : Coefficients : SpcCoeff
!   to
!     Instrument Information : SpcCoeff
!
! Revision 2.3  2003/06/19 17:12:32  paulv
! - Corrected bug in subset channel indexing after the last channel had been found.
!
! Revision 2.2  2003/02/12 19:04:15  paulv
! - Updated to use new SpcCoeff netCDF I/O module routines.
!
! Revision 2.1  2002/12/27 19:33:35  paulv
! - New version. netCDF access routines used for all I/O.
!
! Revision 1.2  2002/11/26 13:31:06  paulv
! - Changed the default input and output filenames to the same form as the
!   current operational files.
!
! Revision 1.1  2002/11/25 22:29:54  paulv
! Initial checkin.
!
!
!
!
!
