!------------------------------------------------------------------------------
!P+
! NAME:
!       Interpolate_SRFs
!
! PURPOSE:
!       Program to read the netCDF SRF data files and interpolate the SRFs to
!       to the same frequency grid used in the line-by-line transmittance data
!       files.
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
!       File_Utility:               Module containing generic file utility
!                                   routines
!
!       Message_Handler:              Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       Interpolate_Utility:        Module containing interpolation routines
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       SensorInfo_Define:          Module defining the SensorInfo data
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       SensorInfo_LinkedList:      Module defining the SensorInfo Linked
!                                   List data structure and containing
!                                   routines to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         SENSORINFO_DEFINE module
!
!       SensorInfo_IO:              Module continaing routines to read and
!                                   write ASCII SensorInfo format files.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         SENSORINFO_DEFINE module
!
!       SRF_Define:                 Module defining the generic SRF data
!                                   structure and its manipulation routines.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         INTEGRATE module
!
!       SRF_netCDF_IO:              Module containing routines to read and
!                                   write netCDF format SRF files.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         SRF_DEFINE module
!                                         NETCDF module
!                                         NETCDF_UTILITY module
!
!       Tau_Production_Parameters:  Module defining parameters used in the LBL
!                                   transmittance production runs
!                                   USEs: TYPE_KINDS module
!                                         LBLRTM_PARAMETERS module
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
!       Input: - User specified SensorInfo file.
!              - Original netCDF format SRF files
!
!       Output: Interpolated netCDF format SRF files
!
! SIDE EFFECTS:
!       If the interpolated output netCDF format SRF file already
!       exists, it is overwritten.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-May-2002
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

PROGRAM Interpolate_SRFs


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE Interpolate_Utility

  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO

  USE SRF_Define
  USE SRF_netCDF_IO

  USE Tau_Production_Parameters


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ),  PARAMETER :: PROGRAM_NAME = 'Interpolate_SRFs'
  CHARACTER( * ),  PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Interpolate_SRFs.f90,v 2.8 2006/07/26 22:51:20 wd20pd Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Numeric literals
  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ONEpointFIVE = 1.5_fp_kind

  ! -- The index for the required frequency interval = 1
  ! -- This corresponds to the 0.1cm^-1 frequency interval
  ! -- from the Tau_Production_Parameters module.
  INTEGER, PARAMETER :: INTERPOLATION_INDEX = 1

  ! -- Interpolation order
  ! -- If 1 == linear
  ! --    3 == cubic
  INTEGER, PARAMETER :: ORDER = 3

  ! -- Keyword set (TRUE/ON) value
  INTEGER, PARAMETER :: SET = 1


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: SensorInfo_Filename
  CHARACTER( 256 ) ::  SRF_Filename
  CHARACTER( 256 ) :: iSRF_Filename

  INTEGER :: Error_Status

  INTEGER           :: n_Channels, l
  INTEGER           :: NCEP_Sensor_ID
  INTEGER           :: WMO_Satellite_ID
  INTEGER           :: WMO_Sensor_ID
  CHARACTER( 256 )  :: Title
  CHARACTER( 5000 ) :: History
  CHARACTER( 256 )  :: Sensor_Name
  CHARACTER( 256 )  :: Platform_Name
  CHARACTER( 5000 ) :: Comment

  REAL( fp_kind ) :: dF
  INTEGER :: n_iPoints, i1, i2

  TYPE( SRF_type ) ::  SRF
  TYPE( SRF_type ) :: iSRF

  INTEGER :: n_Sensors, n
  TYPE( SensorInfo_type )      :: SensorInfo
  TYPE( SensorInfo_List_type ) :: SensorInfo_List



  !#----------------------------------------------------------------------------#
  !#                   -- INITIALIZE LINKED LIST STRUCTURE --                   #
  !#----------------------------------------------------------------------------#

  SensorInfo_List = New_SensorInfo_List()



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to read the netCDF SRF data files and interpolate ")' )
  WRITE( *, '( 5x, "   the SRFs to the same frequency grid used in the line-   ")' )
  WRITE( *, '( 5x, "   by-line transmittance data files                        ")' )
  WRITE( *, '(/5x, " $Revision: 2.8 $")' )
  WRITE( *, '( 5x, a, /)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                       -- READ THE SensorInfo FILE --                       #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Get the SensorInfo filename
  ! ---------------------------

  WRITE( *, FMT     = '( /5x, "Enter a SensorInfo filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) SensorInfo_Filename
  SensorInfo_Filename = ADJUSTL( SensorInfo_Filename )


  ! -----------------------------------
  ! Populate the SensorInfo linked list
  ! -----------------------------------

  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List, &
                                  Quiet = SET )
                               
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//&
                          TRIM( SensorInfo_Filename ), &
                          FAILURE )
    STOP
  END IF


  ! ---------------------------
  ! Count the number of sensors
  ! ---------------------------

  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )

  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#           -- DEFINE THE FREQUENCY INTERPOLATION INFORMATION --             #
  !#----------------------------------------------------------------------------#

  dF = FREQUENCY_INTERVAL( INTERPOLATION_INDEX )



  !#----------------------------------------------------------------------------#
  !#                      -- BEGIN LOOP OVER SENSORS --                         #
  !#----------------------------------------------------------------------------#

  Sensor_Loop: DO n = 1, n_Sensors


    ! ---------------------------------------------
    ! Get the current SensorInfo data from the list
    ! ---------------------------------------------

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


    ! ---------------------------
    ! Construct the SRF filenames
    ! ---------------------------

     SRF_Filename = TRIM( SensorInfo%File_Prefix )//'.srf.nc'
    iSRF_Filename = TRIM( SensorInfo%File_Prefix )//'.interpolated_srf.nc'



    !#--------------------------------------------------------------------------#
    !#                   -- OPERATE ONLY ON FILES THAT EXIST --                 #
    !#--------------------------------------------------------------------------#

    Available_Sensors: IF ( File_Exists( TRIM( SRF_Filename ) ) ) THEN


      WRITE( *, '(/5x, "Processing SRF file ", a, "..." )' ) TRIM( SRF_Filename )


      ! ------------------------------------------------
      ! Inquire the input SRF file and check the results
      ! ------------------------------------------------

      Error_Status = Inquire_SRF_netCDF( TRIM( SRF_Filename ),                &
                                         n_Channels       = n_Channels,       &
                                         NCEP_Sensor_ID   = NCEP_Sensor_ID,   &
                                         WMO_Satellite_ID = WMO_Satellite_ID, &
                                         WMO_Sensor_ID    = WMO_Sensor_ID,    &
                                         Title            = Title,            &
                                         History          = History,          &
                                         Sensor_Name      = Sensor_Name,      &
                                         Platform_Name    = Platform_Name,    &
                                         Comment          = Comment           )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring input SRF file '//TRIM( SRF_Filename ), &
                              FAILURE )
        STOP
      END IF

      ! -- Check the number of channels
      IF ( n_Channels /= SensorInfo%n_Channels ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Number of channels in ", a, ", ", i5, &
                          &", is different from SensorInfo entry, ", i5, "." )' ) &
                        TRIM( SRF_Filename ), n_Channels, SensorInfo%n_Channels
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      ! -- Check the sensor IDs
      IF ( NCEP_Sensor_ID   /= SensorInfo%NCEP_Sensor_ID   .OR. &
           WMO_Satellite_ID /= SensorInfo%WMO_Satellite_ID .OR. &
           WMO_Sensor_ID    /= SensorInfo%WMO_Sensor_ID         ) THEN
        Error_Status = FAILURE
        CALL Display_Message( PROGRAM_NAME, &
                              'Sensor Ids in '//TRIM( SRF_Filename )//&
                              ' are different from SensorInfo entry.', &
                              Error_Status )
        STOP
      END IF


      ! --------------------------------------------
      ! Create the output interpolated SRF data file
      ! --------------------------------------------

      ! -- Modify the comment attribute
      IF ( LEN_TRIM( Comment )        == 0 .OR. &
           TRIM( ADJUSTL( Comment ) ) == 'None' ) THEN
        Comment = 'Interpolated SRFs for transmittance production'
      ELSE
        Comment = 'Interpolated SRFs for transmittance production; '//&
                  TRIM( ADJUSTL( Comment ) )
      END IF

      ! -- Create the netCDF file
      Error_Status = Create_SRF_netCDF( TRIM( iSRF_Filename ), &
                                        SensorInfo%Sensor_Channel, &
                                        NCEP_Sensor_ID   = NCEP_Sensor_ID, &
                                        WMO_Satellite_ID = WMO_Satellite_ID, &
                                        WMO_Sensor_ID    = WMO_Sensor_ID, &
                                        Title            = TRIM( Title ), &
                                        History          = PROGRAM_RCS_ID//'; '//&
                                                           TRIM( History ), &
                                        Sensor_Name      = TRIM( Sensor_Name ), &
                                        Platform_Name    = TRIM( Platform_Name ), &
                                        Comment          = TRIM( Comment ) )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error creating '//TRIM( iSRF_Filename ), &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#                     -- LOOP OVER THE CHANNELS --                       #
      !#------------------------------------------------------------------------#

      Channel_Loop: DO l = 1, n_Channels


        WRITE( *, '( 5x, "Interpolating ", a, " channel ", i5, " SRF..." )' ) &
                  TRIM( SRF_Filename ), SensorInfo%Sensor_Channel( l )



        ! ---------------------------------------
        ! Read the current channel input SRF data
        ! ---------------------------------------

        Error_Status = Read_SRF_netCDF( TRIM( SRF_Filename ), &
                                        SensorInfo%Sensor_Channel( l ), &
                                        SRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error reading channel #", i5, " SRF from ", a )' ) &
                          SensorInfo%Sensor_Channel( l ), TRIM( SRF_Filename )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF

        ! -- Assign the channel number and sensor Ids
        ! -- to the interpolation structure
        iSRF%Channel = SRF%Channel
        iSRF%NCEP_Sensor_ID   = SRF%NCEP_Sensor_ID
        iSRF%WMO_Satellite_ID = SRF%WMO_Satellite_ID
        iSRF%WMO_Sensor_ID    = SRF%WMO_Sensor_ID


        ! ----------------------------------------------------
        ! Calculate the begin and end interpolated frequencies
        ! and the number of interpolated points
        ! ----------------------------------------------------

        ! -- The begin point and frequency
        i1 = INT( ONEpointFIVE + ( ( SRF%Begin_Frequency - FREQUENCY_BEGIN ) / &
             !                     -----------------------------------------
                                                     dF                      ) )

        iSRF%Begin_Frequency = FREQUENCY_BEGIN + ( REAL( i1 - 1, fp_kind ) * dF )


        ! -- The end point and frequency
        i2 = INT( ONEpointFIVE + ( ( SRF%End_Frequency - FREQUENCY_BEGIN ) / &
             !                     ---------------------------------------
                                                     dF                    ) )

        iSRF%End_Frequency = FREQUENCY_BEGIN + ( REAL( i2 - 1, fp_kind ) * dF )

        ! -- Total number of points
        n_iPoints = i2 - i1 + 1


        ! ----------------------------------------------
        ! Allocate the interpolated SRF structure arrays
        ! ----------------------------------------------

        Error_Status = Allocate_SRF( n_iPoints, iSRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error allocating iSRF arrays for channel #", i5, "." )' ) &
                          SensorInfo%Sensor_Channel( l )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF


        ! ----------------------------------------
        ! Compute the interpolation frequency grid
        ! ----------------------------------------

        Error_Status = Frequency_SRF( iSRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error computing the interpolation frequency grid ", &
                            &"for channel #", i5 )' ) &
                          SensorInfo%Sensor_Channel( l )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF


        ! -------------------------
        ! Interpolate the input SRF
        ! -------------------------

        Error_Status = Polynomial_Interpolate(  SRF%Frequency, &  ! Input
                                                SRF%Response,  &  ! Input
                                               iSRF%Frequency, &  ! Input
                                               iSRF%Response,  &  ! Output
                                               Order = ORDER   )  ! Optional input

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error interpolating SRF for channel #", i5 )' ) &
                          SensorInfo%Sensor_Channel( l )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF

        ! -- Any -ve bits due to the interpolation are set to zero
        WHERE( iSRF%Response < ZERO ) iSRF%Response = ZERO

        ! -- Normalise it
        iSRF%Response = iSRF%Response / MAXVAL( iSRF%Response )


        ! ------------------------------
        ! Integrate the interpolated SRF
        ! ------------------------------

        Error_Status = Integrate_SRF( iSRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error integrating channel #", i5, " interpolated SRF." )' ) &
                          SensorInfo%Sensor_Channel( l )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF

        ! -- Output the integration comparisons
        WRITE( *, '( 10x, "Integrated SRF: ", es13.6, 5x, &
                         &"Summed SRF: ", es13.6, 5x, &
                         &"% difference: ", es13.6 )' ) &
                  iSRF%Integrated_SRF, &
                  iSRF%Summation_SRF, &
                  100.0_fp_kind * ( iSRF%Integrated_SRF - iSRF%Summation_SRF ) / &
        !                         --------------------------------------------
                                                iSRF%Integrated_SRF


        ! ----------------------------------------
        ! Write the channel SRF to the netCDF file
        ! ----------------------------------------

        Error_Status = Write_SRF_netCDF( TRIM( iSRF_Filename ), &
                                         iSRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error occurred writing channel #", i5, &
                            &" interpolated SRF to ", a, "." )' ) &
                          iSRF%Channel, TRIM( iSRF_Filename )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF


        ! ---------------------------------------
        ! Destroy SRF structures for next channel
        ! ---------------------------------------

        ! -- The interpolated SRF
        Error_Status = Destroy_SRF( iSRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error occurred destroying channel #", i4, &
                            &" iSRF structure." )' ) &
                          SensorInfo%Sensor_Channel(l)
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF

        ! -- The input SRF
        Error_Status = Destroy_SRF( SRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error occurred destroying channel #", i4, &
                            &" SRF structure." )' ) &
                          SensorInfo%Sensor_Channel(l)
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF

      END DO Channel_Loop

    END IF Available_Sensors



    !#--------------------------------------------------------------------------#
    !#              -- DESTROY THE CURRENT SensorInfo STRUCTURE --              #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_SensorInfo( SensorInfo )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error destroying SensorInfo data for sensor # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

  END DO Sensor_Loop



  !#----------------------------------------------------------------------------#
  !#                  -- DESTROY THE SensorInfo LINKED LIST --                  #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo linked list.', &
                          WARNING )
  END IF

END PROGRAM Interpolate_SRFs


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Interpolate_SRFs.f90,v 2.8 2006/07/26 22:51:20 wd20pd Exp $
!
! $Date: 2006/07/26 22:51:20 $
!
! $Revision: 2.8 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Interpolate_SRFs.f90,v $
! Revision 2.8  2006/07/26 22:51:20  wd20pd
! Renamed Inteprolate module to Interpolate_Utility to reflect changes in
! the CRTM Utility modules.
!
! Revision 2.7  2006/06/30 16:47:16  dgroff
! Changed "Error_Handler" references to "Message_Handler"
!
! Revision 2.6  2006/01/26 23:40:30  paulv
! - Added code to set to 0.0 any interpolated SRF points that are < 0.0.
!
! Revision 2.5  2005/08/15 20:59:07  paulv
! - Changed interpolation order from 1 (Linear) to 3 (cubic).
!
! Revision 2.4  2005/05/08 19:20:02  paulv
! - Upgraded to Fortran-95
! - Altered to use new SRF and SensorInfo modules. SRF and SensorInfo structure
!   initialisation subroutine calls removed. SensorInfo linked list initialisation
!   function replaced with New_SensorInfo_List() function.
! - Interpolation frequency index added and fixed to the 0.1cm-1 interval.
!   Since this program is used for the broadband sensors, the lower resolution
!   is sufficient.
!
! Revision 2.3  2003/11/19 19:54:24  paulv
! - Minor changes to output format of global attributes.
!
! Revision 2.2  2003/09/09 16:57:39  paulv
! - Cosmetic changes only.
!
! Revision 2.1  2003/09/03 14:58:02  paulv
! - Corrected bug where the Sensor Ids were not being assigned to the
!   interpolated SRF structure.
!
! Revision 2.0  2003/09/03 14:42:36  paulv
! - New version using new SRF define and I/O modules and using the SensorInfo
!   modules to loop over available sensors.
!
! Revision 1.4  2002/11/22 18:57:14  paulv
! - Using new SRF_netCDF_IO interfaces. Calls to the SRF CREATE() and WRITE()
!   functions no longer require the NC_dataID structure.
!
! Revision 1.3  2002/05/31 22:32:31  paulv
! - Added summation_SRF output.
! - Comment field checked for presence of marker 'None'.
!
! Revision 1.2  2002/05/08 13:24:43  paulv
! - Changed SRF INQUIRE, CREATE, and WRITE function calls to reflect changes made to
!   SRF_netCDF_IO module functions. The function interfaces should be stable now.
!
! Revision 1.1  2002/05/07 18:26:50  paulv
! Initial checkin.
!
!
!
!
