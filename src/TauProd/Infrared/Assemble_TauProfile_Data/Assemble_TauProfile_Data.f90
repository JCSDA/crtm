!------------------------------------------------------------------------------
!M+
! NAME:
!       Assemble_TauProfile_Data
!
! PURPOSE:
!       Program to assemble the individual TauProfile datafiles into a single
!       datafile for each sensor.
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
!       Message_Handler:               Module to define simple error codes and
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
!       Input:  - Process Control file
!               - Sensor TauProfile netCDF data files for each profile and
!                 each molecule set.
!
!       Output: - TauProfile netCDF data file combining all the profile
!                 and molecule set data for a single sensor.
!
! SIDE EFFECTS:
!       Any output files that exist are overwritten.
!
! RESTRICTIONS:
!       *ALL* of the required data must be present for the output files to
!       be successfully written.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 17-June-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002 Paul van Delst
!
!M-
!------------------------------------------------------------------------------

PROGRAM Assemble_TauProfile_Data


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

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

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Assemble_TauProfile_Data'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ONE  = 1.0_fp_kind

  REAL( fp_kind ), PARAMETER :: TOLERANCE     = EPSILON( ONE )
  REAL( fp_kind ), PARAMETER :: TAU_TOLERANCE = 100.0_fp_kind * TOLERANCE

  CHARACTER( * ),  PARAMETER, DIMENSION( 2 ) :: DIRECTION_NAME = (/ 'upwelling  ', &
                                                                    'downwelling' /)


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER(20)     :: Sensor_ID
  INTEGER           :: WMO_Satellite_ID
  INTEGER           :: WMO_Sensor_ID   
  CHARACTER( 256 )  :: ID_Tag
  CHARACTER( 5000 ) :: History
  CHARACTER( 5000 ) :: Comment
  CHARACTER( 256 ) :: Control_Filename
  CHARACTER( 256 ) :: TauProfile_Filename
  CHARACTER( 256 ) :: Signal_Filename
  INTEGER :: IO_Status
  INTEGER :: Error_Status
  INTEGER :: i, j, k, l, m, n
  INTEGER :: im, m1, m2
  INTEGER :: l1, l2
  INTEGER :: n_m, Profile_Set
  INTEGER :: n_j, jIdx
  INTEGER, DIMENSION( N_MOLECULE_SETS ) :: Molecule_Set_Numbers
  INTEGER, DIMENSION( 1 ) :: Idx
  CHARACTER( 40 ) :: jTag
  CHARACTER(  9 ) :: mTag
  CHARACTER(  6 ) :: iTag
  INTEGER :: Direction
  TYPE( ProcessControl_type ) :: ProcessControl
  TYPE( TauProfile_type )     :: TauProfile



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to assemble the individual TauProfile datafiles  ")' )
  WRITE( *, '( 5x, "   into a single datafile.                                ")' )
  WRITE( *, '(/5x, " $Revision: 2.2 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#         -- ENTER DATA FOR TRANSMITTANCE DATA FILE IDENTIFICATION --        #
  !#----------------------------------------------------------------------------#

  ! -------------------------------
  ! The profile set being processed
  ! -------------------------------

  WRITE( *, FMT = '( /5x, "Select the DEPENDENT PROFILE SET" )' )
  DO n_m = 1, N_PROFILE_SETS
    WRITE( *, FMT = '( 10x, i2, ") ", a, " profile set" )' ) &
              n_m, TRIM( PROFILE_SET_ID_TAG( n_m ) )
  END DO
  WRITE( *, FMT = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i1 )', &
           IOSTAT = IO_Status ) Profile_Set

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid DEPENDENT PROFILE SET identifier input.', &
                          FAILURE )
    STOP
  END IF

  IF ( Profile_Set < 1 .OR. Profile_Set > N_PROFILE_SETS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid DEPENDENT PROFILE SET identifier value.', &
                          FAILURE )
    STOP
  ENDIF


  ! --------------------------------
  ! The molecule set index number(s)
  ! --------------------------------

  ! -- How many molecule sets?
  WRITE( *, FMT     = '( /5x, "Enter the number of MOLECULE SETS to assemble: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i2 )', &
           IOSTAT = IO_Status ) n_j

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid input.', &
                          FAILURE )
    STOP
  END IF

  IF ( n_j < 1 .OR. n_j > N_MOLECULE_SETS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Value is outside valid range.', &
                          FAILURE )
    STOP
  ENDIF


  ! -- Now enter the actual molecule set numbers
  Molecule_Set_Numbers = -1
  WRITE( *, FMT     = '( 5x, "Enter the MOLECULE SET numbers to process: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = *, &
           IOSTAT = IO_Status ) Molecule_Set_Numbers( 1:n_j )

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid MOLECULE SET input.', &
                          FAILURE )
    STOP
  END IF

  DO j = 1, n_j
    IF ( .NOT. ANY( MOLECULE_SET_TAG_ID == Molecule_Set_Numbers( j ) ) ) THEN
      WRITE( Message, '( "Input MOLECULE SET value ", i3, " is invalid." )' ) &
                      Molecule_Set_Numbers( j )
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
  END DO


  ! ---------------
  ! The "Direction"
  ! ---------------

  WRITE( *, FMT     = '( /5x, "Select atmospheric path", &
                       &/10x, "1) ", a, &
                       &/10x, "2) ", a, &
                       &/5x, "Enter choice: " )', &
            ADVANCE = 'NO' ) DIRECTION_NAME( 1 ), DIRECTION_NAME( 2 )
  READ( *, FMT    = '( i5 )', &
           IOSTAT = IO_Status ) Direction

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPHERIC PATH identifier input.', &
                          FAILURE )
    STOP
  END IF

  IF ( Direction /= 1 .AND. Direction /= 2 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPERIC PATH identifier value.', &
                          FAILURE )
    STOP
  ENDIF



  !#----------------------------------------------------------------------------#
  !#                      -- READ A PROCESS CONTROL FILE --                     #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter the Process Control filename : " )', &
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
  !#                    -- DEFINE THE PROFILE LIMITS --                         #
  !#                                                                            #
  !#      These limits can be modified, and the code recompiled, to handle      #
  !#      less than the all the profiles in any particular profile set.         #
  !#----------------------------------------------------------------------------#

  m1 = 1
  m2 = N_PROFILES( Profile_Set )



  !#----------------------------------------------------------------------------#
  !#            -- BEGIN MAIN LOOP OVER SENSOR/PLATFORM FILES --                #
  !#----------------------------------------------------------------------------#

  Sensor_Loop: DO n = 1, ProcessControl%n_files



    !#--------------------------------------------------------------------------#
    !#                     -- LOOP OVER MOLECULE SETS --                        #
    !#--------------------------------------------------------------------------#

    Molecule_Set_loop: DO jIdx = 1, n_j


      ! ------------------------------
      ! Define the molecule set number
      ! ------------------------------

      Idx = PACK( (/ ( j, j = 1, N_MOLECULE_SETS ) /), &
                     Molecule_Set_Numbers( jIdx ) == MOLECULE_SET_TAG_ID )
      j = Idx(1)


      ! -- Define a string of the molecule set
      jTag = MOLECULE_SET_TAG( j )



      !#------------------------------------------------------------------------#
      !#                       -- LOOP OVER PROFILES --                         #
      !#------------------------------------------------------------------------#

      Profile_Loop: DO m = m1, m2


        ! -- Create a string of the profile number
        WRITE( mTag, '( "profile", i2.2 )' ) m



        !#----------------------------------------------------------------------#
        !#                       -- LOOP OVER ANGLES --                         #
        !#----------------------------------------------------------------------#

        Angle_Loop: DO i = ZENITH_ANGLE_BEGIN, ZENITH_ANGLE_END


          ! -- Create a string of the angle number
          WRITE( iTag, '( "angle", i1 )' ) i


          ! ------------------------------
          ! Construct the profile filename
          ! ------------------------------

          TauProfile_Filename = './'//mTag//'/'//iTag//'/'//TRIM( jTag )//'/'//&
                                TRIM( DIRECTION_NAME( Direction ) )//&
                                '.'//mTag//'_'//iTag//'_'//TRIM( jTag )//'.'//&
                                TRIM( ProcessControl%File_Prefix( n ) )//&
                                '.TauProfile.nc'


          ! --------------------------------------------------------
          ! Create the output TauProfile file. I do this here rather
          ! than in the outermost loop so I have access to the input
          ! file attributes.
          ! --------------------------------------------------------

          Output_TauProfile_Creation: IF ( i == 1 .AND. m == m1 .AND. jIdx == 1 ) THEN


            ! -------------------------------------------------
            ! Inquire the current input file for its attributes
            ! -------------------------------------------------

            Error_Status = Inquire_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                                      Sensor_ID        = Sensor_ID, &
                                                      WMO_Satellite_ID = WMO_Satellite_ID, &
                                                      WMO_Sensor_ID    = WMO_Sensor_ID, &
                                                      ID_Tag        = ID_Tag, &
                                                      History       = History, &
                                                      Comment       = Comment )
            IF ( Error_Status /= SUCCESS ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error inquiring netCDF TauProfile file '//&
                                    TRIM( TauProfile_Filename ), &
                                    Error_Status )
              STOP
            END IF


            ! ------------------------------------------
            ! Create the output assembly TauProfile file
            ! ------------------------------------------

            WRITE( *, '( 5x, "Creating the output TauProfile data file for the ", a, " sensor..." )' ) &
                      TRIM( ProcessControl%File_Prefix( n ) )

            ! -- Construct the filename
            ProcessControl%TauProfile_Filename( n ) = './TauProfile_data/'//&
                                                      TRIM( DIRECTION_NAME( Direction ) )//'.'//&
                                                      TRIM( ProcessControl%File_Prefix( n ) )//&
                                                      '.TauProfile.nc'

            ! -- Determine the channel index list limits
            l1 = ProcessControl%Channel_Index( 1, n )
            l2 = ProcessControl%Channel_Index( 2, n )

            ! -- Create the file (CLOBBER mode)
            Error_Status = Create_TauProfile_netCDF( TRIM( ProcessControl%TauProfile_Filename( n ) ), &
                                                     LEVEL_PRESSURE, &
                                                     ProcessControl%List( l1:l2 )%Channel, &
                                                     ZENITH_ANGLE_SECANT, &
                                                     (/ ( im, im = m1, m2 ) /), &
                                                     Molecule_Set_Numbers( 1:n_j ), &
                                                     Release = TauProfile%Release, &
                                                     Version = TauProfile%Version, &
                                                     Sensor_ID        = TRIM(Sensor_ID), &
                                                     WMO_Satellite_ID = WMO_Satellite_ID, &
                                                     WMO_Sensor_ID    = WMO_Sensor_ID, &
                                                     ID_Tag  = TRIM(ID_Tag), &
                                                     Title   = TRIM(DIRECTION_NAME( Direction ))//&
                                                               ' transmittances for '//&
                                                               TRIM(Sensor_Id), &
                                                     History = PROGRAM_RCS_ID//'; '//TRIM(History), &
                                                     Comment = TRIM(Comment) )
            IF ( Error_Status /= SUCCESS ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error creating netCDF TauProfile file '//&
                                    TRIM( ProcessControl%TauProfile_Filename( n ) ), &
                                    Error_Status )
              STOP
            END IF

            WRITE( *, '( 10x, a, " created." )' ) TRIM( ProcessControl%TauProfile_Filename( n ) )


          END IF Output_TauProfile_Creation



          ! --------------------------------
          ! Read the current input file data
          ! --------------------------------

          Error_Status = Read_TauProfile_netCDF( TauProfile_Filename, &
                                                 TauProfile )

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading netCDF TauProfile file '//&
                                  TRIM( TauProfile_Filename ), &
                                  Error_Status )
            STOP
          END IF

          ! -- Check that it's the correct molecule set, profile and angle
          IF ( TauProfile%Molecule_Set( 1 ) /= j .OR. &
               TauProfile%Profile( 1 )      /= m .OR. &
               TauProfile%Angle( 1 )        /= ZENITH_ANGLE_SECANT( i ) ) THEN
            Error_Status = FAILURE
            WRITE( Message, '( "Molecule_Set, Profile, or Angle value for ", a, &
                              &" (",i2,",",i3,",",f4.2,") are different than expected", &
                              &" (",i2,",",i3,",",f4.2,")." )' ) &
                            TRIM( TauProfile_Filename ), &
                            TauProfile%Molecule_Set( 1 ), &
                            TauProfile%Profile( 1 ), &
                            TauProfile%Angle( 1 ), &
                            j, m, ZENITH_ANGLE_SECANT( i )
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM( Message ), &
                                  Error_Status )
            STOP
          END IF


          ! -----------------------------------------------
          ! Perform a gross check of transmittance validity
          ! -----------------------------------------------

          ! -- Check for 1 < tau 1+e
          WHERE( TauProfile%Tau > ONE .AND. TauProfile%Tau < (ONE+TAU_TOLERANCE) )
            TauProfile%Tau = ONE
          END WHERE

          ! -- Check that there still aren't transmittances > 1
          IF ( ANY( TauProfile%Tau > ONE ) ) THEN
            WRITE( Message, '( "Number of transmittances > 1.0 found in ", a, &
                              &": ", i10, ". NOT correcting. Check result." )' ) &
                            TRIM( TauProfile_Filename ), COUNT( TauProfile%Tau > ONE )
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM( Message ), &
                                  WARNING )
          END IF


          ! -- Check for 0-e < tau < 0
          WHERE( TauProfile%Tau > (ZERO-TAU_TOLERANCE) .AND. TauProfile%Tau < ZERO )
            TauProfile%Tau = ZERO
          END WHERE

          ! -- Check that there still aren't transmittances < 0
          IF ( ANY( TauProfile%Tau < ZERO ) ) THEN
            WRITE( Message, '( "Number of transmittances < 0.0 found in ", a, &
                              &": ", i10, ". NOT correcting. Check result." )' ) &
                            TRIM( TauProfile_Filename ), COUNT( TauProfile%Tau < ZERO )
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM( Message ), &
                                  WARNING )
          END IF


          ! ------------------------------------------
          ! Write the data into the main assembly file
          ! ------------------------------------------

          Error_Status = Write_TauProfile_netCDF( TRIM( ProcessControl%TauProfile_Filename( n ) ), &
                                                  TauProfile%Tau( :, :, 1, 1, 1 ), &
                                                  Angle = ZENITH_ANGLE_SECANT( i ), &
                                                  Profile = m, &
                                                  Molecule_Set = j )

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error writing data from '//&
                                  TRIM( TauProfile_Filename )//&
                                  ' to '//TRIM( ProcessControl%TauProfile_Filename( n ) ), &
                                  Error_Status )
            STOP
          END IF


          ! --------------------------------
          ! Destroy the TauProfile structure
          ! --------------------------------

          Error_Status = Destroy_TauProfile( TauProfile )

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error destroying TauProfile structure for molecule set ", a, &
                              &", profile #", i2, ", and secant angle ", f4.2, &
                              &", reading from ", a, "." )' ) &
                            TRIM( jTag ), m, ZENITH_ANGLE_SECANT( i ), &
                            TRIM( TauProfile_Filename )
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM( Message ), &
                                  Error_Status )
            STOP
          END IF
 
        END DO Angle_Loop

      END DO Profile_Loop

      WRITE( *, '( 15x, "Molecule Set ", a, " written..." )' ) TRIM( MOLECULE_SET_TAG( j ) )

    END DO Molecule_Set_loop

  END DO Sensor_Loop



  !#----------------------------------------------------------------------------#
  !#                -- CREATE A DIRECTION DEPENDENT SIGNAL FILE --              #
  !#----------------------------------------------------------------------------#

  WRITE( Signal_Filename, '( a, ".",  a )' ) &
                          PROGRAM_NAME, &
                         TRIM( DIRECTION_NAME( Direction ) )

  Error_Status = Create_Signal_File( TRIM( Signal_Filename ) )


END PROGRAM Assemble_TauProfile_Data


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/06/30 16:47:16 $
!
! $Revision: 2.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Assemble_TauProfile_Data.f90,v $
! Revision 2.2  2006/06/30 16:47:16  dgroff
! Changed "Error_Handler" references to "Message_Handler"
!
! Revision 2.1  2006/01/26 18:37:40  paulv
! - Added simple hooks to allow code to be easily modified to process only
!   a subset of profiles rather than all for a particular set.
!
! Revision 2.0  2005/09/23 22:34:25  paulv
! - Modified for multiple angle input TauProfile file.
!
! Revision 1.20  2005/09/16 20:27:06  paulv
! - Updated to reflect changes in the ProcessControl structure name and
!   components.
!
! Revision 1.19  2004/04/08 17:40:43  paulv
! - Changes made to zenith angle multiplier name to reflect changes to the
!   Tau_PRoduction_Parameters module.
!
! Revision 1.18  2004/02/13 22:20:29  paulv
! - Removed correction of transmittances <0 and >1 after precision correction.
!
! Revision 1.17  2004/01/22 19:16:41  paulv
! - Added sensor/satellite Id arguments to the TauProfile Inquire() and
!   Create() functions.
!
! Revision 1.16  2003/12/01 18:11:01  paulv
! - Cosmetic change made to the netCDF History string output to make it easier
!   to read in the netCDF file dump.
!
! Revision 1.15  2003/09/09 20:12:30  paulv
! - Updated code to use new Process Control modules.
!
! Revision 1.14  2003/07/16 20:34:28  paulv
! - Changes made to use new TauProfile and LBLRTM I/O modules.
!
! Revision 1.13  2002/10/29 22:12:52  paulv
! - No longer opening the output file in the "Input" directory of "TauProfile_data"
!
! Revision 1.12  2002/09/16 17:08:16  paulv
! - Added the transmittance checks for _just_ greater than 1.0 and _just_
!   less than 0.0 using a tolerance value of 100xTOLERANCE. If any values
!   are encountered they are quietly set to 1.0 or 0.0. If, after this check,
!   there are still transmittances > 1.0 or < 0.0, they are then beyond the
!   bounds of what could be considered a numerical precision error and
!   a message is output. The values are still set to either 1.0 or 0.0.
!
! Revision 1.11  2002/09/09 20:00:50  paulv
! - Altered the way in which molecule set ID numbers were handled to allow
!   for non-contiguous values.
!
! Revision 1.10  2002/08/02 19:11:20  paulv
! - Changed the method of the gross transmittance value check. Now, only
!   values > 1.0 and < 0.0 are checked for. If any are found, they are
!   set to 1.0 and 0.0 respectively.
!
! Revision 1.9  2002/07/25 21:12:48  paulv
! - Altered output name of singla file.
!
! Revision 1.8  2002/07/25 20:28:42  paulv
! - Moved the output TauProfile creation call into the innermost loop. This
!   allows for inquiring of the input file to retrieve its attributes for use
!   in creating the output file.
! - Removed all SRF definitions and calls - not needed now that the output file
!   creation has access to the input files.
!
! Revision 1.7  2002/07/25 17:14:00  paulv
! - Added signal file creation call at end of processing.
!
! Revision 1.6  2002/07/25 15:03:12  paulv
! - Updated header documentation.
! - Added user input for the profile set to process.
! - Added ID_Tag optional argument to the output TauProfile file creation
!   function call.
! - Changed all references
!     PROFILE_BEGIN, PROFILE_END
!   to
!     1, N_PROFILES( profile_set )
! - Moved the TauProfile structure destruction call to the end of the molecule
!   set loop from just before the read. The initialization call will set up
!   the structure for the first read and moving the destruction call to the end
!   of the loop means when everything is done, the structure will be destroyed.
!
! Revision 1.5  2002/06/26 17:44:49  paulv
! - Added more informational output to error message when profile and/or molecule
!   set numbers don't match.
!
! Revision 1.4  2002/06/26 17:23:52  paulv
! - The output files are created (overwriting previous output files) every
!   invocation. Previously they were created if they didn't exist, closed, and
!   then reopened.
!
! Revision 1.2  2002/06/25 21:36:18  paulv
! - This version actually does something besides print the filename to read.
!
! Revision 1.1  2002/06/20 18:27:58  paulv
! Initial checkin.
!
!
!
!
