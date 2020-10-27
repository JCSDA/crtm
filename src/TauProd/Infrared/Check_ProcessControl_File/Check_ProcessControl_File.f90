!------------------------------------------------------------------------------
! NAME:
!       Check_ProcessControl_File
!
! PURPOSE:
!       Program to read a ProcessControl file to determine what channels
!       have been processed. A file is created containing a list of the LBL
!       bands that are scheduled for deletion as they are no longer needed.
!
! CATEGORY:
!       Transmittance Production
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                Module containing definitions for kinds
!                                  of variable types.
!
!       File_Utility:              Module containing generic file utility routines
!
!       Message_Handler:             Module to define simple error codes and
!                                  handle error conditions
!                                  USEs: FILE_UTILITY module
!
!       ProcessControl_Define:     Module containing the ProcessControl
!                                  data type definition and routines to
!                                  manipulate the structure.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!
!       ProcessControl_IO:         Module containing routines to read and write
!                                  ProcessControl files.
!                                  USEs: TYPE_KINDS module
!                                        FILE_UTILITY module
!                                        ERROR_HANDLER module
!                                        PROCESSCONTROL_DEFINE module
!
!       Tau_Production_Parameters: Module defining parameters used in the LBL
!                                  transmittance production runs
!                                  USEs: TYPE_KINDS module
!                                        LBLRTM_PARAMETERS module
!
!       Tau_Production_Utility:    Module continaing utility routines for the LBL
!                                  transmittance production runs.
!                                  USEs: TYPE_KINDS module
!                                        FILE_UTILITY module
!                                        ERROR_HANDLER module
!                                        TAU_PRODUCTION_PARAMETERS module
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
!       Input:  User defined ProcessControl file.
!
! SIDE EFFECTS:
!       If any LBL transmittance data files are no longer needed, they are deleted.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 05-Jun-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002 Paul van Delst
!
!
!------------------------------------------------------------------------------

PROGRAM Check_ProcessControl_File


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE ProcessControl_Define
  USE ProcessControl_IO

  USE Tau_Production_Parameters
  USE Tau_Production_Utility


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Check_ProcessControl_File'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: ProcessControl_Filename
  CHARACTER( 256 ) :: DeleteBand_Filename
  INTEGER          :: DeleteBand_FileID
  INTEGER :: IO_Status
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: dF_Index
  INTEGER :: lBand, lCh
  INTEGER :: l1, l2
  LOGICAL, DIMENSION( : ), ALLOCATABLE :: DeleteBand
  TYPE( ProcessControl_type ) :: ProcessControl



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to read a process control file to determine what ")' )
  WRITE( *, '( 5x, " channels have been processed and delete the LBL data     ")' )
  WRITE( *, '( 5x, " files associated with those channels.                    ")' )
  WRITE( *, '(/5x, " $Revision: 1.9 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                      -- READ THE ProcessControl FILE --                    #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter the ProcessControl filename : " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) ProcessControl_Filename
  ProcessControl_Filename = ADJUSTL( ProcessControl_Filename )


  ! --------------------------------
  ! Read the ProcessControl datafile
  ! --------------------------------

  Error_Status = Read_ProcessControl( TRIM( ProcessControl_Filename ), &
                                      ProcessControl )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading ProcessControl file '//&
                          TRIM( ProcessControl_Filename )//'.', &
                          FAILURE )
    STOP
  END IF


  ! ------------------------------------------------------------------------
  ! Check that the frequency interval index is the same for every sensor
  ! in the ProcessControl file. Different frequency indices cannot be
  ! mixed as the LBLband numbering differs for different frequency intervals
  ! ------------------------------------------------------------------------

  dF_Index = ProcessControl%dF_Index(1)

  IF ( ANY( ProcessControl%dF_Index /= dF_Index ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Different dF_Index values in ProcessControl file '//&
                          TRIM( ProcessControl_Filename )//'.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                    -- ALLOCATE THE DeleteBand ARRAY --                     #
  !#----------------------------------------------------------------------------#

  ALLOCATE( DeleteBand( N_LBLBANDS( dF_Index ) ), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating logical DeleteBand array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF


  ! ----------------------------------------
  ! Set all DeleteBand entries to .TRUE.,
  ! i.e. delete ALL LBL band data by default
  ! ----------------------------------------

  DeleteBand = .TRUE.



  !#----------------------------------------------------------------------------#
  !#                      -- LOOP OVER ALL THE CHANNELS --                      #
  !#----------------------------------------------------------------------------#

  Channel_Check_Loop: DO lCh = 1, ProcessControl%n_Channels

    IF ( ProcessControl%List( lCh )%Processed == 0 ) THEN

      l1 = ProcessControl%List( lCh )%Begin_LBLband
      l2 = ProcessControl%List( lCh )%End_LBLband

      DeleteBand( l1:l2 ) = .FALSE.

    END IF

  END DO Channel_Check_Loop



  !#----------------------------------------------------------------------------#
  !#                      -- CREATE BAND DELETION FILE --                       #
  !#----------------------------------------------------------------------------#

  DeleteBand_Filename = TRIM( ProcessControl_Filename )//'.deleteband'

  ! -- Get a file unit number
  DeleteBand_FileID = Get_Lun()

  IF ( DeleteBand_FileID < 0 ) THEN
   CALL Display_Message( PROGRAM_NAME, &
                         'Error obtaining a file unit number for output.', &
                         FAILURE )
   STOP
  END IF

  ! -- Create the file
  OPEN( DeleteBand_FileID, FILE   = TRIM( DeleteBand_Filename ), &
                           ACCESS = 'SEQUENTIAL', &
                           FORM   = 'FORMATTED',  &
                           STATUS = 'REPLACE',    &
                           ACTION = 'WRITE',      &
                           IOSTAT = IO_Status     )

  IF ( IO_Status /= 0 ) THEN
    WRITE( Message, '( "Error opening output Delete Band file. IOSTAT = ", i5 )' ) &
                    IO_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                   -- LOOP OVER THE BANDS FOR DELETION --                   #
  !#----------------------------------------------------------------------------#

  WRITE( *, * )

  Band_Deletion_Loop: DO lBand = 1, N_LBLBANDS( dF_Index )

    ! -- If band is not to be deleted, skip it
    IF ( .NOT. DeleteBand( lBand ) ) CYCLE Band_Deletion_Loop

    ! -- Output for log file
    WRITE( *, '( 5x, "Band scheduled for deletion: ", i3 )' ) lBand

    ! -- Write band number to be deleted to band deletion file
    WRITE( DeleteBand_FileID, FMT = '( i3.3 )', &
                              IOSTAT = IO_Status ) lBand

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing BAND to be deleted. IOSTAT = ", i5 )' ) &
                      IO_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

  END DO Band_Deletion_Loop

  CLOSE( DeleteBand_FileID )



  !#----------------------------------------------------------------------------#
  !#      -- CREATE A SIGNAL FILE INDICATING SUCCESSFUL DELETEBAND WRITE --     #
  !#----------------------------------------------------------------------------#

  Error_Status = Create_Signal_File( TRIM( DeleteBand_Filename ) )



  !#----------------------------------------------------------------------------#
  !#        -- CREATE A SIGNAL FILE INDICATING ALL CHANNELS PROCESSED --        #
  !#----------------------------------------------------------------------------#

  IF ( ALL( ProcessControl%List%Processed == 1 ) ) THEN

    Error_Status = Create_Signal_File( TRIM( ProcessControl_Filename )//'.completed' )

  END IF



  !#----------------------------------------------------------------------------#
  !#                   -- DEALLOCATE THE DeleteBand ARRAY --                    #
  !#----------------------------------------------------------------------------#

  DEALLOCATE( DeleteBand, STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error deallocating logical DeleteBand array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          WARNING )
  END IF



  !#----------------------------------------------------------------------------#
  !#               -- DESTROY THE ProcessControl DATA STRUCTURE --              #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_ProcessControl( ProcessControl )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying the Process Control structure.', &
                          WARNING )
  END IF

END PROGRAM Check_ProcessControl_File


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/06/30 16:47:16 $
!
! $Revision: 1.9 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Check_ProcessControl_File.f90,v $
! Revision 1.9  2006/06/30 16:47:16  dgroff
! Changed "Error_Handler" references to "Message_Handler"
!
! Revision 1.8  2005/09/16 20:27:06  paulv
! - Updated to reflect changes in the ProcessControl structure name and
!   components.
!
! Revision 1.7  2005/05/15 23:34:45  paulv
! - Upgraded to Fortran-95
! - Modified to use new ProcessControl modules.
! - Removed all structure initialisation subroutine calls.
!
! Revision 1.6  2004/04/08 17:42:49  paulv
! - Changes made for calculations by angle.
!
! Revision 1.5  2003/09/05 17:09:21  paulv
! - Updated Process Control read call to reflect changes made in the Process
!   Control I/O functions. The Process Control file no longer has to be opened
!   before a Read()/Write() - this is now handled within the functions.
!
! Revision 1.4  2003/07/16 18:49:32  paulv
! - ensured all character arguments passed were TRIMmed.
!
! Revision 1.3  2002/10/08 14:14:24  paulv
! - Added user input for the DEPENDENT PROFILE SET, via profile_set, to bring
!   code in line with changes made to the Tau_Production_Parameters module.
!   When the profile number is input it is now checked against
!     N_PROFILES( profile_set )
!   rather than simply
!     N_PROFILES
!   when only one profile set was allowed for.
! - A search is now made for the allowed molecule_set_number to take into
!   account non-contiguous definitions.
!
! Revision 1.2  2002/06/11 18:00:36  paulv
! - New version that actually works.
! - Signal file output.
!
! Revision 1.1  2002/06/07 17:04:39  paulv
! Initial checkin.
!
!
!
!
