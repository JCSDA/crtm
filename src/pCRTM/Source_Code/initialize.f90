!------------------------------------------------------------------------------
!M+
! NAME:
!       Initialize
!
! PURPOSE:
!       Module for the prototype CRTM (pCRTM) initialization
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Initialize
!
! MODULES:
!       Message_Handler:             Module to define error codes and handle
!                                    error conditions.
!                                    USEs: FILE_UTILITY module
!
!       Spectral_Coefficients:       Module containing the RT model spectral
!                                    coefficients and their load/destruction
!                                    routines.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          PARAMETERS module
!                                          SPCCOEFF_DEFINE module
!                                          SPCCOEFF_BINARY_IO module
!
!       Transmittance_Coefficients:  Module containing the RT model transmittance
!                                    coefficients and their load/destruction
!                                    routines.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          PARAMETERS module
!                                          TAUCOEFF_DEFINE module
!                                          TAUCOEFF_BINARY_IO module
!
!
! CONTAINS:
!       Initialize_RTM:   Function to initialize the pCRTM.
!
!       Destroy_RTM:      Function to destroy the pCRTM model space.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       Shared spectral (SpcCoeff) and gas absorption model (TauCoeff) coefficient
!       structures are filled.
!
! COMMENTS:
!       All of the array documentation lists the dimensions by a single letter.
!       Throughout the RTM code these are:
!         I: Array dimension is of I predictors (Istd and Iint are variants).
!         J: Array dimension is of J absorbing species.
!         K: Array dimension is of K atmospheric layers.
!         L: Array dimension is of L spectral channels.
!         M: Array dimension is of M profiles.
!       Not all of these dimensions will appear in every module.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 31-Jul-2000
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000 Paul van Delst
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

MODULE Initialize


  ! ----------
  ! Module use
  ! ----------

  USE Error_handler
  USE Transmittance_Coefficients
  USE Spectral_Coefficients


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Initialize_RTM
  PUBLIC :: Destroy_RTM


CONTAINS


!------------------------------------------------------------------------------
!S+
! NAME:
!       Initialize_RTM
!
! PURPOSE:
!       Function to initialize the pCRTM by loading all the shared spectral
!       and gas absorption model coefficient data.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Initialize_RTM( Tau_File          = Tau_File,          &  ! Optional input
!                                      Spectral_File     = Spectral_File,     &  ! Optional input
!                                      Path              = Path,              &  ! Optional input
!                                      Quiet             = Quiet,             &  ! Optional input
!                                      Process_ID        = Process_ID,        &  ! Optional input
!                                      Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                      Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Tau_File:           Character string specifying a file name for the
!                           gas absorption model coefficient data file. If not
!                           specified, "transmittance_coefficients" is the
!                           default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Spectral_File:      Character string specifying a file name for the
!                           spectral coefficient data file. If not
!                           specified, "spectral_coefficients" is the
!                           default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Path:               Character string specifying a file Path for the
!                           input coefficient files. If not specified, the
!                           current directory is the default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Quiet:              Set this argument to suppress INFORMATION messages
!                           being printed to standard output (or the message
!                           log file if the Message_Log optional argument is
!                           used.) By default, INFORMATION messages are printed.
!                           If QUIET = 0, INFORMATION messages are OUTPUT.
!                              QUIET = 1, INFORMATION messages are SUPPRESSED.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATIOn message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Output_Process_ID:  Set this argument to the MPI process ID in which
!                           all INFORMATION messages are to be output. If
!                           the passed Process_ID value agrees with this value
!                           the INFORMATION messages are output. 
!                           This argument is ignored if the Quiet argument
!                           is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to the screen.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the
!                           error status. The error codes are defined in
!                           the ERROR_HANDLER module.
!                           If == SUCCESS the pCRTM initialization was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!       Load_Tau_Coefficients:       Function to load the gas absorption model
!                                    coefficient data into a public data
!                                    structure.
!                                    SOURCE: TRANSMITTANCE_COEFFICIENTS module
!
!       Load_Spectral_Coefficients:  Function to load the spectral coefficient
!                                    data into a public data structure.
!                                    SOURCE: SPECTRAL_COEFFICIENTS module
!
!       Display_Message:             Subroutine to output messages
!                                    SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       All public data arrays accessed by this module and its dependencies
!       are overwritten.
!
! RESTRICTIONS:
!       If specified, the length of the combined path and filename strings
!       cannot exceed 512 characters.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Initialize_RTM( Tau_File,          &  ! Optional input
                           Spectral_file,     &  ! Optional input
                           Path,              &  ! Optional input
                           Quiet,             &  ! Optional input
                           Process_ID,        &  ! Optional input
                           Output_Process_ID, &  ! Optional input
                           Message_Log )      &  ! Error messaging
                         RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Tau_File
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Spectral_File
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Path
    INTEGER,        OPTIONAL, INTENT( IN ) :: Quiet
    INTEGER,        OPTIONAL, INTENT( IN ) :: Process_ID
    INTEGER,        OPTIONAL, INTENT( IN ) :: Output_Process_ID

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Initialize_RTM'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 512 ) :: TauCoeff_File
    CHARACTER( 512 ) :: SpcCoeff_File



    !#--------------------------------------------------------------------------#
    !#              -- LOAD THE GAS ABSORPTION MODEL COEFFICIENTS --            #
    !#--------------------------------------------------------------------------#

    ! ------------------
    ! Construct filename
    ! ------------------

    ! -- Default name
    TauCoeff_File = 'transmittance_coefficients'

    ! -- User specified name
    IF ( PRESENT( Tau_File ) ) &
      TauCoeff_File = TRIM( ADJUSTL( Tau_File ) )

    ! -- User specified path
    IF ( PRESENT( Path ) ) &
      TauCoeff_File = TRIM( ADJUSTL( Path ) ) // TRIM( TauCoeff_File )


    ! -------------------------
    ! Load the coefficient data
    ! -------------------------

    Error_Status = Load_Tau_Coefficients( TRIM( TauCoeff_File ), &
                                          Quiet             = Quiet, &
                                          Process_ID        = Process_ID, &
                                          Output_Process_ID = Output_Process_ID, &
                                          Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading gas absorption model coefficients', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- READ THE SPECTRAL COEFFICIENTS --                  #
    !#--------------------------------------------------------------------------#

    ! ------------------
    ! Construct filename
    ! ------------------

    ! -- Default name
    SpcCoeff_File = 'spectral_coefficients'

    ! -- User specified name
    IF ( PRESENT( Spectral_File ) ) &
      SpcCoeff_File = TRIM( ADJUSTL( Spectral_File ) )

    ! -- User specified path
    IF ( PRESENT( Path ) ) &
      SpcCoeff_File = TRIM( ADJUSTL( Path ) ) // TRIM( SpcCoeff_File )


    ! -------------------------
    ! Load the coefficient data
    ! -------------------------

    Error_Status = Load_Spectral_Coefficients( TRIM( SpcCoeff_File ), &
                                               Quiet             = Quiet, &
                                               Process_ID        = Process_ID, &
                                               Output_Process_ID = Output_Process_ID, &
                                               Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading spectral coefficients', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Initialize_RTM





!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_RTM
!
! PURPOSE:
!       Function to destroy all the allocated shared data arrays created
!       during the pCRTM initialization.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_RTM( Process_ID  = Process_ID, &  ! Optional input
!                                   Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Process_ID:   Set this argument to the MPI process ID that this
!                     function call is running under. This value is used
!                     solely for controlling message output. If MPI is not
!                     being used, ignore this argument.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to the screen.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the
!                      error status. The error codes are defined in
!                      the ERROR_HANDLER module.
!                      If == SUCCESS the pCRTM model space destruction
!                                    was successful
!                         == FAILURE an unrecoverable error occurred.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       Destroy_Tau_Coefficients:       Function to deallocate the gas absorption
!                                       model coefficient shared data arrays.
!                                       SOURCE: TRANSMITTANCE_COEFFICIENTS module
!
!       Destroy_Spectral_Coefficients:  Function to deallocate the spectral
!                                       coefficient shared data arrays.
!                                       SOURCE: SPECTRAL_COEFFICIENTS module
!
!       Display_Message:                Subroutine to output messages
!                                       SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       All pCRTM shared data arrays are deallocated.
!
! RESTRICTIONS:
!       None.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_RTM( Process_ID,   &  ! Optional input
                        Message_Log ) &  ! Error messaging
                      RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN ) :: Process_ID

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_RTM'



    !#--------------------------------------------------------------------------#
    !#       -- DEALLOCATE THE GAS ABSORPTION MODEL COEFFICIENT ARRAYS --       #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_Tau_Coefficients( Process_ID  = Process_ID, &
                                             Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating gas absorption model coefficients', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- DEALLOCATE THE SPECTRAL COEFFICIENT ARRAYS --            #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_Spectral_Coefficients( Process_ID  = Process_ID, &
                                                  Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating spectral coefficients', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Destroy_RTM

END MODULE Initialize


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: initialize.f90,v 2.2 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 2.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: initialize.f90,v $
! Revision 2.2  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 2.1  2004/12/22 17:43:33  paulv
! - Updated header documentation.
!
! Revision 2.0  2003/05/16 17:27:53  paulv
! - New initialisation module using the new coefficient I/O routines.
!
! Revision 1.7  2001/10/01 20:28:46  paulv
! - Added "Name" to RCS keyword list.
!
! Revision 1.6  2001/08/31 21:17:25  paulv
! - Add TAU_FILE, SPECTRAL_FILE, and PATH optional arguments to Initialize_RTM
!   to allow users to specify alternate file names and locations.
!
! Revision 1.5  2001/08/16 16:40:56  paulv
! - Updated documentation
!
! Revision 1.4  2001/08/01 16:51:52  paulv
! - Removed USE of module ABSORBER_SPACE to reflect changes in code.
!   The absorber space levels are no longer calculated during model
!   initialisation, but are precalculated and stored in the transmittance
!   coefficient data file.
! - Removed COMPUTE_ABSORBER_SPACE() function call for same reason.
!
! Revision 1.3  2001/05/29 17:40:22  paulv
! - Changed name of initialisation routine from RTM_INITIALIZE to INITIALIZE_RTM.
! - Added DESTROY_RTM function to deallocate memory used in coefficient read.
!
! Revision 1.2  2000/08/31 19:36:32  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 1.1  2000/08/08 16:39:35  paulv
! Initial checkin
!
!
!
