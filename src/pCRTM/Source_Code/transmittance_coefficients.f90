!------------------------------------------------------------------------------
!M+
! NAME:
!       Transmittance_Coefficients
!
! PURPOSE:
!       Module containing the prototype CRTM (pCRTM) gas absorption model
!       coefficients and their load/destruction routines. 
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Transmittance_Coefficients
!
! PUBLIC DATA:
!       TC:   Data structure containing the transmittance coefficient data
!             UNITS:      N/A
!             TYPE:       TauCoeff_type
!             DIMENSION:  Scalar
!             ATTRIBUTES: PUBLIC, SAVE
!
! MODULES:
!       Type_Kinds:           Module containing data type kind definitions.
!
!       Message_Handler:      Module to define error codes and handle error
!                             conditions
!                             USEs: FILE_UTILITY module
!
!       Parameters:           Module containing parameter definitions for the
!                             RT model.
!                             USEs: TYPE_KINDS module
!
!       TauCoeff_Define:      Module defining the TauCoeff data structure and
!                             containing routines to manipulate it.
!                             USEs: TYPE_KINDS module
!                                   ERROR_HANDLER module
!                                   COMPUTE_FLOAT_NUMBERS module
!
!       TauCoeff_Binary_IO:   Module containing routines to read and write
!                             binary format TauCoeff files.
!                             USEs: TYPE_KINDS module
!                                   FILE_UTILITY module
!                                   ERROR_HANDLER module
!                                   TAUCOEFF_DEFINE module
!                                   COEFFICIENT_UTILITY module
!
! CONTAINS:
!       Load_Tau_Coefficients:     Function to load the transmittance coefficient
!                                  data into the module public data structure TC.
!
!       Destroy_Tau_Coefficients:  Function to destroy the module public data
!                                  structure TC.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       The shared data structure, TC, is modified.
!
! COMMENTS:
!       All of the array documentation lists the dimensions by a single letter.
!       Throughout the RTM code these are:
!         I: Array dimension is of I predictors (Istd and Iint are variants).
!         J: Array dimension is of J absorbing species.
!         K: Array dimension is of K atmospheric layers.
!         L: Array dimension is of L spectral channels.
!         M: Array dimension is of M profiles.
!         N: Array dimension is of N sensors
!       Not all of these dimensions will appear in every module.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Jun-2000
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000, 2003 Paul van Delst
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

MODULE Transmittance_Coefficients


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds
  USE Message_Handler

  USE Parameters

  USE TauCoeff_Define
  USE TauCoeff_Binary_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Load_Tau_Coefficients
  PUBLIC :: Destroy_Tau_Coefficients


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- RCS Id for the module
  CHARACTER( * ),  PARAMETER, PRIVATE :: MODULE_RCS_ID = &
  '$Id: transmittance_coefficients.f90,v 3.4 2006/05/02 14:58:35 dgroff Exp $'


  ! ---------------------------------------------------
  ! The shared transmittance coefficient data structure
  !
  ! Note that the SAVE attribute is specified to ensure
  ! that the data is retained even when this module is
  ! not being directly accessed.
  ! ---------------------------------------------------

  TYPE( TauCoeff_type ), SAVE, PUBLIC :: TC



CONTAINS


!------------------------------------------------------------------------------
!S+
! NAME:
!       Load_Tau_Coefficients
!
! PURPOSE:
!       Function to load the gas absorption model coefficient data into
!       the public data structure TC.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Load_Tau_Coefficients( Coefficient_File,                      &  ! Input
!                                             Quiet             = Quiet,             &  ! Optional input
!                                             Process_ID        = Process_ID,        &  ! Optional input
!                                             Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                             Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Coefficient_File:   Name of the pCRTM binary format TauCoeff file
!                           containing the gas absorption model coefficient
!                           data.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
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
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the
!                           error status. The error codes are defined in
!                           the ERROR_HANDLER module.
!                           If == SUCCESS the gas absorption model coefficient
!                                         read was successful
!                              == FAILURE an unrecoverable error occurred.
!                              == WARNING the number of channels read in is different
!                                         from that set in the Parameters module.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:         Subroutine to output messages
!                               SOURCE: ERROR_HANDLER module
!
!      Read_TauCoeff_Binary:    Function to read the binary format TauCoeff
!                               data file.
!                               SOURCE: TAUCOEFF_BINARY_IO module
!
!      Get_Max_n_Channels:      Routine to retrieve the value of the
!                               MAX_N_CHANNELS "pseudo-parameter".
!                               SOURCE: PARAMETERS module
!
!      Set_Max_n_Channels:      Routine to set the value of the
!                               MAX_N_CHANNELS "pseudo-parameter".
!                               SOURCE: PARAMETERS module
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure TC.
!
! RESTRICTIONS:
!       None.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Load_Tau_Coefficients( Coefficient_file,  &  ! Input
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

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Coefficient_File

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Quiet
    INTEGER,        OPTIONAL, INTENT( IN )  :: Process_ID
    INTEGER,        OPTIONAL, INTENT( IN )  :: Output_Process_ID

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Load_Tau_Coefficients'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    CHARACTER( 128 ) :: Process_ID_Tag

    ! -- Maximum channels pseudo parameter
    INTEGER :: MAX_N_CHANNELS
    LOGICAL :: is_set


    ! ----------
    ! Intrinsics
    ! ----------

    INTRINSIC TRIM                          




    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    ! -----------------------------------

    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '( ";  MPI Process ID: ", i5 )' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- READ THE TAUCOEFF DATA FILE --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = Read_TauCoeff_Binary( Coefficient_File, &
                                         TC, &
                                         Quiet             = Quiet, &
                                         Process_ID        = Process_ID, &
                                         Output_Process_ID = Output_Process_ID, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading gas absorption model coefficients from '//&
                            TRIM( Coefficient_File )//&
                            TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- SET THE GLOBAL DEFINITION FOR MAX_N_CHANNELS --             #
    !#--------------------------------------------------------------------------#

    CALL Get_Max_n_Channels( Max_n_Channels, Is_Set )

    IF ( Is_Set  ) THEN
      IF ( Max_n_Channels /= TC%n_Channels ) THEN
        Error_Status = WARNING
        WRITE( Message, '( "MAX_N_CHANNELS set to different value, ", i4, ", ", &
                          &"than defined in gas absorption model coefficient file, ", i4, &
                          &". Overwriting" )' ) &
                        Max_n_Channels, TC%n_Channels
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message )//TRIM( Process_ID_Tag ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CALL Set_Max_n_Channels( TC%n_Channels )
      END IF
    ELSE
      CALL Set_Max_n_Channels( TC%n_Channels )
    END IF

  END FUNCTION Load_Tau_Coefficients 



!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_Tau_Coefficients
!
! PURPOSE:
!       Function to deallocate the gas absorption model coefficient shared
!       data arrays.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_Tau_Coefficients( Process_ID  = Process_ID, &  ! Optional input
!                                                Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Process_ID:       Set this argument to the MPI process ID that this
!                         function call is running under. This value is used
!                         solely for controlling message output. If MPI is not
!                         being used, ignore this argument.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to the screen.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the
!                         error status. The error codes are defined in
!                         the ERROR_HANDLER module.
!                         If == SUCCESS the structure reinitialisation was
!                                       successful
!                            == FAILURE an unrecoverable error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:         Subroutine to output messages
!                               SOURCE: ERROR_HANDLER module
!
!      Destroy_TauCoeff:        Function to destroy the public data
!                               structure used to share the transmittance
!                               coefficient data
!                               SOURCE: TAUCOEFF_DEFINE module
!
!      Reset_Max_n_Channels:    Routine to reset the value of the
!                               MAX_N_CHANNELS "pseudo-parameter".
!                               SOURCE: PARAMETERS module
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure TC.
!
! RESTRICTIONS:
!       None.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_Tau_Coefficients( Process_ID,   &  ! Optional input
                                     Message_Log ) &  ! Error messaging
                                   RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Process_ID

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_Tau_Coefficients'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 128 ) :: Process_ID_Tag



    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    ! -----------------------------------

    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '( ";  MPI Process ID: ", i5 )' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- DESTROY THE STRUCTURE --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_TauCoeff( TC, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred destroying gas absorption model '//&
                            'coefficient data arrays'//&
                            TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- RESET THE MAX_N_CHANNELS "PSEUDO-PARAMETER" --              #
    !#--------------------------------------------------------------------------#

    CALL Reset_Max_n_Channels()

  END FUNCTION Destroy_Tau_Coefficients 

END MODULE Transmittance_Coefficients


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: transmittance_coefficients.f90,v 3.4 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 3.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: transmittance_coefficients.f90,v $
! Revision 3.4  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 3.3  2004/12/22 17:43:33  paulv
! - Updated header documentation.
!
! Revision 3.2  2004/09/10 21:46:19  paulv
! - Updated to use new definition module. No structure initialisation.
!
! Revision 3.1  2003/07/10 16:31:48  paulv
! - Removed DEFAULT_RELEASE parameter and the optional VALID_RELEASE argument
!   to the Load() routine. The TauCoeff definition module contains the valid
!   release value now. Any difference between that value and what resides in
!   the file generates an error since a change in the release value indicates
!   a change in the file format.
!
! Revision 3.0  2003/05/16 17:34:17  paulv
! - New version using the new transmittance coefficient structure and I/O routines.
!
! Revision 2.6  2002/11/26 18:03:16  paulv
! - Corrected bug in WRITE() function. Alpha_cN values were not being written.
!
! Revision 2.5  2002/10/29 16:20:37  paulv
! - Changed the absorber number check in the Read() function from
!     IF ( n_Absorbers /= MAX_N_ABSORBERS ) THEN
!   to
!     IF ( n_Absorbers > MAX_N_ABSORBERS ) THEN
!   so that any number of absorbers less than or equal to the maximum can
!   be read in.
! - Added the ALPHA_C1 and ALPHA_C2 data items to the shared data list,
!   Read(), Destroy(), and Write() functions.
! - Changed the minimum valid file release number from 2 to 3 due to the
!   shared data additions of ALPHA_C1 and ALPHA_C2.
!
! Revision 2.4  2002/10/04 21:09:48  paulv
! - Corrected bug with dimension check.
! - Now only using the file release value to check for code/data updates.
!
! Revision 2.3  2002/10/02 20:34:56  paulv
! - Corrected bug in output messages.
!
! Revision 2.2  2002/08/21 16:32:39  paulv
! - Corrected transmittance coefficient variable name bug.
! - Updated some documentation.
!
! Revision 2.1  2002/08/09 19:37:16  paulv
! New version for new transmittance algorithm.
!
! Revision 1.13  2002/07/24 14:55:59  paulv
! - Updated documentation.
!
! Revision 1.12  2001/10/01 20:28:48  paulv
! - Added "Name" to RCS keyword list.
!
! Revision 1.11  2001/08/31 21:14:36  paulv
! - Added MIN and MAX release/version parameters to allow for valid use of
!   data files within a specified range.
! - Added the absorber space exponential parameter ALPHA to the transittance
!   file data. READ_ and WRITE_ functions updated as well as the valid version
!   number.
!
! Revision 1.10  2001/08/16 17:17:54  paulv
! - Updated documentation
! - The comparison of n_channels and MAX_N_CHANNELS is now done via the
!   MAX_N_CHANNELS methods in the PARAMETERS module.
!
! Revision 1.9  2001/08/09 20:47:25  paulv
! - Added the WRITE_TRANSMITTANCE_COEFFICIENTS function.
! - Moved all the transmittance data type and name definitions from the
!   COEFFICIENT_UTILITY module to this one. Altered USE statement of the
!   COEFFICIENT_UTILITY module to reflect this change.
! - Added VALID_RELEASE and VALID_VERSION parameters for data file version
!   checking.
! - Added data file release and version number read/write to the requisite
!   read/write function.
!
! Revision 1.8  2001/08/01 17:07:02  paulv
! - The absorber space levels are no longer calculated during model
!   initialisation, but are precalculated and stored in the transmittance
!   coefficient data file. Thus a shared data array, ABSORBER_SPACE_LEVELS,
!   was added to this module as were array allocation/deallocation and
!   data read statements.
!
! Revision 1.7  2001/07/12 17:49:04  paulv
! - Removed definitions of the number, type, and name of the transmittance items
!   and moved them into the COEFFICIENT_UTILITY module. They are now available
!   via:
!     USE coefficient_utility, ONLY: Open_Coefficient_File, &
!                                    N_TRANSMITTANCE_ITEMS,      &
!                                    TRANSMITTANCE_DATA_TYPE,    &
!                                    TRANSMITTANCE_DATA_NAME
!   This was done to allow the COEFFICIENT_UTILITY module to be used for
!   reformatting. However, this may change - now definitions for the contents
!   of the transmittance coefficient data file are distributed in two different
!   modules. I don't like that.
!
! Revision 1.6  2001/05/29 17:50:31  paulv
! - Added DESTROY_TRANSMITTANCE_COEFFICIENTS function. This deallocates the
!   data arrays used to store the transmittance coefficient data.
!
! Revision 1.5  2000/11/09 20:42:11  paulv
! - Coefficient arrays are now ALLOCATABLE.
! - Input file format has changed to contain data dimension and type
!   information for file data checks and array allocation.
! - Coefficient data is now read directly into the correct channel position
!   straight from the file. Previosuly a dummy chunk was read and then placed
!   into the shared data array in a separate loop. The new method is faster
!   and a lot easier to follow.
!
! Revision 1.4  2000/08/31 19:36:34  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 1.3  2000/08/24 16:17:02  paulv
! - Dimension order of PREDICTOR_INDEX changed from:
!
!     0:MAX_N_PREDICTORS_TO_USE x MAX_N_ABSORBERS x MAX_N_CHANNELS
!   to:
!     0:MAX_N_PREDICTORS_TO_USE x MAX_N_CHANNELS  x MAX_N_ABSORBERS
!
!   and for TAU_COEFFICIENTS from
!
!     0:MAX_N_PREDICTORS_TO_USE x MAX_N_ABSORBERS x 0:MAX_N_ABSORBER_LAYERS x MAX_N_CHANNELS
!   to
!     0:MAX_N_PREDICTORS_TO_USE x 0:MAX_N_ABSORBER_LAYERS x MAX_N_CHANNELS x MAX_N_ABSORBERS
!
!   This allowed for more efficent access to the various data on an absorber
!   by absorber basis (since if there is no significant channel absorption by
!   any particular absorber go to the next absorber).
! - Removed references to the record length parameter. No longer needed as
!   file access is SEQUENTIAL rather than DIRECT.
! - Replaced error check after open_coefficient_file call with a simple
!   test for error_status /= SUCCESS. The open function no longer returns
!   any error status other than SUCCESS or FAILURE (used to return WARNING
!   in some circumstances.)
! - "REC =" keyword removed from file READ statement.
! - TAU_COEFFICIENTS array filled by looping over the number of absorber layers.
!   This is only marginally faster than using array syntax but it makes the code
!   a bit easier to understand (IMO).
! - Channel loop construct name changed from "channel_loop" to "l_channel_loop"
!   to indicate the loop counter variable is "l". This is not a big deal for
!   this situation but has proven useful in other modules with a high degree
!   of nested loops.
! - Updated module and subprogram documentation.
!
! Revision 1.2  2000/08/08 17:03:38  paulv
! Module modified to:
! - Read the transmittance coefficients correctly! and
! - To use the PARAMETERS module rather than the CONSTANTS module.
!
! Revision 1.1  2000/07/12 16:08:10  paulv
! Initial checked in version
!
!
!
