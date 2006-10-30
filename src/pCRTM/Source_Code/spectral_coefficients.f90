!------------------------------------------------------------------------------
!M+
! NAME:
!       Spectral_Coefficients
!
! PURPOSE:
!       Module containing the prototype CRTM (pCRTM) spectral coefficients
!       and their load/destruction routines. 
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Spectral_Coefficients
!
! PUBLIC DATA:
!       SC:   Data structure containing the spectral coefficient data
!             UNITS:      N/A
!             TYPE:       SpcCoeff_Sensor_type
!             DIMENSION:  Scalar
!             ATTRIBUTES: PUBLIC, SAVE
!
! MODULES:
!       Type_Kinds:           Module containing data type kind definitions.
!
!       Message_Handler:      Module to define simple error codes and
!                             handle error conditions
!                             USEs: FILE_UTILITY module
!
!       Parameters:           Module containing parameter definitions for the
!                             RT model.
!                             USEs: TYPE_KINDS module
!
!       SpcCoeff_Define:      Module defining the SpcCoeff data structure and
!                             containing routines to manipulate it.
!                             USEs: TYPE_KINDS module
!                                   ERROR_HANDLER module
!                                   COMPUTE_FLOAT_NUMBERS module
!
!       SpcCoeff_Binary_IO:   Module containing routines to read and write
!                             binary format SpcCoeff files.
!                             USEs: TYPE_KINDS module
!                                   FILE_UTILITY module
!                                   ERROR_HANDLER module
!                                   SPCCOEFF_DEFINE module
!                                   COEFFICIENT_UTILITY module
!
! CONTAINS:
!       Load_Spectral_Coefficients:     Function to load the spectral coefficient data
!                                       into the module public data structure SC.
!
!       Destroy_Spectral_Coefficients:  Function to destroy the module public data
!                                       structure SC.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       The shared data structure, SC, is modified.
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

MODULE Spectral_Coefficients


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds
  USE Message_Handler

  USE Parameters

  USE SpcCoeff_Define
  USE SpcCoeff_Binary_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibilities
  ! ------------------

  PRIVATE
  PUBLIC :: Load_Spectral_Coefficients
  PUBLIC :: Destroy_Spectral_Coefficients


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- RCS Id for the module
  CHARACTER( * ),  PARAMETER, PRIVATE :: MODULE_RCS_ID = &
  '$Id: spectral_coefficients.f90,v 2.6 2006/05/02 14:58:35 dgroff Exp $'


  ! ---------------------------------------------------
  ! The shared spectral coefficient data structure
  !
  ! Note that the SAVE attribute is specified to ensure
  ! that the data is retained even when this module is
  ! not being directly accessed.
  ! ---------------------------------------------------

  TYPE( SpcCoeff_Sensor_type ), SAVE, PUBLIC :: SC



CONTAINS


!------------------------------------------------------------------------------
!S+
! NAME:
!       Load_Spectral_Coefficients
!
! PURPOSE:
!       Function to load the spectral coefficient data into the public
!       data structure SC.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Load_Spectral_Coefficients( Coefficient_File,                      &  ! Input
!                                                  Quiet             = Quiet,             &  ! Optional input
!                                                  Process_ID        = Process_ID,        &  ! Optional input
!                                                  Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                                  Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Coefficient_File:   Name of the pCRTM binary format SpcCoeff file
!                           containing the spectral coefficient data.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
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
!                           If == SUCCESS the spectral coefficient read was successful
!                              == FAILURE an unrecoverable error occurred.
!                              == WARNING the number of channels read in is different
!                                         from that set in the Parameters module.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:         Subroutine to output messages
!                               SOURCE: Message_Handler module
!
!      Read_SpcCoeff_Binary:    Function to read the binary format SpcCoeff
!                               data file.
!                               SOURCE: SPCCOEFF_BINARY_IO module
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
!       This function modifies the contents of the public data structure SC.
!
! RESTRICTIONS:
!       None.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Load_Spectral_Coefficients( Coefficient_file,  &  ! Input
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Load_Spectral_Coefficients'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message 

    CHARACTER( 128 ) :: Process_ID_Tag

    ! -- Maximum channels pseudo parameter
    INTEGER :: MAX_N_CHANNELS
    LOGICAL :: is_set



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
    !#                   -- READ THE SPCCOEFF DATA FILE --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = Read_SpcCoeff_Binary( Coefficient_File, &
                                         SC, &
                                         Quiet             = Quiet, &
                                         Process_ID        = Process_ID, &
                                         Output_Process_ID = Output_Process_ID, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading spectral coefficients from '//&
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
      IF ( Max_n_Channels /= SC%n_Channels ) THEN
        Error_Status = WARNING
        WRITE( Message, '( "MAX_N_CHANNELS set to different value, ", i4, ", ", &
                          &"than defined in spectral coefficient file, ", i4, &
                          &". Overwriting" )' ) &
                        Max_n_Channels, SC%n_Channels
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message )//TRIM( Process_ID_Tag ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CALL Set_Max_n_Channels( SC%n_Channels )
      END IF
    ELSE
      CALL Set_Max_n_Channels( SC%n_Channels )
    END IF

  END FUNCTION Load_Spectral_Coefficients




!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_Spectral_Coefficients
!
! PURPOSE:
!       Function to deallocate the spectral coefficient shared
!       data arrays.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_Spectral_Coefficients( Process_ID  = Process_ID, &  ! Optional input
!                                                     Message_Log = Message_Log )  ! Error messaging
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
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to the screen.
!                         UNITS:      N/A
!                         TYPE:       Character
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
!      Destroy_SpcCoeff:        Function to destroy the public data
!                               structure used to share the spectral
!                               coefficient data
!                               SOURCE: SPCCOEFF_DEFINE module
!
!      Reset_Max_n_Channels:    Routine to reset the value of the
!                               MAX_N_CHANNELS "pseudo-parameter".
!                               SOURCE: PARAMETERS module
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure SC.
!
! RESTRICTIONS:
!       None.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_Spectral_Coefficients( Process_ID,   &  ! Optional input
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_Spectral_Coefficients'


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

    Error_Status = Destroy_SpcCoeff( SC, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred destroying spectral coefficient data arrays'//&
                            TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- RESET THE MAX_N_CHANNELS "PSEUDO-PARAMETER" --              #
    !#--------------------------------------------------------------------------#

    CALL Reset_Max_n_Channels()

  END FUNCTION Destroy_Spectral_Coefficients 

END MODULE Spectral_Coefficients


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: spectral_coefficients.f90,v 2.6 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 2.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: spectral_coefficients.f90,v $
! Revision 2.6  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 2.5  2006/04/20 16:52:56  paulv
! - Updated to use new Sensor form of SpcCoeff type.
!
! Revision 2.4  2004/12/22 17:43:33  paulv
! - Updated header documentation.
!
! Revision 2.3  2004/09/10 21:46:19  paulv
! - Updated to use new definition module. No structure initialisation.
!
! Revision 2.2  2004/08/02 16:47:14  paulv
! - Changed SpcCoeff initialization call from Initialize_SpcCoeff() to
!   Init_SpcCoeff().
!
! Revision 2.1  2003/07/10 16:31:06  paulv
! - Added module RCS Id parameter.
! - Removed DEFAULT_RELEASE parameter and the optional VALID_RELEASE argument
!   to the Load() routine. The SpcCoeff definition module contains the valid
!   release value now. Any difference between that value and what resides in
!   the file generates an error since a change in the release value indicates
!   a change in the file format.
!
! Revision 2.0  2003/05/16 17:31:33  paulv
! - New version using the new spectral coefficient structure and I/O routines.
!
! Revision 1.13  2002/07/24 14:55:13  paulv
! - Updated documentation.
!
! Revision 1.12  2001/10/01 20:28:47  paulv
! - Added "Name" to RCS keyword list.
!
! Revision 1.11  2001/08/31 21:11:41  paulv
! - Added MIN and MAX release/version parameters to allow for valid use of
!   data files within a specified range.
!
! Revision 1.10  2001/08/16 17:16:30  paulv
! - Updated documentation
! - The comparison of n_channels and MAX_N_CHANNELS is now done via the
!   MAX_N_CHANNELS methods in the PARAMETERS module.
!
! Revision 1.9  2001/08/09 20:45:33  paulv
! - Added the WRITE_SPECTRAL_COEFFICIENTS function.
! - Moved all the spectral data type and name definitions from the
!   COEFFICIENT_UTILITY module to this one. Altered USE statement of the
!   COEFFICIENT_UTILITY module to reflect this change.
! - Added VALID_RELEASE and VALID_VERSION parameters for data file version
!   checking.
! - Added data file release and version number read/write to the requisite
!   read/write function.
!
! Revision 1.8  2001/07/12 17:46:31  paulv
! - Removed definitions of the number, type, and name of the spectral items
!   and moved them into the COEFFICIENT_UTILITY module. They are now available
!   via:
!     USE coefficient_utility, ONLY: open_coefficient_file, &
!                                    N_SPECTRAL_ITEMS,      &
!                                    SPECTRAL_DATA_TYPE,    &
!                                    SPECTRAL_DATA_NAME
!   This was done to allow the COEFFICIENT_UTILITY module to be used for
!   reformatting. However, this may change - now definitions for the contents
!   of the spectral coefficient data file are distributed in two different
!   modules. I don't like that.
! - Compressed the coefficient READ statement.
!
! Revision 1.7  2001/05/29 17:49:32  paulv
! - Made ALL real valued spectral coefficients double precision. Previously
!   the cosmic background and solar terms were single precision.
! - Added precalculated cosmic background radiance term.
! - Some cosmetic changes.
! - Changed USE_SOLAR array name to IS_SOLAR_CHANNEL.
! - Added DESTROY_SPECTRAL_COEFFICIENTS function. This deallocates the
!   data arrays used to store the spectral coefficient data.
!
! Revision 1.6  2001/01/09 21:23:27  paulv
! - Added IS_MICROWAVE_CHANNEL and COSMIC_BACKGROUND_TEMPERATURE arrays to
!   read.
! - Updated module documentation.
!
! Revision 1.5  2000/11/09 20:39:49  paulv
! - Coefficient arrays are now ALLOCATABLE.
! - Input file format has changed to contain data dimension and type
!   information for file data checks and array allocation.
!
! Revision 1.4  2000/08/31 19:36:33  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 1.3  2000/08/24 16:06:28  paulv
! - Removed references to the record length parameter. No longer needed as
!   file access is SEQUENTIAL rather than DIRECT.
! - Replaced error check after OPEN_COEFFICIENT_FILE call with a simple
!   test for error_status /= SUCCESS. The open function no longer returns
!   any error status other than SUCCESS or FAILURE (used to return WARNING
!   in some circumstances.)
! - "REC =" keyword removed from file READ statement.
! - Channel loop construct name changed from "channel_loop" to "l_channel_loop"
!   to indicate the loop counter variable is "l". This is not a big deal for
!   this situation but has proven useful in other modules with a high degree
!   of nested loops.
! - Updated module and subprogram documentation.
!
! Revision 1.2  2000/08/08 17:04:02  paulv
! Module modified to:
! - Read the spectral coefficients correctly! and
! - To use the PARAMETERS module rather than the CONSTANTS module.
!
! Revision 1.1  2000/07/12 16:08:10  paulv
! Initial checked in version
!
!
!
