!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_EmisCoeff
!
! PURPOSE:
!       Module containing the Infrared Sea Surface Emissivity Model (IRSSEM)
!       emissivity coefficients and their load/destruction routines. 
!
! CATEGORY:
!       CRTM : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_EmisCoeff
!
! PUBLIC DATA:
!       EmisC:   Data structure containing the emissivity coefficient data
!                UNITS:      N/A
!                TYPE:       EmisCoeff_type
!                DIMENSION:  Scalar
!                ATTRIBUTES: PUBLIC, SAVE
!
! MODULES:
!       Type_Kinds:              Module containing data type kind definitions.
!
!       Message_Handler:         Module to define error codes and handle error
!                                conditions
!                                USEs: FILE_UTILITY module
!
!       EmisCoeff_Define:        Module defining the EmisCoeff data structure and
!                                containing routines to manipulate it.
!                                USEs: TYPE_KINDS module
!                                      ERROR_HANDLER module
!                                      COMPUTE_FLOAT_NUMBERS module
!
!       EmisCoeff_Binary_IO:     Module containing routines to read and write
!                                binary format EmisCoeff files.
!                                USEs: TYPE_KINDS module
!                                      FILE_UTILITY module
!                                      ERROR_HANDLER module
!                                      BINARY_FILE_UTILITY module
!                                      EMISCOEFF_DEFINE module
!
! CONTAINS:
!       CRTM_Load_EmisCoeff:     Function to load the EmisCoeff data into
!                                the module public data structure EmisC.
!
!       CRTM_Destroy_EmisCoeff:  Function to deallocate the module public
!                                data structure EmisC.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       This module contains functions that modify the contents of the
!       public data structure EmisC.
!
! RESTRICTIONS:
!       Routines in this module should only be called during the
!       CRTM initialisation or destruction.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2005 Paul van Delst
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

MODULE CRTM_EmisCoeff


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds
  USE Message_Handler

  USE EmisCoeff_Define
  USE EmisCoeff_Binary_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: CRTM_Load_EmisCoeff
  PUBLIC :: CRTM_Destroy_EmisCoeff


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ),  PARAMETER, PRIVATE :: MODULE_RCS_ID = &
  '$Id: CRTM_EmisCoeff.f90,v 1.3 2006/05/02 14:58:34 dgroff Exp $'


  ! ---------------------------------------------------
  ! The shared emissivity coefficient data structure
  !
  ! Note that the SAVE attribute is specified to ensure
  ! that the data is retained even when this module is
  ! not being directly accessed.
  ! ---------------------------------------------------

  TYPE( EmisCoeff_type ), SAVE, PUBLIC :: EmisC



CONTAINS



!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Load_EmisCoeff
!
! PURPOSE:
!       Function to load the CRTM EmisCoeff data into the public data
!       structure EmisC.
!
! CATEGORY:
!       CRTM : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Load_EmisCoeff( EmisCoeff_File,                        &  ! Input
!                                           Quiet             = Quiet,             &  ! Optional input
!                                           Process_ID        = Process_ID,        &  ! Optional input
!                                           Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                           Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       EmisCoeff_File:     Name of the Binary format EmisCoeff file.
!                           UNITS:      None
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
!                           UNITS:      None
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATIOn message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      None
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
!                           UNITS:      None
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      None
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
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the ERROR_HANDLER module.
!                           If == SUCCESS the EmisCoeff read was successful
!                              == FAILURE an unrecoverable error occurred during
!                                         the data read.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:         Subroutine to output messages
!                               SOURCE: ERROR_HANDLER module
!
!      Read_EmisCoeff_Binary:   Function to read the Binary format EmisCoeff
!                               data files.
!                               SOURCE: EMISCOEFF_BINARY_IO module
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure EmisC.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Load_EmisCoeff( EmisCoeff_File,    &  ! Input
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

    CHARACTER( * ),           INTENT( IN )  :: EmisCoeff_File

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Quiet
    INTEGER,        OPTIONAL, INTENT( IN )  :: Process_ID
    INTEGER,        OPTIONAL, INTENT( IN )  :: Output_Process_ID

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Load_EmisCoeff'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 512 ) :: Process_ID_Tag
    INTEGER :: Read_Status



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
    !#                   -- READ THE EMISCOEFF DATA FILE --                     #
    !#--------------------------------------------------------------------------#

    Read_Status = Read_EmisCoeff_Binary( EmisCoeff_File, &
                                         EmisC, &
                                         Quiet             = Quiet, &
                                         Process_ID        = Process_ID, &
                                         Output_Process_ID = Output_Process_ID, &
                                         Message_Log       = Message_Log )

    IF ( Read_Status == FAILURE ) THEN
      Error_Status = Read_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading IRSSE coefficients from '//&
                            TRIM( EmisCoeff_File )//&
                            TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION CRTM_Load_EmisCoeff 





!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy_EmisCoeff
!
! PURPOSE:
!       Function to deallocate the public data structure EmisC containing
!       the CRTM EmisCoeff data.
!
! CATEGORY:
!       CRTM : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_EmisCoeff( Process_ID  = Process_ID, &  ! Optional input
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Process_ID:       Set this argument to the MPI process ID that this
!                         function call is running under. This value is used
!                         solely for controlling message output. If MPI is not
!                         being used, ignore this argument.
!                         UNITS:      None
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to the screen.
!                         UNITS:      None
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
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the deallocation of the public EmisC data
!                                   structure was successful.
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:         Subroutine to output messages
!                               SOURCE: ERROR_HANDLER module
!
!      Destroy_EmisCoeff:       Function to deallocate EmisCoeff data
!                               structures.
!                               SOURCE: EMISCOEFF_DEFINE module
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure EmisC.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_EmisCoeff( Process_ID,   &  ! Optional input
                                   Message_Log ) &  ! Error messaging
                                 RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Process_ID

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_EmisCoeff'


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

    Error_Status = Destroy_EmisCoeff( EmisC, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred deallocating the public EmisC structure'//&
                            TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION CRTM_Destroy_EmisCoeff 

END MODULE CRTM_EmisCoeff


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_EmisCoeff.f90,v 1.3 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 1.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_EmisCoeff.f90,v $
! Revision 1.3  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.2  2005/07/29 16:30:25  paulv
! - Updated header documentation.
!
! Revision 1.1  2005/06/22 14:38:33  paulv
! Initial checkin.
!
!
!
