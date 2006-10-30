!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_CloudCoeff
!
! PURPOSE:
!       Module containing the shared CRTM scattering coefficients
!       (CloudCoeff) and their load/destruction routines. 
!
! CATEGORY:
!       CRTM : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_CloudCoeff
!
! PUBLIC DATA:
!       ScatC:   Data structure containing the scattering coefficient data
!                UNITS:      N/A
!                TYPE:       CloudCoeff_type
!                DIMENSION:  Scalar
!                ATTRIBUTES: PUBLIC, SAVE
!
! MODULES:
!       Type_Kinds:               Module containing data type kind definitions.
!
!       Message_Handler:          Module to define simple error codes and
!                                 handle error conditions
!                                 USEs: FILE_UTILITY module
!
!       CloudCoeff_Define:      Module defining the CloudCoeff data structure
!                                 and containing routines to manipulate it.
!                                 USEs: TYPE_KINDS module
!                                       ERROR_HANDLER module
!                                       COMPUTE_FLOAT_NUMBERS module
!
!       CloudCoeff_Binary_IO:   Module containing routines to read and write
!                                 binary format CloudCoeff files.
!                                 USEs: TYPE_KINDS module
!                                       FILE_UTILITY module
!                                       ERROR_HANDLER module
!                                       CloudCoeff_DEFINE module
!                                       BINARY_UTILITY module
!
!       CRTM_Parameters:          Module of parameter definitions for the CRTM.
!                                 USEs: TYPE_KINDS module
!
! CONTAINS:
!       CRTM_Load_CloudCoeff:      Function to load the CloudCoeff data
!                                    into the module public data structure
!                                    ScatC.
!
!       CRTM_Destroy_CloudCoeff:   Function to deallocate the module public
!                                    data structure ScatC.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       Routines in this module modify the contents of the public
!       data structure ScatC.
!
! RESTRICTIONS:
!       Routines in this module should only be called during the
!       CRTM initialisation.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
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

MODULE CRTM_CloudCoeff


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds
  USE Message_Handler

  USE CloudCoeff_Define
  USE CloudCoeff_Binary_IO

  USE CRTM_Parameters



  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibilities
  ! ------------------

  PRIVATE
  PUBLIC :: CRTM_Load_CloudCoeff
  PUBLIC :: CRTM_Destroy_CloudCoeff


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- RCS Id for the module
  CHARACTER( * ),  PARAMETER, PRIVATE :: MODULE_RCS_ID = &
  '$Id: CRTM_CloudCoeff.f90,v 1.2 2006/05/02 14:58:34 dgroff Exp $'


  !#----------------------------------------------------------------------------#
  !#                -- THE SHARED CloudCoeff DATA STRUCTURE --                #
  !#                                                                            #
  !# Note that the SAVE attribute is specified to ensure that the data is       #
  !# retained even when this module is not being directly accessed.             #
  !#----------------------------------------------------------------------------#

  TYPE( CloudCoeff_type ), SAVE, PUBLIC :: ScatC





CONTAINS





!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Load_CloudCoeff
!
! PURPOSE:
!       Function to load the CloudCoeff scattering coefficient data into
!       the public data structure ScatC.
!
! CATEGORY:
!       CRTM : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Load_CloudCoeff( CloudCoeff_File,                     &  ! Input
!                                              Quiet             = Quiet,             &  ! Optional input
!                                              Process_ID        = Process_ID,        &  ! Optional input
!                                              Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                              Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       CloudCoeff_File:  Name of the CRTM Binary format CloudCoeff file
!                           containing the scattering coefficient data.
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
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           ERROR_HANDLER module.
!                           If == SUCCESS the CloudCoeff data load was successful
!                              == FAILURE an unrecoverable error occurred.
!                              == WARNING the number of channels read in differs
!                                         from that stored in the CRTM_Parameters
!                                         module.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:           Subroutine to output messages
!                                 SOURCE: ERROR_HANDLER module
!
!      Read_CloudCoeff_Binary:  Function to read the CRTM Binary format
!                                 CloudCoeff data file.
!                                 SOURCE: CloudCoeff_BINARY_IO module
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure ScatC.
!
! RESTRICTIONS:
!       None.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Load_CloudCoeff( CloudCoeff_File, &  ! Input
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

    CHARACTER( * ),           INTENT( IN )  :: CloudCoeff_File

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Load_CloudCoeff'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Process_ID_Tag



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
    !#                 -- READ THE CloudCoeff DATA FILE --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Read_CloudCoeff_Binary( TRIM( CloudCoeff_File ), &  ! Input
                                             ScatC,                     &  ! Output
                                             Quiet             = Quiet, &
                                             Process_ID        = Process_ID, &
                                             Output_Process_ID = Output_Process_ID, &
                                             Message_Log       = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME, &
                            'Error loading CloudCoeff data from '//&
                            TRIM( CloudCoeff_File )//&
                            TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION CRTM_Load_CloudCoeff




!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy_CloudCoeff
!
! PURPOSE:
!       Function to deallocate the public data structure ScatC containing
!       the CRTM CloudCoeff scattering coefficient data.
!
! CATEGORY:
!       CRTM : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_CloudCoeff( Process_ID  = Process_ID, &  ! Optional input
!                                                 Message_Log = Message_Log )  ! Error messaging
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
!                         TYPE:       CHARACTER( * )
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
!       Error_Status:     The return value is an integer defining the error
!                         status. The error codes are defined in the
!                         ERROR_HANDLER module.
!                         If == SUCCESS the deallocation of the public ScatC data
!                                       structure was successful
!                            == FAILURE an unrecoverable error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
!       Destroy_CloudCoeff:    Function to deallocate CloudCoeff data
!                                structures.
!                                SOURCE: CloudCoeff_DEFINE module
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure ScatC.
!
! RESTRICTIONS:
!       None.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_CloudCoeff( Process_ID,   &  ! Optional input
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

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_CloudCoeff'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Process_ID_Tag



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

    Error_Status = Destroy_CloudCoeff( ScatC, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error occurred deallocating the public CloudCoeff structure'//&
                            TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION CRTM_Destroy_CloudCoeff

END MODULE CRTM_CloudCoeff


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_CloudCoeff.f90,v 1.2 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_CloudCoeff.f90,v $
! Revision 1.2  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2005/08/19 19:41:21  qliu
! -- rename CRTM_ScatterCoeff.f90 to CRTM_CloudCoeff.f90
!    first working version for cloud scattering
!
! Revision 1.3  2004/11/04 21:55:01  paulv
! - Updated header documentation.
!
! Revision 1.2  2004/08/06 19:20:45  paulv
! - Altered initialization call to have the same form as other CRTM structure
!   initializations, but without the "CRTM_" prefix.
!
! Revision 1.1  2004/06/24 18:57:28  paulv
! initial checkin.
!
!
!
!
