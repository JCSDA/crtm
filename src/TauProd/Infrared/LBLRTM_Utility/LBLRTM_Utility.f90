!--------------------------------------------------------------------------------
!M+
! NAME:
!       LBLRTM_Utility
!
! PURPOSE:
!       Module containing some LBLRTM utility routines
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran95
!
! CALLING SEQUENCE:
!       USE LBLRTM_Utility
!
! MODULES:
!       Type_Kinds:         Module with data type kind definitions.
!
!       File_Utility:       Module containing generic file utility routines
!
!       Message_Handler:    Module containing error handling definitions and
!                           routines.
!                           USEs: FILE_UTILITY module
!
!       LBLRTM_Parameters:  Module containing shared parameters required
!                           for LBLRTM format file IO
!                           USEs: TYPE_KINDS module
!
! CONTAINS:
!       Open_LBLRTM:        Function to open an LBLRTM format file.
!                           Default action is to open the file
!                           for reading.
!       
!       Compute_n_Points:   Function to compute the number of points
!                           in an LBLRTM spectrum.
!
!       Write_LBLRTM_EOL:   Function to write an end-of-layer (EOL) 
!                           marker to an output LBLRTM format file.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 23-Jan-2000
!                     paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000, 2002 Paul van Delst
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
!--------------------------------------------------------------------------------

MODULE LBLRTM_Utility

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE LBLRTM_Parameters


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Open_LBLRTM
  PUBLIC :: Compute_n_Points
  PUBLIC :: Write_LBLRTM_EOL


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: LBLRTM_Utility.f90,v 1.6 2006/07/26 21:43:58 wd20pd Exp $'

  ! -- Keyword set values
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER ::   SET = 1



CONTAINS




!--------------------------------------------------------------------------------
!S+
! NAME:
!       Open_LBLRTM
!
! PURPOSE:
!       Function to open an LBLRTM format file. Default action is to open the
!       file for reading.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran95
!
! CALLING SEQUENCE:
!       Error_Status = Open_LBLRTM( Filename,                 &  ! Input
!                                   FileID,                   &  ! Output
!                                   Write_File  = Write_File, &  ! Optional input
!                                   RCS_Id      = RCS_Id,     &  ! Revision control
!                                   Message_Log = Message_Log )  ! Error messaging
!
! INPUTS:
!       Filename:     Name of the LBLRTM format file to open.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUTS:
!       Write_File:   Set this argument to open a file for writing.
!                     Default is to open for reading.
!                     If WRITE_FILE = 0; Open for reading [DEFAULT].
!                        WRITE_FILE = 1; Open for writing.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!
! OUTPUTS:
!       FileID:       Logical unit number associated with LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the LBLRTM file open was successful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 23-Jan-2000
!                     paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Open_LBLRTM ( Filename,     &  ! Input
                         FileID,       &  ! Output
                         Write_File,   &  ! Optional input
                         RCS_Id,       &  ! Revision control
                         Message_Log ) &  ! Optional input
                       RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),            INTENT( IN )  :: Filename

    ! -- Output
    INTEGER,                   INTENT( OUT ) :: FileID

    ! -- Optional input
    INTEGER,         OPTIONAL, INTENT( IN )  :: Write_File

    ! -- Revision control
    CHARACTER( * ), OPTIONAL,  INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status

 
    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Open_LBLRTM'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Read_File

    CHARACTER( 7 ) :: File_Status
    CHARACTER( 5 ) :: File_Action

    INTEGER :: Lun
    INTEGER :: IO_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = ROUTINE_NAME//' from '//MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- SET FILE ACCESS BASED ON "WRITE_FILE" ARGUMENT --             #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! Check WRITE_FILE argument
    ! -------------------------

    ! -- Default is to open for reading....
    Read_File = .TRUE.
    ! -- ...unless the WRITE_FILE keyword is set
    IF ( PRESENT( Write_File ) ) THEN
      IF ( Write_File == SET ) Read_File = .FALSE.
    END IF


    ! ---------------
    ! Set file access
    ! ---------------

    IF ( Read_File ) THEN

      File_Status = 'OLD'
      File_Action = 'READ'

      ! -- Check that file exists
      IF ( .NOT. File_Exists( Filename ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'File '//TRIM( Filename )//' not found.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Check that file isn't already open
      IF ( File_Open( Filename ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'File '//TRIM( Filename )//' is already open.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    ELSE

      File_Status = 'REPLACE'
      File_Action = 'WRITE'

    END IF



    !#--------------------------------------------------------------------------#
    !#                           -- OPEN THE FILE --                            #
    !#--------------------------------------------------------------------------#

    ! ------------------------
    ! Get an available file id
    ! ------------------------

    Lun = Get_Lun()

    IF ( Lun < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Valid file unit could not be found.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------

    OPEN( Lun, FILE   = TRIM( Filename ),    &
               STATUS = TRIM( File_Status ), &
               ACTION = TRIM( File_Action ), &
               ACCESS = 'SEQUENTIAL',        &
               FORM   = 'UNFORMATTED',       &
               IOSTAT = IO_Status            )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening LBLRTM file ", a, &
                        &" for data ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), TRIM( File_Action ), IO_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- OPEN WAS SUCCESSFUL --                             #
    !#--------------------------------------------------------------------------#

    FileID = Lun

  END FUNCTION Open_LBLRTM




!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_n_Points
!
! PURPOSE:
!       Function to compute the number of points in an LBLRTM spectrum.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran95
!
! CALLING SEQUENCE:
!       n_Points = Compute_n_Points ( Begin_Frequency,    &  ! Input
!                                     End_Frequency,      &  ! Input
!                                     Frequency_Interval, &  ! Input
!                                     RCS_Id = RCS_Id     )  ! Revision control
!
! INPUTS:
!       Begin_Frequency:    Beginning frequency of the spectral data.
!                           UNITS:      cm^-1
!                           TYPE:       REAL( Double )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       End_Frequency:      Ending frequency of the spectral data.
!                           UNITS:      cm^-1
!                           TYPE:       REAL( Double )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       Frequency_Interval: Frequency spacing of the spectral data.
!                           The data type kind is determined by the
!                           values set in the LBLRTM_Parameters module.
!                           UNITS:      cm^-1
!                           TYPE:       REAL( LBLRTM_FP_KIND )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUTS:
!       None
!
! OUTPUTS:
!       None
!
! OPTIONAL OUTPUTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!
! FUNCTION RESULT:
!       n_Points:           The return value is an integer containing the
!                           number of points in the spectrum.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       No error checking is done so must ensure input is valid.
!
! PROCEDURE:
!       The number of points is calculated from the begin and end frequencies,
!       v1 and v2, and the frequency interval, dv, by:
!
!                   ( v2 - v1        )
!         n = FLOOR (--------- + 1.5 )
!                   (   dv           )
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 23-Jan-2000
!                     paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Compute_n_Points ( Begin_Frequency,      &  ! Input
                              End_Frequency,        &  ! Input
                              Frequency_Interval,   &  ! Input
                              RCS_Id              ) &  ! Revision control
                            RESULT ( n_Points )


    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( Double ),           INTENT( IN )  :: Begin_Frequency
    REAL( Double ),           INTENT( IN )  :: End_Frequency
    REAL( LBLRTM_FP_KIND ),   INTENT( IN )  :: Frequency_Interval

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER( Long ) :: n_Points


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_n_Points'


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( Double ) :: rn_Points



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = ROUTINE_NAME//' from '//MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- CALCULATE THE NUMBER OF SPECTRAL POINTS --               #
    !#                                                                          #
    !# The calculation is done in two steps since somecompilers complain (i.e.  #
    !# they issue "CAUTION" messages) when a REAL division occurs in an         #
    !# expression being converted to INTEGER.                                   #
    !#--------------------------------------------------------------------------#

    rn_Points = 1.5_Double + ( ( End_Frequency - Begin_Frequency ) / &
    !                          -----------------------------------
                                REAL( Frequency_Interval, Double )   )

    n_Points  = FLOOR( rn_Points )

  END FUNCTION Compute_n_Points





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Write_LBLRTM_EOL
!
! PURPOSE:
!       Function to write an end-of-layer (EOL) marker to an output
!       LBLRTM format file.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran95
!
! CALLING SEQUENCE:
!       Error_Status = Write_LBLRTM_EOL( FileID,                   &  ! Input
!                                        RCS_Id      = RCS_Id,     &  ! Revision control
!                                        Message_Log = Message_Log )  ! Error messaging
!
! INPUTS:
!       FileID:       Logical unit number associated with LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUTS:
!       None
!
! OPTIONAL OUTPUTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the LBLRTM file EOL write was successful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If an error occurs writing to the file, it is closed.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 15-Apr-2002
!                     paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Write_LBLRTM_EOL ( FileID,       &  ! Input
                              RCS_Id,       &  ! Revision control
                              Message_Log ) &  ! Error messaging
                            RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                   INTENT( IN )  :: FileID

    ! -- Revision control
    CHARACTER( * ), OPTIONAL,  INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status

 
    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_LBLRTM_EOL'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
    INTEGER :: i, n



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = ROUTINE_NAME//' from '//MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- CHECK IF FILE IS OPEN --                        #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. File_Open( FileID ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'LBLRTM file is not open.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE AN END-OF-LEVEL MARKER --                   #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Determine how many *integer*
    ! elements need to be written
    ! ----------------------------

    ! -- How many bytes required?
    n = ( 2 * n_Bytes_Double ) + &  ! == PHdr begin and end frequency
        LBLRTM_FP_N_BYTES      + &  ! == PHdr frequency interval
        LBLRTM_IP_N_BYTES           ! == PHdr number of points

    ! -- Convert the number of bytes to the number of
    ! -- integers (for the LBLRTM integer type)
    n = n / LBLRTM_IP_N_BYTES


    ! --------------------
    ! Write the EOL marker
    ! --------------------

    WRITE( FileID, IOSTAT = IO_Status ) &
      (/ ( INT( LBLRTM_FILE_PTR_EOL, LBLRTM_IP_KIND ), i = 1, n ) /)

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing EOL marker. IOSTAT = ", i5 )' ) IO_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

  END FUNCTION Write_LBLRTM_EOL

END MODULE LBLRTM_Utility


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: LBLRTM_Utility.f90,v 1.6 2006/07/26 21:43:58 wd20pd Exp $
!
! $Date: 2006/07/26 21:43:58 $
!
! $Revision: 1.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: LBLRTM_Utility.f90,v $
! Revision 1.6  2006/07/26 21:43:58  wd20pd
! Replacement of "Error_Handler" with "Message_Handler" in USE statements and
! in documentaiton blocks.
!
! Revision 1.5  2005/05/08 15:13:24  paulv
! - Upgraded to Fortran-95
! - Added optional RCS_Id argument to public procedures.
!
! Revision 1.4  2002/06/05 19:04:23  paulv
! - Removed MESSAGE as a module variable and placed definitions in each
!   module subprogram.
!
! Revision 1.3  2002/05/15 19:24:33  paulv
! - Using FLOOR instrinsic in COMPUTE_N_POINTS function.
! - Added file unit number check.
!
! Revision 1.2  2002/04/16 18:45:38  paulv
! - Added WRITE_LBLRTM_EOL() function to allow easy writing of EOL markers.
! - Changed the way that the rounding is done in the COMPUTE_N_POINTS()
!   function. Originally I did:
!     n_points  = FLOOR( rn_points )
!   but this caused problems when the value for rn_points was *just* over
!   the .5 between integer values. Now
!     n_points  = NINT( rn_points, Long )
!   is done. This sometimes produces an "extra" point, but this si preferable
!   than one less (will cause fatal error in read functions).
! - Updated documentation.
!
! Revision 1.1  2002/04/12 20:57:12  paulv
! Initial checkin. Replacement for LBLRTM_utility (anal I know)
!
!
!
!
