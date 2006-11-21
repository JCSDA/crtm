!------------------------------------------------------------------------------
!P+
! NAME:
!       Assemble_Coefficient_File
!
! PURPOSE:
!       Program to concatenate individual SENSOR SpcCoeff and TauCoeff files
!       into a single file
!
! CATEGORY:
!       CRTM: Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Message_Handler:        Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       SensorInfo_Define:      Module defining the SensorInfo data structure and
!                               containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!
!       SensorInfo_LinkedList:  Module defining the SensorInfo Linked List
!                               data structure and containing routines to
!                               manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     SENSORINFO_DEFINE module
!
!       SensorInfo_IO:          Module continaing routines to read and write ASCII
!                               SensorInfo format files.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!                                     SENSORINFO_DEFINE module
!
!       SpcCoeff_Define:        Module defining the SpcCoeff data structure and
!                               containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!
!       SpcCoeff_netCDF_IO:     Module continaing routines to read and write
!                               netCDF format SpcCoeff files.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     SPCCOEFF_DEFINE module
!                                     NETCDF module
!                                     NETCDF_UTILITY module
!
!       SpcCoeff_Binary_IO:     Module continaing routines to read and write
!                               RTM binary format SpcCoeff files.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!                                     SPCCOEFF_DEFINE module
!                                     COEFFICIENT_UTILITY module
!
!       TauCoeff_Define:        Module defining the TauCoeff data structure and
!                               containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!
!       TauCoeff_netCDF_IO:     Module continaing routines to read and write
!                               netCDF format TauCoeff files.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     TAUCOEFF_DEFINE module
!                                     NETCDF module
!                                     NETCDF_UTILITY module
!
!       TauCoeff_Binary_IO:     Module continaing routines to read and write
!                               RTM binary format TauCoeff files.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!                                     TAUCOEFF_DEFINE module
!                                     COEFFICIENT_UTILITY module
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
!       INPUT:  - SensorInfo file
!               - Individual instrument coefficient files.
!
!       OUTPUT: Concatenated instrument coefficient files.
!
! SIDE EFFECTS:
!       If any output coefficient files already exist, they are overwritten.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Mar-2002
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

PROGRAM Assemble_Coefficient_File


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO

  USE TauCoeff_Define
  USE TauCoeff_netCDF_IO
  USE TauCoeff_Binary_IO

  USE SpcCoeff_Define
  USE SpcCoeff_netCDF_IO
  USE SpcCoeff_Binary_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Assemble_Coefficient_File'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Assemble_Coefficient_File.f90,v 3.6 2006/05/02 14:58:34 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- The type of coefficient assembly
  INTEGER,        PARAMETER :: N_ASSEMBLY_TYPES = 3
  CHARACTER( * ), PARAMETER, DIMENSION( N_ASSEMBLY_TYPES ) :: ASSEMBLY_TYPE_DESCRIPTION = &
    (/ 'Spectral coefficients     ', &
       'Transmittance coefficients', &
       'Both                      ' /)

  ! -- Allowable output formats.
  ! -- Format codes MUST start at 1 and increment by 1.
  INTEGER,        PARAMETER :: N_OUTPUT_FORMATS = 2
  INTEGER,        PARAMETER, DIMENSION( N_OUTPUT_FORMATS ) :: &
    OUTPUT_FORMAT_CODE = (/ 1, & ! netCDF
                            2 /) ! RTM binary

  CHARACTER( * ), PARAMETER, DIMENSION( N_OUTPUT_FORMATS ) :: &
    OUTPUT_FORMAT_NAME = (/ 'netCDF', &
                            'Binary' /)

  CHARACTER( * ), PARAMETER, DIMENSION( N_OUTPUT_FORMATS ) :: &
    OUTPUT_FORMAT_EXTENSION = (/ '.nc ', &
                                 '.bin' /)

  INTEGER,        PARAMETER ::&
    DEFAULT_OUTPUT_FORMAT = 1


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: IO_Status
  INTEGER :: Error_Status

  CHARACTER( 256 ) :: SensorInfo_Filename
  CHARACTER( 256 ) :: Coeff_File_Path

  INTEGER :: Format_Code

  CHARACTER( 256 ) :: SpcCoeff_Filename
  CHARACTER( 256 ) :: TauCoeff_Filename

  INTEGER :: Assembly_Type
  LOGICAL :: Assemble_SpcCoeff
  LOGICAL :: Assemble_TauCoeff

  INTEGER :: n_Sensors, n

  INTEGER :: SpcCoeff_Release, SpcCoeff_Version
  INTEGER :: TauCoeff_Release, TauCoeff_Version

  TYPE( SensorInfo_type )      :: SensorInfo
  TYPE( SensorInfo_List_type ) :: SensorInfo_List

  TYPE( TauCoeff_type ), TARGET  :: TauCoeff_in, TauCoeff_out
  TYPE( TauCoeff_type ), POINTER :: TauCoeff

  TYPE( SpcCoeff_Sensor_type ), TARGET  :: SpcCoeff_in, SpcCoeff_out
  TYPE( SpcCoeff_Sensor_type ), POINTER :: SpcCoeff

  CHARACTER( 256 )  :: Input_TauCoeff_Sensor_Name,    Input_SpcCoeff_Sensor_Name
  CHARACTER( 256 )  :: Input_TauCoeff_Platform_Name,  Input_SpcCoeff_Platform_Name
  CHARACTER( 256 )  :: TauCoeff_ID_Tag

  CHARACTER( 5000 ) :: Output_TauCoeff_Sensor_Name,   Output_SpcCoeff_Sensor_Name
  CHARACTER( 5000 ) :: Output_TauCoeff_Platform_Name, Output_SpcCoeff_Platform_Name



  !#----------------------------------------------------------------------------#
  !#                   -- CREATE THE SensorInfo LINKED LIST --                  #
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
  WRITE( *, '(/5x, " Program to concatenate SENSOR SpcCoeff and TauCoeff")' )
  WRITE( *, '( 5x, "   coefficient files for individual instruments into a")' )
  WRITE( *, '( 5x, "   single file based on SensorInfo entries.")' )
  WRITE( *, '(/5x, " $Revision: 3.6 $")' )
  WRITE( *, '( 5x, a)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                       -- READ THE SensorInfo FILE --                       #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Get the SensorInfo filename
  ! ---------------------------

  WRITE( *, FMT     = '( /5x, "Enter the SensorInfo filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) SensorInfo_Filename
  SensorInfo_Filename = ADJUSTL( SensorInfo_Filename )


  ! ------------------------
  ! Read the SensorInfo data
  ! ------------------------

  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List, &
                                  Quiet = 1 )
                               
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//TRIM( SensorInfo_Filename ), &
                          FAILURE )
    STOP
  END IF

  ! -- Count the number of sensors
  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )

  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#             -- DETERMINE THE TYPE OF COEFFICIENT ASSEMBLY --               #
  !#----------------------------------------------------------------------------#

  ! ----------------------------------
  ! Get user input on what to assemble
  ! ----------------------------------

  WRITE( *, FMT = '( /5x, "Select the type of coefficient assembly:" )' )
  DO n = 1, N_ASSEMBLY_TYPES
    WRITE( *, FMT = '( 10x, i1, ") ", a )' ) n, TRIM( ASSEMBLY_TYPE_DESCRIPTION( n ) )
  END DO
  WRITE( *, FMT     = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i10 )', &
           IOSTAT = IO_Status ) Assembly_Type

  IF ( IO_Status /= 0    .OR. &
       Assembly_Type < 1 .OR. &
       Assembly_Type > n_Assembly_Types ) THEN
    CALL Display_Message( PROGRAM_NAME,    &
                          'Invalid selection', &
                          FAILURE          )
    STOP
  END IF


  ! ------------------
  ! Set assembly flags
  ! ------------------

  SELECT CASE ( Assembly_Type )
    CASE (1)
      Assemble_SpcCoeff = .TRUE.
      Assemble_TauCoeff = .FALSE.
    CASE (2)
      Assemble_SpcCoeff = .FALSE.
      Assemble_TauCoeff = .TRUE.
    CASE DEFAULT
      Assemble_SpcCoeff = .TRUE.
      Assemble_TauCoeff = .TRUE.
  END SELECT



  !#----------------------------------------------------------------------------#
  !#                     -- DETERMINE THE OUTPUT FORMAT --                      #
  !#----------------------------------------------------------------------------#

  ! --------------
  ! Get user input
  ! --------------

  WRITE( *, FMT = '( /5x, "Select the output format type" )' )
  DO n = 1, N_OUTPUT_FORMATS
    WRITE( *, FMT = '( 10x, i1, ") ", a )' ) OUTPUT_FORMAT_CODE(n), &
                                             TRIM( OUTPUT_FORMAT_NAME(n) )
  END DO
  WRITE( *, FMT = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i10 )', &
           IOSTAT = IO_Status ) Format_Code


  ! ----------------
  ! Check user input
  ! ----------------

  ! -- Invalid input
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid input. Setting output format to '//&
                          OUTPUT_FORMAT_NAME( DEFAULT_OUTPUT_FORMAT ), &
                          INFORMATION )
    Format_Code = DEFAULT_OUTPUT_FORMAT
  END IF

  ! -- Invalid value
  IF ( .NOT. ANY( OUTPUT_FORMAT_CODE == Format_Code ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid format type. Setting output format to '//&
                          OUTPUT_FORMAT_NAME( DEFAULT_OUTPUT_FORMAT ), &
                          INFORMATION )
    Format_Code = DEFAULT_OUTPUT_FORMAT
  END IF
    


  !#----------------------------------------------------------------------------#
  !#                 -- GET INSTRUMENT COEFFICIENT DATA PATH --                 #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter the input data file path [e.g. ./coeff_data/]: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) Coeff_File_Path

  Coeff_File_Path = ADJUSTL( Coeff_File_Path )



  !#----------------------------------------------------------------------------#
  !#          -- INITIALISE THE OUTPUT GLOBAL ATTRIBUTE STRINGS --              #
  !#----------------------------------------------------------------------------#

  Output_TauCoeff_Sensor_Name   =' '
  Output_TauCoeff_Platform_Name =' '

  Output_SpcCoeff_Sensor_Name   =' '
  Output_SpcCoeff_Platform_Name =' '



  !#----------------------------------------------------------------------------#
  !#                        -- LOOP OVER THE SENSORS --                         #
  !#----------------------------------------------------------------------------#

  Concatenate_loop: DO n = 1, n_Sensors

    WRITE( *, '(/)' )



    !#--------------------------------------------------------------------------#
    !#          -- GET THE CURRENT SensorInfo DATA FROM THE LIST --             #
    !#--------------------------------------------------------------------------#

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



    !#--------------------------------------------------------------------------#
    !#                   -- CONCATENATE THE TauCoeff DATA --                    #
    !#--------------------------------------------------------------------------#

    TauCoeff_Concatenation: IF ( Assemble_TauCoeff ) THEN


      ! -------------------------------------------------
      ! Depending on the sensor number, read the TauCoeff
      ! data into the appropriate structure
      ! -------------------------------------------------

      IF ( n > 1 ) THEN
        TauCoeff => TauCoeff_In
      ELSE
        TauCoeff => TauCoeff_Out
      END IF


      ! ----------------------
      ! Construct the filename
      ! ----------------------

      TauCoeff_Filename = TRIM( Coeff_File_Path ) // &
                          TRIM( SensorInfo%File_Prefix ) // &
                          '.TauCoeff.nc'


      ! --------------------------------------
      ! Read the data into the INPUT structure
      ! --------------------------------------

      Error_Status = Read_TauCoeff_netCDF( TRIM( TauCoeff_Filename ), &
                                           TauCoeff, &
                                           Sensor_Name   = Input_TauCoeff_Sensor_Name, &
                                           Platform_Name = Input_TauCoeff_Platform_Name, &
                                           ID_Tag        = TauCoeff_ID_Tag )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading TauCoeff file '//TRIM( TauCoeff_Filename ), &
                              Error_Status )
        STOP
      END IF

      ! -- Concatenate global attribute strings for netCDF output file
      Output_TauCoeff_Sensor_Name   = TRIM( Output_TauCoeff_Sensor_Name )//TRIM( Input_TauCoeff_Sensor_Name )//':'
      Output_TauCoeff_Platform_Name = TRIM( Output_TauCoeff_Platform_Name )//TRIM( Input_TauCoeff_Platform_Name )//':'


      ! -----------------------------
      ! Check the TauCoeff sensor Ids
      ! -----------------------------

      IF ( ANY( TauCoeff%NCEP_Sensor_Id   /= SensorInfo%NCEP_Sensor_Id   ) .OR. &
           ANY( TauCoeff%WMO_Satellite_Id /= SensorInfo%WMO_Satellite_Id ) .OR. &
           ANY( TauCoeff%WMO_Sensor_Id    /= SensorInfo%WMO_Sensor_Id    )      ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "TauCoeff sensor Ids in ", a, ", ", 3i5, &
                          &", are different from SensorInfo values, ", 3i5 )' ) &
               TRIM( TauCoeff_Filename ), &
               TauCoeff%NCEP_Sensor_Id(1), TauCoeff%WMO_Satellite_Id(1), TauCoeff%WMO_Sensor_Id(1), &
               SensorInfo%NCEP_Sensor_Id,  SensorInfo%WMO_Satellite_Id,  SensorInfo%WMO_Sensor_Id
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

           
      ! -----------------------------------------
      ! Concatenate the OUTPUT and INPUT TauCoeff
      ! structures along the CHANNEL dimension
      ! -----------------------------------------

      IF ( n > 1 ) THEN

        Error_Status = Concatenate_Channel_TauCoeff( TauCoeff_Out, &
                                                     TauCoeff_In )

        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error concatenating TauCoeff structures at file '//&
                                TRIM( TauCoeff_Filename ), &
                                Error_Status )
          STOP
        END IF


        ! -- Destroy the INPUT TauCoeff structure
        Error_Status = Destroy_TauCoeff( TauCoeff_In )

        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error destroying the TauCoeff_In structure at file '//&
                                TRIM( TauCoeff_Filename ), &
                                Error_Status )
          STOP
        END IF

      END IF

    END IF TauCoeff_Concatenation



    !#--------------------------------------------------------------------------#
    !#                   -- CONCATENATE THE SpcCoeff DATA --                    #
    !#--------------------------------------------------------------------------#

    SpcCoeff_Concatenation: IF ( Assemble_SpcCoeff ) THEN


      ! -------------------------------------------------
      ! Depending on the sensor number, read the SpcCoeff
      ! data into the appropriate structure
      ! -------------------------------------------------

      IF ( n > 1 ) THEN
        SpcCoeff => SpcCoeff_In
      ELSE
        SpcCoeff => SpcCoeff_Out
      END IF


      ! ----------------------
      ! Construct the filename
      ! ----------------------

      SpcCoeff_Filename = TRIM( Coeff_File_Path ) // &
                          TRIM( SensorInfo%File_Prefix ) // &
                          '.SpcCoeff.nc'


      ! --------------------------------------
      ! Read the data into the INPUT structure
      ! --------------------------------------

      Error_Status = Read_SpcCoeff_netCDF( TRIM( SpcCoeff_Filename ), &
                                           SpcCoeff, &

                                           Sensor_Name   = Input_SpcCoeff_Sensor_Name, &
                                           Platform_Name = Input_SpcCoeff_Platform_Name )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading SpcCoeff file '//TRIM( SpcCoeff_Filename ), &
                              Error_Status )
        STOP
      END IF

      ! -- Concatenate global attribute strings for netCDF output file
      Output_SpcCoeff_Sensor_Name   = TRIM( Output_SpcCoeff_Sensor_Name )//TRIM( Input_SpcCoeff_Sensor_Name )//':'
      Output_SpcCoeff_Platform_Name = TRIM( Output_SpcCoeff_Platform_Name )//TRIM( Input_SpcCoeff_Platform_Name )//':'


      ! -----------------------------
      ! Check the SpcCoeff sensor Ids
      ! -----------------------------

      IF ( ANY( SpcCoeff%NCEP_Sensor_Id   /= SensorInfo%NCEP_Sensor_Id   ) .OR. &
           ANY( SpcCoeff%WMO_Satellite_Id /= SensorInfo%WMO_Satellite_Id ) .OR. &
           ANY( SpcCoeff%WMO_Sensor_Id    /= SensorInfo%WMO_Sensor_Id    )      ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "SpcCoeff sensor Ids in ", a, ", ", 3i5, &
                          &", are different from SensorInfo values, ", 3i5 )' ) &
               TRIM( SpcCoeff_Filename ), &
               SpcCoeff%NCEP_Sensor_Id(1), SpcCoeff%WMO_Satellite_Id(1), SpcCoeff%WMO_Sensor_Id(1), &
               SensorInfo%NCEP_Sensor_Id,  SensorInfo%WMO_Satellite_Id,  SensorInfo%WMO_Sensor_Id
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

           
      ! ----------------------------------------------------
      ! Concatenate the OUTPUT and INPUT SpcCoeff structures
      ! ----------------------------------------------------

      IF ( n > 1 ) THEN

        Error_Status = Concatenate_SpcCoeff( SpcCoeff_Out, &
                                             SpcCoeff_In )

        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error concatenating SpcCoeff structures at file '//&
                                TRIM( SpcCoeff_Filename ), &
                                Error_Status )
          STOP
        END IF


        ! -- Destroy the INPUT SpcCoeff structure
        Error_Status = Destroy_SpcCoeff( SpcCoeff_In )

        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error destroying the SpcCoeff_In structure at file '//&
                                TRIM( SpcCoeff_Filename ), &
                                Error_Status )
          STOP
        END IF

      END IF

    END IF SpcCoeff_Concatenation



    !#--------------------------------------------------------------------------#
    !#             -- DESTROY THE CURRENT SensorInfo STRUCTURE --               #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_SensorInfo( SensorInfo )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error destroying SensorInfo structure for sensor # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

  END DO Concatenate_loop



  !#----------------------------------------------------------------------------#
  !#                  -- DESTROY THE SensorInfo LINKED LIST --                  #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    CALL Display_Message( PROGRAM_NAME, &
                          '( "Error destroying SensorInfo_List.', &
                          Error_Status )
  END IF



  !#----------------------------------------------------------------------------#
  !#            -- OUTPUT THE CONCATENATED TauCoeff DATA STRUCTURE --           #
  !#----------------------------------------------------------------------------#

  TauCoeff_Output: IF ( Assemble_TauCoeff ) THEN

    WRITE( *, '( /5x, "Writing the output TauCoeff file..." )' )


    ! -----------------------------
    ! Construct the output filename
    ! -----------------------------

    TauCoeff_Filename = 'Concatenated.TauCoeff'//TRIM( OUTPUT_FORMAT_EXTENSION( Format_Code ) )


    ! --------------
    ! Write the data
    ! --------------

    SELECT CASE ( Format_Code )

      ! -- netCDF output
      CASE ( 1 )
        Error_Status = Write_TauCoeff_netCDF( TRIM( TauCoeff_Filename ), &
                                              TauCoeff_Out, &
                                              Title         = 'Combined transmittance coefficients dataset.', &
                                              History       = PROGRAM_RCS_ID, &
                                              Sensor_Name   = TRIM( Output_TauCoeff_Sensor_Name ), &
                                              Platform_Name = TRIM( Output_TauCoeff_Platform_Name ), &
                                              Comment       = 'Data concatenated in the order shown '//&
                                                              'in the sensor/platform fields.', &
                                              ID_Tag        = TRIM( TauCoeff_ID_Tag ) )

      ! -- Binary output
      CASE ( 2 )
        Error_Status = Write_TauCoeff_Binary( TRIM( TauCoeff_Filename ), &
                                              TauCoeff_Out )

      ! -- Something went REALLY wrong.
      CASE DEFAULT
        Error_Status = FAILURE
        CALL Display_Message( PROGRAM_NAME, &
                              'Invalid format code in TauCoeff output section for output to '//&
                              TRIM( TauCoeff_Filename ), &
                              Error_Status )
    END SELECT

    ! -- Output write error message.
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing TauCoeff file '//TRIM( TauCoeff_Filename ), &
                            Error_Status )
      STOP
    END IF


    ! -------------------------------------
    ! Destroy the OUTPUT TauCoeff structure
    ! -------------------------------------

    Error_Status = Destroy_TauCoeff( TauCoeff_Out )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying the TauCoeff_Out structure.', &
                            Error_Status )
      STOP
    END IF

  END IF TauCoeff_Output



  !#----------------------------------------------------------------------------#
  !#            -- OUTPUT THE CONCATENATED TauCoeff DATA STRUCTURE --           #
  !#----------------------------------------------------------------------------#

  SpcCoeff_Output: IF ( Assemble_SpcCoeff ) THEN

    WRITE( *, '( /5x, "Writing the output SpcCoeff file..." )' )


    ! -----------------------------
    ! Construct the output filename
    ! -----------------------------

    SpcCoeff_Filename = 'Concatenated.SpcCoeff'//TRIM( OUTPUT_FORMAT_EXTENSION( Format_Code ) )


    ! --------------
    ! Write the data
    ! --------------

    SELECT CASE ( Format_Code )

      ! -- netCDF output
      CASE ( 1 )
        Error_Status = Write_SpcCoeff_netCDF( TRIM( SpcCoeff_Filename ), &
                                              SpcCoeff_Out, &
                                              Title         = 'Combined spectral coefficients dataset.', &
                                              History       = PROGRAM_RCS_ID, &
                                              Sensor_Name   = TRIM( Output_SpcCoeff_Sensor_Name ), &
                                              Platform_Name = TRIM( Output_SpcCoeff_Platform_Name ), &
                                              Comment       = 'Data concatenated in the order shown '//&
                                                              'in the sensor/platform fields.' )

      ! -- Binary output
      CASE ( 2 )
        Error_Status = Write_SpcCoeff_Binary( TRIM( SpcCoeff_Filename ), &
                                              SpcCoeff_Out )

      ! -- Something went REALLY wrong.
      CASE DEFAULT
        Error_Status = FAILURE
        CALL Display_Message( PROGRAM_NAME, &
                              'Invalid format code in SpcCoeff output section for output to '//&
                              TRIM( SpcCoeff_Filename ), &
                              Error_Status )
    END SELECT

    ! -- Output write error message.
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing SpcCoeff file '//TRIM( SpcCoeff_Filename ), &
                            Error_Status )
      STOP
    END IF


    ! -------------------------------------
    ! Destroy the OUTPUT SpcCoeff structure
    ! -------------------------------------

    Error_Status = Destroy_SpcCoeff( SpcCoeff_Out )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying the SpcCoeff_Out structure.', &
                            Error_Status )
    END IF

  END IF SpcCoeff_Output

END PROGRAM Assemble_Coefficient_File


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Assemble_Coefficient_File.f90,v 3.6 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 3.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Assemble_Coefficient_File.f90,v $
! Revision 3.6  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 3.5  2006/04/24 17:58:16  wd20pd
! - Merged CRTM_Sensor branch onto main trunk.
!
! Revision 3.4.2.2  2006/03/20 18:27:15  paulv
! - Altered invalid sensor ID output format from 3i4 to 3i5 to prevent
!   crowding of values.
!
! Revision 3.4.2.1  2005/09/20 20:59:17  paulv
! - Updated SpcCoeff type definition for Sensor-based file assembly.
! - Moved to CRTM_Sensor branch.
!
! Revision 3.4  2004/09/27 18:57:44  paulv
! - Updated to Fortran-95
! - Removed all initialisation functions.
!
! Revision 3.3  2004/08/06 20:20:53  paulv
! - Changed initialization calls from Initialize_ to Init_ prefix.
!
! Revision 3.2  2003/07/25 15:57:35  paulv
! - Corrected bug in sensor Id comparison output formats. PGI f90 compiler
!   let them through. IBM XL compiler caught them.
!
! Revision 3.1  2003/07/24 20:12:45  paulv
! - Added sensor Id checks for both the SpcCoeff and TauCoeff data files.
!
! Revision 3.0  2003/04/18 19:13:13  paulv
! - New version. Uses SensorInfo_LinkedList module and coefficient structure
!   pointers to read the coefficient data files.
!
! Revision 2.0  2003/04/14 19:07:34  paulv
! - New version. New structure definition and I/O modules used.
!
! Revision 1.3  2002/11/26 21:56:45  paulv
! - Added capability to assemble TauCoeff files, SpcCoeff files, or both.
!
! Revision 1.2  2002/08/27 19:06:29  paulv
! - Fixed bug with absorber space array check internal subprogram.
!
! Revision 1.1  2002/08/16 20:48:14  paulv
! Initial checkin.
!
!
!
!
