!------------------------------------------------------------------------------
!M+
! NAME:
!       TauCoeff_SARTA_Binary_IO
!
! PURPOSE:
!       Module containing routines to read and write Binary format
!       TauCoeff_SARTA files.
!       
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE TauCoeff_SARTA_Binary_IO
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       File_Utility:          Module containing generic file utility routines
!
!       Message_Handler:         Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
!       Binary_File_Utility:   Module containing utility routines for "Binary" 
!                              format datafiles.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    Message_Handler module
!
!       TauCoeff_SARTA_Define:       Module defining the TauCoeff_SARTA data structure and
!                              containing routines to manipulate it.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    Message_Handler module
!
!       Tau_OPTRAN_Coeff_Binary_IO:    Module containing routines to read and write
!                                      Tau_OPTRAN_Coeff Binary format files.
!                                USEs: TYPE_KINDS module
!                                      FILE_UTILITY module
!                                      Message_Handler module
!                                      BINARY_FILE_UTILITY module
!                                      TAU_OPTRAN_COEFF_DEFINE module
!
!       TauCoeff_SARTA_TraceGas_Binary_IO:    Module containing routines to read and write
!                                       TauCoeff_SARTA_TraceGas Binary format files.
!                                USEs: TYPE_KINDS module
!                                      FILE_UTILITY module
!                                      Message_Handler module
!                                      BINARY_FILE_UTILITY module
!                                      TAUCOEFF_SARTA_TRACEGAS_DEFINE module
!
!       TauCoeff_SARTA_Subset_Binary_IO:     Module containing routines to read and write
!                                      TauCoeff_SARTA_Subset Binary format files.
!                                USEs: TYPE_KINDS module
!                                      FILE_UTILITY module
!                                      Message_Handler module
!                                      BINARY_FILE_UTILITY module
!                                      TAUCOEFF_SARTA_SUBSET_DEFINE module
!
!
! CONTAINS:
!       Inquire_TauCoeff_SARTA_Binary: Function to inquire a Binary format
!                                TauCoeff_SARTA file.
!
!       Read_TauCoeff_SARTA_Binary:    Function to read a Binary format
!                                TauCoeff_SARTA file.
!
!       Write_TauCoeff_SARTA_Binary:   Function to write a Binary format
!                                TauCoeff_SARTA file.
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
!       User specified Binary format TauCoeff_SARTA data files for both
!       input and output.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 05-May-2006
!                       Yong.Chen@noaa.gov
!
!  Copyright (C) 2006 Yong Chen
!
!M-
!------------------------------------------------------------------------------

MODULE TauCoeff_SARTA_Binary_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE Binary_File_Utility

  USE TauCoeff_SARTA_Define

  USE Tau_OPTRAN_SA_Coeff_Binary_IO
  USE TauCoeff_SA_TraceGas_Binary_IO
  USE TauCoeff_SARTA_Subset_Binary_IO
  
   
  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Inquire_TauCoeff_SARTA_Binary
  PUBLIC :: Read_TauCoeff_SARTA_Binary
  PUBLIC :: Write_TauCoeff_ODCAPS_Binary


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER :: SET = 1


CONTAINS





  FUNCTION Inquire_TauCoeff_SARTA_Binary( Filename,           &  ! Input
                                    Algorithm_ID,       &  ! Optional Output
                                    n_Layers,           &  ! Optional output
                                    n_Channels,         &  ! Optional output
                                    n_Tunings,  	&  ! Optional output 
			            n_F_Predictors,	&  ! Optional output
			            n_RefProfile_Items, &  ! Optional output
			            n_NONLTE_Predictors,&  ! Optional output
			            n_NONLTE_Channels,  &  ! Optional output
			            n_TraceGases,	&  ! Optional output
			            n_Subsets,  	&  ! Optional output
                                    Release,            &  ! Optional Output
                                    Version,            &  ! Optional Output
                                    RCS_Id,             &  ! Revision control
                                    Message_Log )       &  ! Error messaging
                                  RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Filename

    ! -- Optional output
    INTEGER,        OPTIONAL, INTENT( OUT ) :: Algorithm_ID 
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Layers
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Channels
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Tunings 
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_F_Predictors
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_RefProfile_Items
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_NONLTE_Predictors
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_NONLTE_Channels
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_TraceGases
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Subsets 
    
    INTEGER,        OPTIONAL, INTENT( OUT ) :: Release
    INTEGER,        OPTIONAL, INTENT( OUT ) :: Version

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_TauCoeff_SARTA_Binary'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER( Long ) :: File_Algorithm_ID
    INTEGER( Long ) :: File_Release
    INTEGER( Long ) :: File_Version
    INTEGER( Long ) :: File_n_Layers
    INTEGER( Long ) :: File_n_Channels
    INTEGER( Long ) :: File_n_Tunings 
    INTEGER( Long ) :: File_n_F_Predictors 
    INTEGER( Long ) :: File_n_RefProfile_Items 
    INTEGER( Long ) :: File_n_NONLTE_Predictors 
    INTEGER( Long ) :: File_n_NONLTE_Channels 
    INTEGER( Long ) :: File_n_TraceGases 
    INTEGER( Long ) :: File_n_Subsets 
 

    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- OPEN THE BINARY FORMAT TauCoeff_SARTA DATA FILE --              #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID,   &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening TauCoeff_SARTA file '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- READ THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Read the Release/Version information
    ! ------------------------------------

    READ( FileID, IOSTAT = IO_Status ) File_Release, &
                                       File_Version

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading TauCoeff_SARTA file Release/Version values from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! ------------------------------------
    ! Read the Algorithm_ID  information
    ! ------------------------------------
    READ( FileID, IOSTAT = IO_Status ) File_Algorithm_ID 
    
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading TauCoeff_SARTA file Algorithm_ID value from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! -------------------
    ! Read the dimensions
    ! -------------------

    READ( FileID, IOSTAT = IO_Status )File_n_Layers,           &
                                      File_n_Channels,         &
                                      File_n_Tunings,          & 
                                      File_n_F_Predictors,     & 
                                      File_n_RefProfile_Items, & 
                                      File_n_NONLTE_Predictors,& 
                                      File_n_NONLTE_Channels,  & 
                                      File_n_TraceGases,       &
                                      File_n_Subsets 
    
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading data dimensions from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP', &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- ASSIGN THE RETURN ARGUMENTS --                     #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Dimensions
    ! ----------
    IF ( PRESENT( Algorithm_ID ) ) THEN
      Algorithm_ID  = File_Algorithm_ID 
    END IF

    IF ( PRESENT( n_Layers ) ) THEN
      n_Layers  = File_n_Layers 
    END IF

    IF ( PRESENT( n_Channels ) ) THEN
      n_Channels = File_n_Channels
    END IF

    IF ( PRESENT( n_Tunings ) ) THEN
      n_Tunings = File_n_Tunings 
    END IF

    IF ( PRESENT( n_F_Predictors  ) ) THEN
      n_F_Predictors = File_n_F_Predictors 
    END IF

    IF ( PRESENT( n_RefProfile_Items ) ) THEN
      n_RefProfile_Items = File_n_RefProfile_Items 
    END IF

    IF ( PRESENT( n_NONLTE_Predictors ) ) THEN
      n_NONLTE_Predictors = File_n_NONLTE_Predictors 
    END IF

    IF ( PRESENT( n_NONLTE_Channels ) ) THEN
      n_NONLTE_Channels = File_n_NONLTE_Channels 
    END IF

    IF ( PRESENT( n_TraceGases ) ) THEN
      n_TraceGases = File_n_TraceGases  
    END IF

    IF ( PRESENT( n_Subsets  ) ) THEN
      n_Subsets = File_n_Subsets 
    END IF
    
    ! --------------
    ! Ancillary info
    ! --------------

    IF ( PRESENT( Release ) ) THEN
      Release = File_Release
    END IF


    IF ( PRESENT( Version ) ) THEN
      Version = File_Version
    END IF

  END FUNCTION Inquire_TauCoeff_SARTA_Binary


  FUNCTION Read_TauCoeff_SARTA_Binary( Filename,          &  ! Input
                                 TauCoeff,          &  ! Output
                                 Quiet,             &  ! Optional input
                                 Process_ID,        &  ! Optional input
                                 Output_Process_ID, &  ! Optional input
                                 RCS_Id,            &  ! Revision control
                                 Message_Log )      &  ! Error messaging
                               RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )     :: Filename

    ! -- Output
    TYPE( TauCoeff_SARTA_type ),    INTENT( IN OUT ) :: TauCoeff

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: Quiet
    INTEGER,        OPTIONAL, INTENT( IN )     :: Process_ID
    INTEGER,        OPTIONAL, INTENT( IN )     :: Output_Process_ID

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_TauCoeff_SARTA_Binary'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    CHARACTER( 128 ) :: Process_ID_Tag
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER( Long ) :: n_Layers 	    
    INTEGER( Long ) :: n_Channels	    
    INTEGER( Long ) :: n_Tunings 	    
    INTEGER( Long ) :: n_F_Predictors 	    
    INTEGER( Long ) :: n_RefProfile_Items   
    INTEGER( Long ) :: n_NONLTE_Predictors  
    INTEGER( Long ) :: n_NONLTE_Channels    
    INTEGER( Long ) :: n_TraceGases 	    
    INTEGER( Long ) :: n_Subsets 	    
    INTEGER( Long ) :: Sensor_Descriptor_StrLen
  


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info message output
    ! -------------------

    ! -- Output informational messages....
    Noisy = .TRUE.

    ! -- ....unless....
    IF ( PRESENT( Quiet ) ) THEN
      ! -- ....the QUIET keyword is set.
      IF ( Quiet == SET ) Noisy = .FALSE.
    ELSE
      ! -- ....the Process_ID is not selected for output
      IF ( PRESENT( Process_ID ) .AND. PRESENT( Output_Process_ID ) ) THEN
        IF ( Process_ID /= Output_Process_ID ) Noisy = .FALSE.
      END IF
    END IF


    ! -----------------------------------
    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    ! -----------------------------------

    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '( ";  MPI Prcess ID: ", i5 )' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF


    ! -------------------
    ! Module version info
    ! -------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! --------------------------
    ! Check that the file exists
    ! --------------------------

    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Message = 'File '//TRIM( Filename )//' not found.'
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
      Destroy_Status = Destroy_TauCoeff_SARTA(TauCoeff, &
                                              Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                    -- OPEN THE TauCoeff_SARTA DATA FILE --                     #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID,   &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- READ THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Read the Release/Version information
    ! ------------------------------------

    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Release, &
                                       TauCoeff%Version

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading TauCoeff file Release/Version values from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------------
    ! Check the release
    ! -----------------

    Error_Status = Check_TauCoeff_SARTA_Release( TauCoeff, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'TauCoeff Release check failed for '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! Read the Algorithm_ID
    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Algorithm_ID  

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Algorithm_ID from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! -------------------
    ! Read the dimensions
    ! -------------------

    READ( FileID, IOSTAT = IO_Status ) n_Layers,           &
                                       n_Channels,	   &
                                       n_Tunings,	   & 
                                       n_F_Predictors,     & 
                                       n_RefProfile_Items, & 
                                       n_NONLTE_Predictors,& 
                                       n_NONLTE_Channels,  & 
                                       n_TraceGases,	   &
                                       n_Subsets 

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading data dimensions from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ----------------------------------------
    ! Read the sensor descriptor string length
    ! ----------------------------------------

    READ( FileID, IOSTAT = IO_Status ) Sensor_Descriptor_StrLen
 
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Sensor_Descriptor_StrLen from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! -- Check that the string length is consistent
    IF ( Sensor_Descriptor_StrLen /= TauCoeff%Sensor_Descriptor_StrLen ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Sensor descriptor string length ", a, " (", i2, &
                        &") is inconsistent with definition (", i2, ")." )' ) &
                      TRIM( Filename ), &
                      Sensor_Descriptor_StrLen, &
                      TauCoeff%Sensor_Descriptor_StrLen
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF
 
    !#--------------------------------------------------------------------------#
    !#                  -- ALLOCATE THE TauCoeff_SARTA STRUCTURE --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = Allocate_TauCoeff_SARTA( n_Layers,	          & 
                                      n_Channels,	  & 
                                      n_Tunings,	  & 
                                      n_F_Predictors,	  & 
                                      n_RefProfile_Items, & 
                                      n_NONLTE_Predictors,& 
                                      n_NONLTE_Channels,  & 
                                      n_TraceGases,	  & 
                                      n_Subsets,	  &  
                                      TauCoeff,           &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error occurred allocating TauCoeff_SARTA structure.'//&
                            TRIM( Process_ID_Tag ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

 
    !#--------------------------------------------------------------------------#
    !#                       -- READ THE SENSOR ID DATA --                      #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Sensor_Descriptor, &
                                       TauCoeff%NCEP_Sensor_ID, &
                                       TauCoeff%WMO_Satellite_ID, &
                                       TauCoeff%WMO_Sensor_ID
 
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading sensor ID data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    !#--------------------------------------------------------------------------#
    !#                       -- READ THE CHANNEL INDEX DATA --                      #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Sensor_Channel,             &
                                       TauCoeff%Channel_Subset_Index,      &
                                       TauCoeff%Channel_Subset_Position,   &
                                       TauCoeff%Channel_H2O_OPTRAN,        &
                                       TauCoeff%Channel_CO2_Perturbation , &
                                       TauCoeff%Channel_SO2_Perturbation , &
                                       TauCoeff%Channel_HNO3_Perturbation, &
                                       TauCoeff%Channel_N2O_Perturbation,  &
                                       TauCoeff%Channel_NON_LTE 
 
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Channel Index data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                      -- READ THE STANDARD LEVEL PRESSURE DATA --         #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Standard_Level_Pressure 

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Standard level pressure data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#      -- READ THE Fix_Gases,Down_F_Factor,Tuning_Multiple DATA --                        #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Fix_Gases_Adjustment, &
                                       TauCoeff%Down_F_Factor, &
                                       TauCoeff%Tuning_Multiple

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Fix_Gases_Adjustment data from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- READ THE REFERENCE PROFIEL DATA --                       #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Ref_Profile_Data

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading reference profile from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                     -- READ THE NON-LTE COEFFICIENTS --                  #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) TauCoeff%Non_LTE_Coeff 

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading non-LTE coefficients from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                -- READ THE WATER VAPOR OPTRAN COEFFICIENTS DATA--        #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! Get the data filename
    ! ---------------------
 
    !INQUIRE( UNIT = FileID, NAME = Filename )

    ! ---------------------------------------------
    ! Read the water vapor optran coefficients data 
    ! ---------------------------------------------

    Error_Status = Read_Tau_OPTRAN_Coeff_Binary(Filename, &
                                                TauCoeff%Tau_OPTRAN_Coeff, &
                                                No_File_Close = SET, &
                                                Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading water OPTRAN absorption coefficients from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    !#--------------------------------------------------------------------------#
    !#                          -- READ THE TauCoeff_SARTA_TraceGas DATA --                       #
    !#--------------------------------------------------------------------------#

    IF ( n_TraceGases > 0 ) THEN


      ! ---------------------
      ! Get the data filename
      ! ---------------------

      !INQUIRE( UNIT = FileID, NAME = Filename )


      ! -------------------
      ! Read the TauCoeff_SARTA_TraceGas data
      ! -------------------

      Error_Status = Read_TauCoeff_TraceGas_Binary( Filename, &
                                             TauCoeff%TauCoeff_TraceGas, &
                                             No_File_Close = SET, &
                                             Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading TauCoeff_SARTA_TraceGas coefficients from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN

      END IF

    END IF


    !#--------------------------------------------------------------------------#
    !#                          -- READ THE TauCoeff_SARTA_Subset DATA --                       #
    !#--------------------------------------------------------------------------#

    IF ( n_Subsets > 0 ) THEN


      ! ---------------------
      ! Get the data filename
      ! ---------------------

      !INQUIRE( UNIT = FileID, NAME = Filename )


      ! -------------------
      ! Read the TauCoeff_SARTA_TraceGas data
      ! -------------------

      Error_Status = Read_TauCoeff_Subset_Binary( Filename, &
                                             TauCoeff%TauCoeff_Subset, &
                                             No_File_Close = SET, &
                                             Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading TauCoeff_SARTA_Subset coefficients from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN

      END IF

    END IF

    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    !#--------------------------------------------------------------------------#
    !#                   -- COUNT THE NUMBER OF SENSORS --                      #
    !#--------------------------------------------------------------------------#

    CALL Count_TauCoeff_SARTA_Sensors( TauCoeff, Use_WMO_ID = SET )



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO MESSAGE --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Version_TauCoeff_SARTA( TauCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_TauCoeff_SARTA_Binary





  FUNCTION Write_TauCoeff_ODCAPS_Binary( Filename,  &  ! Input 
                                  Sensor_Id,    &  ! Input
                                  Sensor_Type,  &  ! Input
                                  TauCoeff,     &  ! Input
                                  Quiet,        &  ! Optional input
                                  RCS_Id,       &  ! Revision control
                                  Message_Log ) &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Filename
    TYPE( TauCoeff_SARTA_type ),    INTENT( IN )  :: TauCoeff
    INTEGER,        INTENT( IN ) :: Sensor_Type
    CHARACTER( 20 ), INTENT( IN ) :: Sensor_Id       
    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Quiet

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_TauCoeff_SARTA_Binary'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID


    INTEGER :: Algorithm

    INTEGER(Long) :: WMO_Satellite_ID
    INTEGER(Long) :: WMO_Sensor_ID   
     
    
    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS

    Algorithm = 3
    WMO_Satellite_ID = TauCoeff%WMO_Satellite_ID(1)
    WMO_Sensor_ID    = TauCoeff%WMO_Sensor_ID(1)   
    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info message output
    ! -------------------

    ! -- Output informational messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! -------------------
    ! Module version info
    ! -------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_TauCoeff_SARTA( TauCoeff ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT TauCoeff_SARTA pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CHECK INPUT --                               #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Check the TauCoeff_SARTA structure Release
    ! ------------------------------------

    Error_Status = Check_TauCoeff_SARTA_Release( TauCoeff, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'TauCoeff_SARTA Release check failed.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------------------
    ! Check the TauCoeff_SARTA structure dimensions
    ! ---------------------------------------

    IF (TauCoeff% n_Layers	      < 1 .OR. &
        TauCoeff%n_Channels	      < 1 .OR. &
        TauCoeff%n_Tunings	      < 1 .OR. &
        TauCoeff%n_F_Predictors       < 1 .OR. &
        TauCoeff%n_RefProfile_Items   < 1 .OR. &
        TauCoeff%n_NONLTE_Predictors  < 1 .OR. &
	TauCoeff%n_Subsets            < 1 .OR. &
        TauCoeff%n_NONLTE_Channels    < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'One or more dimensions of TauCoeff structure are < or = 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#            -- OPEN THE GAS ABSORPTION COEFFICIENT DATA FILE --           #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID,   &
                                     For_Output  = SET,        &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------
    ! Write the Release/Version information
    ! -------------------------------------

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Release, &
                                        TauCoeff%Version 

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing TauCoeff_SARTA file Release/Version values to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF

    ! Write the Algorithm_ID
    WRITE( FileID, IOSTAT = IO_Status ) Algorithm 

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing TauCoeff_SARTA file  Algorithm_ID to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF
    ! --------------------
    ! Write the dimensions
    ! --------------------

    WRITE( FileID, IOSTAT = IO_Status )  TauCoeff%n_Layers,	      &
                                         TauCoeff%n_Channels,	      &
                                         TauCoeff%n_Tunings,	      & 
                                         TauCoeff%n_F_Predictors,     & 
                                         TauCoeff%n_RefProfile_Items, & 
                                         TauCoeff%n_NONLTE_Predictors,& 
                                         TauCoeff%n_NONLTE_Channels,  & 
                                         TauCoeff%n_TraceGases,       &
                                         TauCoeff%n_Subsets 
    

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing data dimensions to ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                      -- WRITE THE SENSOR ID DATA --                      #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) Sensor_id, &
                                        WMO_Satellite_ID, &
                                        WMO_Sensor_ID, &
                                        Sensor_Type 
 
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing sensor ID data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE CHANNEL INDEX DATA --                     #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Sensor_Channel, 	    &
                                        TauCoeff%Channel_Subset_Index,      &
                                        TauCoeff%Channel_Subset_Position,   &
                                        TauCoeff%Channel_H2O_OPTRAN,	    &
                                        TauCoeff%Channel_CO2_Perturbation , &
                                        TauCoeff%Channel_SO2_Perturbation , &
                                        TauCoeff%Channel_HNO3_Perturbation, &
                                        TauCoeff%Channel_N2O_Perturbation,  &
                                        TauCoeff%Channel_NON_LTE  

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Index data data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- WRITE THE STANDARD LEVEL PRESSURE  DATA --                  #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Standard_Level_Pressure  
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Standard level pressure data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#      -- WRITE THE Fix_Gases,Down_F_Factor,Tuning_Multiple DATA --        #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Fix_Gases_Adjustment, &
                                        TauCoeff%Down_F_Factor, &
                                        TauCoeff%Tuning_Multiple 

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Fix_Gases_Adjustment data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                    -- WRITE THE REFERENCE PROFIEL DATA --                #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Ref_Profile_Data 

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing reference profile data to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                -- WRITE THE NON-LTE COEFFICIENTS  --               #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff%Non_LTE_Coeff 

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing non-LTE coefficients to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    !#--------------------------------------------------------------------------#
    !#                -- WRITE THE WATER VAPOR OPTRAN COEFFICIENTS DATA--        #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! Get the data filename
    ! ---------------------
 
    !INQUIRE( UNIT = FileID, NAME = Filename )

    ! ---------------------------------------------
    ! Read the water vapor optran coefficients data 
    ! ---------------------------------------------

    Error_Status = Write_Tau_OPTRAN_Coeff_Binary(Filename, &
                                                TauCoeff%Tau_OPTRAN_Coeff, &
                                                No_File_Close = SET, &
                                                Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing water OPTRAN absorption coefficients from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    !#--------------------------------------------------------------------------#
    !#              - WRITE THE TauCoeff_SARTA_TraceGas DATA --                       #
    !#--------------------------------------------------------------------------#

    IF ( TauCoeff%n_TraceGases > 0 ) THEN


      ! ---------------------
      ! Get the data filename
      ! ---------------------

      !INQUIRE( UNIT = FileID, NAME = Filename )


      ! -------------------
      ! Write the TauCoeff_SARTA_TraceGas data
      ! -------------------

      Error_Status = Write_TauCoeff_TraceGas_Binary( Filename, &
                                             TauCoeff%TauCoeff_TraceGas, &
                                             No_File_Close = SET, &
                                             Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing TauCoeff_SARTA_TraceGas coefficients from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN

      END IF

    END IF


    !#--------------------------------------------------------------------------#
    !#                -- WRITE THE TauCoeff_SARTA_Subset DATA --                      #
    !#--------------------------------------------------------------------------#

    IF (TauCoeff%n_Subsets > 0 ) THEN


      ! ---------------------
      ! Get the data filename
      ! ---------------------

      !INQUIRE( UNIT = FileID, NAME = Filename )


      ! -------------------
      ! Write the TauCoeff_SARTA_Subset data
      ! -------------------

      Error_Status = Write_TauCoeff_Subset_Binary( Filename, &
                                             TauCoeff%TauCoeff_Subset, &
                                             No_File_Close = SET, &
                                             Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing TauCoeff_SARTA_Subset coefficients from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN

      END IF

    END IF


    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO MESSAGE --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Version_TauCoeff_SARTA( TauCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_TauCoeff_ODCAPS_Binary

END MODULE TauCoeff_SARTA_Binary_IO


 
