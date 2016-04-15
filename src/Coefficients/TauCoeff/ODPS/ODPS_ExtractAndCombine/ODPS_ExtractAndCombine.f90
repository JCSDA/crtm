!
! ODPS_ExtractAndCombine
!
! Program to extract and combine channel data from different ODPS TauCoeff netCDF files.
!
!

PROGRAM ODPS_ExtractAndCombine

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                   Display_Message, Program_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE oSubset_Define
  USE oODPS_Define
  USE oODPS_IO
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'ODPS_ExtractAndCombine'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
    '$Id$'
  INTEGER, PARAMETER :: ML = 256


  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat   ;  CHARACTER(ML) :: err_msg
  INTEGER :: io_stat    ;  CHARACTER(ML) :: io_msg
  INTEGER :: alloc_stat ;  CHARACTER(ML) :: alloc_msg
  CHARACTER(ML) :: msg
  INTEGER :: version
  INTEGER :: i, j, j0, jp, js, k, l, l1, l2, lch, los, ls, n, np
  INTEGER :: n_out_coeffs, n_out_ocoeffs
  INTEGER :: fid
  INTEGER :: n_files
  INTEGER :: n_subset_channels
  INTEGER :: n_orders
  INTEGER :: n_values
  INTEGER, ALLOCATABLE :: idx(:), nmbr(:)
  INTEGER, ALLOCATABLE :: subset_list(:)
  CHARACTER(ML) :: output_filename
  CHARACTER(ML) :: input_filename
  CHARACTER(ML) :: config_filename
  CHARACTER(ML) :: buffer
  TYPE(oSubset_type)   , ALLOCATABLE :: subset(:)
  TYPE(oODPS_File_type), ALLOCATABLE :: oodps_infile(:)
  TYPE(oODPS_File_type) :: oodps_outfile


  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                       'Program to extract and combine channel data from different '//&
                       'ODPS TauCoeff netCDF files.', &
                       '$Revision$' )

  ! Output help if no argument
  IF ( COMMAND_ARGUMENT_COUNT() == 0 ) THEN
    WRITE(*,'(/," Usage: ",a," config-filename")') PROGRAM_NAME
    WRITE(*,*)
    WRITE(*,'(  " Arguments:")')
    WRITE(*,'(  "   config-filename")')
    WRITE(*,'(  "         Configuration file containing the files and channels to be used")')
    WRITE(*,'(  "         in the coefficient extraction and recombination, The file format")')
    WRITE(*,'(  "         is indicated below. Insert your values between the <>.")')
    WRITE(*,*)
    WRITE(*,'(  "           output file: <filename>        ")')
    WRITE(*,'(  "           output version: <V>            ")')
    WRITE(*,'(  "           n_files: <N>                   ")')
    WRITE(*,'(  "           <filename>       }             ")')
    WRITE(*,'(  "           n_channels: <L>  }             ")')
    WRITE(*,'(  "           <ch.1 number>    }  Repeat for ")')
    WRITE(*,'(  "           <ch.2 number>    }-     N      ")')
    WRITE(*,'(  "             ...            }  filenames  ")')
    WRITE(*,'(  "           <ch.L-1 number>  }             ")')
    WRITE(*,'(  "           <ch.L number>    }             ")')
    WRITE(*,*)
    STOP
  END IF

  ! Get the config filename argument
  CALL GET_COMMAND_ARGUMENT(1, VALUE=config_filename, STATUS=err_stat)
  IF ( err_stat /= 0 ) THEN
    err_msg = 'Command line config-filename retrieval failed'
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF


  ! Open the  the configuration file
  OPEN( NEWUNIT = fid, &
        FILE    = config_filename, &
        STATUS  = 'OLD', &
        FORM    = 'FORMATTED', &
        IOSTAT  = io_stat, &
        IOMSG   = io_msg )
  IF ( io_stat /= 0 ) THEN
    err_msg = 'Error opening '//TRIM(config_filename)//' - '//TRIM(io_msg)
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF
  
  
  ! CONFIG: The output filename
  READ(fid, FMT = '(a)', IOSTAT = io_stat, IOMSG = io_msg) buffer
  IF ( io_stat /= 0 ) THEN
    err_msg = 'Error reading the output filename - '//TRIM(io_msg)
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF
  i = MAX(INDEX(buffer,':')+1, 1)
  j = LEN_TRIM(buffer)
  READ(buffer(i:j), '(a)', IOSTAT = io_stat, IOMSG = io_msg ) output_filename
  IF ( io_stat /= 0 ) THEN
    err_msg = 'Error parsing the output filename - '//TRIM(io_msg)
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF
  output_filename = ADJUSTL(output_filename)


  ! CONFIG: The output version
  READ(fid, FMT = '(a)', IOSTAT = io_stat, IOMSG = io_msg) buffer
  IF ( io_stat /= 0 ) THEN
    err_msg = 'Error reading the output file version number - '//TRIM(io_msg)
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF
  i = MAX(INDEX(buffer,':')+1, 1)
  j = LEN_TRIM(buffer)
  READ(buffer(i:j), *, IOSTAT = io_stat, IOMSG = io_msg ) version
  IF ( io_stat /= 0 ) THEN
    err_msg = 'Error parsing the output file version number - '//TRIM(io_msg)
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF


  ! CONFIG: The number of input files
  READ(fid, FMT = '(a)', IOSTAT = io_stat, IOMSG = io_msg) buffer
  IF ( io_stat /= 0 ) THEN
    err_msg = 'Error reading the number of ODPS files to subset - '//TRIM(io_msg)
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF
  i = MAX(INDEX(buffer,':')+1, 1)
  j = LEN_TRIM(buffer)
  READ(buffer(i:j), *, IOSTAT = io_stat, IOMSG = io_msg ) n_files
  IF ( io_stat /= 0 ) THEN
    err_msg = 'Error parsing the number of ODPS files to subset - '//TRIM(io_msg)
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF
  
  
  ! Allocate subset and file objects for the reading
  ALLOCATE( subset(n_files), oodps_infile(n_files), &
            STAT = alloc_stat, ERRMSG = alloc_msg )
  IF ( alloc_stat /= 0 ) THEN
    err_msg = 'Error allocating file arrays - '//TRIM(alloc_msg)
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF


  ! Loop over files to read them as the configuration file is parsed
  Read_Loop: DO n = 1, n_files


    ! CONFIG: The current input filename
    READ(fid, '(a)', IOSTAT = io_stat, IOMSG = io_msg ) input_filename
    IF ( io_stat /= 0 ) THEN
      WRITE(err_msg,'("Error reading subset filename #",i0," - ",a)') n, TRIM(io_msg)
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    END IF
    
    
    ! CONFIG: The number of channel to extract
    READ(fid, FMT = '(a)', IOSTAT = io_stat, IOMSG = io_msg) buffer
    IF ( io_stat /= 0 ) THEN
      err_msg = 'Error reading subset channel number for '//TRIM(input_filename)//' - '//TRIM(io_msg)
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    END IF
    i = MAX(INDEX(buffer,':')+1, 1)
    j = LEN_TRIM(buffer)
    READ(buffer(i:j), *, IOSTAT = io_stat, IOMSG = io_msg ) n_subset_channels
    IF ( io_stat /= 0 ) THEN
      err_msg = 'Error parsing subset channel number for '//TRIM(input_filename)//' - '//TRIM(io_msg)
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    END IF
    IF ( n_subset_channels < 1 ) THEN
      err_msg = 'At least one subset channel must specified for '//TRIM(input_filename)
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    END IF


    ! Allocate the subset list for this file
    ALLOCATE( subset_list(n_subset_channels), &
              STAT = alloc_stat, ERRMSG = alloc_msg )
    IF ( alloc_stat /= 0 ) THEN
      err_msg = 'Error allocating subset list for '//TRIM(input_filename)//' - '//TRIM(alloc_msg)
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    END IF


    ! CONFIG: The list of channels to extract
    DO l = 1, n_subset_channels
      READ(fid, *, IOSTAT = io_stat, IOMSG = io_msg ) subset_list(l)
      IF ( io_stat /= 0 ) THEN
        WRITE(err_msg,'("Error reading subset channel index #",i0," for ",a," - ",a)') &
              l, TRIM(input_filename), TRIM(io_msg)
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
    END DO


    ! Read the current TauCoeff file
    WRITE(*,*)
    msg = 'Reading input file: '//TRIM(input_filename)
    CALL Display_Message( PROGRAM_NAME, msg, INFORMATION )
    
    err_stat = oodps_infile(n)%ReadFile(input_filename)
    IF ( err_stat /= SUCCESS ) THEN
      err_msg = 'Error reading the netCDF ODPS file '//TRIM(input_filename)//' using new method'
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    END IF
    
    
    ! Check file compatibility
    Compatibility_Check: IF ( n > 1 ) THEN

      ! ...The profile set used to generate the coefficients
      IF ( oodps_infile(n)%profile_set_id /= oodps_infile(1)%profile_set_id ) THEN
        WRITE( err_msg, '("Group_Index is different for ",a)') oodps_infile(n)%filename
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...The group index
      IF ( oodps_infile(n)%odps%Group_Index /= oodps_infile(1)%odps%Group_Index ) THEN
        WRITE( err_msg, '("Group_Index is different for ",a)') oodps_infile(n)%filename
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...The sensor type
      IF ( oodps_infile(n)%odps%Sensor_Type /= oodps_infile(1)%odps%Sensor_Type ) THEN
        WRITE( err_msg, '("Sensor_Type is different for ",a)') oodps_infile(n)%filename
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...The WMO sensor Id
      IF ( oodps_infile(n)%odps%WMO_Sensor_Id /= oodps_infile(1)%odps%WMO_Sensor_Id ) THEN
        WRITE( err_msg, '("WMO_Sensor_Id is different for ",a)') oodps_infile(n)%filename
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...The component id's
      IF ( ANY(oodps_infile(n)%odps%Component_Id /= oodps_infile(1)%odps%Component_Id) ) THEN
        WRITE( err_msg, '("Component_Id data are different for ",a)') oodps_infile(n)%filename
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...The absorber id's
      IF ( ANY(oodps_infile(n)%odps%Absorber_Id /= oodps_infile(1)%odps%Absorber_Id) ) THEN
        WRITE( err_msg, '("Absorber_Id data are different for ",a)') oodps_infile(n)%filename
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...The reference level pressures
      IF ( .NOT. ALL(oodps_infile(n)%odps%Ref_Level_Pressure .EqualTo. oodps_infile(1)%odps%Ref_Level_Pressure) ) THEN
        WRITE( err_msg, '("Ref_Level_Pressure data are different for ",a)') oodps_infile(n)%filename
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...The reference pressures
      IF ( .NOT. ALL(oodps_infile(n)%odps%Ref_Pressure .EqualTo. oodps_infile(1)%odps%Ref_Pressure) ) THEN
        WRITE( err_msg, '("Ref_Pressure data are different for ",a)') oodps_infile(n)%filename
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...The reference temperatures
      IF ( .NOT. ALL(oodps_infile(n)%odps%Ref_Temperature .EqualTo. oodps_infile(1)%odps%Ref_Temperature) ) THEN
        WRITE( err_msg, '("Ref_Temperature data are different for ",a)') oodps_infile(n)%filename
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...The reference absorber amounts
      IF ( .NOT. (ALL(oodps_infile(n)%odps%Ref_Absorber .EqualTo. oodps_infile(1)%odps%Ref_Absorber) .AND. &
                  ALL(oodps_infile(n)%odps%Min_Absorber .EqualTo. oodps_infile(1)%odps%Min_Absorber) .AND. &
                  ALL(oodps_infile(n)%odps%Max_Absorber .EqualTo. oodps_infile(1)%odps%Max_Absorber)) ) THEN
        WRITE( err_msg, '("Ref/Min/Max_Absorber data are different for ",a)') oodps_infile(n)%filename
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...The Compact-OPTRAN alpha values
      IF ( (.NOT. (oodps_infile(n)%odps%Alpha    .EqualTo. oodps_infile(1)%odps%Alpha   )) .OR. &
           (.NOT. (oodps_infile(n)%odps%Alpha_C1 .EqualTo. oodps_infile(1)%odps%Alpha_C1)) .OR. &
           (.NOT. (oodps_infile(n)%odps%Alpha_C2 .EqualTo. oodps_infile(1)%odps%Alpha_C2)) ) THEN
        WRITE( err_msg, '("Compact-OPTRAN Alpha data are different for ",a)') oodps_infile(n)%filename
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...The Compact-OPTRAN component index
      IF ( oodps_infile(n)%odps%OComponent_Index /= oodps_infile(1)%odps%OComponent_Index ) THEN
        WRITE( err_msg, '("OComponent_Index is different for ",a)') oodps_infile(n)%filename
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      
    END IF Compatibility_Check


    ! Amass attributes for output
    oodps_outfile%title = TRIM(oodps_infile(n)%title)
    IF ( n == 1 ) THEN
      oodps_outfile%history = TRIM(oodps_infile(n)%filename)//' history: '//TRIM(oodps_infile(n)%history)
      oodps_outfile%comment = TRIM(oodps_infile(n)%filename)//' comment: '//TRIM(oodps_infile(n)%comment)
    ELSE
      oodps_outfile%history = TRIM(oodps_outfile%history)//'; '//&
                              TRIM(oodps_infile(n)%filename)//' history: '//TRIM(oodps_infile(n)%history)
      oodps_outfile%comment = TRIM(oodps_outfile%comment)//'; '//&
                              TRIM(oodps_infile(n)%filename)//' comment: '//TRIM(oodps_infile(n)%comment)
    END IF
    

    ! Generate and display the subset list
    CALL subset(n)%Generate( oodps_infile(n)%odps%Sensor_Channel, subset_list )
    IF ( .NOT. subset(n)%Is_Usable() ) THEN
      err_msg = 'Error generating channel subset for '//TRIM(input_filename)
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    END IF
    WRITE(*,*)
    msg = 'Channel subset information:'
    CALL Display_Message( PROGRAM_NAME, msg, INFORMATION )
    CALL subset(n)%Inspect()

    DEALLOCATE(subset_list)

  END DO Read_Loop


  ! Create the output object using dimensions of first input object
  WRITE(*,*)
  msg = 'Creating output object'
  CALL Display_Message( PROGRAM_NAME, msg, INFORMATION )
  CALL oodps_outfile%odps%Create( oodps_infile(1)%odps%n_Layers    , &
                                  oodps_infile(1)%odps%n_Components, &
                                  oodps_infile(1)%odps%n_Absorbers , &
                                  oodps_infile(1)%odps%n_Channels  , &
                                  oodps_infile(1)%odps%n_Coeffs    , &
                                  oodps_infile(1)%odps%n_OCoeffs     )
  IF ( .NOT. oodps_outfile%odps%Is_Usable() ) THEN
    err_msg = 'Error creating output oODPS object'
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF


  ! Populate the non-channel components
  ! ...File object
  oodps_outfile%filename       = TRIM(output_filename)
  oodps_outfile%profile_set_id = oodps_infile(1)%profile_set_id
  ! ...ODPS data
  oodps_outfile%odps%version            = version
  oodps_outfile%odps%Group_Index        = oodps_infile(1)%odps%Group_Index
  oodps_outfile%odps%WMO_Satellite_ID   = oodps_infile(1)%odps%WMO_Satellite_ID
  oodps_outfile%odps%WMO_Sensor_ID      = oodps_infile(1)%odps%WMO_Sensor_ID
  oodps_outfile%odps%Sensor_Id          = oodps_infile(1)%odps%Sensor_Id
  oodps_outfile%odps%Sensor_Type        = oodps_infile(1)%odps%Sensor_Type
  oodps_outfile%odps%Component_ID       = oodps_infile(1)%odps%Component_ID
  oodps_outfile%odps%Absorber_ID        = oodps_infile(1)%odps%Absorber_ID
  oodps_outfile%odps%Ref_Level_Pressure = oodps_infile(1)%odps%Ref_Level_Pressure
  oodps_outfile%odps%Ref_Pressure       = oodps_infile(1)%odps%Ref_Pressure
  oodps_outfile%odps%Ref_Temperature    = oodps_infile(1)%odps%Ref_Temperature
  oodps_outfile%odps%Ref_Absorber       = oodps_infile(1)%odps%Ref_Absorber
  oodps_outfile%odps%Min_Absorber       = oodps_infile(1)%odps%Min_Absorber
  oodps_outfile%odps%Max_Absorber       = oodps_infile(1)%odps%Max_Absorber
  ! ...Compact-OPTRAN data
  oodps_outfile%odps%Alpha            = oodps_infile(1)%odps%Alpha
  oodps_outfile%odps%Alpha_C1         = oodps_infile(1)%odps%Alpha_C1
  oodps_outfile%odps%Alpha_C2         = oodps_infile(1)%odps%Alpha_C2
  oodps_outfile%odps%OComponent_Index = oodps_infile(1)%odps%OComponent_Index


  ! Begin extraction and combination
  WRITE(*,*)
  msg = 'Extracting and combining file coefficients'
  CALL Display_Message( PROGRAM_NAME, msg, INFORMATION )


  ! Initialise counters for the combination output
  l1 = 1
  ls = 0
  n_out_coeffs = 0
  los = 0
  n_out_ocoeffs = 0


  ! Loop over files to extract data and combine in output
  Subset_Loop: DO n = 1, n_files


    ! Get the subset channel information
    CALL subset(n)%Get_Value( n_values = n_values, number = nmbr, index = idx )


    ! Copy the required channel's data for particular components
    l2 = l1 + n_values - 1
    oodps_outfile%odps%Sensor_Channel(l1:l2) = oodps_infile(n)%odps%Sensor_Channel(idx)
    ! ...ODPS data
    DO lch = 1, n_values
      ls = ls + 1
      DO j = 1, oodps_outfile%odps%n_Components
        np = oodps_infile(n)%odps%n_Predictors(j,idx(lch))
        oodps_outfile%odps%n_Predictors(j, ls) = np
        j0 = oodps_infile(n)%odps%Pos_Index(j,idx(lch))
        js = n_out_coeffs
        IF ( np > 0 ) THEN
          DO i = 1, np
            jp = j0+(i-1)*oodps_infile(n)%odps%n_Layers-1
            DO k = 1, oodps_infile(n)%odps%n_Layers
              n_out_coeffs = n_out_coeffs + 1
              oodps_outfile%odps%C(n_out_coeffs) = oodps_infile(n)%odps%C(jp+k)
            END DO
          END DO
          oodps_outfile%odps%Pos_Index(j,ls) = js+1
        ELSE
          oodps_outfile%odps%Pos_Index(j,ls) = 0
        END IF
      END DO
    END DO
    ! ...Compact-OPTRAN data
    oodps_outfile%odps%OSignificance(l1:l2) = oodps_infile(n)%odps%OSignificance(idx)
    oodps_outfile%odps%Order(l1:l2)         = oodps_infile(n)%odps%Order(idx)
    oodps_outfile%odps%OP_Index(:,l1:l2)    = oodps_infile(n)%odps%OP_Index(:, idx)

    DO lch = 1, n_values
      los      = los + 1
      np       = oodps_infile(n)%odps%OP_Index(0, idx(lch))
      n_orders = oodps_infile(n)%odps%Order(idx(lch))
      j0       = oodps_infile(n)%odps%OPos_Index(idx(lch))
      oodps_outfile%odps%OPos_Index(los) = n_out_ocoeffs + 1
      js = 0
      DO i = 0, np
        jp = j0 + i * (n_orders + 1)
        DO j = 0, n_orders
          js = js + 1
          oodps_outfile%odps%OC(n_out_ocoeffs + js) = oodps_infile(n)%odps%OC(jp + j)
        END DO
      END DO
      n_out_ocoeffs = n_out_ocoeffs + js
    END DO


    ! Increment main channel range counter
    l1 = l2 + 1

  END DO Subset_Loop


  ! Write the combined TauCoeff file
  WRITE(*,*)
  msg = 'Writing output file : '//TRIM(oodps_outfile%filename)
  CALL Display_Message( PROGRAM_NAME, msg, INFORMATION )
  err_stat = oodps_outfile%WriteFile(clobber=.true.)
  IF ( err_stat /= SUCCESS ) THEN
    err_msg = 'Error writing the netCDF ODPS file '//oodps_outfile%filename//' using new method'
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF

END PROGRAM ODPS_ExtractAndCombine


