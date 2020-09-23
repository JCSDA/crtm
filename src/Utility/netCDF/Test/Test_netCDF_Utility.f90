!
! Test_netCDF_Utility
!
! Program to test the netCDF utility modules.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Aug-2004
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_netCDF_Utility


  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds,      ONLY: Byte, Short, Long, Single, Double
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  USE netcdf
  USE netCDF_Utility
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Test_netCDF_Utility'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ), PARAMETER ::  NC_FILENAME = 'Test.netCDF.nc'


  ! --------------
  ! Dimension data
  ! --------------
  ! The maximum dimension
  INTEGER, PARAMETER :: N_DIMENSIONS = 7
  ! The dimension names
  CHARACTER( * ), PARAMETER :: DIM1_DIMNAME = 'n_Dim1'
  CHARACTER( * ), PARAMETER :: DIM2_DIMNAME = 'n_Dim2'
  CHARACTER( * ), PARAMETER :: DIM3_DIMNAME = 'n_Dim3'
  CHARACTER( * ), PARAMETER :: DIM4_DIMNAME = 'n_Dim4'
  CHARACTER( * ), PARAMETER :: DIM5_DIMNAME = 'n_Dim5'
  CHARACTER( * ), PARAMETER :: DIM6_DIMNAME = 'n_Dim6'
  CHARACTER( * ), PARAMETER :: DIM7_DIMNAME = 'n_Dim7'
  CHARACTER( * ), PARAMETER, DIMENSION( N_DIMENSIONS ) :: DIMNAME = (/ DIM1_DIMNAME, &
                                                                       DIM2_DIMNAME, &
                                                                       DIM3_DIMNAME, &
                                                                       DIM4_DIMNAME, &
                                                                       DIM5_DIMNAME, &
                                                                       DIM6_DIMNAME, &
                                                                       DIM7_DIMNAME /)
  ! The dimension values
  INTEGER, PARAMETER :: ID = 4
  INTEGER, PARAMETER :: N_DIM1 = ID
  INTEGER, PARAMETER :: N_DIM2 = N_DIM1
  INTEGER, PARAMETER :: N_DIM3 = N_DIM1
  INTEGER, PARAMETER :: N_DIM4 = N_DIM1
  INTEGER, PARAMETER :: N_DIM5 = N_DIM1
  INTEGER, PARAMETER :: N_DIM6 = N_DIM1
  INTEGER, PARAMETER :: N_DIM7 = N_DIM1
  INTEGER, PARAMETER, DIMENSION( N_DIMENSIONS ) :: DIMVALUE = (/ N_DIM1, &
                                                                 N_DIM2, &
                                                                 N_DIM3, &
                                                                 N_DIM4, &
                                                                 N_DIM5, &
                                                                 N_DIM6, &
                                                                 N_DIM7 /)
  ! The character variable string lengths
  CHARACTER( * ), PARAMETER :: SL_DIMNAME = 'sl'
  INTEGER,        PARAMETER :: SL = 4


  ! -------------------------------------
  ! The variable names and default values
  ! -------------------------------------
  ! Dummy variable for attribute tests
  CHARACTER( * ), PARAMETER :: DUMMY_VARNAME = 'Dummy'
  ! Character variables
  CHARACTER( * ), PARAMETER :: CHARS_VARNAME = 'CharS'
  CHARACTER( * ), PARAMETER :: CHAR1_VARNAME = 'Char1'
  CHARACTER( * ), PARAMETER :: CHAR2_VARNAME = 'Char2'
  CHARACTER( * ), PARAMETER :: CHAR3_VARNAME = 'Char3'
  CHARACTER( * ), PARAMETER :: CHAR4_VARNAME = 'Char4'
  CHARACTER( * ), PARAMETER :: CHAR5_VARNAME = 'Char5'
  CHARACTER( * ), PARAMETER :: CHAR6_VARNAME = 'Char6'
  CHARACTER( * ), PARAMETER :: CHAR7_VARNAME = 'Char7'
  CHARACTER( SL ), PARAMETER :: DEFAULT_CHAR = 'blah'
  ! Byte integer variables
  CHARACTER( * ), PARAMETER :: BYTES_VARNAME = 'ByteS'
  CHARACTER( * ), PARAMETER :: BYTE1_VARNAME = 'Byte1'
  CHARACTER( * ), PARAMETER :: BYTE2_VARNAME = 'Byte2'
  CHARACTER( * ), PARAMETER :: BYTE3_VARNAME = 'Byte3'
  CHARACTER( * ), PARAMETER :: BYTE4_VARNAME = 'Byte4'
  CHARACTER( * ), PARAMETER :: BYTE5_VARNAME = 'Byte5'
  CHARACTER( * ), PARAMETER :: BYTE6_VARNAME = 'Byte6'
  CHARACTER( * ), PARAMETER :: BYTE7_VARNAME = 'Byte7'
  INTEGER( Byte ), PARAMETER :: DEFAULT_BYTE = 73_Byte 
  ! Short integer variables
  CHARACTER( * ), PARAMETER :: SHORTS_VARNAME = 'ShortS'
  CHARACTER( * ), PARAMETER :: SHORT1_VARNAME = 'Short1'
  CHARACTER( * ), PARAMETER :: SHORT2_VARNAME = 'Short2'
  CHARACTER( * ), PARAMETER :: SHORT3_VARNAME = 'Short3'
  CHARACTER( * ), PARAMETER :: SHORT4_VARNAME = 'Short4'
  CHARACTER( * ), PARAMETER :: SHORT5_VARNAME = 'Short5'
  CHARACTER( * ), PARAMETER :: SHORT6_VARNAME = 'Short6'
  CHARACTER( * ), PARAMETER :: SHORT7_VARNAME = 'Short7'
  INTEGER( Short ), PARAMETER :: DEFAULT_SHORT = 137_Short
  ! Long integer variables
  CHARACTER( * ), PARAMETER :: LONGS_VARNAME = 'LongS'
  CHARACTER( * ), PARAMETER :: LONG1_VARNAME = 'Long1'
  CHARACTER( * ), PARAMETER :: LONG2_VARNAME = 'Long2'
  CHARACTER( * ), PARAMETER :: LONG3_VARNAME = 'Long3'
  CHARACTER( * ), PARAMETER :: LONG4_VARNAME = 'Long4'
  CHARACTER( * ), PARAMETER :: LONG5_VARNAME = 'Long5'
  CHARACTER( * ), PARAMETER :: LONG6_VARNAME = 'Long6'
  CHARACTER( * ), PARAMETER :: LONG7_VARNAME = 'Long7'
  INTEGER( Long ), PARAMETER :: DEFAULT_LONG = 531_Long
  ! Single integer variables
  CHARACTER( * ), PARAMETER :: SINGLES_VARNAME = 'SingleS'
  CHARACTER( * ), PARAMETER :: SINGLE1_VARNAME = 'Single1'
  CHARACTER( * ), PARAMETER :: SINGLE2_VARNAME = 'Single2'
  CHARACTER( * ), PARAMETER :: SINGLE3_VARNAME = 'Single3'
  CHARACTER( * ), PARAMETER :: SINGLE4_VARNAME = 'Single4'
  CHARACTER( * ), PARAMETER :: SINGLE5_VARNAME = 'Single5'
  CHARACTER( * ), PARAMETER :: SINGLE6_VARNAME = 'Single6'
  CHARACTER( * ), PARAMETER :: SINGLE7_VARNAME = 'Single7'
  REAL( Single ), PARAMETER :: DEFAULT_SINGLE = 137.0_Single
  ! Double integer variables
  CHARACTER( * ), PARAMETER :: DOUBLES_VARNAME = 'DoubleS'
  CHARACTER( * ), PARAMETER :: DOUBLE1_VARNAME = 'Double1'
  CHARACTER( * ), PARAMETER :: DOUBLE2_VARNAME = 'Double2'
  CHARACTER( * ), PARAMETER :: DOUBLE3_VARNAME = 'Double3'
  CHARACTER( * ), PARAMETER :: DOUBLE4_VARNAME = 'Double4'
  CHARACTER( * ), PARAMETER :: DOUBLE5_VARNAME = 'Double5'
  CHARACTER( * ), PARAMETER :: DOUBLE6_VARNAME = 'Double6'
  CHARACTER( * ), PARAMETER :: DOUBLE7_VARNAME = 'Double7'
  REAL( Double ), PARAMETER :: DEFAULT_DOUBLE = 753.0_Double


  ! ---------
  ! Variables
  ! ---------
  INTEGER :: n
  INTEGER :: NF90_Status
  INTEGER :: Error_Status
  INTEGER, DIMENSION( 11 ) :: Status
  INTEGER :: NC_FileID
  INTEGER, DIMENSION( N_DIMENSIONS ) :: DimID
  INTEGER :: SL_DimID
  INTEGER :: VarID

  CHARACTER( SL ) :: CharS
  CHARACTER( SL ) :: Char1(ID)
  CHARACTER( SL ) :: Char2(ID,ID)
  CHARACTER( SL ) :: Char3(ID,ID,ID)
  CHARACTER( SL ) :: Char4(ID,ID,ID,ID)
  CHARACTER( SL ) :: Char5(ID,ID,ID,ID,ID)
  CHARACTER( SL ) :: Char6(ID,ID,ID,ID,ID,ID)
  CHARACTER( SL ) :: Char7(ID,ID,ID,ID,ID,ID,ID)

  INTEGER( Byte ) :: ByteS
  INTEGER( Byte ) :: Byte1(ID)
  INTEGER( Byte ) :: Byte2(ID,ID)
  INTEGER( Byte ) :: Byte3(ID,ID,ID)
  INTEGER( Byte ) :: Byte4(ID,ID,ID,ID)
  INTEGER( Byte ) :: Byte5(ID,ID,ID,ID,ID)
  INTEGER( Byte ) :: Byte6(ID,ID,ID,ID,ID,ID)
  INTEGER( Byte ) :: Byte7(ID,ID,ID,ID,ID,ID,ID)

  INTEGER( Short ) :: ShortS
  INTEGER( Short ) :: Short1(ID)
  INTEGER( Short ) :: Short2(ID,ID)
  INTEGER( Short ) :: Short3(ID,ID,ID)
  INTEGER( Short ) :: Short4(ID,ID,ID,ID)
  INTEGER( Short ) :: Short5(ID,ID,ID,ID,ID)
  INTEGER( Short ) :: Short6(ID,ID,ID,ID,ID,ID)
  INTEGER( Short ) :: Short7(ID,ID,ID,ID,ID,ID,ID)

  INTEGER( Long ) :: LongS
  INTEGER( Long ) :: Long1(ID)
  INTEGER( Long ) :: Long2(ID,ID)
  INTEGER( Long ) :: Long3(ID,ID,ID)
  INTEGER( Long ) :: Long4(ID,ID,ID,ID)
  INTEGER( Long ) :: Long5(ID,ID,ID,ID,ID)
  INTEGER( Long ) :: Long6(ID,ID,ID,ID,ID,ID)
  INTEGER( Long ) :: Long7(ID,ID,ID,ID,ID,ID,ID)

  REAL( Single ) :: SingleS
  REAL( Single ) :: Single1(ID)
  REAL( Single ) :: Single2(ID,ID)
  REAL( Single ) :: Single3(ID,ID,ID)
  REAL( Single ) :: Single4(ID,ID,ID,ID)
  REAL( Single ) :: Single5(ID,ID,ID,ID,ID)
  REAL( Single ) :: Single6(ID,ID,ID,ID,ID,ID)
  REAL( Single ) :: Single7(ID,ID,ID,ID,ID,ID,ID)

  REAL( Double ) :: DoubleS
  REAL( Double ) :: Double1(ID)
  REAL( Double ) :: Double2(ID,ID)
  REAL( Double ) :: Double3(ID,ID,ID)
  REAL( Double ) :: Double4(ID,ID,ID,ID)
  REAL( Double ) :: Double5(ID,ID,ID,ID,ID)
  REAL( Double ) :: Double6(ID,ID,ID,ID,ID,ID)
  REAL( Double ) :: Double7(ID,ID,ID,ID,ID,ID,ID)


  ! ------
  ! Set up
  ! ------
  CharS = DEFAULT_CHAR
  Char1 = DEFAULT_CHAR
  Char2 = DEFAULT_CHAR
  Char3 = DEFAULT_CHAR
  Char4 = DEFAULT_CHAR
  Char5 = DEFAULT_CHAR
  Char6 = DEFAULT_CHAR
  Char7 = DEFAULT_CHAR

  ByteS = DEFAULT_BYTE
  Byte1 = DEFAULT_BYTE
  Byte2 = DEFAULT_BYTE
  Byte3 = DEFAULT_BYTE
  Byte4 = DEFAULT_BYTE
  Byte5 = DEFAULT_BYTE
  Byte6 = DEFAULT_BYTE
  Byte7 = DEFAULT_BYTE

  ShortS = DEFAULT_SHORT
  Short1 = DEFAULT_SHORT
  Short2 = DEFAULT_SHORT
  Short3 = DEFAULT_SHORT
  Short4 = DEFAULT_SHORT
  Short5 = DEFAULT_SHORT
  Short6 = DEFAULT_SHORT
  Short7 = DEFAULT_SHORT

  LongS = DEFAULT_LONG
  Long1 = DEFAULT_LONG
  Long2 = DEFAULT_LONG
  Long3 = DEFAULT_LONG
  Long4 = DEFAULT_LONG
  Long5 = DEFAULT_LONG
  Long6 = DEFAULT_LONG
  Long7 = DEFAULT_LONG

  SingleS = DEFAULT_SINGLE
  Single1 = DEFAULT_SINGLE
  Single2 = DEFAULT_SINGLE
  Single3 = DEFAULT_SINGLE
  Single4 = DEFAULT_SINGLE
  Single5 = DEFAULT_SINGLE
  Single6 = DEFAULT_SINGLE
  Single7 = DEFAULT_SINGLE

  DoubleS = DEFAULT_DOUBLE
  Double1 = DEFAULT_DOUBLE
  Double2 = DEFAULT_DOUBLE
  Double3 = DEFAULT_DOUBLE
  Double4 = DEFAULT_DOUBLE
  Double5 = DEFAULT_DOUBLE
  Double6 = DEFAULT_DOUBLE
  Double7 = DEFAULT_DOUBLE


  ! ---------------
  ! Create the file
  ! ---------------
  NF90_Status = NF90_CREATE( NC_Filename,  &
                             NF90_CLOBBER, &
                             NC_FileID     )
  IF ( NF90_Status /= NF90_NOERR ) THEN
    Error_Status = FAILURE
    CALL Display_Message( PROGRAM_NAME, &
                          'Error creating '//TRIM( NC_Filename )//' - '// &
                          TRIM( NF90_STRERROR( NF90_Status ) ), &
                          Error_Status )
    STOP
  END IF


  ! ---------------------
  ! Define the dimensions
  ! ---------------------
  ! The array dimensions
  DO n = 1, N_DIMENSIONS
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                DIMNAME(n), &
                                DIMVALUE(n), &
                                DimID(n) )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( PROGRAM_NAME, &
                            'Error defining the '//DIMNAME(n)//' dimension in '// &
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status )
      NF90_Status = NF90_CLOSE( NC_FileID )
      STOP
    END IF
  END DO

  ! The string length
  NF90_Status = NF90_DEF_DIM( NC_FileID, &
                              SL_DIMNAME, &
                              SL, &
                              SL_DimID )
  IF ( NF90_Status /= NF90_NOERR ) THEN
    Error_Status = FAILURE
    CALL Display_Message( PROGRAM_NAME, &
                          'Error defining the '//SL_DIMNAME//' dimension in '// &
                          TRIM( NC_FileNAME )//' - '// &
                          TRIM( NF90_STRERROR( NF90_Status ) ), &
                          Error_Status )
    NF90_Status = NF90_CLOSE( NC_FileID )
    STOP
  END IF


  ! ---------------------------
  ! Test the attribute routines
  ! ---------------------------
  ! Define a dummy variable
  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              DUMMY_VARNAME, &
                              NF90_INT, &
                              VarID = VarID )

  ! Test the Attribute PUT routines
  Status = SUCCESS
  Status(1) = Put_netCDF_Attribute( NC_FileID, &
                                    CHARS_VARNAME, &
                                    CharS, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(2) = Put_netCDF_Attribute( NC_FileID, &
                                    BYTES_VARNAME, &
                                    ByteS, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(3) = Put_netCDF_Attribute( NC_FileID, &
                                    SHORTS_VARNAME, &
                                    ShortS, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(4) = Put_netCDF_Attribute( NC_FileID, &
                                    LONGS_VARNAME, &
                                    LongS, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(5) = Put_netCDF_Attribute( NC_FileID, &
                                    SINGLES_VARNAME, &
                                    SingleS, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(6) = Put_netCDF_Attribute( NC_FileID, &
                                    DOUBLES_VARNAME, &
                                    DoubleS, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(7) = Put_netCDF_Attribute( NC_FileID, &
                                    BYTE1_VARNAME, &
                                    Byte1, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(8) = Put_netCDF_Attribute( NC_FileID, &
                                    SHORT1_VARNAME, &
                                    Short1, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(9) = Put_netCDF_Attribute( NC_FileID, &
                                    LONG1_VARNAME, &
                                    Long1, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(10) = Put_netCDF_Attribute( NC_FileID, &
                                     SINGLE1_VARNAME, &
                                     Single1, &
                                     Variable_Name = DUMMY_VARNAME )
  Status(11) = Put_netCDF_Attribute( NC_FileID, &
                                     DOUBLE1_VARNAME, &
                                     Double1, &
                                     Variable_Name = DUMMY_VARNAME )
  IF ( ANY( Status /= SUCCESS ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error testing Put_netCDF_Attribute routines', &
                          FAILURE )
    NF90_Status = NF90_CLOSE( NC_FileID )
    STOP
  ELSE
    CALL Display_Message( 'Put_netCDF_Attribute', &
                          'Test successful', &
                          SUCCESS )
  END IF

  ! Redefine the attribute variables
  CharS = 'XXXX'
  ByteS = -1_Byte
  Byte1 = -1_Byte
  ShortS = -1_Short
  Short1 = -1_Short
  LongS = -1_Long
  Long1 = -1_Long
  SingleS = -1.0_Single
  Single1 = -1.0_Single
  DoubleS = -1.0_Double
  Double1 = -1.0_Double

  ! Test the Attribute GET routines
  Status = SUCCESS
  Status(1) = Get_netCDF_Attribute( NC_FileID, &
                                    CHARS_VARNAME, &
                                    CharS, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(2) = Get_netCDF_Attribute( NC_FileID, &
                                    BYTES_VARNAME, &
                                    ByteS, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(3) = Get_netCDF_Attribute( NC_FileID, &
                                    SHORTS_VARNAME, &
                                    ShortS, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(4) = Get_netCDF_Attribute( NC_FileID, &
                                    LONGS_VARNAME, &
                                    LongS, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(5) = Get_netCDF_Attribute( NC_FileID, &
                                    SINGLES_VARNAME, &
                                    SingleS, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(6) = Get_netCDF_Attribute( NC_FileID, &
                                    DOUBLES_VARNAME, &
                                    DoubleS, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(7) = Get_netCDF_Attribute( NC_FileID, &
                                    BYTE1_VARNAME, &
                                    Byte1, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(8) = Get_netCDF_Attribute( NC_FileID, &
                                    SHORT1_VARNAME, &
                                    Short1, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(9) = Get_netCDF_Attribute( NC_FileID, &
                                    LONG1_VARNAME, &
                                    Long1, &
                                    Variable_Name = DUMMY_VARNAME )
  Status(10) = Get_netCDF_Attribute( NC_FileID, &
                                     SINGLE1_VARNAME, &
                                     Single1, &
                                     Variable_Name = DUMMY_VARNAME )
  Status(11) = Get_netCDF_Attribute( NC_FileID, &
                                     DOUBLE1_VARNAME, &
                                     Double1, &
                                     Variable_Name = DUMMY_VARNAME )
  IF ( ANY( Status /= SUCCESS ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error testing Get_netCDF_Attribute routines', &
                          FAILURE )
    NF90_Status = NF90_CLOSE( NC_FileID )
    STOP
  END IF

  IF ( CharS   /= DEFAULT_CHAR   .OR. &
       ByteS   /= DEFAULT_BYTE   .OR. &
       ShortS  /= DEFAULT_SHORT  .OR. &
       LongS   /= DEFAULT_LONG   .OR. &
       SingleS /= DEFAULT_SINGLE .OR. &
       DoubleS /= DEFAULT_DOUBLE .OR. &
       ANY( Byte1   /= DEFAULT_BYTE   ) .OR. &
       ANY( Short1  /= DEFAULT_SHORT  ) .OR. &
       ANY( Long1   /= DEFAULT_LONG   ) .OR. &
       ANY( Single1 /= DEFAULT_SINGLE ) .OR. &
       ANY( Double1 /= DEFAULT_DOUBLE )      ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading variable attributes', &
                          FAILURE )
    STOP
  ELSE
    CALL Display_Message( 'Get_netCDF_Attribute', &
                          'Test successful', &
                          SUCCESS )
  END IF

  WRITE( *, * )


  ! ------------------------------
  ! Define the character variables
  ! ------------------------------
  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              CHARS_VARNAME, &
                              NF90_CHAR, &
                              DimIDs = SL_DimID, &
                              VarID = VarID )


  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              CHAR1_VARNAME, &
                              NF90_CHAR, &
                              DimIDs = (/ SL_DimID, &
                                          DimID(1) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              CHAR2_VARNAME, &
                              NF90_CHAR, &
                              DimIDs = (/ SL_DimID, &
                                          DimID(1), &
                                          DimID(2) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              CHAR3_VARNAME, &
                              NF90_CHAR, &
                              DimIDs = (/ SL_DimID, &
                                          DimID(1), &
                                          DimID(2), &
                                          DimID(3) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              CHAR4_VARNAME, &
                              NF90_CHAR, &
                              DimIDs = (/ SL_DimID, &
                                          DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              CHAR5_VARNAME, &
                              NF90_CHAR, &
                              DimIDs = (/ SL_DimID, &
                                          DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              CHAR6_VARNAME, &
                              NF90_CHAR, &
                              DimIDs = (/ SL_DimID, &
                                          DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5), &
                                          DimID(6) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              CHAR7_VARNAME, &
                              NF90_CHAR, &
                              DimIDs = (/ SL_DimID, &
                                          DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5), &
                                          DimID(6), &
                                          DimID(7) /), &
                              VarID = VarID )
                             

  NF90_Status = NF90_ENDDEF( NC_FileID )


  ! -----------------------------
  ! Write the character variables
  ! -----------------------------
  Status = SUCCESS

  Status(1) = Put_netCDF_Variable( NC_FileID, &
                                   CHARS_VARNAME, &
                                   CharS )
           
  Status(2) = Put_netCDF_Variable( NC_FileID, &
                                   CHAR1_VARNAME, &
                                   Char1 )
           
  Status(3) = Put_netCDF_Variable( NC_FileID, &
                                   CHAR2_VARNAME, &
                                   Char2 )
           
  Status(4) = Put_netCDF_Variable( NC_FileID, &
                                   CHAR3_VARNAME, &
                                   Char3 )
           
  Status(5) = Put_netCDF_Variable( NC_FileID, &
                                   CHAR4_VARNAME, &
                                   Char4 )
           
  Status(6) = Put_netCDF_Variable( NC_FileID, &
                                   CHAR5_VARNAME, &
                                   Char5 )
           
  Status(7) = Put_netCDF_Variable( NC_FileID, &
                                   CHAR6_VARNAME, &
                                   Char6 )
           
  Status(8) = Put_netCDF_Variable( NC_FileID, &
                                   CHAR7_VARNAME, &
                                   Char7 )

  IF ( ANY( Status /= SUCCESS ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error testing Put_netCDF_Variable CHAR routines', &
                          FAILURE )
    NF90_Status = NF90_CLOSE( NC_FileID )
    STOP
  ELSE
    CALL Display_Message( 'CHAR   Put_netCDF_Variable', &
                          'Test successful', &
                          SUCCESS )
  END IF

  NF90_Status = NF90_REDEF( NC_FileID )


  ! ----------------------------------
  ! Define the integer(byte) variables
  ! ----------------------------------
  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              BYTES_VARNAME, &
                              NF90_BYTE, &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              BYTE1_VARNAME, &
                              NF90_BYTE, &
                              DimIDs = (/ DimID(1) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              BYTE2_VARNAME, &
                              NF90_BYTE, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              BYTE3_VARNAME, &
                              NF90_BYTE, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              BYTE4_VARNAME, &
                              NF90_BYTE, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              BYTE5_VARNAME, &
                              NF90_BYTE, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              BYTE6_VARNAME, &
                              NF90_BYTE, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5), &
                                          DimID(6) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              BYTE7_VARNAME, &
                              NF90_BYTE, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5), &
                                          DimID(6), &
                                          DimID(7) /), &
                              VarID = VarID )

  NF90_Status = NF90_ENDDEF( NC_FileID )


  ! ---------------------------------
  ! Write the integer(byte) variables
  ! ---------------------------------
  Status = SUCCESS

  Status(1) = Put_netCDF_Variable( NC_FileID, &
                                   BYTES_VARNAME, &
                                   ByteS )
           
  Status(2) = Put_netCDF_Variable( NC_FileID, &
                                   BYTE1_VARNAME, &
                                   Byte1 )
           
  Status(3) = Put_netCDF_Variable( NC_FileID, &
                                   BYTE2_VARNAME, &
                                   Byte2 )
           
  Status(4) = Put_netCDF_Variable( NC_FileID, &
                                   BYTE3_VARNAME, &
                                   Byte3 )
           
  Status(5) = Put_netCDF_Variable( NC_FileID, &
                                   BYTE4_VARNAME, &
                                   Byte4 )
           
  Status(6) = Put_netCDF_Variable( NC_FileID, &
                                   BYTE5_VARNAME, &
                                   Byte5 )
           
  Status(7) = Put_netCDF_Variable( NC_FileID, &
                                   BYTE6_VARNAME, &
                                   Byte6 )
           
  Status(8) = Put_netCDF_Variable( NC_FileID, &
                                   BYTE7_VARNAME, &
                                   Byte7 )

  IF ( ANY( Status /= SUCCESS ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error testing Put_netCDF_Variable BYTE routines', &
                          FAILURE )
    NF90_Status = NF90_CLOSE( NC_FileID )
    STOP
  ELSE
    CALL Display_Message( 'BYTE   Put_netCDF_Variable', &
                          'Test successful', &
                          SUCCESS )
  END IF

  NF90_Status = NF90_REDEF( NC_FileID )


  ! -----------------------------------
  ! Define the integer(short) variables
  ! -----------------------------------
  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              SHORTS_VARNAME, &
                              NF90_SHORT, &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              SHORT1_VARNAME, &
                              NF90_SHORT, &
                              DimIDs = (/ DimID(1) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              SHORT2_VARNAME, &
                              NF90_SHORT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              SHORT3_VARNAME, &
                              NF90_SHORT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              SHORT4_VARNAME, &
                              NF90_SHORT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              SHORT5_VARNAME, &
                              NF90_SHORT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              SHORT6_VARNAME, &
                              NF90_SHORT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5), &
                                          DimID(6) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              SHORT7_VARNAME, &
                              NF90_SHORT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5), &
                                          DimID(6), &
                                          DimID(7) /), &
                              VarID = VarID )
                             
  NF90_Status = NF90_ENDDEF( NC_FileID )


  ! ----------------------------------
  ! Write the integer(short) variables
  ! ----------------------------------
  Status = SUCCESS

  Status(1) = Put_netCDF_Variable( NC_FileID, &
                                   SHORTS_VARNAME, &
                                   ShortS )
           
  Status(2) = Put_netCDF_Variable( NC_FileID, &
                                   SHORT1_VARNAME, &
                                   Short1 )
           
  Status(3) = Put_netCDF_Variable( NC_FileID, &
                                   SHORT2_VARNAME, &
                                   Short2 )
           
  Status(4) = Put_netCDF_Variable( NC_FileID, &
                                   SHORT3_VARNAME, &
                                   Short3 )
           
  Status(5) = Put_netCDF_Variable( NC_FileID, &
                                   SHORT4_VARNAME, &
                                   Short4 )
           
  Status(6) = Put_netCDF_Variable( NC_FileID, &
                                   SHORT5_VARNAME, &
                                   Short5 )
           
  Status(7) = Put_netCDF_Variable( NC_FileID, &
                                   SHORT6_VARNAME, &
                                   Short6 )
           
  Status(8) = Put_netCDF_Variable( NC_FileID, &
                                   SHORT7_VARNAME, &
                                   Short7 )

  IF ( ANY( Status /= SUCCESS ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error testing Put_netCDF_Variable SHORT routines', &
                          FAILURE )
    NF90_Status = NF90_CLOSE( NC_FileID )
    STOP
  ELSE
    CALL Display_Message( 'SHORT  Put_netCDF_Variable', &
                          'Test successful', &
                          SUCCESS )
  END IF

  NF90_Status = NF90_REDEF( NC_FileID )


  ! ----------------------------------
  ! Define the integer(long) variables
  ! ----------------------------------
  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              LONGS_VARNAME, &
                              NF90_INT, &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              LONG1_VARNAME, &
                              NF90_INT, &
                              DimIDs = (/ DimID(1) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              LONG2_VARNAME, &
                              NF90_INT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              LONG3_VARNAME, &
                              NF90_INT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              LONG4_VARNAME, &
                              NF90_INT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              LONG5_VARNAME, &
                              NF90_INT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              LONG6_VARNAME, &
                              NF90_INT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5), &
                                          DimID(6) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              LONG7_VARNAME, &
                              NF90_INT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5), &
                                          DimID(6), &
                                          DimID(7) /), &
                              VarID = VarID )
                             
  NF90_Status = NF90_ENDDEF( NC_FileID )


  ! ---------------------------------
  ! Write the integer(long) variables
  ! ---------------------------------
  Status = SUCCESS

  Status(1) = Put_netCDF_Variable( NC_FileID, &
                                   LONGS_VARNAME, &
                                   LongS )
           
  Status(2) = Put_netCDF_Variable( NC_FileID, &
                                   LONG1_VARNAME, &
                                   Long1 )
           
  Status(3) = Put_netCDF_Variable( NC_FileID, &
                                   LONG2_VARNAME, &
                                   Long2 )
           
  Status(4) = Put_netCDF_Variable( NC_FileID, &
                                   LONG3_VARNAME, &
                                   Long3 )
           
  Status(5) = Put_netCDF_Variable( NC_FileID, &
                                   LONG4_VARNAME, &
                                   Long4 )
           
  Status(6) = Put_netCDF_Variable( NC_FileID, &
                                   LONG5_VARNAME, &
                                   Long5 )
           
  Status(7) = Put_netCDF_Variable( NC_FileID, &
                                   LONG6_VARNAME, &
                                   Long6 )
           
  Status(8) = Put_netCDF_Variable( NC_FileID, &
                                      LONG7_VARNAME, &
                                      Long7 )

  IF ( ANY( Status /= SUCCESS ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error testing Put_netCDF_Variable LONG routines', &
                          FAILURE )
    NF90_Status = NF90_CLOSE( NC_FileID )
    STOP
  ELSE
    CALL Display_Message( 'LONG   Put_netCDF_Variable', &
                          'Test successful', &
                          SUCCESS )
  END IF

  NF90_Status = NF90_REDEF( NC_FileID )


  ! ---------------------------------
  ! Define the real(single) variables
  ! ---------------------------------
  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              SINGLES_VARNAME, &
                              NF90_FLOAT, &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              SINGLE1_VARNAME, &
                              NF90_FLOAT, &
                              DimIDs = (/ DimID(1) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              SINGLE2_VARNAME, &
                              NF90_FLOAT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              SINGLE3_VARNAME, &
                              NF90_FLOAT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              SINGLE4_VARNAME, &
                              NF90_FLOAT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              SINGLE5_VARNAME, &
                              NF90_FLOAT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              SINGLE6_VARNAME, &
                              NF90_FLOAT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5), &
                                          DimID(6) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              SINGLE7_VARNAME, &
                              NF90_FLOAT, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5), &
                                          DimID(6), &
                                          DimID(7) /), &
                              VarID = VarID )
                             
  NF90_Status = NF90_ENDDEF( NC_FileID )


  ! --------------------------------
  ! Write the real(single) variables
  ! --------------------------------
  Status = SUCCESS

  Status(1) = Put_netCDF_Variable( NC_FileID, &
                                   SINGLES_VARNAME, &
                                   SingleS )
           
  Status(2) = Put_netCDF_Variable( NC_FileID, &
                                   SINGLE1_VARNAME, &
                                   Single1 )
           
  Status(3) = Put_netCDF_Variable( NC_FileID, &
                                   SINGLE2_VARNAME, &
                                   Single2 )
           
  Status(4) = Put_netCDF_Variable( NC_FileID, &
                                   SINGLE3_VARNAME, &
                                   Single3 )
           
  Status(5) = Put_netCDF_Variable( NC_FileID, &
                                   SINGLE4_VARNAME, &
                                   Single4 )
           
  Status(6) = Put_netCDF_Variable( NC_FileID, &
                                   SINGLE5_VARNAME, &
                                   Single5 )
           
  Status(7) = Put_netCDF_Variable( NC_FileID, &
                                   SINGLE6_VARNAME, &
                                   Single6 )
           
  Status(8) = Put_netCDF_Variable( NC_FileID, &
                                      SINGLE7_VARNAME, &
                                      Single7 )

  IF ( ANY( Status /= SUCCESS ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error testing Put_netCDF_Variable SINGLE routines', &
                          FAILURE )
    NF90_Status = NF90_CLOSE( NC_FileID )
    STOP
  ELSE
    CALL Display_Message( 'SINGLE Put_netCDF_Variable', &
                          'Test successful', &
                          SUCCESS )
  END IF

  NF90_Status = NF90_REDEF( NC_FileID )


  ! ---------------------------------
  ! Define the real(double) variables
  ! ---------------------------------
  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              DOUBLES_VARNAME, &
                              NF90_DOUBLE, &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              DOUBLE1_VARNAME, &
                              NF90_DOUBLE, &
                              DimIDs = (/ DimID(1) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              DOUBLE2_VARNAME, &
                              NF90_DOUBLE, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              DOUBLE3_VARNAME, &
                              NF90_DOUBLE, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              DOUBLE4_VARNAME, &
                              NF90_DOUBLE, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              DOUBLE5_VARNAME, &
                              NF90_DOUBLE, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              DOUBLE6_VARNAME, &
                              NF90_DOUBLE, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5), &
                                          DimID(6) /), &
                              VarID = VarID )

  NF90_Status = NF90_DEF_VAR( NC_FileID, &
                              DOUBLE7_VARNAME, &
                              NF90_DOUBLE, &
                              DimIDs = (/ DimID(1), &
                                          DimID(2), &
                                          DimID(3), &
                                          DimID(4), &
                                          DimID(5), &
                                          DimID(6), &
                                          DimID(7) /), &
                              VarID = VarID )
                             
  NF90_Status = NF90_ENDDEF( NC_FileID )


  ! --------------------------------
  ! Write the real(double) variables
  ! --------------------------------
  Status = SUCCESS

  Status(1) = Put_netCDF_Variable( NC_FileID, &
                                   DOUBLES_VARNAME, &
                                   DoubleS )
           
  Status(2) = Put_netCDF_Variable( NC_FileID, &
                                   DOUBLE1_VARNAME, &
                                   Double1 )
           
  Status(3) = Put_netCDF_Variable( NC_FileID, &
                                   DOUBLE2_VARNAME, &
                                   Double2 )
           
  Status(4) = Put_netCDF_Variable( NC_FileID, &
                                   DOUBLE3_VARNAME, &
                                   Double3 )
           
  Status(5) = Put_netCDF_Variable( NC_FileID, &
                                   DOUBLE4_VARNAME, &
                                   Double4 )
           
  Status(6) = Put_netCDF_Variable( NC_FileID, &
                                   DOUBLE5_VARNAME, &
                                   Double5 )
           
  Status(7) = Put_netCDF_Variable( NC_FileID, &
                                   DOUBLE6_VARNAME, &
                                   Double6 )
           
  Status(8) = Put_netCDF_Variable( NC_FileID, &
                                   DOUBLE7_VARNAME, &
                                   Double7 )

  IF ( ANY( Status /= SUCCESS ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error testing Put_netCDF_Variable DOUBLE routines', &
                          FAILURE )
    NF90_Status = NF90_CLOSE( NC_FileID )
    STOP
  ELSE
    CALL Display_Message( 'DOUBLE Put_netCDF_Variable', &
                          'Test successful', &
                          SUCCESS )
  END IF

  WRITE( *, * )


  ! ----------------------
  ! Redefine the variables
  ! ----------------------
  CharS = 'XXXX'
  Char1 = 'XXXX'
  Char2 = 'XXXX'
  Char3 = 'XXXX'
  Char4 = 'XXXX'
  Char5 = 'XXXX'
  Char6 = 'XXXX'
  Char7 = 'XXXX'

  ByteS = -1_Byte
  Byte1 = -1_Byte
  Byte2 = -1_Byte
  Byte3 = -1_Byte
  Byte4 = -1_Byte
  Byte5 = -1_Byte
  Byte6 = -1_Byte
  Byte7 = -1_Byte

  ShortS = -1_Short
  Short1 = -1_Short
  Short2 = -1_Short
  Short3 = -1_Short
  Short4 = -1_Short
  Short5 = -1_Short
  Short6 = -1_Short
  Short7 = -1_Short

  LongS = -1_Long
  Long1 = -1_Long
  Long2 = -1_Long
  Long3 = -1_Long
  Long4 = -1_Long
  Long5 = -1_Long
  Long6 = -1_Long
  Long7 = -1_Long

  SingleS = -1.0_Single
  Single1 = -1.0_Single
  Single2 = -1.0_Single
  Single3 = -1.0_Single
  Single4 = -1.0_Single
  Single5 = -1.0_Single
  Single6 = -1.0_Single
  Single7 = -1.0_Single

  DoubleS = -1.0_Double
  Double1 = -1.0_Double
  Double2 = -1.0_Double
  Double3 = -1.0_Double
  Double4 = -1.0_Double
  Double5 = -1.0_Double
  Double6 = -1.0_Double
  Double7 = -1.0_Double


  ! ----------------------------
  ! Read the character variables
  ! ----------------------------
  Status = SUCCESS

  Status(1) = Get_netCDF_Variable( NC_FileID, &
                                   CHARS_VARNAME, &
                                   CharS )
           
  Status(2) = Get_netCDF_Variable( NC_FileID, &
                                   CHAR1_VARNAME, &
                                   Char1 )
           
  Status(3) = Get_netCDF_Variable( NC_FileID, &
                                   CHAR2_VARNAME, &
                                   Char2 )
           
  Status(4) = Get_netCDF_Variable( NC_FileID, &
                                   CHAR3_VARNAME, &
                                   Char3 )
           
  Status(5) = Get_netCDF_Variable( NC_FileID, &
                                   CHAR4_VARNAME, &
                                   Char4 )
           
  Status(6) = Get_netCDF_Variable( NC_FileID, &
                                   CHAR5_VARNAME, &
                                   Char5 )
           
  Status(7) = Get_netCDF_Variable( NC_FileID, &
                                   CHAR6_VARNAME, &
                                   Char6 )
           
  Status(8) = Get_netCDF_Variable( NC_FileID, &
                                   CHAR7_VARNAME, &
                                   Char7 )

  IF ( ANY( Status /= SUCCESS ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error testing Get_netCDF_Variable CHAR routines', &
                          FAILURE )
    STOP
  END IF

  IF ( Chars /= DEFAULT_CHAR .OR. &
       ANY( Char1 /= DEFAULT_CHAR ) .OR. &
       ANY( Char2 /= DEFAULT_CHAR ) .OR. &
       ANY( Char3 /= DEFAULT_CHAR ) .OR. &
       ANY( Char4 /= DEFAULT_CHAR ) .OR. &
       ANY( Char5 /= DEFAULT_CHAR ) .OR. &
       ANY( Char6 /= DEFAULT_CHAR ) .OR. &
       ANY( Char7 /= DEFAULT_CHAR )      ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading character variables', &
                          FAILURE )
    STOP
  ELSE
    CALL Display_Message( 'CHAR   Get_netCDF_Variable', &
                          'Test successful', &
                          SUCCESS )
  END IF


  ! --------------------------------
  ! Read the integer(byte) variables
  ! --------------------------------
  Status = SUCCESS

  Status(1) = Get_netCDF_Variable( NC_FileID, &
                                   BYTES_VARNAME, &
                                   ByteS )
           
  Status(2) = Get_netCDF_Variable( NC_FileID, &
                                   BYTE1_VARNAME, &
                                   Byte1 )
           
  Status(3) = Get_netCDF_Variable( NC_FileID, &
                                   BYTE2_VARNAME, &
                                   Byte2 )
           
  Status(4) = Get_netCDF_Variable( NC_FileID, &
                                   BYTE3_VARNAME, &
                                   Byte3 )
           
  Status(5) = Get_netCDF_Variable( NC_FileID, &
                                   BYTE4_VARNAME, &
                                   Byte4 )
           
  Status(6) = Get_netCDF_Variable( NC_FileID, &
                                   BYTE5_VARNAME, &
                                   Byte5 )
           
  Status(7) = Get_netCDF_Variable( NC_FileID, &
                                   BYTE6_VARNAME, &
                                   Byte6 )
           
  Status(8) = Get_netCDF_Variable( NC_FileID, &
                                   BYTE7_VARNAME, &
                                   Byte7 )

  IF ( ANY( Status /= SUCCESS ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error testing Get_netCDF_Variable BYTE routines', &
                          FAILURE )
    STOP
  END IF

  IF ( Bytes /= DEFAULT_BYTE .OR. &
       ANY( Byte1 /= DEFAULT_BYTE ) .OR. &
       ANY( Byte2 /= DEFAULT_BYTE ) .OR. &
       ANY( Byte3 /= DEFAULT_BYTE ) .OR. &
       ANY( Byte4 /= DEFAULT_BYTE ) .OR. &
       ANY( Byte5 /= DEFAULT_BYTE ) .OR. &
       ANY( Byte6 /= DEFAULT_BYTE ) .OR. &
       ANY( Byte7 /= DEFAULT_BYTE )      ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Byte variables', &
                          FAILURE )
    STOP
  ELSE
    CALL Display_Message( 'BYTE   Get_netCDF_Variable', &
                          'Test successful', &
                          SUCCESS )
  END IF


  ! ---------------------------------
  ! Read the integer(short) variables
  ! ---------------------------------
  Status = SUCCESS

  Status(1) = Get_netCDF_Variable( NC_FileID, &
                                   SHORTS_VARNAME, &
                                   ShortS )
           
  Status(2) = Get_netCDF_Variable( NC_FileID, &
                                   SHORT1_VARNAME, &
                                   Short1 )
           
  Status(3) = Get_netCDF_Variable( NC_FileID, &
                                   SHORT2_VARNAME, &
                                   Short2 )
           
  Status(4) = Get_netCDF_Variable( NC_FileID, &
                                   SHORT3_VARNAME, &
                                   Short3 )
           
  Status(5) = Get_netCDF_Variable( NC_FileID, &
                                   SHORT4_VARNAME, &
                                   Short4 )
           
  Status(6) = Get_netCDF_Variable( NC_FileID, &
                                   SHORT5_VARNAME, &
                                   Short5 )
           
  Status(7) = Get_netCDF_Variable( NC_FileID, &
                                   SHORT6_VARNAME, &
                                   Short6 )
           
  Status(8) = Get_netCDF_Variable( NC_FileID, &
                                   SHORT7_VARNAME, &
                                   Short7 )

  IF ( ANY( Status /= SUCCESS ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error testing Get_netCDF_Variable SHORT routines', &
                          FAILURE )
    STOP
  END IF

  IF ( Shorts /= DEFAULT_SHORT .OR. &
       ANY( Short1 /= DEFAULT_SHORT ) .OR. &
       ANY( Short2 /= DEFAULT_SHORT ) .OR. &
       ANY( Short3 /= DEFAULT_SHORT ) .OR. &
       ANY( Short4 /= DEFAULT_SHORT ) .OR. &
       ANY( Short5 /= DEFAULT_SHORT ) .OR. &
       ANY( Short6 /= DEFAULT_SHORT ) .OR. &
       ANY( Short7 /= DEFAULT_SHORT )      ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Short variables', &
                          FAILURE )
    STOP
  ELSE
    CALL Display_Message( 'SHORT  Get_netCDF_Variable', &
                          'Test successful', &
                          SUCCESS )
  END IF


  ! --------------------------------
  ! Read the integer(long) variables
  ! --------------------------------
  Status = SUCCESS

  Status(1) = Get_netCDF_Variable( NC_FileID, &
                                   LONGS_VARNAME, &
                                   LongS )
           
  Status(2) = Get_netCDF_Variable( NC_FileID, &
                                   LONG1_VARNAME, &
                                   Long1 )
           
  Status(3) = Get_netCDF_Variable( NC_FileID, &
                                   LONG2_VARNAME, &
                                   Long2 )
           
  Status(4) = Get_netCDF_Variable( NC_FileID, &
                                   LONG3_VARNAME, &
                                   Long3 )
           
  Status(5) = Get_netCDF_Variable( NC_FileID, &
                                   LONG4_VARNAME, &
                                   Long4 )
           
  Status(6) = Get_netCDF_Variable( NC_FileID, &
                                   LONG5_VARNAME, &
                                   Long5 )
           
  Status(7) = Get_netCDF_Variable( NC_FileID, &
                                   LONG6_VARNAME, &
                                   Long6 )
           
  Status(8) = Get_netCDF_Variable( NC_FileID, &
                                   LONG7_VARNAME, &
                                   Long7 )

  IF ( ANY( Status /= SUCCESS ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error testing Get_netCDF_Variable LONG routines', &
                          FAILURE )
    STOP
  END IF

  IF ( Longs /= DEFAULT_LONG .OR. &
       ANY( Long1 /= DEFAULT_LONG ) .OR. &
       ANY( Long2 /= DEFAULT_LONG ) .OR. &
       ANY( Long3 /= DEFAULT_LONG ) .OR. &
       ANY( Long4 /= DEFAULT_LONG ) .OR. &
       ANY( Long5 /= DEFAULT_LONG ) .OR. &
       ANY( Long6 /= DEFAULT_LONG ) .OR. &
       ANY( Long7 /= DEFAULT_LONG )      ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Long variables', &
                          FAILURE )
    STOP
  ELSE
    CALL Display_Message( 'LONG   Get_netCDF_Variable', &
                          'Test successful', &
                          SUCCESS )
  END IF


  ! -------------------------------
  ! Read the real(single) variables
  ! -------------------------------
  Status = SUCCESS

  Status(1) = Get_netCDF_Variable( NC_FileID, &
                                   SINGLES_VARNAME, &
                                   SingleS )
           
  Status(2) = Get_netCDF_Variable( NC_FileID, &
                                   SINGLE1_VARNAME, &
                                   Single1 )
           
  Status(3) = Get_netCDF_Variable( NC_FileID, &
                                   SINGLE2_VARNAME, &
                                   Single2 )
           
  Status(4) = Get_netCDF_Variable( NC_FileID, &
                                   SINGLE3_VARNAME, &
                                   Single3 )
           
  Status(5) = Get_netCDF_Variable( NC_FileID, &
                                   SINGLE4_VARNAME, &
                                   Single4 )
           
  Status(6) = Get_netCDF_Variable( NC_FileID, &
                                   SINGLE5_VARNAME, &
                                   Single5 )
           
  Status(7) = Get_netCDF_Variable( NC_FileID, &
                                   SINGLE6_VARNAME, &
                                   Single6 )
           
  Status(8) = Get_netCDF_Variable( NC_FileID, &
                                   SINGLE7_VARNAME, &
                                   Single7 )

  IF ( ANY( Status /= SUCCESS ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error testing Get_netCDF_Variable SINGLE routines', &
                          FAILURE )
    STOP
  END IF

  IF ( Singles /= DEFAULT_SINGLE .OR. &
       ANY( Single1 /= DEFAULT_SINGLE ) .OR. &
       ANY( Single2 /= DEFAULT_SINGLE ) .OR. &
       ANY( Single3 /= DEFAULT_SINGLE ) .OR. &
       ANY( Single4 /= DEFAULT_SINGLE ) .OR. &
       ANY( Single5 /= DEFAULT_SINGLE ) .OR. &
       ANY( Single6 /= DEFAULT_SINGLE ) .OR. &
       ANY( Single7 /= DEFAULT_SINGLE )      ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Single variables', &
                          FAILURE )
    STOP
  ELSE
    CALL Display_Message( 'SINGLE Get_netCDF_Variable', &
                          'Test successful', &
                          SUCCESS )
  END IF


  ! -------------------------------
  ! Read the real(double) variables
  ! -------------------------------
  Status = SUCCESS

  Status(1) = Get_netCDF_Variable( NC_FileID, &
                                   DOUBLES_VARNAME, &
                                   DoubleS )
           
  Status(2) = Get_netCDF_Variable( NC_FileID, &
                                   DOUBLE1_VARNAME, &
                                   Double1 )
           
  Status(3) = Get_netCDF_Variable( NC_FileID, &
                                   DOUBLE2_VARNAME, &
                                   Double2 )
           
  Status(4) = Get_netCDF_Variable( NC_FileID, &
                                   DOUBLE3_VARNAME, &
                                   Double3 )
           
  Status(5) = Get_netCDF_Variable( NC_FileID, &
                                   DOUBLE4_VARNAME, &
                                   Double4 )
           
  Status(6) = Get_netCDF_Variable( NC_FileID, &
                                   DOUBLE5_VARNAME, &
                                   Double5 )
           
  Status(7) = Get_netCDF_Variable( NC_FileID, &
                                   DOUBLE6_VARNAME, &
                                   Double6 )
           
  Status(8) = Get_netCDF_Variable( NC_FileID, &
                                   DOUBLE7_VARNAME, &
                                   Double7 )

  IF ( ANY( Status /= SUCCESS ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error testing Get_netCDF_Variable DOUBLE routines', &
                          FAILURE )
    STOP
  END IF

  IF ( Doubles /= DEFAULT_DOUBLE .OR. &
       ANY( Double1 /= DEFAULT_DOUBLE ) .OR. &
       ANY( Double2 /= DEFAULT_DOUBLE ) .OR. &
       ANY( Double3 /= DEFAULT_DOUBLE ) .OR. &
       ANY( Double4 /= DEFAULT_DOUBLE ) .OR. &
       ANY( Double5 /= DEFAULT_DOUBLE ) .OR. &
       ANY( Double6 /= DEFAULT_DOUBLE ) .OR. &
       ANY( Double7 /= DEFAULT_DOUBLE )      ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Double variables', &
                          FAILURE )
    STOP
  ELSE
    CALL Display_Message( 'DOUBLE Get_netCDF_Variable', &
                          'Test successful', &
                          SUCCESS )
  END IF


  ! ----
  ! Done
  ! ----
  Error_Status = Close_netCDF( NC_FileID )

END PROGRAM Test_netCDF_Utility
