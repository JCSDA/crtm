!
! Read_MW_W_LS_LUT
!
! Module containing routines to read MW Water large-scale emissivity LUT part.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 18-Mar-2002
!                       paul.vandelst@noaa.gov
!
MODULE Read_MW_W_LS_LUT

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds         , ONLY: Long, Double
  USE File_Utility       , ONLY: File_Open, File_Exists, Get_Lun
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: MW_W_LS_LUT_ReadFile, MW_W_LS_type
  PUBLIC :: MW_W_LS
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
    '$Id: Read_MW_W_LS_LUT.f90 13518 2011-04-22 17:25:42Z paul.vandelst@noaa.gov $'
  ! Default message length
  INTEGER, PARAMETER :: ML = 256

  ! ----------------------------
  ! Surface structure definition
  ! ----------------------------
  !:tdoc+:
  TYPE :: MW_W_LS_type
     ! Array dimensions
    INTEGER :: n_Wind_S      = 0   ! I1 dimension
    INTEGER :: n_SST         = 0   ! I2 dimension
    INTEGER :: n_Frequency   = 0   ! I3 dimension
    INTEGER :: n_Zenith      = 0   ! I4 dimension
    INTEGER :: N1            = 0   ! not used
    REAL(Double),  ALLOCATABLE :: WindSp(:)      ! I1
    REAL(Double),  ALLOCATABLE :: SST(:)         ! I2
    REAL(Double),  ALLOCATABLE :: Fre(:)         ! I3
    REAL(Double),  ALLOCATABLE :: Zangle(:)      ! I4
    ! LUT data
    REAL(Double),  ALLOCATABLE :: ev(:,:,:,:)  ! I1 x I2 x I3 x I4 
    REAL(Double),  ALLOCATABLE :: eh(:,:,:,:)  ! I1 x I2 x I3 x I4 
  END TYPE MW_W_LS_type
  
  TYPE(MW_W_LS_type), SAVE :: MW_W_LS
    
CONTAINS

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SpcCoeff_Binary_ReadFile
!
! PURPOSE:
!       Function to read SpcCoeff object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = SpcCoeff_Binary_ReadFile( &
!                        Filename     , &
!                        SpcCoeff     , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       SpcCoeff format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       SpcCoeff:       SpcCoeff object containing the spectral
!                       coefficient data.
!                       UNITS:      N/A
!                       TYPE:       SpcCoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file read was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION MW_W_LS_LUT_ReadFile( Filename ) &  
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),        INTENT(IN)  :: Filename
    ! Function result
    INTEGER :: err_stat, io_stat,fid
    CHARACTER(ML) :: msg
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'MW_W_LS_LUT_ReadFile'
    ! Function variables
    ! Setup
    INTEGER :: ii,jj,kk,ll
    err_stat = SUCCESS

    ! Open the file
    IF ( File_Exists( Filename ) ) THEN
        ! Get a free unit number
        fid = Get_Lun()
        print *, fid, trim(Filename)
!           OPEN( fid, FILE   = Filename, form='unformatted',CONVERT='big_endian', &
           OPEN( fid, FILE   = Filename, form='unformatted', &
               STATUS = 'OLD', &
               ACTION = 'READ', &
               IOSTAT =  err_stat )
               
      IF ( err_Stat /= SUCCESS ) THEN
        msg = 'Error opening '//TRIM(Filename)
        PRINT *,msg; RETURN
      END IF
    ELSE
      msg = 'File '//TRIM(Filename)//' not found.'
      PRINT *,msg
      RETURN
    END IF


    ! Read and check the release and version
    ! Read the spectral coefficient data
    ! ...Read the dimensions

    READ( fid, IOSTAT=io_stat ) MW_W_LS%n_Wind_S,MW_W_LS%n_SST, &
      MW_W_LS%n_Frequency,MW_W_LS%n_Zenith
      
      ALLOCATE ( MW_W_LS%WindSp(MW_W_LS%n_Wind_S), MW_W_LS%SST(MW_W_LS%n_SST) )
      ALLOCATE ( MW_W_LS%Fre(MW_W_LS%n_Frequency), MW_W_LS%Zangle(MW_W_LS%n_Zenith) )
      
      ALLOCATE ( MW_W_LS%ev(MW_W_LS%n_Wind_S,MW_W_LS%n_SST,MW_W_LS%n_Frequency,MW_W_LS%n_Zenith) )
      ALLOCATE ( MW_W_LS%eh(MW_W_LS%n_Wind_S,MW_W_LS%n_SST,MW_W_LS%n_Frequency,MW_W_LS%n_Zenith) )
      
    READ( fid, IOSTAT=io_stat ) MW_W_LS%WindSp
    print *,MW_W_LS%WindSp
    READ( fid, IOSTAT=io_stat ) MW_W_LS%SST  
    READ( fid, IOSTAT=io_stat ) MW_W_LS%Fre
    READ( fid, IOSTAT=io_stat ) MW_W_LS%Zangle
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading data dimensions. IOSTAT = ",i0)' ) io_stat
      PRINT *,msg; RETURN
    END IF
    ! ...Read the sensor info
!    READ( fid, IOSTAT=io_stat ) MW_W_LS%N1
    READ( fid, IOSTAT=io_stat ) MW_W_LS%ev
    READ( fid, IOSTAT=io_stat ) MW_W_LS%eh    

    CLOSE(fid)

  END FUNCTION MW_W_LS_LUT_ReadFile

 END MODULE Read_MW_W_LS_LUT
!
