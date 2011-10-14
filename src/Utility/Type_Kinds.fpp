!
! Type_Kinds
!
! Module to hold specification kinds for variable declaration, as well as 
! associated descriptors.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 12-Jun-2000
!                       paul.vandelst@noaa.gov
!

MODULE Type_Kinds

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything is private by default
  PRIVATE
  ! The integer types
  PUBLIC :: Byte   , n_Bytes_Byte 
  PUBLIC :: Short  , n_Bytes_Short
  PUBLIC :: Long   , n_Bytes_Long 
  PUBLIC :: LLong  , n_Bytes_LLong
  PUBLIC :: IP     , n_Bytes_IP       ! Default integer set by IIP
  ! The floating point types
  PUBLIC :: Single , n_Bytes_Single
  PUBLIC :: Double , n_Bytes_Double
  PUBLIC :: Quad   , n_Bytes_Quad  
  PUBLIC :: FP     , n_Bytes_FP       ! Default real set by IFP


  ! -------------------------------------------------------------------
  ! THE DEFAULT INTEGER INDEX.
  !
  ! The value of IIP sets the integer kind. The following chart
  ! details the correspondence:
  !
  !    IIP        INTEGER(IP)
  !  ==============================
  !     1        Byte 
  !     2       Short (2 bytes)
  !     3        Long (4 bytes)  - Default
  !     4       LLong (8 bytes)  - If available
  !
  ! -------------------------------------------------------------------
#ifndef INT_SIZE
#define INT_SIZE 4  /* Default 4-byte Long */
#endif
#if INT_SIZE == 1
  INTEGER, PARAMETER :: IIP = 1  ! 1=Byte is default integer size
#elif INT_SIZE == 2
  INTEGER, PARAMETER :: IIP = 2  ! 2=Short is default integer size
#elif INT_SIZE == 8
  INTEGER, PARAMETER :: IIP = 4  ! 4=LLong is default integer size
#else
  INTEGER, PARAMETER :: IIP = 3  ! 3=Long is default integer size
#endif


  ! -------------------------------------------------------------------
  ! THE DEFAULT FLOATING POINT INDEX.
  !
  ! The value of IFP sets the floating point kind. The following
  ! chart details the correspondence:
  !
  !    IFP          REAL(FP)
  !  ==============================
  !     1       Single (4  bytes)
  !     2       Double (8  bytes)  - Default
  !     3       Quad   (16 bytes)  - If available
  !
  ! -------------------------------------------------------------------
#ifndef REAL_SIZE
#define REAL_SIZE 8  /* Default 8-byte Double */
#endif
#if REAL_SIZE == 4
  INTEGER, PARAMETER :: IFP = 1  ! 1=Single is default real size
#elif REAL_SIZE == 16
  INTEGER, PARAMETER :: IFP = 3  ! 3=Quad is default real size
#else
  INTEGER, PARAMETER :: IFP = 2  ! 2=Double is default real size
#endif


  ! Integer definitions
  ! ...Integer types
  INTEGER, PARAMETER :: Byte    = SELECTED_INT_KIND(1)   ! Byte  integer
  INTEGER, PARAMETER :: Short   = SELECTED_INT_KIND(4)   ! Short integer
  INTEGER, PARAMETER :: Long    = SELECTED_INT_KIND(8)   ! Long  integer
  INTEGER, PARAMETER :: LLong   = SELECTED_INT_KIND(16)  ! LLong integer
  ! ...Expected 8-bit byte sizes of the integer kinds
  INTEGER, PARAMETER :: n_Bytes_Byte  = 1
  INTEGER, PARAMETER :: n_Bytes_Short = 2
  INTEGER, PARAMETER :: n_Bytes_Long  = 4
  INTEGER, PARAMETER :: n_Bytes_LLong = 8
  ! ...Define arrays for default definition
  INTEGER, PARAMETER :: N_IP = 4
  INTEGER, PARAMETER, DIMENSION(N_IP) :: IP_KIND_TYPES = (/ Byte,  &
                                                            Short, &
                                                            Long,  &
                                                            LLong  /) 
  INTEGER, PARAMETER, DIMENSION(N_IP) :: IP_BYTE_SIZES = (/ n_Bytes_Byte,  &
                                                            n_Bytes_Short, &
                                                            n_Bytes_Long,  &
                                                            n_Bytes_LLong  /)
  ! ...Default values
  INTEGER, PARAMETER :: IP         = IP_KIND_TYPES(IIP)
  INTEGER, PARAMETER :: n_Bytes_IP = IP_BYTE_SIZES(IIP)


  ! Floating point definitions
  ! ...Floating point types
  INTEGER, PARAMETER :: Single = SELECTED_REAL_KIND(6)  ! Single precision
  INTEGER, PARAMETER :: Double = SELECTED_REAL_KIND(15) ! Double precision
  INTEGER, PARAMETER :: Quad   = SELECTED_REAL_KIND(20) ! Quad precision
  ! ...Expected 8-bit byte sizes of the floating point kinds
  INTEGER, PARAMETER :: n_Bytes_Single = 4
  INTEGER, PARAMETER :: n_Bytes_Double = 8
  INTEGER, PARAMETER :: n_Bytes_Quad   = 16
  ! ...Define arrays for default definition
  INTEGER, PARAMETER :: N_FP = 3
  INTEGER, PARAMETER, DIMENSION(N_FP) :: FP_KIND_TYPES = (/ Single, &
                                                            Double, &
                                                            Quad    /) 
  INTEGER, PARAMETER, DIMENSION(N_FP) :: FP_BYTE_SIZES = (/ n_Bytes_Single, &
                                                            n_Bytes_Double, &
                                                            n_Bytes_Quad    /)
  ! ...Default values
  INTEGER, PARAMETER :: FP         = FP_KIND_TYPES(IFP)
  INTEGER, PARAMETER :: n_Bytes_FP = FP_BYTE_SIZES(IFP)
  
END MODULE Type_Kinds
