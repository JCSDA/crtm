!
! SRF_Utility
!
! Module containing SRF application routines
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2003
!                       paul.vandelst@ssec.wisc.edu
!
!       Modified by:    Yong Chen, CIRA/CSU/JCSDA 22-Aug-2008
!                       Yong.Chen@noaa.gov
!
 
MODULE SRF_Utility

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,            ONLY: fp
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Interpolate_Utility,   ONLY: Polynomial_Interpolate
  USE Integrate_Utility,     ONLY: Integral
  USE SRF_Define,            ONLY: SRF_type, &
                                   Associated_SRF, &
                                   Destroy_SRF, &
                                   Allocate_SRF, &
                                   Assign_SRF, &
                                   Frequency_SRF, &
                                   Integrate_SRF
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Interpolate_SRF
  PUBLIC :: Convolve_with_SRF
  PUBLIC :: Calc_SRF_First_Mom


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Maximum msg length
  INTEGER, PARAMETER :: ML = 256
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: ONEpointFIVE  = 1.5_fp


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!:sdoc:+
!
! NAME:
!       Interpolate_SRF
!
! PURPOSE:
!       Function to interpolate an input SRF to another frequency grid.
!
! CALLING SEQUENCE:
!       Error_STatus = Interpolate_SRF( SRF                    , &  ! Input
!                                       iSRF                   , &  ! In/Output
!                                       Order      =Order      , &  ! Optional input
!                                       RCS_Id     =RCS_Id     , &  ! Revision control
!                                       Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SRF:          SRF structure containing the instrument channel
!                     response to be interpolated.
!                     UNITS:      N/A
!                     TYPE:       SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       iSRF:         On input contains the components used to generate the
!                     interpolation frequency grid:
!                       iSRF%f1_Band(:)
!                       iSRF%f2_Band(:)
!                       iSRF%npts_Band(:)
!                     and the interpolation frequency grid itself:
!                       iSRF%Frequency(:)
!                     UNITS:      N/A
!                     TYPE:       SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       iSRF:         On output, contains the interpolated SRF response
!                     in the component,
!                       iSRF%Response(:)
!                     UNITS:      N/A
!                     TYPE:       SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Order:        The order of the interpolating polynomial. This 
!                     argument, if supplied, is passed to the interpolation
!                     function where, if not specified, linear interpolation
!                     (order = 1) is the default. If specified, must be an
!                     odd number > 0.
!                     UNITS:      None
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:  Character string specifying a filename in which any
!                     msgs will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output msgs to standard output.
!                     UNITS:      None
!                     TYPE:       Character
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the SRF integration was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SRF argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2003
!                       paul.vandelst@ssec.wisc.edu
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Interpolate_SRF( SRF         , &  ! Input
                            iSRF        , &  ! Output
                            Order       , &  ! Optional input
                            RCS_Id      , &  ! Revision control
                            Message_Log ) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    TYPE(SRF_type),         INTENT(IN)     :: SRF
    TYPE(SRF_type),         INTENT(IN OUT) :: iSRF
    INTEGER,      OPTIONAL, INTENT(IN)     :: Order
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Interpolate_SRF'
    REAL(fp),     PARAMETER :: EPS = EPSILON(ONE)
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: i1, i2, j1, j2, m, ni, nj
    REAL(fp) :: df

    ! Setup
    ! -----
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    
    ! Check structure association
    IF ( .NOT. Associated_SRF( SRF ) ) THEN
      Error_Status = FAILURE
      msg = 'Some or all INPUT SRF pointer members are NOT associated.'
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF

    IF ( .NOT. Associated_SRF( iSRF ) ) THEN
      Error_Status = FAILURE
      msg = 'Some or all OUTPUT iSRF pointer members are NOT associated.'
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF

    ! Set the output SRF info
    iSRF%Sensor_Id        = SRF%Sensor_Id       
    iSRF%WMO_Satellite_ID = SRF%WMO_Satellite_ID
    iSRF%WMO_Sensor_ID    = SRF%WMO_Sensor_ID   
    iSRF%Sensor_Type      = SRF%Sensor_Type     
    iSRF%Channel          = SRF%Channel
    

    ! Begin loop over bands for interpolation
    ! ---------------------------------------
    i2 = 0; j2 = 0
    Band_Loop: DO m = 1, SRF%n_Bands

      ! Determine where the old SRF slots into its frequency grid for this band
      i1 = i2 + 1
      i2 = SRF%npts_Band(m) + i1 - 1
      
      ! Determine where the new SRF slots into its frequency grid for this band
      j1 = j2 + 1
      j2 = iSRF%npts_Band(m) + j1 - 1

      ! Perform the interpolation
      Error_Status = Polynomial_Interpolate(  SRF%Frequency(i1:i2)  , &  ! Input,  X
                                              SRF%Response(i1:i2)   , &  ! Input,  Y
                                             iSRF%Frequency(j1:j2)  , &  ! Input,  Xint
                                             iSRF%Response(j1:j2)   , &  ! Output, Yint
                                             Order      =Order      , &  ! Optional input
                                             Message_Log=Message_Log  )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( msg,'("Error interpolating band ",i0," SRF response")' ) m
        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF
      
    END DO Band_Loop


    ! Integrate the interpolated SRF
    ! ------------------------------
    Error_Status = Integrate_SRF( iSRF,Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred integrating interpolated SRF', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION Interpolate_SRF


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Convolve_with_SRF
!
! PURPOSE:
!       Procedure to convolve a spectrum with an instrument channel SRF.
!
! CALLING SEQUENCE:
!       Error_Status = Convolve_with_SRF( Frequency              , &  ! Input
!                                         Spectrum               , &  ! Input
!                                         SRF                    , &  ! Input
!                                         Convolved_Spectrum     , &  ! Output
!                                         Interpolate=Interpolate, &  ! Optional input
!                                         Integrate  =Integrate  , &  ! Optional input
!                                         Order      =Order      , &  ! Optional input
!                                         RCS_Id     =RCS_Id     , &  ! Revision control
!                                         Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Frequency:           Frequency grid of the input spectrum.
!                            Must be evenly spaced in frequency.
!                            UNITS:      Variable (cm^-1 or GHz)
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Rank-1
!                            ATTRIBUTES: INTENT(IN)
!
!       Spectrum:            Spectrum to be convolved with the SRF response.
!                            UNITS:      Variable.
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Rank-1
!                            ATTRIBUTES: INTENT(IN)
!
!       SRF:                 SRF structure containing the instrument channel
!                            response to be used in the convolution.
!                            UNITS:      N/A
!                            TYPE:       SRF_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Convolved_Spectrum:  The input spectrum convolved with the input SRF
!                            response.
!                            UNITS:      Same as input Spectrum argument
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Interpolate:         Set this argument to force interpolation of the SRF
!                            response to the input frequency grid. If not set,
!                            the default is to assume that the SRF and spectrum
!                            frequency grids match up for the required span.
!                            If = 0; do not interpolate the SRF response (DEFAULT)
!                               = 1; interpolate input SRF response to the input
!                                    frequency grid.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Integrate:           Set this argument to perform numerical integration
!                            when doing the convolutions. If not set, the default
!                            action is to simply sum the results.
!                            If = 0; use summation to compute convolved value. (DEFAULT)
!                               = 1; perform numerical integration to compute the
!                                    convolved value.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Order:               The order of the interpolating polynomial. This
!                            argument, if supplied, is passed to the interpolation
!                            function where, if not specified, linear interpolation
!                            (order = 1) is the default. If specified, must be an
!                            odd number > 0.
!                            UNITS:      None
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:         Character string specifying a filename in which any
!                            msgs will be logged. If not specified, or if an
!                            error occurs opening the log file, the default action
!                            is to output msgs to standard output.
!                            UNITS:      None
!                            TYPE:       Character
!                            DIMENSION:  Scalar, LEN = *
!                            ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:              Character string containing the Revision Control
!                            System Id field for the module.
!                            UNITS:      None
!                            TYPE:       CHARACTER
!                            DIMENSION:  Scalar, LEN = *
!                            ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:        The return value is an integer defining the error status.
!                            The error codes are defined in the Message_Handler module.
!                            If == SUCCESS the spectrum convolution was successful
!                               == FAILURE an error occurred
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2003
!                       paul.vandelst@ssec.wisc.edu
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Convolve_with_SRF( Frequency         , &  ! Input
                              Spectrum          , &  ! Input
                              SRF               , &  ! Input
                              Convolved_Spectrum, &  ! Output
                              Interpolate       , &  ! Optional input
                              Integrate         , &  ! Optional input
                              Order             , &  ! Optional input
                              RCS_Id            , &  ! Revision control
                              Message_Log       ) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: Frequency(:)
    REAL(fp),               INTENT(IN)  :: Spectrum(:)
    TYPE(SRF_type),         INTENT(IN)  :: SRF
    REAL(fp),               INTENT(OUT) :: Convolved_Spectrum
    INTEGER,      OPTIONAL, INTENT(IN)  :: Interpolate
    INTEGER,      OPTIONAL, INTENT(IN)  :: Integrate
    INTEGER,      OPTIONAL, INTENT(IN)  :: Order
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Convolve_with_SRF'
    INTEGER, PARAMETER :: MAX_N_BANDS = 4
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: Destroy_Status
    LOGICAL :: Frequency_Match
    LOGICAL :: Summation
    TYPE(SRF_type) :: uSRF
    INTEGER :: n_Frequencies, n_Points
    INTEGER :: i1, i2, j1, j2, m
    REAL(fp) :: df, cspc
    REAL(fp) :: f1_Band(MAX_N_BANDS), f2_Band(MAX_N_BANDS)
    INTEGER  :: npts_Band(MAX_N_BANDS)

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Check for consistent array sizes
    n_Frequencies = SIZE(Frequency)
    IF ( SIZE(Spectrum) /= n_Frequencies ) THEN
      Error_Status = FAILURE
      msg = 'Size of input frequency and spectrum arrays are different'
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF

    ! Default is no interpolation as SRF and spectrum frequency match is assumed...
    Frequency_Match = .TRUE.
    ! ...unless the Interpolate argument is set
    IF ( PRESENT( Interpolate ) ) THEN
      IF ( Interpolate == SET ) Frequency_Match = .FALSE.
    END IF

    ! Default is to sum the convolved SRF and spectrum...
    Summation = .TRUE.
    ! ...unless the Integrate argument is set
    IF ( PRESENT( Integrate ) ) THEN
      IF ( Integrate == SET ) Summation = .FALSE.
    END IF

    ! Compute the input spectrum frequency interval
    dF = Frequency(2) - Frequency(1)


    ! Copy or interpolate SRF as required
    ! -----------------------------------
    IF ( Frequency_Match ) THEN
      ! Copy the input SRF to a local structure
      Error_Status = Assign_SRF( SRF,uSRF,Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        msg = 'Error copying SRF'
        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF

    ELSE
      ! Find the frequency points that correspond to the input SRF band edge(s)
      DO m = 1, SRF%n_Bands
        j1 = f_Index( SRF%f1_Band(m),Frequency(1),dF,lowf_edge=.true. )
        j2 = f_Index( SRF%f2_Band(m),Frequency(1),dF )
        f1_Band(m)   = Frequency(j1)
        f2_Band(m)   = Frequency(j2)
        npts_Band(m) = j2-j1+1
      END DO
      ! Allocate local SRF structure
      n_Points = SUM(npts_Band(1:SRF%n_Bands))
      Error_Status = Allocate_SRF( n_Points,uSRF,n_Bands=SRF%n_Bands,Message_Log=Message_Log)
      IF ( Error_Status /= SUCCESS ) THEN
        msg = 'Error allocating SRF'
        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF
      ! Fill the band components
      uSRF%f1_Band   = f1_Band(1:SRF%n_Bands)
      uSRF%f2_Band   = f2_Band(1:SRF%n_Bands)
      uSRF%npts_Band = npts_Band(1:SRF%n_Bands)
      ! Compute the frequency grid
      Error_Status = Frequency_SRF( uSRF,Message_Log=Message_Log)
      IF ( Error_Status /= SUCCESS ) THEN
        msg = 'Error computing SRF frequency grid'
        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF
      ! Interpolate the SRF
      Error_Status = Interpolate_SRF( SRF,uSRF,Order=Order,Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        msg = 'Error interpolating SRF'
        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF
      ! Integrate the SRF
      Error_Status = Integrate_SRF( uSRF,Message_Log=Message_Log)
      IF ( Error_Status /= SUCCESS ) THEN
        msg = 'Error integrating SRF'
        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF
    END IF


    ! Perform the convolution
    ! -----------------------
    ! Initialise the point offsets and sum
    Convolved_Spectrum = ZERO
    i2 = 0
    ! Begin loop over bands
    Band_Loop: DO m = 1, uSRF%n_Bands

      ! Determine where the current band SRF slots into its frequency grid
      i1 = i2 + 1
      i2 = uSRF%npts_Band(m) + i1 - 1

      ! Determine where the current band SRF slots into the spectrum frequency grid
      j1 = f_Index( uSRF%f1_Band(m),Frequency(1),dF )
      j2 = f_Index( uSRF%f2_Band(m),Frequency(1),dF )

      ! Perform the convolution
      IF ( Summation ) THEN
        ! Just sum it all up
        cspc = SUM(uSRF%Response(i1:i2)*Spectrum(j1:j2))*dF
      ELSE
        ! Integrate it
        Error_Status = Integral( Frequency(j1:j2), &
                                 uSRF%Response(i1:i2)*Spectrum(j1:j2), &
                                 cspc )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( msg,'("Error integrating SRF*SPC for band ",i0)' ) m
          CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
          RETURN
        END IF
      END IF
      
      ! Accumulate the result
      Convolved_Spectrum = Convolved_Spectrum + cspc
    END DO Band_Loop

    ! Normalise the result
    IF ( Summation ) THEN
      Convolved_Spectrum = Convolved_Spectrum/uSRF%Summation_SRF
    ELSE
      Convolved_Spectrum = Convolved_Spectrum/uSRF%Integrated_SRF
    END IF
    

    ! Clean up
    ! --------
    Destroy_Status = Destroy_SRF( uSRF,Message_Log=Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying local uSRF structure', &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Convolve_with_SRF
  
!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Calc_SRF_First_Mom
!
! PURPOSE:
!       Procedure to calculate the first moment of an SRF.
!
! CALLING SEQUENCE:
!       Error_Status = Calc_SRF_First_Mom( SRF                    , &  ! Input
!                                          First_Moment           , &  ! Output
!                                          Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!
!          SRF:              SRF structure containing the instrument channel   
!                            response to be used in the convolution.           
!                            UNITS:      N/A                                   
!                            TYPE:       SRF_type                              
!                            DIMENSION:  Scalar                                
!                            ATTRIBUTES: INTENT(IN)                                            
!
! OUTPUT ARGUMENTS:          
!               
!          First_Moment:     Calculated first moment of the SRF.
!                            UNITS:      cm^-1
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!
!  Message_Log:              Character string specifying a filename in which any  
!                            msgs will be logged. If not specified, or if an      
!                            error occurs opening the log file, the default action
!                            is to output msgs to standard output.                
!                            UNITS:      None                                     
!                            TYPE:       Character                                
!                            DIMENSION:  Scalar, LEN = *                          
!                            ATTRIBUTES: INTENT(IN), OPTIONAL     
!
! FUNCTION RESULT:
!       
!       Error_Status:        The return value is an integer defining the error status.
!                            The error codes are defined in the Message_Handler module.
!                            If == SUCCESS the spectrum convolution was successful
!                               == FAILURE an error occurred
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:    David Groff, SAIC 8-June-2009
!                      david.groff@noaa.gov 
!
!:sdoc-:
!------------------------------------------------------------------------------  
    
  FUNCTION Calc_SRF_First_Mom( SRF               , &  ! Input
                               First_Moment      , &  ! Output                          
                               Message_Log) &  ! Error messaging
                               RESULT( Error_Status )
    ! Arguments
    TYPE(SRF_type),           INTENT(IN OUT) :: SRF
    CHARACTER(*),   OPTIONAL, INTENT(IN)     :: Message_Log
    REAL(fp),                 INTENT(OUT)    :: First_Moment
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Calc_SRF_First_Mom'
    INTEGER :: CUBIC_ORDER = 3
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: i1, i2, m, n
    REAL(fp):: Band_Weight

    ! Setup
    ! -----
    Error_Status = SUCCESS
    
    ! ALL pointers must be associated
    IF ( .NOT. Associated_SRF( SRF ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SRF pointer members are NOT associated.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the number of bands
    IF ( SRF%n_Bands < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF structure must contain at least 1 band', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the number of points
    IF ( SRF%n_Points < 2 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF structure must contain at least 2 points.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the number of points in each band
    IF ( ANY(SRF%npts_Band < 2) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF must contain at least 2 points for each band.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check the total points                                                   
    IF ( SUM(SRF%npts_Band) /= SRF%n_Points ) THEN                            
      Error_Status = FAILURE                                             
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF must have consistent data points.', &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF

    ! Compute the first moment of the SRF
    ! -----------------------------------
    ! Initialize the first moment
    First_Moment = ZERO
    Band_Weight = ZERO
    ! Initialise the offset counter
    n = 0

    ! Loop over the bands
    DO m = 1, SRF%n_Bands
      
      ! The point limits for this band
      i1 = n + 1
      i2 = SRF%npts_Band(m) + n
      
      ! Calculate Weight for the band      
      Error_Status = Integral( SRF%Frequency(i1:i2),                      &         
                               SRF%Response(i1:i2)*SRF%Frequency(i1:i2),  &         
                               Band_Weight                                )         
      IF ( Error_Status /= SUCCESS ) THEN                                                    
        WRITE( msg,'("Error occurred integrating channel ",i0," SRF")' ) SRF%Channel         
        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )  
        RETURN                                                                               
      END IF                                                                                 
      
      ! Accumulate the band weights                                                                                       
      First_Moment = First_Moment + Band_Weight                                        
      
      ! Update the offset counter
      n = n + SRF%npts_Band(m)
    END DO
    
    First_Moment = First_Moment/SRF%Integrated_SRF

  END FUNCTION Calc_SRF_First_Mom

!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------------
!
! NAME:
!       f_Interval
!
! PURPOSE:
!       Function to compute the interval of a frequency grid from its parameters
!
! CALLING SEQUENCE:
!       df = f_Interval(f1,f2,n)
!
! INPUT ARGUMENTS:
!       f1:  Begin frequency of the grid.
!            UNITS:      Variable (cm^-1 or GHz)
!            TYPE:       REAL(fp)
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN)
!
!       f2:  End frequency of the grid.
!            UNITS:      Same as input f1 argument.
!            TYPE:       REAL(fp)
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN)
!
!       n:   Number of points in the frequency grid.
!            UNITS:      N/A
!            TYPE:       INTEGER
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       df:  Interval of the frequency grid. 
!            UNITS:      Same as input f1 argument.
!            TYPE:       REAL(fp)
!            DIMENSION:  Scalar
!
!------------------------------------------------------------------------------------

  FUNCTION f_Interval(f1,f2,n) RESULT(dF)
    REAL(fp), INTENT(IN) :: f1, f2
    INTEGER , INTENT(IN) :: n
    REAL(fp) :: dF
    dF = (f2-f1)/REAL(n-1,fp)
  END FUNCTION f_Interval


!------------------------------------------------------------------------------------
!
! NAME:
!       f_Index
!
! PURPOSE:
!       Subroutine to determine the index value of a frequency grid that
!       correspond to a supplied frequency values.
!
! CALLING SEQUENCE:
!       idx = f_Index( f, f1_grid, df_grid, lowf_edge )
!
! INPUT ARGUMENTS:
!       f:          Frequency for which an index value is required.
!                   UNITS:      Variable (cm^-1 or GHz)
!                   TYPE:       REAL(fp)
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN)
!
!       f1_grid:    Frequency of the first point of a frequency grid.
!                   Should be <= f.
!                   UNITS:      Same as input f argument.
!                   TYPE:       REAL(fp)
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN)
!
!       df_grid:    Interval of the frequency grid.
!                   UNITS:      Same as input f argument.
!                   TYPE:       REAL(fp)
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN)
!
! INPUT ARGUMENTS:
!       lowf_edge:  Logical flag to indicate what SRF band edge we are at. This
!                   flag affects how the index value is computed.
!                   If .TRUE.  then idx is adjusted so that f1(idx) >= f
!                      .FALSE. then idx is adjusted so that f1(idx) <= f [DEFAULT]
!                   UNITS:      N/A
!                   TYPE:       LOGICAL
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       idx:        Index of the frequency grid, f1, that most closely corresponds
!                   to the passed SRF band edge frequency.
!                   UNITS:      N/A
!                   TYPE:       INTEGER
!                   DIMENSION:  Scalar
!
!------------------------------------------------------------------------------------

  FUNCTION f_Index(f,f1_grid,df_grid,lowf_edge) RESULT(idx)
    REAL(fp),          INTENT(IN) :: f, f1_grid, df_grid
    LOGICAL, OPTIONAL, INTENT(IN) :: lowf_edge
    INTEGER :: idx
    LOGICAL :: highf_edge
    REAL(fp) :: f1

    ! Assume we are at the high frequency edge of a band...
    highf_edge = .TRUE.
    ! ...unless the low frequency edge keyword is set.
    IF ( PRESENT(lowf_edge) ) THEN
      IF ( lowf_edge ) highf_edge = .FALSE.
    END IF

    ! Compute the index
    idx = INT(ONEpointFIVE + ( (f-f1_grid)/df_grid ))

    ! Adjust the index based on whether we are
    ! at a low or high frequency band edge.
    f1 = f1_grid + REAL(idx-1)*df_grid
    IF ( highf_edge ) THEN
      IF ( f1 > f ) idx = idx-1
    ELSE
      IF ( f1 < f ) idx = idx+1
    END IF
    
  END FUNCTION f_Index

END MODULE SRF_Utility

 
