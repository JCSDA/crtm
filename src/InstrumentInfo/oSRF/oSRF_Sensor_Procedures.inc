
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Is_Microwave_Sensor
!
! PURPOSE:
!   Elemental function to test if the sensor type in the oSRF
!   object is for a microwave instrument.
!
! CALLING SEQUENCE:
!   is_set = oSRF_Is_Microwave_Sensor( oSRF )
!
! OBJECTS:
!   oSRF: Object to be tested.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   is_set: The return value is a logical value.
!            .TRUE.  - The sensor is a microwave instrument.
!            .FALSE. - The sensor is NOT a microwave instrument.
!           UNITS:      N/A
!           TYPE:       LOGICAL
!           DIMENSION:  Same as oSRF input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION oSRF_Is_Microwave_Sensor(oSRF) RESULT(is_set)
    TYPE(oSRF_type), INTENT(IN) :: oSRF
    LOGICAL :: is_set
    is_set = oSRF_Is_Sensor(oSRF, MICROWAVE_SENSOR)
  END FUNCTION oSRF_Is_Microwave_Sensor
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Is_Infrared_Sensor
!
! PURPOSE:
!   Elemental function to test if the sensor type in the oSRF
!   object is for an infrared instrument.
!
! CALLING SEQUENCE:
!   is_set = oSRF_Is_Infrared_Sensor( oSRF )
!
! OBJECTS:
!   oSRF: Object to be tested.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   is_set: The return value is a logical value.
!            .TRUE.  - The sensor is an infrared instrument.
!            .FALSE. - The sensor is NOT an infrared instrument.
!           UNITS:      N/A
!           TYPE:       LOGICAL
!           DIMENSION:  Same as oSRF input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION oSRF_Is_Infrared_Sensor(oSRF) RESULT(is_set)
    TYPE(oSRF_type), INTENT(IN) :: oSRF
    LOGICAL :: is_set
    is_set = oSRF_Is_Sensor(oSRF, INFRARED_SENSOR)
  END FUNCTION oSRF_Is_Infrared_Sensor
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Is_Visible_Sensor
!
! PURPOSE:
!   Elemental function to test if the sensor type in the oSRF
!   object is for a visible instrument.
!
! CALLING SEQUENCE:
!   is_set = oSRF_Is_Visible_Sensor( oSRF )
!
! OBJECTS:
!   oSRF: Object to be tested.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   is_set: The return value is a logical value.
!            .TRUE.  - The sensor is a visible instrument.
!            .FALSE. - The sensor is NOT a visible instrument.
!           UNITS:      N/A
!           TYPE:       LOGICAL
!           DIMENSION:  Same as oSRF input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION oSRF_Is_Visible_Sensor(oSRF) RESULT(is_set)
    TYPE(oSRF_type), INTENT(IN) :: oSRF
    LOGICAL :: is_set
    is_set = oSRF_Is_Sensor(oSRF, VISIBLE_SENSOR)
  END FUNCTION oSRF_Is_Visible_Sensor
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Is_Ultraviolet_Sensor
!
! PURPOSE:
!   Elemental function to test if the sensor type in the oSRF
!   object is for an ultraviolet instrument.
!
! CALLING SEQUENCE:
!   is_set = oSRF_Is_Ultraviolet_Sensor( oSRF )
!
! OBJECTS:
!   oSRF: Object to be tested.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   is_set: The return value is a logical value.
!            .TRUE.  - The sensor is an ultraviolet instrument.
!            .FALSE. - The sensor is NOT an ultraviolet instrument.
!           UNITS:      N/A
!           TYPE:       LOGICAL
!           DIMENSION:  Same as oSRF input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION oSRF_Is_Ultraviolet_Sensor(oSRF) RESULT(is_set)
    TYPE(oSRF_type), INTENT(IN) :: oSRF
    LOGICAL :: is_set
    is_set = oSRF_Is_Sensor(oSRF, ULTRAVIOLET_SENSOR)
  END FUNCTION oSRF_Is_Ultraviolet_Sensor
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Set_Microwave_Sensor
!
! PURPOSE:
!   Elemental subroutine to set the sensor type in the oSRF
!   object for a microwave instrument.
!
! CALLING SEQUENCE:
!   CALL oSRF_Set_Microwave_Sensor( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Set_Microwave_Sensor( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    CALL oSRF_Set_Sensor(oSRF, MICROWAVE_SENSOR)
  END SUBROUTINE oSRF_Set_Microwave_Sensor
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Set_Infrared_Sensor
!
! PURPOSE:
!   Elemental subroutine to set the sensor type in the oSRF
!   object for an infrared instrument.
!
! CALLING SEQUENCE:
!   CALL oSRF_Set_Infrared_Sensor( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Set_Infrared_Sensor( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    CALL oSRF_Set_Sensor(oSRF, INFRARED_SENSOR)
  END SUBROUTINE oSRF_Set_Infrared_Sensor
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Set_Visible_Sensor
!
! PURPOSE:
!   Elemental subroutine to set the sensor type in the oSRF
!   object for a visible instrument.
!
! CALLING SEQUENCE:
!   CALL oSRF_Set_Visible_Sensor( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Set_Visible_Sensor( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    CALL oSRF_Set_Sensor(oSRF, VISIBLE_SENSOR)
  END SUBROUTINE oSRF_Set_Visible_Sensor
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Set_Ultraviolet_Sensor
!
! PURPOSE:
!   Elemental subroutine to set the sensor type in the oSRF
!   object for an ultraviolet instrument.
!
! CALLING SEQUENCE:
!   CALL oSRF_Set_Ultraviolet_Sensor( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Set_Ultraviolet_Sensor( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    CALL oSRF_Set_Sensor(oSRF, ULTRAVIOLET_SENSOR)
  END SUBROUTINE oSRF_Set_Ultraviolet_Sensor
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Clear_Sensor
!
! PURPOSE:
!   Elemental subroutine to reinitialise the sensor type in the oSRF
!   object.
!
! CALLING SEQUENCE:
!   CALL oSRF_Clear_Sensor( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Clear_Sensor(oSRF)
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    oSRF%Sensor_Type = INVALID_SENSOR
  END SUBROUTINE oSRF_Clear_Sensor


  ! --------------------------------
  ! Generic, private sensor routines
  ! --------------------------------

  ELEMENTAL FUNCTION oSRF_Is_Sensor(oSRF, Sensor_Type) RESULT(is_set)
    TYPE(oSRF_type), INTENT(IN) :: oSRF
    INTEGER        , INTENT(IN) :: Sensor_Type
    LOGICAL :: is_set
    is_set = (oSRF%Sensor_Type == Sensor_Type)
  END FUNCTION oSRF_Is_Sensor


  ELEMENTAL SUBROUTINE oSRF_Set_Sensor(oSRF, Sensor_Type)
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    INTEGER        , INTENT(IN)     :: Sensor_Type
    oSRF%Sensor_Type = Sensor_Type
  END SUBROUTINE oSRF_Set_Sensor

