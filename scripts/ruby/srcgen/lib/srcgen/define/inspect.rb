require 'fortran'
require 'define/base'
module SrcGen
  module Define

    class Inspect < SrcGen::Define::Base

      BASE_INDENT = " "*4
      
      
      # Method to generate the inspection procedure
      def generate(debug=false)

        str = <<-EOT


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   #{config.name}_Inspect
!
! PURPOSE:
!   Subroutine to print the contents of #{config.name} objects to stdout.
!
! CALLING SEQUENCE:
!   CALL #{config.name}_Inspect( #{config.name} )
!
! OBJECTS:
!   #{config.name}:
!     Object to display.
!     UNITS:      N/A
!     TYPE:       #{config.type_name}
!     DIMENSION:  Scalar
!     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE #{config.name}_Inspect( &
    self, &  ! Input
    pause )  ! Optional input
    ! Arguments
#{interface_argdef}
    ! Local variables
    LOGICAL :: wait
#{interface_dimvardef}
    INTEGER :: i2, i3, i4

    wait = .FALSE.
    IF ( PRESENT(pause) ) wait = pause
   
    WRITE(*,'(1x,"MWwaterLSCLUT OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version : ",i0,".",i0)') self%Release, self%Version
    ! Dimensions
    WRITE(*,'(3x,"n_Angles        : ",i0)') self%n_Angles     
    WRITE(*,'(3x,"n_Frequencies   : ",i0)') self%n_Frequencies
    WRITE(*,'(3x,"n_Temperatures  : ",i0)') self%n_Temperatures
    WRITE(*,'(3x,"n_Wind_Speeds   : ",i0)') self%n_Wind_Speeds
    IF ( .NOT. MWwaterLSCLUT_Associated(self) ) RETURN
    ! Dimension arrays
    WRITE(*,'(3x,"Angle :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Angle     
    WRITE(*,'(3x,"Frequency :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Frequency 
    WRITE(*,'(3x,"Temperature :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Temperature
    WRITE(*,'(3x,"Wind_Speed :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Wind_Speed

    ! Emissivity arrays
    WRITE(*,'(/3x,"Large-scale correction emissivity(vertical polarisation) :")')
    IF ( wait ) THEN
      WRITE(*,FMT='(/1x,"Paused. Press <ENTER> to continue...")',ADVANCE='NO')
      READ(*,*)
    END IF

    DO i4 = 1, self%n_Wind_Speeds
      WRITE(*,'(5x,"WIND_SPEED  :",es13.6)') self%Wind_Speed(i4)
      DO i3 = 1, self%n_Temperatures
        WRITE(*,'(5x,"TEMPERATURE :",es13.6)') self%Temperature(i3)     
        DO i2 = 1, self%n_Frequencies
          WRITE(*,'(5x,"FREQUENCY   :",es13.6)') self%Frequency(i2)     
          WRITE(*,'(5(1x,es13.6,:))') self%lsc_ev(:,i2,i3,i4)
        END DO
      END DO
    END DO

    WRITE(*,'(/3x,"Large-scale correction emissivity(horizontal polarisation) :")')
    IF ( wait ) THEN
      WRITE(*,FMT='(/1x,"Paused. Press <ENTER> to continue...")',ADVANCE='NO')
      READ(*,*)
    END IF

    DO i4 = 1, self%n_Wind_Speeds
      WRITE(*,'(5x,"WIND_SPEED  :",es13.6)') self%Wind_Speed(i4)
      DO i3 = 1, self%n_Temperatures
        WRITE(*,'(5x,"TEMPERATURE :",es13.6)') self%Temperature(i3)     
        DO i2 = 1, self%n_Frequencies
          WRITE(*,'(5x,"FREQUENCY   :",es13.6)') self%Frequency(i2)     
          WRITE(*,'(5(1x,es13.6,:))') self%lsc_eh(:,i2,i3,i4)
        END DO
      END DO
    END DO
  END SUBROUTINE MWwaterLSCLUT_Inspect
        EOT

        # Output definition in debug mode
        if debug
          puts("\n---BEGIN-DEBUG-OUTPUT---")
          puts("\n#{self.class} #{__method__} method output:")
          puts("\n#{str}")
          puts("\n----END-DEBUG-OUTPUT----")
        end

        str
      end

    private

      def interface_argdef
        arg_list = ["self", "pause"]
        type_list = ["TYPE(#{config.type_name})", "LOGICAL, OPTIONAL"]
        str = sandwich_string(BASE_INDENT,type_list,", ")
        intent_list = ["INTENT(IN)"]*arg_list.length
        str = sandwich_string(str,intent_list," :: ")
        str = sandwich_string(str,arg_list,"\n",last="").join
      end

      def interface_dimvardef
        "stuff"
      end
    end
  end
end
