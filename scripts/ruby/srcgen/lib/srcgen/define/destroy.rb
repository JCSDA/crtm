require 'fortran'
require 'define/base'
module SrcGen
  module Define

    class Destroy < SrcGen::Define::Base


      # Method to generate the destroy procedure
      def generate(debug=false)

        # Construct the definition string
        str = <<-EOT
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   #{config.name}_Destroy
!
! PURPOSE:
!   Pure subroutine to re-initialize a #{config.name} structure.
!
! CALLING SEQUENCE:
!   CALL #{config.name}_Destroy( #{config.name} )
!
! OBJECTS:
!   #{config.name}:
!     Structure which is to be re-initialized.
!     UNITS:      N/A
!     TYPE:       #{config.type_name}
!     DIMENSION:  Scalar
!     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE SUBROUTINE #{config.name}_Destroy( self )
    TYPE(#{config.type_name}), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE #{config.name}_Destroy
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

    end
  end
end
