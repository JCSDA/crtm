require 'fortran'
require 'define/base'
module SrcGen
  module Define

    class Allocated < SrcGen::Define::Base


      # Method to generate the allocated procedure
      def generate(debug=false)

        # Construct the definition string
        str = <<-EOT


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   #{config.name}_Allocated
!
! PURPOSE:
!   Pure function to test the allocation status #{config.name} structure.
!
! CALLING SEQUENCE:
!   Status = #{config.name}_Allocated( #{config.name} )
!
! OBJECTS:
!   #{config.name}:
!     Structure which is to tested.
!     UNITS:      N/A
!     TYPE:       #{config.type_name}
!     DIMENSION:  Scalar
!     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   Status:
!     The return value is a logical value indicating the allocation status
!       .TRUE.  - if ALL of the allocatable members are allocated.
!       .FALSE. - if ANY of the allocatable members are not allocated.
!     UNITS:      N/A
!     TYPE:       LOGICAL
!     DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION #{config.name}_Allocated( self ) RESULT( Status )
    TYPE(#{config.type_name}), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION #{config.name}_Allocated
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
