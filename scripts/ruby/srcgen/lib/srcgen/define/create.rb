require 'fortran'
require 'define/base'
module SrcGen
  module Define

    class Create < SrcGen::Define::Base


      # Method to generate the creation procedure
      def generate(debug=false)

x = prepend_pad_append_string("!          ",[config.name,config.dim_names].flatten,", &\n",last="  )\n")

puts x
 
        str = <<-EOT
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   #{config.name}_Create
!
! PURPOSE:
!   Pure subroutine to create an instance of the #{config.name} object.
!
! CALLING SEQUENCE:
!   CALL #{config.name}_Create( &
#{prepend_pad_append_string("!          ",
                            [config.name,config.dim_names].flatten,
                            ", &\n",
                            last="  )")}
!
! OBJECTS:
!   #{config.name}:
!     Structure which is to be created.
!     UNITS:      N/A
!     TYPE:       #{config.type_name}
!     DIMENSION:  Scalar
!     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
  xxxx loop here to create input descriptions xxxx
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE SUBROUTINE #{config.name}_Create( &
    xxxx loop here to create argument list... including self xxxx
    ! Arguments
    xxxx loop here to create argument definitions... including self xxxx
    1. need to align the type.
    2. need to align the intent attributes, e.g.
    TYPE(MWwaterLSCLUT_type), INTENT(OUT) :: self
    INTEGER                 , INTENT(IN)  :: n_Angles             
    INTEGER                 , INTENT(IN)  :: n_Frequencies             
    INTEGER                 , INTENT(IN)  :: n_Temperatures       
    INTEGER                 , INTENT(IN)  :: n_Wind_Speeds             
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    xxxx loop here to create dimension value check, e.g. xxxx
    IF ( n_Angles       < 1 .OR. &
         n_Frequencies  < 1 .OR. &
         n_Temperatures < 1 .OR. &
         n_Wind_Speeds  < 1 ) RETURN

    
    ! Perform the allocation
    xxxx loop here to create the allocation list, e.g. xxxx
    ALLOCATE( self%Angle( n_Angles ), &
              self%Frequency( n_Frequencies ), &
              self%Temperature( n_Temperatures ), &
              self%Wind_Speed( n_Wind_Speeds ), &
              self%lsc_ev( n_Angles, n_Frequencies, n_Temperatures, n_Wind_Speeds ), &
              self%lsc_eh( n_Angles, n_Frequencies, n_Temperatures, n_Wind_Speeds ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    xxxx loop here to create dimension assignment xxxx
    self%n_Angles       = n_Angles     
    self%n_Frequencies  = n_Frequencies
    self%n_Temperatures = n_Temperatures
    self%n_Wind_Speeds  = n_Wind_Speeds
    ! ...Arrays
    xxxx loop here to create array initialisations xxxx
    self%Angle       = ZERO
    self%Frequency   = ZERO
    self%Temperature = ZERO
    self%Wind_Speed  = ZERO
    self%lsc_ev      = ZERO
    self%lsc_eh      = ZERO

    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE #{config.name}_Create
        EOT

        # Output definition in debug mode
        if debug
          puts("\n---BEGIN-DEBUG-OUTPUT---")
          puts("\n#{self.class} #{__method__} method output:")
          puts("\n#{str}")
          puts("\n----END-DEBUG-OUTPUT----")
        end

puts config.dim_names

        str
      end

    end
  end
end
