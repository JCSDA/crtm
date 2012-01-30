require 'fortran'
require 'define/base'
module SrcGen
  module Define

    class Create < SrcGen::Define::Base

      BASE_INDENT = " "*4
      
      
      # Method to generate the creation procedure
      def generate(debug=false)

        str = <<-EOT


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   #{config.name}_Create
!
! PURPOSE:
!   Elemental subroutine to create an instance of the #{config.name} object.
!
! CALLING SEQUENCE:
!   CALL #{config.name}_Create( &
#{doc_calling_sequence}
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
#{doc_inputs}
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE #{config.name}_Create( &
#{interface_arglist}
    ! Arguments
#{interface_argdef}
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
#{body_check_input}

    
    ! Perform the allocation
#{body_allocation}


    ! Initialise
#{body_initialise}


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

        str
      end

    private

      def doc_calling_sequence
        sandwich_string("!          ",
                        [config.name,config.dim_names],
                        ", &\n",
                        last="  )").join
      end
    
      def doc_inputs
        input_doc_string = ""
        config.dim_names.each_with_index do |name,index|
          str = <<-EOT
!   #{name}:
!     #{config.dimensions[index][:description]}
!     UNITS:      N/A
!     TYPE:       INTEGER
!     DIMENSION:  Scalar
!     ATTRIBUTES: INTENT(IN)
! 
          EOT
          input_doc_string << str
        end
        input_doc_string.chomp
      end
      
      def interface_arglist
        append_array = ["  ! Output\n"] + ["  ! Input\n"]*config.n_dimensions
        arglist_string = sandwich_string(BASE_INDENT,
                                         ["self",config.dim_names],
                                         ", &\n",
                                         last="  )").join
      end
      
      def interface_argdef
        type_list = ["TYPE(#{config.type_name})"] + ["INTEGER"]*config.n_dimensions
        str = sandwich_string(BASE_INDENT,type_list,", ")
        intent_list = ["INTENT(OUT)"] + ["INTENT(IN)"]*config.n_dimensions
        str = sandwich_string(str,intent_list," :: ")
        arg_list = ["self",config.dim_names]
        str = sandwich_string(str,arg_list,"\n",last="").join
      end

      def body_check_input
        leader = "IF ( "; space = " "*leader.length
        check_string = ["#{leader}#{config.dim_names[0]}"] + 
                       prepend_string(config.dim_names[1..-1],space)
        check_string = sandwich_string(BASE_INDENT,
                                       check_string,
                                       " < 1 .OR. &\n",
                                       last=" < 1 ) RETURN").join
      end

      def body_allocation
        leader = "ALLOCATE( "
        alloc_string = []
        config.dimvectors.each do |v|
          alloc_string << "#{v[:name]}( #{config.component_dim_list(v)} ), &"
        end
        config.arraydata.each do |a|
          alloc_string << "#{a[:name]}( #{config.component_dim_list(a)} ), &"
        end
        alloc_string << "STAT = alloc_stat )"
        alloc_string = ["#{leader}#{alloc_string[0]}"] + 
                       prepend_string(alloc_string[1..-1]," "*leader.length)
        alloc_string << "IF ( alloc_stat /= 0 ) RETURN"
        alloc_string = sandwich_string(BASE_INDENT,
                                       alloc_string,
                                       "\n",
                                       last="").join
      end
      
      def body_initialise
        init_string = ["! ...Dimensions"]
        str = sandwich_string("self%",config.dim_names," = ")
        str = append_string(str,config.dim_names)
        init_string = init_string + str + 
                      ["! ...Dimension vectors"]
        str = sandwich_string("self%",config.dimvec_names," = ZERO")
        init_string = init_string + str + 
                      ["! ...Arrays"]
        str = sandwich_string("self%",config.array_names," = ZERO")
        init_string = init_string + str
        init_string = sandwich_string(BASE_INDENT,
                                      init_string,
                                      "\n",
                                      last="").join
      end
      
    end
  end
end
