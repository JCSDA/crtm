#!/usr/bin/env ruby

class FDefMod

  # ===============================
  # The FDefMod regular expressions
  # ===============================
  NAMESPACEREGEXP=%r{
      ^\s*NAMESPACE\s*:\s*
      (\w*)            # Capture the namespace prefix to $1
    }ix
    
  STRUCTBEGINREGEXP=%r{
      ^\s*TYPE\s*::\s*
      (\w*)            # Capture the structure name to $1
    }ix

  STRUCTENDREGEXP=%r{
      ^\s*END\s+TYPE\s*
      (\w*)            # Capture the structure name to $1
    }ix

  DIMREGEXP=%r{
      ^\s*INTEGER\s*::\s* # Only integer types are allowed for dimensions
      (\w*)               # Capture the dimension name to $1
    }ix
    
  COMPONENTREGEXP=%r{
      ^\s*
      # Look for the valid component type
      (REAL|INTEGER|COMPLEX|LOGICAL|CHARACTER)  # Capture the component type to $1
      # Look for the optional parameters or derived typename
      \s*
      (?:                             # Non-capture grouping
        \(                            # kind/len or type paren start
          \s*
          (?:(?:KIND|LEN)\s*=\s*)?    # Keywords are optional
          ([^()]\w+)                  # Capture parameter to $2
          \s*
        \)                            # kind/len or type paren end
      )?                              # The whole group is optional
      # Look for the dimension attribute
      (?:                             # Non-capture grouping
        \s*,\s*DIMENSION\s*           # attribute name
        \(                            # dimlist paren start
          ([-\w:,]+)                  # capture dimension list text to $3
        \)                            # dimlist paren end
      )?                              # The whole group is optional
      # Look for the component variable name
      \s*::\s*                        # The :: separator
      (\w*)                           # Capture the component variable name to $4
      # Look for init val
      (?:                             # Non-capture grouping
        \s*=\s*                       # Assignment operator + whitespace
        (['"'.,()\s\w]*)              # Capture the initialisation value or parameter name to $5
      )?                              # The whole group is optional
      # Look for description
      (?:                             # Non-capture grouping
        \s*!\s*                       # Comment character + whitespace
        (.*)                          # Capture the rest of the line to $6
      )?                              # The whole group is optional
    }ix
  
  # =====================
  # Class variable access
  # =====================
  attr_accessor :nameSpace,:structName,:dimList,:scalarList,:arrayList,:debug

  # =================
  # Class constructor
  # =================
  def initialize(debug=false)
    @nameSpace =""
    @structName=""
    @dimList   =[]
    @scalarList=[]
    @arrayList =[]
    @debug     =debug
    @dfmt=""
    @sfmt=""
    @afmt=""
    @dimdecl=""
  end

  # ========================================
  # Method to process a dimension definition
  # ========================================
  def dimProcess(line)
    dimMatch = DIMREGEXP.match(line)
    unless dimMatch.nil?
      # We have matched a dimension definition
      @dimList<<dimMatch[1]
      puts(@dimList.last.inspect) if @debug
    else
      # No match, so raise an error
      raise StandardError, "Invalid dimension definition"
    end
  end 

  # =====================================
  # Method to process a scalar definition
  # =====================================
  def scalarProcess(line)
    scalarMatch = COMPONENTREGEXP.match(line)
    unless scalarMatch.nil?
      # We have matched a scalar component definition
      @scalarList<<{:type     => scalarMatch[1].upcase,
                    :param    => scalarMatch[2],
                    :name     => scalarMatch[4],
                    :initval  =>(scalarMatch[5].chomp.rstrip unless scalarMatch[5].nil?),
                    :desc     => scalarMatch[6]}
      puts(@scalarList.last.inspect) if @debug
    else
      # No match, so raise an error
      raise StandardError, "Invalid scalar definition"
    end
  end

  # =====================================
  # Method to process an array definition
  # =====================================
  def arrayProcess(line)
    arrayMatch = COMPONENTREGEXP.match(line)
    unless arrayMatch.nil?
      # We have matched an array component definition
      dims=[]                                              # Array of dimension extents
      arrayMatch[3].split(/\s*,\s*/).each do |d|
        thisdim=d.split(/\s*:\s*/)
        thisdim=["1"]+thisdim if thisdim.length == 1
        dims<<thisdim
      end
      ndims =dims.length                                   # The number of dimensions
      dimidx=(1..ndims).to_a.collect {|i| "i"+i.to_s}      # Loop index variables for each dimension
      @arrayList<<{:type     => arrayMatch[1].upcase,
                   :param    => arrayMatch[2],
                   :dims     => dims,
                   :ndims    => ndims,
                   :dimidx   => dimidx,
                   :name     => arrayMatch[4],
                   :desc     => arrayMatch[6]}
      @dimdecl=dimidx if ndims>@dimdecl.length             
      puts(@arrayList.last.inspect) if @debug
    else
      # No match, so raise an error
      raise StandardError, "Invalid array definition"
    end
  end


  # ==============================
  # Method to check the dimensions
  # ==============================
  def dimCheck
    @arrayList.each do |a|
      a[:dims].each do |d|
        dim=/[_a-zA-Z0-9]+/.match(d[1])[0]
        raise(StandardError, "Invalid dimension, #{dim}, specified for #{a[:name]}") unless @dimList.include?(dim)
      end
    end
  end


  # ==========================================
  # Method to parse an fdefmod definition file
  # ==========================================
  def read(file)
    parse(File.open(file,"r").readlines)
  end
  
  
  # ===================================
  # Method to parse fdefmod definitions
  # ===================================
  def parse(lines)

    # Set the component identifier tag
    tag=""
    
    # Parse line by line
    lines.each do |line|
    
      # Retrieve the namespace prefix
      if line =~ NAMESPACEREGEXP
        @nameSpace=$1+"_"
        puts("Namespace prefix: #{@nameSpace}") if @debug
        next
      end
    
      # Retrieve the structure name
      if line =~ STRUCTBEGINREGEXP
        @structName=$1
        puts("Structure name: #{@structName}") if @debug
        next
      end
    
      # Check for definition end
      break if line =~ STRUCTENDREGEXP
    
      # Match fdefmod tags and set switches
      case line
      when /!\s*Dimensions/i
        tag="dim"
        puts("Dimensions") if @debug
        next
      when /!\s*Scalars/i
        tag="scalar"
        puts("Scalar components") if @debug
        next
      when /!\s*Arrays/i
        tag="array"
        puts("Array components") if @debug
        next
      end
    
      # Process structure components
      case tag
      when "dim"
        dimProcess(line)
        next
      when "scalar"
        scalarProcess(line)
        next
      when "array"
        arrayProcess(line)
        next
      end
      
    end

    # Check that all array component dimensions are valid
    dimCheck()
    
    # Convert the dimension declaration
    @dimdecl="INTEGER :: #{@dimdecl.join(",")}"
    
    # Construct component name formats
    @dfmt=nameFmt(@dimList)
    @sfmt=nameFmt(@scalarList.collect {|s| s[:name]})
    @afmt=nameFmt(@arrayList.collect  {|a| a[:name]})
    
  end # def parse


  # ============================================
  # Method to construct the structure definition
  # ============================================
  def structDef
    str=strip_output(<<-EOT)
      TYPE :: #{@nameSpace}#{@structName}_type
        INTEGER :: n_Allocates=0
        #{dimDef()}
        #{scalarDef()}
        #{arrayDef()}
      END TYPE #{@nameSpace}#{@structName}_type
    EOT
  end # def structDef


  # ========================================
  # Method to construct the Clear subroutine
  # ========================================
  def clearSub
    str=strip_output(<<-EOT)
      SUBROUTINE #{@nameSpace}Clear_#{@structName}(#{@structName})
        TYPE(#{@nameSpace}#{@structName}_type), INTENT(IN OUT) :: #{@structName}
        #{scalarClear()}
      END SUBROUTINE #{@nameSpace}Clear_#{@structName}
    EOT
  end # def clearSub
  

  # ===========================================
  # Method to construct the Associated function
  # ===========================================
  def assocFunc

    # Declaration and argument type definition format
    dfmt=nameFmt(["#{@structName}","ANY_Test"])
    afmt=nameFmt(["TYPE(#{@nameSpace}#{@structName}_type)","INTEGER, OPTIONAL"])

    # The function
    str=strip_output(<<-EOT)
      FUNCTION #{@nameSpace}Associated_#{@structName}( &
                 #{dfmt%"#{@structName}"}, &  ! Input
                 #{dfmt%"ANY_Test"}) &  ! Optional input
               RESULT(Association_Status)
        ! Arguments
        #{afmt%"TYPE(#{@nameSpace}#{@structName}_type)"}, INTENT(IN) :: #{@structName}
        #{afmt%"INTEGER, OPTIONAL"}, INTENT(IN) :: ANY_Test
        ! Function result
        LOGICAL :: Association_Status
        ! Local variables
        LOGICAL :: ALL_Test
        
        ! Default is to test ALL the pointer members
        ! for a true association status....
        ALL_Test = .TRUE.
        ! ...unless the ANY_Test argument is set.
        IF ( PRESENT( ANY_Test ) ) THEN
          IF ( ANY_Test == 1 ) ALL_Test = .FALSE.
        END IF
        
        ! Test the structure associations    
        Association_Status = .FALSE.
        IF (ALL_Test) THEN
          #{assocIf("AND")}
        ELSE
          #{assocIf("OR")}
        END IF
        
      END FUNCTION #{@nameSpace}Associated_#{@structName}
    EOT
  end # def assocFunc
  

  # ========================================
  # Method to construct the Destroy function
  # ========================================
  def destroyFunc

    # Declaration and argument type definition format
    dfmt=nameFmt(["#{@structName}","Message_Log"])
    afmt=nameFmt(["TYPE(#{@nameSpace}#{@structName}_type)","CHARACTER(*), OPTIONAL"])

    # The function
    str=strip_output(<<-EOT)
      FUNCTION #{@nameSpace}Destroy_#{@structName}( &
                 #{dfmt%"#{@structName}"}, &  ! Output
                 #{dfmt%"No_Clear"}, &  ! Optional input
                 #{dfmt%"RCS_Id"}, &  ! Revision control
                 #{dfmt%"Message_Log"}) &  ! Error messaging
               RESULT(Error_Status)
        ! Arguments
        #{afmt%"TYPE(#{@nameSpace}#{@structName}_type)"}, INTENT(IN OUT) :: #{@structName}
        #{afmt%"INTEGER,      OPTIONAL"}, INTENT(IN)     :: No_Clear
        #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT)    :: RCS_Id
        #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN)     :: Message_Log
        ! Function result
        INTEGER :: Error_Status
        ! Local parameters
        CHARACTER(*), PARAMETER :: ROUTINE_NAME = '#{@nameSpace}Destroy_#{@structName}'
        ! Local variables
        CHARACTER(256)  :: Message
        LOGICAL :: Clear
        INTEGER :: Allocate_Status
        
        ! Set up
        Error_Status = SUCCESS
        IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
        
        ! Default is to clear scalar members...
        Clear = .TRUE.
        ! ....unless the No_Clear argument is set
        IF ( PRESENT( No_Clear ) ) THEN
          IF ( No_Clear == 1 ) Clear = .FALSE.
        END IF
        
        ! Initialise the scalar members
        IF ( Clear ) CALL #{@nameSpace}Clear_#{@structName}(#{@structName})
        
        ! If ALL pointer members are NOT associated, do nothing
        IF ( .NOT. #{@nameSpace}Associated_#{@structName}(#{@structName}) ) RETURN
        
        ! Deallocate the pointer members
        #{deallocStatement()}
        IF ( Allocate_Status /= 0 ) THEN
          WRITE( Message, '("Error deallocating #{@structName}. STAT = ",i0)') &
                          Allocate_Status
          #{failReturn(6,"TRIM(Message)",:lstrip=>true)}
        END IF

        ! Reset the dimension indicators
        #{dimClear()}
        
        ! Decrement and test allocation counter
        #{@structName}%n_Allocates = #{@structName}%n_Allocates - 1
        IF ( #{@structName}%n_Allocates /= 0 ) THEN
          WRITE( Message, '("Allocation counter /= 0, Value = ",i0)') &
                          #{@structName}%n_Allocates
          #{failReturn(6,"TRIM(Message)",:lstrip=>true)}
        END IF
        
      END FUNCTION #{@nameSpace}Destroy_#{@structName}
    EOT
  end # def destroyFunc
  

  # =========================================
  # Method to construct the Allocate function
  # =========================================
  def allocFunc

    # Declaration and argument type definition format
    dfmt=nameFmt(["#{@structName}","Message_Log"]+@dimList)
    afmt=nameFmt(["TYPE(#{@nameSpace}#{@structName}_type)","CHARACTER(*), OPTIONAL"])

    # The function
    str=strip_output(<<-EOT)
      FUNCTION #{@nameSpace}Allocate_#{@structName}( &
                 #{dimFuncDef(dfmt)}
                 #{dfmt%"#{@structName}"}, &  ! Output
                 #{dfmt%"RCS_Id"}, &  ! Revision control
                 #{dfmt%"Message_Log"}) &  ! Error messaging
               RESULT( Error_Status )
        ! Arguments
        #{dimArgDef(afmt)}
        #{afmt%"TYPE(#{@nameSpace}#{@structName}_type)"}, INTENT(IN OUT) :: #{@structName}
        #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT)    :: RCS_Id
        #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN)     :: Message_Log
        ! Function result
        INTEGER :: Error_Status
        ! Local parameters
        CHARACTER(*), PARAMETER :: ROUTINE_NAME = '#{@nameSpace}Allocate_#{@structName}'
        ! Local variables
        CHARACTER(256)  :: Message
        INTEGER :: Allocate_Status
        
        ! Set up
        Error_Status = SUCCESS
        IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
        
        ! Check dimensions
        #{dimCheckIf()}
        
        ! Check if ANY pointers are already associated.
        ! If they are, deallocate them but leave scalars.
        IF ( #{@nameSpace}Associated_#{@structName}( #{@structName}, ANY_Test=1 ) ) THEN
          Error_Status = #{@nameSpace}Destroy_#{@structName}( &
                           #{@structName}, &
                           No_Clear=1, &
                           Message_Log=Message_Log )
          IF ( Error_Status /= SUCCESS ) THEN
            #{failReturn(8,"'Error deallocating #{@structName} prior to allocation.'",:lstrip=>true)}
          END IF
        END IF
        
        ! Perform the pointer allocation
        #{arrayAlloc()}
        
        ! Assign the dimensions
        #{dimAssign()}
        
        ! Initialise the arrays
        #{arrayInit()}
        
        ! Increment and test the allocation counter
        #{@structName}%n_Allocates = #{@structName}%n_Allocates + 1
        IF ( #{@structName}%n_Allocates /= 1 ) THEN
          WRITE( Message, '("Allocation counter /= 1, Value = ",i0)') &
                          #{@structName}%n_Allocates
          #{failReturn(6,"TRIM(Message)",:lstrip=>true)}
        END IF
        
      END FUNCTION #{@nameSpace}Allocate_#{@structName}
    EOT
  end # def allocFunc
  

  # =======================================
  # Method to construct the Assign function
  # =======================================
  def assignFunc

    # Declaration and argument type definition format
    dfmt=nameFmt(["#{@structName}_out","Message_Log"])
    afmt=nameFmt(["TYPE(#{@nameSpace}#{@structName}_type)","CHARACTER(*), OPTIONAL"])

    # Create the function
    str=strip_output(<<-EOT)
      FUNCTION #{@nameSpace}Assign_#{@structName}( &
                 #{dfmt%"#{@structName}_in"}, &  ! Input
                 #{dfmt%"#{@structName}_out"}, &  ! Output
                 #{dfmt%"RCS_Id"}, &  ! Revision control
                 #{dfmt%"Message_Log"}) &  ! Error messaging
               RESULT( Error_Status )
        ! Arguments
        #{afmt%"TYPE(#{@nameSpace}#{@structName}_type)"}, INTENT(IN)     :: #{@structName}_in
        #{afmt%"TYPE(#{@nameSpace}#{@structName}_type)"}, INTENT(IN OUT) :: #{@structName}_out
        #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT)    :: RCS_Id
        #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN)     :: Message_Log
        ! Function result
        INTEGER :: Error_Status
        ! Local parameters
        CHARACTER(*), PARAMETER :: ROUTINE_NAME = '#{@nameSpace}Assign_#{@structName}'
        ! Local variables
        #{@dimdecl}

        ! Set up
        Error_Status = SUCCESS
        IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

        ! ALL *input* pointers must be associated
        #{structAssocTest("_In")}
        
        ! Allocate data arrays
        #{allocStruct()}
        
        ! Assign non-dimension scalar members
        #{scalarAssign()}
        
        ! Copy array data
        #{arrayAssign()}
        
      END FUNCTION #{@nameSpace}Assign_#{@structName}
    EOT
  end # def assignFunc
  

  # ======================================
  # Method to construct the Equal function
  # ======================================
  def equalFunc

    # Declaration and argument type definition format
    dfmt=nameFmt(["#{@structName}_LHS","Message_Log"])
    afmt=nameFmt(["TYPE(#{@nameSpace}#{@structName}_type)","CHARACTER(*), OPTIONAL"])
    
    # Create the function
    str=strip_output(<<-EOT)
      FUNCTION #{@nameSpace}Equal_#{@structName}( &
                 #{dfmt%"#{@structName}_LHS"}, &  ! Input
                 #{dfmt%"#{@structName}_RHS"}, &  ! Input
                 #{dfmt%"ULP_Scale"}, &  ! Optional input
                 #{dfmt%"RCS_Id"}, &  ! Revision control
                 #{dfmt%"Message_Log"}) &  ! Error messaging
               RESULT( Error_Status )
        ! Arguments
        #{afmt%"TYPE(#{@nameSpace}#{@structName}_type)"}, INTENT(IN)  :: #{@structName}_LHS
        #{afmt%"TYPE(#{@nameSpace}#{@structName}_type)"}, INTENT(IN)  :: #{@structName}_RHS
        #{afmt%"INTEGER,      OPTIONAL"}, INTENT(IN)  :: ULP_Scale
        #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT) :: RCS_Id
        #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN)  :: Message_Log
        ! Function result
        INTEGER :: Error_Status
        ! Local parameters
        CHARACTER(*), PARAMETER :: ROUTINE_NAME = '#{@nameSpace}Equal_#{@structName}'
        ! Local variables
        CHARACTER(256)  :: Message
        INTEGER :: ULP
        INTEGER :: l, j
        #{@dimdecl}

        ! Set up
        Error_Status = SUCCESS
        IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

        ! Default precision is a single unit in last place
        ULP = 1
        ! ... unless the ULP_Scale argument is set and positive
        IF ( PRESENT( ULP_Scale ) ) THEN
          IF ( ULP_Scale > 0 ) ULP = ULP_Scale
        END IF

        ! Check the structure association status
        #{structAssocTest("_LHS")}
        #{structAssocTest("_RHS")}
        
        ! Check dimensions
        #{dimEqualTest()}

        ! Check the scalar components
        #{scalarEqualTest()}
        
        ! Check the array components
        #{arrayEqualTest()}
        
      END FUNCTION #{@nameSpace}Equal_#{@structName}
    EOT
  end # def equalFunc


  # =======================================
  # Method to construct the Info subroutine
  # =======================================
  def infoSub

    # The indents
    i0,i1=indent(0),indent(1)

    # Construct output format string
    infoFmt=i1+"FmtString='(a,1x,\"#{@structName}:\",2x,&\n"
    @dimList.each {|d| infoFmt<<indent(7)+"&\"#{d.upcase}=\",i0,2x,&\n"}
    infoFmt<<indent(7)+"&)'"
    
    # Output dimension list for write
    infoDim=indent(2)+"ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &\n"+
            @dimList.collect {|d| indent(2)+"#{@structName}%#{@dfmt%d}"}.join(", &\n")             
    
    # Declaration and argument type definition format
    dfmt=nameFmt(["#{@structName}","RCS_Id"])
    afmt=nameFmt(["TYPE(#{@nameSpace}#{@structName}_type)","CHARACTER(*), OPTIONAL"])
    
    # Create the function
    str=<<EOF
  SUBROUTINE #{@nameSpace}Info_#{@structName}( &
               #{dfmt%"#{@structName}"}, &
               #{dfmt%"Info"}, &  ! Output
               #{dfmt%"RCS_Id"}  )  ! Revision control
    ! Arguments
    #{afmt%"TYPE(#{@nameSpace}#{@structName}_type)"}, INTENT(IN)  :: #{@structName}
    #{afmt%"CHARACTER(*)"}, INTENT(OUT) :: Info
    #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT) :: RCS_Id
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(80)  :: FmtString
    CHARACTER(512) :: LongString

    ! Set up
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Write the required info to the local string
#{infoFmt}
    WRITE( LongString, FMT=FmtString ) &
#{infoDim}

    ! Trim the output based on the
    ! dummy argument string length
    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

  END SUBROUTINE #{@nameSpace}Info_#{@structName}
EOF

    # Done
    str    
  end # def infoSub


  # -------------------------------------  
  # Method to construct the Define module
  # -------------------------------------  
  def createModule(fdefmodFile)

    # Read the definition file
    self.read(fdefmodFile)
    
    # Create the module
    str=<<EOF
!
! #{@nameSpace}#{@structName}_Define
!
! Module defining the #{@nameSpace}#{@structName} data structure and containing routines to 
! manipulate it.
!
! CREATION HISTORY:
!       This file was initially automatically generated so edit at your own risk.
!       Generated by #{File.basename(__FILE__)} on #{Time.now.strftime('%d-%b-%Y at %X')}
!       Contact info:  Paul van Delst, CIMSS/SSEC
!                      paul.vandelst@ssec.wisc.edu
!

MODULE #{@nameSpace}#{@structName}_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,            ONLY: fp=>fp_kind
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! #{@structName} data structure definition
  PUBLIC :: #{@nameSpace}#{structName}_type
  ! Structure procedures
  PUBLIC :: #{@nameSpace}Associated_#{structName}
  PUBLIC :: #{@nameSpace}Destroy_#{structName}
  PUBLIC :: #{@nameSpace}Allocate_#{structName}
  PUBLIC :: #{@nameSpace}Assign_#{structName}
  PUBLIC :: #{@nameSpace}Equal_#{structName}
  PUBLIC :: #{@nameSpace}Info_#{structName}
    

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: $'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp


  ! -----------------------
  ! Derived type definition
  ! -----------------------
#{structDef}

CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

#{clearSub}


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

#{assocFunc}

#{destroyFunc}

#{allocFunc}

#{assignFunc}

#{equalFunc}

#{infoSub}
END MODULE #{@nameSpace}#{@structName}_Define
EOF
  

    # Done
    str    
  end # def createModule
  
  
# =======
# ####  private
# =======

  # ------------------------
  # structDef helper methods
  # ------------------------
  # Method to construct the dimension definitions
  # in the structure definition function
  def dimDef
    str="! Dimensions\n"
    @dimList.each {|d| str<<"    INTEGER :: #{d}=0\n"}
    str.chomp!  # Remove the last newline
  end # def dimDef
  
  # Method to construct the scalar component definitions
  # in the structure definition function
  def scalarDef
    unless @scalarList.empty?
      # Get the pretty print output format for the type definitions
      type_def=@scalarList.collect {|s| %Q{#{s[:type]}#{"(#{s[:param]})" unless s[:param].nil?}}}
      fmt=nameFmt(type_def)
      # Construct the scalar definitions
      str="! Scalars\n"
      @scalarList.each_index do |i|
        str<<"    #{fmt%type_def[i]} :: #{@scalarList[i][:name]}"
        str<<" = #{@scalarList[i][:initval]}" unless @scalarList[i][:initval].nil?
        str<<" ! #{@scalarList[i][:desc]}" unless @scalarList[i][:desc].nil?
        str<<"\n"
      end
    else
      str=""
    end
    str.chomp!  # Remove the last newline
  end # def scalarDef
  
  # Method to construct the array component definitions
  # in the structure definition function
  def arrayDef
      # Get the pretty print output format for the type definitions
    type_def=@arrayList.collect {|s| %Q{#{s[:type]}#{"(#{s[:param]})" unless s[:param].nil?}}}
    fmt=nameFmt(type_def)
    # Construct the array definitions
    str="! Arrays\n"
    @arrayList.each_index do |i|
      str<<"    #{fmt%type_def[i]}"
      str<<", DIMENSION("<<([":"]*@arrayList[i][:ndims]).join(",")<<")"
      str<<", POINTER :: #{@arrayList[i][:name]}=>NULL()"
      str<<" ! #{@arrayList[i][:desc]}" unless @arrayList[i][:desc].nil?
      str<<"\n"
    end
    str.chomp!  # Remove the last newline
  end # def arrayDef

  # -----------------------
  # clearSub helper methods
  # -----------------------
  # Method to construct the scalar component reinitialisation statements
  # in the clear subroutine
  def scalarClear
    str=""
    scalarList.each {|s| str<<"    #{@structName}%#{@sfmt%s[:name]}=#{s[:initval]}\n" unless s[:initval].nil?}
    str.lstrip!.chomp!  # Strip leading spaces of first line, newline of last line
  end # def scalarClear

  # ------------------------
  # assocFunc helper methods
  # ------------------------
  # Method to generate the IF statements
  # in the associated function
  def assocIf(operator)
    if @arrayList.length>1
      str="IF (ASSOCIATED(#{@structName}%#{@afmt%@arrayList.first[:name]}) .#{operator}. &\n"
      1.upto(@arrayList.length-2) {|i| str<<indent(10)+"ASSOCIATED(#{@structName}%#{@afmt%@arrayList[i][:name]}) .#{operator}. &\n"}
      str<<indent(10)+"ASSOCIATED(#{@structName}%#{@afmt%@arrayList.last[:name]})) THEN\n"
    else
      str="IF (ASSOCIATED(#{@structName}%#{@arrayList.first[:name]})) THEN\n"  
    end
    str<<indent(8)+ "Association_Status = .TRUE.\n"
    str<<indent(6)+ "END IF"
  end # def assocIf


  # --------------------------
  # destroyFunc helper methods
  # --------------------------
  # Method to construct the array component deallocation statement
  # in the destroy function
  def deallocStatement
    str="DEALLOCATE( #{@structName}%#{@afmt%@arrayList.first[:name]}, &\n"
    1.upto(@arrayList.length-1) do |i|
      str<<indent(16)+"#{@structName}%#{@afmt%@arrayList[i][:name]}, &\n"
    end
    str<<indent(16)+"STAT = Allocate_Status )"
  end # def deallocStatement

  # Method to construct the dimension reinitialisation statements
  # in the destroy funciton
  def dimClear
    str=""
    @dimList.each {|d| str<<"    #{@structName}%#{@dfmt%d}=0\n"}
    str.lstrip!.chomp! # Strip leading spaces of first line, newline of last line
  end # def dimClear


  # ------------------------
  # allocFunc helper methods
  # ------------------------
  # method to construct the dimension argument entries
  # in the allocate function definition
  def dimFuncDef(d_fmt)
    str=""
    @dimList.each {|d| str<<indent(13)+"#{d_fmt%d}, &  ! Input\n"}
    str.lstrip.chomp  # Strip leading spaces of first line, newline of last line
  end

  # Method to construct the dimension argument definitions
  # in the allocate function
  def dimArgDef(a_fmt)
    str=""
    @dimList.each {|d| str<<indent(4)+a_fmt%"INTEGER"+", INTENT(IN)     :: #{d}\n"}
    str.lstrip.chomp  # Strip leading spaces of first line, newline of last line
  end

  # Generate the dimension value check statement
  # in the allocate function
  def dimCheckIf
    cmd="IF ("; n=cmd.length
    if @dimList.length==1
      cmd<<"#{@dfmt%@dimList.first} < 1) THEN\n"
    else
      str=""
      @dimList[0..-2].each {|d| str<<indent(n+4)+"#{@dfmt%d} < 1 .OR. &\n"}
      str<<indent(n+4)+"#{@dfmt%@dimList.last} < 1) THEN\n"
      cmd<<str.lstrip
    end
    cmd<<failReturn(6,"'Input #{@structName} dimensions must all be > 0.'")+"\n"
    cmd<<indent(4)+"END IF"
  end # def dimCheckIf

  # Method to construct the allocate statement 
  # in the allocate function
  def dimExtDef(a)
    a[:dims].collect {|d| d.join(":")}.join(",")
  end
  def arrayAlloc
    cmd="ALLOCATE( "; n=cmd.length
    str=""
    @arrayList.each do |a|
      str<<indent(n+4)+%Q{#{@structName}%#{a[:name]}(#{dimExtDef(a)}), &\n}
    end
    cmd<<str.lstrip
    cmd<<indent(n+4)+%Q{STAT = Allocate_Status )\n}
    cmd<<indent(4)+%Q{IF ( Allocate_Status /= 0 ) THEN\n}
    cmd<<indent(4)+%Q{  WRITE( Message, '("Error allocating #{@structName} data arrays. STAT = ",i0)') &\n}
    cmd<<indent(4)+%Q{                  Allocate_Status\n}
    cmd<<failReturn(6,"TRIM(Message)")+"\n"
    cmd<<indent(4)+%Q{END IF}
  end

  # Method to construct the dimension assignment statements
  # in the allocate function
  def dimAssign
    str=""
    @dimList.each {|d| str<<"    #{@structName}%#{@dfmt%d} = #{d}\n"}
    str.lstrip.chomp  # Strip leading spaces of first line, newline of last line
  end

  # Method to construct the array initialisation statemnts
  # in the allocate function
  def arrayInit
    str=""
    @arrayList.each do |a|
      # Only intrinsic types are initialisaed
      next if a[:type] == "TYPE"
      # Construct the initialistions
      str<<"    #{@structName}%#{@afmt%a[:name]} = " 
      str<< case a[:type]
              when "INTEGER"   then "0\n"
              when "REAL"      then "ZERO\n"
              when "COMPLEX"   then "CMPLX(ZERO,ZERO)\n"
              when "CHARACTER" then "' '\n"
              when "LOGICAL"   then ".FALSE.\n"
              else raise StandardError, "*** Type #{a[:type]} array assign not yet implemented ***"
            end
    end
    str.lstrip.chomp  # Strip leading spaces of first line, newline of last line
  end # def arrayInit


  # -------------------------
  # assignFunc helper methods
  # -------------------------
  # Method to construct the output structure allocation statement
  # in the assign function
  def allocStruct
    cmd="Error_Status = #{@nameSpace}Allocate_#{@structName}( "; n=cmd.length
    str=""
    @dimList.each do |d|
      str<<indent(n+4)+"#{@structName}_in%#{@dfmt%d}, &\n"
    end
    cmd<<str.lstrip
    cmd<<indent(n+4)+"#{@structName}_out, &\n"+
         indent(n+4)+"Message_Log=Message_Log )\n"
    cmd<<indent(4)+"IF ( Error_Status /= SUCCESS ) THEN\n"
    cmd<<failReturn(6,"'Error allocating output structure'")+"\n"
    cmd<<indent(4)+"END IF"
  end # def allocStruct

  # Method to construct the scalar component assignment statements
  # in the assign function
  def scalarAssign
    str=""
    @scalarList.each {|s| str<<"    #{@structName}_out%#{@sfmt%s[:name]} = #{@structName}_in%#{s[:name]}\n"}
    str.lstrip.chomp  # Strip leading spaces of first line, newline of last line
  end # def scalarAssign

  # Method to construct the array component assignment statements
  # in the assign function
  def arrayAssign
    str=""
    @arrayList.each {|a| str<<"    #{@structName}_out%#{@afmt%a[:name]} = #{@structName}_in%#{a[:name]}\n"}
    str.lstrip.chomp  # Strip leading spaces of first line, newline of last line
  end # def arrayAssign


  # ------------------------
  # equalFunc helper methods
  # ------------------------
  # Method to construct the structure association IF tests
  # in the equal function
  def structAssocTest(id="")
    cmd ="IF ( .NOT. #{@nameSpace}Associated_#{@structName}( #{@structName}#{id} ) ) THEN\n"
    cmd<<failReturn(6,"'Some or all INPUT #{@structName}#{id} pointer members are NOT associated.'")+"\n"
    cmd<<indent(4)+"END IF"
  end # def structAssocTest
  
  # Method to construct the dimension equality IF test
  # in the equal function
  def dimEqualTest
    cmd="IF ("; n=cmd.length
    if @dimList.length==1
      cmd<<"#{@structName}_LHS%#{@dfmt%@dimList.first} /= #{@structName}_RHS%#{@dfmt%@dimList.first}) THEN\n"
    else
      str=""
      @dimList[0..-2].each {|d| str<<indent(n+4)+"#{@structName}_LHS%#{@dfmt%d} /= #{@structName}_RHS%#{@dfmt%d} .OR. &\n"}
      str<<indent(n+4)+"#{@structName}_LHS%#{@dfmt%@dimList.last} /= #{@structName}_RHS%#{@dfmt%@dimList.last} ) THEN\n"
      cmd<<str.lstrip
    end
    cmd<<failReturn(6,"'Structure dimensions are different.'")+"\n"
    cmd<<indent(4)+"END IF"
  end # def dimEqualTest

  # Method to construct the scalar component equality checks
  # in the equal function
  def scalarEqualTest
    cmd=""
    @scalarList.each do |s|
      case s[:type]
        when "INTEGER","CHARACTER","LOGICAL"
          # Integers, characters, and logicals can be directly compared
          op = s[:type]=="LOGICAL" ? ".EQV" : "/="
          str =indent(4)+"IF ( #{@structName}_LHS%#{s[:name]} #{op} #{@structName}_RHS%#{s[:name]} ) THEN\n"
          str<<failReturn(6,"'#{@structName} scalar component #{s[:name]} values are different.'")+"\n"
          str<<indent(4)+"END IF\n"
          cmd<<str
        when "REAL","COMPLEX"
          # Floating point values use Compare_Float
          str =indent(4)+"IF ( .NOT. Compare_Float( #{@structName}_LHS%#{s[:name]}, &\n"
          str<<indent(4)+"                          #{@structName}_RHS%#{s[:name]}, &\n"
          str<<indent(4)+"                          ULP=ULP ) ) THEN\n"
          str<<failReturn(6,"'#{@structName} scalar component #{s[:name]} values are different.'")+"\n"
          str<<indent(4)+"END IF\n"
          cmd<<str
        else
          # Other data types not supported
          raise StandardError, "*** Type #{s[:type]} scalar equality test not yet implemented ***"
      end
    end
    cmd.lstrip.chomp  # Strip leading spaces of first line, newline of last line
  end # def scalarEqualTest

  # Method to construct the array component equality checks
  # in the equal function
  #
  # Build the loop DO statements
  def beginLoop(a)
    cmd=""
    0.upto(a[:ndims]-1) do |i|
      cmd<<indent(4+(i*2))+"DO #{a[:dimidx].reverse[i]}=#{a[:dims].reverse[i][0]},#{a[:dims].reverse[i][1]}\n"
    end
    cmd
  end # def beginLoop
  #
  # Build the loop END DO statements
  def endLoop(a)
    n=a[:ndims]
    cmd=""
    (n-1).downto(0) do |i|
      cmd<<indent(4+(i*2))+"END DO\n"
    end
    cmd
    # Restore the inner loop indent after lstripping for insertion into code
#    indent((n-1)*2,:nopad=>true)+(cmd.lstrip)
  end # def endLoop
  #
  # The main method
  def arrayEqualTest
    cmd=""
    @arrayList.each do |a|
      n=a[:ndims]
      case a[:type]
        when "INTEGER","CHARACTER","LOGICAL"
          # Integers, characters, and logicals can be directly compared
          op = a[:type]=="LOGICAL" ? ".EQV" : "/="
          str =indent(4)+"IF ( ANY( #{@structName}_LHS%#{a[:name]} #{op} #{@structName}_RHS%#{a[:name]} ) ) THEN\n"
          str<<failReturn(6,%Q{'#{@structName} array component #{a[:name]} values are different.'})+"\n"
          str<<indent(4)+"END IF\n"
          cmd<<str
        when "REAL","COMPLEX"
          # Floating point values use Compare_Float
          cmd<<beginLoop(a)
          i=indent(4+(n*2))
          str ="#{i}IF ( .NOT. Compare_Float( #{@structName}_LHS(#{a[:dimidx].join(",")})%#{a[:name]}, &\n"
          str<<"#{i}                          #{@structName}_RHS(#{a[:dimidx].join(",")})%#{a[:name]}, &\n"
          str<<"#{i}                          ULP=ULP ) ) THEN\n"
          str<<%Q{#{i}  WRITE( Message, '("#{a[:name]} values are different at index (",#{a[:ndims]}(1x,i0),")")') &\n}
          str<<"#{i}                  #{a[:dimidx].join(",")}\n"
          str<<failReturn(6+(n*2),"TRIM(Message)")+"\n"
          str<<"#{i}END IF\n"
          cmd<<str
          cmd<<endLoop(a)
        else
          # Non-implemented types
          raise StandardError, "*** Type #{a[:type]} array equality test not yet implemented ***"
      end
    end
    cmd.lstrip.chomp  # Strip leading spaces of first line, newline of last line
  end # def arrayEqualTest


  # ----------------------------
  # Miscellaneous helper methods
  # ----------------------------
  # Generate formats for list of names
  def nameFmt(nameList)
    strLen=nameList.inject(0) {|memo,n| memo >= n.length ? memo : n.length}
    "%-#{strLen}.#{strLen}s"
  end
  
  # Method to replace only the first occurance of the leading spaces
  # in each line of input text.
  # Arguments:
  #   :rstr   => string      the replacement string. Default is "  "
  #   :lstrip => true        strip all left spaces from the first line
  def strip_output(text,args={})
    rstr = args[:rstr] ? args[:rstr] : "  "
    text =~ /^\s+/
    leading_spaces = $&
    text = text.to_a.collect {|l| l.sub(/^#{leading_spaces}/,rstr)}.to_s
    args[:lstrip] ? text.lstrip : text
  end
  
  # Generate indent levels for code
  def indent(nspaces,args={})
    npad = args[:nopad] ? 0 : 4 # 4 accounts for strip_output
    " "*(nspaces+npad)
  end # def indent

  # Generate the error message code block
  def failReturn(nspaces,message,args={})
    str=(<<-EOT
      Error_Status = FAILURE
      CALL Display_Message( &
             ROUTINE_NAME, &
             #{message}, &
             Error_Status, &
             Message_Log=Message_Log )
      RETURN
    EOT
    ).chomp
    strip_output(str,:rstr=>indent(nspaces,:nopad=>args[:nopad]),:lstrip=>args[:lstrip])
  end # def failReturn

end # class FDefMod
