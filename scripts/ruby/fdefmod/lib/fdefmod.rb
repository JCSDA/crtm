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
      # Look for the component type
      (REAL|INTEGER|COMPLEX|LOGICAL|CHARACTER|TYPE)  # Capture the component type to $1
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
                    :typename =>(scalarMatch[2].split("_")[0] if scalarMatch[1].upcase == "TYPE"),
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
                   :typename =>(arrayMatch[2].split("_")[0] if arrayMatch[1].upcase == "TYPE"),
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
    dimCheck
    
    # Convert the dimension declaration
    @dimdecl=indent(1)+"INTEGER :: #{@dimdecl.join(",")}"
    
    # Construct component name formats
    @dfmt=nameFmt(@dimList)
    @sfmt=nameFmt(@scalarList.collect {|s| s[:name]})
    @afmt=nameFmt(@arrayList.collect  {|a| a[:name]})
    
  end # def parse


  # ============================================
  # Method to construct the structure definition
  # ============================================
  def structDef

    # Dimensions
    dimDef="    ! Dimensions\n"
    @dimList.each {|d| dimDef<<"    INTEGER :: #{d}=0\n"}
    
    # -------
    # Scalars
    # -------
    # Get the pretty print output format for the type definitions
    scalarTypeDef=@scalarList.collect {|s| %Q{#{s[:type]}#{"(#{s[:param]})" unless s[:param].nil?}}}
    sFmt=nameFmt(scalarTypeDef)
    # Construct the scalar definitions
    scalarDef="    ! Scalars\n"
    @scalarList.each_index do |i|
      scalarDef<<"    #{sFmt%scalarTypeDef[i]}"
      scalarDef<<" :: #{@scalarList[i][:name]}"
      scalarDef<<" = #{@scalarList[i][:initval]}" unless @scalarList[i][:initval].nil?
      scalarDef<<" ! #{@scalarList[i][:desc]}" unless @scalarList[i][:desc].nil?
      scalarDef<<"\n"
    end
    
    # ------
    # Arrays
    # ------
    # Get the pretty print output format for the type definitions
    arrayTypeDef=@arrayList.collect {|s| %Q{#{s[:type]}#{"(#{s[:param]})" unless s[:param].nil?}}}
    aFmt=nameFmt(arrayTypeDef)
    # Construct the array definitions
    arrayDef="    ! Arrays\n"
    @arrayList.each_index do |i|
      arrayDef<<"    #{aFmt%arrayTypeDef[i]}"
      arrayDef<<", DIMENSION("<<([":"]*@arrayList[i][:ndims]).join(",")<<")"
      arrayDef<<", POINTER :: #{@arrayList[i][:name]}=>NULL()"
      arrayDef<<" ! #{@arrayList[i][:desc]}" unless @arrayList[i][:desc].nil?
      arrayDef<<"\n"
    end

    # The definition
    str=<<EOF
  TYPE :: #{@nameSpace}#{@structName}_type
    INTEGER :: n_Allocates=0
#{dimDef.chomp}
#{"#{scalarDef.chomp}" unless @scalarList.empty?}
#{arrayDef.chomp}
  END TYPE #{@nameSpace}#{@structName}_type
EOF

    # Done
    str
  end # def structDef


  # ========================================
  # Method to construct the Clear subroutine
  # ========================================
  def clearSub

    # The scalar clear statements
    scalarClear=""
    scalarList.each {|s| scalarClear<<"    #{@structName}%#{@sfmt%s[:name]}=#{s[:initval]}\n" unless s[:initval].nil?}

    # The subroutine
    str=<<EOF
  SUBROUTINE #{@nameSpace}Clear_#{@structName}(#{@structName})
    TYPE(#{@nameSpace}#{@structName}_type), INTENT(IN OUT) :: #{@structName}
#{scalarClear.chomp}
  END SUBROUTINE #{@nameSpace}Clear_#{@structName}
EOF

    # Done
    str
  end # def clearSub
  

  # ===========================================
  # Method to construct the Associated function
  # ===========================================
  def assocFunc

    # Declaration and argument type definition format
    dFmt=nameFmt(["#{@structName}","ANY_Test"])
    aFmt=nameFmt(["TYPE(#{@nameSpace}#{@structName}_type)","INTEGER, OPTIONAL"])

    # The function
    str=<<EOF
  FUNCTION #{@nameSpace}Associated_#{@structName}( &
             #{dFmt%"#{@structName}"}, &  ! Input
             #{dFmt%"ANY_Test"}) & ! Optional input
           RESULT(Association_Status)
    ! Arguments
    #{aFmt%"TYPE(#{@nameSpace}#{@structName}_type)"}, INTENT(IN) :: #{@structName}
    #{aFmt%"INTEGER, OPTIONAL"}, INTENT(IN) :: ANY_Test
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
EOF

    # Done
    str    
  end # def assocFunc
  

  # ========================================
  # Method to construct the Destroy function
  # ========================================
  def destroyFunc

    # Declaration and argument type definition format
    dFmt=nameFmt(["#{@structName}","Message_Log"])
    aFmt=nameFmt(["TYPE(#{@nameSpace}#{@structName}_type)","CHARACTER(*), OPTIONAL"])

    # The array component deallocation statement
    deallocStatement="    DEALLOCATE( #{@structName}%#{@afmt%@arrayList.first[:name]}, &\n"
    1.upto(@arrayList.length-1) do |i|
      deallocStatement<<indent(7)+"#{@structName}%#{@afmt%@arrayList[i][:name]}, &\n"
    end
    deallocStatement<<indent(7)+"STAT = Allocate_Status )"
    
    # The dimension clear statements
    dimClear=""
    @dimList.each {|d| dimClear<<"    #{@structName}%#{@dfmt%d}=0\n"}

    # The function
    str=<<EOF
  FUNCTION #{@nameSpace}Destroy_#{@structName}( &
             #{dFmt%"#{@structName}"}, &  ! Output
             #{dFmt%"No_Clear"}, &  ! Optional input
             #{dFmt%"RCS_Id"}, &  ! Revision control
             #{dFmt%"Message_Log"}) &  ! Error messaging
           RESULT(Error_Status)
    ! Arguments
    #{aFmt%"TYPE(#{@nameSpace}#{@structName}_type)"}, INTENT(IN OUT) :: #{@structName} 
    #{aFmt%"INTEGER,      OPTIONAL"}, INTENT(IN)     :: No_Clear
    #{aFmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT)    :: RCS_Id
    #{aFmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN)     :: Message_Log
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
#{deallocStatement}
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error deallocating #{@structName}. STAT = ", i5 )' ) &
                      Allocate_Status
#{failReturn(2,"TRIM(Message)")}
    END IF

    ! Reset the dimension indicators
#{dimClear.chomp}
    
    ! Decrement and test allocation counter
    #{@structName}%n_Allocates = #{@structName}%n_Allocates - 1
    IF ( #{@structName}%n_Allocates /= 0 ) THEN
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      #{@structName}%n_Allocates
#{failReturn(2,"TRIM(Message)")}
    END IF
    
  END FUNCTION #{@nameSpace}Destroy_#{@structName}
EOF

    # Done
    str    
  end # def destroyFunc
  

  # =========================================
  # Method to construct the Allocate function
  # =========================================
  def allocFunc

    # Declaration and argument type definition format
    dFmt=nameFmt(["#{@structName}_out","Message_Log"]+@dimList)
    aFmt=nameFmt(["TYPE(#{@nameSpace}#{@structName}_type)","CHARACTER(*), OPTIONAL"])

    # The function declaration dimension inputs
    dimFuncDef=""
    @dimList.each {|d| dimFuncDef<<indent(5)+" #{dFmt%d}, &  ! Input\n"}
    dimFuncDef.chomp! # Remove last newline

    # The argument declaration dimension inputs
    dimArgDef=""
    @dimList.each {|d| dimArgDef<<indent(1)+aFmt%"INTEGER"+", INTENT(IN)     :: #{d}\n"}
    dimArgDef.chomp! # Remove last newline
    
    # The array component allocation statement
    arrayAlloc="    ALLOCATE( &\n"
    @arrayList.each do |a|
      dimExtDef=a[:dims].collect {|d| d.join(":")}.join(",")
      arrayAlloc<<"      #{@structName}%#{a[:name]}(#{dimExtDef}), &\n"
    end
    arrayAlloc<<"      STAT = Allocate_Status )"
    
    # The dimension assignment statements
    dimAssign=""
    @dimList.each {|d| dimAssign<<"    #{@structName}%#{@dfmt%d} = #{d}\n"}
    
    # The array initialisation statements
    arrayAssign=""
    @arrayList.each do |a|
      # Only intrinsic types are initialisaed
      next if a[:type] == "TYPE"
      # Construct the initialistions
      arrayAssign<<"    #{@structName}%#{@afmt%a[:name]} = " 
      arrayAssign<< case a[:type]
                      when "INTEGER"   then "0\n"
                      when "REAL"      then "ZERO\n"
                      when "COMPLEX"   then "CMPLX(ZERO,ZERO)\n"
                      when "CHARACTER" then "' '\n"
                      else raise StandardError, "*** Type #{a[:type]} array assign not yet implemented ***"
                    end
    end

    # The function
    str=<<EOF
  FUNCTION #{@nameSpace}Allocate_#{@structName}( &
#{dimFuncDef}
             #{dFmt%"#{@structName}"}, &  ! Output
             #{dFmt%"RCS_Id"}, &  ! Revision control
             #{dFmt%"Message_Log"}) &  ! Error messaging
           RESULT( Error_Status )
    ! Arguments
#{dimArgDef}
    #{aFmt%"TYPE(#{@nameSpace}#{@structName}_type)"}, INTENT(IN OUT) :: #{@structName}
    #{aFmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT)    :: RCS_Id
    #{aFmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN)     :: Message_Log
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
#{dimcheckIf}
#{failReturn(2,"'Input #{@structName} dimensions must all be > 0.'")}
    END IF
    
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    IF ( #{@nameSpace}Associated_#{@structName}( #{@structName}, ANY_Test=1 ) ) THEN
      Error_Status = #{@nameSpace}Destroy_#{@structName}( &
                       #{@structName}, &
                       No_Clear=1, &
                       Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
#{failReturn(3,"'Error deallocating #{@structName} prior to allocation.'")}
      END IF
    END IF
    
    ! Perform the pointer allocation
#{arrayAlloc}
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating #{@structName} data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
#{failReturn(2,"TRIM(Message)")}
    END IF
    
    ! Assign the dimensions
#{dimAssign}
    ! Initialise the arrays
#{arrayAssign}
    ! Increment and test the allocation counter
    #{@structName}%n_Allocates = #{@structName}%n_Allocates + 1
    IF ( #{@structName}%n_Allocates /= 1 ) THEN
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      #{@structName}%n_Allocates
#{failReturn(2,"TRIM(Message)")}
    END IF
    
  END FUNCTION #{@nameSpace}Allocate_#{@structName}
EOF

    # Done
    str    
  end # def allocFunc
  

  # =======================================
  # Method to construct the Assign function
  # =======================================
  def assignFunc

    # The indents
    i0,i1=indent(0),indent(1)

    # The output structure allocation statement
    allocStruct=i1+"Error_Status = #{@nameSpace}Allocate_#{@structName}( &\n"
    @dimList.each {|d| allocStruct<<indent(9)+" #{@structName}_in%#{@dfmt%d}, &\n"}
    allocStruct<<indent(9)+" #{@structName}_out, &\n"+indent(9)+" Message_Log=Message_Log)"

    # The scalar assignment statements
    scalarAssign=""
    @scalarList.each do |s|

      # Everything but derived types are simply assigned
      unless s[:type] == "TYPE"
        scalarAssign<<i1+"#{@structName}_out%#{@sfmt%s[:name]} = #{@structName}_in%#{s[:name]}\n"

      # Derived type call their Assign function
      else
        typeAssign=<<-EOF
    Error_Status = Assign_#{s[:typename]}( &
                     #{@structName}_in%#{s[:name]}, &
                     #{@structName}_out%#{s[:name]}, &
                     Message_Log=Message )
    IF ( Error_Status /= SUCCESS ) THEN
#{failReturn(2,"'Error copying #{@structName} scalar structure component #{s[:name]}'")}
    END IF
EOF
        scalarAssign<<typeAssign
      end
    end

    # The array assignment statements
    arrayAssign=""
    @arrayList.each do |a|

      # Everything but derived types are simply assigned
      unless a[:type] == "TYPE"
        arrayAssign<<i1+"#{@structName}_out%#{@afmt%a[:name]} = #{@structName}_in%#{a[:name]}\n"

      # Derived type call their Assign function
      else
      
        # The loop beginning
        arrayAssign<<beginLoop(a)

        # The loop body
        i=indent(a[:ndims]+1)
        dimLoopBody = <<EOF
#{i}Error_Status = Assign_#{a[:typename]}( &
#{i}                 #{@structName}_in(#{a[:dimidx].join(",")})%#{a[:name]}, &
#{i}                 #{@structName}_out(#{a[:dimidx].join(",")})%#{a[:name]}, &
#{i}                 Message_Log=Message_Log )
#{i}IF ( Error_Status /= SUCCESS ) THEN
#{failReturn(a[:ndims]+2,"'Error copying #{@structName} array structure component #{a[:name]}'")}
#{i}END IF
EOF
        arrayAssign<<dimLoopBody
        
        # The loop end
        arrayAssign<<endLoop(a)
      end
    end

    # Declaration and argument type definition format
    dFmt=nameFmt(["#{@structName}_out","Message_Log"])
    aFmt=nameFmt(["TYPE(#{@nameSpace}#{@structName}_type)","CHARACTER(*), OPTIONAL"])

    # Create the function
    str=<<EOF
  FUNCTION #{@nameSpace}Assign_#{@structName}( &
             #{dFmt%"#{@structName}_in"}, &  ! Input
             #{dFmt%"#{@structName}_out"}, &  ! Output
             #{dFmt%"RCS_Id"}, &  ! Revision control
             #{dFmt%"Message_Log"}) &  ! Error messaging
           RESULT( Error_Status )
    ! Arguments
    #{aFmt%"TYPE(#{@nameSpace}#{@structName}_type)"}, INTENT(IN)     :: #{@structName}_in
    #{aFmt%"TYPE(#{@nameSpace}#{@structName}_type)"}, INTENT(IN OUT) :: #{@structName}_out
    #{aFmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT)    :: RCS_Id
    #{aFmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = '#{@nameSpace}Assign_#{@structName}'
    ! Local variables
#{@dimdecl}

    ! Set up
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. #{@nameSpace}Associated_#{@structName}(#{@structName}_In) ) THEN
#{failReturn(2,"'Some or all INPUT #{@structName} pointer members are NOT associated.'")}
    END IF
    
    ! Allocate data arrays
#{allocStruct}
    IF ( Error_Status /= SUCCESS ) THEN
#{failReturn(2,"'Error allocating output structure'")}
    END IF
    
    ! Assign non-dimension scalar members
#{scalarAssign}
    ! Copy array data
#{arrayAssign}
  END FUNCTION #{@nameSpace}Assign_#{@structName}
EOF

    # Done
    str    
  end # def assignFunc
  

  # ======================================
  # Method to construct the Equal function
  # ======================================
  def equalFunc

    # The indents
    i0,i1=indent(0),indent(1)

    # -----------------------------------
    # The dimension value equality checks
    # -----------------------------------
    d=@dfmt%@dimList.first
    dimEqual=indent(1)+"IF ( #{@structName}_LHS%#{d} /= #{@structName}_RHS%#{d} .OR. &\n"
    1.upto(@dimList.length-2) do |i|
      d=@dfmt%@dimList[i]
      dimEqual<<indent(3) + " #{@structName}_LHS%#{d} /= #{@structName}_RHS%#{d} .OR. &\n"
    end
    d=@dfmt%@dimList.last
    dimEqual<<indent(3) + " #{@structName}_LHS%#{d} /= #{@structName}_RHS%#{d} ) THEN"

    # ------------------------------------
    # The scalar component equality checks
    # ------------------------------------
    scalarEqual=""
    @scalarList.each do |s|
      case s[:type]

        # Integers and characters can be directly compared
        when "INTEGER","CHARACTER"
          i=indent(1)
          thisEqual=<<EOF
#{i}IF( #{@structName}_LHS%#{s[:name]} /= #{@structName}_RHS%#{s[:name]}) THEN
#{failReturn(2,"'#{@structName} scalar component #{s[:name]} values are different.'")}
#{i}END IF
EOF
          scalarEqual<<thisEqual

        # Floating point values use Compare_Float
        when "REAL"
          i=indent(1)
          thisEqual=<<EOF
#{i}IF ( .NOT. Compare_Float( #{@structName}_LHS%#{s[:name]}, &
#{i}                          #{@structName}_RHS%#{s[:name]}, &
#{i}                          ULP=ULP ) ) THEN
#{failReturn(2,"'#{@structName} scalar component #{s[:name]} values are different.'")}
#{i}END IF
EOF
          scalarEqual<<thisEqual

        # Other data structures use their Equal functions
        when "TYPE"
          i=indent(1)
          thisEqual=<<EOF
#{i}Error_Status = Equal_#{s[:typename]}( &
#{i}                 #{@structName}_in%#{s[:name]}, &
#{i}                 #{@structName}_out%#{s[:name]}, &
#{i}                 Message_Log=Message_Log )
#{i}IF ( Error_Status /= SUCCESS ) THEN
#{failReturn(2,"'#{@structName} scalar structure component #{s[:name]} values are different.'")}
#{i}END IF
EOF
          scalarEqual<<thisEqual

        # Non-implemented types
        else
          raise StandardError, "*** Type #{s[:type]} array equality test not yet implemented ***"
      end
    end

    # -----------------------------------
    # The array component equality checks
    # -----------------------------------
    arrayEqual=""
    @arrayList.each do |a|
      case a[:type]

        # Integers and characters can be directly compared
        when "INTEGER","CHARACTER"
          i=indent(1)
          thisEqual = <<EOF
#{i}IF ( ANY( #{@structName}_LHS%#{a[:name]} /= #{@structName}_RHS%#{a[:name]} ) ) THEN
#{failReturn(2,"'#{@structName} array component #{a[:name]} values are different.'")}
#{i}END IF
EOF
          arrayEqual<<thisEqual


        # Floating point values use Compare_Float
        when "REAL"
        
          # The loop beginning
          arrayEqual<<beginLoop(a)

          # The loop body
          i=indent(a[:ndims]+1)
          thisEqual=<<EOF
#{i}IF ( .NOT. Compare_Float( #{@structName}_LHS(#{a[:dimidx].join(",")})%#{a[:name]}, &
#{i}                          #{@structName}_RHS(#{a[:dimidx].join(",")})%#{a[:name]}, &
#{i}                          ULP=ULP ) ) THEN
#{i}  WRITE( Message, '("#{a[:name]} values are different at index (",#{a[:ndims]}(1x,i0),")")') &
#{i}                  #{a[:dimidx].join(",")} 
#{failReturn(a[:ndims]+2,"TRIM(Message)")}
#{i}END IF
EOF
          arrayEqual<<thisEqual
        
          # The loop end
          arrayEqual<<endLoop(a)


        # Other data structures use their Equal functions
        when "TYPE"

          # The loop beginning
          arrayEqual<<beginLoop(a)

          # The loop body
          i=indent(a[:ndims]+1)
          thisEqual=<<EOF
#{i}Error_Status = Equal_#{a[:typename]}( &
#{i}                 #{@structName}_in(#{a[:dimidx].join(",")})%#{a[:name]}, &
#{i}                 #{@structName}_out(#{a[:dimidx].join(",")})%#{a[:name]}, &
#{i}                 Message_Log=Message_Log )
#{i}IF ( Error_Status /= SUCCESS ) THEN
#{i}  WRITE( Message, '("#{a[:name]} structures are different at index (",#{a[:ndims]}(1x,i0),")")') &
#{i}                  #{a[:dimidx].join(",")} 
#{failReturn(a[:ndims]+2,"TRIM(Message)")}
#{i}END IF
EOF
          arrayEqual<<thisEqual
        
          # The loop end
          arrayEqual<<endLoop(a)


        # Non-implemented types
        else
          raise StandardError, "*** Type #{a[:type]} array equality test not yet implemented ***"
      end
    end

    # Declaration and argument type definition format
    dFmt=nameFmt(["#{@structName}_LHS","Message_Log"])
    aFmt=nameFmt(["TYPE(#{@nameSpace}#{@structName}_type)","CHARACTER(*), OPTIONAL"])
    
    # Create the function
    str=<<EOF
  FUNCTION #{@nameSpace}Equal_#{@structName}( &
             #{dFmt%"#{@structName}_LHS"}, &  ! Input
             #{dFmt%"#{@structName}_RHS"}, &  ! Input
             #{dFmt%"ULP_Scale"}, &  ! Optional input
             #{dFmt%"RCS_Id"}, &  ! Revision control
             #{dFmt%"Message_Log"}) &  ! Error messaging
           RESULT( Error_Status )
    ! Arguments
    #{aFmt%"TYPE(#{@nameSpace}#{@structName}_type)"}, INTENT(IN)  :: #{@structName}_LHS
    #{aFmt%"TYPE(#{@nameSpace}#{@structName}_type)"}, INTENT(IN)  :: #{@structName}_RHS
    #{aFmt%"INTEGER,      OPTIONAL"}, INTENT(IN)  :: ULP_Scale
    #{aFmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT) :: RCS_Id
    #{aFmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN)  :: Message_Log
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
    IF ( .NOT. #{@nameSpace}Associated_#{@structName}( #{@structName}_LHS ) ) THEN
#{failReturn(2,"'Some or all INPUT #{@structName}_LHS pointer members are NOT associated.'")}
    END IF
    IF ( .NOT. #{@nameSpace}Associated_#{@structName}( #{@structName}_RHS ) ) THEN
#{failReturn(2,"'Some or all INPUT #{@structName}_RHS pointer members are NOT associated.'")}
    END IF
    
    ! Check dimensions
#{dimEqual}
#{failReturn(2,"'Structure dimensions are different.'")}
    END IF

    ! Check the scalar components
#{scalarEqual}
    ! Check the array components
#{arrayEqual}
  END FUNCTION #{@nameSpace}Equal_#{@structName}
EOF

    # Done
    str    
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
    dFmt=nameFmt(["#{@structName}","RCS_Id"])
    aFmt=nameFmt(["TYPE(#{@nameSpace}#{@structName}_type)","CHARACTER(*), OPTIONAL"])
    
    # Create the function
    str=<<EOF
  SUBROUTINE #{@nameSpace}Info_#{@structName}( &
               #{dFmt%"#{@structName}"}, &
               #{dFmt%"Info"}, &  ! Output
               #{dFmt%"RCS_Id"}  )  ! Revision control
    ! Arguments
    #{aFmt%"TYPE(#{@nameSpace}#{@structName}_type)"}, INTENT(IN)  :: #{@structName}
    #{aFmt%"CHARACTER(*)"}, INTENT(OUT) :: Info
    #{aFmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT) :: RCS_Id
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
  private
# =======

  # Generate formats for list of names
  def nameFmt(nameList)
    strLen=nameList.inject(0) {|memo,n| memo >= n.length ? memo : n.length}
    "%-#{strLen}.#{strLen}s"
  end
  
  # Generate indent levels for code
  def indent(indentLevel)
    base="  "
    step="  "
    base + step*indentLevel
  end # def indent

  # Build the loop DO statements
  def beginLoop(a)
    str=""
    0.upto(a[:ndims]-1) do |i|
      str<<indent(i+1)+"DO #{a[:dimidx].reverse[i]}=#{a[:dims].reverse[i][0]},#{a[:dims].reverse[i][1]}\n"
    end
    str
  end # def beginLoop

  # Build the loop END DO statements
  def endLoop(a)
    str=""
    (a[:ndims]-1).downto(0) do |i|
      str<<indent(i+1)+"END DO\n"
    end
    str
  end # def endLoop

  # Generate dimension value check for Allocate function
  def dimcheckIf
    str="    IF ( #{@dfmt%@dimList.first} < 1"
    if @dimList.length==1
      str<<" ) THEN"
    else
      str<<" .OR. &\n"
      1.upto(@dimList.length-2) do |i|
        str<<"         #{@dfmt%@dimList[i]} < 1 .OR. &\n"
      end
      str<<"         #{@dfmt%@dimList.last} < 1 ) THEN"
    end
  end
  
  # Generate IF statement for Associated function
  def assocIf(operator)
    n=2
    if @arrayList.length>1
      str=indent(n)+"IF ( ASSOCIATED(#{@structName}%#{@afmt%@arrayList.first[:name]}) .#{operator}. &\n"
      1.upto(@arrayList.length-2) {|i| str<<indent(n+2)+" ASSOCIATED(#{@structName}%#{@afmt%@arrayList[i][:name]}) .#{operator}. &\n"}
      str<<indent(n+2)+" ASSOCIATED(#{@structName}%#{@afmt%@arrayList.last[:name]}) ) THEN\n"
    else
      str=indent(n)+"IF ( ASSOCIATED(#{@structName}%#{@arrayList.first[:name]}) ) THEN\n"  
    end
    str<<indent(n+1)+ "Association_Status = .TRUE.\n"
    str<<indent(n)+ "END IF"
    str
  end # def assocIf

  # Generate 
  def failReturn(indentLevel,message)
    i=indent(indentLevel)
    code = <<EOF
#{i}Error_Status = FAILURE
#{i}CALL Display_Message( &
#{i}       ROUTINE_NAME, &
#{i}       #{message}, &
#{i}       Error_Status, &
#{i}       Message_Log=Message_Log )
#{i}RETURN
EOF
    code.chomp # Remove last newline
  end # def failReturn

end # class FDefMod
