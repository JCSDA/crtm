#!/usr/bin/env ruby

class FDefMod

  # The FDefMod regular expressions
  STRUCTBEGINREGEXP=%r{
      ^\s*TYPE\s*::\s*
      (\w*)_type       # Capture the structure name to $1
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
          ([\w,]+)                    # capture dimension list text to $3
        \)                            # dimlist paren end
      )?                              # The whole group is optional
      # Look for the component variable name
      \s*::\s*                        # The :: separator
      (\w*)                           # Capture the component variable name to $4
      # Look for init val
      (?:                             # Non-capture grouping
        \s*=\s*                       # Assignment operator + whitespace
        (['"'\s\w]*)                  # Capture the initialisation value or parameter name to $5
      )?                              # The whole group is optional
      # Look for description
      (?:                             # Non-capture grouping
        \s*!\s*                       # Comment character + whitespace
        (.*)                          # Capture the rest of the line to $6
      )?                              # The whole group is optional
    }ix
  
  # ---------------------
  # Class variable access
  # ---------------------
  attr_accessor :structName,:dimList,:scalarList,:arrayList,:debug

  # -----------------
  # Class constructor
  # -----------------
  def initialize(debug=false)
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

  # ----------------------------------------
  # Method to process a dimension definition
  # ----------------------------------------
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

  # -------------------------------------
  # Method to process a scalar definition
  # -------------------------------------
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

  # -------------------------------------
  # Method to process an array definition
  # -------------------------------------
  def arrayProcess(line)
    arrayMatch = COMPONENTREGEXP.match(line)
    unless arrayMatch.nil?
      # We have matched an array component definition
      dims  =arrayMatch[3].split(/\s*,\s*/)                # Array of dimension names
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

  # ------------------------------
  # Method to check the dimensions
  # ------------------------------
  def dimCheck
    @arrayList.each do |a|
      a[:dims].each do |d|
        raise(StandardError, "Invalid dimension, #{d}, specified for #{a[:name]}") unless @dimList.include?(d)
      end
    end
  end

  # ------------------------------------------
  # Method to parse an fdefmod definition file
  # ------------------------------------------
  def read(file)

    # Initialise component type tag switch
    tag=""
    
    # Process file line by line
    File.open(file,"r").readlines.each do |line|

      # Retrieve the structure name
      if line =~ STRUCTBEGINREGEXP
        @structName=$1
        puts("Structure name: #{structName}") if @debug
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
      
    end # File.open

    # Check that all array component dimensions are valid
    dimCheck
    
    # Convert the dimension declaration
    @dimdecl=indent(1)+"INTEGER :: #{@dimdecl.join(",")}"
    
    # Construct component name formats
    dsl=@dimList.inject(0) {|memo,d| memo >= d.length ? memo : d.length}
    @dfmt="%-#{dsl}.#{dsl}s"
    ssl=(@scalarList.collect {|s| s[:name]}).inject(0) {|memo,s| memo >= s.length ? memo : s.length}
    @sfmt="%-#{ssl}.#{ssl}s"
    asl=(@arrayList.collect {|a| a[:name]}).inject(0) {|memo,a| memo >= a.length ? memo : a.length}
    @afmt="%-#{asl}.#{asl}s"

  end # def read

  # --------------------------------------------
  # Method to construct the structure definition
  # --------------------------------------------
  def structDef

    # The indents
    i0,i1=indent(0),indent(1)
    
    # Dimensions
    dimDef=""
    @dimList.each {|d| dimDef<<i1+"INTEGER :: #{d}=0\n"}
    
    # Scalars
    scalarDef=""
    @scalarList.each do |s|
      scalarDef<<i1+"#{s[:type]}"
      scalarDef<<"(#{s[:param]})" unless s[:param].nil?
      scalarDef<<" :: #{s[:name]}"
      scalarDef<<" = #{s[:initval]}" unless s[:initval].nil?
      scalarDef<<" ! #{s[:desc]}" unless s[:desc].nil?
      scalarDef<<"\n"
    end
    
    # Arrays
    arrayDef=""
    @arrayList.each do |a|
      arrayDef<<i1+"#{a[:type]}"
      arrayDef<<"(#{a[:param]})" unless a[:param].nil?
      arrayDef<<", DIMENSION("<<([":"]*a[:ndims]).join(",")<<")"
      arrayDef<<", POINTER :: #{a[:name]}=>NULL()"
      arrayDef<<" ! #{a[:desc]}" unless a[:desc].nil?
      arrayDef<<"\n"
    end

    # The definition
    str=<<EOF
#{i0}TYPE :: #{@structName}_type
#{i1}INTEGER :: n_Allocates=0
#{i1}
#{i1}! Dimensions
#{dimDef}
#{i1}! Scalars
#{scalarDef}
#{i1}! Arrays
#{arrayDef}
#{i0}END TYPE #{@structName}_type
EOF

    # Done
    str
  end # def structDef


  # ----------------------------------------
  # Method to construct the Clear subroutine
  # ----------------------------------------
  def clearSub

    # The indents
    i0,i1=indent(0),indent(1)
    
    # The dimension clear statements
    dimClear=""
    @dimList.each {|d| dimClear<<i1+"#{@structName}%#{@dfmt%d}=0\n"}
    
    # The scalar clear statements
    scalarClear=""
    scalarList.each {|s| scalarClear<<i1+"#{@structName}%#{@sfmt%s[:name]}=#{s[:initval]}\n" unless s[:initval].nil?}

    # The subroutine
    str=<<EOF
#{i0}SUBROUTINE Clear_#{@structName}(#{@structName})
#{i1}TYPE(#{@structName}_type), INTENT(IN OUT) :: #{@structName}
#{i1}    
#{i1}! Dimensions
#{dimClear}
#{i1}! Scalars
#{scalarClear}
#{i0}END SUBROUTINE Clear_#{@structName}
EOF

    # Done
    str
  end # def clearSub
  

  # -------------------------------------------
  # Method to construct the Associated function
  # -------------------------------------------
  def assocFunc

    # The indents
    i0,i1=indent(0),indent(1)

    # The function
    str=<<EOF
#{i0}FUNCTION Associated_#{@structName}( &
#{i1}         #{@structName}, &  ! Input
#{i1}         ANY_Test ) & ! Optional input
#{i1}       RESULT(Association_Status)
#{i1}! Arguments
#{i1}TYPE(#{@structName}_type), INTENT(IN) :: #{@structName}
#{i1}INTEGER, OPTIONAL, INTENT(IN) :: ANY_Test
#{i1}! Function result
#{i1}LOGICAL :: Association_Status
#{i1}! Local variables
#{i1}LOGICAL :: ALL_Test
#{i1}
#{i1}! Default is to test ALL the pointer members
#{i1}! for a true association status....
#{i1}ALL_Test = .TRUE.
#{i1}! ...unless the ANY_Test argument is set.
#{i1}IF ( PRESENT( ANY_Test ) ) THEN
#{i1}  IF ( ANY_Test == 1 ) ALL_Test = .FALSE.
#{i1}END IF
#{i1}
#{i1}! Test the structure associations    
#{i1}Association_Status = .FALSE.
#{i1}IF (ALL_Test) THEN
#{assocIf("AND")}
#{i1}ELSE
#{assocIf("OR")}
#{i1}END IF
#{i1}
#{i0}END FUNCTION Associated_#{@structName}
EOF

    # Done
    str    
  end # def assocFunc
  

  # ----------------------------------------
  # Method to construct the Destroy function
  # ----------------------------------------
  def destroyFunc

    # The indents
    i0,i1=indent(0),indent(1)

    # The array component deallocation statement
    deallocStatement=i1+"DEALLOCATE( #{@structName}%#{@afmt%@arrayList.first[:name]}, &\n"
    1.upto(@arrayList.length-1) do |i|
      deallocStatement<<indent(7)+"#{@structName}%#{@afmt%@arrayList[i][:name]}, &\n"
    end
    deallocStatement<<indent(7)+"STAT = Allocate_Status )"
    
    # The function
    str=<<EOF
#{i0}FUNCTION Destroy_#{@structName}( &
#{i1}         #{@structName}, &  ! Output
#{i1}         No_Clear, &  ! Optional input
#{i1}         RCS_Id, &  ! Revision control
#{i1}         Message_Log ) &  ! Error messaging
#{i1}       RESULT(Error_Status)
#{i1}! Arguments
#{i1}TYPE(#{@structName}_type), INTENT(IN OUT) :: #{@structName} 
#{i1}INTEGER,      OPTIONAL, INTENT(IN)  :: No_Clear
#{i1}CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
#{i1}CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
#{i1}! Function result
#{i1}INTEGER :: Error_Status
#{i1}! Local parameters
#{i1}CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_#{@structName}'
#{i1}! Local variables
#{i1}CHARACTER(256)  :: Message
#{i1}LOGICAL :: Clear
#{i1}INTEGER :: Allocate_Status
#{i1}
#{i1}! Set up
#{i1}Error_Status = SUCCESS
#{i1}IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
#{i1}
#{i1}! Default is to clear scalar members...
#{i1}Clear = .TRUE.
#{i1}! ....unless the No_Clear argument is set
#{i1}IF ( PRESENT( No_Clear ) ) THEN
#{i1}  IF ( No_Clear == 1 ) Clear = .FALSE.
#{i1}END IF
#{i1}
#{i1}! Initialise the scalar members
#{i1}IF ( Clear ) CALL Clear_#{@structName}(#{@structName})
#{i1}
#{i1}! If ALL pointer members are NOT associated, do nothing
#{i1}IF ( .NOT. Associated_#{@structName}(#{@structName}) ) RETURN
#{i1}
#{i1}! Deallocate the pointer members
#{deallocStatement}
#{i1}IF ( Allocate_Status /= 0 ) THEN
#{i1}  WRITE( Message, '( "Error deallocating #{@structName}. STAT = ", i5 )' ) &
#{i1}                  Allocate_Status
#{failReturn(2,"TRIM(Message)")}
#{i1}END IF
#{i1}
#{i1}! Decrement and test allocation counter
#{i1}#{@structName}%n_Allocates = #{@structName}%n_Allocates - 1
#{i1}IF ( #{@structName}%n_Allocates /= 0 ) THEN
#{i1}  WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
#{i1}                  #{@structName}%n_Allocates
#{failReturn(2,"TRIM(Message)")}
#{i1}END IF
#{i1}
#{i0}END FUNCTION Destroy_#{@structName}
EOF

    # Done
    str    
  end # def destroyFunc
  

  # -----------------------------------------
  # Method to construct the Allocate function
  # -----------------------------------------
  def allocFunc

    # The indents
    i0,i1=indent(0),indent(1)

    # The function declaration dimension inputs
    dimFuncDef=""
    @dimList.each {|d| dimFuncDef<<indent(5)+" #{@dfmt%d}, &  ! Input\n"}
    dimFuncDef.chomp! # Remove last newline

    # The argument declaration dimension inputs
    dimArgDef=""
    @dimList.each {|d| dimArgDef<<i1+"INTEGER, INTENT(IN) :: #{d}\n"}
    dimArgDef.chomp! # Remove last newline
    
    # The dimension value check
    dimValCheck=i1+"IF ( #{@dfmt%@dimList.first} < 1 .OR. &\n"
    1.upto(@dimList.length-2) do |i|
      dimValCheck<<indent(3)+" #{@dfmt%@dimList[i]} < 1 .OR. &\n"
    end
    dimValCheck<<indent(3)+" #{@dfmt%@dimList.last} < 1 ) THEN"
    
    # The array component allocation statement
    arrayAlloc=i1+"ALLOCATE( #{@structName}%#{@arrayList.first[:name]}(#{@arrayList.first[:dims].join(",")}), &\n"
    1.upto(@arrayList.length-1) do |i|
      arrayAlloc<<indent(6)+"#{@structName}%#{@arrayList[i][:name]}(#{@arrayList[i][:dims].join(",")}), &\n"
    end
    arrayAlloc<<indent(6)+"STAT = Allocate_Status )"
    
    # The dimension assignment statements
    dimAssign=""
    @dimList.each {|d| dimAssign<<i1+"#{@structName}%#{@dfmt%d} = #{d}\n"}
    
    # The array initialisation statements
    arrayAssign=""
    @arrayList.each do |a|
      # Only intrinsic types are initialisaed
      next if a[:type] == "TYPE"
      # Construct the initialistions
      arrayAssign<<i1+"#{@structName}%#{@afmt%a[:name]} = " 
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
#{i0}FUNCTION Allocate_#{@structName}( &
#{dimFuncDef}
#{i1}         #{@structName}, &  ! Output
#{i1}         RCS_Id,       &  ! Revision control
#{i1}         Message_Log ) &  ! Error messaging
#{i1}       RESULT( Error_Status )
#{i1}! Arguments
#{dimArgDef}
#{i1}TYPE(#{@structName}_type), INTENT(IN OUT) :: #{@structName}
#{i1}CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
#{i1}CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
#{i1}! Function result
#{i1}INTEGER :: Error_Status
#{i1}! Local parameters
#{i1}CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_#{@structName}'
#{i1}! Local variables
#{i1}CHARACTER(256)  :: Message
#{i1}INTEGER :: Allocate_Status
#{i1}
#{i1}! Set up
#{i1}Error_Status = SUCCESS
#{i1}IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
#{i1}
#{i1}! Check dimensions
#{dimValCheck}
#{failReturn(2,"'Input #{@structName} dimensions must all be > 0.'")}
#{i1}END IF
#{i1}
#{i1}! Check if ANY pointers are already associated.
#{i1}! If they are, deallocate them but leave scalars.
#{i1}IF ( Associated_#{@structName}( #{@structName}, ANY_Test=1 ) ) THEN
#{i1}  Error_Status = Destroy_#{@structName}( &
#{i1}                   #{@structName}, &
#{i1}                   No_Clear=1, &
#{i1}                   Message_Log=Message_Log )
#{i1}  IF ( Error_Status /= SUCCESS ) THEN
#{failReturn(3,"'Error deallocating #{@structName} prior to allocation.'")}
#{i1}  END IF
#{i1}END IF
#{i1}
#{i1}! Perform the pointer allocation
#{arrayAlloc}
#{i1}IF ( Allocate_Status /= 0 ) THEN
#{i1}  WRITE( Message, '( "Error allocating #{@structName} data arrays. STAT = ", i5 )' ) &
#{i1}                  Allocate_Status
#{failReturn(2,"TRIM(Message)")}
#{i1}END IF
#{i1}
#{i1}! Assign the dimensions
#{dimAssign}
#{i1}! Initialise the arrays
#{arrayAssign}
#{i1}! Increment and test the allocation counter
#{i1}#{@structName}%n_Allocates = #{@structName}%n_Allocates + 1
#{i1}IF ( #{@structName}%n_Allocates /= 1 ) THEN
#{i1}  WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
#{i1}                  #{@structName}%n_Allocates
#{failReturn(2,"TRIM(Message)")}
#{i1}END IF
#{i1}
#{i0}END FUNCTION Allocate_#{@structName}
EOF

    # Done
    str    
  end # def allocFunc
  

  # ---------------------------------------
  # Method to construct the Assign function
  # ---------------------------------------
  def assignFunc

    # The indents
    i0,i1=indent(0),indent(1)

    # The output structure allocation statement
    allocStruct=i1+"Error_Status = Allocate_#{@structName}( &\n"
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
#{i1}Error_Status = Assign_#{s[:typename]}( &
#{i1}                 #{@structName}_in%#{s[:name]}, &
#{i1}                 #{@structName}_out%#{s[:name]}, &
#{i1}                 Message_Log=Message )
#{i1}IF ( Error_Status /= SUCCESS ) THEN
#{failReturn(2,"'Error copying #{@structName} scalar structure component #{s[:name]}'")}
#{i1}END IF
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

    # Create the function
    str=<<EOF
#{i0}FUNCTION Assign_#{@structName}( &
#{i1}         #{@structName}_in , &  ! Input
#{i1}         #{@structName}_out, &  ! Output
#{i1}         RCS_Id      , &  ! Revision control
#{i1}         Message_Log ) &  ! Error messaging
#{i1}       RESULT( Error_Status )
#{i1}! Arguments
#{i1}TYPE(#{@structName}_type), INTENT(IN)     :: #{@structName}_in
#{i1}TYPE(#{@structName}_type), INTENT(IN OUT) :: #{@structName}_out
#{i1}CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
#{i1}CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
#{i1}! Function result
#{i1}INTEGER :: Error_Status
#{i1}! Local parameters
#{i1}CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_#{@structName}'
#{i1}! Local variables
#{@dimdecl}
#{i1}
#{i1}! Set up
#{i1}IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
#{i1}
#{i1}! ALL *input* pointers must be associated
#{i1}IF ( .NOT. Associated_#{@structName}(#{@structName}_In) ) THEN
#{failReturn(2,"'Some or all INPUT #{@structName} pointer members are NOT associated.'")}
#{i1}END IF
#{i1}
#{i1}! Allocate data arrays
#{allocStruct}
#{i1}IF ( Error_Status /= SUCCESS ) THEN
#{failReturn(2,"'Error allocating output structure'")}
#{i1}END IF
#{i1}
#{i1}! Assign non-dimension scalar members
#{scalarAssign}
#{i1}! Copy array data
#{arrayAssign}
#{i0}END FUNCTION Assign_#{@structName}
EOF

    # Done
    str    
  end # def assignFunc
  

  # --------------------------------------
  # Method to construct the Equal function
  # --------------------------------------
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

    # Create the function
    str=<<EOF
#{i0}FUNCTION Equal_#{@structName}( &
#{i1}         #{@structName}_LHS, &  ! Input
#{i1}         #{@structName}_RHS, &  ! Input
#{i1}         ULP_Scale,    &  ! Optional input
#{i1}         RCS_Id,       &  ! Revision control
#{i1}         Message_Log ) &  ! Error messaging
#{i1}       RESULT( Error_Status )
#{i1}! Arguments
#{i1}TYPE(#{@structName}_type), INTENT(IN)  :: #{@structName}_LHS
#{i1}TYPE(#{@structName}_type), INTENT(IN)  :: #{@structName}_RHS
#{i1}INTEGER,      OPTIONAL, INTENT(IN)  :: ULP_Scale
#{i1}CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
#{i1}CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
#{i1}! Function result
#{i1}INTEGER :: Error_Status
#{i1}! Local parameters
#{i1}CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_#{@structName}'
#{i1}! Local variables
#{i1}CHARACTER(256)  :: Message
#{i1}INTEGER :: ULP
#{i1}INTEGER :: l, j
#{@dimdecl}
#{i1}
#{i1}! Set up
#{i1}Error_Status = SUCCESS
#{i1}IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
#{i1}
#{i1}! Default precision is a single unit in last place
#{i1}ULP = 1
#{i1}! ... unless the ULP_Scale argument is set and positive
#{i1}IF ( PRESENT( ULP_Scale ) ) THEN
#{i1}  IF ( ULP_Scale > 0 ) ULP = ULP_Scale
#{i1}END IF
#{i1}
#{i1}! Check the structure association status
#{i1}IF ( .NOT. Associated_#{@structName}( #{@structName}_LHS ) ) THEN
#{failReturn(2,"'Some or all INPUT #{@structName}_LHS pointer members are NOT associated.'")}
#{i1}END IF
#{i1}IF ( .NOT. Associated_#{@structName}( #{@structName}_RHS ) ) THEN
#{failReturn(2,"'Some or all INPUT #{@structName}_RHS pointer members are NOT associated.'")}
#{i1}END IF
#{i1}
#{i1}! Check dimensions
#{dimEqual}
#{failReturn(2,"'Structure dimensions are different.'")}
#{i1}END IF
#{i1}
#{i1}! Check the scalar components
#{scalarEqual}
#{i1}! Check the array components
#{arrayEqual}
#{i0}END FUNCTION Equal_#{@structName}
EOF

    # Done
    str    
  end # def equalFunc


  # ---------------------------------------
  # Method to construct the Info subroutine
  # ---------------------------------------
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
    
    # Create the function
    str=<<EOF
#{i0}SUBROUTINE Info_#{@structName}( &
#{i1}           #{@structName}, &
#{i1}           Info,     &  ! Output
#{i1}           RCS_Id    )  ! Revision control
#{i1}! Arguments
#{i1}TYPE(#{@structName}_type),    INTENT(IN)  :: #{@structName}
#{i1}CHARACTER(*),           INTENT(OUT) :: Info
#{i1}CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
#{i1}! Parameters
#{i1}INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
#{i1}INTEGER, PARAMETER :: LINEFEED = 10
#{i1}! Local variables
#{i1}CHARACTER(80)  :: FmtString
#{i1}CHARACTER(512) :: LongString
#{i1}
#{i1}! Set up
#{i1}IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
#{i1}
#{i1}! Write the required info to the local string
#{infoFmt}
#{i1}WRITE( LongString, FMT=FmtString ) &
#{infoDim}
#{i1}
#{i1}! Trim the output based on the
#{i1}! dummy argument string length
#{i1}Info = LongString(1:MIN( LEN(Info), LEN_TRIM( LongString ) ))
#{i1}
#{i0}END SUBROUTINE Info_#{@structName}
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
! #{@structName}_Define
!
! Module defining the #{@structName} data structure and containing routines to 
! manipulate it.
!
! CREATION HISTORY:
!       This file was initially automatically generated so edit at your own risk.
!       Generated by #{File.basename(__FILE__)} on #{Time.now.strftime('%d-%b-%Y at %X')}
!       Contact info:  Paul van Delst, CIMSS/SSEC
!                      paul.vandelst@ssec.wisc.edu
!

MODULE #{@structName}_Define

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
  PUBLIC :: #{structName}_type
  ! Structure procedures
  PUBLIC :: Associated_#{structName}
  PUBLIC :: Destroy_#{structName}
  PUBLIC :: Allocate_#{structName}
  PUBLIC :: Assign_#{structName}
  PUBLIC :: Equal_#{structName}
  PUBLIC :: Info_#{structName}
    

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
END MODULE #{@structName}_Define
EOF
  

    # Done
    str    
  end # def createModule
  
  
# =======
  private
# =======

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
      str<<indent(i+1)+"DO #{a[:dimidx].reverse[i]}=1,#{a[:dims].reverse[i]}\n"
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
