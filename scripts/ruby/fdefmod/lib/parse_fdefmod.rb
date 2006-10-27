#!/usr/bin/env ruby

def indent(indentLevel)
  base="    "
  step="  "
  base + step*indentLevel
end

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
end      

def beginLoop(a)
  str=""
  0.upto(a[:ndims]-1) {|i| str<<indent(i)+"DO #{a[:dimidx].reverse[i]}=1,#{a[:dims].reverse[i]}\n"}
  str
end

def endLoop(a)
  str=""
  (a[:ndims]-1).downto(0) {|i| str<<indent(i)+"END DO\n"}
  str
end


# Initialise fdefmod tag switches
dim,scalar,array = false,false,false

# Initialise fdefmod data structures
structName=""
dimList,scalarList,arrayList = [],[],[]

# Define regular expressions
structBeginRegexp=%r{
    ^\s*TYPE\s*::\s*
    (\w*)_type       # Capture the structure name to $1
  }ix

structEndRegexp=%r{
    ^\s*END\s+TYPE\s*
    (\w*)            # Capture the structure name to $1
  }ix


dimRegexp=%r{
    ^\s*INTEGER\s*::\s* # Only integer types are allowed for dimensions
    (\w*)               # Capture the dimension name to $1
  }ix
  
componentRegexp=%r{
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

# The space indent multiple
ns=2  
# Read fdefmod file and process line by line
File.open("test.type","r").readlines.each do |line|

  # Retrieve the structure name
  if line =~ structBeginRegexp
    structName=$1
    puts("Structure name: #{structName}")
    next
  end
  
  # Check for definition end
  break if line =~ structEndRegexp
  
  # Match fdefmod tags and set switches
  if line =~ /!\s*Dimensions/i
    dim,scalar,array=true,false,false
    #puts("Dimensions")
    next
  end
  if line =~ /!\s*Scalars/i
    dim,scalar,array=false,true,false
    #puts("Scalar components")
    next
  end
  if line =~ /!\s*Arrays/i
    dim,scalar,array=false,false,true
    #puts("Array components")
    next
  end
  
  # Process dimensions
  if dim
    dimMatch = dimRegexp.match(line)
    unless dimMatch.nil?
      # We have matched a dimension definition
      dimList<<dimMatch[1]
      #puts(dimList.last.inspect)
    else
      # No match, so raise an error
      raise StandardError, "Invalid dimension definition"
    end
    next
  end

  # Process scalars
  if scalar
    scalarMatch = componentRegexp.match(line)
    unless scalarMatch.nil?
      # We have matched a scalar component definition
      scalarList<<{:type     => scalarMatch[1].upcase,
                   :param    => scalarMatch[2],
                   :typename =>(scalarMatch[2].split("_")[0] if scalarMatch[1].upcase == "TYPE"),
                   :name     => scalarMatch[4],
                   :initval  =>(scalarMatch[5].chomp.rstrip unless scalarMatch[5].nil?),
                   :desc     => scalarMatch[6]}
      #puts(scalarList.last.inspect)
    else
      # No match, so raise an error
      raise StandardError, "Invalid scalar definition"
    end
    next
  end

  # Process arrays
  if array
    arrayMatch = componentRegexp.match(line)
    unless arrayMatch.nil?
      # We have matched an array component definition
      dims  =arrayMatch[3].split(/\s*,\s*/)                # Array of dimension names
      ndims =dims.size                                     # The number of dimensions
      dimidx=(1..ndims).to_a.collect {|i| "i"+i.to_s}  # Loop index variables for each dimension
      arrayList<<{:type     => arrayMatch[1].upcase,
                  :param    => arrayMatch[2],
                  :typename =>(arrayMatch[2].split("_")[0] if arrayMatch[1].upcase == "TYPE"),
                  :dims     => dims,
                  :ndims    => ndims,
                  :dimidx   => dimidx,
                  :name     => arrayMatch[4],
                  :desc     => arrayMatch[6]}
      #puts(arrayList.last.inspect)
    else
      # No match, so raise an error
      raise StandardError, "Invalid array definition"
    end
    next
  end
end


# Check that all array component dimensions are valid
arrayList.each do |a|
  a[:dims].each do |d|
    raise(StandardError, "Invalid dimension, #{d}, specified for #{a[:name]}") unless dimList.include?(d)
  end
end


# -------------------------------
# Create the structure definition
# -------------------------------
# Preamble
structDef = <<EOF
  TYPE :: #{structName}_type
    INTEGER :: n_Allocates=0
EOF
# Dimensions
structDef<<"    ! Dimensions\n"
dimList.each {|d| structDef<<"    INTEGER :: #{d}=0\n"}
# Scalars
structDef<<"    ! Scalars\n"
scalarList.each do |s|
  scalarString = "    #{s[:type]}"
  scalarString << "(#{s[:param]})" unless s[:param].nil?
  scalarString << " :: #{s[:name]}"
  scalarString << " = #{s[:initval]}" unless s[:initval].nil?
  scalarString << " ! #{s[:desc]}" unless s[:desc].nil?
  structDef<<scalarString+"\n"
end
# Arrays
structDef<<"    ! Arrays\n"
arrayList.each do |a|
  arrayString = "    #{a[:type]}"
  arrayString << "(#{a[:param]})" unless a[:param].nil?
  arrayString << ", DIMENSION("<<([":"]*a[:dims].length).join(",")<<")"
  arrayString << ", POINTER :: #{a[:name]}=>NULL()"
  arrayString << " ! #{a[:desc]}" unless a[:desc].nil?
  structDef<<arrayString+"\n"
end
# The end
structDef<<"  END TYPE #{structName}_type"

puts structDef


# ---------------------------
# Create the Clear subroutine
# ---------------------------
# The dimension clear statements
dimClear = ""
dimList.each {|d| dimClear<<"    #{structName}%#{d}=0\n"}
# The scalar clear statements
scalarClear=""
scalarList.each {|s| scalarClear<<"    #{structName}%#{s[:name]}=#{s[:initval]}\n" unless s[:initval].nil?}
# The subroutine
clearSub = <<EOF

  SUBROUTINE Clear_#{structName}(#{structName})
    TYPE(#{structName}_type), INTENT(IN OUT) :: #{structName}
    
    ! Dimensions
#{dimClear}
    ! Scalars
#{scalarClear}
  END SUBROUTINE Clear_#{structName}
EOF

puts clearSub


# ------------------------------
# Create the Associated function
# ------------------------------
# Preamble
assocFunc = <<EOF

  FUNCTION Associated_#{structName}( &
             #{structName}, &  ! Input
             ANY_Test ) & ! Optional input
           RESULT(Association_Status)
    ! Arguments
    TYPE(#{structName}_type), INTENT(IN) :: #{structName}
    INTEGER, OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Test
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
EOF
# ALL Structure association test
assocFunc<<" "*ns*3 + "IF ( ASSOCIATED(#{structName}%#{arrayList.first[:name]}) .AND. &\n"
1.upto(arrayList.length-2) {|i| assocFunc<<" "*ns*5 + " ASSOCIATED(#{structName}%#{arrayList[i][:name]}) .AND. &\n"}
assocFunc<<" "*ns*5 + " ASSOCIATED(#{structName}%#{arrayList.last[:name]}) ) THEN\n"
assocFunc<<" "*ns*4 + "Association_Status = .TRUE.\n"
assocFunc<<" "*ns*3 + "END IF\n"
assocFunc<<" "*ns*2 + "ELSE\n"
# ANY Structure association test
assocFunc<<" "*ns*3 + "IF ( ASSOCIATED(#{structName}%#{arrayList.first[:name]}) .OR. &\n"
1.upto(arrayList.length-2) {|i| assocFunc<<" "*ns*5 + " ASSOCIATED(#{structName}%#{arrayList[i][:name]}) .OR. &\n"}
assocFunc<<" "*ns*5 + " ASSOCIATED(#{structName}%#{arrayList.last[:name]}) ) THEN\n"
assocFunc<<" "*ns*4 + "Association_Status = .TRUE.\n"
assocFunc<<" "*ns*3 + "END IF\n"
assocFunc<<" "*ns*2 + "END IF\n"
# The end
assocFunc<<"  END FUNCTION Associated_#{structName}\n"

puts assocFunc


# ---------------------------
# Create the Destroy function
# ---------------------------
# The array component deallocation statement
deallocStatement = "    DEALLOCATE( &\n"
arrayList.each {|a| deallocStatement<<" "*16 + structName + "%" + a[:name] + ", &\n"}
deallocStatement<<" "*16 + "STAT = Allocate_Status )"
# The function
destroyFunc = <<EOF

  FUNCTION Destroy_#{structName}( &
             #{structName}, &  ! Output
             No_Clear, &  ! Optional input
             RCS_Id, &  ! Revision control
             Message_Log ) &  ! Error messaging
           RESULT(Error_Status)
    ! Arguments
    TYPE(#{structName}_type), INTENT(IN OUT) :: #{structName} 
    INTEGER,      OPTIONAL, INTENT(IN)  :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_#{structName}'
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
    IF ( Clear ) CALL Clear_#{structName}(#{structName})

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_#{structName}(#{structName}) ) RETURN

    ! Deallocate the pointer members
#{deallocStatement}
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating #{structName}. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF
    
    ! Decrement and test allocation counter
    #{structName}%n_Allocates = #{structName}%n_Allocates - 1
    IF ( #{structName}%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      #{structName}%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM(Message), &
                            Error_Status,    &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION Destroy_#{structName}
EOF

puts destroyFunc


# ----------------------------
# Create the Allocate function
# ----------------------------
# The function declaration dimension inputs
dimFuncDef=""
dimList.each {|d| dimFuncDef<<" "*13 + d + ", &  ! Input\n"}
dimFuncDef.chomp! # Remove last newline
# The argument declaration dimension inputs
dimArgDef=""
dimList.each {|d| dimArgDef<<"    INTEGER, INTENT(IN) :: #{d}\n"}
dimArgDef.chomp! # Remove last newline
# The dimension value check
dimValCheck=""
dimValCheck<<"    " + "IF ( #{dimList.first} < 1 .OR. &\n"
1.upto(dimList.length-2) {|i| dimValCheck<<" "*ns*4 + " #{dimList[i]} < 1 .OR. &\n"}
dimValCheck<<" "*ns*4 + " #{dimList.last} < 1 ) THEN"
# The array component allocation statement
arrayAlloc = "    ALLOCATE( &\n"
arrayList.each {|a| arrayAlloc<<" "*14 + structName + "%" + a[:name] + "(" + a[:dims].join(",") + "), &\n"}
arrayAlloc<<" "*14 + "STAT = Allocate_Status )"
# The dimension assignment statements
dimAssign=""
dimList.each {|d| dimAssign<<"    #{structName}%#{d} = #{d}\n"}
# The array initialisation statements
arrayAssign=""
arrayList.each do |a|
  # Only intrinsic types are initialisaed
  next if a[:type].upcase == "TYPE"
  # Construct the initialistions
  arrayAssign<<"    " + structName + "%" + a[:name] + " = " 
  arrayAssign<< case a[:type].upcase
                  when "INTEGER"   then "0\n"
                  when "REAL"      then "ZERO\n"
                  when "CHARACTER" then "' '\n"
                  else raise StandardError, "*** Type #{a[:type]} array assign not implemented ***"
                end
end

# The function
allocFunc = <<EOF

  FUNCTION Allocate_#{structName}( &
#{dimFuncDef}
             #{structName}, &  ! Output
             RCS_Id,       &  ! Revision control
             Message_Log ) &  ! Error messaging
           RESULT( Error_Status )
    ! Arguments
#{dimArgDef}
    TYPE(#{structName}_type), INTENT(IN OUT) :: #{structName}
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_#{structName}'
    ! Local variables
    CHARACTER(256)  :: Message
    INTEGER :: Allocate_Status

    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Dimensions
#{dimValCheck}
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input #{structName} dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_#{structName}( #{structName}, ANY_Test=1 ) ) THEN
      Error_Status = Destroy_#{structName}( &
                       #{structName}, &
                       No_Clear=1, &
                       Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating #{structName} prior to allocation.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! Perform the pointer allocation
#{arrayAlloc}
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating #{structName} data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Assign the dimensions
#{dimAssign}
    ! Initialise the arrays
#{arrayAssign}
    ! Increment and test the allocation counter
    #{structName}%n_Allocates = #{structName}%n_Allocates + 1
    IF ( #{structName}%n_Allocates /= 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      #{structName}%n_Allocates
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION Allocate_#{structName}
EOF

puts allocFunc


# ==========================
# Create the Assign function
# ==========================

# The output structure allocation statement
allocStruct=indent(0)+"Error_Status = Allocate_#{structName}( &\n"
dimList.each {|d| allocStruct<<indent(8)+" #{structName}%#{d}, &\n"}
allocStruct<<indent(8)+" #{structName}_out, &\n"+indent(8)+" Message_Log=Message_Log)"

# --------------------------------
# The scalar assignment statements
# --------------------------------
scalarAssign=""
scalarList.each do |s|

  # ------------------------------------------------
  # Everything but derived types are simply assigned
  # ------------------------------------------------
  unless s[:type] == "TYPE"
    scalarAssign<<indent(0)+"#{structName}_out%#{s[:name]}=#{structName}_in%#{s[:name]}\n"

  # ---------------------------------------
  # Derived type call their Assign function
  # ---------------------------------------
  else
    i=indent(0)
    typeAssign = <<-EOF
#{i}Error_Status = Assign_#{s[:typename]}( &
#{i}                 #{structName}_in%#{s[:name]}, &
#{i}                 #{structName}_out%#{s[:name]}, &
#{i}                 Message_Log=Message )
#{i}IF ( Error_Status /= SUCCESS ) THEN
#{failReturn(1,"'Error copying #{structName} scalar structure component #{s[:name]}'")}
#{i}END IF
EOF
    scalarAssign<<typeAssign
  end
end

# -------------------------------
# The array assignment statements
# -------------------------------
arrayAssign=""
arrayList.each do |a|

  # ------------------------------------------------
  # Everything but derived types are simply assigned
  # ------------------------------------------------
  unless a[:type] == "TYPE"
    arrayAssign<<indent(0)+"#{structName}_out%#{a[:name]}=#{structName}_in%#{a[:name]}\n"

  # ---------------------------------------
  # Derived type call their Assign function
  # ---------------------------------------
  else
  
    # The loop beginning
    arrayAssign<<beginLoop(a)

    # The loop body
    i=indent(a[:ndims])
    dimLoopBody = <<EOF
#{i}Error_Status = Assign_#{a[:typename]}( &
#{i}                 #{structName}_in(#{a[:dimidx].join(",")})%#{a[:name]}, &
#{i}                 #{structName}_out(#{a[:dimidx].join(",")})%#{a[:name]}, &
#{i}                 Message_Log=Message_Log )
#{i}IF ( Error_Status /= SUCCESS ) THEN
#{failReturn(a[:ndims]+1,"'Error copying #{structName} array structure component #{a[:name]}'")}
#{i}END IF
EOF
    arrayAssign<<dimLoopBody
    
    # The loop end
    arrayAssign<<endLoop(a)
  end
end

# -------------------
# Create the function
# -------------------
assignFunc = <<EOF
  FUNCTION Assign_#{structName}( &
             #{structName}_in, &  ! Input
             #{structName}_out, &  ! Output
             RCS_Id, &  ! Revision control
             Message_Log ) &  ! Error messaging
           RESULT( Error_Status )
    ! Arguments
    TYPE(#{structName}_type), INTENT(IN)     :: #{structName}_in
    TYPE(#{structName}_type), INTENT(IN OUT) :: #{structName}_out
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_#{structName}'

    ! Set up
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. Associated_#{structName}(#{structName}_In) ) THEN
#{failReturn(1,"'Some or all INPUT #{structName} pointer members are NOT associated.'")}
    END IF

    ! Allocate data arrays
#{allocStruct}
    IF ( Error_Status /= SUCCESS ) THEN
#{failReturn(1,"'Error allocating output structure'")}
    END IF

    ! Assign non-dimension scalar members
#{scalarAssign}
    ! Copy array data
#{arrayAssign}
  END FUNCTION Assign_#{structName}
EOF

puts assignFunc


# =========================
# Create the Equal function
# =========================

# -----------------------------------
# The dimension value equality checks
# -----------------------------------
dimEqual=""
dimEqual<<indent(0) + "IF ( #{structName}_LHS%#{dimList.first} /= #{structName}_RHS%#{dimList.first} .OR. &\n"
1.upto(dimList.length-2) {|i| dimEqual<<indent(2) + " #{structName}_LHS%#{dimList[i]} /= #{structName}_RHS%#{dimList[i]} .OR. &\n"}
dimEqual<<indent(2) + " #{structName}_LHS%#{dimList.last} /= #{structName}_RHS%#{dimList.last} ) THEN"
dimCheck = <<EOF
#{failReturn(1,"'Structure dimensions are different.'")}
#{indent(0)}END IF
EOF
dimEqual<<dimCheck

# ------------------------------------
# The scalar component equality checks
# ------------------------------------
scalarEqual=""
scalarList.each do |s|
  case s[:type]

    # ------------------------------------------------
    # Integers and characters can be directly compared
    # ------------------------------------------------
    when "INTEGER","CHARACTER"
      i=indent(0)
      scalarCheck = <<EOF
#{i}IF( #{structName}_LHS%#{s[:name]} /= #{structName}_RHS%#{s[:name]}) THEN
#{failReturn(1,"'#{structName} scalar component #{s[:name]} values are different.'")}
#{i}END IF
EOF
      scalarEqual<<scalarCheck

    # ---------------------------------------
    # Floating point values use Compare_Float
    # ---------------------------------------
    when "REAL"
      i=indent(0)
      scalarCheck = <<EOF
#{i}IF ( .NOT. Compare_Float( #{structName}_LHS%#{s[:name]}, &
#{i}                          #{structName}_RHS%#{s[:name]}, &
#{i}                          ULP=ULP ) ) THEN
#{failReturn(1,"'#{structName} scalar component #{s[:name]} values are different.'")}
#{i}END IF
EOF
      scalarEqual<<scalarCheck

    # -----------------------------------------------
    # Other data structures use their Equal functions
    # -----------------------------------------------
    when "TYPE"
      i=indent(0)
      typeCheck = <<EOF
#{i}Error_Status = Equal_#{s[:typename]}( &
#{i}                 #{structName}_in%#{s[:name]}, &
#{i}                 #{structName}_out%#{s[:name]}, &
#{i}                 Message_Log=Message_Log )
#{i}IF ( Error_Status /= SUCCESS ) THEN
#{failReturn(1,"'#{structName} scalar structure component #{s[:name]} values are different.'")}
#{i}END IF
EOF
      scalarEqual<<typeCheck

    # ---------------------
    # Non-implemented types
    # ---------------------
    else
      raise StandardError, "*** Type #{s[:type]} array equality test not implemented ***"
  end
end

# -----------------------------------
# The array component equality checks
# -----------------------------------
arrayEqual=""
arrayList.each do |a|
  case a[:type]

    # ------------------------------------------------
    # Integers and characters can be directly compared
    # ------------------------------------------------
    when "INTEGER","CHARACTER"
      i=indent(0)
      arrayCheck = <<EOF
#{i}IF ( ANY( #{structName}_LHS%#{a[:name]} /= #{structName}_RHS%#{a[:name]} ) ) THEN
#{failReturn(1,"'#{structName} array component #{a[:name]} values are different.'")}
#{i}END IF
EOF
      arrayEqual<<arrayCheck

    # ---------------------------------------
    # Floating point values use Compare_Float
    # ---------------------------------------
    when "REAL"
    
      # The loop beginning
      arrayEqual<<beginLoop(a)

      # The loop body
      i=indent(a[:ndims])
      dimLoopBody = <<EOF
#{i}IF ( .NOT. Compare_Float( #{structName}_LHS(#{a[:dimidx].join(",")})%#{a[:name]}, &
#{i}                          #{structName}_RHS(#{a[:dimidx].join(",")})%#{a[:name]}, &
#{i}                          ULP=ULP ) ) THEN
#{i}  WRITE( Message, '("#{a[:name]} values are different at index (",#{a[:ndims]}(1x,i0),")")') &
#{i}                  #{a[:dimidx].join(",")} 
#{failReturn(a[:ndims]+1,"TRIM(Message)")}
#{i}END IF
EOF
      arrayEqual<<dimLoopBody
    
      # The loop end
      arrayEqual<<endLoop(a)

    # -----------------------------------------------
    # Other data structures use their Equal functions
    # -----------------------------------------------
    when "TYPE"

      # The loop beginning
      arrayEqual<<beginLoop(a)

      # The loop body
      i=indent(a[:ndims])
      dimLoopBody = <<EOF
#{i}Error_Status = Equal_#{a[:typename]}( &
#{i}                 #{structName}_in(#{a[:dimidx].join(",")})%#{a[:name]}, &
#{i}                 #{structName}_out(#{a[:dimidx].join(",")})%#{a[:name]}, &
#{i}                 Message_Log=Message_Log )
#{i}IF ( Error_Status /= SUCCESS ) THEN
#{i}  WRITE( Message, '("#{a[:name]} structures are different at index (",#{a[:ndims]}(1x,i0),")")') &
#{i}                  #{a[:dimidx].join(",")} 
#{failReturn(a[:ndims]+1,"TRIM(Message)")}
#{i}END IF
EOF
      arrayEqual<<dimLoopBody
    
      # The loop end
      arrayEqual<<endLoop(a)

    # ---------------------
    # Non-implemented types
    # ---------------------
    else
      raise StandardError, "*** Type #{a[:type]} array equality test not implemented ***"
  end
end

# -------------------
# Create the function
# -------------------
equalFunc = <<EOF
  FUNCTION Equal_#{structName}( &
             #{structName}_LHS, &  ! Input
             #{structName}_RHS, &  ! Input
             ULP_Scale,    &  ! Optional input
             RCS_Id,       &  ! Revision control
             Message_Log ) &  ! Error messaging
           RESULT( Error_Status )
    ! Arguments
    TYPE(#{structName}_type), INTENT(IN)  :: #{structName}_LHS
    TYPE(#{structName}_type), INTENT(IN)  :: #{structName}_RHS
    INTEGER,      OPTIONAL, INTENT(IN)  :: ULP_Scale
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_#{structName}'
    ! Local variables
    CHARACTER(256)  :: Message
    INTEGER :: ULP
    INTEGER :: l, j

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
    IF ( .NOT. Associated_#{structName}( #{structName}_LHS ) ) THEN
#{failReturn(1,"'Some or all INPUT #{structName}_LHS pointer members are NOT associated.'")}
    END IF
    IF ( .NOT. Associated_#{structName}( #{structName}_RHS ) ) THEN
#{failReturn(1,"'Some or all INPUT #{structName}_RHS pointer members are NOT associated.'")}
    END IF

    ! Check dimensions
#{dimEqual}

    ! Check the scalar components
#{scalarEqual}

    ! Check the array components
#{arrayEqual}
  END FUNCTION Equal_TauCoeff
  
EOF

puts equalFunc


# --------------------------
# Create the Info subroutine
# --------------------------
# The info format string
infoFmt="'("
dimList.each {|d| infoFmt<<"\"#{d.upcase}=\",i0,2x,"}
infoFmt<<")'"

puts infoFmt


