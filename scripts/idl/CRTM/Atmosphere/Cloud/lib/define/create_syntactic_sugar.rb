#!/usr/bin/env ruby

types = ['water  ',
         'ice    ',
         'rain   ',
         'snow   ',
         'graupel',
         'hail   ']

def keywords(types)
  types.each do |t|
    puts str=<<-EOT
  #{t.capitalize} = #{t.capitalize}, $
  EOT
  end
end


# Cloud type IsA functions
puts str=<<-EOT
; Cloud type IsA functions
EOT
types.each do |t|
  puts str=<<-EOT
FUNCTION Cloud::IsA_#{t.capitalize.strip}_Cloud, Debug = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, Type EQ #{t.upcase.strip}_CLOUD
END
  EOT
end

# Generic TypeOf function
puts str=<<-EOT

; Generic TypeOf function
FUNCTION Cloud::TypeOf, $
EOT
keywords(types)
puts str=<<-EOT
  Debug   = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_func_err_handler
  EOT
types.each do |t|
  puts str=<<-EOT
  IF ( KEYWORD_SET(#{t.capitalize}) ) THEN RETURN, self->IsA_#{t.capitalize.strip}_Cloud(Debug = Debug)
  EOT
end
puts str=<<-EOT
  RETURN, FALSE
END
EOT


# Cloud type SetTo procedures
puts str=<<-EOT

; Cloud type SetTo procedures
EOT
types.each do |t|
  puts str=<<-EOT
PRO Cloud::SetTo_#{t.capitalize.strip}_Cloud, Debug = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_pro_err_handler
  self->Set_Property, Type = #{t.upcase.strip}_CLOUD, Debug = Debug
END
  EOT
end


# Cloud type code definition function
puts str=<<-EOT

; Cloud type code definition function
FUNCTION Cloud::TypeCode, $
EOT
keywords(types)
puts str=<<-EOT
  Debug   = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_func_err_handler
  EOT
types.each do |t|
  puts str=<<-EOT
  IF ( KEYWORD_SET(#{t.capitalize}) ) THEN RETURN, #{t.upcase.strip}_CLOUD
  EOT
end
puts str=<<-EOT
  RETURN, INVALID_CLOUD
END
EOT


# Cloud type code definition function
puts str=<<-EOT

; Cloud type name function
FUNCTION Cloud::TypeName, $
  Debug = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, CLOUD_TYPE_NAME[Type]
END
EOT
