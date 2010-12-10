#!/usr/bin/env ruby

types = ['Dust          ',
         'SeaSalt_AM    ',
         'SeaSalt_CM1   ',
         'SeaSalt_CM2   ',
         'SeaSalt_CM3   ',
         'Organic_Carbon',
         'Black_Carbon  ',
         'Sulfate       ']


def keywords(types)
  types.each do |t|
    puts str=<<-EOT
  #{t} = #{t}, $
  EOT
  end
end


# Aerosol type IsA functions
puts str=<<-EOT
; Aerosol type IsA functions
EOT
types.each do |t|
  puts str=<<-EOT
FUNCTION Aerosol::IsA_#{t.strip}_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, Type EQ #{t.upcase.strip}_AEROSOL
END
  EOT
end

# Generic TypeOf function
puts str=<<-EOT

; Generic TypeOf function
FUNCTION Aerosol::TypeOf, $
EOT
keywords(types)
puts str=<<-EOT
  Debug   = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_func_err_handler
  EOT
types.each do |t|
  puts str=<<-EOT
  IF ( KEYWORD_SET(#{t}) ) THEN RETURN, self->IsA_#{t.strip}_Aerosol(Debug = Debug)
  EOT
end
puts str=<<-EOT
  RETURN, FALSE
END
EOT


# Aerosol type SetTo procedures
puts str=<<-EOT

; Aerosol type SetTo procedures
EOT
types.each do |t|
  puts str=<<-EOT
PRO Aerosol::SetTo_#{t.strip}_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_pro_err_handler
  self->Set_Property, Type = #{t.upcase.strip}_AEROSOL, Debug = Debug
END
  EOT
end


# Aerosol type code definition function
puts str=<<-EOT

; Aerosol type code definition function
FUNCTION Aerosol::TypeCode, $
EOT
keywords(types)
puts str=<<-EOT
  Debug   = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_func_err_handler
  EOT
types.each do |t|
  puts str=<<-EOT
  IF ( KEYWORD_SET(#{t}) ) THEN RETURN, #{t.upcase.strip}_AEROSOL
  EOT
end
puts str=<<-EOT
  RETURN, INVALID_AEROSOL
END
EOT


# Aerosol type code definition function
puts str=<<-EOT

; Aerosol type name function
FUNCTION Aerosol::TypeName, $
  Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, AEROSOL_TYPE_NAME[Type]
END
EOT
