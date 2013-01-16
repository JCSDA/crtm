# Put current heirarchy at
# beginning of search path
$:.unshift File.join(File.dirname(__FILE__))

require 'base/base'

module Fortran
  
  class BaseType
    # "Mandatory" class variables
#    attr_reader :type, :name
    # "Optional" class variables for definition
#    attr_reader :kind, :len, :attributes, :value, :dimspec
    # "Optional" class variables for definition formatting
#    attr_reader :name_align, :description, :indent, :newline

    # Defaults for all optional class variables
    OPT_DEFAULT = {
      :kind        => String.new,
      :len         => "",
      :dimspec     => "",
      :value       => "",
      :attributes  => [],
      :name_align  => 0,
      :description => "",
      :indent      => "",
      :newline     => false }
    
    def initialize(type, name, optional={})
      # The mandatory bits
      @type       = type.upcase
      @name       = name
      # The optional bits
      set_default_options()
      process_options(optional)
    end
    
    def definition
      str = @indent.dup
      str << @type
      if @type.upcase == "CHARACTER"
        str << "(#{@len})" unless @len.empty?    # Only LEN accepted for character for now
      else
        str << "(#{@kind})" unless @kind.empty?
      end
      @attributes.sort.each {|a| str << ", #{a}"} unless @attributes.empty?
      str << " :: #{@name}"
      str << "#{@dimspec}" unless @dimspec.empty?
      str << " "*@name_align
      assign_opr = @attributes.include?("POINTER") ? "=>" : "="
      str << " #{assign_opr} #{@value}" unless @value.empty?
      str << "  ! #{@description}" unless @description.empty?
      str << "\n" if @newline
      str
    end
    
  private

    def set_default_options
      process_options(OPT_DEFAULT)
    end
      
    def process_options(options)
      options.each do |name, value|
        case name
          # Options that are just assigned
          when :kind, :len, :dimspec, :value, :description, :newline, :indent
            instance_variable_set("@#{name.to_s}", value) if value
          # Options that must be integers
          when :name_align
            instance_variable_set("@#{name.to_s}", value.to_i) if value
          # Special cases
          # ...Attributes
          when :attributes
            instance_variable_set("@#{name.to_s}", [value].flatten.collect {|a| a.upcase}) if value
          # Handle unallowed options (?)
          else
            raise(ArgumentError, "Initialisation option #{name} invalid!")
        end
      end
    end
  end

end
