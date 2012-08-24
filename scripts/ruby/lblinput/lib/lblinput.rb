#!/usr/bin/env ruby
#
# $Id$
#

module LBLInput
  require 'rexml/document'

  # LBLInput Base class with shared code
  class Base

    # Extract an XML entry
    def extract_xml(xml,type)
      elements = []
      xml.elements.each("#{type}") do |e|
        elements << e
      end
      elements
    end

    # Extract the attributes
    # from an XML entry
    def extract_attribute(xml,attribute)
      xml.attributes[attribute].strip if xml.attributes[attribute]
    end

    # Generate output format
    # for an array of names
    def string_format(array)
      str_len=array.inject(0) {|memo,n| memo >= n.length ? memo : n.length}
      "%-#{str_len}.#{str_len}s"
    end

  private

    # The instance variables to
    # set from the XML input
    def instance_variable_list
      (instance_variables-["@xml","@entries","@parameters"])
    end

    # Set the instance variables
    # to the XML input value
    def set_instance_variables
      instance_variable_list.each do |var|
        instance_variable_set("#{var}", extract_attribute(@xml,"#{var.delete("@")}"))
      end
    end

    def display_instance_variables(indent)
      instance_variable_list.each do |var|
        puts("#{indent}#{var.delete("@").capitalize} : #{self.instance_variable_get(var)}")
      end
    end
  end
end



module LBLInput

  # LBLInput Parameter class for PARAMETER definitions
  class Parameter < LBLInput::Base

    attr_reader :name, :type, :value, :desc

    # Constructor
    def initialize(xml)
      @xml = xml
      @name  = nil
      @type  = nil
      @value = nil
      @desc  = nil
      set_instance_variables
    end


    # Housekeeping/XML methods

    def display
      display_instance_variables("    ")
    end


    # Code output methods

    def type_definition
      str = ""
      str << "  ! #{@desc}\n"
      str << "  #{@type}, PARAMETER :: #{@name} = #{@value}\n"
    end

  end
end


module LBLInput

  # LBLInput Entry class for type definition entries
  class Entry < LBLInput::Base

    attr_reader :type, :name, :dim, :value, :desc
    attr_accessor :type_fmt, :name_fmt, :value_fmt

    # Constructor
    def initialize(xml)
      @xml = xml
      @name  = nil
      @type  = nil
      @dim   = nil
      @value = nil
      @desc  = nil
      set_instance_variables
    end


    # Housekeeping/XML methods

    def display
      display_instance_variables("    ")
    end


    # Code output methods

    def type_definition
      if @dim
        name = "#{@name}#{@dim}"
        "    #{@type_fmt%@type} :: #{@name_fmt%name} = #{@value_fmt%@value}  !  #{@desc}\n"
      else
        "    #{@type_fmt%@type} :: #{@name_fmt%@name} = #{@value_fmt%@value}  !  #{@desc}\n"
      end
    end

  end
end


module LBLInput

  # LBLInput Record class for LBL input file records
  class Record < LBLInput::Base

    attr_reader :name, :type, :format, :entries

    # Constructor
    def initialize(xml)
      @xml  = xml
      @name   = nil
      @type   = nil
      @format = nil
      @parameters = extract_parameters
      @entries = extract_entries
      set_instance_variables
    end


    # Housekeeping/XML methods

    def display
      display_instance_variables("  ")
      display_type("parameter",@parameters) if @parameters
      display_type("entry",@entries) if @entries
    end

    def display_type(type, array)
      array.each_with_index do |x,i|
        puts("  #{type.capitalize} \##{i}: #{x}")
        x.display
      end
    end

    # Extract the parameters from the XML input
    # and add to the parameter class array
    def extract_parameters
      parameters = []
      extract_xml(@xml,"parameter").each do |p|
        parameters << LBLInput::Parameter.new(p)
      end
      parameters.empty? ? nil : parameters
    end

    # Extract the type definition entries from the
    # XML input and add to the entries class array
    def extract_entries
      entries = []
      extract_xml(@xml,"entry").each do |e|
        entries << LBLInput::Entry.new(e)
      end
      # Get the output formats
      tfmt=string_format(entries.collect {|e| e.type})
      nfmt=string_format(entries.collect {|e| e.dim ? "#{e.name}#{e.dim}" : e.name})
      vfmt=string_format(entries.collect {|e| e.value})
      # ...save them for each entry
      entries.each do |e|
        e.type_fmt  = tfmt
        e.name_fmt  = nfmt
        e.value_fmt = vfmt
      end
      entries.empty? ? nil : entries
    end


    # Code output methods

    PROCEDURES = ["Write"]

    def module_root
      "#{@type.upcase}_#{@name}"
    end

    def module_name
      "#{module_root}_Module"
    end

    def module_doc_header
      str=<<-EOF
!
! #{module_name}
!
! Module containing procedures for #{@type.upcase} Record #{@name}.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, #{Time.now.strftime("%d-%b-%Y")}
!                     paul.vandelst@noaa.gov
!
      EOF
    end


    def module_begin
      "\nMODULE #{module_name}"
    end


    def module_environment
      str=<<-EOF
\n
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds     , ONLY: fp
  USE File_Utility   , ONLY: File_Open
  USE Message_Handler, ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  ! Line-by-line model parameters
  USE LBL_Parameters
  ! Disable implicit typing
  IMPLICIT NONE
      EOF
    end


    def module_datatype_names
      "  PUBLIC :: #{module_root}_type"
    end


    def module_procedure_names
      str=""
      PROCEDURES.each do |p|
        str << "  PUBLIC :: #{module_root}_#{p}\n"
      end
      str.chomp
    end


    def module_visibility
      str=<<-EOF
\n
  ! ----------
  ! Visibility
  ! ----------
  ! Everything private by default
  PRIVATE
  ! Datatypes
#{module_datatype_names}
  ! Procedures
#{module_procedure_names}
      EOF
    end


    def module_fmt_parameter
      "#{module_root.upcase}_FMT"
    end


    def module_default_parameters
      str=<<-EOF
\n
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: #{module_fmt_parameter} = '#{@format}'
      EOF
    end


    def module_parameters
      str=""
      @parameters.each {|p| str << p.type_definition} if @parameters
      str
    end


    def module_datatype_defn_begin
      str=<<-EOF
\n
  ! -------------
  ! Derived types
  ! -------------
  TYPE :: #{module_root}_type
      EOF
    end

    def module_datatype_defn_middle
      str=""
      @entries.each {|e| str << e.type_definition}
      str
    end

    def module_datatype_defn_end
      "  END TYPE #{module_root}_type\n"
    end


    def module_datatype_defn
      module_datatype_defn_begin << module_datatype_defn_middle << module_datatype_defn_end
    end


    def module_contains
      "\n\nCONTAINS\n"
    end


    def module_end
      "\nEND MODULE #{module_name}"
    end

    def module_procedure_write
      procedure_name = "#{module_root}_Write"
      type = "TYPE(#{module_root}_type)"
      fmt = string_format([type,"INTEGER"])
      str=<<-EOF
\n
  FUNCTION #{procedure_name}(#{@name},fid) RESULT(err_stat)

    ! Arguments
    #{fmt%type}, INTENT(IN) :: #{@name}
    #{fmt%"INTEGER"}, INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = '#{procedure_name}'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat

    ! Setup
    err_stat = SUCCESS
    ! ...Check unit is open
    IF ( .NOT. File_Open(fid) ) THEN
      msg = 'File unit is not connected'
      CALL Cleanup(); RETURN
    END IF

    ! Write the record
    WRITE( fid,FMT=#{module_fmt_parameter},IOSTAT=io_stat) #{@name}
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing record. IOSTAT = ",i0)' ) io_stat
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION #{procedure_name}
      EOF
    end

  end
end



module LBLInput
  class Input < LBLInput::Base

    attr_reader :records

    # Constructor
    def initialize(file)
      @xml     = REXML::Document.new(File.open(file)).root
      puts @xml.inspect
      @records = extract_records
    end


    # Housekeeping/XML methods

    def display
      @records.each_with_index do |r,i|
        puts("\nRecord \##{i}: #{r}")
        r.display
      end
    end

    def extract_records
      records = []
      extract_xml(@xml,"record").each do |r|
        records << LBLInput::Record.new(r)
      end
      records
    end


    # Code output methods

    def module_write
      @records.each do |r|
        file = "#{r.module_name}.f90"
        File.open(file,'w') do |f|
          f << r.module_doc_header
          f << r.module_begin
          f << r.module_environment
          f << r.module_visibility
          f << r.module_default_parameters
          f << r.module_parameters
          f << r.module_datatype_defn
          f << r.module_contains
          f << r.module_procedure_write
          f << r.module_end
        end
      end
    end

  end
end
