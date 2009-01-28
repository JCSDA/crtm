module FDoc
  module Type
    class Generator < FDoc::Base
    
      TAG = "tdoc"
      TYPE_REGEXP = %r{^\s*TYPE\s*(?:,\s*PUBLIC\s*)?::\s*(\w*)}ix
      
      def initialize(source)
        @source = source
        @list   = []
      end
      
      def generate
        list(@source.extract(TAG))
        generate_latex
      end

      # Create hash list of entries
      def list(entries)
        @list = []
        entries.each {|e| @list << {:name=>$1, :entry=>e} if e =~ TYPE_REGEXP}
      end
      
      def generate_latex
        @list.each do |element|
          latex_string=<<-EOT
\\begin{figure}[htp]
  \\centering
  \\doublebox{
  \\begin{minipage}[b]{6.5in}
    \\begin{alltt}
#{element[:entry]}
    \\end{alltt}
  \\end{minipage}
  }
  \\caption{#{element[:name].gsub(/_/,"\\_")} structure definition.}
  \\label{fig:#{element[:name]}_structure}
\\end{figure}
          EOT
          tex_file_name = "#{element[:name]}.tex"
          File.open(tex_file_name,'w') {|f| f.write(latex_string)}
          signal(tex_file_name)
        end
      end      
    end
  end
end


