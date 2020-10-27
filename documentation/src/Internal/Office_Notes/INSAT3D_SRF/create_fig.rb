#! /usr/bin/env ruby

require 'optparse'

# Command line option processing
options = {}
# ...Specify the options
opts = OptionParser.new do |opts|
  opts.banner = "Usage: create_fig.rb sndr|imgr $"
  # Simple switch to create the Tfit plot includes.
  options[:tfit] = false
  opts.on("-t", "--tfit", "Set this option to create the Tfit includes") do
    options[:tfit] = true
  end
  # The help screen
  opts.on_tail("-h", "--help", "Show this message") do
    puts opts
    exit(0)
  end
end
# ...Parse the options
begin
  opts.parse! ARGV
rescue OptionParser::ParseError => error_message
  puts("ERROR --> #{error_message}")
  puts(opts)
  exit(1)
end
# ...Check the arguments
begin
  raise 'Must specify either "sndr" or "imgr" sensor.' if ARGV.length < 1
  sensor = ARGV[0]
rescue Exception => error_message
  puts("ERROR --> #{error_message}")
  exit(1)
end

# Set sensor specific bits
case sensor
  when "imgr"
    full_name="Imager"
    channel_begin=3
    channel_end  =6
  when "sndr"
    full_name="Sounder"
    channel_begin=1
    channel_end  =18
  else
    raise "Invalid sensor name"
end

# Loop over channels
(channel_begin..channel_end).each do |i|

  if options[:tfit] then
  
    # Temperature residual plot includes
    src=<<-EOF

\\subsection{Channel #{i}}
\\begin{figure}[H]
  \\label{fig:#{sensor}_ch#{i}_tfit}
  \\centering
  \\begin{tabular}{c}
    \\includegraphics[scale=0.55]{graphics/#{sensor}/tfit/#{sensor}_insat3d-#{i}.tfit.eps} \\\\
    \\includegraphics[scale=0.55]{graphics/#{sensor}/tfit/#{sensor}_insat3d-#{i}.tfit.difference.eps}
  \\end{tabular}
  \\caption{INSAT-3D #{full_name} channel #{i} polychromatic correction temperature fit residuals. \\emph{(Top)} Comparison of residuals for original and new SRFs. \\emph{(Bottom)} Residual differences for the original and new SRFs.}
\\end{figure}
EOF

  else
  
    # SRF plot  includes
    src=<<-EOF

\\subsection{Channel #{i}}
\\begin{figure}[H]
  \\label{fig:#{sensor}_ch#{i}}
  \\centering
  \\begin{tabular}{c}
    \\includegraphics[scale=0.55]{graphics/#{sensor}/srf/#{sensor}_insat3d-#{i}.eps} \\\\
    \\includegraphics[scale=0.55]{graphics/#{sensor}/srf/#{sensor}_insat3d-#{i}.difference.eps}
  \\end{tabular}
  \\caption{INSAT-3D #{full_name} channel #{i} spectral responses. Vertical dashed lines are the locations of the computed central frequencies. \\emph{(Top)} Comparison of original and new SRFs. \\emph{(Bottom)} Response difference between the original and new SRFs.}
\\end{figure}
EOF
  end
  
  puts src
  
end
