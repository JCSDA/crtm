#! /usr/bin/env ruby

require 'optparse'

# Command line option processing
options = {}
# ...Specify the options
opts = OptionParser.new do |opts|
  opts.banner = "Usage: create_fig.rb Tset|Vset $"
  # Simple switch to create the Tfit plot includes.
  options[:tfit] = false
  opts.on("-t", "--tfit", "Set this option to create the Tfit includes") do
    options[:tfit] = true
  end
  # Simple switch to create the dTb plot includes.
  options[:dtb] = false
  opts.on("-d", "--dtb", "Set this option to create the dTb includes") do
    options[:dtb] = true
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
  raise 'Must specify either "Tset" or "Vset" dataset indicator.' if ARGV.length < 1
  dataset = (ARGV[0].downcase).capitalize
rescue Exception => error_message
  puts("ERROR --> #{error_message}")
  exit(1)
end

# Set dataset specific bits
case dataset
  when "Tset"
    dtb_descriptor = '-10\textdegree{}C and 50\textdegree{}C compared to nominal temperature (20\textdegree{}C) at nominal bias voltage'
    descriptor     = 'voltage for the three test temperatures: 20\textdegree{}C (nominal), -10\textdegree{}C, and 50\textdegree{}C'
  when "Vset"
    dtb_descriptor = '$V_{LO}$ and $V_{HI}$ compared to nominal bias voltage at nominal temperature (20\textdegree{}C)'
    descriptor     = 'temperature (20\textdegree{}C) for the three test voltages: $V_{NOM}$, $V_{LO}$, and $V_{HI}$'
  else
    raise "Invalid dataset name"
end


# Define the output filenames
srf_file  = "#{dataset}.srf.fig" ; f_srf  = File.open(srf_file ,"w")
tfit_file = "#{dataset}.tfit.fig"; f_tfit = File.open(tfit_file,"w")
dtb_file  = "#{dataset}.dtb.fig" ; f_dtb  = File.open(dtb_file ,"w")


# Loop over channels
sensor_id     = "atms_npp"
channel_begin = 1
channel_end   = 22
n_channels    = channel_end - channel_begin + 1

hspace = Hash.new("1.75cm")
hspace[6]      = "0.75cm"
(10..15).each {|ch| hspace[ch] = "0.75cm"}
(17..22).each {|ch| hspace[ch] = "0.75cm"}


(channel_begin..channel_end).each do |i|

  # Temperature residual plot includes
  if options[:tfit] then
    src=<<-EOF

\\subsection{Channel #{i}}
\\begin{figure}[H]
  \\label{fig:#{dataset}.ch#{i}_tfit}
  \\centering
  \\includegraphics[scale=0.45]{graphics/tfit/#{dataset}/#{sensor_id}-#{i}.tfit.eps}
  \\caption{ATMS channel #{i} polychromatic correction temperature fit residuals at nominal #{descriptor}.}
\\end{figure}
EOF
    f_tfit << src
  end

  
  # delta brightness temperature plot includes
  if options[:dtb] then
    src=<<-EOF

\\subsection{Channel #{i}}
\\begin{figure}[H]
  \\label{fig:#{dataset}.ch#{i}_dtb}
  \\centering
  \\hspace{1.5cm}\\includegraphics[scale=0.45]{graphics/dtb/#{dataset}/#{sensor_id}.ch#{i}.dTb_T_PW_stats.eps}
  \\caption{ATMS channel #{i} differences in brightness temperatures as a function of $T_B$ (top) and total preciptable water (bottom) for #{dtb_descriptor}. MonoRTM v5.0 was used with $\\epsilon=0.6$ and $r=0.4$ for the ECMWF83 profile set.}
\\end{figure}
EOF
    f_dtb << src
  end

  
  # SRF plot includes
  src=<<-EOF

\\subsection{Channel #{i}}
\\begin{figure}[H]
  \\label{fig:#{dataset}.ch#{i}_response}
  \\centering
  \\begin{tabular}{c}
    \\hspace{#{hspace[i]}}\\sffamily\\textbf{Linear y-axis} \\\\
    \\includegraphics[scale=0.55]{graphics/srf/#{dataset}/lin/#{sensor_id}-#{i}.eps} \\\\
    \\hspace{#{hspace[i]}}\\sffamily\\textbf{Base-10 logarithmic y-axis} \\\\
    \\includegraphics[scale=0.55]{graphics/srf/#{dataset}/log/#{sensor_id}-#{i}.eps}
  \\end{tabular}
  \\caption{ATMS channel #{i} response at nominal #{descriptor}. Vertical dashed lines are the locations of the computed central frequencies. \\textbf{(Top)} Linear y-axis. \\textbf{(Bottom)} Base-10 logarithmic y-axis.}
\\end{figure}
EOF
  f_srf << src

end

f_srf.close
f_tfit.close
f_dtb.close
