# Put sensorinfo heirarchy at
# beginning of search path
$:.unshift File.join(File.dirname(__FILE__))

module SensorInfo

  class Node
    attr_accessor :sensor_id, :sensor_name, :satellite_name,
                  :microwave, :wmo_sensor_id, :wmo_satellite_id,
                  :n_channels, 
                  :channel, :use, :noise  
    def initialize(debug=false)
      @sensor_id        = ""
      @sensor_name      = ""
      @satellite_name   = ""
      @microwave        = 0      # Not microwave
      @wmo_sensor_id    = 2047   # Invalid
      @wmo_satellite_id = 1023   # Invalid
      @n_channels       = 0
      @channel = []  # Channel number
      @use     = []  # Use flag. O==do not use, 1==use
      @noise   = []  # Noise estimate (K)
      @debug = debug
    end

    def self.load(file_name,debug=false)
      list = Hash.new(self.new(debug))
      File.open(file_name,"r") do |fid|
        while line = fid.gets
          # Skip comments
          next if line =~ /^\s*!.*/
          # Process the entry header
          hdr = line.split
          node = self.new(debug)
          node.sensor_id        = hdr[2]
          node.sensor_name      = hdr[0]
          node.satellite_name   = hdr[1]
          node.microwave        = hdr[3].to_i
          node.wmo_sensor_id    = hdr[4].to_i
          node.wmo_satellite_id = hdr[5].to_i
          node.n_channels       = hdr[6].to_i
          # Read the entry channel data
          node.n_channels.times do
            ch = fid.gets.split
            node.channel << ch[0].to_i
            node.use     << ch[1].to_i
            node.noise   << ch[2].to_f
          end
          # Add current node to the list
          list[node.sensor_id] = node
        end
      end
      list
    end
  end
end
