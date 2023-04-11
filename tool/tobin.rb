#!/bin/ruby

File.open(ARGV[0]) do |f|
    bins = []
    f.each_line do |line|
        if m = line.match(/:(( ([0-9a-f]){4})*)/)
            ws = m[1].strip.split(' ').map{|w| x = w.to_i(16); [x>>8, x&0xff]}.flatten
            bins.concat(ws)
        end
    end
    print bins.pack('C*')
end

