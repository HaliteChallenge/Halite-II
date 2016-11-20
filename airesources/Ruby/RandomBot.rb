$:.unshift(File.dirname(__FILE__))
require 'networking'

network = Networking.new("RandomRubyBot")
tag, map = network.configure

while true
  moves = []
  map = network.frame

  (0...map.height).each do |y|
    (0...map.width).each do |x|
      loc = Location.new(x, y)
      site = map.site(loc)

      if site.owner == tag
        moves << Move.new(loc, GameMap::DIRECTIONS.shuffle.first)
      end
    end
  end

  network.send_moves(moves)
end

