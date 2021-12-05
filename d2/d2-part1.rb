class Sub
  def initialize
    @pos = [0,0,0] # x, y, z - will only use x and z right now.
  end

  def pos; @pos; end
  def dx(n); @pos[0] += n; end
  def dy(n); @pos[1] += n; end
  def dz(n); @pos[2] += n; end

  def forward(n)
    dx(n)
  end
  def up(n)
    dz(-1 * n)
  end
  def down(n)
    dz(n)
  end
end

instructions = File.readlines("input.txt").map(&:split).map{|(cmd, val)| [cmd, val.to_i] }
sub = Sub.new
instructions.each do |cmd, n|
  sub.send(cmd, n)
end
puts "Sub now at #{sub.pos}"