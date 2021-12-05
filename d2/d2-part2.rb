class Sub
  def initialize
    @hpos = 0
    @depth = 0
    @aim = 0
  end

  def pos; [@hpos, @depth, @aim]; end

  def d_aim(n); @aim += n; end

  def forward(n)
    @hpos += n
    @depth += (n * @aim)
  end
  def up(n)
    d_aim(-1 * n)
  end
  def down(n)
    d_aim(n)
  end
end

instructions = File.readlines("input.txt").map(&:split).map{|(cmd, val)| [cmd, val.to_i] }
sub = Sub.new
instructions.each do |cmd, n|
  sub.send(cmd, n)
end
puts "Sub now at #{sub.pos}"