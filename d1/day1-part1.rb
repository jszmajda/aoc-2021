depths = File.readlines('day1-input.txt').map(&:to_i)
increases = 0
cur = depths.first
depths.each do |e|
  if e > cur
    increases += 1
  end
  cur = e
end
puts increases
