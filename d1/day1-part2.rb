depths = File.readlines('day1-input.txt').map(&:to_i)
increases = 0
cur = depths[0..2].sum
depths.each_cons(3) do |window|
  e = window.sum
  if e > cur
    increases += 1
  end
  cur = e
end
puts increases
