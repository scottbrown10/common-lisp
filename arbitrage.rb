start = 100000

n = readline.to_i

n.times do
  a, b, c = readline.split.map(&:to_f)
  after = start / a / b / c
  profit = (after - start).to_i
  puts (profit > 0 ? profit : 0)
end

