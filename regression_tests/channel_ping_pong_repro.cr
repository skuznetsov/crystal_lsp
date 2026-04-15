# Reduces bench_comprehensive ping-pong failure (final=0 instead of rounds).
# Two fibers exchange Int32 via Channel(Int32); after `rounds` ping-pong
# round-trips the main fiber's `result` should equal `rounds`.
# EXPECT: channel_ping_pong_ok
ping_ch = Channel(Int32).new
pong_ch = Channel(Int32).new
rounds = 10

spawn do
  i = 0
  while i < rounds
    val = ping_ch.receive
    pong_ch.send(val + 1)
    i += 1
  end
end

result = 0
j = 0
while j < rounds
  ping_ch.send(result)
  result = pong_ch.receive
  j += 1
end

if result == rounds
  puts "channel_ping_pong_ok"
else
  puts "channel_ping_pong_fail"
  puts result
end
