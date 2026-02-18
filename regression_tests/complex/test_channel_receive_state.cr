# EXPECT: channel_receive_ok

channel = Channel(Int32).new

spawn do
  channel.send(7)
end

received = channel.receive
puts(received == 7 ? "channel_receive_ok" : "channel_receive_bad")
