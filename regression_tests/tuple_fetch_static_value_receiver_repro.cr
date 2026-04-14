t = {1.25_f64}
direct = t[0]
fetched = t.fetch(0) { 0.0 }

raise "tuple direct index failed: #{direct}" unless direct == 1.25_f64
raise "tuple fetch failed: #{fetched}" unless fetched == 1.25_f64

puts "tuple_fetch_static_value_receiver_ok"
