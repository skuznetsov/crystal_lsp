# EXPECT: stored_proc_ok
# Tests Proc stored in array and called later with captured variable.
# Reduced repro for closure capture null-deref bug found in stage2 investigation.

procs = [] of Proc(Int32)
total = 0

procs << ->{ total += 10; total }
procs << ->{ total += 20; total }

procs.each do |p|
  p.call
end

if total == 30
  puts "stored_proc_ok"
else
  puts "stored_proc_bad: #{total}"
end
