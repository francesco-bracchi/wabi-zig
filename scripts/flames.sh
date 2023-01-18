zig-out/bin/wabivm < amb.wabi&
pid=$!
echo "PID: $pid"
perf record -g -p $pid
perf script > out.stacks1
cat out.stacks1 | stackcollapse-perf.pl | grep -v cpu_idle | flamegraph.pl --color=java --hash > out.stacks01.svg
firefox out.stacks01.svg
