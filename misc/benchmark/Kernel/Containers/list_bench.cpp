#include <benchmark/benchmark.h>
#include "list.hpp"


static list<long> gen(int64_t n) {
  auto normal = list<long>();
  for (long i=0; i<n; i++)
    normal << i;
  return normal;
}

static void N (benchmark::State& state) {
  auto list = gen(state.range(0));
  for (auto _ : state)
    N (list);
}
BENCHMARK (N)
  ->Arg(1)
  ->Arg(2)
  ->Arg(4)
  ->Arg(8)
  ->Arg(16)
  ->Arg(32)
  ->Arg(64)
  ->Arg(128)
  ->Arg(256)
  ->Arg(512)
  ->Arg(1024);
