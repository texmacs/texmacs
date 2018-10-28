#include <benchmark/benchmark.h>
#include "list.hpp"

static void N (benchmark::State& state) {
  auto normal = list<long>(1, 2, 3, list<long>());
  for (auto _ : state)
    N (normal);
}
BENCHMARK(N);
