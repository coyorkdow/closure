#include <cassert>
#include <iostream>

#include "closure.h"

void TestArg() {
  using namespace details::arg;

  static_assert(IsPrefixV<ArgList<>, ArgList<>>);
  static_assert(std::is_same_v<RemovePrefixT<ArgList<>, ArgList<>>, ArgList<>>);

  static_assert(IsPrefixV<ArgList<>, ArgList<int>>);
  static_assert(std::is_same_v<RemovePrefixT<ArgList<>, ArgList<int>>, ArgList<int>>);

  static_assert(IsPrefixV<ArgList<int>, ArgList<int>>);
  static_assert(std::is_same_v<RemovePrefixT<ArgList<int>, ArgList<int>>, ArgList<>>);

  static_assert(IsPrefixV<ArgList<int>, ArgList<int, double>>);
  static_assert(std::is_same_v<RemovePrefixT<ArgList<int>, ArgList<int, double>>, ArgList<double>>);

  static_assert(IsPrefixV<ArgList<int, double>, ArgList<int, double, long>>);
  static_assert(std::is_same_v<RemovePrefixT<ArgList<int, double>, ArgList<int, double, long>>, ArgList<long>>);

  static_assert(IsPrefixV<ArgList<int>, ArgList<int, double, long>>);
  static_assert(std::is_same_v<RemovePrefixT<ArgList<int>, ArgList<int, double, long>>, ArgList<double, long>>);

  static_assert(IsPrefixV<ArgList<int, double, long>, ArgList<int, double, long>>);
  static_assert(std::is_same_v<RemovePrefixT<ArgList<int, double, long>, ArgList<int, double, long>>, ArgList<>>);

  static_assert(!IsPrefixV<ArgList<int, double, long>, ArgList<double, long>>);
  static_assert(!IsPrefixV<ArgList<int, double, long>, ArgList<int, double>>);
}

std::size_t sum(const int& v1, double v2, int v3, int v4) { return v1 + v2 + v3 + v4; }

int forwarding_test(std::unique_ptr<int> p) { return *p.get(); }

int calculate_sum(std::string exp) {
  int ans = 0;
  size_t pos = 0;
  int cur_num = 0;
  for (auto iter = exp.begin(); iter < exp.end(); ++iter) {
    if (*iter == '+') {
      assert(iter != exp.begin());
      ans += cur_num;
      cur_num = 0;
    } else {
      assert('0' <= *iter && *iter <= '9');
      cur_num = cur_num * 10 + *iter - '0';
    }
  }
  ans += cur_num;
  return ans;
}

void TestClosure() {
  Closure<std::size_t(double, int, int)> closure1 = MakeClosure(sum, 1);
  assert(closure1.Run(2, 3, 4) == 10);

  Closure<int(std::unique_ptr<int>)> closure2 = MakeClosure(forwarding_test);
  assert(closure2.Run(std::make_unique<int>(10)) == 10);

  std::string exp = "11+12+13";
  assert(MakeClosure(calculate_sum, std::move(exp)).Run() == 36);
  assert(exp.size() == 0);
}

int main() {
  TestArg();
  TestClosure();
}