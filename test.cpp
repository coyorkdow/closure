#include <iostream>
#include <cassert>
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

void TestClosure() {
  auto closure1 = NewClosure(sum, 1);
  assert(closure1->Run(2, 3, 4) == 10);
  delete closure1;
}

int main() {
  TestArg();
  TestClosure();
}