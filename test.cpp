#include <cassert>
#include <iostream>

#include "closure.h"

void TestArg() {
  using namespace details::arg;

  static_assert(IsPrefixWeakV<ArgList<>, ArgList<>>);
  static_assert(std::is_same_v<RemovePrefixWeakT<ArgList<>, ArgList<>>, ArgList<>>);

  static_assert(IsPrefixWeakV<ArgList<>, ArgList<int>>);
  static_assert(std::is_same_v<RemovePrefixWeakT<ArgList<>, ArgList<int>>, ArgList<int>>);

  static_assert(IsPrefixWeakV<ArgList<int>, ArgList<int>>);
  static_assert(std::is_same_v<RemovePrefixWeakT<ArgList<int>, ArgList<int>>, ArgList<>>);

  static_assert(IsPrefixWeakV<ArgList<int>, ArgList<int, double>>);
  static_assert(std::is_same_v<RemovePrefixWeakT<ArgList<int>, ArgList<int, double>>, ArgList<double>>);

  static_assert(IsPrefixWeakV<ArgList<int, double>, ArgList<int, double, long>>);
  static_assert(std::is_same_v<RemovePrefixWeakT<ArgList<int, double>, ArgList<int, double, long>>, ArgList<long>>);

  static_assert(IsPrefixWeakV<ArgList<int>, ArgList<int, double, long>>);
  static_assert(std::is_same_v<RemovePrefixWeakT<ArgList<int>, ArgList<int, double, long>>, ArgList<double, long>>);

  static_assert(IsPrefixWeakV<ArgList<int, double, long>, ArgList<int, double, long>>);
  static_assert(std::is_same_v<RemovePrefixWeakT<ArgList<int, double, long>, ArgList<int, double, long>>, ArgList<>>);

  static_assert(!IsPrefixWeakV<ArgList<int, double, long>, ArgList<double, long>>);
  static_assert(!IsPrefixWeakV<ArgList<int, double, long>, ArgList<int, double>>);

  static_assert(IsPrefixWeakV<ArgList<const int&>, ArgList<int>>);
  static_assert(std::is_same_v<RemovePrefixWeakT<ArgList<const int&>, ArgList<int>>, ArgList<>>);
  static_assert(IsPrefixWeakV<ArgList<int&&>, ArgList<int>>);
  static_assert(std::is_same_v<RemovePrefixWeakT<ArgList<int&&>, ArgList<int>>, ArgList<>>);

  static_assert(IsPrefixWeakV<ArgList<int, double>, ArgList<int&&, double, const long&>>);
  static_assert(
      std::is_same_v<RemovePrefixWeakT<ArgList<int, double>, ArgList<int, double, const long&>>, ArgList<const long&>>);

  static_assert(
      IsPrefixWeakV<ArgList<decltype(std::ref(std::declval<int&>())), double>, ArgList<int&, double, const long&>>);
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

void test_ref(int& v) { v++; }

void TestClosure() {
  Closure<std::size_t(double, int, int)> closure1 = MakeClosure(sum, 1);
  assert(closure1.Run(2, 3, 4) == 10);

  Closure<int(std::unique_ptr<int>)> closure2 = MakeClosure(forwarding_test);
  assert(closure2.Run(std::make_unique<int>(10)) == 10);

  std::string exp = "11+12+13";
  assert(MakeClosure(calculate_sum, std::move(exp)).Run() == 36);
  assert(exp.size() == 0);

  int v = 0;
  auto closure3 = MakeClosure(test_ref, v);
  static_assert(!std::is_const_v<decltype(v)>);
  closure3.Run();
  assert(v == 0);
  closure3 = MakeClosure(test_ref, std::ref(v));
  closure3.Run();
  assert(v == 1);
}

void TestPlaceHolder() {
  static_assert(placeholders::IsContinuousSince<ArgList<placeholders::PH<2>>, 2>{});
  static_assert(
      placeholders::IsContinuousSince<ArgList<placeholders::PH<0>, placeholders::PH<1>, placeholders::PH<2>>, 0>{});
  static_assert(placeholders::IsContinuousSince<
                ArgList<placeholders::PH<3>, placeholders::PH<1>, placeholders::PH<0>, placeholders::PH<2>>, 0>{});
  static_assert(placeholders::IsContinuousSince<
                ArgList<placeholders::PH<6>, placeholders::PH<4>, placeholders::PH<3>, placeholders::PH<5>>, 3>{});

  using within_others =
      ArgList<int, int, placeholders::PH<2>, int, placeholders::PH<3>, placeholders::PH<0>, int, placeholders::PH<1>>;
  static_assert(placeholders::HasPlaceHolderV<within_others>);

  static_assert(
      std::is_same_v<typename placeholders::FilterPlaceHolder<within_others>::type,
                     ArgList<placeholders::PH<2>, placeholders::PH<3>, placeholders::PH<0>, placeholders::PH<1>>>);
}

int main() {
  TestArg();
  TestClosure();
  TestPlaceHolder();
}
