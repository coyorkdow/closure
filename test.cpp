#include <cassert>
#include <iostream>

#include "closure.h"
#include "gtest/gtest.h"

using namespace closure;

TEST(TestArg, Main) {
  using namespace details;

  static_assert(IsPrefixWeakV<ArgList<>, ArgList<>>, "");
  static_assert(__CLOSTD::is_same_v<RemovePrefixWeakT<ArgList<>, ArgList<>>, ArgList<>>, "");

  static_assert(IsPrefixWeakV<ArgList<>, ArgList<int>>, "");
  static_assert(__CLOSTD::is_same_v<RemovePrefixWeakT<ArgList<>, ArgList<int>>, ArgList<int>>, "");

  static_assert(IsPrefixWeakV<ArgList<int>, ArgList<int>>, "");
  static_assert(__CLOSTD::is_same_v<RemovePrefixWeakT<ArgList<int>, ArgList<int>>, ArgList<>>, "");

  static_assert(IsPrefixWeakV<ArgList<int>, ArgList<int, double>>, "");
  static_assert(__CLOSTD::is_same_v<RemovePrefixWeakT<ArgList<int>, ArgList<int, double>>, ArgList<double>>, "");

  static_assert(IsPrefixWeakV<ArgList<int, double>, ArgList<int, double, long>>, "");
  static_assert(__CLOSTD::is_same_v<RemovePrefixWeakT<ArgList<int, double>, ArgList<int, double, long>>, ArgList<long>>,
                "");

  static_assert(IsPrefixWeakV<ArgList<int>, ArgList<int, double, long>>, "");
  static_assert(__CLOSTD::is_same_v<RemovePrefixWeakT<ArgList<int>, ArgList<int, double, long>>, ArgList<double, long>>,
                "");

  static_assert(IsPrefixWeakV<ArgList<int, double, long>, ArgList<int, double, long>>, "");
  static_assert(
      __CLOSTD::is_same_v<RemovePrefixWeakT<ArgList<int, double, long>, ArgList<int, double, long>>, ArgList<>>, "");

  static_assert(!IsPrefixWeakV<ArgList<int, double, long>, ArgList<double, long>>, "");
  static_assert(!IsPrefixWeakV<ArgList<int, double, long>, ArgList<int, double>>, "");

  static_assert(IsPrefixWeakV<ArgList<const int&>, ArgList<int>>, "");
  static_assert(__CLOSTD::is_same_v<RemovePrefixWeakT<ArgList<const int&>, ArgList<int>>, ArgList<>>, "");
  static_assert(IsPrefixWeakV<ArgList<int&&>, ArgList<int>>, "");
  static_assert(__CLOSTD::is_same_v<RemovePrefixWeakT<ArgList<int&&>, ArgList<int>>, ArgList<>>, "");

  static_assert(IsPrefixWeakV<ArgList<int, double>, ArgList<int&&, double, const long&>>, "");
  static_assert(__CLOSTD::is_same_v<RemovePrefixWeakT<ArgList<int, double>, ArgList<int, double, const long&>>,
                                    ArgList<const long&>>,
                "");

  static_assert(
      IsPrefixWeakV<ArgList<decltype(std::ref(std::declval<int&>())), double>, ArgList<int&, double, const long&>>, "");
}

std::size_t sum(const int& v1, double v2, int v3, int v4) { return v1 + v2 + v3 + v4; }

int forwarding_test(std::unique_ptr<int> p) { return *p.get(); }

int calculate_sum(const std::string& exp) {
  int ans = 0;
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

TEST(TestClosure, Basic) {
  Closure<std::size_t(double, int, int)> closure1 = MakeClosure(sum, 1);
  ASSERT_EQ(closure1.Run(2, 3, 4), 10);

  Closure<int(std::unique_ptr<int>)> closure2 = MakeClosure(forwarding_test);
  ASSERT_EQ(closure2.Run(std::make_unique<int>(10)), 10);

  std::string exp = "11+12+13";
  ASSERT_EQ(MakeClosure(calculate_sum, std::move(exp)).Run(), 36);
  ASSERT_TRUE(exp.size() == 0);

  int v = 0;
  auto closure3 = MakeClosure(test_ref, v);
  static_assert(!__CLOSTD::is_const_v<decltype(v)>, "");
  closure3.Run();
  ASSERT_TRUE(v == 0);
  closure3 = MakeClosure(test_ref, std::ref(v));
  closure3.Run();
  ASSERT_TRUE(v == 1);
}

TEST(TestPlaceHolder, Sort) {
  using namespace placeholders;
  using namespace details;
  static_assert(__CLOSTD::is_same_v<std::index_sequence<0>, typename sort::TEST_Sort<PH<0>>::type>, "");
  static_assert(__CLOSTD::is_same_v<std::index_sequence<1, 2, 3>, typename sort::TEST_Sort<PH<2>, PH<3>, PH<1>>::type>,
                "");
  static_assert(
      __CLOSTD::is_same_v<std::index_sequence<3, 4, 5, 6>, typename sort::TEST_Sort<PH<6>, PH<4>, PH<3>, PH<5>>::type>,
      "");
  static_assert(__CLOSTD::is_same_v<std::index_sequence<2, 4, 5, 6, 9>,
                                    typename sort::TEST_Sort<PH<5>, PH<6>, PH<4>, PH<9>, PH<2>>::type>,
                "");
  static_assert(__CLOSTD::is_same_v<std::index_sequence<2, 4, 5, 5, 6, 6, 9>,
                                    typename sort::TEST_Sort<PH<6>, PH<5>, PH<6>, PH<4>, PH<9>, PH<5>, PH<2>>::type>,
                "");

  using within_others = ArgList<int, int, PH<2>, int, PH<3>, PH<0>, int, PH<1>>;
  static_assert(HasPlaceHolderV<within_others>, "");

  static_assert(
      __CLOSTD::is_same_v<typename FilterPlaceHolder<within_others>::type, ArgList<PH<2>, PH<3>, PH<0>, PH<1>>>, "");
}

TEST(TestAgentAndGetter, AgentBasic) {
  using namespace placeholders;
  std::string str = "123";
  Agent<std::string> str_agent(std::move(str));
  ASSERT_EQ(str, "123");
  ASSERT_EQ(str_agent.Get(), "123");
  str = "1";
  ASSERT_EQ(str_agent.Get(), "1");
  std::string{str_agent.Get()};  // str_agent.Get() returns rvalue reference, so the move constructor incurred.
  EXPECT_TRUE(str_agent.Get().empty());
  EXPECT_TRUE(str.empty());
  EXPECT_TRUE(str_agent);
  // Bind to another string.
  std::string ano_str("123");  // do not use temporary object.
  str_agent = std::move(ano_str);
  ASSERT_EQ(str_agent.Get(), "123");
  ASSERT_EQ(str, "");  // str_agent now has nothing to do with str.
  Agent<std::string> ano = std::move(str_agent);
  ASSERT_EQ(ano.Get(), "123");
  ASSERT_EQ(str, "");
  ASSERT_TRUE(ano);
  ASSERT_FALSE(str_agent);
  Agent<std::string> ano2;
  ASSERT_FALSE(ano2);
  ano2 = std::move(ano);
  ASSERT_TRUE(ano2);
  ASSERT_FALSE(ano);
  ASSERT_EQ(ano2.Get(), "123");
  ano_str = "456";
  ASSERT_EQ(ano2.Get(), "456");
}

TEST(TestAgentAndGetter, AgentBasic2) {
  using namespace placeholders;
  std::string str = "123";
  Agent<std::string&> str_agent(str);
  str_agent.Get() = "456";
  EXPECT_EQ(str, "456");
}

TEST(TestAgentAndGetter, AgentTuple) {
  using namespace placeholders;
  std::tuple<Agent<int&>, Agent<double&>, Agent<std::string>> agents;

  auto bind_value = [&](auto arg1, auto arg2, auto arg3) {
#if __cplusplus < 201703L
    auto& v1 = std::get<0>(agents);
    auto& v2 = std::get<1>(agents);
    auto& v3 = std::get<2>(agents);
#else
    auto& [v1, v2, v3] = agents;
#endif
    auto arg3_dup = arg3;
    v1 = arg1;
    v2 = arg2;
    v3 = std::move(arg3);
    ASSERT_EQ(arg3, arg3_dup);
    EXPECT_EQ(v1.Get(), arg1);
    EXPECT_EQ(v2.Get(), arg2);
    EXPECT_EQ(v3.Get(), arg3);
    EXPECT_EQ(std::get<2>(agents).Get(), arg3);
  };
  bind_value(1, 1.5, std::string("2.0"));
  bind_value(3, 3.5, std::string("4.0"));
}

TEST(TestAgentAndGetter, GettersMapping) {
  using namespace placeholders;
  std::string arg1("1234");
  int arg2 = 1;
  std::tuple<Agent<std::string&>, Agent<int&>, int> agents(arg1, arg2, 0);
  using Getter0 = Getter<decltype(agents), 0>;
  static_assert(IsGetterDecayV<Getter0>, "");
  Getter0 getter;
  ASSERT_FALSE(getter);
  EXPECT_EQ(std::get<0>(agents).Get(), "1234");
  getter.Map(agents);
  EXPECT_EQ(std::get<0>(agents).Get(), "1234");
  EXPECT_EQ(getter.Get(), "1234");
#if __cplusplus < 201703L
  auto& v1 = std::get<0>(agents);
  auto& v2 = std::get<1>(agents);
#else
  auto& [v1, v2, _] = agents;
#endif
  using Getter1 = Getter<decltype(agents), 1>;
  // There is no Getter2.
  auto getters = std::make_tuple(0, Getter1{}, Getter0{});
  static_assert(!IsGetterDecayV<decltype(std::get<0>(std::declval<decltype(getters)>()))>, "");
  static_assert(IsGetterDecayV<decltype(std::get<1>(std::declval<decltype(getters)>()))>, "");
  static_assert(IsGetterDecayV<decltype(std::get<2>(std::declval<decltype(getters)>()))>, "");
  int cnt = placeholders::MapGetters(getters, agents);
  EXPECT_EQ(cnt, 2);
  EXPECT_EQ(v2.Get(), std::get<1>(getters).Get());
  arg1 = std::string("modified");
  arg2 = 2;
  EXPECT_EQ(v2.Get(), 2);
  EXPECT_EQ(getter.Get(), v1.Get());
  EXPECT_EQ(placeholders::Get<0>(getters), 0);
  EXPECT_EQ(placeholders::Get<1>(getters), v2.Get());
  EXPECT_EQ(v2.Get(), 2);
  EXPECT_EQ(placeholders::Get<1>(getters), 2);
  EXPECT_EQ(placeholders::Get<2>(getters), v1.Get());
}

TEST(TestAgentAndGetter, GetPlaceHoldersCorrespondTypes) {
  using namespace placeholders;
  using args = ArgList<int, double, std::string, long>;
  using binds = ArgList<int, PH<1>, PH<0>, long>;
  static_assert(details::IsPrefixWeakV<binds, args>, "");
  using ph_args = details::GetPlaceHoldersCorrespondTypesT<binds, args>;
  static_assert(__CLOSTD::is_same_v<ph_args, ArgList<double, std::string>>, "");

  static_assert(
      __CLOSTD::is_same_v<details::GetPlaceHoldersCorrespondTypesT<ArgList<PH<0>>, ArgList<long, int>>, ArgList<long>>,
      "");
}

TEST(TestAgentAndGetter, SortPlaceHoldersCorrespondTypes) {
  using namespace placeholders;
  using args = ArgList<int, double, std::string, long, char, float>;
  using binds = ArgList<int, PH<1>, PH<3>, long, PH<0>, PH<2>>;
  using ph_args = details::GetPlaceHoldersCorrespondTypesT<binds, args>;
  using phl = FilterPlaceHolderT<binds>;
  using result = details::SortUniqueFillPlaceHoldersCorrespondTypesT<ph_args, phl>;
  static_assert(__CLOSTD::is_same_v<result, ArgList<char, double, float, std::string>>, "");

  static_assert(__CLOSTD::is_same_v<details::SortUniqueFillPlaceHoldersCorrespondTypesT<ArgList<int>, ArgList<PH<0>>>,
                                    ArgList<int>>,
                "");
}

TEST(TestAgentAndGetter, StableSort) {
  using namespace placeholders;
  using args = ArgList<int, double, std::string, long, char, float, void*>;
  using binds = ArgList<PH<3>, PH<1>, PH<3>, PH<1>, PH<2>, PH<5>, PH<2>>;
  using ph_args = details::GetPlaceHoldersCorrespondTypesT<binds, args>;
  using phl = FilterPlaceHolderT<binds>;
  using sorted = details::sort::RemoveIndicesV<details::sort::SortT<ph_args, phl>>;
  static_assert(__CLOSTD::is_same_v<sorted, ArgList<double, long, char, void*, int, std::string, float>>, "");

  using result = details::SortUniqueFillPlaceHoldersCorrespondTypesT<ph_args, phl>;
  static_assert(
      __CLOSTD::is_same_v<result, ArgList<closure::Auto /*fill from zero*/, double, char, int, closure::Auto, float>>,
      "");
}

TEST(TestAgentAndGetter, ReplacePlaceHoldersCorrespondTypes) {
  using namespace placeholders;
  using args = ArgList<int, double, std::string, long, char, float, void*, unsigned>;
  using binds = ArgList<PH<3>, PH<2>, std::string, PH<2>, char, float, PH<5>>;
  using result = details::ReplacePlaceHoldersWithGettersT<binds, args>;

  using bind_type = std::tuple<Agent<Auto>, Agent<Auto>, Agent<double>, Agent<int>, Agent<Auto>, Agent<void*>>;

  static_assert(__CLOSTD::is_same_v<result, ArgList<Getter<bind_type, 3>, Getter<bind_type, 2>, std::string,
                                                    Getter<bind_type, 2>, char, float, Getter<bind_type, 5>>>,
                "");
  static_assert(__CLOSTD::is_same_v<bind_type, details::PlaceHoldersAgentsT<binds, args>>, "");
}
