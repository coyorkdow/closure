#include <cassert>
#include <iostream>
#include <memory>

#include "closure.hpp"
#include "gtest/gtest.h"

using namespace closure;

TEST(TestArg, Main) {
  using namespace __closure;

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

TEST(TestPlaceHolder, Sort) {
  using namespace placeholders;
  using namespace __closure;
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
  ASSERT_EQ(str_agent.Target(), "123");
  str = "1";
  ASSERT_EQ(str_agent.Target(), "1");
  std::string{str_agent.Target()};  // str_agent.Target() returns rvalue reference, so the move constructor incurred.
  EXPECT_TRUE(str_agent.Target().empty());
  EXPECT_TRUE(str.empty());
  EXPECT_TRUE(str_agent);
  // Bind to another string.
  std::string ano_str("123");  // do not use temporary object.
  str_agent = std::move(ano_str);
  ASSERT_EQ(str_agent.Target(), "123");
  ASSERT_EQ(str, "");  // str_agent now has nothing to do with str.
  Agent<std::string> ano = str_agent;
  ASSERT_EQ(ano.Target(), "123");
  ASSERT_EQ(str, "");
  ASSERT_TRUE(ano);
  Agent<std::string> ano2;
  ASSERT_FALSE(ano2);
  ano2 = ano;
  ASSERT_TRUE(ano2);
  ASSERT_EQ(ano2.Target(), "123");
  ano_str = "456";
  ASSERT_EQ(ano2.Target(), "456");
}

TEST(TestAgentAndGetter, AgentBasic2) {
  using namespace placeholders;
  std::string str = "123";
  Agent<std::string&> str_agent(str);
  str_agent.Target() = "456";
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
    EXPECT_EQ(v1.Target(), arg1);
    EXPECT_EQ(v2.Target(), arg2);
    EXPECT_EQ(v3.Target(), arg3);
    EXPECT_EQ(std::get<2>(agents).Target(), arg3);
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
  EXPECT_EQ(std::get<0>(agents).Target(), "1234");
  getter.Map(agents);
  EXPECT_EQ(std::get<0>(agents).Target(), "1234");
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
  int cnt = placeholders::MapGettersToAgents(getters, agents);
  EXPECT_EQ(cnt, 2);
  EXPECT_EQ(v2.Target(), std::get<1>(getters).Get());
  arg1 = std::string("modified");
  arg2 = 2;
  EXPECT_EQ(v2.Target(), 2);
  EXPECT_EQ(getter.Get(), v1.Target());
  EXPECT_EQ(placeholders::Get<0>(getters), 0);
  EXPECT_EQ(placeholders::Get<1>(getters), v2.Target());
  EXPECT_EQ(v2.Target(), 2);
  EXPECT_EQ(placeholders::Get<1>(getters), 2);
  EXPECT_EQ(placeholders::Get<2>(getters), v1.Target());

  static_assert(__CLOSTD::is_copy_constructible_v<decltype(getters)>, "");
}

TEST(TestAgentAndGetter, GetPlaceHoldersCorrespondTypes) {
  using namespace placeholders;
  using namespace __closure;
  using args = ArgList<int, double, std::string, long>;
  using binds = ArgList<int, PH<1>, PH<0>, long>;
  static_assert(IsPrefixWeakV<binds, args>, "");
  using ph_args = GetPlaceHoldersCorrespondTypesT<binds, args>;
  static_assert(__CLOSTD::is_same_v<ph_args, ArgList<double, std::string>>, "");

  static_assert(__CLOSTD::is_same_v<GetPlaceHoldersCorrespondTypesT<ArgList<PH<0>>, ArgList<long, int>>, ArgList<long>>,
                "");
}

TEST(TestAgentAndGetter, SortPlaceHoldersCorrespondTypes) {
  using namespace placeholders;
  using namespace __closure;
  using args = ArgList<int, double, std::string, long, char, float>;
  using binds = ArgList<int, PH<1>, PH<3>, long, PH<0>, PH<2>>;
  using ph_args = GetPlaceHoldersCorrespondTypesT<binds, args>;
  using phl = FilterPlaceHolderT<binds>;
  using result = SortUniqueFillPlaceHoldersCorrespondTypesT<ph_args, phl>;
  static_assert(__CLOSTD::is_same_v<result, ArgList<char, double, float, std::string>>, "");

  static_assert(
      __CLOSTD::is_same_v<SortUniqueFillPlaceHoldersCorrespondTypesT<ArgList<int>, ArgList<PH<0>>>, ArgList<int>>, "");
}

TEST(TestAgentAndGetter, StableSort) {
  using namespace placeholders;
  using namespace __closure;
  using args = ArgList<int, double, std::string, long, char, float, void*>;
  using binds = ArgList<PH<3>, PH<1>, PH<3>, PH<1>, PH<2>, PH<5>, PH<2>>;
  using ph_args = GetPlaceHoldersCorrespondTypesT<binds, args>;
  using phl = FilterPlaceHolderT<binds>;
  using sorted = sort::RemoveIndicesV<sort::SortT<ph_args, phl>>;
  static_assert(__CLOSTD::is_same_v<sorted, ArgList<double, long, char, void*, int, std::string, float>>, "");

  using result = SortUniqueFillPlaceHoldersCorrespondTypesT<ph_args, phl>;
  static_assert(
      __CLOSTD::is_same_v<result, ArgList<closure::Any /*fill from zero*/, double, char, int, closure::Any, float>>,
      "");
}

TEST(TestAgentAndGetter, ReplacePlaceHoldersWithGetters1) {
  using namespace placeholders;
  using namespace __closure;
  using args = ArgList<int, double, std::string, long, char, float, void*, unsigned>;
  using binds = ArgList<PH<3>, PH<2>, std::string, PH<0>, char, PH<5>>;
  using result = ReplacePlaceHoldersWithGettersT<binds, args>;

  using agents_type = std::tuple<Agent<long>, Agent<Any>, Agent<double>, Agent<int>, Agent<Any>, Agent<float>>;

  static_assert(__CLOSTD::is_same_v<result, ArgList<Getter<agents_type, 3>, Getter<agents_type, 2>, std::string,
                                                    Getter<agents_type, 0>, char, Getter<agents_type, 5>>>,
                "");
  static_assert(__CLOSTD::is_same_v<agents_type, MakeAgentsT<PlaceHoldersAgentsPrototypeT<binds, args>>>, "");
}

TEST(TestAgentAndGetter, ReplacePlaceHoldersWithGetters2) {
  using namespace placeholders;
  using namespace __closure;
  using args = ArgList<int, double, std::string, long, char, float, void*, unsigned>;
  using binds = ArgList<PH<3>, PH<2>, std::string, PH<2>, char, float, PH<5>>;
  using result = ReplacePlaceHoldersWithGettersT<binds, args>;

  using agents_type = std::tuple<Agent<Any>, Agent<Any>, Agent<double>, Agent<int>, Agent<Any>, Agent<void*>>;

  static_assert(__CLOSTD::is_same_v<result, ArgList<Getter<agents_type, 3>, Getter<agents_type, 2>, std::string,
                                                    Getter<agents_type, 2>, char, float, Getter<agents_type, 5>>>,
                "");
  static_assert(__CLOSTD::is_same_v<agents_type, MakeAgentsT<PlaceHoldersAgentsPrototypeT<binds, args>>>, "");
}

TEST(TestAnyType, Any) {
  auto test = [](closure::Any) {};
  test(1);
  test(0.0);
  test(1ULL);
  test("123");
}

TEST(TestClosure, EmptyBaseOptimize) {
  using c1 = __closure::ClosureImpl<void(), void (*)(), ArgList<>>;
  // TODO eliminate vptr
  static_assert(sizeof(c1) == 16, "");
  //  static_assert(sizeof(c1) == 8, "");
}

std::size_t sum(const int& v1, double v2, int v3, int v4) noexcept { return v1 + v2 + v3 + v4; }

int forwarding_test(std::unique_ptr<int> p) { return *p; }

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

TEST(TestClosure, FunctionPointer) {
  auto closure1 = MakeClosure(sum, 1);
  static_assert(__CLOSTD::is_same_v<decltype(closure1), Closure<std::size_t(double, int, int)>>, "");
  ASSERT_EQ(closure1.Run(2, 3, 4), 10);
  double lvalue = 2;
  EXPECT_EQ(closure1(lvalue, 3, 4), 10);  // can accept lvalue
  typename std::add_const<decltype(sum)*>::type fptr = sum;
  closure1 = MakeClosure(fptr, 1);  // test move assignment
  ASSERT_EQ(closure1.Run(4, 5, 6), 16);

  Closure<int(std::unique_ptr<int>)> closure2(forwarding_test);
  ASSERT_EQ(closure2.Run(std::make_unique<int>(10)), 10);

  std::string exp = "11+12+13";
  ASSERT_EQ(MakeClosure(calculate_sum, std::move(exp)).Run(), 36);
  ASSERT_TRUE(exp.empty());

  int v = 0;
  Closure<void()> closure3;
  EXPECT_FALSE(closure3);
  closure3 = MakeClosure(test_ref, v);  // test move assignment
  static_assert(__CLOSTD::is_same_v<decltype(closure3), Closure<void()>>, "");
  static_assert(!__CLOSTD::is_const_v<decltype(v)>, "");
  closure3.Run();
  ASSERT_TRUE(v == 0);
  closure3 = MakeClosure(test_ref, std::ref(v));
  closure3.Run();
  ASSERT_TRUE(v == 1);

  Closure<int(std::string)> closure4;
  closure4 = calculate_sum;
  EXPECT_EQ(closure4("1+2+3"), 6);
}

TEST(TestClosureWithPlaceHolders, FunctionPointer) {
  auto closure1 = MakeClosure(sum, closure::PlaceHolder<0>());
  static_assert(__CLOSTD::is_same_v<decltype(closure1), Closure<std::size_t(const int&, double, int, int)>>, "");
  EXPECT_EQ(closure1(1, 2, 3, 4), 10);
  auto closure2 = MakeClosure(sum, closure::PlaceHolder<2>(), closure::PlaceHolder<1>(), closure::PlaceHolder<3>());
  static_assert(
      __CLOSTD::is_same_v<decltype(closure2), Closure<std::size_t(closure::Any, double, const int&, int, int)>>, "");
  EXPECT_EQ(closure2("ignored", 1, 2, 3, 4), 10);
}

TEST(TestClosure, Functor) {
  std::string exp = "11+12+13";
  auto wrap_sum = [=]() { return calculate_sum(exp); };
  auto closure1 = MakeClosure(wrap_sum);
  EXPECT_EQ(closure1.Run(), 36);
  std::function<float()> wrap_twice(wrap_sum);
  EXPECT_TRUE(wrap_twice);
  closure1 = std::move(wrap_twice);
  EXPECT_FALSE(wrap_twice);  // should be empty after moved
  EXPECT_EQ(closure1(), 36);
}

TEST(TestClosure, Copy) {
  std::string exp = "11+12+13";
  auto lambda1 = [=]() { return calculate_sum(exp); };
  auto closure1 = MakeClosure(lambda1);
  EXPECT_EQ(closure1(), 36);
  int v = 0;
  auto lambda2 = [&]() {
    test_ref(v);
    return v;
  };
  auto closure2 = MakeClosure(lambda2);
  EXPECT_EQ(closure2(), 1);
  static_assert(__CLOSTD::is_same_v<decltype(closure1), decltype(closure2)>, "");
  EXPECT_TRUE(closure2.Copyable());
  closure1 = closure2;
  EXPECT_TRUE(closure1);  // copy succeeded
  EXPECT_EQ(closure1(), 2);

  std::unique_ptr<int> ptr;
  auto lambda3 = [ptr = std::move(ptr)]() mutable -> int {
    auto v = static_cast<bool>(ptr);
    ptr = std::make_unique<int>(0);
    return v;
  };  // uncopyable
  //  std::function<int()> _ = std::move(lambda3); /*can't compile*/
  closure2 = std::move(lambda3);
  EXPECT_TRUE(closure2);
  EXPECT_FALSE(closure2.Copyable());
  EXPECT_EQ(closure2(), 0);
  closure1 = closure2;
  EXPECT_FALSE(closure1);            // copy failed
  EXPECT_TRUE(closure1.Copyable());  // an empty closure is copyable
  closure1 = std::move(closure2);
  EXPECT_TRUE(closure1);
  EXPECT_EQ(closure1(), 1);
}

class TestClassBindMethod {
 public:
  int ResIntArg0() const { return 0; }
  int ResIntArg1(int v) const { return v; }
  int ResIntArgs3(int v1, int v2, int v3) const { return v1 + v2 + v3; }
  std::size_t ResSizeTArg1NonConst(int add) {
    change_.emplace_back(add);
    return change_.size();
  }
  static void StaticFunc() {}

  std::vector<int> change_;
};

TEST(TestClosure, Method) {
  TestClassBindMethod cl;
  static_assert(__CLOSTD::is_member_function_pointer_v<decltype(&TestClassBindMethod::ResIntArg0)>, "");
  static_assert(traits::IsDereferencable<std::unique_ptr<TestClassBindMethod>>::value, "");
  auto ptr = std::make_unique<TestClassBindMethod>();

  auto closure1 = MakeClosure(&TestClassBindMethod::ResIntArg0, &cl);
  EXPECT_EQ(closure1(), 0);

  closure1 = MakeClosure(&TestClassBindMethod::ResIntArg1, std::make_unique<TestClassBindMethod>(), 233);
  EXPECT_EQ(closure1(), 233);
}
