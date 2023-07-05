#include <cassert>
#include <memory>
#include <numeric>
#include <sstream>

#include "closure/closure.hpp"
#include "gtest/gtest.h"

using namespace closure;

TEST(TestArg, Main) {
  using namespace closureimpl;

  static_assert(IsPrefixWeak<ArgList<>, ArgList<>>::value, "");
  static_assert(std::is_same<RemovePrefixWeakT<ArgList<>, ArgList<>>, ArgList<>>::value, "");

  static_assert(IsPrefixWeak<ArgList<>, ArgList<int>>::value, "");
  static_assert(std::is_same<RemovePrefixWeakT<ArgList<>, ArgList<int>>, ArgList<int>>::value, "");

  static_assert(IsPrefixWeak<ArgList<int>, ArgList<int>>::value, "");
  static_assert(std::is_same<RemovePrefixWeakT<ArgList<int>, ArgList<int>>, ArgList<>>::value, "");

  static_assert(IsPrefixWeak<ArgList<int>, ArgList<int, double>>::value, "");
  static_assert(std::is_same<RemovePrefixWeakT<ArgList<int>, ArgList<int, double>>, ArgList<double>>::value, "");

  static_assert(IsPrefixWeak<ArgList<int, double>, ArgList<int, double, long>>::value, "");
  static_assert(std::is_same<RemovePrefixWeakT<ArgList<int, double>, ArgList<int, double, long>>, ArgList<long>>::value,
                "");

  static_assert(IsPrefixWeak<ArgList<int>, ArgList<int, double, long>>::value, "");
  static_assert(std::is_same<RemovePrefixWeakT<ArgList<int>, ArgList<int, double, long>>, ArgList<double, long>>::value,
                "");

  static_assert(IsPrefixWeak<ArgList<int, double, long>, ArgList<int, double, long>>::value, "");
  static_assert(
      std::is_same<RemovePrefixWeakT<ArgList<int, double, long>, ArgList<int, double, long>>, ArgList<>>::value, "");

  static_assert(!IsPrefixWeak<ArgList<int, double, long>, ArgList<double, long>>::value, "");
  static_assert(!IsPrefixWeak<ArgList<int, double, long>, ArgList<int, double>>::value, "");

  static_assert(IsPrefixWeak<ArgList<const int&>, ArgList<int>>::value, "");
  static_assert(std::is_same<RemovePrefixWeakT<ArgList<const int&>, ArgList<int>>, ArgList<>>::value, "");
  static_assert(IsPrefixWeak<ArgList<int&&>, ArgList<int>>::value, "");
  static_assert(std::is_same<RemovePrefixWeakT<ArgList<int&&>, ArgList<int>>, ArgList<>>::value, "");

  static_assert(IsPrefixWeak<ArgList<int, double>, ArgList<int&&, double, const long&>>::value, "");
  static_assert(std::is_same<RemovePrefixWeakT<ArgList<int, double>, ArgList<int, double, const long&>>,
                             ArgList<const long&>>::value,
                "");

  static_assert(IsPrefixWeak<ArgList<decltype(std::ref(std::declval<int&>())), double>,
                             ArgList<int&, double, const long&>>::value,
                "");
}

TEST(TestPlaceHolder, Sort) {
  using namespace placeholders;
  using namespace closureimpl;
  static_assert(std::is_same<std::index_sequence<0>, typename sort::TEST_Sort<PH<0>>::type>::value, "");
  static_assert(std::is_same<std::index_sequence<1, 2, 3>, typename sort::TEST_Sort<PH<2>, PH<3>, PH<1>>::type>::value,
                "");
  static_assert(
      std::is_same<std::index_sequence<3, 4, 5, 6>, typename sort::TEST_Sort<PH<6>, PH<4>, PH<3>, PH<5>>::type>::value,
      "");
  static_assert(std::is_same<std::index_sequence<2, 4, 5, 6, 9>,
                             typename sort::TEST_Sort<PH<5>, PH<6>, PH<4>, PH<9>, PH<2>>::type>::value,
                "");
  static_assert(std::is_same<std::index_sequence<2, 4, 5, 5, 6, 6, 9>,
                             typename sort::TEST_Sort<PH<6>, PH<5>, PH<6>, PH<4>, PH<9>, PH<5>, PH<2>>::type>::value,
                "");

  using within_others = ArgList<int, int, PH<2>, int, PH<3>, PH<0>, int, PH<1>>;
  static_assert(HasPlaceHolder<within_others>::value, "");

  static_assert(
      std::is_same<typename FilterPlaceHolder<within_others>::type, ArgList<PH<2>, PH<3>, PH<0>, PH<1>>>::value, "");
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
  static_assert(IsGetter<Getter0>::value, "");
  Getter0 getter;
  EXPECT_EQ(std::get<0>(agents).Target(), "1234");
  EXPECT_EQ(std::get<0>(agents).Target(), "1234");
  EXPECT_EQ(getter.Get(agents), "1234");
#if __cplusplus < 201703L
  auto& v1 = std::get<0>(agents);
  auto& v2 = std::get<1>(agents);
#else
  auto& [v1, v2, _] = agents;
#endif
  using Getter1 = Getter<decltype(agents), 1>;
  // There is no Getter2.
  auto getters = std::make_tuple(0, Getter1{}, Getter0{});
  static_assert(!IsGetter<std::decay_t<decltype(std::get<0>(std::declval<decltype(getters)>()))>>::value, "");
  static_assert(IsGetter<std::decay_t<decltype(std::get<1>(std::declval<decltype(getters)>()))>>::value, "");
  static_assert(IsGetter<std::decay_t<decltype(std::get<2>(std::declval<decltype(getters)>()))>>::value, "");
  EXPECT_EQ(v2.Target(), std::get<1>(getters).Get(agents));
  arg1 = std::string("modified");
  arg2 = 2;
  EXPECT_EQ(v2.Target(), 2);
  EXPECT_EQ(getter.Get(agents), v1.Target());
  EXPECT_EQ(placeholders::TryMapAndGet<0>(getters, agents), 0);
  EXPECT_EQ(placeholders::TryMapAndGet<1>(getters, agents), v2.Target());
  EXPECT_EQ(v2.Target(), 2);
  EXPECT_EQ(placeholders::TryMapAndGet<1>(getters, agents), 2);
  EXPECT_EQ(placeholders::TryMapAndGet<2>(getters, agents), v1.Target());

  static_assert(std::is_copy_constructible<decltype(getters)>::value, "");
}

TEST(TestAgentAndGetter, GetPlaceHoldersCorrespondTypes) {
  using namespace placeholders;
  using namespace closureimpl;
  using args = ArgList<int, double, std::string, long>;
  using binds = ArgList<int, PH<1>, PH<0>, long>;
  static_assert(IsPrefixWeak<binds, args>::value, "");
  using ph_args = GetPlaceHoldersCorrespondTypesT<binds, args>;
  static_assert(std::is_same<ph_args, ArgList<double, std::string>>::value, "");

  static_assert(std::is_same<GetPlaceHoldersCorrespondTypesT<ArgList<PH<0>>, ArgList<long, int>>, ArgList<long>>::value,
                "");
}

TEST(TestAgentAndGetter, SortPlaceHoldersCorrespondTypes) {
  using namespace placeholders;
  using namespace closureimpl;
  using args = ArgList<int, double, std::string, long, char, float>;
  using binds = ArgList<int, PH<1>, PH<3>, long, PH<0>, PH<2>>;
  using ph_args = GetPlaceHoldersCorrespondTypesT<binds, args>;
  using phl = FilterPlaceHolderT<binds>;
  using result = SortUniqueFillPlaceHoldersCorrespondTypesT<ph_args, phl>;
  static_assert(std::is_same<result, ArgList<char, double, float, std::string>>::value, "");

  static_assert(
      std::is_same<SortUniqueFillPlaceHoldersCorrespondTypesT<ArgList<int>, ArgList<PH<0>>>, ArgList<int>>::value, "");
}

TEST(TestAgentAndGetter, StableSort) {
  using namespace placeholders;
  using namespace closureimpl;
  using args = ArgList<int, double, std::string, long, char, float, void*>;
  using binds = ArgList<PH<3>, PH<1>, PH<3>, PH<1>, PH<2>, PH<5>, PH<2>>;
  using ph_args = GetPlaceHoldersCorrespondTypesT<binds, args>;
  using phl = FilterPlaceHolderT<binds>;
  using sorted = sort::RemoveIndicesT<sort::SortT<ph_args, phl>>;
  static_assert(std::is_same<sorted, ArgList<double, long, char, void*, int, std::string, float>>::value, "");

  using result = SortUniqueFillPlaceHoldersCorrespondTypesT<ph_args, phl>;
  static_assert(
      std::is_same<result, ArgList<closure::Any /*fill from zero*/, double, char, int, closure::Any, float>>::value,
      "");
}

TEST(TestAgentAndGetter, ReplacePlaceHoldersWithGetters1) {
  using namespace placeholders;
  using namespace closureimpl;
  using args = ArgList<int, double, std::string, long, char, float, void*, unsigned>;
  using binds = ArgList<PH<3>, PH<2>, std::string, PH<0>, char, PH<5>>;
  using result = ReplacePlaceHoldersWithGettersT<binds, args>;

  using agents_type = std::tuple<Agent<long>, Agent<Any>, Agent<double>, Agent<int>, Agent<Any>, Agent<float>>;

  static_assert(std::is_same<result, ArgList<Getter<agents_type, 3>, Getter<agents_type, 2>, std::string,
                                             Getter<agents_type, 0>, char, Getter<agents_type, 5>>>::value,
                "");
  static_assert(
      std::is_same<agents_type,
                   MakeAgentsT<typename ReplacePlaceHoldersWithGetters<binds, args>::agents_prototype>>::value,
      "");
}

TEST(TestAgentAndGetter, ReplacePlaceHoldersWithGetters2) {
  using namespace placeholders;
  using namespace closureimpl;
  using args = ArgList<int, double, std::string, long, char, float, void*, unsigned>;
  using binds = ArgList<PH<3>, PH<2>, std::string, PH<2>, char, float, PH<5>>;
  using result = ReplacePlaceHoldersWithGettersT<binds, args>;

  using agents_type = std::tuple<Agent<Any>, Agent<Any>, Agent<double>, Agent<int>, Agent<Any>, Agent<void*>>;

  static_assert(std::is_same<result, ArgList<Getter<agents_type, 3>, Getter<agents_type, 2>, std::string,
                                             Getter<agents_type, 2>, char, float, Getter<agents_type, 5>>>::value,
                "");
  static_assert(
      std::is_same<agents_type,
                   MakeAgentsT<typename ReplacePlaceHoldersWithGetters<binds, args>::agents_prototype>>::value,
      "");
}

TEST(TestRangePlaceHolder, TypeReplace) {
  using namespace placeholders;
  using namespace closureimpl;

  using replaced = ReplaceRangePlaceHolderWithPlaceHoldersT<ArgList<RangePH<0, 4>>>;
  static_assert(std::is_same<ArgList<placeholders::PH<0>, placeholders::PH<1>, placeholders::PH<2>, placeholders::PH<3>,
                                     placeholders::PH<4>>,
                             replaced>::value,
                "");
}

TEST(TestRangePlaceHolder, Partition) {
  using namespace placeholders;
  using namespace closureimpl;
  {
    using args = ArgList<int, RangePH<1, 3>, float, double>;
    using partition = BoundPartition<args>;
    static_assert(std::is_same<typename partition::former, ArgList<int>>::value, "");
    static_assert(std::is_same<typename partition::range_placeholders, ArgList<PH<1>, PH<2>, PH<3>>>::value, "");
    static_assert(std::is_same<typename partition::latter, ArgList<float, double>>::value, "");
  }
  {
    using args = ArgList<int, double, RangePH<1, 3>>;
    using partition = BoundPartition<args>;
    static_assert(std::is_same<typename partition::former, ArgList<int, double>>::value, "");
    static_assert(std::is_same<typename partition::range_placeholders, ArgList<PH<1>, PH<2>, PH<3>>>::value, "");
    static_assert(std::is_same<typename partition::latter, ArgList<>>::value, "");
  }
  {
    using args = ArgList<RangePH<1, 3>, int, double>;
    using partition = BoundPartition<args>;
    static_assert(std::is_same<typename partition::former, ArgList<>>::value, "");
    static_assert(std::is_same<typename partition::range_placeholders, ArgList<PH<1>, PH<2>, PH<3>>>::value, "");
    static_assert(std::is_same<typename partition::latter, ArgList<int, double>>::value, "");
  }
  {
    using args = ArgList<RangePH<1, 3>>;
    using partition = BoundPartition<args>;
    static_assert(std::is_same<typename partition::former, ArgList<>>::value, "");
    static_assert(std::is_same<typename partition::range_placeholders, ArgList<PH<1>, PH<2>, PH<3>>>::value, "");
    static_assert(std::is_same<typename partition::latter, ArgList<>>::value, "");
  }
  {
    using args = ArgList<RangePH<1, 2>&&>;
    using partition = BoundPartition<args>;
    static_assert(std::is_same<typename partition::former, ArgList<>>::value, "");
    static_assert(std::is_same<typename partition::range_placeholders, ArgList<PH<1>, PH<2>>>::value, "");
    static_assert(std::is_same<typename partition::latter, ArgList<>>::value, "");
  }
}

TEST(TestRangePlaceHolder, FlatArguments) {
  using namespace placeholders;
  using namespace closureimpl;
  using args_tuple0 = std::tuple<int, RangePH<1, 3>, double>;
  args_tuple0 args{1, PlaceHolder<1, 3>(), 1.1};
  static_assert(std::is_same<typename placeholders::ReplaceRangePlaceHolderWithPlaceHoldersT<
                                 ArgList<int, RangePH<1, 3>, double>>::tuple_type,
                             std::tuple<int, PH<1>, PH<2>, PH<3>, double>>::value,
                "");
  auto res = FlatBoundArguments(std::move(args));
  static_assert(std::tuple_size<decltype(res)>::value == 5, "");
  static_assert(std::is_same<decltype(res), std::tuple<int&&, PH<1>&&, PH<2>&&, PH<3>&&, double&&>>::value, "");
  EXPECT_EQ(std::get<0>(res), 1);
  EXPECT_DOUBLE_EQ(std::get<4>(res), 1.1);

  {
    using args_tuple = std::tuple<int>;
    using res_type = decltype(FlatBoundArguments(std::declval<args_tuple>()));
    static_assert(std::is_same<res_type, std::tuple<int&&>>::value, "");
  }
  {
    using res_type = decltype(FlatBoundArguments(std::forward_as_tuple(RangePH<1, 2>{})));
    using res_type2 = typename ReplaceRangePlaceHolderWithPlaceHoldersT<ArgList<RangePH<1, 2>&&>>::forward_tuple;
    static_assert(std::is_same<res_type, res_type2>::value, "");
    static_assert(
        std::is_same<ReplaceRangePlaceHolderWithPlaceHoldersT<ArgList<RangePH<1, 2>&&>>, ArgList<PH<1>, PH<2>>>::value,
        "");
    static_assert(std::is_same<res_type, std::tuple<PH<1>&&, PH<2>&&>>::value, "");
  }
}

TEST(TestStoragePool, SmallObject) {
  closureimpl::StoragePool pool;
  pool.emplace<int>(5);
  EXPECT_EQ(*pool.get<int>(), 5);
  pool.erase<int>();
  pool.emplace<double>(3.1415926);
  EXPECT_DOUBLE_EQ(*pool.get<double>(), 3.1415926);
  pool.erase<double>();
  auto lambda = [](int a, int b) { return a + b; };
  pool.emplace<decltype(lambda)>(lambda);
  EXPECT_EQ((*pool.get<decltype(lambda)>())(1, 2), 3);
  pool.erase<decltype(lambda)>();
}

struct NonSmallObject {
  std::string str;
  NonSmallObject() = default;
  template <class... Args>
  explicit NonSmallObject(Args&&... args) : str(std::forward<Args>(args)...) {}
};
static_assert(!closureimpl::soo::IsSmallObject<NonSmallObject>::value, "");

TEST(TestStoragePool, NonTrivial) {
  closureimpl::StoragePool pool;
  pool.emplace<NonSmallObject>();
  NonSmallObject* ref = pool.get<NonSmallObject>();
  ref->str = "12345";
  EXPECT_EQ(pool.get<NonSmallObject>()->str, "12345");
  pool.erase<NonSmallObject>();
  pool.emplace<NonSmallObject>(5, 'a');
  EXPECT_EQ(pool.get<NonSmallObject>()->str, "aaaaa");
  pool.erase<NonSmallObject>();
  NonSmallObject tmp(1000, '0');
  pool.emplace<NonSmallObject>(std::move(tmp));
  EXPECT_EQ(pool.get<NonSmallObject>()->str, std::string(1000, '0'));
  pool.erase<NonSmallObject>();
}

TEST(TestAnyType, Any) {
  auto test = [](closure::Any) {};
  test(1);
  test(0.0);
  test(1ULL);
  test("123");
}

std::size_t sum(const int& v1, double v2, int v3, int v4) noexcept { return v1 + v2 + v3 + v4; }

int forwarding_test(std::unique_ptr<int> p) noexcept { return *p; }

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

void test_ref(int& v) { v++; }  // 0x7ff7b422ccf0

TEST(TestValidator, InvokeFunctionPointer) {
  using validator =
      closureimpl::Validator<int (*)(std::unique_ptr<int, std::default_delete<int>>),
                             closure::ArgList<std::unique_ptr<int, std::default_delete<int>>>, closure::ArgList<>>;
  static_assert(!validator::is_invokable, "");
  static_assert(std::is_same<validator::invoke_result, typename validator::ErrType>::value, "");
}

TEST(TestValidator, InvokeMemberFunction) {
  struct C {
    int foo(int, int) { return 0; }
  };
  using m1 = int (C::*)(int, int);
  using _ = traits::invoke_result_t<m1, C*, int, int>;
  static_assert(std::is_same<_, int>::value, "");
  using v1 = closureimpl::Validator<m1, ArgList<std::unique_ptr<C>>, ArgList<int, int>>;
  static_assert(v1::is_invokable, "");
  static_assert(std::is_same<typename v1::invoke_result, int>::value, "");
}

TEST(TestClosure, EmptyBaseOptimize) {
  using c1 = closureimpl::ClosureImpl<void(), void (*)(), ArgList<>>;
  static_assert(sizeof(c1) == 8, "");
  using agents = std::tuple<placeholders::Agent<int>, placeholders::Agent<int>>;
  using c2 = closureimpl::ClosureImpl<void(), void (*)(int, int),
                                      ArgList<placeholders::Getter<agents, 1>, placeholders::Getter<agents, 0>>>;
  static_assert(sizeof(c2) == 8, "");
  using c3 = Closure<int(int, int)>;
  static_assert(sizeof(c3) == 32, "");
}

TEST(TestClosure, TrivialTuple) {
  static_assert(std::is_trivially_copyable<placeholders::Agent<int>>::value, "");
  using c1 = closureimpl::ClosureImpl<void(), void (*)(), ArgList<>>;
  static_assert(std::is_trivially_copyable<c1>::value, "");

  using c2 = std::remove_pointer_t<decltype(MakeClosure_ClosureImplType(test_ref, 1))>;
  static_assert(closureimpl::soo::IsSmallObject<c2>::value, "");

  using c3 = std::remove_pointer_t<decltype(MakeClosure_ClosureImplType(sum, PlaceHolder<0>(), PlaceHolder<1>(), 1))>;
  static_assert(closureimpl::soo::IsSmallObject<c3>::value, "");
  using c3tuple = typename c3::stored_types;
  static_assert(sizeof(c3tuple) == 8, "");

  auto bad_bind = std::bind([](int a, int b) { return a + b; }, 1, std::placeholders::_1);
  static_assert(!std::is_trivially_copyable<decltype(bad_bind)>::value, "");
  static_assert(!closureimpl::soo::IsSmallObject<decltype(bad_bind)>::value, "");
  auto _ = [](int a, int b) { return a + b; };
  using c4 = std::remove_pointer_t<decltype(MakeClosure_ClosureImplType(_, 1))>;
  static_assert(closureimpl::soo::IsSmallObject<c4>::value, "");
  EXPECT_EQ(MakeClosure([](int a, int b) { return a + b; }, 1)(2), 3);
}

TEST(TestClosure, FunctionPointer) {
  auto closure1 = MakeClosure(sum, 1);
  static_assert(std::is_same<decltype(closure1), Closure<std::size_t(double, int, int)>>::value, "");
  ASSERT_EQ(closure1(2, 3, 4), 10);
  double lvalue = 2;
  EXPECT_EQ(closure1(lvalue, 3, 4), 10);  // can accept lvalue
  typename std::add_const<decltype(sum)*>::type fptr = sum;
  closure1 = MakeClosure(fptr, 1);  // test move assignment
  ASSERT_EQ(closure1(4, 5, 6), 16);

  Closure<int(std::unique_ptr<int>)> closure2(forwarding_test);
  ASSERT_EQ(closure2(std::make_unique<int>(10)), 10);

  std::string exp = "11+12+13";
  ASSERT_EQ(MakeClosure(calculate_sum, std::move(exp))(), 36);
  ASSERT_TRUE(exp.empty());

  int v = 0;
  Closure<void()> closure3;
  EXPECT_FALSE(closure3);
  closure3 = MakeClosure(test_ref, v);  // test move assignment
  static_assert(std::is_same<decltype(closure3), Closure<void()>>::value, "");
  static_assert(!std::is_const<decltype(v)>::value, "");
  closure3();
  ASSERT_TRUE(v == 0);
  closure3 = MakeClosure(test_ref, std::ref(v));
  closure3();
  ASSERT_TRUE(v == 1);

  Closure<int(std::string)> closure4;
  closure4 = calculate_sum;
  EXPECT_EQ(closure4("1+2+3"), 6);
}

struct NonSimpleFunctor {
  int operator()() const { return 0; }
  int operator()(int v) const { return v; }
  template <class A, class B>
  decltype(auto) operator()(A&& a, B&& b) const {
    return a * b;
  }
};

TEST(TestClosure, Functor) {
  std::string exp = "11+12+13";
  auto wrap_sum = [=]() { return calculate_sum(exp); };
  auto closure1 = MakeClosure(wrap_sum);
  EXPECT_EQ(closure1(), 36);
  std::function<float()> wrap_twice(wrap_sum);
  EXPECT_TRUE(wrap_twice);
  closure1 = std::move(wrap_twice);
  EXPECT_FALSE(wrap_twice);  // should be empty after moved
  EXPECT_EQ(closure1(), 36);

  struct SimpleFunctor {
    int operator()(int a, int b) const { return a + b; }
  };
  auto closure2 = MakeClosure(SimpleFunctor{});
  EXPECT_EQ(closure2(1, 2), 3);
  //  closure2 = MakeClosure(NonSimpleFunctor{});
  closure2 = NonSimpleFunctor{};
  EXPECT_EQ(closure2(2, 3), 6);
  closure1 = NonSimpleFunctor{};
  EXPECT_EQ(closure1(), 0);
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
  static_assert(std::is_same<decltype(closure1), decltype(closure2)>::value, "");
  EXPECT_TRUE(closure2.copyable());
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
  EXPECT_FALSE(closure2.copyable());
  EXPECT_EQ(closure2(), 0);
  closure1 = closure2;
  EXPECT_FALSE(closure1);            // copy failed
  EXPECT_TRUE(closure1.copyable());  // an empty closure is copyable
  closure1 = std::move(closure2);
  EXPECT_TRUE(closure1);
  EXPECT_EQ(closure1(), 1);
}

class TestClassBindMethod {
 public:
  int ResIntArg0() const { return 0; }
  int ResIntArg1NonConst(int v) { return v; }
  std::string ResStrArgs3(const std::string& v1, int v2, double v3) const {
    std::ostringstream from_double;
    from_double.precision(2);
    from_double << std::fixed << v3;
    return v1 + std::to_string(v2) + from_double.str();
  }
  std::size_t ResSizeTArg1NonConst(int add) {
    change_.emplace_back(add);
    return std::accumulate(change_.begin(), change_.end(), 0);
  }
  static void StaticFunc() {}

  std::vector<int> change_;
};

TEST(TestClosure, Method) {
  TestClassBindMethod cl;
  static_assert(std::is_member_function_pointer<decltype(&TestClassBindMethod::ResIntArg0)>::value, "");

  auto closure1 = MakeClosure(&TestClassBindMethod::ResIntArg0, &cl);
  EXPECT_EQ(closure1(), 0);

  closure1 = MakeClosure(&TestClassBindMethod::ResIntArg1NonConst, std::make_unique<TestClassBindMethod>(), 233);
  EXPECT_EQ(closure1(), 233);

  auto closure2 = MakeClosure(&TestClassBindMethod::ResIntArg1NonConst);
  static_assert(std::is_same<decltype(closure2), Closure<int(TestClassBindMethod*, int)>>::value, "");
  EXPECT_EQ(closure2(&cl, 5), 5);
  closure2 = &TestClassBindMethod::ResSizeTArg1NonConst;
  EXPECT_EQ(closure2(&cl, 1), 1);
  EXPECT_EQ(closure2(&cl, 2), 3);
  EXPECT_EQ(closure2(&cl, 3), 6);

  const TestClassBindMethod ccl;  // cannot non-const method
  //  MakeClosure(&TestClassBindMethod::ResIntArg1NonConst, ccl);
  EXPECT_EQ(MakeClosure(&TestClassBindMethod::ResIntArg1NonConst, cl)(1), 1);  // OK
  EXPECT_EQ(MakeClosure(&TestClassBindMethod::ResStrArgs3, &ccl, "1", 2, 3.45)(), "123.45");

  Closure<std::string()> closure3(&TestClassBindMethod::ResStrArgs3, &ccl, "1", 2, 3.45);
  EXPECT_EQ(closure3(), "123.45");

  {
    auto c1 = MakeClosure(&TestClassBindMethod::ResIntArg0);
    static_assert(std::is_same<Closure<int(TestClassBindMethod*)>, decltype(c1)>::value, "");
    //    std::function f1 = &TestClassBindMethod::ResIntArg0;
  }

  auto ptr = std::make_unique<TestClassBindMethod>();
  auto closure4 = MakeClosure(&TestClassBindMethod::ResIntArg1NonConst, std::move(ptr));
  EXPECT_EQ(closure4(123), 123);
  auto bounded = [capture0 = std::make_unique<TestClassBindMethod>()](int v) {
    return capture0->ResIntArg1NonConst(v);
  };
  EXPECT_EQ(bounded(123), 123);
  //  std::function<int(int)> _ = std::move(bounded);  // can't compile
  closure4 = std::move(bounded);
  assert(closure4);
  EXPECT_EQ(closure4(123), 123);
  EXPECT_FALSE(closure4.copyable());
  auto closure5 = closure4;
  EXPECT_FALSE(closure5);
  assert(!closure5);
}

TEST(TestClosure, NonSimpleFunctor) {
  struct NonSimple {
    std::string operator()() const { return "empty"; }
    int operator()(int a, int b) const { return a + b; }
  };
  //  MakeClosure(NonSimple{}); // can't compile
  Closure<std::string()> closure1 = NonSimple{};
  EXPECT_EQ(closure1(), "empty");
  Closure<int(int, int)> closure2 = NonSimple{};
  EXPECT_EQ(closure2(1, 2), 3);

  struct Simple {
    int operator()(int a, int b) const volatile { return a + b; }
  };
  closure2 = MakeClosure(Simple{});  // ok
  EXPECT_EQ(closure2(7, 8), 15);
}

TEST(TestClosureWithPlaceHolders, FunctionPointer) {
  auto closure1 = MakeClosure(sum, closure::PlaceHolder<0>());
  static_assert(std::is_same<decltype(closure1), Closure<std::size_t(const int&, double, int, int)>>::value, "");
  EXPECT_EQ(closure1(1, 2, 3, 4), 10);
  auto closure2 = MakeClosure(sum, closure::PlaceHolder<2>(), closure::PlaceHolder<1>(), closure::PlaceHolder<3>());
  static_assert(
      std::is_same<decltype(closure2), Closure<std::size_t(closure::Any, double, const int&, int, int)>>::value, "");
  EXPECT_EQ(closure2("ignored", 1, 2, 3, 4), 10);
  closure1 = sum;
  EXPECT_EQ(closure1(1, 2, 3, 4), 10);
  auto closure3 = MakeClosure(forwarding_test, closure::PlaceHolder<1>());
  auto ptr = std::make_unique<int>(5);
  EXPECT_EQ(closure3(nullptr, std::move(ptr)), 5);

  Closure<int(int, std::unique_ptr<int>)> closure4(forwarding_test, PlaceHolder<1>());
  EXPECT_EQ(closure4(1, std::make_unique<int>(1)), 1);
  closure4 = std::move(closure3);
  EXPECT_EQ(closure4(2, std::make_unique<int>(1)), 1);
  EXPECT_FALSE(closure3);
}

TEST(TestClosureWithPlaceHolders, Functor) {
  std::string unused = "12345";
  auto lambda1 = [unused](int v1, int v2) { return v1 - v2; };
  auto closure1 = MakeClosure(lambda1, closure::PlaceHolder<1>(), closure::PlaceHolder<0>());
  EXPECT_EQ(closure1(5, 3), -2);
  auto lptr = closure1.target<decltype(lambda1)>();
  ASSERT_NE(lptr, nullptr);
  EXPECT_EQ((*lptr)(6, 9), -3);
  closure1 = MakeClosure(lambda1, closure::PlaceHolder<1>(), 1);  // Now it stores the closure returned by MakeClosure
  EXPECT_EQ(closure1.target<decltype(lambda1)>(), nullptr);
  EXPECT_EQ(closure1(4, 1), 0);
  Closure<int64_t(int, int)> closure2(lambda1, PlaceHolder<1>(), PlaceHolder<0>());
  EXPECT_EQ(closure2(3, 4), 1);
  closure2 = closure1;
  EXPECT_EQ(closure2(7, 3), 2);

  lptr = closure2.target<decltype(lambda1)>();
  ASSERT_EQ(lptr, nullptr);

  auto cptr = closure2.target<Closure<int(int, int)>>();
  ASSERT_NE(cptr, nullptr);
  EXPECT_EQ((*cptr)(8, 6), 5);

  auto lambda2 = [](const std::string&, const char*) {};
  (void)lambda2;
  //    auto closure3 = MakeClosure(lambda2, PlaceHolder<0>(), PlaceHolder<0>()); cannot initialize such closure
  auto lambda3 = [](const char* v1, const std::string& v2) -> std::string { return v1 + v2; };
  auto closure3 = MakeClosure(lambda3, PlaceHolder<0>(), PlaceHolder<0>());
  EXPECT_EQ(closure3("123"), "123123");
}

TEST(TestClosureWithPlaceHolders, Method) {
  TestClassBindMethod cl;
  auto closure1 = MakeClosure(&TestClassBindMethod::ResStrArgs3, PlaceHolder<2>(), PlaceHolder<1>(), PlaceHolder<0>());
  static_assert(std::is_same<decltype(closure1),
                             Closure<std::string(int, const std::string&, TestClassBindMethod*, double)>>::value,
                "");
  EXPECT_EQ(closure1(3, "+", &cl, 5.55), "+35.55");

  Closure<std::string(int, std::string, const TestClassBindMethod&, double)> closure2(
      &TestClassBindMethod::ResStrArgs3, PlaceHolder<2>(), PlaceHolder<1>(), PlaceHolder<0>());
  EXPECT_EQ(closure2(3, "+", cl, 5.55), "+35.55");

  auto ptr = closure2.target<decltype(&TestClassBindMethod::ResStrArgs3)>();
  EXPECT_NE(ptr, nullptr);
  EXPECT_EQ((cl.**ptr)("+", 3, 4.44), "+34.44");
  auto ptr2 = closure2.target<int()>();
  EXPECT_EQ(ptr2, nullptr);
}

TEST(TestClosureWithPlaceHolders, Any) {
  auto lambda = [](int a, int b) { return a + b; };
  auto closure = MakeClosure(lambda, PlaceHolder<1>(), PlaceHolder<3>());
  EXPECT_EQ(closure("123", 4, "567", 8), 12);
  EXPECT_EQ(closure(std::vector<int>{1, 2}, 3, std::vector<long>{4, 5}, 6), 9);

  static_assert(std::is_same<decltype(closure), closure::Closure<int(closure::Any, int, closure::Any, int)>>::value,
                "");
  // it's ok that the 4th parameter is float type, because float can implicitly convert to int
  Closure<int(int, int, std::string, float)> c2(lambda, PlaceHolder<1>(), PlaceHolder<3>());
  c2 = closure;
  EXPECT_EQ(c2(1, 2, "3", 4.2), 6);
}

TEST(TestClosureWithPlaceHolders, Range) {
  auto lambda = [](int a, int b, int c, int d, int e, int f, int g) {
    using std::to_string;
    return to_string(a) + to_string(b) + to_string(c) + to_string(d) + to_string(e) + to_string(f) + to_string(g);
  };
  auto closure = MakeClosure(lambda, PlaceHolder<0, 4>(), 6, 7);
  std::string res = closure(1, 2, 3, 4, 5);
  EXPECT_EQ(res, std::string{"1234567"});
}