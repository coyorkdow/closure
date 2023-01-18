//
// Created by Youtao Guo on 2022/12/27
//
#pragma once

#include <cstring>
#include <memory>
#include <tuple>
#include <type_traits>

#ifdef __CLOSTD
#error "Macro __CLOSTD is already defined"
#endif

namespace closure {

#if __cplusplus < 201703L
#define __CLOSTD __clostd

namespace __clostd {

template <class Tuple>
constexpr auto tuple_size_v = std::tuple_size<Tuple>::value;

template <class From, class To>
constexpr auto is_convertible_v = std::is_convertible<From, To>::value;

template <class Tp1, class Tp2>
constexpr auto is_same_v = std::is_same<Tp1, Tp2>::value;

template <class Tp>
constexpr auto is_const_v = std::is_const<Tp>::value;

}  // namespace __clostd

#else
#define __CLOSTD std
#endif

template <class... Tps>
struct ArgList {};

template <class, class>
struct Concat;

template <class... Tps1, class... Tps2>
struct Concat<ArgList<Tps1...>, ArgList<Tps2...>> {
  using type = ArgList<Tps1..., Tps2...>;
};

template <class... Tps1, class... Tps2>
struct Concat<std::tuple<Tps1...>, std::tuple<Tps2...>> {
  using type = std::tuple<Tps1..., Tps2...>;
};

template <size_t... I1, size_t... I2>
struct Concat<std::index_sequence<I1...>, std::index_sequence<I2...>> {
  using type = std::index_sequence<I1..., I2...>;
};

template <class L1, class L2>
using ConcatT = typename Concat<L1, L2>::type;

// When the placeholders is discontinuous, the closure will have several superfluous parameters which can take any
// types. We use Auto to express them.
struct Auto {};

namespace placeholders {
template <size_t>
struct PH {};

template <class>
struct IsPlaceHolder : std::false_type {};

template <size_t I>
struct IsPlaceHolder<PH<I>> : std::true_type {};

template <class Tp>
constexpr auto IsPlaceHolderV = IsPlaceHolder<Tp>{};

template <class>
struct HasPlaceHolder;

template <>
struct HasPlaceHolder<ArgList<>> : std::false_type {};

template <size_t I, class... Tps>
struct HasPlaceHolder<ArgList<PH<I>, Tps...>> : std::true_type {};

template <class F, class... Os>
struct HasPlaceHolder<ArgList<F, Os...>> : HasPlaceHolder<ArgList<Os...>> {};

template <class... Tps>
constexpr auto HasPlaceHolderV = HasPlaceHolder<ArgList<Tps...>>{};

template <class... Tps>
constexpr auto HasPlaceHolderV<ArgList<Tps...>> = HasPlaceHolder<ArgList<Tps...>>{};

template <class>
struct FilterPlaceHolder;

template <>
struct FilterPlaceHolder<ArgList<>> {
  using type = ArgList<>;
};

template <size_t I, class... Tps>
struct FilterPlaceHolder<ArgList<PH<I>, Tps...>> {
  using type = ConcatT<ArgList<PH<I>>, typename FilterPlaceHolder<ArgList<Tps...>>::type>;
};

template <class F, class... Os>
struct FilterPlaceHolder<ArgList<F, Os...>> {
  using type = typename FilterPlaceHolder<ArgList<Os...>>::type;
};

template <class ArgL>
using FilterPlaceHolderT = typename FilterPlaceHolder<ArgL>::type;

// Agent stores the reference of the placeholder corresponding argument and the argument can be accessed by Getter.
// Initialize Agent with a temporary object (prvalue) will occur a dangling reference.
template <class Tp>
class Agent {
  class Wrapper {
   public:
    explicit Wrapper(Tp&& v) noexcept : data_(std::forward<Tp>(v)) {}
    Tp&& data_;
  };

 public:
  Agent() noexcept : ptr_(nullptr), mem_{} {}
  explicit Agent(Tp&& v) noexcept : ptr_(reinterpret_cast<Wrapper*>(mem_)), mem_{} {
    new (mem_) Wrapper(std::forward<Tp>(v));
  }
  Agent& operator=(Tp&& v) noexcept {
    ptr_ = reinterpret_cast<Wrapper*>(mem_);
    new (mem_) Wrapper(std::forward<Tp>(v));
    return *this;
  }
  Agent(Agent&& rhs) noexcept : ptr_(nullptr), mem_{} {
    if (rhs.ptr_) {
      new (mem_) Wrapper(std::move(*rhs.ptr_));
      rhs.ptr_ = nullptr;
      ptr_ = reinterpret_cast<Wrapper*>(mem_);
    }
  }
  Agent& operator=(Agent&& rhs) noexcept {
    ptr_ = nullptr;
    if (rhs.ptr_) {
      new (mem_) Wrapper(std::move(*rhs.ptr_));
      rhs.ptr_ = nullptr;
      ptr_ = reinterpret_cast<Wrapper*>(mem_);
    }
    return *this;
  }

  explicit operator bool() const noexcept { return ptr_; }
  decltype(auto) Get() const noexcept { return std::forward<Tp>(ptr_->data_); }

 private:
  Wrapper* ptr_;
  unsigned char mem_[sizeof(Wrapper)];
};

template <>
class Agent<Auto> {
 public:
  constexpr Agent() noexcept = default;
  template <class Tp>
  explicit constexpr Agent(Tp&&) noexcept {}
};

template <class... Tps>
auto MakeAgents(std::tuple<Tps...>) -> std::tuple<Agent<Tps>...>;

template <class... Tps>
auto MakeAgents(ArgList<Tps...>) -> std::tuple<Agent<Tps>...>;

template <class Tuple>
using MakeAgentsT = decltype(MakeAgents(std::declval<Tuple>()));

template <class>
struct IsAgent : std::false_type {};

template <class Tp>
struct IsAgent<Agent<Tp>> : std::true_type {};

template <class Tp>
constexpr auto IsAgentDecayV = IsAgent<std::decay_t<Tp>>{};

template <class AgentsTuple, size_t I>
class Getter {
  static_assert(I < __CLOSTD::tuple_size_v<AgentsTuple>, "the index of Getter is out of bounds");
  static_assert(IsAgentDecayV<decltype(std::get<I>(std::declval<AgentsTuple&>()))>, "Getter's target is not an agent");

 public:
  Getter() noexcept = default;
  Getter(PH<I>) noexcept {}  // allow that the PlaceHolder can be implicitly converted to the Getter.
  // Mapping a Getter to an Agent.
  void Map(AgentsTuple& tuple) { agents_tuple_ = tuple; }

  decltype(auto) Get() const noexcept { return std::get<I>(agents_tuple_.Get()).Get(); }

  explicit operator bool() const noexcept { return static_cast<bool>(agents_tuple_); }

 private:
  Agent<AgentsTuple&> agents_tuple_;
};

template <class>
struct IsGetter : std::false_type {};

template <class Tuple, size_t I>
struct IsGetter<Getter<Tuple, I>> : std::true_type {};

template <class Tp>
constexpr auto IsGetterDecayV = IsGetter<std::decay_t<Tp>>{};

template <size_t I, class GettersTuple, class AgentsTuple,
          std::enable_if_t<IsGetterDecayV<decltype(std::get<I>(std::declval<GettersTuple>()))>, int> = 0>
void TryMap(GettersTuple&& getters, AgentsTuple& agents, int& cnt) {
  std::get<I>(std::forward<GettersTuple>(getters)).Map(agents);
  cnt++;
}

template <size_t I, class... Tps>
constexpr void TryMap(Tps&&...) noexcept {}

template <size_t... I, class GettersTuple, class AgentsTuple>
int MapGetters(std::index_sequence<I...>, GettersTuple&& getters, AgentsTuple& agents) {
  using expander = int[];
  int cnt = 0;
  (void)expander{(TryMap<I>(std::forward<GettersTuple>(getters), agents, cnt), 0)...};
  return cnt;
}

template <class GettersTuple, class AgentsTuple>
int MapGetters(GettersTuple&& getters, AgentsTuple& agents) {
  constexpr auto size = __CLOSTD::tuple_size_v<std::decay_t<GettersTuple>>;
  return MapGetters(std::make_index_sequence<size>{}, std::forward<GettersTuple>(getters), agents);
}

template <size_t I, class Tuple,
          std::enable_if_t<IsGetterDecayV<decltype(std::get<I>(std::declval<Tuple>()))>, int> = 0>
decltype(auto) Get(Tuple&& tuple) noexcept {
  return std::get<I>(std::forward<Tuple>(tuple)).Get();
}

template <size_t I, class Tuple,
          std::enable_if_t<!IsGetterDecayV<decltype(std::get<I>(std::declval<Tuple>()))>, int> = 0>
decltype(auto) Get(Tuple&& tuple) noexcept {
  return std::get<I>(std::forward<Tuple>(tuple));
}

};  // namespace placeholders

template <size_t I>
constexpr auto PlaceHolder() noexcept {
  return placeholders::PH<I>{};
}

namespace details {

template <class, class>
struct IsPrefixWeak;

template <class... Args2>
struct IsPrefixWeak<ArgList<>, ArgList<Args2...>> : std::true_type {};

template <class F1, class... Os1>
struct IsPrefixWeak<ArgList<F1, Os1...>, ArgList<>> : std::false_type {};

template <class F1, class... Os1, class F2, class... Os2>
struct IsPrefixWeak<ArgList<F1, Os1...>, ArgList<F2, Os2...>>
    : std::conditional_t<placeholders::IsPlaceHolderV<F1> || __CLOSTD::is_convertible_v<F1, F2>,
                         IsPrefixWeak<ArgList<Os1...>, ArgList<Os2...>>, std::false_type> {};

template <class, class>
constexpr bool IsPrefixWeakV = false;

template <class... Args1, class... Args2>
constexpr bool IsPrefixWeakV<ArgList<Args1...>, ArgList<Args2...>> =
    IsPrefixWeak<ArgList<Args1...>, ArgList<Args2...>>::value;

template <class Prefix, class ArgL>
struct RemovePrefixWeak {};

template <>
struct RemovePrefixWeak<ArgList<>, ArgList<>> {
  using type = ArgList<>;
};

template <class... Args2>
struct RemovePrefixWeak<ArgList<>, ArgList<Args2...>> {
  using type = ArgList<Args2...>;
};

template <class F1, class... Os1, class F2, class... Os2>
struct RemovePrefixWeak<ArgList<F1, Os1...>, ArgList<F2, Os2...>> {
  static_assert(IsPrefixWeakV<ArgList<F1, Os1...>, ArgList<F2, Os2...>>,
                "template argument 1 is not the weak prefix of template argument 2");
  using type = typename RemovePrefixWeak<ArgList<Os1...>, ArgList<Os2...>>::type;
};

template <class Prefix, class ArgL>
using RemovePrefixWeakT = typename RemovePrefixWeak<Prefix, ArgL>::type;

template <class Prefix, class ArgL>
struct GetPlaceHoldersCorrespondTypes;

template <size_t I, class... Os1, class F2, class... Os2>
struct GetPlaceHoldersCorrespondTypes<ArgList<placeholders::PH<I>, Os1...>, ArgList<F2, Os2...>> {
  using type = ConcatT<ArgList<F2>, typename GetPlaceHoldersCorrespondTypes<ArgList<Os1...>, ArgList<Os2...>>::type>;
};

template <class F1, class... Os1, class F2, class... Os2>
struct GetPlaceHoldersCorrespondTypes<ArgList<F1, Os1...>, ArgList<F2, Os2...>> {
  using type = typename GetPlaceHoldersCorrespondTypes<ArgList<Os1...>, ArgList<Os2...>>::type;
};

template <class... Tps>
struct GetPlaceHoldersCorrespondTypes<ArgList<>, ArgList<Tps...>> {
  using type = ArgList<>;
};

template <class Prefix, class ArgL>
using GetPlaceHoldersCorrespondTypesT = typename GetPlaceHoldersCorrespondTypes<Prefix, ArgL>::type;

namespace sort {

enum class Cond { Unchecked, False, True };

template <size_t I, class Tp>
struct Component {};

template <class Com, class Sorted, Cond = Cond::Unchecked>
struct Insert;

template <size_t I, class Tp, size_t FI, class FT, class... Os>
struct Insert<Component<I, Tp>, ArgList<Component<FI, FT>, Os...>, Cond::Unchecked> {
  using type =
      typename Insert<Component<I, Tp>, ArgList<Component<FI, FT>, Os...>, (I <= FI) ? Cond::True : Cond::False>::type;
};

template <size_t I, class Tp, class... Os>
struct Insert<Component<I, Tp>, ArgList<Os...>, Cond::True> {
  using type = ArgList<Component<I, Tp>, Os...>;
};

template <size_t I, class Tp, size_t FI, class FT, class... Os>
struct Insert<Component<I, Tp>, ArgList<Component<FI, FT>, Os...>, Cond::False> {
  using type = ConcatT<ArgList<Component<FI, FT>>, typename Insert<Component<I, Tp>, ArgList<Os...>>::type>;
};

template <size_t I, class Tp>
struct Insert<Component<I, Tp>, ArgList<>> {
  using type = ArgList<Component<I, Tp>>;
};

template <class ArgL, class PHL>
struct Sort;

template <class Tp, class... Tps, size_t FI, class... Os>
struct Sort<ArgList<Tp, Tps...>, ArgList<placeholders::PH<FI>, Os...>> {
  using type = typename Insert<Component<FI, Tp>, typename Sort<ArgList<Tps...>, ArgList<Os...>>::type>::type;
};

template <class Tp, size_t FI>
struct Sort<ArgList<Tp>, ArgList<placeholders::PH<FI>>> {
  using type = ArgList<Component<FI, Tp>>;
};

template <class ArgL, class PHL>
using SortT = typename Sort<ArgL, PHL>::type;

template <class Sorted>
struct Unique;

template <size_t I, class Tp1, class Tp2, class... Os>
struct Unique<ArgList<Component<I, Tp1>, Component<I, Tp2>, Os...>> {
  using type = typename Unique<ArgList<Component<I, Tp1>, Os...>>::type;
};

template <size_t I, class Tp, class... Os>
struct Unique<ArgList<Component<I, Tp>, Os...>> {
  using type = ConcatT<ArgList<Component<I, Tp>>, typename Unique<ArgList<Os...>>::type>;
};

template <>
struct Unique<ArgList<>> {
  using type = ArgList<>;
};

template <class Sorted>
using UniqueT = typename Unique<Sorted>::type;

template <class Uniqued>
struct Fill;

template <size_t I, class Tp1, class Tp2, class... Os>
struct Fill<ArgList<Component<I, Tp1>, Component<I + 1, Tp2>, Os...>> {
  using type = ConcatT<ArgList<Component<I, Tp1>>, typename Fill<ArgList<Component<I + 1, Tp2>, Os...>>::type>;
};

template <size_t I1, class Tp1, size_t I2, class Tp2, class... Os>
struct Fill<ArgList<Component<I1, Tp1>, Component<I2, Tp2>, Os...>> {
  static_assert(I1 < I2, "the given argument is not uniqued");
  using type = ConcatT<ArgList<Component<I1, Tp1>>,
                       typename Fill<ArgList<Component<I1 + 1, Auto>, Component<I2, Tp2>, Os...>>::type>;
};

template <size_t I, class Tp>
struct Fill<ArgList<Component<I, Tp>>> {
  using type = ArgList<Component<I, Tp>>;
};

template <class Tp, class... Os>
auto FillFromZero(ArgList<Component<0, Tp>, Os...>) -> typename Fill<ArgList<Component<0, Tp>, Os...>>::type;

template <size_t I, class Tp, class... Os, class = std::enable_if_t<I != 0>>
auto FillFromZero(ArgList<Component<I, Tp>, Os...>) ->
    typename Fill<ArgList<Component<0, Auto>, Component<I, Tp>, Os...>>::type;

template <class Uniqued>
using FillFromZeroT = decltype(FillFromZero(std::declval<Uniqued>()));

template <size_t... I, class... Tps>
auto RemoveIndices(ArgList<Component<I, Tps>...>) -> ArgList<Tps...>;

template <class Arg>
using RemoveIndicesV = decltype(RemoveIndices(std::declval<Arg>()));

// For sort testing

template <size_t... I, class... Tps>
auto TEST_RemoveArgs(ArgList<Component<I, Tps>...>) -> std::index_sequence<I...>;

template <size_t N>
struct TEST_MakeDummyArgs {
  using type = ConcatT<typename TEST_MakeDummyArgs<N - 1>::type, ArgList<void>>;
};

template <>
struct TEST_MakeDummyArgs<0> {
  using type = ArgList<>;
};

template <class... PHs>
struct TEST_Sort {
  using sorted = SortT<typename TEST_MakeDummyArgs<sizeof...(PHs)>::type, ArgList<PHs...>>;
  using type = decltype(TEST_RemoveArgs(std::declval<sorted>()));
};

}  // namespace sort

template <class ArgL, class PHL>
struct SortUniqueFillPlaceHoldersCorrespondTypes;

template <class... Tps, size_t... I>
struct SortUniqueFillPlaceHoldersCorrespondTypes<ArgList<Tps...>, ArgList<placeholders::PH<I>...>> {
  using type = sort::RemoveIndicesV<
      sort::FillFromZeroT<sort::UniqueT<sort::SortT<ArgList<Tps...>, ArgList<placeholders::PH<I>...>>>>>;
};

template <class ArgL, class PHL>
using SortUniqueFillPlaceHoldersCorrespondTypesT = typename SortUniqueFillPlaceHoldersCorrespondTypes<ArgL, PHL>::type;

template <class ArgL, class Tuple>
struct ReplacePlaceHoldersWithGettersImpl;

template <class F, class... Os, class Tuple>
struct ReplacePlaceHoldersWithGettersImpl<ArgList<F, Os...>, Tuple> {
  using type = ConcatT<ArgList<F>, typename ReplacePlaceHoldersWithGettersImpl<ArgList<Os...>, Tuple>::type>;
};

template <size_t I, class... Os, class Tuple>
struct ReplacePlaceHoldersWithGettersImpl<ArgList<placeholders::PH<I>, Os...>, Tuple> {
  using type = ConcatT<ArgList<placeholders::Getter<Tuple, I>>,
                       typename ReplacePlaceHoldersWithGettersImpl<ArgList<Os...>, Tuple>::type>;
};

template <class Tuple>
struct ReplacePlaceHoldersWithGettersImpl<ArgList<>, Tuple> {
  using type = ArgList<>;
};

template <class Prefix, class ArgL>
struct ReplacePlaceHoldersWithGetters {
  static_assert(IsPrefixWeakV<Prefix, ArgL>, "template argument 1 is not the weak prefix of template argument 2");

 private:
  using ph_args = GetPlaceHoldersCorrespondTypesT<Prefix, ArgL>;
  using phl = placeholders::FilterPlaceHolderT<Prefix>;
  using sorted = SortUniqueFillPlaceHoldersCorrespondTypesT<ph_args, phl>;

  // TODO: For the discontinuous placeholders, the agent should filling to continuous.

 public:
  using type = typename ReplacePlaceHoldersWithGettersImpl<Prefix, placeholders::MakeAgentsT<sorted>>::type;
  using agents_type = placeholders::MakeAgentsT<sorted>;
};

template <class Prefix, class ArgL>
using ReplacePlaceHoldersWithGettersT = typename ReplacePlaceHoldersWithGetters<Prefix, ArgL>::type;

template <class Prefix, class ArgL>
using PlaceHoldersAgentsT = typename ReplacePlaceHoldersWithGetters<Prefix, ArgL>::agents_type;

}  // namespace details

namespace __closure {

template <class>
class ClosureImplBase;

template <class R, class... Args>
class ClosureImplBase<R(Args...)> {
 public:
  using result_type = R;
  using arguments_type = ArgList<Args...>;
  using closure_type = R(Args...);
  ClosureImplBase() = default;
  virtual ~ClosureImplBase() = default;
  virtual result_type Run(Args...) = 0;
};

template <class...>
class FuncClosure;

template <class R, class... Args, class... CallableArgs, class... Binds>
class FuncClosure<R (*)(Args...), R (*)(CallableArgs...), Binds...> : public ClosureImplBase<R(Args...)> {
 public:
  using result_type = R;
  using callable_type = R (*)(CallableArgs...);
  using binder_type = std::tuple<Binds...>;

  template <class... BindArgs, class = std::enable_if_t<!placeholders::HasPlaceHolderV<std::decay_t<BindArgs>...>>>
  explicit FuncClosure(callable_type f, BindArgs&&... args)
      : callable_(f), bind_list_(std::forward<BindArgs>(args)...) {}

  result_type Run(Args... args) override {
    return RunHelper(std::index_sequence_for<Binds...>{}, std::forward<Args>(args)...);
  }

 private:
  template <std::size_t... I>
  result_type RunHelper(std::index_sequence<I...>, Args... args) {
    return callable_(std::get<I>(bind_list_)..., std::forward<Args>(args)...);
  }

  callable_type callable_;
  binder_type bind_list_;
};

template <class...>
class FuncClosureWithPlaceHolders;  // TODO

template <class R, class... ClosureArgs, class... Args, class... Binds>
std::enable_if_t<!placeholders::HasPlaceHolderV<std::decay_t<Binds>...>,
                 FuncClosure<R (*)(ClosureArgs...), R (*)(Args...), std::decay_t<Binds>...>*>
NewClosureImpl(ArgList<ClosureArgs...>, R (*func)(Args...), Binds&&... bind_args) {
  return new FuncClosure<R (*)(ClosureArgs...), R (*)(Args...), std::decay_t<Binds>...>(
      func, std::forward<Binds>(bind_args)...);
}

template <class R, class... ClosureArgs, class... Args, class... Binds>
std::enable_if_t<placeholders::HasPlaceHolderV<std::decay_t<Binds>...>,
                 FuncClosure<R (*)(ClosureArgs...), R (*)(Args...), std::decay_t<Binds>...>*>
NewClosureImpl(ArgList<ClosureArgs...>, R (*func)(Args...), Binds&&... bind_args) {
  return new FuncClosureWithPlaceHolders<R (*)(ClosureArgs...), R (*)(Args...), std::decay_t<Binds>...>(
      func, std::forward<Binds>(bind_args)...);
}

}  // namespace __closure

template <class>
class Closure;

template <class R, class... Args>
class Closure<R(Args...)> {
  using impl_type = __closure::ClosureImplBase<R(Args...)>;

 public:
  using result_type = R;
  using arguments_type = ArgList<Args...>;

  Closure() = default;

  template <class... FuncArgs, class... Binds>
  Closure(R (*func)(FuncArgs...), Binds&&... bind_args) {
    static_assert(
        __CLOSTD::is_same_v<arguments_type, details::RemovePrefixWeakT<ArgList<Binds...>, ArgList<FuncArgs...>>>,
        "the given arguments is not match the type of Closure");
    pimpl_.reset(NewClosureImpl(arguments_type{}, func, std::forward<Binds>(bind_args)...));
  }

  explicit Closure(impl_type* pimpl) : pimpl_(pimpl) {}
  Closure(const Closure&) = delete;
  Closure(Closure&&) noexcept = default;
  Closure& operator=(Closure&&) noexcept = default;

  result_type Run(Args... args) const { return pimpl_->Run(std::forward<Args>(args)...); }

  std::unique_ptr<impl_type> pimpl_;
};

template <class R, class... Args, class... Binds>
decltype(auto) MakeClosure(R (*func)(Args...), Binds&&... bind_args) {
  using closure_args = details::RemovePrefixWeakT<ArgList<Binds...>, ArgList<Args...>>;
  auto res = __closure::NewClosureImpl(closure_args{}, func, std::forward<Binds>(bind_args)...);
  using closure_res_t = typename std::remove_pointer_t<decltype(res)>::closure_type;
  return Closure<closure_res_t>(res);
}

}  // namespace closure
