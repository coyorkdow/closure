//
// Created by Youtao Guo on 2022/12/27
//
#pragma once

#include <memory>
#include <tuple>
#include <type_traits>

template <class... Tps>
struct ArgList {};

template <class, class>
struct Concat;

template <class... Tps1, class... Tps2>
struct Concat<ArgList<Tps1...>, ArgList<Tps2...>> {
  using type = ArgList<Tps1..., Tps2...>;
};

template <size_t... I1, size_t... I2>
struct Concat<std::index_sequence<I1...>, std::index_sequence<I2...>> {
  using type = std::index_sequence<I1..., I2...>;
};

template <class L1, class L2>
using ConcatT = typename Concat<L1, L2>::type;

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

template <class, class, size_t>
struct IsContinuousSinceImpl;

template <size_t FI, class... Before, class... After, size_t I>
struct IsContinuousSinceImpl<ArgList<Before...>, ArgList<PH<FI>, After...>, I>
    : IsContinuousSinceImpl<ArgList<Before..., PH<FI>>, ArgList<After...>, I> {};

template <class... Before, class... After, size_t I>
struct IsContinuousSinceImpl<ArgList<Before...>, ArgList<PH<I>, After...>, I>
    : IsContinuousSinceImpl<ArgList<>, ArgList<Before..., After...>, I + 1> {};

template <class... Before, size_t I>
struct IsContinuousSinceImpl<ArgList<Before...>, ArgList<>, I> : std::false_type {};

template <size_t I>
struct IsContinuousSinceImpl<ArgList<>, ArgList<PH<I>>, I> : std::true_type {};

template <class>
struct GetPlaceHolderIndexImpl;

template <>
struct GetPlaceHolderIndexImpl<ArgList<>> {
  using type = std::index_sequence<>;
};

template <size_t I, class... Os>
struct GetPlaceHolderIndexImpl<ArgList<PH<I>, Os...>> {
  using type = ConcatT<std::index_sequence<I>, typename GetPlaceHolderIndexImpl<ArgList<Os...>>::type>;
};

template <class, size_t>
struct IsContinuousSince;

template <class... PHs, size_t I>
struct IsContinuousSince<ArgList<PHs...>, I> : IsContinuousSinceImpl<ArgList<>, ArgList<PHs...>, I> {};

template <class PlaceHoldersList>
using GetPlaceHolderIndex = typename GetPlaceHolderIndexImpl<PlaceHoldersList>::type;

template <class Tp>
class Agent {
  class Wrapper {
   public:
    Wrapper(Tp&& v) : data_(std::forward<Tp>(v)) {}
    Tp&& data_;
  };

 public:
  Agent() = default;
  Agent(Tp&& v) : ref_(std::make_unique<Wrapper>(std::forward<Tp>(v))) {}
  // Has already has the implicit move constructor/assign operator.

  explicit operator bool() const { return static_cast<bool>(ref_); }
  Tp&& Get() const { return std::forward<Tp>(ref_->data_); }

 private:
  std::unique_ptr<Wrapper> ref_;
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

// @Tuple must be the set of Agent.
// @Tuple -> std::tuple<Agent<Tp>...>
template <class Tuple, size_t I>
class Getter {
  static_assert(I < std::tuple_size_v<Tuple>);

 public:
  Getter() = default;
  // allow that the PlaceHolder can be implicitly converted to the Getter.
  Getter(PH<I>) {}

  void Map(Tuple& tuple) { tuple_agent_ = tuple; }

  template <class = std::enable_if_t<IsAgentDecayV<decltype(std::get<I>(std::declval<Tuple&>()))>>>
  decltype(auto) Get() const {
    return std::get<I>(tuple_agent_.Get()).Get();
  }

  explicit operator bool() const { return static_cast<bool>(tuple_agent_); }

 private:
  Agent<Tuple&> tuple_agent_;
};

template <class>
struct IsGetter : std::false_type {};

template <class Tuple, size_t I>
struct IsGetter<Getter<Tuple, I>> : std::true_type {};

template <class Tp>
constexpr auto IsGetterDecayV = IsGetter<std::decay_t<Tp>>{};

template <size_t I, class Tuple, class = std::enable_if_t<IsGetterDecayV<decltype(std::get<I>(std::declval<Tuple>()))>>>
decltype(auto) Get(Tuple&& tuple) {
  return std::get<I>(std::forward<Tuple>(tuple)).Get();
}

template <size_t I, class Tuple>
decltype(auto) Get(Tuple&& tuple, ...) {
  return std::get<I>(std::forward<Tuple>(tuple));
}

// TODO

// ReplacePlaceHoldersToGetters<Tuple, ArgList<...,PlaceHolder<I>,...>> ->
// ArgList<...,Getter<Tuple, I>,...>

// GetArgsFromIndexSequence

};  // namespace placeholders

template <size_t I>
constexpr auto PlaceHolder() {
  return placeholders::PH<I>{};
}

namespace details {

template <class, class>
struct IsPrefixWeak;

template <class... Args2>
struct IsPrefixWeak<ArgList<>, ArgList<Args2...>> : std::true_type {};

template <class F1, class... O1>
struct IsPrefixWeak<ArgList<F1, O1...>, ArgList<>> : std::false_type {};

template <class F1, class... O1, class F2, class... O2>
struct IsPrefixWeak<ArgList<F1, O1...>, ArgList<F2, O2...>>
    : std::conditional_t<placeholders::IsPlaceHolderV<F1> || std::is_convertible_v<F1, F2>,
                         IsPrefixWeak<ArgList<O1...>, ArgList<O2...>>, std::false_type> {};

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

template <class F1, class... O1, class F2, class... O2>
struct RemovePrefixWeak<ArgList<F1, O1...>, ArgList<F2, O2...>> {
  static_assert(IsPrefixWeakV<ArgList<F1, O1...>, ArgList<F2, O2...>>);
  using type = typename RemovePrefixWeak<ArgList<O1...>, ArgList<O2...>>::type;
};

template <class Prefix, class ArgL>
using RemovePrefixWeakT = typename RemovePrefixWeak<Prefix, ArgL>::type;

template <class Prefix, class ArgL>
struct GetPlaceHoldersCorrespondTypes;

template <size_t I, class... O1, class F2, class... O2>
struct GetPlaceHoldersCorrespondTypes<ArgList<placeholders::PH<I>, O1...>, ArgList<F2, O2...>> {
  using type = ConcatT<ArgList<F2>, typename GetPlaceHoldersCorrespondTypes<ArgList<O1...>, ArgList<O2...>>::type>;
};

template <class F1, class... O1, class F2, class... O2>
struct GetPlaceHoldersCorrespondTypes<ArgList<F1, O1...>, ArgList<F2, O2...>> {
  using type = typename GetPlaceHoldersCorrespondTypes<ArgList<O1...>, ArgList<O2...>>::type;
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

template <class Com, class SortedBefore, class SortedAfter, Cond = Cond::Unchecked>
struct Insert;

template <size_t I, class Tp, size_t FI, class FT, class... O1, class... O2>
struct Insert<Component<I, Tp>, ArgList<O1...>, ArgList<Component<FI, FT>, O2...>, Cond::Unchecked> {
  using type = typename Insert<Component<I, Tp>, ArgList<O1...>, ArgList<Component<FI, FT>, O2...>,
                               (I < FI) ? Cond::True : Cond::False>::type;
};

template <size_t I, class Tp, class... O1, class... O2>
struct Insert<Component<I, Tp>, ArgList<O1...>, ArgList<O2...>, Cond::True> {
  using type = ArgList<O1..., Component<I, Tp>, O2...>;
};

template <size_t I, class Tp, size_t FI, class FT, class... O1, class... O2>
struct Insert<Component<I, Tp>, ArgList<O1...>, ArgList<Component<FI, FT>, O2...>, Cond::False> {
  using type = typename Insert<Component<I, Tp>, ArgList<O1..., Component<FI, FT>>, ArgList<O2...>>::type;
};

template <size_t I, class Tp, class... O1>
struct Insert<Component<I, Tp>, ArgList<O1...>, ArgList<>> {
  using type = ArgList<O1..., Component<I, Tp>>;
};

template <class ArgL, class PHL>
struct Sort;

template <class Tp, class... Tps, size_t FI, class... Os>
struct Sort<ArgList<Tp, Tps...>, ArgList<placeholders::PH<FI>, Os...>> {
  using type =
      typename Insert<Component<FI, Tp>, ArgList<>, typename Sort<ArgList<Tps...>, ArgList<Os...>>::type>::type;
};

template <class Tp, size_t FI>
struct Sort<ArgList<Tp>, ArgList<placeholders::PH<FI>>> {
  using type = ArgList<Component<FI, Tp>>;
};

template <class ArgL, class PHL>
using SortT = typename Sort<ArgL, PHL>::type;

template <size_t... I, class... Tps>
auto RemoveIndices(ArgList<Component<I, Tps>...>) -> ArgList<Tps...>;

template <class Arg>
using RemoveIndicesV = decltype(RemoveIndices(std::declval<Arg>()));

}  // namespace sort

template <class ArgL, class PHL>
struct SortPlaceHoldersCorrespondTypes;

template <class... Tps, size_t... I>
struct SortPlaceHoldersCorrespondTypes<ArgList<Tps...>, ArgList<placeholders::PH<I>...>> {
  using type = sort::RemoveIndicesV<sort::SortT<ArgList<Tps...>, ArgList<placeholders::PH<I>...>>>;
};

template <class ArgL, class PHL>
using SortPlaceHoldersCorrespondTypesT = typename SortPlaceHoldersCorrespondTypes<ArgL, PHL>::type;

template <class ArgL, class Tuple>
struct ReplacePlaceHoldersToGettersImpl;

template <class F, class... Os, class Tuple>
struct ReplacePlaceHoldersToGettersImpl<ArgList<F, Os...>, Tuple> {
  using type = ConcatT<ArgList<F>, typename ReplacePlaceHoldersToGettersImpl<ArgList<Os...>, Tuple>::type>;
};

template <size_t I, class... Os, class Tuple>
struct ReplacePlaceHoldersToGettersImpl<ArgList<placeholders::PH<I>, Os...>, Tuple> {
  using type = ConcatT<ArgList<placeholders::Getter<Tuple, I>>,
                       typename ReplacePlaceHoldersToGettersImpl<ArgList<Os...>, Tuple>::type>;
};

template <class Tuple>
struct ReplacePlaceHoldersToGettersImpl<ArgList<>, Tuple> {
  using type = ArgList<>;
};

template <class Prefix, class ArgL>
struct ReplacePlaceHoldersToGetters {
 private:
  using ph_args = GetPlaceHoldersCorrespondTypesT<Prefix, ArgL>;
  using phl = placeholders::FilterPlaceHolderT<Prefix>;
  using sorted = SortPlaceHoldersCorrespondTypesT<ph_args, phl>;

 public:
  using type = typename ReplacePlaceHoldersToGettersImpl<Prefix, placeholders::MakeAgentsT<sorted>>::type;
};

template <class Prefix, class ArgL>
using ReplacePlaceHoldersToGettersT = typename ReplacePlaceHoldersToGetters<Prefix, ArgL>::type;

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

}  // namespace details

template <class>
class Closure;

template <class R, class... Args>
class Closure<R(Args...)> {
  using impl_type = details::ClosureImplBase<R(Args...)>;

 public:
  using result_type = R;
  using arguments_type = ArgList<Args...>;

  template <class... FuncArgs, class... Binds>
  Closure(R (*func)(FuncArgs...), Binds&&... bind_args) {
    static_assert(std::is_same_v<arguments_type, details::RemovePrefixWeakT<ArgList<Binds...>, ArgList<FuncArgs...>>>);
    pimpl_.reset(NewClosureImpl(arguments_type{}, func, std::forward<Binds>(bind_args)...));
  }

  explicit Closure(impl_type* pimpl) : pimpl_(pimpl) {}
  Closure(const Closure&) = delete;
  Closure(Closure&&) = default;
  Closure& operator=(Closure&&) = default;

  result_type Run(Args... args) const { return pimpl_->Run(std::forward<Args>(args)...); }

  std::unique_ptr<impl_type> pimpl_;
};

template <class R, class... Args, class... Binds>
decltype(auto) MakeClosure(R (*func)(Args...), Binds&&... bind_args) {
  using closure_args = details::RemovePrefixWeakT<ArgList<Binds...>, ArgList<Args...>>;
  auto res = details::NewClosureImpl(closure_args{}, func, std::forward<Binds>(bind_args)...);
  using closure_res_t = typename std::remove_pointer_t<decltype(res)>::closure_type;
  return Closure<closure_res_t>(res);
}
