//
// Created by Youtao Guo on 2022/12/27
//

#pragma once

#include <cstring>
#include <memory>
#include <tuple>

#include "closure/placeholders.hpp"
#include "closure/traits.hpp"

namespace closure {

template <size_t I>
constexpr auto PlaceHolder() noexcept {
  return placeholders::PH<I>{};
}

namespace details {

template <class, class>
struct IsPrefixWeak : std::false_type {};

template <class... Args2>
struct IsPrefixWeak<ArgList<>, ArgList<Args2...>> : std::true_type {};

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
struct RemovePrefixWeak;

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

template <class Uniqued, class = void>
struct Fill;

template <size_t I, class Tp1, class Tp2, class... Os>
struct Fill<ArgList<Component<I, Tp1>, Component<I + 1, Tp2>, Os...>> {
  using type = ConcatT<ArgList<Component<I, Tp1>>, typename Fill<ArgList<Component<I + 1, Tp2>, Os...>>::type>;
};

template <size_t I1, class Tp1, size_t I2, class Tp2, class... Os>
struct Fill<ArgList<Component<I1, Tp1>, Component<I2, Tp2>, Os...>, std::enable_if_t<I1 + 1 < I2>> {
  using type = ConcatT<ArgList<Component<I1, Tp1>>,
                       typename Fill<ArgList<Component<I1 + 1, Auto>, Component<I2, Tp2>, Os...>>::type>;
};

template <size_t I, class Tp>
struct Fill<ArgList<Component<I, Tp>>> {
  using type = ArgList<Component<I, Tp>>;
};

template <class Tp, class... Os>
auto FillFromZero(ArgList<Component<0, Tp>, Os...>) -> typename Fill<ArgList<Component<0, Tp>, Os...>>::type;

template <size_t I, class Tp, class... Os /*, class = std::enable_if_t<I != 0>*/>
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

template <class ClosureArg, class Callee, class CalleeArgsList, class = void>
class ClosureImpl;

// Overload for function pointer and functor
// It is guaranteed that all the StoredArgs... are non-reference types.
template <class R, class... Args, class Callable, class... StoredArgs>
class ClosureImpl<R(Args...), Callable, ArgList<StoredArgs...>,
                  std::enable_if_t<__CLOSTD::is_function_v<std::remove_pointer_t<Callable>> ||
                                   traits::IsFunctorV<std::remove_reference_t<Callable>>>>
    : public ClosureImplBase<R(Args...)> {
  using base = ClosureImplBase<R(Args...)>;

 public:
  using callee_type = Callable;
  using stored_types = std::tuple<StoredArgs...>;

  static_assert(
      __CLOSTD::is_convertible_v<
          __CLOSTD::invoke_result_t<
              callee_type, std::add_lvalue_reference_t<StoredArgs>... /*stored args are passed by lvalue*/, Args...>,
          typename base::result_type>,
      "the result type of the given callee is not match");

  template <class Callee, class... BoundArgs,
            std::enable_if_t<!placeholders::HasPlaceHolderV<std::decay_t<BoundArgs>...>, int> = 0>
  explicit ClosureImpl(Callee&& f, BoundArgs&&... args)
      : callable_(std::forward<Callee>(f)), stored_list_(std::forward<BoundArgs>(args)...) {}

  typename base::result_type Run(Args... args) override {
    return RunHelper(std::index_sequence_for<StoredArgs...>{}, std::forward<Args>(args)...);
  }

 private:
  template <std::size_t... I>
  decltype(auto) RunHelper(std::index_sequence<I...>, Args... args) {
    return callable_(std::get<I>(stored_list_)..., std::forward<Args>(args)...);
  }

  callee_type callable_;
  stored_types stored_list_;
};

// TODO Overload for class method (non const)
// It is guaranteed that the CPtr and all StoredArgs... are non-reference types.
template <class R, class... Args, class Class, class CPtr, class... CallableArgs, class... StoredArgs>
class ClosureImpl<R(Args...), R (Class::*)(CallableArgs...), ArgList<CPtr, StoredArgs...>>
    : public ClosureImplBase<R(Args...)> {
  using base = ClosureImplBase<R(Args...)>;

 public:
  using callee_type = R (Class::*)(CallableArgs...);
  using stored_types = std::tuple<CPtr, StoredArgs...>;

  static_assert(traits::IsDereferencableV<CPtr, Class>, "Cptr is not the correct dereferencable type");
  static_assert(traits::CanUsePointerToMemberFunctionV<CPtr, callee_type>,
                "cannot use pointer to the non-const class member function, maybe a const class instance is given");

  template <class CArg, class... BoundArgs,
            std::enable_if_t<!placeholders::HasPlaceHolderV<std::decay_t<BoundArgs>...>, int> = 0>
  explicit ClosureImpl(callee_type f, CArg&& carg, BoundArgs&&... args)
      : callable_(f), stored_list_(std::forward<CArg>(carg), std::forward<BoundArgs>(args)...) {}

  typename base::result_type Run(Args... args) override {
    return RunHelper(std::index_sequence_for<StoredArgs...>{}, std::forward<Args>(args)...);
  }

 private:
  template <std::size_t... I>
  typename base::result_type RunHelper(std::index_sequence<I...>, Args... args) {
    return (std::get<0>(stored_list_)->*callable_)(std::get<I + 1>(stored_list_)..., std::forward<Args>(args)...);
  }

 private:
  callee_type callable_;
  stored_types stored_list_;
};

// Overload for function pointer
template <class R, class... ClosureArgs, class... Args, class... Bounds,
          std::enable_if_t<!placeholders::HasPlaceHolderV<Bounds...>, int> = 0>
decltype(auto) MakeClosureImpl(ArgList<ClosureArgs...>, R (*func)(Args...), Bounds&&... bound_args) {
  return new ClosureImpl<R(ClosureArgs...), R (*)(Args...), ArgList<std::remove_reference_t<Bounds>...>>(
      func, std::forward<Bounds>(bound_args)...);
}

// Overload for functor
template <
    class R, class... ClosureArgs, class Functor, class... Bounds,
    std::enable_if_t<traits::IsFunctorV<std::remove_reference_t<Functor>> && !placeholders::HasPlaceHolderV<Bounds...>,
                     int> = 0>
decltype(auto) MakeClosureImpl(ArgList<ClosureArgs...>, Functor&& functor, Bounds&&... bound_args) {
  return new ClosureImpl<R(ClosureArgs...), std::remove_reference_t<Functor>,
                         ArgList<std::remove_reference_t<Bounds>...>>(std::forward<Functor>(functor),
                                                                      std::forward<Bounds>(bound_args)...);
}

// TODO Overload for closure which has placeholders.
template <class R, class... ClosureArgs, class... Args, class... Bounds,
          std::enable_if_t<placeholders::HasPlaceHolderV<Bounds...>, int> = 0>
decltype(auto) MakeClosureImpl(ArgList<ClosureArgs...>, R (*func)(Args...), Bounds&&... bind_args) {}

}  // namespace __closure

template <class>
class Closure;

template <class R, class... Args>
class Closure<R(Args...)> {
  using impl_type = __closure::ClosureImplBase<R(Args...)>;

 public:
  using result_type = R;
  using arguments_type = typename impl_type::arguments_type;

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

// MakeClosure for function pointer, remove cv-qualifier and noexcept
template <class R, class... Args, class... Bounds>
decltype(auto) MakeClosure(R (*func)(Args...), Bounds&&... bound_args) {
  using closure_args = details::RemovePrefixWeakT<ArgList<Bounds...>, ArgList<Args...>>;
  auto res = __closure::MakeClosureImpl(closure_args{}, func, std::forward<Bounds>(bound_args)...);
  using closure_res_t = typename std::remove_pointer_t<decltype(res)>::closure_type;
  return Closure<closure_res_t>(res);
}

template <class R, class... ClosureArgs, class Functor, class... Bounds>
decltype(auto) MakeClosure(Functor&& functor, Bounds&&... bound_args) {
  auto res = __closure::MakeClosureImpl<R>(ArgList<ClosureArgs...>{}, std::forward<Functor>(functor),
                                           std::forward<Bounds>(bound_args)...);
  using closure_res_t = typename std::remove_pointer_t<decltype(res)>::closure_type;
  return Closure<closure_res_t>(res);
}

// TODO noexcept had been a part of type system since c++17. make a specialization so that we can call noexcept function
// in noexcept.

}  // namespace closure
