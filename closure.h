//
// Created by Youtao Guo on 2022/12/27
//
#pragma once

#include <tuple>
#include <type_traits>

namespace details {

namespace arg {

template <class... Tp>
struct ArgList {};

template <class, class>
struct IsPrefix;

template <class... Args2>
struct IsPrefix<ArgList<>, ArgList<Args2...>> : std::true_type {};

template <class F1, class... O1>
struct IsPrefix<ArgList<F1, O1...>, ArgList<>> : std::false_type {};

template <class F1, class... O1, class... O2>
struct IsPrefix<ArgList<F1, O1...>, ArgList<F1, O2...>> : IsPrefix<ArgList<O1...>, ArgList<O2...>> {};

template <class F1, class... O1, class F2, class... O2>
struct IsPrefix<ArgList<F1, O1...>, ArgList<F2, O2...>> : std::false_type {};

template <class, class>
constexpr bool IsPrefixV;

template <class... Args1, class... Args2>
constexpr bool IsPrefixV<ArgList<Args1...>, ArgList<Args2...>> = IsPrefix<ArgList<Args1...>, ArgList<Args2...>>::value;

template <class Prefix, class ArgL>
struct RemovePrefix {};

template <>
struct RemovePrefix<ArgList<>, ArgList<>> {
  using type = ArgList<>;
};

template <class... Args2>
struct RemovePrefix<ArgList<>, ArgList<Args2...>> {
  using type = ArgList<Args2...>;
};

template <class F1, class... O1, class... O2>
struct RemovePrefix<ArgList<F1, O1...>, ArgList<F1, O2...>> {
  static_assert(IsPrefixV<ArgList<O1...>, ArgList<O2...>>);
  using type = typename RemovePrefix<ArgList<O1...>, ArgList<O2...>>::type;
};

template <class Prefix, class ArgL>
using RemovePrefixT = typename RemovePrefix<Prefix, ArgL>::type;

template <class, class>
struct IsPrefixWeak;

template <class... Args2>
struct IsPrefixWeak<ArgList<>, ArgList<Args2...>> : std::true_type {};

template <class F1, class... O1>
struct IsPrefixWeak<ArgList<F1, O1...>, ArgList<>> : std::false_type {};

template <class F1, class... O1, class F2, class... O2>
struct IsPrefixWeak<ArgList<F1, O1...>, ArgList<F2, O2...>>
    : std::conditional_t<std::is_convertible_v<F1, F2>, IsPrefix<ArgList<O1...>, ArgList<O2...>>, std::false_type> {};

template <class, class>
constexpr bool IsPrefixWeakV;

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

}  // namespace arg

template <class>
class Closure;

template <class R, class... Args>
class Closure<R(Args...)> {
 public:
  using result_type = R;
  using arguments_type = arg::ArgList<Args...>;
  Closure() = default;
  virtual ~Closure() = default;
  virtual result_type Run(Args...) const = 0;
};

template <class...>
class FuncClosure;

template <class R, class... Args, class... CallableArgs, class... Binds>
class FuncClosure<R (*)(Args...), R (*)(CallableArgs...), Binds...> : public Closure<R(Args...)> {
 public:
  using result_type = typename Closure<R(Args...)>::result_type;
  using callable_type = R (*)(CallableArgs...);
  using binder_type = std::tuple<Binds...>;

  template <class... BindArgs>
  explicit FuncClosure(callable_type f, BindArgs&&... args)
      : callable_(f), bind_list_(std::forward<BindArgs>(args)...) {}

  result_type Run(Args... args) const override {
    return RunHelper(std::index_sequence_for<binder_type>{}, std::forward<Args>(args)...);
  }

 private:
  template <std::size_t... I>
  result_type RunHelper(std::index_sequence<I...>, Args... args) const {
    return callable_(std::get<I>(bind_list_)..., std::forward<Args>(args)...);
  }

  callable_type callable_;
  binder_type bind_list_;
};

template <class R, class... ClosureArgs, class... Args, class... Binds>
Closure<R(ClosureArgs...)>* NewClosureImpl(arg::ArgList<ClosureArgs...>, R (*func)(Args...), Binds&&... bind_args) {
  return new FuncClosure<R (*)(ClosureArgs...), R (*)(Args...), std::decay_t<Binds>...>(
      func, std::forward<Binds>(bind_args)...);
}

};  // namespace details

template <class R, class... Args, class... Binds,
          class = std::enable_if_t<
              details::arg::IsPrefixWeakV<details::arg::ArgList<Binds...>, details::arg::ArgList<Args...>>>>
auto NewClosure(R (*func)(Args...), Binds&&... bind_args) {
  using namespace details;
  using closure_args = arg::RemovePrefixWeakT<arg::ArgList<Binds...>, arg::ArgList<Args...>>;
  return NewClosureImpl(closure_args{}, func, std::forward<Binds>(bind_args)...);
}
