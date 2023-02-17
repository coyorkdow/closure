//
// Created by Youtao Guo on 20/1/23.
//

#pragma once

#include <type_traits>

#include "closure/util.hpp"

namespace closure {

// Implementing invoke_result, source https://en.cppreference.com/w/cpp/types/result_of
// This part of codes is also used for universal invoking from the closure
namespace invoke {
template <class T>
struct is_reference_wrapper : std::false_type {};
template <class U>
struct is_reference_wrapper<std::reference_wrapper<U>> : std::true_type {};

template <class T>
struct invoke_impl {
  template <class F, class... Args>
  static auto call(F&& f, Args&&... args) -> decltype(std::forward<F>(f)(std::forward<Args>(args)...)) {
    return std::forward<F>(f)(std::forward<Args>(args)...);
  }
};

template <class B, class MT>
struct invoke_impl<MT B::*> {
  template <class T, class Td = typename std::decay<T>::type,
            class = typename std::enable_if<std::is_base_of<B, Td>::value>::type>
  static auto get(T&& t) -> T&& {
    return static_cast<T&&>(t);
  }

  template <class T, class Td = typename std::decay<T>::type,
            class = typename std::enable_if<is_reference_wrapper<Td>::value>::type>
  static auto get(T&& t) -> decltype(t.get()) {
    return t.get();
  }

  template <class T, class Td = typename std::decay<T>::type,
            class = typename std::enable_if<!std::is_base_of<B, Td>::value>::type,
            class = typename std::enable_if<!is_reference_wrapper<Td>::value>::type>
  static auto get(T&& t) -> decltype(*std::forward<T>(t)) {
    return *std::forward<T>(t);
  }

  template <class T, class... Args, class MT1, class = typename std::enable_if<std::is_function<MT1>::value>::type>
  static auto call(MT1 B::*pmf, T&& t, Args&&... args)
      -> decltype((invoke_impl::get(std::forward<T>(t)).*pmf)(std::forward<Args>(args)...)) {
    return (invoke_impl::get(std::forward<T>(t)).*pmf)(std::forward<Args>(args)...);
  }

  template <class T>
  static auto call(MT B::*pmd, T&& t) -> decltype(invoke_impl::get(std::forward<T>(t)).*pmd) {
    return invoke_impl::get(std::forward<T>(t)).*pmd;
  }
};

template <class F, class... Args, class Fd = typename std::decay<F>::type>
auto INVOKE(F&& f, Args&&... args) -> decltype(invoke_impl<Fd>::call(std::forward<F>(f), std::forward<Args>(args)...)) {
  return invoke_impl<Fd>::call(std::forward<F>(f), std::forward<Args>(args)...);
}

}  // namespace invoke

#if __cplusplus < 201703L
namespace traits {

template <class...>
using void_t = void;

// Implementing invoke_result, source https://en.cppreference.com/w/cpp/types/result_of
// Conforming C++14 implementation (is also a valid C++11 implementation):
namespace detail {
template <typename AlwaysVoid, typename, typename...>
struct invoke_result {};
template <typename F, typename... Args>
struct invoke_result<decltype(void(invoke::INVOKE(std::declval<F>(), std::declval<Args>()...))), F, Args...> {
  using type = decltype(invoke::INVOKE(std::declval<F>(), std::declval<Args>()...));
};
}  // namespace detail

template <class F, class... ArgTypes>
struct invoke_result : detail::invoke_result<void, F, ArgTypes...> {};

template <class F, class... ArgTypes>
using invoke_result_t = typename invoke_result<F, ArgTypes...>::type;

}  // namespace traits
#else
namespace traits {
using std::invoke_result;
using std::invoke_result_t;
using std::void_t;
}  // namespace traits
#endif

namespace traits {

template <class Op>
struct MemberFunctionPointerTraits {};

template <class R, class Class, class... Args>
struct MemberFunctionPointerTraits<R (Class::*)(Args...)> {
  using class_type = Class;
  using return_type = R;
  using args_type = ArgList<Args...>;
};

template <class R, class Class, class... Args>
struct MemberFunctionPointerTraits<R (Class::*)(Args...) const> {
  using class_type = Class;
  using return_type = R;
  using args_type = ArgList<Args...>;
};

template <class R, class Class, class... Args>
struct MemberFunctionPointerTraits<R (Class::*)(Args...) volatile> {
  using class_type = Class;
  using return_type = R;
  using args_type = ArgList<Args...>;
};

template <class R, class Class, class... Args>
struct MemberFunctionPointerTraits<R (Class::*)(Args...) const volatile> {
  using class_type = Class;
  using return_type = R;
  using args_type = ArgList<Args...>;
};

#if !(__cplusplus < 201703L)
template <class R, class Class, class... Args>
struct MemberFunctionPointerTraits<R (Class::*)(Args...) noexcept> {
  using class_type = Class;
  using return_type = R;
  using args_type = ArgList<Args...>;
};

template <class R, class Class, class... Args>
struct MemberFunctionPointerTraits<R (Class::*)(Args...) const noexcept> {
  using class_type = Class;
  using return_type = R;
  using args_type = ArgList<Args...>;
};

template <class R, class Class, class... Args>
struct MemberFunctionPointerTraits<R (Class::*)(Args...) volatile noexcept> {
  using class_type = Class;
  using return_type = R;
  using args_type = ArgList<Args...>;
};

template <class R, class Class, class... Args>
struct MemberFunctionPointerTraits<R (Class::*)(Args...) const volatile noexcept> {
  using class_type = Class;
  using return_type = R;
  using args_type = ArgList<Args...>;
};
#endif

// Traits for simple functor.
// A functor is considered as a simple functor iff it has only one parenthesis operator. That is, type T is a simple
// functor type iff &T::operator() is a valid expression.

template <class F, class = void>
struct SimpleFunctorTraits;

template <class F>
struct SimpleFunctorTraits<F, decltype((void)&F::operator())> : MemberFunctionPointerTraits<decltype(&F::operator())> {
};

template <class Tp>
auto IsSimpleFunctorImpl(int) -> decltype(SimpleFunctorTraits<Tp>{}, std::true_type{});

template <class Tp>
auto IsSimpleFunctorImpl(...) -> std::false_type;

template <class Tp>
struct IsSimpleFunctor : decltype(IsSimpleFunctorImpl<Tp>(0)) {};

}  // namespace traits

}  // namespace closure
