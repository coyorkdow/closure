//
// Created by Youtao Guo on 20/1/23.
//

#pragma once

#include <type_traits>

#include "closure/util.hpp"

namespace closure {
#if __cplusplus < 201703L
namespace traits {

template <class...>
using void_t = void;

// Implementing invoke_result, source https://en.cppreference.com/w/cpp/types/result_of
namespace detail {
template <class T>
struct is_reference_wrapper : std::false_type {};
template <class U>
struct is_reference_wrapper<std::reference_wrapper<U>> : std::true_type {};

template <class T>
struct invoke_impl {
  template <class F, class... Args>
  static auto call(F&& f, Args&&... args) -> decltype(std::forward<F>(f)(std::forward<Args>(args)...));
};

template <class B, class MT>
struct invoke_impl<MT B::*> {
  template <class T, class Td = typename std::decay<T>::type,
            class = typename std::enable_if<std::is_base_of<B, Td>::value>::type>
  static auto get(T&& t) -> T&&;

  template <class T, class Td = typename std::decay<T>::type,
            class = typename std::enable_if<is_reference_wrapper<Td>::value>::type>
  static auto get(T&& t) -> decltype(t.get());

  template <class T, class Td = typename std::decay<T>::type,
            class = typename std::enable_if<!std::is_base_of<B, Td>::value>::type,
            class = typename std::enable_if<!is_reference_wrapper<Td>::value>::type>
  static auto get(T&& t) -> decltype(*std::forward<T>(t));

  template <class T, class... Args, class MT1, class = typename std::enable_if<std::is_function<MT1>::value>::type>
  static auto call(MT1 B::*pmf, T&& t, Args&&... args)
      -> decltype((invoke_impl::get(std::forward<T>(t)).*pmf)(std::forward<Args>(args)...));

  template <class T>
  static auto call(MT B::*pmd, T&& t) -> decltype(invoke_impl::get(std::forward<T>(t)).*pmd);
};

template <class F, class... Args, class Fd = typename std::decay<F>::type>
auto INVOKE(F&& f, Args&&... args) -> decltype(invoke_impl<Fd>::call(std::forward<F>(f), std::forward<Args>(args)...));

}  // namespace detail

// Conforming C++14 implementation (is also a valid C++11 implementation):
namespace detail {
template <typename AlwaysVoid, typename, typename...>
struct invoke_result {};
template <typename F, typename... Args>
struct invoke_result<decltype(void(detail::INVOKE(std::declval<F>(), std::declval<Args>()...))), F, Args...> {
  using type = decltype(detail::INVOKE(std::declval<F>(), std::declval<Args>()...));
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

template <class Tp, void_t<decltype(&Tp::operator->), decltype(&Tp::operator*)>* = nullptr>
auto IsDereferencableImpl(int) -> std::integral_constant<
    bool, std::is_pointer<decltype(std::declval<Tp>().operator->())>::value &&
              std::is_reference<decltype(std::declval<Tp>().operator*())>::value &&
              std::is_same<std::remove_pointer_t<decltype(std::declval<Tp>().operator->())>,
                           std::remove_reference_t<decltype(std::declval<Tp>().operator*())>>::value>;

template <class Tp>
auto IsDereferencableImpl(...) -> std::is_pointer<Tp>;

template <class Ptr>
struct IsDereferencable : decltype(IsDereferencableImpl<Ptr>(0)) {};

template <class Dereferencable, class Method, class... Args>
auto TryCallMethod(int, Dereferencable&&, Method&&, Args&&...)
    -> decltype(((*std::declval<Dereferencable>()).*std::declval<Method>())(std::declval<Args>()...), std::true_type{});

auto TryCallMethod(float, ...) -> std::false_type;

template <class Dereferencable, class Method>
struct CanUsePointerToMemberFunction : std::false_type {};

template <class Dereferencable, class R, class Class, class... Args>
struct CanUsePointerToMemberFunction<Dereferencable, R (Class::*)(Args...)>
    : decltype(TryCallMethod(0, std::declval<Dereferencable>(), std::declval<R (Class::*)(Args...)>(),
                             std::declval<Args>()...)) {};

template <class Dereferencable, class R, class Class, class... Args>
struct CanUsePointerToMemberFunction<Dereferencable, R (Class::*)(Args...) const>
    : decltype(TryCallMethod(0, std::declval<Dereferencable>(), std::declval<R (Class::*)(Args...) const>(),
                             std::declval<Args>()...)) {};

#if !(__cplusplus < 201703L)
template <class Dereferencable, class R, class Class, class... Args>
struct CanUsePointerToMemberFunction<Dereferencable, R (Class::*)(Args...) noexcept>
    : decltype(TryCallMethod(0, std::declval<Dereferencable>(), std::declval<R (Class::*)(Args...) noexcept>(),
                             std::declval<Args>()...)) {};

template <class Dereferencable, class R, class Class, class... Args>
struct CanUsePointerToMemberFunction<Dereferencable, R (Class::*)(Args...) const noexcept>
    : decltype(TryCallMethod(0, std::declval<Dereferencable>(), std::declval<R (Class::*)(Args...) const noexcept>(),
                             std::declval<Args>()...)) {};
#endif

template <class Op>
struct FunctorTraitsImpl {};

template <class R, class Class, class... Args>
struct FunctorTraitsImpl<R (Class::*)(Args...)> {
  using return_type = R;
  using args_type = ArgList<Args...>;
};

template <class R, class Class, class... Args>
struct FunctorTraitsImpl<R (Class::*)(Args...) const> {
  using return_type = R;
  using args_type = ArgList<Args...>;
};

#if !(__cplusplus < 201703L)
template <class R, class Class, class... Args>
struct FunctorTraitsImpl<R (Class::*)(Args...) noexcept> {
  using return_type = R;
  using args_type = ArgList<Args...>;
};

template <class R, class Class, class... Args>
struct FunctorTraitsImpl<R (Class::*)(Args...) const noexcept> {
  using return_type = R;
  using args_type = ArgList<Args...>;
};
#endif

template <class F, class = void>
struct FunctorTraits;

template <class F>
struct FunctorTraits<F, decltype((void)&F::operator())> : FunctorTraitsImpl<decltype(&F::operator())> {};

template <class Tp>
auto IsFunctorImpl(int) -> decltype(FunctorTraits<Tp>{}, std::true_type{});

template <class Tp>
auto IsFunctorImpl(...) -> std::false_type;

template <class Tp>
struct IsFunctor : decltype(IsFunctorImpl<Tp>(0)) {};

template <class Tp>
constexpr auto IsFunctorV = IsFunctor<Tp>::value;

}  // namespace traits

}  // namespace closure
